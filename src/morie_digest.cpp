// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Native hash family for morie_digest() / morie_hmac() / morie_aes():
// MD5 (RFC 1321), SHA-1 and SHA-224/256/384/512 (FIPS 180-4), CRC-32
// (IEEE 802.3, zlib), CRC-32C (Castagnoli), xxHash32 / xxHash64,
// MurmurHash3 x86_32 (PMurHash32 semantics), Bob Jenkins' one-at-a-time,
// and AES-128/192/256 block encryption (FIPS 197). Each is written from
// its specification; the R-level tests pin every one to the digest
// package's output and to the published test vectors.
//
// All digests are returned as raw bytes; the 32- and 64-bit hashes are
// big-endian so that their hex form is printf("%08x") / ("%016llx"), as
// digest prints them.

#include <Rcpp.h>
#include <cstdint>
#include <cstring>
#include <string>
#include <vector>

namespace {

typedef std::vector<unsigned char> bytes;

bytes as_bytes(SEXP x) {
  if (TYPEOF(x) == RAWSXP) {
    Rcpp::RawVector r(x);
    return bytes(r.begin(), r.end());
  }
  if (TYPEOF(x) == STRSXP && Rf_length(x) == 1) {
    const char* s = CHAR(STRING_ELT(x, 0));
    return bytes(s, s + std::strlen(s));
  }
  Rcpp::stop("expected a raw vector or a single string");
}

inline uint32_t rotl32(uint32_t x, int n) { return (x << n) | (x >> (32 - n)); }
inline uint32_t rotr32(uint32_t x, int n) { return (x >> n) | (x << (32 - n)); }
inline uint64_t rotl64(uint64_t x, int n) { return (x << n) | (x >> (64 - n)); }
inline uint64_t rotr64(uint64_t x, int n) { return (x >> n) | (x << (64 - n)); }
inline uint32_t rd32le(const unsigned char* p) {
  return (uint32_t)p[0] | ((uint32_t)p[1] << 8) | ((uint32_t)p[2] << 16) | ((uint32_t)p[3] << 24);
}
inline uint32_t rd32be(const unsigned char* p) {
  return ((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) | ((uint32_t)p[2] << 8) | (uint32_t)p[3];
}
inline uint64_t rd64le(const unsigned char* p) {
  return (uint64_t)rd32le(p) | ((uint64_t)rd32le(p + 4) << 32);
}
inline uint64_t rd64be(const unsigned char* p) {
  return ((uint64_t)rd32be(p) << 32) | (uint64_t)rd32be(p + 4);
}
inline void wr32be(unsigned char* p, uint32_t v) {
  p[0] = v >> 24; p[1] = v >> 16; p[2] = v >> 8; p[3] = v;
}
inline void wr32le(unsigned char* p, uint32_t v) {
  p[0] = v; p[1] = v >> 8; p[2] = v >> 16; p[3] = v >> 24;
}
inline void wr64be(unsigned char* p, uint64_t v) { wr32be(p, v >> 32); wr32be(p + 4, (uint32_t)v); }
inline void wr64le(unsigned char* p, uint64_t v) { wr32le(p, (uint32_t)v); wr32le(p + 4, v >> 32); }

// ---------------------------------------------------------------- MD5
bytes md5(const bytes& in) {
  static const uint32_t K[64] = {
    0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
    0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be, 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
    0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
    0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
    0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c, 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
    0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
    0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
    0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1, 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391};
  static const int S[64] = {7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
                            5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
                            4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
                            6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21};
  uint32_t a0 = 0x67452301, b0 = 0xefcdab89, c0 = 0x98badcfe, d0 = 0x10325476;
  bytes m(in);
  uint64_t bitlen = (uint64_t)in.size() * 8;
  m.push_back(0x80);
  while (m.size() % 64 != 56) m.push_back(0);
  unsigned char lenb[8]; wr64le(lenb, bitlen);
  m.insert(m.end(), lenb, lenb + 8);
  for (size_t off = 0; off < m.size(); off += 64) {
    uint32_t M[16];
    for (int i = 0; i < 16; ++i) M[i] = rd32le(&m[off + 4 * i]);
    uint32_t A = a0, B = b0, C = c0, D = d0;
    for (int i = 0; i < 64; ++i) {
      uint32_t F; int g;
      if (i < 16) { F = (B & C) | (~B & D); g = i; }
      else if (i < 32) { F = (D & B) | (~D & C); g = (5 * i + 1) % 16; }
      else if (i < 48) { F = B ^ C ^ D; g = (3 * i + 5) % 16; }
      else { F = C ^ (B | ~D); g = (7 * i) % 16; }
      F = F + A + K[i] + M[g];
      A = D; D = C; C = B;
      B = B + rotl32(F, S[i]);
    }
    a0 += A; b0 += B; c0 += C; d0 += D;
  }
  bytes out(16);
  wr32le(&out[0], a0); wr32le(&out[4], b0); wr32le(&out[8], c0); wr32le(&out[12], d0);
  return out;
}

// ---------------------------------------------------------------- SHA-1
bytes sha1(const bytes& in) {
  uint32_t h0 = 0x67452301, h1 = 0xEFCDAB89, h2 = 0x98BADCFE, h3 = 0x10325476, h4 = 0xC3D2E1F0;
  bytes m(in);
  uint64_t bitlen = (uint64_t)in.size() * 8;
  m.push_back(0x80);
  while (m.size() % 64 != 56) m.push_back(0);
  unsigned char lenb[8]; wr64be(lenb, bitlen);
  m.insert(m.end(), lenb, lenb + 8);
  for (size_t off = 0; off < m.size(); off += 64) {
    uint32_t w[80];
    for (int i = 0; i < 16; ++i) w[i] = rd32be(&m[off + 4 * i]);
    for (int i = 16; i < 80; ++i) w[i] = rotl32(w[i - 3] ^ w[i - 8] ^ w[i - 14] ^ w[i - 16], 1);
    uint32_t a = h0, b = h1, c = h2, d = h3, e = h4;
    for (int i = 0; i < 80; ++i) {
      uint32_t f, k;
      if (i < 20) { f = (b & c) | (~b & d); k = 0x5A827999; }
      else if (i < 40) { f = b ^ c ^ d; k = 0x6ED9EBA1; }
      else if (i < 60) { f = (b & c) | (b & d) | (c & d); k = 0x8F1BBCDC; }
      else { f = b ^ c ^ d; k = 0xCA62C1D6; }
      uint32_t t = rotl32(a, 5) + f + e + k + w[i];
      e = d; d = c; c = rotl32(b, 30); b = a; a = t;
    }
    h0 += a; h1 += b; h2 += c; h3 += d; h4 += e;
  }
  bytes out(20);
  wr32be(&out[0], h0); wr32be(&out[4], h1); wr32be(&out[8], h2); wr32be(&out[12], h3); wr32be(&out[16], h4);
  return out;
}

// ---------------------------------------------------------------- SHA-224/256
bytes sha256_family(const bytes& in, bool is224) {
  static const uint32_t K[64] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2};
  uint32_t H[8];
  if (is224) {
    const uint32_t I[8] = {0xc1059ed8, 0x367cd507, 0x3070dd17, 0xf70e5939, 0xffc00b31, 0x68581511, 0x64f98fa7, 0xbefa4fa4};
    std::memcpy(H, I, sizeof H);
  } else {
    const uint32_t I[8] = {0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19};
    std::memcpy(H, I, sizeof H);
  }
  bytes m(in);
  uint64_t bitlen = (uint64_t)in.size() * 8;
  m.push_back(0x80);
  while (m.size() % 64 != 56) m.push_back(0);
  unsigned char lenb[8]; wr64be(lenb, bitlen);
  m.insert(m.end(), lenb, lenb + 8);
  for (size_t off = 0; off < m.size(); off += 64) {
    uint32_t w[64];
    for (int i = 0; i < 16; ++i) w[i] = rd32be(&m[off + 4 * i]);
    for (int i = 16; i < 64; ++i) {
      uint32_t s0 = rotr32(w[i - 15], 7) ^ rotr32(w[i - 15], 18) ^ (w[i - 15] >> 3);
      uint32_t s1 = rotr32(w[i - 2], 17) ^ rotr32(w[i - 2], 19) ^ (w[i - 2] >> 10);
      w[i] = w[i - 16] + s0 + w[i - 7] + s1;
    }
    uint32_t a = H[0], b = H[1], c = H[2], d = H[3], e = H[4], f = H[5], g = H[6], h = H[7];
    for (int i = 0; i < 64; ++i) {
      uint32_t S1 = rotr32(e, 6) ^ rotr32(e, 11) ^ rotr32(e, 25);
      uint32_t ch = (e & f) ^ (~e & g);
      uint32_t t1 = h + S1 + ch + K[i] + w[i];
      uint32_t S0 = rotr32(a, 2) ^ rotr32(a, 13) ^ rotr32(a, 22);
      uint32_t mj = (a & b) ^ (a & c) ^ (b & c);
      uint32_t t2 = S0 + mj;
      h = g; g = f; f = e; e = d + t1; d = c; c = b; b = a; a = t1 + t2;
    }
    H[0] += a; H[1] += b; H[2] += c; H[3] += d; H[4] += e; H[5] += f; H[6] += g; H[7] += h;
  }
  bytes out(is224 ? 28 : 32);
  for (int i = 0; i < (is224 ? 7 : 8); ++i) wr32be(&out[4 * i], H[i]);
  return out;
}

// ---------------------------------------------------------------- SHA-384/512
bytes sha512_family(const bytes& in, bool is384) {
  static const uint64_t K[80] = {
    0x428a2f98d728ae22ULL, 0x7137449123ef65cdULL, 0xb5c0fbcfec4d3b2fULL, 0xe9b5dba58189dbbcULL,
    0x3956c25bf348b538ULL, 0x59f111f1b605d019ULL, 0x923f82a4af194f9bULL, 0xab1c5ed5da6d8118ULL,
    0xd807aa98a3030242ULL, 0x12835b0145706fbeULL, 0x243185be4ee4b28cULL, 0x550c7dc3d5ffb4e2ULL,
    0x72be5d74f27b896fULL, 0x80deb1fe3b1696b1ULL, 0x9bdc06a725c71235ULL, 0xc19bf174cf692694ULL,
    0xe49b69c19ef14ad2ULL, 0xefbe4786384f25e3ULL, 0x0fc19dc68b8cd5b5ULL, 0x240ca1cc77ac9c65ULL,
    0x2de92c6f592b0275ULL, 0x4a7484aa6ea6e483ULL, 0x5cb0a9dcbd41fbd4ULL, 0x76f988da831153b5ULL,
    0x983e5152ee66dfabULL, 0xa831c66d2db43210ULL, 0xb00327c898fb213fULL, 0xbf597fc7beef0ee4ULL,
    0xc6e00bf33da88fc2ULL, 0xd5a79147930aa725ULL, 0x06ca6351e003826fULL, 0x142929670a0e6e70ULL,
    0x27b70a8546d22ffcULL, 0x2e1b21385c26c926ULL, 0x4d2c6dfc5ac42aedULL, 0x53380d139d95b3dfULL,
    0x650a73548baf63deULL, 0x766a0abb3c77b2a8ULL, 0x81c2c92e47edaee6ULL, 0x92722c851482353bULL,
    0xa2bfe8a14cf10364ULL, 0xa81a664bbc423001ULL, 0xc24b8b70d0f89791ULL, 0xc76c51a30654be30ULL,
    0xd192e819d6ef5218ULL, 0xd69906245565a910ULL, 0xf40e35855771202aULL, 0x106aa07032bbd1b8ULL,
    0x19a4c116b8d2d0c8ULL, 0x1e376c085141ab53ULL, 0x2748774cdf8eeb99ULL, 0x34b0bcb5e19b48a8ULL,
    0x391c0cb3c5c95a63ULL, 0x4ed8aa4ae3418acbULL, 0x5b9cca4f7763e373ULL, 0x682e6ff3d6b2b8a3ULL,
    0x748f82ee5defb2fcULL, 0x78a5636f43172f60ULL, 0x84c87814a1f0ab72ULL, 0x8cc702081a6439ecULL,
    0x90befffa23631e28ULL, 0xa4506cebde82bde9ULL, 0xbef9a3f7b2c67915ULL, 0xc67178f2e372532bULL,
    0xca273eceea26619cULL, 0xd186b8c721c0c207ULL, 0xeada7dd6cde0eb1eULL, 0xf57d4f7fee6ed178ULL,
    0x06f067aa72176fbaULL, 0x0a637dc5a2c898a6ULL, 0x113f9804bef90daeULL, 0x1b710b35131c471bULL,
    0x28db77f523047d84ULL, 0x32caab7b40c72493ULL, 0x3c9ebe0a15c9bebcULL, 0x431d67c49c100d4cULL,
    0x4cc5d4becb3e42b6ULL, 0x597f299cfc657e2aULL, 0x5fcb6fab3ad6faecULL, 0x6c44198c4a475817ULL};
  uint64_t H[8];
  if (is384) {
    const uint64_t I[8] = {0xcbbb9d5dc1059ed8ULL, 0x629a292a367cd507ULL, 0x9159015a3070dd17ULL, 0x152fecd8f70e5939ULL,
                           0x67332667ffc00b31ULL, 0x8eb44a8768581511ULL, 0xdb0c2e0d64f98fa7ULL, 0x47b5481dbefa4fa4ULL};
    std::memcpy(H, I, sizeof H);
  } else {
    const uint64_t I[8] = {0x6a09e667f3bcc908ULL, 0xbb67ae8584caa73bULL, 0x3c6ef372fe94f82bULL, 0xa54ff53a5f1d36f1ULL,
                           0x510e527fade682d1ULL, 0x9b05688c2b3e6c1fULL, 0x1f83d9abfb41bd6bULL, 0x5be0cd19137e2179ULL};
    std::memcpy(H, I, sizeof H);
  }
  bytes m(in);
  uint64_t bitlen = (uint64_t)in.size() * 8;
  m.push_back(0x80);
  while (m.size() % 128 != 112) m.push_back(0);
  for (int i = 0; i < 8; ++i) m.push_back(0);  // high 64 bits of the 128-bit length
  unsigned char lenb[8]; wr64be(lenb, bitlen);
  m.insert(m.end(), lenb, lenb + 8);
  for (size_t off = 0; off < m.size(); off += 128) {
    uint64_t w[80];
    for (int i = 0; i < 16; ++i) w[i] = rd64be(&m[off + 8 * i]);
    for (int i = 16; i < 80; ++i) {
      uint64_t s0 = rotr64(w[i - 15], 1) ^ rotr64(w[i - 15], 8) ^ (w[i - 15] >> 7);
      uint64_t s1 = rotr64(w[i - 2], 19) ^ rotr64(w[i - 2], 61) ^ (w[i - 2] >> 6);
      w[i] = w[i - 16] + s0 + w[i - 7] + s1;
    }
    uint64_t a = H[0], b = H[1], c = H[2], d = H[3], e = H[4], f = H[5], g = H[6], h = H[7];
    for (int i = 0; i < 80; ++i) {
      uint64_t S1 = rotr64(e, 14) ^ rotr64(e, 18) ^ rotr64(e, 41);
      uint64_t ch = (e & f) ^ (~e & g);
      uint64_t t1 = h + S1 + ch + K[i] + w[i];
      uint64_t S0 = rotr64(a, 28) ^ rotr64(a, 34) ^ rotr64(a, 39);
      uint64_t mj = (a & b) ^ (a & c) ^ (b & c);
      uint64_t t2 = S0 + mj;
      h = g; g = f; f = e; e = d + t1; d = c; c = b; b = a; a = t1 + t2;
    }
    H[0] += a; H[1] += b; H[2] += c; H[3] += d; H[4] += e; H[5] += f; H[6] += g; H[7] += h;
  }
  bytes out(is384 ? 48 : 64);
  for (int i = 0; i < (is384 ? 6 : 8); ++i) wr64be(&out[8 * i], H[i]);
  return out;
}

// ---------------------------------------------------------------- CRC-32 / CRC-32C
uint32_t crc32_generic(const bytes& in, uint32_t poly) {
  uint32_t table[256];
  for (uint32_t i = 0; i < 256; ++i) {
    uint32_t c = i;
    for (int k = 0; k < 8; ++k) c = (c & 1) ? (poly ^ (c >> 1)) : (c >> 1);
    table[i] = c;
  }
  uint32_t crc = 0xFFFFFFFFu;
  for (size_t i = 0; i < in.size(); ++i) crc = table[(crc ^ in[i]) & 0xFF] ^ (crc >> 8);
  return crc ^ 0xFFFFFFFFu;
}

// ---------------------------------------------------------------- xxHash32 / xxHash64
uint32_t xxh32(const bytes& in, uint32_t seed) {
  const uint32_t P1 = 2654435761U, P2 = 2246822519U, P3 = 3266489917U, P4 = 668265263U, P5 = 374761393U;
  const unsigned char* p = in.data(); const unsigned char* end = p + in.size();
  uint32_t h;
  if (in.size() >= 16) {
    uint32_t v1 = seed + P1 + P2, v2 = seed + P2, v3 = seed, v4 = seed - P1;
    const unsigned char* limit = end - 16;
    do {
      v1 = rotl32(v1 + rd32le(p) * P2, 13) * P1; p += 4;
      v2 = rotl32(v2 + rd32le(p) * P2, 13) * P1; p += 4;
      v3 = rotl32(v3 + rd32le(p) * P2, 13) * P1; p += 4;
      v4 = rotl32(v4 + rd32le(p) * P2, 13) * P1; p += 4;
    } while (p <= limit);
    h = rotl32(v1, 1) + rotl32(v2, 7) + rotl32(v3, 12) + rotl32(v4, 18);
  } else {
    h = seed + P5;
  }
  h += (uint32_t)in.size();
  while (p + 4 <= end) { h += rd32le(p) * P3; h = rotl32(h, 17) * P4; p += 4; }
  while (p < end) { h += (*p) * P5; h = rotl32(h, 11) * P1; ++p; }
  h ^= h >> 15; h *= P2; h ^= h >> 13; h *= P3; h ^= h >> 16;
  return h;
}

uint64_t xxh64(const bytes& in, uint64_t seed) {
  const uint64_t P1 = 11400714785074694791ULL, P2 = 14029467366897019727ULL, P3 = 1609587929392839161ULL,
                 P4 = 9650029242287828579ULL, P5 = 2870177450012600261ULL;
  const unsigned char* p = in.data(); const unsigned char* end = p + in.size();
  uint64_t h;
  auto round = [&](uint64_t acc, uint64_t input) { acc += input * P2; acc = rotl64(acc, 31); return acc * P1; };
  auto merge = [&](uint64_t acc, uint64_t val) { val = round(0, val); acc ^= val; return acc * P1 + P4; };
  if (in.size() >= 32) {
    uint64_t v1 = seed + P1 + P2, v2 = seed + P2, v3 = seed, v4 = seed - P1;
    const unsigned char* limit = end - 32;
    do {
      v1 = round(v1, rd64le(p)); p += 8;
      v2 = round(v2, rd64le(p)); p += 8;
      v3 = round(v3, rd64le(p)); p += 8;
      v4 = round(v4, rd64le(p)); p += 8;
    } while (p <= limit);
    h = rotl64(v1, 1) + rotl64(v2, 7) + rotl64(v3, 12) + rotl64(v4, 18);
    h = merge(h, v1); h = merge(h, v2); h = merge(h, v3); h = merge(h, v4);
  } else {
    h = seed + P5;
  }
  h += (uint64_t)in.size();
  while (p + 8 <= end) { uint64_t k1 = round(0, rd64le(p)); h ^= k1; h = rotl64(h, 27) * P1 + P4; p += 8; }
  if (p + 4 <= end) { h ^= (uint64_t)rd32le(p) * P1; h = rotl64(h, 23) * P2 + P3; p += 4; }
  while (p < end) { h ^= (*p) * P5; h = rotl64(h, 11) * P1; ++p; }
  h ^= h >> 33; h *= P2; h ^= h >> 29; h *= P3; h ^= h >> 32;
  return h;
}

// ---------------------------------------------------------------- MurmurHash3 x86_32
uint32_t murmur32(const bytes& in, uint32_t seed) {
  const uint32_t c1 = 0xcc9e2d51, c2 = 0x1b873593;
  uint32_t h1 = seed;
  size_t nblocks = in.size() / 4;
  const unsigned char* p = in.data();
  for (size_t i = 0; i < nblocks; ++i) {
    uint32_t k1 = rd32le(p + 4 * i);
    k1 *= c1; k1 = rotl32(k1, 15); k1 *= c2;
    h1 ^= k1; h1 = rotl32(h1, 13); h1 = h1 * 5 + 0xe6546b64;
  }
  const unsigned char* tail = p + nblocks * 4;
  uint32_t k1 = 0;
  switch (in.size() & 3) {
    case 3: k1 ^= (uint32_t)tail[2] << 16;  // fall through
    case 2: k1 ^= (uint32_t)tail[1] << 8;   // fall through
    case 1: k1 ^= tail[0]; k1 *= c1; k1 = rotl32(k1, 15); k1 *= c2; h1 ^= k1;
  }
  h1 ^= (uint32_t)in.size();
  h1 ^= h1 >> 16; h1 *= 0x85ebca6b; h1 ^= h1 >> 13; h1 *= 0xc2b2ae35; h1 ^= h1 >> 16;
  return h1;
}

// ---------------------------------------------------------------- Jenkins one-at-a-time
uint32_t jenkins_oaat(const char* key, uint32_t seed) {
  uint32_t hash = seed;
  for (; *key; ++key) {
    hash += (uint32_t)(int32_t)(signed char)*key;  // digest adds the (signed) char
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);
  return hash;
}

// ---------------------------------------------------------------- AES (FIPS 197)
struct Aes {
  int rounds;
  uint32_t rk[60];
  static const unsigned char sbox[256];
  static const unsigned char inv_sbox[256];
  static inline unsigned char xtime(unsigned char x) { return (x << 1) ^ ((x & 0x80) ? 0x1b : 0); }
  static inline unsigned char mul(unsigned char a, unsigned char b) {
    unsigned char r = 0;
    while (b) { if (b & 1) r ^= a; a = xtime(a); b >>= 1; }
    return r;
  }
  explicit Aes(const bytes& key) {
    int nk = key.size() / 4;
    rounds = nk + 6;
    int total = 4 * (rounds + 1);
    for (int i = 0; i < nk; ++i) rk[i] = rd32be(&key[4 * i]);
    uint32_t rcon = 1;
    for (int i = nk; i < total; ++i) {
      uint32_t t = rk[i - 1];
      if (i % nk == 0) {
        t = rotl32(t, 8);
        t = ((uint32_t)sbox[t >> 24] << 24) | ((uint32_t)sbox[(t >> 16) & 0xff] << 16) |
            ((uint32_t)sbox[(t >> 8) & 0xff] << 8) | sbox[t & 0xff];
        t ^= rcon << 24;
        rcon = xtime(rcon);
      } else if (nk > 6 && i % nk == 4) {
        t = ((uint32_t)sbox[t >> 24] << 24) | ((uint32_t)sbox[(t >> 16) & 0xff] << 16) |
            ((uint32_t)sbox[(t >> 8) & 0xff] << 8) | sbox[t & 0xff];
      }
      rk[i] = rk[i - nk] ^ t;
    }
  }
  void add_round_key(unsigned char s[16], int r) const {
    for (int c = 0; c < 4; ++c) {
      uint32_t k = rk[4 * r + c];
      s[4 * c] ^= k >> 24; s[4 * c + 1] ^= k >> 16; s[4 * c + 2] ^= k >> 8; s[4 * c + 3] ^= k;
    }
  }
  void encrypt_block(unsigned char s[16]) const {
    add_round_key(s, 0);
    for (int r = 1; r <= rounds; ++r) {
      for (int i = 0; i < 16; ++i) s[i] = sbox[s[i]];
      unsigned char t[16];
      for (int c = 0; c < 4; ++c) for (int rr = 0; rr < 4; ++rr) t[4 * c + rr] = s[4 * ((c + rr) % 4) + rr];
      std::memcpy(s, t, 16);
      if (r != rounds) {
        for (int c = 0; c < 4; ++c) {
          unsigned char a0 = s[4 * c], a1 = s[4 * c + 1], a2 = s[4 * c + 2], a3 = s[4 * c + 3];
          s[4 * c] = mul(a0, 2) ^ mul(a1, 3) ^ a2 ^ a3;
          s[4 * c + 1] = a0 ^ mul(a1, 2) ^ mul(a2, 3) ^ a3;
          s[4 * c + 2] = a0 ^ a1 ^ mul(a2, 2) ^ mul(a3, 3);
          s[4 * c + 3] = mul(a0, 3) ^ a1 ^ a2 ^ mul(a3, 2);
        }
      }
      add_round_key(s, r);
    }
  }
  void decrypt_block(unsigned char s[16]) const {
    add_round_key(s, rounds);
    for (int r = rounds - 1; r >= 0; --r) {
      unsigned char t[16];
      for (int c = 0; c < 4; ++c) for (int rr = 0; rr < 4; ++rr) t[4 * ((c + rr) % 4) + rr] = s[4 * c + rr];
      for (int i = 0; i < 16; ++i) s[i] = inv_sbox[t[i]];
      add_round_key(s, r);
      if (r != 0) {
        for (int c = 0; c < 4; ++c) {
          unsigned char a0 = s[4 * c], a1 = s[4 * c + 1], a2 = s[4 * c + 2], a3 = s[4 * c + 3];
          s[4 * c] = mul(a0, 14) ^ mul(a1, 11) ^ mul(a2, 13) ^ mul(a3, 9);
          s[4 * c + 1] = mul(a0, 9) ^ mul(a1, 14) ^ mul(a2, 11) ^ mul(a3, 13);
          s[4 * c + 2] = mul(a0, 13) ^ mul(a1, 9) ^ mul(a2, 14) ^ mul(a3, 11);
          s[4 * c + 3] = mul(a0, 11) ^ mul(a1, 13) ^ mul(a2, 9) ^ mul(a3, 14);
        }
      }
    }
  }
};
const unsigned char Aes::sbox[256] = {
  0x63,0x7c,0x77,0x7b,0xf2,0x6b,0x6f,0xc5,0x30,0x01,0x67,0x2b,0xfe,0xd7,0xab,0x76,
  0xca,0x82,0xc9,0x7d,0xfa,0x59,0x47,0xf0,0xad,0xd4,0xa2,0xaf,0x9c,0xa4,0x72,0xc0,
  0xb7,0xfd,0x93,0x26,0x36,0x3f,0xf7,0xcc,0x34,0xa5,0xe5,0xf1,0x71,0xd8,0x31,0x15,
  0x04,0xc7,0x23,0xc3,0x18,0x96,0x05,0x9a,0x07,0x12,0x80,0xe2,0xeb,0x27,0xb2,0x75,
  0x09,0x83,0x2c,0x1a,0x1b,0x6e,0x5a,0xa0,0x52,0x3b,0xd6,0xb3,0x29,0xe3,0x2f,0x84,
  0x53,0xd1,0x00,0xed,0x20,0xfc,0xb1,0x5b,0x6a,0xcb,0xbe,0x39,0x4a,0x4c,0x58,0xcf,
  0xd0,0xef,0xaa,0xfb,0x43,0x4d,0x33,0x85,0x45,0xf9,0x02,0x7f,0x50,0x3c,0x9f,0xa8,
  0x51,0xa3,0x40,0x8f,0x92,0x9d,0x38,0xf5,0xbc,0xb6,0xda,0x21,0x10,0xff,0xf3,0xd2,
  0xcd,0x0c,0x13,0xec,0x5f,0x97,0x44,0x17,0xc4,0xa7,0x7e,0x3d,0x64,0x5d,0x19,0x73,
  0x60,0x81,0x4f,0xdc,0x22,0x2a,0x90,0x88,0x46,0xee,0xb8,0x14,0xde,0x5e,0x0b,0xdb,
  0xe0,0x32,0x3a,0x0a,0x49,0x06,0x24,0x5c,0xc2,0xd3,0xac,0x62,0x91,0x95,0xe4,0x79,
  0xe7,0xc8,0x37,0x6d,0x8d,0xd5,0x4e,0xa9,0x6c,0x56,0xf4,0xea,0x65,0x7a,0xae,0x08,
  0xba,0x78,0x25,0x2e,0x1c,0xa6,0xb4,0xc6,0xe8,0xdd,0x74,0x1f,0x4b,0xbd,0x8b,0x8a,
  0x70,0x3e,0xb5,0x66,0x48,0x03,0xf6,0x0e,0x61,0x35,0x57,0xb9,0x86,0xc1,0x1d,0x9e,
  0xe1,0xf8,0x98,0x11,0x69,0xd9,0x8e,0x94,0x9b,0x1e,0x87,0xe9,0xce,0x55,0x28,0xdf,
  0x8c,0xa1,0x89,0x0d,0xbf,0xe6,0x42,0x68,0x41,0x99,0x2d,0x0f,0xb0,0x54,0xbb,0x16};
const unsigned char Aes::inv_sbox[256] = {
  0x52,0x09,0x6a,0xd5,0x30,0x36,0xa5,0x38,0xbf,0x40,0xa3,0x9e,0x81,0xf3,0xd7,0xfb,
  0x7c,0xe3,0x39,0x82,0x9b,0x2f,0xff,0x87,0x34,0x8e,0x43,0x44,0xc4,0xde,0xe9,0xcb,
  0x54,0x7b,0x94,0x32,0xa6,0xc2,0x23,0x3d,0xee,0x4c,0x95,0x0b,0x42,0xfa,0xc3,0x4e,
  0x08,0x2e,0xa1,0x66,0x28,0xd9,0x24,0xb2,0x76,0x5b,0xa2,0x49,0x6d,0x8b,0xd1,0x25,
  0x72,0xf8,0xf6,0x64,0x86,0x68,0x98,0x16,0xd4,0xa4,0x5c,0xcc,0x5d,0x65,0xb6,0x92,
  0x6c,0x70,0x48,0x50,0xfd,0xed,0xb9,0xda,0x5e,0x15,0x46,0x57,0xa7,0x8d,0x9d,0x84,
  0x90,0xd8,0xab,0x00,0x8c,0xbc,0xd3,0x0a,0xf7,0xe4,0x58,0x05,0xb8,0xb3,0x45,0x06,
  0xd0,0x2c,0x1e,0x8f,0xca,0x3f,0x0f,0x02,0xc1,0xaf,0xbd,0x03,0x01,0x13,0x8a,0x6b,
  0x3a,0x91,0x11,0x41,0x4f,0x67,0xdc,0xea,0x97,0xf2,0xcf,0xce,0xf0,0xb4,0xe6,0x73,
  0x96,0xac,0x74,0x22,0xe7,0xad,0x35,0x85,0xe2,0xf9,0x37,0xe8,0x1c,0x75,0xdf,0x6e,
  0x47,0xf1,0x1a,0x71,0x1d,0x29,0xc5,0x89,0x6f,0xb7,0x62,0x0e,0xaa,0x18,0xbe,0x1b,
  0xfc,0x56,0x3e,0x4b,0xc6,0xd2,0x79,0x20,0x9a,0xdb,0xc0,0xfe,0x78,0xcd,0x5a,0xf4,
  0x1f,0xdd,0xa8,0x33,0x88,0x07,0xc7,0x31,0xb1,0x12,0x10,0x59,0x27,0x80,0xec,0x5f,
  0x60,0x51,0x7f,0xa9,0x19,0xb5,0x4a,0x0d,0x2d,0xe5,0x7a,0x9f,0x93,0xc9,0x9c,0xef,
  0xa0,0xe0,0x3b,0x4d,0xae,0x2a,0xf5,0xb0,0xc8,0xeb,0xbb,0x3c,0x83,0x53,0x99,0x61,
  0x17,0x2b,0x04,0x7e,0xba,0x77,0xd6,0x26,0xe1,0x69,0x14,0x63,0x55,0x21,0x0c,0x7d};

}  // namespace

// Algorithm codes follow digest's algo_int(): 1 md5, 2 sha1, 3 crc32,
// 4 sha256, 5 sha512, 6 xxhash32, 7 xxhash64, 8 murmur32, 11 crc32c,
// and this package's extensions 21 sha224, 22 sha384.
// [[Rcpp::export(name = ".rmorie_digest_impl")]]
Rcpp::RawVector morie_digest_native(SEXP data, int algo, double seed) {
  bytes in = as_bytes(data);
  bytes out;
  uint32_t s32 = (uint32_t)(int64_t)seed;
  switch (algo) {
    case 1: out = md5(in); break;
    case 2: out = sha1(in); break;
    case 3: { out.resize(4); wr32be(&out[0], crc32_generic(in, 0xEDB88320u)); break; }
    case 4: out = sha256_family(in, false); break;
    case 5: out = sha512_family(in, false); break;
    case 6: { out.resize(4); wr32be(&out[0], xxh32(in, s32)); break; }
    case 7: { out.resize(8); wr64be(&out[0], xxh64(in, (uint64_t)(int64_t)seed)); break; }
    case 8: { out.resize(4); wr32be(&out[0], murmur32(in, s32)); break; }
    case 11: { out.resize(4); wr32be(&out[0], crc32_generic(in, 0x82F63B78u)); break; }
    case 21: out = sha256_family(in, true); break;
    case 22: out = sha512_family(in, true); break;
    default: Rcpp::stop("unknown digest algorithm code %d", algo);
  }
  Rcpp::RawVector r(out.size());
  if (!out.empty()) std::memcpy(&r[0], out.data(), out.size());
  return r;
}

// [[Rcpp::export(name = ".rmorie_digest2int_impl")]]
Rcpp::IntegerVector morie_digest2int_native(Rcpp::CharacterVector x, int seed) {
  R_xlen_t n = x.size();
  Rcpp::IntegerVector out(n);
  for (R_xlen_t i = 0; i < n; ++i) {
    if (Rcpp::CharacterVector::is_na(x[i])) { out[i] = NA_INTEGER; continue; }
    out[i] = (int32_t)jenkins_oaat(CHAR(STRING_ELT(x, i)), (uint32_t)seed);
  }
  return out;
}

// AES-ECB over whole 16-byte blocks; the chaining modes are composed in R
// exactly as digest::AES composes them over its ECB primitive.
// [[Rcpp::export(name = ".rmorie_aes_ecb_impl")]]
Rcpp::RawVector morie_aes_ecb_native(Rcpp::RawVector key, Rcpp::RawVector data, bool encrypt) {
  size_t kn = key.size();
  if (kn != 16 && kn != 24 && kn != 32) Rcpp::stop("AES only supports 16, 24 and 32 byte keys");
  if (data.size() % 16 != 0) Rcpp::stop("Text length must be a multiple of 16 bytes");
  Aes aes(bytes(key.begin(), key.end()));
  Rcpp::RawVector out(data.size());
  for (R_xlen_t off = 0; off < data.size(); off += 16) {
    unsigned char blk[16];
    std::memcpy(blk, &data[off], 16);
    if (encrypt) aes.encrypt_block(blk); else aes.decrypt_block(blk);
    std::memcpy(&out[off], blk, 16);
  }
  return out;
}
