// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Native SHA-256 / HMAC-SHA256 / PBKDF2-HMAC-SHA256 (module 22).
// Standalone FIPS 180-4 / RFC 2104 / RFC 8018 implementations with no
// external library dependency (unlike the libsodium-gated primitives
// in morie_crypto_sym.cpp), so hashing works on every build. SHA-512
// is deliberately not implemented: nothing in the package consumes it
// (YAGNI; add alongside if a caller appears).
//
// Validated against NIST FIPS 180-4 test vectors in
// tests/testthat/test-crypto-native.R and against digest/openssl in
// tests/cross/test-morie_vs_digest.R.

#include <Rcpp.h>
#include <cstdint>
#include <cstring>
#include <string>
#include <vector>

namespace {

struct Sha256 {
  uint32_t h[8];
  uint64_t len;
  unsigned char buf[64];
  size_t buflen;

  static const uint32_t K[64];

  Sha256() { reset(); }

  void reset() {
    static const uint32_t H0[8] = {
      0x6a09e667u, 0xbb67ae85u, 0x3c6ef372u, 0xa54ff53au,
      0x510e527fu, 0x9b05688cu, 0x1f83d9abu, 0x5be0cd19u};
    std::memcpy(h, H0, sizeof(H0));
    len = 0; buflen = 0;
  }

  static uint32_t rotr(uint32_t x, int n) {
    return (x >> n) | (x << (32 - n));
  }

  void block(const unsigned char* p) {
    uint32_t w[64];
    for (int i = 0; i < 16; ++i) {
      w[i] = (uint32_t(p[4 * i]) << 24) | (uint32_t(p[4 * i + 1]) << 16) |
             (uint32_t(p[4 * i + 2]) << 8) | uint32_t(p[4 * i + 3]);
    }
    for (int i = 16; i < 64; ++i) {
      uint32_t s0 = rotr(w[i - 15], 7) ^ rotr(w[i - 15], 18) ^ (w[i - 15] >> 3);
      uint32_t s1 = rotr(w[i - 2], 17) ^ rotr(w[i - 2], 19) ^ (w[i - 2] >> 10);
      w[i] = w[i - 16] + s0 + w[i - 7] + s1;
    }
    uint32_t a = h[0], b = h[1], c = h[2], d = h[3];
    uint32_t e = h[4], f = h[5], g = h[6], hh = h[7];
    for (int i = 0; i < 64; ++i) {
      uint32_t S1 = rotr(e, 6) ^ rotr(e, 11) ^ rotr(e, 25);
      uint32_t ch = (e & f) ^ (~e & g);
      uint32_t t1 = hh + S1 + ch + K[i] + w[i];
      uint32_t S0 = rotr(a, 2) ^ rotr(a, 13) ^ rotr(a, 22);
      uint32_t maj = (a & b) ^ (a & c) ^ (b & c);
      uint32_t t2 = S0 + maj;
      hh = g; g = f; f = e; e = d + t1;
      d = c; c = b; b = a; a = t1 + t2;
    }
    h[0] += a; h[1] += b; h[2] += c; h[3] += d;
    h[4] += e; h[5] += f; h[6] += g; h[7] += hh;
  }

  void update(const unsigned char* p, size_t n) {
    len += n;
    while (n > 0) {
      size_t take = std::min(n, size_t(64) - buflen);
      std::memcpy(buf + buflen, p, take);
      buflen += take; p += take; n -= take;
      if (buflen == 64) { block(buf); buflen = 0; }
    }
  }

  void final(unsigned char out[32]) {
    uint64_t bits = len * 8;
    unsigned char pad = 0x80;
    update(&pad, 1);
    unsigned char zero = 0;
    while (buflen != 56) update(&zero, 1);
    unsigned char lenb[8];
    for (int i = 0; i < 8; ++i) lenb[i] = (bits >> (56 - 8 * i)) & 0xff;
    update(lenb, 8);
    for (int i = 0; i < 8; ++i) {
      out[4 * i]     = (h[i] >> 24) & 0xff;
      out[4 * i + 1] = (h[i] >> 16) & 0xff;
      out[4 * i + 2] = (h[i] >> 8) & 0xff;
      out[4 * i + 3] = h[i] & 0xff;
    }
  }
};

const uint32_t Sha256::K[64] = {
  0x428a2f98u, 0x71374491u, 0xb5c0fbcfu, 0xe9b5dba5u,
  0x3956c25bu, 0x59f111f1u, 0x923f82a4u, 0xab1c5ed5u,
  0xd807aa98u, 0x12835b01u, 0x243185beu, 0x550c7dc3u,
  0x72be5d74u, 0x80deb1feu, 0x9bdc06a7u, 0xc19bf174u,
  0xe49b69c1u, 0xefbe4786u, 0x0fc19dc6u, 0x240ca1ccu,
  0x2de92c6fu, 0x4a7484aau, 0x5cb0a9dcu, 0x76f988dau,
  0x983e5152u, 0xa831c66du, 0xb00327c8u, 0xbf597fc7u,
  0xc6e00bf3u, 0xd5a79147u, 0x06ca6351u, 0x14292967u,
  0x27b70a85u, 0x2e1b2138u, 0x4d2c6dfcu, 0x53380d13u,
  0x650a7354u, 0x766a0abbu, 0x81c2c92eu, 0x92722c85u,
  0xa2bfe8a1u, 0xa81a664bu, 0xc24b8b70u, 0xc76c51a3u,
  0xd192e819u, 0xd6990624u, 0xf40e3585u, 0x106aa070u,
  0x19a4c116u, 0x1e376c08u, 0x2748774cu, 0x34b0bcb5u,
  0x391c0cb3u, 0x4ed8aa4au, 0x5b9cca4fu, 0x682e6ff3u,
  0x748f82eeu, 0x78a5636fu, 0x84c87814u, 0x8cc70208u,
  0x90befffau, 0xa4506cebu, 0xbef9a3f7u, 0xc67178f2u};

void sha256_buf(const unsigned char* p, size_t n, unsigned char out[32]) {
  Sha256 s;
  s.update(p, n);
  s.final(out);
}

void hmac_sha256_buf(const unsigned char* key, size_t keylen,
                     const unsigned char* msg, size_t msglen,
                     unsigned char out[32]) {
  unsigned char k[64];
  std::memset(k, 0, 64);
  if (keylen > 64) {
    sha256_buf(key, keylen, k);        // hashed key, first 32 bytes set
  } else {
    std::memcpy(k, key, keylen);
  }
  unsigned char ipad[64], opad[64];
  for (int i = 0; i < 64; ++i) {
    ipad[i] = k[i] ^ 0x36;
    opad[i] = k[i] ^ 0x5c;
  }
  Sha256 inner;
  inner.update(ipad, 64);
  inner.update(msg, msglen);
  unsigned char ih[32];
  inner.final(ih);
  Sha256 outer;
  outer.update(opad, 64);
  outer.update(ih, 32);
  outer.final(out);
}

std::vector<unsigned char> as_bytes(SEXP x) {
  if (TYPEOF(x) == RAWSXP) {
    Rcpp::RawVector r(x);
    return std::vector<unsigned char>(r.begin(), r.end());
  }
  if (TYPEOF(x) == STRSXP && Rf_length(x) == 1) {
    const char* s = CHAR(STRING_ELT(x, 0));
    size_t n = std::strlen(s);
    return std::vector<unsigned char>(s, s + n);
  }
  Rcpp::stop("expected a raw vector or a single string");
}

std::string to_hex(const unsigned char* p, size_t n) {
  static const char* hexd = "0123456789abcdef";
  std::string out(2 * n, '0');
  for (size_t i = 0; i < n; ++i) {
    out[2 * i] = hexd[p[i] >> 4];
    out[2 * i + 1] = hexd[p[i] & 0xf];
  }
  return out;
}

}  // namespace

// [[Rcpp::export(name = ".rmorie_sha256_impl")]]
Rcpp::RawVector morie_crypto_sha256_native(SEXP data) {
  std::vector<unsigned char> in = as_bytes(data);
  unsigned char out[32];
  sha256_buf(in.data(), in.size(), out);
  Rcpp::RawVector r(32);
  std::memcpy(&r[0], out, 32);
  return r;
}

// [[Rcpp::export(name = ".rmorie_sha256_hex_impl")]]
std::string morie_crypto_sha256_hex_native(SEXP data) {
  std::vector<unsigned char> in = as_bytes(data);
  unsigned char out[32];
  sha256_buf(in.data(), in.size(), out);
  return to_hex(out, 32);
}

// [[Rcpp::export(name = ".rmorie_hmac_sha256_impl")]]
Rcpp::RawVector morie_crypto_hmac_sha256_native(SEXP key, SEXP msg) {
  std::vector<unsigned char> k = as_bytes(key);
  std::vector<unsigned char> m = as_bytes(msg);
  unsigned char out[32];
  hmac_sha256_buf(k.data(), k.size(), m.data(), m.size(), out);
  Rcpp::RawVector r(32);
  std::memcpy(&r[0], out, 32);
  return r;
}

// [[Rcpp::export(name = ".rmorie_pbkdf2_sha256_impl")]]
Rcpp::RawVector morie_crypto_pbkdf2_sha256_native(SEXP password,
                                                  SEXP salt,
                                                  int iterations,
                                                  int dklen) {
  if (iterations < 1) Rcpp::stop("iterations must be >= 1");
  if (dklen < 1 || dklen > 1024) Rcpp::stop("dklen must be in 1..1024");
  std::vector<unsigned char> pw = as_bytes(password);
  std::vector<unsigned char> st = as_bytes(salt);
  int nblocks = (dklen + 31) / 32;
  std::vector<unsigned char> dk;
  dk.reserve(size_t(nblocks) * 32);
  for (int b = 1; b <= nblocks; ++b) {
    std::vector<unsigned char> msg = st;
    msg.push_back((b >> 24) & 0xff);
    msg.push_back((b >> 16) & 0xff);
    msg.push_back((b >> 8) & 0xff);
    msg.push_back(b & 0xff);
    unsigned char u[32], acc[32];
    hmac_sha256_buf(pw.data(), pw.size(), msg.data(), msg.size(), u);
    std::memcpy(acc, u, 32);
    for (int it = 1; it < iterations; ++it) {
      unsigned char u2[32];
      hmac_sha256_buf(pw.data(), pw.size(), u, 32, u2);
      std::memcpy(u, u2, 32);
      for (int i = 0; i < 32; ++i) acc[i] ^= u[i];
    }
    dk.insert(dk.end(), acc, acc + 32);
  }
  Rcpp::RawVector r(dklen);
  std::memcpy(&r[0], dk.data(), size_t(dklen));
  return r;
}

// ---------------------------------------------------------------
// BLAKE2b (RFC 7693). Needed by the Argon2 module in secarg_native.R,
// which called a blake2b() that existed in neither R tree, so every
// entry point in that file was dead. Implemented here beside SHA-256
// rather than in R because the compression function is 64-bit
// rotate-heavy and R has no unsigned 64-bit integer type.
//
// Sources: Saarinen, M-J. O. and Aumasson, J-P. (2015) "The BLAKE2
// Cryptographic Hash and Message Authentication Code (MAC)", RFC 7693,
// doi:10.17487/RFC7693 -- sections 2.1 (IV), 2.7 (SIGMA), 3.1 (G),
// 3.2 (F) and 3.3 (the parameter block and final-block flag).
// ---------------------------------------------------------------

namespace {

const uint64_t B2B_IV[8] = {
  0x6a09e667f3bcc908ULL, 0xbb67ae8584caa73bULL,
  0x3c6ef372fe94f82bULL, 0xa54ff53a5f1d36f1ULL,
  0x510e527fade682d1ULL, 0x9b05688c2b3e6c1fULL,
  0x1f83d9abfb41bd6bULL, 0x5be0cd19137e2179ULL
};

const unsigned char B2B_SIGMA[12][16] = {
  { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15},
  {14,10, 4, 8, 9,15,13, 6, 1,12, 0, 2,11, 7, 5, 3},
  {11, 8,12, 0, 5, 2,15,13,10,14, 3, 6, 7, 1, 9, 4},
  { 7, 9, 3, 1,13,12,11,14, 2, 6, 5,10, 4, 0,15, 8},
  { 9, 0, 5, 7, 2, 4,10,15,14, 1,11,12, 6, 8, 3,13},
  { 2,12, 6,10, 0,11, 8, 3, 4,13, 7, 5,15,14, 1, 9},
  {12, 5, 1,15,14,13, 4,10, 0, 7, 6, 3, 9, 2, 8,11},
  {13,11, 7,14,12, 1, 3, 9, 5, 0,15, 4, 8, 6, 2,10},
  { 6,15,14, 9,11, 3, 0, 8,12, 2,13, 7, 1, 4,10, 5},
  {10, 2, 8, 4, 7, 6, 1, 5,15,11, 9,14, 3,12,13, 0},
  { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15},
  {14,10, 4, 8, 9,15,13, 6, 1,12, 0, 2,11, 7, 5, 3}
};

inline uint64_t b2b_rotr64(uint64_t x, int n) {
  return (x >> n) | (x << (64 - n));
}

inline void b2b_g(uint64_t v[16], int a, int b, int c, int d,
                  uint64_t x, uint64_t y) {
  v[a] = v[a] + v[b] + x;
  v[d] = b2b_rotr64(v[d] ^ v[a], 32);
  v[c] = v[c] + v[d];
  v[b] = b2b_rotr64(v[b] ^ v[c], 24);
  v[a] = v[a] + v[b] + y;
  v[d] = b2b_rotr64(v[d] ^ v[a], 16);
  v[c] = v[c] + v[d];
  v[b] = b2b_rotr64(v[b] ^ v[c], 63);
}

struct Blake2b {
  uint64_t h[8];
  uint64_t t[2];
  unsigned char buf[128];
  size_t buflen;
  size_t outlen;

  void init(size_t outlen_, const unsigned char *key, size_t keylen) {
    outlen = outlen_;
    for (int i = 0; i < 8; ++i) h[i] = B2B_IV[i];
    // parameter block: digest length, key length, fanout 1, depth 1
    h[0] ^= 0x01010000ULL ^ (uint64_t(keylen) << 8) ^ uint64_t(outlen);
    t[0] = t[1] = 0;
    buflen = 0;
    std::memset(buf, 0, sizeof(buf));
    if (keylen > 0) {
      unsigned char block[128];
      std::memset(block, 0, sizeof(block));
      std::memcpy(block, key, keylen);
      update(block, 128);
    }
  }

  void compress(const unsigned char *block, bool last) {
    uint64_t m[16], v[16];
    for (int i = 0; i < 16; ++i) {
      uint64_t w = 0;
      for (int b = 7; b >= 0; --b) w = (w << 8) | block[i * 8 + b];
      m[i] = w;
    }
    for (int i = 0; i < 8; ++i) v[i] = h[i];
    for (int i = 0; i < 8; ++i) v[i + 8] = B2B_IV[i];
    v[12] ^= t[0];
    v[13] ^= t[1];
    if (last) v[14] = ~v[14];
    for (int r = 0; r < 12; ++r) {
      const unsigned char *s = B2B_SIGMA[r];
      b2b_g(v, 0, 4,  8, 12, m[s[0]],  m[s[1]]);
      b2b_g(v, 1, 5,  9, 13, m[s[2]],  m[s[3]]);
      b2b_g(v, 2, 6, 10, 14, m[s[4]],  m[s[5]]);
      b2b_g(v, 3, 7, 11, 15, m[s[6]],  m[s[7]]);
      b2b_g(v, 0, 5, 10, 15, m[s[8]],  m[s[9]]);
      b2b_g(v, 1, 6, 11, 12, m[s[10]], m[s[11]]);
      b2b_g(v, 2, 7,  8, 13, m[s[12]], m[s[13]]);
      b2b_g(v, 3, 4,  9, 14, m[s[14]], m[s[15]]);
    }
    for (int i = 0; i < 8; ++i) h[i] ^= v[i] ^ v[i + 8];
  }

  void update(const unsigned char *in, size_t inlen) {
    while (inlen > 0) {
      if (buflen == 128) {
        t[0] += 128;
        if (t[0] < 128) t[1] += 1;
        compress(buf, false);
        buflen = 0;
      }
      size_t take = 128 - buflen;
      if (take > inlen) take = inlen;
      std::memcpy(buf + buflen, in, take);
      buflen += take;
      in += take;
      inlen -= take;
    }
  }

  void final(unsigned char *out) {
    t[0] += buflen;
    if (t[0] < buflen) t[1] += 1;
    std::memset(buf + buflen, 0, 128 - buflen);
    compress(buf, true);
    for (size_t i = 0; i < outlen; ++i)
      out[i] = (unsigned char)((h[i / 8] >> (8 * (i % 8))) & 0xff);
  }
};

void blake2b_buf(const unsigned char *in, size_t inlen,
                 const unsigned char *key, size_t keylen,
                 unsigned char *out, size_t outlen) {
  Blake2b s;
  s.init(outlen, key, keylen);
  s.update(in, inlen);
  s.final(out);
}

}  // namespace

// [[Rcpp::export(name = ".morie_blake2b_impl")]]
Rcpp::RawVector morie_crypto_blake2b_native(SEXP data, int outlen,
                                            SEXP key = R_NilValue) {
  if (outlen < 1 || outlen > 64)
    Rcpp::stop("blake2b: the digest length must be in 1..64, got %d",
               outlen);
  std::vector<unsigned char> in = as_bytes(data);
  std::vector<unsigned char> k;
  if (key != R_NilValue) k = as_bytes(key);
  if (k.size() > 64) Rcpp::stop("blake2b: the key may be at most 64 bytes");
  std::vector<unsigned char> out(static_cast<size_t>(outlen), 0);
  blake2b_buf(in.data(), in.size(), k.empty() ? NULL : k.data(),
              k.size(), out.data(), size_t(outlen));
  Rcpp::RawVector r(outlen);
  std::memcpy(&r[0], out.data(), size_t(outlen));
  return r;
}

// ---------------------------------------------------------------
// Argon2 (RFC 9106), the core that R cannot host.
//
// The R arm tried to do this with bitwAnd/bitwShiftR, which are
// 32-bit-integer operations: .MASK64 is a double, so bitwAnd(va,
// .MASK64) is a type error and the whole compression function was
// dead. R has no unsigned 64-bit type, and the indexing needs an
// exact (J1*J1) >> 32 on a 32-bit J1, which overflows a double's 53
// bits of mantissa. So the block arithmetic lives here, beside
// BLAKE2b, and the R side is a wrapper.
//
// Source: Biryukov, A., Dinu, D., Khovratovich, D. and Josefsson, S.
// (2021) "Argon2 Memory-Hard Function for Password Hashing and
// Proof-of-Work Applications", RFC 9106, doi:10.17487/RFC9106 --
// section 3.2 (the filling rules and the reference-index mapping),
// 3.3 (H'), 3.4 (the variants) and 3.5 (G and the permutation P).
// ---------------------------------------------------------------

namespace {

const size_t A2_BLOCK = 1024;
const int A2_SL = 4;

void a2_le32(uint32_t v, unsigned char *out) {
  out[0] = (unsigned char)(v & 0xff);
  out[1] = (unsigned char)((v >> 8) & 0xff);
  out[2] = (unsigned char)((v >> 16) & 0xff);
  out[3] = (unsigned char)((v >> 24) & 0xff);
}

void a2_push32(std::vector<unsigned char> &b, uint32_t v) {
  unsigned char t[4];
  a2_le32(v, t);
  b.insert(b.end(), t, t + 4);
}

// H': BLAKE2b stretched past 64 bytes by chaining and keeping the
// FIRST 32 bytes of each link, which is what stops it repeating.
std::vector<unsigned char> a2_variable_hash(const unsigned char *a,
                                            size_t alen, size_t T) {
  if (T < 1) Rcpp::stop("secarg: the output length must be positive");
  std::vector<unsigned char> in;
  a2_push32(in, (uint32_t)T);
  in.insert(in.end(), a, a + alen);
  if (T <= 64) {
    std::vector<unsigned char> out(T, 0);
    blake2b_buf(in.data(), in.size(), NULL, 0, out.data(), T);
    return out;
  }
  size_t r = ((T + 31) / 32) - 2;
  std::vector<unsigned char> out;
  unsigned char v[64];
  blake2b_buf(in.data(), in.size(), NULL, 0, v, 64);
  out.insert(out.end(), v, v + 32);
  for (size_t i = 1; i < r; ++i) {
    unsigned char v2[64];
    blake2b_buf(v, 64, NULL, 0, v2, 64);
    std::memcpy(v, v2, 64);
    out.insert(out.end(), v, v + 32);
  }
  size_t last = T - 32 * r;
  std::vector<unsigned char> tail(last, 0);
  blake2b_buf(v, 64, NULL, 0, tail.data(), last);
  out.insert(out.end(), tail.begin(), tail.end());
  out.resize(T);
  return out;
}

inline void a2_gb(uint64_t *v, int a, int b, int c, int d) {
  const uint64_t M32 = 0xffffffffULL;
  v[a] = v[a] + v[b] + 2ULL * (v[a] & M32) * (v[b] & M32);
  v[d] = b2b_rotr64(v[d] ^ v[a], 32);
  v[c] = v[c] + v[d] + 2ULL * (v[c] & M32) * (v[d] & M32);
  v[b] = b2b_rotr64(v[b] ^ v[c], 24);
  v[a] = v[a] + v[b] + 2ULL * (v[a] & M32) * (v[b] & M32);
  v[d] = b2b_rotr64(v[d] ^ v[a], 16);
  v[c] = v[c] + v[d] + 2ULL * (v[c] & M32) * (v[d] & M32);
  v[b] = b2b_rotr64(v[b] ^ v[c], 63);
}

inline void a2_P(uint64_t *v) {
  a2_gb(v, 0, 4,  8, 12);
  a2_gb(v, 1, 5,  9, 13);
  a2_gb(v, 2, 6, 10, 14);
  a2_gb(v, 3, 7, 11, 15);
  a2_gb(v, 0, 5, 10, 15);
  a2_gb(v, 1, 6, 11, 12);
  a2_gb(v, 2, 7,  8, 13);
  a2_gb(v, 3, 4,  9, 14);
}

// G(X, Y): rows, then COLUMNS, then XOR back. Rows alone would leave
// each 128-byte strip independent; the column pass diffuses across
// the whole 1024-byte block.
void a2_compress(const uint64_t *X, const uint64_t *Y, uint64_t *out) {
  uint64_t R[128], Q[128], t[16];
  for (int i = 0; i < 128; ++i) { R[i] = X[i] ^ Y[i]; Q[i] = R[i]; }
  for (int i = 0; i < 8; ++i) {
    for (int k = 0; k < 16; ++k) t[k] = Q[16 * i + k];
    a2_P(t);
    for (int k = 0; k < 16; ++k) Q[16 * i + k] = t[k];
  }
  for (int j = 0; j < 8; ++j) {
    int idx[16];
    for (int i = 0; i < 8; ++i) {
      idx[2 * i] = 16 * i + 2 * j;
      idx[2 * i + 1] = 16 * i + 2 * j + 1;
    }
    for (int k = 0; k < 16; ++k) t[k] = Q[idx[k]];
    a2_P(t);
    for (int k = 0; k < 16; ++k) Q[idx[k]] = t[k];
  }
  for (int i = 0; i < 128; ++i) out[i] = Q[i] ^ R[i];
}

void a2_addresses(uint64_t pass_no, uint64_t lane, uint64_t slice_no,
                  uint64_t m_prime, uint64_t passes, uint64_t y,
                  uint64_t counter, uint64_t *out) {
  uint64_t zero[128], inp[128], mid[128];
  for (int i = 0; i < 128; ++i) { zero[i] = 0; inp[i] = 0; }
  inp[0] = pass_no; inp[1] = lane; inp[2] = slice_no;
  inp[3] = m_prime; inp[4] = passes; inp[5] = y; inp[6] = counter;
  a2_compress(zero, inp, mid);
  a2_compress(zero, mid, out);
}

void a2_to_words(const unsigned char *bs, size_t n, uint64_t *out) {
  for (size_t i = 0; i < n / 8; ++i) {
    uint64_t w = 0;
    for (int b = 7; b >= 0; --b) w = (w << 8) | bs[i * 8 + b];
    out[i] = w;
  }
}

void a2_to_bytes(const uint64_t *ws, size_t n, unsigned char *out) {
  for (size_t i = 0; i < n; ++i)
    for (int b = 0; b < 8; ++b)
      out[i * 8 + b] = (unsigned char)((ws[i] >> (8 * b)) & 0xff);
}

}  // namespace

// [[Rcpp::export(name = ".morie_argon2_impl")]]
Rcpp::RawVector morie_crypto_argon2_native(SEXP password, SEXP salt,
                                           int memory, int passes,
                                           int parallelism,
                                           int tag_length,
                                           std::string variant,
                                           SEXP secret = R_NilValue,
                                           SEXP associated = R_NilValue) {
  uint32_t y;
  if (variant == "argon2d") y = 0;
  else if (variant == "argon2i") y = 1;
  else if (variant == "argon2id") y = 2;
  else Rcpp::stop("secarg: variant must be one of argon2d, argon2i, "
                  "argon2id, got %s", variant);
  const uint32_t VERSION = 0x13;
  int p = parallelism, t = passes, m = memory;
  if (p < 1) Rcpp::stop("secarg: parallelism must be at least 1");
  if (t < 1) Rcpp::stop("secarg: at least one pass is required");
  if (m < 8 * p)
    Rcpp::stop("secarg: memory must be at least 8*p = %d KiB, got %d",
               8 * p, m);
  if (tag_length < 4) Rcpp::stop("secarg: the tag must be at least 4 bytes");

  std::vector<unsigned char> P = as_bytes(password);
  std::vector<unsigned char> S = as_bytes(salt);
  std::vector<unsigned char> K, X;
  if (secret != R_NilValue) K = as_bytes(secret);
  if (associated != R_NilValue) X = as_bytes(associated);
  if (S.size() < 8)
    Rcpp::stop("secarg: the salt must be at least 8 bytes (the RFC "
               "recommends 16), got %d", (int)S.size());

  // H0 over EVERY parameter, so a tag cannot be compared silently
  // across configurations.
  std::vector<unsigned char> buf;
  a2_push32(buf, (uint32_t)p);
  a2_push32(buf, (uint32_t)tag_length);
  a2_push32(buf, (uint32_t)m);
  a2_push32(buf, (uint32_t)t);
  a2_push32(buf, VERSION);
  a2_push32(buf, y);
  a2_push32(buf, (uint32_t)P.size()); buf.insert(buf.end(), P.begin(), P.end());
  a2_push32(buf, (uint32_t)S.size()); buf.insert(buf.end(), S.begin(), S.end());
  a2_push32(buf, (uint32_t)K.size()); buf.insert(buf.end(), K.begin(), K.end());
  a2_push32(buf, (uint32_t)X.size()); buf.insert(buf.end(), X.begin(), X.end());
  unsigned char H0[64];
  blake2b_buf(buf.data(), buf.size(), NULL, 0, H0, 64);

  int m_prime = (m / (A2_SL * p)) * (A2_SL * p);
  int q = m_prime / p;
  int seg = q / A2_SL;

  std::vector<std::vector<uint64_t> > B((size_t)p);
  for (int i = 0; i < p; ++i) B[i].assign((size_t)q * 128, 0);

  for (int i = 0; i < p; ++i) {
    for (int col = 0; col < 2; ++col) {
      std::vector<unsigned char> in(H0, H0 + 64);
      a2_push32(in, (uint32_t)col);
      a2_push32(in, (uint32_t)i);
      std::vector<unsigned char> blk =
          a2_variable_hash(in.data(), in.size(), A2_BLOCK);
      a2_to_words(blk.data(), A2_BLOCK, &B[i][(size_t)col * 128]);
    }
  }

  std::vector<uint64_t> addr(128, 0), tmp(128, 0);
  for (int r = 0; r < t; ++r) {
    for (int sl = 0; sl < A2_SL; ++sl) {
      for (int i = 0; i < p; ++i) {
        bool data_indep = (y == 1) || (y == 2 && r == 0 && sl < 2);
        uint64_t counter = 0;
        int start = 0;
        if (r == 0 && sl == 0) {
          start = 2;
          if (data_indep) {
            counter += 1;
            a2_addresses(r, i, sl, m_prime, t, y, counter, addr.data());
          }
        }
        for (int idx = start; idx < seg; ++idx) {
          if (data_indep && idx % 128 == 0) {
            counter += 1;
            a2_addresses(r, i, sl, m_prime, t, y, counter, addr.data());
          }
          int j = sl * seg + idx;
          const uint64_t *prev =
              (j > 0) ? &B[i][(size_t)(j - 1) * 128]
                      : &B[i][(size_t)(q - 1) * 128];
          uint64_t pr = data_indep ? addr[idx % 128] : prev[0];
          uint64_t J1 = pr & 0xffffffffULL;
          uint64_t J2 = (pr >> 32) & 0xffffffffULL;
          int lane = (r == 0 && sl == 0) ? i : (int)(J2 % (uint64_t)p);
          int W;
          if (r == 0) {
            if (sl == 0 || lane == i) W = j - 1;
            else W = sl * seg - (idx == 0 ? 1 : 0);
          } else {
            if (lane == i) W = q - seg + idx - 1;
            else W = q - seg - (idx == 0 ? 1 : 0);
          }
          if (W < 1) W = 1;
          // exact 64-bit: a double loses this above 2^53
          uint64_t x = (J1 * J1) >> 32;
          uint64_t yy = ((uint64_t)W * x) >> 32;
          uint64_t zz = (uint64_t)W - 1 - yy;
          int startpos = (r == 0) ? 0 : (((sl + 1) % A2_SL) * seg);
          int ref = (int)(((uint64_t)startpos + zz) % (uint64_t)q);
          a2_compress(prev, &B[lane][(size_t)ref * 128], tmp.data());
          uint64_t *dst = &B[i][(size_t)j * 128];
          if (r == 0) std::memcpy(dst, tmp.data(), 128 * sizeof(uint64_t));
          else for (int k = 0; k < 128; ++k) dst[k] = tmp[k] ^ dst[k];
        }
      }
    }
  }

  std::vector<uint64_t> C(128, 0);
  std::memcpy(C.data(), &B[0][(size_t)(q - 1) * 128], 128 * sizeof(uint64_t));
  for (int i = 1; i < p; ++i)
    for (int k = 0; k < 128; ++k) C[k] ^= B[i][(size_t)(q - 1) * 128 + k];

  std::vector<unsigned char> cb(A2_BLOCK, 0);
  a2_to_bytes(C.data(), 128, cb.data());
  std::vector<unsigned char> tag =
      a2_variable_hash(cb.data(), cb.size(), (size_t)tag_length);
  Rcpp::RawVector out(tag_length);
  std::memcpy(&out[0], tag.data(), (size_t)tag_length);
  return out;
}
