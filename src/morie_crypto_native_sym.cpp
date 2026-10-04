// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The symmetric layer with nothing outside this file, for builds without
// libsodium and for the key store (R/crypto_keystore.R): ChaCha20-Poly1305
// (RFC 8439, the cipher of morie's Python crypto, so either arm opens what the
// other sealed), scrypt's ROMix (RFC 7914; its two PBKDF2 steps are
// rmoriebricklayer's) and, to open key stores written by rmorie 1.3.x through
// the sodium package, XSalsa20-Poly1305 (libsodium's crypto_secretbox_easy).

#include <Rcpp.h>
#include <cstdint>
#include <cstring>
#include <vector>

namespace {

inline uint32_t rotl32(uint32_t x, int k) { return (x << k) | (x >> (32 - k)); }
inline uint32_t ld32(const unsigned char* p) {
  return (uint32_t)p[0] | (uint32_t)p[1] << 8 | (uint32_t)p[2] << 16 | (uint32_t)p[3] << 24;
}
inline void st32(unsigned char* p, uint32_t v) {
  p[0] = (unsigned char)v; p[1] = (unsigned char)(v >> 8);
  p[2] = (unsigned char)(v >> 16); p[3] = (unsigned char)(v >> 24);
}

// ---- Salsa20 (scrypt's Salsa20/8, XSalsa20) --------------------------------
void salsa_rounds(uint32_t x[16], int rounds) {
#define SQR(a, b, c, d) \
  x[b] ^= rotl32(x[a] + x[d], 7); x[c] ^= rotl32(x[b] + x[a], 9); \
  x[d] ^= rotl32(x[c] + x[b], 13); x[a] ^= rotl32(x[d] + x[c], 18);
  for (int i = 0; i < rounds; i += 2) {
    SQR(0, 4, 8, 12) SQR(5, 9, 13, 1) SQR(10, 14, 2, 6) SQR(15, 3, 7, 11)
    SQR(0, 1, 2, 3) SQR(5, 6, 7, 4) SQR(10, 11, 8, 9) SQR(15, 12, 13, 14)
  }
#undef SQR
}

void salsa_core(uint32_t b[16], int rounds) {
  uint32_t x[16];
  std::memcpy(x, b, sizeof x);
  salsa_rounds(x, rounds);
  for (int i = 0; i < 16; ++i) b[i] += x[i];
}

// scrypt BlockMix with Salsa20/8 over 2r 64-byte blocks: Y = even blocks, then odd
void block_mix(const uint32_t* B, uint32_t* Y, int r) {
  uint32_t X[16];
  std::memcpy(X, &B[(2 * r - 1) * 16], sizeof X);
  for (int i = 0; i < 2 * r; ++i) {
    for (int k = 0; k < 16; ++k) X[k] ^= B[i * 16 + k];
    salsa_core(X, 8);
    std::memcpy(&Y[((i & 1) * r + i / 2) * 16], X, sizeof X);
  }
}

void ro_mix(unsigned char* b, int N, int r) {
  const size_t w = 32 * (size_t)r;
  std::vector<uint32_t> X(w), Y(w), V(w * (size_t)N);
  for (size_t k = 0; k < w; ++k) X[k] = ld32(b + 4 * k);
  for (int i = 0; i < N; ++i) {
    std::memcpy(&V[w * i], X.data(), 4 * w);
    block_mix(X.data(), Y.data(), r);
    X.swap(Y);
  }
  for (int i = 0; i < N; ++i) {
    uint32_t j = X[(2 * r - 1) * 16] & (uint32_t)(N - 1);  // Integerify mod N (N a power of 2)
    for (size_t k = 0; k < w; ++k) X[k] ^= V[w * j + k];
    block_mix(X.data(), Y.data(), r);
    X.swap(Y);
  }
  for (size_t k = 0; k < w; ++k) st32(b + 4 * k, X[k]);
}

const uint32_t SIGMA[4] = {0x61707865u, 0x3320646eu, 0x79622d32u, 0x6b206574u};

// the Salsa20 state: constants on the diagonal, key at 1-4 and 11-14, 16 input bytes at 6-9
void salsa_state(uint32_t x[16], const unsigned char* key, const unsigned char* in16) {
  x[0] = SIGMA[0]; x[5] = SIGMA[1]; x[10] = SIGMA[2]; x[15] = SIGMA[3];
  for (int i = 0; i < 4; ++i) {
    x[1 + i] = ld32(key + 4 * i);
    x[11 + i] = ld32(key + 16 + 4 * i);
    x[6 + i] = ld32(in16 + 4 * i);
  }
}

// XSalsa20 keystream: HSalsa20 subkey from the first 16 nonce bytes, then Salsa20/20
void xsalsa20_stream(unsigned char* out, size_t n, const unsigned char* key,
                     const unsigned char* nonce24) {
  uint32_t x[16];
  salsa_state(x, key, nonce24);
  salsa_rounds(x, 20);
  unsigned char sub[32];
  const int pick[8] = {0, 5, 10, 15, 6, 7, 8, 9};
  for (int i = 0; i < 8; ++i) st32(sub + 4 * i, x[pick[i]]);
  unsigned char in16[16] = {0};
  std::memcpy(in16, nonce24 + 16, 8);
  for (uint64_t ctr = 0, off = 0; off < n; ++ctr, off += 64) {
    for (int i = 0; i < 8; ++i) in16[8 + i] = (unsigned char)(ctr >> (8 * i));
    uint32_t s[16];
    salsa_state(s, sub, in16);
    salsa_core(s, 20);
    unsigned char blk[64];
    for (int i = 0; i < 16; ++i) st32(blk + 4 * i, s[i]);
    for (size_t i = 0; i < 64 && off + i < n; ++i) out[off + i] = blk[i];
  }
}

// ---- ChaCha20 (RFC 8439) ----------------------------------------------------
void chacha_block(unsigned char out[64], const unsigned char* key, uint32_t ctr,
                  const unsigned char* nonce12) {
  uint32_t s[16], x[16];
  for (int i = 0; i < 4; ++i) s[i] = SIGMA[i];
  for (int i = 0; i < 8; ++i) s[4 + i] = ld32(key + 4 * i);
  s[12] = ctr;
  for (int i = 0; i < 3; ++i) s[13 + i] = ld32(nonce12 + 4 * i);
  std::memcpy(x, s, sizeof x);
#define CQR(a, b, c, d) \
  x[a] += x[b]; x[d] = rotl32(x[d] ^ x[a], 16); x[c] += x[d]; x[b] = rotl32(x[b] ^ x[c], 12); \
  x[a] += x[b]; x[d] = rotl32(x[d] ^ x[a], 8);  x[c] += x[d]; x[b] = rotl32(x[b] ^ x[c], 7);
  for (int i = 0; i < 10; ++i) {
    CQR(0, 4, 8, 12) CQR(1, 5, 9, 13) CQR(2, 6, 10, 14) CQR(3, 7, 11, 15)
    CQR(0, 5, 10, 15) CQR(1, 6, 11, 12) CQR(2, 7, 8, 13) CQR(3, 4, 9, 14)
  }
#undef CQR
  for (int i = 0; i < 16; ++i) st32(out + 4 * i, x[i] + s[i]);
}

void chacha_xor(unsigned char* buf, size_t n, const unsigned char* key, uint32_t ctr,
                const unsigned char* nonce12) {
  unsigned char ks[64];
  for (size_t off = 0; off < n; off += 64, ++ctr) {
    chacha_block(ks, key, ctr, nonce12);
    for (size_t i = 0; i < 64 && off + i < n; ++i) buf[off + i] ^= ks[i];
  }
}

// ---- Poly1305 (26-bit limbs) ------------------------------------------------
struct Poly1305 {
  uint32_t r[5], h[5] = {0, 0, 0, 0, 0}, pad[4];
  explicit Poly1305(const unsigned char key[32]) {
    r[0] = ld32(key) & 0x3ffffff;
    r[1] = (ld32(key + 3) >> 2) & 0x3ffff03;
    r[2] = (ld32(key + 6) >> 4) & 0x3ffc0ff;
    r[3] = (ld32(key + 9) >> 6) & 0x3f03fff;
    r[4] = (ld32(key + 12) >> 8) & 0x00fffff;
    for (int i = 0; i < 4; ++i) pad[i] = ld32(key + 16 + 4 * i);
  }
  void block(const unsigned char m[16], uint32_t hibit) {
    const uint32_t s1 = r[1] * 5, s2 = r[2] * 5, s3 = r[3] * 5, s4 = r[4] * 5;
    h[0] += ld32(m) & 0x3ffffff;
    h[1] += (ld32(m + 3) >> 2) & 0x3ffffff;
    h[2] += (ld32(m + 6) >> 4) & 0x3ffffff;
    h[3] += (ld32(m + 9) >> 6) & 0x3ffffff;
    h[4] += (ld32(m + 12) >> 8) | hibit;
    typedef uint64_t u;
    u d0 = (u)h[0] * r[0] + (u)h[1] * s4 + (u)h[2] * s3 + (u)h[3] * s2 + (u)h[4] * s1;
    u d1 = (u)h[0] * r[1] + (u)h[1] * r[0] + (u)h[2] * s4 + (u)h[3] * s3 + (u)h[4] * s2;
    u d2 = (u)h[0] * r[2] + (u)h[1] * r[1] + (u)h[2] * r[0] + (u)h[3] * s4 + (u)h[4] * s3;
    u d3 = (u)h[0] * r[3] + (u)h[1] * r[2] + (u)h[2] * r[1] + (u)h[3] * r[0] + (u)h[4] * s4;
    u d4 = (u)h[0] * r[4] + (u)h[1] * r[3] + (u)h[2] * r[2] + (u)h[3] * r[1] + (u)h[4] * r[0];
    uint32_t c;
    c = (uint32_t)(d0 >> 26); h[0] = (uint32_t)d0 & 0x3ffffff;
    d1 += c; c = (uint32_t)(d1 >> 26); h[1] = (uint32_t)d1 & 0x3ffffff;
    d2 += c; c = (uint32_t)(d2 >> 26); h[2] = (uint32_t)d2 & 0x3ffffff;
    d3 += c; c = (uint32_t)(d3 >> 26); h[3] = (uint32_t)d3 & 0x3ffffff;
    d4 += c; c = (uint32_t)(d4 >> 26); h[4] = (uint32_t)d4 & 0x3ffffff;
    h[0] += c * 5; c = h[0] >> 26; h[0] &= 0x3ffffff; h[1] += c;
  }
  // the message zero-padded to 16-byte blocks (the AEAD layout), or, with
  // pad16 false, its last partial block closed by a 1 byte (plain Poly1305)
  void update(const unsigned char* m, size_t n, bool pad16) {
    for (; n >= 16; m += 16, n -= 16) block(m, 1u << 24);
    if (n) {
      unsigned char t[16] = {0};
      std::memcpy(t, m, n);
      if (pad16) {
        block(t, 1u << 24);
      } else {
        t[n] = 1;
        block(t, 0);
      }
    }
  }
  void finish(unsigned char tag[16]) {
    uint32_t c, g[5];
    c = h[1] >> 26; h[1] &= 0x3ffffff;
    h[2] += c; c = h[2] >> 26; h[2] &= 0x3ffffff;
    h[3] += c; c = h[3] >> 26; h[3] &= 0x3ffffff;
    h[4] += c; c = h[4] >> 26; h[4] &= 0x3ffffff;
    h[0] += c * 5; c = h[0] >> 26; h[0] &= 0x3ffffff;
    h[1] += c;
    g[0] = h[0] + 5; c = g[0] >> 26; g[0] &= 0x3ffffff;
    g[1] = h[1] + c; c = g[1] >> 26; g[1] &= 0x3ffffff;
    g[2] = h[2] + c; c = g[2] >> 26; g[2] &= 0x3ffffff;
    g[3] = h[3] + c; c = g[3] >> 26; g[3] &= 0x3ffffff;
    g[4] = h[4] + c - (1u << 26);
    uint32_t keep_g = (g[4] >> 31) - 1;  // h >= p: take g = h - p
    for (int i = 0; i < 5; ++i) h[i] = (h[i] & ~keep_g) | (g[i] & keep_g);
    uint32_t w0 = h[0] | (h[1] << 26), w1 = (h[1] >> 6) | (h[2] << 20),
             w2 = (h[2] >> 12) | (h[3] << 14), w3 = (h[3] >> 18) | (h[4] << 8);
    uint64_t f = (uint64_t)w0 + pad[0];
    st32(tag, (uint32_t)f);
    f = (uint64_t)w1 + pad[1] + (f >> 32); st32(tag + 4, (uint32_t)f);
    f = (uint64_t)w2 + pad[2] + (f >> 32); st32(tag + 8, (uint32_t)f);
    f = (uint64_t)w3 + pad[3] + (f >> 32); st32(tag + 12, (uint32_t)f);
  }
};

void aead_tag(unsigned char tag[16], const unsigned char* key, const unsigned char* nonce12,
              const unsigned char* aad, size_t na, const unsigned char* ct, size_t n) {
  unsigned char otk[64];
  chacha_block(otk, key, 0, nonce12);
  Poly1305 p(otk);
  p.update(aad, na, true);
  p.update(ct, n, true);
  unsigned char lens[16];
  for (int i = 0; i < 8; ++i) {
    lens[i] = (unsigned char)((uint64_t)na >> (8 * i));
    lens[8 + i] = (unsigned char)((uint64_t)n >> (8 * i));
  }
  p.update(lens, 16, true);
  p.finish(tag);
}

bool same16(const unsigned char* a, const unsigned char* b) {
  unsigned char d = 0;
  for (int i = 0; i < 16; ++i) d |= a[i] ^ b[i];
  return d == 0;
}

void need(const Rcpp::RawVector& x, R_xlen_t n, const char* what) {
  if (x.size() != n) Rcpp::stop("%s must be %d bytes", what, (int)n);
}

}  // namespace

// [[Rcpp::export(name = ".morie_scrypt_romix_impl")]]
Rcpp::RawVector morie_scrypt_romix(Rcpp::RawVector B, int N, int r, int p) {
  if (N < 2 || (N & (N - 1)) != 0 || N > (1 << 20)) Rcpp::stop("N must be a power of 2 in [2, 2^20]");
  if (r < 1 || r > 64 || p < 1 || p > 16) Rcpp::stop("r must be in [1, 64] and p in [1, 16]");
  need(B, 128 * (R_xlen_t)r * p, "B");
  Rcpp::RawVector out = Rcpp::clone(B);
  for (int i = 0; i < p; ++i) ro_mix(&out[128 * (R_xlen_t)r * i], N, r);
  return out;
}

// ciphertext || tag
// [[Rcpp::export(name = ".morie_aead_seal_impl")]]
Rcpp::RawVector morie_aead_seal(Rcpp::RawVector key, Rcpp::RawVector nonce, Rcpp::RawVector pt,
                                Rcpp::RawVector aad) {
  need(key, 32, "key");
  need(nonce, 12, "nonce");
  const size_t n = pt.size();
  Rcpp::RawVector out(n + 16);
  if (n) std::memcpy(&out[0], &pt[0], n);
  chacha_xor(&out[0], n, &key[0], 1, &nonce[0]);
  aead_tag(&out[n], &key[0], &nonce[0], aad.size() ? &aad[0] : &out[0], aad.size(), &out[0], n);
  return out;
}

// the plaintext, or NULL when the tag does not verify (wrong key, or a changed message)
// [[Rcpp::export(name = ".morie_aead_open_impl")]]
SEXP morie_aead_open(Rcpp::RawVector key, Rcpp::RawVector nonce, Rcpp::RawVector ct_with_tag,
                     Rcpp::RawVector aad) {
  need(key, 32, "key");
  need(nonce, 12, "nonce");
  if (ct_with_tag.size() < 16) Rcpp::stop("ciphertext+tag too short (need >= 16 bytes)");
  const size_t n = ct_with_tag.size() - 16;
  unsigned char want[16];
  aead_tag(want, &key[0], &nonce[0], aad.size() ? &aad[0] : want, aad.size(), &ct_with_tag[0], n);
  if (!same16(want, &ct_with_tag[n])) return R_NilValue;
  Rcpp::RawVector out(n);
  if (n) std::memcpy(&out[0], &ct_with_tag[0], n);
  chacha_xor(n ? &out[0] : want, n, &key[0], 1, &nonce[0]);
  return out;
}

// sodium::data_encrypt's box (tag || ciphertext, XSalsa20-Poly1305); NULL on a bad tag
// [[Rcpp::export(name = ".morie_ks_secretbox_open_impl")]]
SEXP morie_ks_secretbox_open(Rcpp::RawVector key, Rcpp::RawVector nonce, Rcpp::RawVector box) {
  need(key, 32, "key");
  need(nonce, 24, "nonce");
  if (box.size() < 16) return R_NilValue;
  const size_t n = box.size() - 16;
  std::vector<unsigned char> ks(32 + n);
  xsalsa20_stream(ks.data(), ks.size(), &key[0], &nonce[0]);
  Poly1305 p(ks.data());
  p.update(n ? &box[16] : ks.data(), n, false);
  unsigned char want[16];
  p.finish(want);
  if (!same16(want, &box[0])) return R_NilValue;
  Rcpp::RawVector out(n);
  for (size_t i = 0; i < n; ++i) out[i] = box[16 + i] ^ ks[32 + i];
  return out;
}
