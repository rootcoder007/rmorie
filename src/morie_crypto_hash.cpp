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
