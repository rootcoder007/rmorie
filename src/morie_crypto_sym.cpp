// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Phase 3JJJ1: libsodium-backed symmetric crypto for morie.
//
// Mirrors morie.crypto._chacha + morie.crypto._kdf (pure-Python
// reference) with libsodium's audited C implementation. Byte-compat
// with the Python morie ChaCha20-Poly1305-IETF + HKDF-SHA256 (RFC
// 5869) so the on-disk Python keystore / hybrid envelope formats
// stay interoperable.
//
// Gated on MORIE_HAVE_SODIUM (defined by ./configure when libsodium
// is found via pkg-config or bare-link probe). When absent, all
// exported functions return a clear error so install still succeeds
// and the rest of morie remains usable.

#include <Rcpp.h>
#include <vector>
#include <cstring>

#ifdef MORIE_HAVE_SODIUM
#include <sodium.h>
#endif

using namespace Rcpp;

#ifdef MORIE_HAVE_SODIUM
// Lazy global init; libsodium's sodium_init() is idempotent + thread-safe.
static bool morie_sodium_ready() {
  static bool inited = false;
  if (!inited) {
    if (sodium_init() < 0) {
      Rcpp::stop("libsodium sodium_init() failed");
    }
    inited = true;
  }
  return true;
}
#endif

// [[Rcpp::export]]
bool morie_crypto_sodium_available() {
#ifdef MORIE_HAVE_SODIUM
  return true;
#else
  return false;
#endif
}

// [[Rcpp::export]]
std::string morie_crypto_sodium_version() {
#ifdef MORIE_HAVE_SODIUM
  morie_sodium_ready();
  return std::string(sodium_version_string());
#else
  return std::string("");
#endif
}

// ChaCha20-Poly1305 IETF (RFC 8439) -- AEAD encrypt.
// Returns a raw vector: ciphertext || 16-byte tag (libsodium convention).
// The Python morie API splits these; the R wrapper handles the split.
// [[Rcpp::export]]
SEXP morie_crypto_chacha20poly1305_encrypt(
    SEXP key_sxp, SEXP nonce_sxp, SEXP plaintext_sxp, SEXP aad_sxp) {
#ifdef MORIE_HAVE_SODIUM
  morie_sodium_ready();
  RawVector key(key_sxp);
  RawVector nonce(nonce_sxp);
  RawVector plaintext(plaintext_sxp);
  RawVector aad(aad_sxp);
  if (key.size() != crypto_aead_chacha20poly1305_IETF_KEYBYTES)
    Rcpp::stop("key must be %d bytes (got %d)",
               crypto_aead_chacha20poly1305_IETF_KEYBYTES,
               (int) key.size());
  if (nonce.size() != crypto_aead_chacha20poly1305_IETF_NPUBBYTES)
    Rcpp::stop("nonce must be %d bytes (got %d)",
               crypto_aead_chacha20poly1305_IETF_NPUBBYTES,
               (int) nonce.size());
  std::vector<unsigned char> ct(plaintext.size() +
                                  crypto_aead_chacha20poly1305_IETF_ABYTES);
  unsigned long long ct_len = 0;
  const unsigned char* aad_ptr = (aad.size() > 0) ? &aad[0] : nullptr;
  int rc = crypto_aead_chacha20poly1305_ietf_encrypt(
      ct.data(), &ct_len,
      (plaintext.size() > 0) ? &plaintext[0] : nullptr,
      plaintext.size(),
      aad_ptr, aad.size(),
      nullptr,  // nsec
      &nonce[0], &key[0]);
  if (rc != 0)
    Rcpp::stop("crypto_aead_chacha20poly1305_ietf_encrypt failed");
  ct.resize(ct_len);
  RawVector out(ct_len);
  std::memcpy(&out[0], ct.data(), ct_len);
  return out;
#else
  Rcpp::stop("morie was built without libsodium; reinstall with "
             "libsodium-dev / brew install libsodium and rebuild morie.");
  return R_NilValue;
#endif
}

// [[Rcpp::export]]
SEXP morie_crypto_chacha20poly1305_decrypt(
    SEXP key_sxp, SEXP nonce_sxp, SEXP ct_with_tag_sxp, SEXP aad_sxp) {
#ifdef MORIE_HAVE_SODIUM
  morie_sodium_ready();
  RawVector key(key_sxp);
  RawVector nonce(nonce_sxp);
  RawVector ctwt(ct_with_tag_sxp);
  RawVector aad(aad_sxp);
  if (key.size() != crypto_aead_chacha20poly1305_IETF_KEYBYTES)
    Rcpp::stop("key must be %d bytes",
               crypto_aead_chacha20poly1305_IETF_KEYBYTES);
  if (nonce.size() != crypto_aead_chacha20poly1305_IETF_NPUBBYTES)
    Rcpp::stop("nonce must be %d bytes",
               crypto_aead_chacha20poly1305_IETF_NPUBBYTES);
  if ((std::size_t) ctwt.size() < crypto_aead_chacha20poly1305_IETF_ABYTES)
    Rcpp::stop("ciphertext+tag too short (need >= %d bytes)",
               crypto_aead_chacha20poly1305_IETF_ABYTES);
  std::vector<unsigned char> pt(ctwt.size());
  unsigned long long pt_len = 0;
  const unsigned char* aad_ptr = (aad.size() > 0) ? &aad[0] : nullptr;
  int rc = crypto_aead_chacha20poly1305_ietf_decrypt(
      pt.data(), &pt_len,
      nullptr,  // nsec
      &ctwt[0], ctwt.size(),
      aad_ptr, aad.size(),
      &nonce[0], &key[0]);
  if (rc != 0)
    Rcpp::stop("ChaCha20-Poly1305 decrypt failed: bad tag, key, or nonce");
  pt.resize(pt_len);
  RawVector out(pt_len);
  std::memcpy(&out[0], pt.data(), pt_len);
  return out;
#else
  Rcpp::stop("morie was built without libsodium");
  return R_NilValue;
#endif
}

// HKDF-SHA256 (RFC 5869).
// Mirrors the Python hkdf_sha256(ikm, length=32, salt=b"", info=b"")
// signature. Empty salt -> zero-filled 32-byte salt (RFC 5869 §2.2 spec
// + Python morie code path match).
// [[Rcpp::export]]
SEXP morie_crypto_hkdf_sha256(
    SEXP ikm_sxp, SEXP length_sxp, SEXP salt_sxp, SEXP info_sxp) {
#ifdef MORIE_HAVE_SODIUM
  morie_sodium_ready();
  RawVector ikm(ikm_sxp);
  int length = Rcpp::as<int>(length_sxp);
  RawVector salt(salt_sxp);
  RawVector info(info_sxp);
  const int HASH_LEN = 32;
  if (length <= 0 || length > 255 * HASH_LEN)
    Rcpp::stop("length must be in 1..%d (got %d)", 255 * HASH_LEN, length);
  // libsodium's crypto_kdf_hkdf_sha256_* arrived in v1.0.20. Fall back
  // to the two-call extract+expand pattern using HMAC-SHA256 directly
  // via libsodium's crypto_auth_hmacsha256, which has been available
  // since v0.4.x and is guaranteed present everywhere.
  unsigned char prk[crypto_auth_hmacsha256_BYTES];
  std::vector<unsigned char> effective_salt;
  const unsigned char* salt_ptr;
  std::size_t salt_len;
  if (salt.size() == 0) {
    effective_salt.assign(HASH_LEN, 0);
    salt_ptr = effective_salt.data();
    salt_len = HASH_LEN;
  } else {
    salt_ptr = &salt[0];
    salt_len = salt.size();
  }
  // Extract: PRK = HMAC-SHA256(salt, IKM)
  crypto_auth_hmacsha256_state st;
  crypto_auth_hmacsha256_init(&st, salt_ptr, salt_len);
  if (ikm.size() > 0)
    crypto_auth_hmacsha256_update(&st, &ikm[0], ikm.size());
  crypto_auth_hmacsha256_final(&st, prk);
  // Expand: T(i) = HMAC-SHA256(PRK, T(i-1) || info || i)
  std::vector<unsigned char> okm(length);
  unsigned char t[crypto_auth_hmacsha256_BYTES];
  std::size_t t_len = 0;
  int n = (length + HASH_LEN - 1) / HASH_LEN;
  for (int i = 1; i <= n; ++i) {
    crypto_auth_hmacsha256_init(&st, prk, sizeof(prk));
    if (t_len > 0)
      crypto_auth_hmacsha256_update(&st, t, t_len);
    if (info.size() > 0)
      crypto_auth_hmacsha256_update(&st, &info[0], info.size());
    unsigned char counter = (unsigned char) i;
    crypto_auth_hmacsha256_update(&st, &counter, 1);
    crypto_auth_hmacsha256_final(&st, t);
    t_len = HASH_LEN;
    int copy = std::min(HASH_LEN, length - (i - 1) * HASH_LEN);
    std::memcpy(okm.data() + (i - 1) * HASH_LEN, t, copy);
  }
  RawVector out(length);
  std::memcpy(&out[0], okm.data(), length);
  return out;
#else
  Rcpp::stop("morie was built without libsodium");
  return R_NilValue;
#endif
}

// Cryptographically secure random bytes (libsodium randombytes_buf).
// [[Rcpp::export]]
SEXP morie_crypto_random_bytes(int n) {
#ifdef MORIE_HAVE_SODIUM
  morie_sodium_ready();
  if (n <= 0) Rcpp::stop("n must be positive");
  RawVector out(n);
  randombytes_buf(&out[0], n);
  return out;
#else
  Rcpp::stop("morie was built without libsodium");
  return R_NilValue;
#endif
}
