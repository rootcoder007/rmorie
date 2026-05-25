// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Phase 3JJJ2: liboqs-backed post-quantum crypto for morie.
//
// Wraps the NIST-standardised ML-KEM-768 (FIPS 203) key encapsulation
// mechanism and ML-DSA-65 (FIPS 204) signature scheme via liboqs's
// audited C implementation. Mirrors morie.crypto._mlkem + _dilithium
// Python APIs byte-for-byte so the on-disk hybrid envelope format
// (3JJJ3) stays interoperable.
//
// Gated on MORIE_HAVE_LIBOQS (defined by ./configure when liboqs is
// found). When absent, all exported functions return a clear error
// so install still succeeds and the rest of morie remains usable.

#include <Rcpp.h>
#include <cstring>

#ifdef MORIE_HAVE_LIBOQS
#include <oqs/oqs.h>
#endif

using namespace Rcpp;

#ifdef MORIE_HAVE_LIBOQS
// Lazy global init. OQS_init() is idempotent. We never call
// OQS_destroy() because R's package unload semantics make ordering
// versus liboqs's internal OpenSSL teardown unreliable.
static void morie_oqs_ready() {
  static bool inited = false;
  if (!inited) {
    OQS_init();
    inited = true;
  }
}
#endif

// [[Rcpp::export]]
bool morie_crypto_liboqs_available() {
#ifdef MORIE_HAVE_LIBOQS
  return true;
#else
  return false;
#endif
}

// [[Rcpp::export]]
std::string morie_crypto_liboqs_version() {
#ifdef MORIE_HAVE_LIBOQS
  morie_oqs_ready();
  return std::string(OQS_version());
#else
  return std::string("");
#endif
}

// ============================================================
// ML-KEM-768 (FIPS 203, Kyber-based KEM)
// ============================================================
//
// Sizes (NIST FIPS 203):
//   public key  : 1184 bytes
//   secret key  : 2400 bytes
//   ciphertext  : 1088 bytes
//   shared sec  :   32 bytes

// [[Rcpp::export]]
Rcpp::List morie_crypto_mlkem768_keygen() {
#ifdef MORIE_HAVE_LIBOQS
  morie_oqs_ready();
  OQS_KEM* kem = OQS_KEM_new(OQS_KEM_alg_ml_kem_768);
  if (kem == nullptr)
    Rcpp::stop("OQS_KEM_new(ML-KEM-768) returned NULL");
  RawVector pk(kem->length_public_key);
  RawVector sk(kem->length_secret_key);
  if (OQS_KEM_keypair(kem, &pk[0], &sk[0]) != OQS_SUCCESS) {
    OQS_KEM_free(kem);
    Rcpp::stop("OQS_KEM_keypair failed");
  }
  OQS_KEM_free(kem);
  return Rcpp::List::create(Rcpp::Named("pk") = pk,
                              Rcpp::Named("sk") = sk);
#else
  Rcpp::stop("morie was built without liboqs; reinstall with "
             "liboqs-dev / brew install liboqs and rebuild morie.");
  return R_NilValue;
#endif
}

// [[Rcpp::export]]
Rcpp::List morie_crypto_mlkem768_encaps(SEXP pk_sxp) {
#ifdef MORIE_HAVE_LIBOQS
  morie_oqs_ready();
  RawVector pk(pk_sxp);
  OQS_KEM* kem = OQS_KEM_new(OQS_KEM_alg_ml_kem_768);
  if (kem == nullptr)
    Rcpp::stop("OQS_KEM_new(ML-KEM-768) returned NULL");
  if ((std::size_t) pk.size() != kem->length_public_key) {
    std::size_t expect = kem->length_public_key;
    OQS_KEM_free(kem);
    Rcpp::stop("pk must be %d bytes (got %d)",
               (int) expect, (int) pk.size());
  }
  RawVector ct(kem->length_ciphertext);
  RawVector ss(kem->length_shared_secret);
  if (OQS_KEM_encaps(kem, &ct[0], &ss[0], &pk[0]) != OQS_SUCCESS) {
    OQS_KEM_free(kem);
    Rcpp::stop("OQS_KEM_encaps failed");
  }
  OQS_KEM_free(kem);
  return Rcpp::List::create(Rcpp::Named("ct") = ct,
                              Rcpp::Named("shared_secret") = ss);
#else
  Rcpp::stop("morie was built without liboqs");
  return R_NilValue;
#endif
}

// [[Rcpp::export]]
SEXP morie_crypto_mlkem768_decaps(SEXP sk_sxp, SEXP ct_sxp) {
#ifdef MORIE_HAVE_LIBOQS
  morie_oqs_ready();
  RawVector sk(sk_sxp);
  RawVector ct(ct_sxp);
  OQS_KEM* kem = OQS_KEM_new(OQS_KEM_alg_ml_kem_768);
  if (kem == nullptr)
    Rcpp::stop("OQS_KEM_new(ML-KEM-768) returned NULL");
  if ((std::size_t) sk.size() != kem->length_secret_key ||
      (std::size_t) ct.size() != kem->length_ciphertext) {
    std::size_t esk = kem->length_secret_key;
    std::size_t ect = kem->length_ciphertext;
    OQS_KEM_free(kem);
    Rcpp::stop("size mismatch: sk=%d (need %d), ct=%d (need %d)",
               (int) sk.size(), (int) esk,
               (int) ct.size(), (int) ect);
  }
  RawVector ss(kem->length_shared_secret);
  if (OQS_KEM_decaps(kem, &ss[0], &ct[0], &sk[0]) != OQS_SUCCESS) {
    OQS_KEM_free(kem);
    Rcpp::stop("OQS_KEM_decaps failed (bad ciphertext or key)");
  }
  OQS_KEM_free(kem);
  return ss;
#else
  Rcpp::stop("morie was built without liboqs");
  return R_NilValue;
#endif
}

// ============================================================
// ML-DSA-65 (FIPS 204, Dilithium-based signatures)
// ============================================================
//
// Sizes (NIST FIPS 204):
//   public key  : 1952 bytes
//   secret key  : 4032 bytes
//   signature   : 3309 bytes (max; actual is variable up to this)

// [[Rcpp::export]]
Rcpp::List morie_crypto_mldsa65_keygen() {
#ifdef MORIE_HAVE_LIBOQS
  morie_oqs_ready();
  OQS_SIG* sig = OQS_SIG_new(OQS_SIG_alg_ml_dsa_65);
  if (sig == nullptr)
    Rcpp::stop("OQS_SIG_new(ML-DSA-65) returned NULL");
  RawVector pk(sig->length_public_key);
  RawVector sk(sig->length_secret_key);
  if (OQS_SIG_keypair(sig, &pk[0], &sk[0]) != OQS_SUCCESS) {
    OQS_SIG_free(sig);
    Rcpp::stop("OQS_SIG_keypair failed");
  }
  OQS_SIG_free(sig);
  return Rcpp::List::create(Rcpp::Named("pk") = pk,
                              Rcpp::Named("sk") = sk);
#else
  Rcpp::stop("morie was built without liboqs");
  return R_NilValue;
#endif
}

// [[Rcpp::export]]
SEXP morie_crypto_mldsa65_sign(SEXP sk_sxp, SEXP message_sxp) {
#ifdef MORIE_HAVE_LIBOQS
  morie_oqs_ready();
  RawVector sk(sk_sxp);
  RawVector message(message_sxp);
  OQS_SIG* sig = OQS_SIG_new(OQS_SIG_alg_ml_dsa_65);
  if (sig == nullptr)
    Rcpp::stop("OQS_SIG_new(ML-DSA-65) returned NULL");
  if ((std::size_t) sk.size() != sig->length_secret_key) {
    std::size_t expect = sig->length_secret_key;
    OQS_SIG_free(sig);
    Rcpp::stop("sk must be %d bytes (got %d)",
               (int) expect, (int) sk.size());
  }
  std::vector<unsigned char> sigbuf(sig->length_signature);
  std::size_t sig_len = 0;
  const unsigned char* msg_ptr =
      (message.size() > 0) ? &message[0] : nullptr;
  if (OQS_SIG_sign(sig, sigbuf.data(), &sig_len,
                    msg_ptr, message.size(),
                    &sk[0]) != OQS_SUCCESS) {
    OQS_SIG_free(sig);
    Rcpp::stop("OQS_SIG_sign failed");
  }
  OQS_SIG_free(sig);
  sigbuf.resize(sig_len);
  RawVector out(sig_len);
  std::memcpy(&out[0], sigbuf.data(), sig_len);
  return out;
#else
  Rcpp::stop("morie was built without liboqs");
  return R_NilValue;
#endif
}

// [[Rcpp::export]]
bool morie_crypto_mldsa65_verify(SEXP pk_sxp, SEXP message_sxp,
                                   SEXP signature_sxp) {
#ifdef MORIE_HAVE_LIBOQS
  morie_oqs_ready();
  RawVector pk(pk_sxp);
  RawVector message(message_sxp);
  RawVector signature(signature_sxp);
  OQS_SIG* sig = OQS_SIG_new(OQS_SIG_alg_ml_dsa_65);
  if (sig == nullptr)
    Rcpp::stop("OQS_SIG_new(ML-DSA-65) returned NULL");
  if ((std::size_t) pk.size() != sig->length_public_key) {
    std::size_t expect = sig->length_public_key;
    OQS_SIG_free(sig);
    Rcpp::stop("pk must be %d bytes (got %d)",
               (int) expect, (int) pk.size());
  }
  const unsigned char* msg_ptr =
      (message.size() > 0) ? &message[0] : nullptr;
  OQS_STATUS rc = OQS_SIG_verify(sig, msg_ptr, message.size(),
                                    &signature[0], signature.size(),
                                    &pk[0]);
  OQS_SIG_free(sig);
  return rc == OQS_SUCCESS;
#else
  Rcpp::stop("morie was built without liboqs");
  return false;
#endif
}
