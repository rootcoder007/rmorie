// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Post-quantum families beyond lattices (module 22 extension, after
// the Red Hat PQC series): hash-based signatures (SLH-DSA / SPHINCS+,
// FIPS 205) and a code-based KEM (HQC, NIST's 2025 fourth-round
// selection). Same liboqs gating and calling conventions as the
// lattice primitives in morie_crypto_pqc.cpp (ML-KEM-768 / ML-DSA-65).
//
// Algorithm objects are constructed by NAME with old/new liboqs
// spellings tried in order, so builds against liboqs 0.9-0.14+ all
// resolve.

#include <Rcpp.h>
using Rcpp::RawVector;

#ifdef MORIE_HAVE_LIBOQS
#include <oqs/oqs.h>

namespace {

OQS_SIG* new_slhdsa128s() {
  static const char* names[] = {
    "SLH-DSA-SHA2-128s",            // liboqs >= 0.13 (FIPS 205 final)
    "SPHINCS+-SHA2-128s-simple",    // liboqs 0.8 - 0.12
    "SPHINCS+-SHA256-128s-simple"}; // very old spelling
  for (const char* n : names) {
    OQS_SIG* s = OQS_SIG_new(n);
    if (s != nullptr) return s;
  }
  return nullptr;
}

OQS_KEM* new_hqc128() {
  static const char* names[] = {"HQC-128", "HQC-RMRS-128"};
  for (const char* n : names) {
    OQS_KEM* k = OQS_KEM_new(n);
    if (k != nullptr) return k;
  }
  return nullptr;
}

}  // namespace
#endif

#define MORIE_NO_OQS_STOP                                              \
  Rcpp::stop("morie was built without liboqs; reinstall with "        \
             "liboqs-dev / brew install liboqs and rebuild morie.");   \
  return R_NilValue;

// ---------------------------------------------------------------------
// SLH-DSA-SHA2-128s (hash-based signatures, FIPS 205)
// ---------------------------------------------------------------------

// [[Rcpp::export(name = ".rmorie_slhdsa128s_keygen_impl")]]
SEXP morie_crypto_slhdsa128s_keygen() {
#ifdef MORIE_HAVE_LIBOQS
  OQS_SIG* sig = new_slhdsa128s();
  if (sig == nullptr)
    Rcpp::stop("this liboqs build does not include SLH-DSA/SPHINCS+");
  RawVector pk(sig->length_public_key);
  RawVector sk(sig->length_secret_key);
  if (OQS_SIG_keypair(sig, &pk[0], &sk[0]) != OQS_SUCCESS) {
    OQS_SIG_free(sig);
    Rcpp::stop("OQS_SIG_keypair(SLH-DSA-128s) failed");
  }
  OQS_SIG_free(sig);
  return Rcpp::List::create(Rcpp::Named("pk") = pk,
                            Rcpp::Named("sk") = sk);
#else
  MORIE_NO_OQS_STOP
#endif
}

// [[Rcpp::export(name = ".rmorie_slhdsa128s_sign_impl")]]
SEXP morie_crypto_slhdsa128s_sign(SEXP sk_sxp, SEXP message_sxp) {
#ifdef MORIE_HAVE_LIBOQS
  RawVector sk(sk_sxp), msg(message_sxp);
  OQS_SIG* sig = new_slhdsa128s();
  if (sig == nullptr)
    Rcpp::stop("this liboqs build does not include SLH-DSA/SPHINCS+");
  if ((size_t)sk.size() != sig->length_secret_key) {
    OQS_SIG_free(sig);
    Rcpp::stop("secret key must be %d bytes", (int)sig->length_secret_key);
  }
  RawVector out(sig->length_signature);
  size_t sig_len = 0;
  if (OQS_SIG_sign(sig, &out[0], &sig_len, &msg[0], msg.size(),
                   &sk[0]) != OQS_SUCCESS) {
    OQS_SIG_free(sig);
    Rcpp::stop("OQS_SIG_sign(SLH-DSA-128s) failed");
  }
  OQS_SIG_free(sig);
  if (sig_len < (size_t)out.size()) {
    RawVector trimmed(sig_len);
    std::copy(out.begin(), out.begin() + sig_len, trimmed.begin());
    return trimmed;
  }
  return out;
#else
  MORIE_NO_OQS_STOP
#endif
}

// [[Rcpp::export(name = ".rmorie_slhdsa128s_verify_impl")]]
SEXP morie_crypto_slhdsa128s_verify(SEXP pk_sxp, SEXP message_sxp,
                                    SEXP signature_sxp) {
#ifdef MORIE_HAVE_LIBOQS
  RawVector pk(pk_sxp), msg(message_sxp), sg(signature_sxp);
  OQS_SIG* sig = new_slhdsa128s();
  if (sig == nullptr)
    Rcpp::stop("this liboqs build does not include SLH-DSA/SPHINCS+");
  bool ok = OQS_SIG_verify(sig, &msg[0], msg.size(), &sg[0], sg.size(),
                           &pk[0]) == OQS_SUCCESS;
  OQS_SIG_free(sig);
  return Rcpp::wrap(ok);
#else
  MORIE_NO_OQS_STOP
#endif
}

// ---------------------------------------------------------------------
// HQC-128 (code-based KEM, NIST round-4 selection 2025)
// ---------------------------------------------------------------------

// [[Rcpp::export(name = ".rmorie_hqc128_keygen_impl")]]
SEXP morie_crypto_hqc128_keygen() {
#ifdef MORIE_HAVE_LIBOQS
  OQS_KEM* kem = new_hqc128();
  if (kem == nullptr)
    Rcpp::stop("this liboqs build does not include HQC");
  RawVector pk(kem->length_public_key);
  RawVector sk(kem->length_secret_key);
  if (OQS_KEM_keypair(kem, &pk[0], &sk[0]) != OQS_SUCCESS) {
    OQS_KEM_free(kem);
    Rcpp::stop("OQS_KEM_keypair(HQC-128) failed");
  }
  OQS_KEM_free(kem);
  return Rcpp::List::create(Rcpp::Named("pk") = pk,
                            Rcpp::Named("sk") = sk);
#else
  MORIE_NO_OQS_STOP
#endif
}

// [[Rcpp::export(name = ".rmorie_hqc128_encaps_impl")]]
SEXP morie_crypto_hqc128_encaps(SEXP pk_sxp) {
#ifdef MORIE_HAVE_LIBOQS
  RawVector pk(pk_sxp);
  OQS_KEM* kem = new_hqc128();
  if (kem == nullptr)
    Rcpp::stop("this liboqs build does not include HQC");
  if ((size_t)pk.size() != kem->length_public_key) {
    OQS_KEM_free(kem);
    Rcpp::stop("public key must be %d bytes", (int)kem->length_public_key);
  }
  RawVector ct(kem->length_ciphertext);
  RawVector ss(kem->length_shared_secret);
  if (OQS_KEM_encaps(kem, &ct[0], &ss[0], &pk[0]) != OQS_SUCCESS) {
    OQS_KEM_free(kem);
    Rcpp::stop("OQS_KEM_encaps(HQC-128) failed");
  }
  OQS_KEM_free(kem);
  return Rcpp::List::create(Rcpp::Named("ct") = ct,
                            Rcpp::Named("shared_secret") = ss);
#else
  MORIE_NO_OQS_STOP
#endif
}

// [[Rcpp::export(name = ".rmorie_hqc128_decaps_impl")]]
SEXP morie_crypto_hqc128_decaps(SEXP sk_sxp, SEXP ct_sxp) {
#ifdef MORIE_HAVE_LIBOQS
  RawVector sk(sk_sxp), ct(ct_sxp);
  OQS_KEM* kem = new_hqc128();
  if (kem == nullptr)
    Rcpp::stop("this liboqs build does not include HQC");
  if ((size_t)sk.size() != kem->length_secret_key ||
      (size_t)ct.size() != kem->length_ciphertext) {
    OQS_KEM_free(kem);
    Rcpp::stop("bad secret key / ciphertext length for HQC-128");
  }
  RawVector ss(kem->length_shared_secret);
  if (OQS_KEM_decaps(kem, &ss[0], &ct[0], &sk[0]) != OQS_SUCCESS) {
    OQS_KEM_free(kem);
    Rcpp::stop("OQS_KEM_decaps(HQC-128) failed");
  }
  OQS_KEM_free(kem);
  return ss;
#else
  MORIE_NO_OQS_STOP
#endif
}
