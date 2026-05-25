# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3JJJ2: liboqs-backed post-quantum crypto for morie.
#
# ML-KEM-768 (FIPS 203) key encapsulation + ML-DSA-65 (FIPS 204)
# signatures via the Open Quantum Safe project's audited C library.
# Byte-compatible with the Python morie.crypto._mlkem +
# _dilithium reference implementations so hybrid envelopes (3JJJ3)
# can interoperate.

#' Is liboqs available in this morie build?
#'
#' Phase 3JJJ2. Returns `TRUE` when morie's compiled .so was linked
#' against the Open Quantum Safe library at install time. If
#' `FALSE`, install liboqs and reinstall morie:
#'
#' \itemize{
#'   \item macOS:  `brew install liboqs`
#'   \item Debian: `sudo apt-get install liboqs-dev`
#'     (or build from source: \url{https://github.com/open-quantum-safe/liboqs})
#' }
#'
#' @return Single logical.
#' @export
morie_crypto_liboqs_available <- function() {
  .Call(`_morie_morie_crypto_liboqs_available`)
}

#' liboqs runtime version string
#'
#' @return Single character (e.g. `"0.15.0"`); empty if liboqs absent.
#' @export
morie_crypto_liboqs_version <- function() {
  .Call(`_morie_morie_crypto_liboqs_version`)
}

# ============================================================
# ML-KEM-768 (FIPS 203 -- Kyber-based KEM)
# ============================================================

#' ML-KEM-768 keypair generation (NIST FIPS 203)
#'
#' Phase 3JJJ2. Generates a post-quantum key encapsulation keypair.
#' Sizes: `pk` = 1184 bytes, `sk` = 2400 bytes.
#'
#' @return List with `pk` (raw, 1184 B) and `sk` (raw, 2400 B).
#' @examples
#' if (morie_crypto_liboqs_available()) {
#'   kp <- morie_crypto_mlkem768_keygen()
#'   c(pk = length(kp$pk), sk = length(kp$sk))
#' }
#' @export
morie_crypto_mlkem768_keygen <- function() {
  .Call(`_morie_morie_crypto_mlkem768_keygen`)
}

#' ML-KEM-768 encapsulation
#'
#' Encapsulate a shared secret under a recipient's ML-KEM-768 public
#' key. Returns the ciphertext (1088 B) the sender transmits, plus
#' the 32-byte shared secret the sender holds locally.
#'
#' @param pk 1184-byte raw vector (recipient's ML-KEM-768 public key).
#' @return List with `ct` (raw, 1088 B) and `shared_secret` (raw, 32 B).
#' @export
morie_crypto_mlkem768_encaps <- function(pk) {
  stopifnot(is.raw(pk))
  .Call(`_morie_morie_crypto_mlkem768_encaps`, pk)
}

#' ML-KEM-768 decapsulation
#'
#' Recover the shared secret from an encapsulation ciphertext using
#' the recipient's secret key.
#'
#' @param sk 2400-byte raw vector (recipient's ML-KEM-768 secret key).
#' @param ct 1088-byte raw vector (sender's encapsulation ciphertext).
#' @return Raw vector (32 B), the shared secret.
#' @export
morie_crypto_mlkem768_decaps <- function(sk, ct) {
  stopifnot(is.raw(sk), is.raw(ct))
  .Call(`_morie_morie_crypto_mlkem768_decaps`, sk, ct)
}

# ============================================================
# ML-DSA-65 (FIPS 204 -- Dilithium-based signatures)
# ============================================================

#' ML-DSA-65 keypair generation (NIST FIPS 204)
#'
#' Phase 3JJJ2. Generates a post-quantum signature keypair.
#' Sizes: `pk` = 1952 bytes, `sk` = 4032 bytes.
#'
#' @return List with `pk` (raw, 1952 B) and `sk` (raw, 4032 B).
#' @export
morie_crypto_mldsa65_keygen <- function() {
  .Call(`_morie_morie_crypto_mldsa65_keygen`)
}

#' ML-DSA-65 signature
#'
#' Sign a message with an ML-DSA-65 secret key. Signature length is
#' variable up to a 3309-byte ceiling (typical: ~3293 B).
#'
#' @param sk 4032-byte raw vector (signer's secret key).
#' @param message Raw vector to sign.
#' @return Raw vector signature.
#' @export
morie_crypto_mldsa65_sign <- function(sk, message) {
  stopifnot(is.raw(sk), is.raw(message))
  .Call(`_morie_morie_crypto_mldsa65_sign`, sk, message)
}

#' ML-DSA-65 signature verification
#'
#' @param pk 1952-byte raw vector (signer's public key).
#' @param message Raw vector that was signed.
#' @param signature Raw vector signature returned by
#'   [morie_crypto_mldsa65_sign()].
#' @return Single logical: `TRUE` if signature is valid.
#' @export
morie_crypto_mldsa65_verify <- function(pk, message, signature) {
  stopifnot(is.raw(pk), is.raw(message), is.raw(signature))
  .Call(`_morie_morie_crypto_mldsa65_verify`, pk, message, signature)
}
