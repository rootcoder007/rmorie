# SPDX-License-Identifier: AGPL-3.0-or-later

# Hybrid KEM-DEM encryption (ML-KEM-768 + ChaCha20-Poly1305).
#
# R port of morie/crypto/hybrid.py.  Combines post-quantum key
# encapsulation (ML-KEM-768) with symmetric authenticated encryption
# (ChaCha20-Poly1305) via HKDF-SHA256 key derivation.
#
# Symmetric layer uses the {sodium} R package; HMAC-SHA256 for HKDF
# uses {openssl}.  The post-quantum ML-KEM-768 primitive is not
# available in sodium/openssl as of this writing, so the public
# `morie_crypto_hybrid_*` entry points currently raise a clear
# not-implemented error pointing at the v1.0.1 sprint.
#
# WARNING: Research/educational implementation.  NOT constant-time.
# For production use, prefer audited hybrid KEM libraries (e.g. liboqs).

.morie_require_sodium <- function() {
  if (!requireNamespace("sodium", quietly = TRUE)) {
    stop(
      "morie_crypto requires sodium; install.packages('sodium')",
      call. = FALSE
    )
  }
}

.morie_require_openssl <- function() {
  if (!requireNamespace("openssl", quietly = TRUE)) {
    stop(
      "morie_crypto requires openssl; install.packages('openssl')",
      call. = FALSE
    )
  }
}

.morie_hkdf_sha256 <- function(ikm, len = 32L, salt = NULL,
                               info = raw(0)) {
  # 'len' not 'length' — base length() must not be shadowed
  .morie_require_openssl()
  if (len < 1L || len > 255L * 32L) {
    stop("HKDF output length must be in 1..255*32", call. = FALSE)
  }
  if (is.null(salt)) salt <- as.raw(rep(0L, 32L))
  if (is.character(ikm))  ikm  <- charToRaw(ikm)
  if (is.character(info)) info <- charToRaw(info)
  if (is.character(salt)) salt <- charToRaw(salt)
  prk <- as.raw(openssl::sha256(ikm, key = salt))
  n <- ceiling(len / 32L)
  t_prev <- raw(0)
  okm <- raw(0)
  for (i in seq_len(n)) {
    msg <- c(t_prev, info, as.raw(i))
    t_prev <- as.raw(openssl::sha256(msg, key = prk))
    okm <- c(okm, t_prev)
  }
  okm[seq_len(len)]
}

.morie_wrapping_key <- function(kem_ct, pk) {
  .morie_require_openssl()
  salt <- as.raw(openssl::sha256(charToRaw("morie-hybrid-wrap-v1")))
  .morie_hkdf_sha256(
    ikm    = c(kem_ct, pk),
    len    = 32L,
    salt   = salt,
    info   = charToRaw("key-wrap")
  )
}

#' Generate an ML-KEM-768 key pair for hybrid encryption
#' @return A named list with `pk` (raw) and `sk` (raw).
#' @export
morie_crypto_hybrid_keygen <- function() {
  stop(
    "not implemented: ML-KEM-768 keygen requires a post-quantum ",
    "backend not yet wired into the R bindings.  Tracked for v1.0.1.",
    call. = FALSE
  )
}

#' Hybrid encrypt: ML-KEM-768 + ChaCha20-Poly1305
#' @param plaintext   Raw vector or character string to encrypt.
#' @param recipient_pk Raw vector: recipient's ML-KEM-768 public key.
#' @return Raw vector container.
#' @export
morie_crypto_hybrid_encrypt <- function(plaintext, recipient_pk) {
  .morie_require_sodium()
  if (is.character(plaintext)) plaintext <- charToRaw(plaintext)
  if (!is.raw(plaintext) || !is.raw(recipient_pk)) {
    stop("plaintext and recipient_pk must be raw vectors", call. = FALSE)
  }
  stop(
    "not implemented: hybrid_encrypt depends on ML-KEM-768 encaps, ",
    "which is not in the current R crypto backends.  Tracked for v1.0.1.",
    call. = FALSE
  )
}

#' Hybrid decrypt: ML-KEM-768 + ChaCha20-Poly1305
#' @param ciphertext  Raw vector container.
#' @param recipient_sk Raw vector: recipient's ML-KEM-768 secret key.
#' @return Raw vector of decrypted plaintext.
#' @export
morie_crypto_hybrid_decrypt <- function(ciphertext, recipient_sk) {
  .morie_require_sodium()
  if (!is.raw(ciphertext) || !is.raw(recipient_sk)) {
    stop("ciphertext and recipient_sk must be raw vectors", call. = FALSE)
  }
  stop(
    "not implemented: hybrid_decrypt depends on ML-KEM-768 decaps, ",
    "which is not in the current R crypto backends.  Tracked for v1.0.1.",
    call. = FALSE
  )
}

#' HKDF-SHA256 (RFC 5869)
#' @param ikm    Raw vector or character string.
#' @param length Output length in bytes.
#' @param salt   Raw vector or character; NULL for zeroed salt.
#' @param info   Raw vector or character (context info).
#' @return Raw vector of `length` bytes.
#' @export
morie_crypto_hkdf_sha256 <- function(ikm, len = 32L, salt = NULL,
                                     info = raw(0)) {
  .morie_hkdf_sha256(ikm = ikm, len = len, salt = salt, info = info)
}
