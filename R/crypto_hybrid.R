# SPDX-License-Identifier: AGPL-3.0-or-later

# Hybrid KEM-DEM encryption (ML-KEM-768 + ChaCha20-Poly1305).
#
# R port of morie/crypto/hybrid.py.  Combines post-quantum key
# encapsulation (ML-KEM-768) with symmetric authenticated encryption
# (ChaCha20-Poly1305) via HKDF-SHA256 key derivation.
#
# Symmetric layer uses the native C ChaCha20-Poly1305 backend; the
# post-quantum ML-KEM-768 primitive comes from the liboqs bindings
# (morie_crypto_mlkem768_*).  Container format matches the Python
# morie.crypto.hybrid module byte-for-byte.
#
# WARNING: Research/educational implementation.  NOT constant-time.
# For production use, prefer audited hybrid KEM libraries (e.g. liboqs).

#' Internal helper: Morie Require Sodium
#' @noRd
.morie_require_sodium <- function() {
  if (!requireNamespace("sodium", quietly = TRUE)) {
    stop(
      "morie_crypto requires sodium; install.packages('sodium')",
      call. = FALSE
    )
  }
}

#' Internal helper: Morie Hkdf Sha256
#' @noRd
.morie_hkdf_sha256 <- function(ikm, len = 32L, salt = NULL,
                               info = raw(0)) {
  # 'len' not 'length' — base length() must not be shadowed
  if (len < 1L || len > 255L * 32L) {
    stop("HKDF output length must be in 1..255*32", call. = FALSE)
  }
  if (is.null(salt)) salt <- as.raw(rep(0L, 32L))
  if (is.character(ikm))  ikm  <- charToRaw(ikm)
  if (is.character(info)) info <- charToRaw(info)
  if (is.character(salt)) salt <- charToRaw(salt)
  # Module 22: native HMAC-SHA256 (RFC 5869 extract step).
  prk <- .morie_hmac_sha256_impl(salt, ikm)
  n <- ceiling(len / 32L)
  t_prev <- raw(0)
  okm <- raw(0)
  for (i in seq_len(n)) {
    msg <- c(t_prev, info, as.raw(i))
    t_prev <- .morie_hmac_sha256_impl(prk, msg)
    okm <- c(okm, t_prev)
  }
  okm[seq_len(len)]
}

#' Internal helper: Morie Wrapping Key
#' @noRd
.morie_wrapping_key <- function(kem_ct, pk) {
  salt <- .morie_sha256_impl(charToRaw("morie-hybrid-wrap-v1"))
  .morie_hkdf_sha256(
    ikm    = c(kem_ct, pk),
    len    = 32L,
    salt   = salt,
    info   = charToRaw("key-wrap")
  )
}

#' Generate an ML-KEM-768 key pair for hybrid encryption
#'
#' Convenience wrapper around [morie_crypto_mlkem768_keygen()] for the
#' hybrid KEM-DEM scheme. Requires the liboqs backend
#' ([morie_crypto_liboqs_available()]).
#'
#' @return A named list with `pk` (raw, 1184 B) and `sk` (raw, 2400 B).
#' @examples
#' if (morie_crypto_liboqs_available()) {
#'   kp <- morie_crypto_hybrid_keygen()
#'   c(pk = length(kp$pk), sk = length(kp$sk))
#' }
#' @export
morie_crypto_hybrid_keygen <- function() {
  morie_crypto_mlkem768_keygen()
}

#' Hybrid encrypt: ML-KEM-768 + ChaCha20-Poly1305
#'
#' Mirrors `morie.crypto.hybrid_encrypt` byte-for-byte. Encapsulates
#' under the recipient's ML-KEM-768 public key, derives a wrapping key
#' from `HKDF(kem_ct || pk)`, wraps a random 32-byte symmetric key with
#' ChaCha20-Poly1305, then encrypts the payload under that symmetric
#' key. Container format (big-endian lengths):
#' `len(kem_ct)\[4\] || kem_ct || wrap_nonce\[12\] || wrapped_key\[32\] ||
#' wrap_tag\[16\] || payload_nonce\[12\] || aead_ct || payload_tag\[16\]`.
#'
#' @param plaintext   Raw vector or character string to encrypt.
#' @param recipient_pk Raw vector: recipient's ML-KEM-768 public key
#'   (1184 bytes).
#' @return Raw vector container.
#' @examples
#' if (morie_crypto_liboqs_available()) {
#'   kp <- morie_crypto_hybrid_keygen()
#'   ct <- morie_crypto_hybrid_encrypt("hello", kp$pk)
#'   rawToChar(morie_crypto_hybrid_decrypt(ct, kp$sk))
#' }
#' @export
morie_crypto_hybrid_encrypt <- function(plaintext, recipient_pk) {
  if (is.character(plaintext)) plaintext <- charToRaw(plaintext)
  if (!is.raw(plaintext) || !is.raw(recipient_pk)) {
    stop("plaintext and recipient_pk must be raw vectors", call. = FALSE)
  }
  e <- morie_crypto_mlkem768_encaps(recipient_pk)
  kem_ct <- e$ct
  wrap_key <- .morie_wrapping_key(kem_ct, recipient_pk)
  sym_key <- morie_crypto_random_bytes(32L)
  wrap_nonce <- morie_crypto_random_bytes(12L)
  w <- morie_crypto_chacha20_poly1305_encrypt(wrap_key, wrap_nonce, sym_key)
  payload_nonce <- morie_crypto_random_bytes(12L)
  p <- morie_crypto_chacha20_poly1305_encrypt(sym_key, payload_nonce,
                                              plaintext)
  len4 <- writeBin(length(kem_ct), raw(), size = 4L, endian = "big")
  c(len4, kem_ct, wrap_nonce, w$ct, w$tag, payload_nonce, p$ct, p$tag)
}

#' Hybrid decrypt: ML-KEM-768 + ChaCha20-Poly1305
#'
#' Inverse of [morie_crypto_hybrid_encrypt()]. The wrapping key is
#' re-derived from `HKDF(kem_ct || pk)` using the public key embedded
#' in the ML-KEM-768 secret key (bytes 1153..2336), matching the
#' Python `morie.crypto.hybrid_decrypt` exactly, so no decapsulation
#' round-trip is required.
#'
#' @param ciphertext  Raw vector container.
#' @param recipient_sk Raw vector: recipient's ML-KEM-768 secret key
#'   (2400 bytes).
#' @return Raw vector of decrypted plaintext.
#' @examples
#' if (morie_crypto_liboqs_available()) {
#'   kp <- morie_crypto_hybrid_keygen()
#'   ct <- morie_crypto_hybrid_encrypt(charToRaw("secret"), kp$pk)
#'   rawToChar(morie_crypto_hybrid_decrypt(ct, kp$sk))
#' }
#' @export
morie_crypto_hybrid_decrypt <- function(ciphertext, recipient_sk) {
  if (!is.raw(ciphertext) || !is.raw(recipient_sk)) {
    stop("ciphertext and recipient_sk must be raw vectors", call. = FALSE)
  }
  if (length(ciphertext) < 4L) {
    stop("ciphertext too short to contain header", call. = FALSE)
  }
  kem_ct_len <- readBin(ciphertext[1:4], "integer", size = 4L,
                        endian = "big")
  offset <- 4L
  min_len <- offset + kem_ct_len + 12L + 32L + 16L + 12L + 16L
  if (kem_ct_len < 0L || length(ciphertext) < min_len) {
    stop("ciphertext too short", call. = FALSE)
  }
  kem_ct <- ciphertext[(offset + 1L):(offset + kem_ct_len)]
  offset <- offset + kem_ct_len
  # ML-KEM-768 sk layout: s_hat (1152 B) || pk (1184 B) || H(pk) || z.
  pk_start <- 3L * 384L
  pk_end <- pk_start + 3L * 384L + 32L
  if (length(recipient_sk) < pk_end) {
    stop("recipient_sk too short for ML-KEM-768", call. = FALSE)
  }
  recipient_pk <- recipient_sk[(pk_start + 1L):pk_end]
  wrap_key <- .morie_wrapping_key(kem_ct, recipient_pk)
  wrap_nonce <- ciphertext[(offset + 1L):(offset + 12L)]
  offset <- offset + 12L
  wrapped_ct <- ciphertext[(offset + 1L):(offset + 32L)]
  offset <- offset + 32L
  wrap_tag <- ciphertext[(offset + 1L):(offset + 16L)]
  offset <- offset + 16L
  sym_key <- morie_crypto_chacha20_poly1305_decrypt(
    wrap_key, wrap_nonce, c(wrapped_ct, wrap_tag))
  payload_nonce <- ciphertext[(offset + 1L):(offset + 12L)]
  offset <- offset + 12L
  aead_with_tag <- ciphertext[(offset + 1L):length(ciphertext)]
  morie_crypto_chacha20_poly1305_decrypt(sym_key, payload_nonce,
                                         aead_with_tag)
}

# Note: the public `morie_crypto_hkdf_sha256()` is the libsodium-backed
# wrapper in R/crypto_sym.R (byte-for-byte with the Python impl). The
# pure-R `.morie_hkdf_sha256()` helper above is the openssl fallback used
# internally by the hybrid key-wrap path only; it is not re-exported here
# to avoid a duplicate S3/namespace binding and a codoc mismatch.

# (public morie_crypto_hkdf_sha256() lives in R/crypto_sym.R, per the note
#  above; only the internal .morie_hkdf_sha256() fallback is kept here.)
