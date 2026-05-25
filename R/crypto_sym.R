# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3JJJ1: libsodium-backed symmetric crypto for morie.
#
# Mirrors the Python morie.crypto._chacha + morie.crypto._kdf APIs
# byte-for-byte so the on-disk Python keystore + hybrid envelope
# formats stay interoperable. The Python implementation is a
# pure-Python REFERENCE (admittedly NOT constant-time per the upstream
# docstrings); these R wrappers dispatch to libsodium's audited C
# code and are safe for production use.

#' Is libsodium available in this morie build?
#'
#' Phase 3JJJ1. Returns `TRUE` when morie's compiled .so was linked
#' against libsodium at install time (detected by ./configure via
#' `pkg-config --libs libsodium` or a bare `-lsodium` probe). If
#' `FALSE`, install libsodium and reinstall morie:
#'
#' \itemize{
#'   \item macOS:  `brew install libsodium`
#'   \item Debian: `sudo apt-get install libsodium-dev`
#'   \item Fedora: `sudo dnf install libsodium-devel`
#' }
#'
#' @return Single logical.
#' @examples
#' morie_crypto_sodium_available()
#' @export
morie_crypto_sodium_available <- function() {
  .Call(`_morie_morie_crypto_sodium_available`)
}

#' libsodium runtime version string
#'
#' Phase 3JJJ1. Returns the bundled libsodium version (e.g.,
#' `"1.0.20"`); empty string if libsodium wasn't linked.
#'
#' @return Single character.
#' @export
morie_crypto_sodium_version <- function() {
  .Call(`_morie_morie_crypto_sodium_version`)
}

#' ChaCha20-Poly1305 IETF authenticated encryption
#'
#' Phase 3JJJ1. Wraps libsodium's
#' `crypto_aead_chacha20poly1305_ietf_encrypt` (RFC 8439 IETF
#' variant: 32-byte key, 12-byte nonce, 16-byte authentication tag).
#'
#' Byte-compatible with the Python morie
#' `chacha20_poly1305_encrypt(key, nonce, plaintext, aad)`. The C
#' transport returns ciphertext || tag as a single buffer;
#' this R wrapper splits it into `list(ct = ..., tag = ...)` to
#' match the Python tuple return shape.
#'
#' @param key 32-byte raw vector.
#' @param nonce 12-byte raw vector (single-use per key; reuse is
#'   catastrophic).
#' @param plaintext Raw vector to encrypt (may be empty).
#' @param aad Optional raw vector of additional authenticated data
#'   (default empty).
#' @return List with `ct` (raw vector, length = `length(plaintext)`)
#'   and `tag` (raw vector, 16 bytes).
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   k <- morie_crypto_random_bytes(32)
#'   n <- morie_crypto_random_bytes(12)
#'   r <- morie_crypto_chacha20_poly1305_encrypt(k, n, charToRaw("hello"))
#'   p <- morie_crypto_chacha20_poly1305_decrypt(k, n, c(r$ct, r$tag))
#'   rawToChar(p)
#' }
#' @export
morie_crypto_chacha20_poly1305_encrypt <- function(key, nonce, plaintext,
                                                      aad = raw(0)) {
  stopifnot(is.raw(key), is.raw(nonce), is.raw(plaintext), is.raw(aad))
  out <- .Call(`_morie_morie_crypto_chacha20poly1305_encrypt`,
                key, nonce, plaintext, aad)
  n_pt <- length(plaintext)
  list(ct = out[seq_len(n_pt)], tag = out[(n_pt + 1L):(n_pt + 16L)])
}

#' ChaCha20-Poly1305 IETF authenticated decryption
#'
#' Phase 3JJJ1. Inverse of [morie_crypto_chacha20_poly1305_encrypt()].
#' Accepts the full ciphertext || tag buffer (concatenate as
#' `c(ct, tag)`).
#'
#' @param key 32-byte raw vector.
#' @param nonce 12-byte raw vector.
#' @param ct_with_tag Raw vector containing ciphertext appended
#'   with the 16-byte tag.
#' @param aad Optional raw vector of additional authenticated data.
#' @return Decrypted plaintext as raw vector.
#' @export
morie_crypto_chacha20_poly1305_decrypt <- function(key, nonce,
                                                      ct_with_tag,
                                                      aad = raw(0)) {
  stopifnot(is.raw(key), is.raw(nonce), is.raw(ct_with_tag), is.raw(aad))
  .Call(`_morie_morie_crypto_chacha20poly1305_decrypt`,
        key, nonce, ct_with_tag, aad)
}

#' HKDF-SHA256 (RFC 5869) key derivation
#'
#' Phase 3JJJ1. Mirrors the Python
#' `morie.crypto.hkdf_sha256(ikm, length=32, salt=b"", info=b"")`
#' byte-for-byte. Empty `salt` defaults to a 32-byte zero-filled
#' salt per RFC 5869 §2.2 (matches Python).
#'
#' @param ikm Input keying material (raw vector).
#' @param length Output length in bytes (1..8160).
#' @param salt Optional salt raw vector. Empty -> zero-fill.
#' @param info Optional context/application info raw vector.
#' @return Derived key material as raw vector of length `length`.
#' @export
morie_crypto_hkdf_sha256 <- function(ikm, length = 32L,
                                       salt = raw(0), info = raw(0)) {
  if (is.character(ikm))  ikm  <- charToRaw(paste(ikm, collapse = ""))
  if (is.character(salt)) salt <- charToRaw(paste(salt, collapse = ""))
  if (is.character(info)) info <- charToRaw(paste(info, collapse = ""))
  stopifnot(is.raw(ikm), is.raw(salt), is.raw(info))
  .Call(`_morie_morie_crypto_hkdf_sha256`,
        ikm, as.integer(length), salt, info)
}

#' Cryptographically secure random bytes (libsodium)
#'
#' Phase 3JJJ1. Wraps libsodium's `randombytes_buf`.
#'
#' @param n Number of bytes to generate.
#' @return Raw vector of length `n`.
#' @export
morie_crypto_random_bytes <- function(n) {
  .Call(`_morie_morie_crypto_random_bytes`, as.integer(n))
}
