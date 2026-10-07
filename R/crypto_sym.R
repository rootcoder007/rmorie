# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Symmetric crypto for morie: libsodium's C code when rmorie was linked
# against it, otherwise the native constant-time code in
# src/morie_crypto_native_sym.cpp (ChaCha20-Poly1305), the native HMAC
# (HKDF) and rmoriebricklayer's operating-system random source -- the same
# bytes either way, and the same as the Python morie.crypto._chacha and
# morie.crypto._kdf, so the key store and hybrid envelope formats are
# shared by all three.

#' Is libsodium available in this morie build?
#'
#' Returns `TRUE` when morie's compiled .so was linked against libsodium
#' at install time (detected by ./configure via `pkg-config --libs
#' libsodium` or a bare `-lsodium` probe). Nothing needs it: without it
#' ChaCha20-Poly1305, HKDF and the random bytes run on rmorie's native
#' code, with identical output. To use libsodium's code, install it and
#' reinstall morie:
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
  .Call(`_rmorie_morie_crypto_sodium_available`)
}

#' libsodium runtime version string
#'
#' Phase 3JJJ1. Returns the included libsodium version (e.g.,
#' `"1.0.20"`); empty string if libsodium wasn't linked.
#'
#' @return Single character.
#' @examples
#' morie_crypto_sodium_version()
#' @export
morie_crypto_sodium_version <- function() {
  .Call(`_rmorie_morie_crypto_sodium_version`)
}

#' ChaCha20-Poly1305 IETF authenticated encryption
#'
#' RFC 8439 (IETF variant: 32-byte key, 12-byte nonce, 16-byte
#' authentication tag): libsodium's
#' `crypto_aead_chacha20poly1305_ietf_encrypt` when rmorie is linked
#' against it, rmorie's native constant-time code otherwise.
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
#' k <- morie_crypto_random_bytes(32)
#' n <- morie_crypto_random_bytes(12)
#' r <- morie_crypto_chacha20_poly1305_encrypt(k, n, charToRaw("hello"))
#' p <- morie_crypto_chacha20_poly1305_decrypt(k, n, c(r$ct, r$tag))
#' rawToChar(p)
#' @export
morie_crypto_chacha20_poly1305_encrypt <- function(key, nonce, plaintext,
                                                   aad = raw(0)) {
  stopifnot(is.raw(key), is.raw(nonce), is.raw(plaintext), is.raw(aad))
  out <- if (morie_crypto_sodium_available()) {
    .Call(`_rmorie_morie_crypto_chacha20poly1305_encrypt`, key, nonce, plaintext, aad)
  } else {
    .morie_aead_seal_impl(key, nonce, plaintext, aad)
  }
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
#' @examples
#' k <- morie_crypto_random_bytes(32)
#' n <- morie_crypto_random_bytes(12)
#' pt <- charToRaw("the quick brown fox")
#' aad <- charToRaw("hdr:v1")
#' enc <- morie_crypto_chacha20_poly1305_encrypt(k, n, pt, aad)
#' dec <- morie_crypto_chacha20_poly1305_decrypt(k, n, c(enc$ct, enc$tag), aad)
#' identical(dec, pt)
#' @export
morie_crypto_chacha20_poly1305_decrypt <- function(key, nonce,
                                                   ct_with_tag,
                                                   aad = raw(0)) {
  stopifnot(is.raw(key), is.raw(nonce), is.raw(ct_with_tag), is.raw(aad))
  if (morie_crypto_sodium_available()) {
    return(.Call(`_rmorie_morie_crypto_chacha20poly1305_decrypt`, key, nonce, ct_with_tag, aad))
  }
  out <- .morie_aead_open_impl(key, nonce, ct_with_tag, aad)
  if (is.null(out)) stop("ChaCha20-Poly1305 decrypt failed: bad tag, key, or nonce", call. = FALSE)
  out
}

#' HKDF-SHA256 (RFC 5869) key derivation
#'
#' Mirrors the Python
#' `morie.crypto.hkdf_sha256(ikm, length=32, salt=b"", info=b"")`
#' byte-for-byte. Empty `salt` defaults to a 32-byte zero-filled
#' salt per RFC 5869 Section 2.2 (matches Python). libsodium's HMAC when
#' rmorie is linked against it, the native one otherwise.
#'
#' @param ikm Input keying material (raw vector).
#' @param length Output length in bytes (1..8160).
#' @param salt Optional salt raw vector. Empty -> zero-fill.
#' @param info Optional context/application info raw vector.
#' @return Derived key material as raw vector of length `length`.
#' @examples
#' out <- morie_crypto_hkdf_sha256("seed",
#'   len = 32L, salt = "salt",
#'   info = "ctx"
#' )
#' length(out)
#' @export
morie_crypto_hkdf_sha256 <- function(ikm, length = 32L,
                                     salt = raw(0), info = raw(0)) {
  if (is.character(ikm)) ikm <- charToRaw(paste(ikm, collapse = ""))
  if (is.character(salt)) salt <- charToRaw(paste(salt, collapse = ""))
  if (is.character(info)) info <- charToRaw(paste(info, collapse = ""))
  stopifnot(is.raw(ikm), is.raw(salt), is.raw(info))
  if (!morie_crypto_sodium_available()) {
    # an empty HMAC key is the zero-filled salt (RFC 2104 pads the key with zeros)
    return(.morie_hkdf_sha256(ikm, len = as.integer(length), salt = salt, info = info))
  }
  .Call(
    `_rmorie_morie_crypto_hkdf_sha256`,
    ikm, as.integer(length), salt, info
  )
}

#' Cryptographically secure random bytes
#'
#' libsodium's `randombytes_buf` when rmorie is linked against it,
#' otherwise the operating system's source through
#' [rmoriebricklayer::random_bytes()] (never R's own generator).
#'
#' @param n Number of bytes to generate.
#' @return Raw vector of length `n`.
#' @examples
#' r <- morie_crypto_random_bytes(32L)
#' length(r)
#' @export
morie_crypto_random_bytes <- function(n) {
  if (!morie_crypto_sodium_available()) {
    if (length(n) != 1L || is.na(n) || n < 1) stop("n must be positive", call. = FALSE)
    return(rmoriebricklayer::random_bytes(as.integer(n)))
  }
  .Call(`_rmorie_morie_crypto_random_bytes`, as.integer(n))
}
