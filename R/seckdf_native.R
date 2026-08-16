# HKDF: extract-and-expand key derivation (HMAC-SHA-256).
# Sources: Krawczyk, H. & Eronen, P. (2010) "HMAC-based Extract-and-
# Expand Key Derivation Function (HKDF)", RFC 5869,
# doi:10.17487/RFC5869. Krawczyk, H. (2010) "Cryptographic Extraction
# and Key Derivation: The HKDF Scheme", CRYPTO 2010, LNCS 6223,
# 631-648, doi:10.1007/978-3-642-14623-7_34. NIST (2008) FIPS PUB
# 198-1 (HMAC).
#
# Native implementation mirroring morie.fn.seckdf exactly: the salt
# is the HMAC key and the IKM is the message; expand caps output at
# 255*HashLen because the counter is a single octet.

.KDF_HASH_LEN <- 32L
.KDF_MAX_BLOCKS <- 255L

#' HKDF Extract
#'
#' PRK = HMAC(salt, IKM). The salt is the HMAC KEY and the IKM is the
#' MESSAGE; the order is the way round implementations get backwards.
#'
#' @param ikm Raw input keying material.
#' @param salt Optional raw salt; default HashLen zeros.
#' @return List with \code{prk}, \code{salt_supplied}, \code{note}.
#' @references RFC 5869 Sec. 2.2.
#' @export
morie_seckdf_extract <- function(ikm, salt = NULL) {
  s <- if (is.null(salt)) raw(.KDF_HASH_LEN) else as.raw(salt)
  list(prk = .kdf_hmac(s, ikm), salt_supplied = !is.null(salt),
       note = "the salt is the HMAC KEY; the IKM is the message")
}

#' HKDF Expand
#'
#' Counter-mode expansion; the counter is a single octet so output is
#' capped at \code{255 * HashLen}.
#'
#' @param prk Raw pseudorandom key (at least HashLen bytes).
#' @param info Optional raw context info.
#' @param length Positive integer output length in bytes.
#' @return List with \code{okm}, \code{blocks}, \code{length}.
#' @references RFC 5869 Sec. 2.3.
#' @export
morie_seckdf_expand <- function(prk, info = raw(), length = 32L) {
  L <- as.integer(length)
  if (L < 1L) stop("seckdf: the output length must be positive")
  if (L > .KDF_MAX_BLOCKS * .KDF_HASH_LEN)
    stop("seckdf: L = ", L, " exceeds 255*HashLen = ",
         .KDF_MAX_BLOCKS * .KDF_HASH_LEN,
         "; the counter is a single octet, so this cannot be satisfied")
  p <- as.raw(prk)
  if (length(p) < .KDF_HASH_LEN)
    stop("seckdf: the PRK is ", length(p), " bytes, shorter than the hash length ",
         .KDF_HASH_LEN, " -- Extract was probably skipped on non-uniform input")
  inf <- as.raw(info)
  out <- raw(); tt <- raw(); i <- 1L
  while (length(out) < L) {
    tt <- .kdf_hmac(p, c(tt, inf, as.raw(i)))
    out <- c(out, tt)
    i <- i + 1L
  }
  list(okm = out[seq_len(L)], blocks = i - 1L, length = L)
}

#' HKDF: extract then expand, or expand alone on uniform input
#'
#' \code{skip_extract = TRUE} is the documented case for input that
#' is already a uniformly random key, not a shortcut.
#'
#' @param ikm Raw input keying material.
#' @param salt Optional raw salt.
#' @param info Optional raw context info.
#' @param length Output length in bytes.
#' @param skip_extract Logical; skip the Extract step.
#' @return List with \code{okm}, \code{okm_hex} (==\code{estimate}),
#'   \code{prk}, \code{prk_hex}, \code{length}, \code{blocks},
#'   \code{salt_supplied}, \code{extract_skipped}, \code{method},
#'   \code{note}.
#' @references RFC 5869 Sec. 2.2-2.3.
#' @export
morie_seckdf_hkdf <- function(ikm, salt = NULL, info = raw(),
                             length = 32L, skip_extract = FALSE) {
  if (isTRUE(skip_extract)) {
    prk <- as.raw(ikm); salted <- FALSE
  } else {
    e <- morie_seckdf_extract(ikm, salt)
    prk <- e$prk; salted <- e$salt_supplied
  }
  r <- morie_seckdf_expand(prk, info, length)
  list(estimate = .kdf_hex(r$okm), okm = r$okm,
       okm_hex = .kdf_hex(r$okm), prk = prk, prk_hex = .kdf_hex(prk),
       length = r$length, blocks = r$blocks,
       salt_supplied = salted, extract_skipped = isTRUE(skip_extract),
       method = "HKDF-SHA256; Krawczyk & Eronen (2010) RFC 5869",
       note = "info binds the output to a context, so one PRK gives independent keys for independent purposes")
}

#' Derive one key per context from a single PRK
#'
#' Deriving an encryption key and a MAC key from the same secret is
#' safe precisely because the contexts differ.
#'
#' @param ikm Raw input keying material.
#' @param contexts Character vector of context labels.
#' @param salt Optional raw salt.
#' @param length Output length per key.
#' @return List with \code{keys}, \code{hex}, \code{prk},
#'   \code{all_distinct}, \code{note}.
#' @export
morie_seckdf_derive_context_keys <- function(ikm, contexts, salt = NULL,
                                              length = 32L) {
  e <- morie_seckdf_extract(ikm, salt)
  keys <- list()
  for (c in contexts) {
    keys[[c]] <- morie_seckdf_expand(e$prk, charToRaw(c), length)$okm
  }
  hexed <- lapply(keys, .kdf_hex)
  list(keys = keys, hex = hexed, prk = e$prk,
       all_distinct = length(unique(unlist(hexed))) == length(hexed),
       note = "same PRK, different info, unrelated outputs")
}

# Re-uses the same SHA-256 / HMAC primitives as sechsh via the local
# .sech_sha256 and .sech_hmac defined there; this file is intended to
# be sourced together with sechsh_native.R.
#' Re-uses the same SHA-256 / HMAC primitives as sechsh via the local
#'
#' .sech_sha256 and .sech_hmac defined there; this file is intended to
#' be sourced together with sechsh_native.R.
#'
#' @param key A vector; its length is taken.
#' @param msg See Usage.
#' @return The value of \code{.sech_sha256}.
#' @export
.kdf_hmac <- function(key, msg) {
  key <- as.raw(key)
  if (length(key) > 64L) key <- .sech_sha256(key)
  if (length(key) < 64L) key <- c(key, rep(as.raw(0x00), 64L - length(key)))
  opad <- bitwXor(key, rep(as.raw(0x5c), 64L))
  ipad <- bitwXor(key, rep(as.raw(0x36), 64L))
  .sech_sha256(c(opad, .sech_sha256(c(ipad, as.raw(msg)))))
}

#' .kdf_hex
#'
#' A step of the seckdf_native implementation. Called by \code{morie_seckdf_hkdf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param bs See Usage.
#' @return A character value.
#' @export
.kdf_hex <- function(bs) {
  paste(format(as.hexmode(as.integer(bs)), width = 2L), collapse = "")
}

# house entry point: the package exports one morie_<module>
morie_seckdf <- morie_seckdf_extract
