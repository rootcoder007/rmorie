# Envelope encryption: DEK per record, KEK above it.
# Sources: NIST (2020) SP 800-57 Part 1 Rev. 5 (key management);
# Housley, R. (2009) RFC 5652 (CMS enveloped data); Nir, Y. & Langley,
# A. (2018) RFC 8439 (ChaCha20-Poly1305 AEAD); Krawczyk, H. & Eronen,
# P. (2010) RFC 5869 (HKDF).
#
# Native implementation mirroring morie.fn.secrtt exactly: HKDF for
# per-record DEK derivation, AEAD for the wrap and the record, KEK
# rotation that re-wraps without touching record ciphertext, and
# crypto-shredding by reporting the covered scope.

#' Derive a per-record DEK
#'
#' Deriving from a master seed and the record id means the key
#' table is the seed plus an identifier, not one row per record.
#'
#' @param master_seed Raw bytes.
#' @param record_id Raw bytes (or coercible).
#' @param salt Optional raw salt.
#' @return List with \code{dek}, \code{dek_hex}, \code{record_id},
#'   \code{note}.
#' @export
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   seed <- as.raw(1:32)
#'   r <- morie_secrtt_generate_dek(seed, record_id = 7L)
#'   r$dek_hex
#' }
#' @keywords internal
morie_secrtt_generate_dek <- function(master_seed, record_id,
                                      salt = NULL) {
  info <- c(charToRaw("dek:"), as.raw(record_id))
  r <- morie_seckdf_hkdf(master_seed, salt, info, 32L)
  list(dek = r$okm, dek_hex = r$okm_hex, record_id = record_id,
       note = "per-record, so one leaked DEK exposes one record")
}

#' Wrap a DEK under a KEK
#'
#' The KEK id is bound into the AAD, so a wrapped DEK cannot be
#' replayed under a different KEK.
#'
#' @param dek Raw 32-byte DEK.
#' @param kek Raw KEK.
#' @param nonce Raw 12-byte nonce.
#' @param kek_id KEK identifier.
#' @param aad Raw additional authenticated data.
#' @return List with \code{wrapped}, \code{tag}, \code{nonce},
#'   \code{kek_id}, \code{aad}, \code{wrapped_hex}, \code{note}.
#' @export
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   seed <- as.raw(1:32)
#'   dek <- morie_secrtt_generate_dek(seed, record_id = 7L)$dek
#'   kek <- as.raw(rev(1:32))
#'   w <- morie_secrtt_wrap_dek(dek, kek, nonce = as.raw(1:12))
#'   w$kek_id
#' }
#' @keywords internal
morie_secrtt_wrap_dek <- function(dek, kek, nonce, kek_id = "kek-1",
                                  aad = raw()) {
  d <- as.raw(dek)
  if (length(d) != 32L)
    stop("secrtt: a DEK must be 32 bytes, got ", length(d))
  bound <- c(as.raw(aad), charToRaw(as.character(kek_id)))
  r <- morie_secaead_aead_encrypt(kek, nonce, d, bound)
  list(wrapped = r$ciphertext, tag = r$tag, nonce = as.raw(nonce),
       kek_id = kek_id, aad = as.raw(aad), wrapped_hex = r$ciphertext_hex,
       note = "the KEK id is authenticated, so a wrapped DEK cannot be replayed under a different KEK")
}

#' Unwrap a DEK and record the call
#'
#' The envelope bounds offline compromise; an attacker who can call
#' the KEK is bounded only by what the audit trail catches, so the
#' call is logged rather than left silent.
#'
#' @param wrapped List with \code{wrapped}, \code{nonce}, \code{tag},
#'   \code{kek_id}, optional \code{aad}.
#' @param kek Raw KEK.
#' @param audit_log Optional list to append to.
#' @return List with \code{dek}, \code{kek_id}, \code{audited} and
#'   \code{audit_log}, the log extended by this call (R does not modify
#'   its argument in place).
#' @export
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   seed <- as.raw(1:32)
#'   dek <- morie_secrtt_generate_dek(seed, record_id = 7L)$dek
#'   kek <- as.raw(rev(1:32))
#'   w <- morie_secrtt_wrap_dek(dek, kek, nonce = as.raw(1:12))
#'   u <- morie_secrtt_unwrap_dek(w, kek)
#'   identical(u$dek, dek)
#' }
#' @keywords internal
morie_secrtt_unwrap_dek <- function(wrapped, kek, audit_log = NULL) {
  aad <- c(as.raw(wrapped$aad %||% raw()),
           charToRaw(as.character(wrapped$kek_id)))
  r <- morie_secaead_aead_decrypt(kek, as.raw(wrapped$nonce),
                                   as.raw(wrapped$wrapped),
                                   as.raw(wrapped$tag), aad)
  if (!is.null(audit_log)) {
    audit_log[[length(audit_log) + 1L]] <- list(
      event = "unwrap", kek_id = wrapped$kek_id, ok = r$valid)
  }
  if (!isTRUE(r$valid))
    stop("secrtt: the wrapped DEK failed authentication -- wrong KEK, or it was tampered with")
  # R copies its arguments, so the extended log is returned rather than
  # modified in place
  list(dek = r$plaintext, kek_id = wrapped$kek_id,
       audited = !is.null(audit_log), audit_log = audit_log)
}

#' Encrypt a record under its own DEK
#'
#' @param plaintext Raw bytes.
#' @param dek Raw 32-byte DEK.
#' @param nonce Raw 12-byte nonce.
#' @param aad Raw additional authenticated data.
#' @return List with \code{ciphertext}, \code{tag}, \code{nonce},
#'   \code{aad}.
#' @export
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   seed <- as.raw(1:32)
#'   dek <- morie_secrtt_generate_dek(seed, record_id = 7L)$dek
#'   s <- morie_secrtt_seal_record(charToRaw("hello"), dek,
#'                                 nonce = as.raw(13:24))
#'   s$tag
#' }
#' @keywords internal
morie_secrtt_seal_record <- function(plaintext, dek, nonce,
                                     aad = raw()) {
  r <- morie_secaead_aead_encrypt(dek, nonce, as.raw(plaintext),
                                   as.raw(aad))
  list(ciphertext = r$ciphertext, tag = r$tag,
       nonce = as.raw(nonce), aad = as.raw(aad))
}

#' Decrypt a record; fails closed
#'
#' @param sealed List with \code{ciphertext}, \code{nonce}, \code{tag},
#'   optional \code{aad}.
#' @param dek Raw DEK.
#' @return Raw plaintext.
#' @export
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   seed <- as.raw(1:32)
#'   dek <- morie_secrtt_generate_dek(seed, record_id = 7L)$dek
#'   s <- morie_secrtt_seal_record(charToRaw("hello"), dek,
#'                                 nonce = as.raw(13:24))
#'   rawToChar(morie_secrtt_open_record(s, dek))
#' }
#' @keywords internal
morie_secrtt_open_record <- function(sealed, dek) {
  r <- morie_secaead_aead_decrypt(dek, as.raw(sealed$nonce),
                                   as.raw(sealed$ciphertext),
                                   as.raw(sealed$tag),
                                   as.raw(sealed$aad %||% raw()))
  if (!isTRUE(r$valid))
    stop("secrtt: the record failed authentication")
  # the ChaCha20 core hands back byte VALUES as integers; the documented
  # return type is raw
  as.raw(r$plaintext)
}

#' Rotate the KEK: re-wrap every DEK, no record ciphertext touched
#'
#' The cheap rotation, and the reason the envelope exists.
#'
#' @param wrapped_deks List of wrapped DEKs.
#' @param old_kek Raw KEK.
#' @param new_kek Raw KEK.
#' @param new_nonces One fresh nonce per wrapped DEK.
#' @param new_kek_id New KEK identifier.
#' @param audit_log Optional audit list.
#' @return List with \code{wrapped} (==\code{estimate}), \code{n},
#'   \code{records_reencrypted}, \code{kek_id}, \code{audit_log},
#'   \code{method}, \code{note}.
#' @export
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   seed <- as.raw(1:32)
#'   dek <- morie_secrtt_generate_dek(seed, record_id = 7L)$dek
#'   kek1 <- as.raw(rev(1:32))
#'   kek2 <- as.raw(rep(7L, 32))
#'   w <- morie_secrtt_wrap_dek(dek, kek1, nonce = as.raw(1:12))
#'   r <- morie_secrtt_rotate_kek(list(w), kek1, kek2,
#'                                new_nonces = list(as.raw(2:13)))
#'   r$kek_id
#' }
#' @keywords internal
morie_secrtt_rotate_kek <- function(wrapped_deks, old_kek, new_kek,
                                    new_nonces, new_kek_id = "kek-2",
                                    audit_log = NULL) {
  if (length(new_nonces) != length(wrapped_deks))
    stop("secrtt: ", length(wrapped_deks), " wrapped DEKs but ",
         length(new_nonces),
         " nonces -- a nonce must never be reused under a new KEK")
  out <- list()
  for (i in seq_along(wrapped_deks)) {
    w <- wrapped_deks[[i]]
    u <- morie_secrtt_unwrap_dek(w, old_kek, audit_log)
    audit_log <- u$audit_log
    out[[i]] <- morie_secrtt_wrap_dek(u$dek, new_kek,
                                        as.raw(new_nonces[[i]]),
                                        new_kek_id, w$aad %||% raw())
  }
  list(estimate = out, wrapped = out, n = length(out),
       audit_log = audit_log,
       records_reencrypted = 0L, kek_id = new_kek_id,
       method = "envelope KEK rotation; NIST SP 800-57 Part 1 Rev. 5",
       note = "one small re-wrap per record and zero record ciphertext rewritten")
}

#' Rotate a DEK: this one DOES re-encrypt the record
#'
#' @param sealed List with \code{ciphertext}, \code{nonce}, \code{tag},
#'   optional \code{aad}.
#' @param old_dek Raw DEK.
#' @param new_dek Raw DEK.
#' @param new_nonce Raw 12-byte nonce.
#' @return List with \code{sealed}, \code{records_reencrypted},
#'   \code{note}.
#' @export
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   seed <- as.raw(1:32)
#'   dek1 <- morie_secrtt_generate_dek(seed, record_id = 7L)$dek
#'   dek2 <- morie_secrtt_generate_dek(seed, record_id = 8L)$dek
#'   s <- morie_secrtt_seal_record(charToRaw("hello"), dek1,
#'                                 nonce = as.raw(13:24))
#'   r <- morie_secrtt_rotate_dek(s, dek1, dek2, new_nonce = as.raw(25:36))
#'   rawToChar(morie_secrtt_open_record(r$sealed, dek2))
#' }
#' @keywords internal
morie_secrtt_rotate_dek <- function(sealed, old_dek, new_dek,
                                    new_nonce) {
  pt <- morie_secrtt_open_record(sealed, old_dek)
  aad <- as.raw(sealed$aad %||% raw())
  list(sealed = morie_secrtt_seal_record(pt, new_dek, new_nonce, aad),
       records_reencrypted = 1L,
       note = "rotating a DEK rewrites its record; rotating the KEK does not")
}

#' Compare the bytes re-encrypted under each strategy
#'
#' @param n_records Integer number of records.
#' @param mean_record_bytes Mean record size in bytes.
#' @param dek_bytes Size of a DEK in bytes.
#' @return List with \code{single_key_bytes},
#'   \code{envelope_kek_bytes}, \code{ratio},
#'   \code{records_touched_single}, \code{records_touched_envelope},
#'   \code{note}.
#' @export
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   morie_secrtt_rotation_cost(n_records = 5L, mean_record_bytes = 5L)
#' }
#' @keywords internal
morie_secrtt_rotation_cost <- function(n_records, mean_record_bytes,
                                       dek_bytes = 32) {
  n <- as.integer(n_records)
  b <- as.numeric(mean_record_bytes)
  if (n < 1L || b <= 0)
    stop("secrtt: the record count and size must be positive")
  single <- n * b
  kek <- n * as.numeric(dek_bytes)
  list(single_key_bytes = single, envelope_kek_bytes = kek,
       ratio = if (kek > 0) single / kek else Inf,
       records_touched_single = n, records_touched_envelope = 0L,
       note = "rotation that is expensive is rotation that is deferred")
}

#' Report the scope of destroying a KEK
#'
#' A partial rotation leaves records readable under the other KEK, so
#' the scope has to be stated for "we destroyed the key" to be a
#' deletion claim.
#'
#' @param kek_id Identifier of the KEK to be destroyed.
#' @param wrapped_deks List of wrapped DEKs.
#' @return List with \code{kek_id}, \code{records_shredded},
#'   \code{indices}, \code{still_recoverable}, \code{complete},
#'   \code{note}.
#' @export
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   seed <- as.raw(1:32)
#'   dek <- morie_secrtt_generate_dek(seed, record_id = 7L)$dek
#'   kek <- as.raw(rev(1:32))
#'   w1 <- morie_secrtt_wrap_dek(dek, kek, nonce = as.raw(1:12), kek_id = "kek-1")
#'   w2 <- morie_secrtt_wrap_dek(dek, kek, nonce = as.raw(2:13), kek_id = "kek-2")
#'   morie_secrtt_crypto_shred("kek-1", list(w1, w2))
#' }
#' @keywords internal
morie_secrtt_crypto_shred <- function(kek_id, wrapped_deks) {
  ids <- vapply(wrapped_deks, function(w) w$kek_id, character(1))
  covered <- which(ids == kek_id) - 1L
  orphaned <- which(ids != kek_id) - 1L
  list(kek_id = kek_id, records_shredded = length(covered),
       indices = covered, still_recoverable = orphaned,
       complete = length(orphaned) == 0L,
       note = "any DEK wrapped under a DIFFERENT KEK survives, so a partial rotation leaves data readable")
}

# base-R `%||%` to mirror Python's "x or default"
`%||%` <- function(x, y) if (is.null(x)) y else x

# house entry point: the package exports one morie_<module>
morie_secrtt <- morie_secrtt_generate_dek

# -- restored: morie-only definition kept through the rmorie sync --
#' Crypto-shred: destroy a KEK and report what that covers
#' @param kek_id See Usage.
#' @param wrapped_deks See Usage.
#' @export
crypto_shred <- function(kek_id, wrapped_deks) {
  covered <- which(vapply(wrapped_deks,
                          function(w) identical(w$kek_id, kek_id),
                          logical(1))) - 1L
  orphaned <- which(vapply(wrapped_deks,
                           function(w) !identical(w$kek_id, kek_id),
                           logical(1))) - 1L
  list(kek_id = kek_id, records_shredded = length(covered),
       indices = covered,
       still_recoverable = orphaned,
       complete = length(orphaned) == 0L,
       note = paste("any DEK wrapped under a DIFFERENT KEK survives, ",
                    "so a partial rotation leaves data readable"))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' .secrtt_as_bytes
#'
#' A step of the secrtt_native implementation. Called by \code{generate_dek},
#' \code{open_record}, \code{rotate_dek} and 3 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Optional; may be \code{NULL}. Character; the body checks with \code{is.character}.
#' @return Nothing; this branch always raises.
#' @export
.secrtt_as_bytes <- function(x) {
  if (is.raw(x)) return(x)
  if (is.character(x)) return(charToRaw(paste(x, collapse = "")))
  if (is.null(x)) return(raw(0))
  stop("expected raw, character or NULL")
}

# -- restored: morie-only definition kept through the rmorie sync --
generate_dek <- function(master_seed, record_id, salt = NULL) {
  r <- hkdf(master_seed, salt,
            c(charToRaw("dek:"), .secrtt_as_bytes(record_id)), 32L)
  list(dek = r$okm, dek_hex = r$okm_hex,
       record_id = record_id,
       note = paste("per-record, so one leaked DEK exposes one ",
                    "record"))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Open a sealed record
#' @param sealed See Usage.
#' @param dek See Usage.
#' @export
open_record <- function(sealed, dek) {
  r <- .secrtt_aead_decrypt(dek, sealed$nonce, sealed$ciphertext,
                    sealed$tag, .secrtt_as_bytes(sealed$aad %||% raw(0)))
  if (!r$valid) {
    stop("secrtt: the record failed authentication")
  }
  r$plaintext
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Rotate a DEK: re-encrypt one record
#' @param sealed See Usage.
#' @param old_dek See Usage.
#' @param new_dek See Usage.
#' @param new_nonce See Usage.
#' @export
rotate_dek <- function(sealed, old_dek, new_dek, new_nonce) {
  pt <- open_record(sealed, old_dek)
  list(sealed = seal_record(pt, new_dek, new_nonce,
                            .secrtt_as_bytes(sealed$aad %||% raw(0))),
       records_reencrypted = 1L,
       note = paste("rotating a DEK rewrites its record; rotating ",
                    "the KEK does not"))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Rotate the KEK: re-wrap every DEK, no record ciphertext touched
#' @param wrapped_deks See Usage.
#' @param old_kek See Usage.
#' @param new_kek See Usage.
#' @param new_nonces See Usage.
#' @param new_kek_id See Usage.
#' @param audit_log See Usage.
#' @export
rotate_kek <- function(wrapped_deks, old_kek, new_kek, new_nonces,
                       new_kek_id = "kek-2", audit_log = NULL) {
  if (length(new_nonces) != length(wrapped_deks)) {
    stop("secrtt: ", length(wrapped_deks), " wrapped DEKs but ",
         length(new_nonces), " nonces -- a nonce must never be ",
         "reused under a new KEK")
  }
  out <- list()
  for (i in seq_along(wrapped_deks)) {
    w <- wrapped_deks[[i]]
    dek <- unwrap_dek(w, old_kek, audit_log)$dek
    out[[i]] <- wrap_dek(dek, new_kek, new_nonces[[i]], new_kek_id)
  }
  list(estimate = out, wrapped = out, n = length(out),
       records_reencrypted = 0, kek_id = new_kek_id,
       method = "envelope KEK rotation; NIST SP 800-57 Part 1 Rev. 5",
       note = paste("one small re-wrap per record and zero record ",
                    "ciphertext rewritten"))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Cost model: single-key vs envelope rotation
#' @param n_records See Usage.
#' @param mean_record_bytes See Usage.
#' @param dek_bytes See Usage.
#' @export
rotation_cost <- function(n_records, mean_record_bytes,
                          dek_bytes = 32L) {
  n <- as.integer(n_records)
  b <- as.numeric(mean_record_bytes)
  if (n < 1L || b <= 0) {
    stop("secrtt: the record count and size must be positive")
  }
  single <- n * b
  kek <- n * as.numeric(dek_bytes)
  list(single_key_bytes = single, envelope_kek_bytes = kek,
       ratio = if (kek > 0) single / kek else Inf,
       records_touched_single = n,
       records_touched_envelope = 0L,
       note = paste("rotation that is expensive is rotation that is ",
                    "deferred"))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Seal a record under its DEK
#' @param plaintext See Usage.
#' @param dek See Usage.
#' @param nonce See Usage.
#' @param aad See Usage.
#' @export
seal_record <- function(plaintext, dek, nonce, aad = raw(0)) {
  r <- .secrtt_aead_encrypt(dek, nonce, plaintext, aad)
  list(ciphertext = r$ciphertext, tag = r$tag,
       nonce = .secrtt_as_bytes(nonce), aad = .secrtt_as_bytes(aad))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Unwrap a DEK, optionally logging the call
#' @param wrapped See Usage.
#' @param kek See Usage.
#' @param audit_log See Usage.
#' @export
unwrap_dek <- function(wrapped, kek, audit_log = NULL) {
  r <- .secrtt_aead_decrypt(kek, wrapped$nonce, wrapped$wrapped, wrapped$tag,
                    c(.secrtt_as_bytes(wrapped$aad %||% raw(0)),
                      .secrtt_as_bytes(wrapped$kek_id)))
  if (!is.null(audit_log)) {
    audit_log[[length(audit_log) + 1L]] <- list(
      event = "unwrap", kek_id = wrapped$kek_id, ok = r$valid)
  }
  if (!r$valid) {
    stop("secrtt: the wrapped DEK failed authentication -- wrong ",
         "KEK, or it was tampered with")
  }
  list(dek = r$plaintext, kek_id = wrapped$kek_id,
       audited = !is.null(audit_log))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Wrap a DEK under a KEK
#' @param dek See Usage.
#' @param kek See Usage.
#' @param nonce See Usage.
#' @param kek_id See Usage.
#' @param aad See Usage.
#' @export
wrap_dek <- function(dek, kek, nonce, kek_id = "kek-1",
                     aad = raw(0)) {
  d <- .secrtt_as_bytes(dek)
  if (length(d) != 32L) {
    stop("secrtt: a DEK must be 32 bytes, got ", length(d))
  }
  bound <- c(.secrtt_as_bytes(aad), .secrtt_as_bytes(kek_id))
  r <- .secrtt_aead_encrypt(kek, nonce, d, bound)
  list(wrapped = r$ciphertext, tag = r$tag,
       nonce = .secrtt_as_bytes(nonce), kek_id = kek_id,
       wrapped_hex = r$ciphertext_hex,
       note = paste("the KEK id is authenticated, so a wrapped DEK ",
                    "cannot be replayed under a different KEK"))
}

# -- restored: pre-sync definition (.secrtt_aead_encrypt) --
#' @noRd
.secrtt_aead_encrypt <- function(key, nonce, plaintext, aad = raw(0)) {
  r <- morie_crypto_chacha20_poly1305_encrypt(as.raw(key), as.raw(nonce),
                                              as.raw(plaintext), as.raw(aad))
  list(ciphertext = r$ct, tag = r$tag,
       ciphertext_hex = .secrtt_hex(r$ct))
}

# -- restored: pre-sync definition (.secrtt_aead_decrypt) --
#' @noRd
.secrtt_aead_decrypt <- function(key, nonce, ciphertext, tag,
                                 aad = raw(0)) {
  pt <- tryCatch(
    morie_crypto_chacha20_poly1305_decrypt(as.raw(key), as.raw(nonce),
      c(as.raw(ciphertext), as.raw(tag)), as.raw(aad)),
    error = function(e) NULL)
  list(valid = !is.null(pt),
       plaintext = if (is.null(pt)) raw(0) else pt)
}

# -- restored: pre-sync definition (.secrtt_hex) --
#' Derive a distinct DEK per record
#' @param bs See Usage.
#' @export
.secrtt_hex <- function(bs) paste(sprintf("%02x", as.integer(bs)),
                                 collapse = "")
