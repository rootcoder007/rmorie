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
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   morie_crypto_liboqs_available()
#' }
#' @export
morie_crypto_liboqs_available <- function() {
  .Call(`_rmorie_morie_crypto_liboqs_available`)
}

#' liboqs runtime version string
#'
#' @return Single character (e.g. `"0.15.0"`); empty if liboqs absent.
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   if (morie_crypto_liboqs_available()) {
#'     morie_crypto_liboqs_version()
#'   }
#' }
#' @export
morie_crypto_liboqs_version <- function() {
  .Call(`_rmorie_morie_crypto_liboqs_version`)
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
#' if (morie_crypto_sodium_available()) {
#'   if (morie_crypto_liboqs_available()) {
#'     kp <- morie_crypto_mlkem768_keygen()
#'     c(pk = length(kp$pk), sk = length(kp$sk))
#'   }
#' }
#' @export
morie_crypto_mlkem768_keygen <- function() {
  if (.morie_pqc_native()) {
    k <- rmoriebricklayer::kem_keygen(768L)
    return(list(pk = .morie_pqc_h2r(k$public), sk = .morie_pqc_h2r(k$secret)))
  }
  .Call(`_rmorie_morie_crypto_mlkem768_keygen`)
}

#' ML-KEM-768 encapsulation
#'
#' Encapsulate a shared secret under a recipient's ML-KEM-768 public
#' key. Returns the ciphertext (1088 B) the sender transmits, plus
#' the 32-byte shared secret the sender holds locally.
#'
#' @param pk 1184-byte raw vector (recipient's ML-KEM-768 public key).
#' @return List with `ct` (raw, 1088 B) and `shared_secret` (raw, 32 B).
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   if (morie_crypto_liboqs_available()) {
#'     kp <- morie_crypto_mlkem768_keygen()
#'     e <- morie_crypto_mlkem768_encaps(kp$pk)
#'     length(e$ct) # 1088
#'     length(e$shared_secret) # 32
#'   }
#' }
#' @export
morie_crypto_mlkem768_encaps <- function(pk) {
  stopifnot(is.raw(pk))
  # the sizes are checked here, so both backends refuse a wrong key with the same words
  if (length(pk) != 1184L) stop("ML-KEM-768 pk must be 1184 bytes", call. = FALSE)
  if (.morie_pqc_native()) {
    key <- structure(list(public = .morie_pqc_r2h(pk), level = 768L), class = c("bricklayer_kem_public_key", "list"))
    cap <- rmoriebricklayer::kem_encapsulate(key)
    return(list(ct = .morie_pqc_h2r(cap$ciphertext), shared_secret = .morie_pqc_h2r(cap$shared)))
  }
  .Call(`_rmorie_morie_crypto_mlkem768_encaps`, pk)
}

#' ML-KEM-768 decapsulation
#'
#' Recover the shared secret from an encapsulation ciphertext using
#' the recipient's secret key.
#'
#' @param sk 2400-byte raw vector (recipient's ML-KEM-768 secret key).
#' @param ct 1088-byte raw vector (sender's encapsulation ciphertext).
#' @return Raw vector (32 B), the shared secret.
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   if (morie_crypto_liboqs_available()) {
#'     kp <- morie_crypto_mlkem768_keygen()
#'     e <- morie_crypto_mlkem768_encaps(kp$pk)
#'     ss <- morie_crypto_mlkem768_decaps(kp$sk, e$ct)
#'     print(identical(ss, e$shared_secret))
#'   }
#' }
#' @export
morie_crypto_mlkem768_decaps <- function(sk, ct) {
  .morie_arg(sk, "r")
  stopifnot(is.raw(sk), is.raw(ct))
  if (length(sk) != 2400L || length(ct) != 1088L) {
    stop("ML-KEM-768 size mismatch: sk must be 2400 bytes and ct 1088 bytes", call. = FALSE)
  }
  if (.morie_pqc_native()) {
    key <- structure(list(public = "", secret = .morie_pqc_r2h(sk), level = 768L), class = c("bricklayer_kem_key", "list"))
    return(.morie_pqc_h2r(rmoriebricklayer::kem_decapsulate(key, ct)))
  }
  .Call(`_rmorie_morie_crypto_mlkem768_decaps`, sk, ct)
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
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   if (morie_crypto_liboqs_available()) {
#'     kp <- morie_crypto_mldsa65_keygen()
#'     length(kp$pk) # 1952 (FIPS 204)
#'     length(kp$sk) # 4032
#'   }
#' }
#' @export
morie_crypto_mldsa65_keygen <- function() {
  if (.morie_pqc_native()) {
    k <- rmoriebricklayer::fips_keygen("ML-DSA-65")
    return(list(pk = .morie_pqc_h2r(k$public), sk = .morie_pqc_h2r(k$secret)))
  }
  .Call(`_rmorie_morie_crypto_mldsa65_keygen`)
}

#' ML-DSA-65 signature
#'
#' Sign a message with an ML-DSA-65 secret key. Signature length is
#' variable up to a 3309-byte ceiling (typical: ~3293 B).
#'
#' @param sk 4032-byte raw vector (signer's secret key).
#' @param message Raw vector to sign.
#' @return Raw vector signature.
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   if (morie_crypto_liboqs_available()) {
#'     kp <- morie_crypto_mldsa65_keygen()
#'     msg <- charToRaw("signed payload v1")
#'     sig <- morie_crypto_mldsa65_sign(kp$sk, msg)
#'     print(morie_crypto_mldsa65_verify(kp$pk, msg, sig))
#'   }
#' }
#' @export
morie_crypto_mldsa65_sign <- function(sk, message) {
  stopifnot(is.raw(sk), is.raw(message))
  if (length(sk) != 4032L) stop("ML-DSA-65 sk must be 4032 bytes", call. = FALSE)
  if (.morie_pqc_native()) {
    # hedged ML-DSA.Sign (FIPS 204, empty context) needs only the secret key; the public half of the
    # key object is a placeholder of the right length that signing never reads
    key <- rmoriebricklayer::fips_key("ML-DSA-65", public = raw(1952L), secret = sk)
    return(.morie_pqc_h2r(rmoriebricklayer::capsule_sign(message, key)$signature))
  }
  .Call(`_rmorie_morie_crypto_mldsa65_sign`, sk, message)
}

#' ML-DSA-65 signature verification
#'
#' @param pk 1952-byte raw vector (signer's public key).
#' @param message Raw vector that was signed.
#' @param signature Raw vector signature returned by
#'   [morie_crypto_mldsa65_sign()].
#' @return Single logical: `TRUE` if signature is valid.
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   if (morie_crypto_liboqs_available()) {
#'     kp <- morie_crypto_mldsa65_keygen()
#'     msg <- charToRaw("signed payload v1")
#'     sig <- morie_crypto_mldsa65_sign(kp$sk, msg)
#'     print(morie_crypto_mldsa65_verify(kp$pk, msg, sig))
#'   }
#' }
#' @export
morie_crypto_mldsa65_verify <- function(pk, message, signature) {
  stopifnot(is.raw(pk), is.raw(message), is.raw(signature))
  if (length(pk) != 1952L) stop("ML-DSA-65 pk must be 1952 bytes", call. = FALSE)
  if (.morie_pqc_native()) {
    key <- tryCatch(rmoriebricklayer::fips_key("ML-DSA-65", public = pk), error = function(e) NULL)
    if (is.null(key)) return(FALSE)
    sig <- structure(list(scheme = "ML-DSA-65", signature = .morie_pqc_r2h(signature), prehash = "none"),
                     class = c("bricklayer_signature", "list"))
    return(isTRUE(rmoriebricklayer::capsule_verify(message, sig, key)))
  }
  .Call(`_rmorie_morie_crypto_mldsa65_verify`, pk, message, signature)
}

#' Internal helper: ML-KEM / ML-DSA through rmoriebricklayer when rmorie was built without liboqs
#'
#' rmoriebricklayer carries its own FIPS 203 / FIPS 204 code (no system library), with the same
#' key, ciphertext and signature sizes as liboqs, so the API above works on every install.
#' @noRd
.morie_pqc_native <- function() !isTRUE(tryCatch(morie_crypto_liboqs_available(), error = function(e) FALSE))

#' @noRd
.morie_pqc_h2r <- function(h) {
  h <- as.character(h)
  if (!nzchar(h)) return(raw(0))
  as.raw(strtoi(substring(h, seq(1L, nchar(h), 2L), seq(2L, nchar(h), 2L)), 16L))
}

#' @noRd
.morie_pqc_r2h <- function(r) paste(format(as.raw(r)), collapse = "")
