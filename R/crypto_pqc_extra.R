# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Post-quantum cryptography beyond the lattice family (module 22
# extension). rmorie now covers all three PQC families in the
# migration literature:
#
#   * lattice     -- ML-KEM-768 (FIPS 203) + ML-DSA-65 (FIPS 204),
#                    R/crypto_pqc.R (module-LWE; via liboqs).
#   * hash-based  -- SLH-DSA-SHA2-128s (FIPS 205, SPHINCS+; liboqs)
#                    plus a dependency-free native Lamport one-time
#                    signature on the module-22 SHA-256 core.
#   * code-based  -- HQC-128 KEM (NIST 2025 fourth-round selection;
#                    liboqs).
#
# The hybrid guidance (combine PQC with classical crypto) is realized
# in R/crypto_hybrid.R: ML-KEM encapsulation + native HKDF-SHA256.

#' SLH-DSA-SHA2-128s keypair (hash-based signatures, FIPS 205)
#'
#' Hash-based signatures rest only on the security of the underlying
#' hash function — the most conservative post-quantum assumption.
#' Requires a liboqs build that includes SLH-DSA / SPHINCS+.
#'
#' @return List with `pk` (raw, 32 B) and `sk` (raw, 64 B).
#' @examples
#' if (morie_crypto_liboqs_available()) {
#'   kp <- try(morie_crypto_slhdsa_keygen(), silent = TRUE)
#' }
#' @export
morie_crypto_slhdsa_keygen <- function() {
  .Call(`_rmorie_morie_crypto_slhdsa128s_keygen`)
}

#' SLH-DSA-SHA2-128s signature
#' @param sk Secret key from \code{\link{morie_crypto_slhdsa_keygen}}.
#' @param message Raw vector or single string.
#' @return Raw signature (about 7856 B for the 128s parameter set).
#' @examples
#' if (morie_crypto_liboqs_available()) {
#'   kp <- try(morie_crypto_slhdsa_keygen(), silent = TRUE)
#'   if (!inherits(kp, "try-error")) {
#'     sig <- morie_crypto_slhdsa_sign(kp$sk, "post-quantum")
#'     print(morie_crypto_slhdsa_verify(kp$pk, "post-quantum", sig))
#'   }
#' }
#' @export
morie_crypto_slhdsa_sign <- function(sk, message) {
  stopifnot(is.raw(sk))
  if (is.character(message)) message <- charToRaw(message)
  .Call(`_rmorie_morie_crypto_slhdsa128s_sign`, sk, message)
}

#' SLH-DSA-SHA2-128s verification
#' @param pk Public key.
#' @param message Raw vector or single string.
#' @param signature Raw signature.
#' @return TRUE if the signature verifies.
#' @examples
#' if (morie_crypto_liboqs_available()) {
#'   kp <- try(morie_crypto_slhdsa_keygen(), silent = TRUE)
#'   if (!inherits(kp, "try-error")) {
#'     sig <- morie_crypto_slhdsa_sign(kp$sk, "post-quantum")
#'     print(morie_crypto_slhdsa_verify(kp$pk, "post-quantum", sig))
#'     print(morie_crypto_slhdsa_verify(kp$pk, "tampered", sig))
#'   }
#' }
#' @export
morie_crypto_slhdsa_verify <- function(pk, message, signature) {
  stopifnot(is.raw(pk), is.raw(signature))
  if (is.character(message)) message <- charToRaw(message)
  .Call(`_rmorie_morie_crypto_slhdsa128s_verify`, pk, message, signature)
}

#' HQC-128 keypair (code-based KEM)
#'
#' Code-based cryptography (decoding random linear codes) is the
#' third PQC family; HQC is NIST's 2025 fourth-round KEM selection,
#' standardized as the backup to ML-KEM precisely so that a lattice
#' break does not strand deployments. Requires liboqs with HQC.
#'
#' @return List with `pk` and `sk` raw vectors.
#' @examples
#' if (morie_crypto_liboqs_available()) {
#'   kp <- try(morie_crypto_hqc_keygen(), silent = TRUE)
#'   if (!inherits(kp, "try-error")) {
#'     length(kp$pk)
#'     length(kp$sk)
#'   }
#' }
#' @export
morie_crypto_hqc_keygen <- function() {
  .Call(`_rmorie_morie_crypto_hqc128_keygen`)
}

#' HQC-128 encapsulation
#' @param pk Recipient's HQC-128 public key.
#' @return List with `ct` and `shared_secret` (raw, 64 B).
#' @examples
#' if (morie_crypto_liboqs_available()) {
#'   kp <- try(morie_crypto_hqc_keygen(), silent = TRUE)
#'   if (!inherits(kp, "try-error")) {
#'     enc <- morie_crypto_hqc_encaps(kp$pk)
#'     str(enc)
#'   }
#' }
#' @export
morie_crypto_hqc_encaps <- function(pk) {
  stopifnot(is.raw(pk))
  .Call(`_rmorie_morie_crypto_hqc128_encaps`, pk)
}

#' HQC-128 decapsulation
#' @param sk Secret key.
#' @param ct Ciphertext from \code{\link{morie_crypto_hqc_encaps}}.
#' @return Raw shared secret.
#' @examples
#' if (morie_crypto_liboqs_available()) {
#'   kp <- try(morie_crypto_hqc_keygen(), silent = TRUE)
#'   if (!inherits(kp, "try-error")) {
#'     enc <- morie_crypto_hqc_encaps(kp$pk)
#'     ss <- morie_crypto_hqc_decaps(kp$sk, enc$ct)
#'     print(identical(ss, enc$shared_secret))
#'   }
#' }
#' @export
morie_crypto_hqc_decaps <- function(sk, ct) {
  stopifnot(is.raw(sk), is.raw(ct))
  .Call(`_rmorie_morie_crypto_hqc128_decaps`, sk, ct)
}

# ---------------------------------------------------------------------------
# Native Lamport one-time signatures (hash-based, no liboqs needed)
# ---------------------------------------------------------------------------

#' Lamport one-time signature keypair (native, SHA-256)
#'
#' The textbook hash-based signature (Lamport 1979): 256 secret pairs
#' of 32 random bytes; the public key is their SHA-256 images.
#' Security reduces entirely to SHA-256 preimage resistance — a
#' post-quantum assumption — but each keypair signs EXACTLY ONE
#' message. Signing twice with the same key leaks enough secrets to
#' forge; \code{\link{morie_crypto_lamport_sign}} therefore refuses a
#' key it has already used in this session. For many-time hash-based
#' signatures use \code{\link{morie_crypto_slhdsa_keygen}} (SLH-DSA).
#'
#' @return An object of class \code{morie_lamport_keypair}: list with
#'   \code{pk} (2 x 256 matrix of hex strings) and \code{sk}
#'   (2 x 256 list matrix of raw secrets).
#' @references Lamport, L. (1979). Constructing digital signatures
#'   from a one-way function. SRI CSL-98.
#' @examplesIf isTRUE(tryCatch(morie_crypto_sodium_available(), error = function(e) FALSE))
#' kp <- morie_crypto_lamport_keygen()
#' sig <- morie_crypto_lamport_sign(kp, "hello")
#' morie_crypto_lamport_verify(kp$pk, "hello", sig)
#' @export
morie_crypto_lamport_keygen <- function() {
  sk <- matrix(vector("list", 512L), nrow = 2L)
  pk <- matrix(character(512L), nrow = 2L)
  for (b in 1:2) {
    for (i in 1:256) {
      s <- morie_crypto_random_bytes(32L)
      sk[[b, i]] <- s
      pk[b, i] <- .rmorie_sha256_hex_impl(s)
    }
  }
  structure(list(pk = pk, sk = sk, used = new.env(parent = emptyenv())),
            class = c("morie_lamport_keypair", "list"))
}

#' Sign one message with a Lamport keypair (native)
#'
#' @param keypair From \code{\link{morie_crypto_lamport_keygen}}.
#' @param message Raw vector or single string.
#' @return An object of class \code{morie_lamport_signature}: the 256
#'   revealed 32-byte secrets (list) plus the message digest.
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   kp <- morie_crypto_lamport_keygen()
#'   sig <- morie_crypto_lamport_sign(kp, "the die is cast")
#'   print(morie_crypto_lamport_verify(kp$pk, "the die is cast", sig))
#' }
#' @export
morie_crypto_lamport_sign <- function(keypair, message) {
  stopifnot(inherits(keypair, "morie_lamport_keypair"))
  if (isTRUE(keypair$used$signed)) {
    stop("This Lamport keypair has already signed a message; one-time ",
         "keys must never be reused (reuse forfeits security). ",
         "Generate a fresh keypair, or use SLH-DSA for many-time ",
         "signatures.", call. = FALSE)
  }
  if (is.character(message)) message <- charToRaw(message)
  dg <- .rmorie_sha256_impl(message)
  bits <- as.integer(rawToBits(dg))       # 256 bits, LSB-first per byte
  reveal <- vector("list", 256L)
  for (i in 1:256) reveal[[i]] <- keypair$sk[[bits[i] + 1L, i]]
  keypair$used$signed <- TRUE
  structure(list(reveal = reveal, digest_hex = .rmorie_sha256_hex_impl(message)),
            class = c("morie_lamport_signature", "list"))
}

#' Verify a Lamport one-time signature (native)
#'
#' @param pk The 2 x 256 hex public-key matrix.
#' @param message Raw vector or single string.
#' @param signature From \code{\link{morie_crypto_lamport_sign}}.
#' @return TRUE if every revealed secret hashes to the committed
#'   public value selected by the message digest bits.
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   kp <- morie_crypto_lamport_keygen()
#'   sig <- morie_crypto_lamport_sign(kp, "the die is cast")
#'   print(morie_crypto_lamport_verify(kp$pk, "the die is cast", sig))
#'   print(morie_crypto_lamport_verify(kp$pk, "tampered message", sig))
#' }
#' @export
morie_crypto_lamport_verify <- function(pk, message, signature) {
  stopifnot(inherits(signature, "morie_lamport_signature"))
  if (is.character(message)) message <- charToRaw(message)
  dg <- .rmorie_sha256_impl(message)
  if (!identical(.rmorie_sha256_hex_impl(message), signature$digest_hex)) {
    return(FALSE)
  }
  bits <- as.integer(rawToBits(dg))
  for (i in 1:256) {
    if (!identical(.rmorie_sha256_hex_impl(signature$reveal[[i]]),
                   pk[bits[i] + 1L, i])) {
      return(FALSE)
    }
  }
  TRUE
}

#' Inventory of the post-quantum families available in this build
#'
#' @return Data frame with one row per primitive: family (lattice /
#'   hash-based / code-based), primitive, standard, and whether the
#'   current build provides it.
#' @examples
#' morie_crypto_pqc_inventory()
#' @export
morie_crypto_pqc_inventory <- function() {
  oqs <- isTRUE(tryCatch(morie_crypto_liboqs_available(),
                         error = function(e) FALSE))
  has_alg <- function(fn) {
    oqs && !inherits(tryCatch(fn(), error = function(e) e), "error")
  }
  data.frame(
    family = c("lattice", "lattice", "hash-based", "hash-based",
               "code-based"),
    primitive = c("ML-KEM-768 (Kyber)", "ML-DSA-65 (Dilithium)",
                  "SLH-DSA-SHA2-128s (SPHINCS+)",
                  "Lamport OTS (native SHA-256)", "HQC-128"),
    standard = c("FIPS 203", "FIPS 204", "FIPS 205",
                 "Lamport 1979", "NIST round-4 (2025)"),
    available = c(oqs, oqs,
                  has_alg(morie_crypto_slhdsa_keygen),
                  !inherits(tryCatch(morie_crypto_random_bytes(1L),
                                     error = function(e) e), "error"),
                  has_alg(morie_crypto_hqc_keygen)),
    stringsAsFactors = FALSE
  )
}
