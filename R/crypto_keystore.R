# SPDX-License-Identifier: AGPL-3.0-or-later

# Encrypted keystore for ML-KEM key pairs.
#
# R port of morie/crypto/keystore.py.

.MORIE_KEYSTORE_DEFAULT_PATH <- "~/.morie/keys/keystore.json"
.MORIE_SCRYPT_N  <- 2L^14L
.MORIE_SCRYPT_R  <- 8L
.MORIE_SCRYPT_P  <- 1L
.MORIE_SCRYPT_DK <- 32L
.MORIE_SODIUM_NONCE_LEN <- 24L

.morie_keystore_require <- function() {
  if (!requireNamespace("sodium", quietly = TRUE)) {
    stop("morie_crypto requires sodium; install.packages('sodium')",
         call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("morie_crypto_keystore requires jsonlite; install.packages('jsonlite')",
         call. = FALSE)
  }
}

.morie_resolve_path <- function(path) {
  normalizePath(path.expand(path), mustWork = FALSE)
}

.morie_derive_key <- function(password, salt) {
  .morie_keystore_require()
  if (!is.raw(salt)) stop("salt must be a raw vector", call. = FALSE)
  if (!is.character(password) || length(password) != 1L) {
    stop("password must be a single character string", call. = FALSE)
  }
  if (!is.null(getNamespace("sodium")$scrypt)) {
    return(sodium::scrypt(
      charToRaw(password),
      salt = salt,
      size = .MORIE_SCRYPT_DK
    ))
  }
  stop(
    "not implemented: scrypt KDF requires sodium >= 1.0.18 with ",
    "crypto_pwhash_scryptsalsa208sha256 exposed.  Tracked for v1.0.1.",
    call. = FALSE
  )
}

.morie_hex_to_raw <- function(h) {
  if (!is.character(h) || length(h) != 1L) {
    stop("expected single hex string", call. = FALSE)
  }
  if (nchar(h) %% 2L != 0L) {
    stop("hex string has odd length", call. = FALSE)
  }
  if (nchar(h) == 0L) return(raw(0))
  pairs <- substring(h, seq(1L, nchar(h), 2L), seq(2L, nchar(h), 2L))
  as.raw(strtoi(pairs, 16L))
}

.morie_raw_to_hex <- function(r) {
  if (!is.raw(r)) stop("expected raw vector", call. = FALSE)
  paste(format(r), collapse = "")
}

.morie_read_store <- function(path) {
  .morie_keystore_require()
  p <- .morie_resolve_path(path)
  if (!file.exists(p)) {
    stop(sprintf("Keystore not found: %s", p), call. = FALSE)
  }
  jsonlite::fromJSON(p, simplifyVector = FALSE)
}

.morie_write_store <- function(data, path) {
  .morie_keystore_require()
  p <- .morie_resolve_path(path)
  dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE)
  json <- jsonlite::toJSON(data, pretty = TRUE, auto_unbox = TRUE)
  con <- file(p, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(as.character(json)), con)
  Sys.chmod(p, mode = "0600", use_umask = FALSE)
  invisible(NULL)
}

#' Create a new empty morie keystore
#' @param password Character scalar: keystore password.
#' @param path     File path.
#' @return Invisibly, NULL.
#' @export
morie_crypto_keystore_create <- function(password,
                                         path = .MORIE_KEYSTORE_DEFAULT_PATH) {
  .morie_keystore_require()
  p <- .morie_resolve_path(path)
  if (file.exists(p)) {
    stop(sprintf("Keystore already exists: %s", p), call. = FALSE)
  }
  salt <- sodium::random(32L)
  invisible(.morie_derive_key(password, salt))
  store <- list(salt = .morie_raw_to_hex(salt), keys = list())
  .morie_write_store(store, path)
  invisible(NULL)
}

#' Store a key pair in the morie keystore
#' @param name     Identifier.
#' @param pk       Raw vector: public key.
#' @param sk       Raw vector: secret key.
#' @param password Character scalar.
#' @param path     Keystore path.
#' @return Invisibly, NULL.
#' @export
morie_crypto_keystore_store <- function(name, pk, sk, password,
                                        path = .MORIE_KEYSTORE_DEFAULT_PATH) {
  .morie_keystore_require()
  if (!is.character(name) || length(name) != 1L) {
    stop("name must be a single character string", call. = FALSE)
  }
  if (!is.raw(pk) || !is.raw(sk)) {
    stop("pk and sk must be raw vectors", call. = FALSE)
  }
  store <- .morie_read_store(path)
  salt <- .morie_hex_to_raw(store$salt)
  enc_key <- .morie_derive_key(password, salt)
  nonce <- sodium::random(.MORIE_SODIUM_NONCE_LEN)
  sealed <- sodium::data_encrypt(sk, key = enc_key, nonce = nonce)
  if (is.null(store$keys)) store$keys <- list()
  store$keys[[name]] <- list(
    pk       = .morie_raw_to_hex(pk),
    sk_nonce = .morie_raw_to_hex(nonce),
    sk_ct    = .morie_raw_to_hex(sealed)
  )
  .morie_write_store(store, path)
  invisible(NULL)
}

#' Load a key pair from the morie keystore
#' @param name     Identifier.
#' @param password Character scalar.
#' @param path     Keystore path.
#' @return Named list with pk (raw) and sk (raw).
#' @export
morie_crypto_keystore_load <- function(name, password,
                                       path = .MORIE_KEYSTORE_DEFAULT_PATH) {
  .morie_keystore_require()
  if (!is.character(name) || length(name) != 1L) {
    stop("name must be a single character string", call. = FALSE)
  }
  store <- .morie_read_store(path)
  if (is.null(store$keys) || is.null(store$keys[[name]])) {
    stop(sprintf("Key '%s' not found in keystore", name), call. = FALSE)
  }
  salt <- .morie_hex_to_raw(store$salt)
  enc_key <- .morie_derive_key(password, salt)
  entry <- store$keys[[name]]
  nonce  <- .morie_hex_to_raw(entry$sk_nonce)
  sealed <- .morie_hex_to_raw(entry$sk_ct)
  sk <- tryCatch(
    sodium::data_decrypt(sealed, key = enc_key, nonce = nonce),
    error = function(e) {
      stop("Failed to decrypt secret key (wrong password or corrupt entry)",
           call. = FALSE)
    }
  )
  pk <- .morie_hex_to_raw(entry$pk)
  list(pk = pk, sk = sk)
}

#' List key names in the morie keystore
#' @param password Character scalar.
#' @param path     Keystore path.
#' @return Character vector of identifiers.
#' @export
morie_crypto_keystore_list <- function(password,
                                       path = .MORIE_KEYSTORE_DEFAULT_PATH) {
  .morie_keystore_require()
  store <- .morie_read_store(path)
  salt <- .morie_hex_to_raw(store$salt)
  invisible(.morie_derive_key(password, salt))
  if (is.null(store$keys)) return(character(0))
  names(store$keys)
}
