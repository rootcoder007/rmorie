# SPDX-License-Identifier: AGPL-3.0-or-later

# Encrypted keystore for ML-KEM key pairs.
#
# R port of morie/crypto/keystore.py.

# CRAN policy: packages must not write outside tempdir() without
# explicit user opt-in. The keystore default path therefore resolves
# to a session-scoped tempdir() location; users who want persistent
# keys set MORIE_KEYSTORE_PATH (or pass path = ... explicitly to the
# keystore_create / load / store / wipe functions).
#' Internal helper: Morie Keystore Default Path
#' @noRd
.morie_keystore_default_path <- function() {
  override <- Sys.getenv("MORIE_KEYSTORE_PATH", "")
  if (nzchar(override)) {
    path.expand(override)
  } else {
    file.path(tempdir(), ".morie", "keys", "keystore.json")
  }
}
.MORIE_SCRYPT_N  <- 2L^14L
.MORIE_SCRYPT_R  <- 8L
.MORIE_SCRYPT_P  <- 1L
.MORIE_SCRYPT_DK <- 32L
.MORIE_SODIUM_NONCE_LEN <- 24L

#' Internal helper: Morie Keystore Require
#' @noRd
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

#' Internal helper: Morie Resolve Path
#' @noRd
.morie_resolve_path <- function(path) {
  normalizePath(path.expand(path), mustWork = FALSE)
}

#' Internal helper: Morie Derive Key
#' @noRd
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

#' Internal helper: Morie Hex To Raw
#' @noRd
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

#' Internal helper: Morie Raw To Hex
#' @noRd
.morie_raw_to_hex <- function(r) {
  if (!is.raw(r)) stop("expected raw vector", call. = FALSE)
  paste(format(r), collapse = "")
}

#' Internal helper: Morie Read Store
#' @noRd
.morie_read_store <- function(path) {
  .morie_keystore_require()
  p <- .morie_resolve_path(path)
  if (!file.exists(p)) {
    stop(sprintf("Keystore not found: %s", p), call. = FALSE)
  }
  .morie_from_json(p, simplifyVector = FALSE)
}

#' Internal helper: Morie Write Store
#' @noRd
.morie_write_store <- function(data, path) {
  .morie_keystore_require()
  p <- .morie_resolve_path(path)
  dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE)
  json <- .morie_to_json(data, pretty = TRUE, auto_unbox = TRUE)
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
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   path <- tempfile(fileext = ".keystore")
#'   morie_crypto_keystore_create("open sesame", path = path)
#'   print(file.exists(path))
#'   unlink(path)
#' }
#' @export
morie_crypto_keystore_create <- function(password,
                                         path = .morie_keystore_default_path()) {
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
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   path <- tempfile(fileext = ".keystore")
#'   morie_crypto_keystore_create("pw", path = path)
#'   pk <- as.raw(sample(0:255, 32, replace = TRUE))
#'   sk <- as.raw(sample(0:255, 64, replace = TRUE))
#'   morie_crypto_keystore_store("alice", pk = pk, sk = sk, password = "pw", path = path)
#'   print(morie_crypto_keystore_list("pw", path = path))
#'   unlink(path)
#' }
#' @export
morie_crypto_keystore_store <- function(name, pk, sk, password,
                                        path = .morie_keystore_default_path()) {
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
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   path <- tempfile(fileext = ".keystore")
#'   morie_crypto_keystore_create("pw", path = path)
#'   pk <- as.raw(sample(0:255, 32, replace = TRUE))
#'   sk <- as.raw(sample(0:255, 64, replace = TRUE))
#'   morie_crypto_keystore_store("alice", pk = pk, sk = sk, password = "pw", path = path)
#'   out <- morie_crypto_keystore_load("alice", password = "pw", path = path)
#'   print(identical(out$sk, sk))
#'   unlink(path)
#' }
#' @export
morie_crypto_keystore_load <- function(name, password,
                                       path = .morie_keystore_default_path()) {
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
#' @examples
#' if (morie_crypto_sodium_available()) {
#'   path <- tempfile(fileext = ".keystore")
#'   morie_crypto_keystore_create("pw", path = path)
#'   morie_crypto_keystore_store("k1", as.raw(1:4), as.raw(5:8), "pw", path = path)
#'   morie_crypto_keystore_store("k2", as.raw(1:4), as.raw(5:8), "pw", path = path)
#'   print(morie_crypto_keystore_list("pw", path = path))
#'   unlink(path)
#' }
#' @export
morie_crypto_keystore_list <- function(password,
                                       path = .morie_keystore_default_path()) {
  .morie_keystore_require()
  store <- .morie_read_store(path)
  salt <- .morie_hex_to_raw(store$salt)
  invisible(.morie_derive_key(password, salt))
  if (is.null(store$keys)) return(character(0))
  names(store$keys)
}
