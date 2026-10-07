# SPDX-License-Identifier: AGPL-3.0-or-later

# Encrypted keystore for ML-KEM key pairs.
#
# R port of morie/crypto/keystore.py, file for file: the same JSON, scrypt
# (N = 2^14, r = 8, p = 1) for the password, ChaCha20-Poly1305 for each
# secret key, so either arm opens a store the other wrote. All of it is
# native (src/morie_crypto_native_sym.cpp, rmoriebricklayer's PBKDF2 and OS
# random source); entries sealed by rmorie 1.3.x through the sodium package
# (XSalsa20-Poly1305) still open.

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
.MORIE_SCRYPT_N <- 2L^14L
.MORIE_SCRYPT_R <- 8L
.MORIE_SCRYPT_P <- 1L
.MORIE_SCRYPT_DK <- 32L

#' Internal helper: scrypt (RFC 7914)
#'
#' PBKDF2-HMAC-SHA256 (rmoriebricklayer) around the native ROMix; the
#' result equals Python's hashlib.scrypt and sodium::scrypt.
#' @noRd
.morie_scrypt <- function(password, salt, N = .MORIE_SCRYPT_N, r = .MORIE_SCRYPT_R,
                          p = .MORIE_SCRYPT_P, size = .MORIE_SCRYPT_DK) {
  B <- .morie_hex_to_raw(rmoriebricklayer::derive_key(password, salt, 1L, 128L * r * p))
  B <- .morie_scrypt_romix_impl(B, as.integer(N), as.integer(r), as.integer(p))
  .morie_hex_to_raw(rmoriebricklayer::derive_key(password, B, 1L, as.integer(size)))
}

#' Internal helper: Morie Resolve Path
#' @noRd
.morie_resolve_path <- function(path) {
  normalizePath(path.expand(path), mustWork = FALSE)
}

#' Internal helper: Morie Derive Key
#' @noRd
.morie_derive_key <- function(password, salt) {
  if (!is.raw(salt)) stop("salt must be a raw vector", call. = FALSE)
  if (!is.character(password) || length(password) != 1L || is.na(password)) {
    stop("password must be a single character string", call. = FALSE)
  }
  .morie_scrypt(charToRaw(enc2utf8(password)), salt)
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
  if (nchar(h) == 0L) {
    return(raw(0))
  }
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
  p <- .morie_resolve_path(path)
  if (!file.exists(p)) {
    stop(sprintf("Keystore not found: %s", p), call. = FALSE)
  }
  .morie_from_json(p, simplifyVector = FALSE)
}

#' Internal helper: Morie Write Store
#' @noRd
.morie_write_store <- function(data, path) {
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
#' path <- tempfile(fileext = ".keystore")
#' morie_crypto_keystore_create("open sesame", path = path)
#' print(file.exists(path))
#' unlink(path)
#' @export
morie_crypto_keystore_create <- function(password,
                                         path = .morie_keystore_default_path()) {
  p <- .morie_resolve_path(path)
  if (file.exists(p)) {
    stop(sprintf("Keystore already exists: %s", p), call. = FALSE)
  }
  salt <- rmoriebricklayer::random_bytes(16L)
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
#' set.seed(1)
#' path <- tempfile(fileext = ".keystore")
#' morie_crypto_keystore_create("pw", path = path)
#' pk <- as.raw(sample(0:255, 32, replace = TRUE))
#' sk <- as.raw(sample(0:255, 64, replace = TRUE))
#' morie_crypto_keystore_store("alice", pk = pk, sk = sk, password = "pw", path = path)
#' print(morie_crypto_keystore_list("pw", path = path))
#' unlink(path)
#' @export
morie_crypto_keystore_store <- function(name, pk, sk, password,
                                        path = .morie_keystore_default_path()) {
  if (!is.character(name) || length(name) != 1L) {
    stop("name must be a single character string", call. = FALSE)
  }
  if (!is.raw(pk) || !is.raw(sk)) {
    stop("pk and sk must be raw vectors", call. = FALSE)
  }
  store <- .morie_read_store(path)
  salt <- .morie_hex_to_raw(store$salt)
  enc_key <- .morie_derive_key(password, salt)
  nonce <- rmoriebricklayer::random_bytes(12L)
  sealed <- .morie_aead_seal_impl(enc_key, nonce, sk, raw(0))
  n <- length(sk)
  if (is.null(store$keys)) store$keys <- list()
  store$keys[[name]] <- list(
    v        = 2L, # as morie's Python store marks a pair made by 1.4.0 or later
    pk       = .morie_raw_to_hex(pk),
    sk_nonce = .morie_raw_to_hex(nonce),
    sk_ct    = .morie_raw_to_hex(sealed[seq_len(n)]),
    sk_tag   = .morie_raw_to_hex(sealed[n + seq_len(16L)])
  )
  .morie_write_store(store, path)
  invisible(NULL)
}

#' Public key of a key pair in the morie keystore
#'
#' Public keys are stored in the clear, so no password is needed
#' (encrypting to someone needs only this).
#' @param name Identifier.
#' @param path Keystore path.
#' @return Raw vector, the public key.
#' @examples
#' path <- tempfile(fileext = ".keystore")
#' morie_crypto_keystore_create("pw", path = path)
#' morie_crypto_keystore_store("alice", pk = as.raw(1:32), sk = as.raw(1:64), password = "pw", path = path)
#' identical(morie_crypto_keystore_public_key("alice", path = path), as.raw(1:32))
#' unlink(path)
#' @export
morie_crypto_keystore_public_key <- function(name, path = .morie_keystore_default_path()) {
  if (!is.character(name) || length(name) != 1L) {
    stop("name must be a single character string", call. = FALSE)
  }
  store <- .morie_read_store(path)
  if (is.null(store$keys) || is.null(store$keys[[name]])) {
    stop(sprintf("Key '%s' not found in keystore", name), call. = FALSE)
  }
  .morie_hex_to_raw(store$keys[[name]]$pk)
}

#' Load a key pair from the morie keystore
#' @param name     Identifier.
#' @param password Character scalar.
#' @param path     Keystore path.
#' @return Named list with pk (raw) and sk (raw).
#' @examples
#' set.seed(1)
#' path <- tempfile(fileext = ".keystore")
#' morie_crypto_keystore_create("pw", path = path)
#' pk <- as.raw(sample(0:255, 32, replace = TRUE))
#' sk <- as.raw(sample(0:255, 64, replace = TRUE))
#' morie_crypto_keystore_store("alice", pk = pk, sk = sk, password = "pw", path = path)
#' out <- morie_crypto_keystore_load("alice", password = "pw", path = path)
#' print(identical(out$sk, sk))
#' unlink(path)
#' @export
morie_crypto_keystore_load <- function(name, password,
                                       path = .morie_keystore_default_path()) {
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
  nonce <- .morie_hex_to_raw(entry$sk_nonce)
  sealed <- .morie_hex_to_raw(entry$sk_ct)
  sk <- if (is.null(entry$sk_tag)) {
    # rmorie 1.3.x: sodium::data_encrypt's XSalsa20-Poly1305 box, 24-byte nonce
    if (length(nonce) == 24L) .morie_secretbox_open_impl(enc_key, nonce, sealed)
  } else if (length(nonce) == 12L) {
    .morie_aead_open_impl(enc_key, nonce, c(sealed, .morie_hex_to_raw(entry$sk_tag)), raw(0))
  }
  if (is.null(sk)) {
    stop("Failed to decrypt secret key (wrong password or corrupt entry)", call. = FALSE)
  }
  pk <- .morie_hex_to_raw(entry$pk)
  list(pk = pk, sk = sk)
}

#' List key names in the morie keystore
#' @param password Character scalar.
#' @param path     Keystore path.
#' @return Character vector of identifiers.
#' @examples
#' path <- tempfile(fileext = ".keystore")
#' morie_crypto_keystore_create("pw", path = path)
#' morie_crypto_keystore_store("k1", as.raw(1:4), as.raw(5:8), "pw", path = path)
#' morie_crypto_keystore_store("k2", as.raw(1:4), as.raw(5:8), "pw", path = path)
#' print(morie_crypto_keystore_list("pw", path = path))
#' unlink(path)
#' @export
morie_crypto_keystore_list <- function(password,
                                       path = .morie_keystore_default_path()) {
  store <- .morie_read_store(path)
  salt <- .morie_hex_to_raw(store$salt)
  invisible(.morie_derive_key(password, salt))
  if (is.null(store$keys)) {
    return(character(0))
  }
  names(store$keys)
}
