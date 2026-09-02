# SPDX-License-Identifier: AGPL-3.0-or-later
#
# The digest package's public surface, natively: digest(), hmac(),
# makeRaw(), digest2int(), sha1() with its class methods, AES(). The hash
# kernels live in src/morie_digest.cpp (MD5, SHA-1, SHA-224/256/384/512,
# CRC-32, CRC-32C, xxHash32/64, MurmurHash3, Jenkins one-at-a-time, AES).
# Semantics follow digest 0.6.39 exactly -- serialize() with the 14-byte
# header skipped, the same option names and defaults, hex or raw output,
# seeds for the seeded hashes -- and tests/testthat/test-digest-parity.R
# pins every algorithm to digest's own output whenever digest is
# installed, plus the published test vectors.
#
# Not provided (would need a third-party implementation, not a
# specification): spookyhash, blake3, xxh3_64, xxh3_128. Requesting them
# is an error that says so.

.MORIE_DIGEST_ALGOS <- c(md5 = 1L, sha1 = 2L, crc32 = 3L, sha256 = 4L, sha512 = 5L,
                         xxhash32 = 6L, xxhash64 = 7L, murmur32 = 8L, crc32c = 11L,
                         sha224 = 21L, sha384 = 22L)
.MORIE_DIGEST_MISSING <- c("spookyhash", "blake3", "xxh3_64", "xxh3_128")
# sha1() tags objects with attributes named after the digest package; the
# names are part of what gets hashed, so they must match digest byte for byte
# (they are strings, not calls into digest).
.MORIE_SHA1_ATTR <- paste0("digest", "::", "sha1")
.MORIE_SHA1_ATTRS <- paste0("digest", "::", "attributes")

#' @noRd
.morie_digest_hex <- function(r) paste(sprintf("%02x", as.integer(r)), collapse = "")

#' Hash an R object, string, raw vector or file (digest's digest, natively)
#'
#' Same contract as `digest::digest()`: by default the object is
#' `serialize()`d (version 2, the 14-byte header skipped so the hash does
#' not depend on the R version), `serialize = FALSE` hashes a character
#' string or raw vector as-is, `file = TRUE` hashes a file's bytes, `length`
#' and `skip` window the input, `raw = TRUE` returns the digest bytes, and
#' `seed` feeds xxhash32 / xxhash64 / murmur32.
#'
#' @param object the object, string, raw vector, or (with `file`) path.
#' @param algo one of md5, sha1, crc32, sha256, sha512, xxhash32, xxhash64,
#'   murmur32, crc32c, sha224, sha384.
#' @param serialize,file,length,skip,ascii,raw,seed,errormode,serializeVersion as in digest.
#' @return the hex digest (character), or a raw vector when `raw = TRUE`.
#' @examples
#' morie_digest("abc", algo = "sha256", serialize = FALSE)
#' morie_digest(1:10)
#' morie_digest(mtcars, algo = "xxhash64")
#' @export
morie_digest <- function(object, algo = c("md5", "sha1", "crc32", "sha256", "sha512",
                                          "xxhash32", "xxhash64", "murmur32", "crc32c",
                                          "sha224", "sha384"),
                         serialize = TRUE, file = FALSE, length = Inf, skip = "auto",
                         ascii = FALSE, raw = FALSE, seed = 0,
                         errormode = c("stop", "warn", "silent"),
                         serializeVersion = getOption("serializeVersion", 2L)) {
  errormode <- match.arg(errormode)
  fail <- function(...) {
    msg <- paste0(...)
    if (errormode == "stop") stop(msg, call. = FALSE)
    if (errormode == "warn") {
      warning(msg, call. = FALSE)
      return(invisible(NA))
    }
    invisible(NULL)
  }
  algo <- algo[1L]
  if (algo %in% .MORIE_DIGEST_MISSING)
    return(fail("algorithm '", algo, "' is not implemented natively (no specification to ",
                "implement from); use one of: ", paste(names(.MORIE_DIGEST_ALGOS), collapse = ", ")))
  if (!(algo %in% names(.MORIE_DIGEST_ALGOS)))
    return(fail("unknown algorithm '", algo, "'"))
  code <- .MORIE_DIGEST_ALGOS[[algo]]
  if (is.character(file) && missing(object)) {
    object <- file
    file <- TRUE
  }
  if (is.infinite(length)) length <- -1L
  if (serialize && !file) {
    object <- serialize(object, connection = NULL, ascii = ascii, version = serializeVersion)
    if (is.character(skip) && skip == "auto")
      skip <- if (!ascii) 14L else which(object[1:30] == as.raw(10))[4]
  } else if (!is.character(object) && !is.raw(object)) {
    return(fail("Argument object must be of type character or raw vector if serialize is FALSE"))
  }
  if (file && !is.character(object))
    return(fail("file=TRUE can only be used with a character object"))
  if (is.character(skip)) skip <- 0L
  skip <- as.integer(skip)
  length <- as.integer(length)
  bytes <- if (file) {
    path <- path.expand(object)
    if (!file.exists(path)) return(fail("The file does not exist: ", path))
    if (isTRUE(file.info(path)$isdir)) return(fail("The specified pathname is not a file: ", path))
    if (file.access(path, 4) != 0) return(fail("The specified file is not readable: ", path))
    readBin(path, "raw", n = file.info(path)$size)
  } else if (is.character(object)) {
    charToRaw(enc2utf8(object[1L]))
  } else {
    object
  }
  if (skip > 0L) bytes <- if (skip >= length(bytes)) raw(0) else bytes[-seq_len(skip)]
  if (length >= 0L && length < length(bytes)) bytes <- bytes[seq_len(length)]
  val <- .rmorie_digest_impl(bytes, code, as.numeric(seed))
  if (isTRUE(raw)) val else .morie_digest_hex(val)
}

#' Coerce to raw the way digest::makeRaw does
#' @param object raw, character, hex digest (class `digest`), or numeric bytes.
#' @return a raw vector.
#' @export
morie_make_raw <- function(object) {
  if (is.raw(object)) return(object)
  if (inherits(object, "digest") || (is.character(object) && length(object) == 1L &&
                                     grepl("^[0-9a-fA-F]+$", object) && nchar(object) %% 2L == 0L &&
                                     nchar(object) >= 8L && inherits(object, "digest")))
    return(as.raw(strtoi(substring(object, seq(1L, nchar(object), 2L), seq(2L, nchar(object), 2L)), 16L)))
  if (is.character(object)) return(charToRaw(object))
  as.raw(object)
}
#' @noRd
.morie_digest_hex_to_raw <- function(h) as.raw(strtoi(substring(h, seq(1L, nchar(h), 2L), seq(2L, nchar(h), 2L)), 16L))

#' HMAC (digest's hmac, natively)
#'
#' @param key,object key and message (character, raw, or anything
#'   `morie_make_raw()` accepts).
#' @param algo md5, sha1, crc32, sha256, sha512 (digest's set), plus sha224 / sha384.
#' @param serialize,raw,... as in digest.
#' @return hex digest, or raw with `raw = TRUE`.
#' @examples
#' morie_hmac("key", "The quick brown fox jumps over the lazy dog", algo = "sha256")
#' @export
morie_hmac <- function(key, object, algo = c("md5", "sha1", "crc32", "sha256", "sha512", "sha224", "sha384"),
                       serialize = FALSE, raw = FALSE, ...) {
  algo <- match.arg(algo)
  blocksize <- switch(algo, crc32 = 4L, sha512 = , sha384 = 128L, 64L)
  k <- morie_make_raw(key)
  if (length(k) > blocksize) {
    k <- morie_digest(k, algo = algo, serialize = FALSE, raw = TRUE)
    if (algo == "crc32") {
      # digest::hmac's own (odd) crc32 key folding, reproduced literally
      k <- substring(k, seq(1, 7, 2), seq(2, 8, 2))
      k <- suppressWarnings(morie_make_raw(strtoi(k, 16)))
    }
  }
  padded <- c(k, raw(blocksize - length(k)))
  ikey <- xor(padded, as.raw(rep(0x36, blocksize)))
  inner <- morie_digest(c(ikey, morie_make_raw(object)), algo = algo, serialize = serialize, ...)
  okey <- xor(padded, as.raw(rep(0x5c, blocksize)))
  result <- morie_digest(c(okey, .morie_digest_hex_to_raw(inner)), algo = algo, serialize = serialize, ...)
  if (raw) .morie_digest_hex_to_raw(result) else result
}

#' Bob Jenkins' one-at-a-time hash of strings to integers (digest's digest2int)
#' @param x character vector.
#' @param seed integer seed.
#' @return an integer vector.
#' @examples
#' morie_digest2int(c("abc", "def"))
#' @export
morie_digest2int <- function(x, seed = 0L) {
  if (!is.character(x)) stop("invalid input - should be character vector", call. = FALSE)
  .rmorie_digest2int_impl(x, as.integer(seed))
}

# ---------------------------------------------------------------- sha1()

#' Hash R objects with numeric rounding (digest's sha1 generic, natively)
#'
#' Doubles are written as hexadecimal mantissa/exponent with `digits`
#' significant digits and `zapsmall` small-value flooring, so that the
#' hash is the same across platforms; every method and attribute rule of
#' `digest::sha1()` is reproduced.
#'
#' @param x the object.
#' @param digits,zapsmall,algo,... as in digest.
#' @return the hex digest.
#' @examples
#' morie_sha1(c(1.1, 2.2, pi))
#' morie_sha1(data.frame(a = 1:3, b = c("x", "y", "z")))
#' @export
morie_sha1 <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") UseMethod("morie_sha1")

#' @noRd
.morie_num2hex <- function(x, digits = 14L, zapsmall = 7L) {
  if (!is.numeric(x)) stop("x is not numeric", call. = FALSE)
  digits <- as.integer(digits)
  zapsmall <- as.integer(zapsmall)
  if (length(digits) != 1L || digits < 1L) stop("digits must be one positive integer", call. = FALSE)
  if (length(zapsmall) != 1L || zapsmall < 1L) stop("zapsmall must be one positive integer", call. = FALSE)
  if (length(x) == 0L) return(character(0))
  x.na <- is.na(x)
  if (all(x.na)) return(x)
  x.inf <- is.infinite(x)
  output <- rep(NA_character_, length(x))
  output[x.inf & x > 0] <- "Inf"
  output[x.inf & x < 0] <- "-Inf"
  x.zero <- !x.na & !x.inf & abs(x) <= (2^floor(log2(10^-zapsmall)))
  output[x.zero] <- "0"
  x.finite <- !(x.na | x.inf | x.zero)
  if (!any(x.finite)) return(output)
  x_abs <- abs(x[x.finite])
  exponent <- floor(log2(x_abs))
  negative <- c("", "-")[(x[x.finite] < 0) + 1]
  x.hex <- sprintf("%a", x_abs * 2^-exponent)
  nc_x <- nchar(x.hex)
  digits.hex <- ceiling(log(10^digits, base = 16))
  mask_decimal <- startsWith(x.hex, "0x1.")
  start_character <- 4 + mask_decimal
  stop_character <- pmin(nc_x - 3, start_character + digits.hex - 1)
  mantissa <- substring(x.hex, start_character, stop_character)
  mantissa <- gsub(x = mantissa, pattern = "0*$", replacement = "")
  output[x.finite] <- sprintf("%s%s %d", negative, mantissa, exponent)
  output
}
#' @noRd
.morie_attr_sha1 <- function(x, digits, zapsmall, algo, ...) {
  if (algo == "sha1") return(list(class = class(x), digits = as.integer(digits), zapsmall = as.integer(zapsmall), ...))
  list(class = class(x), digits = as.integer(digits), zapsmall = as.integer(zapsmall), algo = algo, ...)
}
#' @noRd
.morie_sha1_add_attributes <- function(x, y) {
  extra <- attributes(x)
  extra <- extra[names(extra) != "srcref"]
  # digest does c(attributes(y), "digest::attributes" = extra): c() splices
  # the list and prefixes each element name with the argument name
  if (length(extra)) {
    names(extra) <- paste0(.MORIE_SHA1_ATTRS, ".", names(extra))
    attributes(y) <- c(attributes(y), extra)
  }
  y
}
#' @noRd
.morie_sha1_attr_digest <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
  attr(x, .MORIE_SHA1_ATTR) <- .morie_attr_sha1(x = x, digits = digits, zapsmall = zapsmall, algo = algo, ...)
  morie_digest(x, algo = algo)
}
#' @export
morie_sha1.default <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
  if (is.list(x)) return(morie_sha1.list(x, digits = digits, zapsmall = zapsmall, ..., algo = algo))
  warning("morie_sha1() has no method for the '", paste(class(x), collapse = "', '"),
          "' class, so using fallback.", call. = FALSE)
  .morie_sha1_attr_digest(x = x, digits = digits, zapsmall = zapsmall, ..., algo = algo)
}
#' @export
morie_sha1.numeric <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
  y <- .morie_num2hex(x, digits = digits, zapsmall = zapsmall)
  y <- .morie_sha1_add_attributes(x, y)
  attr(y, .MORIE_SHA1_ATTR) <- .morie_attr_sha1(x = x, digits = digits, zapsmall = zapsmall, algo = algo, ...)
  morie_digest(y, algo = algo)
}
#' @export
morie_sha1.matrix <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
  if (storage.mode(x) == "double") {
    y <- matrix(apply(x, 2, .morie_num2hex, digits = digits, zapsmall = zapsmall), ncol = ncol(x))
    y <- .morie_sha1_add_attributes(x, y)
    attr(y, .MORIE_SHA1_ATTR) <- .morie_attr_sha1(x = x, digits = digits, zapsmall = zapsmall, algo = algo, ...)
    return(morie_digest(y, algo = algo))
  }
  attr(x, .MORIE_SHA1_ATTR) <- .morie_attr_sha1(x = x, digits = digits, zapsmall = zapsmall, algo = algo, ...)
  morie_digest(x, algo = algo)
}
#' @export
morie_sha1.complex <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
  y <- cbind(Re(x), Im(x))
  y <- .morie_sha1_add_attributes(x, y)
  attr(y, .MORIE_SHA1_ATTR) <- .morie_attr_sha1(x = x, digits = digits, zapsmall = zapsmall, algo = algo, ...)
  morie_sha1(y, digits = digits, zapsmall = zapsmall, ..., algo = algo)
}
#' @export
morie_sha1.Date <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
  y <- as.numeric(x)
  y <- .morie_sha1_add_attributes(x, y)
  attr(y, .MORIE_SHA1_ATTR) <- .morie_attr_sha1(x = x, digits = digits, zapsmall = zapsmall, algo = algo, ...)
  morie_sha1(y, digits = digits, zapsmall = zapsmall, ..., algo = algo)
}
#' @export
morie_sha1.array <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
  y <- list(dimension = dim(x), value = as.numeric(x))
  y <- .morie_sha1_add_attributes(x, y)
  attr(y, .MORIE_SHA1_ATTR) <- .morie_attr_sha1(x = x, digits = digits, zapsmall = zapsmall, algo = algo, ...)
  morie_sha1(y, digits = digits, zapsmall = zapsmall, ..., algo = algo)
}
#' @export
morie_sha1.data.frame <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
  y <- if (length(x)) vapply(x, morie_sha1, digits = digits, zapsmall = zapsmall, ..., algo = algo, FUN.VALUE = NA_character_) else x
  y <- .morie_sha1_add_attributes(x, y)
  attr(y, .MORIE_SHA1_ATTR) <- .morie_attr_sha1(x = x, digits = digits, zapsmall = zapsmall, algo = algo, ...)
  morie_digest(y, algo = algo)
}
#' @export
morie_sha1.list <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
  y <- if (length(x)) vapply(x, morie_sha1, digits = digits, zapsmall = zapsmall, ..., algo = algo, FUN.VALUE = NA_character_) else x
  y <- .morie_sha1_add_attributes(x, y)
  attr(y, .MORIE_SHA1_ATTR) <- list(class = class(x), digits = as.integer(digits), zapsmall = as.integer(zapsmall), ... = ...)
  morie_digest(y, algo = algo)
}
#' @export
morie_sha1.pairlist <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
  y <- vapply(x, morie_sha1, digits = digits, zapsmall = zapsmall, ..., algo = algo, FUN.VALUE = NA_character_)
  y <- .morie_sha1_add_attributes(x, y)
  attr(y, .MORIE_SHA1_ATTR) <- .morie_attr_sha1(x = x, digits = digits, zapsmall = zapsmall, algo = algo, ...)
  morie_digest(y, algo = algo)
}
#' @export
morie_sha1.POSIXlt <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
  y <- do.call(data.frame, lapply(unclass(as.POSIXlt(x)), unlist))
  y$sec <- .morie_num2hex(y$sec, digits = digits, zapsmall = zapsmall)
  y <- .morie_sha1_add_attributes(x, y)
  attr(y, .MORIE_SHA1_ATTR) <- .morie_attr_sha1(x = x, digits = digits, zapsmall = zapsmall, algo = algo, ...)
  morie_digest(y, algo = algo)
}
#' @export
morie_sha1.POSIXct <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
  y <- morie_sha1(as.POSIXlt(x), digits = digits, zapsmall = zapsmall, ..., algo = algo)
  y <- .morie_sha1_add_attributes(x, y)
  attr(y, .MORIE_SHA1_ATTR) <- .morie_attr_sha1(x = x, digits = digits, zapsmall = zapsmall, algo = algo, ...)
  morie_digest(y, algo = algo)
}
#' @export
morie_sha1.anova <- function(x, digits = 4L, zapsmall = 7L, ..., algo = "sha1") {
  y <- apply(x, 1, .morie_num2hex, digits = digits, zapsmall = zapsmall)
  y <- .morie_sha1_add_attributes(x, y)
  attr(y, .MORIE_SHA1_ATTR) <- .morie_attr_sha1(x = x, digits = digits, zapsmall = zapsmall, algo = algo, ...)
  morie_digest(y, algo = algo)
}
#' @export
morie_sha1.function <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
  dots <- list(...)
  if (is.null(dots$environment)) dots$environment <- TRUE
  y <- if (isTRUE(dots$environment)) {
    list(formals = formals(x), body = as.character(body(x)), environment = morie_digest(environment(x), algo = algo))
  } else {
    list(formals = formals(x), body = as.character(body(x)))
  }
  y <- vapply(y, morie_sha1, digits = digits, zapsmall = zapsmall, environment = dots$environment,
              ... = dots, algo = algo, FUN.VALUE = NA_character_)
  y <- .morie_sha1_add_attributes(x, y)
  attr(y, .MORIE_SHA1_ATTR) <- .morie_attr_sha1(x = y, digits = digits, zapsmall = zapsmall, algo = algo, dots)
  morie_digest(y, algo = algo)
}
#' @export
morie_sha1.formula <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
  dots <- list(...)
  if (is.null(dots$environment)) dots$environment <- TRUE
  y <- vapply(x, morie_sha1, digits = digits, zapsmall = zapsmall, ... = dots, algo = algo, FUN.VALUE = NA_character_)
  if (isTRUE(dots$environment)) y <- c(y, morie_digest(environment(x), algo = algo))
  y <- .morie_sha1_add_attributes(x, y)
  attr(y, .MORIE_SHA1_ATTR) <- .morie_attr_sha1(x = x, digits = digits, zapsmall = zapsmall, algo = algo, ...)
  morie_digest(y, algo = algo)
}
#' @export
`morie_sha1.(` <- function(...) morie_sha1.formula(...)
#' @export
morie_sha1.NULL <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") morie_digest(x, algo = algo)
#' @export
morie_sha1.name <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") morie_digest(x, algo = algo)
#' @export
morie_sha1.call <- function(...) .morie_sha1_attr_digest(...)
#' @export
morie_sha1.character <- function(...) .morie_sha1_attr_digest(...)
#' @export
morie_sha1.factor <- function(...) .morie_sha1_attr_digest(...)
#' @export
morie_sha1.logical <- function(...) .morie_sha1_attr_digest(...)
#' @export
morie_sha1.integer <- function(...) .morie_sha1_attr_digest(...)
#' @export
morie_sha1.raw <- function(...) .morie_sha1_attr_digest(...)
#' @export
morie_sha1.environment <- function(...) .morie_sha1_attr_digest(...)

# ---------------------------------------------------------------- AES

#' AES block cipher object (digest's AES, natively)
#'
#' Returns the same closure object as `digest::AES()`: `$encrypt(text)`,
#' `$decrypt(ciphertext, raw = FALSE)`, `$IV()`, `$block_size()`,
#' `$key_size()`, `$mode()`. ECB, CBC (optionally PKCS#7-padded), CFB and
#' CTR modes over AES-128/192/256.
#'
#' @param key raw key of 16, 24 or 32 bytes.
#' @param mode "ECB", "CBC", "CFB" or "CTR".
#' @param IV raw initialisation vector (16 bytes) for the chained modes.
#' @param padding pad CBC input to whole blocks.
#' @return an object of class `AES`.
#' @examples
#' key <- as.raw(1:16)
#' aes <- morie_aes(key, mode = "ECB")
#' ct <- aes$encrypt(charToRaw("0123456789abcdef"))
#' aes$decrypt(ct)
#' @export
morie_aes <- function(key, mode = c("ECB", "CBC", "CFB", "CTR"), IV = NULL, padding = FALSE) {
  modes <- c("ECB", "CBC", "CFB", "PGP", "OFB", "CTR", "OPENPGP")
  mode <- match(match.arg(mode), modes)
  if (padding && mode != 2L) stop("Only CBC mode supports padding", call. = FALSE)
  key <- as.raw(key)
  IV <- as.raw(IV)
  if (!(length(key) %in% c(16L, 24L, 32L))) stop("AES only supports 16, 24 and 32 byte keys", call. = FALSE)
  block_size <- 16L
  key_size <- length(key)
  ecb <- function(text, encrypt) .rmorie_aes_ecb_impl(key, text, encrypt)
  encrypt <- function(text) {
    if (typeof(text) == "character") text <- charToRaw(text)
    if (mode == 1L) return(ecb(text, TRUE))
    if (mode == 2L) {
      if (padding) {
        bytes <- block_size - (length(text) %% block_size)
        text <- c(text, rep(as.raw(bytes), times = bytes))
      }
      len <- length(text)
      if (len %% block_size != 0L)
        stop("Text length must be a multiple of ", block_size, " bytes, or use `padding=TRUE`", call. = FALSE)
      result <- raw(len)
      for (i in seq_len(len / block_size)) {
        ind <- (i - 1L) * block_size + seq(block_size)
        IV <<- ecb(xor(text[ind], IV), TRUE)
        result[ind] <- IV
      }
      return(result)
    }
    if (mode == 3L) {
      if (length(IV) != block_size) stop("IV length must equal block size", call. = FALSE)
      result <- raw(length(text))
      blocks <- split(text, ceiling(seq_along(text) / block_size))
      i <- 0L
      for (b in blocks) {
        out <- ecb(IV, TRUE)
        len <- length(b)
        ind <- i * length(IV) + 1:len
        i <- i + 1L
        result[ind] <- xor(b, out[0:len])
        IV <<- result[ind]
      }
      return(result)
    }
    len <- length(text)
    blocks <- (len + 15L) %/% 16L
    result <- raw(16L * blocks)
    for (i in seq_len(blocks)) {
      result[16L * (i - 1L) + 1:16] <- IV
      byte <- 16L
      repeat {
        IV[byte] <<- as.raw((as.integer(IV[byte]) + 1L) %% 256L)
        if (IV[byte] != as.raw(0) || byte == 1L) break
        byte <- byte - 1L
      }
    }
    result <- ecb(result, TRUE)
    length(result) <- len
    xor(text, result)
  }
  decrypt <- function(ciphertext, raw = FALSE) {
    if (mode == 1L) {
      result <- ecb(ciphertext, FALSE)
    } else if (mode == 2L) {
      len <- length(ciphertext)
      if (len %% 16L != 0L) stop("Ciphertext length must be a multiple of 16 bytes", call. = FALSE)
      result <- raw(len)
      for (i in seq_len(len / 16L)) {
        ind <- (i - 1L) * 16L + 1:16
        res <- ecb(ciphertext[ind], FALSE)
        result[ind] <- xor(res, IV)
        IV <<- ciphertext[ind]
      }
      if (padding) {
        bytes_to_remove <- as.integer(utils::tail(result, 1))
        result <- utils::head(result, -bytes_to_remove)
      }
    } else if (mode == 3L) {
      if (length(IV) != block_size) stop("IV length must equal block size", call. = FALSE)
      result <- raw(length(ciphertext))
      blocks <- split(ciphertext, ceiling(seq_along(ciphertext) / block_size))
      i <- 0L
      for (b in blocks) {
        out <- ecb(IV, TRUE)
        len <- length(b)
        ind <- i * length(IV) + 1:len
        i <- i + 1L
        result[ind] <- xor(b, out[0:len])
        IV <<- b
      }
    } else {
      result <- encrypt(ciphertext)
    }
    if (!raw) result <- rawToChar(result)
    result
  }
  structure(list(encrypt = encrypt, decrypt = decrypt, block_size = function() block_size,
                 IV = function() IV, key_size = function() key_size, mode = function() modes[mode]),
            class = "AES")
}
