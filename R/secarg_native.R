# Argon2: a memory-hard password hash built on BLAKE2b.
# Sources: Biryukov, A., Dinu, D., Khovratovich, D. & Josefsson, S.
# (2021) "Argon2 Memory-Hard Function for Password Hashing and
# Proof-of-Work Applications", RFC 9106, doi:10.17487/RFC9106.
# Biryukov, A., Dinu, D. & Khovratovich, D. (2016) "Argon2: New
# Generation of Memory-Hard Functions for Password Hashing and Other
# Applications", 2016 IEEE EuroS&P, 292-302,
# doi:10.1109/EuroSP.2016.31.
# Saarinen, M.-J. & Aumasson, J.-P. (2015) "BLAKE2", RFC 7693,
# doi:10.17487/RFC7693.
#
# Native implementation mirroring morie.fn.secarg exactly: the same
# prehash H_0 over every parameter, the same block init from H_0,
# the same J_1/J_2 reference indices and the same G (rows then
# columns) under BLAKE2b.

.MASK64 <- bitwShiftL(1, 64) - 1
.MASK32 <- bitwShiftL(1, 32) - 1
.BLOCK <- 1024
.SL <- 4
.TYPES <- c(argon2d = 0, argon2i = 1, argon2id = 2)
.VERSION <- 0x13

#' .le32
#'
#' Part of the secarg_native implementation; see the file header for the
#' source it follows.
#'
#' @param n See Usage.
#' @return The value of \code{writeBin}.
#' @export
.le32 <- function(n) {
  n <- as.integer(n)
  writeBin(n, raw(), size = 4L, endian = "little")
}

#' .le64
#'
#' Part of the secarg_native implementation; see the file header for the
#' source it follows.
#'
#' @param n See Usage.
#' @return The value of \code{writeBin}.
#' @export
.le64 <- function(n) {
  n <- bitwAnd(n, .MASK64)
  writeBin(n, raw(), size = 8L, endian = "little")
}

#' Argon2's variable-length hash H'
#'
#' BLAKE2b stretched past 64 bytes; the first 32 bytes of every
#' subsequent 64-byte block are kept so the output does not repeat.
#'
#' @param data Raw vector of bytes.
#' @param length Positive integer target length.
#' @return Raw vector of \code{length} bytes.
#' @references RFC 9106 Sec. 3.3.
#' @export
morie_secarg_variable_hash <- function(data, length) {
  T <- as.integer(length)
  if (T < 1L) stop("secarg: the output length must be positive")
  a <- as.raw(data)
  if (T <= 64L) return(.morie_blake2b_impl(c(.le32(T), a), T))
  r <- -(-T %/% 32L) - 2L
  out <- raw()
  v <- .morie_blake2b_impl(c(.le32(T), a), 64L)
  out <- c(out, v[1:32])
  for (kk in seq_len(r - 1L)) {
    v <- .morie_blake2b_impl(v, 64L)
    out <- c(out, v[1:32])
  }
  v <- .morie_blake2b_impl(v, T - 32L * r)
  out <- c(out, v)
  out[seq_len(T)]
}

#' Argon2 prehash H_0 over every parameter
#'
#' Changing any of the parameters changes the digest, so a tag cannot
#' silently be compared across configurations.
#'
#' @param password Raw bytes.
#' @param salt Raw bytes, at least 8.
#' @param parallelism Positive integer.
#' @param tag_length Positive integer.
#' @param memory Memory in KiB.
#' @param passes Positive integer.
#' @param variant One of \code{argon2d}, \code{argon2i}, \code{argon2id}.
#' @param secret Raw bytes (optional).
#' @param associated Raw bytes (optional).
#' @param version Integer (default 0x13).
#' @return Raw 64-byte BLAKE2b digest.
#' @references RFC 9106 Sec. 3.1.
#' @export
morie_secarg_prehash <- function(password, salt, parallelism, tag_length,
                                 memory, passes,
                                 variant = "argon2id",
                                 secret = raw(), associated = raw(),
                                 version = .VERSION) {
  y <- .TYPES[[as.character(variant)]]
  if (is.null(y))
    stop("secarg: variant must be one of argon2d, argon2i, argon2id")
  P <- as.raw(password); S <- as.raw(salt)
  K <- as.raw(secret); X <- as.raw(associated)
  if (length(S) < 8L)
    stop("secarg: the salt must be at least 8 bytes (the RFC recommends 16), got ",
         length(S))
  buf <- c(.le32(parallelism), .le32(tag_length), .le32(memory),
           .le32(passes), .le32(version), .le32(y),
           .le32(length(P)), P, .le32(length(S)), S,
           .le32(length(K)), K, .le32(length(X)), X)
  .morie_blake2b_impl(buf, 64L)
}

# In-place G sub-round; .gb_mut mutates v (1-indexed) at positions a,b,c,d
#' In-place G sub-round; .gb_mut mutates v (1-indexed) at positions
#' a,b,c,d
#'
#' Part of the secarg_native implementation; see the file header for the
#' source it follows.
#'
#' @param v See Usage.
#' @param a See Usage.
#' @param b See Usage.
#' @param c See Usage.
#' @param d See Usage.
#' @return The value of \code{<<-}.
#' @export
.gb_mut <- function(v, a, b, c, d) {
  va <- v[a]; vb <- v[b]; vc <- v[c]; vd <- v[d]
  v[a] <<- bitwAnd(va + vb +
                     2L * (bitwAnd(va, .MASK32) *
                           bitwAnd(vb, .MASK32)), .MASK64)
  xd <- bitwXor(v[d], v[a])
  v[d] <<- bitwAnd(bitwXor(bitwShiftR(xd, 32L),
                            bitwShiftL(xd, 32L)), .MASK64)
  v[c] <<- bitwAnd(v[c] + v[d] +
                     2L * (bitwAnd(v[c], .MASK32) *
                           bitwAnd(v[d], .MASK32)), .MASK64)
  x <- bitwXor(v[b], v[c])
  v[b] <<- bitwAnd(bitwXor(bitwShiftR(x, 24L),
                            bitwShiftL(x, 40L)), .MASK64)
  v[a] <<- bitwAnd(v[a] + v[b] +
                     2L * (bitwAnd(v[a], .MASK32) *
                           bitwAnd(v[b], .MASK32)), .MASK64)
  x <- bitwXor(v[d], v[a])
  v[d] <<- bitwAnd(bitwXor(bitwShiftR(x, 16L),
                            bitwShiftL(x, 48L)), .MASK64)
  v[c] <<- bitwAnd(v[c] + v[d] +
                     2L * (bitwAnd(v[c], .MASK32) *
                           bitwAnd(v[d], .MASK32)), .MASK64)
  x <- bitwXor(v[b], v[c])
  v[b] <<- bitwAnd(bitwXor(bitwShiftR(x, 63L),
                            bitwShiftL(x, 1L)), .MASK64)
}

#' .P_mut
#'
#' Part of the secarg_native implementation; see the file header for the
#' source it follows.
#'
#' @param v See Usage.
#' @return The value of \code{.gb_mut}.
#' @export
.P_mut <- function(v) {
  .gb_mut(v, 1, 5, 9, 13)
  .gb_mut(v, 2, 6, 10, 14)
  .gb_mut(v, 3, 7, 11, 15)
  .gb_mut(v, 4, 8, 12, 16)
  .gb_mut(v, 1, 6, 11, 16)
  .gb_mut(v, 2, 7, 12, 13)
  .gb_mut(v, 3, 4, 9, 14)
  .gb_mut(v, 4, 5, 10, 15)
}

#' Argon2 compression function G(X, Y)
#'
#' Applies the BLAKE2b permutation to rows then columns of the 1024-byte
#' block and XORs back. Rows alone would not diffuse across the block.
#'
#' @param X Integer vector of 128 64-bit words.
#' @param Y Integer vector of 128 64-bit words.
#' @return Integer vector of 128 64-bit words.
#' @references RFC 9106 Sec. 3.5-3.6.
#' @export
morie_secarg_compress <- function(X, Y) {
  R <- mapply(bitwXor, as.numeric(X), as.numeric(Y), SIMPLIFY = TRUE)
  Q <- R
  for (i in 0:7) {
    row <- Q[16L * i + 1:16]
    .P_mut(row)
    Q[16L * i + 1:16] <- row
  }
  for (j in 0:7) {
    idx <- c(sapply(0:7, function(i)
      c(16L * i + 2L * j + 1L, 16L * i + 2L * j + 2L)))
    col <- Q[idx]
    .P_mut(col)
    Q[idx] <- col
  }
  mapply(bitwXor, Q, R, SIMPLIFY = TRUE)
}

#' .to_words
#'
#' Part of the secarg_native implementation; see the file header for the
#' source it follows.
#'
#' @param bs See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.to_words <- function(bs) {
  bs <- as.raw(bs)
  n <- length(bs) %/% 8L
  out <- integer(n)
  for (i in seq_len(n)) {
    seg <- bs[(8L * (i - 1L) + 1):(8L * i)]
    out[i] <- sum(bitwShiftL(as.integer(seg),
                              c(0, 8, 16, 24, 32, 40, 48, 56)))
  }
  out
}

#' .to_bytes
#'
#' Part of the secarg_native implementation; see the file header for the
#' source it follows.
#'
#' @param ws See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.to_bytes <- function(ws) {
  ws <- as.numeric(ws)
  out <- raw(length(ws) * 8L)
  for (i in seq_along(ws)) {
    v <- bitwAnd(round(ws[i]), .MASK64)
    out[(8L * (i - 1L) + 1):(8L * i)] <- writeBin(v, raw(),
                                                    size = 8L,
                                                    endian = "little")
  }
  out
}

#' .addresses
#'
#' Part of the secarg_native implementation; see the file header for the
#' source it follows.
#'
#' @param pass_no See Usage.
#' @param lane See Usage.
#' @param slice_no See Usage.
#' @param m_prime See Usage.
#' @param passes See Usage.
#' @param y See Usage.
#' @param counter See Usage.
#' @return The value of \code{morie_secarg_compress}.
#' @export
.addresses <- function(pass_no, lane, slice_no, m_prime, passes, y,
                       counter) {
  zero <- rep(0, 128)
  inp <- rep(0, 128)
  inp[1] <- pass_no; inp[2] <- lane; inp[3] <- slice_no
  inp[4] <- m_prime; inp[5] <- passes; inp[6] <- y; inp[7] <- counter
  morie_secarg_compress(zero, morie_secarg_compress(zero, inp))
}

#' Argon2 password hash
#'
#' Computes the tag and returns the parameters it was computed under,
#' since a tag compared across different parameters is meaningless.
#'
#' @param password Raw bytes.
#' @param salt Raw bytes.
#' @param memory Memory in KiB.
#' @param passes Number of passes.
#' @param parallelism Number of lanes.
#' @param tag_length Tag length in bytes.
#' @param variant \code{argon2d}, \code{argon2i} or \code{argon2id}.
#' @param secret Optional secret.
#' @param associated Optional associated data.
#' @return List with \code{tag}, \code{tag_hex}, \code{variant},
#'   \code{memory_kib}, \code{memory_used_kib}, \code{passes},
#'   \code{parallelism}, \code{version}, \code{estimate} (==\code{tag_hex}),
#'   \code{data_independent_first_half}, \code{method}, \code{note}.
#' @references RFC 9106 Sec. 3.1-3.4.
#' @export
morie_secarg_argon2 <- function(password, salt, memory = 32, passes = 3,
                                parallelism = 4, tag_length = 32,
                                variant = "argon2id", secret = NULL,
                                associated = NULL) {
  tag <- .morie_argon2_impl(password, salt, as.integer(memory),
                           as.integer(passes), as.integer(parallelism),
                           as.integer(tag_length), as.character(variant),
                           secret, associated)
  p <- as.integer(parallelism)
  m <- as.integer(memory)
  m_prime <- (m %/% (4L * p)) * (4L * p)
  y <- switch(as.character(variant), argon2d = 0L, argon2i = 1L,
              argon2id = 2L)
  hex <- paste(sprintf("%02x", as.integer(tag)), collapse = "")
  list(estimate = hex, tag = tag, tag_hex = hex,
       variant = as.character(variant), memory_kib = m,
       memory_used_kib = m_prime, passes = as.integer(passes),
       parallelism = p, version = 19L,
       data_independent_first_half = (y == 2L),
       method = paste0("Argon2 v1.3; Biryukov, Dinu, Khovratovich & ",
                       "Josefsson (2021) RFC 9106"),
       note = paste0("a tag is only comparable against another computed ",
                     "under the SAME parameters, which is why they are ",
                     "returned with it"))
}

#' RFC 9106 recommended Argon2id configurations
#'
#' @param profile \code{first} or \code{second}.
#' @return List with \code{variant}, \code{memory}, \code{memory_gib},
#'   \code{passes}, \code{parallelism}, \code{tag_length},
#'   \code{salt_bytes}, \code{note}, \code{warning}.
#' @references RFC 9106 Sec. 4 and 7.4.
#' @export
morie_secarg_parameter_advice <- function(profile = "first") {
  rec <- list(
    first = list(variant = "argon2id", memory = 2L * 1024L * 1024L,
                 passes = 1L, parallelism = 4L, tag_length = 32L,
                 salt_bytes = 16L,
                 note = "RFC 9106 Sec. 4 first recommended option: 2 GiB, t = 1, p = 4"),
    second = list(variant = "argon2id", memory = 64L * 1024L,
                  passes = 3L, parallelism = 4L, tag_length = 32L,
                  salt_bytes = 16L,
                  note = "RFC 9106 Sec. 4 second option for memory-constrained environments: 64 MiB, t = 3, p = 4"))
  if (!profile %in% names(rec))
    stop("secarg: profile must be 'first' or 'second'")
  out <- rec[[profile]]
  out$memory_gib <- out$memory / (1024 * 1024)
  out$warning <- paste0("lowering memory in favour of more passes ",
                        "weakens time-space trade-off resistance")
  out
}

#' .secarg_hexlify
#'
#' Part of the secarg_native implementation; see the file header for the
#' source it follows.
#'
#' @param bs See Usage.
#' @return A character value.
#' @export
.secarg_hexlify <- function(bs) {
  paste(format(as.hexmode(as.integer(bs)), width = 2L), collapse = "")
}

# house entry point: the package exports one morie_<module>
morie_secarg <- morie_secarg_argon2
