# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Arbitrary-precision integers, implemented natively.
#
# R has no exact integer type beyond 2^53. This is not a rounding
# inconvenience, it is silent wrongness on quantities combinatorics
# cares about:
#
#   2^53 + 1 == 2^53                                  is TRUE in R
#   choose(100, 50) returns 100891344545563076171808112640
#   the exact value is    100891344545564193334812497256
#
# The two part company at the thirteenth significant digit, and nothing
# in the output says so. Every exact count in the combinatorics shelves
# -- factorials, binomials, Bell, Catalan, Stirling, partition numbers
# -- runs through this file instead.
#
# Representation: a list with `sign` (-1, 0 or 1) and `limbs`, a
# little-endian numeric vector of base-10^6 digits. Base 10^6 keeps
# every intermediate below 10^12 + 10^6, comfortably inside the 2^53
# where doubles are still exact integers, provided multiplication
# carries row by row -- which it does below.
#
# gmp is deliberately not used. The point is a native specialization,
# and the operations needed here are elementary.

.MORIE_BIG_BASE <- 1e6
.MORIE_BIG_DIG <- 6L

.morie_big_trim <- function(limbs) {
  n <- length(limbs)
  while (n > 1L && limbs[n] == 0) n <- n - 1L
  limbs[seq_len(n)]
}

.morie_big_new <- function(sign, limbs) {
  limbs <- .morie_big_trim(limbs)
  if (length(limbs) == 1L && limbs[1] == 0) sign <- 0
  structure(list(sign = sign, limbs = limbs), class = "morie_bigint")
}

#' Arbitrary-precision integer from a string or a double
#'
#' @param x A decimal string, or a numeric below 2^53.
#' @return An object of class `morie_bigint`.
#' @export
morie_bigint <- function(x) {
  if (inherits(x, "morie_bigint")) return(x)
  if (is.numeric(x)) {
    if (length(x) != 1L || !is.finite(x)) {
      stop("numeric input must be a single finite value.", call. = FALSE)
    }
    if (abs(x) > 2^53) {
      stop(paste("numeric input exceeds 2^53, where doubles stop being",
                 "exact integers; pass a decimal string instead."),
           call. = FALSE)
    }
    if (x != floor(x)) stop("numeric input must be a whole number.", call. = FALSE)
    x <- format(x, scientific = FALSE, trim = TRUE)
  }
  s <- trimws(as.character(x))
  sign <- 1
  if (startsWith(s, "-")) { sign <- -1; s <- substring(s, 2) }
  else if (startsWith(s, "+")) s <- substring(s, 2)
  if (!nzchar(s) || grepl("[^0-9]", s)) {
    stop(sprintf("not a decimal integer: '%s'", x), call. = FALSE)
  }
  s <- sub("^0+(?=[0-9])", "", s, perl = TRUE)
  n <- nchar(s)
  nl <- ceiling(n / .MORIE_BIG_DIG)
  limbs <- numeric(nl)
  pos <- n
  for (i in seq_len(nl)) {
    lo <- max(pos - .MORIE_BIG_DIG + 1L, 1L)
    limbs[i] <- as.numeric(substring(s, lo, pos))
    pos <- lo - 1L
  }
  .morie_big_new(sign, limbs)
}

#' @export
as.character.morie_bigint <- function(x, ...) {
  if (x$sign == 0) return("0")
  n <- length(x$limbs)
  parts <- character(n)
  parts[1] <- format(x$limbs[n], scientific = FALSE, trim = TRUE)
  if (n > 1L) {
    for (i in seq.int(n - 1L, 1L)) {
      parts[n - i + 1L] <- formatC(x$limbs[i], width = .MORIE_BIG_DIG,
                                   flag = "0", format = "d")
    }
  }
  paste0(if (x$sign < 0) "-" else "", paste(parts, collapse = ""))
}

#' Print method for \code{morie_bigint} objects
#'
#' @param x A \code{morie_bigint} object.
#' @param ... Ignored; accepted for S3 consistency.
#' @export
print.morie_bigint <- function(x, ...) {
  cat(as.character(x), "\n", sep = "")
  invisible(x)
}

#' Format method for \code{morie_bigint} objects
#'
#' @param x A \code{morie_bigint} object.
#' @param ... Ignored; accepted for S3 consistency.
#' @export
format.morie_bigint <- function(x, ...) as.character(x)

.morie_big_cmp_abs <- function(a, b) {
  la <- length(a); lb <- length(b)
  if (la != lb) return(if (la > lb) 1L else -1L)
  for (i in seq.int(la, 1L)) {
    if (a[i] != b[i]) return(if (a[i] > b[i]) 1L else -1L)
  }
  0L
}

.morie_big_add_abs <- function(a, b) {
  n <- max(length(a), length(b))
  a <- c(a, numeric(n - length(a)))
  b <- c(b, numeric(n - length(b)))
  out <- numeric(n + 1L)
  carry <- 0
  for (i in seq_len(n)) {
    t <- a[i] + b[i] + carry
    if (t >= .MORIE_BIG_BASE) { out[i] <- t - .MORIE_BIG_BASE; carry <- 1 }
    else { out[i] <- t; carry <- 0 }
  }
  out[n + 1L] <- carry
  .morie_big_trim(out)
}

# assumes |a| >= |b|
.morie_big_sub_abs <- function(a, b) {
  n <- length(a)
  b <- c(b, numeric(n - length(b)))
  out <- numeric(n)
  borrow <- 0
  for (i in seq_len(n)) {
    t <- a[i] - b[i] - borrow
    if (t < 0) { out[i] <- t + .MORIE_BIG_BASE; borrow <- 1 }
    else { out[i] <- t; borrow <- 0 }
  }
  .morie_big_trim(out)
}

#' Compare two arbitrary-precision integers
#'
#' @param a,b Values coercible by [morie_bigint()].
#' @return -1, 0 or 1.
#' @export
morie_big_cmp <- function(a, b) {
  a <- morie_bigint(a); b <- morie_bigint(b)
  if (a$sign != b$sign) return(if (a$sign > b$sign) 1L else -1L)
  if (a$sign == 0) return(0L)
  c <- .morie_big_cmp_abs(a$limbs, b$limbs)
  if (a$sign > 0) c else -c
}

#' Add two arbitrary-precision integers
#'
#' @param a,b Values coercible by [morie_bigint()].
#' @return A `morie_bigint`.
#' @export
morie_big_add <- function(a, b) {
  a <- morie_bigint(a); b <- morie_bigint(b)
  if (a$sign == 0) return(b)
  if (b$sign == 0) return(a)
  if (a$sign == b$sign) {
    return(.morie_big_new(a$sign, .morie_big_add_abs(a$limbs, b$limbs)))
  }
  c <- .morie_big_cmp_abs(a$limbs, b$limbs)
  if (c == 0L) return(.morie_big_new(0, 0))
  if (c > 0L) .morie_big_new(a$sign, .morie_big_sub_abs(a$limbs, b$limbs))
  else .morie_big_new(b$sign, .morie_big_sub_abs(b$limbs, a$limbs))
}

#' Subtract arbitrary-precision integers
#'
#' @param a,b Values coercible by [morie_bigint()].
#' @return A `morie_bigint`.
#' @export
morie_big_sub <- function(a, b) {
  b <- morie_bigint(b)
  morie_big_add(a, .morie_big_new(-b$sign, b$limbs))
}

#' Multiply two arbitrary-precision integers
#'
#' Schoolbook multiplication carrying row by row, which keeps every
#' intermediate below \eqn{10^{12} + 10^6} and so exactly representable
#' as a double.
#'
#' @param a,b Values coercible by [morie_bigint()].
#' @return A `morie_bigint`.
#' @export
morie_big_mul <- function(a, b) {
  a <- morie_bigint(a); b <- morie_bigint(b)
  if (a$sign == 0 || b$sign == 0) return(.morie_big_new(0, 0))
  la <- length(a$limbs); lb <- length(b$limbs)
  res <- numeric(la + lb)
  for (i in seq_len(la)) {
    carry <- 0
    ai <- a$limbs[i]
    if (ai == 0) next
    for (j in seq_len(lb)) {
      t <- res[i + j - 1L] + ai * b$limbs[j] + carry
      carry <- floor(t / .MORIE_BIG_BASE)
      res[i + j - 1L] <- t - carry * .MORIE_BIG_BASE
    }
    k <- i + lb
    while (carry > 0) {
      t <- res[k] + carry
      carry <- floor(t / .MORIE_BIG_BASE)
      res[k] <- t - carry * .MORIE_BIG_BASE
      k <- k + 1L
    }
  }
  .morie_big_new(a$sign * b$sign, res)
}

#' Divide an arbitrary-precision integer by a small whole number
#'
#' @param a Value coercible by [morie_bigint()].
#' @param d A positive whole number below 2^31.
#' @return A list with `quotient` (a `morie_bigint`) and `remainder`.
#' @export
morie_big_divmod_small <- function(a, d) {
  a <- morie_bigint(a)
  d <- as.numeric(d)
  if (length(d) != 1L || d <= 0 || d != floor(d) || d >= 2^31) {
    stop("d must be a positive whole number below 2^31.", call. = FALSE)
  }
  if (a$sign == 0) return(list(quotient = .morie_big_new(0, 0), remainder = 0))
  n <- length(a$limbs)
  q <- numeric(n)
  rem <- 0
  for (i in seq.int(n, 1L)) {
    cur <- rem * .MORIE_BIG_BASE + a$limbs[i]
    q[i] <- floor(cur / d)
    rem <- cur - q[i] * d
  }
  list(quotient = .morie_big_new(a$sign, q), remainder = rem)
}

#' Raise an arbitrary-precision integer to a whole power
#'
#' Binary exponentiation.
#'
#' @param a Base, coercible by [morie_bigint()].
#' @param k Non-negative whole exponent.
#' @return A `morie_bigint`.
#' @export
morie_big_pow <- function(a, k) {
  k <- as.integer(k)
  if (is.na(k) || k < 0L) stop("k must be a non-negative whole number.",
                               call. = FALSE)
  result <- morie_bigint(1)
  base <- morie_bigint(a)
  while (k > 0L) {
    if (k %% 2L == 1L) result <- morie_big_mul(result, base)
    base <- morie_big_mul(base, base)
    k <- k %/% 2L
  }
  result
}

#' Exact factorial
#'
#' @param n Non-negative whole number.
#' @return A `morie_bigint`.
#' @export
morie_big_factorial <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 0L) stop("n must be a non-negative whole number.",
                               call. = FALSE)
  out <- morie_bigint(1)
  if (n < 2L) return(out)
  for (i in seq.int(2L, n)) out <- morie_big_mul(out, morie_bigint(i))
  out
}

#' Exact binomial coefficient
#'
#' Uses the multiplicative recurrence
#' \eqn{\binom{n}{k} = \prod_{i=1}^{k} (n-k+i)/i}, in which every
#' partial product is exactly divisible by \eqn{i}. That keeps the
#' whole computation inside multiplication and small division, and
#' never needs long division.
#'
#' `choose()` in base R returns 100891344545563076171808112640 for
#' \eqn{\binom{100}{50}}; the exact value is
#' 100891344545564193334812497256, so the two differ from the
#' thirteenth significant digit.
#'
#' @param n,k Whole numbers.
#' @return A `morie_bigint`.
#' @export
morie_big_binom <- function(n, k) {
  n <- as.integer(n); k <- as.integer(k)
  if (is.na(n) || is.na(k) || n < 0L) {
    stop("n must be a non-negative whole number.", call. = FALSE)
  }
  if (k < 0L || k > n) return(morie_bigint(0))
  k <- min(k, n - k)
  out <- morie_bigint(1)
  if (k == 0L) return(out)
  for (i in seq_len(k)) {
    out <- morie_big_mul(out, morie_bigint(n - k + i))
    dm <- morie_big_divmod_small(out, i)
    if (dm$remainder != 0) {
      stop("internal error: the multiplicative recurrence left a remainder.",
           call. = FALSE)
    }
    out <- dm$quotient
  }
  out
}

#' Number of decimal digits
#'
#' @param a Value coercible by [morie_bigint()].
#' @return Integer digit count; 1 for zero.
#' @export
morie_big_ndigits <- function(a) nchar(sub("^-", "", as.character(morie_bigint(a))))

#' Is this value exactly representable as an R double?
#'
#' Anything with magnitude above 2^53 is not, and converting it silently
#' loses low-order digits.
#'
#' @param a Value coercible by [morie_bigint()].
#' @return TRUE or FALSE.
#' @export
morie_big_fits_double <- function(a) {
  a <- morie_bigint(a)
  morie_big_cmp(.morie_big_new(1, a$limbs), morie_bigint("9007199254740992")) <= 0L
}
