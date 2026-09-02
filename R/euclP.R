# SPDX-License-Identifier: AGPL-3.0-or-later

#' Polynomial GCD via Euclid
#'
#' Formula: repeat polynomial division, gcd(a, b) = gcd(b, a mod b),
#' until the remainder vanishes; the last non-zero remainder, made monic,
#' is the greatest common divisor.  Algorithm E of Knuth, TAOCP Vol. 2,
#' sec. 4.6.1, run over the reals with a numerical tolerance in place of
#' exact zero testing.
#'
#' Coefficients are ASCENDING: \code{c(c0, c1, c2)} means
#' c0 + c1 x + c2 x^2.
#'
#' @param p,q Coefficient vectors in ascending order.
#' @param tol Magnitude at or below which a leading coefficient is zero.
#' @return List with \code{estimate}, \code{gcd}, \code{degree},
#'   \code{steps}, \code{n}, \code{method}.
#' @references Knuth (1997), The Art of Computer Programming, Vol. 2:
#'   Seminumerical Algorithms, 3rd ed., sec. 4.6.1, Addison-Wesley.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' EuclP(V, V)
EuclP <- function(p, q, tol = 1e-10) {
  .trim <- function(cc) {
    while (length(cc) > 1L && abs(cc[length(cc)]) <= tol) cc <- cc[-length(cc)]
    cc
  }
  .dvm <- function(a, b) {
    db <- length(b) - 1L
    lead <- b[db + 1L]
    repeat {
      da <- length(a) - 1L
      if (da < db || (length(a) == 1L && abs(a[1]) <= tol)) break
      f <- a[da + 1L] / lead
      for (i in seq_len(db + 1L))
        a[da - db + i] <- a[da - db + i] - f * b[i]
      a <- .trim(a)
      if (length(a) - 1L < db) break
    }
    a
  }
  tol <- as.numeric(tol)
  if (tol <= 0) stop("tol must be positive")
  a <- .trim(as.numeric(p)); b <- .trim(as.numeric(q))
  if (length(a) == 0L || length(b) == 0L)
    stop("empty input: p and q must have coefficients")
  zero_a <- length(a) == 1L && abs(a[1]) <= tol
  zero_b <- length(b) == 1L && abs(b[1]) <= tol
  if (zero_a && zero_b) stop("gcd(0, 0) is undefined")
  if (zero_b) { tmp <- a; a <- b; b <- tmp; zero_a <- TRUE; zero_b <- FALSE }
  steps <- 0L
  if (zero_a) {
    g <- b
  } else {
    if (length(a) < length(b)) { tmp <- a; a <- b; b <- tmp }
    while (!(length(b) == 1L && abs(b[1]) <= tol)) {
      r <- .dvm(a, b)
      a <- b; b <- .trim(r)
      steps <- steps + 1L
      if (steps > 10000L) stop("Euclid's algorithm failed to terminate")
    }
    g <- a
  }
  lead <- g[length(g)]
  g <- if (abs(lead) <= tol) 1 else g / lead
  .t1_result(estimate = as.numeric(length(g) - 1L), gcd = g,
             degree = length(g) - 1L, steps = steps, n = length(g),
             method = "Polynomial GCD via Euclid")
}
