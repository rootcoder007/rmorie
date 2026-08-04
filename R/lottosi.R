# SPDX-License-Identifier: AGPL-3.0-or-later
#' Draw n units with probability proportional to size, and estimate.
#'
#' The "lottery" or cumulative-total method, drawing WITH replacement, so
#' the same unit can legitimately appear twice. Selection uses a pinned
#' Lehmer generator so the two language arms draw the SAME units.
#' Indices are one-based.
#'
#' Formula: p_i = z_i / sum_j z_j; Yhat = (1/n) sum_i y_i / p_i;
#'   v(Yhat) = sum_i (y_i/p_i - Yhat)^2 / (n (n - 1))
#'
#' @param z Size measure of every unit, strictly positive.
#' @param y Value of the variable of interest for every unit.
#' @param n Number of draws (with replacement), n >= 2.
#' @param seed Seed for the pinned generator.
#' @return List with \code{index}, \code{p}, \code{estimate}, \code{se},
#'   \code{true_total}, \code{n}, \code{N}.
#' @references Cochran (1977), Sampling Techniques, 3rd edition, Chapter
#'   9A. Chapter 9A was NOT in the scanned excerpt available to this
#'   batch, so the standard published form (Hansen & Hurwitz 1943, Annals
#'   of Mathematical Statistics 14(4), 333-362) is used.
#' @export
Ppssamp <- function(z, y, n, seed = 1) {
  z <- .t1_vec(z); y <- .t1_vec(y); N <- length(z)
  if (length(y) != N) stop("z and y must have the same length")
  if (any(z <= 0)) stop("sizes must be strictly positive")
  n <- as.integer(n)
  if (n < 2L) stop("a variance needs at least two draws")
  p <- z / sum(z)
  cum <- cumsum(p)
  g <- .t1_lcg(seed)
  idx <- integer(n)
  for (t in seq_len(n)) {
    u <- g$unif()
    j <- 1L
    while (j < N && u > cum[j]) j <- j + 1L
    idx[t] <- j
  }
  r <- y[idx] / p[idx]
  est <- mean(r)
  var <- sum((r - est)^2) / (n * (n - 1))
  .t1_result(index = idx, p = p, estimate = est, se = sqrt(var),
             true_total = sum(y), n = n, N = N,
             method = "PPS with replacement, Hansen-Hurwitz estimator")
}
