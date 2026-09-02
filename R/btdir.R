# SPDX-License-Identifier: AGPL-3.0-or-later
#' Dirichlet(1,...,1) weight matrix for the Bayesian bootstrap
#'
#' Rubin, D. B. (1981), "The Bayesian Bootstrap", The Annals of Statistics
#' 9(1), 130-134, doi:10.1214/aos/1176345338 (verified against Crossref).
#' Section 2: the posterior of the multinomial probability vector under the
#' improper Dirichlet(0,...,0) prior is Dirichlet(1,...,1) on the n observed
#' values, and Rubin's stated simulation recipe is to draw u_1,...,u_(n-1)
#' iid U(0,1), sort them to 0 = v_0 <= v_1 <= ... <= v_(n-1) <= v_n = 1, and
#' set w_i = v_i - v_(i-1).  The gaps between the order statistics of n-1 iid
#' uniforms are exactly Dirichlet(1,...,1); no rejection or gamma-ratio step
#' is involved.  This is the weight generator only; Btbayes consumes it.
#'
#' Deterministic by construction: the uniforms come from the package's shared
#' Lehmer minstd stream, whose intermediates fit exactly in a double so the
#' Python and R arms produce bit-identical draws.
#'
#' @param n sample size; the weight vectors have this length.
#' @param B number of weight vectors.
#' @param rng seed for the shared deterministic stream.
#' @return list: W, rowsum_max_err, w_mean, w_var, w_min, w_max, n, B,
#'   estimate, method.
#' @keywords internal
#' @examples
#' Btdir(4, 10)$rowsum_max_err
#' @export
Btdir <- function(n, B = 200, rng = 1) {
  W <- .btdir_rows(n, B, rng)
  n <- as.integer(n)
  B <- as.integer(B)
  err <- 0
  tot <- 0
  tot2 <- 0
  for (row in W) {
    s <- 0
    for (w in row) { s <- s + w
    tot <- tot + w
    tot2 <- tot2 + w * w }
    d <- abs(s - 1)
    if (d > err) err <- d
  }
  N <- n * B
  m <- tot / N
  list(W = W, rowsum_max_err = err, w_mean = m, w_var = tot2 / N - m * m,
       w_min = min(vapply(W, min, 0)), w_max = max(vapply(W, max, 0)),
       n = n, B = B, estimate = m,
       method = "Rubin (1981) Ann. Statist. 9(1):130-134, uniform-gap Dirichlet(1,...,1)")
}

#' @noRd
.btdir_rows <- function(n, B, seed = 1) {
  n <- as.integer(n)
  B <- as.integer(B)
  if (n < 1L) stop("boot_dirichlet_weights: n must be at least 1")
  if (B < 1L) stop("boot_dirichlet_weights: B must be at least 1")
  g <- .t1_lcg(seed)
  W <- vector("list", B)
  for (b in seq_len(B)) {
    if (n == 1L) { W[[b]] <- 1
    next }
    v <- sort(vapply(seq_len(n - 1L), function(i) g$unif(), 0))
    row <- numeric(n)
    row[1] <- v[1]
    if (n > 2L) for (i in 2:(n - 1L)) row[i] <- v[i] - v[i - 1L]
    row[n] <- 1 - v[n - 1L]
    W[[b]] <- row
  }
  W
}
