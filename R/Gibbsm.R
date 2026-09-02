# SPDX-License-Identifier: AGPL-3.0-or-later
#' Gibbs sampler
#'
#' One sweep replaces each block in turn by a draw from its full
#' conditional, leaving the joint distribution invariant.  Each
#' conditional is supplied as a function of the current state and one
#' uniform variate, so the sampler is an inverse-CDF draw; the uniforms
#' come from the van der Corput sequence, which makes the run
#' reproducible and identical across language arms.
#'
#' Formula: x_i <- conditional_i(x, u) for each block in turn.
#'
#' @param conditionals List of functions f(x, u) returning the new value
#'   of block i.
#' @param x0 Starting state, one entry per conditional.
#' @param n_iter Number of sweeps.
#' @param burn Sweeps discarded before the summaries are formed.
#' @return List with \code{estimate}, \code{mean}, \code{draws},
#'   \code{last}, \code{n}, \code{method}.
#' @references Geman and Geman (1984), Stochastic relaxation, Gibbs
#'   distributions, and the Bayesian restoration of images, IEEE
#'   Transactions on Pattern Analysis and Machine Intelligence
#'   6(6):721-741. \doi{10.1109/TPAMI.1984.4767596}
#' @export
#' @examples
#' conditionals <- list(function(x, u) qnorm(u, 0.5 * x[2], 1),
#'                      function(x, u) qnorm(u, 0.5 * x[1], 1))
#' Gibbsm(conditionals, x0 = c(0, 0), n_iter = 50, burn = 10)
Gibbsm <- function(conditionals, x0, n_iter = 100, burn = 0) {
  x <- .s03vec(x0)
  d <- length(x)
  if (d == 0L) stop("gibbs_sampler: x0 is empty")
  cs <- as.list(conditionals)
  if (length(cs) != d) stop("gibbs_sampler: one conditional per block is required")
  for (cc in cs) if (!is.function(cc)) stop("gibbs_sampler: every conditional must be callable")
  it <- as.integer(n_iter)
  if (it < 1L) stop("gibbs_sampler: n_iter must be at least 1")
  bn <- as.integer(burn)
  if (bn < 0L || bn >= it) stop("gibbs_sampler: burn must lie in [0, n_iter)")
  counter <- 0L
  draws <- matrix(0, it, d)
  for (s in seq_len(it)) {
    for (i in seq_len(d)) {
      u <- .s03vdc(counter + 1L)
      counter <- counter + 1L
      x[i] <- as.numeric(cs[[i]](x, u))
    }
    draws[s, ] <- x
  }
  kept <- draws[seq(bn + 1L, it), , drop = FALSE]
  means <- colMeans(kept)
  .t1_result(estimate = means[1], mean = means, draws = draws, last = x,
             n = it,
             method = "componentwise full-conditional updates with van der Corput uniforms, Geman & Geman (1984)")
}
