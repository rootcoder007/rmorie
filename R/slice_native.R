# Univariate slice sampling.
# Source: Neal, R. M. (2003), Slice sampling, Annals of Statistics
# 31(3), 705-767: the auxiliary-variable scheme of his Sec. 4
# (sample y ~ U(0, f(x)), then x' uniformly from the slice
# {x : f(x) > y}), with the "stepping out" procedure of his Fig. 3
# to bracket the slice and the "shrinkage" procedure of his Fig. 5 to
# sample from it.  The random split of the step budget between the
# two directions (j and k = m - 1 - j) is what keeps the stepping-out
# bracket a valid, reversible construction.
#
# Native implementation mirroring Python morie.fn.slice exactly: the
# same number of uniforms in the same order per iteration, so the
# chains coincide draw for draw.

#' Univariate slice sampler
#'
#' Draws a Markov chain whose stationary distribution is the density
#' proportional to \code{exp(log_target)}, by Neal's (2003) slice
#' sampling: stepping out to bracket the slice, then shrinking the
#' bracket until an accepted point is found.  Unlike
#' Metropolis-Hastings there is no rejection and no acceptance rate to
#' tune; \code{width} only affects efficiency, not correctness.
#'
#' @param log_target Function of one number giving the unnormalised
#'   log density.
#' @param init Starting value.
#' @param width Initial bracket width \eqn{w} (Neal's Fig. 3).
#' @param n_iter Number of iterations.
#' @param max_steps Step-out budget \eqn{m} (Neal's Fig. 3).
#' @param seed Seed for the generator shared with the Python arm.
#' @return A list with \code{samples} and \code{n_iter}.
#' @references Neal, R. M. (2003). Slice sampling. Annals of
#'   Statistics, 31(3), 705-767.
#' @export
morie_slice <- function(log_target, init = 0, width = 1, n_iter = 5000L,
                        max_steps = 100L, seed = 42) {
  n_iter <- as.integer(n_iter)
  if (n_iter < 1L) stop("n_iter must be >= 1.")
  if (width <= 0) stop("width must be > 0.")
  max_steps <- as.integer(max_steps)
  e <- .ghc_rng(seed)
  x <- as.numeric(init)
  samples <- numeric(n_iter)
  for (i in seq_len(n_iter)) {
    log_y <- log_target(x) + log(.ghc_unif(e, 1L))
    L <- x - width * .ghc_unif(e, 1L)
    R <- L + width
    j <- floor(max_steps * .ghc_unif(e, 1L))
    k <- max_steps - 1 - j
    while (j > 0 && log_target(L) > log_y) { L <- L - width
    j <- j - 1 }
    while (k > 0 && log_target(R) > log_y) { R <- R + width
    k <- k - 1 }
    repeat {
      x_prop <- L + .ghc_unif(e, 1L) * (R - L)
      if (log_target(x_prop) >= log_y) { x <- x_prop
      break }
      if (x_prop < x) L <- x_prop else R <- x_prop
    }
    samples[i] <- x
  }
  list(samples = samples, n_iter = n_iter)
}
