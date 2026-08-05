# SPDX-License-Identifier: AGPL-3.0-or-later
#' GP length-scale adaptation
#'
#' f ~ GP(0, k_l) with l itself given a prior.  The evidence-weighted
#' posterior over the length scale concentrates near the scale of the
#' truth, which is how a Gaussian prior adapts to an unknown smoothness
#' without being told it.  The marginal likelihood of each candidate l is
#' exact -- the model is conjugate Gaussian -- so the selection involves
#' no approximation beyond the finite grid of candidates.
#'
#' Formula: log p(y | l) = -y' (K_l + sigma^2 I)^(-1) y / 2
#'   - log det(K_l + sigma^2 I) / 2, up to the constant.
#'
#' @param n Sample size on an equispaced design.
#' @param l_true Length scale of the simulated truth.
#' @param l_grid Candidate length scales, all positive.
#' @param noise Observation noise standard deviation.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (selected length scale),
#'   \code{log_evidence}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 11.6.
#' @export
Ghosalgpadaptthm <- function(n = 60, l_true = 0.2,
                             l_grid = c(0.05, 0.2, 0.8), noise = 0.1,
                             seed = 42) {
  n <- as.integer(n)
  if (n < 2L) stop("n must be at least 2")
  if (l_true <= 0) stop("l_true must be positive")
  if (length(l_grid) == 0L) stop("l_grid must be non-empty")
  if (any(l_grid <= 0)) stop("every candidate length scale must be positive")
  e <- .ghc_rng(seed)
  xs <- (seq_len(n) - 0.5) / n
  f0 <- sin(2 * pi * xs / (5 * l_true))
  ys <- f0 + noise * .ghc_norm(e, n)
  D <- outer(xs, xs, "-")
  logev <- function(l) {
    K <- exp(-0.5 * (D / l)^2) + diag(noise^2 + 1e-8, n)
    quad <- sum(solve(K, ys) * ys)
    # log-determinant by the same unpivoted elimination the Python arm
    # uses, so the two agree to machine precision rather than only in
    # exact arithmetic.
    m <- K
    ld <- 0
    for (i in seq_len(n)) {
      ld <- ld + log(m[i, i])
      if (i < n) for (r in (i + 1):n) {
        fmul <- m[r, i] / m[i, i]
        m[r, i:n] <- m[r, i:n] - fmul * m[i, i:n]
      }
    }
    -0.5 * quad - 0.5 * ld
  }
  evs <- vapply(l_grid, logev, numeric(1))
  .t1_result(estimate = l_grid[which.max(evs)], log_evidence = evs,
             method = "GP length-scale adaptation (GvdV 2017 sec. 11.6)")
}
