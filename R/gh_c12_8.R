# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cox-model Bernstein-von Mises
#'
#' sqrt(n)(beta_post - beta0) tends to N(0, I_beta^(-1)) with the
#' information taken from the PARTIAL likelihood.  That the infinite
#' dimensional baseline hazard costs nothing asymptotically -- the
#' partial likelihood is already efficient for beta -- is the reason the
#' Cox model is the standard example of semiparametric BvM.  Exponential
#' baseline and a binary covariate here, so the truth is known.
#'
#' Formula: -log PL(b) = sum over the event ordering of
#'   [log(sum_{j in risk set} exp(b z_j)) - b z_i]; minimised on a grid.
#'
#' @param beta0 True log hazard ratio.
#' @param n Sample size.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (the partial-likelihood maximiser),
#'   \code{error}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 12.3.3.
#' @export
Ghosalcoxbvmsp <- function(beta0 = 0.8, n = 600, seed = 42) {
  n <- as.integer(n)
  if (n < 1L) stop("n must be positive")
  e <- .ghc_rng(seed)
  zs <- as.numeric((seq_len(n) - 1L) %% 2L == 0L)
  times <- -log(pmax(.ghc_unif(e, n), 1e-12)) / exp(beta0 * zs)
  ord <- order(times)
  neg_pll <- function(b) {
    ez <- exp(b * zs)
    # risk set shrinks along the event ordering, so the running sum is
    # the reverse cumulative total of exp(b z) in that order
    risk <- sum(ez) - c(0, cumsum(ez[ord])[-n])
    sum(log(pmax(risk, 1e-300)) - b * zs[ord])
  }
  grid <- beta0 - 1 + 2 * (seq_len(51) - 1) / 50
  vals <- vapply(grid, neg_pll, numeric(1))
  b_hat <- grid[which.min(vals)]
  .t1_result(estimate = b_hat, error = abs(b_hat - beta0),
             method = "Cox partial-likelihood BvM (GvdV 2017 sec. 12.3.3)")
}
