# SPDX-License-Identifier: AGPL-3.0-or-later
#' Beta-process definition
#'
#' BP(c, H0) gives the cumulative hazard INDEPENDENT Beta increments,
#' dH(t) ~ Be(c dH0(t), c(1 - dH0(t))).  Putting the prior on the hazard
#' rather than on the survival function is what makes right censoring
#' conjugate, which is the whole reason the beta process is the standard
#' survival prior.  The path is simulated on a grid with H0 the
#' unit-exponential cumulative hazard.
#'
#' Formula: H(t_k) = sum_\{j <= k\} dH_j,
#'   dH_j ~ Be(c dH0_j, c(1 - dH0_j)).
#'
#' @param grid_t Increasing grid of time points.
#' @param c Concentration of the beta increments, positive.
#' @param Lambda0_rate Rate of the base cumulative hazard.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (terminal cumulative hazard),
#'   \code{cum_hazard}, \code{nondecreasing}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 13.3.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ghosalbetaprocdef(V)
Ghosalbetaprocdef <- function(grid_t, c = 2, Lambda0_rate = 1, seed = 42) {
  ts <- as.numeric(grid_t)
  if (length(ts) == 0L) stop("grid_t must be non-empty")
  if (c <= 0) stop("c must be positive")
  e <- .ghc_rng(seed)
  H <- 0; prev <- 0
  path <- numeric(length(ts))
  for (i in seq_along(ts)) {
    dH0 <- Lambda0_rate * (ts[i] - prev)
    H <- H + .ghc_beta1(e, max(c * dH0, 1e-8), max(c * (1 - dH0), 1e-8))
    path[i] <- H
    prev <- ts[i]
  }
  nd <- if (length(path) < 2L) TRUE else all(diff(path) >= -1e-12)
  .t1_result(estimate = path[length(path)], cum_hazard = path,
             nondecreasing = nd,
             method = "beta process (GvdV 2017 sec. 13.3)")
}
