# SPDX-License-Identifier: AGPL-3.0-or-later
#' Intrinsic CAR (ICAR) prior: precision structure, not a fit.
#'
#' ICAR is the CAR model at rho = 1. Its precision matrix is the graph
#' Laplacian Q = D - W with D = diag(W 1), which makes the prior
#' IMPROPER: Q1 = 0, so it is rank deficient by the number of connected
#' components and specifies only differences between neighbouring values,
#' not their overall level. That is why an ICAR term is used as a prior
#' component -- the structured half of a BYM model, for instance -- and
#' needs a sum-to-zero constraint to be identified.
#'
#' @param w Adjacency weights (n by n).
#' @param tau2 Precision scale; Q is returned scaled by 1 / tau2.
#' @return Named list: Q, D, rank, n_components, is_improper,
#'   conditional_variances, tau2.
#' @references Schabenberger & Gotway (2005), Sec 6.4.3 "Selected
#'   Spatial Models".
#' @examples
#' W <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
#' spicar(W)
#' @export
spicar <- function(w, tau2 = 1) {
  W <- as.matrix(w)
  if (nrow(W) != ncol(W)) stop("`w` must be square")
  if (tau2 <= 0) stop("`tau2` must be > 0")
  n <- nrow(W)
  d <- rowSums(W)
  D <- diag(d, nrow = n)
  Q <- (D - W) / tau2
  rk <- qr(Q)$rank
  cond_var <- ifelse(d > 0, tau2 / ifelse(d > 0, d, 1), Inf)
  list(Q = Q, D = D, rank = rk, n_components = n - rk,
       is_improper = rk < n, conditional_variances = cond_var, tau2 = tau2)
}
