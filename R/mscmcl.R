# SPDX-License-Identifier: AGPL-3.0-or-later
#' Impute the untreated counterfactual panel by nuclear-norm completion
#'
#' Athey et al. treat causal panel estimation as a matrix completion
#' problem: the untreated potential outcomes form one matrix \code{L}, of
#' which the control cells are observed and the treated cells are
#' missing. Estimating the treatment effect is then filling in the holes.
#' Their estimator regularises with the nuclear norm, the convex
#' relaxation of rank:
#' \code{min_L (1/|O|) sum_{(i,t) in O} (Y_it - L_it)^2 + lam ||L||_*}
#' with \code{O} the set of untreated cells.
#'
#' The solution is reached by the soft-impute iteration: fill the missing
#' cells with the current estimate, take the SVD of the completed matrix,
#' and soft-threshold its singular values,
#' \code{L <- U diag(max(s - lam, 0)) V'}, until the Frobenius change
#' falls below \code{tol}. Singular value THRESHOLDING, not truncation,
#' is what makes this the exact proximal step of the nuclear norm;
#' truncating to a fixed rank would solve a different, non-convex problem.
#'
#' \code{lam = 0} is a DEGENERATE limit, not the unregularised ideal.
#' With no shrinkage the SVD step reconstructs its input exactly, so the
#' iteration reaches \code{L = P_O(Y)} -- observed cells reproduced,
#' missing cells left at their zero start -- after two passes and stops.
#' The reported ATT is then simply the raw mean of \code{Y} over the
#' treated cells, with no counterfactual imputed at all. It is retained
#' as a sharp check that the projection step does what it claims, but it
#' must not be read as an estimate. Any real use wants \code{lam} on the
#' order of the panel's singular values.
#'
#' The iteration is fully deterministic -- zero start, fixed schedule,
#' fixed tolerance -- so both language arms land on the same numbers. The
#' SVD's sign convention cannot separate them either: \code{U diag(s) V'}
#' is invariant to flipping the sign of a matched column pair.
#'
#' @param y Observed outcome panel, units by periods.
#' @param D Treatment indicator, 1 where the cell is treated.
#' @param lam Nuclear-norm penalty, in the units of a singular value.
#' @param max_iter Iteration cap.
#' @param tol Frobenius convergence tolerance on successive iterates.
#' @return List with estimate (ATT), att, L, tau, n_treated, n_observed,
#'   rank, nuclear, iterations, converged, N, T.
#' @references Athey, Bayati, Doudchenko, Imbens and Khosravi (2021),
#'   JASA 116(536), 1716-1730, \doi{10.1080/01621459.2021.1891924},
#'   verified against Crossref (NBER working paper w25132). The article
#'   was not in the local corpus; the objective and the soft-impute step
#'   above are its standard published form, stated in full.
#' @export
Mscmcl <- function(y, D, lam, max_iter = 500, tol = 1e-10) {
  Y <- as.matrix(y); W <- as.matrix(D)
  if (length(dim(Y)) != 2L) stop("y must be a 2-D panel, units by periods")
  if (!identical(dim(W), dim(Y))) stop("D must have the same shape as y")
  N <- nrow(Y); Tt <- ncol(Y)
  lm <- as.numeric(lam)
  if (lm < 0) stop("lam must be non-negative")
  if (any(W != 0 & W != 1)) stop("D must be binary 0/1")
  obs <- W == 0
  n_obs <- sum(obs)
  if (n_obs == 0L) stop("every cell is treated; there is nothing to learn from")
  n_treated <- N * Tt - n_obs
  if (n_treated == 0L) stop("no cell is treated; there is no effect to estimate")
  L <- matrix(0, N, Tt)
  it <- 0L; converged <- FALSE
  for (it in seq_len(as.integer(max_iter))) {
    Z <- ifelse(obs, Y, L)
    sv <- svd(Z)
    s_th <- pmax(sv$d - lm, 0)
    Lnew <- sv$u %*% diag(s_th, nrow = length(s_th)) %*% t(sv$v)
    diff <- sqrt(sum((Lnew - L)^2))
    L <- Lnew
    if (diff <= tol) { converged <- TRUE; break }
  }
  d <- svd(L, nu = 0, nv = 0)$d
  tau <- as.numeric(t(Y - L))[as.logical(t(W))]
  att <- sum(tau) / n_treated
  eps <- 1e-10 * (if (length(d)) d[1] else 1)
  .t1_result(estimate = att, att = att, L = L, tau = tau,
             n_treated = n_treated, n_observed = n_obs,
             rank = sum(d > eps), nuclear = sum(d),
             iterations = it, converged = if (converged) 1 else 0,
             N = N, T = Tt,
             method = "Matrix completion for causal panel data (MC-NNM)")
}
