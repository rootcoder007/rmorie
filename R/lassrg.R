# SPDX-License-Identifier: AGPL-3.0-or-later
#' Least squares with an L1 penalty, which selects while it shrinks
#'
#' Ridge shrinks every coefficient and drops none; subset selection drops
#' but does not shrink, and its objective is combinatorial. The L1 penalty
#' is the convex relaxation that does both at once, and the
#' non-differentiable corner at zero is exactly the feature that produces
#' exact zeros rather than merely small numbers.
#'
#' An alias. The solver is \code{\link{Esllso}};
#' \code{ledger/wave2/DUPMAP.tsv} records \code{lassrg} as a duplicate of
#' \code{esllso} and it is the same problem solved the same way, so only
#' the argument order differs here.
#'
#' Formula: \code{min_beta ||y - X beta||^2 + lambda ||beta||_1} --
#' Tibshirani (1996).
#'
#' @param y Response of length n.
#' @param X Design matrix, n by p.
#' @param lam Penalty, non-negative.
#' @param max_iter Maximum sweeps.
#' @param tol Convergence tolerance.
#' @return Whatever \code{\link{Esllso}} returns, unchanged.
#' @references Tibshirani, R. (1996). Journal of the Royal Statistical
#'   Society Series B 58(1):267-288.
#'   \doi{10.1111/j.2517-6161.1996.tb02080.x}.
#' @seealso \code{\link{Esllso}}
#' @export
Lassrg <- function(y, X, lam, max_iter = 10000, tol = 1e-12) {
  Esllso(X, y, lam, max_iter, tol)
}
