# SPDX-License-Identifier: AGPL-3.0-or-later

#' Spatial autoregressive error model (SEM) -- alias of \code{sarre}
#'
#' \code{y = X beta + u}, \code{u = lam W u + eps},
#' \code{eps ~ N(0, sigma2 I)}.
#'
#' An alias. The estimator already exists as \code{\link{sarre}} -- the
#' same concentrated maximum likelihood in \code{lam} over the admissible
#' eigenvalue interval. Carrying it a second time would give two copies
#' that agree with each other at 1e-9 forever and are never checked
#' against anything else, so this only adapts the calling convention:
#' \code{sarre} takes \code{(x, y, w)}, this takes \code{(y, X, W)}.
#'
#' \code{ledger/wave2/DUPMAP.tsv} originally recorded \code{sarerr} as a
#' duplicate of \code{lmerr}. That is wrong and the correction is
#' appended there: \code{lmerr} is Anselin's Lagrange multiplier
#' \emph{diagnostic} for spatial error dependence, a test statistic, not
#' the estimator.
#'
#' @param y Response, length n.
#' @param X Design matrix (n by p); the intercept must be explicit.
#' @param W n-by-n weights matrix.
#' @return Whatever \code{\link{sarre}} returns, unchanged.
#' @references Anselin, L. (1988). Spatial Econometrics: Methods and
#'   Models. Schabenberger, O. and Gotway, C. A. (2005). Statistical
#'   Methods for Spatial Data Analysis, Sec. 6.2.2, pp. 335-341.
#' @seealso \code{\link{sarre}}
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
Sarerr <- function(y, X, W) {
  sarre(X, y, W)
}

#' @rdname Sarerr
#' @keywords internal
#' @export
morie_spatial_ar_error_model <- Sarerr
