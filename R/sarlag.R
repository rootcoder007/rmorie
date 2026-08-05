# SPDX-License-Identifier: AGPL-3.0-or-later

#' Spatial autoregressive lag model -- alias of \code{sarla}.
#'
#' \code{y = rho W y + X beta + eps}, \code{eps ~ N(0, sigma2 I)}.
#'
#' An alias. The estimator already exists as \code{\link{sarla}} -- the
#' same concentrated maximum likelihood in \code{rho};
#' \code{ledger/wave2/DUPMAP.tsv} records \code{sarlag} as a duplicate of
#' \code{sarla}. Carrying the likelihood a second time would give two
#' copies that agree with each other at 1e-9 forever and are never
#' checked against anything else, so this only adapts the calling
#' convention: \code{sarla} takes \code{(x, y, w)}, this takes
#' \code{(y, X, W)}.
#'
#' @param y Response, length n.
#' @param X Design matrix (n by p); the intercept must be explicit.
#' @param W n-by-n weights matrix.
#' @return Whatever \code{\link{sarla}} returns, unchanged.
#' @references Anselin, L. (1988). Spatial Econometrics: Methods and
#'   Models. Ord, J. K. (1975). Estimation methods for models of spatial
#'   interaction. Journal of the American Statistical Association
#'   70(349), 120-126.
#' @seealso \code{\link{sarla}}
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
Sarlag <- function(y, X, W) {
  sarla(X, y, W)
}

#' @rdname Sarlag
#' @keywords internal
#' @export
morie_spatial_ar_lag_model <- Sarlag
