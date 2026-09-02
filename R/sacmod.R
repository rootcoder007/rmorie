# SPDX-License-Identifier: AGPL-3.0-or-later

#' SAC model -- spatial lag and spatial error sharing one weights matrix
#'
#' \code{y = rho W y + X beta + u}, \code{u = lam W u + eps},
#' \code{eps ~ N(0, sigma2 I)}.
#'
#' An alias. This is the SARAR model of \code{\link{Sarmix}} at
#' \code{W1 = W2 = W}; LeSage and Pace (2009) call it the SAC
#' specification and note it is the general nesting model from which SAR
#' (\code{lam = 0}) and SEM (\code{rho = 0}) drop out. Carrying the
#' concentrated likelihood a second time would give two copies that agree
#' with each other at 1e-9 forever and are never checked against anything
#' else, so this only fixes the second weights argument; the audit note
#' is recorded in \code{ledger/wave2/DUPMAP.tsv}.
#'
#' @param y Response, length n.
#' @param X Design matrix (n by p); the intercept must be explicit.
#' @param W Spatial weights, used for both the lag and the disturbance.
#' @return Whatever \code{\link{Sarmix}} returns, with \code{method}
#'   relabelled.
#' @references LeSage, J. and Pace, R. K. (2009). Introduction to Spatial
#'   Econometrics. Chapman & Hall/CRC, Sec. 2.4 (the SAC model).
#'   Kelejian, H. H. and Prucha, I. R. (1998). The Journal of Real Estate
#'   Finance and Economics 17(1), 99-121. \doi{10.1023/A:1007707430416}.
#'   Anselin, L. (1988). Spatial Econometrics: Methods and Models.
#' @seealso \code{\link{Sarmix}}
#' @examples
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' @export
Sacmod <- function(y, X, W) {
  res <- Sarmix(y, X, W, W)
  res$method <- "SAC (spatial lag + spatial error, one W) by concentrated ML"
  res
}

#' @rdname Sacmod
#' @keywords internal
#' @export
morie_spatial_combined <- Sacmod
