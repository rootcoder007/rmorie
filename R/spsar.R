# SPDX-License-Identifier: AGPL-3.0-or-later
#' Spatial autoregressive (lag) model
#'
#' Same estimator as [sarla()], which fits the concentrated
#' log-likelihood in rho; this delegates rather than carrying a second
#' implementation.
#'
#' @param x Covariates (n by p).
#' @param y Response, length n.
#' @param w Spatial weights (n by n).
#' @return The result of `sarla()`.
#' @references Schabenberger & Gotway (2005), Sec 6.2.2.1
#'   "Simultaneous Autoregressive (SAR) Models", pp. 335-341.
#' @examples
#' n <- 20
#' W <- matrix(0, n, n); W[cbind(1:(n-1), 2:n)] <- 1; W <- W + t(W)
#' W <- W / pmax(rowSums(W), 1)
#' spsar(cbind(1, runif(n)), rnorm(n), W)
#' @export
spsar <- function(x, y, w) {
  sarla(x, y, w)
}
