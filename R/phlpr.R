# SPDX-License-Identifier: AGPL-3.0-or-later
#' Phillips-Perron unit-root test.
#'
#' Thin re-export of \code{Pptest}, which carries the implementation,
#' the Bartlett long-run variance and the Dickey-Fuller tables.  Three
#' modules in this package spelled the same test three different ways;
#' the arithmetic lives in one place so they cannot drift apart.
#'
#' @param y Series in time order.
#' @param lags Bartlett truncation lag; short-lag rule if NULL.
#' @param kind "Z(t_alpha)" or "Z(alpha)".
#' @return As \code{Pptest}.
#' @references Phillips and Perron (1988), Biometrika 75:335-346; coded form from tseries::pp.test.  See Pptest for the full note.
#' @export
#' @examples
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' Ppunit(D)
Ppunit <- function(y, lags = NULL, kind = "Z(t_alpha)") Pptest(y, lags = lags, kind = kind)
