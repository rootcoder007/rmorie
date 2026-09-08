# SPDX-License-Identifier: AGPL-3.0-or-later
#' Phillips-Perron unit-root test with an explicit trend switch
#'
#' The implementation lives in \code{Pptest}, whose auxiliary regression
#' always includes the linear trend -- that is the tseries::pp.test
#' specification and the one the tabulated critical values belong to.
#' \code{trend = FALSE} is refused rather than silently answered with
#' trend-case critical values.
#'
#' @param y Series in time order.
#' @param trend Must be TRUE; see above.
#' @param lags Bartlett truncation lag.
#' @param kind "Z(t_alpha)" or "Z(alpha)".
#' @return As \code{Pptest}.
#' @references Phillips and Perron (1988), Biometrika 75:335-346; coded form from
#' tseries::pp.test.  See Pptest for the full note.
#' @export
#' @examples
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' Pptrend(D)
Pptrend <- function(y, trend = TRUE, lags = NULL, kind = "Z(t_alpha)") {
  if (!isTRUE(trend))
    stop("the tabulated critical values carried here are for the trend-included regression; trend = FALSE is not available")
  Pptest(y, lags = lags, kind = kind)
}
