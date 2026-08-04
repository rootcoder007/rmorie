# SPDX-License-Identifier: AGPL-3.0-or-later
#' Grand-mean centering of a covariate
#'
#' Enders and Tofighi (2007), Centering predictor variables in
#' cross-sectional multilevel models, Psychological Methods 12(2),
#' 121-138: x_ij(CGM) = x_ij - xbar..  The paper is paywalled; the
#' transformation is arithmetic and is quoted in its standard published
#' form.  CGM leaves the between-cluster component of the predictor in
#' place, which is the distinction Enders and Tofighi draw against
#' centering within cluster.
#'
#' @param y the covariate, pooled over all clusters.
#' @return list: estimate, centered, grand_mean, sd, n, method.
#' @keywords internal
#' @examples
#' Gmcenter(c(1, 2, 3, 4))$grand_mean
#' @export
Gmcenter <- function(y) {
  v <- .s03vec(y)
  gm <- .s03mean(v)
  cc <- v - gm
  list(estimate = cc, centered = cc, grand_mean = gm,
       sd = if (length(v) > 1L) .s03sd(v, 1L) else NaN, n = length(v),
       method = "Grand-mean centering of a level-1 or level-2 covariate")
}
