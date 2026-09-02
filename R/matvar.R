# SPDX-License-Identifier: AGPL-3.0-or-later
#' Matern variogram model
#'
#' Matern (1960), Spatial Variation, Meddelanden fran Statens
#' Skogsforskningsinstitut 49(5), and Stein (1999), Interpolation of
#' Spatial Data, section 2.7, give C(h) = (2^(1-nu)/Gamma(nu)) (h/a)^nu
#' K_nu(h/a) with K_nu the modified Bessel function of the second kind.
#' The variogram with nugget c0 and partial sill c is gamma(h) = c0 + c(1
#' - C(h)) with gamma(0) = 0, the discontinuity at the origin being the
#' nugget itself -- which is why gamma(0) is returned as exactly zero
#' rather than as c0.  Neither source was retrievable here as a full text;
#' both expressions are quoted in their standard published form.  nu = 1/2
#' gives the exponential model, which is returned alongside for
#' comparison.
#'
#' @param h the lags.
#' @param c0 the nugget.
#' @param c the partial sill.
#' @param a the range.
#' @param nu the smoothness.
#' @return list: estimate, gamma, corr, exponential, nugget, sill, method.
#' @keywords internal
#' @examples
#' Maternvg(c(0, 0.5, 1, 2), 0, 1, 1, 0.5)$gamma
#' @export
Maternvg <- function(h, c0 = 0, c = 1, a = 1, nu = 0.5) {
  hs <- .s03vec(h)
  aa <- as.numeric(a)
  v <- as.numeric(nu)
  out <- numeric(length(hs))
  cor <- numeric(length(hs))
  expo <- numeric(length(hs))
  for (i in seq_along(hs)) {
    x <- hs[i]
    if (x <= 0) { out[i] <- 0
    cor[i] <- 1
    expo[i] <- 0
    next }
    u <- x / aa
    C <- (2^(1 - v) / exp(lgamma(v))) * (u^v) * .s03besselk(v, u)
    cor[i] <- C
    out[i] <- as.numeric(c0) + as.numeric(c) * (1 - C)
    expo[i] <- as.numeric(c0) + as.numeric(c) * (1 - exp(-u))
  }
  list(estimate = if (length(out)) out[1] else NaN, gamma = out, corr = cor,
       exponential = expo, nugget = as.numeric(c0),
       sill = as.numeric(c0) + as.numeric(c),
       method = "Matern semivariogram c0 + c (1 - C(h)) with C the Matern correlation")
}
