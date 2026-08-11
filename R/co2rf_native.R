# SPDX-License-Identifier: AGPL-3.0-or-later
#
# CO2 radiative forcing (Co2rf), IPCC AR6 / Meinshausen 2020.
# Bit-identical mirror of src/morie/fn/co2RF.py; coefficients from
# Table 7.SM.1 read off the rendered supplementary page.

#' CO2 stratospheric-temperature-adjusted radiative forcing (SARF)
#'
#' method = "ar6" (default): Meinshausen et al. (2020) expression as
#' adopted by IPCC AR6 Table 7.SM.1 -- piecewise alpha in (C - C0)
#' with the c1 sqrt(N) N2O band-overlap term, times ln(C/C0).
#' Printed anchor (7.SM.1.2): doubling from the 1750 baseline
#' (C0 = 278.3 ppm, N = 270.1 ppb) gives SARF 3.75 W m-2, ERF (+5%)
#' 3.93 W m-2. method = "myhre1998": 5.35 ln(C/C0) (AR5 and earlier;
#' the stub's documented formula, retained and labelled).
#'
#' @param C CO2 concentration (ppm).
#' @param C0 Reference concentration (ppm); 277.15 table value,
#'   278.3 for 1750-relative forcing.
#' @param N N2O concentration (ppb) for the overlap term.
#' @param method "ar6" or "myhre1998".
#' @param erf_adjustment If TRUE add the AR6 +5% tropospheric
#'   adjustment.
#' @return List with \code{estimate}, \code{sarf}, \code{alpha_prime},
#'   \code{method_used}, inputs, \code{method}.
#' @references IPCC AR6 WG1 (2021), Ch. 7 Supplementary Material,
#'   Table 7.SM.1, Sec. 7.SM.1.2 (local: fetched-wave3/
#'   ipcc-ar6-wg1-ch7-supplementary.pdf p. 3); Meinshausen, M. et al.
#'   (2020), Geosci. Model Dev. 13, 3571-3605; Myhre, G. et al.
#'   (1998), GRL 25(14), 2715-2718.
#' @export
Co2rf <- function(C, C0 = 277.15, N = 273.87, method = "ar6",
                  erf_adjustment = FALSE) {
  a1 <- -2.4785e-7; b1 <- 7.5906e-4; c1 <- -2.1492e-3; d1 <- 5.2488
  C <- as.numeric(C)[1]; C0 <- as.numeric(C0)[1]; N <- as.numeric(N)[1]
  if (C <= 0 || C0 <= 0) stop("Co2rf: concentrations must be positive", call. = FALSE)
  if (method == "myhre1998") {
    sarf <- 5.35 * log(C / C0)
    alpha <- NULL
  } else if (method == "ar6") {
    if (N < 0) stop("Co2rf: N2O concentration must be non-negative", call. = FALSE)
    c_amax <- C0 - b1 / (2 * a1)
    alpha <- if (C > c_amax) d1 - b1 * b1 / (4 * a1)
             else if (C > C0) d1 + a1 * (C - C0)^2 + b1 * (C - C0)
             else d1
    sarf <- (alpha + c1 * sqrt(N)) * log(C / C0)
  } else stop("Co2rf: method must be ar6 or myhre1998", call. = FALSE)
  est <- if (erf_adjustment) sarf * 1.05 else sarf
  list(estimate = est, sarf = sarf, alpha_prime = alpha,
       method_used = method, C = C, C0 = C0, N = N,
       erf_adjustment = erf_adjustment,
       method = if (method == "ar6") "CO2 SARF, Meinshausen 2020 / AR6 Table 7.SM.1" else "CO2 SARF, Myhre 1998 5.35 ln(C/C0)")
}
