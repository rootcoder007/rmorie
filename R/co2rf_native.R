# CO2 radiative forcing.
# Source: IPCC AR6 WG1 (2021) Ch. 7 Supplementary Material,
# Table 7.SM.1 and Sec. 7.SM.1.2 (fetched-wave3/
# ipcc-ar6-wg1-ch7-supplementary.pdf, p. 3); Meinshausen et al.
# (2020), Geosci. Model Dev. 13, 3571-3605 (the fit); Myhre et al.
# (1998), Geophys. Res. Lett. 25(14), 2715-2718 (5.35 ln(C/C0)).
# Mirrors Python morie.fn.co2RF exactly.

.CO2RF_A1 <- -2.4785e-7   # W m-2 ppm-2
.CO2RF_B1 <- 7.5906e-4    # W m-2 ppm-1
.CO2RF_C1 <- -2.1492e-3   # W m-2 ppb-1/2 (N2O band overlap)
.CO2RF_D1 <- 5.2488       # W m-2

#' Stratospheric-temperature-adjusted radiative forcing of CO2
#'
#' method = "ar6" (default) is the Meinshausen et al. (2020)
#' simplified expression adopted by IPCC AR6 (Table 7.SM.1):
#' C_amax = C0 - b1/(2 a1); alpha' = d1 - b1^2/(4 a1) for C > C_amax,
#' d1 + a1 (C-C0)^2 + b1 (C-C0) for C0 < C < C_amax, else d1; and
#' SARF = (alpha' + c1 sqrt(N)) log(C/C0), with N the N2O
#' concentration (ppb).  method = "myhre1998" gives the older
#' 5.35 log(C/C0) used through AR5.  \code{erf_adjustment = TRUE}
#' applies the AR6 +5\% tropospheric adjustment (ERF).
#'
#' @param C CO2 concentration (ppm).
#' @param C0 Reference concentration (ppm); table value 277.15, use
#'   278.3 for forcing relative to 1750.
#' @param N N2O concentration (ppb) for the overlap term; table
#'   reference 273.87, 1750 value 270.1.
#' @param method "ar6" or "myhre1998".
#' @param erf_adjustment Logical; add the +5\% adjustment.
#' @return A list with elements \code{estimate}, \code{sarf},
#'   \code{alpha_prime}, \code{method_used}, \code{C}, \code{C0},
#'   \code{N}, \code{erf_adjustment}, \code{method}.
#' @references IPCC AR6 WG1 (2021), Chapter 7 Supplementary Material,
#'   Table 7.SM.1. Meinshausen, M. et al. (2020). Geoscientific Model
#'   Development, 13, 3571-3605. Myhre, G. et al. (1998).
#'   Geophysical Research Letters, 25(14), 2715-2718.
#' @export
morie_co2RF <- function(C, C0 = 277.15, N = 273.87, method = "ar6",
                        erf_adjustment = FALSE) {
  C <- as.numeric(C); C0 <- as.numeric(C0); N <- as.numeric(N)
  if (C <= 0 || C0 <= 0)
    stop("co2RF: concentrations must be positive")
  alpha <- NULL
  if (method == "myhre1998") {
    sarf <- 5.35 * log(C / C0)
  } else if (method == "ar6") {
    if (N < 0) stop("co2RF: N2O concentration must be non-negative")
    c_amax <- C0 - .CO2RF_B1 / (2 * .CO2RF_A1)
    alpha <- if (C > c_amax) {
      .CO2RF_D1 - .CO2RF_B1 * .CO2RF_B1 / (4 * .CO2RF_A1)
    } else if (C > C0) {
      .CO2RF_D1 + .CO2RF_A1 * (C - C0)^2 + .CO2RF_B1 * (C - C0)
    } else {
      .CO2RF_D1
    }
    sarf <- (alpha + .CO2RF_C1 * sqrt(N)) * log(C / C0)
  } else {
    stop("co2RF: method must be 'ar6' or 'myhre1998'")
  }
  est <- if (isTRUE(erf_adjustment)) sarf * 1.05 else sarf
  list(estimate = est, sarf = sarf, alpha_prime = alpha,
       method_used = method, C = C, C0 = C0, N = N,
       erf_adjustment = isTRUE(erf_adjustment),
       method = if (method == "ar6")
         "CO2 SARF, Meinshausen 2020 / AR6 Table 7.SM.1"
       else "CO2 SARF, Myhre 1998 5.35 ln(C/C0)")
}
