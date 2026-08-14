# CO2 radiative forcing (IPCC AR6 / Meinshausen 2020; Myhre 1998 option).
# Sources: IPCC AR6 WG1 (2021), Chapter 7 Supplementary Material,
# Table 7.SM.1 and Section 7.SM.1.2. Meinshausen, M. et al. (2020),
# Geoscientific Model Development 13, 3571-3605 (the fit). Myhre, G.,
# Highwood, E. J., Shine, K. P. and Stordal, F. (1998), Geophysical
# Research Letters 25(14), 2715-2718 (the 5.35 ln(C/C0) expression).

# Table 7.SM.1 coefficients (IPCC AR6 WG1 Chapter 7 Supplementary
# Material, p. 3; Meinshausen et al. 2020 fit to the Oslo LBL cases)
.A1 <- -2.4785e-7   # W m-2 ppm-2
.B1 <- 7.5906e-4    # W m-2 ppm-1
.C1 <- -2.1492e-3   # W m-2 ppb-1/2 (N2O band-overlap term)
.D1 <- 5.2488       # W m-2
.C0_FIT <- 277.15   # ppm (table reference concentration)

radiative_forcing_co2 <- function(C, C0 = .C0_FIT, N = 273.87,
                                  method = "ar6",
                                  erf_adjustment = FALSE) {
  C <- as.numeric(C)
  C0 <- as.numeric(C0)
  N <- as.numeric(N)
  if (C <= 0 || C0 <= 0)
    stop("co2RF: concentrations must be positive")
  if (identical(method, "myhre1998")) {
    sarf <- 5.35 * log(C / C0)
    alpha <- NULL
  } else if (identical(method, "ar6")) {
    if (N < 0)
      stop("co2RF: N2O concentration must be non-negative")
    c_amax <- C0 - .B1 / (2 * .A1)
    if (C > c_amax) {
      alpha <- .D1 - .B1 * .B1 / (4 * .A1)
    } else if (C > C0) {
      alpha <- .D1 + .A1 * (C - C0)^2 + .B1 * (C - C0)
    } else {
      alpha <- .D1
    }
    sarf <- (alpha + .C1 * sqrt(N)) * log(C / C0)
  } else {
    stop("co2RF: method must be 'ar6' or 'myhre1998'")
  }
  est <- if (isTRUE(erf_adjustment)) sarf * 1.05 else sarf
  list(estimate = est,
       sarf = sarf,
       alpha_prime = alpha,
       method_used = method,
       C = C, C0 = C0, N = N,
       erf_adjustment = isTRUE(erf_adjustment),
       method = if (identical(method, "ar6"))
         "CO2 SARF, Meinshausen 2020 / AR6 Table 7.SM.1"
       else
         "CO2 SARF, Myhre 1998 5.35 ln(C/C0)")
}

co2RF <- radiative_forcing_co2

morie_co2RF <- radiative_forcing_co2

co2RF_cheatsheet <- function() {
  "co2RF(C, C0, N) -> AR6/Meinshausen-2020 CO2 SARF (Table 7.SM.1); method=myhre1998 for 5.35 ln(C/C0)"
}
