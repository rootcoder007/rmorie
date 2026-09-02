# SPDX-License-Identifier: AGPL-3.0-or-later
#' Clausius-Clapeyron scaling of saturation vapour pressure
#'
#' d e_s / dT = L_v e_s / (R_v T^2), so the fractional rate is L_v/(R_v T^2),
#' about 7 percent per kelvin near 288 K; integrating at constant latent heat
#' gives e_s(T) = 611.2 exp((L_v/R_v)(1/273.15 - 1/T)) Pa.  Source consulted:
#' Held and Soden (2006), Robust responses of the hydrological cycle to global
#' warming, Journal of Climate 19(21), 5686-5699.
#'
#' @param T absolute temperature in kelvin.
#' @param Lv latent heat of vaporisation, J/kg.
#' @param Rv gas constant for water vapour, J/(kg K).
#' @return list: estimate, rate, rate_percent_per_K, es, des_dt, T, n, method.
#' @keywords internal
#' @examples
#' clausC(288)
#' @export
clausC <- function(T, Lv = 2.501e6, Rv = 461.5) {
  es0 <- 611.2
  t0 <- 273.15
  Tv <- as.numeric(T)
  n <- length(Tv)
  es <- es0 * exp((Lv / Rv) * (1 / t0 - 1 / Tv))
  der <- Lv * es / (Rv * Tv^2)
  rate <- Lv / (Rv * Tv^2)
  list(estimate = mean(rate), rate = rate,
       rate_percent_per_K = 100 * mean(rate), es = es, des_dt = der,
       T = Tv, n = as.integer(n),
       method = "Clausius-Clapeyron scaling (Held & Soden 2006)")
}

# CANONICAL TEST
# r <- clausC(288); stopifnot(r$estimate > 0.06, r$estimate < 0.08)
# stopifnot(abs(clausC(273.15)$es[1] - 611.2) < 1e-9)

#' @rdname clausC
#' @keywords internal
#' @export
morie_clausius_clapeyron <- clausC
