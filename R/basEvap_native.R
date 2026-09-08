# FAO-56 Penman-Monteith reference evapotranspiration.
# Source: Allen, R. G., Pereira, L. S., Raes, D. and Smith, M. (1998),
# Crop evapotranspiration -- guidelines for computing crop water
# requirements, FAO Irrigation and Drainage Paper 56: eq. (6) for
# ET0, eq. (13) for the slope of the saturation vapour pressure
# curve, eq. (8) for the psychrometric constant, and eq. (11) for the
# saturation vapour pressure.
#
# Native implementation mirroring Python morie.fn.basEvap exactly.

#' FAO-56 Penman-Monteith reference evapotranspiration
#'
#' Reference crop evapotranspiration in mm/day from Allen et al.
#' (1998), eq. (6):
#' \deqn{ET_0 = \frac{0.408\Delta(R_n - G) + \gamma \frac{900}{T+273}
#'   u_2 D}{\Delta + \gamma(1 + 0.34 u_2)}}
#' with \eqn{\Delta} from their eq. (13) and \eqn{\gamma} from their
#' eq. (8).  The two terms are returned separately: the first is the
#' radiative (energy) contribution, the second the aerodynamic
#' (vapour-transport) contribution.
#'
#' @param T Mean air temperature at 2 m, degrees Celsius.
#' @param R_n Net radiation at the crop surface, MJ m-2 day-1.
#' @param u2 Wind speed at 2 m, m/s, non-negative.
#' @param VPD Vapour pressure deficit \eqn{e_s - e_a}, kPa,
#'   non-negative.
#' @param G Soil heat flux density, MJ m-2 day-1, default 0 (the
#'   recommended daily value).
#' @param P Atmospheric pressure, kPa, default 101.3 (sea level).
#' @return A list with \code{estimate} (ET0, mm/day),
#'   \code{radiative_term}, \code{aerodynamic_term}, \code{delta},
#'   \code{gamma}, and the inputs \code{T}, \code{R_n}, \code{u2},
#'   \code{VPD}, \code{G}, \code{P}, plus \code{method}.
#' @references Allen, R. G., Pereira, L. S., Raes, D. and Smith, M.
#'   (1998). Crop evapotranspiration. FAO Irrigation and Drainage
#'   Paper 56.
#' @export
#' @examples
#' morie_basEvap(T = 25, R_n = 15, u2 = 2, VPD = 1.5)
morie_basEvap <- function(T, R_n, u2, VPD, G = 0, P = 101.3) {
  T <- as.numeric(T)
  R_n <- as.numeric(R_n)
  u2 <- as.numeric(u2)
  VPD <- as.numeric(VPD)
  G <- as.numeric(G)
  P <- as.numeric(P)
  if (VPD < 0) stop("basEvap: VPD must be non-negative")
  if (u2 < 0) stop("basEvap: wind speed must be non-negative")
  if (P <= 0) stop("basEvap: pressure must be positive")
  es_T <- 0.6108 * exp(17.27 * T / (T + 237.3))
  delta <- 4098 * es_T / (T + 237.3)^2
  gamma <- 0.665e-3 * P
  denom <- delta + gamma * (1 + 0.34 * u2)
  rad <- 0.408 * delta * (R_n - G) / denom
  aero <- gamma * (900 / (T + 273)) * u2 * VPD / denom
  list(estimate = rad + aero, radiative_term = rad,
       aerodynamic_term = aero, delta = delta, gamma = gamma,
       T = T, R_n = R_n, u2 = u2, VPD = VPD, G = G, P = P,
       method = "FAO-56 Penman-Monteith ET0 (Allen et al. 1998, Eq. 6)")
}
