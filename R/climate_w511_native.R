# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Climate shelf, wave3 w5_11 batch 2 (coordinator): FAO-56
# Penman-Monteith ET0 (Basevap), empirical quantile mapping (Qmds),
# Planck wavelength-form spectrum (Plncf). Bit-identical mirrors of
# src/morie/fn/{basEvap,qmDS,plncF}.py.

#' FAO-56 Penman-Monteith reference evapotranspiration (Eq. 6)
#'
#' ET0 = (0.408 Delta (Rn - G) + gamma 900/(T+273) u2 VPD) /
#' (Delta + gamma (1 + 0.34 u2)), Delta per Eq. 13, gamma = 0.665e-3 P
#' per Eq. 8, G = 0 daily per Eq. 42. Reproduces the printed Example
#' 18 (Uccle): ET0 = 3.88 mm/day from T 16.9, Rn 13.28, u2 2.078,
#' VPD 0.589, P 100.1.
#'
#' @param T Mean daily air temperature at 2 m (deg C).
#' @param R_n Net radiation (MJ m-2 day-1).
#' @param u2 Wind speed at 2 m (m/s).
#' @param VPD Vapour pressure deficit es - ea (kPa).
#' @param G Soil heat flux (MJ m-2 day-1), 0 daily.
#' @param P Atmospheric pressure (kPa), 101.3 default.
#' @return List with \code{estimate}, \code{radiative_term},
#'   \code{aerodynamic_term}, \code{delta}, \code{gamma}, inputs,
#'   \code{method}.
#' @references Allen, R. G., Pereira, L. S., Raes, D. and Smith, M.
#'   (1998), FAO Irrigation and Drainage Paper 56, Eqs. 6, 8, 13, 42,
#'   Example 18. Local: fetched-wave3/fao56-x0490e0{6,7,8}.html,
#'   zotarelli-2010-fao56-step-by-step-AE459.pdf.
#' @export
Basevap <- function(T, R_n, u2, VPD, G = 0, P = 101.3) {
  T <- as.numeric(T)[1]; R_n <- as.numeric(R_n)[1]
  u2 <- as.numeric(u2)[1]; VPD <- as.numeric(VPD)[1]
  G <- as.numeric(G)[1]; P <- as.numeric(P)[1]
  if (VPD < 0) stop("Basevap: VPD must be non-negative", call. = FALSE)
  if (u2 < 0) stop("Basevap: wind speed must be non-negative", call. = FALSE)
  if (P <= 0) stop("Basevap: pressure must be positive", call. = FALSE)
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

.morie_qm_ecdf <- function(sx, v) {
  n <- length(sx)
  if (n == 1L) return(0.5)
  if (v <= sx[1]) return(0)
  if (v >= sx[n]) return(1)
  lo <- 1L; hi <- n
  while (hi - lo > 1L) {
    mid <- (lo + hi) %/% 2L
    if (sx[mid] <= v) lo <- mid else hi <- mid
  }
  g <- if (sx[lo + 1L] == sx[lo]) 0 else (v - sx[lo]) / (sx[lo + 1L] - sx[lo])
  (lo - 1L + g) / (n - 1L)
}

.morie_qm_quant <- function(sx, p) {
  n <- length(sx)
  if (n == 1L) return(sx[1])
  h <- (n - 1L) * p
  j <- floor(h)
  if (j >= n - 1L) return(sx[n])
  g <- h - j
  sx[j + 1L] * (1 - g) + sx[j + 2L] * g
}

#' Empirical quantile-mapping bias correction (QUANT)
#'
#' x' = F_obs^{-1}(F_mod(x)) (Gudmundsson et al. 2012, Eq. 2) with
#' both distributions empirical: piecewise-linear type-7-consistent
#' CDF and quantile function (percentile tables + linear
#' interpolation, Boe et al. 2007 procedure; beyond-range values take
#' the extreme trained quantile). Mapping a sample onto itself is the
#' identity; a pure shift is recovered exactly (test anchors).
#'
#' @param x_mod Modelled values to correct.
#' @param obs Observed calibration sample.
#' @param mod Modelled calibration sample.
#' @return List with \code{estimate}, \code{probs}, \code{n_obs},
#'   \code{n_mod}, \code{method}.
#' @references Gudmundsson, L., Bremnes, J. B., Haugen, J. E. and
#'   Engen-Skaugen, T. (2012), HESS 16, 3383-3390,
#'   \doi{10.5194/hess-16-3383-2012}, Eq. 2, Sec. 2.3.1. Local:
#'   fetched-wave3/gudmundsson-2012-quantile-mapping-hess16-3383.pdf.
#'   Wood et al. (2004) Climatic Change 62, 189-216; Boe et al.
#'   (2007) Int J Climatology 27, 1643-1655; Maraun (2013) J Climate
#'   26, 2137-2143.
#' @export
Qmds <- function(x_mod, obs, mod) {
  xm <- as.numeric(x_mod)
  ob <- sort(as.numeric(obs))
  md <- sort(as.numeric(mod))
  if (!length(ob) || !length(md)) {
    stop("Qmds: calibration samples must be non-empty", call. = FALSE)
  }
  probs <- vapply(xm, function(v) .morie_qm_ecdf(md, v), 0)
  est <- vapply(probs, function(p) .morie_qm_quant(ob, p), 0)
  list(estimate = est, probs = probs, n_obs = length(ob),
       n_mod = length(md),
       method = "empirical quantile mapping x' = F_obs^-1(F_mod(x)) (Gudmundsson 2012 Eq. 2, QUANT)")
}

#' Planck blackbody spectral radiance, wavelength form
#'
#' B(lam, T) = (2 h c^2 / lam^5) / (exp(hc/(lam kB T)) - 1)
#' (W sr-1 m-3), with the Wien displacement peak lam_max = b/T
#' (b = hc/(kB x*), x* the root of (x-5)e^x + 5 = 0) and the
#' Stefan-Boltzmann total power. Exactly consistent with the in-tree
#' frequency form (plank/Plank) via B_lam = B_nu(c/lam) c/lam^2.
#'
#' @param lam Wavelengths (m), > 0.
#' @param T Temperature (K), > 0.
#' @param h,c,kB Physical constants (2019 SI exact defaults).
#' @return List with \code{estimate}, \code{peak_wavelength},
#'   \code{wien_constant}, \code{total_power}, \code{T},
#'   \code{method}.
#' @references Planck, M. (1900), Verh. Dtsch. Phys. Ges. 2, 237-245;
#'   BIPM SI (2019) exact constants; CODATA 2018.
#' @export
Plncf <- function(lam, T, h = 6.62607015e-34, c = 299792458,
                  kB = 1.380649e-23) {
  lam <- as.numeric(lam)
  T <- as.numeric(T)[1]
  if (T <= 0) stop("Plncf: temperature must be > 0", call. = FALSE)
  if (any(lam <= 0)) stop("Plncf: wavelengths must be > 0", call. = FALSE)
  x <- pmin(h * c / (lam * kB * T), 700)
  B <- (2 * h * c * c / lam^5) / (exp(x) - 1 + 1e-300)
  xstar <- 4.965114231744276
  b_wien <- h * c / (kB * xstar)
  sigma <- 2 * pi^5 * kB^4 / (15 * h^3 * c^2)
  list(estimate = B, peak_wavelength = b_wien / T,
       wien_constant = b_wien, total_power = sigma * T^4, T = T,
       method = "Planck spectral radiance, wavelength form B(lam,T) = 2hc^2/lam^5 / (exp(hc/lam kB T)-1)")
}
