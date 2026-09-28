#' Soil hydraulic pedotransfer functions and retention curves
#'
#' \code{SaxtonRawls}: Saxton and Rawls (2006) Table 1 moisture regressions,
#' density effects and conductivity from sand and clay fractions and percent
#' organic matter. \code{VanGenuchten}: van Genuchten (1980) retention and
#' Mualem conductivity. Identical to the Python arm \code{morie.fn.soilhyd}.
#'
#' @param sand,clay Weight fractions.
#' @param om Organic matter (percent).
#' @param density_factor Density adjustment factor.
#' @param h Suction (positive).
#' @param theta_r,theta_s Residual and saturated water contents.
#' @param alpha,n Van Genuchten parameters.
#' @param ks Saturated conductivity.
#' @param pore_l Pore-connectivity parameter.
#' @return List.
#' @references Saxton, K. E. and Rawls, W. J. (2006). Soil water
#'   characteristic estimates by texture and organic matter for hydrologic
#'   solutions. Soil Science Society of America Journal 70, 1569-1578.
#'
#'   van Genuchten, M. Th. (1980). A closed-form equation for predicting the
#'   hydraulic conductivity of unsaturated soils. Soil Science Society of
#'   America Journal 44, 892-898.
#' @examples
#' SaxtonRawls(0.4, 0.2, 2.5)$theta33
#' VanGenuchten(c(0, 100), 0.05, 0.45, 0.02, 1.5, ks = 10)$theta
#' @export
SaxtonRawls <- function(sand, clay, om, density_factor = 1) {
  S <- sand
  C <- clay
  OM <- om
  t15t <- -0.024 * S + 0.487 * C + 0.006 * OM + 0.005 * S * OM - 0.013 * C * OM + 0.068 * S * C + 0.031
  t15 <- t15t + (0.14 * t15t - 0.02)
  t33t <- -0.251 * S + 0.195 * C + 0.011 * OM + 0.006 * S * OM - 0.027 * C * OM + 0.452 * S * C + 0.299
  t33 <- t33t + (1.283 * t33t^2 - 0.374 * t33t - 0.015)
  ts33t <- 0.278 * S + 0.034 * C + 0.022 * OM - 0.018 * S * OM - 0.027 * C * OM - 0.584 * S * C + 0.078
  ts33 <- ts33t + (0.636 * ts33t - 0.107)
  pet <- -21.67 * S - 27.93 * C - 81.97 * ts33 + 71.12 * S * ts33 + 8.29 * C * ts33 + 14.05 * S * C + 27.16
  pe <- pet + (0.02 * pet^2 - 0.113 * pet - 0.70)
  ts <- t33 + ts33 - 0.097 * S + 0.043
  rho_n <- (1 - ts) * 2.65
  rho_df <- rho_n * density_factor
  ts_df <- 1 - rho_df / 2.65
  B <- (log(1500) - log(33)) / (log(t33) - log(t15))
  lam <- 1 / B
  list(theta1500 = t15, theta33 = t33, theta_s = ts, available_water = t33 - t15, psi_e = pe, rho_n = rho_n, B = B,
       A = exp(log(33) + B * log(t33)), lambda = lam, ksat = 1930 * (ts - t33)^(3 - lam), theta_s_df = ts_df,
       theta33_df = t33 - 0.2 * (ts - ts_df), rho_df = rho_df)
}

#' @rdname SaxtonRawls
#' @export
VanGenuchten <- function(h, theta_r, theta_s, alpha, n, ks = NULL, pore_l = 0.5) {
  m <- 1 - 1 / n
  se <- ifelse(h <= 0, 1, (1 + (alpha * h)^n)^(-m))
  out <- list(theta = theta_r + (theta_s - theta_r) * se, Se = se)
  if (!is.null(ks)) out$K <- ks * se^pore_l * (1 - (1 - se^(1 / m))^m)^2
  out
}

#' Infiltration, runoff and erosion models
#'
#' \code{Infiltration}: Horton (1940), Philip (1957) and Green and Ampt
#' (1911) rates and cumulative depths. \code{ScsRunoff}: SCS curve-number
#' runoff and runoff coefficient (USDA-SCS 1972). \code{Rusle}: \eqn{A = R K
#' LS C P} with the Wischmeier and Smith (1978) LS factor.
#'
#' @param t Times.
#' @param model \code{"horton"}, \code{"philip"} or \code{"green_ampt"}.
#' @param f0,fc,k Horton parameters.
#' @param S,A Philip sorptivity and transmissivity.
#' @param Ks,psi,dtheta Green-Ampt conductivity, wetting-front suction and
#'   moisture deficit.
#' @param P Rainfall (mm) or, for \code{Rusle}, the support practice factor.
#' @param CN Curve number.
#' @param ia_ratio Initial abstraction ratio.
#' @param R,K,C RUSLE factors.
#' @param slope_length Slope length (m).
#' @param slope_pct Slope (percent).
#' @return List.
#' @references Green, W. H. and Ampt, G. A. (1911). Studies on soil physics.
#'   Journal of Agricultural Science 4, 1-24.
#'
#'   Wischmeier, W. H. and Smith, D. D. (1978). Predicting Rainfall Erosion
#'   Losses. USDA Agriculture Handbook 537.
#' @examples
#' Infiltration(1, "horton", f0 = 10, fc = 2, k = 1)$rate
#' ScsRunoff(50, 80)$runoff
#' Rusle(100, 0.3, 0.2, 1, slope_length = 22.13, slope_pct = 9)$LS
#' @export
Infiltration <- function(t, model = "horton", f0 = NULL, fc = NULL, k = NULL, S = NULL, A = NULL, Ks = NULL,
                         psi = NULL, dtheta = NULL) {
  if (model == "horton") {
    return(list(rate = fc + (f0 - fc) * exp(-k * t), cumulative = fc * t + (f0 - fc) * (1 - exp(-k * t)) / k))
  }
  if (model == "philip") return(list(rate = ifelse(t > 0, 0.5 * S / sqrt(t) + A, Inf), cumulative = S * sqrt(t) + A * t))
  if (model != "green_ampt") stop("model must be horton, philip or green_ampt")
  pd <- psi * dtheta
  cum <- vapply(t, function(v) {
    F <- max(Ks * v, sqrt(2 * pd * Ks * v), 1e-12)
    for (it in 1:200) {
      dF <- (F - pd * log(1 + F / pd) - Ks * v) / (1 - pd / (pd + F))
      F <- F - dF
      if (abs(dF) < 1e-14 * max(1, F)) break
    }
    F
  }, 0)
  list(rate = ifelse(cum > 0, Ks * (1 + pd / cum), Inf), cumulative = cum)
}

#' @rdname Infiltration
#' @export
ScsRunoff <- function(P, CN, ia_ratio = 0.2) {
  if (CN <= 0 || CN > 100) stop("CN must be in (0, 100]")
  S <- 25400 / CN - 254
  Ia <- ia_ratio * S
  Q <- ifelse(P > Ia, (P - Ia)^2 / (P - Ia + S), 0)
  list(runoff = Q, coefficient = ifelse(P > 0, Q / P, 0), S = S, Ia = Ia)
}

#' @rdname Infiltration
#' @export
Rusle <- function(R, K, C, P, slope_length, slope_pct) {
  m <- if (slope_pct >= 5) 0.5 else if (slope_pct >= 3.5) 0.4 else if (slope_pct >= 1) 0.3 else 0.2
  th <- atan(slope_pct / 100)
  LS <- (slope_length / 22.13)^m * (65.41 * sin(th)^2 + 4.56 * sin(th) + 0.065)
  list(A = R * K * LS * C * P, LS = LS, m = m)
}

#' Soil chemistry, carbon, texture and Sobel filtering
#'
#' \code{SoilChemistry}: SAR, CEC (sum of cations) and ESP (U.S. Salinity
#' Laboratory Staff 1954). \code{SoilCarbon}: SOC stock, Pieri (1992)
#' structure index and mean weight diameter (van Bavel 1950).
#' \code{UsdaTexture}: USDA texture triangle class. \code{SobelFilter}:
#' Sobel gradients and magnitude.
#'
#' @param na,ca,mg,k,h_al Cations.
#' @param na_ex Exchangeable sodium (default \code{na}).
#' @param soc_pct Organic carbon (percent).
#' @param bulk_density Bulk density (g/cm3).
#' @param depth_cm Layer depth (cm).
#' @param coarse_fraction Coarse fragment fraction.
#' @param clay_pct,silt_pct Percentages.
#' @param aggregate_fractions,aggregate_diameters Aggregate classes.
#' @param sand,silt,clay Percentages summing to 100.
#' @param grid Numeric matrix.
#' @param res Cell size.
#' @return List or character.
#' @references Pieri, C. (1992). Fertility of Soils: A Future for Farming in
#'   the West African Savannah. Springer.
#' @examples
#' SoilChemistry(na = 10, ca = 4, mg = 4, k = 1, h_al = 1)$sar
#' SoilCarbon(1, 1.3, 30)$stock
#' UsdaTexture(40, 40, 20)
#' @export
SoilChemistry <- function(na, ca, mg, k = 0, h_al = 0, na_ex = NULL) {
  cec <- ca + mg + k + na + h_al
  list(sar = na / sqrt((ca + mg) / 2), cec = cec, esp = 100 * (if (is.null(na_ex)) na else na_ex) / cec)
}

#' @rdname SoilChemistry
#' @export
SoilCarbon <- function(soc_pct, bulk_density, depth_cm, coarse_fraction = 0, clay_pct = NULL, silt_pct = NULL,
                       aggregate_fractions = NULL, aggregate_diameters = NULL) {
  out <- list(stock = soc_pct * bulk_density * depth_cm * (1 - coarse_fraction))
  if (!is.null(clay_pct) && !is.null(silt_pct)) out$pieri_si <- 1.724 * soc_pct / (clay_pct + silt_pct) * 100
  if (!is.null(aggregate_fractions)) {
    out$mwd <- sum(aggregate_fractions * aggregate_diameters) / sum(aggregate_fractions)
  }
  out
}

#' @rdname SoilChemistry
#' @export
UsdaTexture <- function(sand, silt, clay) {
  s <- sand
  si <- silt
  c <- clay
  if (abs(s + si + c - 100) > 1e-6) stop("sand + silt + clay must be 100")
  if (si + 1.5 * c < 15) return("sand")
  if (si + 2 * c < 30) return("loamy sand")
  if ((c >= 7 && c < 20 && s > 52 && si + 2 * c >= 30) || (c < 7 && si < 50 && si + 2 * c >= 30)) return("sandy loam")
  if (c >= 7 && c < 27 && si >= 28 && si < 50 && s <= 52) return("loam")
  if ((si >= 50 && c >= 12 && c < 27) || (si >= 50 && si < 80 && c < 12)) return("silt loam")
  if (si >= 80 && c < 12) return("silt")
  if (c >= 20 && c < 35 && si < 28 && s > 45) return("sandy clay loam")
  if (c >= 27 && c < 40 && s > 20 && s <= 45) return("clay loam")
  if (c >= 27 && c < 40 && s <= 20) return("silty clay loam")
  if (c >= 35 && s > 45) return("sandy clay")
  if (c >= 40 && si >= 40) return("silty clay")
  "clay"
}

#' @rdname SoilChemistry
#' @export
SobelFilter <- function(grid, res = 1) {
  G <- as.matrix(grid)
  nr <- nrow(G)
  nc <- ncol(G)
  gx <- gy <- matrix(NaN, nr, nc)
  if (nr >= 3 && nc >= 3) {
    for (i in 2:(nr - 1)) {
      for (j in 2:(nc - 1)) {
        z <- as.vector(t(G[(i - 1):(i + 1), (j - 1):(j + 1)]))
        gx[i, j] <- ((z[3] + 2 * z[6] + z[9]) - (z[1] + 2 * z[4] + z[7])) / (8 * res)
        gy[i, j] <- ((z[7] + 2 * z[8] + z[9]) - (z[1] + 2 * z[2] + z[3])) / (8 * res)
      }
    }
  }
  list(gx = gx, gy = gy, magnitude = sqrt(gx^2 + gy^2))
}
