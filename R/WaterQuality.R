#' Water quality indices
#'
#' \code{CcmeWqi}: CCME Water Quality Index 1.0 (F1 scope, F2 frequency, F3
#' amplitude; \code{WQI = 100 - sqrt(F1^2 + F2^2 + F3^2) / 1.732}) against
#' use-specific objectives. \code{WeightedArithmeticWqi}: Brown et al. (1972)
#' index with unit weights \code{K / S_i}. \code{DoSaturation}: Benson and
#' Krause (1984) oxygen solubility with pressure correction.
#' \code{CarlsonTsi}: Carlson (1977) trophic state indices. Identical to the
#' Python arm \code{morie.fn.waterqual}.
#'
#' @param values Samples x variables matrix (\code{NA} = not measured) or
#'   parameter values.
#' @param objectives,standards Objectives or standards per variable.
#' @param direction \code{"max"} or \code{"min"} per variable.
#' @param ideal Ideal values (default 0).
#' @param temperature_c Water temperature (C).
#' @param salinity Salinity (g/kg).
#' @param pressure_atm Pressure (atm).
#' @param measured Measured DO for percent saturation.
#' @param secchi_m,chla_ugl,tp_ugl Secchi depth, chlorophyll-a and total P.
#' @return List.
#' @references Canadian Council of Ministers of the Environment (2001). CCME
#'   Water Quality Index 1.0, Technical Report.
#'
#'   Benson, B. B. and Krause, D. (1984). The concentration and isotopic
#'   fractionation of oxygen dissolved in freshwater and seawater in
#'   equilibrium with the atmosphere. Limnology and Oceanography 29, 620-632.
#'
#'   Carlson, R. E. (1977). A trophic state index for lakes. Limnology and
#'   Oceanography 22, 361-369.
#' @examples
#' CcmeWqi(rbind(c(1, 8), c(3, 5), c(1.5, 9)), c(2, 6), c("max", "min"))$wqi
#' DoSaturation(c(0, 20, 30))$cs
#' CarlsonTsi(secchi_m = 2, chla_ugl = 10, tp_ugl = 30)$state
#' @export
CcmeWqi <- function(values, objectives, direction = NULL) {
  X <- as.matrix(values)
  p <- length(objectives)
  if (is.null(direction)) direction <- rep("max", p)
  O <- matrix(objectives, nrow(X), p, byrow = TRUE)
  mx <- matrix(direction == "max", nrow(X), p, byrow = TRUE)
  ok <- !is.na(X)
  bad <- ok & ifelse(mx, X > O, X < O)
  ex <- ifelse(mx, X / O - 1, O / X - 1)
  n_tests <- sum(ok)
  F1 <- 100 * sum(colSums(bad) > 0) / p
  F2 <- 100 * sum(bad) / n_tests
  nse <- sum(ex[bad]) / n_tests
  F3 <- nse / (0.01 * nse + 0.01)
  wqi <- 100 - sqrt(F1^2 + F2^2 + F3^2) / 1.732
  cat <- if (wqi >= 95) "Excellent" else if (wqi >= 80) "Good" else if (wqi >= 65) "Fair" else if (wqi >= 45) {
    "Marginal"
  } else {
    "Poor"
  }
  list(F1 = F1, F2 = F2, F3 = F3, nse = nse, wqi = wqi, category = cat)
}

#' @rdname CcmeWqi
#' @export
WeightedArithmeticWqi <- function(values, standards, ideal = NULL) {
  if (is.null(ideal)) ideal <- rep(0, length(values))
  q <- 100 * (values - ideal) / (standards - ideal)
  w <- (1 / sum(1 / standards)) / standards
  list(q = q, w = w, wqi = sum(q * w) / sum(w))
}

#' @rdname CcmeWqi
#' @export
DoSaturation <- function(temperature_c, salinity = 0, pressure_atm = 1, measured = NULL) {
  tk <- temperature_c + 273.15
  cs <- exp(-139.34411 + 1.575701e5 / tk - 6.642308e7 / tk^2 + 1.243800e10 / tk^3 - 8.621949e11 / tk^4 -
              salinity * (0.017674 - 10.754 / tk + 2140.7 / tk^2))
  if (pressure_atm != 1) {
    pwv <- exp(11.8571 - 3840.70 / tk - 216961 / tk^2)
    th <- 0.000975 - 1.426e-5 * temperature_c + 6.436e-8 * temperature_c^2
    P <- pressure_atm
    cs <- cs * P * (1 - pwv / P) * (1 - th * P) / ((1 - pwv) * (1 - th))
  }
  out <- list(cs = cs)
  if (!is.null(measured)) out$percent_saturation <- 100 * measured / cs
  out
}

#' @rdname CcmeWqi
#' @export
CarlsonTsi <- function(secchi_m = NULL, chla_ugl = NULL, tp_ugl = NULL) {
  out <- list()
  if (!is.null(secchi_m)) out$tsi_sd <- 60 - 14.41 * log(secchi_m)
  if (!is.null(chla_ugl)) out$tsi_chl <- 9.81 * log(chla_ugl) + 30.6
  if (!is.null(tp_ugl)) out$tsi_tp <- 14.42 * log(tp_ugl) + 4.15
  if (length(out) == 0) stop("give at least one of secchi_m, chla_ugl, tp_ugl")
  m <- mean(unlist(out))
  out$tsi_mean <- m
  out$state <- if (m < 40) "oligotrophic" else if (m < 50) "mesotrophic" else if (m <= 70) "eutrophic" else {
    "hypereutrophic"
  }
  out
}

#' Irrigation suitability, solids, loads and treatment removal
#'
#' \code{IrrigationWaterQuality}: SAR, residual sodium carbonate, percent
#' sodium, Kelly's ratio, magnesium hazard and USSL salinity class (ions in
#' meq/L). \code{TotalDissolvedSolids}: \code{k * EC} and the ionic sum with
#' bicarbonate as carbonate (\code{0.4917 HCO3}; Hem 1985).
#' \code{SuspendedSolids}: APHA 2540 D/E gravimetric TSS and VSS.
#' \code{ConstituentLoad}: load and flow-weighted mean concentration.
#' \code{RemovalEfficiency}: percent removal and log removal value.
#'
#' @param na,ca,mg,k,hco3,co3 Ions (meq/L).
#' @param ec_us_cm Electrical conductivity (uS/cm).
#' @param kfac Conductance factor.
#' @param ions Dissolved constituents (mg/L) other than bicarbonate.
#' @param bicarbonate Bicarbonate (mg/L).
#' @param residue_mg,tare_mg,ignited_mg Filter weights (mg).
#' @param volume_ml Sample volume (mL).
#' @param concentration,flow Concentrations and flows.
#' @param dt Time step.
#' @param influent,effluent Concentrations.
#' @return List.
#' @references Richards, L. A. (ed.) (1954). Diagnosis and Improvement of
#'   Saline and Alkali Soils. USDA Agriculture Handbook 60.
#'
#'   Hem, J. D. (1985). Study and Interpretation of the Chemical
#'   Characteristics of Natural Water. USGS Water-Supply Paper 2254.
#' @examples
#' IrrigationWaterQuality(na = 6, ca = 3, mg = 5, k = 0.5, hco3 = 4, ec_us_cm = 900)$sar
#' SuspendedSolids(1523.4, 1510.2, 250, ignited_mg = 1514.6)$tss
#' RemovalEfficiency(c(200, 1e6), c(20, 1e2))$lrv
#' @export
IrrigationWaterQuality <- function(na, ca, mg, k = 0, hco3 = 0, co3 = 0, ec_us_cm = NULL) {
  out <- list(sar = na / sqrt((ca + mg) / 2), rsc = (co3 + hco3) - (ca + mg),
              percent_na = 100 * (na + k) / (ca + mg + na + k), kelly_ratio = na / (ca + mg),
              magnesium_hazard = 100 * mg / (ca + mg))
  if (!is.null(ec_us_cm)) {
    out$salinity_class <- if (ec_us_cm < 250) "C1" else if (ec_us_cm <= 750) "C2" else if (ec_us_cm <= 2250) {
      "C3"
    } else {
      "C4"
    }
  }
  out
}

#' @rdname IrrigationWaterQuality
#' @export
TotalDissolvedSolids <- function(ec_us_cm = NULL, kfac = 0.64, ions = NULL, bicarbonate = 0) {
  out <- list()
  if (!is.null(ec_us_cm)) out$tds_ec <- kfac * ec_us_cm
  if (!is.null(ions)) out$tds_ions <- sum(ions) + 0.4917 * bicarbonate
  out
}

#' @rdname IrrigationWaterQuality
#' @export
SuspendedSolids <- function(residue_mg, tare_mg, volume_ml, ignited_mg = NULL) {
  out <- list(tss = (residue_mg - tare_mg) * 1000 / volume_ml)
  if (!is.null(ignited_mg)) {
    out$vss <- (residue_mg - ignited_mg) * 1000 / volume_ml
    out$fss <- out$tss - out$vss
  }
  out
}

#' @rdname IrrigationWaterQuality
#' @export
ConstituentLoad <- function(concentration, flow, dt = 86400) {
  cq <- sum(concentration * flow)
  list(load = cq * dt, fwmc = cq / sum(flow))
}

#' @rdname IrrigationWaterQuality
#' @export
RemovalEfficiency <- function(influent, effluent) {
  list(percent = 100 * (1 - effluent / influent), lrv = log10(influent / effluent))
}
