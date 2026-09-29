#' Soil process indices
#'
#' \code{Ec25Correction}: EC referred to 25 C. \code{CesiumRedistribution}:
#' caesium-137 proportional and profile models. \code{IpccSoilCarbon}: IPCC
#' Tier 1 stock and change. \code{RweqWindErosion}: Revised Wind Erosion
#' Equation. Identical to the Python arm \code{morie.fn.soilext}.
#'
#' @param ec Conductivity at temperature T.
#' @param temperature Temperature (C).
#' @param method \code{"exponential"} or \code{"linear"}.
#' @param inventory,reference Caesium-137 inventory and reference inventory.
#' @param depth Plough depth (m).
#' @param bulk_density Bulk density (kg per cubic m).
#' @param years Years since 1963 (or the IPCC transition time).
#' @param p Particle-size correction.
#' @param model \code{"proportional"} or \code{"profile"}.
#' @param h Relaxation mass depth.
#' @param soc_ref Reference stock (t C per ha).
#' @param f_lu,f_mg,f_i Land-use, management and input factors.
#' @param area Area (ha).
#' @param soc_initial Initial stock.
#' @param wf,ef,scf,k_prime,cog RWEQ factors (ef, scf may be NULL).
#' @param field_length Field length (m).
#' @param sand,silt,clay,om,caco3 Soil properties (percent).
#' @return A number, vector or list.
#' @references Sheets, K. R. and Hendrickx, J. M. H. (1995). Water Resources
#'   Research 31, 2401-2409.
#'
#'   Walling, D. E. and He, Q. (1999). Journal of Environmental Quality 28,
#'   611-622.
#'
#'   Fryrear, D. W. et al. (1998). Revised Wind Erosion Equation (RWEQ).
#'   USDA-ARS Technical Bulletin 1.
#' @examples
#' Ec25Correction(1.2, 15)
#' CesiumRedistribution(1800, 2400, 0.2, 1300, 50)
#' @export
Ec25Correction <- function(ec, temperature, method = "exponential") {
  switch(method,
    exponential = ec * (0.4470 + 1.4034 * exp(-temperature / 26.815)),
    linear = ec / (1 + 0.02 * (temperature - 25)),
    stop("method must be 'exponential' or 'linear'")
  )
}

#' @rdname Ec25Correction
#' @export
CesiumRedistribution <- function(inventory, reference, depth, bulk_density, years, p = 1, model = "proportional",
                                 h = 0) {
  x <- 100 * (reference - inventory) / reference
  switch(model,
    proportional = -10 * depth * bulk_density * x / (100 * years * p),
    profile = 10 / (years * p) * h * log(1 - x / 100),
    stop("model must be 'proportional' or 'profile'")
  )
}

#' @rdname Ec25Correction
#' @export
IpccSoilCarbon <- function(soc_ref, f_lu, f_mg, f_i, area = 1, soc_initial = NULL, years = 20) {
  stock <- soc_ref * f_lu * f_mg * f_i * area
  out <- list(stock = stock)
  if (!is.null(soc_initial)) {
    out$annual_change <- (stock - soc_initial) / years
    out$annual_co2 <- out$annual_change * 44 / 12
  }
  out
}

#' @rdname Ec25Correction
#' @export
RweqWindErosion <- function(wf, ef, scf, k_prime, cog, field_length = NULL, sand = NULL, silt = NULL, clay = NULL,
                            om = NULL, caco3 = NULL) {
  if (is.null(ef)) ef <- (29.09 + 0.31 * sand + 0.17 * silt + 0.33 * sand / clay - 2.59 * om - 0.95 * caco3) / 100
  if (is.null(scf)) scf <- 1 / (1 + 0.0066 * clay^2 + 0.021 * om^2)
  prod <- wf * ef * scf * k_prime * cog
  qmax <- 109.8 * prod
  s <- 150.71 * prod^-0.3711
  out <- list(q_max = qmax, critical_length = s, ef = ef, scf = scf)
  if (!is.null(field_length)) {
    out$transport <- qmax * (1 - exp(-(field_length / s)^2))
    out$soil_loss <- 2 * field_length / s^2 * qmax * exp(-(field_length / s)^2)
  }
  out
}
