# Global warming potentials (IPCC AR6 Table 7.SM.7).
# Source: IPCC AR6 WG1 Ch.7 Supplementary Material, Table 7.SM.7
# (fetched-wave3/ipcc-ar6-wg1-ch7-supplementary.pdf); GWP_H(x) =
# AGWP_x(H)/AGWP_CO2(H) (Sec. 7.SM.5).  Assessed horizons only
# (20/100/500 yr): CH4/N2O radiative efficiencies include
# horizon-dependent chemical adjustments, so no extrapolation.
# Mirrors Python morie.fn.gwPot exactly.

.gwpot_table <- list(
  "CO2"      = list(lifetime = NA_real_, re = 1.33e-5,
                    agwp = c("20" = 0.0243, "100" = 0.0895, "500" = 0.314),
                    gwp  = c("20" = 1,      "100" = 1,      "500" = 1)),
  "CH4"      = list(lifetime = 11.8, re = 3.88e-4,
                    agwp = c("20" = 1.98, "100" = 2.49, "500" = 2.5),
                    gwp  = c("20" = 81.2, "100" = 27.9, "500" = 7.95)),
  "N2O"      = list(lifetime = 109, re = 3.2e-3,
                    agwp = c("20" = 6.65, "100" = 24.5, "500" = 40.7),
                    gwp  = c("20" = 273,  "100" = 273,  "500" = 130)),
  "CFC-11"   = list(lifetime = 52, re = 0.291,
                    agwp = c("20" = 203, "100" = 557, "500" = 657),
                    gwp  = c("20" = 8320, "100" = 6230, "500" = 2090)),
  "CFC-12"   = list(lifetime = 102, re = 0.358,
                    agwp = c("20" = 310, "100" = 1120, "500" = 1790),
                    gwp  = c("20" = 12700, "100" = 12500, "500" = 5700)),
  "HFC-134a" = list(lifetime = 14, re = 0.167,
                    agwp = c("20" = 101, "100" = 137, "500" = 137),
                    gwp  = c("20" = 4140, "100" = 1530, "500" = 436)),
  "SF6"      = list(lifetime = 1000, re = 0.567,
                    agwp = c("20" = 442, "100" = 2180, "500" = 9100),
                    gwp  = c("20" = 18200, "100" = 24300, "500" = 29000))
)

#' Global warming potential (IPCC AR6 assessed values)
#'
#' Returns the assessed AR6 global warming potential
#' \eqn{GWP_H(x) = AGWP_x(H)/AGWP_{CO2}(H)} for a greenhouse gas at
#' an assessed horizon H of 20, 100 or 500 years, transcribed from
#' IPCC AR6 WG1 Chapter 7 Supplementary Material Table 7.SM.7.
#' No extrapolation to other horizons is offered because the CH4 and
#' N2O radiative efficiencies include horizon-dependent indirect
#' chemical adjustments.
#'
#' @param gas Character; one of "CO2", "CH4", "N2O", "CFC-11",
#'   "CFC-12", "HFC-134a", "SF6" (case-insensitive).
#' @param horizon Integer time horizon in years: 20, 100 (default)
#'   or 500.
#' @return A list with elements \code{estimate} (the GWP),
#'   \code{agwp}, \code{agwp_co2}, \code{gwp_from_agwp},
#'   \code{lifetime}, \code{radiative_efficiency}, \code{gas},
#'   \code{horizon}, \code{method}.
#' @references IPCC AR6 WG1 (2021) Chapter 7 Supplementary Material,
#'   Table 7.SM.7 and Sec. 7.SM.5; Hodnebrog et al. (2020).
#' @export
morie_gwpot <- function(gas, horizon = 100) {
  key <- toupper(gsub("_", "-", trimws(as.character(gas))))
  aliases <- c("CFC11" = "CFC-11", "CFC12" = "CFC-12",
               "HFC134A" = "HFC-134a", "HFC-134A" = "HFC-134a")
  if (key %in% names(aliases)) key <- aliases[[key]]
  tab_keys <- names(.gwpot_table)
  hit <- match(key, toupper(tab_keys))
  if (is.na(hit)) {
    stop("unknown gas '", gas, "'; known: ",
         paste(sort(tab_keys), collapse = ", "))
  }
  h <- as.integer(horizon)
  if (!h %in% c(20L, 100L, 500L)) {
    stop("horizon must be one of 20, 100, 500 (AR6 assessed horizons)")
  }
  row <- .gwpot_table[[hit]]
  hs <- as.character(h)
  agwp <- unname(row$agwp[[hs]])
  agwp_co2 <- unname(.gwpot_table[["CO2"]]$agwp[[hs]])
  list(estimate = unname(row$gwp[[hs]]),
       agwp = agwp, agwp_co2 = agwp_co2,
       gwp_from_agwp = agwp / agwp_co2,
       lifetime = row$lifetime,
       radiative_efficiency = row$re,
       gas = tab_keys[[hit]], horizon = h,
       method = "IPCC AR6 Table 7.SM.7 assessed GWP")
}
