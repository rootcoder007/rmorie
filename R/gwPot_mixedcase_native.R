# morie native arm -- gwPot
# Global warming potentials, IPCC AR6 WG1 Table 7.SM.7.
#
#   GWP_H(x) = AGWP_x(H) / AGWP_CO2(H)
#
# Returns the ASSESSED AR6 values at the assessed horizons
# H in {20, 100, 500} yr. It deliberately does not extrapolate to
# other horizons: the CH4 and N2O radiative efficiencies carry
# horizon-dependent indirect chemical adjustments that a pure
# exponential-decay AGWP cannot reproduce, so an interpolated GWP
# would look precise and be wrong.
#
# IPCC AR6 WG1 (2021) Ch. 7 Supplementary Material, Table 7.SM.7 and
# Sec. 7.SM.5; Hodnebrog et al. (2020) Rev. Geophys. 58(3),
# doi:10.1029/2019RG000691.

.gwPot_TABLE <- list(
  "CO2"      = list(lifetime = NA_real_, re = 1.33e-5,
                    agwp = c("20" = 0.0243, "100" = 0.0895, "500" = 0.314),
                    gwp  = c("20" = 1,      "100" = 1,      "500" = 1)),
  "CH4"      = list(lifetime = 11.8, re = 3.88e-4,
                    agwp = c("20" = 1.98,  "100" = 2.49,  "500" = 2.5),
                    gwp  = c("20" = 81.2,  "100" = 27.9,  "500" = 7.95)),
  "N2O"      = list(lifetime = 109, re = 3.2e-3,
                    agwp = c("20" = 6.65,  "100" = 24.5,  "500" = 40.7),
                    gwp  = c("20" = 273,   "100" = 273,   "500" = 130)),
  "CFC-11"   = list(lifetime = 52, re = 0.291,
                    agwp = c("20" = 203,   "100" = 557,   "500" = 657),
                    gwp  = c("20" = 8320,  "100" = 6230,  "500" = 2090)),
  "CFC-12"   = list(lifetime = 102, re = 0.358,
                    agwp = c("20" = 310,   "100" = 1120,  "500" = 1790),
                    gwp  = c("20" = 12700, "100" = 12500, "500" = 5700)),
  "HFC-134a" = list(lifetime = 14, re = 0.167,
                    agwp = c("20" = 101,   "100" = 137,   "500" = 137),
                    gwp  = c("20" = 4140,  "100" = 1530,  "500" = 436)),
  "SF6"      = list(lifetime = 1000, re = 0.567,
                    agwp = c("20" = 442,   "100" = 2180,  "500" = 9100),
                    gwp  = c("20" = 18200, "100" = 24300, "500" = 29000))
)

#' morie_gwPot
#'
#' Part of the gwPot_mixedcase_native implementation; see the file
#' header for the source it follows.
#'
#' @param gas See Usage.
#' @param horizon Defaults to \code{100}.
#' @return A list with \code{estimate}, \code{agwp}, \code{agwp_co2}, \code{gwp_from_agwp}, \code{lifetime}, \code{radiative_efficiency}, \code{gas}, \code{horizon}, \code{method}.
#' @export
morie_gwPot <- function(gas, horizon = 100) {
  key <- toupper(gsub("_", "-", trimws(as.character(gas))))
  aliases <- c("CFC11" = "CFC-11", "CFC12" = "CFC-12",
               "HFC134A" = "HFC-134a", "HFC-134A" = "HFC-134a")
  if (key %in% names(aliases)) key <- unname(aliases[key])
  if (!(key %in% names(.gwPot_TABLE))) {
    stop(sprintf("unknown gas '%s'; known: %s", gas,
                 paste(sort(names(.gwPot_TABLE)), collapse = ", ")))
  }
  h <- as.integer(horizon)
  if (!(h %in% c(20L, 100L, 500L))) {
    stop(paste0("horizon must be one of 20, 100, 500 ",
                "(AR6 assessed horizons)"))
  }
  row <- .gwPot_TABLE[[key]]
  hk <- as.character(h)
  agwp <- unname(row$agwp[[hk]])
  agwp_co2 <- unname(.gwPot_TABLE[["CO2"]]$agwp[[hk]])
  list(
    estimate = unname(row$gwp[[hk]]),
    agwp = agwp,
    agwp_co2 = agwp_co2,
    gwp_from_agwp = agwp / agwp_co2,
    lifetime = row$lifetime,
    radiative_efficiency = row$re,
    gas = key, horizon = h,
    method = "IPCC AR6 Table 7.SM.7 assessed GWP"
  )
}

# The 2026-08-11 arm of this module was a second
# implementation of the same paper; it has been removed and
# its exported name kept as an alias. The formals were
# identical, so this is exact and the man page still applies.
morie_gwpot <- morie_gwPot
