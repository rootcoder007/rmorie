# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Batch figure export for the OTIS restrictive-confinement analyses.
# Fully portable and offline: the default input is the bundled b01
# sample (real data.ontario.ca slice; rmoriedata fallback), and every
# file is written under the caller-supplied out_dir.

#' Export the OTIS restrictive-confinement figures
#'
#' Renders the OTIS exploratory figure set in base R from a b01-schema
#' data frame: consecutive-days survival tail (log-log CCDF, the
#' Goffman power-law view), alert-share trends by fiscal year, alert
#' co-occurrence (Cramer's V per alert pair, the mortification view),
#' placements by age band and year, and a Pareto chart of placements
#' by region. Runs offline on the bundled sample by default.
#'
#' @param out_dir Directory to write PNG files into (created if
#'   missing). Required; nothing is written anywhere else.
#' @param df A b01-schema data.frame. Default loads the shipped
#'   sample via [morie_sample()] (data.ontario.ca, OGL-Ontario).
#' @param which Figure families: any of `"powerlaw"`, `"alert_trend"`,
#'   `"mortification"`, `"age_year"`, `"pareto_region"`.
#' @return Invisibly, a character vector of the PNG paths written.
#' @examples
#' \donttest{
#' try(morie_otis_figures(file.path(tempdir(), "otis-figs")))
#' }
#' @export
morie_otis_figures <- function(out_dir,
                               df = morie_sample("otis_b01"),
                               which = c("powerlaw", "alert_trend",
                                         "mortification", "age_year",
                                         "pareto_region")) {
  stopifnot(is.character(out_dir), length(out_dir) == 1L,
            is.data.frame(df))
  which <- match.arg(which, several.ok = TRUE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  written <- character(0)
  wfig <- function(name, width = 1140, height = 620, draw) {
    p <- file.path(out_dir, name)
    grDevices::png(p, width = width, height = height, res = 110)
    on.exit(grDevices::dev.off(), add = TRUE)
    draw()
    written <<- c(written, p)
  }
  yr <- df[["EndFiscalYear"]]
  alerts <- c("MentalHealth_Alert", "SuicideRisk_Alert",
              "SuicideWatch_Alert")
  alert_cols <- intersect(alerts, colnames(df))

  if ("powerlaw" %in% which &&
      "NumberConsecutiveDays_Segregation" %in% colnames(df)) {
    d <- as.numeric(df[["NumberConsecutiveDays_Segregation"]])
    d <- d[is.finite(d) & d > 0]
    wfig("otis_powerlaw_days.png", draw = function() {
      ss <- sort(d)
      ccdf <- rev(seq_along(ss)) / length(ss)
      graphics::plot(ss, ccdf, log = "xy", pch = 16, cex = 0.5,
                     col = "#3584e4",
                     xlab = "consecutive days in restrictive confinement",
                     ylab = "P(D > d)",
                     main = "Placement-length survival tail (Goffman view)")
      graphics::abline(v = 15, col = "#ff7800", lty = 2, lwd = 2)
      graphics::legend("bottomleft", "15-day Mandela threshold",
                       col = "#ff7800", lty = 2, bty = "n")
    })
  }

  if ("alert_trend" %in% which && length(alert_cols) && !is.null(yr)) {
    shares <- sapply(alert_cols, function(a) {
      tapply(df[[a]] == "Yes", yr, mean, na.rm = TRUE)
    })
    years <- as.integer(rownames(shares))
    wfig("otis_alert_trend.png", draw = function() {
      graphics::matplot(years, shares, type = "b", pch = 16, lty = 1,
                        col = c("#3584e4", "#ff7800", "#26a269"),
                        xlab = "fiscal year", ylab = "share of placements",
                        main = "Alert prevalence by fiscal year")
      graphics::legend("topleft", sub("_Alert", "", alert_cols),
                       col = c("#3584e4", "#ff7800", "#26a269"),
                       pch = 16, bty = "n")
    })
  }

  if ("mortification" %in% which && length(alert_cols) == 3L) {
    co <- mrm_otis_mortification_cooccurrence(df, alert_cols = alert_cols)
    wfig("otis_mortification.png", draw = function() {
      lab <- paste(sub("_Alert", "", co$alert_a),
                   sub("_Alert", "", co$alert_b), sep = " x ")
      graphics::par(mar = c(8, 4, 2.5, 1))
      graphics::barplot(co$morie_cramers_v, names.arg = lab, las = 2,
                        col = "#3584e4", ylab = "Cramer's V",
                        main = "Alert co-occurrence (mortification view)")
    })
  }

  if ("age_year" %in% which && "Age_Category" %in% colnames(df) &&
      !is.null(yr)) {
    tab <- table(df[["Age_Category"]], yr)
    wfig("otis_age_year.png", draw = function() {
      graphics::matplot(as.integer(colnames(tab)), t(tab), type = "b",
                        pch = 16, lty = 1,
                        col = grDevices::hcl.colors(nrow(tab), "Dark 3"),
                        xlab = "fiscal year", ylab = "placements",
                        main = "Placements by age band and year")
      graphics::legend("topleft", rownames(tab),
                       col = grDevices::hcl.colors(nrow(tab), "Dark 3"),
                       pch = 16, bty = "n")
    })
  }

  if ("pareto_region" %in% which &&
      "Region_AtTimeOfPlacement" %in% colnames(df)) {
    cnt <- sort(table(df[["Region_AtTimeOfPlacement"]]),
                decreasing = TRUE)
    wfig("otis_pareto_region.png", draw = function() {
      cum <- cumsum(cnt) / sum(cnt)
      bp <- graphics::barplot(as.numeric(cnt), names.arg = names(cnt),
                              col = "#3584e4", las = 2,
                              ylab = "placements",
                              main = "Placements by region (Pareto)")
      graphics::par(new = TRUE)
      graphics::plot(bp, cum, type = "b", pch = 16, col = "#ff7800",
                     axes = FALSE, xlab = "", ylab = "",
                     ylim = c(0, 1))
      graphics::axis(4, at = seq(0, 1, 0.25),
                     labels = paste0(seq(0, 100, 25), "%"))
      graphics::abline(h = 0.8, col = "#ff7800", lty = 3)
    })
  }

  invisible(written)
}
