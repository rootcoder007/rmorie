# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Batch figure export for the TPS stochastic analyses. Fully portable:
# data comes from the public Toronto Police Service ArcGIS API via
# morie_fetch_tps() (no private infrastructure), and every file is
# written under the caller-supplied out_dir (CRAN-safe: no default
# location, nothing outside it).

#' Export the TPS stochastic diagnostic figures
#'
#' Reproduces the MORIE gallery's per-category stochastic figures in
#' base R: the Hawkes fit panel (monthly counts, dashed background-rate
#' line, time-rescaling residual histogram with its KS p-value), and
#' optionally the SARIMA hold-out forecast panel. Data are fetched live
#' from the public TPS ArcGIS portal, so results reproduce anywhere
#' without access to any private mirror.
#'
#' @param out_dir Directory to write PNG files into (created if
#'   missing). Required; nothing is written anywhere else.
#' @param categories TPS categories (see [morie_tps_layer_urls()]).
#'   Default `"Homicides"`.
#' @param which Figure families to render: any of `"hawkes"`,
#'   `"sarima"`, `"langevin"`, `"fokker_planck"`.
#' @param cache_dir Passed to [morie_fetch_tps()]; defaults to a
#'   session temporary directory.
#' @return Invisibly, a character vector of the PNG paths written.
#' @examples
#' \donttest{
#' # Live fetch from the public TPS portal; writes only under tempdir().
#' try(morie_tps_figures(file.path(tempdir(), "tps-figs"),
#'                       categories = "Homicides", which = "hawkes"))
#' }
#' @export
morie_tps_figures <- function(out_dir,
                              categories = "Homicides",
                              which = c("hawkes", "sarima",
                                        "langevin", "fokker_planck"),
                              cache_dir = file.path(tempdir(), "morie", "tps")) {
  stopifnot(is.character(out_dir), length(out_dir) == 1L)
  which <- match.arg(which, several.ok = TRUE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  written <- character(0)

  for (cat_name in categories) {
    csv <- morie_fetch_tps(cat_name, cache_dir = cache_dir)
    df <- utils::read.csv(csv, stringsAsFactors = FALSE)
    dt <- .tps_stoch_date_series(df)
    if (length(dt) < 100L) {
      warning(sprintf("%s: only %d usable timestamps; skipped.",
                      cat_name, length(dt)))
      next
    }

    if ("hawkes" %in% which) {
      fit <- morie_tps_hawkes_temporal_fit(df, ds_name = cat_name)
      if (is.finite(fit$mu %||% NA_real_)) {
        p <- file.path(out_dir, sprintf("hawkes_%s.png", cat_name))
        grDevices::png(p, width = 1140, height = 820, res = 110)
        # CRAN: restore par() before this private device is closed.
        oldpar <- graphics::par(mfrow = c(2, 1), mar = c(3.5, 4, 2.5, 1))
        mo <- table(format(dt, "%Y-%m"))
        mo_x <- as.Date(paste0(names(mo), "-01"))
        graphics::plot(mo_x, as.integer(mo), type = "l",
                       col = "#3584e4", lwd = 1.4, xlab = "",
                       ylab = "incidents / month",
                       main = sprintf("%s -- Hawkes fit", cat_name))
        graphics::abline(h = fit$mu * 30, col = "#ff7800",
                         lty = 2, lwd = 2)
        graphics::legend("topright",
                         c("monthly count",
                           sprintf("mu*30days = %.1f", fit$mu * 30)),
                         col = c("#3584e4", "#ff7800"),
                         lty = c(1, 2), bty = "n")
        res <- .tps_stoch_hawkes_residuals(dt, fit$mu, fit$kappa, fit$omega)
        ks <- suppressWarnings(stats::ks.test(res, "pexp"))
        graphics::hist(diff(res), breaks = 50, col = "#62a0ea",
                       border = NA, xlab = "residual dt",
                       main = sprintf(
                         "residual interarrivals -- KS p = %.3f",
                         ks$p.value))
        graphics::par(oldpar)
        grDevices::dev.off()
        written <- c(written, p)
      }
    }

    if ("sarima" %in% which) {
      sf <- tryCatch(morie_tps_sarima_forecast(df, ds_name = cat_name),
                     error = function(e) NULL)
      if (!is.null(sf) && !is.null(sf$forecast) && !is.null(sf$actual)) {
        p <- file.path(out_dir, sprintf("sarima_%s.png", cat_name))
        grDevices::png(p, width = 1140, height = 520, res = 110)
        graphics::par(mar = c(3.5, 4, 2.5, 1))
        h <- length(sf$actual)
        graphics::plot(seq_len(h), sf$actual, type = "b", pch = 16,
                       col = "#3584e4", xlab = "hold-out month",
                       ylab = "incidents / month",
                       main = sprintf("%s -- SARIMA hold-out forecast",
                                      cat_name))
        graphics::lines(seq_len(h), sf$forecast, type = "b", pch = 1,
                        col = "#ff7800")
        graphics::legend("topright", c("actual", "forecast"),
                         col = c("#3584e4", "#ff7800"),
                         pch = c(16, 1), bty = "n")
        grDevices::dev.off()
        written <- c(written, p)
      }
    }
    if ("langevin" %in% which) {
      lv <- tryCatch(morie_tps_langevin_simulate(df, ds_name = cat_name),
                     error = function(e) NULL)
      if (!is.null(lv) && !is.null(lv$paths)) {
        p <- file.path(out_dir, sprintf("langevin_%s.png", cat_name))
        grDevices::png(p, width = 1140, height = 620, res = 110)
        graphics::par(mar = c(3.5, 4, 2.5, 1))
        k <- min(30L, ncol(lv$paths))
        graphics::matplot(lv$paths[, seq_len(k)], type = "l", lty = 1,
                          col = grDevices::adjustcolor("#3584e4", 0.25),
                          xlab = "day", ylab = "incidents / day",
                          main = sprintf(
                            "%s -- Langevin OU paths (theta=%.2f)",
                            cat_name, lv$theta))
        graphics::abline(h = lv$mu, col = "#ff7800", lty = 2, lwd = 2)
        graphics::legend("topright",
                         c("simulated paths",
                           sprintf("long-run mean mu = %.1f", lv$mu)),
                         col = c("#3584e4", "#ff7800"),
                         lty = c(1, 2), bty = "n")
        grDevices::dev.off()
        written <- c(written, p)
      }
    }

    if ("fokker_planck" %in% which) {
      fp <- tryCatch(morie_tps_fokker_planck_grid(df, ds_name = cat_name),
                     error = function(e) NULL)
      if (!is.null(fp) && !is.null(fp$density)) {
        p <- file.path(out_dir, sprintf("fokker_planck_%s.png", cat_name))
        grDevices::png(p, width = 1140, height = 620, res = 110)
        graphics::par(mar = c(3.5, 4, 2.5, 1))
        graphics::plot(fp$grid, fp$density, type = "l", lwd = 1.6,
                       col = "#3584e4", xlab = "incidents / day",
                       ylab = "density",
                       main = sprintf(
                         "%s -- Fokker-Planck evolved density", cat_name))
        graphics::lines(fp$grid,
                        stats::dnorm(fp$grid, fp$mu,
                                     sqrt(fp$stationary_var)),
                        col = "#ff7800", lty = 2, lwd = 2)
        graphics::legend("topright",
                         c("evolved p(x, T)", "OU stationary density"),
                         col = c("#3584e4", "#ff7800"),
                         lty = c(1, 2), bty = "n")
        grDevices::dev.off()
        written <- c(written, p)
      }
    }
  }
  invisible(written)
}

# Time-rescaling residuals for the exponential-kernel Hawkes fit;
# Exp(1)-distributed when the model is correct (Ogata 1988).
#' Internal helper: Tps Stoch Hawkes Residuals
#' @noRd
.tps_stoch_hawkes_residuals <- function(dt, mu, kappa, omega) {
  t <- sort(as.numeric(difftime(dt, min(dt), units = "days")))
  n <- length(t)
  A <- numeric(n)
  for (i in 2:n) A[i] <- exp(-omega * (t[i] - t[i - 1])) * (1 + A[i - 1])
  res <- numeric(n - 1)
  for (i in 2:n) {
    dtx <- t[i] - t[i - 1]
    res[i - 1] <- mu * dtx + kappa * (1 + A[i - 1]) * (1 - exp(-omega * dtx))
  }
  res
}
