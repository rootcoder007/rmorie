# SPDX-License-Identifier: AGPL-3.0-or-later
# Benchmark: native geostatistics vs gstat (skip-if-not-installed).
# Stages timed separately: empirical variogram, MLE model fit (a
# quality feature gstat's WLS fit does not attempt), and kriging with
# a PRE-FITTED model (the comparable operation).
suppressMessages(pkgload::load_all(quiet = TRUE))
set.seed(2)
n <- 800L
d <- data.frame(x = runif(n), y = runif(n))
d$z <- sin(4 * d$x) + cos(4 * d$y) + rnorm(n, sd = 0.2)
bench <- function(thunk) { t0 <- proc.time()[[3]]; thunk(); proc.time()[[3]] - t0 }
vgm_fit <- morie_spatial_variogram_fit(d[, c("x", "y")], d$z)
rows <- c(
  variogram_native = bench(function() morie_spatial_variogram(d[, c("x", "y")], d$z)),
  vgm_mle_fit_native = bench(function() morie_spatial_variogram_fit(d[, c("x", "y")], d$z)),
  krige_native_prefit = bench(function() morie_spatial_krige(d[, c("x", "y")], d$z,
                                                             d[1:100, c("x", "y")],
                                                             vgm = vgm_fit))
)
if (requireNamespace("gstat", quietly = TRUE) && requireNamespace("sp", quietly = TRUE)) {
  sp_d <- d; sp::coordinates(sp_d) <- ~ x + y
  rows["variogram_gstat"] <- bench(function() gstat::variogram(z ~ 1, sp_d))
  vg <- gstat::variogram(z ~ 1, sp_d)
  fitted_g <- gstat::fit.variogram(vg, gstat::vgm("Exp"))
  new_sp <- d[1:100, c("x", "y")]; sp::coordinates(new_sp) <- ~ x + y
  rows["krige_gstat_prefit"] <- bench(function()
    suppressWarnings(gstat::krige(z ~ 1, sp_d, new_sp, model = fitted_g,
                                  debug.level = 0)))
}
print(round(rows, 4))
dir.create("inst/benchmarks/results", showWarnings = FALSE)
write.csv(data.frame(t = rows), sprintf("inst/benchmarks/results/%s-gstat.csv", Sys.Date()))
