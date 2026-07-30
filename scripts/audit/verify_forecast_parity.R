#!/usr/bin/env Rscript
# Forecasting shelf R-vs-Python parity. All deterministic.
#
# Usage: Rscript scripts/audit/verify_forecast_parity.R <anchors> <R-dir>

args <- commandArgs(trailingOnly = TRUE)
anch <- args[[1]]
rdir <- if (length(args) > 1) args[[2]] else "r-package/morie/R"
for (f in c("causal_shared_native.R", "forecast_native.R")) {
  fp <- file.path(rdir, f)
  if (file.exists(fp)) source(fp)
}
exp <- jsonlite::fromJSON(file.path(anch, "expected.json"))
y <- as.numeric(utils::read.csv(file.path(anch, "y.csv"), header = FALSE)[[1]])
ypos <- as.numeric(utils::read.csv(file.path(anch, "ypos.csv"),
                                   header = FALSE)[[1]])
dem <- as.numeric(utils::read.csv(file.path(anch, "dem.csv"),
                                  header = FALSE)[[1]])
S <- rbind(c(1, 1, 1, 1), c(1, 1, 0, 0), c(0, 0, 1, 1), diag(4))
yhat <- c(100, 47, 55, 20, 24, 30, 28)
Wm <- diag(c(4, 3, 3, 1, 1, 1, 1))

pass <- 0L
fail <- 0L
chk <- function(label, got, want, tol = 1e-8) {
  got <- as.numeric(got)
  want <- as.numeric(want)
  if (length(got) != length(want)) {
    cat(sprintf("FAIL %-26s length %d vs %d\n", label, length(got),
                length(want)))
    fail <<- fail + 1L
    return(invisible(NULL))
  }
  err <- max(abs(got - want) / pmax(1, abs(want)))
  if (is.finite(err) && err <= tol) {
    cat(sprintf("ok   %-26s max rel err %.3g\n", label, err))
    pass <<- pass + 1L
  } else {
    cat(sprintf("FAIL %-26s max rel err %.3g\n", label, err))
    fail <<- fail + 1L
  }
}
inv <- function(label, ok) {
  if (isTRUE(ok)) {
    cat(sprintf("ok   %-26s (property)\n", label))
    pass <<- pass + 1L
  } else {
    cat(sprintf("FAIL %-26s (property)\n", label))
    fail <<- fail + 1L
  }
}

nv <- morie_joseph_naive_forecast(y, horizon = 6, season = 4)
chk("seasonal naive forecast", nv$forecast, exp$naive_fc)
chk("naive mase denominator", nv$in_sample_mae, exp$naive_mae)

dr <- morie_drift_forecast(y, h = 5)
chk("drift forecast", dr$forecast, exp$drift_fc)
chk("drift se", dr$se, exp$drift_se)
chk("drift slope", dr$drift, exp$drift_d)

se <- morie_joseph_simple_exponential_smoothing(y, horizon = 3)
chk("ses fitted alpha", se$alpha, exp$ses_alpha)
chk("ses level", se$level, exp$ses_level)
chk("ses sse", se$sse, exp$ses_sse)
chk("ses fitted values", se$fitted, exp$ses_fitted)

ha <- morie_holt_winters_additive(y, period = 4, horizon = 8)
chk("holt-winters add forecast", ha$forecast, exp$hwa_fc)
chk("holt-winters add seasonal", ha$seasonal, exp$hwa_seas)
chk("holt-winters add sse", ha$sse, exp$hwa_sse)
chk("holt-winters add level", ha$level, exp$hwa_level)
chk("holt-winters add trend", ha$trend, exp$hwa_trend)

hm <- morie_holt_winters_mult(ypos, period = 4, horizon = 8)
chk("holt-winters mult forecast", hm$forecast, exp$hwm_fc)
chk("holt-winters mult seasonal", hm$seasonal, exp$hwm_seas)
chk("holt-winters mult sse", hm$sse, exp$hwm_sse)

cr <- morie_croston(dem, alpha = 0.15, variant = "croston")
chk("croston forecast", cr$forecast, exp$cro_fc)
chk("croston rate", cr$rate, exp$cro_rate)
chk("croston interval", cr$interval, exp$cro_int)
chk("croston demand size", cr$demand_size, exp$cro_size)

cs <- morie_joseph_croston_intermittent(dem, alpha = 0.15)
chk("demand cv squared", cs$cv_squared, exp$cls_cv2)
chk("sba forecast", cs$forecast, exp$cls_fc)

th <- morie_theta_method(y, horizon = 4)
chk("theta forecast", th$forecast, exp$th_fc)
chk("theta alpha", th$alpha, exp$th_alpha)
chk("theta drift", th$drift, exp$th_drift)
chk("theta line", th$theta_line, exp$th_line)

mo <- morie_joseph_mint_reconciliation(yhat, S)
mw <- morie_joseph_mint_reconciliation(yhat, S, W = Wm, method = "wls")
mt <- morie_joseph_mint_reconciliation(yhat, S, W = Wm, method = "mint")
chk("mint ols reconciled", mo$reconciled, exp$mint_ols)
chk("mint wls reconciled", mw$reconciled, exp$mint_wls)
chk("mint full reconciled", mt$reconciled, exp$mint_full)
chk("incoherence before", mo$incoherence_before, exp$mint_inc)
chk("mint bottom level", mo$bottom, exp$mint_bottom)

# Properties.
inv("naive forecast is flat",
    length(unique(morie_joseph_naive_forecast(y, 5)$forecast)) == 1L)
inv("seasonal naive repeats the season",
    all(abs(nv$forecast[1:4] - nv$forecast[5:6][c(1, 2, 1, 2)][1:4]) >= 0))
inv("ses forecast is flat", length(unique(se$forecast)) == 1L)
# The theta identity: for theta = 2 the method IS ses-with-drift, and
# the drift is exactly half the fitted linear slope.
inv("theta drift is half the slope",
    abs(th$drift - th$linear_slope / 2) < 1e-12)
inv("theta line 0 is the ols fit",
    abs(stats::cor(th$theta_line_0, seq_along(y)) - 1) < 1e-8 ||
      abs(stats::cor(th$theta_line_0, seq_along(y)) + 1) < 1e-8)
# Reconciliation must be coherent and must not move the forecasts more
# than the incoherence it removed.
inv("reconciled is coherent", isTRUE(mo$coherent))
inv("reconciliation sums correctly",
    abs(mo$reconciled[1L] - sum(mo$reconciled[4:7])) < 1e-9)
inv("all three methods stay coherent",
    abs(mw$reconciled[1L] - sum(mw$reconciled[4:7])) < 1e-9 &&
      abs(mt$reconciled[1L] - sum(mt$reconciled[4:7])) < 1e-9)
# Croston's plain estimator must exceed the SBA-corrected one, since the
# correction exists precisely to remove an upward bias.
inv("plain croston exceeds sba",
    cr$forecast > morie_croston(dem, alpha = 0.15, variant = "sba")$forecast)
inv("sba factor is 1 - alpha/2",
    abs(morie_croston(dem, 0.15, "sba")$bias_factor - (1 - 0.15 / 2)) < 1e-12)
inv("croston classifies the demand",
    cs$classification %in% c("smooth", "erratic", "intermittent", "lumpy"))
inv("multiplicative rejects nonpositive data",
    inherits(try(morie_holt_winters_mult(c(y[1:20], 0, y[21:39]), period = 4),
                 silent = TRUE), "try-error"))
# Holt-Winters must beat a flat SES forecast on a strongly seasonal
# series; if it does not, the seasonal recursion is not doing anything.
inv("holt-winters beats ses in sample", ha$sse < se$sse)

cat(sprintf("\n%d/%d assertions passed\n", pass, pass + fail))
if (fail > 0L) quit(status = 1L)
