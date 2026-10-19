# R/tps_hawkes_advanced.R: the base-R kernel, baseline and likelihood paths against the C++
# ones, the event conversion, and the three public drivers on simulated events.

.hwka_pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"
.hwka <- function(name) get(name, envir = environment(morie_tps_hawkes_advanced_fit))

.hwka_events <- function(n = 160, seed = 7, rate = 1 / 1.6) {
  set.seed(seed)
  d <- as.Date("2021-01-01") + sort(cumsum(stats::rexp(n, rate)))
  data.frame(OCC_DATE = format(d, "%Y-%m-%dT%H:%M:%S"))
}

test_that("the base-R kernel density and CDF equal the C++ ones for every kernel", {
  u <- c(0.05, 0.5, 1, 3, 10)
  psi <- list(exponential = 0.7, gamma = c(1.5, 0.8), weibull = c(1.3, 2), lomax = c(2.5, 3))
  dens <- .hwka(".tps_hwka_kernel_density")
  cdf <- .hwka(".tps_hwka_kernel_cdf")
  expect_true(.hwka(".tps_hwka_cpp_ok")())
  with_cpp <- lapply(names(psi), function(k) list(d = dens(u, k, psi[[k]]), p = cdf(u, k, psi[[k]])))
  testthat::local_mocked_bindings(.tps_hwka_cpp_ok = function() FALSE, .package = .hwka_pkg)
  for (i in seq_along(psi)) {
    k <- names(psi)[i]
    expect_equal(dens(u, k, psi[[k]]), with_cpp[[i]]$d, tolerance = 1e-12, info = k)
    expect_equal(cdf(u, k, psi[[k]]), with_cpp[[i]]$p, tolerance = 1e-12, info = k)
  }
  # the closed forms themselves
  expect_equal(dens(u, "exponential", 0.7), 0.7 * exp(-0.7 * u))
  expect_equal(dens(u, "gamma", c(1.5, 0.8)), stats::dgamma(u, 1.5, 0.8))
  expect_equal(dens(u, "weibull", c(1.3, 2)), stats::dweibull(u, 1.3, 2))
  expect_equal(dens(u, "lomax", c(2.5, 3)), 2.5 * 3^2.5 / (u + 3)^3.5)
  expect_equal(cdf(u, "lomax", c(2.5, 3)), 1 - (3 / (u + 3))^2.5)
  expect_error(dens(u, "cauchy", 1), "unknown kernel kind")
  expect_error(cdf(u, "cauchy", 1), "unknown kernel kind")
})

test_that("the trapezoid baseline integral agrees with the C++ quadrature", {
  bi <- .hwka(".tps_hwka_baseline_integral")
  a <- c(log(0.4), 0.3, 0.2, -0.1)
  with_cpp <- bi(400, "sinusoidal", a)
  testthat::local_mocked_bindings(.tps_hwka_cpp_ok = function() FALSE, .package = .hwka_pkg)
  expect_equal(bi(400, "sinusoidal", a), with_cpp, tolerance = 1e-6)
  expect_equal(bi(400, "constant", a), 0.4 * 400)
  expect_error(.hwka(".tps_hwka_baseline")(1:3, "step", a, 400), "unknown baseline kind")
})

test_that("the general negative log-likelihood is the same by the R loop and the C++ sum", {
  nll <- .hwka(".tps_hwka_neg_loglik_general")
  td <- .hwka(".tps_hwka_events_to_days")(.hwka_events(60))
  t <- td$t
  T_ <- td$T_
  cases <- list(
    list(theta = c(log(0.3), 0.4, 0.9), k = "exponential", b = "constant"),
    list(theta = c(log(0.3), 0.1, 0.2, -0.1, 0.5, 1.5, 0.8), k = "gamma", b = "sinusoidal"),
    list(theta = c(log(0.3), 0.3, 1.2, 2), k = "weibull", b = "constant"),
    list(theta = c(log(0.3), 0.3, 2.5, 3), k = "lomax", b = "constant")
  )
  with_cpp <- vapply(cases, function(cs) nll(cs$theta, t, T_, cs$k, cs$b), numeric(1))
  expect_true(all(is.finite(with_cpp)))
  # the hand sum: log-intensities less the compensator
  cs <- cases[[1]]
  lam <- 0.3 + 0.4 * vapply(seq_along(t), function(i) sum(0.9 * exp(-0.9 * (t[i] - t[t < t[i]]))), numeric(1))
  comp <- 0.3 * T_ + 0.4 * sum(1 - exp(-0.9 * (T_ - t)))
  expect_equal(with_cpp[1], -(sum(log(lam)) - comp), tolerance = 1e-10)
  testthat::local_mocked_bindings(.tps_hwka_cpp_ok = function() FALSE, .package = .hwka_pkg)
  for (i in seq_along(cases)) {
    expect_equal(nll(cases[[i]]$theta, t, T_, cases[[i]]$k, cases[[i]]$b), with_cpp[i], tolerance = 1e-10)
  }
  # the guards
  expect_equal(nll(c(log(0.3), 0, 0.9), t, T_, "exponential", "constant"), 1e12)
  expect_equal(nll(c(log(0.3), 0.9999, 0.9), t, T_, "exponential", "constant"), 1e12)
  expect_equal(nll(c(log(0.3), 0.4, 0), t, T_, "exponential", "constant"), 1e12)
  expect_equal(nll(c(log(0.3), 0.4, 1, 3), t, T_, "lomax", "constant"), 1e12)
  expect_error(.hwka(".tps_hwka_split_theta")(1:3, "gamma", "constant"), "expected 4 params, got 3")
  sp <- .hwka(".tps_hwka_split_theta")(c(1, 2, 3, 4, 5, 6, 7), "gamma", "sinusoidal")
  expect_equal(sp, list(a = 1:4, eta = 5, psi = 6:7))
})

test_that("the starting point follows the event rate and the mean gap", {
  x0 <- .hwka(".tps_hwka_x0")
  expect_equal(x0("exponential", "constant", 100, 200, 2), c(log(0.5 * 0.6), 0.4, 0.5))
  expect_equal(x0("gamma", "sinusoidal", 100, 200, 2), c(log(0.3), 0, 0, 0, 0.4, 1.5, 0.5))
  expect_equal(x0("weibull", "constant", 100, 200, 2), c(log(0.3), 0.4, 1.5, 2.4))
  expect_equal(x0("lomax", "constant", 100, 200, 2), c(log(0.3), 0.4, 2.5, 10))
  # tiny rates and gaps are floored
  expect_equal(x0("exponential", "constant", 0, 200, 0), c(log(1e-3 * 0.6), 0.4, 1e3))
})

test_that("time-rescaling residuals equal the C++ residuals of the fitted process", {
  td <- .hwka(".tps_hwka_events_to_days")(.hwka_events())
  tr <- .hwka(".tps_hwka_time_rescaling")
  for (kb in list(c("exponential", "constant"), c("gamma", "sinusoidal"))) {
    f <- .hwka(".tps_hwka_fit_one")(td$t, td$T_, kb[1], kb[2])
    u <- tr(f$theta, td$t, td$T_, kb[1], kb[2])
    expect_length(u, length(td$t))
    expect_true(all(u > 0 & u < 1))
    expect_equal(u, rmoriebricklayer::chwkres(td$t, td$T_, kb[1], f$theta, base = kb[2]),
                 tolerance = 1e-9)
    expect_true(f$converged)
    expect_equal(f$T_days, td$T_)
  }
})

test_that("events become days from dated columns, epoch milliseconds, or text, with the same jitter", {
  e2d <- .hwka(".tps_hwka_events_to_days")
  df <- .hwka_events(120)
  td <- e2d(df)
  expect_length(td$t, 120)
  expect_equal(td$T_, td$t[120])
  expect_true(all(diff(td$t) >= 0))
  dt <- as.POSIXct(df$OCC_DATE, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
  base <- as.numeric(difftime(dt, dt[1], units = "days"))
  jitter <- rmoriebricklayer::cunif(120, 42)
  expect_equal(td$t, sort(base + jitter))
  # the same events as year / month / day columns, with month names and abbreviations
  ymd <- data.frame(OCC_YEAR = as.integer(format(dt, "%Y")), OCC_MONTH = format(dt, "%B"),
                    OCC_DAY = as.integer(format(dt, "%d")))
  ymd$OCC_MONTH[1:10] <- substr(ymd$OCC_MONTH[1:10], 1, 3)
  ymd$OCC_MONTH[11:20] <- as.character(as.integer(format(dt[11:20], "%m")))
  td2 <- e2d(ymd)
  expect_equal(td2$t, sort(floor(base) + jitter), tolerance = 1e-12)
  # epoch milliseconds
  ms <- data.frame(REPORT_DATE = as.numeric(dt) * 1000)
  expect_equal(e2d(ms)$t, td$t)
  # years before min_year are dropped; a subsample keeps the events with the smallest keys
  old <- df
  old$OCC_DATE[1:5] <- "2010-01-01T00:00:00"
  expect_length(e2d(old)$t, 115)
  expect_length(e2d(old, min_year = NULL)$t, 120)
  sub <- e2d(df, max_n = 50)
  expect_length(sub$t, 50)
  keys <- rmoriebricklayer::cunif(120, 43)
  keep <- sort(order(keys)[1:50])
  expect_equal(sub$t, sort(base[keep] - base[keep[1]] + rmoriebricklayer::cunif(50, 42)))
  expect_error(e2d(data.frame(x = 1)), "no OCC_DATE or REPORT_DATE column")
})

test_that("morie_tps_hawkes_advanced_fit reports a fitted process and names every failure", {
  df <- .hwka_events()
  r <- morie_tps_hawkes_advanced_fit(df, kernel = "exponential", baseline = "constant", ds_name = "sim")
  expect_s3_class(r, "morie_tps_hawkes_advanced_result")
  expect_length(r$warnings, 0)
  sl <- r$summary_lines
  expect_equal(sl$events_fitted, 160)
  expect_equal(sl$kernel, "exponential")
  expect_equal(sl$baseline, "constant")
  expect_true(sl$branching_ratio >= 0 && sl$branching_ratio < 1)
  expect_equal(sl$stationary, "Yes (eta<1)")
  expect_equal(sl$aic, round(r$payload$aic, 1))
  expect_match(r$title, "Hawkes \\[exponential kernel, constant baseline\\] -- sim")
  expect_match(r$interpretation, "Process is stationary")
  expect_match(r$interpretation, if (sl$ks_pvalue >= 0.05) "consistent with" else "depart from")
  r2 <- morie_tps_hawkes_advanced_fit(df, kernel = "gamma", baseline = "sinusoidal", max_n = 120)
  expect_equal(r2$summary_lines$events_fitted, 120)
  expect_length(r2$summary_lines$baseline_params, 4)
  expect_length(r2$summary_lines$kernel_params, 2)
  # failures
  expect_error(morie_tps_hawkes_advanced_fit(df, kernel = "cauchy"), "unknown kernel")
  expect_error(morie_tps_hawkes_advanced_fit(df, baseline = "step"), "unknown baseline")
  expect_error(morie_tps_hawkes_advanced_fit(df, method = "fast"), "method must be one of")
  expect_error(morie_tps_hawkes_advanced_fit(df, kernel = "exponential", method = "soe"), "completely monotone")
  expect_error(morie_tps_hawkes_advanced_fit(df, baseline = "sinusoidal", method = "inar"), "stationary process")
  nd <- morie_tps_hawkes_advanced_fit(data.frame(x = 1:5), ds_name = "none")
  expect_equal(nd$warnings, "no OCC_DATE or REPORT_DATE column")
  expect_equal(nd$title, "Hawkes-gamma/sinusoidal -- none")
  few <- morie_tps_hawkes_advanced_fit(df[1:40, , drop = FALSE])
  expect_equal(few$warnings, "only 40 timestamps")
  testthat::local_mocked_bindings(.tps_hwka_fit_one = function(...) stop("optimiser fell over"),
                                  .package = .hwka_pkg)
  bad <- morie_tps_hawkes_advanced_fit(df)
  expect_equal(bad$warnings, "optimiser fell over")
  expect_length(bad$summary_lines, 0)
})

test_that("the kernel comparison ranks the fitted combinations by AIC and keeps the failures", {
  df <- .hwka_events()
  nd <- morie_tps_compare_hawkes_kernels(data.frame(x = 1:5), ds_name = "none")
  expect_equal(nd$warnings, "no OCC_DATE or REPORT_DATE column")
  expect_equal(morie_tps_compare_hawkes_kernels(df[1:30, , drop = FALSE])$warnings, "only 30 timestamps")
  real_fit <- .hwka(".tps_hwka_fit_one")
  testthat::local_mocked_bindings(
    .tps_hwka_fit_one = function(t, T_, kernel_kind, baseline_kind, ...) {
      if (kernel_kind == "weibull") stop("weibull refused")
      real_fit(t, T_, kernel_kind, baseline_kind, ...)
    },
    .package = .hwka_pkg)
  r <- morie_tps_compare_hawkes_kernels(df, ds_name = "sim", kernels = c("exponential", "weibull", "gamma"),
                                        baselines = "constant")
  rows <- r$payload$rows
  expect_length(rows, 3)
  expect_equal(vapply(rows, function(x) x$kernel, ""), c("exponential", "weibull", "gamma"))
  expect_equal(rows[[2]]$error, "weibull refused")
  expect_equal(r$summary_lines$combinations_fitted, 2)
  expect_equal(r$summary_lines$combinations_failed, 1)
  fitted <- rows[c(1, 3)]
  aics <- vapply(fitted, function(x) x$aic, numeric(1))
  best <- fitted[[which.min(aics)]]
  expect_equal(r$payload$best$kernel, best$kernel)
  expect_equal(r$summary_lines$best_lowest_aic, sprintf("%s / constant", best$kernel))
  expect_equal(r$summary_lines$delta_aic_vs_markovian, round(rows[[1]]$aic - best$aic, 1))
  expect_true(rows[[1]]$markovian)
  expect_false(rows[[3]]$markovian)
  expect_true(rows[[1]]$stationary_baseline)
  expect_match(r$title, "Markovian vs non-Markovian Hawkes -- sim")
  # without the Markovian row the delta is NA
  r2 <- morie_tps_compare_hawkes_kernels(df, kernels = "gamma", baselines = "sinusoidal")
  expect_true(is.na(r2$summary_lines$delta_aic_vs_markovian))
  # every combination failing leaves no best
  testthat::local_mocked_bindings(.tps_hwka_fit_one = function(...) stop("all refused"), .package = .hwka_pkg)
  r3 <- morie_tps_compare_hawkes_kernels(df, kernels = "gamma", baselines = "constant")
  expect_null(r3$payload$best)
  expect_equal(r3$summary_lines$combinations_fitted, 0)
  expect_null(r3$summary_lines$best_lowest_aic)
})

test_that("the Markovian-versus-non-Markovian driver is the exponential/gamma by constant/sinusoidal grid", {
  df <- .hwka_events()
  r <- morie_tps_hawkes_markovian_vs_nonmarkovian(df, ds_name = "sim", max_n = 120)
  rows <- r$payload$rows
  expect_length(rows, 4)
  expect_equal(unique(vapply(rows, function(x) x$kernel, "")), c("exponential", "gamma"))
  expect_equal(unique(vapply(rows, function(x) x$baseline, "")), c("constant", "sinusoidal"))
  expect_equal(r$summary_lines$combinations_fitted + r$summary_lines$combinations_failed, 4)
})
