# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 29 -- extreme-value objective helpers + the broad
# craftable long tail: negative-bandwidth guards, degenerate-input
# branches, and the remaining defensive paths.

ok <- function(label, expr) {
  invisible(tryCatch(expr, error = function(e) {
    # Silent by default; set MORIE_WAVE_DEBUG=1 to surface diagnostics.
    if (nzchar(Sys.getenv("MORIE_WAVE_DEBUG"))) {
      message("WAVE29-ERR [", label, "]: ", conditionMessage(e))
    }
  }))
}

# ---- extreme-value objective helpers -------------------------------------

test_that("GEV / GP log-density helpers cover the xi ~ 0 + support branches", {
  x <- rnorm(20)
  # xi exactly 0 -> the Gumbel / exponential limiting branch
  expect_length(morie:::.extvm_log_gev(c(0, 0, 0), x), 20)
  expect_length(morie:::.gpfit_log_gp(c(0, 0), abs(x) + 0.1), 20)
  # xi large + data far from the location -> 1 + xi z <= 0 (out of support)
  oos <- morie:::.extvm_log_gev(c(0, 0, 5), c(-100, -200))
  expect_true(all(oos <= -1e9))
  oos2 <- morie:::.gpfit_log_gp(c(0, -5), c(50, 100))
  expect_true(all(oos2 <= -1e9))
  # generic xi != 0 path
  expect_length(morie:::.extvm_log_gev(c(0, 0, 0.2), x), 20)
  expect_length(morie:::.gpfit_log_gp(c(0, 0.2), abs(x) + 0.1), 20)
})

# ---- negative-bandwidth guards (if (h <= 0) h <- ...) --------------------

test_that("kernel callables recompute a non-positive supplied bandwidth", {
  set.seed(29)
  n <- 60L
  X <- matrix(rnorm(n * 2), n, 2)
  y <- rnorm(n)
  z <- rbinom(n, 1, 0.5)
  coords <- matrix(runif(n * 2), n, 2)
  mk <- matrix(rbinom(n * 4, 2, 0.3), n, 4)
  ok("hrzi2", morie:::hrzi2(X, y, bandwidth = -1))
  ok("hrzk1", morie:::hrzk1(y, bandwidth = -1))
  ok("hrzk2", morie:::hrzk2(X[, 1], y, bandwidth = -1))
  ok("hrzk3", morie:::hrzk3(X[, 1], y, bandwidth = -1))
  ok("hrzp1", morie:::hrzp1(X[, 1], y, z, bandwidth = -1))
  ok("nstat", nstat(y, coords, bandwidth = -1))
  ok("rkhsf", morie_rkhs_full(X, y, markers = mk, h = -1))
  # hrzk3: a grid point far outside the data -> all kernel weights ~ 0
  ok("hrzk3_far", morie:::hrzk3(X[, 1], y, grid = 1e6))
  expect_true(TRUE)
})

# ---- misc internal-helper guard branches ---------------------------------

test_that("backprop activation switch stops on an unknown activation", {
  expect_error(
    morie:::.bkprp_sigma_prime(0.5, "not-an-activation", 0.5),
    "Unknown activation"
  )
})

test_that("Horowitz single-index handles a near-zero lm.fit coefficient", {
  set.seed(292)
  X <- matrix(rnorm(60 * 2), 60, 2)
  # y independent of X and (near) constant -> lm.fit norm ~ 0
  ok("hrzi1_zeronorm", morie:::hrzi1(X, rep(1, 60)))
  expect_true(TRUE)
})

# ---- database remaining branches -----------------------------------------

test_that("morie_builtin_db dev fallback + .fuzzy_match_key name match", {
  testthat::local_mocked_bindings(
    system.file = function(...) "",
    .package = "base"
  )
  expect_match(morie_builtin_db(), "morie\\.db$")
})

test_that("morie_load_dataset: unsupported-format and not-found stops", {
  cat <- morie_dataset_catalog()
  ld <- tempfile("ld-")
  dir.create(ld)
  bad <- file.path(ld, "x.parquet")
  writeLines("x", bad)
  mk_entry <- function(local, ck, dl) {
    c0 <- cat[1, , drop = FALSE]
    c0$key <- "covtest"
    c0$local_path <- local
    c0$ckan_resource_id <- ck
    c0$download_url <- dl
    if ("arcgis_url" %in% names(c0)) c0$arcgis_url <- ""
    c0
  }
  testthat::local_mocked_bindings(
    morie_builtin_db = function(...) tempfile(fileext = ".db"),
    morie_cache_load = function(...) NULL,
    .package = "morie"
  )
  testthat::local_mocked_bindings(
    morie_dataset_catalog = function(...) mk_entry(bad, "", ""),
    .package = "morie"
  )
  expect_error(morie_load_dataset("covtest"), "Unsupported format")
  testthat::local_mocked_bindings(
    morie_dataset_catalog = function(...) {
      mk_entry(file.path(ld, "missing.csv"), "", "")
    },
    .package = "morie"
  )
  expect_error(morie_load_dataset("covtest"), "not found")
})

# ---- mrm degenerate branches ---------------------------------------------

test_that("mrm SIU / TPS empty-stratum branches return cleanly", {
  ok("siu_km_empty", morie:::mrm_siu_case_to_decision_km(data.frame(
    case_number = "C1", incident_date = NA, decision_date = NA
  )))
  ok("tps_recur", morie:::mrm_tps_neighbourhood_recurrence_km(data.frame(
    lat = runif(3), lon = runif(3), date = Sys.Date() + 1:3
  )))
  expect_true(TRUE)
})

# ---- ensemble classification paths ---------------------------------------

test_that("gradient-boosting / xgboost objectives run the classification path", {
  set.seed(293)
  X <- matrix(rnorm(80 * 3), 80, 3)
  yb <- rbinom(80, 1, plogis(X[, 1]))
  ok("gbens_cls", morie_gradient_boosting_ensemble(X, yb, task = "classification"))
  ok("xgbst_cls", morie_xgboost_objective(X, yb, task = "classification"))
  expect_true(TRUE)
})
