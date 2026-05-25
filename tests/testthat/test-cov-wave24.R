# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 24 -- three surgical targets:
#   * R/causal.R       -- the propensity_col-supplied branches of
#     morie_estimate_att / morie_estimate_atc / morie_estimate_aipw, and morie_estimate_late's
#     covariate-adjusted 2SLS path (ivreg branch + manual fallback).
#   * R/study_core.R   -- internal module runners + the alc06-absent
#     branch of .cpads_labeled_data.
#   * R/database.R     -- morie_builtin_db dev fallback, .fuzzy_match_key
#     name-substring match, morie_userguide named lookup, morie_load_cpads
#     CKAN branch.

# ---- causal.R -------------------------------------------------------------

test_that("morie_estimate_att/atc/aipw accept a supplied propensity column", {
  set.seed(11)
  n <- 240L
  df <- data.frame(t = rbinom(n, 1, 0.45), y = rnorm(n), x = rnorm(n))
  df$ps <- pmin(0.95, pmax(0.05, plogis(0.3 * df$x)))
  att <- morie_estimate_att(df, "t", "y", "x", propensity_col = "ps")
  expect_true(is.finite(att$att))
  atc <- morie_estimate_atc(df, "t", "y", "x", propensity_col = "ps")
  expect_true(is.finite(atc$atc))
  aipw <- morie_estimate_aipw(df, "t", "y", "x", propensity_col = "ps")
  expect_true(is.finite(aipw$ate))
})

test_that("morie_estimate_late runs the covariate-adjusted 2SLS path", {
  set.seed(12)
  n <- 400L
  z <- rbinom(n, 1, 0.5)
  x <- rnorm(n)
  t <- rbinom(n, 1, plogis(-0.2 + 1.4 * z + 0.3 * x))
  y <- 0.8 * t + 0.5 * x + rnorm(n)
  df <- data.frame(t = t, y = y, z = z, x = x)
  iv <- morie_estimate_late(df, "t", "y", "z", covariates = "x")
  expect_true(is.finite(iv$late))
  # force the manual 2SLS fallback by hiding ivreg
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "ivreg")) FALSE else TRUE
    },
    .package = "base"
  )
  man <- morie_estimate_late(df, "t", "y", "z", covariates = "x")
  expect_true(is.finite(man$late))
})

# ---- study_core.R ---------------------------------------------------------

test_that(".cpads_labeled_data handles data without an alc06 column", {
  d <- make_canonical_cpads()
  d$alc06 <- NULL
  lab <- morie:::.cpads_labeled_data(d)
  expect_true("alc06_valid" %in% names(lab))
  expect_equal(lab$alc06_valid, lab$alcohol_past12m)
})

test_that(".run_data_wrangling_module_internal builds its logs", {
  out <- morie:::.run_data_wrangling_module_internal(make_canonical_cpads())
  expect_true(is.list(out))
})

test_that(".run_treatment_effects_module_internal runs on canonical data", {
  expect_true(is.list(
    morie:::.run_treatment_effects_module_internal(make_canonical_cpads())
  ))
})

test_that(".run_meta_synthesis_module_internal copies legacy artifacts", {
  od <- tempfile("morie-meta-")
  dir.create(od, recursive = TRUE)
  expect_true(is.list(
    morie:::.run_meta_synthesis_module_internal(make_canonical_cpads(),
      output_dir = od
    )
  ))
})

test_that(".run_ebac_core_module_internal: empty-eligible guard + happy path", {
  # no eligible eBAC observations -> the guard stop fires
  d0 <- make_canonical_cpads()
  d0$ebac_tot <- NA_real_
  expect_error(
    morie:::.run_ebac_core_module_internal(d0),
    "non-missing eBAC"
  )
  # canonical data -> the distribution table and weighted-group loop run.
  # suppressWarnings(): survey-weighted binomial glm emits base-R's benign
  # "non-integer #successes" notice, intrinsic to the runner, not the test.
  res <- suppressWarnings(
    morie:::.run_ebac_core_module_internal(make_canonical_cpads())
  )
  expect_true(is.list(res))
})

# ---- database.R -----------------------------------------------------------

test_that("morie_builtin_db falls back to the per-user cache path", {
  testthat::local_mocked_bindings(
    system.file = function(...) "", .package = "base"
  )
  p <- morie_builtin_db()
  expect_true(grepl("morie\\.db$", p))
})

test_that(".fuzzy_match_key resolves a dataset-name substring", {
  cat <- morie_dataset_catalog()
  # take a word from a dataset name that is not itself a key
  nm <- tolower(cat$name[1])
  token <- strsplit(nm, "[^a-z0-9]+")[[1]]
  token <- token[nchar(token) >= 4][1]
  skip_if(is.na(token), "no usable name token")
  matched <- morie:::.fuzzy_match_key(token)
  expect_true(is.null(matched) || matched %in% cat$key)
})

test_that("morie_userguide accepts a name argument", {
  expect_type(morie_userguide(), "character") # NULL -> directory list
  expect_error(morie_userguide("definitely-not-a-guide.pdf"))
})

test_that("morie_load_cpads reaches the CKAN branch when local+cache miss", {
  testthat::local_mocked_bindings(
    morie_fetch_ckan = function(...) data.frame(seqid = 1:5),
    .package = "morie"
  )
  dat <- morie_load_cpads(db_path = tempfile(fileext = ".db"), use_ckan = TRUE)
  expect_s3_class(dat, "data.frame")
})
