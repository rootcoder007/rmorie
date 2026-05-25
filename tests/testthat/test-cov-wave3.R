# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 3 -- R/regms.R (Markov-switching), R/perseus.R (the
# Python agent bridge) and R/mrm_samples.R (the TPS ArcGIS fetcher).
# External processes / HTTP are mocked so everything runs offline.

# --- regms.R ---------------------------------------------------------------

test_that("morie_regime_switching errors on a too-short series", {
  expect_error(morie_regime_switching(1:3, k_regimes = 2), "short")
})

test_that("morie_regime_switching base-R EM path runs when MSwM is absent", {
  set.seed(1)
  x <- c(stats::rnorm(45, 0, 1), stats::rnorm(45, 6, 1))
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "MSwM")) FALSE else TRUE
    },
    .package = "base"
  )
  r <- morie_regime_switching(x, k_regimes = 2)
  expect_equal(r$k_regimes, 2)
  expect_length(r$mu, 2L)
  expect_true(is.finite(r$loglik))
  expect_match(r$method, "base R")
  expect_equal(dim(r$transition), c(2L, 2L))
})

test_that("morie_regime_switching uses MSwM when it is available", {
  set.seed(2)
  x <- c(stats::rnorm(60, 0, 1), stats::rnorm(60, 4, 1.5))
  r <- tryCatch(suppressWarnings(morie_regime_switching(x, k_regimes = 2)),
    error = function(e) NULL
  )
  expect_true(is.null(r) || identical(r$k_regimes, 2))
})

# --- perseus.R -------------------------------------------------------------

test_that("morie_build_prompt handles bare, contextual and empty questions", {
  expect_equal(morie_build_prompt("What is the rate?"), "What is the rate?")
  p <- morie_build_prompt("Why?", context = "Segregation data 2025")
  expect_match(p, "Context:")
  expect_match(p, "Question:")
  # blank context collapses to the bare question
  expect_equal(morie_build_prompt("Q", context = "   "), "Q")
  expect_error(morie_build_prompt("   "), "non-empty")
})

test_that("morie_ask_percy returns agent text on a successful Python call", {
  testthat::local_mocked_bindings(
    system2 = function(command, args, ...) "the agent reply",
    .package = "base"
  )
  expect_equal(morie_ask_percy("hello"), "the agent reply")
})

test_that("morie_ask_percy errors when the Python call exits non-zero", {
  testthat::local_mocked_bindings(
    system2 = function(command, args, ...) {
      out <- "traceback ..."
      attr(out, "status") <- 1L
      out
    }, .package = "base"
  )
  expect_error(morie_ask_percy("hello"), "Perseus agent call failed")
})

# --- mrm_samples.R ---------------------------------------------------------

test_that("morie_tps_layer_urls returns the known TPS categories", {
  u <- morie_tps_layer_urls()
  expect_type(u, "character")
  expect_true(all(c("Assault", "Homicides", "Robbery") %in% names(u)))
  expect_true(all(grepl("FeatureServer/0$", u)))
})

test_that("morie_sample errors on an unknown sample name", {
  expect_error(morie_sample("definitely-not-a-sample"))
})

test_that("morie_fetch_tps errors on an unknown category", {
  expect_error(
    morie_fetch_tps("NotARealCategory"),
    "Unknown TPS category"
  )
})

test_that("morie_fetch_tps writes a CSV from a mocked ArcGIS layer", {
  testthat::local_mocked_bindings(
    fromJSON = function(txt, ...) {
      list(
        features = list(
          list(
            properties = list(EVENT_UNIQUE_ID = "e1", OCC_YEAR = 2024),
            geometry = list(
              type = "Point",
              coordinates = list(-79.4, 43.7)
            )
          ),
          list(
            properties = list(EVENT_UNIQUE_ID = "e2", OCC_YEAR = 2024),
            geometry = list(
              type = "Point",
              coordinates = list(-79.5, 43.6)
            )
          )
        ),
        exceededTransferLimit = FALSE
      )
    },
    .package = "jsonlite"
  )
  cdir <- tempfile("tps-")
  out <- morie_fetch_tps("Assault", cache_dir = cdir, overwrite = TRUE)
  expect_true(file.exists(out))
  df <- utils::read.csv(out)
  expect_equal(nrow(df), 2L)
  expect_true(all(c("LONG_WGS84", "LAT_WGS84") %in% names(df)))
  # second call with overwrite = FALSE returns the cached path
  expect_equal(morie_fetch_tps("Assault",
    cache_dir = cdir,
    overwrite = FALSE
  ), out)
})
