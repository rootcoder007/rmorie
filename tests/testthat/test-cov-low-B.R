# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage lift batch B: cslat, rgcrl, mrm_samples, csphr, mrm_mandela_spectrum.

# ==== cslat.R ====
test_that("causal_attention_mask works with integer length", {
  set.seed(1)
  out <- morie:::causal_attention_mask(5L)
  expect_type(out, "list")
  expect_equal(out$n, 5L)
  expect_equal(out$method, "causal-mask")
  expect_equal(dim(out$tensor), c(5L, 5L))
})

test_that("causal_attention_mask works with a tensor-like array (uses dim)", {
  set.seed(1)
  arr <- array(0, dim = c(2L, 4L, 3L))
  out <- morie:::causal_attention_mask(arr)
  expect_type(out, "list")
  expect_equal(out$n, 4L)
  expect_equal(dim(out$tensor), c(4L, 4L))
})

test_that("causal_attention_mask falls back to length() for plain vectors", {
  set.seed(1)
  v <- 1:7
  out <- morie:::causal_attention_mask(v)
  expect_false(is.null(out))
  expect_equal(out$n, 7L)
  expect_equal(dim(out$tensor), c(7L, 7L))
})

# ==== rgcrl.R ====
test_that("rgcrl returns expected structure on random input", {
  set.seed(1)
  out <- rgcrl(rnorm(200), m = 3L, tau = 1L, n_r = 15L)
  expect_type(out, "list")
  expect_named(out, c("D2", "log_r", "log_C", "m", "tau"))
  expect_equal(out$m, 3L)
  expect_equal(out$tau, 1L)
  expect_true(length(out$log_r) == length(out$log_C))
  expect_true(is.numeric(out$D2))
})

test_that("rgcrl errors on series too short for embedding", {
  set.seed(1)
  expect_error(rgcrl(rnorm(5), m = 3L, tau = 1L), "too short")
})

# ==== mrm_samples.R ====
test_that("morie_tps_layer_urls returns the expected named character vector", {
  set.seed(1)
  urls <- morie_tps_layer_urls()
  expect_type(urls, "character")
  expect_true("Assault" %in% names(urls))
  expect_true("Homicides" %in% names(urls))
  expect_gte(length(urls), 9L)
  expect_true(all(grepl("^https://", urls)))
})

test_that("morie_fetch_tps errors on unknown category without network", {
  set.seed(1)
  expect_error(
    morie_fetch_tps("NotARealCategory", cache_dir = tempfile()),
    "Unknown TPS category"
  )
})

# ==== csphr.R ====
test_that("csphr returns zero-weight stub when votes are NULL", {
  set.seed(1)
  X <- matrix(rnorm(50 * 2), 50, 2)
  out <- csphr(X, votes = NULL)
  expect_type(out, "list")
  expect_equal(out$method, "morie_cutting_plane_sphere")
  expect_equal(out$n, 50L)
  expect_equal(out$p, 2L)
  expect_equal(out$correct_class, 0L)
  expect_true(all(out$w == 0))
})

test_that("csphr handles a single-class vote vector via majority stub", {
  set.seed(1)
  X <- matrix(rnorm(40 * 2), 40, 2)
  out <- csphr(X, votes = rep(1L, 40))
  expect_equal(out$method, "morie_cutting_plane_sphere")
  expect_equal(out$correct_class, 40L)
  expect_true(all(out$w == 0))
})

test_that("csphr separates two clouds with > 50% accuracy", {
  set.seed(1)
  n <- 50L
  X1 <- matrix(rnorm(n * 2, mean = 2), ncol = 2)
  X0 <- matrix(rnorm(n * 2, mean = -2), ncol = 2)
  X <- rbind(X1, X0)
  v <- c(rep(1L, n), rep(0L, n))
  out <- csphr(X, votes = v)
  expect_type(out, "list")
  expect_equal(out$n, 2L * n)
  expect_equal(out$p, 2L)
  expect_gte(out$correct_class, n)
  expect_length(out$w, 2L)
})

# ==== mrm_mandela_spectrum.R ====
test_that("mrm_otis_mandela_spectrum returns tidy long-format frame", {
  set.seed(1)
  n <- 60L
  df <- data.frame(
    NumberConsecutiveDays_Segregation = sample(c(1, 5, 10, 16, 25, 40), n, replace = TRUE),
    EndFiscalYear = sample(c(2023, 2024, 2025), n, replace = TRUE),
    UniqueIndividual_ID = sample(paste0("id", 1:20), n, replace = TRUE),
    MentalHealth_Alert = sample(c("Yes", "No"), n, replace = TRUE),
    SuicideRisk_Alert = sample(c("Yes", "No"), n, replace = TRUE),
    SuicideWatch_Alert = sample(c("Yes", "No"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  out <- mrm_otis_mandela_spectrum(df)
  expect_s3_class(out, "data.frame")
  expect_true(all(c(
    "year", "denominator", "contact_proxy",
    "n_eligible", "n_mandela", "rate", "pct"
  ) %in% names(out)))
  expect_true("pooled" %in% out$year)
})

test_that("mrm_otis_mandela_spectrum errors when required columns are missing", {
  set.seed(1)
  bad <- data.frame(x = 1:5, y = 1:5)
  expect_error(mrm_otis_mandela_spectrum(bad))
})

test_that("mrm_otis_mandela_spectrum errors when data is not a data.frame", {
  set.seed(1)
  expect_error(mrm_otis_mandela_spectrum(list(a = 1)))
})
