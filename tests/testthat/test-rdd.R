# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

# ---------------------------------------------------------------------------
# Coverage tests for R/rdd.R
# Synthetic data: running ~ uniform[-1, 1]; treated <- running >= 0
# Outcome jumps by +1 at the cutoff
# ---------------------------------------------------------------------------

set.seed(1)

make_rdd_data <- function(n = 400, tau = 1.0, cutoff = 0, seed = 1) {
  set.seed(seed)
  x <- runif(n, -1, 1)
  treated <- as.integer(x >= cutoff)
  y <- 0.3 * x + tau * treated + rnorm(n, sd = 0.3)
  data.frame(x = x, y = y, treated = treated)
}

make_fuzzy_rdd_data <- function(n = 400, seed = 1) {
  set.seed(seed)
  x <- runif(n, -1, 1)
  z <- as.integer(x >= 0)
  # Imperfect compliance
  treated <- as.integer(plogis(2 * z + rnorm(n)) > 0.5)
  y <- 0.3 * x + 1.0 * treated + rnorm(n, sd = 0.3)
  data.frame(x = x, y = y, treated = treated)
}

# ---------------------------------------------------------------------------
# Kernels
# ---------------------------------------------------------------------------

test_that("kernel functions are zero outside [-1, 1] (where applicable)", {
  expect_equal(morie_rdd_kernel_triangular(2), 0)
  expect_equal(morie_rdd_kernel_epanechnikov(2), 0)
  expect_equal(morie_rdd_kernel_uniform(2), 0)
  expect_true(is.finite(morie_rdd_kernel_gaussian(0)))
})

test_that("kernel functions positive at u=0", {
  expect_gt(morie_rdd_kernel_triangular(0), 0)
  expect_gt(morie_rdd_kernel_epanechnikov(0), 0)
  expect_gt(morie_rdd_kernel_uniform(0), 0)
  expect_gt(morie_rdd_kernel_gaussian(0), 0)
})

# ---------------------------------------------------------------------------
# Local polynomial
# ---------------------------------------------------------------------------

test_that("morie_rdd_local_polynomial returns one row per eval point", {
  set.seed(1)
  d <- make_rdd_data()
  ep <- c(-0.5, 0, 0.5)
  out <- morie_rdd_local_polynomial(d$x, d$y, ep, h = 0.5)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), length(ep))
})

# ---------------------------------------------------------------------------
# Bandwidth selectors
# ---------------------------------------------------------------------------

test_that("morie_rdd_bandwidth_rot returns positive bandwidth", {
  set.seed(1)
  d <- make_rdd_data()
  bw <- morie_rdd_bandwidth_rot(d$x, d$y)
  expect_gt(bw$bandwidth, 0)
})

test_that("morie_rdd_bandwidth_ik returns positive bandwidth (rdrobust or fallback)", {
  set.seed(1)
  d <- make_rdd_data()
  bw <- morie_rdd_bandwidth_ik(d$x, d$y)
  expect_gt(bw$bandwidth, 0)
})

test_that("morie_rdd_bandwidth_cct returns positive bandwidth (rdrobust or fallback)", {
  set.seed(1)
  d <- make_rdd_data()
  bw <- morie_rdd_bandwidth_cct(d$x, d$y)
  expect_gt(bw$bandwidth, 0)
})

# ---------------------------------------------------------------------------
# Sharp / fuzzy / bias-corrected
# ---------------------------------------------------------------------------

test_that("morie_rdd_sharp recovers tau ~ 1 on synthetic jump", {
  d <- make_rdd_data(n = 1000, tau = 1.0, seed = 2)
  res <- morie_rdd_sharp(d, "y", "x")
  expect_true(is.finite(res$estimate))
  expect_equal(res$estimate, 1.0, tolerance = 0.4)
  expect_lt(res$ci_lower, res$ci_upper)
})

test_that("morie_rdd_sharp honours a user-supplied bandwidth", {
  d <- make_rdd_data(n = 400)
  res <- morie_rdd_sharp(d, "y", "x", bandwidth = 0.3)
  expect_true(is.finite(res$estimate))
})

test_that("morie_rdd_fuzzy returns a finite Wald-ratio estimate", {
  d <- make_fuzzy_rdd_data(n = 800)
  res <- morie_rdd_fuzzy(d, "y", "x", "treated")
  expect_true(is.finite(res$estimate))
  expect_true(is.finite(res$std_error))
})

test_that("morie_rdd_bias_corrected runs (rdrobust or fallback)", {
  d <- make_rdd_data(n = 600)
  res <- morie_rdd_bias_corrected(d, "y", "x")
  expect_true(is.finite(res$estimate))
})

# ---------------------------------------------------------------------------
# Density tests
# ---------------------------------------------------------------------------

test_that("morie_rdd_mccrary returns a list with name", {
  set.seed(1)
  res <- morie_rdd_mccrary(runif(300, -1, 1))
  expect_true("name" %in% names(res))
})

test_that("morie_rdd_cattaneo_density delegates / runs", {
  set.seed(1)
  res <- morie_rdd_cattaneo_density(runif(300, -1, 1))
  expect_true("name" %in% names(res))
})

# ---------------------------------------------------------------------------
# Validity diagnostics
# ---------------------------------------------------------------------------

test_that("morie_rdd_covariate_balance returns one row per covariate", {
  set.seed(1)
  d <- make_rdd_data(n = 400)
  d$c1 <- rnorm(nrow(d)); d$c2 <- rnorm(nrow(d))
  out <- morie_rdd_covariate_balance(d, "x", c("c1", "c2"))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
})

test_that("morie_rdd_placebo_cutoff skips the true cutoff", {
  set.seed(1)
  d <- make_rdd_data(n = 600)
  out <- morie_rdd_placebo_cutoff(d, "y", "x", true_cutoff = 0,
                                  placebo_cutoffs = c(-0.5, 0, 0.5))
  expect_true(nrow(out) <= 2L)
})

test_that("morie_rdd_donut excludes points within the donut radius", {
  d <- make_rdd_data(n = 800)
  res <- morie_rdd_donut(d, "y", "x", donut = 0.05)
  expect_true(grepl("donut=0.05", res$method, fixed = TRUE))
  expect_equal(res$details$donut, 0.05)
})

test_that("morie_rdd_discrete uses p=0 and uniform kernel", {
  set.seed(1)
  d <- make_rdd_data(n = 400)
  res <- morie_rdd_discrete(d, "y", "x")
  expect_true(grepl("discrete running var", res$method, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Plot data / sensitivity / kink / local-randomisation / geographic
# ---------------------------------------------------------------------------

test_that("morie_rdd_plot_data returns bins + poly", {
  set.seed(1)
  d <- make_rdd_data(n = 300)
  out <- morie_rdd_plot_data(d, "y", "x", n_bins = 10L)
  expect_true("bins" %in% names(out))
  expect_true("poly" %in% names(out))
})

test_that("morie_rdd_bandwidth_sensitivity returns a frame of estimates", {
  set.seed(1)
  d <- make_rdd_data(n = 400)
  out <- morie_rdd_bandwidth_sensitivity(d, "y", "x")
  expect_s3_class(out, "data.frame")
  expect_true(all(c("bandwidth", "estimate", "p_value") %in% names(out)))
})

test_that("morie_rdd_kink runs (rdrobust or sharp fallback)", {
  set.seed(1)
  d <- make_rdd_data(n = 400)
  res <- morie_rdd_kink(d, "y", "x")
  expect_true(is.finite(res$estimate))
})

test_that("morie_rdd_local_randomisation returns p in [0, 1]", {
  d <- make_rdd_data(n = 400)
  res <- morie_rdd_local_randomisation(d, "y", "x", window = 0.3,
                                       n_permutations = 200L, seed = 1)
  expect_gte(res$p_value, 0)
  expect_lte(res$p_value, 1)
  expect_true(is.finite(res$estimate))
})

test_that("morie_rdd_geographic returns a sharp-RDD result on signed distance", {
  set.seed(1)
  n <- 400
  side <- rbinom(n, 1, 0.5)
  dist_to_boundary <- runif(n, 0, 1)
  signed <- ifelse(side == 1, dist_to_boundary, -dist_to_boundary)
  y <- 0.5 * (signed >= 0) + rnorm(n, sd = 0.3)
  d <- data.frame(y = y, dist_to_boundary = dist_to_boundary,
                  side = side)
  res <- morie_rdd_geographic(d, "y", "dist_to_boundary", "side")
  expect_true(grepl("geographic", res$method))
  expect_true(is.finite(res$estimate))
})

# ---------------------------------------------------------------------------
# Power and sample-size
# ---------------------------------------------------------------------------

test_that("morie_rdd_power returns power in [0, 1] for each kernel", {
  for (k in c("triangular", "epanechnikov", "uniform", "gaussian")) {
    res <- morie_rdd_power(n = 500, tau = 0.5, sigma = 1, kernel = k)
    expect_gte(res$power, 0); expect_lte(res$power, 1)
  }
})

test_that("morie_rdd_sample_size returns a positive integer", {
  ss <- morie_rdd_sample_size(tau = 0.5, sigma = 1, power = 0.8)
  expect_true(is.integer(ss))
  expect_gt(ss, 0)
})

test_that("morie_rdd_power respects user-supplied bandwidth", {
  res <- morie_rdd_power(n = 500, tau = 0.5, sigma = 1, bandwidth = 0.2)
  expect_true(is.finite(res$power))
})