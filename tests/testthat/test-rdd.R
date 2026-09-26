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
  d$c1 <- rnorm(nrow(d))
  d$c2 <- rnorm(nrow(d))
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
    expect_gte(res$power, 0)
    expect_lte(res$power, 1)
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


test_that("round four: rdd_plot_data returns the local fits around the cutoff", {
  set.seed(10)
  x <- runif(300, -1, 1)
  y <- 1 + 2 * (x >= 0) + x + rnorm(300, 0, 0.3)
  df <- data.frame(y = y, x = x)
  pd <- morie_rdd_plot_data(df, "y", "x", cutoff = 0, bandwidth = 0.5, p_local = 1)
  expect_true(all(c("bins", "poly", "local", "bandwidth", "cutoff") %in% names(pd)))
  expect_equal(pd$bandwidth, 0.5)
  expect_true(is.data.frame(pd$local$left) && is.data.frame(pd$local$right))
  expect_true(all(pd$local$left$x < 0) && all(pd$local$right$x >= 0))
})

test_that("round four: bandwidth_rot reports both sides of the cutoff", {
  set.seed(11)
  x <- runif(200, -2, 2)
  bw <- morie_rdd_bandwidth_rot(x, rnorm(200), cutoff = 0.5)
  expect_equal(bw$details$cutoff, 0.5)
  expect_true(is.finite(bw$details$h_left) && is.finite(bw$details$h_right))
})

test_that("round four: rdd_sharp runs a cluster bootstrap when asked", {
  set.seed(12)
  x <- runif(240, -1, 1)
  df <- data.frame(y = 1 + 2 * (x >= 0) + x + rnorm(240, 0, 0.3), x = x,
                   cl = rep(seq_len(24), each = 10))
  plain <- morie_rdd_sharp(df, "y", "x", bandwidth = 0.5)
  clus <- morie_rdd_sharp(df, "y", "x", bandwidth = 0.5, cluster = "cl")
  expect_equal(clus$estimate, plain$estimate)
  expect_true(is.finite(clus$std_error) && clus$std_error > 0)
})

test_that("sharp, fuzzy and kink RD equal rdrobust (vce = 'nn', h = 0.5)", {
  n <- 400
  i <- 0:(n - 1)
  x <- sin(1.37 * i) * 1.2 + 0.4 * cos(0.21 * i)
  tr <- as.integer(x >= 0)
  fz <- as.integer((x >= 0 & (i %% 5 != 0)) | (x < 0 & i %% 7 == 0))
  d <- data.frame(x,
                  y = 1 + 0.8 * x + 0.3 * x^2 + 1.2 * tr + 0.4 * sin(3.1 * i),
                  yf = 1 + 0.8 * x + 2 * fz + 0.4 * sin(3.1 * i),
                  yk = 1 + 0.8 * x + 1.5 * pmax(x, 0) + 0.4 * sin(3.1 * i), fz)
  s <- morie_rdd_sharp(d, "y", "x", bandwidth = 0.5)
  expect_equal(c(s$estimate, s$std_error), c(1.31895764244, 0.0843098682739), tolerance = 1e-10)
  f <- morie_rdd_fuzzy(d, "yf", "x", "fz", bandwidth = 0.5)
  expect_equal(c(f$estimate, f$std_error), c(2.30166428120295, 0.275553805228321), tolerance = 1e-12)
  k <- morie_rdd_kink(d, "yk", "x", bandwidth = 0.5)
  expect_equal(c(k$estimate, k$std_error), c(4.65174038836, 1.39440321497), tolerance = 1e-10)
  # fuzzy bias correction and robust SE: rdrobust(fuzzy = fz, h = 0.5, b = 0.5)
  cc <- morie_causrddc(d$yf, d$x, treatment = d$fz, h = 0.5, b = 0.5)
  expect_equal(c(cc$bias_corrected, cc$se_robust), c(2.45321344528486, 0.40614527539738), tolerance = 1e-12)
})

test_that("default bandwidths are rdrobust's mserd and results equal rdrobust()", {
  n <- 400
  i <- 0:(n - 1)
  x <- sin(1.37 * i) * 1.2 + 0.4 * cos(0.21 * i)
  tr <- as.integer(x >= 0)
  fz <- as.integer((x >= 0 & (i %% 5 != 0)) | (x < 0 & i %% 7 == 0))
  d <- data.frame(x,
                  y = 1 + 0.8 * x + 0.3 * x^2 + 1.2 * tr + 0.4 * sin(3.1 * i),
                  yf = 1 + 0.8 * x + 2 * fz + 0.4 * sin(3.1 * i),
                  yk = 1 + 0.8 * x + 1.5 * pmax(x, 0) + 0.4 * sin(3.1 * i), fz)
  # rdrobust::rdbwselect(y, x) / (fuzzy = fz) / (deriv = 1, p = 2): h, b
  bw <- morie_rd_mserd_bandwidth(d$y, d$x)
  expect_equal(c(bw$h, bw$b), c(0.584477848907144, 0.93283333886784), tolerance = 1e-10)
  bw <- morie_rd_mserd_bandwidth(d$yf, d$x, treatment = d$fz)
  expect_equal(c(bw$h, bw$b), c(0.479323994236654, 0.824084209003945), tolerance = 1e-10)
  bw <- morie_rd_mserd_bandwidth(d$yk, d$x, p = 2, deriv = 1)
  expect_equal(c(bw$h, bw$b), c(0.635424623301575, 0.965984594756331), tolerance = 1e-9)
  # rdrobust(y, x), rdrobust(yf, x, fuzzy = fz), rdrobust(yk, x, deriv = 1, p = 2)
  s <- morie_rdd_sharp(d, "y", "x")
  expect_equal(c(s$estimate, s$std_error), c(1.29449761957, 0.0787346761035), tolerance = 1e-9)
  f <- morie_rdd_fuzzy(d, "yf", "x", "fz")
  expect_equal(c(f$estimate, f$std_error), c(2.31807950054, 0.29104188413), tolerance = 1e-9)
  k <- morie_rdd_kink(d, "yk", "x")
  expect_equal(c(k$estimate, k$std_error), c(4.41013383404, 0.999751323253), tolerance = 1e-8)
  bc <- morie_rdd_bias_corrected(d, "y", "x")
  expect_equal(c(bc$estimate, bc$std_error), c(1.31362793469, 0.0921089745395), tolerance = 1e-9)
})

test_that("the density test equals rddensity (t_jk and its bandwidths)", {
  i <- 0:399
  x <- sin(1.37 * i) * 1.2 + 0.4 * cos(0.21 * i)
  # rddensity::rddensity(x, c = 0, h = 0.5); rddensity(x); rddensity(x, p = 1)
  expect_equal(morie_rdd_cattaneo_density(x, bandwidth = 0.5)$statistic, -0.424998995462388, tolerance = 1e-10)
  r <- morie_rdd_cattaneo_density(x)
  expect_equal(c(r$statistic, r$details$h_left, r$details$h_right),
               c(-0.328188049213095, 0.566842258247533, 0.536752772079201), tolerance = 1e-9)
  r <- morie_rdd_cattaneo_density(x, p = 1)
  expect_equal(c(r$statistic, r$details$h_left), c(0.829783793810616, 0.240307416380212), tolerance = 1e-9)
  # mass points: rddensity(round(x, 1)) and rddensity(round(x, 1), h = 0.6)
  expect_equal(morie_rdd_cattaneo_density(round(x, 1))$statistic, 0.317121592199131, tolerance = 1e-9)
  expect_equal(morie_rdd_cattaneo_density(round(x, 1), bandwidth = 0.6)$statistic, -0.746116220795682, tolerance = 1e-9)
})
