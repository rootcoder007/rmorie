# SPDX-License-Identifier: AGPL-3.0-or-later
# Tests for batch03: cnnge, cntgc, cntrl, cohrc, coitg, cokrg, confm,
# copul, cov2s, covsp, cslat, cslnc, csphr, ctmed, ctrlc.

test_that("morie_cnn_genomic returns expected structure", {
  set.seed(7)
  M <- matrix(rnorm(160), 20, 8)
  y <- M[, 2] + M[, 4] + 0.2 * rnorm(20)
  res <- morie_cnn_genomic(rep(0, 20), y, M, n_epochs = 10, seed = 7)
  expect_true(is.list(res))
  expect_true(all(c(
    "estimate", "y_hat", "W_conv", "b_conv", "W1", "b1",
    "w2", "b2", "loss_curve", "se", "n", "method"
  ) %in%
    names(res)))
  expect_equal(res$n, 20L)
  expect_length(res$y_hat, 20L)
  expect_true(all(is.finite(res$y_hat)))
  expect_true(is.finite(res$estimate))
  expect_true(is.finite(res$se))
  expect_gte(res$se, 0)
  expect_length(res$loss_curve, 10L)
  expect_type(res$method, "character")
})

test_that("morie_cnn_genomic respects hyperparameters and clamps kernel", {
  set.seed(11)
  M <- matrix(rnorm(60), 15, 4)
  y <- as.numeric(M %*% c(1, -1, 0.5, 0) + 0.1 * rnorm(15))
  res <- morie_cnn_genomic(NULL, y, M,
    n_filters = 4, kernel = 9,
    hidden = 5, n_epochs = 5, lr = 5e-3, l2 = 1e-2,
    seed = 1
  )
  expect_equal(res$n, 15L)
  expect_equal(nrow(res$W_conv), 4L)
  expect_equal(ncol(res$W_conv), 4L)
  expect_length(res$loss_curve, 5L)
  expect_true(all(is.finite(res$loss_curve)))
})

test_that("morie_cnn_genomic accepts a data.frame marker input", {
  set.seed(3)
  M <- as.data.frame(matrix(rnorm(48), 12, 4))
  y <- rnorm(12)
  res <- morie_cnn_genomic(rep(0, 12), y, M, n_epochs = 4, seed = 3)
  expect_equal(res$n, 12L)
  expect_length(res$y_hat, 12L)
})

test_that("morie_contingency_coefficient computes C and Cramer's V", {
  tbl <- matrix(c(20, 10, 5, 8, 15, 12), nrow = 2, byrow = TRUE)
  res <- morie_contingency_coefficient(tbl)
  expect_true(is.list(res))
  expect_true(all(c(
    "statistic", "morie_cramers_v", "chi2", "p_value", "df",
    "max_C", "n", "method"
  ) %in% names(res)))
  expect_gte(res$statistic, 0)
  expect_lte(res$statistic, 1)
  expect_gte(res$morie_cramers_v, 0)
  expect_lte(res$morie_cramers_v, 1)
  expect_gte(res$chi2, 0)
  expect_gte(res$p_value, 0)
  expect_lte(res$p_value, 1)
  expect_equal(res$n, sum(tbl))
  expect_true(is.finite(res$max_C))
})

test_that("morie_contingency_coefficient handles a square table", {
  tbl <- matrix(c(10, 5, 3, 4, 12, 6, 2, 7, 9), nrow = 3, byrow = TRUE)
  res <- morie_contingency_coefficient(tbl)
  expect_equal(res$df, 4L)
  expect_true(is.finite(res$statistic))
})

test_that("morie_contingency_coefficient returns NA structure for empty input", {
  res <- morie_contingency_coefficient(matrix(numeric(0), 0, 0))
  expect_true(is.na(res$statistic))
  expect_equal(res$n, 0L)
})

test_that("morie_control_variates reduces variance with a correlated control", {
  set.seed(0)
  u <- runif(1000)
  y <- u + rnorm(1000, sd = 0.01)
  res <- morie_control_variates(y, u, 0.5)
  expect_true(is.list(res))
  expect_true(all(c(
    "estimate", "se", "c_coef",
    "var_ratio_cv_over_crude", "n", "method"
  ) %in%
    names(res)))
  expect_equal(res$n, 1000L)
  expect_true(is.finite(res$estimate))
  expect_gte(res$se, 0)
  expect_gte(res$var_ratio_cv_over_crude, 0)
  expect_lte(res$var_ratio_cv_over_crude, 1)
  expect_lt(abs(res$estimate - 0.5), 0.05)
})

test_that("morie_control_variates flags bad input", {
  res <- morie_control_variates(c(1, 2, 3), c(1, 2), 0)
  expect_true(is.na(res$estimate))
  res2 <- morie_control_variates(1, 1, 0)
  expect_true(is.na(res2$estimate))
})

test_that("cntrl_estimator internal helper works directly", {
  set.seed(5)
  y <- rnorm(100, mean = 2)
  cc <- y + rnorm(100, sd = 0.5)
  res <- morie:::cntrl_estimator(y, cc, mean(cc))
  expect_true(is.finite(res$estimate))
  expect_equal(res$n, 100L)
})

test_that("morie_coherence returns frequencies and bounded morie_coherence", {
  set.seed(1)
  n <- 200
  t <- seq_len(n)
  x <- sin(2 * pi * t / 20) + rnorm(n, sd = 0.3)
  y <- sin(2 * pi * t / 20 + 0.5) + rnorm(n, sd = 0.3)
  res <- morie_coherence(x, y)
  expect_true(is.list(res))
  expect_true(all(c(
    "frequencies", "morie_coherence", "n_segments", "nperseg",
    "fs", "n", "method"
  ) %in% names(res)))
  expect_equal(res$n, n)
  expect_equal(length(res$frequencies), length(res$morie_coherence))
  expect_true(all(is.finite(res$morie_coherence)))
  expect_true(all(res$morie_coherence >= 0))
  expect_true(all(res$morie_coherence <= 1 + 1e-8))
  expect_gte(res$n_segments, 1)
})

test_that("morie_coherence honours nperseg and fs arguments", {
  set.seed(2)
  x <- rnorm(120)
  y <- rnorm(120)
  res <- morie_coherence(x, y, nperseg = 32, fs = 100)
  expect_equal(res$nperseg, 32L)
  expect_equal(res$fs, 100)
  expect_equal(max(res$frequencies), 50)
})

test_that("morie_coherence errors on mismatched or short input", {
  expect_error(morie_coherence(rnorm(50), rnorm(40)), "mismatch")
  expect_error(morie_coherence(rnorm(5), rnorm(5)), ">=8")
})

test_that("morie_eg_coint returns Engle-Granger structure", {
  set.seed(4)
  w <- cumsum(rnorm(120))
  y2 <- w + rnorm(120, sd = 0.5)
  y1 <- 2 * w + rnorm(120, sd = 0.5)
  res <- morie_eg_coint(y1, y2)
  expect_true(is.list(res))
  expect_true(all(c(
    "adf_statistic", "p_value", "beta",
    "critical_values", "n", "method"
  ) %in% names(res)))
  expect_true(is.finite(res$adf_statistic))
  expect_gte(res$p_value, 0)
  expect_lte(res$p_value, 1)
  expect_length(res$beta, 2L)
  expect_equal(res$n, 120L)
  expect_length(res$critical_values, 3L)
})

test_that("morie_eg_coint accepts an explicit max_lag", {
  set.seed(6)
  y2 <- cumsum(rnorm(60))
  y1 <- y2 + rnorm(60, sd = 0.3)
  res <- morie_eg_coint(y1, y2, max_lag = 2)
  expect_true(is.finite(res$adf_statistic))
  expect_equal(res$n, 60L)
})

test_that("morie_eg_coint errors on mismatched or short series", {
  expect_error(morie_eg_coint(rnorm(30), rnorm(25)), "mismatch")
  expect_error(morie_eg_coint(rnorm(10), rnorm(10)), ">=20")
})

test_that("cokrg predicts at a single target", {
  set.seed(8)
  coords <- matrix(runif(40), 20, 2)
  x <- rnorm(20)
  y <- x + rnorm(20, sd = 0.2)
  res <- cokrg(x, y, coords, target = c(0.5, 0.5))
  expect_true(is.list(res))
  expect_true(all(c("estimate", "se", "n", "method") %in% names(res)))
  expect_equal(res$n, 20L)
  expect_true(is.finite(res$estimate))
  expect_gte(res$se, 0)
})

test_that("cokrg predicts at multiple targets", {
  set.seed(9)
  coords <- matrix(runif(30), 15, 2)
  x <- rnorm(15)
  y <- rnorm(15)
  target <- matrix(runif(8), 4, 2)
  res <- cokrg(x, y, coords, target,
    sill_p = 2, range_p = 1.5,
    sill_s = 1.5, range_s = 1,
    cross_sill = 0.4, cross_range = 1.2,
    nugget = 0.1
  )
  expect_length(res$estimate, 4L)
  expect_length(res$se, 4L)
  expect_true(all(is.finite(res$estimate)))
  expect_true(all(res$se >= 0))
})

test_that("morie_cokriging alias matches cokrg and validates dims", {
  set.seed(10)
  coords <- matrix(runif(24), 12, 2)
  x <- rnorm(12)
  y <- rnorm(12)
  res <- morie_cokriging(x, y, coords, target = c(0.3, 0.7))
  expect_true(is.finite(res$estimate))
  expect_error(
    cokrg(x, y, coords, target = c(0.3, 0.7, 0.1)),
    "dim mismatch"
  )
  expect_error(
    cokrg(x[1:5], y, coords, target = c(0.3, 0.7)),
    "matching n"
  )
})

test_that("morie_confusion_matrix_metrics computes accuracy and F1", {
  yt <- c("a", "a", "b", "b", "c", "c", "a", "b")
  yp <- c("a", "b", "b", "b", "c", "a", "a", "b")
  res <- morie_confusion_matrix_metrics(yt, yp)
  expect_true(is.list(res))
  expect_true(all(c(
    "estimate", "accuracy", "confusion_matrix", "labels",
    "precision", "recall", "f1", "macro_precision",
    "macro_recall", "macro_f1", "weighted_f1", "n",
    "method"
  ) %in% names(res)))
  expect_equal(res$accuracy, res$estimate)
  expect_gte(res$accuracy, 0)
  expect_lte(res$accuracy, 1)
  expect_equal(res$n, length(yt))
  expect_equal(dim(res$confusion_matrix), c(3L, 3L))
  expect_equal(sum(res$confusion_matrix), length(yt))
  expect_true(all(res$f1 >= 0 & res$f1 <= 1))
  expect_true(all(res$precision >= 0 & res$precision <= 1))
})

test_that("morie_confusion_matrix_metrics honours explicit label ordering", {
  yt <- c(1, 0, 1, 1, 0)
  yp <- c(1, 0, 0, 1, 0)
  res <- morie_confusion_matrix_metrics(yt, yp, labels = c(0, 1))
  expect_equal(res$labels, c("0", "1"))
  expect_equal(dim(res$confusion_matrix), c(2L, 2L))
})

test_that("morie_confusion_matrix_metrics handles a perfect classifier", {
  yt <- c("x", "y", "x", "y")
  res <- morie_confusion_matrix_metrics(yt, yt)
  expect_equal(res$accuracy, 1)
  expect_equal(res$macro_f1, 1)
})

test_that("morie_copula_estimation works for the gaussian family", {
  set.seed(0)
  x <- rnorm(300)
  y <- x + rnorm(300, sd = 0.5)
  res <- morie_copula_estimation(x, y, family = "gaussian")
  expect_true(is.list(res))
  expect_true(all(c(
    "estimate", "morie_kendall_tau", "se_tau", "u", "v",
    "family", "n", "method"
  ) %in% names(res)))
  expect_equal(res$family, "gaussian")
  expect_equal(res$n, 300L)
  expect_true(is.finite(res$estimate))
  expect_length(res$u, 300L)
  expect_length(res$v, 300L)
  expect_true(all(res$u > 0 & res$u < 1))
})

test_that("morie_copula_estimation works for clayton and gumbel families", {
  set.seed(1)
  x <- rnorm(150)
  y <- x + rnorm(150, sd = 0.4)
  rc <- morie_copula_estimation(x, y, family = "clayton")
  rg <- morie_copula_estimation(x, y, family = "gumbel")
  expect_equal(rc$family, "clayton")
  expect_equal(rg$family, "gumbel")
  expect_true(is.finite(rc$estimate) || is.infinite(rc$estimate))
  expect_true(is.finite(rg$estimate) || is.infinite(rg$estimate))
})

test_that("copul internal helper flags too-few observations", {
  res <- morie:::copul(c(1, 2), c(3, 4))
  expect_true(is.na(res$estimate))
  expect_equal(res$n, 2L)
})

test_that("morie_two_sample_coverage tabulates block frequencies", {
  set.seed(12)
  x <- rnorm(10)
  y <- rnorm(25)
  res <- morie_two_sample_coverage(x, y)
  expect_true(is.list(res))
  expect_true(all(c(
    "block_freq", "block_prop", "expected_prop", "m",
    "n", "cumulative", "method"
  ) %in% names(res)))
  expect_equal(res$m, 10L)
  expect_equal(res$n, 25L)
  expect_length(res$block_freq, 11L)
  expect_equal(sum(res$block_freq), 25L)
  expect_equal(res$cumulative, 25L)
  expect_equal(res$expected_prop, 1 / 11)
  expect_equal(sum(res$block_prop), 1)
})

test_that("morie_two_sample_coverage returns empty structure for empty input", {
  res <- morie_two_sample_coverage(numeric(0), rnorm(5))
  expect_length(res$block_freq, 0L)
  expect_true(is.na(res$expected_prop))
})

test_that("morie_one_sample_coverage returns coverages and cumulative", {
  set.seed(13)
  x <- rnorm(30)
  res <- morie_one_sample_coverage(x)
  expect_true(is.list(res))
  expect_true(all(c(
    "coverages", "cumulative", "expected", "n",
    "sample_min", "sample_max", "method"
  ) %in% names(res)))
  expect_equal(res$n, 30L)
  expect_length(res$coverages, 31L)
  expect_equal(sum(res$coverages), 1)
  expect_equal(res$expected, 1 / 31)
  expect_lte(res$sample_min, res$sample_max)
  expect_gte(res$cumulative, 0)
})

test_that("morie_one_sample_coverage handles too-short input", {
  res <- morie_one_sample_coverage(c(1))
  expect_length(res$coverages, 0L)
  expect_true(is.na(res$cumulative))
})

test_that("causal_attention_mask builds a triangular -Inf mask", {
  res <- morie:::causal_attention_mask(5L)
  expect_true(is.list(res))
  expect_true(all(c("tensor", "n", "method") %in% names(res)))
  expect_equal(res$n, 5L)
  expect_equal(dim(res$tensor), c(5L, 5L))
  expect_true(all(res$tensor[lower.tri(res$tensor, diag = TRUE)] == 0))
  expect_true(all(is.infinite(res$tensor[upper.tri(res$tensor)])))
})

test_that("causal_attention_mask infers length from a vector", {
  res <- morie:::causal_attention_mask(c(1, 2, 3, 4))
  expect_equal(res$n, 4L)
  expect_equal(dim(res$tensor), c(4L, 4L))
})

test_that("cosine_lr_schedule decays from lr_max", {
  steps <- c(0, 250, 500, 750, 1000)
  res <- morie:::cosine_lr_schedule(steps,
    lr_max = 1e-3,
    total_steps = 1000L
  )
  expect_true(is.list(res))
  expect_true(all(c(
    "value", "tensor", "step", "lr_max", "lr_min",
    "total_steps", "warmup_steps", "method"
  ) %in%
    names(res)))
  expect_length(res$tensor, 5L)
  expect_true(all(is.finite(res$tensor)))
  expect_true(all(res$tensor >= 0))
  expect_true(all(res$tensor <= 1e-3 + 1e-12))
  expect_equal(res$value, res$tensor[1])
})

test_that("cosine_lr_schedule applies a warmup ramp", {
  res <- morie:::cosine_lr_schedule(c(0, 50, 100, 500),
    lr_max = 1e-2, lr_min = 1e-4,
    total_steps = 1000L,
    warmup_steps = 100L
  )
  expect_true(res$tensor[1] <= res$tensor[2])
  expect_equal(res$warmup_steps, 100L)
})

test_that("cosine_lr_schedule errors when warmup exceeds total", {
  expect_error(
    morie:::cosine_lr_schedule(c(1, 2),
      total_steps = 10L,
      warmup_steps = 20L
    ),
    "total_steps"
  )
})

test_that("csphr fits a separating hyperplane", {
  set.seed(14)
  X <- rbind(
    matrix(rnorm(40, mean = 2), 20, 2),
    matrix(rnorm(40, mean = -2), 20, 2)
  )
  votes <- c(rep(1L, 20), rep(0L, 20))
  res <- csphr(X, votes)
  expect_true(is.list(res))
  expect_true(all(c(
    "w", "c", "midpoint", "correct_class", "n", "p",
    "method"
  ) %in% names(res)))
  expect_equal(res$n, 40L)
  expect_equal(res$p, 2L)
  expect_length(res$w, 2L)
  expect_true(is.finite(res$c))
  expect_gte(res$correct_class, 0L)
  expect_lte(res$correct_class, 40L)
})

test_that("csphr handles NULL votes and single-class input", {
  X <- matrix(rnorm(20), 10, 2)
  res_null <- csphr(X, votes = NULL)
  expect_true(is.na(res_null$c))
  expect_equal(res_null$correct_class, 0L)
  res_one <- csphr(X, votes = rep(1L, 10))
  expect_equal(res_one$correct_class, 10L)
})

test_that("morie_cutting_plane_sphere alias accepts a vector x", {
  res <- morie_cutting_plane_sphere(rnorm(8), votes = c(1, 0, 1, 0, 1, 0, 1, 0))
  expect_equal(res$p, 1L)
  expect_equal(res$n, 8L)
})

test_that("morie_control_median_test runs Mood's median test", {
  set.seed(15)
  x <- rnorm(40, mean = 0)
  y <- rnorm(45, mean = 1)
  res <- morie_control_median_test(x, y)
  expect_true(is.list(res))
  expect_true(all(c(
    "statistic", "p_value", "df", "n", "m", "n_y",
    "grand_median", "table", "method"
  ) %in% names(res)))
  expect_true(is.finite(res$statistic))
  expect_gte(res$p_value, 0)
  expect_lte(res$p_value, 1)
  expect_equal(res$n, 85L)
  expect_equal(res$m, 40L)
  expect_equal(res$n_y, 45L)
  expect_equal(dim(res$table), c(2L, 2L))
  expect_equal(sum(res$table), 85L)
})

test_that("morie_control_median_test flags too-short input", {
  res <- morie_control_median_test(c(1), c(2, 3, 4))
  expect_true(is.na(res$statistic))
  expect_equal(res$n, 4L)
})

test_that("morie_control_comparison compares treatments to a control", {
  set.seed(16)
  groups <- list(
    control = rnorm(20, mean = 0),
    trt1    = rnorm(20, mean = 1),
    trt2    = rnorm(20, mean = 2)
  )
  res <- morie_control_comparison(groups)
  expect_true(is.list(res))
  expect_true(all(c(
    "statistic", "p_value", "p_adjusted", "n", "k",
    "control_n", "adjust", "method"
  ) %in% names(res)))
  expect_equal(res$k, 2L)
  expect_equal(res$control_n, 20L)
  expect_length(res$statistic, 2L)
  expect_length(res$p_value, 2L)
  expect_length(res$p_adjusted, 2L)
  expect_true(all(res$p_adjusted >= res$p_value - 1e-12))
  expect_true(all(res$p_adjusted <= 1))
  expect_equal(res$adjust, "bonferroni")
})

test_that("morie_control_comparison supports adjust='none' and control_index", {
  set.seed(17)
  groups <- list(
    trt1    = rnorm(15, mean = 1),
    control = rnorm(15, mean = 0),
    trt2    = rnorm(15, mean = 2)
  )
  res <- morie_control_comparison(groups, control_index = 2L, adjust = "none")
  expect_equal(res$adjust, "none")
  expect_equal(res$p_adjusted, res$p_value)
  expect_equal(res$k, 2L)
})

test_that("morie_control_comparison returns empty structure for bad input", {
  res <- morie_control_comparison(list(rnorm(10)))
  expect_equal(res$k, 0L)
  expect_length(res$statistic, 0L)
})

test_that("morie_control_comparison flags groups too small for Wilcoxon", {
  groups <- list(control = c(1), trt1 = rnorm(10))
  res <- morie_control_comparison(groups)
  expect_true(is.na(res$statistic[1]))
  expect_true(is.na(res$p_value[1]))
})
