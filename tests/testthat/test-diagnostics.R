# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

set.seed(1)

make_diag_data <- function(n = 100, p = 3, seed = 1) {
  set.seed(seed)
  X <- cbind(1, matrix(rnorm(n * (p - 1L)), n, p - 1L))
  colnames(X) <- c("intercept", paste0("x", seq_len(p - 1L)))
  beta <- c(0.5, 1.0, -0.5)[seq_len(p)]
  y <- drop(X %*% beta + rnorm(n, sd = 0.5))
  y_hat <- drop(X %*% solve(crossprod(X), crossprod(X, y)))
  list(y = y, X = X, y_hat = y_hat, beta = beta)
}

# ---------------------------------------------------------------------------
# compute_residuals
# ---------------------------------------------------------------------------

test_that("compute_residuals returns expected structure", {
  set.seed(1); d <- make_diag_data()
  r <- compute_residuals(d$y, d$y_hat, d$X)
  expect_s3_class(r, "morie_residual_diagnostics")
  expect_length(r$raw_residuals, length(d$y))
  expect_length(r$standardized_residuals, length(d$y))
  expect_length(r$studentized_residuals, length(d$y))
  expect_true(is.numeric(r$fitted_values))
  expect_true(is.list(r$normality_test))
  expect_true(is.list(r$heteroskedasticity_test))
})

test_that("compute_residuals supports logistic", {
  set.seed(2); n <- 80L
  X <- cbind(1, rnorm(n))
  y <- rbinom(n, 1, plogis(X %*% c(0, 1)))
  y_hat <- plogis(drop(X %*% c(0, 1)))
  r <- compute_residuals(y, y_hat, X, model_type = "logistic")
  expect_true(!is.null(r$deviance_residuals))
  expect_true(!is.null(r$pearson_residuals))
})

test_that("compute_residuals supports poisson", {
  set.seed(3); n <- 80L
  X <- cbind(1, rnorm(n))
  y <- rpois(n, exp(X %*% c(0.5, 0.3)))
  y_hat <- exp(drop(X %*% c(0.5, 0.3)))
  r <- compute_residuals(y, y_hat, X, model_type = "poisson")
  expect_true(!is.null(r$deviance_residuals))
})

# ---------------------------------------------------------------------------
# compute_influence
# ---------------------------------------------------------------------------

test_that("compute_influence returns hat / cooks / dffits / dfbetas", {
  set.seed(4); d <- make_diag_data(n = 40L)
  r <- compute_influence(d$y, d$X)
  expect_s3_class(r, "morie_influence_diagnostics")
  expect_length(r$hat_values, 40L)
  expect_length(r$cooks_distance, 40L)
  expect_equal(dim(r$dfbetas), c(40L, ncol(d$X)))
  expect_true(is.integer(r$influential_indices) ||
              is.numeric(r$influential_indices))
})

# ---------------------------------------------------------------------------
# compute_vif and collinearity_diagnostics
# ---------------------------------------------------------------------------

test_that("compute_vif on uncorrelated columns is near 1", {
  set.seed(5)
  X <- matrix(rnorm(300), 100, 3)
  colnames(X) <- c("a","b","c")
  v <- compute_vif(X)
  expect_named(v, c("a","b","c"))
  expect_true(all(v < 2))
})

test_that("compute_vif on highly collinear columns is large", {
  set.seed(6)
  x1 <- rnorm(100)
  X <- cbind(x1, x1 + rnorm(100, sd = 0.01), rnorm(100))
  colnames(X) <- c("a","b","c")
  v <- compute_vif(X)
  expect_true(v["a"] > 5)
})

test_that("collinearity_diagnostics returns full structure", {
  set.seed(7)
  X <- matrix(rnorm(200), 50, 4)
  r <- collinearity_diagnostics(X)
  expect_s3_class(r, "morie_collinearity_diagnostics")
  expect_true(is.numeric(r$vif))
  expect_true(is.finite(r$condition_number))
})

# ---------------------------------------------------------------------------
# specification tests
# ---------------------------------------------------------------------------

test_that("ramsey_reset_test returns spec test object", {
  set.seed(8); d <- make_diag_data()
  r <- ramsey_reset_test(d$y, d$X)
  expect_s3_class(r, "morie_specification_test")
  expect_equal(r$name, "RESET")
  expect_true(is.finite(r$statistic))
})

test_that("link_test returns spec test", {
  set.seed(9); d <- make_diag_data()
  r <- link_test(d$y, d$X)
  expect_s3_class(r, "morie_specification_test")
  expect_equal(r$name, "link_test")
})

test_that("hosmer_lemeshow_test returns spec test", {
  set.seed(10); n <- 200L
  X <- cbind(1, rnorm(n))
  y_prob <- plogis(drop(X %*% c(0, 1)))
  y <- rbinom(n, 1, y_prob)
  r <- hosmer_lemeshow_test(y, y_prob, n_groups = 5L)
  expect_s3_class(r, "morie_specification_test")
  expect_equal(r$name, "hosmer_lemeshow")
  expect_true(is.finite(r$statistic))
})

# ---------------------------------------------------------------------------
# compute_goodness_of_fit
# ---------------------------------------------------------------------------

test_that("compute_goodness_of_fit linear returns R^2 + F", {
  set.seed(11); d <- make_diag_data()
  g <- compute_goodness_of_fit(d$y, d$y_hat, d$X)
  expect_s3_class(g, "morie_goodness_of_fit")
  expect_true(is.finite(g$r_squared))
  expect_true(g$r_squared >= 0 && g$r_squared <= 1)
  expect_true(is.finite(g$aic))
})

test_that("compute_goodness_of_fit logistic returns pseudo-R^2", {
  set.seed(12); n <- 80L
  X <- cbind(1, rnorm(n))
  y <- rbinom(n, 1, plogis(X %*% c(0, 1)))
  y_hat <- plogis(drop(X %*% c(0, 1)))
  g <- compute_goodness_of_fit(y, y_hat, X, model_type = "logistic")
  expect_true(is.finite(g$pseudo_r_squared))
  expect_true(is.finite(g$deviance))
})

test_that("compute_goodness_of_fit poisson works", {
  set.seed(13); n <- 80L
  X <- cbind(1, rnorm(n))
  y_hat <- exp(drop(X %*% c(0.5, 0.3)))
  y <- rpois(n, y_hat)
  g <- compute_goodness_of_fit(y, y_hat, X, model_type = "poisson")
  expect_true(is.finite(g$pseudo_r_squared))
})

# ---------------------------------------------------------------------------
# ph_assumption_test
# ---------------------------------------------------------------------------

test_that("ph_assumption_test returns list per covariate", {
  set.seed(14); n <- 50L
  times <- rexp(n)
  events <- rbinom(n, 1, 0.7)
  X <- matrix(rnorm(n * 2L), n, 2L)
  colnames(X) <- c("age","trt")
  out <- ph_assumption_test(times, events, X)
  expect_length(out, 2L)
  expect_s3_class(out[[1L]], "morie_specification_test")
  expect_true(grepl("age", out[[1L]]$name))
})

# ---------------------------------------------------------------------------
# likelihood / wald / score tests
# ---------------------------------------------------------------------------

test_that("likelihood_ratio_test computes chi-square p", {
  r <- likelihood_ratio_test(ll_restricted = -120, ll_full = -100,
                             df_diff = 2L)
  expect_s3_class(r, "morie_specification_test")
  expect_true(r$p_value < 0.05)
  expect_equal(r$df, 2L)
})

test_that("wald_test rejects under restriction", {
  V <- diag(2)
  r <- wald_test(estimates = c(5, 0), vcov = V)
  expect_s3_class(r, "morie_specification_test")
  expect_true(r$statistic > 0)
})

test_that("wald_test custom R/r works", {
  V <- diag(2)
  r <- wald_test(estimates = c(2, 1), vcov = V,
                 R = matrix(c(1, -1), nrow = 1L), r = 0)
  expect_true(is.finite(r$statistic))
})

test_that("score_test runs", {
  r <- score_test(score_vector = c(1, 0.5),
                  information_matrix = diag(2))
  expect_s3_class(r, "morie_specification_test")
  expect_true(is.finite(r$statistic))
})

# ---------------------------------------------------------------------------
# full_diagnostics
# ---------------------------------------------------------------------------

test_that("full_diagnostics linear runs end-to-end", {
  set.seed(15); d <- make_diag_data()
  r <- full_diagnostics(d$y, d$X)
  expect_s3_class(r, "morie_diagnostic_report")
  expect_s3_class(r$residuals, "morie_residual_diagnostics")
  expect_s3_class(r$influence, "morie_influence_diagnostics")
  expect_s3_class(r$collinearity, "morie_collinearity_diagnostics")
  expect_s3_class(r$goodness_of_fit, "morie_goodness_of_fit")
  expect_type(r$overall_assessment, "character")
})

test_that("full_diagnostics logistic runs and includes hosmer-lemeshow", {
  set.seed(16); n <- 80L
  X <- cbind(1, rnorm(n))
  y <- rbinom(n, 1, plogis(X %*% c(0, 1)))
  y_hat <- plogis(drop(X %*% c(0, 1)))
  r <- full_diagnostics(y, X, y_hat = y_hat, model_type = "logistic")
  expect_s3_class(r, "morie_diagnostic_report")
  names_spec <- vapply(r$specification_tests,
                       function(x) x$name, character(1))
  expect_true(any(grepl("hosmer", names_spec)))
})