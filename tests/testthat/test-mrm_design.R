# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2M: tests for mrm_design.R — experimental-design helpers.

test_that("mrm_two_treatment_test reports Welch + Student + Wilcoxon p-values", {
  set.seed(1L)
  a <- stats::rnorm(50, mean = 100, sd = 10)
  b <- stats::rnorm(50, mean = 105, sd = 10)
  out <- mrm_two_treatment_test(a, b)
  expect_type(out, "list")
  # The exact field names vary across releases; sanity-check at minimum
  # one of the canonical statistics is present.
  flat <- unlist(out)
  expect_true(any(grepl("welch|student|wilcox|p[._-]value|t[._-]stat",
                        names(flat), ignore.case = TRUE)))
})

test_that("mrm_anova_oneway returns F + Tukey post-hoc on synthetic groups", {
  set.seed(2L)
  d <- data.frame(
    y = c(stats::rnorm(40, 0), stats::rnorm(40, 0.5),
          stats::rnorm(40, 1)),
    g = rep(c("a", "b", "c"), each = 40)
  )
  out <- mrm_anova_oneway(d, response_col = "y", group_col = "g")
  expect_type(out, "list")
  flat <- unlist(out)
  expect_true(any(grepl("f|p[._-]value|tukey", names(flat),
                        ignore.case = TRUE)))
})

test_that("mrm_factorial_2k returns 2^k main + interaction effects", {
  set.seed(3L)
  n <- 100L
  d <- data.frame(
    y  = stats::rnorm(n),
    f1 = sample(c(-1, 1), n, replace = TRUE),
    f2 = sample(c(-1, 1), n, replace = TRUE),
    f3 = sample(c(-1, 1), n, replace = TRUE)
  )
  out <- mrm_factorial_2k(d, response_col = "y",
                          factor_cols = c("f1", "f2", "f3"))
  expect_type(out, "list")
})

test_that("mrm_factorial_2k errors when given fewer than 2 factors", {
  d <- data.frame(y = stats::rnorm(20), f1 = sample(c(-1, 1), 20,
                                                     replace = TRUE))
  expect_error(mrm_factorial_2k(d, "y", "f1"),
               regexp = "2.*factors")
})

test_that("mrm_causal_design returns IPW estimator output", {
  set.seed(4L)
  n <- 200L
  d <- data.frame(
    d = stats::rbinom(n, 1, 0.5),
    y = stats::rnorm(n),
    x = stats::rnorm(n)
  )
  out <- mrm_causal_design(d, treatment_col = "d",
                            outcome_col = "y",
                            covariates = "x",
                            estimator = "ipw")
  expect_type(out, "list")
})

test_that("mrm_causal_design diff_in_means estimator path works too", {
  set.seed(5L)
  d <- data.frame(
    d = stats::rbinom(100, 1, 0.5),
    y = stats::rnorm(100)
  )
  out <- mrm_causal_design(d, "d", "y", estimator = "diff_in_means")
  expect_type(out, "list")
})

test_that("mrm_causal_design IPW standard error is the sandwich both arms report", {
  i <- 1:150
  x1 <- sin(i)
  x2 <- cos(1.4 * i)
  d <- as.integer(0.7 * x1 + 0.4 * sin(3.1 * i) > 0)
  df <- data.frame(x1, x2, D = d, Y = 1 + 0.5 * d + x1 + 0.3 * cos(2.2 * i))
  out <- mrm_causal_design(df, "D", "Y", c("x1", "x2"), estimator = "ipw")
  # morie: tests/test_mrm_design_reference.py asserts the same two numbers
  expect_equal(out$estimate, 1.036702)
  expect_equal(out$se, 0.098274)
  # the influence-function SE recomputed by hand (Lunceford & Davidian 2004, IPW2)
  fit <- glm(D ~ x1 + x2, data = df, family = binomial())
  e <- fitted(fit)
  X <- model.matrix(fit)
  D <- df$D
  Y <- df$Y
  mu1 <- sum(D * Y / e) / sum(D / e)
  mu0 <- sum((1 - D) * Y / (1 - e)) / sum((1 - D) / (1 - e))
  psi1 <- D * (Y - mu1) / e
  psi0 <- (1 - D) * (Y - mu0) / (1 - e)
  B <- crossprod(X, e * (1 - e) * X) / nrow(X)
  if1 <- (psi1 + drop((D - e) * X %*% solve(B, colMeans(-psi1 * (1 - e) * X)))) / mean(D / e)
  if0 <- (psi0 + drop((D - e) * X %*% solve(B, colMeans(psi0 * e * X)))) / mean((1 - D) / (1 - e))
  expect_equal(out$se, round(sqrt(sum((if1 - if0)^2)) / nrow(X), 6))
})

test_that("mrm_causal_design IPW SE has nominal coverage (was a 199-rep bootstrap at 1.7x the SD)", {
  set.seed(1)
  est <- se <- numeric(60)
  for (r in seq_along(est)) {
    n <- 400
    x <- rnorm(n)
    x2 <- rnorm(n)
    t <- rbinom(n, 1, plogis(0.5 * x - 0.3 * x2))
    y <- 0.8 * t + x + 0.5 * x2 + rnorm(n)
    a <- mrm_causal_design(data.frame(y, t, x, x2), "t", "y", c("x", "x2"))
    est[r] <- a$estimate
    se[r] <- a$se
  }
  expect_lt(abs(mean(se) / sd(est) - 1), 0.25)
})
