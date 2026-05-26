library(testthat)

# Tests for the Phase 1.h causal-extender wrappers added in R/causal.R:
#   * morie_causal_impact()    -> CausalImpact
#   * morie_causal_weighting() -> WeightIt
#   * morie_causal_robust_se() -> sandwich
#
# Every test is guarded with skip_if_not_installed() so the suite stays
# green on CRAN-only / minimal Suggests installs (per Phase 1.f
# precedent and CRAN policy).


# ---------------------------------------------------------------------------
# Hard-error contracts (no Suggests dependency)
# ---------------------------------------------------------------------------

test_that("morie_causal_impact hard-errors when CausalImpact is missing", {
  with_mocked_bindings(
    .causal_have_causalimpact = function() FALSE,
    .package = "rmorie",
    code = expect_error(
      morie_causal_impact(data = data.frame(y = rnorm(10), x = rnorm(10)),
                          pre_period = c(1, 5),
                          post_period = c(6, 10)),
      regexp = "CausalImpact"
    )
  )
})

test_that("morie_causal_weighting hard-errors when WeightIt is missing", {
  with_mocked_bindings(
    .causal_have_weightit = function() FALSE,
    .package = "rmorie",
    code = expect_error(
      morie_causal_weighting(data = data.frame(t = rbinom(20, 1, 0.4),
                                               x = rnorm(20)),
                             treatment = "t",
                             covariates = "x"),
      regexp = "WeightIt"
    )
  )
})

test_that("morie_causal_robust_se hard-errors when sandwich is missing", {
  with_mocked_bindings(
    .causal_have_sandwich = function() FALSE,
    .package = "rmorie",
    code = expect_error(
      morie_causal_robust_se(stats::lm(rnorm(20) ~ rnorm(20))),
      regexp = "sandwich"
    )
  )
})

# ---------------------------------------------------------------------------
# morie_causal_impact -- live CausalImpact integration
# ---------------------------------------------------------------------------

test_that("morie_causal_impact returns expected result fields", {
  skip_if_not_installed("CausalImpact")

  set.seed(42)
  n <- 80L
  pre_n <- 60L
  x <- arima.sim(model = list(ar = 0.6), n = n)
  y <- as.numeric(x) + rnorm(n, sd = 0.2)
  # Inject post-period intervention
  y[(pre_n + 1L):n] <- y[(pre_n + 1L):n] + 1.5
  d <- data.frame(y = y, x = as.numeric(x))

  res <- morie_causal_impact(
    data = d,
    pre_period = c(1L, pre_n),
    post_period = c(pre_n + 1L, n),
    model_args = list(niter = 200L)
  )
  expect_true(all(c("average_effect", "cumulative_effect",
                    "ci_lower", "ci_upper", "summary",
                    "posterior_prob_causal", "impact") %in% names(res)))
  expect_true(is.numeric(res$average_effect))
  expect_true(is.numeric(res$cumulative_effect))
})


# ---------------------------------------------------------------------------
# morie_causal_weighting -- live WeightIt integration
# ---------------------------------------------------------------------------

test_that("morie_causal_weighting returns weights with expected fields", {
  skip_if_not_installed("WeightIt")

  set.seed(1)
  n <- 200L
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  t <- rbinom(n, 1, plogis(0.4 * x1 - 0.3 * x2))
  d <- data.frame(t = t, x1 = x1, x2 = x2)

  res <- morie_causal_weighting(data = d, treatment = "t",
                                covariates = c("x1", "x2"),
                                method = "glm", estimand = "ATE")
  expect_true(all(c("weights", "method", "estimand", "ess",
                    "weightit") %in% names(res)))
  expect_length(res$weights, n)
  expect_true(all(is.finite(res$weights) & res$weights > 0))
  expect_equal(res$method, "glm")
  expect_equal(res$estimand, "ATE")
  expect_gt(res$ess, 0)
})


# ---------------------------------------------------------------------------
# morie_causal_robust_se -- live sandwich integration
# ---------------------------------------------------------------------------

test_that("morie_causal_robust_se HC3 returns vcov + named SE vector", {
  skip_if_not_installed("sandwich")

  set.seed(2)
  n <- 100L
  x <- rnorm(n)
  y <- 0.4 * x + rnorm(n) * (1 + 0.5 * abs(x))   # heteroskedastic
  fit <- stats::lm(y ~ x)

  res <- morie_causal_robust_se(fit, type = "HC3")
  expect_true(is.matrix(res$vcov))
  expect_named(res$se, c("(Intercept)", "x"))
  expect_equal(res$type, "HC3")
  expect_equal(res$n_coef, 2L)
  # Robust SE should be positive and finite
  expect_true(all(is.finite(res$se) & res$se > 0))
})

test_that("morie_causal_robust_se type='CL' requires a cluster argument", {
  skip_if_not_installed("sandwich")
  set.seed(3)
  n <- 50L
  d <- data.frame(y = rnorm(n), x = rnorm(n))
  fit <- stats::lm(y ~ x, data = d)
  expect_error(morie_causal_robust_se(fit, type = "CL"),
               regexp = "cluster")
})


