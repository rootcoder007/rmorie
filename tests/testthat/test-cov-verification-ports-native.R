# Verification ports: the Reinsch smoothing spline (its lambda limits),
# normal-normal empirical Bayes shrinkage, dynamic-regime values against
# glm/lm nuisances, the horseshoe Gibbs sampler, and the monotone
# Kantorovich map against the exact quantile map.

test_that("the smoothing spline interpolates at lambda = 0 and becomes the LS line as lambda grows", {
  set.seed(3)
  x <- sort(stats::runif(12, 0, 5))
  y <- sin(x) + stats::rnorm(12, sd = 0.2)
  s0 <- morie_esl_smoothing_spline(x, y, 0)
  expect_equal(s0$estimate, y, tolerance = 1e-12)
  expect_equal(s0$effective_df, 12, tolerance = 1e-12)
  # the penalty's null space is the linear functions: any lambda leaves a
  # straight line untouched, and the degrees of freedom fall towards 2
  lin <- 2 + 3 * x
  expect_equal(morie_esl_smoothing_spline(x, lin, 10)$estimate, lin, tolerance = 1e-10)
  dfs <- vapply(c(0.01, 1, 100, 1e4), function(l) morie_esl_smoothing_spline(x, y, l)$effective_df, 1)
  expect_true(all(diff(dfs) < 0) && all(dfs > 2 - 1e-6))
  mid <- morie_esl_smoothing_spline(x, y, 0.5)
  expect_equal(mid$rss, sum((y - mid$estimate)^2), tolerance = 1e-15)
  expect_true(mid$effective_df > 2 && mid$effective_df < 12)
  expect_error(morie_esl_smoothing_spline(x, y[-1], 1), "lengths differ")
  expect_error(morie_esl_smoothing_spline(rev(x), y, 1), "strictly increasing")
  expect_error(morie_esl_smoothing_spline(x, y, -1), "non-negative")
  expect_error(morie_esl_smoothing_spline(1:2, 1:2, 1), "at least 3")
})

test_that("empirical Bayes maximises the marginal likelihood and shrinks by s^2/(s^2 + tau^2)", {
  th <- c(1.2, -0.4, 2.5, 0.3, 0.9, 3.1, -1.0)
  se <- c(0.5, 0.8, 0.6, 0.4, 1.0, 0.7, 0.9)
  r <- morie_empirical_bayes(th, se)
  nll <- function(t2) {
    V <- se^2 + t2
    mu <- sum(th / V) / sum(1 / V)
    0.5 * sum(log(V) + (th - mu)^2 / V)
  }
  h <- 1e-4 * r$tau2
  expect_lt(abs((nll(r$tau2 + h) - nll(r$tau2 - h)) / (2 * h)), 1e-3)
  V <- se^2 + r$tau2
  gm <- sum(th / V) / sum(1 / V)
  B <- se^2 / V
  expect_equal(r$grand_mean, gm, tolerance = 1e-15)
  expect_equal(r$shrunk_estimates, gm + (1 - B) * (th - gm), tolerance = 1e-15)
})

test_that("regime values: IPW, regression and AIPW with fitted nuisances", {
  set.seed(8)
  n <- 150
  X <- cbind(stats::rnorm(n), stats::rnorm(n))
  d <- stats::rbinom(n, 1, stats::plogis(0.5 * X[, 1]))
  y <- 1 + d * (0.5 + X[, 2]) + X[, 1] + stats::rnorm(n, sd = 0.5)
  rule <- function(X) as.numeric(X[, 2] > -0.5)
  e <- stats::fitted(stats::glm(d ~ X, family = stats::binomial()))
  e <- pmin(pmax(e, 0.01), 0.99)
  pi <- ifelse(d == 1, e, 1 - e)
  mu1 <- as.numeric(cbind(1, X) %*% stats::coef(stats::lm(y ~ X, subset = d == 1)))
  mu0 <- as.numeric(cbind(1, X) %*% stats::coef(stats::lm(y ~ X, subset = d == 0)))
  g <- rule(X)
  mud <- ifelse(g == 1, mu1, mu0)
  f <- as.numeric(d == g)
  ref <- list(ipw = f / pi * y, regression = mud, aipw = mud + f / pi * (y - mud))
  for (m in names(ref)) {
    r <- morie_regime_value(y, d, X, rule, method = m)
    expect_equal(r$value, mean(ref[[m]]), tolerance = 1e-6)
    expect_equal(r$se, stats::sd(ref[[m]]) / sqrt(n), tolerance = 1e-6)
  }
  r <- morie_regime_value(y, d, X, g, propensity = rep(0.5, n), method = "ipw")
  expect_equal(r$value, mean(f / 0.5 * y), tolerance = 1e-12)
  expect_identical(r$n_following, sum(d == g))
  expect_error(morie_regime_value(y, d * 2, X, g), "binary")
  expect_error(morie_regime_value(y, d, X, g * 2), "0 or 1")
  expect_error(morie_regime_value(y[-1], d, X, g), "first dimension")
})

test_that("the horseshoe sampler is seeded and recovers a sparse signal", {
  set.seed(11)
  X <- matrix(stats::rnorm(60 * 5), 60)
  y <- as.numeric(X %*% c(3, 0, 0, -2, 0)) + stats::rnorm(60, sd = 0.5)
  a <- morie_bayesian_horseshoe(X, y, n_iter = 600, seed = 5)
  b <- morie_bayesian_horseshoe(X, y, n_iter = 600, seed = 5)
  expect_identical(a$beta_samples, b$beta_samples)
  expect_equal(a$posterior_mean, colMeans(a$beta_samples))
  # with n = 60 and noise sd 0.5 the posterior mean sits within 0.3 of the truth
  expect_lt(max(abs(a$posterior_mean - c(3, 0, 0, -2, 0))), 0.3)
  expect_identical(dim(a$beta_samples), c(600L, 5L))
})

test_that("the Kantorovich map is monotone and tracks the exact quantile map", {
  set.seed(2)
  src <- stats::rnorm(80)
  tgt <- stats::rexp(120)
  r <- morie_neural_kantorovich_map(src, tgt, n_iter = 400, seed = 1)
  ranks <- rank(src, ties.method = "first") - 1
  exact <- as.numeric(stats::quantile(tgt, (ranks + 0.5) / 80, type = 7))
  expect_equal(r$exact_map, exact, tolerance = 1e-15)
  expect_true(r$monotone)
  expect_equal(r$rmse_vs_exact, sqrt(mean((r$map_at_source - exact)^2)), tolerance = 1e-15)
  expect_equal(r$w2_exact, mean((exact - src)^2), tolerance = 1e-15)
  expect_error(morie_neural_kantorovich_map(1:4, 1:10), "at least 5")
  expect_error(morie_neural_kantorovich_map(src, tgt, n_basis = 1), "at least 2")
  expect_error(morie_neural_kantorovich_map(c(src, NA), tgt), "non-finite")
})
