# Coverage for the causal-weighting shelf: stabilised treatment and
# censoring weights as products of per-time probabilities, AIPW (its
# influence function, SE and the IPW / plug-in companions), TMLE (the
# logistic fluctuation against stats::glm with an offset), the controlled
# direct effect against stats::lm with an interaction, and closed-form
# g-estimation of a linear structural nested mean model.

test_that("stabilised IPT and censoring weights are numerator / denominator products", {
  D <- rbind(c(0.8, 0.5), c(0.4, 0.9), c(0.6, 0.7))
  N <- rbind(c(0.7, 0.6), c(0.5, 0.8), c(0.6, 0.6))
  g <- Gstabwt(denominator_model = D, numerator_model = N, history = 1:3, treatment = 1:3)
  w <- apply(N, 1, prod) / apply(D, 1, prod)
  expect_equal(g$weights, w, tolerance = 1e-12)
  expect_equal(g$unstabilized, 1 / apply(D, 1, prod), tolerance = 1e-12)
  expect_equal(c(g$mean_weight, g$max_weight), c(mean(w), max(w)), tolerance = 1e-12)
  expect_equal(Gstabwt(denominator_model = D)$weights, 1 / apply(D, 1, prod), tolerance = 1e-12)
  expect_error(Gstabwt(), "denominator_model")
  expect_error(Gstabwt(denominator_model = D - 0.5), "must be positive")
  expect_error(Gstabwt(denominator_model = D, numerator_model = N[, 1]), "must match")
  expect_error(Gstabwt(denominator_model = D, history = 1:2), "one row per subject")
  expect_error(Gstabwt(denominator_model = D, treatment = 1:2), "one row per subject")
  s <- Stbciw(D, numerator = N)
  expect_equal(s$weights, w, tolerance = 1e-12)
  expect_error(Stbciw(D + 0.5), "\\(0, 1\\]")
  expect_error(Stbciw(D, H = 1:2), "one row per subject")
  expect_error(Stbciw(D, numerator = N[1, ]), "must match")
})

.dat <- function() {
  set.seed(9)
  n <- 60
  x <- stats::rnorm(n)
  e <- stats::plogis(0.4 * x)
  d <- stats::rbinom(n, 1, e)
  y <- stats::plogis(0.5 * d + 0.6 * x + stats::rnorm(n, sd = 0.3))
  list(x = x, e = e, d = d, y = y, m1 = stats::plogis(0.5 + 0.55 * x), m0 = stats::plogis(0.6 * x))
}

test_that("AIPW is the mean of its influence function", {
  z <- .dat()
  r <- Eaiprl(z$y, z$d, ml_outcome = list(z$m1, z$m0), ml_propensity = z$e)
  inf <- z$d * (z$y - z$m1) / z$e + z$m1 - (1 - z$d) * (z$y - z$m0) / (1 - z$e) - z$m0
  expect_equal(r$influence, inf, tolerance = 1e-12)
  expect_equal(r$estimate, mean(inf), tolerance = 1e-12)
  expect_equal(r$se, sqrt(sum((inf - mean(inf))^2)) / 60, tolerance = 1e-12)
  expect_equal(r$ci_upper - r$estimate, stats::qnorm(0.975) * r$se, tolerance = 1e-12)
  expect_equal(r$ipw, mean(z$d * z$y / z$e - (1 - z$d) * z$y / (1 - z$e)), tolerance = 1e-12)
  expect_equal(r$plugin, mean(z$m1 - z$m0), tolerance = 1e-12)
  expect_error(Eaiprl(z$y, z$d), "required")
  expect_error(Eaiprl(z$y, z$d[-1], ml_outcome = list(z$m1, z$m0), ml_propensity = z$e), "same length")
  expect_error(Eaiprl(z$y, z$d, ml_outcome = list(z$m1, z$m0), ml_propensity = z$d), "strictly in \\(0, 1\\)")
})

test_that("TMLE fluctuates Q along the clever covariates like glm with an offset", {
  z <- .dat()
  t <- Caustmle(z$y, z$d, z$e, z$m1, z$m0)
  h1 <- z$d / z$e
  h0 <- (1 - z$d) / (1 - z$e)
  off <- stats::qlogis(ifelse(z$d == 1, z$m1, z$m0))
  f <- suppressWarnings(stats::glm(z$y ~ 0 + h0 + h1 + offset(off), family = stats::binomial(),
                                   control = stats::glm.control(epsilon = 1e-14, maxit = 100)))
  expect_equal(t$epsilon, unname(stats::coef(f)), tolerance = 1e-9)
  q1 <- stats::plogis(stats::qlogis(z$m1) + t$epsilon[2] / z$e)
  q0 <- stats::plogis(stats::qlogis(z$m0) + t$epsilon[1] / (1 - z$e))
  expect_equal(t$estimate, mean(q1 - q0), tolerance = 1e-12)
  ic <- h1 * (z$y - q1) - h0 * (z$y - q0) + q1 - q0 - mean(q1 - q0)
  expect_equal(t$se, sqrt(sum(ic^2)) / 60, tolerance = 1e-12)
  # the targeting step solves the efficient score equations
  expect_equal(c(sum(h0 * (z$y - q0) * (1 - z$d)), sum(h1 * (z$y - q1) * z$d)), c(0, 0), tolerance = 1e-8)
  expect_error(Caustmle(z$y + 1, z$d, z$e, z$m1, z$m0), "bounded in \\[0, 1\\]")
  expect_error(Caustmle(z$y, z$d, z$d, z$m1, z$m0), "strictly in \\(0, 1\\)")
  expect_error(Caustmle(z$y[-1], z$d, z$e, z$m1, z$m0), "same length")
})

test_that("CDE is beta_x + beta_xm m with its delta-method SE", {
  set.seed(1)
  X <- stats::rnorm(30)
  M <- stats::rnorm(30)
  Y <- 1 + 0.5 * X - 0.3 * M + 0.2 * X * M + stats::rnorm(30, sd = 0.4)
  r <- Cde(Y, X, M, m = 1.5)
  f <- stats::lm(Y ~ X * M)
  b <- stats::coef(f)
  V <- stats::vcov(f)
  expect_equal(r$cde, unname(b["X"] + 1.5 * b["X:M"]), tolerance = 1e-10)
  expect_equal(r$se, sqrt(V["X", "X"] + 2.25 * V["X:M", "X:M"] + 3 * V["X", "X:M"]), tolerance = 1e-10)
  expect_equal(c(r$intercept, r$beta_m), unname(b[c(1, 3)]), tolerance = 1e-10)
  expect_error(Cde(1:4, 1:4, 1:4, 0), "at least five")
  expect_error(Cde(1:6, 1:5, 1:6, 0), "same length")
})

test_that("SNMM g-estimation solves sum (a - E a)(y - psi a) = 0", {
  set.seed(2)
  A <- matrix(stats::rbinom(40, 1, 0.5), 20)
  a <- rowSums(A)
  y <- 2 + 0.7 * a + stats::rnorm(20)
  s <- Snmlin(y, A)
  r <- a - mean(a)
  expect_equal(s$psi, sum(r * y) / sum(r * a), tolerance = 1e-12)
  expect_equal(sum(r * (y - s$psi * a)), 0, tolerance = 1e-10)
  expect_equal(s$ols_slope, unname(stats::coef(stats::lm(y ~ a))[2]), tolerance = 1e-10)
  expect_equal(s$se, sqrt(sum((r * (y - s$psi * a))^2)) / abs(sum(r * a)), tolerance = 1e-12)
  p <- Snmlin(y, A, propensity = rep(1, 20), time = c(5, 10))
  expect_equal(p$psi, sum((a - 1) * y) / sum((a - 1) * a), tolerance = 1e-12)
  expect_identical(p$times, c(5, 10))
  expect_error(Snmlin(y, A, time = 1:3), "label the columns")
  expect_error(Snmlin(y, A, propensity = 1:3), "length n")
  expect_error(Snmlin(y, matrix(1, 20, 2)), "no residual treatment variation")
})
