# Coverage tests for R/tmldyk_native.R: the Laplace mechanism applied to
# a TMLE of the ATE (Dwork et al. 2006; Niu et al. 2022). Noise draws are
# reproduced from the package's documented SplitMix64 uniform stream; the
# TMLE fluctuation is recomputed with glm.

dyk_lap <- function(b, seed) {
  u <- .ghc_unif(.ghc_rng(seed), 1L) - 0.5
  -b * sign(u) * log(1 - 2 * abs(u))
}

test_that("ATE sensitivity and composition", {
  for (f in list(ate_sensitivity, morie_ate_sensitivity)) {
    s <- f(200, 0.05, y_range = 2)
    expect_equal(s$sensitivity, 2 * 2 / (200 * 0.05))
    expect_equal(s$inflation, 40)
    expect_equal(s$naive_1_over_n, 0.01)
    expect_error(f(0, 0.1), "at least 1")
    expect_error(f(10, 0.6), "\\(0, 0.5\\]")
  }
  for (f in list(composition_budget, morie_composition_budget)) {
    expect_equal(f(c(0.5, 0.25, 1))$total_epsilon, 1.75)
    expect_equal(f(c(0.5, 0.25, 1))$n_releases, 3L)
    expect_error(f(c(1, 0)), "positive")
  }
})

test_that("Laplace noise is the inverse-CDF transform of one uniform draw", {
  for (f in list(laplace_noise, morie_laplace_noise)) {
    expect_equal(f(0.7, .ghc_rng(11)), dyk_lap(0.7, 11), tolerance = 1e-12)
    expect_error(f(0, .ghc_rng(1)), "positive")
  }
  # the transform is the Laplace(0, b) quantile function at u + 1/2
  u <- .ghc_unif(.ghc_rng(11), 1L)
  b <- 0.7
  q <- if (u < 0.5) b * log(2 * u) else -b * log(2 - 2 * u)
  expect_equal(dyk_lap(b, 11), q, tolerance = 1e-12)
})

test_that("private release and interval add Lap(sens/eps) and widen the se", {
  for (pair in list(list(private_release, private_ci), list(morie_private_release, morie_private_ci))) {
    r <- pair[[1]](0.3, sensitivity = 0.02, epsilon = 0.5, seed = 7)
    b <- 0.04
    expect_equal(r$scale, b)
    expect_equal(r$noise, dyk_lap(b, 7), tolerance = 1e-12)
    expect_equal(r$released, 0.3 + r$noise, tolerance = 1e-12)
    expect_equal(r$noise_variance, 2 * b^2)
    ci <- pair[[2]](0.3, 0.02, 0.5, se = 0.03, seed = 7)
    sp <- sqrt(0.03^2 + 2 * b^2)
    expect_equal(ci$se_private, sp, tolerance = 1e-12)
    expect_equal(ci$ci, r$released + c(-1, 1) * 1.96 * sp, tolerance = 1e-12)
    expect_equal(ci$width_ratio, sp / 0.03, tolerance = 1e-12)
    expect_true(is.nan(pair[[2]](0.3, 0.02, 0.5, se = 0, seed = 7)$width_ratio))
    expect_error(pair[[1]](0.3, 0.02, 0), "epsilon")
    expect_error(pair[[1]](0.3, 0, 1), "sensitivity")
  }
})

test_that("private TMLE: fluctuated estimate plus Laplace noise", {
  n <- 30
  i <- 1:n
  w1 <- ((i * 7) %% 11) / 11
  a <- as.numeric(((i * 3) %% 5) < 2)
  y <- pmin(pmax(0.2 + 0.3 * a + 0.4 * w1 + ((i %% 7) - 3) / 20, 0), 1)
  g <- 0.3 + 0.4 * w1
  Q1 <- pmin(pmax(0.5 + 0.4 * w1, 0.05), 0.95)
  Q0 <- pmin(pmax(0.2 + 0.4 * w1, 0.05), 0.95)
  H <- a / g - (1 - a) / (1 - g)
  off <- qlogis(ifelse(a == 1, Q1, Q0))
  fl <- suppressWarnings(glm(y ~ -1 + H + offset(off), family = quasibinomial(),
    control = glm.control(epsilon = 1e-14, maxit = 100)))
  e <- unname(coef(fl))
  psi <- mean(plogis(qlogis(Q1) + e / g) - plogis(qlogis(Q0) - e / (1 - g)))
  r <- morie_tmle_diff_kernel(y, a, cbind(w1), epsilon = 2, g_min = 0.1, seed = 3, g = g, Q1 = Q1, Q0 = Q0)
  expect_equal(r$non_private_psi, psi, tolerance = 1e-9)
  sens <- 2 / (n * 0.1)
  expect_equal(r$sensitivity, sens)
  expect_equal(r$estimate, r$non_private_psi + dyk_lap(sens / 2, 3), tolerance = 1e-12)
  expect_equal(r$se_private, sqrt(r$se_sampling^2 + 2 * (sens / 2)^2), tolerance = 1e-12)
  r2 <- morie_tmldyk(y, a, cbind(w1), epsilon = 2, g_min = 0.1, seed = 3, g = g, Q1 = Q1, Q0 = Q0)
  expect_equal(r2$non_private_psi, psi, tolerance = 1e-9)
  expect_identical(morie_tmlediffkernel, morie_tmldyk)
  expect_error(morie_tmle_diff_kernel(y + 1, a, cbind(w1)), "\\[0,1\\]")
  expect_error(morie_tmldyk(y, a[-1], cbind(w1)), "differ in length")
  expect_match(morie_tmldyk_cheatsheet(), "LAPLACE")
})
