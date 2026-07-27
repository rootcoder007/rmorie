
# ---------------------------------------------------------------------------
# GARCH family -- parameter recovery
#
# These fit simulated series whose true parameters are known. A test that
# only checks the statistic is finite passes on an optimiser that never
# moves off its starting values, which is exactly the failure the Python
# sibling had while its wrapper branch masked it.
# ---------------------------------------------------------------------------

test_that("EGARCH recovers size and sign effects from a simulated series", {
  set.seed(7)
  n <- 1500
  om <- -0.1; sz <- 0.25; sg <- -0.15; be <- 0.95
  EZ <- sqrt(2 / pi)
  ls2 <- numeric(n); r <- numeric(n)
  for (t in 2:n) {
    z <- rnorm(1)
    r[t - 1] <- z * sqrt(exp(ls2[t - 1]))
    ls2[t] <- om + sz * (abs(z) - EZ) + sg * z + be * ls2[t - 1]
  }
  r[n] <- rnorm(1) * sqrt(exp(ls2[n]))
  f <- morie_egarch_model(r)
  # Relative tolerances. QMLE at these sample sizes carries real sampling
  # error -- R and Python independently land on the same estimates, so the
  # gap to truth is the estimator's, not the implementation's.
  expect_equal(f$alpha, sz, tolerance = 0.25)   # size effect
  expect_equal(f$gamma, sg, tolerance = 0.25)   # sign (leverage) effect
  expect_equal(f$beta, be, tolerance = 0.05)
  expect_lt(f$gamma, 0)                          # leverage is negative here
  expect_false(isTRUE(all.equal(c(f$omega, f$alpha, f$gamma, f$beta),
                                c(0, 0.1, 0, 0.9))))  # not the start values
})

test_that("GARCH(1,1) recovers persistence from a simulated series", {
  set.seed(13)
  n <- 3000
  om <- 0.02; al <- 0.08; be <- 0.88
  s2 <- numeric(n); e <- numeric(n); s2[1] <- om / (1 - al - be)
  for (t in 2:n) {
    e[t - 1] <- rnorm(1) * sqrt(s2[t - 1])
    s2[t] <- om + al * e[t - 1]^2 + be * s2[t - 1]
  }
  e[n] <- rnorm(1) * sqrt(s2[n])
  g <- morie_garch_fit(e)
  expect_equal(g$persistence, al + be, tolerance = 0.08)
  expect_true(g$persistence < 1)
  expect_equal(length(g$conditional_variance), n)
})

test_that("GJR-GARCH recovers a positive leverage term", {
  set.seed(11)
  n <- 3000
  om <- 0.02; al <- 0.03; ga <- 0.12; be <- 0.85
  s2 <- numeric(n); e <- numeric(n)
  s2[1] <- om / (1 - al - 0.5 * ga - be)
  for (t in 2:n) {
    e[t - 1] <- rnorm(1) * sqrt(s2[t - 1])
    I <- if (e[t - 1] <= 0) 1 else 0
    s2[t] <- om + (al + ga * I) * e[t - 1]^2 + be * s2[t - 1]
  }
  e[n] <- rnorm(1) * sqrt(s2[n])
  h <- morie_tgarch_model(e)
  expect_gt(h$gamma, 0)
  expect_equal(h$gamma, ga, tolerance = 0.3)
  # Persistence uses kappa = 0.5, the Gaussian probability of a negative
  # standardised residual.
  expect_equal(h$persistence, h$alpha + 0.5 * h$gamma + h$beta, tolerance = 1e-12)
})

test_that("DCC tracks a correlation regime shift", {
  set.seed(21)
  n <- 800
  rho <- c(rep(0.1, n / 2), rep(0.85, n / 2))
  X <- matrix(0, n, 2)
  for (t in seq_len(n)) {
    z1 <- rnorm(1)
    X[t, ] <- c(z1, rho[t] * z1 + sqrt(1 - rho[t]^2) * rnorm(1))
  }
  d <- morie_dcc_multivariate_garch(X)
  cr <- d$conditional_correlation
  expect_lt(mean(cr[1:(n / 2), 1, 2]), mean(cr[(n / 2 + 1):n, 1, 2]))
  expect_gt(mean(cr[(n / 2 + 1):n, 1, 2]), 0.6)
  expect_true(d$a + d$b < 1.0001)
})
