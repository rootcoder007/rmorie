# Anchors for Brus (2022), Spatial Sampling with R.
#
# Every estimator here has a closed form in the book, so the assertions
# are those forms computed independently, not the module's own output.
# The kriging block additionally has an anchor the book does not print:
# the definitional variance
#
#   Var(Z(s0) - sum lam_i Z(s_i)) = sigma2 - 2 lam'c + lam' C lam
#
# which both of the module's variance expressions must equal. They did
# not: the semivariance form added the covariance system's Lagrange
# multiplier where the book's formula wants the semivariance system's,
# and those two are negatives of each other.

test_that("the Horvitz-Thompson estimators are equations 2.2 to 2.4", {
  z <- c(10, 20, 30); pi <- c(0.5, 0.25, 0.2); N <- 100
  h <- morie_ht_estimators(z, pi, N)
  expect_equal(h$total, sum(z / pi), tolerance = 1e-12)
  expect_equal(h$mean, sum(z / pi) / N, tolerance = 1e-12)
  expect_equal(h$weights, 1 / pi, tolerance = 1e-12)
  # a census, every unit certain, recovers the population total exactly
  cen <- morie_ht_estimators(c(1, 2, 3), c(1, 1, 1), 3)
  expect_equal(cen$total, 6, tolerance = 1e-12)
  expect_equal(cen$mean, 2, tolerance = 1e-12)
  # the design weights sum to the estimated population size
  expect_equal(sum(h$weights), sum(1 / pi), tolerance = 1e-12)
  expect_error(morie_ht_estimators(c(1, 2), c(0.5), 10))
  expect_error(morie_ht_estimators(c(1), c(0), 10))
  expect_error(morie_ht_estimators(c(1), c(1.5), 10))
  expect_error(morie_ht_estimators(c(1), c(0.5), 0))
})

test_that("the simple-random-sampling proportion carries its fpc", {
  y <- c(1, 0, 1, 1, 0, 1, 0, 1); n <- 8L; N <- 200L
  s <- morie_si_estimators(y = y, n = n, n_population = N)
  expect_equal(s$p_hat, mean(y), tolerance = 1e-12)
  expect_equal(s$var_p, (1 - n / N) * mean(y) * (1 - mean(y)) / (n - 1),
               tolerance = 1e-12)
  # a census leaves no sampling variance
  full <- morie_si_estimators(y = y, n = 8L, n_population = 8L)
  expect_equal(full$var_p, 0, tolerance = 1e-12)
  # a degenerate sample has no variance either
  allone <- morie_si_estimators(y = rep(1, 5), n = 5L, n_population = 100L)
  expect_equal(allone$p_hat, 1, tolerance = 1e-12)
  expect_equal(allone$var_p, 0, tolerance = 1e-12)
  # only 0/1 data are a proportion
  expect_error(morie_si_estimators(y = c(0, 1, 2)))
})

test_that("the confidence interval is the estimate plus or minus u times se", {
  s <- morie_si_estimators(estimate = 10, variance = 4, u_crit = 1.96)
  expect_equal(s$ci, c(10 - 1.96 * 2, 10 + 1.96 * 2), tolerance = 1e-12)
  # it is symmetric about the estimate and widens with the variance
  wide <- morie_si_estimators(estimate = 10, variance = 16, u_crit = 1.96)
  expect_equal(mean(wide$ci), 10, tolerance = 1e-12)
  expect_gt(diff(wide$ci), diff(s$ci))
})

test_that("the design-based total scales by the area ratio", {
  s <- morie_si_estimators(zbar_hat = 4, area = 1000, sample_area = 50,
                           s2_hat = 9, n = 25)
  expect_equal(s$total_inf, 1000 / 50 * 4, tolerance = 1e-12)
  expect_equal(s$var_total_inf, (1000 / 50)^2 * 9 / 25, tolerance = 1e-12)
})

test_that("the pps variance is the Hansen-Hurwitz estimator", {
  z <- c(12, 9, 15); p <- c(0.2, 0.3, 0.5)
  n <- length(z)
  t_hat <- sum(z / p) / n
  got <- morie_pps_variance(z, p, t_hat)
  # v = sum((z_k/p_k - t_hat)^2) / (n (n - 1)), from the sample
  want <- sum((z / p - t_hat)^2) / (n * (n - 1))
  expect_equal(as.numeric(got[[1]]), want, tolerance = 1e-12)
  # a design where every ratio is identical has no variance
  flat <- morie_pps_variance(c(2, 4), c(0.25, 0.5), 8)
  expect_equal(as.numeric(flat[[1]]), 0, tolerance = 1e-12)
})

# an exponential covariance on a line, used for the kriging anchors
krig_setup <- function(sites = c(0, 1, 2.5, 4), sigma2 = 2, phi = 1.5) {
  Cf <- function(h) sigma2 * exp(-h / phi)
  list(sites = sites, sigma2 = sigma2, Cf = Cf,
       C = outer(sites, sites, function(a, b) Cf(abs(a - b))))
}

test_that("the ordinary kriging weights satisfy the unbiasedness constraint", {
  k <- krig_setup()
  for (s0 in c(-1, 0.3, 1.8, 3.2, 7)) {
    c0v <- k$Cf(abs(k$sites - s0))
    r <- morie_kriging(cov_ss = k$C, cov_s0 = c0v)
    expect_length(r$lam, length(k$sites))
    # the weights must sum to one, which is what the multiplier enforces
    expect_equal(sum(r$lam), 1, tolerance = 1e-12)
    # and they solve the system they were derived from
    expect_equal(as.numeric(k$C %*% r$lam + r$nu), c0v, tolerance = 1e-10)
  }
})

test_that("kriging interpolates exactly at an observed site", {
  k <- krig_setup()
  for (i in seq_along(k$sites)) {
    c0v <- k$Cf(abs(k$sites - k$sites[i]))
    r <- morie_kriging(cov_ss = k$C, cov_s0 = c0v)
    want <- rep(0, length(k$sites)); want[i] <- 1
    expect_equal(r$lam, want, tolerance = 1e-9)
    # with no uncertainty left there
    v <- morie_kriging(lam = r$lam, cov_s0 = c0v, sigma2 = k$sigma2,
                       nu = r$nu)$v_ok_cov
    expect_equal(v, 0, tolerance = 1e-9)
  }
})

test_that("both kriging variance forms equal the definitional variance", {
  # the anchor that found the sign error: neither expression is allowed to
  # disagree with Var(Z(s0) - sum lam Z(s_i))
  for (cfg in list(krig_setup(), krig_setup(c(0, 0.5, 3, 5, 6), 1.4, 2.2))) {
    for (s0 in c(0.2, 1.8, 2.0, 4.4)) {
      c0v <- cfg$Cf(abs(cfg$sites - s0))
      r <- morie_kriging(cov_ss = cfg$C, cov_s0 = c0v)
      truth <- cfg$sigma2 - 2 * sum(r$lam * c0v) +
        as.numeric(t(r$lam) %*% cfg$C %*% r$lam)
      vc <- morie_kriging(lam = r$lam, cov_s0 = c0v, sigma2 = cfg$sigma2,
                          nu = r$nu)$v_ok_cov
      vg <- morie_kriging(lam = r$lam, gamma_s0 = cfg$sigma2 - c0v,
                          nu = r$nu)$v_ok_gamma
      expect_equal(vc, truth, tolerance = 1e-10)
      expect_equal(vg, truth, tolerance = 1e-10)
      expect_equal(vc, vg, tolerance = 1e-10)
      # and a variance is not negative
      expect_gte(truth, -1e-10)
    }
  }
})

test_that("the exponential semivariogram has a nugget and reaches its sill", {
  g <- morie_kriging(h = c(0, 1, 2, 100), c0 = 0.5, c1 = 1.5, phi = 2)$gamma_h
  # gamma(0) is zero by definition, whatever the nugget
  expect_identical(g[1], 0)
  expect_equal(g[2:3], 0.5 + 1.5 * (1 - exp(-c(1, 2) / 2)), tolerance = 1e-12)
  # far apart, it approaches c0 + c1
  expect_equal(g[4], 2, tolerance = 1e-6)
  # monotone increasing in the separation
  gg <- morie_kriging(h = seq(0, 10, by = 0.5), c0 = 0.2, c1 = 1,
                      phi = 3)$gamma_h
  expect_true(all(diff(gg) > 0))
  # 95 percent of the sill by three distance parameters, the book's prose
  three <- morie_kriging(h = 3 * 3, c0 = 0, c1 = 1, phi = 3)$gamma_h
  expect_equal(three, 1 - exp(-3), tolerance = 1e-12)
  expect_gt(three, 0.95)
  expect_error(morie_kriging(h = c(-1), c0 = 0, c1 = 1, phi = 1))
  expect_error(morie_kriging(h = c(1), c0 = 0, c1 = 1, phi = -1))
})

test_that("the Gaussian log-likelihood matches an independent computation", {
  k <- krig_setup()
  set.seed(1)
  z <- rnorm(length(k$sites)); mu <- rep(0.3, length(k$sites))
  ll <- morie_kriging(z = z, mu = mu, cov = k$C)$loglik
  d <- z - mu
  want <- -0.5 * (length(z) * log(2 * pi) + log(det(k$C)) +
                    as.numeric(t(d) %*% solve(k$C, d)))
  expect_equal(ll, want, tolerance = 1e-10)
  # at the mean the quadratic form drops out
  ll0 <- morie_kriging(z = mu, mu = mu, cov = k$C)$loglik
  expect_equal(ll0, -0.5 * (length(z) * log(2 * pi) + log(det(k$C))),
               tolerance = 1e-10)
  # a singular covariance is refused rather than returning nonsense
  bad <- matrix(1, 3, 3)
  expect_error(morie_kriging(z = c(1, 2, 3), mu = rep(0, 3), cov = bad))
})

test_that("calling with nothing returns nothing, rather than failing", {
  expect_length(morie_kriging(), 0L)
  expect_length(morie_si_estimators(), 0L)
})
