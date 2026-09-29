bm_se2 <- function(x, b = 40) {
  m <- matrix(x[seq_len(length(x) %/% b * b)], ncol = b)
  stats::sd(colMeans(m)) / sqrt(b)
}

test_that("Bayesian samplers reproduce posteriors computed by numerical integration", {
  z <- .morie_random_normal(60, seed = 51)
  y <- 2 + 0.8 * z[1:12]
  # half-Cauchy regression with an intercept only: 2-D grid over (beta, sigma)
  bg <- seq(0, 4, by = 0.01)
  sg <- seq(0.05, 4, by = 0.01)
  lp <- outer(bg, sg, function(b, s) {
    vapply(seq_along(b), function(i) sum(stats::dnorm(y, b[i], s[i], log = TRUE)), 0) +
      stats::dnorm(b, 0, sqrt(10), log = TRUE) - log(1 + (s / 2)^2)
  })
  w <- exp(lp - max(lp))
  w <- w / sum(w)
  o <- BayesLinearHalfcauchy(y, matrix(1, 12, 1), prior_var = 10, scale = 2, ndraw = 20000, burn_in = 500, seed = 1)
  expect_lt(abs(o$beta - sum(w * bg)) / bm_se2(o$draws[, 1]), 5)
  expect_lt(abs(o$sigma2 - sum(t(t(w) * sg^2))) / bm_se2(o$draws[, 2]), 5)
  # contaminated normal: grid over (mu, sigma) with p(mu, sigma^2) proportional to 1 / sigma^2
  yo <- c(z[13:24], 5.5)
  mg <- seq(-2, 2, by = 0.01)
  sg2 <- seq(0.1, 4, by = 0.01)
  mix <- function(m, s) sum(log(0.95 * stats::dnorm(yo, m, s) + 0.05 * stats::dnorm(yo, m, 5 * s))) - log(s)
  lp2 <- outer(mg, sg2, Vectorize(mix))
  w2 <- exp(lp2 - max(lp2))
  w2 <- w2 / sum(w2)
  pout <- sum(w2 * outer(mg, sg2, Vectorize(function(m, s) {
    b <- 0.05 * stats::dnorm(5.5, m, 5 * s)
    b / (b + 0.95 * stats::dnorm(5.5, m, s))
  })))
  co <- ContaminatedNormalOutliers(yo, ndraw = 20000, burn_in = 500, seed = 2)
  expect_equal(co$outlier_prob[13], pout, tolerance = 0.02)
  expect_equal(co$mu, sum(w2 * mg), tolerance = 0.02)
})

test_that("BayesA and BayesB with one marker equal the one-dimensional exact posterior", {
  z <- .morie_random_normal(60, seed = 52)
  m <- round(1 + z[1:15])
  y <- 1 + 0.6 * m + 0.7 * z[16:30]
  n <- 15
  nu <- 4.012
  S2 <- 0.3
  ug <- seq(-4, 4, by = 0.0005)
  ss <- vapply(ug, function(u) {
    r <- y - m * u
    sum((r - mean(r))^2)
  }, 0)
  lt <- -(nu + 1) / 2 * log(1 + ug^2 / (nu * S2)) + lgamma((nu + 1) / 2) - lgamma(nu / 2) - 0.5 * log(nu * pi * S2)
  lik <- -(n - 3) / 2 * log(ss)
  mx <- max(lt + lik)
  wa <- exp(lt + lik - mx)
  a <- BayesA(y, matrix(m), nu = nu, s2 = S2, ndraw = 20000, burn_in = 500, seed = 3)
  expect_equal(a$effects, sum(wa * ug) / sum(wa), tolerance = 0.02)
  i1 <- sum(wa) * 0.0005
  r0 <- y - mean(y)
  i0 <- exp(-(n - 3) / 2 * log(sum(r0^2)) - mx)
  pi0 <- 0.6
  b <- BayesB(y, matrix(m), pi = pi0, nu = nu, s2 = S2, ndraw = 20000, burn_in = 500, seed = 4)
  expect_equal(b$inclusion, (1 - pi0) * i1 / ((1 - pi0) * i1 + pi0 * i0), tolerance = 0.03)
})
