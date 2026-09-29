# Coverage for the k05 tranche: sample ACF/PACF (against stats::acf and
# stats::pacf), the Kupiec and Christoffersen VaR backtests (likelihood
# ratios rebuilt from the transition counts), the one-way ANOVA ICC with
# the Kish design effect, the BNS (2006) jump test and Alexandersson's SNHT.

test_that("sample ACF and PACF equal stats::acf and stats::pacf", {
  set.seed(4)
  y <- as.numeric(stats::arima.sim(list(ar = c(0.5, -0.3)), 60))
  a <- morie_sample_acf(y, max_lag = 8)
  ref <- stats::acf(y, lag.max = 8, plot = FALSE)
  expect_equal(a$acf, as.numeric(ref$acf), tolerance = 1e-12)
  expect_equal(a$acvf, as.numeric(stats::acf(y, 8, type = "covariance", plot = FALSE)$acf), tolerance = 1e-12)
  expect_identical(a$lags, 0:8)
  expect_equal(a$ci_bound, 1.96 / sqrt(60))
  p <- morie_sample_pacf(y, max_lag = 8)
  expect_equal(p$pacf, as.numeric(stats::pacf(y, lag.max = 8, plot = FALSE)$acf), tolerance = 1e-12)
  expect_identical(morie_sample_acf(1:4, max_lag = 10)$max_lag, 3L)
  expect_error(morie_sample_acf(rep(2, 5)), "constant")
  expect_error(morie_sample_pacf(1:2), "at least 3")
  expect_error(morie_sample_acf(1:5, max_lag = 0), "at least 1")
})

.lr_uc <- function(p, t, n) {
  ph <- n / t
  -2 * ((t - n) * log(1 - p) + n * log(p) - (t - n) * log(1 - ph) - n * log(ph))
}

test_that("Kupiec and Christoffersen likelihood ratios", {
  h <- c(0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1)
  k <- morie_kupiec_var_test(h, alpha = 0.1)
  expect_equal(k$statistic, .lr_uc(0.1, 20, 6), tolerance = 1e-12)
  expect_equal(k$pvalue, stats::pchisq(k$statistic, 1, lower.tail = FALSE), tolerance = 1e-12)
  expect_equal(c(k$expected_exceedances, k$rate), c(2, 0.3))
  # zero exceedances: LR = -2 T log(1 - p)
  expect_equal(morie_kupiec_var_test(rep(0, 10), 0.05)$statistic, -20 * log(0.95), tolerance = 1e-12)
  cc <- morie_christoffersen_cc(h, alpha = 0.1)
  a <- h[-20]
  b <- h[-1]
  n <- c(sum(!a & !b), sum(!a & b), sum(a & !b), sum(a & b))
  expect_identical(c(cc$n00, cc$n01, cc$n10, cc$n11), as.integer(n))
  p0 <- n[2] / (n[1] + n[2])
  p1 <- n[4] / (n[3] + n[4])
  pp <- (n[2] + n[4]) / 19
  ind <- -2 * ((n[1] + n[3]) * log(1 - pp) + (n[2] + n[4]) * log(pp) -
                 n[1] * log(1 - p0) - n[2] * log(p0) - n[3] * log(1 - p1) - n[4] * log(p1))
  expect_equal(cc$lr_ind, ind, tolerance = 1e-12)
  expect_equal(cc$lr_uc, k$statistic, tolerance = 1e-12)
  expect_equal(cc$statistic, k$statistic + ind, tolerance = 1e-12)
  expect_equal(cc$pvalue, stats::pchisq(cc$statistic, 2, lower.tail = FALSE), tolerance = 1e-12)
  vb <- morie_var_backtest(h, alpha = 0.1)
  expect_identical(vb$lr_cc, cc$statistic)
  expect_identical(vb$pvalue_cc, cc$pvalue)
  expect_error(morie_kupiec_var_test(c(0, 2)), "0/1")
  expect_error(morie_kupiec_var_test(1), "at least 2")
  expect_error(morie_christoffersen_cc(h, alpha = 1), "strictly between")
})

test_that("ICC is (MSB - MSW)/n0 over (that + MSW), with the Kish deff", {
  y <- c(4, 5, 6, 5, 9, 8, 10, 2, 3, 1, 2, 7)
  g <- c(1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3)
  r <- morie_icc_rho(y, g)
  tab <- stats::anova(stats::lm(y ~ factor(g)))
  msb <- tab[["Mean Sq"]][1]
  msw <- tab[["Mean Sq"]][2]
  ni <- c(4, 3, 5)
  n0 <- (12 - sum(ni^2) / 12) / 2
  rho <- ((msb - msw) / n0) / ((msb - msw) / n0 + msw)
  expect_equal(c(r$msb, r$msw, r$n0), c(msb, msw, n0), tolerance = 1e-12)
  expect_equal(r$rho, rho, tolerance = 1e-12)
  expect_equal(r$deff, 1 + (4 - 1) * rho, tolerance = 1e-12)
  expect_equal(r$effective_n, 12 / r$deff, tolerance = 1e-12)
  expect_error(morie_icc_rho(1:3, c(1, 1, 1)), "at least 2 clusters")
  expect_error(morie_icc_rho(1:2, 1:2), "more observations")
  expect_error(morie_icc_rho(c(1, 1, 1, 1), c(1, 1, 2, 2)), "zero total variance")
})

.bns <- function(r) {
  n <- length(r)
  a <- abs(r)
  mu <- sqrt(2 / pi)
  mu43 <- 2^(2 / 3) * gamma(7 / 6) / gamma(1 / 2)
  rv <- sum(r^2)
  bv <- mu^-2 * sum(a[-1] * a[-n])
  tq <- n * mu43^-3 * n / (n - 2) * sum((a[3:n] * a[2:(n - 1)] * a[1:(n - 2)])^(4 / 3))
  (rv - bv) / sqrt((pi^2 / 4 + pi - 5) * tq / n)
}

test_that("BNS jump statistic, pooled and per block", {
  set.seed(8)
  r <- stats::rnorm(40, sd = 0.01)
  r[17] <- 0.08
  b <- morie_bns_jump_test(r)
  expect_equal(b$statistic, .bns(r), tolerance = 1e-12)
  expect_equal(b$pvalue, stats::pnorm(.bns(r), lower.tail = FALSE), tolerance = 1e-12)
  expect_equal(b$jump_component, b$rv - b$bpv, tolerance = 1e-12)
  d <- rep(c("a", "b"), each = 20)
  bb <- morie_bns_jump_test(r, block_index = d)
  z <- c(.bns(r[1:20]), .bns(r[21:40]))
  expect_equal(bb$z, z, tolerance = 1e-12)
  expect_equal(bb$statistic, max(z), tolerance = 1e-12)
  expect_identical(bb$days, c("a", "b"))
  expect_error(morie_bns_jump_test(r[1:3]), "at least 4")
  expect_error(morie_bns_jump_test(r, block_index = 1:3), "one entry per return")
  expect_error(morie_bns_jump_test(r[1:6], block_index = c(1, 1, 1, 2, 2, 2)), "at least 4 returns")
})

test_that("SNHT T_k = k z1^2 + (n - k) z2^2 and its Monte Carlo p-value", {
  x <- c(0.3, -0.1, 0.4, 0.0, 0.2, 2.1, 2.5, 1.9, 2.3, 2.0)
  s <- morie_snht(x, n_mc = 0)
  z <- (x - mean(x)) / stats::sd(x)
  tk <- vapply(1:9, function(k) k * mean(z[1:k])^2 + (10 - k) * mean(z[(k + 1):10])^2, 1)
  expect_equal(s$tk, tk, tolerance = 1e-12)
  expect_equal(s$statistic, max(tk), tolerance = 1e-12)
  expect_identical(s$change_point, 5L)
  expect_true(is.na(s$pvalue))
  m <- morie_snht(x, n_mc = 49, seed = 3)
  expect_equal(m$statistic, s$statistic)
  # p = (#{T* >= T} + 1)/(n_mc + 1) lies on the 1/50 grid
  expect_equal(m$pvalue * 50, round(m$pvalue * 50), tolerance = 1e-12)
  expect_lte(m$pvalue, 0.1)
  expect_identical(morie_snht(x, n_mc = 49, seed = 3)$pvalue, m$pvalue)
  expect_error(morie_snht(rep(1, 5)), "constant")
  expect_error(morie_snht(1:2), "at least 3")
})
