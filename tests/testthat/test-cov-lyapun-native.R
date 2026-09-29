# Largest Lyapunov exponent (Rosenstein, Collins & De Luca 1993): delay
# embedding, the 1 - 1/e autocorrelation delay, the spectral mean
# period, and the exponent of the logistic map at mu = 4 (ln 2).

ly_logistic <- function(n, x0 = 0.1234) {
  x <- numeric(n)
  x[1] <- x0
  for (i in 2:n) x[i] <- 4 * x[i - 1] * (1 - x[i - 1])
  x
}

test_that("delay embedding stacks y_j, y_{j+tau}, ...", {
  y <- as.numeric(1:12)
  e <- morie_lyapun_embed(y, 3, 2)
  expect_length(e, 8L)
  expect_equal(e[[1]], c(1, 3, 5))
  expect_equal(e[[8]], c(8, 10, 12))
  expect_error(morie_lyapun_embed(y, 0, 1), "embedding dimension")
  expect_error(morie_lyapun_embed(y, 2, 0), "delay")
  expect_error(morie_lyapun_embed(y, 6, 2), "leave only 2")
  expect_error(morie_lyapun_embed(1:5, 2, 1), "at least 10")
  expect_error(morie_lyapun_embed(c(1:10, NA), 2, 1), "non-finite")
})

test_that("the delay is the first lag with autocorrelation below 1 - 1/e", {
  y <- sin(seq(0, 20, by = 0.2))
  ac <- stats::acf(y, lag.max = 40, plot = FALSE)$acf[-1]
  expect_identical(autocorrelation_lag(y), which(ac <= 1 - exp(-1))[1])
  expect_identical(autocorrelation_lag(y, threshold = 0), which(ac <= 0)[1])
  expect_error(autocorrelation_lag(rep(1, 12)), "constant")
})

test_that("the mean period inverts the power-weighted mean frequency", {
  n <- 64
  y <- sin(2 * pi * (0:(n - 1)) / 8)
  P <- Mod(stats::fft(y - mean(y)))^2
  f <- (0:(n / 2)) / n
  fm <- sum(f[-1] * P[2:(n / 2 + 1)]) / sum(P[2:(n / 2 + 1)])
  expect_equal(mean_period(y), 1 / fm, tolerance = 1e-12)
  expect_equal(mean_period(y), 8, tolerance = 1e-9)
  # the period is returned in SAMPLES, so the sampling interval cancels
  expect_equal(mean_period(y, dt = 0.5), 1 / fm, tolerance = 1e-12)
})

test_that("Rosenstein's slope recovers ln 2 for the logistic map", {
  y <- ly_logistic(600)
  r <- lyapunov_exponent(y, embedding = 2, tau = 1, min_sep = 10, max_steps = 12)
  lo <- r$fit_range[1]
  hi <- r$fit_range[2]
  fit <- stats::lm(r$log_divergence[lo:hi] ~ r$time[lo:hi])
  expect_equal(r$rosenstein, unname(stats::coef(fit)[2]), tolerance = 1e-10)
  # the paper's Table 1 value is 0.693; with 600 points the estimate lies
  # within 0.1 of it
  expect_lt(abs(r$estimate - log(2)), 0.1)
  s <- lyapunov_exponent(y, 2, 1, min_sep = 10, max_steps = 12, method = "sato")
  expect_equal(s$estimate, s$sato)
  expect_equal(largest_lyapunov(y, 2, 1, min_sep = 10, max_steps = 12)$estimate, r$estimate)
  expect_equal(morie_lyapun("lyapunov_exponent", y, 2, 1, min_sep = 10, max_steps = 12)$estimate,
               r$estimate)
  expect_error(lyapunov_exponent(y, 2, 1, method = "wolf"), "method must be")
  expect_error(lyapunov_exponent(y, 2, 1, min_sep = 2000), "leaves no admissible")
  expect_error(morie_lyapun("nope"), "unknown op")
})
