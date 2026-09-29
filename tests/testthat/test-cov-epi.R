# Coverage for epidemiology and expected-prediction-error helpers
# (epbias.R, epicur.R, epimet.R, Epeols.R, Epetheor.R, finalsz.R,
# contam.R, Expmc.R, expmc_native.R, EpiSurveillance.R BufferExposure):
# closed forms are recomputed, root-finders checked against uniroot,
# regressions against lm.

test_that("Epbias inverts non-differential exposure misclassification", {
  r <- Epbias(c(40, 25), Se = 0.9, Sp = 0.95, N = c(100, 120))
  at <- (c(40, 25) - 0.05 * c(100, 120)) / 0.85
  expect_equal(r$a_true, at, tolerance = 1e-12)
  expect_equal(r$or_true, (at[1] * (120 - at[2])) / (at[2] * (100 - at[1])), tolerance = 1e-12)
  expect_equal(r$or_obs, (40 * 95) / (25 * 60), tolerance = 1e-12)
  two <- Epbias(c(30, 70), Se = 0.8, Sp = 0.9)
  expect_equal(two$a_true, (30 - 0.1 * 100) / 0.7, tolerance = 1e-12)
  expect_error(Epbias(c(1, 2, 3), 0.9, 0.9), "N is required")
  expect_error(Epbias(1, 0.9, 0.9, N = 1:2), "same length")
  expect_error(Epbias(c(1, 2), c(0.9, 0.9, 0.9), 0.9, N = c(5, 5)), "one value per group")
  expect_error(Epbias(1, 0, 0.9, N = 5), "\\(0, 1\\]")
  expect_error(Epbias(1, 0.4, 0.5, N = 5), "exceed 1")
  expect_error(Epbias(1, 0.9, 0.9, N = 0), "positive")
  expect_error(Epbias(9, 0.9, 0.9, N = 5), "between 0")
})

test_that("Epicur is robust local-linear (lowess-type) smoothing", {
  x <- 1:12
  y <- c(2, 4, 7, 12, 20, 35, 30, 22, 15, 60, 6, 3)
  r <- Epicur(x, y, bandwidth = 4, iterations = 2)
  tri <- function(u) ifelse(abs(u) < 1, (1 - abs(u)^3)^3, 0)
  bis <- function(u) ifelse(abs(u) < 1, (1 - u^2)^2, 0)
  delta <- rep(1, 12)
  for (pass in 1:3) {
    fit <- vapply(1:12, function(k) {
      w <- tri((x - x[k]) / 4) * delta
      unname(coef(lm(y ~ I(x - x[k]), weights = w))[1])
    }, 0)
    r0 <- y - fit
    s <- median(abs(r0))
    delta <- bis(r0 / (6 * s))
  }
  expect_equal(r$fitted, fit, tolerance = 1e-9)
  expect_equal(r$peak_date, x[which.max(fit)])
  expect_error(Epicur(numeric(0), numeric(0), 1), "empty")
  expect_error(Epicur(x, y[-1], 1), "same length")
  expect_error(Epicur(x, y, 0), "positive")
  expect_error(Epicur(x, y, 2, iterations = -1), "non-negative")
})

test_that("Rtrenew divides incidence by the renewal force of infection", {
  inc <- c(2, 3, 5, 8, 12, 18, 25, 30, 33, 31)
  w <- c(0.2, 0.5, 0.3)
  r <- Rtrenew(inc, w * 10)
  rt <- vapply(4:10, function(t) inc[t] / sum(w * inc[(t - 1):(t - 3)]), 0)
  expect_equal(r$rt, rt, tolerance = 1e-12)
  expect_equal(r$time, 3:9)
  d <- Rtrenew(inc, w, delays = c(0, 0.5, 0.5))
  expect_equal(d$shift, 2L)
  expect_equal(d$infections, inc[-(1:2)])
  expect_error(Rtrenew(inc, c(0, 0)), "positive mass")
  expect_error(Rtrenew(inc, w, delays = c(0, 0)), "positive mass")
})

test_that("Epeols and Epetheor: expected prediction error of least squares", {
  X <- cbind(1, c(0.5, 1.2, -0.3, 2.1, 1.7, 0.1, 2.8, 0.9))
  y <- c(1.9, 3.1, 0.2, 5.0, 4.6, 1.3, 6.4, 2.9)
  x0 <- c(1, 1.5)
  r <- Epeols(X, y, x0)
  f <- lm(y ~ X - 1)
  s2 <- sum(resid(f)^2) / 6
  lev <- drop(t(x0) %*% solve(crossprod(X), x0))
  expect_equal(r$sigma2, s2, tolerance = 1e-10)
  expect_equal(r$epe, s2 + lev * s2, tolerance = 1e-10)
  expect_equal(r$approx, s2 * (2 / 8 + 1), tolerance = 1e-10)
  expect_equal(Epeols(X, y, x0, sigma2 = 2)$variance, 2 * lev, tolerance = 1e-10)
  expect_error(Epeols(X[0, ], y[0], x0), "empty")
  expect_error(Epeols(X, y[-1], x0), "same number of rows")
  expect_error(Epeols(X, y, 1), "one entry per column")
  expect_error(Epeols(X[1:2, ], y[1:2], x0), "N > p")
  expect_error(Epeols(X, y, x0, sigma2 = -1), "non-negative")
  t <- Epetheor(X, y)
  expect_equal(t$beta, unname(coef(f)), tolerance = 1e-10)
  expect_equal(t$epe, mean(resid(f)^2), tolerance = 1e-10)
  expect_equal(t$exx, crossprod(X) / 8, tolerance = 1e-12)
  expect_error(Epetheor(X[0, ], y[0]), "empty")
  expect_error(Epetheor(X, y[-1]), "same number of rows")
})

test_that("Finalsz solves the Kermack-McKendrick final-size relation", {
  r <- Finalsz(2.5, s0 = 0.99)
  z <- stats::uniroot(function(Z) 0.99 * (1 - exp(-2.5 * (Z + 0.01))) - Z, c(1e-6, 0.99),
                      tol = 1e-14)$root
  expect_equal(r$final_size, z, tolerance = 1e-12)
  expect_equal(r$attack_rate, z / 0.99, tolerance = 1e-12)
  expect_lt(abs(r$residual), 1e-12)
  expect_equal(Finalsz(0.8)$final_size, 0)
  expect_error(Finalsz(-1), "non-negative")
  expect_error(Finalsz(2, s0 = 0), "\\(0, 1\\]")
  expect_error(Finalsz(2, s0 = 0.9, i0 = 0.2), "s0 \\+ i0")
  expect_error(Finalsz(2, tol = 0), "positive")
})

test_that("Contam gives the contaminated cdf and Huber's minimax k", {
  H <- c(4, 5, 6, 10)
  r <- Contam(0.1, H, x = c(-1, 0, 5, 20))
  expect_equal(r$F, 0.9 * pnorm(c(-1, 0, 5, 20)) + 0.1 * c(0, 0, 2, 4) / 4, tolerance = 1e-12)
  expect_equal(r$mean, 0.1 * mean(H), tolerance = 1e-12)
  expect_equal(r$var, 0.9 + 0.1 * mean(H^2) - (0.1 * mean(H))^2, tolerance = 1e-12)
  k <- stats::uniroot(function(k) 2 * dnorm(k) / k - 2 * pnorm(-k) - 0.1 / 0.9, c(0.01, 10),
                      tol = 1e-14)$root
  expect_equal(r$k, k, tolerance = 1e-9)
  expect_equal(Contam(0, H)$k, Inf)
  expect_error(Contam(1, H), "\\[0, 1\\)")
  expect_error(Contam(0.1, numeric(0)), "empty")
})

test_that("Expmc and morie_expmc are the exponential mechanism", {
  u <- c(1, 3, 2, 5)
  r <- Expmc(letters[1:4], u, epsilon = 0.8, sensitivity = 2, seed = 7)
  p <- exp(0.8 * u / 4)
  p <- p / sum(p)
  expect_equal(r$probabilities, p, tolerance = 1e-12)
  idx <- withr::with_seed(7, sample.int(4, 1, prob = p))
  expect_equal(r$index, idx - 1L)
  expect_equal(r$selected, letters[idx])
  n <- morie_expmc(letters[1:4], u, epsilon = 0.8, sensitivity = 2, seed = 7)
  expect_equal(n$index, r$index)
  expect_error(Expmc(1:3, 1:2), "3 candidates")
})

test_that("BufferExposure counts sources within the radius", {
  R <- rbind(c(0, 0), c(2, 1))
  S <- rbind(c(0.5, 0), c(1.5, 1.5), c(3, 1))
  r <- BufferExposure(R, S, radius = 1.2, weights = c(1, 2, 3))
  D <- unname(as.matrix(dist(rbind(R, S)))[1:2, 3:5])
  expect_equal(r$count, rowSums(D <= 1.2))
  expect_equal(r$weighted, as.numeric((D <= 1.2) %*% c(1, 2, 3)))
  expect_equal(r$nearest, apply(D, 1, min), tolerance = 1e-12)
  one <- BufferExposure(R, S[1, , drop = FALSE], radius = 1)
  expect_equal(one$nearest, D[, 1], tolerance = 1e-12)
  hv <- BufferExposure(rbind(c(43.65, -79.38)), rbind(c(43.66, -79.39)), radius = 5,
                       metric = "haversine")
  a <- sin(0.01 * pi / 360)^2 + cos(43.65 * pi / 180) * cos(43.66 * pi / 180) * sin(-0.01 * pi / 360)^2
  expect_equal(hv$nearest, 2 * 6371 * asin(sqrt(a)), tolerance = 1e-12)
})
