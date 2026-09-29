d <- .spdiag_data()

test_that("Sardet and Sarjac are log|det(I - rho W)|", {
  for (a in c(0.4, -0.7)) {
    expect_equal(Sardet(d$W, a)$statistic, log(abs(det(diag(8) - a * d$W))), tolerance = 1e-12)
    expect_equal(Sarjac(d$W3, a)$statistic, log(abs(det(diag(3) - a * d$W3))), tolerance = 1e-12)
  }
})

test_that("Sarlrt, Sarwald and Sarr2 follow their formulas", {
  r <- Sarlrt(-10.2, -13.9)
  expect_equal(r$statistic, 7.4)
  expect_equal(r$p_value, 1 - pchisq(7.4, 1), tolerance = 1e-12)
  w <- Sarwald(0.37, 0.12)
  expect_equal(w$statistic, (0.37 / 0.12)^2, tolerance = 1e-14)
  expect_equal(w$p_value, 2 * pnorm(-0.37 / 0.12), tolerance = 1e-12)
  cs <- 1 - exp(2 / 40 * (-31.5 + 20))
  expect_equal(Sarr2(-20, -31.5, 40)$statistic, cs / (1 - exp(2 / 40 * -31.5)), tolerance = 1e-14)
})

test_that("Sarfilt, Sarres and Sarsc", {
  expect_equal(Sarfilt(d$y, d$W, 0.35)$filtered, as.vector(d$y - 0.35 * d$W %*% d$y), tolerance = 1e-14)
  expect_equal(Sarres(d$e, d$W)$statistic, .spdiag_moran(d$e, d$W), tolerance = 1e-13)
  e <- d$e - mean(d$e)
  r <- Sarres(e, d$W, matrix(1, 8, 1))
  S0 <- sum(d$W)
  S1 <- 0.5 * sum((d$W + t(d$W))^2)
  S2 <- sum((rowSums(d$W) + colSums(d$W))^2)
  EI <- -1 / 7
  expect_equal(r$expected, EI, tolerance = 1e-13)
  expect_equal(r$variance, (64 * S1 - 8 * S2 + 3 * S0^2) / (S0^2 * 63) - EI^2, tolerance = 1e-12)
  f <- lm.fit(d$X, d$y)
  u <- f$residuals
  s2 <- sum(u^2) / 8
  Tr <- sum(d$W * (d$W + t(d$W)))
  wyh <- d$W %*% (d$y - u)
  nJ <- sum(lm.fit(d$X, wyh)$residuals^2) / s2 + Tr
  expect_equal(Sarsc(d$y, d$X, d$W)$statistic, (sum(u * d$W %*% d$y) / s2)^2 / nJ, tolerance = 1e-10)
})

test_that("Sarimp, Sarspil and Sarsim use (I - rho W)^-1", {
  Ai <- solve(diag(8) - 0.4 * d$W)
  r <- Sarimp(c(2, -1), 0.4, d$W)
  expect_equal(r$direct, c(2, -1) * sum(diag(Ai)) / 8, tolerance = 1e-12)
  expect_equal(r$total, c(2, -1) * sum(Ai) / 8, tolerance = 1e-12)
  expect_equal(Sarspil(c(2, -1), 0.4, d$W)$ratio, rep((sum(Ai) - sum(diag(Ai))) / sum(diag(Ai)), 2), tolerance = 1e-12)
  z <- .morie_random_normal(12 * 2, seed = 5)
  tot <- vapply(1:12, function(s) {
    rho <- 0.4 + 0.1 * z[2 * s - 1]
    (2 + 0.2 * z[2 * s]) * sum(solve(diag(8) - rho * d$W)) / 8
  }, numeric(1))
  expect_equal(Sarsim(2, 0.4, d$W, nsim = 12, vcov = diag(c(0.01, 0.04)), seed = 5)$statistic, mean(tot), tolerance = 1e-12)
  expect_error(Sarsim(2, 0.4, d$W))
})

test_that("Sarvar inverts the Gaussian Fisher information", {
  r <- Sarvar(d$X, d$W, 0.3, 0.5, c(1, 2))
  Fm <- .spdiag_fisher(d$X, d$W, c(1, 2), 0.3, 0, 0.5, TRUE, FALSE)
  expect_equal(r$information, Fm, tolerance = 1e-9)
  expect_equal(r$cov %*% Fm, diag(4), tolerance = 1e-9)
})

test_that("Sarboot resamples innovations with Philox indices", {
  B <- 5
  f <- .sxd_fit(d$y, d$X, d$W, "lag")
  ml <- SpatialRegressionML(d$y, d$X, d$W, "lag")
  expect_equal(f$rho, ml$rho, tolerance = 1e-6)
  Xb <- as.vector(d$X %*% f$beta)
  e <- d$y - f$rho * as.vector(d$W %*% d$y) - Xb
  e <- e - mean(e)
  u <- .morie_random_uniform(B * 8, seed = 11)
  Ai <- solve(diag(8) - f$rho * d$W)
  dr <- vapply(1:B, function(b) {
    ys <- as.vector(Ai %*% (Xb + e[floor(u[(b - 1) * 8 + 1:8] * 8) + 1]))
    SpatialRegressionML(ys, d$X, d$W, "lag")$rho
  }, numeric(1))
  r <- Sarboot(d$y, d$X, d$W, B = B, seed = 11, level = 0.9)
  expect_equal(sort(r$draws), sort(dr), tolerance = 1e-6)
  expect_equal(c(r$ci_lower, r$ci_upper), unname(quantile(r$draws, c(0.05, 0.95))), tolerance = 1e-14)
})
