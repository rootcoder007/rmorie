amax <- c(120, 95, 310, 180, 150, 220, 90, 260, 140, 175, 205, 130, 400, 160, 110)

test_that("L-moment fits and quantiles equal lmom", {
  skip_if_not_installed("lmom")
  lm <- lmom::samlmu(amax)
  expect_equal(SampleLmoments(amax), unname(lm), tolerance = 1e-13)
  Tp <- c(2, 5, 10, 25, 50, 100)
  pg <- lmom::pelgev(lm)
  r <- FloodFrequency(amax, "gev")
  expect_equal(r$params, unname(pg), tolerance = 1e-13)
  expect_equal(r$quantiles, lmom::quagev(1 - 1 / Tp, pg), tolerance = 1e-13)
  for (t3 in c(-0.9, -0.85, -0.5, 0, 1e-7, 0.3, 0.7)) {
    expect_equal(rmorie:::.hy_pelgev(10, 2, t3), unname(lmom::pelgev(c(10, 2, t3))), tolerance = 1e-12)
  }
  gu <- lmom::pelgum(lm)
  expect_equal(FloodFrequency(amax, "gumbel")$quantiles, lmom::quagum(1 - 1 / Tp, gu), tolerance = 1e-13)
  y <- log10(amax)
  n <- length(y)
  g <- n * sum((y - mean(y))^3) / ((n - 1) * (n - 2) * sd(y)^3)
  expect_equal(FloodFrequency(amax, "lp3")$quantiles, 10^lmom::quape3(1 - 1 / Tp, c(mean(y), sd(y), g)),
               tolerance = 1e-12)
  q <- 5 + (0:(365 * 8 - 1)) %% 30 + 3 * (0:(365 * 8 - 1)) %/% 365 + 2 * sin((0:(365 * 8 - 1)) / 50)
  r <- LowFlowFrequency(q, (0:(365 * 8 - 1)) %/% 365, d = 7, T = 10)
  pw <- lmom::pelwei(lmom::samlmu(r$annual_minima))
  expect_equal(r$params, unname(pw), tolerance = 1e-12)
  expect_equal(r$quantile, unname(lmom::quawei(0.1, pw)), tolerance = 1e-12)
})

test_that("IdfFit equals nls", {
  t <- c(5, 10, 15, 30, 60, 120, 360, 720)
  obs <- 900 / (t + 10)^0.75 * c(1.02, 0.98, 1.01, 0.99, 1.03, 0.97, 1.01, 0.99)
  f <- IdfFit(t, obs)
  ref <- stats::nls(obs ~ a / (t + b)^cc, start = list(a = 900, b = 10, cc = 0.75))
  expect_equal(c(f$a, f$b, f$c), unname(coef(ref)), tolerance = 1e-5)
  expect_lte(f$rss, deviance(ref) * (1 + 1e-12))
})
