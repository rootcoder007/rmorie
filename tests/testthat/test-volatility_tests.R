test_that("ARCH-LM recomputes", {
  r <- sin(1.3 * (0:79)) * (1 + 0.8 * sin(0.2 * (0:79)))
  e2 <- (r - mean(r))^2
  f <- stats::lm(e2[3:80] ~ e2[2:79] + e2[1:78])
  a <- vol_engle_lagrange(r, q = 2)
  expect_equal(a$statistic, 78 * summary(f)$r.squared, tolerance = 1e-10)
})

test_that("multi-horizon statistics and the exact Kolmogorov tail recompute", {
  r <- 0.01 * sin(1.7 * (0:119)) + 0.004 * cos(0.3 * (0:119)^2)
  o <- vol_corradi_swan_persistence(r, horizons = c(1, 4), n_mc = 39, seed = 3)
  agg <- sort(colSums(matrix(r, 4)))
  expect_equal(o$per_horizon[[2]]$statistic,
               unname(stats::ks.test(agg, "pnorm", mean(agg), sd(agg))$statistic), tolerance = 1e-12)
  s <- vol_corradi_swan_persistence(r, horizons = c(1, 4), cdf = function(x, h) stats::pnorm(x, 0, 0.01 * sqrt(h)))
  x <- sort(colSums(matrix(r, 4)))
  kt <- stats::ks.test(x, "pnorm", 0, 0.02, exact = TRUE)
  expect_equal(s$per_horizon[[2]]$p_value, kt$p.value, tolerance = 1e-10)
})
