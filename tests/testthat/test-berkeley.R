st <- rbind(c(0, 0), c(4, 0.5), c(1, 3), c(3.5, 3.5), c(2, 1.5), c(0.5, 2))
th <- sin((0:11) / 3)
W <- outer(0:5, 0:11, function(i, t) 0.05 * sin(3.1 * i + 1.7 * t))
T <- outer(8 + 0:5, th, "+") + W

test_that("BerkeleyEarth recovers the common signal", {
  r <- BerkeleyEarth(st, T)
  expect_lt(max(abs(r$theta - (th - mean(th)))), 0.08)
  expect_lt(abs(sum(r$theta)), 1e-9)
  expect_equal(r$baselines, rowMeans(sweep(T, 2, r$theta)), tolerance = 1e-9)
})

test_that("BerkeleyEarth handles missing values", {
  Tm <- T
  Tm[3, 4] <- NA
  r <- BerkeleyEarth(st, Tm, n_iter = 3)
  expect_true(all(is.finite(r$theta)))
})
