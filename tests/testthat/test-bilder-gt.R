test_that("group testing and SPMI match binGroup2, MRCV and the Python arm", {
  d <- GtDorfman(0.05, 0.95, 0.97, 0.9, 0.99, size = 6)
  expect_equal(d$expected_tests, 2.64229276375, tolerance = 1e-10)
  expect_equal(unname(d$overall["PPPV"]), 0.94974347204575, tolerance = 1e-12)
  r <- GtPrev(3, 7, 24, ci = "score")
  expect_equal(r$ci, c(0.00632494931890608, 0.0516362362808093), tolerance = 1e-12)
  expect_equal(c(r$eb1, r$eb2), c(0.0188574984247741, 0.0185957956040668), tolerance = 1e-12)
  xs <- 12345
  u <- function() { xs <<- (xs * 16807) %% 2147483647; xs / 2147483647 }
  x <- y <- numeric(150)
  for (i in 1:150) { x[i] <- 4 * u() - 2; y[i] <- as.numeric(u() < plogis(-2.5 + 1.5 * x[i])) }
  grp <- rep(0:29, each = 5)
  z <- numeric(30)
  for (k in 1:30) { pos <- any(y[grp == k - 1] == 1); rr <- u(); z[k] <- if (pos) as.numeric(rr < 0.95) else as.numeric(rr > 0.98) }
  f <- GtRegEM(z, grp, cbind(1, x), se = 0.95, sp = 0.98)
  expect_equal(unname(f$beta), c(-1.1479358561534339, 0.9762824948165424), tolerance = 1e-7)
  xs <- 777
  M <- matrix(0, 120, 5)
  for (r in 1:120) { a <- u(); M[r, 1] <- u() < 0.3 + 0.3 * a; M[r, 2] <- u() < 0.5; M[r, 3] <- u() < 0.2 + 0.5 * a; M[r, 4] <- u() < 0.25 + 0.5 * a; M[r, 5] <- u() < 0.4 }
  s <- Spmi(M[, 1:3], M[, 4:5])
  expect_equal(c(s$statistic, s$rs2_statistic, s$rs2_df), c(12.050532970495411, 12.223014793644012, 6.0858792670353621), tolerance = 1e-10)
})
