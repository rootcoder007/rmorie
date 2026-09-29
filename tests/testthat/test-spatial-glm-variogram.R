test_that("Sgcrh follows Cressie (1993) and gstat's form", {
  xy <- rbind(c(0, 0), c(1, 0.2), c(2.1, 0), c(0.1, 1), c(1.2, 1.1), c(2, 0.9), c(0.3, 2.2), c(1.1, 1.9), c(2.3, 2.1))
  z <- c(1, 2.4, 1.3, 3.1, 1.9, 2.2, 0.7, 2.8, 1.6)
  d <- as.matrix(dist(xy))
  pr <- which(upper.tri(d) & d <= 2, arr.ind = TRUE)
  k <- ceiling(d[pr] / 0.5)
  a <- abs(z[pr[, 1]] - z[pr[, 2]])^0.5
  N <- as.numeric(table(k))
  m <- as.numeric(tapply(a, k, mean))
  expect_equal(Sgcrh(z, xy, 4, 2)$gamma, m^4 / (0.457 + 0.494 / N + 0.045 / N^2) / 2, tolerance = 1e-13)
  expect_equal(Sgcrh(z, xy, 4, 2, "gstat")$gamma, m^4 / (0.457 + 0.494 / N) / 2, tolerance = 1e-13)
})

test_that("Sglm redirects and Sglss, Sgrwn", {
  P <- cbind(0:11 %% 4, 0:11 %/% 4)
  X <- cbind(1, P[, 1])
  y <- c(1.1, 1.9, 3.2, 3.8, 1.3, 2.2, 2.9, 4.1, 0.8, 2.1, 3, 4.2)
  f <- Likfit(y, P, list(model = "Exp", range = max(dist(P)) / 3), X = X)
  r <- Sglm(X, y, P)
  expect_equal(r$estimate, f$beta)
  V <- f$psill * exp(-as.matrix(dist(P)) / f$range) + diag(f$nugget, 12)
  expect_equal(r$se, sqrt(diag(solve(t(X) %*% solve(V) %*% X))), tolerance = 1e-10)
  expect_equal(Sglss(c(1, 2, 4), c(1.5, 2, 3))$statistic, 1.25 / 3)
  expect_equal(Sgrwn(rbind(c(0, 2, 2), c(1, 0, 3), c(0, 0, 0)))$W_normalized,
               rbind(c(0, 0.5, 0.5), c(0.25, 0, 0.75), c(0, 0, 0)))
})
