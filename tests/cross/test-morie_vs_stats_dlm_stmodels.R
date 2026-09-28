test_that("ARMA ACF equals stats::ARMAacf; the DLM filters equal dlm::dlmFilter", {
  skip_if_not_installed("dlm")
  for (m in list(list(c(0.5, -0.3), c(0.4, 0.2, 0.1)), list(numeric(0), c(0.7, -0.2)), list(0.9, numeric(0)),
                 list(c(1.2, -0.5, 0.1), 0.3))) {
    expect_equal(ArmaAcf(m[[1]], m[[2]], 12), unname(stats::ARMAacf(m[[1]], m[[2]], 12)), tolerance = 1e-12)
  }
  e <- .morie_random_normal(30, seed = 9)
  Y <- t(sapply(0:9, function(t) 20 + 5 * cos(pi * t / 12) + e[3 * t + 1:3]))
  P <- rbind(c(0, 0), c(1, 0.5), c(2, 2))
  a <- c(0.3, -0.2)
  r <- HarmonicOzoneDlm(Y, 0:9, P, a, 1.5, 0.5, c(0.2, 0.05, 0.02), c0 = 100)
  JFF <- matrix(0, 3, 7)
  for (s in 1:3) {
    JFF[s, 1 + s] <- 1
    JFF[s, 4 + s] <- 2
  }
  X <- cbind(cos(pi * (0:9) / 12) + a[1] * sin(pi * (0:9) / 12), cos(pi * (0:9) / 6) + a[2] * sin(pi * (0:9) / 6))
  mod <- dlm::dlm(FF = cbind(1, diag(3), diag(3)), V = 0.5 * exp(-as.matrix(stats::dist(P)) / 1.5), GG = diag(7),
                  W = diag(c(0.2, rep(0.05, 3), rep(0.02, 3))), m0 = rep(0, 7), C0 = diag(100, 7), JFF = JFF, X = X)
  f <- dlm::dlmFilter(Y, mod)
  expect_equal(r$means[[10]], f$m[11, ], tolerance = 1e-9)
  expect_equal(-r$loglik, dlm::dlmLL(Y, mod) + 15 * log(2 * pi), tolerance = 1e-10)
  y <- .morie_random_normal(12, seed = 4)
  ng <- NormalGammaDlm(y, c(1, 0.5), matrix(c(1, 0, 1, 1), 2), diag(c(0.2, 0.05)), c(0, 0), diag(c(2, 1)), 1e12, 0.8)
  m2 <- dlm::dlm(FF = matrix(c(1, 0.5), 1), V = 0.8, GG = matrix(c(1, 0, 1, 1), 2), W = 0.8 * diag(c(0.2, 0.05)),
                 m0 = c(0, 0), C0 = 0.8 * diag(c(2, 1)))
  f2 <- dlm::dlmFilter(y, m2)
  expect_equal(ng$m[[12]], f2$m[13, ], tolerance = 1e-9)
  expect_equal(ng$C[[12]], dlm::dlmSvd2var(f2$U.C, f2$D.C)[[13]], tolerance = 1e-9)
})
