test_that("proportional odds, Rao-Scott and GLM fit statistics match references", {
  xs <- 4242
  u <- function() { xs <<- (xs * 16807) %% 2147483647; xs / 2147483647 }
  x1 <- x2 <- y <- numeric(200)
  for (i in 1:200) {
    a <- 4 * u() - 2
    b <- u()
    p1 <- u()
    p2 <- u()
    v <- 0.8 * a - 1.2 * b + log(p1 / (1 - p2))
    x1[i] <- a
    x2[i] <- b
    y[i] <- if (v < -1) 0 else if (v < 0.5) 1 else if (v < 1.5) 2 else 3
  }
  r <- PolrFit(y, cbind(x1, x2))
  expect_equal(c(r$intercepts, unname(r$beta)), c(-1.786386446619, 0.717897594557, 1.535142493703, -1.46834013453, 2.33457023102), tolerance = 1e-9)
  expect_equal(r$loglik, -187.819396146, tolerance = 1e-10)
  d <- c(0.8, 1.4, 2.1, 0.5)
  rs <- RaoScott(9.3, d)
  expect_equal(c(rs$rs2, rs$rs2_df), c(4 * mean(d) * 9.3 / sum(d^2), sum(d)^2 / sum(d^2)), tolerance = 1e-12)
  dist <- c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65)
  succ <- c(19, 18, 17, 15, 12, 10, 8, 5, 3, 1)
  g <- GlmStdRes(succ, cbind(1, dist), trials = rep(20, 10))
  expect_equal(c(g$deviance, g$pearson_chisq, g$df), c(0.76421933017812904, 0.71358727577615844, 8), tolerance = 1e-9)
})
