test_that("accqlv and accpln match the book and AcceptanceSampling::find.plan", {
  r <- accqlv(46, 1, lot_size = 1000)
  expect_equal(c(r$aql, r$rql, r$aoql), c(0.0077802465479319639, 0.0819470508731946490, 0.017305563505389011),
               tolerance = 1e-12)
  expect_equal(stats::pbinom(1, 46, c(r$aql, r$rql)), c(0.95, 0.10), tolerance = 1e-12)
  expect_equal(unlist(accpln(0.0077, 0.0819)[c("n", "c")]), c(n = 64, c = 2))
  expect_equal(unlist(accpln(0.01, 0.06)[c("n", "c")]), c(n = 110, c = 3))
  expect_equal(unlist(accpln(0.02, 0.10, 0.01, 0.05)[c("n", "c")]), c(n = 116, c = 6))
})

test_that("bcppcc reproduces the book's bcplot", {
  r <- bcppcc(c(20, 22, 24, 21, 19, 30, 40, 23, 24, 25, 19))
  expect_equal(r$lambda, -3, tolerance = 1e-14)
  expect_equal(r$cor[c(1, 21, 51, 71)],
               c(0.97075861011113906, 0.98495222774565838, 0.92657242289873065, 0.82882425530178350), tolerance = 1e-13)
  r <- bcppcc(c(3.1, 0.4, 7.9, 1.2, 2.2, 5.5, 0.9), c(-1, 0, 0.5, 1))
  expect_equal(r$cor, c(0.89213850349800383, 0.99366574276927155, 0.98340414167770396, 0.94462963339686346),
               tolerance = 1e-13)
})

test_that("schomg equals (n - 1) R^2 of the score ANOVA", {
  tb <- matrix(c(14, 22, 32, 18, 16, 8, 8, 2, 0), 3, byrow = TRUE)
  g <- rep(rep(1:3, 3), c(t(tb)))
  sc <- rep(rep(c(1, 0, -1), each = 3), c(t(tb)))
  expect_equal(schomg(tb, c(1, 0, -1))$statistic, (length(sc) - 1) * summary(lm(sc ~ factor(g)))$r.squared,
               tolerance = 1e-12)
  expect_equal(schomg(tb, c(1, 0, -1))$statistic, 20.16410940627085, tolerance = 1e-13)
})

test_that("chisqmc equals chisq.test with simulate.p.value under the same seed", {
  tb <- matrix(c(14, 22, 32, 18, 16, 8, 8, 2, 0), 3, byrow = TRUE)
  r <- chisqmc(tb, B = 1000, seed = 7)
  set.seed(7)
  ref <- chisq.test(tb, simulate.p.value = TRUE, B = 1000)
  expect_equal(c(r$statistic, r$p_value), c(unname(ref$statistic), ref$p.value), tolerance = 1e-13)
  r <- chisqmc(matrix(c(3, 1, 1, 3), 2), B = 20000, seed = 11)
  expect_lt(abs(r$p_value - 34 / 70), 4 * sqrt(34 / 70 * 36 / 70 / 20000))
})

test_that("bkelim follows drop1 F statistics", {
  i <- 1:30
  X <- cbind(x1 = sin(i), x2 = cos(2 * i), x3 = (i %% 7) / 7, x4 = log(i), x5 = ((i * 13) %% 11) / 11)
  y <- 1 + 2 * X[, 1] + 0.05 * X[, 3] - 0.8 * X[, 4] + 0.3 * cos(5 * i)
  d <- data.frame(y, X)
  r <- bkelim(X, y)
  expect_equal(r$removed$name, c("x2", "x3"))
  expect_equal(r$removed$F, c(drop1(lm(y ~ ., d), test = "F")["x2", "F value"],
                              drop1(lm(y ~ x1 + x3 + x4 + x5, d), test = "F")["x3", "F value"]), tolerance = 1e-10)
  expect_equal(unname(r$last_f), drop1(lm(y ~ x1 + x4 + x5, d), test = "F")[-1, "F value"], tolerance = 1e-10)
  expect_equal(r$coefficients, unname(coef(lm(y ~ x1 + x4 + x5, d))), tolerance = 1e-12)
})

test_that("ll3way equals loglin", {
  t3 <- array(c(12, 7, 9, 15, 4, 11, 20, 6, 8, 13, 5, 10), dim = c(2, 3, 2))
  for (m in list(list(1, 2, 3), list(c(1, 2), 3), list(c(1, 2), c(1, 3)), list(c(1, 2), c(1, 3), c(2, 3)))) {
    r <- ll3way(t3, m)
    l <- loglin(t3, m, fit = TRUE, eps = 1e-13, iter = 10000, print = FALSE)
    expect_equal(c(r$lrt, r$pearson, r$df), c(l$lrt, l$pearson, l$df), tolerance = 1e-9)
    expect_equal(r$fit, unclass(l$fit), tolerance = 1e-9, ignore_attr = TRUE)
  }
  y <- array(c(911, 538, 44, 456, 3, 43, 2, 279), c(2, 2, 2))
  r <- ll3way(y, list(c(1, 2), c(1, 3), c(2, 3)))
  expect_equal(c(r$lrt, r$df), c(0.37398587014248030, 1), tolerance = 1e-9)
})

test_that("linsys classifies and solves", {
  r <- linsys(matrix(c(2, 1, -1, -3, -1, 2, -2, 1, 2), 3, byrow = TRUE), c(8, -11, -3))
  expect_equal(r$x, c(2, 3, -1), tolerance = 1e-13)
  expect_equal(r$kind, "unique")
  A2 <- cbind(1, 1:5)
  b2 <- c(2.1, 3.9, 6.2, 7.8, 10.1)
  r <- linsys(A2, b2)
  expect_equal(r$x, unname(qr.solve(A2, b2)), tolerance = 1e-13)
  expect_equal(r$kind, "least-squares")
  r <- linsys(matrix(c(1, 2, 2, 4), 2, byrow = TRUE), c(1, 3))
  expect_equal(c(r$rank_A, r$rank_Ab), c(1, 2))
  expect_null(r$x)
  expect_true(linsys(matrix(c(1, 2, 2, 4), 2, byrow = TRUE), c(1, 2))$consistent)
})
