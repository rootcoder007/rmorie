.libPaths(c("~/tmp/Rlib", .libPaths()))
skip_if_not_installed("spdep")
skip_if_not_installed("spatialreg")

.sxd_case <- function() {
  n <- 30
  lw <- spdep::nb2listw(spdep::cell2nb(5, 6))
  W <- spdep::listw2mat(lw)
  ii <- 0:(n - 1)
  X <- cbind(1, ((ii * 7) %% 11) / 5, ((ii * 5) %% 13) / 4 - 1.5)
  eps <- ((ii * 11) %% 17 - 8) / 6
  y <- as.vector(solve(diag(n) - 0.4 * W, X %*% c(1, 2, -1) + eps))
  list(n = n, lw = lw, W = W, X = X, y = y, d = data.frame(y = y, x1 = X[, 2], x2 = X[, 3]))
}

test_that("Sarvar and Semvar equal the asymptotic covariance of lagsarlm / errorsarlm (eigen)", {
  cs <- .sxd_case()
  f <- spatialreg::lagsarlm(y ~ x1 + x2, cs$d, cs$lw, method = "eigen")
  r <- Sarvar(cs$X, cs$W, f$rho, f$s2, f$coefficients)
  o <- c(5, 4, 1:3)
  expect_equal(unname(r$cov[o, o]), unname(f$resvar), tolerance = 1e-8)
  e <- spatialreg::errorsarlm(y ~ x1 + x2, cs$d, cs$lw, method = "eigen")
  s <- Semvar(cs$X, cs$W, e$lambda, e$s2)
  expect_equal(unname(s$cov[o, o]), unname(e$resvar), tolerance = 1e-8)
  expect_equal(sqrt(s$statistic), e$lambda.se, tolerance = 1e-8)
})

test_that("Semlm and Sarsc equal spdep::lm.RStests", {
  cs <- .sxd_case()
  m <- lm(y ~ x1 + x2, cs$d)
  t <- spdep::lm.RStests(m, cs$lw, test = c("RSerr", "RSlag", "adjRSlag"))
  expect_equal(Semlm(residuals(m), cs$W)$statistic, unname(t$RSerr$statistic), tolerance = 1e-10)
  s <- Sarsc(cs$y, cs$X, cs$W)
  expect_equal(s$statistic, unname(t$RSlag$statistic), tolerance = 1e-10)
  expect_equal(s$robust_statistic, unname(t$adjRSlag$statistic), tolerance = 1e-10)
})

test_that("Sarres with the design equals spdep::lm.morantest", {
  cs <- .sxd_case()
  m <- lm(y ~ x1 + x2, cs$d)
  t <- spdep::lm.morantest(m, cs$lw)
  r <- Sarres(residuals(m), cs$W, cs$X)
  expect_equal(r$statistic, unname(t$estimate[1]), tolerance = 1e-12)
  expect_equal(r$expected, unname(t$estimate[2]), tolerance = 1e-12)
  expect_equal(r$variance, unname(t$estimate[3]), tolerance = 1e-12)
  expect_equal(r$p_value, as.numeric(t$p.value), tolerance = 1e-10)
})

test_that("Sarimp and Sarspil equal spatialreg::impacts", {
  cs <- .sxd_case()
  f <- spatialreg::lagsarlm(y ~ x1 + x2, cs$d, cs$lw, method = "eigen")
  im <- spatialreg::impacts(f, listw = cs$lw)
  r <- Sarimp(f$coefficients[-1], f$rho, cs$W)
  expect_equal(r$direct, as.numeric(im$direct), tolerance = 1e-10)
  expect_equal(r$indirect, as.numeric(im$indirect), tolerance = 1e-10)
  expect_equal(r$total, as.numeric(im$total), tolerance = 1e-10)
  expect_equal(Sarspil(f$coefficients[-1], f$rho, cs$W)$ratio, as.numeric(im$indirect / im$direct), tolerance = 1e-10)
})

test_that("Sacconv bounds equal 1 / range of spatialreg::eigenw", {
  cs <- .sxd_case()
  ev <- Re(spatialreg::eigenw(cs$lw))
  r <- Sacconv(cs$W, 0.2, -0.1)
  expect_equal(c(r$lower, r$upper), 1 / range(ev), tolerance = 1e-12)
})

test_that("Sacrob equals spatialreg::gstsls with robust = TRUE", {
  cs <- .sxd_case()
  g <- spatialreg::gstsls(y ~ x1 + x2, cs$d, cs$lw, robust = TRUE)
  r <- Sacrob(cs$y, cs$X, cs$W)
  expect_equal(r$coefficients, unname(g$coefficients), tolerance = 1e-8)
  expect_equal(r$se, unname(g$rest.se), tolerance = 1e-8)
})
