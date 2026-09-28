nn <- 6
tt <- 5
ee <- .morie_random_normal(3 * nn * tt, seed = 5)
unit <- rep(seq_len(nn) - 1, each = tt)
time <- rep(seq_len(tt) - 1, nn)
X <- cbind(ee[1:30] + 0.2 * time, ee[31:60] * 0.5 + unit)
y <- 1 + 0.4 * X[, 1] - 0.3 * X[, 2] + cos(unit) + ee[61:90]

test_that("within transform and variance components follow their definitions", {
  w <- PanelWithin(X, unit)
  expect_equal(w, X - apply(X, 2, function(v) ave(v, unit)), tolerance = 1e-12)
  e <- PanelResiduals(y, X, unit)$residuals
  vc <- PanelVarianceComponents(e, unit)
  m <- tapply(e, unit, mean)
  expect_equal(vc$sigma2_idios, sum((e - m[as.character(unit)])^2) / (nn * (tt - 1)), tolerance = 1e-12)
  expect_equal(vc$sigma2_1, tt * sum(m^2) / nn, tolerance = 1e-12)
  expect_error(PanelVarianceComponents(e[-1], unit[-1]), "balanced")
})

test_that("cross-section dependence and unobserved effects by hand", {
  e <- PanelResiduals(y, X, unit, "heterogeneous")$residuals
  E <- matrix(e, tt)
  R <- stats::cor(E)
  r <- R[lower.tri(R)]
  expect_equal(CrossSectionDependence(e, unit, time)$statistic, sqrt(tt / length(r)) * sum(r), tolerance = 1e-10)
  expect_equal(CrossSectionDependence(e, unit, time, "lm")$statistic, tt * sum(r^2), tolerance = 1e-10)
  expect_equal(CrossSectionDependence(e, unit, time, "rho")$statistic, mean(r), tolerance = 1e-10)
  p <- PanelResiduals(y, X, unit)$residuals
  s <- tapply(p, unit, function(v) (sum(v)^2 - sum(v^2)) / 2)
  expect_equal(UnobservedEffectsTest(p, unit)$statistic, sum(s) / sqrt(sum(s^2)), tolerance = 1e-10)
})

test_that("Baltagi-Li statistic uses the inverse information", {
  two <- BaltagiLiTest(y, X, unit)
  one <- BaltagiLiTest(y, X, unit, "onesided")
  expect_equal(one$statistic^2, two$statistic, tolerance = 1e-10)
  s2e <- two$sigma2_e
  s21 <- two$sigma2_1
  a <- (s2e - s21) / (tt * s21)
  J <- matrix(c(nn * (2 * a^2 * (tt - 1)^2 + 2 * a * (2 * tt - 3) + tt - 1), nn * (tt - 1) * s2e / s21^2,
                nn * (tt - 1) / tt * s2e * (1 / s21^2 - 1 / s2e^2),
                nn * (tt - 1) * s2e / s21^2, nn * tt^2 / (2 * s21^2), nn * tt / (2 * s21^2),
                nn * (tt - 1) / tt * s2e * (1 / s21^2 - 1 / s2e^2), nn * tt / (2 * s21^2),
                nn / 2 * (1 / s21^2 + (tt - 1) / s2e^2)), 3)
  expect_equal(two$J11, solve(J)[1, 1], tolerance = 1e-10)
  expect_equal(two$statistic, two$D^2 * two$J11, tolerance = 1e-12)
})

test_that("serial test chi-square and F forms agree; Conley limits", {
  for (o in 1:2) {
    c1 <- PanelSerialTest(y, X, unit, o)$statistic
    f1 <- PanelSerialTest(y, X, unit, o, "F")$statistic
    expect_equal(c1, 30 * (1 - 1 / (1 + f1 * o / (30 - 2 - o))), tolerance = 1e-9)
  }
  e <- PanelResiduals(y, X, unit)$residuals
  lat <- 44 + 0.1 * (seq_len(30) - 1)
  lon <- -79 + 0.05 * ((seq_len(30) - 1) %% 7)
  Xi <- cbind(1, X)
  B <- solve(crossprod(Xi))
  white <- B %*% crossprod(Xi * e) %*% B * 30 / 27
  expect_equal(ConleyVcov(X, e, lat, lon, 1e-9)$vcov, white, tolerance = 1e-10)
  expect_lt(max(abs(ConleyVcov(X, e, lat, lon, 1e5)$vcov)), 1e-12)
  d <- 6371 * abs(outer(c(0, 0.3, 0.9), c(0, 0.3, 0.9), "-")) * pi / 180
  s <- c(0, -2, 3)
  r <- ConleyVcov(c(0, 1, 3), c(1, -2, 1), c(0, 0, 0), c(0, 0.3, 0.9), 80, kernel = "bartlett",
                  adjust = FALSE, intercept = FALSE)
  expect_equal(r$vcov[1, 1], sum(outer(s, s) * pmax(1 - d / 80, 0)) / 100, tolerance = 1e-10)
})
