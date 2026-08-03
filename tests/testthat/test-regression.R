# Parity for the regression block, anchored on R's own lm, sandwich,
# lmtest and car.

N <- 40
X1 <- sapply(0:(N-1), function(i) ((i*7) %% 13) + 0.5*((i*3) %% 5))
X2 <- sapply(0:(N-1), function(i) ((i*5) %% 11) - 0.25*((i*2) %% 7))
E  <- sapply(0:(N-1), function(i) ((i*13) %% 17)/17 - 0.5)
Y  <- 3 + 1.5*X1 - 0.8*X2 + E*(1 + 0.05*X1)
X  <- cbind(X1, X2)
FIT <- morie_ols(Y, X)

test_that("OLS matches lm and summary.lm", {
  expect_equal(unname(FIT$coef), c(2.83062921605554, 1.50887378011902,
                           -0.786477982373891), tolerance = 1e-12)
  expect_equal(unname(FIT$se), c(0.211842232683341, 0.0190862901909392,
                         0.0230017208671273), tolerance = 1e-12)
  expect_equal(FIT$r_squared, 0.997397231487757, tolerance = 1e-12)
  expect_equal(FIT$sigma, 0.396084329507282, tolerance = 1e-12)
  expect_equal(FIT$f_statistic, 7089.31612463033, tolerance = 1e-10)
  expect_equal(FIT$f_p_value, 1.53305439891331e-48, tolerance = 1e-10)
  expect_gt(FIT$f_p_value, 0)
})

test_that("HC0-HC3 match sandwich::vcovHC", {
  want <- list(HC0 = c(0.191679800355021, 0.0172033171162341,
                       0.0212173733280863),
               HC1 = c(0.199299166307748, 0.0178871573981345,
                       0.0220607743105732),
               HC2 = c(0.21338832217753, 0.0190716967679078,
                       0.0234116174677771),
               HC3 = c(0.239053563666654, 0.0212641516186098,
                       0.0259807964174029))
  for (k in names(want))
    expect_equal(unname(morie_robust_vcov(FIT, k)$se), want[[k]],
                 tolerance = 1e-12)
  ses <- vapply(names(want),
                function(k) morie_robust_vcov(FIT, k)$se[2], numeric(1))
  expect_true(all(diff(ses) > 0))   # HC3 widest under leverage
})

test_that("Newey-West matches sandwich with the same lag", {
  expect_equal(unname(morie_newey_west_vcov(FIT, lags = 3)$se),
               c(0.190394553869704, 0.0167770191608669,
                 0.0214790245326673), tolerance = 1e-11)
  expect_equal(morie_newey_west_vcov(FIT)$lags,
               floor(4 * (N / 100)^(2 / 9)))
  expect_equal(unname(morie_newey_west_vcov(FIT, lags = 0)$se),
               unname(morie_robust_vcov(FIT, "HC1")$se),
               tolerance = 1e-12)
})

test_that("diagnostics match lmtest and car", {
  bp <- morie_breusch_pagan(FIT)
  expect_equal(bp$statistic, 0.792751135858652, tolerance = 1e-10)
  expect_equal(bp$df, 2)
  expect_equal(bp$p_value, 0.672753983664454, tolerance = 1e-11)
  dw <- morie_durbin_watson(FIT)
  expect_equal(dw$statistic, 2.09214798929709, tolerance = 1e-12)
  expect_equal(unname(morie_vif(X)$vif),
               c(1.38183972779563, 1.38183972779563), tolerance = 1e-11)
})

test_that("VIF rises with collinearity and OLS refuses singular designs", {
  z <- cbind(X1, X1 + 1e-3 * X2)
  expect_gt(morie_vif(z)$vif[1], 100)
  expect_error(morie_ols(Y, cbind(X1, X1)), "collinear")
  expect_error(morie_ols(Y[1:5], X))
})

test_that("residuals are orthogonal to the design", {
  expect_lt(max(abs(crossprod(FIT$design, FIT$residuals))), 1e-9)
})
