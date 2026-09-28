
test_that("ThinPlateSpline equals fields::Tps", {
  skip_if_not_installed("fields")
  u <- .morie_random_uniform(90, seed = 9, stream = 0)
  X <- cbind(u[1:30], u[31:60])
  y <- sin(3 * X[, 1]) + X[, 2]^2 + 0.1 * (u[61:90] - 0.5)
  nd <- rbind(c(.2, .3), c(.5, .6), c(.8, .1))
  for (lam in c(1e-4, 1e-2)) {
    ft <- fields::Tps(X, y, lambda = lam, scale.type = "unscaled")
    r <- ThinPlateSpline(X, y, lam = lam, newdata = nd)
    expect_equal(r$predicted, as.vector(stats::predict(ft, nd)), tolerance = 1e-10)
    expect_equal(r$df, ft$eff.df, tolerance = 1e-10)
  }
})
