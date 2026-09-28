test_that("SmacofMds and ProcrustesFit equal smacof and vegan", {
  skip_if_not_installed("smacof")
  skip_if_not_installed("vegan")
  u <- .morie_random_uniform(90, seed = 29, stream = 0)
  P <- cbind(u[1:12], u[13:24], u[25:36])
  D <- round(as.matrix(dist(P)) * 10) / 10
  for (t in c("ratio", "interval", "ordinal")) {
    s <- smacof::smacofSym(stats::as.dist(D), type = t)
    r <- SmacofMds(D, type = t)
    expect_equal(r$stress, s$stress, tolerance = 1e-10, info = t)
    expect_equal(r$confdist, as.vector(s$confdist), tolerance = 1e-10, info = t)
    expect_equal(r$niter, s$niter, info = t)
  }
  X <- P[, 1:2]
  Y <- cbind(P[, 2] + u[37:48] / 10, -P[, 1])
  v <- vegan::procrustes(X, Y)
  p <- ProcrustesFit(X, Y)
  expect_equal(p$ss, v$ss, tolerance = 1e-12)
  expect_equal(p$residuals, as.vector(stats::residuals(v)), tolerance = 1e-12)
})
