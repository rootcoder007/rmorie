D1 <- rbind(c(0, 3, 4, 6, 5), c(3, 0, 5, 4, 2), c(4, 5, 0, 3, 6), c(6, 4, 3, 0, 4), c(5, 2, 6, 4, 0))
D2 <- rbind(c(0, 2, 5, 6, 4), c(2, 0, 4, 5, 3), c(5, 4, 0, 2, 6), c(6, 5, 2, 0, 5), c(4, 3, 6, 5, 0))
D3 <- rbind(c(0, 4, 3, 5, 6), c(4, 0, 6, 3, 2), c(3, 6, 0, 4, 5), c(5, 3, 4, 0, 3), c(6, 2, 5, 3, 0))

test_that("three-way SMACOF stress recomputes and constraints nest", {
  fits <- lapply(c(identity = "identity", indscal = "indscal", idioscal = "idioscal"), function(cc) {
    SmacofIndDiff(list(D1, D2, D3), 2, constraint = cc, eps = 1e-12, itmax = 5000L)
  })
  for (r in fits) {
    tot <- sum(mapply(function(D, X) {
      d <- D[lower.tri(D)]
      sum((d * sqrt(10 / sum(d^2)) - as.vector(dist(X)))^2)
    }, list(D1, D2, D3), r$conf))
    expect_equal(r$stress, sqrt(tot / 30), tolerance = 1e-12)
    expect_equal(sum(r$sps), 100, tolerance = 1e-12)
    expect_equal(sum(r$spp), 100, tolerance = 1e-12)
  }
  expect_lt(fits$indscal$stress, fits$identity$stress)
  expect_lte(fits$idioscal$stress, fits$indscal$stress + 1e-9)
  r1 <- SmacofIndDiff(list(D1, D1), 2, constraint = "identity", eps = 1e-12, itmax = 5000L)
  expect_equal(r1$stress, SmacofMds(D1, 2, eps = 1e-12, itmax = 5000L)$stress, tolerance = 1e-9)
})

test_that("jackknife and bootstrap measures recompute", {
  r <- MdsJackknife(D1)
  den <- sum(vapply(r$jackknife_conf, function(Y) sum(Y^2), 0))
  expect_equal(r$stab, 1 - sum(vapply(r$jackknife_conf, function(Y) sum((Y - r$comparison_conf)^2), 0)) / den,
               tolerance = 1e-12)
  expect_equal(r$disp, 2 - (r$stab + r$cross), tolerance = 1e-15)
  expect_lt(r$loss, MdsJackknife(D1, method = "smacof")$loss)
  X6 <- cbind(1:6, c(2, 1, 5, 3, 6, 4), c(3, 4, 2, 6, 5, 8), c(1, 0, 2, 1, 3, 2))
  b <- MdsBootstrap(X6, 2, method_dat = "euclidean", resamples = rep(list(1:6), 4))
  expect_equal(b$stressvec, rep(b$stress, 4), tolerance = 1e-15)
  expect_equal(b$stab, 1, tolerance = 1e-12)
  b <- MdsBootstrap(X6, 2, nrep = 7, seed = 3)
  expect_equal(b$bootci, unname(quantile(b$stressvec, c(0.025, 0.975))), tolerance = 1e-15)
})

test_that("oblique Procrustes and orientation checks", {
  A <- rbind(c(0.8, 0.1), c(0.7, 0.2), c(0.2, 0.9), c(0.1, 0.7), c(0.5, 0.5))
  B <- rbind(c(1, 0), c(1, 0), c(0, 1), c(0, 1), c(0.5, 0.5))
  o <- ProcrustesOblique(A, B, eps = 1e-12)
  expect_equal(o$loadings, A %*% t(solve(o$T)), tolerance = 1e-14)
  expect_equal(o$f, sum((o$loadings - B)^2), tolerance = 1e-14)
  expect_equal(colSums(o$T^2), c(1, 1), tolerance = 1e-14)
  expect_true(o$converged)
  expect_equal(MdsReflect(rbind(c(1, -3), c(-2, 1)))$signs, c(-1, -1))
  f <- MdsFlip(rbind(c(0, 0), c(1, 0), c(0, 2)), rbind(c(0, 0), c(-1, 0), c(0, 2)))
  expect_true(f$reflected)
  expect_equal(f$flipped_axes, 1L)
  p <- MdsPolarity(rbind(c(1, 2), c(-1, -2), c(0, 0)), c(2, 1))
  expect_equal(p$signs, c(-1, 1))
  expect_equal(p$poles, list(c(1L, 2L), c(2L, 1L)))
  a <- MdsAnisotropy(rbind(c(-2, 0), c(2, 0), c(0, -1), c(0, 1)))
  expect_equal(c(a$ratio, a$angle), c(4, 0), tolerance = 1e-12)
  expect_equal(MdsAnisotropy(rbind(c(-1, -1), c(1, 1), c(-0.1, 0.1), c(0.1, -0.1)))$angle, 45, tolerance = 1e-10)
})
