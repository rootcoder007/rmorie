test_that("AffinityPropagation equals apcluster; Diana equals cluster::diana", {
  skip_if_not_installed("apcluster")
  skip_if_not_installed("cluster")
  u <- .morie_random_uniform(120, seed = 7, stream = 0)
  X <- cbind(c(u[1:20] * 3, u[21:40] * 3 + 2.5), c(u[41:60] * 2, u[61:80] * 2 + 1.5))
  S <- -as.matrix(dist(X))^2
  for (lam in c(0.5, 0.9)) {
    ours <- AffinityPropagation(S = S, damping = lam)
    ref <- apcluster::apcluster(S, lam = lam, nonoise = TRUE)
    expect_equal(ours$exemplars, as.numeric(ref@exemplars))
    lab <- integer(nrow(X))
    for (j in seq_along(ref@clusters)) lab[ref@clusters[[j]]] <- j
    expect_identical(ours$cluster, lab)
  }
  D <- as.matrix(dist(X))
  dv <- cluster::diana(D, diss = TRUE)
  expect_equal(Diana(D)$dc, dv$dc, tolerance = 1e-12)
  hc <- stats::as.hclust(dv)
  for (k in 2:6) {
    ref <- stats::cutree(hc, k)
    expect_identical(Diana(D, k)$cluster, match(ref, unique(ref)))
  }
  pm <- cluster::pam(D, 2, diss = TRUE)
  expect_equal(Clarans(X, 2, numlocal = 4, seed = 3)$cost, sum(apply(D[, pm$id.med], 1, min)), tolerance = 1e-12)
})
