test_that("map equation identities and planted cliques", {
  n <- 20
  A <- matrix(0, n, n)
  grp <- (seq_len(n) - 1) %/% 5
  A[outer(grp, grp, "==")] <- 1
  diag(A) <- 0
  for (c in 0:3) {
    a <- c * 5 + 1
    b <- ((c + 1) %% 4) * 5 + 2
    A[a, b] <- 1
    A[b, a] <- 1
  }
  p <- rowSums(A) / sum(A)
  expect_equal(MapEquation(A, rep(0, n)), -sum(p * log2(p)), tolerance = 1e-14)
  r <- InfomapPartition(A)
  expect_equal(r$membership, grp)
  expect_equal(r$codelength, MapEquation(A, grp), tolerance = 1e-14)
})
