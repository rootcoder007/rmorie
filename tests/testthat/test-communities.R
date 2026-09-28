test_that("community detection reports the modularity of its partition", {
  A <- matrix(0, 6, 6)
  A[cbind(c(1, 1, 2, 3, 4, 4, 5), c(2, 3, 3, 4, 5, 6, 6))] <- 1
  A <- A + t(A)
  k <- rowSums(A)
  mod <- function(lab) sum((A - outer(k, k) / sum(k)) * outer(lab, lab, "==")) / sum(k)
  for (r in list(FastGreedyModularity(A), WalktrapCommunities(A))) {
    expect_identical(r$membership, c(1L, 1L, 1L, 2L, 2L, 2L))
    expect_equal(r$max_modularity, mod(r$membership), tolerance = 1e-12)
  }
  expect_equal(GraphModularity(A, c(1, 1, 1, 2, 2, 2)), mod(c(1, 1, 1, 2, 2, 2)), tolerance = 1e-14)
})
