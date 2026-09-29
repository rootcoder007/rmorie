test_that("Sgtadj builds symmetric or directed adjacency", {
  r <- Sgtadj(list(c("A", "B"), c("B", "C")))
  expect_equal(r$A, rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0)))
  expect_equal(r$m, 2)
  d <- Sgtadj(list(c(0, 1), c(1, 2)), n = 3, directed = TRUE)
  expect_equal(d$A[1, 2], 1)
  expect_equal(d$A[2, 1], 0)
  u <- Sgtadj(list(c(0, 1), c(1, 0), c(0, 1)), n = 4)
  expect_equal(sum(u$A), 2)
  expect_equal(u$m, 1)
})

test_that("Sgtnbe: cycle gives a permutation matrix, no backtracking", {
  cyc <- lapply(0:4, function(i) c(i, (i + 1) %% 5))
  B <- Sgtnbe(cyc)$B
  expect_equal(rowSums(B), rep(1, 10))
  expect_equal(colSums(B), rep(1, 10))
  s <- Sgtnbe(list(c(0, 1), c(0, 2), c(0, 3)))
  i <- which(vapply(s$directed_edges, function(e) all(e == c(1, 0)), NA))
  expect_equal(sum(s$B[i, ]), 2)
})

test_that("Sgtsbnd and Prnkpg", {
  r <- Sgtsbnd(20, 2, 4)
  expect_equal(r$margin, 18 - 4 * sqrt(6.5))
  G <- rbind(c(0, 1, 1), c(0, 0, 1), c(1, 0, 0))
  P <- G / rowSums(G)
  ev <- eigen(0.85 * t(P) + 0.05)$vectors[, 1]
  expect_equal(Prnkpg(G)$pr, Re(ev / sum(ev)), tolerance = 1e-12)
})
