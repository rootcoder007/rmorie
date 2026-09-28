PTS <- rbind(c(0, 0), c(1, 0.2), c(2.1, 0), c(0.1, 1.3), c(1.2, 1.1), c(2, 1.4), c(0.6, 2.4), c(1.7, 2.6))
D <- as.matrix(dist(PTS))
W <- (D > 0 & D < 1.6) + 0

test_that("Moran eigenvectors satisfy their definition", {
  r <- MoranEigenvectors(W)
  M <- diag(8) - 1 / 8
  B <- M %*% W %*% M
  for (k in 1:7) {
    v <- r$vectors[, k]
    expect_equal(sum(v), 0, tolerance = 1e-12)
    expect_equal(as.vector(B %*% v), r$eigenvalues[k] * v, tolerance = 1e-12)
    expect_equal(r$moran_i[k], 8 / sum(W) * sum(v * (W %*% v)), tolerance = 1e-12)
  }
  expect_equal(round(MoranEigenvectors(rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0)))$moran_i, 6), c(0, -1))
})

test_that("filtering and Getis filter", {
  y <- c(1, 1.4, 2.2, 1.6, 2, 2.9, 2.4, 3.3)
  r <- EigenvectorFiltering(y, W, criterion = "r2", tol = 0, max_vectors = 1)
  me <- MoranEigenvectors(W)
  r2 <- vapply(me$positive, function(k) summary(lm(y ~ me$vectors[, k]))$r.squared, 0)
  expect_equal(r$selected, me$positive[which.max(r2)])
  expect_equal(r$r2, max(r2))
  expect_equal(GetisFilter(c(2, 4, 6), rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0)))$filtered, c(2.5, 4, 4.5))
})
