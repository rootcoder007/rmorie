# Tests for GraphLogic: graph indices, CDCL satisfiability and linear-chain CRFs.

A <- outer(0:9, 0:9, function(i, j) as.numeric(i != j & ((i * 7 + j * 3) %% 5 == 0 | abs(i - j) == 1)))
A <- pmax(A, t(A))
floyd_ <- function(M) {
  d <- ifelse(M != 0, 1, Inf)
  diag(d) <- 0
  for (k in seq_len(nrow(M))) d <- pmin(d, outer(d[, k], d[k, ], "+"))
  d
}

test_that("GraphDegreeCentrality and GraphDiameter", {
  expect_equal(GraphDegreeCentrality(A), rowSums(A) / 9)
  d <- floyd_(A)
  r <- GraphDiameter(A)
  expect_equal(r$eccentricity, apply(d, 1, max))
  expect_equal(d[r$pair[1], r$pair[2]], r$diameter)
})

test_that("GraphBetweenness on a star and a path", {
  star <- rbind(c(0, 1, 1, 1), c(1, 0, 0, 0), c(1, 0, 0, 0), c(1, 0, 0, 0))
  expect_equal(GraphBetweenness(star), c(3, 0, 0, 0))
  path <- rbind(c(0, 1, 0, 0), c(1, 0, 1, 0), c(0, 1, 0, 1), c(0, 0, 1, 0))
  expect_equal(GraphBetweenness(path, normalized = TRUE), c(0, 2, 2, 0) / 3)
})

test_that("CdclSolve models satisfy their formulas", {
  cnf <- lapply(0:29, function(k) sapply(0:2, function(j) ((k * 7 + j * 3) %% 7 + 1) * (if ((k + j) %% 3 != 0) 1 else -1)))
  r <- CdclSolve(cnf)
  if (r$satisfiable) expect_true(all(vapply(cnf, function(cl) any(cl %in% r$model), TRUE)))
  php <- list(c(1, 2), c(3, 4), c(5, 6), c(-1, -3), c(-1, -5), c(-3, -5), c(-2, -4), c(-2, -6), c(-4, -6))
  expect_false(CdclSolve(php)$satisfiable)
})

test_that("CrfMarginals and CrfViterbi match enumeration", {
  theta <- 0.3 * sin(1:18)
  X <- t(sapply(0:4, function(tt) c(sin(tt + 1), cos(1.4 * tt))))
  ys <- as.matrix(expand.grid(rep(list(0:2), 5)))
  p <- list(W = matrix(theta[1:6], 3, 2, byrow = TRUE), b = theta[7:9], T = matrix(theta[10:18], 3, 3, byrow = TRUE))
  sc <- apply(ys, 1, function(y) sum(X %*% t(p$W[y + 1, ]) * diag(5)) + sum(p$b[y + 1]) + sum(p$T[cbind(y[-5] + 1, y[-1] + 1)]))
  m <- CrfMarginals(theta, X, 3)
  expect_equal(m$log_z, log(sum(exp(sc))), tolerance = 1e-12)
  expect_equal(CrfViterbi(theta, X, 3)$labels, as.integer(ys[which.max(sc), ]))
  expect_equal(rowSums(m$node), rep(1, 5), tolerance = 1e-12)
})
