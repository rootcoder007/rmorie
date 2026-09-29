n <- 24
i <- 0:(n - 1)
S <- cbind((i %% 6) / 5 + ((i * 7) %% 11) / 50, (i %/% 6) / 4 + ((i * 3) %% 7) / 40)
X <- matrix(((i * 7) %% 11) / 10)
y <- 1 + 2 * X[, 1] + sin(3 * S[, 1]) + S[, 2]^2 + ((i * 3) %% 5 - 2) / 10
Z <- cbind(X, S)

test_that("cross-validation schemes recompute by refitting", {
  r <- Zxscv(y, X, S)
  loo <- vapply(1:n, function(k) sum(c(1, Z[k, ]) * coef(lm.fit(cbind(1, Z[-k, ]), y[-k]))), 0)
  expect_equal(r$predictions, loo, tolerance = 1e-9)
  b <- Zxsbu(y, X, S, 0.3)
  D <- as.matrix(dist(S))
  expect_equal(b$n_train, as.integer(rowSums(D > 0.3)))
  k <- Zxsbv(y, X, S, n_blocks = 3, k = 3, seed = 5)
  expect_true(all(k$folds %in% 0:2))
})

test_that("penalised, quantile and kernel fits satisfy their optimality conditions", {
  r <- Zxsls(y, X, S, 0.1)
  Q <- cbind(X, S, S[, 1]^2, S[, 1] * S[, 2], S[, 2]^2)
  sd <- sqrt(colMeans(sweep(Q, 2, colMeans(Q))^2))
  res <- y - as.vector(cbind(1, Q) %*% r$coefficients)
  g <- colSums(sweep(sweep(Q, 2, colMeans(Q)), 2, sd, "/") * res) / n
  bs <- r$coefficients[-1] * sd
  expect_true(all(abs(g[bs == 0]) <= 0.1 + 1e-8))
  expect_equal(g[bs != 0], 0.1 * sign(bs[bs != 0]), tolerance = 1e-8)
  q <- Zxsqr(y, X, S, 0.3)
  e <- y - q$fitted
  expect_equal(q$value, sum(ifelse(e >= 0, 0.3 * e, -0.7 * e)), tolerance = 1e-10)
  v <- Zxsvm(y, X, S, C = 5)
  expect_equal(sum(v$alpha), 0, tolerance = 1e-9)
  expect_equal(y - v$fitted, v$alpha / 5, tolerance = 1e-9)
})

test_that("trees, forests, boosting and stacking", {
  st <- Zxsgb(y, X, S, n_trees = 1, rate = 1, max_depth = 1, min_leaf = 2)
  expect_length(unique(round(st$fitted, 12)), 2)
  a <- Zxsbg(y, X, S, n_trees = 6, min_leaf = 3, seed = 4)
  b <- Zxsrf(y, X, S, n_trees = 6, mtry = 3, min_leaf = 3, seed = 4)
  expect_identical(a$fitted, b$fitted)
  s <- Zxsen(y, X, S, n_blocks = 2, k = 2, seed = 3, n_trees = 6)
  g <- as.vector(crossprod(s$oof, y - s$oof %*% s$weights))
  expect_true(all(s$weights >= 0))
  expect_true(all(abs(g[s$weights > 0]) < 1e-8))
  expect_equal(Zsglm(cbind(1, S[, 1]), c(0.2, 0.8), S[, 2] - 0.5, seed = 3)$y,
               SpatialGlmmSimulate(cbind(1, S[, 1]), c(0.2, 0.8), S[, 2] - 0.5, seed = 3)$y)
})
