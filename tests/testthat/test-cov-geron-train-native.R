# Coverage for the Geron transformer-block helpers: position-wise feed-
# forward max(0, X W1 + b1) W2 + b2, layer normalisation, multi-head
# attention (heads concatenated, then W_O) by matrix algebra, symmetric
# integer quantisation, batch-gradient logistic regression against its
# own recursion and glm at convergence, and dual-number forward-mode
# arithmetic against analytic derivatives.

test_that("feed-forward and layer norm", {
  set.seed(1)
  X <- matrix(stats::rnorm(6), 2)
  W1 <- matrix(stats::rnorm(12), 3)
  W2 <- matrix(stats::rnorm(12), 4)
  b1 <- c(0.1, -0.2, 0, 0.3)
  b2 <- c(1, 2, 3)
  f <- morie_geron_block_feed_forward(X, list(W1 = W1, W2 = W2, b1 = b1, b2 = b2))
  H <- pmax(X %*% W1 + matrix(b1, 2, 4, byrow = TRUE), 0)
  expect_equal(f, H %*% W2 + matrix(b2, 2, 3, byrow = TRUE), tolerance = 1e-12)
  expect_equal(morie_geron_block_feed_forward(X, list(W1 = W1, W2 = W2)), pmax(X %*% W1, 0) %*% W2, tolerance = 1e-12)
  expect_error(morie_geron_block_feed_forward(X, list(W1 = W1)), "missing W1/W2")
  expect_error(morie_geron_block_feed_forward(X, list(W1 = t(W1), W2 = W2)), "d_model rows")
  expect_error(morie_geron_block_feed_forward(X, list(W1 = W1, W2 = W2, b1 = 1)), "biases do not match")
  L <- morie_geron_block_layer_norm(X, gamma = c(1, 2, 3), beta = c(0, 1, 0))
  Z <- t(apply(X, 1, function(r) (r - mean(r)) / sqrt(mean((r - mean(r))^2) + 1e-5)))
  expect_equal(L, sweep(sweep(Z, 2, c(1, 2, 3), "*"), 2, c(0, 1, 0), "+"), tolerance = 1e-12)
  expect_equal(morie_geron_block_layer_norm(c(1, 2, 3)), matrix((c(1, 2, 3) - 2) / sqrt(2 / 3 + 1e-5), 1), tolerance = 1e-12)
  expect_error(morie_geron_block_layer_norm(X, eps = 0), "eps must be positive")
  expect_error(morie_geron_block_layer_norm(X, gamma = 1:2), "gamma must have d entries")
})

test_that("multi-head attention concatenates softmax(Q K' / sqrt(dk)) V heads", {
  set.seed(2)
  Xq <- matrix(stats::rnorm(8), 2)
  Xk <- matrix(stats::rnorm(12), 3)
  WQ <- list(matrix(stats::rnorm(8), 4), matrix(stats::rnorm(8), 4))
  WK <- list(matrix(stats::rnorm(8), 4), matrix(stats::rnorm(8), 4))
  WV <- list(matrix(stats::rnorm(4), 4), matrix(stats::rnorm(12), 4))
  WO <- matrix(stats::rnorm(8), 4)
  mask <- rbind(c(TRUE, TRUE, FALSE), c(TRUE, TRUE, TRUE))
  r <- morie_geron_block_multi_head_attention(Xq, Xk, list(WQ = WQ, WK = WK, WV = WV, WO = WO), mask = mask)
  heads <- lapply(1:2, function(h) {
    S <- (Xq %*% WQ[[h]]) %*% t(Xk %*% WK[[h]]) / sqrt(2)
    S[!mask] <- -Inf
    A <- exp(S - apply(S, 1, max))
    A <- A / rowSums(A)
    list(A = A, O = A %*% (Xk %*% WV[[h]]))
  })
  expect_equal(r$weights[[2]], heads[[2]]$A, tolerance = 1e-12)
  expect_equal(r$output, cbind(heads[[1]]$O, heads[[2]]$O) %*% WO, tolerance = 1e-12)
  expect_identical(r$weights[[1]][1, 3], 0)
  w <- list(WQ = WQ, WK = WK, WV = WV, WO = WO)
  expect_error(morie_geron_block_multi_head_attention(Xq, Xk, w[1:3]), "missing WQ/WK/WV/WO")
  w1 <- w
  w1$WK <- WK[1]
  expect_error(morie_geron_block_multi_head_attention(Xq, Xk, w1), "same number of heads")
  w1 <- w
  w1$WO <- diag(3)
  expect_error(morie_geron_block_multi_head_attention(Xq, Xk, w1), "concatenated head width")
  expect_error(morie_geron_block_multi_head_attention(Xq, Xk, w, mask = matrix(FALSE, 2, 3)), "blocks every key")
})

test_that("symmetric quantisation maps max|x| to 2^(b-1) - 1", {
  x <- c(-1.3, 0.2, 0.75, 2.6)
  q <- morie_geron_quantize_symmetric(x, bits = 4)
  s <- 2.6 / 7
  expect_equal(q$scale, s, tolerance = 1e-12)
  expect_equal(q$q, round(x / s))
  expect_equal(q$dequantized, round(x / s) * s, tolerance = 1e-12)
  expect_lte(max(abs(q$dequantized - x)), s / 2 + 1e-12)
  expect_error(morie_geron_quantize_symmetric(c(0, 0)), "all zeros")
  expect_error(morie_geron_quantize_symmetric(1, bits = 1), "\\[2, 32\\]")
  expect_error(morie_geron_quantize_symmetric(c(1, NA)), "finite")
})

test_that("batch gradient logistic regression", {
  set.seed(3)
  X <- matrix(stats::rnorm(60), 30)
  y <- stats::rbinom(30, 1, stats::plogis(X %*% c(1, -1)))
  w <- morie_geron_train_logreg(X, y, eta = 0.3, n_iter = 5, l2 = 0.1)
  A <- cbind(1, X)
  r <- c(0, 0, 0)
  for (i in 1:5) {
    g <- as.numeric(t(A) %*% (stats::plogis(A %*% r) - y)) / 30
    g[-1] <- g[-1] + 0.1 * r[-1]
    r <- r - 0.3 * g
  }
  expect_equal(w, r, tolerance = 1e-12)
  full <- morie_geron_train_logreg(X, y, eta = 2, n_iter = 5000)
  expect_equal(full, unname(stats::coef(stats::glm(y ~ X, family = stats::binomial()))), tolerance = 1e-6)
})

test_that("dual numbers carry exact first derivatives", {
  x <- morie_gr_dual(1.3, 1)
  f <- exp(x) * sin(x) / (x + 2) - x^3 + sqrt(x) * log(x) + tanh(x) - cos(x) + 5
  v <- 1.3
  df <- (exp(v) * sin(v) + exp(v) * cos(v)) / (v + 2) - exp(v) * sin(v) / (v + 2)^2 - 3 * v^2 +
    log(v) / (2 * sqrt(v)) + sqrt(v) / v + (1 - tanh(v)^2) + sin(v)
  expect_equal(f$value, exp(v) * sin(v) / (v + 2) - v^3 + sqrt(v) * log(v) + tanh(v) - cos(v) + 5, tolerance = 1e-12)
  expect_equal(f$deriv, df, tolerance = 1e-12)
  # a dual exponent: d/dx x^x = x^x (log x + 1)
  expect_equal((x^x)$deriv, v^v * (log(v) + 1), tolerance = 1e-12)
  expect_equal((-x)$deriv, -1)
  expect_equal((2 - x)$deriv, -1)
  expect_error(x / morie_gr_dual(0), "value 0")
  expect_error(log(morie_gr_dual(-1, 1)), "non-positive")
  expect_error(abs(x), "unsupported function")
})
