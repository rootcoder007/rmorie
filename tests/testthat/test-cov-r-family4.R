# Coverage for respwt .. rfpmi exports. Every expectation is recomputed in
# the test body.

test_that("morie_respwt applies weighting-class nonresponse factors", {
  w <- c(10, 12, 8, 15, 9, 11, 14, 10)
  resp <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)
  cl <- c("a", "a", "a", "b", "b", "b", "c", "c")
  r <- morie_respwt(w, resp, cl)
  phi <- tapply(w * resp, cl, sum) / tapply(w, cl, sum)
  adj <- ifelse(resp, w / phi[cl], NA)
  expect_equal(r$adjusted, unname(adj), tolerance = 1e-12)
  expect_equal(unname(unlist(r$phi_hat)), as.numeric(phi), tolerance = 1e-12)
  expect_lt(r$balance_error, 1e-12)
  expect_error(morie_respwt(w, resp, cl[-1]), "must be paired")
  expect_error(morie_respwt(-w, resp, cl), "positive")
  expect_error(morie_respwt(w, c(FALSE, FALSE, FALSE, resp[4:8]), cl), "no respondents")
})

test_that("RetestIQ, RetestR and RevSlope follow the linear signal model", {
  iq <- RetestIQ()
  sx <- 15 / sqrt(2)
  expect_equal(iq$sigma_y, sqrt(2 * sx^2), tolerance = 1e-12)
  expect_equal(iq$r, sx / 15, tolerance = 1e-12)
  expect_equal(RetestR(3, 4)$r, 3 / 5, tolerance = 1e-12)
  expect_equal(RevSlope(0.6, 2, 5)$slope, 0.6 * 2 / 5, tolerance = 1e-12)
  expect_error(RevSlope(1.5, 1, 1), "\\[-1, 1\\]")
  expect_error(RevSlope(0.5, 1, 0), "sigmas must be positive")
})

test_that("Retention matches its parallel form", {
  Q <- rbind(c(0.2, -0.5), c(1.0, 0.3), c(-0.4, 0.8), c(0.6, 0.1))
  K <- rbind(c(0.5, 0.1), c(-0.3, 0.9), c(0.2, 0.4), c(0.7, -0.2))
  V <- rbind(c(1, 0, 2), c(0, 1, -1), c(2, 1, 0), c(-1, 0.5, 1))
  r <- Retention(NULL, Q = Q, K = K, V = V, gamma = 0.8)
  Dm <- outer(1:4, 1:4, function(t, m) ifelse(m <= t, 0.8^(t - m), 0))
  par <- ((Q %*% t(K)) * Dm) %*% V
  expect_equal(r$out, par, tolerance = 1e-12)
  expect_equal(r$out_par, par, tolerance = 1e-12)
  expect_lt(r$max_gap, 1e-12)
  S <- t(K) %*% (V * 0.8^(3:0))
  expect_equal(r$state, S, tolerance = 1e-12)
})

test_that("Rfcomp does principal factors on the exhaustive MCD correlation", {
  X <- rbind(c(1.0, 2.1, 0.5), c(1.5, 2.9, 0.8), c(2.2, 3.8, 1.4), c(2.8, 5.1, 1.2),
             c(3.1, 5.9, 2.0), c(3.9, 7.2, 2.3), c(9.0, 1.0, 7.0), c(4.4, 8.3, 2.7))
  h <- (8 + 3 + 1) %/% 2
  best <- NULL
  bd <- Inf
  for (s in utils::combn(8, h, simplify = FALSE)) {
    d <- det(stats::cov(X[s, ]))
    if (d < bd) {
      bd <- d
      best <- s
    }
  }
  Cm <- stats::cor(X[best, ])
  e <- eigen(Cm, symmetric = TRUE)
  L <- e$vectors[, 1:2] %*% diag(sqrt(e$values[1:2]))
  r <- Rfcomp(X, k_factors = 2)
  expect_equal(r$correlation, Cm, tolerance = 1e-10)
  expect_equal(r$center, colMeans(X[best, ]), tolerance = 1e-12)
  # loadings are defined up to column signs; L L' is not
  expect_equal(r$loadings %*% t(r$loadings), L %*% t(L), tolerance = 1e-10)
  expect_equal(r$communalities, rowSums(L^2), tolerance = 1e-10)
  expect_equal(r$uniquenesses, 1 - rowSums(L^2), tolerance = 1e-10)
  expect_equal(r$eigenvalues, e$values, tolerance = 1e-10)
  expect_equal(diag(r$reproduced), rep(1, 3), tolerance = 1e-12)
  expect_error(Rfcomp(X, k_factors = 4), "1 <= k_factors <= p")
  expect_error(Rfcomp(matrix(0, 0, 2)), "X is empty")
})

test_that("Rfkrn builds random Fourier features on a Halton stream", {
  vdc <- function(i, b) {
    k <- i + 1
    f <- 1
    r <- 0
    while (k > 0) {
      f <- f / b
      r <- r + f * (k %% b)
      k <- k %/% b
    }
    r
  }
  X <- rbind(c(0.1, 0.4), c(-0.3, 0.2), c(0.5, -0.6))
  D <- 50
  r <- Rfkrn(X, D = D, gamma = 0.7)
  W <- sqrt(1.4) * rbind(stats::qnorm(vapply(1:D, vdc, 0, b = 2)), stats::qnorm(vapply(1:D, vdc, 0, b = 3)))
  b <- 2 * pi * vapply(1:D, vdc, 0, b = 5)
  Z <- sqrt(2 / D) * cos(sweep(X %*% W, 2, b, "+"))
  expect_equal(r$W, W, tolerance = 1e-12)
  expect_equal(r$Z, Z, tolerance = 1e-12)
  Ke <- exp(-0.7 * as.matrix(stats::dist(X))^2)
  expect_equal(r$K_exact, unname(Ke), tolerance = 1e-12)
  expect_equal(r$estimate, mean(abs(Z %*% t(Z) - Ke)), tolerance = 1e-12)
  expect_error(Rfkrn(X, kernel = "laplace"), "only the rbf kernel")
  expect_error(Rfkrn(X, D = 0), "at least 1")
  expect_error(Rfkrn(X, gamma = 0), "gamma must be positive")
})

test_that("forest importances credit only informative columns", {
  x1 <- c(0.1, 0.5, 0.9, 1.3, 1.7, 2.1, 2.5, 2.9, 3.3, 3.7, 4.1, 4.5)
  X <- cbind(x1, 1)
  y <- ifelse(x1 > 2, 5, 1) + c(0.1, -0.1, 0.05, 0, 0.1, -0.05, 0.1, 0, -0.1, 0.05, 0, 0.1)
  m <- Rfmdi(10, X, y, mtry = 2, nodesize = 2)
  expect_equal(m$importance[2], 0)
  expect_gt(m$importance[1], 0)
  expect_equal(m$relative, c(1, 0), tolerance = 1e-12)
  expect_equal(m$ranking, c(0L, 1L))
  expect_equal(m$total, sum(m$importance), tolerance = 1e-12)
  p <- Rfpmi(10, X, y, mtry = 2, nodesize = 2)
  # permuting a constant column changes no prediction
  expect_equal(p$importance[2], 0)
  expect_gt(p$importance[1], 0)
  expect_error(Rfmdi(0, X, y), "at least 1")
  expect_error(Rfpmi(1, X, y), "at least two trees")
  expect_error(Rfmdi(5, X, y, mtry = 3), "mtry must lie")
  expect_error(Rfpmi(5, X, y[-1]), "different number of rows")
})

test_that("Rfmlt predictions are the tree average of standardised responses", {
  X <- cbind(c(0.1, 0.5, 0.9, 1.3, 1.7, 2.1, 2.5, 2.9, 3.3, 3.7),
             c(1, 0, 1, 1, 0, 0, 1, 0, 1, 0))
  Y <- cbind(c(1.2, 1.9, 3.4, 3.8, 5.3, 5.9, 7.4, 7.6, 9.1, 9.8),
             c(0.5, 0.2, 0.9, 1.1, 0.4, 0.3, 1.3, 0.6, 1.4, 0.7))
  n <- 10
  Ys <- apply(Y, 2, function(v) (v - mean(v)) / sqrt(mean((v - mean(v))^2)))
  # nodesize above n / 2: every tree is a single leaf, the bootstrap mean
  r0 <- Rfmlt(X, Y, n_trees = 4, nodesize = 6)
  want <- Reduce(`+`, lapply(0:3, function(b) colMeans(Ys[.rfboot(b, n), ]))) / 4
  expect_equal(r0$y_hat, matrix(want, n, 2, byrow = TRUE), tolerance = 1e-12)
  expect_equal(r0$mss, sum(Ys^2), tolerance = 1e-12)
  expect_equal(r0$estimate, mean(r0$y_hat), tolerance = 1e-12)
  r1 <- Rfmlt(X, Y, n_trees = 4, nodesize = 2, standardize = FALSE)
  f <- .rfforest(X, Y, 4, 2L, 1L, 2)
  expect_equal(r1$y_hat, .rfpredict(f$trees, X, 2), tolerance = 1e-12)
  expect_equal(r1$oob_size, vapply(f$oob, length, 0L))
  expect_error(Rfmlt(X, Y, n_trees = 0), "at least 1")
  expect_error(Rfmlt(X, Y, nodesize = 0), "at least 1")
})
