# Coverage for Snr2u .. spatial_voting exports. Every expectation is
# recomputed in the test body.

test_that("Snr2u is the Snijders-Bosker level-2 R2", {
  r <- Snr2u(0.6, 0.2, 0.9, 0.3, n = 5)
  expect_equal(r$estimate, 1 - (0.12 + 0.2) / (0.18 + 0.3), tolerance = 1e-12)
  expect_equal(r$limit_large_n, 1 - 0.2 / 0.3, tolerance = 1e-12)
  expect_error(Snr2u(1, 1, 1, 1, n = 0), "n must be positive")
  expect_error(Snr2u(-1, 1, 1, 1), "non-negative")
})

test_that("Sobolidx applies the Saltelli estimators on a Halton design", {
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
  model <- function(x) x[1] + 2 * x[2]^2 + x[1] * x[2]
  n <- 32
  A <- cbind(vapply(0:(n - 1), vdc, 0, b = 2), vapply(0:(n - 1), vdc, 0, b = 3))
  B <- cbind(vapply(0:(n - 1), vdc, 0, b = 5), vapply(0:(n - 1), vdc, 0, b = 7))
  fA <- apply(A, 1, model)
  fB <- apply(B, 1, model)
  V <- stats::var(c(fA, fB))
  S <- ST <- numeric(2)
  for (i in 1:2) {
    AB <- A
    AB[, i] <- B[, i]
    fAB <- apply(AB, 1, model)
    S[i] <- mean(fB * (fAB - fA)) / V
    ST[i] <- mean((fA - fAB)^2) / 2 / V
  }
  r <- Sobolidx(model, N = n, d = 2)
  expect_equal(r$S, S, tolerance = 1e-12)
  expect_equal(r$ST, ST, tolerance = 1e-12)
  expect_equal(r$V, V, tolerance = 1e-12)
  sq <- Sobolidx(model, input_dist = list(function(u) 2 * u, function(u) u - 0.5), N = 8)
  expect_length(sq$S, 2)
})

test_that("2-D Haar transform and the wavelet trend", {
  X <- matrix(c(1, 2, 3, 4, 2, 4, 6, 8, 5, 3, 1, 2, 0, 1, 1, 0), 4, byrow = TRUE)
  h <- HaarDwt2d(X, 1)
  a <- h$approximation
  for (i in 1:2) for (j in 1:2) {
    b <- X[(2 * i - 1):(2 * i), (2 * j - 1):(2 * j)]
    expect_equal(a[i, j], sum(b) / 2, tolerance = 1e-12)
    # row pairs (odd, even) then column pairs; details are even minus odd
    expect_equal(h$levels[[1]]$h[i, j], (sum(b[2, ]) - sum(b[1, ])) / 2, tolerance = 1e-12)
    expect_equal(h$levels[[1]]$v[i, j], (sum(b[, 2]) - sum(b[, 1])) / 2, tolerance = 1e-12)
    expect_equal(h$levels[[1]]$d[i, j], (b[1, 1] - b[1, 2] - b[2, 1] + b[2, 2]) / 2, tolerance = 1e-12)
  }
  h2 <- HaarDwt2d(X, 2)
  expect_equal(h2$approximation[1, 1], sum(X) / 4, tolerance = 1e-12)
  x <- c(1, 3, 2, 6, 5, 7, 8, 4)
  w <- WaveletDetrend(x, 2)
  expect_equal(w$trend, rep(c(mean(x[1:4]), mean(x[5:8])), each = 4), tolerance = 1e-12)
  expect_equal(w$detrended, x - w$trend, tolerance = 1e-12)
})

test_that("StPredictGrid is space-time ordinary kriging on a grid", {
  P <- cbind(c(0, 1, 0, 1, 0.5), c(0, 0, 1, 1, 0.5))
  tm <- c(0, 0, 1, 1, 0.5)
  z <- c(1.2, 0.8, 1.5, 1.1, 1.0)
  mod <- list(type = "separable", sill = 1, space = list(model = "Exp", psill = 1, range = 1),
              time = list(model = "Exp", psill = 1, range = 2))
  xs <- c(0.25, 0.75)
  ys <- c(0.5)
  r <- StPredictGrid(z, P, tm, mod, xs, ys, new_times = c(0.5, 1))
  cst <- function(ds, dt) exp(-ds / 1) * exp(-abs(dt) / 2)
  C <- cst(as.matrix(stats::dist(P)), outer(tm, tm, "-"))
  g <- expand.grid(x = xs, y = ys, t = c(0.5, 1))
  one <- rep(1, 5)
  Ci <- solve(C)
  b <- sum(Ci %*% z) / sum(Ci)
  pred <- vapply(seq_len(nrow(g)), function(k) {
    c0 <- cst(sqrt((P[, 1] - g$x[k])^2 + (P[, 2] - g$y[k])^2), g$t[k] - tm)
    b + sum(c0 * (Ci %*% (z - b)))
  }, 0)
  expect_equal(r$frames[[1]], matrix(pred[1:2], 1, 2), tolerance = 1e-10)
  expect_equal(r$frames[[2]], matrix(pred[3:4], 1, 2), tolerance = 1e-10)
  sk <- StPredictGrid(z, P, tm, mod, xs, ys, new_times = 0.5, beta = 1)
  c0 <- cst(sqrt((P[, 1] - 0.25)^2 + (P[, 2] - 0.5)^2), 0.5 - tm)
  expect_equal(sk$frames[[1]][1, 1], 1 + sum(c0 * (Ci %*% (z - 1))), tolerance = 1e-10)
  expect_equal(sk$variance[[1]][1, 1], 1 - sum(c0 * (Ci %*% c0)), tolerance = 1e-10)
})

test_that("morie_cokrig solves the ordinary cokriging system", {
  P <- cbind(c(0, 1, 2, 0.5, 1.5), c(0, 0.5, 0, 1.5, 1))
  z1 <- c(1.2, 0.4, -0.3, 0.8, 0.1)
  z2 <- c(2.0, 1.1, 0.5, 1.9, 0.9)
  s0 <- c(1, 1)
  r <- morie_cokrig(P, z1, z2, s0)
  D <- as.matrix(stats::dist(P))
  rg <- mean(D[upper.tri(D)])
  g11 <- function(h) 1 - exp(-h / rg)
  g12 <- function(h) 0.5 * (1 - exp(-h / rg))
  d0 <- sqrt(colSums((t(P) - s0)^2))
  A <- rbind(cbind(g11(D), g12(D), 1, 0), cbind(g12(D), g11(D), 0, 1), c(rep(1, 5), rep(0, 7)),
             c(rep(0, 5), rep(1, 5), 0, 0))
  sol <- solve(A, c(g11(d0), g12(d0), 1, 0))
  expect_equal(r$prediction, sum(sol[1:5] * z1) + sum(sol[6:10] * z2), tolerance = 1e-10)
  expect_equal(sum(r$lambda), 1, tolerance = 1e-12)
  expect_equal(sum(r$mu), 0, tolerance = 1e-12)
  # at a data location the predictor is exact with zero variance
  at <- morie_cokrig(P, z1, z2, P[3, ])
  expect_equal(at$prediction, z1[3], tolerance = 1e-10)
  expect_equal(at$variance, 0, tolerance = 1e-10)
  expect_error(morie_cokrig(P, z1[-1], z2, s0), "one value per coordinate")
})

test_that("morie_spatial_voting_mlsmu6 unfolds a preference matrix", {
  D <- rbind(c(1, 3, 4, 2), c(2, 1, 3, 4), c(4, 3, 1, 2), c(3, 4, 2, 1), c(1, 2, 4, 3))
  r <- morie_spatial_voting_mlsmu6(D, n_dims = 2, max_iter = 50, n_restarts = 2)
  expect_equal(colMeans(r$respondent_coords), c(0, 0), tolerance = 1e-12)
  expect_equal(colMeans(r$stimulus_coords), c(0, 0), tolerance = 1e-12)
  expect_equal(r$stress, morie_spatial_voting_unfolding_stress(r$respondent_coords, r$stimulus_coords, D),
               tolerance = 1e-12)
  expect_identical(morie_spatial_voting_mlsmu6(D, n_dims = 2, max_iter = 50, n_restarts = 2)$stress, r$stress)
  expect_equal(dim(r$stimulus_coords), c(4L, 2L))
})
