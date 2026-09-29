# Coverage for tqalg1 .. tqval exports (TurboQuant helpers). Every
# expectation is recomputed in the test body.

tq_norms <- function(seed, k) {
  s <- seed %% 2147483647
  vapply(seq_len(k), function(i) {
    s <<- (48271 * s) %% 2147483647
    stats::qnorm(s / 2147483647)
  }, 0)
}

tq_rotation <- function(d, seed) {
  A <- matrix(tq_norms(seed, d * d), d, d, byrow = TRUE)
  Q <- qr.Q(qr(A))
  sweep(Q, 2, sign(diag(Q)), "*")
}

tq_centroids <- function(cb) {
  cut <- c(-Inf, (cb[-1] + cb[-length(cb)]) / 2, Inf)
  (stats::dnorm(cut[-length(cut)]) - stats::dnorm(cut[-1])) / diff(stats::pnorm(cut))
}

test_that("Kvquant rotates, quantises to a Lloyd-Max codebook and rotates back", {
  x <- c(0.8, -1.2, 0.3, 2.0, -0.5)
  r <- Kvquant(x, b = 2, seed = 3)
  Pi <- tq_rotation(5, 3)
  sc <- sqrt(sum(x^2)) / sqrt(5)
  base <- r$codebook / sc
  # the codebook approximates the Lloyd-Max fixed point for N(0, 1); the
  # package iterates on a 2001-point grid whose point at 0 always ties
  # between the two inner centroids and goes to the lower one, which skews
  # the centroids by a few 1e-3
  expect_lt(max(abs(base - tq_centroids(base))), 5e-3)
  expect_lt(max(abs(base + rev(base))), 1e-2)
  y <- as.numeric(Pi %*% x)
  idx <- apply(abs(outer(y, r$codebook, "-")), 1, which.min) - 1
  expect_equal(r$idx, idx)
  xt <- as.numeric(t(Pi) %*% r$codebook[idx + 1])
  expect_equal(r$reconstruction, xt, tolerance = 1e-10)
  expect_equal(r$mse, sum((x - xt)^2), tolerance = 1e-10)
  expect_equal(r$bound, sqrt(3) * pi / 2 / 16, tolerance = 1e-12)
  expect_error(Kvquant(numeric(0)), "non-empty")
  expect_error(Kvquant(x, b = 0), "at least 1")
  K <- rbind(x, rev(x), c(0, 0, 0, 0, 0))
  m <- Kvmse(K, b = 2, seed = 3)
  expect_equal(m$mse[1:2], c(r$mse, Kvquant(rev(x), b = 2, seed = 3)$mse), tolerance = 1e-12)
  expect_true(is.nan(m$relative_mse[3]))
  expect_error(Kvmse(K, b = 0), "at least 1")
})

test_that("Vcquant adds a QJL sign sketch of the residual", {
  V <- rbind(c(0.8, -1.2, 0.3, 2.0), c(-0.4, 0.9, 1.5, -0.2))
  r <- Vcquant(V, b = 3, seed = 2)
  Pi <- tq_rotation(4, 2)
  S <- matrix(tq_norms(3, 16), 4, 4, byrow = TRUE)
  base <- Kvquant(V[1, ], b = 2, seed = 2)$codebook / (sqrt(sum(V[1, ]^2)) / 2)
  for (i in 1:2) {
    x <- V[i, ]
    cb <- base * sqrt(sum(x^2)) / 2
    y <- as.numeric(Pi %*% x)
    yt <- cb[apply(abs(outer(y, cb, "-")), 1, which.min)]
    xm <- as.numeric(t(Pi) %*% yt)
    res <- x - xm
    q <- ifelse(as.numeric(S %*% res) >= 0, 1, -1)
    xt <- xm + sqrt(pi / 2) / 4 * sqrt(sum(res^2)) * as.numeric(t(S) %*% q)
    expect_equal(r$reconstruction[i, ], xt, tolerance = 1e-10)
    expect_equal(r$residual_norm[i], sqrt(sum(res^2)), tolerance = 1e-10)
  }
  expect_error(Vcquant(V, b = 1), "at least 2")
})

test_that("Tqdist, Tqmom and Scoredist evaluate their closed forms", {
  d <- Tqdist(0.1, 0.05)
  m <- 4 / 3 * 1.1 / 0.01 * log(40)
  expect_equal(d$m_real, m, tolerance = 1e-12)
  expect_equal(d$m_min, ceiling(m))
  for (l in c(1, 2, 3.5)) {
    ref <- stats::integrate(function(z) abs(z)^l * stats::dnorm(z, sd = 1.7), -Inf, Inf, rel.tol = 1e-12)$value
    expect_equal(Tqmom(1.7, l)$moment, ref, tolerance = 1e-9)
  }
  s <- Scoredist(2, 64, query_norm = 1.5, n_keys = 100)
  v <- sqrt(3) * pi^2 * 2.25 / 64 / 16
  expect_equal(s$variance, v, tolerance = 1e-12)
  expect_equal(s$expected_max, sqrt(v) * sqrt(2 * log(100)), tolerance = 1e-12)
  expect_equal(s$ratio, v / (1 / 16 / 64), tolerance = 1e-12)
  expect_equal(Scoredist(2, 64)$expected_max, sqrt(sqrt(3) * pi^2 / 64 / 16), tolerance = 1e-12)
  expect_error(Scoredist(-1, 4), "non-negative")
  expect_error(Scoredist(1, 0), "at least 1")
})

test_that("Tqhs, Tqorth and Tqrot build sign sketches and orthogonal matrices", {
  S <- rbind(c(1, -0.5, 0.2), c(0.3, 0.8, -1.1))
  k <- c(0.5, 1.0, 0.25)
  h <- Tqhs(k, S)
  expect_equal(h$signs, ifelse(as.numeric(S %*% k) >= 0, 1, -1))
  expect_equal(h$estimate, mean(h$signs))
  o <- Tqorth(S)
  Qs <- qr.Q(qr(t(S)))
  Qs <- sweep(Qs, 2, sign(diag(qr.R(qr(t(S))))), "*")
  expect_equal(o$S_orth, t(Qs), tolerance = 1e-12)
  expect_equal(o$orth_err, max(abs(tcrossprod(o$S_orth) - diag(2))), tolerance = 1e-15)
  q <- Tqrot(4, seed = 5)
  A <- matrix(tq_norms(5, 16), 4, 4, byrow = TRUE)
  Qa <- qr.Q(qr(A))
  Qa <- sweep(Qa, 2, sign(diag(qr.R(qr(A)))), "*")
  expect_equal(q$Q, Qa, tolerance = 1e-10)
  expect_lt(q$orth_err, 1e-12)
})

test_that("Outsplit gives the outlier channels more bits", {
  x <- c(0.1, -3.0, 0.2, 0.05, 2.5, -0.3, 0.15, 0.0, 0.4, -0.1)
  r <- Outsplit(x, b_out = 8, b_in = 2, frac = 0.2)
  expect_equal(r$outlier_index, c(2, 5))
  expect_equal(r$effective_bits, (2 * 8 + 8 * 2) / 10)
  expect_equal(r$outlier_energy, (9 + 6.25) / sum(x^2), tolerance = 1e-12)
  expect_equal(r$threshold, 2.5)
  expect_error(Outsplit(x, b_out = 1, b_in = 2), "fewer bits")
  expect_error(Outsplit(x, frac = 1), "strictly between")
  expect_error(Outsplit(c(1, 2), frac = 0.9), "every coordinate")
})

test_that("morie_tqipb bounds the inner-product distortion", {
  r <- morie_tqipb(2, norm_sq = 2, d = 10, eps = 0.3, delta = 0.05, x_norm_sq = 1.5, n_blocks = 3)
  dims <- c(4, 3, 3)
  const <- c(0.56 / 4, 0.56 / 3, 0.56 / 3)
  v <- sum(const * (1.5 * dims / 10) * (2 * dims / 10))
  expect_equal(r$block_dims, dims)
  expect_equal(r$variance, v, tolerance = 1e-12)
  expect_equal(r$delta_bound, min(1, v / (0.09 * 3)), tolerance = 1e-12)
  expect_equal(r$relative_error, sqrt(v / 3), tolerance = 1e-12)
  vb <- vapply(1:32, function(b) {
    c4 <- if (b <= 4) c(1.57, 0.56, 0.18, 0.047)[b] else sqrt(3) * pi^2 / 4^b
    sum(c4 / dims * (1.5 * dims / 10) * (2 * dims / 10))
  }, 0)
  expect_equal(r$bits_needed, which(vb / 0.27 <= 0.05)[1])
  sg <- morie_tqipb(5, d = 8, route = "panter_dite", tail = "sub_gaussian")
  vs <- sqrt(3) * pi^2 / (8 * 4^5)
  expect_equal(sg$variance, vs, tolerance = 1e-12)
  expect_equal(sg$delta_bound, min(1, 2 * exp(-0.01 / (2 * vs))), tolerance = 1e-12)
  expect_equal(morie_tqipb(3, d = 8, route = "qjl")$variance, pi / 16, tolerance = 1e-12)
  expect_equal(morie_tqipb(3, d = 8, route = "lower_bound")$variance, 1 / (8 * 64), tolerance = 1e-12)
  expect_error(morie_tqipb(2), "d \\(dimension\\) is required")
  expect_error(morie_tqipb(2, d = 4, route = "exact"), "route must be one of")
  expect_error(morie_tqipb(2, d = 4, tail = "gauss"), "tail must be one of")
  expect_error(morie_tqipb(2, d = 4, eps = 0), "eps must be positive")
  expect_error(morie_tqipb(2, d = 4, n_blocks = 5), "n_blocks must be in")
  expect_match(morie_tqipb_cheatsheet(), "sub_gaussian")
})
