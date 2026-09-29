# Coverage for scintg .. SdXbar exports (Harmony, t-SNE, standard
# deviations of sums and means, geomasking, SDNE). Every expectation is
# recomputed in the test body.

cov_s4_cells <- function() {
  base <- rbind(c(1, 0.2), c(0.9, 0.3), c(1.1, 0.1), c(0.2, 1), c(0.3, 0.9), c(0.1, 1.1))
  rbind(base, base + matrix(c(0.5, -0.2), 6, 2, byrow = TRUE))
}

test_that("morie_scintg: Harmony objective, batch-mean removal and reference cells", {
  Z <- cov_s4_cells()
  bt <- rep(c("a", "b"), each = 6)
  r <- morie_scintg(Z, bt, K = 2, seed = 1, max_iter = 3)
  expect_equal(colSums(r$R), rep(1, 12), tolerance = 1e-12)
  Zn <- r$embedding / sqrt(rowSums(r$embedding^2))
  fit <- 2 * sum(r$R * (1 - r$Y %*% t(Zn)))
  ent <- sum(r$R * log(r$R))
  O <- cbind(rowSums(r$R[, 1:6]), rowSums(r$R[, 7:12]))
  E <- cbind(rowSums(r$R) / 2, rowSums(r$R) / 2)
  kl <- sum(O * log(O / E))
  expect_equal(r$objective$fit, fit, tolerance = 1e-10)
  expect_equal(r$objective$total, fit + 0.1 * ent + 0.1 * 2 * kl, tolerance = 1e-10)
  # one cluster, small ridge: the correction removes the batch mean difference
  one <- morie_scintg(Z, bt, K = 1, lam = 1e-8, max_iter = 1)
  d <- colMeans(one$embedding[7:12, ]) - colMeans(one$embedding[1:6, ])
  expect_lt(max(abs(d)), 1e-6)
  # reference cells have design row (1, 0, ...) and the intercept is not corrected
  rf <- morie_scintg(Z, bt, K = 1, lam = 1e-8, max_iter = 1, reference = rep(c(TRUE, FALSE), each = 6))
  expect_equal(rf$embedding[1:6, ], Z[1:6, ], tolerance = 1e-12)
  expect_same_function(harmony_integrate, morie_scintg)
  expect_error(morie_scintg(Z, rep("a", 12)), "two batches")
  expect_error(morie_scintg(Z, bt[-1]), "one batch label per cell")
  expect_error(morie_scintg(Z, bt, diversity = "maybe"), "diversity must be one of")
  expect_error(morie_scintg(Z, bt, sigma = 0), "sigma must be positive")
})

test_that("morie_sctsne calibrates perplexity and lowers the KL divergence", {
  X <- rbind(c(0, 0), c(0.2, 0.1), c(0.1, 0.3), c(3, 3), c(3.2, 2.9), c(2.9, 3.1), c(-2, 4), c(-2.2, 4.1))
  r <- morie_sctsne(X, perplexity = 3, T = 100, seed = 4)
  expect_lt(r$perplexity_error, 1e-5)
  expect_lt(r$kl, r$kl_initial)
  Pc <- .tsne_pcond(as.matrix(stats::dist(X))^2, 3)
  P <- (Pc + t(Pc)) / 16
  W <- 1 / (1 + as.matrix(stats::dist(r$embedding))^2)
  diag(W) <- 0
  Q <- W / sum(W)
  m <- P > 1e-300
  expect_equal(r$kl, sum(P[m] * log(P[m] / Q[m])), tolerance = 1e-10)
  expect_identical(morie_sctsne(X, perplexity = 3, T = 100, seed = 4)$embedding, r$embedding)
  expect_error(morie_sctsne(X[1:4, ]), "five points")
  expect_error(morie_sctsne(X, perplexity = 10), "perplexity must be")
})

test_that("standard deviations of binomial counts, sums and means", {
  a <- SdAvgBin(400, 0.3)
  expect_equal(a$sd_single, sqrt(0.21), tolerance = 1e-12)
  expect_equal(a$sd_tot, sqrt(400 * 0.21), tolerance = 1e-12)
  expect_equal(a$sd_avg, sqrt(0.21) / 20, tolerance = 1e-12)
  expect_error(SdAvgBin(0), "integer >= 1")
  expect_equal(SdBinom(50, 0.2)$sd, sqrt(8), tolerance = 1e-12)
  expect_error(SdBinom(5, 2), "p must be")
  expect_equal(SdCoinSum(100)$sd, 5)
  expect_equal(SdCoinSum(100)$sd, SdBinom(100, 0.5)$sd, tolerance = 1e-12)
  expect_error(SdCoinSum(-1), "integer >= 0")
  expect_equal(SdFromVar(6.25)$sd, 2.5)
  expect_error(SdFromVar(-1), ">= 0")
  expect_equal(SdIidSum(2, 9)$sd_sum, 6)
  expect_error(SdIidSum(-1, 2), "sigma must be")
  expect_equal(SdSum(c(3, 4))$sd_sum, 5)
  expect_error(SdSum(numeric(0)), "non-empty")
  expect_equal(SdXbar(6, 9)$sd_mean, 2)
  expect_error(SdXbar(1, 0), "integer >= 1")
})

test_that("Sdcdis displaces points uniformly within a disc", {
  P <- rbind(c(0, 0), c(1, 2), c(3, 1), c(-1, 4), c(2, -2))
  r <- Sdcdis(P, 0.5, seed = 3)
  s <- 3
  u <- function() {
    s <<- (48271 * s) %% 2147483647
    s / 2147483647
  }
  M <- P
  dsp <- numeric(5)
  for (k in 1:5) {
    rho <- 0.5 * sqrt(u())
    th <- 2 * pi * u()
    M[k, ] <- P[k, ] + rho * c(cos(th), sin(th))
    dsp[k] <- rho
  }
  expect_equal(r$masked, M, tolerance = 1e-12)
  expect_equal(r$displacement, dsp, tolerance = 1e-12)
  expect_equal(r$rms_displacement, sqrt(mean(dsp^2)), tolerance = 1e-12)
  expect_equal(r$centre_shift, sqrt(sum((colMeans(M) - colMeans(P))^2)), tolerance = 1e-12)
  expect_equal(r$mean_pairwise_after, mean(stats::dist(M)), tolerance = 1e-12)
  expect_equal(c(r$expected_displacement, r$expected_rms), c(1 / 3, 0.5 / sqrt(2)), tolerance = 1e-12)
  expect_error(Sdcdis(P[, 1, drop = FALSE], 1), "exactly two columns")
  expect_error(Sdcdis(P, -1), "non-negative")
})

test_that("SDNE joint objective and cheatsheet", {
  A <- rbind(c(0, 1, 1, 0), c(1, 0, 0, 1), c(1, 0, 0, 0), c(0, 1, 0, 0))
  H <- A * 0.8 + 0.1
  Y <- rbind(c(0, 1), c(0.5, 0.5), c(0.2, 0.9), c(1, 0))
  Pm <- matrix(c(0.1, -0.2, 0.3, 0.4), 2)
  r <- morie_sdne(A, H, Y, beta = 4, alpha = 0.2, nu = 0.01, parameters = Pm)
  B <- ifelse(A != 0, 4, 1)
  s2 <- sum(((H - A) * B)^2)
  s1 <- sum(A * as.matrix(stats::dist(Y))^2)
  expect_equal(r$second_order, s2, tolerance = 1e-12)
  expect_equal(r$first_order, s1, tolerance = 1e-12)
  expect_equal(r$loss, s2 + 0.2 * s1 + 0.01 * sum(Pm^2), tolerance = 1e-12)
  expect_error(morie_sdne(A, H[1:3, ], Y), "reconstruction is")
  expect_error(morie_sdne(A, H, Y, beta = 0.5), "at least 1")
  expect_match(morie_sdne_cheatsheet(), "SECOND-order")
  expect_same_function(morie_structuraldeepnetwork, morie_sdne)
})
