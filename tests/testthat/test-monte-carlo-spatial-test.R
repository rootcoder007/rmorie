test_that("monte_carlo_spatial_test ranks inverse-distance Moran's I among Philox permutations", {
  C <- cbind(c(0, 1, 2, 0, 1, 2, 0.3, 1.1, 2.4), c(0, 0, 0, 1, 1, 1.2, 2, 2.2, 2.1))
  z <- c(1, 1.2, 0.9, 1.1, 2, 2.4, 2.2, 3.1, 3.3)
  moran <- function(z, C) {
    W <- 1 / as.matrix(dist(C))
    diag(W) <- 0
    d <- z - mean(z)
    length(z) / sum(W) * sum(W * outer(d, d)) / sum(d^2)
  }
  B <- 19
  n <- length(z)
  u <- .morie_random_uniform(B * (n - 1), seed = 7, stream = 0)
  sims <- numeric(B)
  pos <- 1
  for (b in seq_len(B)) {
    p <- z
    for (i in seq(n - 1, 1)) {
      j <- floor(u[pos] * (i + 1))
      pos <- pos + 1
      p[c(i + 1, j + 1)] <- p[c(j + 1, i + 1)]
    }
    sims[b] <- moran(p, C)
  }
  r <- monte_carlo_spatial_test(z, C, n_sim = B, seed = 7)
  expect_equal(r$observed, moran(z, C), tolerance = 1e-12)
  expect_equal(r$sim_mean, mean(sims), tolerance = 1e-12)
  expect_equal(r$p_value, (1 + sum(sims >= moran(z, C))) / (B + 1))
  expect_equal(monte_carlo_spatial_test(z, C, stat_fn = function(z, C) max(z), n_sim = 9)$p_value, 1)
})
