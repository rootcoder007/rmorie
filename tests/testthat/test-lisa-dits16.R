# Local Moran z-scores and DiT Gflops/parameter counts, checked against
# first principles rather than against remembered outputs.

.all_perms <- function(v) {
  if (length(v) <= 1L) return(list(v))
  out <- list()
  for (i in seq_along(v)) {
    for (p in .all_perms(v[-i])) out[[length(out) + 1L]] <- c(v[i], p)
  }
  out
}

test_that("LisaClust z is the z-score of I(s_i) under exact conditional randomization", {
  x <- c(2.1, 1.8, 3.3, -0.9, -1.7, -1.2)
  n <- length(x)
  W <- matrix(0, n, n)
  for (i in seq_len(n - 1L)) W[i, i + 1L] <- W[i + 1L, i] <- 1
  W[1L, n] <- W[n, 1L] <- 1
  r <- LisaClust(x, W)
  d <- x - mean(x)
  ss <- sum(d^2)
  for (i in seq_len(n)) {
    # enumerate every placement of the other n-1 deviations
    vals <- vapply(.all_perms(d[-i]), function(p) {
      n * d[i] * sum(W[i, -i] * p) / ss
    }, numeric(1))
    mu <- mean(vals)
    sdp <- sqrt(mean((vals - mu)^2))
    expect_equal(r$local[i], n * d[i] * sum(W[i, ] * d) / ss, tolerance = 1e-12)
    expect_equal(r$z[i], (r$local[i] - mu) / sdp, tolerance = 1e-10)
  }
  # below-mean sites surrounded by below-mean sites are positive
  # autocorrelation, so their z must be positive
  low <- which(d < 0 & r$local > 0)
  expect_true(length(low) > 0L)
  expect_true(all(r$z[low] > 0))
})

test_that("gflops counts a multiply-add once and reproduces DiT Table 4", {
  # DiT-XL/2: 256 latent tokens, 28 blocks, width 1152
  T <- 256; L <- 28; d <- 1152
  g <- gflops(T, L, d)$gflops
  expect_equal(g, L * (4 * T * d^2 + 2 * T^2 * d + 8 * T * d^2) / 1e9,
               tolerance = 1e-12)
  # the paper's 118.6 also includes patch embedding and the output head
  expect_true(abs(g / 118.6 - 1) < 0.005)
  s <- scaling_comparison(list(list("XL/2", 32, 2, 28, 1152)))
  expect_equal(s$ranked[[1]]$parameters, 28 * 18 * 1152^2)
  # the paper's 675M adds embeddings and the final layer to the blocks
  expect_true(abs(s$ranked[[1]]$parameters / 675e6 - 1) < 0.01)
})
