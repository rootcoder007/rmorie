karate <- function() {
  E <- matrix(c(
    0L, 1L, 0L, 2L, 0L, 3L, 0L, 4L, 0L, 5L, 0L, 6L, 0L, 7L, 0L, 8L,
    0L, 10L, 0L, 11L, 0L, 12L, 0L, 13L, 0L, 17L, 0L, 19L, 0L, 21L, 0L, 31L,
    1L, 2L, 1L, 3L, 1L, 7L, 1L, 13L, 1L, 17L, 1L, 19L, 1L, 21L, 1L, 30L,
    2L, 3L, 2L, 7L, 2L, 8L, 2L, 9L, 2L, 13L, 2L, 27L, 2L, 28L, 2L, 32L,
    3L, 7L, 3L, 12L, 3L, 13L, 4L, 6L, 4L, 10L, 5L, 6L, 5L, 10L, 5L, 16L,
    6L, 16L, 8L, 30L, 8L, 32L, 8L, 33L, 9L, 33L, 13L, 33L, 14L, 32L, 14L, 33L,
    15L, 32L, 15L, 33L, 18L, 32L, 18L, 33L, 19L, 33L, 20L, 32L, 20L, 33L, 22L, 32L,
    22L, 33L, 23L, 25L, 23L, 27L, 23L, 29L, 23L, 32L, 23L, 33L, 24L, 25L, 24L, 27L,
    24L, 31L, 25L, 31L, 26L, 29L, 26L, 33L, 27L, 33L, 28L, 31L, 28L, 33L, 29L, 32L,
    29L, 33L, 30L, 32L, 30L, 33L, 31L, 32L, 31L, 33L, 32L, 33L
  ), ncol = 2, byrow = TRUE) + 1
  A <- matrix(0, 34, 34)
  A[E] <- 1
  A + t(A)
}

sbm <- function() {
  n <- 60
  U <- .morie_random_uniform(n * n, seed = 11, stream = 0)
  S <- matrix(0, n, n)
  for (i in 0:(n - 2)) for (j in (i + 1):(n - 1)) {
    u <- U[i * n + j + 1]
    if (u < (if (i %/% 15 == j %/% 15) 0.35 else 0.04)) S[i + 1, j + 1] <- S[j + 1, i + 1] <- round(0.5 + u * 3, 6)
  }
  S
}

rosen <- function(x) (1 - x[1])^2 + 100 * (x[2] - x[1]^2)^2

test_that("Diffevol and morie_sa_opt run on Philox and match the Python arm", {
  pop <- t(sapply(0:19, function(i) c(-2 + 0.2 * i, 2 - 0.15 * i)))
  r <- Diffevol(rosen, pop, generations = 300)
  expect_lt(r$estimate, 1e-20)
  r <- Diffevol(rosen, pop, generations = 3, seed = 4)
  expect_equal(r$estimate, 0.3692960000000009, tolerance = 1e-12)
  expect_equal(r$x, c(0.4400000000000001, 0.16999999999999987), tolerance = 1e-12)
})

test_that("morie_sa_opt follows the Python arm on the same platform arithmetic", {
  # the Metropolis accept test amplifies last-bit (FMA) differences, so the exact path is not pinned on macOS
  s <- morie_sa_opt(rosen, c(-1.2, 1), step = 0.1, n_iter = 2000, seed = 3)
  expect_equal(s$fun, rosen(s$x), tolerance = 1e-12)
  expect_lte(s$fun, min(s$trace) + 1e-15)
  skip_on_os("mac")
  s <- morie_sa_opt(rosen, c(-1.2, 1), step = 0.1, n_iter = 2000, seed = 3)
  expect_equal(s$fun, 3.890860177211241e-05, tolerance = 1e-12)
  expect_equal(s$x, c(0.9942293634111342, 0.9887288467288058), tolerance = 1e-12)
  expect_identical(s$n_accepted, 87L)
})

test_that("Leidenclus reaches the karate optimum and matches the Python arm", {
  A <- karate()
  for (seed in 0:2) {
    r <- Leidenclus(A, seed = seed)
    expect_equal(r$estimate, 0.41978961209730437, tolerance = 1e-12)
    expect_true(r$connected)
  }
  expect_identical(r$labels, c(0L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 2L, 2L, 1L, 0L, 0L, 0L, 2L, 2L, 1L, 0L, 2L, 0L, 2L, 0L, 2L, 3L, 3L, 3L, 2L, 3L, 3L, 2L, 2L, 3L, 2L, 2L))
  expect_equal(Leidenclus(A, resolution = 0.1, quality = "cpm", seed = 1)$estimate, 43.1, tolerance = 1e-12)
  S <- sbm()
  r <- Leidenclus(S, resolution = 0.3, quality = "cpm", seed = 0)
  expect_identical(r$labels, c(0L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 0L, 0L, 0L, 2L, 3L, 2L, 4L, 3L, 5L, 3L, 5L, 3L, 3L, 3L, 5L, 5L, 3L, 3L, 6L, 7L, 7L, 6L, 8L, 7L, 9L, 8L, 8L, 7L, 7L, 6L, 7L, 6L, 6L, 10L, 11L, 12L, 12L, 13L, 11L, 12L, 10L, 11L, 10L, 12L, 12L, 10L, 10L, 11L))
  expect_equal(r$estimate, 50.900972, tolerance = 1e-12)
  r <- Leidenclus(S, resolution = 0.3, quality = "cpm", seed = 1)
  expect_identical(r$labels, c(0L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 0L, 0L, 0L, 2L, 2L, 3L, 4L, 2L, 5L, 2L, 6L, 6L, 2L, 6L, 6L, 6L, 2L, 2L, 7L, 8L, 8L, 7L, 9L, 8L, 10L, 9L, 9L, 8L, 8L, 7L, 8L, 7L, 7L, 11L, 12L, 13L, 13L, 14L, 12L, 13L, 14L, 12L, 11L, 14L, 12L, 11L, 11L, 12L))
  expect_equal(r$estimate, 51.069916000000006, tolerance = 1e-12)
  r <- Leidenclus(S, resolution = 0.3, quality = "cpm", seed = 2)
  expect_identical(r$labels, c(0L, 0L, 1L, 0L, 2L, 3L, 0L, 2L, 3L, 0L, 3L, 3L, 2L, 0L, 2L, 1L, 4L, 1L, 5L, 4L, 6L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 7L, 7L, 7L, 7L, 8L, 7L, 8L, 7L, 8L, 7L, 8L, 7L, 7L, 8L, 9L, 10L, 11L, 12L, 12L, 13L, 11L, 12L, 13L, 11L, 10L, 13L, 11L, 10L, 10L, 11L))
  expect_equal(r$estimate, 50.532725000000006, tolerance = 1e-12)
})

test_that("GirvanNewman matches igraph edge betweenness and the Python arm", {
  A <- karate()
  r <- GirvanNewman(A)
  expect_equal(r$estimate, 0.4012984878369494, tolerance = 1e-12)
  expect_identical(as.integer(r$labels), c(0L, 0L, 1L, 0L, 2L, 2L, 2L, 0L, 3L, 4L, 2L, 0L, 0L, 0L, 3L, 3L, 2L, 0L, 3L, 0L, 3L, 0L, 3L, 3L, 1L, 1L, 3L, 1L, 1L, 3L, 3L, 1L, 3L, 3L))
  expect_identical(as.integer(r$removed[1, ]), c(0L, 31L))
  expect_identical(GirvanNewman(A, n_communities = 2)$n_communities, 2L)
})
