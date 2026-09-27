path_w <- function(n) 1 * (abs(outer(seq_len(n), seq_len(n), "-")) == 1)
zsc <- function(v) (v - mean(v)) / stats::sd(v)

test_that("LocalGeary follows the formula and the documented values", {
  x <- c(1, 2, 4, 8)
  W <- path_w(4)
  z <- zsc(x)
  want <- rowSums(W * outer(z, z, "-")^2)
  got <- LocalGeary(x, W)$local_values
  expect_equal(got, want, tolerance = 1e-12)
  expect_equal(round(got, 6), c(0.104348, 0.521739, 2.086957, 1.669565))
  expect_equal(got[1], 3 / 28.75, tolerance = 1e-12)
})

test_that("multivariate LocalGeary is the mean of the univariate ones", {
  x <- c(1, 2, 4, 8, 3)
  y <- c(5, 1, 2, 2.5, 9)
  W <- path_w(5)
  m <- LocalGeary(cbind(x, y), W)$local_values
  expect_equal(m, (LocalGeary(x, W)$local_values + LocalGeary(y, W)$local_values) / 2, tolerance = 1e-12)
  expect_error(LocalGeary(c(1, 1, 1), path_w(3)))
})

test_that("SpatialCorrelogram lag 1 is Moran's I", {
  x <- c(1, 2, 3, 5, 4, 6, 8, 7)
  A <- path_w(8)
  r <- SpatialCorrelogram(A, x, order = 2)
  z <- x - mean(x)
  W <- A / rowSums(A)
  expect_equal(r$estimate[1], sum(z * (W %*% z)) / sum(z^2), tolerance = 1e-12)
  expect_equal(round(r$estimate, 6), c(0.797619, 0.25))
  # every unit of the 8-path has a lag-2 neighbour
  expect_equal(r$expectation, c(-1 / 7, -1 / 7), tolerance = 1e-15)
  expect_equal(r$n_with_neighbours, c(8L, 8L))
  g <- SpatialCorrelogram(A, x, order = 3, method = "C", randomisation = FALSE)
  expect_equal(g$expectation, c(1, 1, 1))
  expect_error(SpatialCorrelogram(path_w(4), c(1, 2, 4, 3), order = 3))
})

test_that("JoinCountMulti counts joins and matches the documented values", {
  r <- JoinCountMulti(strsplit("aabbbccaa", "")[[1]], path_w(9))
  expect_equal(r$rows, c("a:a", "b:b", "c:c", "b:a", "c:a", "c:b", "Jtot"))
  expect_equal(r$joincount, c(2, 2, 1, 1, 1, 1, 3))
})

test_that("LocalMoranBivariate statistic, permutation and quadrants", {
  x <- c(1, 2, 4, 8)
  y <- c(2, 1, 5, 7)
  W <- path_w(4)
  r <- LocalMoranBivariate(x, y, W, nsim = 9)
  expect_equal(r$local_values, zsc(x) * as.vector(W %*% zsc(y)), tolerance = 1e-12)
  expect_equal(round(r$local_values, 6), c(0.887109, 0.102641, 0.014663, 0.623176))
  expect_identical(LocalMoranBivariate(x, y, W, nsim = 9)$expected, r$expected)
  expect_true(all(r$p_folded > 0 & r$p_folded <= 0.6))
  expect_equal(r$quadrant, c("Low-Low", "Low-High", "High-High", "High-Low"))
})

test_that("MoranScatter slope is Moran's I and hat values sum to 2", {
  W <- matrix(c(0, 1, 0, 0, .5, 0, .5, 0, 0, .5, 0, .5, 0, 0, 1, 0), 4, byrow = TRUE)
  x <- c(1, 2, 4, 3)
  r <- MoranScatter(x, W)
  z <- x - mean(x)
  expect_equal(r$slope, sum(z * (W %*% z)) / sum(z^2), tolerance = 1e-12)
  expect_equal(round(r$slope, 6), 0.3)
  expect_equal(sum(r$hat), 2, tolerance = 1e-12)
  fit <- stats::lm(r$wx ~ x)
  im <- stats::influence.measures(fit)$infmat
  expect_equal(unname(im[, "cook.d"]), r$cook_d, tolerance = 1e-12)
  expect_equal(unname(im[, "dfb.x"]), r$dfb_x, tolerance = 1e-12)
})
