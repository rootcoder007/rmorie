.csrnn_csr <- function(n = 120, seed = 0) {
  set.seed(seed); matrix(runif(n * 2), n, 2)
}

.csrnn_clustered <- function(n = 120, seed = 0, k = 6, sd = 0.02) {
  set.seed(seed)
  parents <- matrix(runif(k * 2, 0.1, 0.9), k, 2)
  idx <- sample.int(k, n, replace = TRUE)
  pmin(pmax(parents[idx, ] + matrix(rnorm(n * 2, 0, sd), n, 2), 0), 1)
}

.csrnn_regular <- function(side = 10, jitter = 0.005, seed = 0) {
  set.seed(seed)
  g <- seq(0.05, 0.95, length.out = side)
  pts <- as.matrix(expand.grid(x = g, y = g))
  pts + matrix(rnorm(length(pts), 0, jitter), nrow(pts), 2)
}

test_that("a CSR pattern is not rejected", {
  set.seed(7)
  expect_gt(morie_csr_nn_test(.csrnn_csr(seed = 1), nsim = 99)$p_value, 0.05)
})

test_that("a clustered pattern is rejected", {
  set.seed(7)
  expect_lte(morie_csr_nn_test(.csrnn_clustered(seed = 2), nsim = 99)$p_value, 0.05)
})

test_that("a regular pattern is rejected", {
  set.seed(7)
  expect_lte(morie_csr_nn_test(.csrnn_regular(seed = 3), nsim = 99)$p_value, 0.05)
})

test_that("clustering shortens the mean nearest-neighbour distance", {
  set.seed(7)
  a <- morie_csr_nn_test(.csrnn_clustered(seed = 4), nsim = 9)$mean_nn
  b <- morie_csr_nn_test(.csrnn_csr(seed = 4), nsim = 9)$mean_nn
  expect_lt(a, b)
})

test_that("the p-value is a rank and cannot reach zero", {
  set.seed(7)
  r <- morie_csr_nn_test(.csrnn_clustered(seed = 5), nsim = 99)
  expect_gte(r$p_value, 1 / 100)
  expect_lte(r$p_value, 1)
  expect_equal(r$p_value * 100 %% 1, 0, tolerance = 1e-9)
})

test_that("nearest-neighbour distances are correct", {
  # Three points on a line at 0, 1, 3: NN distances are 1, 1, 2.
  r <- morie_csr_nn_test(matrix(c(0, 1, 3), ncol = 1L), nsim = 5)
  expect_equal(sort(unname(r$nn_distances)), c(1, 1, 2))
})

test_that("window accepts a matrix and a flat vector", {
  set.seed(7)
  P <- .csrnn_csr(seed = 9)
  for (w in list(matrix(c(0, 1, 0, 1), 2, 2, byrow = TRUE), c(0, 1, 0, 1))) {
    p <- morie_csr_nn_test(P, window = w, nsim = 9)$p_value
    expect_gt(p, 0); expect_lte(p, 1)
  }
})

test_that("morie_csr_nn_test validates its inputs", {
  expect_error(morie_csr_nn_test(matrix(0, 2, 2)), "at least 3 events")
  expect_error(morie_csr_nn_test(matrix(c(0, 0, 1, NA, 2, 1), 3, 2)), "must be finite")
  expect_error(morie_csr_nn_test(.csrnn_csr(seed = 9), window = c(1, 0, 1, 0)),
               "upper bounds must exceed")
  expect_error(morie_csr_nn_test(.csrnn_csr(seed = 9), nsim = 0), "at least 1")
})
