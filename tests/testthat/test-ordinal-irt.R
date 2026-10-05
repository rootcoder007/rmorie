# Native ordinal IRT (Quinn 2004).

ord_sim <- function(n, J, seed) {
  set.seed(seed)
  th <- stats::rnorm(n)
  eta <- outer(th, stats::runif(J, 0.8, 1.6)) +
    matrix(stats::rnorm(J, 0, 0.5), n, J, byrow = TRUE)
  Y <- matrix(cut(eta + stats::rnorm(n * J), c(-Inf, -0.5, 0.4, 1.2, Inf),
                  labels = FALSE), n, J)
  list(th = th, Y = Y)
}

test_that("ordinal IRT recovers the ideal points with item cutpoints", {
  skip_on_cran()
  s <- ord_sim(120L, 8L, 7L)
  s$Y[sample(length(s$Y), 30)] <- NA
  f <- morie_spatial_voting_ordinal_irt(s$Y, n_samples = 300L,
                                        burn_in = 300L, seed = 1L)
  expect_gt(stats::cor(f$ideal_points[, 1], s$th), 0.9)
  # first item loads positively after the reflection step
  expect_gt(f$discrimination[1, 1], 0)
  # cutpoints: first fixed at zero, then increasing
  for (g in f$cutpoints) {
    expect_identical(g[1], 0)
    expect_true(all(diff(g) > 0))
  }
  expect_true(all(f$acceptance > 0 & f$acceptance < 1))
})

test_that("item categories are taken from each item's observed values", {
  s <- ord_sim(40L, 4L, 3L)
  Y2 <- s$Y
  Y2[, 2] <- c(10, 20, 30, 40)[Y2[, 2]]
  set.seed(5)
  a <- morie_spatial_voting_ordinal_irt(s$Y, n_samples = 20L, burn_in = 10L)
  b <- morie_spatial_voting_ordinal_irt(Y2, n_samples = 20L, burn_in = 10L)
  expect_equal(a$ideal_points, b$ideal_points, tolerance = 1e-12)
  expect_error(morie_spatial_voting_ordinal_irt(cbind(s$Y, 1)),
               "at least two observed categories")
})
