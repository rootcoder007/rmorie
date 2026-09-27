lattice <- function() {
  nr <- 7
  n <- 49
  U <- .morie_random_uniform(3 * n, seed = 21, stream = 0)
  i <- 0:(n - 1)
  X <- cbind(sin((i %% nr + 1) / 2) + 0.3 * U[3 * i + 1], cos((i %/% nr + 1) / 3) + 0.3 * U[3 * i + 2],
             ifelse(i %% nr + 1 > 4, 1.5, 0) + 0.2 * U[3 * i + 3])
  nb <- lapply(i, function(j) as.integer(sort(c(if (j %% nr) j - 1, if (j %% nr != nr - 1) j + 1, if (j >= nr) j - nr, if (j + nr < n) j + nr)) + 1))
  list(X = X, nb = nb, pop = 1 + (i * 7) %% 5)
}

test_that("Skater matches spdep::skater and the Python arm", {
  L <- lattice()
  expect_identical(Skater(L$X, L$nb, 2, min_size = 1)$labels, c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L))
  expect_identical(Skater(L$X, L$nb, 2, min_size = 5)$labels, c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L))
  expect_identical(Skater(L$X, L$nb, 4, min_size = 1)$labels, c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 3L, 3L, 3L, 2L, 2L, 4L, 3L, 3L, 3L, 3L, 2L, 4L, 4L, 3L, 3L, 3L, 3L, 2L, 4L, 4L, 3L, 3L, 3L, 3L, 2L, 4L, 4L))
  expect_identical(Skater(L$X, L$nb, 4, min_size = 5)$labels, c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 3L, 3L, 3L, 2L, 2L, 4L, 3L, 3L, 3L, 3L, 2L, 4L, 4L, 3L, 3L, 3L, 3L, 2L, 4L, 4L, 3L, 3L, 3L, 3L, 2L, 4L, 4L))
  expect_identical(Skater(L$X, L$nb, 6, min_size = 1)$labels, c(1L, 1L, 1L, 1L, 5L, 5L, 5L, 1L, 1L, 1L, 1L, 5L, 5L, 5L, 1L, 1L, 1L, 1L, 5L, 5L, 5L, 1L, 3L, 3L, 3L, 2L, 5L, 4L, 3L, 3L, 3L, 3L, 2L, 4L, 4L, 6L, 6L, 6L, 6L, 2L, 4L, 4L, 6L, 6L, 6L, 6L, 2L, 4L, 4L))
  expect_identical(Skater(L$X, L$nb, 6, min_size = 5)$labels, c(1L, 1L, 1L, 1L, 5L, 5L, 5L, 1L, 1L, 1L, 1L, 5L, 5L, 5L, 1L, 1L, 1L, 1L, 5L, 5L, 5L, 1L, 3L, 3L, 3L, 2L, 2L, 4L, 3L, 3L, 3L, 3L, 2L, 4L, 4L, 6L, 6L, 6L, 6L, 2L, 4L, 4L, 6L, 6L, 6L, 6L, 2L, 4L, 4L))
})

test_that("Redcap, ConstrainedHierarchical, AutomaticZoning and MaxPRegions match the Python arm", {
  L <- lattice()
  expect_identical(Redcap(L$X, L$nb, 5, linkage = "single")$labels, c(1L, 1L, 1L, 1L, 5L, 5L, 5L, 1L, 1L, 1L, 1L, 5L, 5L, 5L, 1L, 1L, 1L, 1L, 5L, 5L, 4L, 1L, 3L, 3L, 3L, 2L, 5L, 4L, 3L, 3L, 3L, 3L, 2L, 4L, 4L, 3L, 3L, 3L, 3L, 2L, 4L, 4L, 3L, 3L, 3L, 3L, 2L, 4L, 4L))
  expect_identical(as.integer(ConstrainedHierarchical(L$X, L$nb, 5, linkage = "single")$labels), c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 3L, 1L, 1L, 1L, 1L, 4L, 2L, 3L, 1L, 1L, 1L, 1L, 4L, 5L, 3L, 1L, 1L, 1L, 1L, 4L, 5L, 5L, 1L, 1L, 1L, 1L, 4L, 5L, 5L))
  expect_identical(Redcap(L$X, L$nb, 5, linkage = "complete")$labels, c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 4L, 1L, 3L, 3L, 3L, 2L, 2L, 4L, 3L, 3L, 3L, 3L, 5L, 5L, 4L, 3L, 3L, 3L, 3L, 5L, 5L, 5L, 3L, 3L, 3L, 3L, 5L, 5L, 5L))
  expect_identical(as.integer(ConstrainedHierarchical(L$X, L$nb, 5, linkage = "complete")$labels), c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 3L, 1L, 4L, 4L, 4L, 5L, 5L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 4L, 4L, 4L, 4L, 5L, 5L, 5L))
  expect_identical(Redcap(L$X, L$nb, 5, linkage = "average")$labels, c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 4L, 1L, 3L, 3L, 3L, 2L, 2L, 4L, 3L, 3L, 3L, 3L, 5L, 5L, 4L, 3L, 3L, 3L, 3L, 5L, 5L, 5L, 3L, 3L, 3L, 3L, 5L, 5L, 5L))
  expect_identical(as.integer(ConstrainedHierarchical(L$X, L$nb, 5, linkage = "average")$labels), c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 3L, 1L, 4L, 4L, 4L, 2L, 2L, 3L, 4L, 4L, 4L, 4L, 2L, 5L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 4L, 4L, 4L, 4L, 5L, 5L, 5L))
  expect_identical(Redcap(L$X, L$nb, 5, linkage = "ward")$labels, c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 5L, 1L, 3L, 3L, 3L, 2L, 2L, 5L, 3L, 3L, 3L, 3L, 4L, 4L, 5L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 4L, 4L, 4L))
  expect_identical(as.integer(ConstrainedHierarchical(L$X, L$nb, 5, linkage = "ward")$labels), c(1L, 1L, 1L, 1L, 2L, 3L, 3L, 1L, 1L, 1L, 1L, 2L, 3L, 3L, 1L, 1L, 1L, 1L, 2L, 3L, 3L, 1L, 4L, 4L, 4L, 2L, 2L, 3L, 4L, 4L, 4L, 4L, 2L, 5L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 4L, 4L, 4L, 4L, 5L, 5L, 5L))
  expect_identical(Redcap(L$X, L$nb, 5, linkage = "single", order = "first")$labels, c(1L, 1L, 1L, 1L, 5L, 5L, 5L, 1L, 1L, 1L, 1L, 5L, 5L, 5L, 1L, 1L, 1L, 1L, 5L, 5L, 4L, 1L, 3L, 3L, 3L, 2L, 5L, 4L, 3L, 3L, 3L, 3L, 2L, 4L, 4L, 3L, 3L, 3L, 3L, 2L, 4L, 4L, 3L, 3L, 3L, 3L, 2L, 4L, 4L))
  expect_identical(AutomaticZoning(L$X, L$nb, 5, seed = 0)$labels, c(1L, 1L, 1L, 1L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 2L, 3L, 3L, 4L, 4L, 4L, 4L, 2L, 2L, 2L, 5L, 5L, 5L, 5L, 2L, 2L, 2L, 5L, 5L, 5L, 5L, 2L, 2L, 2L))
  expect_identical(AutomaticZoning(L$X, L$nb, 5, seed = 1)$labels, c(1L, 3L, 3L, 3L, 2L, 2L, 2L, 1L, 3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 3L, 3L, 2L, 2L, 2L, 1L, 5L, 5L, 5L, 4L, 2L, 2L, 5L, 5L, 5L, 5L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 4L, 4L, 4L))
  b <- AutomaticZoning(L$X, L$nb, 5, objective = "balance", weights = L$pop, seed = 1)
  expect_identical(b$labels, c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 3L, 3L, 3L, 2L, 2L, 1L, 1L, 3L, 3L, 3L, 3L, 2L, 1L, 5L, 5L, 5L, 3L, 2L, 2L, 1L, 5L, 5L, 5L, 3L, 2L, 2L, 5L, 5L, 4L, 5L, 4L, 4L, 4L, 5L, 4L, 4L, 4L, 4L, 4L, 4L))
  expect_equal(b$objective, 4.8, tolerance = 1e-12)
  m <- MaxPRegions(L$X, L$nb, L$pop, 10, n_construct = 20, seed = 2)
  expect_identical(m$labels, c(2L, 5L, 5L, 5L, 4L, 9L, 9L, 2L, 2L, 5L, 6L, 4L, 9L, 9L, 2L, 6L, 6L, 6L, 4L, 9L, 11L, 12L, 8L, 8L, 8L, 4L, 4L, 11L, 12L, 12L, 8L, 8L, 10L, 1L, 11L, 12L, 7L, 7L, 7L, 10L, 1L, 1L, 3L, 3L, 3L, 3L, 10L, 1L, 1L))
  expect_true(min(m$region_weights) >= 10)
  expect_identical(MaxPRegions(L$X, L$nb, L$pop, 20, n_construct = 20, seed = 2)$labels, c(1L, 1L, 1L, 1L, 4L, 4L, 4L, 6L, 1L, 1L, 1L, 4L, 4L, 4L, 6L, 6L, 1L, 1L, 4L, 4L, 4L, 6L, 6L, 2L, 2L, 4L, 4L, 4L, 6L, 5L, 2L, 2L, 2L, 3L, 3L, 5L, 5L, 2L, 5L, 3L, 3L, 3L, 5L, 5L, 5L, 5L, 3L, 3L, 3L))
})
