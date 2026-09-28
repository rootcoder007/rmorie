# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: EllipticScan and FlexScan vs smerc::elliptic.test / flex.test.

cd_data <- function(seed, n = 40) {
  U <- .morie_random_uniform(4 * n, seed = seed, stream = 0)
  coords <- cbind(10 * U[1:n], 10 * U[n + 1:n])
  pop <- round(50 + 150 * U[2 * n + 1:n])
  hot <- sqrt((coords[, 1] - 3)^2 + ((coords[, 2] - 5) / 3)^2) < 1.5
  cases <- round(pop * (0.02 + 0.06 * hot) * (0.6 + 0.8 * U[3 * n + 1:n]))
  list(coords = coords, pop = pop, cases = cases)
}

test_that("EllipticScan clusters equal smerc::elliptic.test with nsim = 0", {
  skip_if_not_installed("smerc")
  for (seed in c(4, 12)) {
    d <- cd_data(seed)
    m <- suppressWarnings(EllipticScan(d$coords, d$cases, d$pop, nsim = 0, alpha = 1, ubpop = 0.3))
    s <- suppressMessages(suppressWarnings(smerc::elliptic.test(d$coords, d$cases, d$pop, nsim = 0, alpha = 1,
                                                                ubpop = 0.3)))
    expect_equal(length(m$clusters), length(s$clusters))
    for (i in seq_along(s$clusters)) {
      expect_equal(sort(m$clusters[[i]]$zone), sort(s$clusters[[i]]$locids))
      expect_equal(m$clusters[[i]]$llr, s$clusters[[i]]$test_statistic, tolerance = 1e-10)
      expect_equal(m$clusters[[i]]$shape, s$clusters[[i]]$shape,
                   tolerance = 1e-12)
      expect_equal(m$clusters[[i]]$angle, s$clusters[[i]]$angle, tolerance = 1e-12)
    }
  }
})

test_that("FlexZones and FlexScan statistics equal smerc::flex.zones / flex.test", {
  skip_if_not_installed("smerc")
  d <- cd_data(7, 25)
  D <- as.matrix(stats::dist(d$coords))
  w <- 1 * (D < 2.6 & D > 0)
  mz <- FlexZones(d$coords, w, k = 5)
  sz <- smerc::flex.zones(d$coords, w, k = 5)
  key <- function(z) vapply(z, function(v) paste(sort(v), collapse = ","), "")
  expect_setequal(key(mz), key(sz))
  expect_equal(length(mz), length(sz))
  m <- FlexScan(d$coords, d$cases, d$pop, w, k = 5, nsim = 0, alpha = 1)
  s <- suppressMessages(suppressWarnings(smerc::flex.test(d$coords, d$cases, d$pop, w, k = 5, nsim = 0, alpha = 1)))
  expect_equal(sort(m$clusters[[1]]$zone), sort(s$clusters[[1]]$locids))
  expect_equal(m$clusters[[1]]$llr, s$clusters[[1]]$test_statistic, tolerance = 1e-10)
  sb <- suppressMessages(suppressWarnings(smerc::flex.test(d$coords, d$cases, d$pop, w, k = 5, nsim = 0, alpha = 1,
                                                           type = "binomial")))
  mb <- FlexScan(d$coords, d$cases, d$pop, w, k = 5, kind = "binomial", nsim = 0, alpha = 1)
  expect_equal(mb$clusters[[1]]$llr, sb$clusters[[1]]$test_statistic, tolerance = 1e-10)
})

test_that("FixedCircleScan GAM p-values are Poisson upper tails", {
  d <- cd_data(3, 30)
  r <- FixedCircleScan(d$coords, d$cases, d$pop, c(1.5, 2.5), alpha = 1)
  for (cc in r$circles[1:5]) {
    E <- sum(d$pop[cc$members]) * sum(d$cases) / sum(d$pop)
    expect_equal(cc$pvalue, stats::ppois(sum(d$cases[cc$members]) - 1, E, lower.tail = FALSE), tolerance = 1e-12)
  }
})
