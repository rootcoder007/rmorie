test_that("polarization, bargaining, RPS and party positions match the Python arm", {
  x <- c(1, 2, 2.5, 4, 7, 3)
  g <- c(0, 0, 0, 1, 1, 1)
  p <- PolarizationMeasures(x, party = g)
  expect_equal(c(p$wolfson, p$sorting, p$separation, p$between_share),
               c(0.29292929292929293, 0.7419408268023742, 1.8070796117392958, 0.5504761904761906), tolerance = 1e-12)
  expect_equal(PolarizationMeasures(c(0, 0, 1, 1))$esteban_ray, 0.25, tolerance = 1e-15)
  expect_equal(SpatialBargaining(c(0, 0), c(2, 0), status_quo = c(1.8, 1))$nash_t, 0.7338295961275132, tolerance = 1e-12)
  expect_equal(SpatialBargaining(0, 1, delta1 = 0.9, delta2 = 0.8)$rubinstein_share, 0.2 / 0.28, tolerance = 1e-15)
  expect_equal(RankedProbabilityScore(rbind(c(0.2, 0.5, 0.3)), 2)$rps, 0.065, tolerance = 1e-15)
  pp <- PartyPositions(c(1, 2, 3, 7, 8, 9), c("D", "D", "D", "R", "R", "R"), n_boot = 50, seed = 3)
  expect_equal(as.vector(pp$se), c(0.47063425145165094, 0.44817594536580735), tolerance = 1e-12)
})

test_that("roll calls, dimensionality and Wordscores match the Python arm", {
  U <- .morie_random_uniform(400, seed = 4, stream = 0)
  P <- matrix(2 * U[1:30] - 1, ncol = 2, byrow = TRUE)
  s <- OptimalCuttingLines(P, n_votes = 6, beta = 4, seed = 2)
  expect_identical(as.integer(s$errors), c(0L, 0L, 0L, 1L, 1L, 1L))
  expect_equal(s$apre, 0.875, tolerance = 1e-15)
  d <- Dimensionality(s$votes * 1, n_sim = 50, seed = 1)
  expect_equal(round(d$eigenvalues, 4), c(0.5864, 0.2094, 0.1593, 0.1429, 0.0759, 0.0358))
  expect_identical(c(d$elbow, d$parallel), c(1, 1))
  w <- Wordscores(rbind(c(10, 5, 0, 0, 5), c(0, 5, 5, 0, 10), c(0, 0, 5, 10, 5)), c(-1.5, 0, 1.5),
                  rbind(c(5, 5, 5, 5, 5), c(8, 4, 1, 0, 7)))
  expect_equal(w$rescaled, c(0.7044101717798211, -1.4169101717798211), tolerance = 1e-12)
})
