M <- list(model = "Exp", psill = 1, range = 2)

test_that("SGS reproduces the kriging distribution and data", {
  D <- rbind(c(0, 0), c(3, 0))
  z <- c(1, -0.5)
  r <- SgsSimulate(D, z, rbind(c(1, 0)), M, mean = 0.2, seed = 5)
  C <- outer(c(0, 3), c(0, 3), function(a, b) exp(-abs(a - b) / 2))
  c0 <- exp(-c(1, 2) / 2)
  w <- solve(C, c0)
  e <- .morie_random_normal(1, seed = 5, stream = 1)
  expect_equal(r$realizations[1, 1], 0.2 + sum(w * (z - 0.2)) + sqrt(1 - sum(w * c0)) * e, tolerance = 1e-12)
  g <- SgsSimulate(D, z, rbind(c(0, 0), c(1.5, 0), c(3, 0)), M, nsim = 3, seed = 1)
  expect_equal(g$realizations[, 1], rep(1, 3))
  ns <- NormalScore(c(5, 1, 3, 3, 9))
  expect_equal(BackTransform(ns$scores, ns$table_z, ns$table_y), c(5, 1, 3, 3, 9))
})

test_that("SIS, summaries and Markov chains", {
  m <- list(model = "Sph", psill = 0.25, range = 3)
  r <- SisSimulate(rbind(c(0, 0), c(4, 0)), c(0, 1), rbind(c(0, 0), c(1, 0), c(4, 0)), c(0, 1), m,
                   categorical = TRUE, nsim = 5)
  expect_true(all(r$realizations[, 1] == 0 & r$realizations[, 3] == 1))
  s <- SimulationSummary(rbind(c(1, 4, 0), c(3, 2, 1), c(2, 9, 2)), probs = 0.5, threshold = 2.5,
                         blocks = c("a", "a", "b"))
  expect_equal(c(s$etype, s$variance), c(2, 5, 1, 1, 13, 1))
  expect_equal(s$block_averages, rbind(c(2.5, 0), c(2.5, 1), c(5.5, 2)))
  tm <- TransitionMatrix(list(c("a", "a", "b", "b", "b", "a"), c("b", "a")))
  expect_equal(tm$matrix, matrix(0.5, 2, 2))
  expect_equal(MarkovChainSimulate(rbind(c(0, 1), c(1, 0)), 0, 5), c(0, 1, 0, 1, 0))
})
