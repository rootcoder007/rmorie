test_that("binary spatial vote probabilities", {
  expect_equal(LogitVote(1, 0, 2)$value, stats::plogis(3), tolerance = 1e-15)
  expect_equal(ProbitVote(1, 0, 2)$value, stats::pnorm(3), tolerance = 1e-15)
  expect_equal(CauchyVote(1, 0, 2)$value, 0.5 + atan(3) / pi, tolerance = 1e-15)
  expect_equal(NormalVote(0, 0, 1)$value, stats::pnorm(1 - exp(-0.5)), tolerance = 1e-15)
  expect_equal(MixedLogitVote(1, 0, 2, beta_sd = 0)$value, stats::plogis(3), tolerance = 1e-15)
  expect_equal(ProximityVote(c(1, 1), c(0, 0), c(2, 0), p = 1)$value, 0.5)
})

test_that("softmax, utilities and choice", {
  P <- BoltzmannVote(matrix(c(0, 1, 2)), ideal_point = 0)$probabilities
  expect_equal(P, exp(-c(0, 1, 4)) / sum(exp(-c(0, 1, 4))), tolerance = 1e-15)
  expect_equal(QuadUtility(c(1, 2), c(0, 0), matrix(c(1, 0.5, 0.5, 1), 2))$value, -7)
  expect_equal(UtilityMax(rbind(c(3, 0), c(1, 1), c(0, 2)), c(0, 0))$value, 2)
  expect_equal(DiscountUtility(3, 1, 0, discount = 0.5)$value, -0.25)
  expect_equal(MixedUtility(2, 1, beta = 0.5)$value, 1.5)
})

test_that("Plott total median, vote trading and power", {
  r <- MedianVoter2d(rbind(c(0, 0), c(1, 1), c(2, 2), c(0, 2), c(2, 0)))
  expect_equal(r$point, c(1, 1))
  expect_equal(r$value, 3)
  expect_true(r$is_core)
  expect_false(MedianVoter2d(rbind(c(0, 0), c(1, 0), c(0, 1)))$is_core)
  V <- rbind(c(3, -1), c(-1, 3), c(-3, -3))
  expect_equal(VoteTrading(V)$value, 1)
  expect_equal(VoteTrade2d(V)$welfare_after, -2)
  expect_error(VoteTrade2d(cbind(V, 1)))
  expect_equal(WeightedVote(c(3, 2, 2), 4)$value, rep(1 / 3, 3))
})
