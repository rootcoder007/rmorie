test_that("majority rule and pivots", {
  r <- MajorityCompare(rbind(c(0, 0), c(4, 0), c(2, 3)), c(2, 1), c(3.5, 3))
  expect_equal(c(r$votes_x, r$votes_y), c(2, 1))
  p <- PivotPoints(1:7, weights = c(1, 1, 1, 1, 1, 1, 4), supermajority = 0.6, veto = 2 / 3,
                   chambers = list(1:3, 4:7))
  expect_equal(c(p$median, p$supermajority_pivots, p$veto_pivot, p$gridlock), c(5, 4, 6, 4, 2, 7))
  expect_equal(ParetoSet(rbind(c(0, 0), c(4, 0), c(0, 4), c(1, 1)), rbind(c(1, 1), c(3, 3)))$inside, c(TRUE, FALSE))
  expect_equal(nrow(WinSet(matrix(c(0, 1, 2)), 1, matrix(c(0.5, 1.5)))$members), 0)
})

test_that("probabilistic voting, competition, bargaining, trading", {
  expect_equal(ProbabilisticVote(matrix(c(0, 1)), matrix(c(0, 1)))$shares, c(0.5, 0.5))
  eq <- CandidateEquilibrium(matrix(c(0, 0.4, 0.8)), matrix(seq(0, 1, by = 0.1)), beta = 0.5)
  expect_equal(eq$positions, matrix(c(0.4, 0.4)))
  expect_equal(HotellingPriceLocation(0, 0)$profits, c(0.5, 0.5))
  expect_equal(SalopCircle(4, t = 2, fixed_cost = 0.05)$price, 0.5)
  expect_equal(KalaiSmorodinsky(rbind(c(0, 2), c(1, 1.5), c(2, 0)))$kalai_smorodinsky, c(1.2, 1.2))
  expect_equal(BaronFerejohn(5, 1)$proposer_share, 0.6)
  vt <- VoteTradingRikerBrams(rbind(c(3, -1), c(-1, 3), c(-3, -3)))
  expect_equal(c(vt$after_trade, vt$welfare_after), c(1, 1, -2))
})
