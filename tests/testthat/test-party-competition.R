# Tests for PartyCompetition: spatial party competition and legislative bargaining.

v <- sin((0:30) * 1.7) * 0.5 + ifelse((0:30) %% 3 == 0, 0.6, -0.2)

test_that("VoteShares and the median voter equilibrium", {
  s <- VoteShares(c(-0.3, 0.1, 0.6), v)
  d <- abs(outer(v, c(-0.3, 0.1, 0.6), "-"))
  expect_equal(s, as.numeric(table(factor(apply(d, 1, which.min), levels = 1:3))) / 31)
  r <- BestResponseEquilibrium(v, 2, (0:40) / 20 - 1)
  expect_true(r$converged)
  expect_true(all(abs(r$positions - stats::median(v)) <= 0.05))
})

test_that("indices, bargaining, setter and committee models", {
  p <- c(10, 25, 7, 3) / 45
  expect_equal(EffectiveNumberParties(c(10, 25, 7, 3))$laakso_taagepera, 1 / sum(p^2))
  expect_equal(RubinsteinBargaining(0.8, 0.95)$proposer, 0.05 / 0.24)
  expect_equal(MedianParty(c(0.3, -1, 0.8, 0.1), c(20, 30, 25, 25))$index, 3)
  expect_equal(SetterOutcome(0.9, 0.5, 0.3)$outcome, 0.7)
  expect_false(CommitteeOutcome(c(0.1, 0.2, 0.3), c(0.1, 0.3, 0.5, 0.6, 0.9), 0.2)$gate_opened)
  s <- StrategicVoteShares(c(-0.5, 0, 0.4, 0.8), v)
  expect_equal(sum(s$strategic), 1)
})
