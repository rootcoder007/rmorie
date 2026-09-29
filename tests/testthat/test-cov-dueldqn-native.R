# Coverage for the dueling architecture (Wang et al. 2016, eqs. 8-9):
# Q = V + (A - anchor) with the mean, max and naive anchors, batched
# aggregation, the Double-DQN target r + gamma Q_target(s', argmax
# Q_online(s')), and one full TD step.

test_that("dueling aggregation subtracts the chosen anchor", {
  a <- c(1, 3, -2)
  expect_equal(dueling_aggregate(0.5, a), 0.5 + a - mean(a), tolerance = 1e-12)
  expect_equal(dueling_aggregate(0.5, a, "max"), 0.5 + a - 3)
  expect_equal(dueling_aggregate(0.5, a, "naive"), 0.5 + a)
  expect_identical(morie_dueldqn, dueling_aggregate)
  expect_error(dueling_aggregate(1, numeric(0)), "no actions")
  expect_error(dueling_aggregate(1, a, "median"), "mode must be one of")
  q <- dueling_q(c(1, 2), list(c(0, 1), c(2, 2, 5)))
  expect_equal(q, list(1 + c(-0.5, 0.5), 2 + c(-1, -1, 2)))
  expect_identical(duelingq, dueling_q)
  expect_identical(dueling_dqn, dueling_q)
  expect_identical(duelingdqn, dueling_q)
  expect_error(dueling_q(1:2, list(1)), "2 values but 1 advantage rows")
})

test_that("Double-DQN evaluates the online argmax with the target net", {
  expect_equal(double_q_target(1, 0.9, c(0.2, 0.8, 0.5), c(3, -1, 7)), 1 + 0.9 * -1, tolerance = 1e-12)
  expect_equal(double_q_target(1, 0.9, c(0.8, 0.8), c(2, 5)), 1 + 0.9 * 2, tolerance = 1e-12)
  expect_identical(double_q_target(2, 0.9, 1:2, 1:2, done = TRUE), 2)
  expect_error(double_q_target(1, 0.9, 1:2, 1:3), "action counts differ")
})

test_that("one dueling TD step", {
  s <- dueling_step(1, c(0.5, -0.5, 1), 2, 0.3, 0.95, 0.8, c(1, 0, -1), 0.6, c(0.2, 0.4, 0), mode = "mean")
  q <- 1 + c(0.5, -0.5, 1) - 1 / 3
  qo <- 0.8 + c(1, 0, -1)
  qt <- 0.6 + c(0.2, 0.4, 0) - 0.2
  tgt <- 0.3 + 0.95 * qt[which.max(qo)]
  expect_equal(s$q, q, tolerance = 1e-12)
  expect_equal(s$target, tgt, tolerance = 1e-12)
  expect_equal(s$td_error, tgt - q[3], tolerance = 1e-12)
  expect_identical(s$greedy_action, 2L)
  d <- dueling_step(1, c(0.5, -0.5), 0, 0.3, 0.95, 0, c(0, 0), 0, c(0, 0), done = TRUE)
  expect_equal(d$td_error, 0.3 - 1.5, tolerance = 1e-12)
  expect_error(dueling_step(1, c(0.5, -0.5), 2, 0, 0.9, 0, c(0, 0), 0, c(0, 0)), "action 2 out of range")
})
