# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/tlonsl_native.R (online super learner, van der Laan &
# Rose 2018, Ch. 18). Sequential validation trains on 1..t-1 and
# scores observation t; exponential weights come from cumulative
# losses; the ensemble risk is recomputed step by step.

.ol_y <- c(1, 2, 1.5, 3, 2.5, 3.5, 3, 4, 3.8, 4.5)
.ol_lib <- list(
  mean = function(h) { m <- mean(h); function(z) m },
  last = function(h) function(z) z[length(z)],
  zero = function(h) function(z) 0)

test_that("summary_measure returns the last `lags` values, zero-padded", {
  expect_equal(morie_tlonsl_summary_measure(1:5, 2), c(4, 5))
  expect_equal(morie_tlonsl_summary_measure(7, 3), c(0, 0, 7))
  expect_length(morie_tlonsl_summary_measure(1:5, 0), 0L)
  expect_error(morie_tlonsl_summary_measure(1:5, -1), "non-negative")
})

test_that("sequential_risk scores one-step-ahead predictions", {
  r <- morie_tlonsl_sequential_risk(.ol_y, .ol_lib$mean, burn_in = 4)
  pm <- vapply(5:10, function(t) mean(.ol_y[1:(t - 1)]), 0)
  expect_equal(r$predictions, pm, tolerance = 1e-12)
  expect_equal(r$risk, mean((.ol_y[5:10] - pm)^2), tolerance = 1e-12)
  rl <- morie_tlonsl_sequential_risk(.ol_y, .ol_lib$last, burn_in = 3)
  expect_equal(rl$predictions, .ol_y[3:9])
  yb <- c(0, 1, 1, 0, 1, 1)
  lg <- morie_tlonsl_sequential_risk(yb, function(h) { m <- mean(h); function(z) m }, "log", burn_in = 2)
  pb <- vapply(3:6, function(t) mean(yb[1:(t - 1)]), 0)
  expect_equal(lg$losses, -(yb[3:6] * log(pb) + (1 - yb[3:6]) * log(1 - pb)), tolerance = 1e-12)
  expect_error(morie_tlonsl_sequential_risk(.ol_y, .ol_lib$mean, "abs"), "loss must be one of")
  expect_error(morie_tlonsl_sequential_risk(.ol_y, .ol_lib$mean, burn_in = 10), "burn_in must lie")
})

test_that("update_weights is a softmax of -eta * cumulative loss", {
  cl <- c(3, 1, 2)
  expect_equal(morie_tlonsl_update_weights(cl, 0.5), exp(-0.5 * cl) / sum(exp(-0.5 * cl)), tolerance = 1e-12)
  expect_equal(morie_tlonsl_update_weights(c(1e6, 1e6 + 1)), c(1, exp(-1)) / (1 + exp(-1)), tolerance = 1e-12)
  expect_error(morie_tlonsl_update_weights(numeric(0)), "no cumulative")
})

test_that("online_super_learner mixes predictions with the running weights", {
  for (fn in list(morie_tlonsl_online_super_learner, morie_tlonsl, morie_tlonsl_onlinesuperlearner)) {
    r <- fn(.ol_y, .ol_lib, burn_in = 4, eta = 2)
    nm <- sort(names(.ol_lib))
    per <- lapply(.ol_lib[nm], function(a) morie_tlonsl_sequential_risk(.ol_y, a, burn_in = 4))
    cum <- rep(0, 3)
    ens <- 0
    for (s in 1:6) {
      w <- exp(-2 * (cum - min(cum)))
      w <- w / sum(w)
      p <- sum(w * vapply(per, function(q) q$predictions[s], 0))
      ens <- ens + (.ol_y[4 + s] - p)^2
      cum <- cum + vapply(per, function(q) q$losses[s], 0)
    }
    expect_equal(r$risk, ens / 6, tolerance = 1e-12)
    expect_equal(unlist(r$weights), setNames(w, nm), tolerance = 1e-12)
    expect_identical(r$best_member, nm[which.min(vapply(per, function(q) q$risk, 0))])
    expect_equal(r$n_scored, 6L)
  }
  expect_error(morie_tlonsl_online_super_learner(.ol_y, list()), "library is empty")
})

test_that("morie_tlonsl_cheatsheet names sequential validation", {
  expect_match(morie_tlonsl_cheatsheet(), "SEQUENTIAL", fixed = TRUE)
})
