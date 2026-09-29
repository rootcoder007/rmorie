# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/qsfrgr_native.R (quantile survival forest: log-rank
# splits, forest weights, weighted Kaplan-Meier inverted at 1 - q).
# The log-rank statistic is checked against survival::survdiff, the
# unit-weight Kaplan-Meier against survival::survfit, and the forest
# pipeline by recomposing its exported steps.

.qs_t <- c(2, 5, 3, 8, 6, 1, 9, 4, 7, 10, 2.5, 6.5)
.qs_e <- c(1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 1L, 0L, 1L, 1L)
.qs_X <- cbind(c(0.1, 0.9, 0.3, 0.8, 0.6, 0.05, 0.95, 0.4, 0.7, 0.85, 0.2, 0.65),
               c(1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0))

test_that("logrank equals survdiff's chi-square", {
  skip_if_not_installed("survival")
  left <- which(.qs_X[, 1] <= 0.5)
  right <- which(.qs_X[, 1] > 0.5)
  grp <- as.integer(.qs_X[, 1] > 0.5)
  sd <- survival::survdiff(survival::Surv(.qs_t, .qs_e) ~ grp)
  expect_equal(morie_qsfrgr_logrank(.qs_t, .qs_e, left, right), sd$chisq, tolerance = 1e-12)
  expect_equal(morie_qsfrgr_logrank(.qs_t, .qs_e, integer(0), right), 0)
  expect_equal(morie_qsfrgr_logrank(.qs_t, rep(0L, 12), left, right), 0)
})

test_that("km with unit weights is the Kaplan-Meier estimator", {
  skip_if_not_installed("survival")
  k <- morie_qsfrgr_km(.qs_t, .qs_e, rep(1, 12), grid = c(0, 3, 6.2, 11))
  sf <- survival::survfit(survival::Surv(.qs_t, .qs_e) ~ 1)
  ev <- sf$n.event > 0
  expect_equal(k$t, sf$time[ev])
  expect_equal(k$s, sf$surv[ev], tolerance = 1e-12)
  expect_equal(k$grid, summary(sf, times = c(0, 3, 6.2, 11), extend = TRUE)$surv, tolerance = 1e-12)
  # weights: a zero weight removes the observation entirely
  w <- rep(1, 12)
  w[c(1, 2)] <- 0
  kw <- morie_qsfrgr_km(.qs_t, .qs_e, w)
  sw <- survival::survfit(survival::Surv(.qs_t[-(1:2)], .qs_e[-(1:2)]) ~ 1)
  expect_equal(kw$s, sw$surv[sw$n.event > 0], tolerance = 1e-12)
})

test_that("quantile inverts the curve at the first S <= 1 - q", {
  cur <- list(t = c(1, 2, 4, 7), s = c(0.9, 0.6, 0.45, 0.2))
  expect_equal(morie_qsfrgr_quantile(cur, 0.5), 4)
  expect_equal(morie_qsfrgr_quantile(cur, 0.1), 1)
  expect_true(is.na(morie_qsfrgr_quantile(cur, 0.9)))
  expect_error(morie_qsfrgr_quantile(cur, 1), "strictly inside")
})

test_that("weights average 1/|leaf| over the trees", {
  t1 <- list(leaf = FALSE, f = 1, thr = 0.5, l = list(leaf = TRUE, rows = c(1L, 2L)),
             r = list(leaf = TRUE, rows = 3:5))
  t2 <- list(leaf = TRUE, rows = c(2L, 6L))
  w <- morie_qsfrgr_weights(list(t1, t2), c(0.2, 0), 6)
  expect_equal(w$w, c(0.25, 0.5, 0, 0, 0, 0.25))
  expect_equal(w$used, 2L)
  w2 <- morie_qsfrgr_weights(list(t1), c(0.9, 0), 6)
  expect_equal(w2$w, c(0, 0, 1, 1, 1, 0) / 3)
  expect_equal(morie_qsfrgr_weights(list(list(leaf = TRUE, rows = integer(0))), 1, 3)$used, 0L)
})

test_that("forest trees route every point to a non-empty honest leaf", {
  tr <- morie_qsfrgr_forest(.qs_X, .qs_t, .qs_e, n_trees = 5, min_leaf = 2, seed = 3)
  expect_length(tr, 5L)
  for (x in seq_len(12)) {
    w <- morie_qsfrgr_weights(tr, .qs_X[x, ], 12)
    expect_equal(sum(w$w), 1, tolerance = 1e-12)
  }
  expect_identical(tr, morie_qsfrgr_forest(.qs_X, .qs_t, .qs_e, n_trees = 5, min_leaf = 2, seed = 3))
  te <- morie_qsfrgr_forest(.qs_X, .qs_t, .qs_e, n_trees = 3, min_leaf = 2, honest = FALSE, rule = "events")
  expect_length(te, 3L)
  expect_error(morie_qsfrgr_forest(.qs_X, .qs_t, .qs_e, rule = "gini"), "rule must be one of")
})

test_that("morie_qsfrgr composes forest, weights, weighted KM and inversion", {
  r <- morie_qsfrgr(.qs_t, .qs_e, .qs_X, quantile = 0.5, n_trees = 6, min_leaf = 2, seed = 1)
  tr <- morie_qsfrgr_forest(.qs_X, .qs_t, .qs_e, 6, NULL, 2, 6, TRUE, 1, "logrank")
  q <- vapply(1:12, function(i) {
    km <- morie_qsfrgr_km(.qs_t, .qs_e, morie_qsfrgr_weights(tr, .qs_X[i, ], 12)$w, sort(unique(.qs_t)))
    morie_qsfrgr_quantile(km, 0.5)
  }, 0)
  expect_equal(r$quantile_estimate, ifelse(is.na(q), NaN, q))
  expect_equal(r$estimate, mean(q[!is.na(q)]), tolerance = 1e-12)
  expect_equal(r$n_events, 9L)
  w1 <- morie_qsfrgr_weights(tr, .qs_X[1, ], 12)$w
  expect_equal(r$ess[1], 1 / sum(w1^2), tolerance = 1e-12)
  nq <- morie_qsfrgr(.qs_t, .qs_e, .qs_X, n_trees = 3, min_leaf = 2, newX = .qs_X[1:2, ])
  expect_equal(nq$n_query, 2L)
  expect_error(morie_qsfrgr(.qs_t[1:3], .qs_e[1:3], .qs_X[1:3, ]), "at least four")
  expect_error(morie_qsfrgr(.qs_t, .qs_e[-1], .qs_X), "agree in length")
})

test_that("morie_qsfrgr_cheatsheet lists the split rules", {
  expect_match(morie_qsfrgr_cheatsheet(), "logrank, events", fixed = TRUE)
})
