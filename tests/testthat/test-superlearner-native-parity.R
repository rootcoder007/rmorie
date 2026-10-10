# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie_weight_super / morie_otis_aipw_superlearner use rmorie's native
# random forest (trees_native.R) instead of an optional ranger learner, so
# the learner library no longer depends on what is installed.

sl_data <- function(seed = 5, n = 600) {
  set.seed(seed)
  X <- matrix(stats::rnorm(n * 4), n)
  colnames(X) <- paste0("x", 1:4)
  t <- stats::rbinom(n, 1, stats::plogis(X[, 1] - 0.5 * X[, 2]))
  list(X = X, t = t, n = n)
}

test_that("morie_weight_super always stacks the native forest", {
  s <- sl_data()
  d <- data.frame(t = s$t, s$X)
  w <- morie_weight_super(d, "t", colnames(s$X))
  expect_named(w$learner_weights, c("logistic", "logistic_sq", "forest"))
  expect_equal(sum(w$learner_weights), 1, tolerance = 1e-12)
  expect_false(any(grepl("ranger", deparse(morie_weight_super))))
})

test_that("morie_otis_aipw_superlearner runs without ranger and recovers the ATE", {
  s <- sl_data(7)
  df <- data.frame(y = 1 + 2 * s$t + s$X[, 1] + stats::rnorm(s$n), d = s$t, s$X)
  o <- morie_otis_aipw_superlearner(df, "d", "y", colnames(s$X), n_folds = 3L)
  expect_false(any(grepl("ranger", deparse(morie_otis_aipw_superlearner))))
  # true ATE 2; n = 600 gives se ~ 0.1, so a 0.4 band is ~4 se
  expect_lt(abs(o$ate - 2), 0.4)
})

test_that("native probability / regression forest agrees with ranger", {
  skip_if_not_installed("ranger")
  s <- sl_data()
  X <- s$X
  tr <- 1:500
  te <- 501:600
  f <- rmorie:::.morie_rf_fit(X[tr, ], s$t[tr], "regression",
                              n_estimators = 500L, mtry = 2L, min_node = 10L)
  a <- rmorie:::.morie_rf_predict(f, X[te, ])
  r <- ranger::ranger(t ~ ., data = data.frame(t = factor(s$t[tr]), X[tr, ]),
                      probability = TRUE, num.trees = 500, seed = 1)
  b <- stats::predict(r, data = data.frame(X[te, ]))$predictions[, "1"]
  # Different bootstrap draws and split tie-breaking, so only agreement in
  # distribution is expected: observed cor 0.98, mean |diff| 0.034.
  expect_gt(stats::cor(a, b), 0.9)
  expect_lt(mean(abs(a - b)), 0.08)
})
