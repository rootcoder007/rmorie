# SPDX-License-Identifier: AGPL-3.0-or-later
# Snapshot regression guards for the native DiD estimators. These lock
# in the exact numeric output of morie_did_panel_fe (TWFE), the
# Callaway-Sant'Anna att_gt loop, and the Sant'Anna-Zhao drdid_rc engine
# on fixed seeds, so the performance rewrites (C++ demean, att_gt vector
# indexing, drdid outcome-regression subsetting) cannot silently drift
# the results. Unlike tests/cross/test-morie_vs_did.R these need no
# reference package, so they run in the coverage tier too. Values were
# generated from the estimators cross-validated against fixest / did /
# DRDID to machine precision; tolerance 1e-6 tolerates BLAS noise while
# catching any real regression.

test_that("native TWFE output is stable (snapshot)", {
  set.seed(123); nu <- 400L; nt <- 6L
  g_on <- sample(c(0, 3, 4, 5), nu, TRUE, c(.4, .2, .2, .2))
  x1 <- rnorm(nu); x2 <- runif(nu); u <- rnorm(nu)
  id <- rep(seq_len(nu), each = nt); tt <- rep(seq_len(nt), nu)
  g <- g_on[id]; d <- as.integer(g > 0 & tt >= g)
  pan <- data.frame(id, tt, g, d, x1 = x1[id], x2 = x2[id],
                    y = u[id] + 0.4 * tt + 1.5 * d + rnorm(nu * nt, 0, .5))
  tw <- morie_did_panel_fe(pan, "y", "d", "id", "tt")
  expect_equal(tw$estimate, 1.5913916437, tolerance = 1e-6)
  expect_equal(tw$std_error, 0.0380526903, tolerance = 1e-6)
})

test_that("native Callaway-Sant'Anna att_gt output is stable (snapshot)", {
  set.seed(123); nu <- 400L; nt <- 6L
  g_on <- sample(c(0, 3, 4, 5), nu, TRUE, c(.4, .2, .2, .2))
  x1 <- rnorm(nu); x2 <- runif(nu); u <- rnorm(nu)
  id <- rep(seq_len(nu), each = nt); tt <- rep(seq_len(nt), nu)
  g <- g_on[id]; d <- as.integer(g > 0 & tt >= g)
  pan <- data.frame(id, tt, g, d, x1 = x1[id], x2 = x2[id],
                    y = u[id] + 0.4 * tt + 1.5 * d + rnorm(nu * nt, 0, .5))
  at <- morie_did_group_time_att(pan, "y", "id", "tt", "g",
                                 covariates = c("x1", "x2"),
                                 n_bootstrap = 0L)
  expect_length(at$att, 15L)
  expect_equal(at$att[1:3],
               c(0.0778974497, 1.4841404589, 1.5459602837), tolerance = 1e-6)
  expect_equal(at$std_error[1:3],
               c(0.0953605839, 0.1000096600, 0.0985291009), tolerance = 1e-6)
})

test_that("native Sant'Anna-Zhao drdid_rc output is stable (snapshot)", {
  set.seed(9); m <- 1600L
  xx <- cbind(rnorm(m), runif(m))
  D <- rbinom(m, 1, plogis(0.4 * xx[, 1])); post <- rbinom(m, 1, .5)
  yv <- 1 + .6 * xx[, 1] + .5 * post + 2 * D * post + rnorm(m)
  dr <- data.frame(y = yv, d = D, post, x1 = xx[, 1], x2 = xx[, 2])
  z <- morie_did_doubly_robust(dr, "y", "d", "post",
                               covariates = c("x1", "x2"), n_bootstrap = 0L)
  expect_equal(z$estimate, 1.9960221929, tolerance = 1e-6)
  expect_equal(z$std_error, 0.1042749708, tolerance = 1e-6)
})
