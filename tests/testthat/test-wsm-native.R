# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the plug-in / resampling / Monte Carlo
# shelf. Anchors printed from the Python modules at full double
# precision -- testthat tolerances are RELATIVE, so a rounded anchor
# silently weakens the test.
#
# The deterministic estimators (window widths, kernel density,
# importance sampling from supplied draws, admissibility) are
# anchored exactly, on a shared linear-congruential fixture. The
# resampling ones draw from the language's own RNG and cannot be, so
# they are checked against the structural facts their sources assert.

wsm_unif <- function(n = 500L, s = 4242) {
  u <- numeric(n)
  for (i in seq_len(n)) {
    s <- (1664525 * s + 1013904223) %% 4294967296
    u[i] <- (s + 0.5) / 4294967296
  }
  u
}

wsm_fixture <- function(n = 500L, s = 4242) stats::qnorm(wsm_unif(n, s))

test_that("the fixture matches the one Python anchored against", {
  expect_equal(wsm_fixture()[1:3],
               c(1.1753136328651195, -1.706701964389945,
                 -0.37985904382553287), tolerance = 1e-12)
})

test_that("the window-width rules match morie.fn._wsm", {
  z <- wsm_fixture()
  expect_equal(.wsm_bandwidth(z, "3.31"), 0.24791895164026487,
               tolerance = 1e-12)
  expect_equal(.wsm_bandwidth(z, "3.28"), 0.3000995243270585,
               tolerance = 1e-12)
  expect_equal(.wsm_bandwidth(z, "3.29"), 0.291607780229316,
               tolerance = 1e-12)
  expect_equal(.wsm_spread(z), 0.9546874602771096, tolerance = 1e-12)
  # (3.31) is 0.9 A n^(-1/5), NOT the 1.06 sigma n^(-1/5) of (3.28)
  expect_equal(.wsm_bandwidth(z, "3.31"),
               0.9 * .wsm_spread(z) * 500^(-0.2), tolerance = 1e-12)
  expect_lt(.wsm_bandwidth(z, "3.31"), .wsm_bandwidth(z, "3.28"))
  expect_error(.wsm_bandwidth(z, "silverman"), "3.28")
})

test_that("the adaptive spread resists an outlier that moves the sd", {
  # (3.30) is why (3.31) beats (3.28) off the normal model: one
  # contaminating point moves the standard deviation a long way and
  # the interquartile range hardly at all
  z <- wsm_fixture()
  dirty <- c(z, 60)
  expect_gt(stats::sd(dirty), 2 * stats::sd(z))
  expect_equal(.wsm_spread(dirty), .wsm_spread(z), tolerance = 0.15)
  expect_equal(.wsm_bandwidth(dirty, "3.31"), .wsm_bandwidth(z, "3.31"),
               tolerance = 0.15)
  expect_gt(.wsm_bandwidth(dirty, "3.28"), 2 * .wsm_bandwidth(z, "3.28"))
})

test_that("morie_wsm_kde matches morie.fn.wsmkdn", {
  z <- wsm_fixture()
  g <- seq(-3, 3, length.out = 7L)
  o <- morie_wsm_kde(g, z, h = 0.4)
  expect_equal(o$density,
               c(0.0038255447914845387, 0.05391790864509986,
                 0.21700002049205425, 0.38533259344457016,
                 0.24730849343265945, 0.0823209922589707,
                 0.008427159170860617), tolerance = 1e-10)
  expect_equal(morie_wsm_kde(g, z)$h, 0.24791895164026487, tolerance = 1e-12)
  expect_true(o$is_density)
  expect_equal(o$rule, "3.31")
})

test_that("the kernel density is a density and matches (2.2a) directly", {
  z <- wsm_fixture()
  g <- seq(-4, 4, length.out = 300L)
  o <- morie_wsm_kde(g, z, h = 0.35)
  direct <- vapply(g, function(t) mean(stats::dnorm((t - z) / 0.35)) / 0.35,
                   numeric(1))
  expect_equal(o$density, direct, tolerance = 1e-12)
  expect_true(all(o$density >= 0))
  expect_equal(o$mass, 1, tolerance = 1e-3)
  expect_error(morie_wsm_kde(g, z, h = 0), "positive")
})

test_that("morie_wsm_importance_sampling matches morie.fn.wsmiis", {
  xs <- stats::qcauchy(wsm_unif(5000L, 999))
  o <- morie_wsm_importance_sampling(function(x) x^2, stats::dnorm,
                                     stats::dcauchy, xs)
  expect_equal(o$estimate, 1.0060238829480224, tolerance = 1e-10)
  expect_equal(o$effective_sample_size, 3770.422065254806, tolerance = 1e-9)
  expect_equal(o$max_weight_share, 0.00030231601689907877, tolerance = 1e-10)
  expect_true(o$self_normalised)
  # E[X^2] = 1 under a standard normal target
  expect_equal(o$estimate, 1, tolerance = 0.05)
})

test_that("the self-normalised estimator ignores normalising constants", {
  # (29.22) divides by sum(w), so scaling P* or Q* by any constant
  # leaves the estimate alone. That is the whole point, and the
  # unnormalised alternative cannot do it.
  xs <- stats::qnorm(wsm_unif(4000L, 31), sd = 2)
  phi <- function(x) x^2
  base <- morie_wsm_importance_sampling(
    phi, stats::dnorm, function(x) stats::dnorm(x, sd = 2), xs)$estimate
  scaled <- morie_wsm_importance_sampling(
    phi, function(x) 137 * stats::dnorm(x),
    function(x) 0.004 * stats::dnorm(x, sd = 2), xs)$estimate
  expect_equal(scaled, base, tolerance = 1e-12)
  un <- morie_wsm_importance_sampling(
    phi, function(x) 137 * stats::dnorm(x),
    function(x) 0.004 * stats::dnorm(x, sd = 2), xs,
    normalised = TRUE)$estimate
  expect_false(isTRUE(all.equal(un, base, tolerance = 0.5)))
})

test_that("importance sampling refuses a sampler with no support", {
  expect_error(
    morie_wsm_importance_sampling(function(x) x, function(x) rep(1, length(x)),
                                  function(x) rep(0, length(x)), c(0, 1, 2)),
    "zero or negative")
})

test_that("morie_wsm_bootstrap divides by B - 1", {
  # ESL (7.53), for the same reason a sample variance carries n - 1
  z <- wsm_fixture()
  o <- morie_wsm_bootstrap(z, mean, B = 200L, seed = 1)
  expect_equal(o$variance_ddof0 / o$variance_ddof1, 199 / 200,
               tolerance = 1e-12)
  expect_equal(o$value, o$variance_ddof1)
  expect_equal(morie_wsm_bootstrap(z, mean, B = 200L, seed = 1,
                                   ddof = 0L)$value, o$variance_ddof0)
  expect_error(morie_wsm_bootstrap(z, mean, B = 50L, ddof = 2L), "ddof")
})

test_that("the bootstrap recovers the closed-form variance of a mean", {
  # Var(Xbar) = sigma^2/n is the one case where the answer is known
  # before the bootstrap is run
  z <- wsm_fixture(400L, 77)
  o <- morie_wsm_bootstrap(z, mean, B = 600L, seed = 3)
  expect_equal(o$value, stats::var(z) / 400, tolerance = 0.25)
})

test_that("morie_wsm_plug_in reports T(F_n) and a bootstrap standard error", {
  z <- wsm_fixture()
  o <- morie_wsm_plug_in(z, mean, B = 600L, seed = 5)
  expect_equal(o$estimate, mean(z), tolerance = 1e-12)
  expect_equal(o$se, stats::sd(z) / sqrt(500), tolerance = 0.2)
  expect_lt(o$ci_percentile[1L], mean(z))
  expect_gt(o$ci_percentile[2L], mean(z))
  expect_true(grepl("Hadamard", o$validity_condition))
  expect_null(morie_wsm_plug_in(z, mean, se = FALSE)$se)
})

test_that("the plug-in principle is not special to the mean", {
  # the median's asymptotic SE is 1/(2 f(m) sqrt(n)), which for a
  # standard normal is sqrt(pi/2)/sqrt(n)
  z <- wsm_fixture()
  o <- morie_wsm_plug_in(z, stats::median, B = 500L, seed = 7)
  expect_equal(o$estimate, stats::median(z), tolerance = 1e-12)
  expect_equal(o$se, sqrt(pi / 2) / sqrt(500), tolerance = 0.25)
})

test_that("morie_wsm_mle recovers normal parameters with textbook SEs", {
  # for a normal sample the MLE of the mean has SE sigma/sqrt(n) and
  # the MLE of the standard deviation has sigma/sqrt(2n)
  n <- 800L
  z <- 2.5 + 1.5 * wsm_fixture(n, 13)
  o <- morie_wsm_mle(z, function(d, t) stats::dnorm(d, t[1L], abs(t[2L])),
                     c(0, 1))
  expect_equal(o$estimate[1L], 2.5, tolerance = 0.15)
  expect_equal(abs(o$estimate[2L]), 1.5, tolerance = 0.15)
  expect_true(o$is_maximum)
  expect_equal(o$se[1L], 1.5 / sqrt(n), tolerance = 0.15)
  expect_equal(o$se[2L], 1.5 / sqrt(2 * n), tolerance = 0.2)
})

test_that("morie_wsm_mle reports no SE when it did not find a maximum", {
  z <- wsm_fixture(200L, 17)
  ok <- morie_wsm_mle(z, function(d, t) stats::dnorm(d, t[1L], 1), 0)
  expect_true(ok$is_maximum)
  expect_false(is.null(ok$se))
  # a likelihood that does not depend on theta has a singular Hessian
  flat <- morie_wsm_mle(z, function(d, t) stats::dnorm(d, 0, 1) + 0 * t[1L],
                        0.5)
  expect_false(flat$is_maximum)
  expect_null(flat$se)
  expect_true(grepl("not a maximum", flat$not_a_maximum_note))
  expect_error(morie_wsm_mle(z, function(d, t) rep(-1, length(d)), 0),
               "not finite at theta0")
})

test_that("bagging does essentially nothing for a linear procedure", {
  # ESL Sec. 8.7's sharp corollary: the replicates are identically
  # distributed so only variance can move, and for a fit linear in y
  # the bootstrap average converges back to the original fit
  z <- wsm_fixture(1000L, 23)
  X <- matrix(z[1:450], ncol = 3L)
  y <- as.numeric(X %*% c(1, -2, 0.5)) + 0.5 * z[451:600]
  o <- morie_wsm_bagging(X, y, B = 300L, seed = 2)
  expect_lt(o$max_shift_from_single_fit, 0.05 * stats::sd(y))
  expect_equal(o$bagged_spread, o$replicate_spread / 300, tolerance = 1e-12)
  expect_equal(o$n_oob_missing, 0L)
})

test_that("bagging moves a deep tree far more than a linear fit", {
  # the contrast that makes the previous test mean something
  z <- wsm_fixture(1000L, 29)
  X <- matrix(z[1:450], ncol = 3L)
  y <- as.numeric(X %*% c(1, -2, 0.5)) + 0.5 * z[451:600]
  tree_fit <- function(Xt, yt) {
    t <- .esl_grow(Xt, yt, ncol(Xt), 0L, 12L, 2L)
    function(Xn) .esl_predict_tree(t, Xn)
  }
  lin <- morie_wsm_bagging(X, y, B = 40L, seed = 3)
  nl <- morie_wsm_bagging(X, y, model = tree_fit, B = 40L, seed = 3)
  expect_gt(nl$max_shift_from_single_fit,
            10 * lin$max_shift_from_single_fit)
  expect_gt(nl$replicate_spread, lin$replicate_spread)
})

test_that("the resampling functions do not leak the global RNG stream", {
  z <- wsm_fixture(200L, 31)
  set.seed(4242)
  before <- stats::runif(3L)
  set.seed(4242)
  invisible(morie_wsm_bootstrap(z, mean, B = 20L, seed = 1))
  invisible(morie_wsm_plug_in(z, mean, B = 20L, seed = 1))
  X <- matrix(z[1:60], ncol = 3L)
  invisible(morie_wsm_bagging(X, z[1:20], B = 5L, seed = 1))
  expect_equal(stats::runif(3L), before, tolerance = 1e-12)
})

test_that("morie_wsm_admissible decides dominance over the whole table", {
  o <- morie_wsm_admissible(rbind(c(1, 5), c(2, 2), c(3, 6)),
                            names = c("A", "B", "C"))
  expect_equal(o$admissible, c(TRUE, TRUE, FALSE))
  expect_setequal(o$dominated_by[["C"]], c("A", "B"))
  expect_equal(o$admissible_names, c("A", "B"))
  expect_false(o$bool)
  expect_false(o$is_complete_class)
  # minimax minimises the WORST-case risk, a different criterion:
  # it picks B (worst 2) over A (worst 5)
  expect_equal(o$minimax_rule, "B")
  expect_equal(o$minimax_risk, 2)
})

test_that("identical rules do not dominate each other", {
  # the definition needs STRICT improvement somewhere; a tie is not
  # dominance, so both stay admissible
  o <- morie_wsm_admissible(rbind(c(1, 2), c(1, 2)))
  expect_true(o$bool)
  expect_equal(o$admissible, c(TRUE, TRUE))
  expect_length(o$dominated_by, 0L)
})

test_that("a rule can be admissible without being any good", {
  # admissibility is not optimality: a rule superb at one state and
  # dreadful everywhere else survives as long as nothing beats it there
  o <- morie_wsm_admissible(rbind(c(0, 99), c(1, 1)),
                            names = c("silly", "sensible"))
  expect_equal(o$admissible, c(TRUE, TRUE))
  expect_equal(o$minimax_rule, "sensible")
})

test_that("morie_wsm_admissible validates its table", {
  expect_error(morie_wsm_admissible(rbind(c(1, NA), c(2, 2))), "finite")
  expect_error(morie_wsm_admissible(rbind(c(1, 2), c(2, 1)),
                                    names = "only-one"), "names has")
  o <- morie_wsm_admissible(rbind(c(1, 2)))
  expect_equal(o$n_rules, 1L)
  expect_true(o$bool)
})
