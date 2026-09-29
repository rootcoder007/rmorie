# Optimal dynamic treatment rules (Luedtke & van der Laan 2018, Thm 22.1,
# Sec. 22.6): backward-induction blips recomputed with lm(), the IPW and
# g-computation values recomputed directly, TMLE score equations checked.
# The ridge of 1e-8 separates lm() from the package's ridge solve, so
# lm-based comparisons use tolerance 1e-6.

dy_data <- function() {
  set.seed(21)
  n <- 60
  L0 <- matrix(stats::rnorm(n), n, 1)
  A0 <- stats::rbinom(n, 1, 0.5)
  L1 <- matrix(0.5 * L0[, 1] + stats::rnorm(n), n, 1)
  A1 <- stats::rbinom(n, 1, 0.5)
  y <- 1 + A0 * L0[, 1] + A1 * (L1[, 1] - 0.2) + stats::rnorm(n, sd = 0.3)
  list(y = y, L0 = L0, L1 = L1, A0 = A0, A1 = A1, n = n)
}
dy_q2_design <- function(d, a0, a1) {
  cbind(1, a0, a1, a0 * a1, d$L0, d$L1, a1 * d$L1, a1 * d$L0, a0 * d$L0)
}

test_that("intervention mechanism: known and logistic-fitted g", {
  d <- dy_data()
  kn <- intervention_mechanism(d$L0, d$A0, d$L1, d$A1,
                               known = list(rep(0.3, d$n), rep(0.6, d$n)))
  expect_equal(kn$g0, ifelse(d$A0 == 1, 0.3, 0.7), tolerance = 1e-12)
  expect_equal(kn$g1, ifelse(d$A1 == 1, 0.6, 0.4), tolerance = 1e-12)
  expect_equal(kn$info$max_weight, max(1 / (kn$g0 * kn$g1)), tolerance = 1e-12)
  est <- intervention_mechanism(d$L0, d$A0, d$L1, d$A1)
  f0 <- stats::glm(d$A0 ~ d$L0, family = stats::binomial())
  f1 <- stats::glm(d$A1 ~ d$A0 + d$L0 + d$L1, family = stats::binomial())
  expect_equal(est$info$p0, unname(stats::fitted(f0)), tolerance = 1e-6)
  expect_equal(est$info$p1, unname(stats::fitted(f1)), tolerance = 1e-6)
  tr <- intervention_mechanism(d$L0, d$A0, d$L1, d$A1, trim = 0.45,
                               known = list(rep(0.1, d$n), rep(0.9, d$n)))
  expect_equal(range(tr$info$p0), c(0.45, 0.45))
  expect_error(intervention_mechanism(d$L0, d$A0, d$L1, d$A1, trim = 0.5), "trim")
  expect_error(intervention_mechanism(d$L0, d$A0, d$L1, d$A1, known = list(1, 1)),
               "wrong length")
})

test_that("sequential blips follow backward induction (Theorem 22.1)", {
  d <- dy_data()
  sb <- sequential_blips(d$y, d$L0, d$A0, d$L1, d$A1)
  X <- dy_q2_design(d, d$A0, d$A1)
  b2 <- stats::lm.fit(X, d$y)$coefficients
  q2 <- function(a0, a1) as.numeric(dy_q2_design(d, a0, a1) %*% b2)
  for (a0 in 0:1) {
    raw <- q2(a0, 1) - q2(a0, 0)
    proj <- stats::lm.fit(cbind(1, d$L1), raw)$fitted.values
    expect_equal(sb$blip2[[a0 + 1]], unname(proj), tolerance = 1e-6)
    expect_identical(sb$d1[[a0 + 1]], ifelse(sb$blip2[[a0 + 1]] > 0, 1, 0))
  }
  pseudo <- q2(d$A0, ifelse(d$A0 == 1, sb$d1[[2]], sb$d1[[1]]))
  expect_equal(sb$pseudo, pseudo, tolerance = 1e-6)
  X1 <- cbind(1, d$A0, d$L0, d$A0 * d$L0)
  b1 <- stats::lm.fit(X1, pseudo)$coefficients
  raw1 <- b1[2] + b1[4] * d$L0[, 1]
  expect_equal(sb$blip1, unname(stats::lm.fit(cbind(1, d$L0), raw1)$fitted.values),
               tolerance = 1e-6)
  expect_identical(sb$d0, ifelse(sb$blip1 > 0, 1, 0))
  or <- optimal_rule(d$y, d$L0, d$A0, d$L1, d$A1)
  expect_identical(or$d0, sb$d0)
  expect_identical(or$d1, sb$d1)
})

test_that("exceptional-law share counts blips within tol of zero", {
  b <- c(-0.3, 0.005, 0, 0.02, -0.01)
  expect_equal(exceptional_law_share(b), 3 / 5)
  expect_equal(exceptional_law_share(b, tol = 0.001), 1 / 5)
  expect_identical(exceptional_law_share(numeric(0)), 0)
})

test_that("rule_value_seq is the sequential g-formula under the rule", {
  d <- dy_data()
  b2 <- stats::lm.fit(dy_q2_design(d, d$A0, d$A1), d$y)$coefficients
  pseudo <- as.numeric(dy_q2_design(d, d$A0, 1) %*% b2)
  X1 <- cbind(1, d$A0, d$L0, d$A0 * d$L0)
  b1 <- stats::lm.fit(X1, pseudo)$coefficients
  ref <- mean(cbind(1, 0, d$L0, 0) %*% b1)
  got <- rule_value_seq(d$y, d$L0, d$A0, d$L1, d$A1, rep(0, d$n),
                        list(rep(1, d$n), rep(1, d$n)), NULL, NULL)
  expect_equal(got, ref, tolerance = 1e-6)
})

test_that("IPW, g-computation and TMLE under a supplied static regime", {
  d <- dy_data()
  kg <- list(rep(0.5, d$n), rep(0.5, d$n))
  A <- cbind(d$A0, d$A1)
  ch <- list(d$L0, d$L1)
  rg <- list(rep(1, d$n), rep(1, d$n))
  ip <- morie_tmldyn(d$y, A, ch, regime = rg, method = "ipw", known_g = kg)
  expect_equal(ip$estimate, mean(d$A0 * d$A1 * d$y / 0.25) +
                 min(d$y) * (1 - mean(d$A0 * d$A1 / 0.25)), tolerance = 1e-12)
  expect_identical(ip$rule_source, "supplied")
  gc <- morie_tmldyn(d$y, A, ch, regime = rg, method = "gcomp", known_g = kg)
  expect_equal(gc$estimate, gc$static_11, tolerance = 1e-9)
  expect_equal(gc$best_static, max(gc$static_00, gc$static_01, gc$static_10, gc$static_11))
  tm <- morie_tmldyn(d$y, A, ch, regime = rg, method = "tmle", known_g = kg)
  expect_equal(tm$eic_mean, 0, tolerance = 1e-9)
  z <- stats::qnorm(0.975)
  expect_equal(tm$ci, tm$estimate + c(-z, z) * tm$se, tolerance = 1e-12)
  # the restored entry point is the same estimator
  td <- morie_tmle_dynamic_regime(d$y, A, ch, regime = rg, method = "tmle", known_g = kg)
  expect_equal(td$estimate, tm$estimate, tolerance = 1e-12)
  expect_equal(td$static$static_11, tm$static_11)
  # matrix regime form
  mr <- morie_tmldyn(d$y, A, ch, regime = cbind(rep(1, d$n), rep(1, d$n)),
                     method = "ipw", known_g = kg)
  expect_equal(mr$estimate, ip$estimate, tolerance = 1e-12)
})

test_that("CV-TMLE estimates the rule on training folds and targets the mean", {
  d <- dy_data()
  A <- cbind(d$A0, d$A1)
  ch <- list(d$L0, d$L1)
  r <- morie_tmldyn(d$y, A, ch, n_folds = 5)
  expect_identical(r$n_folds, 5L)
  expect_identical(r$rule_source, "estimated")
  expect_true(all(r$d0 %in% c(0, 1)))
  expect_equal(r$eic_mean, 0, tolerance = 1e-9)
  expect_equal(r$treated_first, mean(r$d0))
  expect_equal(morie_tmledynamicregime(d$y, A, ch, n_folds = 5)$estimate, r$estimate)
  # rules in fold k come from sequential_blips fitted without fold k
  ys <- (d$y - min(d$y)) / diff(range(d$y))
  val <- which(seq_len(d$n) %% 5 == 0)
  fit <- sequential_blips(ys, d$L0, d$A0, d$L1, d$A1, idx = setdiff(seq_len(d$n), val))
  expect_identical(r$d0[val], fit$d0[val])
  full <- morie_tmldyn(d$y, A, ch, method = "tmle")
  expect_identical(full$n_folds, 1L)
})

test_that("morie_tmldyn validates its inputs", {
  d <- dy_data()
  A <- cbind(d$A0, d$A1)
  ch <- list(d$L0, d$L1)
  expect_error(morie_tmldyn(d$y, A, ch, method = "aipw"), "method must be")
  expect_error(morie_tmldyn(d$y[1:3], A[1:3, ], list(d$L0[1:3, , drop = FALSE],
                                                     d$L1[1:3, , drop = FALSE])), "at least 4")
  expect_error(morie_tmldyn(d$y, A[, 1], ch), "n-by-2")
  expect_error(morie_tmldyn(d$y, A * 2, ch), "binary")
  expect_error(morie_tmldyn(d$y, A, list(d$L0)), "two blocks")
  expect_error(morie_tmldyn(d$y, A, NULL), "required")
  expect_error(morie_tmldyn(rep(1, d$n), A, ch), "constant")
  expect_error(morie_tmldyn(d$y, A, ch, regime = "greedy"), "optimal")
  expect_error(morie_tmle_dynamic_regime(d$y, A, ch, method = "x"), "method must be")
})

test_that("the cheatsheet describes the backward induction", {
  expect_match(morie_tmldyn_cheatsheet(), "Thm 22.1", fixed = TRUE)
})
