# Cluster-randomized-trial TMLE with adaptive pre-specification (Balzer
# et al. 2016, 2018) and hierarchical TMLE I/II (Balzer et al. 2019).
# With the unadjusted working model the logistic fit is saturated in A,
# so the targeted predictions are the arm means; the ridge of 1e-8 in
# the IRLS bounds the gap, hence tolerance 1e-6 on those comparisons.

tc_data <- function() {
  set.seed(9)
  n <- 24
  W <- matrix(stats::rnorm(n * 2), n, 2)
  pair <- rep(seq_len(n / 2), each = 2)
  A <- rep(c(1, 0), n / 2)
  y <- 2 + 0.8 * A + 0.9 * W[, 1] + stats::rnorm(n, sd = 0.5)
  list(y = y, A = A, W = W, pair = pair, n = n)
}

test_that("the default library has the unadjusted, main-term and interaction models", {
  lib <- morie_tmlcic_default_library(3)
  expect_length(lib, 7L)
  expect_identical(vapply(lib, `[[`, "", "name"),
                   c("unadjusted", "W1", "W2", "W3", "W1 x A", "W2 x A", "W3 x A"))
  expect_identical(lib[[3]]$cols, 1L)
  expect_true(lib[[7]]$interact)
  expect_length(morie_tmlcic_default_library(2, interactions = FALSE), 3L)
})

test_that("candidate TMLE targets with the clever covariate and solves its score", {
  d <- tc_data()
  ys <- (d$y - min(d$y)) / diff(range(d$y))
  un <- list(name = "unadjusted", cols = integer(0), interact = FALSE)
  ct <- morie_tmlcic_candidate_tmle(ys, d$A, d$W, un, function(i) 0.5)
  expect_equal(ct$q1, rep(mean(ys[d$A == 1]), d$n), tolerance = 1e-6)
  expect_equal(ct$q0, rep(mean(ys[d$A == 0]), d$n), tolerance = 1e-6)
  expect_equal(ct$info$H, ifelse(d$A == 1, 2, -2))
  cand <- list(name = "W1", cols = 0L, interact = FALSE)
  c2 <- morie_tmlcic_candidate_tmle(ys, d$A, d$W, cand, function(i) 0.5)
  expect_equal(sum(c2$info$H * (ys - c2$qa)), 0, tolerance = 1e-9)
  # without the targeting step the predictions are the working model's
  c3 <- morie_tmlcic_candidate_tmle(ys, d$A, d$W, cand, function(i) 0.5,
                                    target_step = FALSE)
  expect_identical(c3$info$eps, 0)
  fit <- suppressWarnings(stats::glm(ys ~ d$A + d$W[, 1], family = stats::quasibinomial()))
  expect_equal(c3$q1, unname(stats::plogis(stats::coef(fit)[1] + stats::coef(fit)[2] +
                                             stats::coef(fit)[3] * d$W[, 1])),
               tolerance = 1e-6)
})

test_that("influence curves follow eqs. 13.3 (PATE) and 13.4 (SATE)", {
  d <- tc_data()
  q1 <- rep(0.6, d$n)
  q0 <- seq(0.2, 0.5, length.out = d$n)
  qa <- ifelse(d$A == 1, q1, q0)
  gA <- rep(0.5, d$n)
  ys <- d$y / 10
  psi <- mean(q1 - q0)
  s <- morie_tmlcic_influence_curve(ys, d$A, q1, q0, qa, gA, seq_len(d$n), psi, "SATE")
  sgn <- ifelse(d$A == 1, 1, -1)
  expect_equal(s, sgn * (ys - qa) / gA, tolerance = 1e-12)
  p <- morie_tmlcic_influence_curve(ys, d$A, q1, q0, qa, gA, seq_len(d$n), psi, "PATE")
  expect_equal(p, sgn * (ys - qa) / gA + q1 - q0 - psi, tolerance = 1e-12)
  part <- morie_tmlcic_influence_curve(ys, d$A, q1, q0, qa, gA, 1:3, psi, "SATE")
  expect_true(all(is.na(part[-(1:3)])))
  expect_error(morie_tmlcic_influence_curve(ys, d$A, q1, q0, qa, gA, 1, psi, "ATT"),
               "SATE or PATE")
})

test_that("variance estimators match the unmatched, pair-matched PATE and SATE forms", {
  d <- tc_data()
  D <- sin(seq_len(d$n))
  qa <- rep(0.4, d$n)
  ys <- d$y / 10
  groups <- split(seq_len(d$n), d$pair)
  names(groups) <- NULL
  u <- morie_tmlcic_variance_estimate(D, ys, qa, groups, d$n, "unmatched", "SATE")
  expect_equal(u$var, mean(D^2) / d$n, tolerance = 1e-12)
  r <- ys - qa
  rho <- mean(vapply(groups, function(g) r[g[1]] * r[g[2]], numeric(1)))
  pm <- morie_tmlcic_variance_estimate(D, ys, qa, groups, d$n, "matched", "PATE")
  expect_equal(pm$info$rho, rho, tolerance = 1e-12)
  expect_equal(pm$var, max(mean(D^2) - 2 * rho, 0) / d$n, tolerance = 1e-12)
  sm <- morie_tmlcic_variance_estimate(D, ys, qa, groups, d$n, "matched", "SATE")
  db <- vapply(groups, function(g) mean(D[g]), numeric(1))
  expect_equal(sm$var, mean(db^2) / length(groups), tolerance = 1e-12)
})

test_that("cluster TMLE without adaptation is the difference in arm means", {
  d <- tc_data()
  r <- morie_tmlcic_tmle_cluster_ic(d$y, d$A, d$W, adapt = FALSE)
  diffm <- mean(d$y[d$A == 1]) - mean(d$y[d$A == 0])
  expect_equal(r$estimate, diffm, tolerance = 1e-6)
  expect_equal(r$unadjusted, r$estimate, tolerance = 1e-12)
  res <- d$y - ifelse(d$A == 1, mean(d$y[d$A == 1]), mean(d$y[d$A == 0]))
  Dic <- ifelse(d$A == 1, 1, -1) * res / 0.5
  expect_equal(r$se, sqrt(mean(Dic^2) / d$n), tolerance = 1e-6)
  expect_equal(r$ci, r$estimate + c(-1, 1) * stats::qnorm(0.975) * r$se, tolerance = 1e-9)
  expect_identical(r$design, "unmatched")
  rm <- morie_tmlcic_tmle_cluster_ic(d$y, d$A, d$W, cluster = d$pair, target = "PATE",
                                     adapt = FALSE)
  expect_identical(rm$design, "matched")
  expect_equal(rm$independent_units, d$n / 2)
  expect_equal(rm$estimate, diffm, tolerance = 1e-6)
})

test_that("adaptive pre-specification picks the minimum cross-validated risk", {
  d <- tc_data()
  r <- morie_tmlcic_tmle_cluster_ic(d$y, d$A, d$W, n_folds = 4)
  qr <- unlist(r$q_risks)
  expect_identical(r$q_selected, names(qr)[which.min(qr)])
  gr <- unlist(r$g_risks)
  expect_identical(r$g_selected, names(gr)[which.min(gr)])
  # the prognostic covariate W1 beats the unadjusted model
  expect_lt(qr[["W1"]], qr[["unadjusted"]])
  expect_identical(identical(morie_tmlcic, morie_tmlcic_tmle_cluster_ic), TRUE)
  sel <- morie_tmlcic_adaptive_prespecification(
    (d$y - min(d$y)) / diff(range(d$y)), d$A, d$W,
    lapply(seq_len(d$n), function(i) i), "unmatched", "SATE", n_folds = 4)
  expect_equal(sel$q_risks, unname(qr), tolerance = 1e-12)
  expect_identical(sel$n_folds, 4L)
})

test_that("cluster TMLE validates its inputs", {
  d <- tc_data()
  expect_error(morie_tmlcic_tmle_cluster_ic(d$y, d$A, d$W, target = "ATE"), "SATE or PATE")
  expect_error(morie_tmlcic_tmle_cluster_ic(d$y, d$A[-1], d$W), "treatments")
  expect_error(morie_tmlcic_tmle_cluster_ic(d$y, d$A * 2, d$W), "binary")
  expect_error(morie_tmlcic_tmle_cluster_ic(d$y, rep(1, d$n), d$W), "non-empty")
  expect_error(morie_tmlcic_tmle_cluster_ic(d$y, d$A, d$W[-1, ]), "covariate rows")
  expect_error(morie_tmlcic_tmle_cluster_ic(d$y, d$A, d$W, design = "x"), "design must be")
  expect_error(morie_tmlcic_tmle_cluster_ic(d$y, d$A, d$W, design = "matched"), "pair labels")
  expect_error(morie_tmlcic_tmle_cluster_ic(d$y, d$A, d$W, cluster = rep(1:8, 3),
                                            design = "matched"), "needs pairs")
  expect_error(morie_tmlcic_tmle_cluster_ic(rep(1, d$n), d$A, d$W), "constant")
})

test_that("cluster weights default to 1/N_j and must sum to one", {
  cl <- c("a", "a", "b", "c", "c", "c")
  w <- morie_tmlcic_cluster_weights(cl)
  expect_equal(w$alpha, c(0.5, 0.5, 1, 1 / 3, 1 / 3, 1 / 3), tolerance = 1e-15)
  expect_identical(w$groups, list(1:2, 3L, 4:6))
  ok <- morie_tmlcic_cluster_weights(cl, c(0.2, 0.8, 1, 0.5, 0.25, 0.25))
  expect_equal(ok$alpha, c(0.2, 0.8, 1, 0.5, 0.25, 0.25))
  expect_error(morie_tmlcic_cluster_weights(cl, rep(0.5, 6)), "sum to")
  expect_error(morie_tmlcic_cluster_weights(cl, rep(1, 5)), "weights for")
  expect_error(morie_tmlcic_cluster_weights(cl, c(-1, 2, 1, 1, 0, 0)), "non-negative")
})

test_that("hierarchical TMLE I and II reduce to cluster-mean contrasts", {
  set.seed(4)
  J <- 8
  size <- c(3, 5, 2, 4, 3, 6, 2, 5)
  cl <- rep(seq_len(J), size)
  Aj <- rep(c(1, 0), 4)
  A <- Aj[cl]
  y <- stats::plogis(-0.5 + 0.7 * A + stats::rnorm(length(cl), sd = 0.6))
  yc <- tapply(y, cl, mean)
  kg <- list(rep(0.5, J), rep(0.5, length(cl)))
  r <- morie_tmlcic_tmle_hierarchical(y, A, NULL, NULL, cl, known_g = kg)
  dm <- mean(yc[Aj == 1]) - mean(yc[Aj == 0])
  expect_equal(r$estimate_cluster, dm, tolerance = 1e-6)
  expect_equal(r$estimate_individual, dm, tolerance = 1e-6)
  expect_equal(r$estimate, r$estimate_individual)
  expect_equal(unname(r$cluster_outcome), unname(as.numeric(yc)), tolerance = 1e-12)
  # TMLE I influence curve: H (Yc - Q*) + Q*(a) - psi, differenced over a
  m1 <- mean(yc[Aj == 1])
  m0 <- mean(yc[Aj == 0])
  Dc <- ifelse(Aj == 1, 2 * (yc - m1), -2 * (yc - m0)) + m1 - m0 - dm
  expect_equal(r$influence_curve_cluster, unname(as.numeric(Dc)), tolerance = 1e-6)
  expect_equal(r$se_cluster, stats::sd(Dc) / sqrt(J), tolerance = 1e-6)
  rc <- morie_tmlcic_tmle_hierarchical(y, A, NULL, NULL, cl, arm = "cluster", known_g = kg)
  expect_identical(rc$arm_reported, "cluster")
  expect_null(rc$estimate_individual)
  # estimated g with a cluster-level covariate
  E <- matrix(rep(c(0.1, 0.9, 0.3, 0.2, 0.5, 0.7, 0.4, 0.8), size), ncol = 1)
  re <- morie_tmlcic_tmle_hierarchical(y, A, E, NULL, cl)
  expect_true(is.finite(re$estimate_cluster))
  expect_equal(re$eic_mean_cluster, mean(re$influence_curve_cluster), tolerance = 1e-12)
  expect_error(morie_tmlcic_tmle_hierarchical(y, A, NULL, NULL, cl, arm = "x"), "arm must")
  expect_error(morie_tmlcic_tmle_hierarchical(y * 3, A, NULL, NULL, cl), "\\[0, 1\\]")
  expect_error(morie_tmlcic_tmle_hierarchical(y, A, NULL, NULL, cl, trim = 0.6), "trim")
  expect_error(morie_tmlcic_tmle_hierarchical(y, 1 - rev(A), NULL, NULL, cl), "varies within")
  expect_error(morie_tmlcic_tmle_hierarchical(y[1:8], A[1:8], NULL, NULL, cl[1:8]),
               "at least 4 clusters")
})

test_that("the cheatsheet names the losses", {
  expect_match(morie_tmlcic_cheatsheet(), "13.5", fixed = TRUE)
})
