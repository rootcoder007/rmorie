# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the nine modules ported after the
# placeholder counter reached zero: mdvtr, smplep, ld50r, abndst,
# aglnvr, cbnrt, facea, midor, wnoma.
#
# The fixture uses a linear congruential generator with exact integer
# arithmetic and a division by a power of two, so R doubles and Python
# integers agree bit for bit. No qnorm anywhere in the fixture.

ports_fixture <- function(seed = 20260731) {
  s <- seed
  nxt <- function() {
    s <<- (1664525 * s + 1013904223) %% 4294967296
    (s + 0.5) / 4294967296
  }
  x <- numeric(200L)
  for (i in seq_len(200L)) x[i] <- 4 * nxt() - 2
  ya <- numeric(40L)
  for (i in seq_len(40L)) ya[i] <- 2 * nxt()
  yb <- numeric(30L)
  for (i in seq_len(30L)) yb[i] <- 2 * nxt()
  list(x = x, ya = ya, yb = yb,
       da = as.numeric(seq_len(40L) > 20L),
       db = as.numeric(seq_len(30L) <= 15L))
}

test_that("the fixture is bit-identical across languages", {
  fx <- ports_fixture()
  expect_equal(sum(fx$x), 9.73489975184202, tolerance = 1e-12)
})

# ------------------------------------------------------------------
# mdvtr
# ------------------------------------------------------------------

test_that("the median voter matches the Python core exactly", {
  fx <- ports_fixture()
  out <- morie_median_voter_ci(fx$x)
  expect_equal(out$estimate, 0.14167581312358379, tolerance = 1e-12)
  expect_equal(out$se, 0.14190237561527225, tolerance = 1e-9)
  expect_equal(out$se_normal, 0.10419901312394506, tolerance = 1e-12)
  expect_equal(out$ci_exact_lower, -0.23742235498502851, tolerance = 1e-12)
  expect_equal(out$ci_exact_upper, 0.45466907834634185, tolerance = 1e-12)
  expect_equal(out$exact_coverage, 0.9599628083865962, tolerance = 1e-12)
})

test_that("the winner is the median, not the mean", {
  out <- morie_median_voter_ci(c(1, 2, 3, 4, 100))
  expect_equal(out$estimate, 3)
  expect_equal(out$mean, 22)
  expect_true(out$unique_winner)
})

test_that("an even electorate gives an interval of winners", {
  out <- morie_median_voter_ci(c(1, 2, 3, 4))
  expect_false(out$unique_winner)
  expect_equal(out$median_interval, c(2, 3))
  expect_true(any(grepl("Condorcet winner", out$warnings)))
})

test_that("the normal formula overstates for a heavy tail", {
  set.seed(2)
  out <- morie_median_voter_ci(stats::rt(4000, df = 2))
  expect_gt(out$se_normal / out$se, 1.3)
  expect_true(any(grepl("far from normal", out$warnings)))
})

test_that("the order-statistic interval covers without any assumption", {
  set.seed(5)
  hits <- 0L
  for (i in seq_len(400L)) {
    o <- morie_median_voter_ci(stats::rcauchy(51))
    hits <- hits + (o$ci_exact_lower <= 0 && 0 <= o$ci_exact_upper)
  }
  expect_gt(hits / 400, 0.94)
})

# ------------------------------------------------------------------
# smplep
# ------------------------------------------------------------------

test_that("the dual-frame total matches the Python core exactly", {
  fx <- ports_fixture()
  out <- morie_dual_frame_total(fx$ya, fx$yb, fx$da, fx$db, theta = 0.5)
  expect_equal(out$estimate, 49.444963024812751, tolerance = 1e-12)
  expect_equal(out$naive_pooled_total, 67.296954847872257, tolerance = 1e-12)
  expect_equal(out$se, 3.9871856882413814, tolerance = 1e-10)
  expect_equal(out$theta_optimal, 0.49152193473212985, tolerance = 1e-12)
})

test_that("the naive pool double-counts the overlap", {
  out <- morie_dual_frame_total(rep(1, 4), rep(1, 3), c(0, 0, 1, 1),
                                c(1, 1, 0), theta = 0.5)
  expect_equal(out$estimate, 5)
  expect_equal(out$naive_pooled_total, 7)
  expect_equal(out$overlap_double_count, 2)
})

test_that("the estimator is unbiased for every theta", {
  ests <- vapply(c(0, 0.25, 0.5, 0.75, 1), function(t) {
    morie_dual_frame_total(rep(2, 4), rep(2, 3), c(0, 0, 1, 1),
                           c(1, 1, 0), theta = t)$estimate
  }, numeric(1))
  expect_equal(stats::sd(ests), 0, tolerance = 1e-12)
})

test_that("the optimal weight favours the more precise frame", {
  expect_equal(morie_optimal_overlap_weight(1, 9), 0.9)
  expect_equal(morie_optimal_overlap_weight(9, 1), 0.1)
  expect_equal(morie_optimal_overlap_weight(0, 0), 0.5)
})

# ------------------------------------------------------------------
# ld50r
# ------------------------------------------------------------------

test_that("the LD50 matches the Python core exactly", {
  d <- c(0.5, 1, 2, 4, 8, 16, 32)
  k <- c(2, 7, 19, 31, 48, 56, 59)
  n <- rep(60, 7)
  out <- morie_ld50(d, k, n)
  expect_equal(out$estimate, 3.4646746689948631, tolerance = 1e-8)
  expect_equal(out$slope, 0.95809303557778192, tolerance = 1e-8)
  expect_equal(out$fieller_g, 0.023999023279251516, tolerance = 1e-8)
  expect_equal(out$deviance, 0.50766158094977865, tolerance = 1e-8)
  expect_equal(out$heterogeneity_p, 0.99184006989365359, tolerance = 1e-8)
  expect_equal(out$ci_lower, 2.9363554979413395, tolerance = 1e-8)
  expect_equal(out$ci_upper, 4.0826399628550378, tolerance = 1e-8)
})

test_that("a flat response gives an unbounded Fieller interval", {
  out <- morie_ld50(c(1, 2, 4, 8, 16), rep(10, 5), rep(20, 5))
  expect_gte(out$fieller_g, 1)
  expect_false(out$bounded)
  expect_true(any(grepl("unbounded", out$warnings)))
})

test_that("the heterogeneity warning does not fire on a clean fit", {
  d <- c(0.5, 1, 2, 4, 8, 16, 32)
  k <- c(2, 7, 19, 31, 48, 56, 59)
  out <- morie_ld50(d, k, rep(60, 7))
  expect_gt(out$heterogeneity_p, 0.05)
  expect_false(any(grepl("heterogeneity factor", out$warnings)))
})

# ------------------------------------------------------------------
# abndst
# ------------------------------------------------------------------

test_that("Bracken matches the Python core exactly", {
  P <- rbind(c(0.6, 0, 0), c(0, 0.5, 0), c(0, 0, 0.4),
             c(0.4, 0.5, 0), c(0, 0, 0.6))
  reads <- c(1200, 900, 700, 1500, 1100)
  out <- morie_bracken_abundance(reads, P)
  expect_equal(out$fractions[1], 0.35441670267654496, tolerance = 1e-10)
  expect_equal(out$fractions[2], 0.31224996399012173, tolerance = 1e-10)
  expect_equal(out$fractions[3], 0.33333333333333331, tolerance = 1e-10)
  expect_equal(out$log_likelihood, -8526.4695761494768, tolerance = 1e-8)
  expect_equal(out$iterations, 28L)
})

test_that("shared-ancestor reads split by k-mer compatibility", {
  P <- rbind(c(0.5, 0), c(0, 0.5), c(0.5, 0.5))
  out <- morie_bracken_abundance(c(100, 300, 400), P)
  expect_equal(out$estimate, c(200, 600), tolerance = 1e-6)
  expect_true(out$converged)
})

test_that("indistinguishable species are reported not split silently", {
  P <- rbind(c(0.5, 0.5), c(0.5, 0.5))
  out <- morie_bracken_abundance(c(100, 100), P)
  expect_false(out$identifiable)
  expect_true(any(grepl("indistinguishable", out$warnings)))
})

# ------------------------------------------------------------------
# aglnvr
# ------------------------------------------------------------------

test_that("the loss-stream variance matches the Python core exactly", {
  ls <- 2 + sin(seq.int(0, 399) / 7) +
    0.3 * (((seq.int(0, 399) * 37) %% 11) - 5) / 5
  out <- morie_loss_stream_variance(ls)
  expect_equal(out$estimate, 2.0012445553039893, tolerance = 1e-10)
  expect_equal(out$variance, 0.53270039623462728, tolerance = 1e-10)
  expect_equal(out$tau_int, 12.989643553895343, tolerance = 1e-8)
  expect_equal(out$ess, 30.793762610987716, tolerance = 1e-8)
  expect_equal(out$se_inflation, 3.6041148086451602, tolerance = 1e-8)
})

test_that("an independent stream needs no correction", {
  set.seed(11)
  out <- morie_loss_stream_variance(stats::rnorm(4000, 2, 0.5))
  expect_equal(out$se_inflation, 1, tolerance = 0.25)
})

test_that("the inflation matches the AR(1) theoretical factor", {
  set.seed(12)
  for (rho in c(0.5, 0.8)) {
    n <- 20000L
    e <- stats::rnorm(n)
    x <- numeric(n)
    x[1] <- e[1] / sqrt(1 - rho^2)
    for (i in 2:n) x[i] <- rho * x[i - 1] + e[i]
    got <- morie_loss_stream_variance(x)$se_inflation
    expect_equal(got, sqrt((1 + rho) / (1 - rho)), tolerance = 0.25)
  }
})

# ------------------------------------------------------------------
# facea
# ------------------------------------------------------------------

test_that("the B-spline basis matches Python and is a partition of unity", {
  t <- seq(0, 1, length.out = 40L)
  B <- morie_bspline_basis(t, n_basis = 10L, degree = 3L)
  expect_equal(sum(B), 40, tolerance = 1e-12)
  expect_equal(B[6, 4], 0.12046449423175258, tolerance = 1e-12)
  expect_equal(rowSums(B), rep(1, 40), tolerance = 1e-12)
  expect_true(all(B >= 0))
})

test_that("FACE recovers the Karhunen-Loeve truth", {
  set.seed(21)
  t <- seq(0, 1, length.out = 60L)
  phi <- cbind(sqrt(2) * sin(2 * pi * t), sqrt(2) * cos(2 * pi * t))
  n <- 400L
  xi <- matrix(stats::rnorm(2 * n), n, 2) *
    rep(sqrt(c(1, 0.5)), each = n)
  Y <- xi %*% t(phi) + matrix(stats::rnorm(n * 60, sd = 0.3), n, 60)
  out <- morie_face_smooth(Y, t, n_basis = 12L)
  dt <- t[2] - t[1]
  expect_equal(out$eigenvalues[1] * dt, 1, tolerance = 0.08)
  expect_equal(out$eigenvalues[2] * dt, 0.5, tolerance = 0.08)
  # R and Python draw different samples, so this is a recovery
  # check, not a parity anchor
  expect_equal(out$noise_variance, 0.09, tolerance = 0.12)
  expect_equal(out$npc, 2L)
  expect_lt(out$negative_eigenvalue_mass, 0.01 * out$total_variance)
})

# ------------------------------------------------------------------
# midor
# ------------------------------------------------------------------

test_that("the back-door criterion on the textbook graphs", {
  A <- matrix(FALSE, 3, 3)
  A[1, 2] <- A[1, 3] <- A[2, 3] <- TRUE     # Z -> T, Z -> Y, T -> Y
  expect_false(morie_is_backdoor_admissible(A, 2, 3, integer(0)))
  expect_true(morie_is_backdoor_admissible(A, 2, 3, 1L))
  expect_equal(morie_backdoor_sets(A, 2, 3), list(1L))

  B <- matrix(FALSE, 3, 3)
  B[1, 2] <- B[2, 3] <- TRUE                # T -> M -> Y
  expect_true(morie_is_backdoor_admissible(B, 1, 3, integer(0)))
  expect_false(morie_is_backdoor_admissible(B, 1, 3, 2L))

  C <- matrix(FALSE, 3, 3)
  C[1, 2] <- C[3, 2] <- TRUE                # T -> C <- Y
  expect_true(morie_is_backdoor_admissible(C, 1, 3, integer(0)))
})

test_that("the effect is recovered when identified", {
  set.seed(31)
  n <- 3000L
  Z <- stats::rnorm(n)
  T <- 0.8 * Z + stats::rnorm(n)
  Y <- 2 * T + 1.5 * Z + stats::rnorm(n)
  A <- matrix(FALSE, 3, 3)
  A[1, 2] <- A[1, 3] <- A[2, 3] <- TRUE
  out <- morie_identify_estimate_refute(A, cbind(Z, T, Y), 2, 3,
                                        n_refute = 40L)
  expect_true(out$identified)
  expect_equal(out$adjustment_set, 1L)
  expect_equal(out$estimate, 2, tolerance = 0.1)
  expect_lt(abs(out$placebo_effect), 0.05)
  expect_true(out$passed_placebo)
})

test_that("conditioning on a mediator is flagged and attenuates", {
  set.seed(32)
  n <- 3000L
  T <- stats::rnorm(n)
  M <- T + stats::rnorm(n)
  Y <- M + stats::rnorm(n)
  A <- matrix(FALSE, 3, 3)
  A[1, 2] <- A[2, 3] <- TRUE
  good <- morie_identify_estimate_refute(A, cbind(T, M, Y), 1, 3,
                                         adjustment = integer(0),
                                         n_refute = 10L)
  bad <- morie_identify_estimate_refute(A, cbind(T, M, Y), 1, 3,
                                        adjustment = 2L, n_refute = 10L)
  expect_equal(good$estimate, 1, tolerance = 0.1)
  expect_lt(abs(bad$estimate), 0.15)
  expect_equal(bad$adjusted_for_mediator, 2L)
  expect_true(any(grepl("mediator", bad$warnings)))
})

# ------------------------------------------------------------------
# cbnrt
# ------------------------------------------------------------------

test_that("TF-IDF rows are unit norm and rare terms weigh more", {
  out <- morie_tfidf(c("common rare1", "common x", "common y", "common z"))
  expect_equal(sqrt(rowSums(out$matrix^2)), rep(1, 4), tolerance = 1e-12)
  expect_gt(out$idf[["rare1"]], out$idf[["common"]])
})

test_that("the bag of words cannot see negation", {
  a <- morie_tfidf(c("history of psychosis", "no history of psychosis"))
  expect_gt(sum(a$matrix[1, ] * a$matrix[2, ]), 0.7)
  b <- morie_tfidf(c("history of psychosis", "no history of psychosis"),
                   min_df = 2)
  expect_equal(sum(b$matrix[1, ] * b$matrix[2, ]), 1, tolerance = 1e-12)
  expect_false("no" %in% b$vocabulary)
})

test_that("adjusting for text removes the confounding", {
  set.seed(41)
  n <- 600L
  sev <- stats::runif(n) < 0.4
  stem <- c("routine case stable outcome",
            "severe acute presentation critical")
  filler <- c("alpha", "beta", "gamma", "delta")
  texts <- vapply(seq_len(n), function(i) {
    paste(stem[sev[i] + 1L],
          paste(sample(filler, 4, replace = TRUE), collapse = " "))
  }, character(1))
  T <- as.numeric(stats::runif(n) < ifelse(sev, 0.8, 0.25))
  Y <- 1 * T + 3 * sev + stats::rnorm(n)
  out <- morie_text_ate(texts, T, Y, n_components = 5L)
  expect_gt(abs(out$naive_difference - 1), 1)
  expect_equal(out$estimate, 1, tolerance = 0.25)
  expect_true(any(grepl("negation", out$warnings)))
})

test_that("a supplied embedding is used instead of the bag of words", {
  set.seed(42)
  n <- 600L
  sev <- stats::runif(n) < 0.4
  T <- as.numeric(stats::runif(n) < ifelse(sev, 0.8, 0.25))
  Y <- 1 * T + 3 * sev + stats::rnorm(n)
  E <- cbind(as.numeric(sev) + stats::rnorm(n, sd = 0.1), stats::rnorm(n))
  out <- morie_text_ate(NULL, T, Y, embedding = E)
  expect_equal(out$estimate, 1, tolerance = 0.3)
  expect_equal(out$n_components, 2L)
})

# ------------------------------------------------------------------
# wnoma
# ------------------------------------------------------------------

test_that("ideal points are recovered in one dimension", {
  set.seed(51)
  n <- 120L
  m <- 220L
  truth <- stats::rnorm(n)
  zy <- stats::rnorm(m) * 0.8
  zn <- stats::rnorm(m) * 0.8
  eta <- 2 * (outer(truth, zn, function(a, b) (a - b)^2) -
                outer(truth, zy, function(a, b) (a - b)^2))
  V <- (matrix(stats::runif(n * m), n, m) < stats::pnorm(eta)) * 1
  out <- morie_wnominate_fit(V, n_dims = 1L, polarity = which.max(truth))
  # recovery check on an R-drawn chamber, not a parity anchor
  expect_gt(abs(stats::cor(out$ideal_points[, 1], truth)), 0.9)
  expect_true(out$converged)
  expect_gt(out$correct_classification, out$modal_baseline)
  expect_gt(out$aggregate_pre, 0.3)
})

test_that("the configuration is normalised and polarity fixes the sign", {
  set.seed(52)
  n <- 80L
  m <- 150L
  truth <- stats::rnorm(n)
  zy <- stats::rnorm(m) * 0.8
  zn <- stats::rnorm(m) * 0.8
  eta <- 2 * (outer(truth, zn, function(a, b) (a - b)^2) -
                outer(truth, zy, function(a, b) (a - b)^2))
  V <- (matrix(stats::runif(n * m), n, m) < stats::pnorm(eta)) * 1
  k <- which.max(truth)
  out <- morie_wnominate_fit(V, n_dims = 1L, polarity = k)
  expect_equal(mean(out$ideal_points), 0, tolerance = 1e-8)
  expect_equal(sqrt(mean(rowSums(out$ideal_points^2))), 1, tolerance = 1e-8)
  expect_gt(out$ideal_points[k, 1], 0)
})

test_that("unanimous roll calls are dropped and reported", {
  set.seed(53)
  n <- 60L
  m <- 100L
  truth <- stats::rnorm(n)
  zy <- stats::rnorm(m) * 0.8
  zn <- stats::rnorm(m) * 0.8
  eta <- 2 * (outer(truth, zn, function(a, b) (a - b)^2) -
                outer(truth, zy, function(a, b) (a - b)^2))
  V <- (matrix(stats::runif(n * m), n, m) < stats::pnorm(eta)) * 1
  V <- cbind(V, matrix(1, n, 5))
  out <- morie_wnominate_fit(V, n_dims = 1L, polarity = which.max(truth))
  expect_equal(out$n_dropped_rollcalls, 5L)
  expect_true(any(grepl("unanimous", out$warnings)))
})

test_that("without polarity the sign is reported as arbitrary", {
  set.seed(54)
  n <- 50L
  m <- 90L
  V <- (matrix(stats::runif(n * m), n, m) < 0.5) * 1
  out <- morie_wnominate_fit(V, n_dims = 1L)
  expect_true(any(grepl("identified only up to", out$warnings)))
})

test_that("the nine ports validate their inputs", {
  expect_error(morie_dual_frame_total(1, numeric(0), 0, numeric(0)),
               "at least one unit")
  expect_error(morie_ld50(c(1, 2), c(11, 1), c(10, 10)), "n_dead must lie")
  expect_error(morie_bracken_abundance(c(1, 1),
                                       rbind(c(0.3, 0), c(0.3, 0.5))),
               "must sum to 1")
  expect_error(morie_loss_stream_variance(numeric(0)), "at least one finite")
  expect_error(morie_bspline_basis(seq(0, 1, length.out = 20), n_basis = 2L),
               "n_basis must be at least")
  expect_error(morie_face_smooth(matrix(1, 1, 10)), "at least two curves")
  expect_error(morie_wnominate_fit(matrix(1, 10, 6), n_dims = 1L),
               "roll calls divide")
})
