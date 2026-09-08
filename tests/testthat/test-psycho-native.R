# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the reliability, IRT and meta-analysis
# shelf. The ICC fixture is Shrout and Fleiss's (1979) own Table 2,
# so the arithmetic is checked against the paper's published
# coefficients as well as against the Python anchors. The IRT tests
# turn on existence -- there is no finite ML estimate for a perfect
# pattern -- and the meta-analysis tests on tau^2 being part of the
# answer.

# Shrout and Fleiss (1979), Table 2: 6 targets rated by 4 judges.
SF_TABLE <- matrix(c(9, 2, 5, 8,
                     6, 1, 3, 2,
                     8, 4, 6, 8,
                     7, 1, 2, 6,
                     10, 5, 6, 9,
                     6, 2, 4, 7), nrow = 6L, byrow = TRUE)

sf_long <- function() {
  list(y = as.vector(t(SF_TABLE)),
       sub = rep(seq_len(6L), each = 4L),
       rat = rep(seq_len(4L), times = 6L))
}

test_that("the three ICCs match morie.fn and Shrout-Fleiss Table 2", {
  d <- sf_long()
  a <- morie_psy_icc1k(d$y, d$sub)
  b <- morie_psy_icc2k(d$y, d$sub, d$rat)
  cc <- morie_psy_icc3k(d$y, d$sub, d$rat)
  # anchors from Python
  expect_equal(a$value, 0.44279713367926865, tolerance = 1e-12)
  expect_equal(a$icc_single, 0.16574176840547544, tolerance = 1e-12)
  expect_equal(a$MSR, 11.241666666666669, tolerance = 1e-12)
  expect_equal(a$MSW, 6.263888888888889, tolerance = 1e-12)
  expect_equal(b$value, 0.6200505475989893, tolerance = 1e-12)
  expect_equal(b$icc_single, 0.2897637795275592, tolerance = 1e-12)
  expect_equal(b$MSC, 32.486111111111114, tolerance = 1e-12)
  expect_equal(b$MSE, 1.019444444444442, tolerance = 1e-10)
  expect_equal(cc$value, 0.9093155423770697, tolerance = 1e-12)
  expect_equal(cc$icc_single, 0.7148407148407154, tolerance = 1e-12)
  # and the paper's own published figures
  expect_equal(a$icc_single, 0.17, tolerance = 0.03)
  expect_equal(b$icc_single, 0.29, tolerance = 0.02)
  expect_equal(cc$icc_single, 0.71, tolerance = 0.01)
  expect_equal(a$value, 0.44, tolerance = 0.01)
  expect_equal(b$value, 0.62, tolerance = 0.01)
  expect_equal(cc$value, 0.91, tolerance = 0.01)
})

test_that("the cases are ordered and average follows Spearman-Brown", {
  d <- sf_long()
  a <- morie_psy_icc1k(d$y, d$sub)
  b <- morie_psy_icc2k(d$y, d$sub, d$rat)
  cc <- morie_psy_icc3k(d$y, d$sub, d$rat)
  expect_lt(a$value, b$value)
  expect_lt(b$value, cc$value)
  expect_equal(b$rater_penalty, cc$value - b$value, tolerance = 1e-9)
  expect_equal(cc$icc2k, b$value, tolerance = 1e-12)
  for (o in list(a, b, cc)) {
    expect_equal(o$value, .psy_spearman_brown(o$icc_single, o$k),
                 tolerance = 1e-9)
  }
})

test_that("a constant rater offset costs Case 3 nothing", {
  base <- c(1, 3, 5, 7, 9, 11)
  M <- cbind(base, base + 4, base + 8)
  y <- as.vector(t(M))
  sub <- rep(seq_len(6L), each = 3L)
  rat <- rep(seq_len(3L), times = 6L)
  c3 <- morie_psy_icc3k(y, sub, rat)
  c2 <- morie_psy_icc2k(y, sub, rat)
  expect_equal(c3$value, 1, tolerance = 1e-9)
  expect_lt(c2$value, 0.85)
  expect_equal(c3$max_rater_offset, 8, tolerance = 1e-12)
})

test_that("the ICCs refuse designs their formulas cannot describe", {
  d <- sf_long()
  expect_error(morie_psy_icc1k(d$y[-1], d$sub[-1]), "k ratings per target")
  expect_error(morie_psy_icc2k(d$y[-1], d$sub[-1], d$rat[-1]),
               "complete and crossed")
  expect_error(.psy_anova2(c(1, 2), c(1, 1), c(1, 2)), "at least 2 subjects")
})

irt_a <- rep(1.2, 20L)
irt_b <- seq(-2, 2, length.out = 20L)
irt_y <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0)

test_that("the four theta estimators match morie.fn", {
  expect_equal(morie_psy_mle_theta(irt_y, a = irt_a, b = irt_b)$theta,
               0.7553364189852929, tolerance = 1e-7)
  expect_equal(morie_psy_mle_theta(irt_y, a = irt_a, b = irt_b)$se,
               0.46718002236683664, tolerance = 1e-7)
  expect_equal(morie_psy_map_theta(irt_y, a = irt_a, b = irt_b)$theta,
               0.6211299559878948, tolerance = 1e-7)
  expect_equal(morie_psy_map_theta(irt_y, a = irt_a, b = irt_b)$se,
               0.4199010867177378, tolerance = 1e-7)
  expect_equal(morie_psy_eap_theta(irt_y, a = irt_a, b = irt_b)$theta,
               0.6309340060078853, tolerance = 1e-7)
  expect_equal(morie_psy_eap_theta(irt_y, a = irt_a, b = irt_b)$se,
               0.4236844492551289, tolerance = 1e-7)
  expect_equal(morie_psy_wle_theta(irt_y, a = irt_a, b = irt_b)$theta,
               0.7382772062082513, tolerance = 1e-7)
})

test_that("no finite ML estimate for a perfect pattern, but the rest deliver", {
  for (pat in list(rep(1, 20L), rep(0, 20L))) {
    sgn <- if (pat[1L] == 1) 1 else -1
    ml <- morie_psy_mle_theta(pat, a = irt_a, b = irt_b)
    expect_false(ml$finite)
    expect_true(is.infinite(ml$theta))
    expect_equal(sign(ml$theta), sgn)
    for (f in list(morie_psy_map_theta, morie_psy_eap_theta,
                   morie_psy_wle_theta)) {
      o <- f(pat, a = irt_a, b = irt_b)
      expect_true(is.finite(o$theta))
      expect_gt(sgn * o$theta, 0)
    }
  }
  expect_equal(morie_psy_map_theta(rep(1, 20L), a = irt_a, b = irt_b)$theta,
               2.4309274460915073, tolerance = 1e-6)
  expect_equal(morie_psy_eap_theta(rep(1, 20L), a = irt_a, b = irt_b)$theta,
               2.505919552395733, tolerance = 1e-6)
})

test_that("MAP and EAP shrink and the prior narrows the interval", {
  ml <- morie_psy_mle_theta(irt_y, a = irt_a, b = irt_b)
  mp <- morie_psy_map_theta(irt_y, a = irt_a, b = irt_b)
  ep <- morie_psy_eap_theta(irt_y, a = irt_a, b = irt_b)
  expect_lt(abs(mp$theta), abs(ml$theta))
  expect_lt(mp$se, ml$se)
  expect_equal(mp$posterior_information, mp$information + 1,
               tolerance = 1e-12)
  expect_equal(ep$posterior_sd, ep$se, tolerance = 1e-15)
  expect_true(ep$no_optimisation)
  expect_true(mp$exists_for_perfect_patterns)
  expect_true(morie_psy_wle_theta(irt_y, a = irt_a,
                                  b = irt_b)$bias_corrected)
})

test_that("the matrix aliases share the single-pattern implementations", {
  items <- cbind(irt_a, irt_b)
  X <- rbind(irt_y, rev(irt_y))
  te <- morie_psy_theta_eap(X, items)
  tm <- morie_psy_theta_map(X, items)
  expect_equal(te$theta[1L],
               morie_psy_eap_theta(irt_y, a = irt_a, b = irt_b)$theta,
               tolerance = 1e-12)
  expect_equal(tm$theta[1L],
               morie_psy_map_theta(irt_y, a = irt_a, b = irt_b)$theta,
               tolerance = 1e-12)
  expect_equal(te$alias_of, "morie_psy_eap_theta")
  expect_equal(tm$alias_of, "morie_psy_map_theta")
  expect_gt(max(abs(te$theta - tm$theta)), 0)
})

test_that("the IRT estimators validate their inputs", {
  expect_error(morie_psy_mle_theta(rep(2, 20L), a = irt_a, b = irt_b),
               "binary")
  expect_error(morie_psy_mle_theta(rep(1, 20L)), "difficulties b are required")
  expect_error(morie_psy_mle_theta(rep(1, 20L), a = irt_a[1:5], b = irt_b),
               "one entry per item")
  expect_error(morie_psy_map_theta(irt_y, a = irt_a, b = irt_b,
                                   prior = c(0, 0)),
               "prior standard deviation")
})

ma_vi <- c(0.02, 0.05, 0.03, 0.09, 0.04, 0.06, 0.02, 0.07, 0.05, 0.03,
           0.08, 0.04)
ma_yi <- c(0.35, 0.71, 0.12, 0.94, 0.44, 0.02, 0.63, 0.28, 0.85, 0.51,
           0.19, 0.77)

test_that("Paule-Mandel matches morie.fn and solves its defining equation", {
  o <- morie_psy_ma_paule_mandel(ma_yi, ma_vi)
  expect_equal(o$tau2, 0.03600993297413879, tolerance = 1e-9)
  expect_equal(o$mu, 0.48063001569061, tolerance = 1e-10)
  expect_equal(o$se, 0.08111700742683745, tolerance = 1e-10)
  expect_equal(o$tau2_dl, 0.03138797213141225, tolerance = 1e-10)
  expect_equal(o$Q, 19.75696274809809, tolerance = 1e-10)
  # the defining equation: generalised Q equals k - 1
  w <- 1 / (ma_vi + o$tau2)
  mu <- sum(w * ma_yi) / sum(w)
  expect_equal(sum(w * (ma_yi - mu)^2), length(ma_yi) - 1, tolerance = 1e-6)
  expect_false(o$at_boundary)
  expect_gt(o$tau2, o$tau2_dl)
})

test_that("REML matches morie.fn and exceeds ML by the df correction", {
  o <- morie_psy_ma_reml(ma_yi, ma_vi)
  expect_equal(o$tau2, 0.023026507591943877, tolerance = 1e-9)
  expect_equal(o$tau2_ml, 0.023024408757361222, tolerance = 1e-9)
  expect_equal(o$mu, 0.47994051428090045, tolerance = 1e-10)
  expect_gte(o$tau2, o$tau2_ml)
  expect_equal(o$reml_correction, o$tau2 - o$tau2_ml, tolerance = 1e-12)
  expect_true(o$converged)
  w <- 1 / (ma_vi + o$tau2)
  expect_equal(o$mu, sum(w * ma_yi) / sum(w), tolerance = 1e-12)
})

test_that("a boundary truncation is reported as one", {
  set.seed(3)
  vi <- stats::runif(15, 0.05, 0.2)
  yi <- 0.4 + stats::rnorm(15, sd = sqrt(vi))
  o <- morie_psy_ma_paule_mandel(yi, vi)
  if (o$tau2 == 0) {
    expect_true(o$at_boundary)
    expect_false(is.null(o$boundary_note))
  } else {
    expect_false(o$at_boundary)
  }
})

test_that("the tau2 estimator changes the pooled effect", {
  pm <- morie_psy_ma_paule_mandel(ma_yi, ma_vi)
  rl <- morie_psy_ma_reml(ma_yi, ma_vi)
  expect_false(isTRUE(all.equal(pm$tau2, rl$tau2, tolerance = 1e-6)))
  expect_false(isTRUE(all.equal(pm$mu, rl$mu, tolerance = 1e-9)))
  expect_false(isTRUE(all.equal(pm$se, rl$se, tolerance = 1e-9)))
})

test_that("robust variance estimation beats naive SEs under clustering", {
  set.seed(1)
  G <- 20L
  per <- 3L
  cl <- rep(seq_len(G), each = per)
  u <- stats::rnorm(G)
  x <- stats::rnorm(G * per)
  y <- 0.4 + 0.6 * x + u[cl] + stats::rnorm(G * per, sd = 0.3)
  o <- morie_psy_ma_rve(y, x, cl)
  expect_equal(o$beta[1L], 0.4, tolerance = 0.4)
  expect_equal(o$beta[2L], 0.6, tolerance = 0.3)
  Xd <- cbind(1, x)
  bn <- qr.coef(qr(Xd), y)
  e <- y - as.numeric(Xd %*% bn)
  s2 <- sum(e^2) / (length(y) - 2)
  naive <- sqrt(diag(s2 * solve(crossprod(Xd))))
  expect_gt(o$se[1L], naive[1L])
  expect_equal(o$n_clusters, G)
  expect_equal(o$n_effects, G * per)
  expect_true(all(o$df > 0))
  expect_error(morie_psy_ma_rve(stats::rnorm(9), matrix(stats::rnorm(36), 9),
                                rep(1:3, each = 3)), "number of STUDIES")
})

test_that("leave-one-out refits tau2 and flags conclusion changes", {
  set.seed(3)
  vi <- stats::runif(12, 0.005, 0.03)
  yi <- 0.3 + stats::rnorm(12, sd = 0.4) + stats::rnorm(12, sd = sqrt(vi))
  o <- morie_psy_ma_loo(yi, vi, method = "PM")
  expect_gt(stats::sd(o$tau2_loo), 1e-6)
  expect_gt(max(abs(o$tau2_loo - o$tau2_full)), 1e-3)
  expect_length(o$mu_loo, 12L)
  expect_equal(dim(o$ci_loo), c(12L, 2L))
  expect_equal(o$max_abs_delta, max(abs(o$delta_mu)), tolerance = 1e-12)
  expect_type(o$flips_significance, "logical")
  expect_error(morie_psy_ma_loo(yi[1:2], vi[1:2]), "at least 3 studies")
  expect_error(morie_psy_ma_loo(yi, vi, method = "magic"), "PM")
})
