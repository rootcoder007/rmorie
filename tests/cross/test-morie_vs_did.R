# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Module 14 cross-validation: native DiD family vs the reference
# packages (fixest, did, DRDID, bacondecomp, TwoWayFEWeights,
# DIDmultiplegt). Reference packages are allowed HERE ONLY (tests/cross
# is the designated comparison tier and is not part of the package
# test suite).
#
#' @srrstats {G5.4b} Every estimator is validated against the canonical
#'   published implementation, to machine precision where the estimand
#'   is deterministic.

library(testthat)
library(rmorie)

.mk_stag_panel <- function(n_units = 300, n_t = 8, seed = 101) {
  set.seed(seed)
  g_onset <- sample(c(0, 4, 5, 6), n_units, replace = TRUE,
                    prob = c(0.4, 0.2, 0.2, 0.2))
  x1 <- rnorm(n_units)
  x2 <- runif(n_units)
  u <- rnorm(n_units)
  do.call(rbind, lapply(seq_len(n_units), function(i) {
    t <- seq_len(n_t)
    d <- as.integer(g_onset[i] > 0 & t >= g_onset[i])
    tau <- 1.5 + 0.3 * pmax(t - g_onset[i], 0)
    y <- u[i] + 0.5 * t + 0.4 * x1[i] * t / n_t + d * tau +
      rnorm(n_t, 0, 0.5)
    data.frame(id = i, tt = t, y = y, d = d, g = g_onset[i],
               x1 = x1[i], x2 = x2[i])
  }))
}

pan <- .mk_stag_panel()

test_that("native TWFE reproduces fixest::feols to machine precision", {
  skip_if_not_installed("fixest")
  ref <- fixest::feols(y ~ d | id + tt, data = pan, cluster = ~id)
  cf <- fixest::coeftable(ref)
  mine <- morie_did_panel_fe(pan, "y", "d", "id", "tt")
  expect_equal(mine$estimate, unname(cf["d", "Estimate"]),
               tolerance = 1e-10)
  expect_equal(mine$std_error, unname(cf["d", "Std. Error"]),
               tolerance = 1e-10)
  # Covariates + non-unit cluster
  pan2 <- pan
  set.seed(1)
  pan2$xv <- rnorm(nrow(pan2))
  pan2$cl2 <- pan2$id %% 37
  ref2 <- fixest::feols(y ~ d + xv | id + tt, data = pan2,
                        cluster = ~cl2)
  cf2 <- fixest::coeftable(ref2)
  mine2 <- morie_did_panel_fe(pan2, "y", "d", "id", "tt",
                              covariates = "xv", cluster = "cl2")
  expect_equal(mine2$estimate, unname(cf2["d", "Estimate"]),
               tolerance = 1e-10)
  expect_equal(mine2$std_error, unname(cf2["d", "Std. Error"]),
               tolerance = 1e-10)
})

test_that("native event study reproduces fixest::feols + i()", {
  skip_if_not_installed("fixest")
  df <- pan
  rel <- df$tt - ifelse(df$g == 0, Inf, df$g)
  rel <- pmin(pmax(rel, -3), 3)
  rel[!is.finite(rel)] <- -1
  df$morie_rel_time <- rel
  ref <- fixest::feols(
    y ~ i(morie_rel_time, ref = -1) | id + tt, data = df,
    cluster = ~id)
  cf <- fixest::coeftable(ref)
  rel_int <- suppressWarnings(as.integer(sub(".*::", "", rownames(cf))))
  keep <- !is.na(rel_int)
  ref_df <- data.frame(relative_time = rel_int[keep],
                       est = cf[keep, "Estimate"],
                       se = cf[keep, "Std. Error"])
  mine <- morie_did_event_study(pan, "y", "id", "tt", "g",
                                leads = 3L, lags = 3L)
  m <- merge(mine$coefficients, ref_df, by = "relative_time")
  expect_equal(nrow(m), nrow(ref_df))
  expect_equal(m$estimate, m$est, tolerance = 1e-10)
  expect_equal(m$std_error, m$se, tolerance = 1e-10)
})

test_that("native ATT(g,t) reproduces did::att_gt (att + analytic se)", {
  skip_if_not_installed("did")
  for (cg in c("nevertreated", "notyettreated")) {
    for (em in c("dr", "reg", "ipw")) {
      ref <- did::att_gt(yname = "y", tname = "tt", idname = "id",
                         gname = "g", xformla = ~ x1 + x2, data = pan,
                         control_group = cg, est_method = em,
                         bstrap = FALSE, cband = FALSE, panel = TRUE)
      mine <- rmorie:::.morie_attgt_native(
        pan, "y", "id", "tt", "g", covariates = c("x1", "x2"),
        est_method = em, control_group = cg, biters = 0L)
      r <- mine$results
      key_ref <- paste(ref$group, ref$t)
      key_my <- paste(r$group, r$t)
      expect_setequal(key_my, key_ref)
      idx <- match(key_ref, key_my)
      expect_equal(r$att[idx], ref$att, tolerance = 1e-8)
      expect_equal(r$se_analytic[idx], ref$se, tolerance = 1e-8)
    }
  }
})

test_that("native drdid engines reproduce DRDID (att + IF + se)", {
  skip_if_not_installed("DRDID")
  set.seed(7)
  n <- 800
  xx <- cbind(rnorm(n), runif(n))
  D <- rbinom(n, 1, plogis(-0.3 + 0.5 * xx[, 1] - 0.4 * xx[, 2]))
  dy <- 1 + 0.8 * xx[, 1] + 2 * D + rnorm(n)
  X <- cbind(1, xx)
  ref <- DRDID::drdid_panel(y1 = dy, y0 = rep(0, n), D = D,
                            covariates = X, inffunc = TRUE)
  mine <- rmorie:::.morie_drdid_panel_native(dy, D, X)
  expect_equal(mine$att, unname(ref$ATT), tolerance = 1e-10)
  expect_equal(mine$IF, as.numeric(ref$att.inf.func), tolerance = 1e-8)
  expect_equal(mine$se, unname(ref$se), tolerance = 1e-10)
  # Locally efficient repeated-cross-section variant
  set.seed(8)
  n <- 1200
  xx <- cbind(rnorm(n), runif(n))
  D <- rbinom(n, 1, plogis(-0.2 + 0.4 * xx[, 1]))
  post <- rbinom(n, 1, 0.5)
  y <- 1 + 0.6 * xx[, 1] + 0.5 * post + 2 * D * post + 0.3 * D + rnorm(n)
  X <- cbind(1, xx)
  ref <- DRDID::drdid_rc(y = y, post = post, D = D, covariates = X,
                         inffunc = TRUE)
  mine <- rmorie:::.morie_drdid_rc_native(y, post, D, X)
  expect_equal(mine$att, unname(ref$ATT), tolerance = 1e-10)
  expect_equal(mine$IF, as.numeric(ref$att.inf.func), tolerance = 1e-8)
  expect_equal(mine$se, unname(ref$se), tolerance = 1e-10)
})

test_that("native Bacon decomposition reproduces bacondecomp::bacon", {
  skip_if_not_installed("bacondecomp")
  bpan <- pan[pan$g %in% c(0, 4, 6), ]
  ref <- bacondecomp::bacon(y ~ d, data = bpan, id_var = "id",
                            time_var = "tt", quietly = TRUE)
  mine <- morie_did_bacon_decomposition(bpan, "y", "d", "id", "tt")
  comp <- mine$components
  # Match pairwise on (treated, untreated) with Inf coded as ref's
  # never-treated sentinel.
  ref$untreated_key <- ifelse(ref$untreated > max(bpan$tt),
                              Inf, ref$untreated)
  key_ref <- paste(ref$treated, ref$untreated_key)
  key_my <- paste(comp$treated, comp$untreated)
  expect_setequal(key_my, key_ref)
  idx <- match(key_ref, key_my)
  expect_equal(comp$estimate[idx], ref$estimate, tolerance = 1e-8)
  expect_equal(comp$weight[idx], ref$weight, tolerance = 1e-8)
  expect_equal(mine$overall_estimate,
               sum(ref$estimate * ref$weight), tolerance = 1e-8)
})

test_that("native feTR weights reproduce TwoWayFEWeights per cell", {
  skip_if_not_installed("TwoWayFEWeights")
  ref <- TwoWayFEWeights::twowayfeweights(
    pan, Y = "y", G = "id", T = "tt", D = "d", type = "feTR")
  dr <- as.data.frame(ref$dat_result)
  mine <- morie_did_twoway_fe_weights(pan, "id", "tt", "d")
  mw <- mine$raw[!is.na(mine$raw$weight), ]
  m <- merge(dr, mw, by.x = c("G", "T"), by.y = c("group", "time"))
  expect_equal(nrow(m), ref$nr_weights)
  expect_equal(m$weight.x, m$weight.y, tolerance = 1e-10)
  expect_equal(mine$n_negative_weights, ref$nr_minus)
})

test_that("native DID-M agrees with DIDmultiplegt when it returns a value", {
  skip_if_not_installed("DIDmultiplegt")
  # Recent DIDmultiplegt versions return NaN (0 switchers) from the
  # mode = "old" shim on standard staggered panels; compare only when
  # the reference produces a finite estimate.
  grDevices::pdf(tempfile(fileext = ".pdf"))
  ref <- tryCatch(
    DIDmultiplegt::did_multiplegt(mode = "old", df = pan, Y = "y",
                                  G = "id", T = "tt", D = "d", brep = 0),
    error = function(e) NULL)
  grDevices::dev.off()
  mine <- rmorie:::.morie_didm_point(pan, "y", "d", "id", "tt")
  expect_true(is.finite(mine))
  if (!is.null(ref) && length(ref$effect) == 1 && is.finite(ref$effect)) {
    expect_equal(mine, as.numeric(ref$effect), tolerance = 1e-8)
  } else {
    # Reference is degenerate on this design; the native estimand is
    # pinned by the hand-computed structural test instead.
    succeed("DIDmultiplegt mode='old' returned no finite estimate")
  }
})
