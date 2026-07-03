# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("morie_taphonomy_schema is a typed zero-row template (no fabricated rows)", {
  s <- morie_taphonomy_schema()
  expect_s3_class(s, "data.frame")
  expect_identical(nrow(s), 0L)
  expect_true(all(c("lime_treatment", "preservation_score", "pxrf_ca_ppm") %in%
                    names(s)))
  roles <- attr(s, "role")
  expect_identical(unname(roles[["lime_treatment"]]), "treatment")
  expect_identical(unname(roles[["preservation_score"]]), "outcome")
})

test_that("preservation_delta refuses empty data", {
  expect_error(
    morie_taphonomy_preservation_delta(morie_taphonomy_schema()),
    "never fabricates"
  )
})

test_that("CATE path: 'none' reports no SE; 'bootstrap' gives a valid SE + CI", {
  set.seed(1)
  n <- 100L
  arid <- stats::rbinom(n, 1, 0.5)
  lime <- stats::rbinom(n, 1, stats::plogis(-0.3 + 0.8 * arid))
  d <- data.frame(
    lime_treatment = lime,
    preservation_score = 0.4 * lime + 0.05 * arid + stats::rnorm(n, 0, 0.2),
    temp_c = stats::rnorm(n, 15), arid = arid
  )

  r_none <- morie_taphonomy_preservation_delta(d, estimator = "cate",
                                               se_method = "none")
  expect_true(is.finite(r_none$value))       # point estimate is a double
  expect_true(is.na(r_none$se))              # no invalid SE emitted
  expect_true(is.na(r_none$p_value))
  expect_true(is.finite(r_none$cate_sd))     # heterogeneity still reported
  expect_length(r_none$cate_per_unit, nrow(d))

  r_boot <- morie_taphonomy_preservation_delta(d, estimator = "cate",
                                               se_method = "bootstrap",
                                               n_boot = 40L)
  expect_true(is.finite(r_boot$se) && r_boot$se > 0)   # valid SE
  expect_lt(r_boot$ci_lower, r_boot$ci_upper)          # ordered percentile CI
  expect_true(is.finite(r_boot$p_value))
  expect_equal(r_boot$value, r_none$value, tolerance = 1e-8)  # same point est
})

test_that("decay chain is a valid absorbing DTMC (rows sum to 1)", {
  ch <- morie_taphonomy_decay_chain(preservation = 0.6)
  expect_true(all(abs(rowSums(ch$P) - 1) < 1e-12))       # row-stochastic
  expect_identical(ch$absorbing, c("skeletal", "mummified"))
  # absorbing states are self-loops
  expect_equal(ch$P["skeletal", "skeletal"], 1)
  expect_equal(ch$P["mummified", "mummified"], 1)
})

test_that("absorption probabilities sum to 1 and rise with preservation", {
  a0 <- morie_taphonomy_decay_absorption(morie_taphonomy_decay_chain(0.0))
  a1 <- morie_taphonomy_decay_absorption(morie_taphonomy_decay_chain(0.8))
  expect_equal(sum(a0$absorption), 1, tolerance = 1e-10)
  expect_equal(sum(a1$absorption), 1, tolerance = 1e-10)
  # more preservation -> more mummification
  expect_gt(a1$absorption[["mummified"]], a0$absorption[["mummified"]])
  expect_gt(a0$expected_steps, 0)
})

test_that("decay delta is positive and simulate reaches an absorbing state", {
  d <- morie_taphonomy_decay_delta(0.8)
  expect_gt(d$delta, 0)
  expect_equal(d$p_mummified_treated - d$p_mummified_natural, d$delta,
               tolerance = 1e-12)
  path <- morie_taphonomy_decay_simulate(morie_taphonomy_decay_chain(0.8),
                                         n_steps = 500L, seed = 1L)
  expect_true(utils::tail(path, 1) %in% c("skeletal", "mummified"))
})

test_that("evidence log-likelihood matches dnorm and rejects bad sd", {
  ll <- morie_taphonomy_evidence_loglik(c(1200, 1310), mean = 1250, sd = 80)
  expect_equal(ll, sum(dnorm(c(1200, 1310), 1250, 80, log = TRUE)))
  expect_error(morie_taphonomy_evidence_loglik(1, 0, sd = 0), "sd")
})

test_that("likelihood ratio is stable in log space with correct verbal band", {
  r <- morie_taphonomy_likelihood_ratio(loglik_h1 = -3.1, loglik_h2 = -12.7)
  expect_equal(r$log_lr, -3.1 - (-12.7))
  expect_equal(r$lr, exp(-3.1 + 12.7))
  expect_true(grepl("for H1", r$verbal))         # H1 strongly favoured
  # symmetric: swapping hypotheses inverts the LR
  r2 <- morie_taphonomy_likelihood_ratio(-12.7, -3.1)
  expect_equal(r2$lr, 1 / r$lr, tolerance = 1e-10)
  expect_true(grepl("for H2", r2$verbal))
})

test_that("preservation LR favours the model the evidence resembles", {
  # evidence near the 'natural' (lime) mean -> LR supports H1
  out <- morie_taphonomy_preservation_lr(
    evidence = c(1200, 1310, 1180),
    natural = list(mean = 1250, sd = 90),
    alternative = list(mean = 300, sd = 120)
  )
  expect_gt(out$lr, 1)
  expect_equal(out$log_lr, out$loglik_h1 - out$loglik_h2, tolerance = 1e-10)
})

test_that("BHM recovers a known effect and the informative prior pulls it", {
  set.seed(1)
  n <- 200L
  lime <- stats::rbinom(n, 1, 0.5)
  df <- data.frame(preservation_score = 0.5 * lime + stats::rnorm(n, 0, 0.3),
                   lime_treatment = lime)
  # diffuse prior -> posterior near the true 0.5
  b_diffuse <- morie_taphonomy_bhm(df, covariates = "lime_treatment")
  eff <- b_diffuse$coefficients
  lime_mean <- eff$post_mean[eff$term == "lime_treatment"]
  expect_gt(lime_mean, 0.3)
  expect_lt(lime_mean, 0.7)
  expect_true(all(eff$post_sd > 0))              # posterior sds are double > 0
  expect_true(all(eff$prob_positive >= 0 & eff$prob_positive <= 1))

  # a tight prior centred at 0 shrinks the estimate toward 0
  b_tight <- morie_taphonomy_bhm(df, covariates = "lime_treatment",
    priors = list(lime_treatment = list(mean = 0, sd = 0.01)))
  lime_tight <- b_tight$coefficients$post_mean[b_tight$coefficients$term == "lime_treatment"]
  expect_lt(abs(lime_tight), abs(lime_mean))     # prior pulls toward 0
})

test_that("BHM partial-pools group intercepts with shrinkage in [0,1]", {
  set.seed(2)
  n <- 150L
  grp <- sample(letters[1:5], n, replace = TRUE)
  df <- data.frame(
    preservation_score = stats::rnorm(n),
    lime_treatment = stats::rbinom(n, 1, 0.5),
    context = grp
  )
  b <- morie_taphonomy_bhm(df, covariates = "lime_treatment", group = "context")
  expect_false(is.null(b$group_effects))
  expect_true(all(b$group_effects$shrinkage >= 0 & b$group_effects$shrinkage <= 1))
  expect_equal(nrow(b$group_effects), 5L)
})
