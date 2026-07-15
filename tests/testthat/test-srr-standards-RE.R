# SPDX-License-Identifier: AGPL-3.0-or-later
#
# srr "RE" (regression) testing standards: noiseless-relationship
# recovery and model-object accessor behaviour.

.mk_did_re <- function(seed, att, n = 4000L, noise = TRUE, sd = 1.0) {
  set.seed(seed)
  d    <- rep(0:1, each = n / 2L)
  post <- rep(rep(0:1, each = n / 4L), 2L)
  y    <- 1 + 0.5 * d + 0.3 * post + att * (d * post)
  if (noise) y <- y + stats::rnorm(n, sd = sd)
  data.frame(y = y, d = d, post = post)
}

test_that("RE7.0/RE7.1 noiseless exact relationships are recovered exactly", {
  # With no noise the 2x2 DiD estimate equals the true ATT to numerical
  # tolerance (exact linear-algebra recovery).
  res <- morie_did_2x2(.mk_did_re(1L, att = 2.5, noise = FALSE),
                       "y", "d", "post")
  expect_equal(res$estimate, 2.5, tolerance = 1e-8)
})

test_that("RE7.0a perfectly collinear (single-level) terms are rejected", {
  # A predictor with no variation carries no information; .viable_terms
  # rejects it rather than producing a rank-deficient design.
  df <- data.frame(x = stats::rnorm(40), z = rep(1, 40))  # z constant
  expect_false("z" %in% suppressWarnings(.viable_terms(df, c("x", "z"))))
})

test_that("RE4/RE7.3 model object exposes the documented accessor fields", {
  res <- morie_did_2x2(.mk_did_re(2L, att = 2.0), "y", "d", "post")
  expect_true(all(c("estimate", "std_error", "t_stat", "p_value",
                    "ci_lower", "ci_upper", "n_treated", "n_control",
                    "method", "details") %in% names(res)))
  # coefficients + vcov-derived SEs are carried in details (RE4.2/RE4.6)
  expect_true(!is.null(res$details$all_coefficients))
  expect_true(!is.null(res$details$all_se))
  # CI brackets the point estimate (RE4.3)
  expect_lte(res$ci_lower, res$estimate)
  expect_gte(res$ci_upper, res$estimate)
})
