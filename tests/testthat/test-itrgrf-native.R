# Anchors for individualised treatment rules from a causal forest.
#
# The doubly robust score is the thing to hold this to, because its
# defining property is checkable directly: the estimated value of a rule
# stays right when EITHER the outcome models or the propensity is wrong,
# and only goes wrong when both are. The file sat at 6.7% coverage with
# no test naming any of its functions.

dgp <- function(n = 30000, seed = 1) {
  set.seed(seed)
  X <- runif(n, -1, 1)
  tau <- 0.8 * X               # treatment helps where X > 0
  mu0 <- 1 + 0.5 * X
  e <- plogis(0.9 * X)         # treatment is not randomised
  W <- rbinom(n, 1, e)
  y <- mu0 + W * tau + rnorm(n, 0, 0.3)
  list(n = n, X = X, tau = tau, mu0 = mu0, mu1 = mu0 + tau, e = e, W = W, y = y)
}

test_that("the rule treats exactly where the effect exceeds the cost", {
  expect_equal(.itrgrf_policy_from_tau(c(-1, 0, 0.5), 0), c(0, 0, 1))
  # a cost raises the bar
  expect_equal(.itrgrf_policy_from_tau(c(-1, 0, 0.5), 0.6), c(0, 0, 0))
  expect_equal(.itrgrf_policy_from_tau(c(1, 2), 1.5), c(0, 1))
  # exactly at the cost is not worth treating, the comparison being strict
  expect_equal(.itrgrf_policy_from_tau(0.5, 0.5), 0)
  # a negative cost, treatment being beneficial in itself, treats more
  expect_equal(.itrgrf_policy_from_tau(c(-0.2, 0.1), -0.5), c(1, 1))
})

test_that("the doubly robust score is the AIPW form for the rule", {
  # mu_d + 1{W = d}(y - mu_W)/P(W | X), written out here
  d <- dgp(n = 200)
  rule <- .itrgrf_policy_from_tau(d$tau, 0)
  got <- .itrgrf_dr_scores(d$y, d$W, d$mu1, d$mu0, d$e, rule)
  want <- vapply(seq_len(d$n), function(i) {
    mu <- if (rule[i] == 1) d$mu1[i] else d$mu0[i]
    if (d$W[i] == rule[i]) {
      p <- if (d$W[i] == 1) d$e[i] else 1 - d$e[i]
      muw <- if (d$W[i] == 1) d$mu1[i] else d$mu0[i]
      mu + (d$y[i] - muw) / p
    } else {
      mu
    }
  }, numeric(1))
  expect_equal(got, want, tolerance = 1e-10)
  # a row whose observed arm is not the rule's arm contributes only the
  # outcome model, with no residual correction
  off <- which(d$W != rule)[1]
  expect_equal(got[off], if (rule[off] == 1) d$mu1[off] else d$mu0[off],
               tolerance = 1e-12)
})

test_that("the estimated value survives one wrong nuisance but not two", {
  # the defining property, and the reason this estimator is used
  d <- dgp()
  rule <- .itrgrf_policy_from_tau(d$tau, 0)
  truth <- mean(d$mu0 + rule * d$tau)
  both <- .itrgrf_rule_value(d$y, d$W, d$mu1, d$mu0, d$e, rule)
  # within a few standard errors when both are right
  expect_lt(abs(both$value - truth), 4 * both$se)
  # the outcome models alone carry it, with the propensity set to a
  # constant that is simply wrong
  out_ok <- .itrgrf_rule_value(d$y, d$W, d$mu1, d$mu0, rep(0.5, d$n), rule)
  expect_lt(abs(out_ok$value - truth), 4 * out_ok$se)
  # and the propensity alone carries it, with the outcome models zeroed
  ps_ok <- .itrgrf_rule_value(d$y, d$W, rep(0, d$n), rep(0, d$n), d$e, rule)
  expect_lt(abs(ps_ok$value - truth), 4 * ps_ok$se)
  # with both wrong it is badly biased, which is what makes the two tests
  # above meaningful rather than vacuous
  neither <- .itrgrf_rule_value(d$y, d$W, rep(0, d$n), rep(0, d$n),
                                rep(0.5, d$n), rule)
  expect_gt(abs(neither$value - truth), 20 * neither$se)
})

test_that("the value is the mean of the scores with its standard error", {
  d <- dgp(n = 500)
  rule <- .itrgrf_policy_from_tau(d$tau, 0)
  v <- .itrgrf_rule_value(d$y, d$W, d$mu1, d$mu0, d$e, rule)
  expect_equal(v$value, mean(v$scores), tolerance = 1e-12)
  expect_equal(v$se, sd(v$scores) / sqrt(length(v$scores)), tolerance = 1e-12)
  expect_length(v$scores, d$n)
  # a single observation has no standard error to report
  one <- .itrgrf_rule_value(d$y[1], d$W[1], d$mu1[1], d$mu0[1], d$e[1], rule[1])
  expect_true(is.nan(one$se))
})

test_that("a degenerate propensity for the observed arm is refused", {
  # the guard looks at the arm actually observed, so a propensity of zero
  # only matters for a treated row
  expect_error(.itrgrf_dr_scores(1, 1, 1, 0, 0, 1), "propensity of zero")
  expect_error(.itrgrf_dr_scores(1, 0, 1, 0, 1, 0), "propensity of zero")
  # and those same numbers are harmless for the other arm
  expect_silent(.itrgrf_dr_scores(1, 1, 1, 0, 1, 1))
  expect_silent(.itrgrf_dr_scores(1, 0, 1, 0, 0, 0))
})

test_that("the fitted rule is worth more than treating everyone or no one", {
  # what an individualised rule is for
  set.seed(2); n <- 600
  X <- cbind(runif(n, -1, 1), rnorm(n))
  tau <- 0.9 * X[, 1]
  W <- rbinom(n, 1, plogis(0.7 * X[, 1]))
  y <- 1 + 0.5 * X[, 1] + W * tau + rnorm(n, 0, 0.3)
  r <- morie_itrgrf(y, W, X, cost = 0, n_trees = 40)
  expect_gt(as.numeric(r$value), as.numeric(r$value_treat_all))
  expect_gt(as.numeric(r$value), as.numeric(r$value_treat_none))
  expect_equal(as.numeric(r$gain_over_treat_all),
               as.numeric(r$value) - as.numeric(r$value_treat_all),
               tolerance = 1e-10)
  expect_equal(as.numeric(r$gain_over_treat_none),
               as.numeric(r$value) - as.numeric(r$value_treat_none),
               tolerance = 1e-10)
  expect_identical(as.numeric(r$estimate), as.numeric(r$value))
  # it treats a sensible share, the true share being about a half
  expect_gt(as.numeric(r$treated_fraction), 0.2)
  expect_lt(as.numeric(r$treated_fraction), 0.8)
  # the rule is a zero-one vector, one per row
  rule <- as.numeric(unlist(r$rule))
  expect_length(rule, n)
  expect_true(all(rule %in% c(0, 1)))
  # and it mostly agrees with the oracle rule
  expect_gt(mean(rule == as.numeric(tau > 0)), 0.7)
  expect_equal(as.numeric(r$cost), 0)
  expect_equal(as.numeric(r$n), n)
})

test_that("a cost shrinks the treated share", {
  set.seed(3); n <- 500
  X <- cbind(runif(n, -1, 1), rnorm(n))
  tau <- 0.9 * X[, 1]
  W <- rbinom(n, 1, plogis(0.7 * X[, 1]))
  y <- 1 + 0.5 * X[, 1] + W * tau + rnorm(n, 0, 0.3)
  free <- morie_itrgrf(y, W, X, cost = 0, n_trees = 30)
  dear <- morie_itrgrf(y, W, X, cost = 0.6, n_trees = 30)
  expect_lte(as.numeric(dear$treated_fraction),
             as.numeric(free$treated_fraction))
  expect_equal(as.numeric(dear$cost), 0.6)
})
