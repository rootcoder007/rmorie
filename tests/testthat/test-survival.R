# SPDX-License-Identifier: AGPL-3.0-or-later
# Tests for R/survival.R -- KM, Nelson-Aalen, Cox, AFT, RMST, log-rank,
# concordance, CIF, residuals, landmark, left-truncated KM.

.make_surv <- function(n = 100L, seed = 1L) {
  set.seed(seed)
  x <- rnorm(n)
  t <- rexp(n, rate = exp(0.5 * x))
  c <- rexp(n, rate = 0.2)
  time <- pmin(t, c)
  event <- as.integer(t <= c)
  data.frame(time = time, event = event, x = x)
}

# ---------------------------------------------------------------------------
# morie_survival_km
# ---------------------------------------------------------------------------

test_that("morie_survival_km returns a tidy list", {
  d <- .make_surv()
  res <- morie_survival_km(d$time, d$event)
  expect_type(res, "list")
  expect_true(all(c("times", "survival", "ci_lower", "ci_upper",
                    "at_risk", "events", "censored") %in% names(res)))
  expect_true(all(res$survival >= 0 & res$survival <= 1))
  # Survival monotone non-increasing
  expect_true(all(diff(res$survival) <= 1e-8))
})

test_that("morie_survival_km log-log CI method works", {
  d <- .make_surv(50)
  res <- morie_survival_km(d$time, d$event, ci_method = "log-log")
  expect_match(res$method, "log-log")
  # Log-log CIs are NA when S(t) is 0 or 1; check only the well-defined entries.
  ok <- !is.na(res$ci_lower) & !is.na(res$survival)
  expect_true(all(res$ci_lower[ok] <= res$survival[ok] + 1e-8))
})

test_that("morie_survival_km accepts NA / negative times by dropping them", {
  t <- c(1, 2, NA, 3, -1, 4)
  e <- c(1, 0, 1, 1, 0, 1)
  res <- morie_survival_km(t, e)
  expect_true(length(res$times) >= 1)
})

# ---------------------------------------------------------------------------
# morie_survival_nelsonaalen
# ---------------------------------------------------------------------------

test_that("morie_survival_nelsonaalen returns monotone-increasing cumhaz", {
  d <- .make_surv()
  res <- morie_survival_nelsonaalen(d$time, d$event)
  expect_true(all(res$cumhaz >= 0))
  expect_true(all(diff(res$cumhaz) >= -1e-8))
  expect_true(all(res$ci_lower <= res$cumhaz + 1e-8))
})

# ---------------------------------------------------------------------------
# morie_survival_logrank
# ---------------------------------------------------------------------------

test_that("morie_survival_logrank: groups with different hazards reject", {
  set.seed(1)
  n <- 80
  t1 <- rexp(n, rate = 1.0); t2 <- rexp(n, rate = 0.3)
  time <- c(t1, t2); event <- rep(1L, 2 * n)
  group <- c(rep("A", n), rep("B", n))
  res <- morie_survival_logrank(time, event, group)
  expect_named(res, c("method", "test_statistic", "p_value", "df",
                      "n_groups", "n_total"),
               ignore.order = TRUE)
  expect_equal(res$n_groups, 2L)
  expect_lt(res$p_value, 0.01)
})

test_that("morie_survival_logrank weight variants run", {
  d <- .make_surv()
  group <- rep(c("a", "b"), length.out = nrow(d))
  for (w in c("logrank", "peto", "gehan", "tarone")) {
    r <- morie_survival_logrank(d$time, d$event, group, weight = w)
    expect_true(is.numeric(r$test_statistic))
  }
})

# ---------------------------------------------------------------------------
# morie_survival_cox + residuals
# ---------------------------------------------------------------------------

test_that("morie_survival_cox recovers a known coefficient sign", {
  d <- .make_surv(150)
  res <- morie_survival_cox(d, "time", "event", "x")
  expect_named(res$coefficients, "x")
  expect_gt(res$coefficients[["x"]], 0)  # positive x -> higher hazard
  expect_equal(unname(res$hazard_ratios[["x"]]),
               exp(res$coefficients[["x"]]), tolerance = 1e-6)
  expect_true(res$concordance > 0.5)
})

test_that("morie_survival_cox breslow ties option works", {
  d <- .make_surv(100)
  res <- morie_survival_cox(d, "time", "event", "x", ties = "breslow")
  expect_match(res$method, "breslow")
})

test_that("morie_survival_schoenfeld returns zph table", {
  d <- .make_surv(80)
  cox <- morie_survival_cox(d, "time", "event", "x")
  sch <- morie_survival_schoenfeld(cox)
  expect_true(is.matrix(sch$residuals) || is.numeric(sch$residuals))
  expect_s3_class(sch$zph_table, "data.frame")
})

test_that("morie_survival_martingale / deviance / coxsnell run", {
  d <- .make_surv(80)
  cox <- morie_survival_cox(d, "time", "event", "x")
  m <- morie_survival_martingale(cox)
  dv <- morie_survival_deviance(cox)
  cs <- morie_survival_coxsnell(cox)
  expect_length(m, nrow(d))
  expect_length(dv, nrow(d))
  expect_true(is.numeric(cs))
})

test_that("morie_survival_schoenfeld errors on non-morie input", {
  expect_error(morie_survival_schoenfeld(list(coefficients = 1)),
               regexp = "morie_survival_cox")
})

# ---------------------------------------------------------------------------
# morie_survival_aft / parametric / compare
# ---------------------------------------------------------------------------

test_that("morie_survival_aft fits weibull and returns AIC", {
  d <- .make_surv(150)
  res <- morie_survival_aft(d, "time", "event", "x", dist = "weibull")
  expect_match(res$distribution, "weibull")
  expect_true(is.finite(res$aic))
  expect_equal(res$n_observations, nrow(d))
})

test_that("morie_survival_parametric returns the chosen dist", {
  d <- .make_surv(100)
  for (dist in c("exponential", "weibull", "lognormal")) {
    r <- morie_survival_parametric(d$time, d$event, dist = dist)
    expect_equal(r$distribution, dist)
    expect_true(is.finite(r$aic))
  }
})

test_that("morie_survival_compare_parametric ranks by AIC", {
  d <- .make_surv(120)
  cmp <- morie_survival_compare_parametric(d$time, d$event)
  expect_s3_class(cmp, "data.frame")
  expect_true(all(diff(cmp$aic) >= -1e-6))
})

# ---------------------------------------------------------------------------
# morie_survival_concordance
# ---------------------------------------------------------------------------

test_that("morie_survival_concordance: higher risk -> earlier event", {
  set.seed(1)
  n <- 100
  x <- rnorm(n)
  t <- rexp(n, rate = exp(x))
  e <- rep(1L, n)
  c <- morie_survival_concordance(t, e, x)
  expect_gt(c, 0.5)
})

test_that("morie_survival_concordance ~0.5 for random risk", {
  set.seed(1)
  d <- .make_surv(100)
  c <- morie_survival_concordance(d$time, d$event, rnorm(nrow(d)))
  expect_true(c > 0.3 && c < 0.7)
})

# ---------------------------------------------------------------------------
# morie_survival_rmst / rmst_diff
# ---------------------------------------------------------------------------

test_that("morie_survival_rmst returns rmst <= tau", {
  d <- .make_surv(80)
  res <- morie_survival_rmst(d$time, d$event, tau = max(d$time))
  expect_true(res$rmst > 0)
  expect_true(res$rmst <= res$tau + 1e-6)
  expect_true(res$ci_lower <= res$rmst && res$ci_upper >= res$rmst)
})

test_that("morie_survival_rmst_diff is positive when group1 has longer survival", {
  set.seed(1)
  t1 <- rexp(80, rate = 0.3); e1 <- rep(1L, 80)
  t2 <- rexp(80, rate = 1.0); e2 <- rep(1L, 80)
  res <- morie_survival_rmst_diff(t1, e1, t2, e2)
  expect_gt(res$rmst_diff, 0)
  expect_true(is.finite(res$p_value))
})

# ---------------------------------------------------------------------------
# morie_survival_cif (Aalen-Johansen)
# ---------------------------------------------------------------------------

test_that("morie_survival_cif returns monotone non-decreasing CIF", {
  set.seed(1)
  n <- 80
  time <- rexp(n, rate = 0.5)
  event <- sample(0:2, n, replace = TRUE, prob = c(0.3, 0.4, 0.3))
  res <- morie_survival_cif(time, event, event_of_interest = 1)
  expect_true(all(res$cif >= 0))
  expect_true(all(diff(res$cif) >= -1e-8))
})

# ---------------------------------------------------------------------------
# morie_survival_finegray (cmprsk delegation)
# ---------------------------------------------------------------------------

test_that("morie_survival_finegray runs when cmprsk available", {
  skip_if_not_installed("cmprsk")
  set.seed(1)
  n <- 80
  d <- data.frame(time = rexp(n, 0.5),
                  event = sample(0:2, n, replace = TRUE,
                                  prob = c(0.3, 0.4, 0.3)),
                  x = rnorm(n))
  res <- morie_survival_finegray(d, "time", "event", "x")
  expect_named(res$coefficients, "x")
  expect_equal(unname(res$hazard_ratios[["x"]]),
               exp(res$coefficients[["x"]]), tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# morie_survival_hr / landmark / left-truncated-km
# ---------------------------------------------------------------------------

test_that("morie_survival_hr requires exactly two groups", {
  set.seed(1)
  t <- rexp(60); e <- rep(1L, 60)
  expect_error(morie_survival_hr(t, e, sample(0:2, 60, replace = TRUE)),
               regexp = "exactly 2")
})

test_that("morie_survival_hr returns hr/ci/log_hr/se for 2-group data", {
  set.seed(1)
  t <- c(rexp(40, 1.0), rexp(40, 0.3))
  e <- rep(1L, 80)
  g <- rep(c("A", "B"), each = 40)
  res <- morie_survival_hr(t, e, g)
  expect_named(res, c("hr", "ci_lower", "ci_upper",
                      "p_value", "log_hr", "se"), ignore.order = TRUE)
  expect_gt(res$hr, 0)
})

test_that("morie_survival_landmark drops < landmark and shifts time", {
  d <- data.frame(time = c(1, 3, 5, 7), event = c(1, 0, 1, 1))
  out <- morie_survival_landmark(d, "time", "event", landmark_time = 3)
  expect_equal(out$time, c(0, 2, 4))
  expect_equal(nrow(out), 3L)
})

test_that("morie_survival_left_truncated_km returns step survival", {
  set.seed(1)
  n <- 60
  entry <- runif(n, 0, 1)
  exit  <- entry + rexp(n, 0.5)
  event <- rbinom(n, 1, 0.7)
  res <- morie_survival_left_truncated_km(entry, exit, event)
  expect_true(all(res$survival >= 0 & res$survival <= 1))
})

# ---------------------------------------------------------------------------
# morie_survival_turnbull (interval-censored)
# ---------------------------------------------------------------------------

test_that("morie_survival_turnbull returns NPMLE list", {
  set.seed(1)
  L <- runif(40, 0, 2)
  R <- L + runif(40, 0.5, 3)
  res <- morie_survival_turnbull(L, R)
  expect_true(is.numeric(res$survival))
  expect_match(res$method, "Turnbull")
})
