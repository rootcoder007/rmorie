# Time-dependent ROC (Heagerty, Lumley & Pepe 2000): Kaplan-Meier
# against survival::survfit, the KM route's Bayes-rule sensitivity and
# specificity, their reduction to empirical proportions without
# censoring, and the AUC equal to the Mann-Whitney statistic.

sr_data <- function(cens = TRUE) {
  set.seed(31)
  n <- 50
  M <- stats::rnorm(n)
  T <- stats::rexp(n, rate = exp(0.8 * M))
  C <- if (cens) stats::rexp(n, 0.3) else rep(Inf, n)
  list(time = pmin(T, C), event = as.integer(T <= C), M = M)
}
sr_km <- function(time, event, t) {
  f <- survival::survfit(survival::Surv(time, event) ~ 1)
  s <- summary(f, times = t, extend = TRUE)$surv
  s
}

test_that("Kaplan-Meier matches survfit", {
  skip_if_not_installed("survival")
  d <- sr_data()
  for (t in c(0.1, 0.5, 1.2, 3)) {
    expect_equal(morie_survroc_kaplan_meier(d$time, d$event, t), sr_km(d$time, d$event, t),
                 tolerance = 1e-12)
  }
  cv <- morie_survroc_kaplan_meier(c(2, 1, 3, 3), c(1, 0, 1, 0))
  expect_equal(cv, list(c(0, 1), c(2, 2 / 3), c(3, 1 / 3)))
  expect_error(morie_survroc_kaplan_meier(1:2, 1), "2 times but 1")
  expect_error(morie_survroc_kaplan_meier(-1, 1), "negative")
  expect_error(morie_survroc_kaplan_meier(1, 2), "0 \\(censored\\) or 1")
  expect_error(morie_survroc_kaplan_meier(numeric(0), integer(0)), "no subjects")
})

test_that("the KM route applies Bayes' rule within the marker subsets", {
  skip_if_not_installed("survival")
  d <- sr_data()
  t <- 0.8
  cc <- 0.2
  hi <- d$M > cc
  S <- sr_km(d$time, d$event, t)
  se <- (1 - sr_km(d$time[hi], d$event[hi], t)) * mean(hi) / (1 - S)
  sp <- sr_km(d$time[!hi], d$event[!hi], t) * mean(!hi) / S
  expect_equal(morie_survroc_sensitivity(d$time, d$event, d$M, cc, t), se, tolerance = 1e-12)
  expect_equal(morie_survroc_specificity(d$time, d$event, d$M, cc, t), sp, tolerance = 1e-12)
  expect_error(morie_survroc_sensitivity(d$time, d$event, d$M, cc, t, route = "empirical"),
               "censored before")
  expect_error(morie_survroc_sensitivity(d$time, d$event, d$M, cc, t, route = "ipcw"), "route must be")
  expect_error(morie_survroc_sensitivity(d$time, d$event, d$M, cc, 0), "horizon must be positive")
  expect_error(morie_survroc_sensitivity(d$time, d$event, d$M, cc, 1e-9), "no events")
})

test_that("without censoring both routes are the empirical proportions and AUC is Mann-Whitney", {
  d <- sr_data(cens = FALSE)
  t <- stats::median(d$time)
  cases <- d$time <= t
  for (cc in c(-0.5, 0, 0.7)) {
    se <- mean(d$M[cases] > cc)
    sp <- mean(d$M[!cases] <= cc)
    for (rt in c("km", "empirical")) {
      expect_equal(morie_survroc_sensitivity(d$time, d$event, d$M, cc, t, rt), se, tolerance = 1e-12)
      expect_equal(morie_survroc_specificity(d$time, d$event, d$M, cc, t, rt), sp, tolerance = 1e-12)
    }
  }
  mw <- mean(outer(d$M[cases], d$M[!cases], ">") + 0.5 * outer(d$M[cases], d$M[!cases], "=="))
  expect_equal(morie_survroc_auc_at(d$time, d$event, d$M, t), mw, tolerance = 1e-12)
  expect_equal(morie_survroc_auc_at(d$time, d$event, d$M, t, "empirical"), mw, tolerance = 1e-12)
  roc <- morie_survroc_roc_at(d$time, d$event, d$M, t)
  expect_identical(length(roc), length(unique(d$M)) + 2L)
  expect_equal(c(roc[[1]]$sensitivity, roc[[1]]$fpr), c(0, 0), tolerance = 1e-12)
})

test_that("morie_survroc reports the AUC with the risk-set bookkeeping", {
  d <- sr_data()
  t <- 0.8
  r <- morie_survroc(d$time, d$event, d$M, t)
  expect_equal(r$auc, morie_survroc_auc_at(d$time, d$event, d$M, t), tolerance = 1e-15)
  expect_identical(r$n_events_by_t, sum(d$time <= t & d$event == 1))
  expect_identical(r$n_censored_before_t, sum(d$time < t & d$event == 0))
  expect_equal(r$survival_at_t, morie_survroc_kaplan_meier(d$time, d$event, t))
  expect_error(morie_survroc(d$time, d$event, d$M[-1], t), "markers but")
})
