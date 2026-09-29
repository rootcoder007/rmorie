# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/tmlcen_native.R (IPCW for right- and interval-censored
# data; van der Laan & Rose 2018 Sec. 8.5, Hernan & Robins 2020 Ch. 17).
# Interval coarsening is derived from its definition, the censoring
# survival curve from glm's discrete hazards, and the Sec. 8.5
# estimator recomputed term by term.

.tc_t <- c(1, 2, 2, 3, 3, 1, 2, 3, 1, 3, 2, 3)
.tc_d <- c(1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0)
.tc_c <- c(0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1)
.tc_a <- c(1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0)
.tc_W <- cbind(cos(1:12))

test_that("coarsen_interval brackets the event between monitoring times", {
  r <- morie_coarsen_interval(c(1, 2, 3, 4), c(0, 0, 1, 1))
  expect_equal(r$L, 2)
  expect_equal(r$R, 3)
  # unsorted input is sorted first
  expect_equal(morie_coarsen_interval(c(3, 1, 4, 2), c(1, 0, 1, 0)), list(L = 2, R = 3))
  # never positive: right-censored at the last visit
  expect_equal(morie_coarsen_interval(c(1, 5), c(0, 0)), list(L = 5, R = Inf))
  # positive at the first visit: left-censored
  expect_equal(morie_coarsen_interval(c(2, 4), c(1, 1)), list(L = 0, R = 2))
  expect_identical(morie_tmlcen(c(1, 2), c(0, 1)), list(L = 1, R = 2))
  expect_error(morie_coarsen_interval(1:3, c(0, 1)), "3 monitoring times but 2 indicators")
  expect_error(morie_coarsen_interval(numeric(0), numeric(0)), "no monitoring times")
  expect_error(morie_coarsen_interval(1:2, c(0, 2)), "Delta must be 0/1")
})

test_that("censoring_survival is the product limit of the fitted discrete hazard", {
  r <- morie_censoring_survival(.tc_t, .tc_c, A = .tc_a, W = .tc_W)
  grid <- sort(unique(.tc_t))
  expect_equal(r$grid, grid)
  # rebuild the person-time panel and fit the same logistic hazard
  kk <- c()
  av <- c()
  wv <- c()
  y <- c()
  for (i in seq_along(.tc_t)) for (k in seq_along(grid)) {
    if (.tc_t[i] < grid[k]) break
    kk <- c(kk, k)
    av <- c(av, .tc_a[i])
    wv <- c(wv, .tc_W[i, 1])
    y <- c(y, as.numeric(.tc_c[i] == 1 && .tc_t[i] == grid[k]))
  }
  b <- coef(glm(y ~ kk + av + wv, family = binomial(), control = list(epsilon = 1e-14, maxit = 200)))
  expect_equal(r$b, unname(b), tolerance = 1e-6)
  for (i in c(1, 5, 12)) {
    h <- plogis(b[1] + b[2] * seq_along(grid) + b[3] * .tc_a[i] + b[4] * .tc_W[i, 1])
    expect_equal(r$G[[i]], cumprod(1 - h), tolerance = 1e-6)
    expect_true(all(diff(r$G[[i]]) <= 1e-12))
  }
  # no covariates at all: one curve shared by everybody
  n0 <- morie_censoring_survival(.tc_t, .tc_c)
  expect_equal(n0$G[[1]], n0$G[[2]], tolerance = 1e-12)
  expect_error(morie_censoring_survival(.tc_t, .tc_c[-1]), "12 times but 11 censoring")
  expect_error(morie_censoring_survival(.tc_t, .tc_c, W = .tc_W[-1, , drop = FALSE]), "11 rows but data has 12")
})

test_that("ipcw_interval is the Sec. 8.5 monitoring-weighted mean", {
  Tm <- list(c(1, 2, 4), c(2, 3), c(1, 5), c(2, 4))
  Dm <- list(c(0, 0, 1), c(0, 1), c(1, 1), c(0, 0))
  A <- c(1, 1, 0, 1)
  W <- cbind(c(0.5, -0.5, 1, 0))
  gsup <- c(0.6, 0.4, 0.7, 0.5)
  rr <- function(tt) 1 + tt
  ex <- 0
  for (i in seq_along(A)) {
    if (A[i] != 1) next
    dens <- 1 / (max(Tm[[i]]) - min(Tm[[i]]))
    s <- sum((1 - Dm[[i]]) * rr(Tm[[i]]) / dens)
    ex <- ex + s / length(Tm[[i]]) / gsup[i]
  }
  expect_equal(morie_ipcw_interval(W, A, Tm, Dm, a = 1, r = rr, g = gsup), ex / 4, tolerance = 1e-12)
  # a supplied monitoring density replaces the uniform default
  gc <- lapply(Tm, function(v) rep(0.25, length(v)))
  ex2 <- 0
  for (i in seq_along(A)) {
    if (A[i] != 1) next
    ex2 <- ex2 + sum((1 - Dm[[i]]) * rr(Tm[[i]]) / 0.25) / length(Tm[[i]]) / gsup[i]
  }
  expect_equal(morie_ipcw_interval(W, A, Tm, Dm, a = 1, r = rr, g = gsup, gc = gc), ex2 / 4,
               tolerance = 1e-12)
  # a = 0 keeps only the control subject
  expect_equal(morie_ipcw_interval(W, A, Tm, Dm, a = 0, g = gsup), 0, tolerance = 1e-12)
  # with no g supplied it is fitted from W by logistic regression
  fitted <- morie_ipcw_interval(W, A, Tm, Dm, a = 1)
  expect_true(is.finite(fitted))
  expect_gt(fitted, 0)
  expect_error(morie_ipcw_interval(W, A, Tm, Dm, g = c(0, 1, 1, 1)), "positivity fails")
  expect_error(morie_ipcw_interval(W, A, list(numeric(0), 1, 1, 1), Dm, g = gsup), "no monitoring times")
  expect_error(morie_ipcw_interval(W, A, Tm[1:2], Dm, g = gsup), "4 treatments but 2 monitoring rows")
})

test_that("tmle_censoring gives an IPCW survival difference, or the interval estimator", {
  r <- morie_tmle_censoring(.tc_t, .tc_d, .tc_c, .tc_a, .tc_W)
  expect_equal(r$grid, sort(unique(.tc_t)))
  expect_length(r$survival_treated, 3L)
  expect_equal(r$estimate, r$survival_treated[3] - r$survival_control[3], tolerance = 1e-12)
  expect_true(all(diff(r$survival_treated) <= 1e-12))
  expect_true(all(r$survival_treated >= 0 & r$survival_treated <= 1))
  expect_gte(r$max_weight, 1)
  expect_equal(r$n, 12L)
  expect_true(is.finite(r$naive))
  expect_true(is.finite(r$unadjusted))
  expect_equal(morie_tmlecensoring(.tc_t, .tc_d, .tc_c, .tc_a, .tc_W)$estimate, r$estimate)
  iv <- morie_tmle_censoring(list(c(1, 2), c(2, 3)), list(c(0, 1), c(0, 0)), NULL, c(1, 1),
                             cbind(c(0, 1)), kind = "interval", g = c(0.5, 0.5))
  expect_equal(iv$estimate, morie_ipcw_interval(cbind(c(0, 1)), c(1, 1), list(c(1, 2), c(2, 3)),
                                                list(c(0, 1), c(0, 0)), g = c(0.5, 0.5)),
               tolerance = 1e-12)
  expect_equal(iv$n, 2L)
  expect_error(morie_tmle_censoring(.tc_t, .tc_d, .tc_c, .tc_a, .tc_W, kind = "left"),
               "kind must be 'right' or 'interval'")
  expect_error(morie_tmle_censoring(.tc_t, .tc_d[-1], .tc_c, .tc_a, .tc_W), "12 times but 11 event")
  expect_error(morie_tmle_censoring(.tc_t, rep(1, 12), rep(1, 12), .tc_a, .tc_W), "both an event and censored")
})
