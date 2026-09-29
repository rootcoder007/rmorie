# Coverage for the regression-diagnostic and model-selection files
# (dfbeta.R, dfbetb.R, dffit.R, dffits.R, covrat.R, Cstat.R, bicg.R,
# dicg.R, aikarp.R, bicarp.R, ebfmi.R): checked against stats'
# influence measures, pacf, lm and survival::concordance.

dg_X <- cbind(c(1.2, 0.4, 2.8, 1.9, 3.3, 0.7, 2.2, 4.1, 1.5, 3.8, 0.2, 2.6),
              c(0.5, 1.1, 0.3, 1.8, 0.9, 1.4, 0.2, 1.6, 0.8, 0.4, 1.9, 1.2))
dg_y <- 1 + 0.8 * dg_X[, 1] - 0.6 * dg_X[, 2] +
  c(0.3, -0.2, 0.1, 0.5, -0.4, 0.2, -0.1, 1.8, -0.3, 0.2, -0.5, 0.1)
dg_fit <- lm(dg_y ~ dg_X)

test_that("DFBETAS, DFFITS and COVRATIO match stats' influence measures", {
  a <- Dfbetas(dg_X, dg_y)
  expect_equal(a$dfbetas, unname(dfbetas(dg_fit)), tolerance = 1e-10)
  expect_equal(a$leverage, unname(hatvalues(dg_fit)), tolerance = 1e-12)
  expect_equal(a$sigma_i, unname(influence(dg_fit)$sigma), tolerance = 1e-10)
  b <- Dfbetb(dg_y, dg_X)
  expect_equal(b$dfbetas, unname(dfbetas(dg_fit)), tolerance = 1e-10)
  expect_equal(b$estimate, max(abs(dfbetas(dg_fit))), tolerance = 1e-10)
  expect_equal(b$n_influential, sum(apply(abs(dfbetas(dg_fit)) > 2 / sqrt(12), 1, any)))
  f0 <- lm(dg_y ~ 0 + dg_X)
  expect_equal(Dfbetb(dg_y, dg_X, intercept = FALSE)$dfbetas, unname(dfbetas(f0)), tolerance = 1e-10)
  d <- Dffit(dg_y, dg_X)
  expect_equal(d$dffits, unname(dffits(dg_fit)), tolerance = 1e-10)
  expect_equal(d$n_influential, sum(abs(dffits(dg_fit)) > 2 * sqrt(3 / 12)))
  o <- Dffitsols(dg_X, dg_y)
  expect_equal(o$dffits, unname(dffits(dg_fit)), tolerance = 1e-10)
  expect_equal(o$student, unname(rstudent(dg_fit)), tolerance = 1e-10)
  expect_equal(Dffitsols(dg_X, dg_y, intercept = FALSE)$dffits, unname(dffits(f0)), tolerance = 1e-10)
  cr <- Covrat(dg_y, dg_X)
  expect_equal(cr$covratio, unname(covratio(dg_fit)), tolerance = 1e-10)
  expect_equal(cr$estimate, max(abs(covratio(dg_fit) - 1)), tolerance = 1e-10)
  expect_error(Dffit(numeric(0), dg_X[0, ]), "empty")
  expect_error(Dffit(dg_y[-1], dg_X), "same number of rows")
  expect_error(Covrat(dg_y[1:3], dg_X[1:3, ]), "n > p \\+ 1")
})

test_that("Cstat computes Harrell's and Uno's concordance", {
  tt <- c(5.1, 3.2, 8.4, 1.7, 6.6, 2.9, 9.8, 4.4, 7.3, 0.9)
  ev <- c(1, 0, 1, 1, 0, 1, 1, 0, 1, 1)
  rs <- c(0.2, 0.8, 0.1, 0.9, 0.3, 0.6, 0.05, 0.4, 0.2, 0.95)
  r <- Cstat(tt, ev, rs)
  cc <- 0
  tie <- 0
  cmp <- 0
  for (i in which(ev == 1)) for (j in which(tt > tt[i])) {
    cmp <- cmp + 1
    cc <- cc + (rs[i] > rs[j])
    tie <- tie + (rs[i] == rs[j])
  }
  cval <- (cc + 0.5 * tie) / cmp
  expect_equal(r$c_statistic, cval, tolerance = 1e-12)
  expect_equal(r$se, sqrt(2 * cval * (1 - cval) / cmp), tolerance = 1e-12)
  expect_equal(r$comparable, as.integer(cmp))
  skip_if_not_installed("survival")
  ref <- survival::concordance(survival::Surv(tt, ev) ~ rs, reverse = TRUE)
  expect_equal(r$c_statistic, unname(ref$concordance), tolerance = 1e-12)
  u <- Cstat(tt, ev, rs, method = "uno")
  ce <- as.numeric(ev == 0)
  km <- function(t) {
    s <- 1
    for (tj in sort(unique(tt[ce == 1]))) if (tj < t) s <- s * (1 - 1 / sum(tt >= tj))
    s
  }
  tau <- unname(quantile(tt[ev == 1], 0.75))
  num <- den <- 0
  for (i in which(ev == 1 & tt <= tau)) {
    w <- 1 / max(km(tt[i])^2, 1e-10)
    for (j in which(tt > tt[i])) {
      den <- den + w
      num <- num + w * ((rs[i] > rs[j]) + 0.5 * (rs[i] == rs[j]))
    }
  }
  expect_equal(u$c_statistic, num / den, tolerance = 1e-12)
  expect_equal(Cstat(c(1, 2), c(0, 0), c(1, 2))$c_statistic, 0.5)
  expect_error(Cstat(tt, ev[-1], rs), "same length")
  expect_error(Cstat(tt, ev, rs, method = "x"), "harrell")
})

test_that("Bic and Dic information criteria", {
  b <- Bic(-120.5, 4, 80)
  expect_equal(b$estimate, 241 + 4 * log(80), tolerance = 1e-12)
  expect_equal(b$aic, 249, tolerance = 1e-12)
  expect_true(is.nan(Bic(-1, 2, 0)$penalty))
  dv <- c(210.3, 208.9, 212.4, 209.7, 211.1)
  d <- Dic(dv)
  expect_equal(d$p_d, var(dv) / 2, tolerance = 1e-12)
  expect_equal(d$estimate, mean(dv) + var(dv) / 2, tolerance = 1e-12)
  d2 <- Dic(dv, d_at_mean = 207)
  expect_equal(d2$estimate, 2 * mean(dv) - 207, tolerance = 1e-12)
  expect_identical(d2$variant, "p_D")
})

test_that("Aicar runs Levinson-Durbin and matches stats::pacf", {
  x <- c(1.2, 0.8, 1.9, 2.4, 2.1, 3.0, 2.7, 1.8, 1.1, 0.6, 1.4, 2.2, 2.9, 3.3, 2.5,
         1.6, 0.9, 1.3, 2.0, 2.6)
  r <- Aicar(x, max_p = 4)
  expect_equal(r$pacf, as.numeric(pacf(x, lag.max = 4, plot = FALSE)$acf), tolerance = 1e-12)
  g0 <- sum((x - mean(x))^2) / 20
  sig <- g0 * cumprod(c(1, 1 - r$pacf^2))
  expect_equal(r$sigma2, sig, tolerance = 1e-12)
  expect_equal(r$aic, log(sig) + 2 * (0:4 + 1) / 20, tolerance = 1e-12)
  expect_equal(r$p, which.min(r$aic) - 1L)
  yw <- ar.yw(x, aic = FALSE, order.max = 4, demean = TRUE)
  expect_equal(r$sigma2[5], yw$var.pred * (20 - 5) / 20, tolerance = 1e-10)
  expect_error(Aicar(1:3, max_p = 4), "too short")
  expect_error(Aicar(rep(1, 10), max_p = 2), "zero variance")
})

test_that("Bicarp compares AR fits on a common sample", {
  x <- c(1.2, 0.8, 1.9, 2.4, 2.1, 3.0, 2.7, 1.8, 1.1, 0.6, 1.4, 2.2, 2.9, 3.3, 2.5, 1.6)
  r <- Bicarp(x, max_p = 3)
  y <- x[4:16]
  s2 <- vapply(0:3, function(p) {
    f <- if (p == 0) lm(y ~ 1) else lm(y ~ sapply(1:p, function(l) x[(4 - l):(16 - l)]))
    mean(resid(f)^2)
  }, 0)
  expect_equal(r$sigma2, s2, tolerance = 1e-10)
  expect_equal(r$bic, log(s2) + (0:3) * log(13) / 13, tolerance = 1e-10)
  expect_equal(r$order, which.min(r$bic) - 1L)
  expect_error(Bicarp(x, max_p = 1.5), "non-negative integer")
  expect_error(Bicarp(x[1:6], max_p = 3), "too few observations")
  expect_error(Bicarp(rep(2, 10), max_p = 2), "exact")
})

test_that("Ebfmi is the energy Bayesian fraction of missing information", {
  e1 <- c(10.2, 11.5, 9.8, 12.1, 10.9, 11.3)
  e2 <- c(8.1, 8.4, 8.2, 8.9, 8.5, 8.3)
  r <- Ebfmi(rbind(e1, e2))
  f <- function(e) sum(diff(e)^2) / sum((e - mean(e))^2)
  expect_equal(r$ebfmi, c(f(e1), f(e2)), tolerance = 1e-12)
  expect_equal(r$min_ebfmi, min(f(e1), f(e2)), tolerance = 1e-12)
  expect_equal(Ebfmi(list(e1, e2))$ebfmi, r$ebfmi)
  expect_equal(Ebfmi(e1)$n_chains, 1L)
  expect_true(is.na(Ebfmi(c(1, 1, 1))$ebfmi))
  expect_error(Ebfmi(1), "at least two")
})
