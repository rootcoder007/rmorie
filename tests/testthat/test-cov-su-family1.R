# Coverage for sunabr .. Survrls exports. Every expectation is recomputed in
# the test body.

test_that("Iwdid averages cohort-specific DID contrasts by cohort size", {
  unit <- rep(1:6, each = 4)
  time <- rep(1:4, 6)
  cohort <- rep(c(3, 3, 4, 0, 0, 0), each = 4)
  y <- c(1.0, 1.2, 2.5, 2.9, 0.8, 1.1, 2.2, 2.6, 1.5, 1.4, 1.8, 3.4,
         1.1, 1.3, 1.4, 1.6, 0.9, 1.0, 1.3, 1.5, 1.2, 1.2, 1.5, 1.7)
  r <- Iwdid(y, unit, time, cohort)
  cm <- function(g, t) {
    v <- y[cohort == g & time == t]
    if (length(v)) mean(v) else NA
  }
  size <- c(`3` = 2, `4` = 1)
  att <- vapply(sort(unique(c(outer(1:4, c(3, 4), "-")))), function(e) {
    num <- den <- 0
    for (g in c(3, 4)) {
      v <- c(cm(g, g + e), cm(g, g - 1), cm(0, g + e), cm(0, g - 1))
      if (!anyNA(v)) {
        num <- num + size[[as.character(g)]] * ((v[1] - v[2]) - (v[3] - v[4]))
        den <- den + size[[as.character(g)]]
      }
    }
    if (den > 0) num / den else NA
  }, 0)
  ev <- sort(unique(c(outer(1:4, c(3, 4), "-"))))
  expect_equal(r$event_time, ev[!is.na(att)])
  expect_equal(r$att, att[!is.na(att)], tolerance = 1e-12)
  expect_equal(r$overall, mean(att[!is.na(att) & ev >= 0]), tolerance = 1e-12)
  expect_equal(r$cohorts, c(3, 4))
})

sv_surv <- function() {
  # every censoring falls after every event, so the censoring distribution
  # is 1 wherever the IPCW estimators evaluate it
  list(t = c(1, 2, 3, 4, 5, 6, 9, 10), e = c(1, 1, 1, 1, 1, 1, 0, 0),
       s = c(0.9, 0.3, 0.6, 0.7, 0.2, 0.8, 0.5, 0.4))
}

test_that("Survbri and Survipa give the Graf Brier score and the IPA", {
  d <- sv_surv()
  r <- Survbri(d$t, d$e, d$s, 4.5)
  bs <- mean(ifelse(d$t <= 4.5, d$s^2, (1 - d$s)^2))
  expect_equal(r$estimate, bs, tolerance = 1e-12)
  km <- 1 - 4 / 8
  bn <- mean(ifelse(d$t <= 4.5, km^2, (1 - km)^2))
  a <- Survipa(d$t, d$e, d$s, 4.5)
  expect_equal(a$brier_score, bs, tolerance = 1e-12)
  expect_equal(a$scaled_brier, 1 - bs / bn, tolerance = 1e-12)
})

test_that("Survci2 is Uno's truncated concordance", {
  d <- sv_surv()
  r <- Survci2(d$t, d$e, d$s)
  tau <- stats::quantile(d$t[d$e == 1], 0.75, type = 7, names = FALSE)
  num <- den <- 0
  for (i in which(d$e == 1 & d$t <= tau)) for (j in seq_along(d$t)) {
    if (d$t[i] < d$t[j]) {
      den <- den + 1
      num <- num + (d$s[i] > d$s[j]) + 0.5 * (d$s[i] == d$s[j])
    }
  }
  expect_equal(r$estimate, num / den, tolerance = 1e-12)
})

test_that("survcind is Harrell's C", {
  skip_if_not_installed("survival")
  t <- c(2, 5, 3, 8, 1, 7, 4, 6)
  e <- c(1, 0, 1, 1, 1, 0, 1, 1)
  risk <- c(0.8, 0.3, 0.6, 0.2, 0.9, 0.3, 0.5, 0.4)
  r <- survcind(t, e, risk)
  cc <- survival::concordance(survival::Surv(t, e) ~ risk, reverse = TRUE)
  expect_equal(r$estimate, unname(cc$concordance), tolerance = 1e-12)
  expect_equal(r$concordant, unname(cc$count["concordant"]))
  expect_equal(r$tied, unname(cc$count["tied.x"]))
  expect_true(is.na(survcind(1, 1, 1)$estimate))
})

test_that("Disctime and Survlnk are person-period binary regressions", {
  t <- c(1, 2, 2, 3, 1, 3, 2, 3, 1, 2)
  e <- c(1, 1, 0, 1, 0, 1, 1, 0, 1, 1)
  x <- c(0.5, -0.2, 1.1, 0.3, -0.8, 0.9, 0.0, -0.5, 1.4, 0.2)
  pp <- do.call(rbind, lapply(seq_along(t), function(i) {
    data.frame(k = seq_len(t[i]), x = x[i], y = as.numeric(seq_len(t[i]) == t[i] & e[i] == 1))
  }))
  M <- cbind(stats::model.matrix(~ 0 + factor(k), pp), pp$x)
  score <- function(b, fam) {
    eta <- as.numeric(M %*% b)
    mu <- fam$linkinv(eta)
    as.numeric(crossprod(M, (pp$y - mu) / (mu * (1 - mu)) * fam$mu.eta(eta)))
  }
  cl <- stats::binomial("cloglog")
  f <- stats::glm(y ~ 0 + factor(k) + x, family = cl, data = pp, control = list(epsilon = 1e-14, maxit = 100))
  r <- Disctime(t, e, X = x)
  b <- c(r$alpha, r$estimate)
  # the likelihood score vanishes at the fit; glm stops on a deviance
  # criterion, which only pins the coefficients to about sqrt(1e-14)
  expect_equal(score(b, cl), rep(0, 4), tolerance = 1e-9)
  expect_equal(b, unname(stats::coef(f)), tolerance = 1e-6)
  expect_equal(r$hazard, 1 - exp(-exp(r$alpha)), tolerance = 1e-12)
  expect_equal(r$loglik, sum(stats::dbinom(pp$y, 1, cl$linkinv(as.numeric(M %*% b)), log = TRUE)), tolerance = 1e-10)
  expect_equal(r$n_person_periods, nrow(pp))
  n0 <- Disctime(t, e)
  expect_true(is.nan(n0$estimate))
  for (lk in c("cloglog", "logit")) {
    s <- Survlnk(t, e, x, link = lk)
    fam <- stats::binomial(lk)
    bs <- c(s$alpha, s$estimate)
    expect_equal(score(bs, fam), rep(0, 4), tolerance = 1e-9)
    eta <- as.numeric(M %*% bs)
    mu <- fam$linkinv(eta)
    info <- crossprod(M * (fam$mu.eta(eta)^2 / (mu * (1 - mu))), M)
    expect_equal(s$se, unname(sqrt(diag(solve(info)))[4]), tolerance = 1e-10)
  }
  expect_error(Survlnk(t, e, x, link = "probit"), "cloglog")
  expect_error(Survlnk(t, 0 * e, x), "no events")
})

test_that("Bartsurv boosts probit stumps on the person-period data", {
  t <- c(1, 2, 2, 3, 1, 3)
  e <- c(1, 1, 0, 1, 0, 1)
  x <- c(0.5, -0.2, 1.1, 0.3, -0.8, 0.9)
  g <- c(1, 2, 3)
  rows <- do.call(rbind, lapply(1:6, function(i) cbind(g[g <= t[i]], x[i], i, as.numeric(g[g <= t[i]] == t[i] & e[i] == 1))))
  pbar <- mean(rows[, 4])
  r0 <- Bartsurv(t, e, X = x, n_trees = 0)
  expect_equal(r0$hazard, rep(pbar, nrow(rows)), tolerance = 1e-12)
  expect_equal(r0$surv, as.numeric((1 - pbar)^table(rows[, 3])), tolerance = 1e-12)
  r1 <- Bartsurv(t, e, X = x, n_trees = 1, shrink = 0.5)
  f <- stats::qnorm(pbar)
  dens <- stats::dnorm(f)
  res <- (rows[, 4] - pbar) * dens / (pbar * (1 - pbar)) / (dens^2 / (pbar * (1 - pbar)))
  best <- c(-Inf, NA, NA, NA, NA)
  for (a in 1:2) {
    v <- sort(unique(rows[, a]))
    for (k in seq_len(length(v) - 1)) {
      thr <- (v[k] + v[k + 1]) / 2
      l <- rows[, a] <= thr
      gain <- sum(l) * mean(res[l])^2 + sum(!l) * mean(res[!l])^2
      if (gain > best[1]) best <- c(gain, a, thr, mean(res[l]), mean(res[!l]))
    }
  }
  expect_equal(r1$trees[[1]], c(best[2] - 1, best[3:5]), tolerance = 1e-12)
  f1 <- f + 0.5 * ifelse(rows[, best[2]] <= best[3], best[4], best[5])
  expect_equal(r1$hazard, stats::pnorm(f1), tolerance = 1e-12)
  expect_equal(r1$surv, as.numeric(tapply(1 - stats::pnorm(f1), rows[, 3], prod)), tolerance = 1e-12)
})

test_that("Survip, Sscompv and Survrls", {
  p <- Survip(6.2, DEFF = 1.8, df = 2)
  expect_equal(p$estimate, stats::pchisq(6.2 / 1.8, 2, lower.tail = FALSE), tolerance = 1e-12)
  expect_equal(p$inflation, p$estimate / stats::pchisq(6.2, 2, lower.tail = FALSE), tolerance = 1e-12)
  expect_error(Survip(-1), "non-negative")
  expect_error(Survip(1, DEFF = 0), "DEFF must be positive")
  expect_error(Survip(1, df = 0), "df must be at least 1")
  t <- c(2, 5, 3, 8, 1, 7)
  d <- c(1, 2, 1, 0, 1, 2)
  Fp <- c(0.7, 0.4, 0.5, 0.2, 0.9, 0.5)
  s <- Sscompv(t, d, Fp)
  conc <- tied <- comp <- 0
  for (i in which(d == 1)) for (j in setdiff(1:6, i)) {
    if (t[i] < t[j] || d[j] >= 2) {
      comp <- comp + 1
      conc <- conc + (Fp[i] > Fp[j])
      tied <- tied + (Fp[i] == Fp[j])
    }
  }
  expect_equal(s$estimate, (conc + tied / 2) / comp, tolerance = 1e-12)
  expect_equal(s$comparable, as.integer(comp))
  expect_error(Sscompv(t, rep(0, 6), Fp), "no comparable pairs")
  expect_error(Sscompv(t[-1], d, Fp), "equal length")
  skip_if_not_installed("survival")
  tm <- c(2, 5, 3, 8, 1, 7, 4, 6)
  ev <- c(1, 0, 1, 1, 1, 0, 1, 1)
  r <- Survrls(tm, ev, 6.5)
  km <- survival::survfit(survival::Surv(tm, ev) ~ 1)
  kt <- c(0, km$time[km$time < 6.5], 6.5)
  ks <- c(1, km$surv[km$time < 6.5])
  expect_equal(r$estimate, sum(diff(kt) * ks), tolerance = 1e-12)
  expect_equal(r$n_events, 6L)
  expect_error(Survrls(tm, ev, 0), "t_star must be positive")
})
