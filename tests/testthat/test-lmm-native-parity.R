# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Parity of morie_lmm / morie_glmm with lme4::lmer, lme4::glmer and
# nlme::lme (references used only here).
#
# How the tolerances are set.  Both sides minimise the same objective;
# we check that directly: morie's deviance function and lme4's
# (devFunOnly = TRUE) agree at the same parameters to ~1e-10, and
# lme4's deviance at morie's optimum is never worse than at lme4's own.
# The estimates then differ only by where each optimiser stops.  lme4's
# defaults stop around 1e-5 relative (bobyqa / Nelder-Mead), so against
# default lme4 we allow 2e-4 relative; against lme4 run with tight
# optimiser settings (bobyqa rhoend = 1e-12) we allow 3e-5.
#
# glmer: lme4's PIRLS stops on a relative change of the penalised
# deviance below tolPwrss = 1e-7 and keeps the Cholesky factor of the
# previous iterate, which perturbs its Laplace deviance by up to ~1e-3
# (e.g. cbpp: 184.05313 vs 184.05257 at lme4's own estimates).  With
# glmerControl(tolPwrss = 1e-13) lme4's deviance equals morie's exact
# Laplace deviance, so parity is checked against that setting.

rel_diff <- function(a, b) max(abs(a - b) / pmax(abs(b), 1e-8))

# variance components keyed by (factor, var1, var2); a:b and b:a are the
# same factor (lme4 names nested factors in reverse order)
.lmmt_key <- function(grp, v1, v2) {
  g <- vapply(strsplit(sub("\\.[0-9]+$", "", grp), ":", fixed = TRUE),
              function(z) paste(sort(z), collapse = ":"), "")
  paste(g, v1, v2)
}
.lmmt_vc <- function(fit, ref) {
  vt <- as.data.frame(lme4::VarCorr(ref))
  vt <- vt[vt$grp != "Residual", ]
  ot <- fit$varcorr_table
  m <- match(.lmmt_key(vt$grp, vt$var1, vt$var2),
             .lmmt_key(ot$grp, ot$var1, ot$var2))
  list(ours = ot$sdcor[m], ref = vt$sdcor, is_sd = is.na(vt$var2))
}
# morie's theta in lme4's order
.lmmt_theta <- function(fit, ref) {
  nm <- function(x) vapply(strsplit(x, ".", fixed = TRUE), function(z)
    paste(c(paste(sort(strsplit(z[1], ":", fixed = TRUE)[[1]]), collapse = ":"),
            z[-1]), collapse = "."), "")
  m <- match(nm(names(lme4::getME(ref, "theta"))), nm(names(fit$theta)))
  stopifnot(!anyNA(m))
  list(ours_in_ref = unname(fit$theta[m]), perm = order(m))
}

check_lmer <- function(formula, data, REML, tol, tight = FALSE) {
  fit <- rmorie::morie_lmm(formula, data, REML = REML)
  ctl <- if (tight) {
    lme4::lmerControl(check.conv.singular = "ignore", optimizer = "bobyqa",
                      optCtrl = list(rhoend = 1e-12, maxfun = 1e5))
  } else {
    lme4::lmerControl(check.conv.singular = "ignore")
  }
  ref <- lme4::lmer(formula, data, REML = REML, control = ctl)
  dfun <- lme4::lmer(formula, data, REML = REML, devFunOnly = TRUE)
  th <- .lmmt_theta(fit, ref)
  th_ref <- unname(lme4::getME(ref, "theta"))
  # same objective at the same parameters
  expect_lt(abs(fit$devfun(th_ref[th$perm]) - dfun(th_ref)), 1e-8)
  # morie's optimum is at least as good on lme4's own objective
  expect_lt(dfun(th$ours_in_ref) - dfun(th_ref), 1e-7)
  fe <- lme4::fixef(ref)
  expect_lt(rel_diff(fit$coefficients[names(fe)], fe), tol)
  expect_lt(rel_diff(sqrt(diag(fit$vcov))[names(fe)],
                     sqrt(diag(as.matrix(stats::vcov(ref))))), tol)
  vc <- .lmmt_vc(fit, ref)
  expect_lt(rel_diff(vc$ours[vc$is_sd], vc$ref[vc$is_sd]), tol)
  if (any(!vc$is_sd))
    expect_lt(max(abs(vc$ours[!vc$is_sd] - vc$ref[!vc$is_sd])), tol)
  expect_lt(rel_diff(fit$sigma, stats::sigma(ref)), tol)
  expect_lt(abs(fit$logLik - as.numeric(stats::logLik(ref))), 1e-6)
  expect_equal(stats::AIC(stats::logLik(fit)), stats::AIC(ref), tolerance = 1e-8)
  invisible(list(fit = fit, ref = ref))
}

test_that("lmm matches lmer: sleepstudy random intercept and slope", {
  testthat::skip_if_not_installed("lme4")
  data("sleepstudy", package = "lme4", envir = environment())
  for (REML in c(TRUE, FALSE)) {
    r <- check_lmer(Reaction ~ Days + (Days | Subject), sleepstudy, REML, 2e-4)
    check_lmer(Reaction ~ Days + (Days | Subject), sleepstudy, REML, 3e-5,
               tight = TRUE)
  }
  re <- lme4::ranef(r$ref)$Subject
  expect_lt(max(abs(as.matrix(r$fit$ranef$Subject) - as.matrix(re))), 1e-3)
  expect_lt(max(abs(fitted(r$fit) - fitted(r$ref))), 1e-3)
  expect_equal(unname(r$fit$ngroups), 18)
  check_lmer(Reaction ~ Days + (Days || Subject), sleepstudy, TRUE, 3e-5,
             tight = TRUE)
})

test_that("lmm matches lmer: crossed, nested and factor covariates", {
  testthat::skip_if_not_installed("lme4")
  testthat::skip_if_not_installed("nlme")
  data("Penicillin", package = "lme4", envir = environment())
  data("Pastes", package = "lme4", envir = environment())
  data("Machines", package = "nlme", envir = environment())
  Machines <- as.data.frame(Machines)
  Machines$Worker <- factor(as.character(Machines$Worker))
  Machines$Machine <- factor(as.character(Machines$Machine))
  for (REML in c(TRUE, FALSE)) {
    check_lmer(diameter ~ 1 + (1 | plate) + (1 | sample), Penicillin, REML,
               3e-5, tight = TRUE)
    check_lmer(strength ~ 1 + (1 | batch/cask), Pastes, REML, 3e-5,
               tight = TRUE)
    check_lmer(score ~ Machine + (1 | Worker/Machine), Machines, REML, 3e-5,
               tight = TRUE)
    check_lmer(score ~ Machine + (0 + Machine | Worker), Machines, REML, 3e-5,
               tight = TRUE)
  }
  f <- rmorie::morie_lmm(strength ~ 1 + (1 | batch/cask), Pastes)
  expect_equal(unname(sort(f$ngroups)), c(10, 30))
})

test_that("lmm matches lmer with prior weights and missing values", {
  testthat::skip_if_not_installed("lme4")
  data("sleepstudy", package = "lme4", envir = environment())
  set.seed(3)
  sw <- sleepstudy
  sw$wt <- stats::runif(nrow(sw), 0.5, 2)
  fit <- rmorie::morie_lmm(Reaction ~ Days + (Days | Subject), sw,
                          weights = sw$wt)
  ref <- lme4::lmer(Reaction ~ Days + (Days | Subject), sw, weights = wt)
  expect_lt(abs(fit$logLik - as.numeric(stats::logLik(ref))), 1e-6)
  expect_lt(rel_diff(fit$coefficients, lme4::fixef(ref)), 2e-4)
  sw$Reaction[c(3, 50)] <- NA
  fit <- rmorie::morie_lmm(Reaction ~ Days + (Days | Subject), sw)
  ref <- lme4::lmer(Reaction ~ Days + (Days | Subject), sw)
  expect_equal(fit$nobs, 178)
  expect_lt(abs(fit$logLik - as.numeric(stats::logLik(ref))), 1e-6)
})

test_that("nlme interface matches nlme::lme, including containment df", {
  testthat::skip_if_not_installed("nlme")
  testthat::skip_if_not_installed("lme4")
  data("sleepstudy", package = "lme4", envir = environment())
  data("Pastes", package = "lme4", envir = environment())
  ctl <- nlme::lmeControl(tolerance = 1e-10, msTol = 1e-12, niterEM = 100,
                          msMaxIter = 500)
  cases <- list(list(Reaction ~ Days, sleepstudy, ~ Days | Subject),
                list(strength ~ 1, Pastes, ~ 1 | batch/cask))
  for (cs in cases) for (REML in c(TRUE, FALSE)) {
    fit <- rmorie::morie_lmm(cs[[1]], cs[[2]], REML = REML, random = cs[[3]])
    ref <- nlme::lme(cs[[1]], cs[[2]], random = cs[[3]],
                     method = if (REML) "REML" else "ML", control = ctl)
    tt <- summary(ref)$tTable
    expect_lt(rel_diff(fit$coefficients, tt[, "Value"]), 1e-6)
    expect_equal(unname(fit$fixef_table$df), unname(tt[, "DF"]))
    expect_lt(abs(fit$logLik - as.numeric(stats::logLik(ref))), 1e-6)
    # nlme scales ML standard errors by sqrt(n / (n - p)) (its varFix uses
    # the REML-type residual divisor); REML SEs coincide with lme4's
    p <- length(fit$coefficients)
    k <- if (REML) 1 else sqrt(fit$nobs / (fit$nobs - p))
    expect_lt(rel_diff(fit$fixef_table$Std.Error * k, tt[, "Std.Error"]), 1e-4)
  }
  # the lme4 and nlme spellings are the same model
  a <- rmorie::morie_lmm(Reaction ~ Days, sleepstudy, random = ~ Days | Subject)
  b <- rmorie::morie_lmm(Reaction ~ Days + (Days | Subject), sleepstudy)
  expect_equal(a$logLik, b$logLik, tolerance = 1e-12)
  c2 <- rmorie::morie_lmm(strength ~ 1, Pastes,
                         random = list(batch = ~ 1, cask = ~ 1))
  d2 <- rmorie::morie_lmm(strength ~ 1 + (1 | batch/cask), Pastes)
  expect_equal(c2$logLik, d2$logLik, tolerance = 1e-12)
})

.lmmt_sim <- function() {
  set.seed(11)
  m <- 60
  nn <- 20
  d <- data.frame(g = factor(rep(seq_len(m), each = nn)), x = stats::rnorm(m * nn))
  b0 <- stats::rnorm(m, 0, 0.8)
  b1 <- stats::rnorm(m, 0, 0.5)
  d$yb <- stats::rbinom(m * nn, 1, stats::plogis(-0.3 + 0.7 * d$x + b0[d$g] +
                                                   b1[d$g] * d$x))
  d$yp <- stats::rpois(m * nn, exp(0.4 + 0.3 * d$x + 0.5 * b0[d$g] +
                                     0.6 * b1[d$g] * d$x))
  d
}

check_glmer <- function(formula, data, family, tol = 3e-5) {
  fit <- rmorie::morie_glmm(formula, data, family = family)
  fam <- switch(family, binomial = stats::binomial(), poisson = stats::poisson())
  ctl <- lme4::glmerControl(tolPwrss = 1e-13, optimizer = c("bobyqa", "bobyqa"),
                            optCtrl = list(rhoend = 1e-12, maxfun = 1e5))
  ref <- lme4::glmer(formula, data, family = fam, control = ctl)
  dfun <- lme4::glmer(formula, data, family = fam, control = ctl,
                      devFunOnly = TRUE)
  th <- .lmmt_theta(fit, ref)
  p_ref <- c(lme4::getME(ref, "theta"), lme4::fixef(ref))
  k <- length(th$ours_in_ref)
  p_ours <- c(th$ours_in_ref, fit$coefficients)
  # same Laplace deviance at the same parameters
  expect_lt(abs(fit$devfun(c(p_ref[seq_len(k)][th$perm], p_ref[-seq_len(k)])) -
                  dfun(p_ref)), 1e-6)
  expect_lt(dfun(p_ours) - dfun(p_ref), 1e-7)
  fe <- lme4::fixef(ref)
  expect_lt(rel_diff(fit$coefficients[names(fe)], fe), tol)
  expect_lt(rel_diff(sqrt(diag(fit$vcov_RX)),
                     sqrt(diag(as.matrix(suppressWarnings(
                       stats::vcov(ref, use.hessian = FALSE)))))), tol)
  # Hessian-based SEs (lme4's default): same finite-difference stencil
  expect_lt(rel_diff(sqrt(diag(fit$vcov)), sqrt(diag(as.matrix(stats::vcov(ref))))),
            1e-4)
  vc <- .lmmt_vc(fit, ref)
  expect_lt(rel_diff(vc$ours[vc$is_sd], vc$ref[vc$is_sd]), tol)
  if (any(!vc$is_sd))
    expect_lt(max(abs(vc$ours[!vc$is_sd] - vc$ref[!vc$is_sd])), tol)
  expect_lt(abs(fit$logLik - as.numeric(stats::logLik(ref))), 1e-6)
  invisible(list(fit = fit, ref = ref))
}

test_that("glmm matches glmer: binomial and poisson, intercept and slope", {
  testthat::skip_if_not_installed("lme4")
  data("cbpp", package = "lme4", envir = environment())
  check_glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), cbpp,
              "binomial")
  d <- .lmmt_sim()
  check_glmer(yb ~ x + (1 | g), d, "binomial")
  r <- check_glmer(yb ~ x + (x | g), d, "binomial")
  expect_lt(max(abs(as.matrix(r$fit$ranef$g) -
                      as.matrix(lme4::ranef(r$ref)$g))), 1e-4)
  expect_lt(max(abs(fitted(r$fit) - fitted(r$ref))), 1e-5)
  check_glmer(yp ~ x + (1 | g), d, "poisson")
  check_glmer(yp ~ x + (x | g), d, "poisson")
})

test_that("glmm handles offsets and nAGQ = 0 as glmer", {
  testthat::skip_if_not_installed("lme4")
  set.seed(5)
  d <- data.frame(g = factor(rep(1:40, each = 10)), x = stats::rnorm(400),
                  ex = stats::runif(400, 1, 5))
  d$y <- stats::rpois(400, d$ex * exp(0.2 + 0.4 * d$x +
                                        stats::rnorm(40, 0, 0.6)[d$g]))
  ref <- lme4::glmer(y ~ x + (1 | g) + offset(log(ex)), d, family = stats::poisson,
                     control = lme4::glmerControl(tolPwrss = 1e-13))
  a <- rmorie::morie_glmm(y ~ x + (1 | g), d, family = "poisson",
                         offset = log(d$ex))
  b <- rmorie::morie_glmm(y ~ x + offset(log(ex)) + (1 | g), d, family = "poisson")
  expect_lt(abs(a$logLik - as.numeric(stats::logLik(ref))), 1e-6)
  expect_equal(a$logLik, b$logLik, tolerance = 1e-10)
  r0 <- lme4::glmer(y ~ x + (1 | g) + offset(log(ex)), d, family = stats::poisson,
                    nAGQ = 0)
  f0 <- rmorie::morie_glmm(y ~ x + (1 | g), d, family = "poisson",
                          offset = log(d$ex), nAGQ = 0)
  expect_lt(rel_diff(f0$coefficients, lme4::fixef(r0)), 1e-4)
})

test_that("sparse and dense Schur updates agree for crossed factors", {
  set.seed(44)
  n <- 3000
  d <- data.frame(s = factor(sample(300, n, TRUE)), it = factor(sample(60, n, TRUE)),
                  x = stats::rnorm(n))
  d$y <- 1 + d$x + stats::rnorm(300)[d$s] + stats::rnorm(60, 0, 0.5)[d$it] +
    stats::rnorm(n)
  st <- rmorie:::.lmm_setup(y ~ x + (x | s) + (1 | it), d)
  expect_false(is.null(st$sp))
  cp <- rmorie:::.lmm_cp(st, rep(1, n), cbind(st$X, st$y))
  Ts <- rmorie:::.lmm_tmats(c(1, 0.3, 0.7, 0.5), st)
  a <- rmorie:::.lmm_pls(cp, st, Ts, 2L)
  st$sp <- NULL
  b <- rmorie:::.lmm_pls(cp, st, Ts, 2L)
  expect_equal(a$ldL2, b$ldL2, tolerance = 1e-12)
  expect_equal(a$beta, b$beta, tolerance = 1e-12)
  expect_equal(a$u2, b$u2, tolerance = 1e-10)
})

test_that("methods and input checks", {
  set.seed(1)
  d <- data.frame(g = factor(rep(1:20, each = 8)), x = rep(0:7, 20))
  d$y <- 1 + 0.5 * d$x + stats::rnorm(20)[d$g] + stats::rnorm(160)
  fit <- rmorie::morie_lmm(y ~ x + (1 | g), d)
  expect_s3_class(fit, "morie_lmm")
  expect_named(coef(fit), c("(Intercept)", "x"))
  expect_equal(dim(vcov(fit)), c(2L, 2L))
  expect_equal(unname(fitted(fit) + residuals(fit)), d$y)
  expect_s3_class(logLik(fit), "logLik")
  expect_output(print(fit), "REML criterion")
  expect_output(print(summary(fit)), "Convergence")
  expect_equal(fit$convergence$code, 0)
  expect_error(rmorie::morie_lmm(y ~ x, d), "no random-effects terms")
  expect_error(rmorie::morie_glmm(y ~ x + (1 | g), d, family = "gaussian"))
})

test_that("speed: n = 20000 lmm and n = 10000 glmm with random slopes", {
  testthat::skip_on_cran()
  set.seed(42)
  m <- 500
  n <- 20000
  d <- data.frame(g = factor(sample(m, n, TRUE)), x = stats::rnorm(n),
                  z = stats::runif(n))
  d$y <- 2 + 0.5 * d$x - d$z + stats::rnorm(m)[d$g] +
    stats::rnorm(m, 0, 0.5)[d$g] * d$x + stats::rnorm(n)
  t1 <- system.time(f <- rmorie::morie_lmm(y ~ x + z + (x | g), d))[["elapsed"]]
  expect_lt(t1, 5)
  set.seed(43)
  m <- 300
  n <- 10000
  d <- data.frame(g = factor(sample(m, n, TRUE)), x = stats::rnorm(n),
                  z = stats::runif(n))
  d$y <- stats::rbinom(n, 1, stats::plogis(-0.5 + 0.8 * d$x + 0.5 * d$z +
                                             stats::rnorm(m)[d$g] +
                                             stats::rnorm(m, 0, 0.5)[d$g] * d$x))
  t2 <- system.time(f <- rmorie::morie_glmm(y ~ x + z + (x | g), d,
                                           family = "binomial"))[["elapsed"]]
  expect_lt(t2, 15)
})
