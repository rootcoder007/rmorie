# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/survvae_native.R (Deep Survival Machines, Nagpal, Li &
# Dubrawski 2021). Densities and survivors are recomputed with base R's
# dweibull / dlnorm / pweibull / plnorm; the ELBO and the exact mixture
# likelihood are recomputed from Sec. III-C.

.sv_X <- list(c(0.1, 1), c(-0.4, 0.5), c(0.8, -0.2), c(0.3, 0.3),
              c(-1, 0.2), c(0.6, 0.9), c(-0.2, -0.7), c(1.1, 0.4))
.sv_t <- c(1.3, 0.7, 2.4, 1.9, 0.5, 3.1, 1.1, 2.2)
.sv_e <- c(1, 1, 0, 1, 1, 0, 1, 1)
.sv_W <- list(c(0.5, -0.3), c(-0.2, 0.4))
.sv_b <- c(0.1, -0.1)
.sv_sh <- c(1.5, 0.8)
.sv_sc <- c(1.6, 2.5)

.sv_gate <- function(x) {
  z <- c(sum(.sv_W[[1]] * x), sum(.sv_W[[2]] * x)) + .sv_b
  exp(z) / sum(exp(z))
}

test_that("morie_survvae_PRIMITIVES lists the two closed-form experts", {
  expect_identical(morie_survvae_PRIMITIVES, c("weibull", "lognormal"))
})

test_that("log_pdf and log_survival match dweibull / dlnorm / pweibull / plnorm", {
  for (t in c(0.3, 1, 2.7)) {
    expect_equal(morie_survvae_log_pdf(t, 1.7, 2.2),
                 dweibull(t, 1.7, 2.2, log = TRUE), tolerance = 1e-12)
    expect_equal(morie_survvae_log_pdf(t, 0.6, 2.2, "lognormal"),
                 dlnorm(t, log(2.2), 0.6, log = TRUE), tolerance = 1e-12)
    expect_equal(morie_survvae_log_survival(t, 1.7, 2.2),
                 pweibull(t, 1.7, 2.2, lower.tail = FALSE, log.p = TRUE), tolerance = 1e-12)
    expect_equal(morie_survvae_log_survival(t, 0.6, 2.2, "lognormal"),
                 plnorm(t, log(2.2), 0.6, lower.tail = FALSE, log.p = TRUE), tolerance = 1e-12)
  }
  expect_identical(morie_survvae_log_survival(0, 1, 1), 0)
  # the log-normal survivor is floored at 1e-300 far in the tail
  expect_equal(morie_survvae_log_survival(1e200, 0.1, 1, "lognormal"), log(1e-300))
  expect_error(morie_survvae_log_pdf(0, 1, 1), "positive")
  expect_error(morie_survvae_log_pdf(1, -1, 1), "shape and scale")
  expect_error(morie_survvae_log_pdf(1, 1, 1, "gamma"), "primitive must be one of")
  expect_error(morie_survvae_log_survival(-1, 1, 1), "non-negative")
})

test_that("morie_survvae_gates is a softmax of W x + b", {
  x <- c(0.4, -1.2)
  g <- morie_survvae_gates(x, .sv_W, .sv_b)
  expect_equal(g, .sv_gate(x), tolerance = 1e-12)
  expect_equal(sum(g), 1, tolerance = 1e-12)
  # shift invariance of the stable softmax
  expect_equal(morie_survvae_gates(x, .sv_W, .sv_b + 700), g, tolerance = 1e-12)
})

test_that("morie_survvae_elbo and exact_loglik recompute Sec. III-C and obey Jensen", {
  for (prim in c("weibull", "lognormal")) {
    lf <- if (prim == "weibull") {
      function(t, k) dweibull(t, .sv_sh[k], .sv_sc[k], log = TRUE)
    } else {
      function(t, k) dlnorm(t, log(.sv_sc[k]), .sv_sh[k], log = TRUE)
    }
    ls <- if (prim == "weibull") {
      function(t, k) pweibull(t, .sv_sh[k], .sv_sc[k], lower.tail = FALSE, log.p = TRUE)
    } else {
      function(t, k) plnorm(t, log(.sv_sc[k]), .sv_sh[k], lower.tail = FALSE, log.p = TRUE)
    }
    u <- 0
    cc <- 0
    eu <- 0
    ec <- 0
    for (i in seq_along(.sv_X)) {
      g <- .sv_gate(.sv_X[[i]])
      if (.sv_e[i] == 1) {
        u <- u + sum(g * c(lf(.sv_t[i], 1), lf(.sv_t[i], 2)))
        eu <- eu + log(sum(g * exp(c(lf(.sv_t[i], 1), lf(.sv_t[i], 2)))))
      } else {
        cc <- cc + sum(g * c(ls(.sv_t[i], 1), ls(.sv_t[i], 2)))
        ec <- ec + log(sum(g * exp(c(ls(.sv_t[i], 1), ls(.sv_t[i], 2)))))
      }
    }
    pen <- 0.3 * (sum(log(.sv_sh)^2) + sum(log(.sv_sc)^2))
    r <- morie_survvae_elbo(.sv_X, .sv_t, .sv_e, .sv_W, .sv_b, .sv_sh, .sv_sc,
                            prim, alpha = 0.4, prior = 0.3)
    expect_equal(r$uncensored, u, tolerance = 1e-12)
    expect_equal(r$censored, cc, tolerance = 1e-12)
    expect_equal(r$prior_penalty, pen, tolerance = 1e-12)
    expect_equal(r$elbo, u + 0.4 * cc - pen, tolerance = 1e-12)
    ex <- morie_survvae_exact_loglik(.sv_X, .sv_t, .sv_e, .sv_W, .sv_b, .sv_sh,
                                     .sv_sc, prim, alpha = 0.4)
    expect_equal(ex$uncensored, eu, tolerance = 1e-12)
    expect_equal(ex$censored, ec, tolerance = 1e-12)
    expect_equal(ex$loglik, eu + 0.4 * ec, tolerance = 1e-12)
    expect_gte(ex$uncensored, r$uncensored)
    expect_gte(ex$censored, r$censored)
  }
  expect_error(morie_survvae_elbo(.sv_X, .sv_t, .sv_e, .sv_W, .sv_b, .sv_sh, .sv_sc,
                                  alpha = 1.5), "alpha must lie")
  expect_error(morie_survvae_exact_loglik(.sv_X, .sv_t, .sv_e, .sv_W, .sv_b, .sv_sh,
                                          .sv_sc, "exp"), "primitive")
})

test_that("morie_survvae fits, reports a consistent ELBO, and alpha = 0 ignores censored times", {
  X1 <- lapply(.sv_X, function(x) x[1])
  f <- morie_survvae(X1, .sv_t, .sv_e, K = 1L, restarts = 1L, seed = 3)
  e <- morie_survvae_elbo(X1, .sv_t, .sv_e, f$W, f$bias, f$shapes, f$scales)
  expect_equal(f$elbo, e$elbo, tolerance = 1e-12)
  expect_equal(f$estimate, f$elbo)
  # with one expert the gate is 1, so the ELBO is the exact likelihood
  expect_equal(f$jensen_gap, 0, tolerance = 1e-10)
  # the fit beats the starting Weibull at the mean observed time
  start <- morie_survvae_elbo(X1, .sv_t, .sv_e, list(0), 0, 1, mean(.sv_t))
  expect_gt(f$elbo, start$elbo)
  f2 <- morie_survvae(X1, .sv_t, .sv_e, K = 2L, restarts = 1L, seed = 3,
                      primitive = "lognormal")
  expect_gte(f2$jensen_gap, -1e-12)
  expect_equal(f2$loglik,
               morie_survvae_exact_loglik(X1, .sv_t, .sv_e, f2$W, f2$bias, f2$shapes,
                                          f2$scales, "lognormal")$loglik, tolerance = 1e-12)
  # alpha = 0: move the two censored times while keeping their sum (t0)
  ta <- .sv_t
  ta[3] <- 2.0
  ta[6] <- 3.5
  fa <- morie_survvae(X1, .sv_t, .sv_e, K = 1L, alpha = 0, restarts = 1L)
  fb <- morie_survvae(X1, ta, .sv_e, K = 1L, alpha = 0, restarts = 1L)
  expect_equal(fa$elbo, fb$elbo, tolerance = 1e-12)
  expect_equal(fa$shapes, fb$shapes, tolerance = 1e-12)
  expect_error(morie_survvae(X1, .sv_t[-1], .sv_e), "same length")
  expect_error(morie_survvae(list(), numeric(0), numeric(0)), "no observations")
  expect_error(morie_survvae(X1, .sv_t, .sv_e, K = 0L), "at least 1")
})

test_that("predict_survival, risk_score and concordance follow the mixture survivor", {
  fit <- list(W = .sv_W, bias = .sv_b, shapes = .sv_sh, scales = .sv_sc, K = 2L,
              primitive = "weibull", times = .sv_t)
  x <- c(0.2, -0.5)
  tt <- c(0, 0.5, 2)
  p <- morie_survvae_predict_survival(fit, x, tt)
  g <- .sv_gate(x)
  s <- vapply(tt, function(t) sum(g * pweibull(t, .sv_sh, .sv_sc, lower.tail = FALSE)), 0)
  expect_equal(p$survival, s, tolerance = 1e-12)
  expect_equal(p$gates, g, tolerance = 1e-12)
  expect_equal(p$time, tt)
  h <- sort(.sv_t)[5]
  rs <- morie_survvae_risk_score(fit, .sv_X)
  expect_equal(rs, vapply(.sv_X, function(x) 1 - sum(.sv_gate(x) * pweibull(h, .sv_sh, .sv_sc, lower.tail = FALSE)), 0),
               tolerance = 1e-12)
  rs2 <- morie_survvae_risk_score(fit, .sv_X, horizon = 1)
  expect_equal(rs2[1], 1 - sum(.sv_gate(.sv_X[[1]]) * pweibull(1, .sv_sh, .sv_sc, lower.tail = FALSE)), tolerance = 1e-12)
  # Harrell's C over comparable pairs (earlier time is an event)
  num <- 0
  den <- 0
  for (i in 1:8) for (j in 1:8) {
    if (.sv_t[i] < .sv_t[j] && .sv_e[i] == 1) {
      den <- den + 1
      num <- num + (rs[i] > rs[j]) + 0.5 * (rs[i] == rs[j])
    }
  }
  expect_equal(morie_survvae_concordance(fit, .sv_X, .sv_t, .sv_e), num / den, tolerance = 1e-12)
})

test_that("morie_survvae_fit_competing fits one model per cause", {
  X1 <- lapply(.sv_X, function(x) x[1])
  causes <- c(1, 2, 0, 1, 2, 0, 1, 2)
  r <- morie_survvae_fit_competing(X1, .sv_t, causes, K = 1L, seed = 1)
  expect_equal(r$estimate, 2)
  expect_identical(r$risks, 1:2)
  f1 <- morie_survvae(X1, .sv_t, as.integer(causes == 1), K = 1L, seed = 1)
  expect_equal(r$fits[["1"]]$elbo, f1$elbo, tolerance = 1e-12)
  expect_equal(r$fits[["2"]]$events, as.numeric(causes == 2))
  expect_error(morie_survvae_fit_competing(X1, .sv_t, rep(0, 8)), "no competing")
})

test_that("morie_survvae_cheatsheet describes the loss", {
  s <- morie_survvae_cheatsheet()
  expect_length(s, 1L)
  expect_match(s, "ELBO_U + alpha ELBO_C", fixed = TRUE)
})
