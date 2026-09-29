# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/survnnr_native.R (DeepSurv, Katzman et al. 2018). The
# partial log-likelihood is checked against survival::coxph at a fixed
# coefficient (Breslow ties), the network forward pass against matrix
# algebra, and the Breslow baseline hazard, survival curve and
# Harrell's C by direct recomputation.

.sn_t <- c(5, 3, 8, 3, 10, 6, 2, 7)
.sn_e <- c(1, 1, 0, 1, 1, 0, 1, 1)
.sn_X <- list(c(0.2, 1), c(1.5, -0.3), c(-0.4, 0.8), c(0.9, 0.1),
              c(-1.2, -0.5), c(0.3, 0.3), c(2, 1), c(-0.1, -1))
.sn_beta <- c(0.7, -0.4)
.sn_fit <- function() {
  risk <- vapply(.sn_X, function(x) sum(x * .sn_beta), 0)
  list(W = list(matrix(.sn_beta, 1)), b = list(0), activation = "identity",
       times = .sn_t, events = as.logical(.sn_e), risk = risk)
}

test_that("forward applies the hidden activation and a linear output", {
  W1 <- matrix(c(0.5, -1, 0.3, 0.2), 2)
  W2 <- matrix(c(1, -2), 1)
  x <- c(0.4, -0.7)
  for (act in c("tanh", "relu", "identity")) {
    f <- switch(act, tanh = tanh, relu = function(v) pmax(v, 0), identity = identity)
    h <- f(W1 %*% x + c(0.1, -0.1))
    out <- morie_survnnr_forward(list(W1, W2), list(c(0.1, -0.1), 0.5), x, act)
    expect_equal(out$output, as.numeric(W2 %*% h + 0.5), tolerance = 1e-12)
    expect_equal(out$acts[[2]], as.numeric(h), tolerance = 1e-12)
  }
})

test_that("partial_loglik is coxph's Breslow log partial likelihood", {
  skip_if_not_installed("survival")
  X <- do.call(rbind, .sn_X)
  risk <- as.numeric(X %*% .sn_beta)
  r <- morie_survnnr_partial_loglik(.sn_t, .sn_e, risk)
  cf <- survival::coxph(survival::Surv(.sn_t, .sn_e) ~ X, ties = "breslow", init = .sn_beta,
                        control = survival::coxph.control(iter.max = 0))
  expect_equal(r$loglik, cf$loglik[2], tolerance = 1e-12)
  expect_equal(r$average, r$loglik / 6)
  # shift invariance of the partial likelihood
  expect_equal(morie_survnnr_partial_loglik(.sn_t, .sn_e, risk + 3)$loglik, r$loglik, tolerance = 1e-12)
  expect_error(morie_survnnr_partial_loglik(.sn_t, .sn_e[-1], risk), "same length")
  expect_error(morie_survnnr_partial_loglik(.sn_t, rep(0, 8), risk), "at least one event")
})

test_that("risk_score, Breslow baseline hazard and survival curve", {
  fit <- .sn_fit()
  expect_equal(morie_survnnr_risk_score(fit, .sn_X), fit$risk, tolerance = 1e-12)
  bh <- morie_survnnr_baseline_hazard(fit)
  ut <- sort(unique(.sn_t[.sn_e == 1]))
  H <- cumsum(vapply(ut, function(s) sum(.sn_t == s & .sn_e == 1) / sum(exp(fit$risk[.sn_t >= s])), 0))
  expect_equal(bh$time, ut)
  expect_equal(bh$cumulative_hazard, H, tolerance = 1e-12)
  x <- c(0.5, 0.5)
  sf <- morie_survnnr_survival_function(fit, x, times = c(1, 3, 4, 20))
  Hx <- c(0, H[2], H[2], H[length(H)])
  expect_equal(sf$survival, exp(-Hx * exp(sum(x * .sn_beta))), tolerance = 1e-12)
  expect_equal(morie_survnnr_survival_function(fit, x)$time, ut)
})

test_that("concordance counts comparable pairs whose earlier time is an event", {
  fit <- .sn_fit()
  r <- fit$risk
  num <- 0
  den <- 0
  for (i in 1:8) for (j in 1:8) if (.sn_e[i] == 1 && .sn_t[i] < .sn_t[j]) {
    den <- den + 1
    num <- num + (r[i] > r[j]) + 0.5 * (r[i] == r[j])
  }
  expect_equal(morie_survnnr_concordance(fit, .sn_X, .sn_t, .sn_e), num / den, tolerance = 1e-12)
  expect_equal(morie_survnnr_concordance(fit, .sn_X[1:2], c(1, 2), c(0, 0)), 0)
})

test_that("morie_survnnr_cheatsheet names the loss", {
  expect_match(morie_survnnr_cheatsheet(), "AVERAGE negative", fixed = TRUE)
})
