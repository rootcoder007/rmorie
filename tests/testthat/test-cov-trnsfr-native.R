# Transporting an effect between cohorts (Wager 2025, Secs. 2.2 and 7.1):
# membership-odds weights from glm(), minimum-norm balancing weights,
# outcome / IPW (Hajek) / doubly robust transported effects and the
# transported MSM, each recomputed with base R. The package's ridge
# (1e-6 in the logistic fit, 1e-8 in the least squares) separates it
# from glm()/lm(), hence tolerance 1e-6 on those comparisons.

tr_data <- function() {
  set.seed(12)
  n <- 120
  X <- cbind(stats::rnorm(n), stats::runif(n))
  S <- stats::rbinom(n, 1, stats::plogis(0.4 - 0.8 * X[, 1]))
  W <- stats::rbinom(n, 1, 0.5)
  Y <- 1 + X[, 1] + W * (0.5 + 1.5 * X[, 1]) + stats::rnorm(n, sd = 0.3)
  list(X = X, S = S, W = W, Y = Y, n = n)
}

test_that("transport weights are the fitted membership odds, mean one on the source", {
  d <- tr_data()
  tw <- morie_trnsfr_transport_weights(d$X, d$S)
  pi <- stats::fitted(stats::glm(d$S ~ d$X, family = stats::binomial()))
  expect_equal(tw$pi, unname(pi), tolerance = 1e-6)
  raw <- ifelse(d$S == 1, (1 - pi) / pi, 0)
  w <- raw * sum(d$S) / sum(raw)
  expect_equal(tw$weights, unname(w), tolerance = 1e-6)
  expect_equal(mean(tw$weights[d$S == 1]), 1, tolerance = 1e-12)
  expect_equal(tw$ess, sum(tw$weights)^2 / sum(tw$weights^2), tolerance = 1e-12)
  expect_error(morie_trnsfr_transport_weights(d$X, d$S, trim = 0.45), "no overlap")
  expect_error(morie_trnsfr_transport_weights(d$X, c(1, rep(0, d$n - 1))), "at least 2 units")
  expect_error(morie_trnsfr_transport_weights(d$X, d$S * 2), "0/1")
})

test_that("balancing weights are the minimum-norm solution matching target means", {
  d <- tr_data()
  bw <- morie_trnsfr_balancing_weights(d$X, d$S)
  Ds <- cbind(1, d$X[d$S == 1, ])
  b <- colMeans(cbind(1, d$X[d$S == 0, ]))
  ref <- as.numeric(Ds %*% solve(crossprod(Ds), b))
  expect_equal(bw$weights[d$S == 1], ref, tolerance = 1e-6)
  expect_equal(bw$achieved, unname(b), tolerance = 1e-6)
  expect_lt(bw$max_imbalance, 1e-6)
  expect_identical(bw$n_negative, sum(ref < 0))
  expect_error(morie_trnsfr_balancing_weights(d$X[1:4, ], c(1, 1, 0, 0)), "cannot balance")
})

test_that("outcome, IPW, balance and DR routes transport the effect", {
  d <- tr_data()
  src <- d$S == 1
  fit <- stats::lm(d$Y[src] ~ d$X[src, ] * d$W[src])
  b <- stats::coef(fit)
  tau <- function(Xr) b[4] + Xr %*% b[5:6]
  out <- morie_trnsfr_transport_ate(d$Y, d$W, d$X, d$S, method = "outcome")
  expect_equal(out$estimate, mean(tau(d$X[!src, ])), tolerance = 1e-6)
  tw <- morie_trnsfr_transport_weights(d$X, d$S)$weights
  hajek <- function(w) {
    sum(w[src] * d$W[src] * d$Y[src]) / sum(w[src] * d$W[src]) -
      sum(w[src] * (1 - d$W[src]) * d$Y[src]) / sum(w[src] * (1 - d$W[src]))
  }
  ip <- morie_trnsfr_transport_ate(d$Y, d$W, d$X, d$S, method = "ipw")
  expect_equal(ip$estimate, hajek(tw), tolerance = 1e-12)
  bl <- morie_trnsfr_transport_ate(d$Y, d$W, d$X, d$S, method = "balance")
  expect_equal(bl$estimate, hajek(morie_trnsfr_balancing_weights(d$X, d$S)$weights),
               tolerance = 1e-12)
  mu <- function(Xr, w) b[1] + Xr %*% b[2:3] + w * tau(Xr)
  Xs <- d$X[src, ]
  term <- tw[src] * (mu(Xs, 1) - mu(Xs, 0) + d$W[src] * (d$Y[src] - mu(Xs, 1)) / 0.5 -
                       (1 - d$W[src]) * (d$Y[src] - mu(Xs, 0)) / 0.5)
  dr <- morie_trnsfr_transport_ate(d$Y, d$W, d$X, d$S)
  expect_equal(dr$estimate, sum(term) / sum(tw[src]), tolerance = 1e-6)
  expect_equal(dr$source_ate, mean(d$Y[src & d$W == 1]) - mean(d$Y[src & d$W == 0]),
               tolerance = 1e-12)
  expect_equal(morie_trnsfr(d$Y, d$W, d$X, d$S)$estimate, dr$estimate)
  expect_error(morie_trnsfr_transport_ate(d$Y, d$W, d$X, d$S, method = "tmle"), "method must be")
  expect_error(morie_trnsfr_transport_ate(d$Y, d$W * 2, d$X, d$S), "0/1")
  expect_error(morie_trnsfr_transport_ate(d$Y, d$W, d$X, d$S, e = 1), "strictly in")
  expect_error(morie_trnsfr_transport_ate(d$Y, as.numeric(!src), d$X, d$S), "both treated")
})

test_that("the transported MSM is weighted least squares on the source", {
  d <- tr_data()
  coh <- ifelse(d$S == 1, "trial", "registry")
  r <- morie_trnsfr_transfer_msm(d$Y, d$W, d$X, coh, target = "registry", e = 0.5)
  tw <- morie_trnsfr_transport_weights(d$X, d$S)$weights
  src <- d$S == 1
  f <- stats::lm(d$Y[src] ~ d$W[src], weights = (tw * 2)[src])
  expect_equal(unname(r$coef), unname(stats::coef(f)), tolerance = 1e-9)
  expect_equal(r$weights, tw * 2, tolerance = 1e-15)
  expect_identical(r$cohorts, c("registry", "trial"))
  expect_equal(morie_trnsfr_transfer_learning_msm(d$Y, d$W, d$X, coh, "registry", e = 0.5)$estimate,
               r$estimate)
  expect_error(morie_trnsfr_transfer_msm(d$Y, d$W, d$X, coh, target = "x"), "not present")
  expect_error(morie_trnsfr_transfer_msm(d$Y, d$W[-1], d$X, coh), "agree in length")
  expect_match(morie_trnsfr_cheatsheet(), "EXACTLY", fixed = TRUE)
})
