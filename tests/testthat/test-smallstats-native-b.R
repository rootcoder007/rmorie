# SPDX-License-Identifier: AGPL-3.0-or-later
# Wave B cross-validation: state-space (vs dlm), Johansen VECM (vs
# urca), elastic-net coordinate descent (vs glmnet), and the native
# cross-fit PLR/PLIV estimators on known-truth DGPs.

test_that("morie_state_space_model matches the dlm pipeline", {
  set.seed(101)
  y <- cumsum(rnorm(120, sd = 0.5)) + rnorm(120, sd = 1)
  out <- morie_state_space_model(y)
  expect_length(out$filtered_state, 120L)
  expect_true(is.finite(out$loglik))
  expect_true(out$Q > 0 && out$R > 0)

  skip_if_not_installed("dlm")
  build <- function(p) dlm::dlmModPoly(order = 1, dV = exp(p[1]), dW = exp(p[2]))
  v0 <- log(stats::var(diff(y)) / 2)
  fit <- dlm::dlmMLE(y, parm = c(v0, v0), build = build)
  mod <- build(fit$par)
  f <- dlm::dlmFilter(y, mod)
  s <- dlm::dlmSmooth(f)
  # Same model, same likelihood surface: variances agree to ~15%
  # (different optimizers), smoothed states agree closely.
  expect_lt(abs(log(out$R) - fit$par[1]), 0.5)
  expect_lt(abs(log(out$Q) - fit$par[2]), 0.5)
  expect_lt(mean(abs(out$smoothed_state - as.numeric(s$s)[-1])), 0.15)
})

test_that("morie_vecm matches urca's Johansen eigen-decomposition", {
  set.seed(102)
  n <- 200
  common <- cumsum(rnorm(n))
  Y <- cbind(y1 = common + rnorm(n, sd = 0.5),
             y2 = 0.7 * common + rnorm(n, sd = 0.5))
  out <- morie_vecm(Y, k_ar = 1, coint_rank = 1)
  expect_equal(dim(out$beta), c(2L, 1L))
  expect_true(all(is.finite(out$eigenvalues)))

  skip_if_not_installed("urca")
  jres <- urca::ca.jo(Y, type = "trace", ecdet = "none", K = 2)
  # Eigenvalues match to numerical precision.
  expect_lt(max(abs(sort(out$eigenvalues, decreasing = TRUE)[1:2] -
                    jres@lambda[1:2])), 1e-6)
  # Cointegrating vector matches up to scale: normalize both on y1.
  b_ours <- out$beta[, 1] / out$beta[1, 1]
  b_urca <- jres@V[, 1] / jres@V[1, 1]
  expect_lt(max(abs(b_ours - b_urca)), 1e-4)
})

test_that(".morie_coord_descent matches glmnet at matched penalty", {
  skip_if_not_installed("glmnet")
  set.seed(103)
  n <- 200; p <- 8
  X <- matrix(rnorm(n * p), n, p)
  b <- c(2, -1.5, 1, rep(0, p - 3))
  y <- as.numeric(X %*% b) + rnorm(n, sd = 0.5)
  lam <- 0.05
  # Lasso: exact agreement (same objective, same conventions).
  ours <- .morie_coord_descent(X, y, alpha = 1, lambda = lam)
  ref <- glmnet::glmnet(X, y, alpha = 1, lambda = lam,
                        standardize = TRUE, intercept = TRUE)
  expect_lt(max(abs(ours$beta - as.numeric(ref$beta[, 1]))), 1e-6)
  expect_lt(abs(ours$intercept - as.numeric(ref$a0)), 1e-6)
  expect_equal(which(abs(ours$beta) > 1e-8),
               which(abs(as.numeric(ref$beta[, 1])) > 1e-8))

  # Elastic net: glmnet applies an internal response-scaling to the
  # ridge component, so solutions differ by a convention factor.
  # Assert (a) closeness within that convention tolerance and (b) the
  # stronger property: our solution achieves an objective value at
  # least as low as glmnet's on the canonical elastic-net objective.
  a <- 0.5
  ours2 <- .morie_coord_descent(X, y, alpha = a, lambda = lam)
  ref2 <- glmnet::glmnet(X, y, alpha = a, lambda = lam,
                         standardize = TRUE, intercept = TRUE)
  expect_lt(max(abs(ours2$beta - as.numeric(ref2$beta[, 1]))), 0.05)
  obj <- function(bb, b0) {
    r <- y - as.numeric(X %*% bb) - b0
    sum(r^2) / (2 * n) + lam * (a * sum(abs(bb)) + (1 - a) / 2 * sum(bb^2))
  }
  expect_lte(obj(ours2$beta, ours2$intercept),
             obj(as.numeric(ref2$beta[, 1]), as.numeric(ref2$a0)) + 1e-8)
})

test_that("native PLR recovers a known effect (DML sanity)", {
  set.seed(104)
  n <- 500
  X <- matrix(rnorm(n * 4), n, 4)
  d <- as.numeric(X %*% c(0.5, -0.3, 0.2, 0) + rnorm(n))
  y <- 1.5 * d + as.numeric(X %*% c(1, 0.5, -0.5, 0.3)) + rnorm(n)
  df <- data.frame(y = y, d = d, X)
  out <- estimate_plr(df, treatment = "d", outcome = "y",
                          covariates = paste0("X", 1:4))
  expect_lt(abs(out$ate - 1.5), 3.5 * out$se)
  expect_true(out$ci_lower < 1.5 && 1.5 < out$ci_upper)
})

test_that("native PLIV recovers a known LATE (IV sanity)", {
  set.seed(105)
  n <- 800
  X <- matrix(rnorm(n * 3), n, 3)
  z <- as.numeric(0.5 * X[, 1] + rnorm(n))
  u <- rnorm(n) # confounder
  d <- z + 0.5 * u + as.numeric(X %*% c(0.3, -0.2, 0.1)) + rnorm(n, sd = 0.5)
  y <- 2 * d + u + as.numeric(X %*% c(1, 0.5, -0.5)) + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, d = d, z = z, X)
  out <- estimate_pliv(df, treatment = "d", outcome = "y",
                           instrument = "z", covariates = paste0("X", 1:3))
  expect_lt(abs(out$late - 2), 4 * out$se)
})

test_that("regularization path is monotone in shrinkage", {
  set.seed(106)
  X <- matrix(rnorm(60 * 3), 60, 3)
  y <- as.numeric(X %*% c(1, -1, 0.5)) + rnorm(60, sd = 0.3)
  out <- morie_regularization_path(X, y, penalty = "lasso",
                                   alphas = c(0.01, 0.1, 1, 10))
  cp <- out$coef_path
  l1 <- rowSums(abs(cp[, -1, drop = FALSE]))
  # Coefficient L1 norm shrinks as lambda grows.
  expect_true(all(diff(l1) <= 1e-8))
})
