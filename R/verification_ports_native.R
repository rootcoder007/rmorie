# SPDX-License-Identifier: AGPL-3.0-or-later
#
# R twins for six morie.fn modules that had no R counterpart, ported
# after the 2026-07-30 verification sweep fixed each on the Python side.
# Algorithms are ported at full precision from the corrected Python; no
# file was copied. Deterministic members (eslsmt, empby, regime_value)
# match Python bit-for-bit; the Gibbs / noise / fit members are checked
# by property. See scripts/audit/MISSING_TO_NATIVIZE.md.

#' Smoothing spline via the Reinsch penalty
#'
#' Cubic smoothing spline fit \code{(I + lambda K)^{-1} y} with the
#' Reinsch / Green-Silverman roughness penalty \eqn{K = D' W^{-1} D}.
#' The tridiagonal \eqn{W} is inverted -- the un-inverted form
#' \eqn{D' W D} agrees only at the two limits and is wrong at every
#' intermediate lambda.
#'
#' @param x strictly increasing design points.
#' @param y responses, same length as \code{x}.
#' @param lambda non-negative smoothing penalty.
#' @return list with \code{estimate}, \code{effective_df}, \code{rss},
#'   \code{lambda}, \code{n}.
#' @references Hastie, Tibshirani and Friedman (2009). \emph{The
#'   Elements of Statistical Learning} (2nd ed.), Sec. 5.4. Reinsch, C.
#'   (1967). Smoothing by spline functions. \emph{Numerische
#'   Mathematik}, 10(3), 177-183.
#' @export
morie_esl_smoothing_spline <- function(x, y, lambda) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  n <- length(x)
  if (length(y) != n) stop("x and y lengths differ", call. = FALSE)
  if (n < 3L) stop("a smoothing spline needs at least 3 points", call. = FALSE)
  if (any(diff(x) <= 0)) stop("design points must be strictly increasing",
                              call. = FALSE)
  if (lambda < 0) stop("the penalty must be non-negative", call. = FALSE)
  h <- diff(x)
  D <- matrix(0, n - 2L, n)
  for (i in seq_len(n - 2L)) {
    D[i, i] <- 1 / h[i]
    D[i, i + 1L] <- -(1 / h[i] + 1 / h[i + 1L])
    D[i, i + 2L] <- 1 / h[i + 1L]
  }
  W <- matrix(0, n - 2L, n - 2L)
  for (i in seq_len(n - 2L)) {
    W[i, i] <- (h[i] + h[i + 1L]) / 3
    if (i < n - 2L) {
      W[i, i + 1L] <- h[i + 1L] / 6
      W[i + 1L, i] <- h[i + 1L] / 6
    }
  }
  K <- crossprod(D, solve(W, D))          # D' W^{-1} D
  S <- solve(diag(n) + lambda * K)
  fit <- as.vector(S %*% y)
  resid <- y - fit
  list(estimate = fit, effective_df = sum(diag(S)),
       rss = sum(resid^2), lambda = as.numeric(lambda), n = n,
       method = "smoothing spline (I + lambda D'W^-1 D)^-1 y; df = tr(S)")
}


#' Parametric empirical Bayes shrinkage (Morris 1983)
#'
#' Normal-normal shrinkage that estimates the prior mean and shrinks
#' toward it, not toward zero. The prior mean's profile MLE is the
#' precision-weighted mean of the estimates; \eqn{\tau^2} maximises the
#' marginal likelihood.
#'
#' @param estimates point estimates \eqn{\hat\theta_k}.
#' @param standard_errors their standard errors.
#' @return list with \code{shrunk_estimates}, \code{tau2},
#'   \code{shrinkage_factors}, \code{grand_mean}, \code{k}.
#' @references Morris, C. N. (1983). Parametric empirical Bayes
#'   inference: theory and applications. \emph{Journal of the American
#'   Statistical Association}, 78(381), 47-55.
#' @export
morie_empirical_bayes <- function(estimates, standard_errors) {
  theta <- as.numeric(estimates)
  se <- as.numeric(standard_errors)
  k <- length(theta)
  sigma2 <- se^2
  neg_ll <- function(log_tau2) {
    tau2 <- exp(log_tau2)
    V <- sigma2 + tau2
    mu <- sum(theta / V) / sum(1 / V)
    0.5 * sum(log(V) + (theta - mu)^2 / V)
  }
  opt <- stats::optimize(neg_ll, c(-10, 20))
  tau2 <- exp(opt$minimum)
  V <- sigma2 + tau2
  grand_mean <- sum(theta / V) / sum(1 / V)
  B <- sigma2 / (sigma2 + tau2)
  shrunk <- grand_mean + (1 - B) * (theta - grand_mean)
  list(shrunk_estimates = shrunk, tau2 = tau2, shrinkage_factors = B,
       grand_mean = grand_mean, k = k)
}


#' Value of a treatment regime (AIPW / IPW / regression)
#'
#' Estimates the policy value of a binary treatment regime and compares
#' it against treat-all and treat-none. \code{near_indifferent} is the
#' share of units whose estimated effect lies within a tenth of a
#' standard deviation of zero -- a boundary-mass heuristic, not a
#' per-unit standard-error test.
#'
#' @param y outcome; @param d binary treatment; @param X covariates.
#' @param regime 0/1 recommendation per unit (vector or function of X).
#' @param propensity optional propensity vector; fitted by logit if NULL.
#' @param method one of \code{"aipw"}, \code{"ipw"}, \code{"regression"}.
#' @param trunc propensity truncation.
#' @return list with the policy value, SE, CI, static comparisons and
#'   diagnostics.
#' @references Zhang, B., Tsiatis, A. A., Davidian, M., Zhang, M. and
#'   Laber, E. (2012). Estimating optimal treatment regimes from a
#'   classification perspective. \emph{Stat}, 1(1), 103-114. Murphy, S.
#'   A. (2003). Optimal dynamic treatment regimes. \emph{Journal of the
#'   Royal Statistical Society Series B}, 65(2), 331-355.
#' @export
morie_regime_value <- function(y, d, X, regime, propensity = NULL,
                               method = c("aipw", "ipw", "regression"),
                               trunc = 0.01) {
  method <- match.arg(method)
  y <- as.numeric(y)
  d <- as.numeric(d)
  X <- as.matrix(X)
  n <- length(y)
  if (length(d) != n || nrow(X) != n) {
    stop("y, d and X must agree in their first dimension", call. = FALSE)
  }
  if (!all(d %in% c(0, 1))) stop("d must be binary 0/1", call. = FALSE)
  g <- if (is.function(regime)) as.numeric(regime(X)) else as.numeric(regime)
  if (length(g) != n) stop("regime length mismatch", call. = FALSE)
  if (!all(g %in% c(0, 1))) stop("regime must recommend 0 or 1", call. = FALSE)
  # Native propensity + per-arm OLS, no stats::glm / lm.fit delegation.
  # .morie_logit_fit adds its own intercept and returns fitted probs.
  if (is.null(propensity)) {
    e <- .morie_logit_fit(X, d)
  } else {
    e <- as.numeric(propensity)
    if (length(e) != n) stop("propensity length mismatch", call. = FALSE)
  }
  e <- pmin(pmax(e, trunc), 1 - trunc)
  pi <- ifelse(d == 1, e, 1 - e)
  B <- cbind(1, X)
  ols <- function(idx) {                       # normal equations, native
    Bi <- B[idx, , drop = FALSE]
    solve(crossprod(Bi), crossprod(Bi, y[idx]))
  }
  mu1 <- as.vector(B %*% ols(d == 1))
  mu0 <- as.vector(B %*% ols(d == 0))
  value <- function(rule) {
    follow <- as.numeric(d == rule)
    mu_d <- ifelse(rule == 1, mu1, mu0)
    if (method == "ipw") follow / pi * y
    else if (method == "regression") mu_d
    else mu_d + follow / pi * (y - mu_d)
  }
  psi <- value(g)
  v <- mean(psi)
  se <- stats::sd(psi) / sqrt(n)
  v_all <- mean(value(rep(1, n)))
  v_none <- mean(value(rep(0, n)))
  tau <- mu1 - mu0
  tau_se <- if (n > 1) stats::sd(tau) / sqrt(n) else NA_real_
  near <- mean(abs(tau) < max(tau_se, 1e-12) * sqrt(n) * 0.1)
  z <- 1.959963984540054
  list(estimate = v, value = v, se = se, ci = c(v - z * se, v + z * se),
       value_treat_all = v_all, value_treat_none = v_none,
       best_static = max(v_all, v_none),
       beats_static = v >= max(v_all, v_none) - 1e-12,
       gain_over_static = v - max(v_all, v_none),
       n_following = sum(d == g), following_fraction = mean(d == g),
       near_indifferent = near, method = method)
}


#' Horseshoe linear regression (Makalic & Schmidt 2016 Gibbs sampler)
#'
#' Gibbs sampler for Bayesian linear regression under the horseshoe
#' prior \eqn{\beta_j \sim N(0, \lambda_j^2 \tau^2 \sigma^2)}. All
#' conditionals follow Makalic & Schmidt (2016), Eqs. (9)-(11); the
#' hypervariance conditionals are inverse-gamma.
#'
#' @param X design matrix; @param y response; @param n_iter iterations;
#'   @param seed RNG seed.
#' @return list with \code{beta_samples}, \code{tau_samples},
#'   \code{posterior_mean}, \code{posterior_sd}, \code{n_iter}.
#' @references Carvalho, C. M., Polson, N. G. and Scott, J. G. (2010).
#'   The horseshoe estimator for sparse signals. \emph{Biometrika},
#'   97(2), 465-480. Makalic, E. and Schmidt, D. F. (2016). A simple
#'   sampler for the horseshoe estimator. \emph{IEEE Signal Processing
#'   Letters}, 23(1), 179-182.
#' @export
morie_bayesian_horseshoe <- function(X, y, n_iter = 3000L, seed = 42L) {
  set.seed(seed)
  X <- as.matrix(X)
  y <- as.numeric(y)
  n <- nrow(X)
  p <- ncol(X)
  beta <- numeric(p)
  sigma2 <- 1
  tau2 <- 1
  lambda2 <- rep(1, p)
  nu <- rep(1, p)
  xi <- 1
  XtX <- crossprod(X)
  Xty <- crossprod(X, y)
  beta_samples <- matrix(0, n_iter, p)
  tau_samples <- numeric(n_iter)
  rig <- function(shape, rate) 1 / stats::rgamma(1L, shape = shape, rate = rate)
  for (it in seq_len(n_iter)) {
    A <- XtX + diag(1 / (lambda2 * tau2), p)     # Eq. (9)
    A_inv <- solve(A)
    m <- as.vector(A_inv %*% Xty)
    ch <- chol(sigma2 * A_inv)
    beta <- as.vector(m + t(ch) %*% stats::rnorm(p))
    resid <- y - X %*% beta                       # Eq. (10)
    sigma2 <- rig((n + p) / 2,
                  0.5 * sum(resid^2) + 0.5 * sum(beta^2 / (lambda2 * tau2)))
    for (j in seq_len(p)) {                        # Eq. (11)
      lambda2[j] <- rig(1, 1 / nu[j] + beta[j]^2 / (2 * tau2 * sigma2))
    }
    tau2 <- rig((p + 1) / 2, 1 / xi + sum(beta^2 / lambda2) / (2 * sigma2))
    for (j in seq_len(p)) nu[j] <- rig(1, 1 + 1 / lambda2[j])
    xi <- rig(1, 1 + 1 / tau2)
    beta_samples[it, ] <- beta
    tau_samples[it] <- sqrt(tau2)
  }
  list(beta_samples = beta_samples, tau_samples = tau_samples,
       posterior_mean = colMeans(beta_samples),
       posterior_sd = apply(beta_samples, 2, stats::sd), n_iter = n_iter)
}


#' Monotone 1-D transport map fitted to the quantile coupling
#'
#' Fits a monotone softplus-sigmoid basis to the known 1-D optimal
#' coupling \eqn{T = F_{target}^{-1}\circ F_{source}} by least squares.
#' Monotone by construction (softplus weights are non-negative). This
#' is a monotone fit to the closed-form coupling, not a solve of the
#' Kantorovich problem.
#'
#' @param source,target samples; @param n_iter,lr,n_basis fit controls;
#'   @param seed RNG seed.
#' @return list with \code{map_at_source}, \code{monotone}, \code{rmse}.
#' @references Makkuva, A., Taghvaei, A., Oh, S. and Lee, J. (2020).
#'   Optimal transport mapping via input convex neural networks. In
#'   \emph{ICML}. Brenier, Y. (1991). Polar factorization and monotone
#'   rearrangement of vector-valued functions. \emph{Communications on
#'   Pure and Applied Mathematics}, 44(4), 375-417.
#' @export
morie_neural_kantorovich_map <- function(source, target, n_iter = 400L,
                                         lr = 0.05, n_basis = 12L, seed = 0L) {
  a <- as.numeric(source)
  b <- as.numeric(target)
  if (length(a) < 5 || length(b) < 5)
    stop("need at least 5 samples in each distribution", call. = FALSE)
  if (any(!is.finite(a)) || any(!is.finite(b)))
    stop("samples contain non-finite values", call. = FALSE)
  K <- as.integer(n_basis)
  if (K < 2L) stop("n_basis must be at least 2", call. = FALSE)
  na <- length(a)
  mu <- mean(a)
  sd <- stats::sd(a); if (sd == 0) sd <- 1
  z <- (a - mu) / sd
  knots <- seq(min(z), max(z), length.out = K)   # linspace, as in Python
  set.seed(seed)
  w <- stats::rnorm(K, sd = 0.1)
  c0 <- mean(b)
  sp <- function(v) log1p(exp(-abs(v))) + pmax(v, 0)   # softplus, stable
  dsp <- function(v) 1 / (1 + exp(-v))                 # d softplus / dv
  Tmap <- function(zz) {
    S <- outer(zz, knots, function(x, kn) 1 / (1 + exp(-(x - kn))))
    list(pred = as.vector(c0 + S %*% sp(w)), S = S)
  }
  qs <- (seq_len(na) - 0.5) / na
  tgt_q <- as.numeric(stats::quantile(b, qs, type = 7))
  order <- order(z)
  zt <- z[order]
  step <- lr
  cur <- NULL
  hist <- numeric(0)
  for (i in seq_len(n_iter)) {
    tm <- Tmap(zt)
    r <- tm$pred - tgt_q
    loss <- mean(r^2)
    # Backtracking: any step that raises the loss halves the step size,
    # so the recorded loss is non-increasing BY CONSTRUCTION. That is
    # the invariant worth testing -- it holds for every input, unlike a
    # numeric error threshold, which depends on the data and the budget.
    if (!is.null(cur) && loss > cur) {
      step <- step * 0.5
      if (step < 1e-9) break
    }
    cur <- loss
    hist <- c(hist, loss)
    gw <- as.vector(crossprod(tm$S * matrix(dsp(w), na, K, byrow = TRUE),
                              2 * r)) / na
    gc <- mean(2 * r)
    w <- w - step * gw
    c0 <- c0 - step * gc
  }
  fitted_sorted <- Tmap(zt)$pred
  fitted <- numeric(na)
  fitted[order] <- fitted_sorted
  ranks <- rank(a, ties.method = "first") - 1L      # argsort(argsort(a))
  exact <- as.numeric(stats::quantile(b, (ranks + 0.5) / na, type = 7))
  list(map_at_source = fitted, exact_map = exact,
       monotone = all(diff(fitted_sorted) >= -1e-8),
       rmse_vs_exact = sqrt(mean((fitted - exact)^2)),
       w2 = mean((fitted - a)^2), w2_exact = mean((exact - a)^2),
       loss_history = hist,
       converged = length(hist) > 2 &&
         abs(hist[length(hist)] - hist[length(hist) - 1L]) < 1e-10,
       n_source = na, n_target = length(b))
}
