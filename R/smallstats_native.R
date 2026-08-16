# SPDX-License-Identifier: AGPL-3.0-or-later
#
# smallstats_native.R -- native replacements for low-use dependencies.
#
# Each helper here replaces an external package that had <= 2 call
# sites in R/ code, so the whole Suggests entry can eventually be
# dropped once the cross-validation tests retire. Every function is
# cross-validated against the package it replaces in
# tests/testthat/test-smallstats-native.R.
#
# Replaced call sites:
#   pracma::hurstexp        -> .morie_hurst_rs        (signal.R)
#   rbounds::psens          -> .morie_psens_wilcoxon  (causal.R, effects.R)
#   ebal::ebalance          -> .morie_entropy_balance (matching.R)
#   FNN::get.knn            -> .morie_knn_index       (tps_spatial*.R)
#   smotefamily::SMOTE      -> .morie_smote           (ml.R)
#   harmonicmeanp::p.hmp    -> .morie_hmp             (multiple_testing.R)
#   MCMCpack::rdirichlet    -> inline rgamma draw     (ghcon.R)
#   boot::boot(parametric)  -> existing inline loop   (bootstrap_methods.R)

#' @srrstats {G1.0} References: Hurst (1951); Rosenbaum (2002);
#'   Hainmueller (2012); Chawla et al. (2002); Wilson (2019).
#' @srrstats {G2.1} Inputs validated for type/length at each entry.
#' @noRd
NULL

# ---------------------------------------------------------------------------
# Hurst exponent via simple rescaled-range (R/S) analysis -- an exact
# replica of pracma::hurstexp()$Hs: pad odd-length series to even,
# truncate to the length in [0.99*N, N] with the most divisors >= 50
# (pracma's block-optimal OptN), then compute the whole-series
# statistic log(R/S) / log(n).
#' .morie_hurst_rs
#'
#' Hurst exponent via simple rescaled-range (R/S) analysis -- an exact
#' replica of pracma::hurstexp()$Hs: pad odd-length series to even,
#' truncate to the length in [0.99*N, N] with the most divisors >= 50
#' (pracma\'s block-optimal OptN), then compute the whole-series
#' statistic log(R/S) / log(n).
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @param d Defaults to \code{50L}.
#' @return A numeric value.
#' @export
.morie_hurst_rs <- function(x, d = 50L) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 16L) {
    stop("Hurst R/S estimation needs at least 16 finite observations.",
         call. = FALSE)
  }
  if (n %% 2L != 0L) {
    x <- c(x, (x[n - 1L] + x[n]) / 2)
    n <- n + 1L
  }
  divisors <- function(m, m0) {
    cand <- m0:floor(m / 2)
    cand[m %% cand == 0]
  }
  n0 <- min(floor(0.99 * n), n - 1L)
  opt_n <- n0
  dv <- divisors(n0, d)
  for (i in (n0 + 1L):n) {
    dw <- divisors(i, d)
    if (length(dw) > length(dv)) {
      opt_n <- i
      dv <- dw
    }
  }
  x <- x[seq_len(opt_n)]
  y <- x - mean(x)
  s <- cumsum(y)
  rs <- (max(s) - min(s)) / stats::sd(x)
  log(rs) / log(length(x))
}

# ---------------------------------------------------------------------------
# Rosenbaum sensitivity bounds for the Wilcoxon signed-rank statistic
# (Rosenbaum 2002, ch. 4), the same quantity rbounds::psens() reports.
# Pairs are formed positionally from equal-length treated/control
# vectors, matching how psens() was called here.
#' .morie_psens_wilcoxon
#'
#' Rosenbaum sensitivity bounds for the Wilcoxon signed-rank statistic
#' (Rosenbaum 2002, ch. 4), the same quantity rbounds::psens() reports.
#' Pairs are formed positionally from equal-length treated/control
#' vectors, matching how psens() was called here.
#'
#' @param treated A vector; its length is taken.
#' @param control A vector; its length is taken.
#' @param gamma Passed to \code{.morie_psens_wilcoxon_d}.
#' @return The value of \code{.morie_psens_wilcoxon_d}.
#' @export
.morie_psens_wilcoxon <- function(treated, control, gamma) {
  stopifnot(length(treated) == length(control), gamma >= 1)
  .morie_psens_wilcoxon_d(as.numeric(treated) - as.numeric(control), gamma)
}

# One-sample form: Rosenbaum bounds directly on pair differences.
# Exact replica of rbounds::psens(): both bounds share the variance
# computed at p+ (a documented quirk of that implementation), the
# lower bound centres at p-, the upper at p+.
#' One-sample form: Rosenbaum bounds directly on pair differences
#'
#' Exact replica of rbounds::psens(): both bounds share the variance
#' computed at p+ (a documented quirk of that implementation), the lower
#' bound centres at p-, the upper at p+.
#'
#' @param d A vector; its length is taken and its elements indexed.
#' @param gamma Numeric; combined arithmetically in the body.
#' @return A vector, from \code{c}.
#' @export
.morie_psens_wilcoxon_d <- function(d, gamma) {
  stopifnot(gamma >= 1)
  d <- as.numeric(d)
  d <- d[d != 0]
  n <- length(d)
  if (n == 0L) {
    return(c(p_lower = 1, p_upper = 1))
  }
  r <- rank(abs(d), ties.method = "average")
  t_obs <- sum(r[d > 0])
  p_plus <- gamma / (1 + gamma)
  p_minus <- 1 / (1 + gamma)
  e_plus <- sum(r * p_plus)
  e_minus <- sum(r * p_minus)
  v <- sum(r^2 * p_plus * (1 - p_plus))
  c(p_lower = 1 - stats::pnorm((t_obs - e_minus) / sqrt(v)),
    p_upper = 1 - stats::pnorm((t_obs - e_plus) / sqrt(v)))
}

# ---------------------------------------------------------------------------
# Entropy balancing (Hainmueller 2012), ATT flavour: reweight controls
# so their covariate means equal the treated means. Solves the convex
# dual  min_l  log(sum_c exp(-Xc l)) + mbar' l  via BFGS with an
# analytic gradient; weights are the softmax of -Xc l, rescaled to sum
# to the number of controls (the ebal::ebalance convention).
#' .morie_entropy_balance
#'
#' Entropy balancing (Hainmueller 2012), ATT flavour: reweight controls
#' so their covariate means equal the treated means. Solves the convex
#' dual min_l log(sum_c exp(-Xc l)) + mbar\' l via BFGS with an analytic
#' gradient; weights are the softmax of -Xc l, rescaled to sum to the
#' number of controls (the ebal::ebalance convention).
#'
#' @param t_mask A flag; the body branches on it.
#' @param X A matrix; indexed by row and column.
#' @param max_iter Defaults to \code{200L}.
#' @param tol Defaults to \code{1e-08}.
#' @return A list with \code{w}, \code{converged}, \code{max_imbalance}.
#' @export
.morie_entropy_balance <- function(t_mask, X, max_iter = 200L, tol = 1e-8) {
  t_mask <- as.logical(t_mask)
  X <- as.matrix(X)
  Xt <- X[t_mask, , drop = FALSE]
  Xc <- X[!t_mask, , drop = FALSE]
  if (nrow(Xt) == 0L || nrow(Xc) == 0L) {
    stop("Entropy balancing needs both treated and control units.",
         call. = FALSE)
  }
  # Scale columns for conditioning; lambda is rescaled back implicitly
  # because weights only depend on Xc %*% l.
  mu <- colMeans(Xc)
  sd_ <- pmax(apply(Xc, 2, stats::sd), 1e-12)
  Zc <- sweep(sweep(Xc, 2, mu), 2, sd_, "/")
  mbar <- (colMeans(Xt) - mu) / sd_

  obj <- function(l) {
    eta <- -as.numeric(Zc %*% l)
    m <- max(eta)
    log(sum(exp(eta - m))) + m + sum(mbar * l)
  }
  grad <- function(l) {
    eta <- -as.numeric(Zc %*% l)
    w <- exp(eta - max(eta))
    w <- w / sum(w)
    -as.numeric(crossprod(Zc, w)) + mbar
  }
  opt <- stats::optim(rep(0, ncol(Zc)), obj, grad, method = "BFGS",
                      control = list(maxit = max_iter, reltol = tol))
  eta <- -as.numeric(Zc %*% opt$par)
  w <- exp(eta - max(eta))
  w <- w / sum(w) * nrow(Zc)
  bal <- max(abs(colSums(w * Zc) / nrow(Zc) * nrow(Zc) / nrow(Zc) - mbar))
  list(w = w, converged = opt$convergence == 0L,
       max_imbalance = max(abs(as.numeric(crossprod(Zc, w / sum(w))) - mbar)))
}

# ---------------------------------------------------------------------------
# k-nearest-neighbour indices (Euclidean), the FNN::get.knn()$nn.index
# surface used here. Brute force O(n^2) -- the call sites feed spatial
# unit tables (hundreds of rows), where this is instant.
#' .morie_knn_index
#'
#' k-nearest-neighbour indices (Euclidean), the FNN::get.knn()$nn.index
#' surface used here. Brute force O(n^2) -- the call sites feed spatial
#' unit tables (hundreds of rows), where this is instant.
#'
#' @param coords A matrix; passed to \code{nrow}.
#' @param k Passed to \code{.morie_knn_index_cpp}.
#' @return The value of \code{.morie_knn_index_cpp}.
#' @export
.morie_knn_index <- function(coords, k) {
  coords <- as.matrix(coords)
  n <- nrow(coords)
  k <- as.integer(k)
  if (k >= n) {
    stop("k must be smaller than the number of rows.", call. = FALSE)
  }
  # C++ kernel (src/morie_smallstats.cpp): partial-sort per row.
  .morie_knn_index_cpp(coords, k)
}

# ---------------------------------------------------------------------------
# SMOTE (Chawla et al. 2002): synthesize minority-class points by
# linear interpolation towards random minority k-NN until classes
# balance. Returns rows to append (X_new, y_new).
#' .morie_smote
#'
#' SMOTE (Chawla et al. 2002): synthesize minority-class points by
#' linear interpolation towards random minority k-NN until classes
#' balance. Returns rows to append (X_new, y_new).
#'
#' @param X A matrix; indexed by row and column.
#' @param y_chr See Usage.
#' @param k Passed to \code{.morie_knn_index}.
#' @return A list with \code{X_new}, \code{y_new}.
#' @export
.morie_smote <- function(X, y_chr, k) {
  X <- as.matrix(X)
  counts <- table(y_chr)
  minority <- names(counts)[which.min(counts)]
  idx_min <- which(y_chr == minority)
  n_needed <- max(counts) - length(idx_min)
  if (n_needed <= 0L || length(idx_min) < 2L) {
    return(list(X_new = X[0, , drop = FALSE], y_new = character(0)))
  }
  k <- min(as.integer(k), length(idx_min) - 1L)
  Xm <- X[idx_min, , drop = FALSE]
  nn <- .morie_knn_index(Xm, k)
  base_idx <- rep_len(seq_len(nrow(Xm)), n_needed)
  X_new <- t(vapply(base_idx, function(i) {
    nb <- Xm[nn[i, sample.int(k, 1L)], ]
    Xm[i, ] + stats::runif(1L) * (nb - Xm[i, ])
  }, numeric(ncol(Xm))))
  colnames(X_new) <- colnames(X)
  list(X_new = X_new, y_new = rep(minority, n_needed))
}

# ---------------------------------------------------------------------------
# Elastic-net coordinate descent on standardized covariates -- the
# shared core behind morie_penalized_regression() and
# morie_regularization_path() (replacing their glmnet delegation).
# Returns coefficients on the ORIGINAL scale plus intercept.
# `warm` optionally seeds beta (standardized scale) for path fits.
#' .morie_coord_descent
#'
#' Elastic-net coordinate descent on standardized covariates -- the
#' shared core behind morie_penalized_regression() and
#' morie_regularization_path() (replacing their glmnet delegation).
#' Returns coefficients on the ORIGINAL scale plus intercept. `warm`
#' optionally seeds beta (standardized scale) for path fits.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param y Numeric; passed to \code{mean}.
#' @param alpha Passed to \code{.morie_coord_descent_cpp}.
#' @param lambda Passed to \code{.morie_coord_descent_cpp}.
#' @param max_iter Defaults to \code{1000L}.
#' @param tol Passed to \code{.morie_coord_descent_cpp}. Defaults to \code{1e-06}.
#' @param warm Defaults to \code{NULL}.
#' @return A list with \code{beta}, \code{beta_std}, \code{intercept}, \code{n_iter}.
#' @export
.morie_coord_descent <- function(X, y, alpha, lambda,
                                 max_iter = 1000L, tol = 1e-6,
                                 warm = NULL) {
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  ym <- mean(y)
  yc <- y - ym
  xm <- colMeans(X)
  # Population (1/n) standard deviation -- glmnet's standardization
  # convention; with it, colSums(Xs^2)/n == 1 exactly.
  xs <- sqrt(colMeans(sweep(X, 2, xm)^2))
  xs[xs == 0] <- 1
  Xs <- sweep(sweep(X, 2, xm), 2, xs, "/")
  warm_v <- if (is.null(warm)) rep(0, p) else warm
  fit <- .morie_coord_descent_cpp(Xs, yc, alpha, lambda,
                                  as.integer(max_iter), tol, warm_v)
  beta <- as.numeric(fit$beta_std)
  n_iter_done <- fit$n_iter
  beta_orig <- beta / xs
  intercept <- ym - sum(xm * beta_orig)
  list(beta = beta_orig, beta_std = beta, intercept = intercept,
       n_iter = n_iter_done)
}

# ---------------------------------------------------------------------------
# Ridge regression with SVD path + k-fold CV lambda selection, then
# prediction -- the cv.glmnet(alpha = 0, s = "lambda.min") surface the
# DML cross-fit nuisance learners used. The SVD makes the whole lambda
# path essentially free.
#' .morie_cv_ridge_predict
#'
#' Ridge regression with SVD path + k-fold CV lambda selection, then
#' prediction -- the cv.glmnet(alpha = 0, s = "lambda.min") surface the
#' DML cross-fit nuisance learners used. The SVD makes the whole lambda
#' path essentially free.
#'
#' @param x_train A matrix; passed to \code{nrow}.
#' @param z_train Numeric; passed to \code{mean}.
#' @param x_test A matrix; passed to \code{as.matrix}.
#' @param n_folds A count; the body uses it as \code{seq_len(...)}. Defaults to \code{5L}.
#' @param lambdas Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.
#' @return A numeric value.
#' @export
.morie_cv_ridge_predict <- function(x_train, z_train, x_test,
                                    n_folds = 5L, lambdas = NULL) {
  x_train <- as.matrix(x_train)
  x_test <- as.matrix(x_test)
  n <- nrow(x_train)
  if (is.null(lambdas)) lambdas <- 10^seq(-4, 4, length.out = 60)
  xm <- colMeans(x_train)
  zs <- mean(z_train)
  Xc <- sweep(x_train, 2, xm)
  zc <- z_train - zs
  ridge_beta <- function(sv, lam) {
    # beta = V diag(d/(d^2 + n*lam)) U' zc
    d <- sv$d
    sv$v %*% ((d / (d^2 + n * lam)) * crossprod(sv$u, zc))
  }
  folds <- sample(rep(seq_len(n_folds), length.out = n))
  cv_err <- numeric(length(lambdas))
  for (f in seq_len(n_folds)) {
    tr <- folds != f
    Xf <- Xc[tr, , drop = FALSE]
    zf <- zc[tr]
    xmf <- colMeans(Xf)
    Xf <- sweep(Xf, 2, xmf)
    zsf <- mean(zf)
    zf <- zf - zsf
    svf <- svd(Xf)
    Xv <- sweep(Xc[!tr, , drop = FALSE], 2, xmf)
    for (li in seq_along(lambdas)) {
      d <- svf$d
      b <- svf$v %*% ((d / (d^2 + sum(tr) * lambdas[li])) *
                        crossprod(svf$u, zf))
      pred <- as.numeric(Xv %*% b) + zsf
      cv_err[li] <- cv_err[li] + sum((zc[!tr] - pred)^2)
    }
  }
  lam <- lambdas[which.min(cv_err)]
  sv <- svd(Xc)
  b <- ridge_beta(sv, lam)
  as.numeric(sweep(x_test, 2, xm) %*% b) + zs
}

# ---------------------------------------------------------------------------
# Sobol low-discrepancy sequence via gray-code construction with
# Joe-Kuo direction numbers (dims 1-10) -- replaces randtoolbox::sobol
# for the QMC helper. Unscrambled; matches randtoolbox's unscrambled
# output (same standard direction numbers), cross-validated in tests.
#' .morie_sobol
#'
#' Sobol low-discrepancy sequence via gray-code construction with
#' Joe-Kuo direction numbers (dims 1-10) -- replaces randtoolbox::sobol
#' for the QMC helper. Unscrambled; matches randtoolbox\'s unscrambled
#' output (same standard direction numbers), cross-validated in tests.
#'
#' @param n Passed to \code{.morie_sobol_cpp}.
#' @param d Passed to \code{.morie_sobol_cpp}.
#' @return The value of \code{.morie_sobol_cpp}.
#' @export
.morie_sobol <- function(n, d) {
  n <- as.integer(n)
  d <- as.integer(d)
  if (d < 1L || d > 10L) {
    stop("native Sobol supports 1 <= d <= 10 dimensions.", call. = FALSE)
  }
  .morie_sobol_cpp(n, d)
}

# ---------------------------------------------------------------------------
# GEE with Poisson family and exchangeable working correlation
# (Liang & Zeger 1986) -- the geepack::geeglm surface used by the OTIS
# batch module. Fisher scoring on the working model, moment estimate
# of the exchangeable alpha, robust (sandwich) covariance.
#' .morie_gee_poisson_exch
#'
#' GEE with Poisson family and exchangeable working correlation (Liang &
#' Zeger 1986) -- the geepack::geeglm surface used by the OTIS batch
#' module. Fisher scoring on the working model, moment estimate of the
#' exchangeable alpha, robust (sandwich) covariance.
#'
#' @param X A matrix; indexed by row and column.
#' @param y A vector; its length is taken and its elements indexed.
#' @param id See Usage.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{50L}.
#' @param tol Defaults to \code{1e-08}.
#' @return A list with \code{coefficients}, \code{vbeta}, \code{alpha}, \code{phi}, \code{n_iter}, \code{converged}.
#' @export
.morie_gee_poisson_exch <- function(X, y, id, max_iter = 50L, tol = 1e-8) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  id <- as.integer(factor(id))
  n <- length(y)
  p <- ncol(X)
  beta <- stats::glm.fit(X, y, family = stats::poisson())$coefficients
  clusters <- split(seq_len(n), id)
  alpha <- 0
  for (it in seq_len(max_iter)) {
    eta <- as.numeric(X %*% beta)
    mu <- exp(eta)
    r <- (y - mu) / sqrt(mu) # Pearson residuals (phi = 1 scale)
    # Moment estimator of exchangeable correlation.
    num <- 0; cnt <- 0
    for (ix in clusters) {
      ni <- length(ix)
      if (ni < 2L) next
      ri <- r[ix]
      num <- num + (sum(ri)^2 - sum(ri^2)) / 2
      cnt <- cnt + ni * (ni - 1) / 2
    }
    phi <- sum(r^2) / (n - p)
    alpha <- if (cnt > 0) num / ((cnt - p / 2) * phi) else 0
    alpha <- min(max(alpha, -0.99), 0.99)
    # Fisher scoring step with working covariance.
    M <- matrix(0, p, p); U <- numeric(p)
    B <- matrix(0, p, p)
    for (ix in clusters) {
      ni <- length(ix)
      Di <- X[ix, , drop = FALSE] * mu[ix] # d mu / d beta
      Ai_half <- sqrt(mu[ix])
      Ri <- matrix(alpha, ni, ni); diag(Ri) <- 1
      Vi <- (Ai_half %o% Ai_half) * Ri * phi
      Vinv <- solve(Vi)
      DtV <- crossprod(Di, Vinv)
      M <- M + DtV %*% Di
      si <- y[ix] - mu[ix]
      U <- U + as.numeric(DtV %*% si)
      B <- B + DtV %*% (si %o% si) %*% t(DtV)
    }
    step <- solve(M, U)
    beta <- beta + step
    if (max(abs(step)) < tol) break
  }
  Minv <- solve(M)
  vbeta <- Minv %*% B %*% Minv
  list(coefficients = as.numeric(beta), vbeta = vbeta,
       alpha = alpha, phi = phi, n_iter = it,
       converged = max(abs(step)) < tol)
}

# ---------------------------------------------------------------------------
# Asymptotically exact harmonic mean p-value (Wilson 2019, PNAS).
# The statistic t = mean(1/p) is asymptotically Landau distributed with
# location log(L) + 0.874367040387922 and scale pi/2; the combined
# p-value is the Landau upper tail at t. The Landau density has no
# closed form; integrate its standard integral representation.
#' .morie_hmp
#'
#' Asymptotically exact harmonic mean p-value (Wilson 2019, PNAS). The
#' statistic t = mean(1/p) is asymptotically Landau distributed with
#' location log(L) + 0.874367040387922 and scale pi/2; the combined
#' p-value is the Landau upper tail at t. The Landau density has no
#' closed form; integrate its standard integral representation.
#'
#' @param p Numeric; combined arithmetically in the body.
#' @param L Numeric; passed to \code{log}. Defaults to \code{length(p)}.
#' @return A numeric value.
#' @export
.morie_hmp <- function(p, L = length(p)) {
  p <- pmax(as.numeric(p), 1e-300)
  t_stat <- mean(1 / p)
  mu <- log(L) + 0.874367040387922
  sigma <- pi / 2
  landau_density <- function(x) {
    vapply(x, function(xi) {
      stats::integrate(function(u) {
        exp(-u * log(u) - xi * u) * sin(pi * u)
      }, 0, Inf, rel.tol = 1e-9, stop.on.error = FALSE)$value / pi
    }, numeric(1))
  }
  z <- (t_stat - mu) / sigma
  # Upper tail from z to a far cutoff; Landau tail ~ 1/x, integrate the
  # transformed tail 1/x^2 weight analytically past the cutoff.
  upper <- stats::integrate(function(x) landau_density(x), z, 400,
                            rel.tol = 1e-8, stop.on.error = FALSE)$value
  tail_corr <- 1 / 400 # Landau upper tail beyond cutoff ~ 1/x
  min(1, max(0, upper + tail_corr))
}
