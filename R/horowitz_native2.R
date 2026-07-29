# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Horowitz shelf mirrors -- built ON TOP of R/aaa_helpers_horowitz.R
# rather than duplicating it.
#
# The collision scan found seven internal helpers there
# (.hrz_silverman, .hrz_gauss_kernel, .hrz_nw_loo, .hrz_probit_newton,
# .hrz_logit_newton, .hrz_qreg_irls, .hrz_hermite) plus existing
# hrz*.R modules. .hrz_silverman already matches morie.fn's
# silverman_bw exactly -- 1.06 * min(sd, IQR/1.349) * n^(-1/5) -- so
# it is reused, keeping the two languages on one bandwidth rule.
#
# Mirrors morie.fn._horowitz and the hrz* modules. Spec: Horowitz,
# J. L., Semiparametric and Nonparametric Methods in Econometrics,
# Springer.

#' Kernel density estimate with the Silverman bandwidth
#'
#' \eqn{\hat f(x) = (nh)^{-1}\sum K((x - X_i)/h)}. The MISE-optimal
#' bandwidth is of order \eqn{n^{-1/5}}, giving the \eqn{n^{-2/5}}
#' rate -- slower than parametric and not improvable for a
#' twice-differentiable density, which is why the book builds root-n
#' functionals of this object rather than using it directly.
#' Mirrors \code{morie.fn.hrzkde}.
#'
#' @param x numeric sample.
#' @param grid evaluation points; a spanning grid when NULL.
#' @param h bandwidth; \code{.hrz_silverman} when NULL.
#' @return list: grid, density, bandwidth, rate_exponent, n.
#' @references Horowitz, J. L. Semiparametric and Nonparametric
#'   Methods in Econometrics. Springer. Ch. 2.
#' @examples
#' morie_kde_h(rnorm(200))$bandwidth
#' @export
morie_kde_h <- function(x, grid = NULL, h = NULL) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L) stop("need at least 2 observations.", call. = FALSE)
  hh <- if (is.null(h)) .hrz_silverman(x) else as.numeric(h)
  if (hh <= 0) stop("bandwidth must be positive.", call. = FALSE)
  g <- if (is.null(grid)) seq(min(x) - 3 * hh, max(x) + 3 * hh, length.out = 512L)
       else as.numeric(grid)
  dens <- vapply(g, function(pt) sum(.hrz_gauss_kernel((pt - x) / hh)), 0) / (n * hh)
  list(grid = g, density = dens, bandwidth = hh, rate_exponent = -0.4, n = n,
       method = "KDE with n^{-1/5} bandwidth; rate n^{-2/5}")
}

#' MISE-optimal bandwidth
#'
#' \eqn{h_{opt} = \[R(K)/(\mu_2(K)^2 \int (f'')^2 n)\]^{1/5}}. Depends on
#' the UNKNOWN \eqn{\int (f'')^2}, which is why every practical rule
#' approximates it; a normal reference is used when it is not
#' supplied, and the result says so. Mirrors
#' \code{morie.fn.hrzbwopt}.
#'
#' @param x numeric sample.
#' @param f_second_deriv_l2 the true integral, if known.
#' @return list: h_opt, R_K, mu2_K, f2_l2, normal_reference_used.
#' @references Horowitz, Ch. 2.
#' @examples
#' morie_bandwidth_mise(rnorm(200))$h_opt
#' @export
morie_bandwidth_mise <- function(x, f_second_deriv_l2 = NULL) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L) stop("need at least 2 observations.", call. = FALSE)
  R_K <- .hrz_R_K_gaussian
  mu2 <- 1 # unit-variance Gaussian kernel
  ref <- is.null(f_second_deriv_l2)
  if (ref) {
    s <- stats::sd(x)
    if (s <= 0) stop("x has zero spread.", call. = FALSE)
    f2 <- 3 / (8 * sqrt(pi) * s^5)
  } else {
    f2 <- as.numeric(f_second_deriv_l2)
    if (f2 <= 0) stop("f_second_deriv_l2 must be positive.", call. = FALSE)
  }
  list(h_opt = (R_K / (mu2^2 * f2 * n))^0.2, R_K = R_K, mu2_K = mu2,
       f2_l2 = f2, normal_reference_used = ref, n = n,
       method = "h_opt = [R(K)/(mu2^2 int(f'')^2 n)]^{1/5}")
}

#' Nadaraya-Watson kernel regression
#'
#' Local CONSTANT fit, so the boundary bias is O(h) where local linear
#' achieves O(h^2). Mirrors \code{morie.fn.hrznwr}.
#'
#' @param x,y numeric regressor and response.
#' @param grid evaluation points.
#' @param h bandwidth.
#' @return list: grid, fitted, bandwidth, n.
#' @references Horowitz, Ch. 2.
#' @examples
#' morie_nw_regression(rnorm(100), rnorm(100))$bandwidth
#' @export
morie_nw_regression <- function(x, y, grid = NULL, h = NULL) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y)) stop("x and y must match.", call. = FALSE)
  hh <- if (is.null(h)) .hrz_silverman(x) else as.numeric(h)
  if (hh <= 0) stop("bandwidth must be positive.", call. = FALSE)
  g <- if (is.null(grid)) seq(min(x), max(x), length.out = 200L) else as.numeric(grid)
  fitted <- vapply(g, function(pt) {
    w <- .hrz_gauss_kernel((pt - x) / hh)
    s <- sum(w)
    if (s > 0) sum(w * y) / s else NA_real_
  }, 0)
  list(grid = g, fitted = fitted, bandwidth = hh, n = length(x),
       method = "NW local constant; O(h) boundary bias")
}

#' Local linear regression
#'
#' Fits a local LINE, making the bias O(h^2) uniformly INCLUDING at
#' the boundary -- the automatic boundary correction that motivates
#' preferring it to Nadaraya-Watson. The local slope estimates m'(x)
#' for free. Mirrors \code{morie.fn.hrzllr}.
#'
#' @param x,y numeric regressor and response.
#' @param grid evaluation points.
#' @param h bandwidth.
#' @return list: grid, fitted, slope, bandwidth, n.
#' @references Horowitz, Ch. 2.
#' @examples
#' morie_local_linear(runif(200), runif(200))$bandwidth
#' @export
morie_local_linear <- function(x, y, grid = NULL, h = NULL) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y)) stop("x and y must match.", call. = FALSE)
  hh <- if (is.null(h)) .hrz_silverman(x) else as.numeric(h)
  if (hh <= 0) stop("bandwidth must be positive.", call. = FALSE)
  g <- if (is.null(grid)) seq(min(x), max(x), length.out = 200L) else as.numeric(grid)
  fit <- slp <- numeric(length(g))
  for (i in seq_along(g)) {
    w <- .hrz_gauss_kernel((x - g[i]) / hh)
    if (sum(w) <= 0) {
      fit[i] <- slp[i] <- NA_real_
      next
    }
    X <- cbind(1, x - g[i])
    WX <- X * w
    coef <- tryCatch(solve(t(X) %*% WX, t(WX) %*% y),
                     error = function(e) qr.solve(t(X) %*% WX, t(WX) %*% y))
    fit[i] <- coef[1]
    slp[i] <- coef[2]
  }
  list(grid = g, fitted = fit, slope = slp, bandwidth = hh, n = length(x),
       method = "Local linear; O(h^2) bias including at the boundary")
}

#' Series (sieve) regression
#'
#' \eqn{\hat m(x) = \sum_k \hat a_k p_k(x)}. K plays the role of a
#' bandwidth and must grow with n: a fixed K is a parametric model
#' that never becomes consistent. Mirrors
#' \code{morie.fn.hrzsier}.
#'
#' @param x,y numeric regressor and response.
#' @param K sieve dimension.
#' @param kind "poly" or "fourier".
#' @return list: fitted, coefficients, K, r_squared, df_ratio.
#' @references Horowitz, Ch. 2.
#' @examples
#' morie_series_regression(runif(100), runif(100), K = 4)$K
#' @export
morie_series_regression <- function(x, y, K = 5L, kind = "poly") {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y)) stop("x and y must match.", call. = FALSE)
  K <- as.integer(K)
  if (K < 1L || K > length(x)) stop("K out of range.", call. = FALSE)
  P <- morie_sieve_basis(x, K, kind)
  a <- qr.solve(P, y)
  fit <- as.numeric(P %*% a)
  ss_res <- sum((y - fit)^2)
  ss_tot <- sum((y - mean(y))^2)
  list(fitted = fit, coefficients = a, K = K,
       r_squared = if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_,
       df_ratio = K / length(x),
       method = "Series regression; K must grow with n, like 1/h")
}

#' Sieve basis
#'
#' Polynomial or Fourier basis of dimension K on the rescaled unit
#' interval. Mirrors \code{morie.fn._horowitz.sieve_basis}.
#'
#' @param x numeric regressor.
#' @param K basis dimension.
#' @param kind "poly" or "fourier".
#' @return numeric matrix (length(x) x K).
#' @references Horowitz, Ch. 2.
#' @examples
#' dim(morie_sieve_basis(runif(20), 4))
#' @export
morie_sieve_basis <- function(x, K = 5L, kind = "poly") {
  x <- as.numeric(x)
  K <- as.integer(K)
  if (K < 1L) stop("K must be at least 1.", call. = FALSE)
  lo <- min(x)
  hi <- max(x)
  z <- if (hi > lo) (x - lo) / (hi - lo) else rep(0, length(x))
  if (kind == "poly") {
    return(vapply(0:(K - 1L), function(k) z^k, numeric(length(z))))
  }
  if (kind == "fourier") {
    cols <- list(rep(1, length(z)))
    for (k in seq_len(K - 1L)) {
      cols[[k + 1L]] <- if (k %% 2L == 1L) sin(pi * k * z) else cos(pi * k * z)
    }
    return(do.call(cbind, cols[seq_len(K)]))
  }
  stop("kind must be 'poly' or 'fourier'.", call. = FALSE)
}

#' Single-index kernel regression
#'
#' \eqn{\hat G(v) = \sum K_h(X_i'\beta - v)Y_i / \sum K_h(X_i'\beta -
#' v)}. Regressing on the scalar index collapses a d-dimensional
#' smoothing problem to one dimension, restoring the \eqn{n^{-2/5}}
#' rate for any d. Mirrors \code{morie.fn.hrznwrg}.
#'
#' @param X numeric matrix of covariates.
#' @param y numeric response.
#' @param beta index coefficients.
#' @param h bandwidth on the index scale.
#' @return list: index_grid, G, index, bandwidth, rate_exponent, d.
#' @references Horowitz, Ch. 2.
#' @examples
#' X <- matrix(rnorm(200), ncol = 2)
#' morie_index_regression(X, rnorm(100), c(1, -0.5))$rate_exponent
#' @export
morie_index_regression <- function(X, y, beta, h = NULL, grid = NULL) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  if (nrow(X) != length(y)) X <- t(X)
  if (nrow(X) != length(y)) stop("X must have one row per y.", call. = FALSE)
  b <- as.numeric(beta)
  if (length(b) != ncol(X)) stop("beta must match the columns of X.", call. = FALSE)
  v <- as.numeric(X %*% b)
  out <- morie_nw_regression(v, y, grid = grid, h = h)
  list(index_grid = out$grid, G = out$fitted, index = v,
       bandwidth = out$bandwidth, rate_exponent = -0.4, d = ncol(X),
       method = "NW on X'beta; n^{-2/5} rate regardless of d")
}

#' Ichimura semiparametric least squares
#'
#' \eqn{\hat\beta = \arg\min_{|b_1|=1} \sum (Y_i -
#' \hat G_{-i,b}(X_i'b))^2}, using the existing \code{.hrz_nw_loo}
#' leave-one-out smoother. The \eqn{|b_1| = 1} normalisation is
#' required because the index scale is not identified, and leaving i
#' out prevents the criterion collapsing by interpolation. beta is
#' root-n even though G is not. Mirrors
#' \code{morie.fn.hrzich} and \code{hrznls}.
#'
#' @param X numeric covariate matrix.
#' @param y numeric response.
#' @param h bandwidth.
#' @return list: beta, sse, converged, root_n, n, d.
#' @references Horowitz, Ch. 2 (Ichimura 1993).
#' @examples
#' X <- matrix(rnorm(200), ncol = 2)
#' morie_ichimura(X, tanh(X %*% c(1, -0.6)))$beta[1]
#' @export
morie_ichimura <- function(X, y, h = NULL) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  if (nrow(X) != length(y)) X <- t(X)
  if (nrow(X) != length(y)) stop("X must have one row per y.", call. = FALSE)
  d <- ncol(X)
  if (d < 2L) stop("need at least 2 covariates.", call. = FALSE)
  sse <- function(rest) {
    b <- c(1, rest)
    v <- as.numeric(X %*% b)
    hh <- if (is.null(h)) .hrz_silverman(v) else as.numeric(h)
    G <- .hrz_nw_loo(v, y, hh)
    r <- y - G
    r <- r[is.finite(r)]
    if (length(r)) sum(r^2) else 1e18
  }
  # With d = 2 the free parameter is scalar, where R warns that
  # Nelder-Mead is unreliable -- and it is. Use optimize() there.
  if (d == 2L) {
    o <- stats::optimize(function(b) sse(b), interval = c(-20, 20), tol = 1e-8)
    res <- list(par = o$minimum, value = o$objective, convergence = 0L)
  } else {
    res <- stats::optim(rep(0, d - 1L), sse, method = "Nelder-Mead",
                        control = list(maxit = 2000))
  }
  list(beta = c(1, res$par), sse = res$value, converged = res$convergence == 0,
       root_n = TRUE, n = nrow(X), d = d,
       method = "Ichimura SLS; |b1|=1 and leave-one-out are both required")
}

#' Manski maximum score and Horowitz's smoothed version
#'
#' Maximum score maximises \eqn{\sum (2Y_i-1)1\{X_i'b>0\}} under only
#' a conditional-MEDIAN restriction, tolerating arbitrary
#' heteroskedasticity, but converges at \eqn{n^{-1/3}} to a
#' non-normal Chernoff limit -- ordinary standard errors do NOT apply.
#' Smoothing the indicator with a kernel CDF lifts the rate to
#' \eqn{n^{-r/(2r+1)}} and restores normality. Mirrors
#' \code{morie.fn.hrzmscr} and \code{hrzsms}.
#'
#' @param X numeric covariate matrix.
#' @param y binary 0/1 response.
#' @param smoothed logical; the smoothed estimator when TRUE.
#' @param h smoothing bandwidth.
#' @param r assumed smoothness order.
#' @return list: beta, rate_exponent, limit_distribution,
#'   standard_errors_valid.
#' @references Horowitz, Ch. 3.
#' @examples
#' X <- matrix(rnorm(200), ncol = 2)
#' morie_maximum_score(X, as.numeric(X %*% c(1, -0.8) > 0))$rate_exponent
#' @export
morie_maximum_score <- function(X, y, smoothed = FALSE, h = NULL, r = 2L) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  if (nrow(X) != length(y)) X <- t(X)
  if (nrow(X) != length(y)) stop("X must have one row per y.", call. = FALSE)
  if (!all(y %in% c(0, 1))) stop("y must be binary 0/1.", call. = FALSE)
  n <- nrow(X)
  d <- ncol(X)
  if (d < 2L) stop("need at least 2 covariates.", call. = FALSE)
  s <- 2 * y - 1
  if (smoothed) {
    hh <- if (is.null(h)) n^(-1 / (2 * as.integer(r) + 1)) else as.numeric(h)
    neg <- function(rest) -mean(s * stats::pnorm(as.numeric(X %*% c(1, rest)) / hh))
    res <- if (d == 2L) {
      o <- stats::optimize(function(b) neg(b), interval = c(-20, 20), tol = 1e-10)
      list(par = o$minimum, value = o$objective)
    } else {
      stats::optim(rep(0, d - 1L), neg, method = "BFGS")
    }
    return(list(beta = c(1, res$par), objective = -res$value, bandwidth = hh,
                rate_exponent = -r / (2 * r + 1),
                limit_distribution = "normal", standard_errors_valid = TRUE,
                n = n, d = d,
                method = "Smoothed max score; normality restored"))
  }
  neg <- function(rest) -sum(s * (as.numeric(X %*% c(1, rest)) > 0))
  best <- NULL
  bestv <- Inf
  set.seed(1)
  if (d == 2L) {
    # scalar free parameter: the objective is a step function, so scan
    # a dense grid and polish, rather than using Nelder-Mead, which R
    # warns is unreliable in one dimension
    gridb <- seq(-20, 20, length.out = 4001)
    vals <- vapply(gridb, neg, 0)
    best <- gridb[which.min(vals)]
    bestv <- min(vals)
  } else {
    for (i in 0:8) {
      st <- if (i == 0) rep(0, d - 1L) else stats::rnorm(d - 1L)
      rr <- stats::optim(st, neg, method = "Nelder-Mead",
                         control = list(maxit = 3000))
      if (rr$value < bestv) {
        bestv <- rr$value
        best <- rr$par
      }
    }
  }
  list(beta = c(1, best), score = -bestv, rate_exponent = -1 / 3,
       limit_distribution = "Chernoff, non-normal",
       standard_errors_valid = FALSE, n = n, d = d,
       method = "Manski max score; median restriction only, n^{-1/3}")
}

#' Robinson's partially linear model
#'
#' \eqn{Y = X'\beta + g(Z) + \epsilon}, with beta from partialling out
#' \eqn{E\[X|Z\]} and \eqn{E\[Y|Z\]} by kernel regression. beta stays
#' root-n even though g converges slowly -- the slower nuisance rate
#' does not contaminate the parametric one. Mirrors
#' \code{morie.fn.hrzplr}.
#'
#' @param X numeric parametric regressors.
#' @param Z numeric nonparametric covariate.
#' @param y numeric response.
#' @param h bandwidth.
#' @return list: beta, se, residuals, root_n, n, p.
#' @references Horowitz, Ch. 2 (Robinson 1988).
#' @examples
#' Z <- runif(200); X <- matrix(rnorm(400), ncol = 2)
#' morie_partially_linear(X, Z, X %*% c(1.5, -0.7) + sin(2 * Z))$root_n
#' @export
morie_partially_linear <- function(X, Z, y, h = NULL) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  Z <- as.numeric(Z)
  if (nrow(X) != length(y)) X <- t(X)
  if (nrow(X) != length(y) || length(Z) != length(y)) {
    stop("X, Z and y must match.", call. = FALSE)
  }
  n <- nrow(X)
  p <- ncol(X)
  hh <- if (is.null(h)) .hrz_silverman(Z) else as.numeric(h)
  my <- morie_local_linear(Z, y, grid = Z, h = hh)$fitted
  Xt <- X
  for (j in seq_len(p)) {
    Xt[, j] <- X[, j] - morie_local_linear(Z, X[, j], grid = Z, h = hh)$fitted
  }
  yt <- y - my
  ok <- is.finite(yt) & apply(is.finite(Xt), 1, all)
  if (sum(ok) <= p) stop("too few usable observations.", call. = FALSE)
  A <- t(Xt[ok, , drop = FALSE]) %*% Xt[ok, , drop = FALSE]
  beta <- as.numeric(solve(A, t(Xt[ok, , drop = FALSE]) %*% yt[ok]))
  resid <- yt[ok] - Xt[ok, , drop = FALSE] %*% beta
  s2 <- sum(resid^2) / max(sum(ok) - p, 1)
  list(beta = beta, se = sqrt(diag(s2 * solve(A))), residuals = as.numeric(resid),
       bandwidth = hh, root_n = TRUE, n = n, p = p,
       method = "Robinson partialling-out; beta root-n despite slow g")
}

#' Backfitting for additive models
#'
#' Iterates \eqn{g_j \leftarrow S_j(Y - \mu - \sum_{k \ne j} g_k)}.
#' Additivity buys back the one-dimensional \eqn{n^{-2/5}} rate in any
#' dimension. Components are centred every sweep because only their
#' SUM is identified. Mirrors \code{morie.fn.hrzbkft}.
#'
#' @param X numeric covariate matrix.
#' @param y numeric response.
#' @param h bandwidth.
#' @param max_iter sweeps.
#' @param tol convergence tolerance.
#' @return list: mu, components, fitted, n_iter, converged.
#' @references Horowitz, Ch. 2.
#' @examples
#' X <- matrix(runif(200), ncol = 2)
#' morie_backfitting(X, X[, 1]^2 + sin(3 * X[, 2]))$converged
#' @export
morie_backfitting <- function(X, y, h = NULL, max_iter = 50L, tol = 1e-6) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  if (nrow(X) != length(y)) X <- t(X)
  if (nrow(X) != length(y)) stop("X must have one row per y.", call. = FALSE)
  n <- nrow(X)
  d <- ncol(X)
  mu <- mean(y)
  G <- matrix(0, n, d)
  conv <- FALSE
  it <- 0L
  for (it in seq_len(as.integer(max_iter))) {
    prev <- G
    for (j in seq_len(d)) {
      partial <- y - mu - (rowSums(G) - G[, j])
      hj <- if (is.null(h)) .hrz_silverman(X[, j]) else as.numeric(h)
      gj <- morie_local_linear(X[, j], partial, grid = X[, j], h = hj)$fitted
      gj[!is.finite(gj)] <- 0
      G[, j] <- gj - mean(gj) # only the SUM is identified
    }
    if (max(abs(G - prev)) < tol) {
      conv <- TRUE
      break
    }
  }
  list(mu = mu, components = G, fitted = mu + rowSums(G), n_iter = it,
       converged = conv, rate_exponent = -0.4, d = d, n = n,
       method = "Backfitting; additivity restores n^{-2/5} in any d")
}

#' Conditional quantile by inverting a kernel CDF
#'
#' \eqn{\hat q_\tau(x) = \inf\{y: \hat F(y|x) \ge \tau\}}. All tau
#' come from ONE conditional CDF, so the estimate is automatically
#' monotone in tau -- the crossing that afflicts independently fitted
#' quantiles cannot arise. Mirrors \code{morie.fn.hrzkqre}.
#'
#' @param x,y numeric regressor and response.
#' @param tau quantile level(s) in (0, 1).
#' @param grid evaluation points.
#' @param h bandwidth.
#' @return list: grid, quantile, tau, bandwidth, monotone_in_tau.
#' @references Horowitz, Ch. 3.
#' @examples
#' morie_kernel_quantile(runif(200), rnorm(200), tau = c(0.1, 0.9))$monotone_in_tau
#' @export
morie_kernel_quantile <- function(x, y, tau = 0.5, grid = NULL, h = NULL) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  if (length(x) != length(y)) stop("x and y must match.", call. = FALSE)
  taus <- as.numeric(tau)
  if (any(taus <= 0 | taus >= 1)) stop("tau must lie in (0, 1).", call. = FALSE)
  hh <- if (is.null(h)) .hrz_silverman(x) else as.numeric(h)
  if (hh <= 0) stop("bandwidth must be positive.", call. = FALSE)
  g <- if (is.null(grid)) seq(min(x), max(x), length.out = 100L) else as.numeric(grid)
  ord <- order(y)
  ys <- y[ord]
  xs <- x[ord]
  out <- matrix(NA_real_, length(g), length(taus))
  for (i in seq_along(g)) {
    w <- .hrz_gauss_kernel((g[i] - xs) / hh)
    tot <- sum(w)
    if (tot <= 0) next
    cdf <- cumsum(w) / tot
    for (j in seq_along(taus)) {
      k <- which(cdf >= taus[j])[1]
      out[i, j] <- ys[if (is.na(k)) length(ys) else k]
    }
  }
  list(grid = g, quantile = if (length(taus) == 1L) as.numeric(out) else out,
       tau = taus, bandwidth = hh, monotone_in_tau = TRUE,
       method = "Invert a kernel conditional CDF; no quantile crossing")
}

#' Measure a convergence exponent
#'
#' Fits \eqn{\log error = c + \gamma \log n} and compares gamma with
#' the theoretical exponent. Rate theorems have no single-sample
#' value, so the measured exponent makes the claim checkable rather
#' than asserted. Mirrors
#' \code{morie.fn._horowitz.check_rate}.
#'
#' @param errors numeric errors.
#' @param n_grid matching sample sizes.
#' @param expected_exponent the theoretical rate.
#' @return list: observed_exponent, expected_exponent, consistent.
#' @references Horowitz, Ch. 2.
#' @examples
#' n <- c(100, 400, 1600, 6400)
#' morie_rate_check(3 * n^-0.4, n, -0.4)$observed_exponent
#' @export
morie_rate_check <- function(errors, n_grid, expected_exponent) {
  e <- as.numeric(errors)
  n <- as.numeric(n_grid)
  if (length(e) != length(n) || length(e) < 3L) {
    stop("need at least 3 matched (error, n) pairs.", call. = FALSE)
  }
  if (any(e <= 0) || any(n <= 0)) {
    stop("errors and sample sizes must be positive.", call. = FALSE)
  }
  fit <- stats::lm(log(e) ~ log(n))
  slope <- unname(stats::coef(fit)[2])
  list(observed_exponent = slope, expected_exponent = expected_exponent,
       intercept = unname(stats::coef(fit)[1]),
       consistent = abs(slope - expected_exponent) < 0.15,
       method = "log-log slope of error against n")
}
