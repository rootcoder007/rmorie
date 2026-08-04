# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Gaussian-process and hierarchical-forecasting shelf -- R mirror of the
# Python modules gpreg, gpregF, gpvarF, gprsk, sfcret (Rasmussen &
# Williams 2006) and naivef, snaivf, botUp, topDn, middle (Hyndman &
# Athanasopoulos, FPP 3rd ed.).
#
# Sources consulted, not recalled:
#   Rasmussen, C.E. & Williams, C.K.I. (2006). Gaussian Processes for
#   Machine Learning.  MIT Press.  Eq. (2.23)-(2.26), (2.30); Sec. 2.7;
#   the squared-exponential covariance on p. 19.  Read from the free
#   official PDF at gaussianprocess.org/gpml.
#   Hyndman, R.J. & Athanasopoulos, G.  Forecasting: Principles and
#   Practice, 3rd ed.  OTexts.  Sections 5.2 and 11.3, read at
#   otexts.com/fpp3.
#
# All closed form -- one linear solve, no iteration -- so this arm
# reproduces the Python arm to machine precision.
#
# Collision scan: b2gpts.R and all ten exported names were free in both
# R trees and in _lazy_map.json at the time of writing.

.b2se <- function(spec = NULL) {
  if (is.function(spec)) return(function(a, b) as.numeric(spec(a, b)))
  pars <- if (is.null(spec)) c(1, 1) else as.numeric(spec)
  if (length(pars) != 2L) stop("kernel must be (sf, l) or a function", call. = FALSE)
  sf <- pars[1]
  ell <- pars[2]
  if (!(ell > 0)) stop("length-scale must be positive", call. = FALSE)
  function(a, b) sf * sf * exp(-sum((as.numeric(a) - as.numeric(b))^2) / (2 * ell * ell))
}

.b2gram <- function(A, B, kf) {
  out <- matrix(0, nrow(A), nrow(B))
  for (i in seq_len(nrow(A))) {
    for (j in seq_len(nrow(B))) out[i, j] <- kf(A[i, ], B[j, ])
  }
  out
}

#' Gaussian-process regression with a squared-exponential kernel
#'
#' \deqn{\bar{f}_* = K_*(K+\sigma_n^2 I)^{-1}y,\quad
#'       V_* = k_{**} - K_*(K+\sigma_n^2 I)^{-1}K_*^\top}
#' Rasmussen & Williams (2006), eq. (2.23)-(2.24), p. 16; the
#' squared-exponential covariance on p. 19.  One linear solve, no
#' iteration.
#'
#' @param X Training inputs, `n x d`.
#' @param y Training targets.
#' @param X_test Test inputs, `m x d`.
#' @param kernel `c(sf, l)` (default `c(1, 1)`) or a function `k(x1, x2)`.
#' @param noise Observation noise standard deviation.
#' @return Named list with `estimate`, `variance`, `loglik`, `n`, `method`.
#' @references Rasmussen & Williams (2006), eq. (2.23)-(2.24), (2.30).
#' @examples
#' Gpreg(matrix(c(0, 1, 2), 3, 1), c(0, 1, 0.5), matrix(c(0.5), 1, 1))$estimate
#' @export
Gpreg <- function(X, y, X_test, kernel = NULL, noise = 0) {
  A <- .b2mat(X)
  B <- .b2mat(X_test)
  yv <- as.numeric(y)
  n <- nrow(A)
  if (length(yv) != n) stop("y must have one entry per row of X", call. = FALSE)
  kf <- .b2se(kernel)
  K <- .b2gram(A, A, kf) + diag(as.numeric(noise)^2, n)
  Ks <- .b2gram(B, A, kf)
  alpha <- solve(K, yv)
  mean <- as.numeric(Ks %*% alpha)
  W <- solve(K, t(Ks))
  var <- vapply(seq_len(nrow(B)), function(p) kf(B[p, ], B[p, ]) - sum(Ks[p, ] * W[, p]), numeric(1))
  ll <- -0.5 * sum(yv * alpha) - 0.5 * determinant(K, logarithm = TRUE)$modulus[1] -
    0.5 * n * log(2 * pi)
  list(estimate = mean, variance = var, loglik = as.numeric(ll), n = n,
       method = "GP regression, SE kernel -- Rasmussen & Williams (2006) eq. (2.23)-(2.24)")
}

#' Posterior mean of a Gaussian process at new inputs
#'
#' \deqn{\bar{f}_* = K_*(K+\sigma_n^2 I)^{-1}y}
#' Rasmussen & Williams (2006), eq. (2.23), (2.25).
#'
#' @param X Training inputs, `n x d`.
#' @param y Training targets.
#' @param X_star Test inputs, `m x d`.
#' @param kernel `c(sf, l)` or a function `k(x1, x2)`.
#' @param noise Observation noise standard deviation.
#' @return Named list with `estimate`, `weights`, `n`, `method`.
#' @references Rasmussen & Williams (2006), eq. (2.25).
#' @export
Gppost <- function(X, y, X_star, kernel = NULL, noise = 0) {
  A <- .b2mat(X)
  B <- .b2mat(X_star)
  yv <- as.numeric(y)
  n <- nrow(A)
  if (length(yv) != n) stop("y must have one entry per row of X", call. = FALSE)
  kf <- .b2se(kernel)
  K <- .b2gram(A, A, kf) + diag(as.numeric(noise)^2, n)
  alpha <- solve(K, yv)
  list(estimate = as.numeric(.b2gram(B, A, kf) %*% alpha), weights = alpha, n = n,
       method = "GP posterior mean K_*(K+sn^2 I)^-1 y -- Rasmussen & Williams (2006) eq. (2.25)")
}

#' Predictive variance of a Gaussian process
#'
#' \deqn{V_* = k_{**} - K_*(K+\sigma_n^2 I)^{-1}K_*^\top}
#' Rasmussen & Williams (2006), eq. (2.24), (2.26).  As the book notes,
#' this does not depend on the observed targets at all.
#'
#' @param X Training inputs, `n x d`.
#' @param X_star Test inputs, `m x d`.
#' @param kernel `c(sf, l)` or a function `k(x1, x2)`.
#' @param sigma2 Observation noise VARIANCE (not the sd; the name
#'   follows the book).
#' @return Named list with `estimate`, `prior`, `n`, `method`.
#' @references Rasmussen & Williams (2006), eq. (2.24).
#' @export
Gpvar <- function(X, X_star, kernel = NULL, sigma2 = 0) {
  A <- .b2mat(X)
  B <- .b2mat(X_star)
  n <- nrow(A)
  kf <- .b2se(kernel)
  K <- .b2gram(A, A, kf) + diag(as.numeric(sigma2), n)
  Ks <- .b2gram(B, A, kf)
  W <- solve(K, t(Ks))
  prior <- vapply(seq_len(nrow(B)), function(p) kf(B[p, ], B[p, ]), numeric(1))
  var <- vapply(seq_len(nrow(B)), function(p) prior[p] - sum(Ks[p, ] * W[, p]), numeric(1))
  list(estimate = var, prior = prior, n = n,
       method = "GP predictive variance -- Rasmussen & Williams (2006) eq. (2.24)")
}

#' Gaussian process fitted to the residuals of a parametric fit
#'
#' Rasmussen & Williams (2006), Sec. 2.7 (the explicit-basis model): a
#' fixed mean function is subtracted and the GP models what is left.
#' Here the parametric fit is supplied rather than estimated, which is
#' the form the residual-kriging literature uses.
#'
#' @param X Inputs, `n x d`.
#' @param y Observed targets.
#' @param y_pred Parametric fitted values at the same inputs.
#' @param kernel `c(sf, l)` or a function `k(x1, x2)`.
#' @param noise Observation noise standard deviation.
#' @return Named list with `estimate`, `residual`, `fitted`, `loglik`,
#'   `n`, `method`.
#' @references Rasmussen & Williams (2006), Sec. 2.7.
#' @export
Gpresid <- function(X, y, y_pred, kernel = NULL, noise = 0) {
  A <- .b2mat(X)
  yv <- as.numeric(y)
  pv <- as.numeric(y_pred)
  n <- nrow(A)
  if (length(yv) != n || length(pv) != n) {
    stop("y and y_pred must have one entry per row of X", call. = FALSE)
  }
  r <- yv - pv
  kf <- .b2se(kernel)
  K <- .b2gram(A, A, kf) + diag(as.numeric(noise)^2, n)
  alpha <- solve(K, r)
  smooth <- as.numeric(.b2gram(A, A, kf) %*% alpha)
  ll <- -0.5 * sum(r * alpha) - 0.5 * determinant(K, logarithm = TRUE)$modulus[1] -
    0.5 * n * log(2 * pi)
  list(estimate = smooth, residual = r, fitted = pv + smooth,
       loglik = as.numeric(ll), n = n,
       method = "GP on parametric residuals -- Rasmussen & Williams (2006) Sec. 2.7")
}

#' Interpolate a scattered surface onto a grid by GP regression
#'
#' Rasmussen & Williams (2006), eq. (2.23)-(2.24).  Simple kriging with
#' a known covariance and zero mean is algebraically the same predictor
#' as the GP posterior mean, so `method` selects the reported label, not
#' a different computation.
#'
#' @param coords Observation locations, `n x d`.
#' @param values Observed values.
#' @param grid Target locations, `m x d`.
#' @param method `"gp"` or `"kriging"`; identical computation.
#' @param kernel `c(sf, l)` or a function `k(x1, x2)`.
#' @param noise Nugget standard deviation.
#' @return Named list with `estimate`, `variance`, `method_used`, `n`,
#'   `method`.
#' @references Rasmussen & Williams (2006), eq. (2.23)-(2.24).
#' @export
Srfintp <- function(coords, values, grid, method = "gp", kernel = NULL, noise = 0) {
  if (!(method %in% c("gp", "kriging"))) stop("method must be 'gp' or 'kriging'", call. = FALSE)
  g <- Gpreg(coords, values, grid, kernel, noise)
  list(estimate = g$estimate, variance = g$variance, method_used = method,
       n = g$n,
       method = "Surface interpolation by GP posterior mean -- Rasmussen & Williams (2006) eq. (2.23)")
}

#' Naive forecast
#'
#' \deqn{\hat{y}_{T+h|T} = y_T}
#' Hyndman & Athanasopoulos, FPP 3rd ed., Sec. 5.2: "we simply set all
#' forecasts to be the value of the last observation".
#'
#' @param y Observed series.
#' @param h Forecast horizon.
#' @return Named list with `estimate`, `last`, `residual_sd`, `n`, `method`.
#' @references Hyndman & Athanasopoulos, FPP3, Sec. 5.2.
#' @examples
#' Naivefc(c(1, 2, 3, 5), 3)$estimate
#' @export
Naivefc <- function(y, h = 1L) {
  v <- as.numeric(y)
  n <- length(v)
  hh <- as.integer(h)
  if (n < 1L) stop("y must be non-empty", call. = FALSE)
  if (hh < 1L) stop("h must be at least 1", call. = FALSE)
  res <- diff(v)
  list(estimate = rep(v[n], hh), last = v[n],
       residual_sd = if (length(res) > 1L) sd(res) else NaN, n = n,
       method = "Naive forecast yhat_{T+h|T} = y_T -- Hyndman & Athanasopoulos, FPP3 Sec. 5.2")
}

#' Seasonal naive forecast
#'
#' \deqn{\hat{y}_{T+h|T} = y_{T+h-m(k+1)},\ k=\lfloor (h-1)/m \rfloor}
#' Hyndman & Athanasopoulos, FPP 3rd ed., Sec. 5.2.
#'
#' @param y Observed series.
#' @param m Seasonal period.
#' @param h Forecast horizon.
#' @return Named list with `estimate`, `m`, `residual_sd`, `n`, `method`.
#' @references Hyndman & Athanasopoulos, FPP3, Sec. 5.2.
#' @export
Snaivefc <- function(y, m, h = 1L) {
  v <- as.numeric(y)
  n <- length(v)
  mm <- as.integer(m)
  hh <- as.integer(h)
  if (mm < 1L) stop("m must be at least 1", call. = FALSE)
  if (hh < 1L) stop("h must be at least 1", call. = FALSE)
  if (n < mm) stop("y must be at least one full season long", call. = FALSE)
  fc <- vapply(seq_len(hh), function(step) {
    k <- (step - 1L) %/% mm
    v[n + step - mm * (k + 1L)]
  }, numeric(1))
  res <- v[(mm + 1L):n] - v[1L:(n - mm)]
  list(estimate = fc, m = mm,
       residual_sd = if (length(res) > 1L) sd(res) else NaN, n = n,
       method = "Seasonal naive y_{T+h-m(k+1)} -- Hyndman & Athanasopoulos, FPP3 Sec. 5.2")
}

#' Bottom-up reconciliation of a hierarchical forecast
#'
#' \deqn{\tilde{y} = S\hat{y}_{\mathrm{bottom}}}
#' Hyndman & Athanasopoulos, FPP 3rd ed., Sec. 11.3.
#'
#' @param bottoms Bottom-level forecasts, length `m`.
#' @param S Summing matrix, `k x m`.
#' @return Named list with `estimate`, `total`, `bottom`, `n`, `method`.
#' @references Hyndman & Athanasopoulos, FPP3, Sec. 11.3.
#' @export
Bottomup <- function(bottoms, S) {
  bv <- as.numeric(bottoms)
  Sm <- .b2mat(S)
  if (ncol(Sm) != length(bv)) stop("S must have one column per bottom-level series", call. = FALSE)
  list(estimate = as.numeric(Sm %*% bv), total = sum(bv), bottom = bv, n = nrow(Sm),
       method = "Bottom-up reconciliation S yhat -- Hyndman & Athanasopoulos, FPP3 Sec. 11.3")
}

#' Top-down disaggregation of a hierarchical forecast
#'
#' \deqn{\tilde{y}_j = p_j\hat{y}}
#' Hyndman & Athanasopoulos, FPP 3rd ed., Sec. 11.3.  The proportions
#' are closed to sum 1 so the parts add back to the total; the raw sum
#' is reported so a caller who supplied unnormalised proportions sees it.
#'
#' @param top Forecast of the Total series.
#' @param props Non-negative disaggregation proportions.
#' @return Named list with `estimate`, `props`, `prop_sum`, `total`,
#'   `n`, `method`.
#' @references Hyndman & Athanasopoulos, FPP3, Sec. 11.3.
#' @export
Topdown <- function(top, props) {
  t <- as.numeric(top)
  pv <- as.numeric(props)
  if (any(pv < 0)) stop("proportions must be non-negative", call. = FALSE)
  raw <- sum(pv)
  if (!(raw > 0)) stop("proportions must have positive total", call. = FALSE)
  p <- pv / raw
  list(estimate = t * p, props = p, prop_sum = raw, total = t, n = length(p),
       method = "Top-down disaggregation p_j yhat -- Hyndman & Athanasopoulos, FPP3 Sec. 11.3")
}

#' Middle-out reconciliation of a hierarchical forecast
#'
#' Bottom-up above the chosen middle level, top-down below it (Hyndman &
#' Athanasopoulos, FPP 3rd ed., Sec. 11.3).  Both directions are the one
#' product `S x` once `S` carries 0/1 rows above and proportion rows
#' below; rows are classified on that basis and reported separately so
#' the reading stays checkable.
#'
#' @param middle Forecasts at the middle level, length `m`.
#' @param S Structure matrix, `k x m`.
#' @return Named list with `estimate`, `aggregated`, `disaggregated`,
#'   `middle`, `n`, `method`.
#' @references Hyndman & Athanasopoulos, FPP3, Sec. 11.3.
#' @export
Middleout <- function(middle, S) {
  mv <- as.numeric(middle)
  Sm <- .b2mat(S)
  if (ncol(Sm) != length(mv)) stop("S must have one column per middle-level series", call. = FALSE)
  out <- as.numeric(Sm %*% mv)
  isagg <- apply(Sm, 1, function(r) all(r == 0 | r == 1))
  list(estimate = out, aggregated = out[isagg], disaggregated = out[!isagg],
       middle = mv, n = nrow(Sm),
       method = "Middle-out reconciliation -- Hyndman & Athanasopoulos, FPP3 Sec. 11.3")
}

# CANONICAL TEST
# stopifnot(identical(Naivefc(c(1, 2, 3, 5), 3)$estimate, rep(5, 3)))
# stopifnot(all(abs(Snaivefc(1:6, 2, 5)$estimate - c(5, 6, 5, 6, 5)) < 1e-12))
