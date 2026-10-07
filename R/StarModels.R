# SPDX-License-Identifier: AGPL-3.0-or-later
# Space-time autoregressive moving-average (STARMA) models of Pfeifer and Deutsch (1980).
# Identical to the Python arm morie.fn.starima.

#' Space-time ARMA models: STACF, STPACF, STAR and STARMA fits, forecasts, portmanteau test
#'
#' Pfeifer and Deutsch (1980) space-time models on a \code{T x N} field
#' (rows are times, columns sites) with spatial weight matrices of orders
#' 1, 2, ... in \code{weights} (order 0 is the identity).
#' \code{StLag}: \code{W^(order) z}. \code{StAcf}: space-time
#' autocorrelation \code{rho_l0(s) = gamma_l0(s) / sqrt(gamma_ll(0) gamma_00(0))}
#' with \code{gamma_lh(s) = (1 / (N (T - s))) sum_t t(W^l z_t) (W^h z_(t+s))},
#' returned as \code{acf[s + 1, l + 1]}. \code{StPacf}: space-time partial
#' autocorrelation from the space-time Yule-Walker equations, one system per
#' time order \code{k}, reported as \code{pacf[k, l + 1]}. \code{StarFit}:
#' STAR model \code{z_t = sum_k sum_(l in ar[[k]]) phi_kl W^(l) z_(t-k) + e_t}
#' by least squares on the \code{N (T - p)} stacked observations.
#' \code{StarmaFit}: STARMA model (moving-average term subtracted, as in the
#' paper) by conditional least squares, L-BFGS-B with central-difference
#' gradients from the STAR start. \code{StarmaForecast}: \code{h}-step
#' forecasts with zero future shocks. \code{StPortmanteau}:
#' \code{Q = N T sum_s sum_l rho_l0(s)^2} of the residuals against
#' \code{chi^2} on \code{S (L + 1) - n_params} degrees of freedom.
#' Fields are centred by their grand mean when \code{center}.
#'
#' @param z Numeric site vector.
#' @param weights List of spatial weight matrices of orders 1, 2, ....
#' @param order Spatial order (0 returns \code{z}).
#' @param x Numeric \code{T x N} matrix, rows are times.
#' @param max_lag_t Largest time lag.
#' @param max_lag_s Largest spatial order (default all of \code{weights}).
#' @param center Centre by the grand mean.
#' @param method \code{"pfeifer"} (time-nested Yule-Walker systems, the published
#'   definition) or \code{"starma"} (systems nested lexicographically in time lag
#'   and spatial order, as the starma package).
#' @param ar List of integer vectors: spatial orders per autoregressive time lag.
#' @param ma List of integer vectors: spatial orders per moving-average time lag.
#' @param intercept Include an intercept in the STAR least squares.
#' @param max_iter Optimiser iterations.
#' @param fit Result of \code{StarFit} or \code{StarmaFit}.
#' @param h Forecast horizon.
#' @param residuals Residual \code{T x N} matrix.
#' @param n_params Number of fitted parameters (portmanteau degrees of freedom).
#' @return A list; see each function.
#' @references Pfeifer, P. E. and Deutsch, S. J. (1980). A three-stage
#'   iterative procedure for space-time modeling. Technometrics 22, 35-47.
#' @examples
#' W <- list(matrix(c(0, 1, 1, 0), 2))
#' x <- rbind(c(1, 2), c(2, 3), c(4, 3), c(3, 1), c(2, 2))
#' StAcf(x, W, max_lag_t = 1)$acf
#' StarFit(x, W, list(c(0, 1)))$coefficients
#' @export
StLag <- function(z, weights, order) {
  z <- as.numeric(z)
  if (order == 0) return(z)
  as.numeric(weights[[order]] %*% z)
}

.st_center <- function(x, center) {
  x <- unname(as.matrix(x)) * 1
  if (center) x - mean(x) else x
}

.st_lagged <- function(z, weights, L) {
  lapply(0:L, function(l) t(apply(z, 1, StLag, weights = weights, order = l)))
}

.st_cov <- function(lagged, L, S) {
  T_ <- nrow(lagged[[1]])
  N <- ncol(lagged[[1]])
  g <- array(0, c(L + 1, L + 1, S + 1))
  for (l in 0:L) for (h in 0:L) for (s in 0:S) {
    tot <- 0
    for (t in seq_len(T_ - s)) tot <- tot + sum(lagged[[l + 1]][t, ] * lagged[[h + 1]][t + s, ])
    g[l + 1, h + 1, s + 1] <- tot / (N * (T_ - s))
  }
  g
}

#' @rdname StLag
#' @export
StAcf <- function(x, weights, max_lag_t = 10, max_lag_s = NULL, center = TRUE) {
  z <- .st_center(x, center)
  L <- if (is.null(max_lag_s)) length(weights) else as.integer(max_lag_s)
  lagged <- .st_lagged(z, weights, L)
  g <- .st_cov(lagged, L, max_lag_t)
  acf <- matrix(0, max_lag_t + 1, L + 1)
  for (s in 0:max_lag_t) for (l in 0:L) {
    acf[s + 1, l + 1] <- g[l + 1, 1, s + 1] / sqrt(g[l + 1, l + 1, 1] * g[1, 1, 1])
  }
  list(acf = acf, gamma = g, n_sites = ncol(z), n_times = nrow(z))
}

#' @rdname StLag
#' @export
StPacf <- function(x, weights, max_lag_t = 5, max_lag_s = NULL, center = TRUE, method = "pfeifer") {
  z <- .st_center(x, center)
  L <- if (is.null(max_lag_s)) length(weights) else as.integer(max_lag_s)
  lagged <- .st_lagged(z, weights, L)
  g <- .st_cov(lagged, L, max_lag_t)
  gam <- function(l, h, s) if (s >= 0) g[l + 1, h + 1, s + 1] else g[h + 1, l + 1, -s + 1]
  last_coef <- function(idx) {
    # Yule-Walker equations indexed like the unknowns: rows (s, l), columns (j, h)
    n <- nrow(idx)
    A <- matrix(0, n, n)
    b <- numeric(n)
    for (r in seq_len(n)) {
      for (cc in seq_len(n)) A[r, cc] <- gam(idx[r, 2], idx[cc, 2], idx[r, 1] - idx[cc, 1])
      b[r] <- gam(idx[r, 2], 0, idx[r, 1])
    }
    solve(A, b)[n]
  }
  pacf <- matrix(0, max_lag_t, L + 1)
  for (k in seq_len(max_lag_t)) {
    for (h_last in 0:L) {
      if (method == "starma") {
        idx <- rbind(if (k > 1) as.matrix(expand.grid(h = 0:L, j = seq_len(k - 1))[, c("j", "h")]),
                     cbind(k, 0:h_last))
      } else {
        full <- as.matrix(expand.grid(h = 0:L, j = seq_len(k))[, c("j", "h")])
        idx <- rbind(full[!(full[, 1] == k & full[, 2] == h_last), , drop = FALSE], c(k, h_last))
      }
      pacf[k, h_last + 1] <- last_coef(unname(idx))
    }
  }
  list(pacf = pacf, gamma = g, method = method)
}

.st_cols <- function(spec) {
  out <- NULL
  for (k in seq_along(spec)) for (l in spec[[k]]) out <- rbind(out, c(k, l))
  out
}

.st_design <- function(lagged, ar, T_, N, intercept) {
  p <- length(ar)
  cols <- .st_cols(ar)
  X <- NULL
  y <- NULL
  for (t in (p + 1):T_) {
    blk <- matrix(0, N, if (is.null(cols)) 0 else nrow(cols))
    for (cc in seq_len(NROW(cols))) blk[, cc] <- lagged[[cols[cc, 2] + 1]][t - cols[cc, 1], ]
    if (intercept) blk <- cbind(1, blk)
    X <- rbind(X, blk)
    y <- c(y, lagged[[1]][t, ])
  }
  list(X = X, y = y, cols = cols)
}

.st_ols <- function(X, y) {
  q <- ncol(X)
  XtX <- crossprod(X)
  beta <- as.numeric(solve(XtX, crossprod(X, y)))
  resid <- as.numeric(y - X %*% beta)
  rss <- sum(resid^2)
  n <- length(y)
  inv <- solve(XtX)
  se <- sqrt(rss / (n - q) * diag(inv))
  list(beta = beta, se = se, resid = resid, rss = rss, n = n)
}

#' @rdname StLag
#' @export
StarFit <- function(x, weights, ar, intercept = FALSE, center = TRUE) {
  z <- .st_center(x, center)
  T_ <- nrow(z)
  N <- ncol(z)
  L <- max(c(0, unlist(ar)))
  lagged <- .st_lagged(z, weights, L)
  d <- .st_design(lagged, ar, T_, N, intercept)
  o <- .st_ols(d$X, d$y)
  off <- if (intercept) 1L else 0L
  q <- length(o$beta)
  s2 <- o$rss / o$n
  ll <- -0.5 * o$n * (log(2 * pi * s2) + 1)
  p <- length(ar)
  coef <- cbind(d$cols, o$beta[off + seq_len(NROW(d$cols))])
  se <- cbind(d$cols, o$se[off + seq_len(NROW(d$cols))])
  colnames(coef) <- colnames(se) <- c("k", "l", "value")
  list(coefficients = coef, se = se, intercept = if (intercept) o$beta[1] else 0,
       residuals = matrix(o$resid, T_ - p, N, byrow = TRUE), sigma2 = o$rss / (o$n - q),
       rss = o$rss, loglik = ll, aic = -2 * ll + 2 * q, bic = -2 * ll + q * log(o$n), n_obs = o$n, ar = ar)
}

.st_css_resid <- function(z, lagged, weights, ar, ma, phi, theta, T_, N, p) {
  e <- matrix(0, T_, N)
  ar_idx <- .st_cols(ar)
  ma_idx <- .st_cols(ma)
  for (t in (p + 1):T_) {
    pred <- numeric(N)
    for (a in seq_len(NROW(ar_idx))) pred <- pred + phi[a] * lagged[[ar_idx[a, 2] + 1]][t - ar_idx[a, 1], ]
    for (a in seq_len(NROW(ma_idx))) {
      if (t - ma_idx[a, 1] >= p + 1) pred <- pred - theta[a] * StLag(e[t - ma_idx[a, 1], ], weights, ma_idx[a, 2])
    }
    e[t, ] <- z[t, ] - pred
  }
  e
}

#' @rdname StLag
#' @export
StarmaFit <- function(x, weights, ar, ma, center = TRUE, max_iter = 500) {
  z <- .st_center(x, center)
  T_ <- nrow(z)
  N <- ncol(z)
  L <- max(c(0, unlist(ar), unlist(ma)))
  lagged <- .st_lagged(z, weights, L)
  p <- max(length(ar), length(ma))
  ar_idx <- .st_cols(ar)
  ma_idx <- .st_cols(ma)
  na <- NROW(ar_idx)
  nm <- NROW(ma_idx)
  start <- if (na > 0) StarFit(x, weights, ar, center = center)$coefficients[, 3] else numeric(0)
  x0 <- c(start, rep(0, nm))
  css <- function(v) {
    e <- .st_css_resid(z, lagged, weights, ar, ma, v[seq_len(na)], v[na + seq_len(nm)], T_, N, p)
    sum(e[(p + 1):T_, ]^2)
  }
  grad <- function(v) {
    out <- numeric(length(v))
    for (a in seq_along(v)) {
      h <- 1e-6 * max(1, abs(v[a]))
      vp <- v
      vm <- v
      vp[a] <- vp[a] + h
      vm[a] <- vm[a] - h
      out[a] <- (css(vp) - css(vm)) / (2 * h)
    }
    out
  }
  v <- if (length(x0)) as.numeric(LbfgsbMinimize(css, x0, grad = grad, pgtol = 1e-10, factr = 10, max_iter = max_iter)$x) else numeric(0)
  e <- .st_css_resid(z, lagged, weights, ar, ma, v[seq_len(na)], v[na + seq_len(nm)], T_, N, p)
  rss <- sum(e[(p + 1):T_, ]^2)
  n <- N * (T_ - p)
  q <- length(v)
  s2 <- rss / n
  ll <- -0.5 * n * (log(2 * pi * s2) + 1)
  phi <- if (na > 0) cbind(ar_idx, v[seq_len(na)]) else matrix(0, 0, 3)
  theta <- if (nm > 0) cbind(ma_idx, v[na + seq_len(nm)]) else matrix(0, 0, 3)
  colnames(phi) <- colnames(theta) <- c("k", "l", "value")
  list(phi = phi, theta = theta, residuals = e[(p + 1):T_, , drop = FALSE], sigma2 = rss / max(n - q, 1),
       rss = rss, loglik = ll, aic = -2 * ll + 2 * q, bic = -2 * ll + q * log(n), ar = ar, ma = ma,
       mean = if (center) mean(as.matrix(x)) else 0, n_obs = n)
}

#' @rdname StLag
#' @export
StarmaForecast <- function(fit, x, weights, h = 1) {
  .morie_arg(fit, "l")
  phi <- if (!is.null(fit$phi)) fit$phi else fit$coefficients
  theta <- if (!is.null(fit$theta)) fit$theta else matrix(0, 0, 3)
  ar <- fit$ar
  ma <- if (!is.null(fit$ma)) fit$ma else list()
  mn <- if (!is.null(fit$mean)) fit$mean else mean(as.matrix(x))
  z <- unname(as.matrix(x)) - mn
  T_ <- nrow(z)
  N <- ncol(z)
  p <- max(length(ar), length(ma))
  e <- rbind(matrix(0, p, N), unname(as.matrix(fit$residuals)))
  out <- matrix(0, h, N)
  for (step in seq_len(h)) {
    t <- T_ + step
    pred <- numeric(N)
    for (a in seq_len(NROW(phi))) pred <- pred + phi[a, 3] * StLag(z[t - phi[a, 1], ], weights, phi[a, 2])
    for (a in seq_len(NROW(theta))) {
      if (t - theta[a, 1] <= T_) pred <- pred - theta[a, 3] * StLag(e[t - theta[a, 1], ], weights, theta[a, 2])
    }
    z <- rbind(z, pred)
    e <- rbind(e, 0)
    out[step, ] <- pred + mn
  }
  list(forecast = out, h = h)
}

#' @rdname StLag
#' @export
StPortmanteau <- function(residuals, weights, max_lag_t = 5, max_lag_s = NULL, n_params = 0) {
  r <- StAcf(residuals, weights, max_lag_t = max_lag_t, max_lag_s = max_lag_s, center = FALSE)
  T_ <- nrow(residuals)
  N <- ncol(residuals)
  L <- ncol(r$acf) - 1
  Q <- N * T_ * sum(r$acf[1 + seq_len(max_lag_t), , drop = FALSE]^2)
  df <- max_lag_t * (L + 1) - n_params
  list(statistic = Q, df = df, p_value = if (df > 0) stats::pchisq(Q, df, lower.tail = FALSE) else NA_real_)
}
