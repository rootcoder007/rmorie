# SPDX-License-Identifier: AGPL-3.0-or-later
#
# GARCH extensions, cointegration and forecasting mirrors.
#
# Mirrors morie.fn._garch and morie.fn._coint. Deliberately does NOT
# duplicate what R already has: morie_garch_fit (R/garch.R),
# morie_egarch_model (R/egrch.R), morie_tgarch_model (R/tgrch.R),
# morie_eg_coint (R/coitg.R), morie_johansen_cointegration
# (R/johsn.R), morie_vecm (R/vecmf.R), and the CCC/DCC multivariate
# fits. This file adds the specifications those files do not cover.
#
# Specifications: Tsay (2010), Analysis of Financial Time Series,
# 3rd ed., Ch. 3 -- IGARCH Sec. 3.6 p. 140-141 (alpha pinned at
# 1 - beta), EGARCH Sec. 3.8 p. 143 eq. (3.24) (E|z| = sqrt(2/pi) for
# a Gaussian), TGARCH Sec. 3.9 p. 149 eq. (3.34). Forecasting methods
# follow Hyndman & Athanasopoulos, Forecasting: Principles and
# Practice, 3rd ed.

.morie_garch_specs <- c("garch", "igarch", "gjr", "aparch")

#' Conditional-variance recursion
#'
#' Mirrors \code{morie.fn._garch.garch_recursion}. Variance at t = 1 is
#' initialised at the sample variance.
#'
#' @param eps numeric mean-zero shocks.
#' @param params named list of parameters for the chosen spec.
#' @param spec one of "garch", "igarch", "gjr", "aparch".
#' @return numeric vector of conditional variances.
#' @references Tsay, R. S. (2010). Analysis of Financial Time Series
#'   (3rd ed.). Wiley. Ch. 3.
#' @examples
#' morie_garch_recursion(rnorm(50), list(omega = .05, alpha = .1, beta = .85))
#' @export
morie_garch_recursion <- function(eps, params, spec = "garch") {
  spec <- match.arg(spec, .morie_garch_specs)
  eps <- as.numeric(eps)
  n <- length(eps)
  if (n < 10L) stop("need at least 10 observations.", call. = FALSE)
  s2 <- numeric(n)
  # POPULATION variance for the burn-in, matching numpy's np.var. R's
  # stats::var uses the n - 1 denominator, which would make the
  # recursion differ from the Python core by a factor n/(n-1) from the
  # very first step.
  s2[1] <- max(mean((eps - mean(eps))^2), 1e-12)
  if (spec == "garch") {
    for (t in 2:n) {
      s2[t] <- params$omega + params$alpha * eps[t - 1]^2 + params$beta * s2[t - 1]
    }
  } else if (spec == "igarch") {
    # Tsay p.141: the unit root pins the ARCH weight at 1 - beta
    for (t in 2:n) {
      s2[t] <- params$omega + params$beta * s2[t - 1] +
        (1 - params$beta) * eps[t - 1]^2
    }
  } else if (spec == "gjr") {
    for (t in 2:n) {
      neg <- as.numeric(eps[t - 1] < 0)
      s2[t] <- params$omega + (params$alpha + params$gamma * neg) * eps[t - 1]^2 +
        params$beta * s2[t - 1]
    }
  } else {
    d <- params$delta
    sd <- numeric(n)
    sd[1] <- s2[1]^(d / 2)
    for (t in 2:n) {
      sd[t] <- params$omega +
        params$alpha * (abs(eps[t - 1]) - params$gamma * eps[t - 1])^d +
        params$beta * sd[t - 1]
    }
    s2 <- pmax(sd, 1e-300)^(2 / d)
  }
  pmax(s2, 1e-12)
}

.morie_garch_pack <- function(spec, x) {
  sig <- function(z) 1 / (1 + exp(-pmax(pmin(z, 30), -30)))
  if (spec == "igarch") {
    return(list(omega = exp(min(x[1], 5)), beta = sig(x[2]) * 0.999))
  }
  tot <- sig(x[2]) * 0.999
  fr <- sig(x[3])
  p <- list(omega = exp(min(x[1], 5)), alpha = tot * fr, beta = tot * (1 - fr))
  if (spec %in% c("gjr", "aparch")) p$gamma <- tanh(x[4]) * 0.5
  if (spec == "aparch") p$delta <- 0.5 + 2.5 * sig(x[5])
  p
}

#' Fit a conditional-variance model by Gaussian quasi-ML
#'
#' Mirrors \code{morie.fn._garch.garch_fit}. Extends
#' \code{\link{morie_garch_fit}} (plain GARCH(1,1)) to the integrated,
#' GJR-threshold and asymmetric-power specifications.
#'
#' @param r numeric return series, at least 50 observations.
#' @param spec one of "garch", "igarch", "gjr", "aparch".
#' @return list: params, sigma2, loglik, persistence, std_residuals,
#'   mu, forecast, spec, n.
#' @references Tsay, R. S. (2010). Ch. 3. Glosten, Jagannathan &
#'   Runkle (1993), J. Finance 48(5), 1779-1801. Ding, Granger & Engle
#'   (1993), J. Empirical Finance 1(1), 83-106.
#' @examples
#' set.seed(1)
#' morie_garch_spec_fit(rnorm(200), "gjr")$persistence
#' @export
morie_garch_spec_fit <- function(r, spec = "garch") {
  spec <- match.arg(spec, .morie_garch_specs)
  r <- as.numeric(r)
  n <- length(r)
  if (n < 50L) stop("need at least 50 observations to fit.", call. = FALSE)
  if (!all(is.finite(r))) stop("r must be finite.", call. = FALSE)
  mu <- mean(r)
  eps <- r - mu
  scale <- stats::sd(eps)
  if (scale <= 0) stop("r has zero variance.", call. = FALSE)
  e <- eps / scale

  x0 <- switch(spec,
    garch = c(-4, 2, -1.5), igarch = c(-6, 2),
    gjr = c(-4, 2, -1.5, 0.2), aparch = c(-4, 2, -1.5, 0.2, 0)
  )
  neg <- function(x) {
    p <- .morie_garch_pack(spec, x)
    s2 <- tryCatch(morie_garch_recursion(e, p, spec), error = function(err) NULL)
    if (is.null(s2) || any(!is.finite(s2)) || any(s2 <= 0)) {
      return(1e10)
    }
    ll <- -0.5 * sum(log(2 * pi * s2) + e^2 / s2)
    if (is.finite(ll)) -ll else 1e10
  }
  res <- stats::optim(x0, neg, method = "Nelder-Mead",
                      control = list(maxit = 6000, reltol = 1e-10))
  p <- .morie_garch_pack(spec, res$par)
  s2 <- morie_garch_recursion(e, p, spec) * scale^2
  pp <- p
  pp$omega <- if (spec == "aparch") p$omega * scale^p$delta else p$omega * scale^2

  pers <- switch(spec,
    garch = p$alpha + p$beta, igarch = 1,
    gjr = p$alpha + p$beta + 0.5 * p$gamma, aparch = p$alpha + p$beta
  )
  fc <- switch(spec,
    igarch = pp$omega + p$beta * s2[n] + (1 - p$beta) * eps[n]^2,
    gjr = pp$omega + (p$alpha + 0.5 * p$gamma) * eps[n]^2 + p$beta * s2[n],
    pp$omega + p$alpha * eps[n]^2 + p$beta * s2[n]
  )
  list(
    params = pp, sigma2 = s2, sigma = sqrt(s2), loglik = -res$value,
    persistence = as.numeric(pers), residuals = eps,
    std_residuals = eps / sqrt(s2), mu = mu, forecast = as.numeric(fc),
    spec = spec, n = as.integer(n),
    method = sprintf("%s(1,1) Gaussian quasi-ML (Tsay 2010 Ch. 3)", toupper(spec))
  )
}

#' Scalar BEKK(1,1) multivariate GARCH
#'
#' \eqn{H_t = C'C + a \epsilon_{t-1}\epsilon_{t-1}' + b H_{t-1}} with
#' variance targeting, so \eqn{H_t} is positive definite by
#' construction. Mirrors \code{morie.fn.mgrch}.
#'
#' @param R matrix (T x k), k >= 2.
#' @return list: H (T x k x k array), a, b, persistence, H_bar,
#'   loglik, T, k.
#' @references Engle, R. F. & Kroner, K. F. (1995). Multivariate
#'   simultaneous generalized ARCH. Econometric Theory, 11(1), 122-150.
#' @examples
#' set.seed(1)
#' morie_bekk_garch(matrix(rnorm(200), ncol = 2))$persistence
#' @export
morie_bekk_garch <- function(R) {
  R <- as.matrix(R)
  Tn <- nrow(R)
  k <- ncol(R)
  if (k < 2L) stop("BEKK needs at least 2 series.", call. = FALSE)
  if (Tn < 50L) stop("need at least 50 observations.", call. = FALSE)
  if (!all(is.finite(R))) stop("R must be finite.", call. = FALSE)
  E <- scale(R, center = TRUE, scale = FALSE)
  Hbar <- stats::cov(E)

  recur <- function(a, b) {
    Cm <- Hbar * (1 - a - b)
    H <- array(0, c(Tn, k, k))
    H[1, , ] <- Hbar
    for (t in 2:Tn) {
      ee <- tcrossprod(E[t - 1, ])
      H[t, , ] <- Cm + a * ee + b * H[t - 1, , ]
    }
    H
  }
  neg <- function(x) {
    a <- 0.999 / (1 + exp(-max(min(x[1], 30), -30)))
    b <- (0.999 - a) / (1 + exp(-max(min(x[2], 30), -30)))
    H <- recur(a, b)
    ll <- 0
    for (t in seq_len(Tn)) {
      L <- tryCatch(chol(H[t, , ]), error = function(e) NULL)
      if (is.null(L)) {
        return(1e10)
      }
      sol <- backsolve(L, E[t, ], transpose = TRUE)
      ll <- ll - 0.5 * (k * log(2 * pi) + 2 * sum(log(diag(L))) + sum(sol^2))
    }
    if (is.finite(ll)) -ll else 1e10
  }
  res <- stats::optim(c(-2, 2), neg, method = "Nelder-Mead",
                      control = list(maxit = 800))
  a <- 0.999 / (1 + exp(-max(min(res$par[1], 30), -30)))
  b <- (0.999 - a) / (1 + exp(-max(min(res$par[2], 30), -30)))
  list(
    H = recur(a, b), a = a, b = b, persistence = a + b, H_bar = Hbar,
    C = Hbar * (1 - a - b), loglik = -res$value,
    T = as.integer(Tn), k = as.integer(k),
    method = "Scalar BEKK(1,1) with variance targeting (Engle-Kroner 1995)"
  )
}

#' Value-at-Risk and expected shortfall of a one-step forecast
#'
#' Normal: \eqn{VaR = -(\mu + \sigma z_\alpha)} and
#' \eqn{ES = -\mu + \sigma \phi(z_\alpha)/\alpha}, both reported as
#' positive losses. Mirrors \code{morie.fn.volgvi} and
#' \code{morie.fn.volges}.
#'
#' @param mu conditional mean forecast.
#' @param sigma one-step volatility (standard deviation).
#' @param alpha tail probability in (0, 1).
#' @param dist "normal" or "t".
#' @param nu degrees of freedom for the t (> 2).
#' @return list: var, es, quantile, alpha, dist.
#' @references Artzner, P., Delbaen, F., Eber, J.-M. & Heath, D.
#'   (1999). Coherent measures of risk. Mathematical Finance, 9(3),
#'   203-228.
#' @examples
#' morie_garch_var_es(0, 1, 0.05)$var
#' @export
morie_garch_var_es <- function(mu, sigma, alpha = 0.05, dist = "normal", nu = 8) {
  if (!isTRUE(alpha > 0 && alpha < 1)) {
    stop("alpha must lie in (0, 1).", call. = FALSE)
  }
  if (!isTRUE(sigma > 0)) stop("sigma must be positive.", call. = FALSE)
  dist <- match.arg(dist, c("normal", "t"))
  if (dist == "normal") {
    z <- stats::qnorm(alpha)
    return(list(
      var = -(mu + sigma * z), es = -(mu - sigma * stats::dnorm(z) / alpha),
      quantile = z, alpha = alpha, dist = dist
    ))
  }
  if (nu <= 2) stop("t distribution needs nu > 2 for a finite variance.", call. = FALSE)
  s <- sqrt((nu - 2) / nu)
  z <- stats::qt(alpha, nu) * s
  pdf <- stats::dt(z / s, nu) / s
  es_std <- -pdf / alpha * (nu + (z / s)^2) / (nu - 1) * s
  list(
    var = -(mu + sigma * z), es = -(mu + sigma * es_std),
    quantile = z, alpha = alpha, dist = dist, nu = nu
  )
}

#' Holt's linear trend exponential smoothing
#'
#' Mirrors \code{morie.fn.joholt}. With \code{damped = TRUE} the trend
#' contribution is \eqn{\sum_{i\le h}\phi^i} rather than h, so long
#' horizons flatten instead of extrapolating a line forever.
#'
#' @param y numeric series, at least 5 observations.
#' @param alpha,beta smoothing parameters in (0, 1); estimated by
#'   one-step SSE when NULL.
#' @param horizon forecast horizon.
#' @param damped logical; damp the trend.
#' @param phi damping parameter in (0, 1\].
#' @return list: forecast, level, trend, fitted, residuals, sse,
#'   alpha, beta, damped, n.
#' @references Hyndman, R. J. & Athanasopoulos, G. (2021).
#'   Forecasting: Principles and Practice (3rd ed.). OTexts. Sec. 8.2.
#' @examples
#' morie_holt_linear(1:30 * 2, horizon = 3)$forecast
#' @export
morie_holt_linear <- function(y, alpha = NULL, beta = NULL, horizon = 1L,
                              damped = FALSE, phi = 0.98) {
  y <- as.numeric(y)
  n <- length(y)
  if (n < 5L) stop("need at least 5 observations.", call. = FALSE)
  if (!all(is.finite(y))) stop("y must be finite.", call. = FALSE)
  h <- as.integer(horizon)
  if (h < 1L) stop("horizon must be at least 1.", call. = FALSE)
  d <- if (damped) phi else 1

  run <- function(a, b) {
    lev <- tr <- fit <- numeric(n)
    lev[1] <- y[1]
    tr[1] <- y[2] - y[1]
    fit[1] <- y[1]
    for (t in 2:n) {
      p <- lev[t - 1] + d * tr[t - 1]
      fit[t] <- p
      lev[t] <- a * y[t] + (1 - a) * p
      tr[t] <- b * (lev[t] - lev[t - 1]) + (1 - b) * d * tr[t - 1]
    }
    list(lev = lev, tr = tr, fit = fit)
  }
  # squash into the OPEN interval: a plain logistic saturates to
  # exactly 1 in double precision and then fails the validity check
  sq <- function(z) 1e-6 + (1 - 2e-6) / (1 + exp(-pmax(pmin(z, 30), -30)))
  if (is.null(alpha) || is.null(beta)) {
    sse <- function(x) {
      o <- run(sq(x[1]), sq(x[2]))
      sum((y[-1] - o$fit[-1])^2)
    }
    par <- stats::optim(c(0, -1), sse, method = "Nelder-Mead")$par
    if (is.null(alpha)) alpha <- sq(par[1])
    if (is.null(beta)) beta <- sq(par[2])
  }
  for (nm in c("alpha", "beta")) {
    v <- get(nm)
    if (!isTRUE(v > 0 && v < 1)) stop(sprintf("%s must lie in (0, 1).", nm), call. = FALSE)
  }
  o <- run(alpha, beta)
  steps <- if (damped) cumsum(phi^seq_len(h)) else seq_len(h)
  list(
    forecast = o$lev[n] + steps * o$tr[n], level = o$lev, trend = o$tr,
    fitted = o$fit, residuals = y - o$fit,
    sse = sum((y[-1] - o$fit[-1])^2), alpha = alpha, beta = beta,
    damped = damped, phi = if (damped) phi else NULL, n = as.integer(n),
    method = paste0("Holt linear trend", if (damped) " (damped)" else "")
  )
}

#' Holt-Winters seasonal exponential smoothing
#'
#' Mirrors \code{morie.fn.johw}. Seasonal indices are initialised from
#' a CENTRED MOVING AVERAGE detrend, not from raw period means: with a
#' trend inside the period the per-position mean absorbs part of the
#' slope and the indices come out phase-shifted.
#'
#' @param y numeric series, at least 2 * m observations.
#' @param alpha,beta,gamma smoothing parameters in (0, 1); estimated
#'   when NULL.
#' @param m seasonal period.
#' @param horizon forecast horizon.
#' @param seasonal "additive" or "multiplicative"; the latter requires
#'   strictly positive data.
#' @return list: forecast, level, trend, season, fitted, residuals,
#'   sse, alpha, beta, gamma, m, seasonal, n.
#' @references Hyndman, R. J. & Athanasopoulos, G. (2021).
#'   Forecasting: Principles and Practice (3rd ed.). OTexts. Sec. 8.3
#'   (Holt-Winters), Sec. 3.4 (classical decomposition).
#' @examples
#' set.seed(1)
#' y <- rep(c(3, 1, -2, -4, -1, 2, 5, 4, 1, -1, -3, -5), 4) + seq_len(48) * 0.5 + 10
#' morie_holt_winters(y, m = 12, horizon = 12)$forecast\[1\]
#' @export
morie_holt_winters <- function(y, alpha = NULL, beta = NULL, gamma = NULL,
                               m = 12L, horizon = 1L, seasonal = "additive") {
  y <- as.numeric(y)
  n <- length(y)
  m <- as.integer(m)
  if (m < 2L) stop("m must be at least 2.", call. = FALSE)
  if (n < 2L * m) stop("need at least 2*m observations.", call. = FALSE)
  if (!all(is.finite(y))) stop("y must be finite.", call. = FALSE)
  seasonal <- match.arg(seasonal, c("additive", "multiplicative"))
  mult <- seasonal == "multiplicative"
  if (mult && any(y <= 0)) {
    stop(paste("multiplicative seasonality needs strictly positive data;",
               "use seasonal = 'additive' for series with zeros or negatives."),
         call. = FALSE)
  }
  h <- as.integer(horizon)
  if (h < 1L) stop("horizon must be at least 1.", call. = FALSE)

  k <- n %/% m
  periods <- matrix(y[seq_len(k * m)], nrow = k, ncol = m, byrow = TRUE)
  overall <- mean(periods)
  w <- if (m %% 2L == 0L) c(0.5, rep(1, m - 1), 0.5) / m else rep(1, m) / m
  ma <- stats::filter(y, w, sides = 2)
  detr <- if (mult) y / ma else y - ma
  s0 <- vapply(seq_len(m), function(j) {
    v <- detr[seq(j, n, by = m)]
    v <- v[is.finite(v)]
    if (length(v)) mean(v) else if (mult) 1 else 0
  }, 0)
  s0 <- if (mult) s0 / mean(s0) else s0 - mean(s0)

  run <- function(a, b, g) {
    lev <- tr <- fit <- numeric(n)
    se <- numeric(n + m)
    se[seq_len(m)] <- s0
    lev[1] <- overall
    tr[1] <- (mean(periods[k, ]) - mean(periods[1, ])) / max((k - 1) * m, 1)
    fit[1] <- if (mult) lev[1] * se[1] else lev[1] + se[1]
    for (t in 2:n) {
      p <- lev[t - 1] + tr[t - 1]
      fit[t] <- if (mult) p * se[t] else p + se[t]
      if (mult) {
        lev[t] <- a * (y[t] / se[t]) + (1 - a) * p
        se[t + m] <- g * (y[t] / max(lev[t], 1e-12)) + (1 - g) * se[t]
      } else {
        lev[t] <- a * (y[t] - se[t]) + (1 - a) * p
        se[t + m] <- g * (y[t] - p) + (1 - g) * se[t]
      }
      tr[t] <- b * (lev[t] - lev[t - 1]) + (1 - b) * tr[t - 1]
    }
    list(lev = lev, tr = tr, se = se, fit = fit)
  }
  sq <- function(z) 1e-6 + (1 - 2e-6) / (1 + exp(-pmax(pmin(z, 30), -30)))
  if (is.null(alpha) || is.null(beta) || is.null(gamma)) {
    sse <- function(x) {
      o <- run(sq(x[1]), sq(x[2]), sq(x[3]))
      r <- y[-seq_len(m)] - o$fit[-seq_len(m)]
      if (all(is.finite(r))) sum(r^2) else 1e18
    }
    par <- stats::optim(c(0, -2, -1), sse, method = "Nelder-Mead",
                        control = list(maxit = 800))$par
    if (is.null(alpha)) alpha <- sq(par[1])
    if (is.null(beta)) beta <- sq(par[2])
    if (is.null(gamma)) gamma <- sq(par[3])
  }
  for (nm in c("alpha", "beta", "gamma")) {
    v <- get(nm)
    if (!isTRUE(v > 0 && v < 1)) stop(sprintf("%s must lie in (0, 1).", nm), call. = FALSE)
  }
  o <- run(alpha, beta, gamma)
  sf <- vapply(seq_len(h), function(i) {
    idx <- n + i
    if (idx <= length(o$se)) o$se[idx] else o$se[idx - m]
  }, 0)
  base <- o$lev[n] + seq_len(h) * o$tr[n]
  list(
    forecast = if (mult) base * sf else base + sf,
    level = o$lev, trend = o$tr, season = o$se[seq_len(n)], fitted = o$fit,
    residuals = y - o$fit,
    sse = sum((y[-seq_len(m)] - o$fit[-seq_len(m)])^2),
    alpha = alpha, beta = beta, gamma = gamma, m = m, seasonal = seasonal,
    n = as.integer(n),
    method = sprintf("Holt-Winters %s seasonal method (m = %d)", seasonal, m)
  )
}

#' Reconcile hierarchical forecasts so they add up
#'
#' Bottom-up sets \eqn{\tilde y = S \hat y_b}. The OLS and WLS routes
#' project the FULL base vector, letting the aggregate levels inform
#' the bottom -- which bottom-up cannot do. Mirrors
#' \code{morie.fn.johbu}.
#'
#' @param y_hat_bottom numeric bottom-level forecasts (bottom-up only).
#' @param S summing matrix (n x m).
#' @param base numeric base forecasts for all n series (projection
#'   methods).
#' @param method "bottom_up", "ols" or "wls".
#' @param residuals matrix (T x n) of in-sample base residuals (wls).
#' @return list: reconciled, bottom, coherent, P, n, m.
#' @references Wickramasuriya, S. L., Athanasopoulos, G. & Hyndman,
#'   R. J. (2019). Optimal forecast reconciliation for hierarchical and
#'   grouped time series through trace minimization. JASA, 114(526),
#'   804-819.
#' @examples
#' S <- rbind(c(1, 1), c(1, 0), c(0, 1))
#' morie_reconcile_hierarchy(c(3, 4), S)$reconciled
#' @export
morie_reconcile_hierarchy <- function(y_hat_bottom = NULL, S, base = NULL,
                                      method = "bottom_up", residuals = NULL) {
  S <- as.matrix(S)
  n <- nrow(S)
  m <- ncol(S)
  method <- match.arg(method, c("bottom_up", "ols", "wls"))
  if (method == "bottom_up") {
    yb <- as.numeric(y_hat_bottom)
    if (length(yb) != m) {
      stop(sprintf("y_hat_bottom must have %d entries.", m), call. = FALSE)
    }
    return(list(
      reconciled = as.numeric(S %*% yb), bottom = yb, coherent = TRUE,
      P = NULL, n = n, m = m,
      method = "Bottom-up reconciliation (coherent by construction)"
    ))
  }
  if (is.null(base)) {
    stop(sprintf("method = '%s' needs the full base forecast vector.", method),
         call. = FALSE)
  }
  yh <- as.numeric(base)
  if (length(yh) != n) stop(sprintf("base must have %d entries.", n), call. = FALSE)
  Winv <- if (method == "ols") {
    diag(n)
  } else {
    if (is.null(residuals)) stop("method = 'wls' needs in-sample residuals.", call. = FALSE)
    Rm <- as.matrix(residuals)
    if (ncol(Rm) != n) stop(sprintf("residuals must have %d columns.", n), call. = FALSE)
    diag(1 / pmax(apply(Rm, 2, stats::var), 1e-12))
  }
  P <- .morie_ginv(t(S) %*% Winv %*% S) %*% t(S) %*% Winv
  list(
    reconciled = as.numeric(S %*% (P %*% yh)), bottom = as.numeric(P %*% yh),
    coherent = TRUE, P = P, n = n, m = m,
    method = sprintf("%s optimal-combination reconciliation", toupper(method))
  )
}

#' Aalen-Johansen multistate transition matrix
#'
#' \eqn{\hat P(s,t) = \prod_{s<u\le t}(I + d\hat A(u))}, a
#' nonparametric product integral over event times. Rows sum to one by
#' construction. Mirrors \code{morie.fn.mstrn}.
#'
#' @param time numeric transition times.
#' @param state_from,state_to integer states, 0-indexed.
#' @param n_states total number of states; inferred if NULL.
#' @param s,t interval endpoints; t defaults to the last event time.
#' @return list: P, event_times, increments, at_risk, n_states,
#'   n_transitions.
#' @references Aalen, O. O. & Johansen, S. (1978). An empirical
#'   transition matrix for non-homogeneous Markov chains based on
#'   censored observations. Scand. J. Statist., 5(3), 141-150.
#' @examples
#' morie_aalen_johansen(1:6, c(0, 0, 0, 1, 0, 1), c(1, 1, 2, 2, 1, 2), 3)$P
#' @export
morie_aalen_johansen <- function(time, state_from, state_to, n_states = NULL,
                                 s = 0, t = NULL) {
  time <- as.numeric(time)
  sf <- as.integer(state_from)
  st <- as.integer(state_to)
  k <- length(time)
  if (length(sf) != k || length(st) != k) {
    stop("time, state_from and state_to must have the same length.", call. = FALSE)
  }
  if (k < 2L) stop("need at least 2 transitions.", call. = FALSE)
  if (any(sf < 0) || any(st < 0)) stop("states must be non-negative.", call. = FALSE)
  ns <- if (is.null(n_states)) max(sf, st) + 1L else as.integer(n_states)
  if (ns < 2L) stop("need at least 2 states.", call. = FALSE)
  if (any(sf >= ns) || any(st >= ns)) {
    stop(sprintf("state labels must be below n_states = %d.", ns), call. = FALSE)
  }
  if (is.null(t)) t <- max(time)
  if (t <= s) stop("t must exceed s.", call. = FALSE)

  o <- order(time)
  time <- time[o]
  sf <- sf[o]
  st <- st[o]
  occ <- tabulate(sf + 1L, nbins = ns)
  at_risk <- occ
  P <- diag(ns)
  times <- numeric(0)
  incs <- list()
  for (u in unique(time)) {
    if (!(u > s && u <= t)) next
    idx <- which(time == u)
    dA <- matrix(0, ns, ns)
    for (i in idx) {
      if (occ[sf[i] + 1L] > 0) {
        dA[sf[i] + 1L, st[i] + 1L] <- dA[sf[i] + 1L, st[i] + 1L] + 1 / occ[sf[i] + 1L]
      }
    }
    diag(dA) <- 0
    diag(dA) <- -rowSums(dA)
    P <- P %*% (diag(ns) + dA)
    for (i in idx) {
      occ[sf[i] + 1L] <- occ[sf[i] + 1L] - 1L
      occ[st[i] + 1L] <- occ[st[i] + 1L] + 1L
    }
    times <- c(times, u)
    incs[[length(incs) + 1L]] <- dA
  }
  list(
    P = P, event_times = times, increments = incs, at_risk = at_risk,
    n_states = ns, n_transitions = as.integer(k), s = s, t = t,
    method = "Aalen-Johansen product-integral transition matrix"
  )
}
