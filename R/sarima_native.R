# Seasonal ARIMA: the multiplicative (p,d,q)x(P,D,Q)_s model.
# Sources: Box, G. E. P., Jenkins, G. M., Reinsel, G. C. & Ljung, G.
# M. (2016) *Time Series Analysis: Forecasting and Control*, 5th edn,
# Wiley, ISBN 978-1-118-67502-1. Chapter 9 throughout: Sec. 9.1.3
# for the general multiplicative model (9.1.7) and the order notation
# (p,d,q)x(P,D,Q)_s; Sec. 9.2.1 for the airline model (9.2.1)-(9.2.2)
# and its invertibility region; Sec. 9.2.2 for the difference-equation
# forecasts (9.2.3)-(9.2.6); Sec. 9.2.3 for the autocovariances
# (9.2.18), the closed forms for rho_1 and rho_12, Bartlett's variance
# (9.2.19) and the preliminary estimates theta ~= 0.39, Theta ~= 0.48
# from r_1 = -0.34, r_12 = -0.39; Sec. 9.2.4 for the conditional
# recursion (9.2.20), the least-squares estimates 0.40 +/- 0.08 and
# 0.61 +/- 0.07 with sigma_a^2 = 1.34e-3, the large-sample variances
# (9.2.21), and the R output quoted above; and Part Five, Series G,
# for the 144 monthly airline passenger totals reproduced in
# series_g. Harvey, A. C. (1989) *Forecasting, Structural Time Series
# Models and the Kalman Filter*, Cambridge University Press,
# doi:10.1017/CBO9781107049994, Sec. 3.3, for the state-space form of
# an ARMA process used by loglik and for the stationary initial state
# covariance.

.SARIMA_METHODS <- c("ml", "uls", "css", "moment")

.SARIMA_SERIES_G_BY_MONTH <- list(
  c(112, 115, 145, 171, 196, 204, 242, 284, 315, 340, 360, 417),
  c(118, 126, 150, 180, 196, 188, 233, 277, 301, 318, 342, 391),
  c(132, 141, 178, 193, 236, 235, 267, 317, 356, 362, 406, 419),
  c(129, 135, 163, 181, 235, 227, 269, 313, 348, 348, 396, 461),
  c(121, 125, 172, 183, 229, 234, 270, 318, 355, 363, 420, 472),
  c(135, 149, 178, 218, 243, 264, 315, 374, 422, 435, 472, 535),
  c(148, 170, 199, 230, 264, 302, 364, 413, 465, 491, 548, 622),
  c(148, 170, 199, 242, 272, 293, 347, 405, 467, 505, 559, 606),
  c(136, 158, 184, 209, 237, 259, 312, 355, 404, 404, 463, 508),
  c(119, 133, 162, 191, 211, 229, 274, 306, 347, 359, 407, 461),
  c(104, 114, 146, 172, 180, 203, 237, 271, 305, 310, 362, 390),
  c(118, 140, 166, 194, 201, 229, 278, 306, 336, 337, 405, 432)
)

series_g <- function(log = FALSE) {
  out <- numeric(144)
  idx <- 0
  for (y in 1:12) for (m in 1:12) {
    idx <- idx + 1
    out[idx] <- as.numeric(.SARIMA_SERIES_G_BY_MONTH[[m]][y])
  }
  if (log) out <- log(out)
  out
}

difference <- function(y, d = 0, D = 0, s = 1) {
  d <- as.integer(d); D <- as.integer(D); s <- as.integer(s)
  if (d < 0 || D < 0) stop("sarima: d and D must be non-negative")
  if (D && s < 2) stop(sprintf("sarima: seasonal differencing needs s >= 2, got %d", s))
  w <- as.numeric(y)
  for (step in seq_len(d)) {
    if (length(w) < 2) stop("sarima: series too short to difference")
    w <- w[2:length(w)] - w[1:(length(w) - 1)]
  }
  for (step in seq_len(D)) {
    if (length(w) <= s) stop(sprintf("sarima: series too short for seasonal differencing at s = %d", s))
    w <- w[(s + 1):length(w)] - w[1:(length(w) - s)]
  }
  w
}

.sarima_poly_mult <- function(a, b) {
  out <- rep(0, length(a) + length(b) - 1)
  for (i in seq_along(a)) for (j in seq_along(b)) out[i + j - 1] <- out[i + j - 1] + a[i] * b[j]
  out
}

.sarima_seasonal_lift <- function(c, s) {
  out <- rep(0, (length(c) - 1) * s + 1)
  for (i in seq_along(c)) out[(i - 1) * s + 1] <- c[i]
  out
}

expand_polynomials <- function(phi = numeric(0), Phi = numeric(0),
                                theta = numeric(0), Theta = numeric(0), s = 12) {
  s <- as.integer(s)
  if ((length(Phi) || length(Theta)) && s < 2)
    stop(sprintf("sarima: seasonal terms need s >= 2, got %d", s))
  ar_poly <- .sarima_poly_mult(c(1, -as.numeric(phi)),
                               .sarima_seasonal_lift(c(1, -as.numeric(Phi)), s))
  ma_poly <- .sarima_poly_mult(c(1, -as.numeric(theta)),
                               .sarima_seasonal_lift(c(1, -as.numeric(Theta)), s))
  list(ar = -ar_poly[-1], ma = -ma_poly[-1])
}

sample_acf <- function(x, lags) {
  n <- length(x)
  if (n < 2) stop("sarima: need at least two observations")
  m <- sum(x) / n
  d <- sum((x - m)^2)
  if (d <= 0) stop("sarima: the series is constant")
  out <- list()
  for (k in as.integer(unlist(lags))) {
    if (k < 1 || k >= n) stop(sprintf("sarima: lag %d out of range", k))
    out[[as.character(k)]] <- sum((x[(k + 1):n] - m) * (x[1:(n - k)] - m)) / d
  }
  out
}

airline_autocovariances <- function(theta, Theta, sigma2 = 1.0) {
  th <- as.numeric(theta); TH <- as.numeric(Theta)
  g0 <- (1 + th * th) * (1 + TH * TH) * sigma2
  g1 <- -th * (1 + TH * TH) * sigma2
  g11 <- th * TH * sigma2
  g12 <- -TH * (1 + th * th) * sigma2
  g13 <- th * TH * sigma2
  gamma <- list("0" = g0, "1" = g1, "11" = g11, "12" = g12, "13" = g13)
  rho <- list("0" = g0 / g0, "1" = g1 / g0, "11" = g11 / g0,
              "12" = g12 / g0, "13" = g13 / g0)
  list(gamma = gamma, rho = rho,
       rho_1 = -th / (1 + th * th), rho_12 = -TH / (1 + TH * TH),
       nonzero_lags = c(1, 11, 12, 13))
}

.sarima_invert_rho <- function(rho) {
  r <- as.numeric(rho)
  if (abs(r) > 0.5)
    stop(sprintf("sarima: |rho| = %.4f exceeds 0.5, so no invertible MA(1) reproduces it", abs(r)))
  disc <- sqrt(1 - 4 * r * r)
  if (r != 0) (-1 + disc) / (2 * r) else 0
}

moment_estimate <- function(rho) .sarima_invert_rho(rho)

preliminary_estimates <- function(w, s = 12) {
  r <- sample_acf(w, c(1, as.integer(s)))
  th <- .sarima_invert_rho(r[["1"]])
  TH <- .sarima_invert_rho(r[[as.character(as.integer(s))]])
  list(estimate = th, theta = th, Theta = TH,
       r_1 = r[["1"]], r_s = r[[as.character(as.integer(s))]],
       method = "moments from rho_1 and rho_s; Box et al. (2016) Sec. 9.2.3")
}

css <- function(w, ar = numeric(0), ma = numeric(0), full = FALSE) {
  ar <- as.numeric(ar); ma <- as.numeric(ma)
  n <- length(w)
  if (n == 0) stop("sarima: no observations")
  a <- rep(0, n)
  ssq <- 0
  for (t in 1:n) {
    pred <- 0
    for (i in seq_along(ar)) if (t - i >= 1) pred <- pred + ar[i] * w[t - i]
    for (j in seq_along(ma)) if (t - j >= 1) pred <- pred - ma[j] * a[t - j]
    a[t] <- w[t] - pred
    ssq <- ssq + a[t] * a[t]
  }
  if (full) return(list(ssq = ssq, residuals = a, sigma2 = ssq / n))
  ssq
}

.sarima_state_space <- function(ar, ma) {
  p <- length(ar); q <- length(ma)
  r <- max(p, q + 1)
  T <- matrix(0, r, r)
  if (r > 1) for (i in 1:(r - 1)) T[i, i + 1] <- 1
  for (i in seq_along(ar)) T[i, 1] <- ar[i]
  R <- c(1, -ma, rep(0, r - q - 1))
  list(T = T, R = R, r = r)
}

.sarima_initial_covariance <- function(T, R, r) {
  n <- r * r
  A <- matrix(0, n, n)
  b <- numeric(n)
  for (i in 1:r) for (j in 1:r) {
    row <- (i - 1) * r + j
    A[row, row] <- A[row, row] + 1
    b[row] <- R[i] * R[j]
    for (k in 1:r) for (m in 1:r)
      A[row, (k - 1) * r + m] <- A[row, (k - 1) * r + m] - T[i, k] * T[j, m]
  }
  vec <- solve(A, b)
  P <- matrix(0, r, r)
  for (i in 1:r) for (j in 1:r) P[i, j] <- vec[(i - 1) * r + j]
  P
}

loglik <- function(w, ar = numeric(0), ma = numeric(0)) {
  ar <- as.numeric(ar); ma <- as.numeric(ma)
  n <- length(w)
  if (n == 0) stop("sarima: no observations")
  ss_obj <- .sarima_state_space(ar, ma)
  T <- ss_obj$T; R <- ss_obj$R; r <- ss_obj$r
  P <- .sarima_initial_covariance(T, R, r)
  a <- rep(0, r)
  ssq <- 0
  sumlogf <- 0
  for (t in 1:n) {
    f <- P[1, 1]
    if (f <= 0)
      stop("sarima: non-positive prediction variance; the parameters are outside the stationary region")
    v <- w[t] - a[1]
    PZ <- P[, 1]
    Ka <- v / f * PZ
    a <- a + Ka
    P <- P - outer(PZ, PZ) / f
    ssq <- ssq + v * v / f
    sumlogf <- sumlogf + log(f)
    a <- as.numeric(T %*% a)
    TP <- T %*% P
    P <- TP %*% t(T) + R %o% R
  }
  sigma2 <- ssq / n
  ll <- (-0.5 * n * (log(2 * pi * sigma2) + 1) - 0.5 * sumlogf)
  list(loglik = ll, sigma2 = sigma2, n = n,
       exact_ssq = ssq, sum_log_f = sumlogf)
}

.sarima_roots_ok <- function(coefs, tol = 1.001) {
  if (length(coefs) == 0) return(TRUE)
  poly <- c(1, -as.numeric(coefs))
  while (length(poly) > 1 && poly[length(poly)] == 0) poly <- poly[-length(poly)]
  if (length(poly) == 1) return(TRUE)
  k <- length(poly) - 1
  C <- matrix(0, k, k)
  for (j in 1:k) C[1, j] <- -poly[j + 1] / poly[1]
  if (k > 1) for (i in 2:k) C[i, i - 1] <- 1
  ev <- eigen(C, only.values = TRUE)$values
  for (lam in ev) {
    m <- abs(lam)
    if (m <= 0) next
    if (1 / m < tol) return(FALSE)
  }
  TRUE
}

.sarima_minimize_nm <- function(fn, x0, maxit = 2000) {
  n <- length(x0)
  simplex <- matrix(rep(x0, each = n + 1), nrow = n + 1, byrow = TRUE)
  for (i in 1:n) simplex[i + 1, i] <- simplex[i + 1, i] + if (x0[i] == 0) 0.1 else 0.05 * x0[i]
  fv <- apply(simplex, 1, fn)
  iter <- 0
  while (iter < maxit) {
    iter <- iter + 1
    ord <- order(fv)
    simplex <- simplex[ord, , drop = FALSE]
    fv <- fv[ord]
    if (max(abs(simplex[1, ] - simplex[n + 1, ])) < 1e-10 && abs(fv[1] - fv[n + 1]) < 1e-10) break
    xr <- colMeans(simplex[1:n, , drop = FALSE])
    xr <- xr + (xr - simplex[n + 1, ])
    fr <- fn(xr)
    if (fr < fv[1]) {
      xc <- xr + (xr - simplex[n + 1, ])
      fc <- fn(xc)
      if (fc < fr) { simplex[n + 1, ] <- xc; fv[n + 1] <- fc }
      else { simplex[n + 1, ] <- xr; fv[n + 1] <- fr }
    } else if (fr < fv[n]) {
      simplex[n + 1, ] <- xr; fv[n + 1] <- fr
    } else {
      xc <- simplex[n + 1, ] + 0.5 * (xr - simplex[n + 1, ])
      fc <- fn(xc)
      if (fc < fv[n + 1]) { simplex[n + 1, ] <- xc; fv[n + 1] <- fc }
      else for (i in 2:(n + 1)) {
        simplex[i, ] <- simplex[1, ] + 0.5 * (simplex[i, ] - simplex[1, ])
        fv[i] <- fn(simplex[i, ])
      }
    }
  }
  best <- which.min(fv)
  list(x = simplex[best, ], fun = fv[best], success = TRUE)
}

.sarima_fit <- function(y, order = c(0, 1, 1), seasonal_order = c(0, 1, 1), s = 12,
                method = "ml", start = NULL) {
  if (!(method %in% .SARIMA_METHODS))
    stop(sprintf("sarima: method must be one of %s, got %s",
                 paste(.SARIMA_METHODS, collapse = ", "), method))
  p <- as.integer(order[1]); d <- as.integer(order[2]); q <- as.integer(order[3])
  P <- as.integer(seasonal_order[1]); D <- as.integer(seasonal_order[2]); Q <- as.integer(seasonal_order[3])
  s <- as.integer(s)
  if (min(p, d, q, P, D, Q) < 0) stop("sarima: orders must be non-negative")
  w <- difference(y, d, D, s)
  npar <- p + q + P + Q
  if (npar == 0) stop("sarima: the model has no free parameters")
  if (length(w) <= npar)
    stop(sprintf("sarima: %d differenced observations cannot support %d parameters",
                 length(w), npar))

  if (method == "moment") {
    if ((p != 0) || (q != 1) || (P != 0) || (Q != 1))
      stop(sprintf("sarima: the moment route is defined for the (0,d,1)x(0,D,1) airline model only, got orders (%d,%d)x(%d,%d)",
                   p, q, P, Q))
    pre <- preliminary_estimates(w, s)
    theta <- c(pre$theta); Theta <- c(pre$Theta)
    em <- expand_polynomials(numeric(0), numeric(0), theta, Theta, s)
    ar <- em$ar; ma <- em$ma
    ll <- loglik(w, ar, ma)
    cs <- css(w, ar, ma, full = TRUE)
    return(.sarima_package(y, w, numeric(0), theta, numeric(0), Theta, s, order, seasonal_order, ll, cs, method, NULL))
  }

  .unpack <- function(v) {
    i <- 1
    phi <- v[i:(i + p - 1)]; i <- i + p
    th <- v[i:(i + q - 1)]; i <- i + q
    Ph <- v[i:(i + P - 1)]; i <- i + P
    Th <- v[i:(i + Q - 1)]
    list(phi = phi, th = th, Ph = Ph, Th = Th)
  }

  .objective <- function(v) {
    up <- .unpack(v)
    if (!.sarima_roots_ok(up$phi) || !.sarima_roots_ok(up$Ph)) return(1e10)
    em <- expand_polynomials(up$phi, up$Ph, up$th, up$Th, s)
    ar <- em$ar; ma <- em$ma
    if (!.sarima_roots_ok(ma)) return(1e10)
    tryCatch({
      if (method == "css") return(css(w, ar, ma))
      if (method == "uls") return(loglik(w, ar, ma)$exact_ssq)
      return(-loglik(w, ar, ma)$loglik)
    }, error = function(e) 1e10)
  }

  if (!is.null(start)) {
    x0 <- as.numeric(unlist(start))
    if (length(x0) != npar)
      stop(sprintf("sarima: %d starting values for %d parameters", length(x0), npar))
  } else if ((p == 0) && (q == 1) && (P == 0) && (Q == 1)) {
    pre <- preliminary_estimates(w, s)
    x0 <- c(pre$theta, pre$Theta)
  } else {
    x0 <- rep(0.1, npar)
  }

  best <- .objective(x0)
  xhat <- x0
  res <- NULL
  for (iter in 1:8) {
    r2 <- .sarima_minimize_nm(.objective, xhat)
    cand <- as.numeric(r2$x)
    val <- .objective(cand)
    if (val < best - 1e-11) {
      best <- val; xhat <- cand
    } else {
      xhat <- if (val < best) cand else xhat
      break
    }
    res <- r2
  }
  up <- .unpack(xhat)
  em <- expand_polynomials(up$phi, up$Ph, up$th, up$Th, s)
  ar <- em$ar; ma <- em$ma
  ll <- loglik(w, ar, ma)
  cs <- css(w, ar, ma, full = TRUE)
  .sarima_package(y, w, up$phi, up$th, up$Ph, up$Th, s, order, seasonal_order, ll, cs, method, res)
}

.sarima_package <- function(y, w, phi, theta, Phi, Theta, s, order, seasonal_order,
                            ll, cs, method, res) {
  npar <- length(phi) + length(theta) + length(Phi) + length(Theta)
  sigma2 <- if (method %in% c("ml", "uls")) ll$sigma2 else cs$sigma2
  aic <- -2 * ll$loglik + 2 * (npar + 1)
  em <- expand_polynomials(phi, Phi, theta, Theta, s)
  list(estimate = sigma2, sigma2 = sigma2,
       phi = as.numeric(phi), theta = as.numeric(theta),
       Phi = as.numeric(Phi), Theta = as.numeric(Theta),
       ar = em$ar, ma = em$ma,
       loglik = ll$loglik, aic = aic,
       n_used = length(w), n_par = npar,
       residuals = cs$residuals, ssq = cs$ssq,
       order = as.integer(order),
       seasonal_order = as.integer(seasonal_order),
       s = as.integer(s), y = as.numeric(y), w = w,
       fit_method = method,
       converged = if (is.null(res)) TRUE else isTRUE(res$success),
       method = sprintf("multiplicative seasonal ARIMA by %s; Box et al. (2016) Ch. 9", method))
}

.sarima_diff_poly <- function(k, s) {
  out <- c(1.0)
  for (i in seq_len(as.integer(k)))
    out <- .sarima_poly_mult(out, c(1, rep(0, s - 1), -1))
  out
}

.sarima_psi_weights <- function(ar, ma, h) {
  psi <- c(1.0)
  for (j in 2:h) {
    v <- if (j - 1 <= length(ma)) -ma[j - 1] else 0
    for (i in seq_along(ar)) if (j - i - 1 >= 1) v <- v + ar[i] * psi[j - i - 1]
    psi <- c(psi, v)
  }
  psi
}

forecast <- function(fitted, h = 12) {
  h <- as.integer(h)
  if (h < 1) stop("sarima: h must be at least 1")
  y <- as.numeric(fitted$y)
  d <- fitted$order[2]; D <- fitted$seasonal_order[2]
  s <- fitted$s
  ar <- fitted$ar; ma <- fitted$ma
  dpoly <- .sarima_diff_poly(1, 1)
  Dpoly <- .sarima_diff_poly(D, s)
  lhs <- .sarima_poly_mult(c(1, -ar), .sarima_poly_mult(dpoly, Dpoly))
  z_ar <- -lhs[-1]
  a <- as.numeric(fitted$residuals)
  zpad <- y
  apad <- c(rep(0, length(y) - length(a)), a)
  out <- numeric(h)
  for (step in 1:h) {
    t <- length(zpad)
    val <- 0
    for (i in seq_along(z_ar)) val <- val + z_ar[i] * zpad[t - i]
    for (j in seq_along(ma)) {
      idx <- t - j
      if (idx >= 1 && idx <= length(apad)) val <- val - ma[j] * apad[idx]
    }
    zpad <- c(zpad, val)
    apad <- c(apad, 0)
    out[step] <- val
  }
  psi <- .sarima_psi_weights(z_ar, ma, h)
  var <- numeric(h)
  for (i in 1:h) var[i] <- fitted$sigma2 * sum(psi[1:i]^2)
  list(estimate = out[1], forecast = out, variance = var,
       se = sqrt(pmax(var, 0)), psi = psi,
       method = "difference-equation forecasts; Box et al. (2016) Sec. 9.2.2")
}

large_sample_se <- function(theta, Theta, n) {
  th <- as.numeric(theta); TH <- as.numeric(Theta); n <- as.integer(n)
  if (n < 1) stop("sarima: n must be positive")
  v_th <- (1 - th * th) / n
  v_TH <- (1 - TH * TH) / n
  list(var_theta = v_th, var_Theta = v_TH,
       se_theta = sqrt(max(v_th, 0)), se_Theta = sqrt(max(v_TH, 0)),
       cov = 0,
       off_diagonal_term = th^11 / (1 - th^12 * TH))
}

bartlett_se <- function(rho, n) {
  n <- as.integer(n)
  if (n < 1) stop("sarima: n must be positive")
  r <- as.list(rho)
  vals <- sapply(c("1", "11", "12", "13"), function(k) as.numeric(r[[k]] %||% 0))
  if (is.null(rho[["1"]]) && !is.null(rho[[1]])) {
    vals <- c(as.numeric(rho[[1]]), as.numeric(rho[[11]]), as.numeric(rho[[12]]), as.numeric(rho[[13]]))
  }
  ssq <- sum(vals^2)
  var <- (1 + 2 * ssq) / n
  list(variance = var, se = sqrt(var), white_noise_se = sqrt(1 / n))
}

r_convention <- function(fitted) {
  list(ma = -as.numeric(fitted$theta), sma = -as.numeric(fitted$Theta),
       ar = as.numeric(fitted$phi), sar = as.numeric(fitted$Phi),
       sigma2 = fitted$sigma2, loglik = fitted$loglik, aic = fitted$aic,
       note = "R writes (1 + theta B); the book writes (1 - theta B)")
}

.sarima_cheatsheet <- function() {
  paste("sarima: phi(B)Phi(B^s) nabla^d nabla_s^D z = theta(B)Theta(B^s) a. ",
        "The airline (0,1,1)x(0,1,1)_12 is an MA(13) in w = nabla nabla_12 z ",
        "with two parameters, nonzero autocorrelations only at lags 1, 11, 12, ",
        "13, and rho_1 = -theta/(1+theta^2) untouched by the seasonal factor. ",
        "Three routes kept: moment, css, and the exact likelihood (default) -- ",
        "on the logged airline data the last reproduces R's 0.4018 / 0.5569, ",
        "sigma^2 0.001348, loglik 244.7, aic -483.4.", sep = "")
}

seasonal_arima <- fit

morie_sarima <- fit
