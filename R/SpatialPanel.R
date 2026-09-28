.spp_lag <- function(W, v, N, T) as.vector(W %*% matrix(v, N, T))

.spp_demean <- function(v, N, T, effects) {
  M <- matrix(v, N, T)
  out <- M
  if (effects %in% c("individual", "twoways")) out <- out - rowSums(M) / T
  if (effects %in% c("time", "twoways")) out <- sweep(out, 2, colSums(M) / N)
  if (effects == "twoways") out <- out + sum(M) / (N * T)
  as.vector(out)
}

.spp_ols <- function(X, y) {
  b <- as.vector(solve(crossprod(X), crossprod(X, y)))
  list(beta = b, res = as.vector(y - X %*% b))
}

.spp_golden <- function(f, lo, hi, tol = 1e-12) {
  g <- (sqrt(5) - 1) / 2
  a <- lo
  b <- hi
  cc <- b - g * (b - a)
  d <- a + g * (b - a)
  fc <- f(cc)
  fd <- f(d)
  while (b - a > tol * (abs(cc) + abs(d) + 1e-3)) {
    if (fc > fd) {
      b <- d
      d <- cc
      fd <- fc
      cc <- b - g * (b - a)
      fc <- f(cc)
    } else {
      a <- cc
      cc <- d
      fc <- fd
      d <- a + g * (b - a)
      fd <- f(d)
    }
  }
  (a + b) / 2
}

.spp_trace_aw <- function(W, r) sum(diag(solve(diag(nrow(W)) - r * W, W)))

.spp_polish <- function(df, x, lo, hi) {
  h <- 1e-4 * max(1, abs(x))
  a <- max(lo, x - h)
  b <- min(hi, x + h)
  if (!(df(a) > 0 && df(b) < 0)) return(x)
  for (i in seq_len(200)) {
    m <- (a + b) / 2
    if (m <= a || m >= b) break
    if (df(m) > 0) a <- m else b <- m
  }
  (a + b) / 2
}

.spp_ldet <- function(W, r) as.numeric(determinant(diag(nrow(W)) - r * W, logarithm = TRUE)$modulus)

#' Spatial panel data models
#'
#' \code{SpatialPanelMl}: maximum likelihood spatial lag, error and Durbin
#' panel models with individual, time or two-way fixed effects, or pooled
#' (Elhorst), with the optional Lee-Yu variance correction.
#' \code{SpatialPanelReLag}: random-effects spatial lag model (Elhorst 2003).
#' \code{InformationCriteria}: AIC, AICc and BIC. Rows are stacked by period.
#' Identical to the Python arm \code{morie.fn.sppanel}.
#'
#' @param y Response.
#' @param X Regressor matrix (no intercept).
#' @param W Spatial weights (N by N).
#' @param n_units Number of units N.
#' @param model \code{"lag"}, \code{"error"} or \code{"durbin"}.
#' @param effects \code{"individual"}, \code{"time"}, \code{"twoways"} or
#'   \code{"pooled"}.
#' @param lee_yu Apply the Lee-Yu variance correction.
#' @param interval Search interval of the spatial parameter.
#' @param tol,maxit Convergence controls.
#' @param loglik Log-likelihood.
#' @param k Number of parameters.
#' @param n Number of observations.
#' @return A list.
#' @references Elhorst, J. P. (2003). Specification and estimation of spatial
#'   panel data models. International Regional Science Review 26, 244-268.
#'
#'   Lee, L.-F. and Yu, J. (2010). Estimation of spatial autoregressive panel
#'   data models with fixed effects. Journal of Econometrics 154, 165-185.
#' @examples
#' InformationCriteria(-120.5, 4, 100)$bic
#' @export
SpatialPanelMl <- function(y, X, W, n_units, model = "lag", effects = "individual", lee_yu = FALSE,
                           interval = c(-0.99, 0.99)) {
  X <- as.matrix(X)
  N <- n_units
  T <- length(y) %/% N
  NT <- N * T
  if (model == "durbin") X <- cbind(X, apply(X, 2, .spp_lag, W = W, N = N, T = T))
  if (effects == "pooled") {
    Xt <- cbind(1, X)
    yt <- y
  } else {
    Xt <- apply(X, 2, .spp_demean, N = N, T = T, effects = effects)
    yt <- .spp_demean(y, N, T, effects)
  }
  wyt <- .spp_lag(W, yt, N, T)
  if (model %in% c("lag", "durbin")) {
    e0 <- .spp_ols(Xt, yt)$res
    e1 <- .spp_ols(Xt, wyt)$res
    a <- sum(e0^2)
    b <- sum(e0 * e1)
    cc <- sum(e1^2)
    rho <- .spp_golden(function(r) -NT / 2 * log(a - 2 * r * b + r^2 * cc) + T * .spp_ldet(W, r),
                       interval[1], interval[2])
    rho <- .spp_polish(function(r) NT * (b - r * cc) / (a - 2 * r * b + r^2 * cc) - T * .spp_trace_aw(W, r),
                       rho, interval[1], interval[2])
    f <- .spp_ols(Xt, yt - rho * wyt)
  } else if (model == "error") {
    wxt <- apply(Xt, 2, .spp_lag, W = W, N = N, T = T)
    fit <- function(r) .spp_ols(Xt - r * wxt, yt - r * wyt)
    rho <- .spp_golden(function(r) -NT / 2 * log(sum(fit(r)$res^2)) + T * .spp_ldet(W, r), interval[1], interval[2])
    df <- function(r) {
      ft <- fit(r)
      NT * sum(ft$res * (wyt - wxt %*% ft$beta)) / sum(ft$res^2) - T * .spp_trace_aw(W, r)
    }
    rho <- .spp_polish(df, rho, interval[1], interval[2])
    f <- fit(rho)
  } else {
    stop("model must be 'lag', 'error' or 'durbin'")
  }
  sse <- sum(f$res^2)
  s2 <- sse / NT
  ll <- T * .spp_ldet(W, rho) - NT / 2 * log(2 * pi) - NT / 2 * log(s2) - sse / (2 * s2)
  if (lee_yu && effects == "individual") s2 <- T / (T - 1) * s2
  if (lee_yu && effects == "time") s2 <- N / (N - 1) * s2
  list(rho = rho, coefficients = f$beta, sigma2 = s2, loglik = ll, residuals = f$res, n_obs = NT)
}

#' @rdname SpatialPanelMl
#' @export
SpatialPanelReLag <- function(y, X, W, n_units, interval = c(-0.99, 0.99), tol = 1e-10, maxit = 500) {
  X <- cbind(1, as.matrix(X))
  N <- n_units
  T <- length(y) %/% N
  NT <- N * T
  wy <- .spp_lag(W, y, N, T)
  tr <- function(v, phi) as.vector(matrix(v, N, T) - (1 - phi) * rowSums(matrix(v, N, T)) / T)
  phi <- 1
  for (it in seq_len(maxit)) {
    ys <- tr(y, phi)
    wys <- tr(wy, phi)
    Xs <- apply(X, 2, tr, phi = phi)
    e0 <- .spp_ols(Xs, ys)$res
    e1 <- .spp_ols(Xs, wys)$res
    a <- sum(e0^2)
    b <- sum(e0 * e1)
    cc <- sum(e1^2)
    rho <- .spp_golden(function(r) -NT / 2 * log(a - 2 * r * b + r^2 * cc) + T * .spp_ldet(W, r),
                       interval[1], interval[2])
    rho <- .spp_polish(function(r) NT * (b - r * cc) / (a - 2 * r * b + r^2 * cc) - T * .spp_trace_aw(W, r),
                       rho, interval[1], interval[2])
    beta <- .spp_ols(Xs, ys - rho * wys)$beta
    d <- as.vector(y - rho * wy - X %*% beta)
    md <- rowSums(matrix(d, N, T)) / T
    q <- sum((matrix(d, N, T) - md)^2)
    pq <- T * sum(md^2)
    new <- min(1, sqrt(q / ((T - 1) * pq)))
    done <- abs(new - phi) < tol
    phi <- new
    if (done) break
  }
  ys <- tr(y, phi)
  wys <- tr(wy, phi)
  Xs <- apply(X, 2, tr, phi = phi)
  f <- .spp_ols(Xs, ys - rho * wys)
  s2 <- sum(f$res^2) / NT
  ll <- T * .spp_ldet(W, rho) + N / 2 * log(phi^2) - NT / 2 * log(2 * pi * s2) - sum(f$res^2) / (2 * s2)
  list(rho = rho, coefficients = f$beta, phi = phi, sigma2 = s2,
       sigma2_mu = if (phi > 0) (1 / phi^2 - 1) * s2 / T else Inf, loglik = ll)
}

#' @rdname SpatialPanelMl
#' @export
InformationCriteria <- function(loglik, k, n) {
  aic <- -2 * loglik + 2 * k
  list(aic = aic, aicc = aic + 2 * k * (k + 1) / (n - k - 1), bic = -2 * loglik + k * log(n))
}
