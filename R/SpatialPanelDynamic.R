# SPDX-License-Identifier: AGPL-3.0-or-later
# Spatial panel data models (Elhorst 2014).
# Identical to the Python arm morie.fn.sppaneldyn.

.spp_wstack_m <- function(M, v, N, T_) as.numeric(M %*% matrix(v, N, T_))

.spp_stack <- function(y, X, W, lagx) {
  y <- as.matrix(y)
  T_ <- nrow(y)
  N <- ncol(y)
  if (is.list(X)) {
    K <- length(X[[1]][[1]])
    Xa <- array(0, c(T_, N, K))
    for (t in seq_len(T_)) for (i in seq_len(N)) Xa[t, i, ] <- as.numeric(X[[t]][[i]])
  } else {
    Xa <- X
    K <- dim(X)[3]
  }
  cols <- matrix(0, T_ * N, K)
  for (k in seq_len(K)) cols[, k] <- as.numeric(t(Xa[, , k, drop = FALSE][, , 1]))
  if (lagx) cols <- cbind(cols, apply(cols, 2, function(v) as.numeric(W %*% matrix(v, N, T_))))
  list(T = T_, N = N, y = as.numeric(t(y)), wy = as.numeric(W %*% t(y)), X = cols)
}

.spp_demean <- function(v, T, N, effects) {
  m <- matrix(v, N, T)
  out <- m
  if (effects %in% c("individual", "twoways")) out <- out - rowSums(m) / T
  if (effects %in% c("time", "twoways")) {
    out <- sweep(out, 2, colSums(m) / N)
    if (effects == "twoways") out <- out + sum(v) / (T * N)
  }
  as.numeric(out)
}

.spp_golden <- function(f, lo, hi, tol = 1e-12, max_iter = 300) {
  r <- (sqrt(5) - 1) / 2
  a <- lo
  b <- hi
  cc <- b - r * (b - a)
  d <- a + r * (b - a)
  fc <- f(cc)
  fd <- f(d)
  for (it in seq_len(max_iter)) {
    if (b - a <= tol) break
    if (fc < fd) {
      b <- d
      d <- cc
      fd <- fc
      cc <- b - r * (b - a)
      fc <- f(cc)
    } else {
      a <- cc
      cc <- d
      fc <- fd
      d <- a + r * (b - a)
      fd <- f(d)
    }
  }
  (a + b) / 2
}

.spp_ld <- function(ev, p) sum(log(Mod(1 - p * ev)))

.spp_tr <- function(ev, p) sum(Re(ev / (1 - p * ev)))

.spp_refine <- function(dfun, x0, lo, hi, width = 1e-6) {
  a <- max(lo, x0 - width)
  b <- min(hi, x0 + width)
  fa <- dfun(a)
  fb <- dfun(b)
  if (fa * fb > 0) return(x0)
  for (it in seq_len(100)) {
    m <- (a + b) / 2
    if (m == a || m == b) break
    fm <- dfun(m)
    if (fm == 0) return(m)
    if (fa * fm < 0) {
      b <- m
      fb <- fm
    } else {
      a <- m
      fa <- fm
    }
  }
  (a + b) / 2
}

#' Dynamic spatial panel (spatial ARX) on the fixed-effects spatial lag model
#'
#' \code{y_t = rho W y_t + gamma y_(t-1) + delta W y_(t-1) + X_t beta + mu + e_t}:
#' conditional on the first period, the lagged response and (when
#' \code{space_time_lag}) its spatial lag join \code{X_t} as regressors of the
#' fixed-effects spatial lag model \code{SpatialPanelMl} (Elhorst 2014, ch. 4).
#' \code{beta} lists gamma, delta (if used), then the \code{X} coefficients.
#' Identical to the Python arm \code{morie.fn.sppaneldyn}.
#'
#' @param y Numeric \code{T x N} matrix, rows are periods.
#' @param X \code{T x N x K} array (or a list of lists of numeric vectors).
#' @param W Row-standardised spatial weights.
#' @param effects \code{"individual"}, \code{"time"} or \code{"twoways"}.
#' @param space_time_lag Include \code{W y_(t-1)}.
#' @param bounds Search interval for \code{rho}.
#' @return A list with \code{rho}, \code{beta}, \code{coefficients},
#'   \code{sigma2}, \code{loglik}, \code{residuals} and \code{n_obs}.
#' @references Elhorst, J. P. (2014). Spatial Econometrics. Springer.
#'
#'   Yu, J., de Jong, R. and Lee, L.-F. (2008). Quasi-maximum likelihood
#'   estimators for spatial dynamic panel data with fixed effects when both n
#'   and T are large. Journal of Econometrics 146, 118-134.
#' @examples
#' W <- matrix(c(0, 1, 0, 0.5, 0, 0.5, 0, 1, 0), 3, byrow = TRUE)
#' y <- rbind(c(1, 2, 1.5), c(2, 2.5, 1), c(1.5, 3, 2.5), c(2.5, 2, 3), c(2, 1, 2.2))
#' X <- array(c(0.5, 1, 0.2, 1.5, 0.7, 0.1, 0.4, 2, 1.1, 1.2, 0.3, 1.9, 0.3, 0.9, 1.4), c(5, 3, 1))
#' SpPanelDynamic(y, X, W)$beta
#' @export
SpPanelDynamic <- function(y, X, W, effects = "individual", space_time_lag = TRUE, bounds = c(-0.99, 0.99)) {
  y <- as.matrix(y)
  T_ <- nrow(y)
  N <- ncol(y)
  if (is.list(X)) {
    K <- length(X[[1]][[1]])
    Xa <- array(0, c(T_, N, K))
    for (t in seq_len(T_)) for (i in seq_len(N)) Xa[t, i, ] <- as.numeric(X[[t]][[i]])
  } else {
    Xa <- X
    K <- dim(X)[3]
  }
  extra <- if (space_time_lag) 2 else 1
  yv <- numeric((T_ - 1) * N)
  Xm <- matrix(0, (T_ - 1) * N, K + extra)
  for (t in 2:T_) {
    wprev <- as.numeric(W %*% y[t - 1, ])
    for (i in seq_len(N)) {
      k <- (t - 2) * N + i
      yv[k] <- y[t, i]
      Xm[k, ] <- c(y[t - 1, i], if (space_time_lag) wprev[i], Xa[t, i, ])
    }
  }
  r <- SpatialPanelMl(yv, Xm, W, N, model = "lag", effects = effects, interval = bounds)
  list(rho = r$rho, beta = r$coefficients, coefficients = r$coefficients, sigma2 = r$sigma2,
       loglik = r$loglik, residuals = r$residuals, n_obs = r$n_obs)
}
