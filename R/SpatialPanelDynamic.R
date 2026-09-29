# SPDX-License-Identifier: AGPL-3.0-or-later
# Spatial panel data models (Elhorst 2014).
# Identical to the Python arm morie.fn.sppaneldyn.

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
