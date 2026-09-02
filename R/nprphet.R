# SPDX-License-Identifier: AGPL-3.0-or-later
#' Additive forecast decomposition: trend, seasonality, autoregression
#'
#' NeuralProphet keeps the Prophet decomposition but fits it, and adds an
#' autoregression block. With the linear AR-Net the model collapses to a
#' single design matrix: piecewise-linear trend, Fourier seasonality and
#' p lags estimated in one least squares.
#'
#' Formula: \code{y(t) = T(t) + S(t) + A(t)},
#' \code{T(t) = (d0 + G(t)'d) t + (r0 + G(t)'r)},
#' \code{S_p(t) = sum_j a_j cos(2 pi j t / p) + b_j sin(2 pi j t / p)},
#' linear AR \code{A(t) = sum_i w_i y(t - i)}.
#'
#' @param ds Numeric increasing time index.
#' @param y Observed series.
#' @param ar_layers Number of autoregressive lags p.
#' @param n_changepoints Interior trend changepoints at equally spaced quantiles of ds.
#' @param seasonality Length-2 vector: period and number of Fourier terms.
#' @return List with \code{estimate}, \code{coef}, \code{fitted}, \code{resid}, \code{rmse}, \code{n}.
#' @references Triebe, O. et al. (2021). NeuralProphet: explainable
#'   forecasting at scale. arXiv:2111.15397.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Nprphet(V, V)
Nprphet <- function(ds, y, ar_layers = 0, n_changepoints = 3, seasonality = c(365.25, 3)) {
  t <- as.numeric(ds)
  yv <- as.numeric(y)
  n <- length(yv)
  p <- as.integer(ar_layers)
  period <- as.numeric(seasonality[1])
  nf <- as.integer(seasonality[2])
  ncp <- as.integer(n_changepoints)
  cps <- if (ncp > 0L) vapply(seq_len(ncp), function(k) .s4_quantile7(t, k / (ncp + 1)), 0) else numeric(0)
  rows <- list()
  targ <- numeric(0)
  for (i in (p + 1L):n) {
    r <- c(1, t[i])
    if (length(cps)) r <- c(r, pmax(t[i] - cps, 0))
    if (nf > 0L) for (j in seq_len(nf)) {
      r <- c(r, cos(2 * pi * j * t[i] / period), sin(2 * pi * j * t[i] / period))
    }
    if (p > 0L) for (k in seq_len(p)) r <- c(r, yv[i - k])
    rows[[length(rows) + 1L]] <- r
    targ <- c(targ, yv[i])
  }
  X <- do.call(rbind, rows)
  fit <- .t1_lstsq(X, targ)
  rmse <- sqrt(sum(fit$resid^2) / length(fit$resid))
  .t1_result(estimate = fit$fitted[length(fit$fitted)], coef = fit$beta,
             fitted = fit$fitted, resid = fit$resid, rmse = rmse, n = n,
             method = "NeuralProphet decomposition with linear AR-Net")
}
