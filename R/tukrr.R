# SPDX-License-Identifier: AGPL-3.0-or-later
#' Robust regression by iteratively reweighted least squares
#'
#' Least squares gives a single bad row unbounded influence. The
#' biweight bounds it and, past the tuning constant, removes it. The
#' scale is re-estimated by MAD on every sweep, so the weights are not
#' decided by a scale the outliers themselves inflated.
#'
#' Determinism: fixed sweeps, ordinary least squares start, no tolerance.
#'
#' Formula: iterate \code{beta = argmin sum w_i (y_i - x_i'beta)^2} with
#' \code{w_i = [1 - (r_i/(c s))^2]^2} inside and 0 outside.
#'
#' @param X Design matrix; supply your own intercept column.
#' @param y Response.
#' @param c Tuning constant.
#' @param n_iter Reweighting sweeps.
#' @return List with \code{estimate}, \code{scale}, \code{weights},
#'   \code{fitted}, \code{resid}, \code{n}.
#' @references Beaton, A. E. & Tukey, J. W. (1974). Technometrics
#'   16:147-185; Holland, P. W. & Welsch, R. E. (1977). Commun Statist
#'   6:813-827.
#' @export
Tukrr <- function(X, y, c = 4.685, n_iter = 25) {
  Xm <- as.matrix(X); yv <- as.numeric(y)
  n <- nrow(Xm); p <- ncol(Xm)
  fit <- .t1_lstsq(Xm, yv)
  beta <- fit$beta; fitted <- fit$fitted; resid <- fit$resid
  w <- rep(1, n); s <- 1
  for (it in seq_len(as.integer(n_iter))) {
    med <- .s4_median(resid)
    s <- .s4_median(abs(resid - med)) / 0.6744897501960817
    if (s <= 0) s <- 1
    u <- resid / (c * s)
    w <- ifelse(abs(u) < 1, (1 - u * u)^2, 0)
    Xw <- Xm * sqrt(w)
    yw <- yv * sqrt(w)
    beta <- .t1_lstsq(Xw, yw)$beta
    fitted <- as.numeric(Xm %*% beta)
    resid <- yv - fitted
  }
  .t1_result(estimate = beta, scale = s, weights = w, fitted = fitted,
             resid = resid, n = n,
             method = "Biweight IRLS robust regression")
}
