# SPDX-License-Identifier: AGPL-3.0-or-later

#' Ordinary least squares fitting of a semivariogram model
#'
#' Minimises \eqn{\sum_m \{\hat\gamma(h_m) - \gamma(h_m; \theta)\}^2}, which is
#' the generalized sum of squares (4.31) under the simplification
#' \eqn{R = \phi I} named in the text immediately after eq (4.34). OLS
#' therefore ignores both the correlation among the empirical semivariogram
#' ordinates and their unequal dispersion. Schabenberger & Gotway report
#' Zimmerman and Zimmerman's (1991) finding that OLS and WLS perform "more or
#' less equally well", and that the greater loss of efficiency comes from
#' ignoring the correlations rather than from preferring OLS to WLS.
#'
#' @param empirical_variogram A list with `lags`, `gamma` and optionally
#'   `counts`, or a matrix of those columns.
#' @param variogram_model One of "exponential", "gaussian", "spherical".
#' @return A list with `nugget`, `partial_sill`, `sill`, `range`,
#'   `objective`, `converged`, `n_lags` and `fitted`.
#' @references Schabenberger Ch 4, Sec 4.5.1
#' @export
spols <- function(empirical_variogram, variogram_model = "exponential") {
  ev <- .schab_as_empirical_variogram(empirical_variogram)
  fit <- .schab_fit_semivariogram(ev$lags, ev$gamma, ev$counts,
                                  model = variogram_model, kind = "ols")
  # Empty lag classes carry NA lags; evaluate the model only where the lag is
  # defined and leave the rest NA, which is what the Python arm returns.
  fitted <- rep(NA_real_, length(ev$lags))
  defined <- is.finite(ev$lags)
  fitted[defined] <- .sp_semivariogram(ev$lags[defined], fit$nugget,
                                       fit$partial_sill, fit$range,
                                       variogram_model)
  list(nugget = fit$nugget, partial_sill = fit$partial_sill,
       sill = fit$nugget + fit$partial_sill, range = fit$range,
       objective = fit$objective, converged = fit$converged,
       n_lags = length(ev$lags), fitted = fitted,
       model = variogram_model, method = "ordinary least squares")
}
