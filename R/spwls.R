# SPDX-License-Identifier: AGPL-3.0-or-later

#' Weighted least squares fitting of a semivariogram model
#'
#' Minimises eq (4.34),
#' \eqn{\sum_m |N(h_m)| / (2 \gamma(h_m;\theta)^2)
#'      \{\hat\gamma(h_m) - \gamma(h_m;\theta)\}^2},
#' the generalized sum of squares (4.31) with \eqn{R(\theta)} replaced by the
#' diagonal \eqn{W(\theta)} whose entries are Cressie's (1985) approximation
#' (4.33), \eqn{Var\[\hat\gamma(h_m)\] \approx 2\gamma(h_m,\theta)^2/|N(h_m)|}.
#' Dividing through by \eqn{2\gamma^2} gives the equivalent and more familiar
#' \eqn{(1/2)\sum_m |N(h_m)| \[\hat\gamma(h_m)/\gamma(h_m;\theta) - 1\]^2}; the
#' two differ by a constant factor and so share a minimiser.
#'
#' The weights are functions of theta, so this is a re-weighted rather than a
#' fixed-weight fit. Note the book's own caveat: because the off-diagonal
#' entries of \eqn{R(\theta)} are appreciable, WLS is a poor approximation to
#' (4.31), and the efficiency lost by ignoring those correlations exceeds
#' anything gained over OLS.
#'
#' @param empirical_variogram A list with `lags`, `gamma` and optionally
#'   `counts`, or a matrix of those columns. Counts matter here: they are the
#'   \eqn{|N(h_m)|} of eq (4.34).
#' @param variogram_model One of "exponential", "gaussian", "spherical".
#' @return A list with `nugget`, `partial_sill`, `sill`, `range`,
#'   `objective`, `converged`, `n_lags` and `fitted`.
#' @references Schabenberger Ch 4, Sec 4.5.1
#' @export
#' @examples
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' spwls(D)
spwls <- function(empirical_variogram, variogram_model = "exponential") {
  ev <- .schab_as_empirical_variogram(empirical_variogram)
  fit <- .schab_fit_semivariogram(ev$lags, ev$gamma, ev$counts,
                                  model = variogram_model, kind = "wls")
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
       model = variogram_model, method = "weighted least squares")
}
