# SPDX-License-Identifier: AGPL-3.0-or-later

#' Lognormal kriging
#'
#' With \eqn{Y(s) = \log Z(s)} Gaussian, the obvious predictor
#' \eqn{\exp\{p_{sk}(Y;s_0)\}} is BIASED for \eqn{Z(s_0)}. Applying the
#' Aitchison and Brown (1957) lognormal moments twice gives the
#' bias-corrected form, eq (5.54),
#' \eqn{p_{slk}(Z;s_0) = \exp\{p_{sk}(Y;s_0) + \sigma^2_{sk}(Y;s_0)/2\}},
#' which the text notes equals \eqn{E[Z(s_0) \mid Z]}.
#'
#' The correction uses the SIMPLE KRIGING variance, not the process variance;
#' the two differ by \eqn{c'\Sigma^{-1}c}, the variance of the predictor, so
#' using the latter over-corrects, and by the most where kriging is most
#' confident.
#'
#' @param coords Matrix of sampling locations.
#' @param z Observed values on the original scale; strictly positive.
#' @param target Prediction location.
#' @param cov_model Covariance as a function of lag, on the log scale.
#' @param mu Known mean of Y; defaults to the mean of log z.
#' @return A list with `prediction`, `naive_prediction`, `log_prediction`,
#'   `log_variance` and `bias_factor`.
#' @references Schabenberger Ch 5, Sec 5.6.1
#' @export
#' @examples
#' splgk(coords = c(1, 2, 3, 4, 5, 6, 7, 8), z = c(1, 2, 3, 4, 5, 6, 7, 8), target = c(1, 2, 3, 4, 5, 6, 7, 8))
splgk <- function(coords, z, target, cov_model = NULL, mu = NULL) {
  z <- as.numeric(z)
  if (any(z <= 0)) {
    stop("`z` must be strictly positive for a lognormal model", call. = FALSE)
  }
  y <- log(z)
  kr <- .sp_simple_kriging(coords, y, matrix(as.numeric(target), nrow = 1),
                           cov_model = cov_model, mu = mu)
  pred_y <- as.numeric(kr$prediction)[1]
  var_y <- as.numeric(kr$variance)[1]
  naive <- exp(pred_y)
  list(prediction = exp(pred_y + 0.5 * var_y), naive_prediction = naive,
       log_prediction = pred_y, log_variance = var_y,
       bias_factor = exp(0.5 * var_y),
       method = "lognormal (simple) kriging")
}
