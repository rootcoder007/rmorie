# SPDX-License-Identifier: AGPL-3.0-or-later
#' Leave-one-out cross-validation of a kriging model.
#'
#' Kriging honours the data, so in-sample residuals are identically zero
#' and carry no information about model fit. Cross-validation removes
#' each observation in turn, predicts it from the rest, and reports the
#' mean squared prediction error. The standardised residuals should have
#' mean near zero and variance near one if the covariance model is right;
#' their variance is the diagnostic that catches a mis-specified sill.
#'
#' @param coords Observation coordinates (n by d).
#' @param z Observed values, length n.
#' @param cov_model List with `model`, `nugget`, `sill`, `range`.
#' @param mu Known mean; the sample mean of the retained points when NULL.
#' @return Named list: mspe, rmspe, me, residuals, standardised,
#'   std_variance, n.
#' @references Schabenberger & Gotway (2005), Ch 5.
#' @examples
#' co <- matrix(runif(40), 20, 2) * 5
#' spkfnn(co, rnorm(20), list(model = "exponential", sill = 1, range = 2))
#' @export
spkfnn <- function(coords, z, cov_model = NULL, mu = NULL) {
  coords <- as.matrix(coords)
  z <- as.numeric(z)
  n <- length(z)
  if (nrow(coords) != n) {
    stop("`coords` and `z` must have the same number of rows")
  }
  if (n < 3) stop("leave-one-out cross-validation needs at least 3 points")
  resid <- numeric(n)
  sdv <- numeric(n)
  for (i in seq_len(n)) {
    keep <- seq_len(n) != i
    r <- .sp_simple_kriging(coords[keep, , drop = FALSE], z[keep],
                            coords[i, , drop = FALSE], cov_model, mu)
    resid[i] <- z[i] - r$prediction[1]
    sdv[i] <- sqrt(max(r$variance[1], 1e-300))
  }
  std <- resid / sdv
  list(mspe = mean(resid^2), rmspe = sqrt(mean(resid^2)), me = mean(resid),
       residuals = resid, standardised = std,
       std_variance = stats::var(std) * (n - 1) / n, n = n)
}
