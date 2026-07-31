# SPDX-License-Identifier: AGPL-3.0-or-later

#' Inverse distance weighted interpolation
#'
#' Bivand, Pebesma and Gomez-Rubio (2013), Sec. 8.3.1 -- NOT a Schabenberger
#' method. Inverse distance weighting appears in \emph{Statistical Methods for
#' Spatial Data Analysis} only in the subject index, so this module is
#' grounded in its own primary source.
#'
#' \eqn{\hat Z(s_0) = \sum_i w(s_i) Z(s_i) / \sum_i w(s_i)} with
#' \eqn{w(s_i) = \|s_i - s_0\|^{-p}}, p "an inverse distance weighting power,
#' defaulting to 2".
#'
#' Three properties the text states, all asserted in the suites: if the
#' target coincides with an observation the observed value is returned "to
#' avoid infinite weights"; for large p the method converges to
#' one-nearest-neighbour interpolation; and "inverse distance does not
#' provide prediction error variances", so `variance` is NULL rather than a
#' fabricated number.
#'
#' @param coords Matrix of observation locations.
#' @param z Observed values.
#' @param target Prediction location.
#' @param power Inverse distance power; non-negative. p = 0 gives the
#'   unweighted mean, the correct limit rather than a special case.
#' @return A list with `prediction`, `variance` (always NULL), `weights`,
#'   `power` and `exact_hits`.
#' @references Bivand, R. S., Pebesma, E., and Gomez-Rubio, V. (2013)
#'   Applied Spatial Data Analysis with R, 2nd ed., Springer, Sec. 8.3.1.
#' @export
spmidw <- function(coords, z, target, power = 2) {
  coords <- as.matrix(coords)
  z <- as.numeric(z)
  target <- as.numeric(target)
  power <- as.numeric(power)
  if (power < 0) stop("`power` must be non-negative", call. = FALSE)
  if (nrow(coords) != length(z)) {
    stop("`coords` and `z` must have the same number of rows", call. = FALSE)
  }
  if (ncol(coords) != length(target)) {
    stop("`coords` and `target` must have the same dimension", call. = FALSE)
  }
  d <- sqrt(rowSums((coords - matrix(target, nrow(coords), ncol(coords),
                                     byrow = TRUE))^2))
  hit <- d == 0
  if (any(hit)) {
    # The coincidence rule the text states, rather than an infinite weight.
    w <- numeric(length(d))
    w[hit] <- 1 / sum(hit)
    return(list(prediction = mean(z[hit]), variance = NULL, weights = w,
                power = power, exact_hits = TRUE,
                method = "inverse distance weighting"))
  }
  w <- d^(-power)
  w <- w / sum(w)
  list(prediction = sum(w * z), variance = NULL, weights = w, power = power,
       exact_hits = FALSE, method = "inverse distance weighting")
}
