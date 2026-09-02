# SPDX-License-Identifier: AGPL-3.0-or-later

#' Normal-scores transform
#'
#' \eqn{\phi^{-1}(z) = \Phi^{-1}(F(z))}, eq (5.60): "matching the percentiles
#' of the data to those of a standard Gaussian distribution". The
#' \eqn{(i-1/2)/n} plotting position is used so no observation maps to an
#' infinite score, which plain \eqn{i/n} would do to the largest value.
#'
#' @param z Numeric vector.
#' @return Numeric vector of Gaussian scores.
#' @references Schabenberger Ch 5, Sec 5.6.2, eq (5.60)
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_normal_scores(V)
morie_normal_scores <- function(z) {
  z <- as.numeric(z)
  n <- length(z)
  if (n == 0L) stop("`z` must be non-empty", call. = FALSE)
  .morie_normal_quantile((rank(z, ties.method = "first") - 0.5) / n)
}

#' Anamorphosis transform
#'
#' \eqn{\phi(y) = F^{-1}(\Phi(y))}, the inverse of [morie_normal_scores()],
#' eq (5.60). Values outside the observed range are clamped to it, since an
#' empirical CDF carries no information beyond its support.
#'
#' @param z Numeric vector defining the empirical distribution.
#' @param y_new Gaussian scores to map back onto the data scale.
#' @return Numeric vector on the scale of `z`.
#' @references Schabenberger Ch 5, Sec 5.6.2, eq (5.60)
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_anamorphosis(V, V)
morie_anamorphosis <- function(z, y_new) {
  zs <- sort(as.numeric(z))
  n <- length(zs)
  scores <- .morie_normal_quantile((seq_len(n) - 0.5) / n)
  stats::approx(scores, zs, xout = as.numeric(y_new), rule = 2)$y
}

#' Trans-Gaussian kriging
#'
#' With \eqn{Z(s) = \phi(Y(s))} and Y Gaussian, the natural predictor
#' \eqn{\phi(p_{ok}(Y;s_0))} is biased. Expanding to second order about
#' \eqn{\mu_Y} and matching expectations gives eq (5.58),
#' \eqn{p_{tg} = \phi(p_{ok}) + \phi''(\mu_Y)/2\,(\sigma^2_{ok} - 2 m_Y)},
#' with mean squared prediction error eq (5.59),
#' \eqn{[\phi'(\mu_Y)]^2 \sigma^2_{ok}}.
#'
#' \eqn{m_Y} enters with the sign convention of eq (5.20); a flipped
#' multiplier would move every prediction with no other symptom.
#'
#' @param coords Matrix of sampling locations.
#' @param z Observed values on the Y (Gaussian) scale.
#' @param target Prediction location.
#' @param phi,dphi,d2phi The transformation and its first two derivatives.
#' @param semivariogram_fn gamma(h) for the Y scale.
#' @return A list with `prediction`, `naive_prediction`, `correction`,
#'   `mspe`, `kriging_variance`, `lagrange` and `mu_y`.
#' @references Schabenberger Ch 5, Sec 5.6.2
#' @export
sptgk <- function(coords, z, target, phi, dphi, d2phi, semivariogram_fn) {
  z <- as.numeric(z)
  kr <- .sp_ordinary_kriging(coords, z, target, semivariogram_fn)
  mu_y <- mean(z)
  naive <- phi(kr$prediction)
  correction <- 0.5 * d2phi(mu_y) * (kr$variance - 2 * kr$lagrange)
  list(prediction = naive + correction, naive_prediction = naive,
       correction = correction, mspe = dphi(mu_y)^2 * kr$variance,
       kriging_variance = kr$variance, lagrange = kr$lagrange, mu_y = mu_y,
       method = "trans-Gaussian (ordinary) kriging")
}
