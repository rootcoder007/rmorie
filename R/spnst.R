# SPDX-License-Identifier: AGPL-3.0-or-later
#' Parametric non-stationary correlation: the point-source model
#'
#' eq (8.1): \eqn{Corr[Z(s_i),Z(s_j)] = \exp\{-\theta_1 ||s_i-s_j||
#' \exp\{\theta_2 |c_i-c_j| + \theta_3 \min[c_i,c_j]\}\}}, with \eqn{c_i}
#' the distance from site i to the point source. Non-stationary because a
#' pair's correlation depends on where it sits relative to the source, not
#' only on its separation. With \eqn{\theta_2 = \theta_3 = 0} it collapses
#' to the exponential model with practical range \eqn{3/\theta_1}.
#'
#' Sec. 8.2.1 states that \eqn{\theta_1 > 0} and
#' \eqn{\theta_2, \theta_3 \ge 0} are necessary but *not* sufficient for
#' positive semi-definiteness and that the eigenvalues must be examined, so
#' `min_eigenvalue` and `valid` are always returned and an invalid model is
#' flagged. A parameter search finds conforming sets with minimum eigenvalue
#' -0.21, so the warning is not hypothetical.
#'
#' The placeholder this replaces printed
#' \eqn{C(s_1,s_2) = \sigma(s_1)\sigma(s_2)\rho(s_1,s_2)}, a generic
#' heteroscedastic form that does not appear in Sec. 8.2.1.
#'
#' @param coords Site coordinates, (n, d).
#' @param z Ignored; the model is a covariance structure, not a fit.
#' @param source The point source c; defaults to the centroid of `coords`.
#' @param theta1 Positive decay parameter.
#' @param theta2,theta3 Non-negative source-effect parameters.
#' @param sill Positive scale carrying the correlation into a covariance.
#' @param anisotropy,source_anisotropy Optional d by d matrices A and A_c of
#'   p. 423.
#' @return A list with `nonstationary_cov`, `correlation`,
#'   `source_distance`, `separation`, `min_eigenvalue`, `valid`,
#'   `practical_range`, `theta`, `sill`, `source`, and `warning` when the
#'   matrix is not positive semi-definite.
#' @references Schabenberger Ch 8, Sec 8.2.1, eq (8.1), pp. 422-423.
#'   Hughes-Oliver, Gonzalez-Farias, Lu and Chen (1998), Statistics &
#'   Probability Letters 40:267-278.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' spnst(V)
spnst <- function(coords, z = NULL, source = NULL, theta1 = 1, theta2 = 0,
                  theta3 = 0, sill = 1, anisotropy = NULL,
                  source_anisotropy = NULL) {
  s <- as.matrix(coords)
  if (is.null(source)) source <- colMeans(s)
  res <- .schab_point_source_corr(s, source, theta1, theta2, theta3,
                                  anisotropy = anisotropy,
                                  source_anisotropy = source_anisotropy)
  sill <- as.numeric(sill)
  if (sill <= 0) stop("sill must be positive")
  res$nonstationary_cov <- sill * res$correlation
  res$sill <- sill
  res$practical_range <- .schab_practical_range(theta1)
  res$source <- as.numeric(source)
  res
}

