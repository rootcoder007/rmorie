# SPDX-License-Identifier: AGPL-3.0-or-later
#' Wasserstein distance between Gaussians, without solving anything
#'
#' The Gaussian family is closed under optimal transport: the optimal map
#' is affine and the cost splits into a mean part and a covariance part,
#' the latter being the Bures metric. This is the one multivariate case
#' with a closed form, which makes it the natural anchor for every
#' numerical transport solver.
#'
#' Formula: \code{W_2^2 = ||m1 - m2||^2 + tr(S1 + S2 - 2 (S1^{1/2} S2
#' S1^{1/2})^{1/2})} -- Peyre and Cuturi (2019) eq. (2.41)-(2.42), p. 34;
#' Olkin and Pukelsheim (1982).
#'
#' @param mu1,mu2 Mean vectors of length d.
#' @param Sigma1,Sigma2 Covariance matrices, symmetric positive
#'   semi-definite.
#' @return List with \code{W2}, \code{W2_sq}, \code{mean_part},
#'   \code{bures_sq}, \code{d}.
#' @references Olkin, I. and Pukelsheim, F. (1982). Linear Algebra and its
#'   Applications 48:257-263. \doi{10.1016/0024-3795(82)90112-4}.
#' @export
Otwsg <- function(mu1, Sigma1, mu2, Sigma2) {
  a <- as.numeric(mu1)
  b <- as.numeric(mu2)
  w2sq <- .ot_w2gauss(mu1, Sigma1, mu2, Sigma2)
  mp <- sum((a - b)^2)
  .t1_result(W2 = sqrt(w2sq), W2_sq = w2sq, mean_part = mp,
             bures_sq = w2sq - mp, d = length(a),
             method = "Gaussian 2-Wasserstein distance (Bures)")
}
