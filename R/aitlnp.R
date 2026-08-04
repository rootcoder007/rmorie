# SPDX-License-Identifier: AGPL-3.0-or-later
#' Density of the additive logistic normal distribution on the simplex.
#'
#' Formula: f(x) = (2 pi)^(-(D-1)/2) |Sigma|^(-1/2) (prod_i x_i)^-1 exp( -0.5 (alr(x) - mu)' Sigma^-1 (alr(x) - mu) )
#'
#' @param x Composition with strictly positive parts, length D.
#' @param mu Mean of the additive log-ratio coordinates, length D - 1.
#' @param Sigma Covariance of the additive log-ratio coordinates; must be positive definite.
#'
#' @return List with ``density``, ``log_density``, ``alr``, ``quadratic_form``, ``log_jacobian``, ``D``.
#' @references Aitchison, J. (1986), The Statistical Analysis of Compositional Data, Chapman and Hall, is this shelf's primary book and is NOT in the reference library, so it could not be read.  The log-ratio algebra and the additive logistic normal law were taken instead from Mateu-Figueras, G., Pawlowsky-Glahn, V. and Egozcue, J. J., The normal distribution in some constrained sample spaces, arXiv:0802.2643 (published as SORT 37(1):29-56, 2013), Sects. 4.1 and 4.3, which attribute the law to Aitchison (1982, 1986); that paper was FETCHED and is archived in the reference library with a row in EXTERNAL_SOURCES.md.  A composition is additive logistic normal when its alr transform is multivariate normal.  Sect. 4.3 eq (15) prints the classical logistic normal density in ilr coordinates with Jacobian (sqrt(D) x_1 x_2 ... x_D)^-1.  In the alr coordinates used here the sqrt(D) contributed by the ilr basis is absent, giving (x_1 x_2 ... x_D)^-1.  That factor was re-derived rather than assumed: with y_i = log(x_i/x_D) and free coordinates x_1..x_{D-1}, dy/dx = diag(1/x_i) + (1/x_D) 1 1', whose determinant is (prod_{i<D} 1/x_i)(1 + (1 - x_D)/x_D) = 1 / prod_{i=1}^{D} x_i.  The composition is closed to sum 1 before the density is evaluated, because the density is with respect to the simplex of unit total; the log-ratios are unchanged by that closure, only the Jacobian factor depends on it.
#' @export
Lognormpdf <- function(x, mu, Sigma) {
  x <- .t1_vec(x); D <- length(x)
  if (D < 2L) stop("the logistic normal needs at least two parts")
  if (any(x <= 0)) stop("compositions must be strictly positive")
  x <- x / sum(x)
  mu <- .t1_vec(mu)
  if (length(mu) != D - 1L) stop("mu must have D - 1 entries")
  Sg <- .t1_mat(Sigma)
  if (nrow(Sg) != D - 1L || ncol(Sg) != D - 1L)
    stop("Sigma must be (D - 1) by (D - 1)")
  L <- t(chol(Sg))
  logdet <- 2 * sum(log(diag(L)))
  a <- log(x[seq_len(D - 1L)]) - log(x[D])
  y <- a - mu
  q <- as.numeric(t(y) %*% solve(Sg, y))
  lj <- -sum(log(x))
  ll <- -0.5 * (D - 1) * log(2 * pi) - 0.5 * logdet + lj - 0.5 * q
  .t1_result(density = exp(ll), log_density = ll, alr = a,
             quadratic_form = q, log_jacobian = lj, D = D,
             method = "Additive logistic normal density")
}
