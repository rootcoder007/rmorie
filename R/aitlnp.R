# SPDX-License-Identifier: AGPL-3.0-or-later
#' Additive logistic-normal density at a composition
#'
#' The Jacobian factor (prod_i x_i)^-1 runs over ALL D parts including
#' the reference one; without it the density does not integrate to one
#' over the simplex.
#'
#' Formula: f(x) = (2 pi)^\{-(D-1)/2\} |Sigma|^\{-1/2\}
#'   (prod_\{i=1\}^\{D\} x_i)^\{-1\}
#'   exp( -1/2 (alr(x) - mu)' Sigma^\{-1\} (alr(x) - mu) )
#'
#' @param x A composition with D strictly positive parts.
#' @param mu Mean of the alr coordinates, length D-1.
#' @param Sigma Covariance of the alr coordinates, positive definite.
#' @return List with \code{density}, \code{log_density}, \code{alr},
#'   \code{quadratic_form}, \code{log_jacobian}, \code{log_det},
#'   \code{D}.
#' @references Aitchison (1986), The Statistical Analysis of
#'   Compositional Data, Chapter 6. The reference part is the LAST,
#'   matching the sibling module aitalr.
#' @export
#' @examples
#' Lgtnpdf(c(0.3, 0.3, 0.4), mu = c(0, 0), Sigma = diag(2))
Lgtnpdf <- function(x, mu, Sigma) {
  x <- .t1_vec(x); D <- length(x)
  if (D < 2L) stop("a composition needs at least two parts")
  if (any(x <= 0)) stop("compositions must be strictly positive")
  mu <- .t1_vec(mu)
  if (length(mu) != D - 1L) stop("mu must have D-1 entries")
  S <- as.matrix(Sigma)
  if (nrow(S) != D - 1L || ncol(S) != D - 1L)
    stop("Sigma must be (D-1) x (D-1)")
  L <- chol(S)
  logdet <- 2 * sum(log(diag(L)))
  y <- log(x[seq_len(D - 1L)]) - log(x[D])
  dv <- y - mu
  q <- sum(dv * solve(S, dv))
  lj <- -sum(log(x))
  ld <- -0.5 * (D - 1) * log(2 * pi) - 0.5 * logdet + lj - 0.5 * q
  .t1_result(density = exp(ld), log_density = ld, alr = y,
             quadratic_form = q, log_jacobian = lj, log_det = logdet,
             D = as.numeric(D),
             method = "Additive logistic-normal density, Aitchison Chapter 6")
}
