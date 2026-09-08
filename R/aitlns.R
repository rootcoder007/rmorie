# SPDX-License-Identifier: AGPL-3.0-or-later
#' Draw compositions from a logistic-normal via the alr inverse
#'
#' Sampling is done in the unconstrained coordinates and mapped back;
#' sampling the parts directly and renormalising gives a different
#' distribution. The Cholesky factor is used because it is unique for a
#' positive-definite Sigma, so both arms map the same normal draws to the
#' same compositions.
#'
#' Formula: Z ~ N(0, I_\{D-1\}); Y = mu + L Z with L L' = Sigma;
#'   X = alr^-1(Y) = C(exp(Y_1), ..., exp(Y_\{D-1\}), 1)
#'
#' @param mu Mean of the alr coordinates, length D-1.
#' @param Sigma Positive-definite covariance of the alr coordinates.
#' @param n Number of compositions drawn.
#' @param seed Seed for the pinned generator.
#' @param total Constant each composition sums to.
#' @return List with \code{sample}, \code{alr}, \code{center},
#'   \code{mean_alr}, \code{n}, \code{D}.
#' @references Aitchison (1986), The Statistical Analysis of
#'   Compositional Data, Chapter 6. The reference part is the LAST,
#'   matching aitalr and aitalri.
#' @export
#' @examples
#' Lgtnsim(mu = 5L, Sigma = 0.5, n = 5L)
Lgtnsim <- function(mu, Sigma, n, seed = 1, total = 1) {
  mu <- .t1_vec(mu)
  p <- length(mu)
  if (p < 1L) stop("mu must have at least one entry")
  S <- as.matrix(Sigma)
  if (nrow(S) != p || ncol(S) != p) stop("Sigma must be (D-1) x (D-1)")
  n <- as.integer(n)
  if (n < 1L) stop("n must be at least 1")
  L <- t(chol(S))
  g <- .t1_lcg(seed)
  D <- p + 1L
  k <- as.numeric(total)
  Y <- matrix(0, n, p)
  Xs <- matrix(0, n, D)
  for (t in seq_len(n)) {
    z <- vapply(seq_len(p), function(i) g$norm(), 0)
    y <- as.numeric(mu + L %*% z)
    Y[t, ] <- y
    e <- c(exp(y), 1)
    Xs[t, ] <- k * e / sum(e)
  }
  e <- c(exp(mu), 1)
  .t1_result(
    sample = Xs, alr = Y, center = k * e / sum(e),
    mean_alr = colMeans(Y), n = as.numeric(n), D = as.numeric(D),
    method = "Logistic-normal sampling via alr^-1"
  )
}
