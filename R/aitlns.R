# SPDX-License-Identifier: AGPL-3.0-or-later
#' Map standard normal draws to additive logistic normal compositions.
#'
#' Formula: x_r = alr^-1( mu + L z_r ),  Sigma = L L' the lower Cholesky factor
#'
#' @param Z Standard normal draws supplied by the caller, one row per composition.  The noise is an argument, not drawn internally, so the function is deterministic.
#' @param mu Mean of the additive log-ratio coordinates, length D - 1.
#' @param Sigma Covariance of the additive log-ratio coordinates; must be positive definite.
#' @param ref 1-based index the reference part is restored to; the default is the last position D.
#' @param total Constant kappa each returned composition sums to.
#'
#' @return List with ``compositions``, ``alr``, ``L``, ``ref``, ``n``, ``D``.
#' @references Aitchison, J. (1986), The Statistical Analysis of Compositional Data, Chapman and Hall, is this shelf's primary book and is NOT in the reference library, so it could not be read.  The log-ratio algebra and the additive logistic normal law were taken instead from Mateu-Figueras, G., Pawlowsky-Glahn, V. and Egozcue, J. J., The normal distribution in some constrained sample spaces, arXiv:0802.2643 (published as SORT 37(1):29-56, 2013), Sects. 4.1 and 4.3, which attribute the law to Aitchison (1982, 1986); that paper was FETCHED and is archived in the reference library with a row in EXTERNAL_SOURCES.md.  Sampling the additive logistic normal is sampling the multivariate normal in alr coordinates and inverting the transform, since the law is defined by exactly that construction.  The caller supplies the standard normal matrix Z so that the result is reproducible and identical in both language arms; no random number generator is touched here.
#' @export
Lognormdraw <- function(Z, mu, Sigma, ref = NULL, total = 1) {
  Zm <- .t1_mat(Z); n <- nrow(Zm); p <- ncol(Zm); D <- p + 1L
  if (n == 0L) stop("Z must have at least one row")
  mu <- .t1_vec(mu)
  if (length(mu) != p) stop("mu must have one entry per column of Z")
  Sg <- .t1_mat(Sigma)
  if (nrow(Sg) != p || ncol(Sg) != p)
    stop("Sigma must match the number of columns of Z")
  L <- t(chol(Sg))
  k <- if (is.null(ref)) D else as.integer(ref)
  if (k < 1L || k > D) stop("ref must be a 1-based part index")
  idx <- setdiff(seq_len(D), k)
  t <- as.numeric(total)
  Y <- sweep(Zm %*% t(L), 2, mu, "+")
  out <- matrix(0, n, D)
  for (r in seq_len(n)) {
    full <- numeric(D); full[idx] <- Y[r, ]
    e <- exp(full - max(full)); out[r, ] <- t * e / sum(e)
  }
  .t1_result(compositions = out, alr = Y, L = L, ref = k, n = n, D = D,
             method = "Additive logistic normal draws from supplied noise")
}
