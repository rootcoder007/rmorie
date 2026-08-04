# SPDX-License-Identifier: AGPL-3.0-or-later
#' Maximum likelihood fit of the additive logistic normal distribution.
#'
#' Formula: muhat = mean of alr(x_r);  Sigmahat = sample covariance of alr(x_r)
#'
#' @param X One composition per row; all parts strictly positive.
#' @param ref 1-based index of the reference part for the alr transform; the default D uses the last part.
#' @param ddof Divisor correction for the covariance: 1 gives the unbiased n - 1 divisor, 0 gives the maximum likelihood n divisor.
#'
#' @return List with ``mu``, ``Sigma``, ``ref``, ``loglik``, ``n``, ``D``.
#' @references Aitchison, J. (1986), The Statistical Analysis of Compositional Data, Chapman and Hall, is this shelf's primary book and is NOT in the reference library, so it could not be read.  The log-ratio algebra and the additive logistic normal law were taken instead from Mateu-Figueras, G., Pawlowsky-Glahn, V. and Egozcue, J. J., The normal distribution in some constrained sample spaces, arXiv:0802.2643 (published as SORT 37(1):29-56, 2013), Sects. 4.1 and 4.3, which attribute the law to Aitchison (1982, 1986); that paper was FETCHED and is archived in the reference library with a row in EXTERNAL_SOURCES.md.  Because a composition is additive logistic normal exactly when its alr transform is multivariate normal, and the alr map is a bijection that does not depend on the parameters, the maximum likelihood estimates of mu and Sigma are the ordinary multivariate normal estimates computed on the transformed data.  The reported ``loglik`` is the log-likelihood on the SIMPLEX, so it includes the sum of the log-Jacobians -sum_r sum_i log x_ri; Sect. 4.3 eq (15) prints the classical logistic normal density in ilr coordinates with Jacobian (sqrt(D) x_1 x_2 ... x_D)^-1.  In the alr coordinates used here the sqrt(D) contributed by the ilr basis is absent, giving (x_1 x_2 ... x_D)^-1.  That factor was re-derived rather than assumed: with y_i = log(x_i/x_D) and free coordinates x_1..x_{D-1}, dy/dx = diag(1/x_i) + (1/x_D) 1 1', whose determinant is (prod_{i<D} 1/x_i)(1 + (1 - x_D)/x_D) = 1 / prod_{i=1}^{D} x_i.  With ``ddof`` = 1 the covariance is the unbiased estimate rather than the MLE, so ``loglik`` is then not the maximised value.
#' @export
Lognormfit <- function(X, ref = NULL, ddof = 1L) {
  Xm <- .t1_mat(X); n <- nrow(Xm); D <- ncol(Xm)
  if (n < 2L) stop("the fit needs at least two compositions")
  if (D < 2L) stop("the logistic normal needs at least two parts")
  if (any(Xm <= 0)) stop("compositions must be strictly positive")
  k <- if (is.null(ref)) D else as.integer(ref)
  if (k < 1L || k > D) stop("ref must be a 1-based part index")
  dd <- as.integer(ddof)
  if (!(dd %in% c(0L, 1L))) stop("ddof must be 0 or 1")
  if (n - dd <= 0L) stop("not enough compositions for this ddof")
  idx <- setdiff(seq_len(D), k)
  P <- Xm / rowSums(Xm)
  Y <- log(P[, idx, drop = FALSE]) - log(P[, k])
  p <- D - 1L
  mu <- colMeans(Y)
  Yc <- sweep(Y, 2, mu, "-")
  Sg <- (t(Yc) %*% Yc) / (n - dd)
  logdet <- 2 * sum(log(diag(t(chol(Sg)))))
  ll <- 0
  for (r in seq_len(n)) {
    y <- as.numeric(Yc[r, ])
    q <- as.numeric(t(y) %*% solve(Sg, y))
    ll <- ll - 0.5 * p * log(2 * pi) - 0.5 * logdet - sum(log(P[r, ])) - 0.5 * q
  }
  .t1_result(mu = mu, Sigma = Sg, ref = k, loglik = ll, n = n, D = D,
             method = "Additive logistic normal maximum likelihood fit")
}
