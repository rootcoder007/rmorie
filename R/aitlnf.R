# SPDX-License-Identifier: AGPL-3.0-or-later
#' Fit the logistic-normal by transforming, then fitting a normal
#'
#' The alr is a bijection onto R^\{D-1\}, so the MLE on the simplex is the
#' multivariate-normal MLE of the transformed data; no optimisation is
#' needed. The parameter-free Jacobian is included in \code{loglik} so
#' that value is comparable with aitlnp.
#'
#' Formula: Y = alr(X); muhat = mean(Y);
#'   Sigmahat = sum (Y_k - muhat)(Y_k - muhat)' / (n - ddof)
#'
#' @param X One composition per row; strictly positive.
#' @param ddof Divisor correction: 1 for the unbiased covariance, 0 for
#'   the MLE.
#' @return List with \code{mu}, \code{Sigma}, \code{center},
#'   \code{loglik}, \code{n}, \code{D}.
#' @references Aitchison (1986), The Statistical Analysis of
#'   Compositional Data, Chapter 7.
#' @export
#' @examples
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' Lgtnfit(D)
Lgtnfit <- function(X, ddof = 1) {
  X <- as.matrix(X)
  n <- nrow(X)
  D <- ncol(X)
  if (n < 2L) stop("at least two compositions are required")
  if (D < 2L) stop("a composition needs at least two parts")
  if (any(X <= 0)) stop("compositions must be strictly positive")
  dd <- as.integer(ddof)
  if (n - dd <= 0L) stop("not enough observations for this ddof")
  Y <- log(X[, seq_len(D - 1L), drop = FALSE]) - log(X[, D])
  p <- D - 1L
  mu <- colMeans(Y)
  Yc <- sweep(Y, 2, mu, "-")
  S <- crossprod(Yc) / (n - dd)
  dimnames(S) <- NULL
  e <- c(exp(mu), 1)
  cen <- e / sum(e)
  L <- chol(S)
  logdet <- 2 * sum(log(diag(L)))
  ll <- 0
  Sinv <- solve(S)
  for (k in seq_len(n)) {
    dv <- Yc[k, ]
    q <- sum(dv * (Sinv %*% dv))
    ll <- ll - 0.5 * p * log(2 * pi) - 0.5 * logdet -
      sum(log(X[k, ])) - 0.5 * q
  }
  .t1_result(
    mu = mu, Sigma = S, center = cen, loglik = ll,
    n = as.numeric(n), D = as.numeric(D),
    method = "Logistic-normal MLE via the alr transform"
  )
}
