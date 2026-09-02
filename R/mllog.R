# SPDX-License-Identifier: AGPL-3.0-or-later
#' Maximum likelihood log-likelihood of the linear regression model
#'
#' Formula: log L = -(n/2) log(2 pi) - n log(sigma) - (1/(2 sigma^2)) (y - X beta)'(y - X beta)
#'
#' @param X Design matrix, one record per row.
#' @param y Response vector of length n.
#' @param beta Coefficients; None uses the OLS solution, which is also the MLE.
#' @param sigma2 Error variance; None uses the MLE RSS/n.
#'
#' @return List with ``loglik``, ``beta``, ``sigma2``, ``rss``, ``n``, ``p``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 3, Sect. 3.3 pp. 75-76: the likelihood of the multiple linear regression model, its logarithm as written above, and the maximum likelihood estimators -- beta-hat is the OLS solution and sigma2-hat = (1/n)(y - X beta-hat)'(y - X beta-hat), which divides by n and not by n - p.  Read from the chapter PDF, not recalled.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' Mlloglik(V, D)
Mlloglik <- function(X, y, beta = NULL, sigma2 = NULL) {
  Xm <- .t1_mat(X)
  y <- .t1_vec(y)
  n <- nrow(Xm)
  p <- ncol(Xm)
  if (n != length(y)) stop("X must have one row per entry of y")
  b <- if (is.null(beta)) .t1_lstsq(Xm, y)$beta else .t1_vec(beta)
  if (length(b) != p) stop("beta must have one entry per column of X")
  res <- y - as.numeric(Xm %*% b)
  rss <- sum(res^2)
  s2 <- if (is.null(sigma2)) rss / n else as.numeric(sigma2)
  if (s2 <= 0) stop("sigma2 must be positive")
  ll <- -0.5 * n * log(2 * pi) - 0.5 * n * log(s2) - rss / (2 * s2)
  .t1_result(loglik = ll, beta = b, sigma2 = s2, rss = rss, n = n, p = p,
             method = "Gaussian ML log-likelihood, MVSML Sect. 3.3")
}
