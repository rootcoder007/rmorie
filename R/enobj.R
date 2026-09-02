# SPDX-License-Identifier: AGPL-3.0-or-later
#' Elastic net penalized residual sum of squares.
#'
#' Formula: PRSS(beta, lambda, alpha) = RSS(beta) + lambda \[ (1 - alpha)/2 * sum_j b_j^2 + alpha * sum_j |b_j| \]
#'
#' @param X Design matrix, one record per row.
#' @param y Response vector of length n.
#' @param beta Coefficient vector.
#' @param lam Regularization parameter lambda; must be non-negative.
#' @param alpha Mixing parameter in \[0, 1\]: 0 is the ridge penalty, 1 is the lasso penalty.
#' @param add_intercept Treat the first entry of beta as an unpenalized intercept and prepend a column of ones to X.
#'
#' @return List with ``prss``, ``rss``, ``penalty``, ``l1``, ``l2``, ``lambda``, ``alpha``, ``n``, ``p``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 3, Sect. 3.6.2 p. 94 describes the elastic net as the combination of the ridge and lasso penalties implemented by glmnet, and Chapter 7 p. 230 defines its mixing parameter alpha: alpha = 0 gives the ridge penalty, alpha = 1 the lasso, and values in between the elastic net.  The book names those two endpoints but does not print the mixed objective itself; the (1 - alpha)/2 and alpha weights are the glmnet parameterization of Friedman, Hastie and Tibshirani (2010), Regularization Paths for Generalized Linear Models via Coordinate Descent, Journal of Statistical Software 33(1):1-22, doi:10.18637/jss.v033.i01, which is the package the book cites for this penalty.  Both chapters read from the PDFs; the mixing weights come from the cited paper, and the endpoints agree with the book: alpha = 0 reproduces MVSML Sect. 3.6.1 up to the factor 1/2 on the ridge term, alpha = 1 reproduces Sect. 3.6.2 exactly.
#' @export
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20), 10, 2)
#' y <- rnorm(10)
#' Enetobj(X, y, beta = c(0, 0.5, -0.3), lam = 0.1, alpha = 0.5)
Enetobj <- function(X, y, beta, lam, alpha, add_intercept = TRUE) {
  Xm <- .t1_mat(X)
  if (isTRUE(add_intercept)) Xm <- .t1_cbind1(Xm)
  y <- .t1_vec(y); b <- .t1_vec(beta)
  lam <- as.numeric(lam); a <- as.numeric(alpha)
  n <- nrow(Xm); p <- ncol(Xm)
  if (n != length(y)) stop("X must have one row per entry of y")
  if (length(b) != p) stop("beta must have one entry per column of the design")
  if (lam < 0) stop("lambda must be non-negative")
  if (a < 0 || a > 1) stop("alpha must lie in [0, 1]")
  rss <- sum((y - as.numeric(Xm %*% b))^2)
  start <- if (isTRUE(add_intercept)) 2L else 1L
  l2 <- sum(b[start:p]^2); l1 <- sum(abs(b[start:p]))
  pen <- lam * (0.5 * (1 - a) * l2 + a * l1)
  .t1_result(prss = rss + pen, rss = rss, penalty = pen, l1 = l1, l2 = l2,
             lambda = lam, alpha = a, n = n, p = p,
             method = "Elastic net penalized RSS, MVSML Sect. 3.6.2 / Chap. 7")
}
