# SPDX-License-Identifier: AGPL-3.0-or-later
#' Logistic regression log-likelihood
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer,
#' volume [Pages 71-108], Chapter 3, Section 3.7, p. 99, read as a rendered
#' page.  The chapter models the response as Bernoulli with
#' p(x_i; beta) = exp(eta_i)/(1 + exp(eta_i)), eta_i = beta_0 + x_i' beta_0,
#' and writes
#'   l(beta; y) = sum_i y_i eta_i - sum_i log(1 + exp(eta_i)),
#' with gradient X'[y - p(X; beta)] and Hessian -X' W X,
#' W = Diag(p(1-p)).
#'
#' The sum-of-logs form and the chapter's form are the same function; the
#' chapter's is used because it does not lose precision when p underflows to
#' 0 or overflows to 1, and the log1p guard keeps it finite for any eta.
#'
#' @param y binary response, entries 0 or 1.
#' @param X n-by-p design matrix; no intercept column is added.
#' @param beta coefficient vector of length p.
#' @return list: estimate, loglik, p, gradient, n, method.
#' @keywords internal
#' @examples
#' Lgobj(c(1, 0), matrix(1, 2, 1), 0)$loglik
#' @export
Lgobj <- function(y, X, beta) {
  yy <- .s03vec(y)
  XX <- .s03mat(X)
  bb <- .s03vec(beta)
  n <- length(yy)
  if (n == 0L) stop("logistic_log_likelihood: y is empty")
  if (nrow(XX) != n) stop("logistic_log_likelihood: X has a different number of rows than y")
  p_ <- ncol(XX)
  if (length(bb) != p_) stop("logistic_log_likelihood: beta does not match the columns of X")
  for (v in yy) if (v != 0 && v != 1) stop("logistic_log_likelihood: y must be 0 or 1")
  log1pexp <- function(z) if (z > 0) z + log1p(exp(-z)) else log1p(exp(z))
  ll <- 0
  p <- numeric(n)
  grad <- numeric(p_)
  for (i in seq_len(n)) {
    eta <- 0
    for (j in seq_len(p_)) eta <- eta + XX[i, j] * bb[j]
    ll <- ll + yy[i] * eta - log1pexp(eta)
    pi_ <- .s03sigmoid(eta)
    p[i] <- pi_
    r <- yy[i] - pi_
    for (j in seq_len(p_)) grad[j] <- grad[j] + XX[i, j] * r
  }
  list(estimate = ll, loglik = ll, p = p, gradient = grad, n = n,
       method = "l(beta;y) = sum y_i eta_i - sum log(1+exp(eta_i)), Chapter 3 Sect. 3.7")
}
