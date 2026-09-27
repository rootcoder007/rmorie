# SPDX-License-Identifier: AGPL-3.0-or-later
#' Support-vector regression with the epsilon-insensitive loss
#'
#' Minimises C sum (|y_i - f(x_i)| - epsilon)_+ + |beta|^2 / 2 through the
#' 2N-variable dual solved by the same SMO as the classifiers (ESL eqs
#' 12.35-12.39, the libsvm epsilon-regression formulation);
#' f(x) = sum_i coef_i K(x, x_i) + b.
#'
#' @param X Predictors, n by p.
#' @param y Response.
#' @param C Cost.
#' @param epsilon Tube half-width.
#' @param kernel "linear", "rbf", "poly" or "sigmoid".
#' @param gamma,degree,coef0 Kernel parameters.
#' @param newdata Points to predict.
#' @param tol SMO tolerance.
#' @return Named list: coef, b, fitted, prediction, n_support, converged.
#' @references Smola, A. & Scholkopf, B. (2004). Statistics and Computing 14,
#'   199-222.
#' @examples
#' X <- cbind(sin(1:40), cos(3 * (1:40)))
#' morie_esl_svr(X, 2 * X[, 1] - X[, 2], epsilon = 0.2, kernel = "linear")$b
#' @export
morie_esl_svr <- function(X, y, C = 1, epsilon = 0.1, kernel = "rbf", gamma = NULL, degree = 3, coef0 = 1,
                          newdata = NULL, tol = 1e-3) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  n <- length(y)
  if (nrow(X) != n || C <= 0 || epsilon < 0) stop("need matching X and y, C > 0 and epsilon >= 0", call. = FALSE)
  K <- .morie_kernel_matrix(X, kernel = kernel, gamma = gamma, degree = degree, coef0 = coef0)
  K2 <- rbind(cbind(K, K), cbind(K, K))
  fit <- .morie_smo(K2, c(rep(1, n), rep(-1, n)), C = C, tol = tol, p = c(epsilon - y, epsilon + y))
  cf <- fit$alpha[seq_len(n)] - fit$alpha[n + seq_len(n)]
  pred <- NULL
  if (!is.null(newdata)) {
    pred <- drop(.morie_kernel_matrix(rbind(newdata), X, kernel = kernel, gamma = gamma, degree = degree,
                                      coef0 = coef0) %*% cf) + fit$b
  }
  list(coef = cf, b = fit$b, fitted = drop(K %*% cf) + fit$b, prediction = pred, n_support = sum(abs(cf) > 1e-12),
       converged = fit$converged)
}
