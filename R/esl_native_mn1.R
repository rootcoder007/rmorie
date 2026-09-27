# SPDX-License-Identifier: AGPL-3.0-or-later
#' L1-penalised multinomial logistic regression
#'
#' Maximises the multinomial log-likelihood minus lambda sum_k sum_j |beta_kj| in
#' the symmetric parametrisation (ESL eqs 18.10, 18.19) by cycling over the
#' classes with IRLS quadratic approximations and lasso coordinate descent, as
#' glmnet's ungrouped multinomial. Intercepts are centred; lambda is N times
#' glmnet's.
#'
#' @param X Predictors, n by p.
#' @param g Class labels.
#' @param lambda Penalty.
#' @param max_outer,max_inner,tol Iteration controls.
#' @return Named list: intercepts, coefficients (K by p), classes, loglik,
#'   objective, prob, converged.
#' @references Friedman, J., Hastie, T. & Tibshirani, R. (2010). JSS 33(1).
#' @examples
#' i <- 1:60
#' X <- cbind(sin(i), cos(3 * i))
#' morie_esl_multinomial_l1(X, ifelse(sin(i) > 0.3, 0, ifelse(cos(3 * i) > 0, 1, 2)), 1.5)$coefficients
#' @export
morie_esl_multinomial_l1 <- function(X, g, lambda, max_outer = 500, max_inner = 10000, tol = 1e-12) {
  X <- as.matrix(X)
  cl <- sort(unique(g))
  n <- nrow(X)
  p <- ncol(X)
  K <- length(cl)
  if (length(g) != n || K < 2 || lambda <= 0) stop("need matching X and g, at least two classes and lambda > 0", call. = FALSE)
  Y <- outer(g, cl, "==") * 1
  b0 <- numeric(K)
  B <- matrix(0, K, p)
  probs <- function() {
    eta <- sweep(X %*% t(B), 2, b0, "+")
    eta <- eta - apply(eta, 1, max)
    e <- exp(eta)
    e / rowSums(e)
  }
  converged <- FALSE
  for (o in seq_len(max_outer)) {
    old <- c(b0, B)
    for (k in seq_len(K)) {
      P <- probs()
      w <- pmax(P[, k] * (1 - P[, k]), 1e-10)
      r <- (Y[, k] - P[, k]) / w
      xw2 <- colSums(w * X^2)
      for (it in seq_len(max_inner)) {
        d0 <- sum(w * r) / sum(w)
        b0[k] <- b0[k] + d0
        r <- r - d0
        delta <- abs(d0)
        for (j in seq_len(p)) {
          rho <- sum(w * X[, j] * r) + xw2[j] * B[k, j]
          new <- if (xw2[j] > 0) sign(rho) * max(abs(rho) - lambda, 0) / xw2[j] else 0
          if (new != B[k, j]) {
            r <- r - X[, j] * (new - B[k, j])
            delta <- max(delta, abs(new - B[k, j]))
            B[k, j] <- new
          }
        }
        if (delta < tol) break
      }
    }
    if (max(abs(c(b0, B) - old)) < tol) {
      converged <- TRUE
      break
    }
  }
  b0 <- b0 - mean(b0)
  P <- probs()
  ll <- sum(log(P[cbind(seq_len(n), match(g, cl))]))
  list(intercepts = b0, coefficients = B, classes = cl, loglik = ll, objective = -ll + lambda * sum(abs(B)), prob = P,
       converged = converged)
}
