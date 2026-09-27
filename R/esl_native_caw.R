# SPDX-License-Identifier: AGPL-3.0-or-later
#' Curds and whey multiple-response shrinkage
#'
#' Y_hat = H Y S with S = U diag(lambda_m) U^-1 from the canonical correlation
#' analysis of the centred X and Y, lambda_m = c_m^2 / (c_m^2 + (p / N) (1 -
#' c_m^2)) (ESL eqs 3.72-3.74); lambda > 0 gives the ridge hybrid A_lambda Y S of
#' eq 3.75. Needs K <= p responses.
#'
#' @param X Predictors, N by p.
#' @param Y Responses, N by K.
#' @param lambda Ridge penalty for the hybrid.
#' @param newdata Points to predict.
#' @return Named list: coefficients, intercept, fitted, prediction,
#'   canonical_correlations, shrinkage, S.
#' @references Breiman, L. & Friedman, J. (1997). JRSS B 59, 3-54.
#' @examples
#' i <- 1:50
#' X <- cbind(sin(i), cos(2 * i), log(i) / 3)
#' Y <- cbind(X[, 1] + 0.3 * cos(9 * i), X[, 2] - X[, 3] + 0.4 * sin(7 * i))
#' morie_esl_curds_whey(X, Y)$shrinkage
#' @export
morie_esl_curds_whey <- function(X, Y, lambda = 0, newdata = NULL) {
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  N <- nrow(X)
  p <- ncol(X)
  K <- ncol(Y)
  if (nrow(Y) != N || K > p || N <= p) stop("need N > p rows in X and Y and K <= p responses", call. = FALSE)
  xm <- colMeans(X)
  ym <- colMeans(Y)
  Xc <- sweep(X, 2, xm)
  Yc <- sweep(Y, 2, ym)
  isq <- function(S) {
    e <- eigen(S, symmetric = TRUE)
    if (min(e$values) <= 0) stop("a covariance matrix is singular", call. = FALSE)
    e$vectors %*% diag(1 / sqrt(e$values), length(e$values)) %*% t(e$vectors)
  }
  Ry <- isq(crossprod(Yc) / N)
  s <- svd(isq(crossprod(Xc) / N) %*% (crossprod(Xc, Yc) / N) %*% Ry)
  U <- Ry %*% s$v
  lam <- s$d^2 / (s$d^2 + (p / N) * (1 - s$d^2))
  S <- U %*% diag(lam, K) %*% solve(U)
  B <- solve(crossprod(Xc) + lambda * diag(p), crossprod(Xc, Yc)) %*% S
  pred <- if (is.null(newdata)) NULL else sweep(sweep(rbind(newdata), 2, xm) %*% B, 2, ym, "+")
  list(coefficients = unname(B), intercept = unname(ym - drop(xm %*% B)), fitted = unname(sweep(Xc %*% B, 2, ym, "+")),
       prediction = pred, canonical_correlations = s$d, shrinkage = lam, S = S)
}
