# SPDX-License-Identifier: AGPL-3.0-or-later
#' Population least-squares coefficient from second moments (ESL eq. 2.16)
#'
#' Hastie, Tibshirani and Friedman (2009), The Elements of Statistical
#' Learning, 2nd ed., Springer, Section 2.4, book pp. 18-19 (PDF pp. 37-38).
#'
#' EPE(f) = E(Y - f(X))^2 (2.9) and beta = \[E(XX')\]^-1 E(XY) (2.16), the
#' latter obtained by plugging the linear model (2.15) into (2.9) and
#' differentiating.  (2.16) is a population statement; the sample moment
#' matrices (1/N) X'X and (1/N) X'y are substituted here, which is exactly
#' the identification the book makes on p. 19 between (2.16) and (2.6).
#'
#' No intercept is added: (2.16) has none.  Supply a constant column in X.
#'
#' @param X N-by-p matrix of inputs.
#' @param y N-vector of responses.
#' @return list: estimate, beta, exx, exy, epe, eyy, n, p, method.
#' @examples
#' Epetheor(cbind(rep(1, 4), c(1, 2, 3, 4)), c(2, 4, 6, 8))$beta
#' @export
Epetheor <- function(X, y) {
  Xm <- .s03mat(X)
  yv <- .s03vec(y)
  n <- nrow(Xm)
  if (n == 0L) stop("epetheor: X is empty")
  if (length(yv) != n) stop("epetheor: X and y must have the same number of rows")
  p <- ncol(Xm)
  if (p == 0L) stop("epetheor: X has no columns")
  exx <- .s03crossprod(Xm) / n
  exy <- as.numeric(.s03matvec(t(Xm), yv)) / n
  beta <- .s03ridgesolve(exx, exy, 0)
  pred <- as.numeric(.s03matvec(Xm, beta))
  epe <- sum((yv - pred)^2) / n
  eyy <- sum(yv * yv) / n
  list(estimate = beta[1], beta = beta, exx = exx, exy = exy, epe = epe,
       eyy = eyy, n = n, p = p,
       method = "Hastie-Tibshirani-Friedman (2009) ESL eqs. (2.9), (2.15), (2.16)")
}
