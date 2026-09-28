#' Thin-plate spline interpolation and smoothing in two dimensions
#'
#' Minimises \eqn{\sum_i (y_i - f(x_i))^2 + \lambda J(f)} with the bending
#' energy J; the solution \eqn{f(s) = a_0 + a_1 u + a_2 v + \sum_i w_i
#' \phi(|s - x_i|)}, \eqn{\phi(r) = r^2 \log(r) / (8\pi)}, solves the
#' bordered system with \eqn{K + \lambda I} and \eqn{P = (1, x)} (Duchon
#' 1977; Wahba 1990). \code{lam = 0} interpolates. Equal to
#' \code{fields::Tps} with the same \code{lambda} and
#' \code{scale.type = "unscaled"}.
#'
#' @param x Two-column matrix of knots.
#' @param y Values.
#' @param lam Smoothing parameter.
#' @param newdata Optional two-column matrix of prediction locations.
#' @return List with \code{w}, \code{a}, \code{fitted}, \code{residuals},
#'   \code{df} (trace of the hat matrix), \code{predicted}, \code{lam}.
#' @references Duchon, J. (1977). Splines minimizing rotation-invariant
#'   semi-norms in Sobolev spaces. Constructive Theory of Functions of
#'   Several Variables, 85-100. Springer, Berlin.
#'
#'   Wahba, G. (1990). Spline Models for Observational Data. SIAM,
#'   Philadelphia.
#' @examples
#' P <- rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1), c(.5, .5))
#' ThinPlateSpline(P, c(0, 1, 1, 2, 1.5), newdata = rbind(c(.5, .5), c(.25, .75)))$predicted
#' @export
ThinPlateSpline <- function(x, y, lam = 0, newdata = NULL) {
  X <- as.matrix(x)
  y <- as.numeric(y)
  n <- nrow(X)
  if (length(y) != n || n < 3) stop("need at least three knots with one value each")
  if (lam < 0) stop("lam must be non-negative")
  phi <- function(D) ifelse(D > 0, D^2 * log(pmax(D, 1e-300)), 0) / (8 * pi)
  K <- phi(as.matrix(stats::dist(X)))
  P <- cbind(1, X)
  A <- rbind(cbind(K + lam * diag(n), P), cbind(t(P), matrix(0, 3, 3)))
  s <- solve(A, c(y, 0, 0, 0))
  w <- s[1:n]
  a <- s[n + 1:3]
  f <- function(S) {
    D <- sqrt(outer(S[, 1], X[, 1], "-")^2 + outer(S[, 2], X[, 2], "-")^2)
    as.vector(phi(D) %*% w + cbind(1, S) %*% a)
  }
  fitted <- f(X)
  df <- if (lam > 0) n - lam * sum(diag(solve(A)[1:n, 1:n, drop = FALSE])) else n
  list(w = w, a = a, fitted = fitted, residuals = y - fitted, df = df,
       predicted = if (is.null(newdata)) NULL else f(as.matrix(newdata)), lam = lam)
}
