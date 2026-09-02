# SPDX-License-Identifier: AGPL-3.0-or-later
#' Gaussian ML log-likelihood of a linear mixed model at a given V
#'
#' \eqn{l(beta, V; y) = -1/2 \[n log(2 pi) + log|V| +
# prime (y - X beta) prime V^-1 (y - X beta)]} with \code{beta} profiled out at its
#' GLS value \eqn{(X prime V^-1 X)^-1 X prime V^-1 y}, which is the maximiser for
#' any fixed \code{V}; what is returned is therefore the profile
#' log-likelihood in \code{V}. \code{V} must be positive definite -- an
#' inadmissible set of variance components is refused, not returned.
#'
#' The Cholesky route is used for both the determinant and the quadratic
#' form, so a non-positive-definite \code{V} fails at the factorisation
#' rather than producing a finite nonsense value.
#'
#' @param y Response, length n.
#' @param X Fixed-effects design, n by p.
#' @param V Marginal variance \eqn{Z D Z prime + R}, n by n.
#' @return List with estimate (log-likelihood), loglik, neg2loglik,
#'   logdet_V, quadratic_form, aic, bic, n, p.
#' @references Hartley and Rao (1967), Biometrika 54(1-2), 93-108,
#'   \doi{10.1093/biomet/54.1-2.93}.
#' @export
#' @examples
#' Mlfit(y = 5L, X = 5L, V = 5L)
Mlfit <- function(y, X, V) {
  yv <- .t1_vec(y); n <- length(yv)
  Xa <- as.matrix(X)
  if (nrow(Xa) != n) Xa <- t(Xa)
  if (nrow(Xa) != n) stop("X has the wrong number of rows for y")
  p <- ncol(Xa)
  Vm <- as.matrix(V)
  if (nrow(Vm) != n || ncol(Vm) != n) stop("V must be n by n")
  Vm <- 0.5 * (Vm + t(Vm))
  L <- tryCatch(chol(Vm), error = function(e) NULL)
  if (is.null(L)) {
    stop("V is not positive definite; the variance components are inadmissible.")
  }
  logdet <- 2 * sum(log(diag(L)))
  # Solve against the factor instead of forming V^-1: same numbers, and
  # it is the factorisation that certifies positive definiteness.
  Vi <- chol2inv(L)
  ViX <- Vi %*% Xa
  XtViX <- crossprod(Xa, ViX)
  beta <- solve(XtViX, crossprod(ViX, yv))
  r <- yv - as.numeric(Xa %*% beta)
  quad <- as.numeric(crossprod(r, Vi %*% r))
  ll <- -0.5 * (logdet + quad + n * log(2 * pi))
  npar <- p + 1
  .t1_result(estimate = ll, loglik = ll, neg2loglik = -2 * ll,
             logdet_V = logdet, quadratic_form = quad,
             aic = -2 * ll + 2 * npar, bic = -2 * ll + npar * log(n),
             n = n, p = p,
             method = "ML log-likelihood of a linear mixed model")
}
