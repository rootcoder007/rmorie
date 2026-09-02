# SPDX-License-Identifier: AGPL-3.0-or-later
#' Kernel ridge regression, the dual of a Gaussian-process mean
#'
#' Saunders, Gammerman and Vovk (1998), Ridge regression learning
#' algorithm in dual variables, ICML 15, 515-521: alpha = (K + lambda
#' I)^-1 y and fhat(x) = sum_i alpha_i k(x_i, x).  The 1998 proceedings
#' were not retrievable here; both are quoted in their standard published
#' form.  The identity worth stating is that this is exactly the posterior
#' mean of a GP with covariance k and noise variance lambda (Rasmussen and
#' Williams 2006, eq. 2.23) -- the two differ only in that the GP also
#' returns a variance, which is returned here too.
#'
#' @param X training inputs, one row per point.
#' @param y responses.
#' @param X_test test inputs.
#' @param lam the ridge / noise variance.
#' @param gamma RBF kernel width.
#' @return list: estimate, pred, var, alpha, lam, method.
#' @keywords internal
#' @examples
#' Krrdual(matrix(c(0, 1, 2), 3, 1), c(0, 1, 0))$pred
#' @export
Krrdual <- function(X, y, X_test = NULL, lam = 1e-2, gamma = 1) {
  rbf <- function(x, z) {
    s <- 0
    for (a in seq_along(x)) { d <- x[a] - z[a]
    s <- s + d * d }
    exp(-as.numeric(gamma) * s)
  }
  Xm <- .s03mat(X)
  yv <- .s03vec(y)
  n <- nrow(Xm)
  K <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) K[i, j] <- rbf(Xm[i, ], Xm[j, ])
  A <- K
  for (i in seq_len(n)) A[i, i] <- A[i, i] + as.numeric(lam)
  alpha <- .s03cholsolve(A, yv)
  Xt <- if (!is.null(X_test)) .s03mat(X_test) else Xm
  pred <- numeric(nrow(Xt))
  var_ <- numeric(nrow(Xt))
  for (t in seq_len(nrow(Xt))) {
    ks <- numeric(n)
    for (i in seq_len(n)) ks[i] <- rbf(Xm[i, ], Xt[t, ])
    s <- 0
    for (i in seq_len(n)) s <- s + alpha[i] * ks[i]
    pred[t] <- s
    w <- .s03cholsolve(A, ks)
    q <- 0
    for (i in seq_len(n)) q <- q + ks[i] * w[i]
    var_[t] <- rbf(Xt[t, ], Xt[t, ]) - q
  }
  list(estimate = if (length(pred)) pred[1] else NaN, pred = pred, var = var_,
       alpha = alpha, lam = as.numeric(lam),
       method = "Dual ridge alpha = (K + lambda I)^-1 y (Saunders et al. 1998); equals the GP posterior mean")
}
