# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sparse GP regression by the FITC approximation
#'
#' Snelson and Ghahramani (2006), Sparse Gaussian processes using
#' pseudo-inputs, NIPS 18, 1257-1264, replace the exact covariance by Q =
#' K_nm K_mm^-1 K_mn + diag(K_nn - K_nm K_mm^-1 K_mn) -- the Nystrom
#' low-rank term plus a diagonal correction restoring the exact marginal
#' variances.  Quinonero-Candela and Rasmussen (2005), JMLR 6, 1939-1959,
#' name it FITC and give the same expression.  Neither was retrievable
#' here as a full text; the covariance is quoted in its standard published
#' form.  Dropping the correction gives DTC, available as kind = "dtc", so
#' its effect is visible rather than assumed.
#'
#' @param X,y training data.
#' @param X_test test inputs.
#' @param inducing inducing inputs.
#' @param gamma RBF width.
#' @param sigma2 noise variance.
#' @param jitter added to K_mm.
#' @param kind "fitc" or "dtc".
#' @return list: estimate, pred, var, lam, alpha, method.
#' @keywords internal
#' @examples
#' Fitcgp(matrix(c(0, 1, 2, 3), 4, 1), c(0, 1, 0, 1),
#'        inducing = matrix(c(0, 3), 2, 1))$pred
#' @export
Fitcgp <- function(X, y, X_test = NULL, inducing = NULL, gamma = 1,
                   sigma2 = 1e-2, jitter = 1e-8, kind = "fitc") {
  g <- as.numeric(gamma)
  rbf <- function(x, z) {
    s <- 0
    for (a in seq_along(x)) { d <- x[a] - z[a]
    s <- s + d * d }
    exp(-g * s)
  }
  cross <- function(A, B) {
    out <- matrix(0, nrow(A), nrow(B))
    for (i in seq_len(nrow(A))) for (j in seq_len(nrow(B))) out[i, j] <- rbf(A[i, ], B[j, ])
    out
  }
  Xm <- .s03mat(X)
  yv <- .s03vec(y)
  Z <- if (!is.null(inducing)) .s03mat(inducing) else Xm
  n <- nrow(Xm)
  m <- nrow(Z)
  Kmm <- cross(Z, Z)
  for (i in seq_len(m)) Kmm[i, i] <- Kmm[i, i] + as.numeric(jitter)
  Knm <- cross(Xm, Z)
  lam <- numeric(n)
  for (i in seq_len(n)) {
    w <- .s03cholsolve(Kmm, Knm[i, ])
    q <- 0
    for (t in seq_len(m)) q <- q + Knm[i, t] * w[t]
    d <- if (identical(kind, "fitc")) 1 - q else 0
    lam[i] <- if (d > 0) d else 0
  }
  A <- Kmm
  rhs <- numeric(m)
  for (i in seq_len(n)) {
    d <- lam[i] + as.numeric(sigma2)
    for (s in seq_len(m)) {
      for (t in seq_len(m)) A[s, t] <- A[s, t] + Knm[i, s] * Knm[i, t] / d
      rhs[s] <- rhs[s] + Knm[i, s] * yv[i] / d
    }
  }
  alpha <- .s03cholsolve(A, rhs)
  Xt <- if (!is.null(X_test)) .s03mat(X_test) else Xm
  Ktm <- cross(Xt, Z)
  pred <- numeric(nrow(Xt))
  var_ <- numeric(nrow(Xt))
  for (t in seq_len(nrow(Xt))) {
    s <- 0
    for (a in seq_len(m)) s <- s + Ktm[t, a] * alpha[a]
    pred[t] <- s
    wa <- .s03cholsolve(A, Ktm[t, ])
    wb <- .s03cholsolve(Kmm, Ktm[t, ])
    qa <- 0
    qb <- 0
    for (a in seq_len(m)) { qa <- qa + Ktm[t, a] * wa[a]
    qb <- qb + Ktm[t, a] * wb[a] }
    var_[t] <- 1 - qb + qa
  }
  list(estimate = if (length(pred)) pred[1] else NaN, pred = pred, var = var_,
       lam = lam, alpha = alpha,
       method = "FITC sparse GP with the diagonal correction (Snelson and Ghahramani 2006)")
}
