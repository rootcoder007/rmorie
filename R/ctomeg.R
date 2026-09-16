# SPDX-License-Identifier: AGPL-3.0-or-later

#' McDonald's omega total
#'
#' Formula: omega = (sum lambda_i)^2 / Var(T) using factor loadings
#'
#' Var(T) is the variance of the total score, i.e. the sum of every
#' entry of the item covariance matrix.  Equivalently
#' omega = (sum lambda)^2 / ((sum lambda)^2 + sum theta) with
#' theta_i = S_ii - lambda_i^2 the unique variances.  Under
#' tau-equivalence (all loadings equal) omega coincides with Cronbach's
#' alpha; otherwise omega is the larger of the two.
#'
#' @param X Either an n x p data matrix or a p x p item covariance
#'   matrix (detected by squareness and symmetry).
#' @param factor_loadings Loading of each item on the general factor.
#' @return List with \code{estimate} (omega), \code{omega},
#'   \code{alpha}, \code{var_total}, \code{uniquenesses}, \code{p},
#'   \code{method}.
#' @references McDonald (1999), Test Theory: A Unified Treatment,
#'   Erlbaum, ch. 6.
#' @export
#' @examples
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' S <- c("a", "b", "c")
#' Ctomeg(M, S)
Ctomeg <- function(X, factor_loadings) {
  lam <- .s03vec(factor_loadings)
  p <- length(lam)
  if (p < 2L) stop("omega needs at least two items")
  M <- .s03mat(X)
  sym <- nrow(M) == p && ncol(M) == p && all(abs(M - t(M)) < 1e-12)
  if (sym) {
    S <- M
  } else {
    n <- nrow(M)
    if (n < 2L) stop("need at least two observations to form a covariance")
    if (ncol(M) != p)
      stop("X and factor_loadings imply different item counts")
    mu <- colSums(M) / n
    S <- matrix(0, p, p)
    for (a in seq_len(p)) for (b in seq_len(p))
      S[a, b] <- sum((M[, a] - mu[a]) * (M[, b] - mu[b])) / (n - 1)
  }
  var_total <- sum(S)
  if (var_total <= 0) stop("total score variance is not positive")
  th <- diag(S) - lam * lam
  sl <- sum(lam)
  omega <- sl * sl / var_total
  tr <- sum(diag(S))
  alpha <- p / (p - 1) * (1 - tr / var_total)
  .t1_result(estimate = omega, omega = omega, alpha = alpha,
             var_total = var_total, uniquenesses = th, p = p,
             method = "McDonald omega total")
}
