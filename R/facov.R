# SPDX-License-Identifier: AGPL-3.0-or-later
#' Factor analytic covariance structure for multi-environment trials
#'
#' NOT IN THE BOOK.  Montesinos Lopez, Montesinos Lopez and Crossa (2022),
#' Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#' Springer, was searched in full -- all seventeen page-range volumes and the
#' index, [Pages 683-691].  The phrase "factor analytic" occurs exactly once,
#' in the front matter, volume [Pages i-xxiv]: "for the traits or
#' environments, unstructured or factor analytic variance-covariance matrices
#' can be chosen".  The structure is named there and nowhere specified;
#' Chapter 5, volume [Pages 141-170], carries only the unstructured and
#' diagonal forms.
#'
#' The structure is therefore taken from the primary source, Smith, A.,
#' Cullis, B. and Thompson, R. (2001), Analyzing variety by environment data
#' using multiplicative mixed models and adjustments for spatial field
#' trends, Biometrics 57(4), 1138-1147, which writes the k-factor variety by
#' environment variance matrix as Sigma_g = Lambda Lambda^T + Psi, Lambda the
#' n_env-by-k loadings and Psi the diagonal of environment specific
#' variances.  The identifiability constraint the same paper imposes -- the
#' upper triangle of Lambda above the diagonal is zero -- fixes the free
#' parameter count at n_env*k - k(k-1)/2 + n_env.
#'
#' DETERMINISM.  When no loadings are supplied the canonical Lambda is laid
#' out on van der Corput points, not drawn, so both arms build the same
#' matrix.
#'
#' @param n_env number of environments (or traits), the order of Sigma.
#' @param n_factors k, the number of factors; 0 <= k <= n_env.
#' @param loadings optional n_env-by-k loading matrix.  When absent a
#'   deterministic low-discrepancy Lambda is used, in the lower-triangular
#'   parameterisation Smith et al. impose.
#' @param psi optional n_env specific variances; unit variances when absent.
#' @return list: estimate, Sigma, Lambda, Psi, n_params, n, method.
#' @keywords internal
#' @examples
#' Facov(3, 1)$Sigma
#' @export
Facov <- function(n_env, n_factors, loadings = NULL, psi = NULL) {
  m <- as.integer(n_env)
  kk <- as.integer(n_factors)
  if (m < 1L) stop("factor_analytic_covariance: n_env must be at least 1")
  if (kk < 0L || kk > m) {
    stop("factor_analytic_covariance: n_factors must lie between 0 and n_env")
  }
  if (is.null(loadings)) {
    L <- matrix(0, m, kk)
    if (kk > 0L) {
      for (i in seq_len(m)) {
        for (j in seq_len(kk)) {
          if (j <= i) L[i, j] <- .s03vdc((i - 1L) * kk + (j - 1L), 2L + (j - 1L)) + 0.5
        }
      }
    }
  } else {
    L <- .s03mat(loadings)
    if (nrow(L) != m || (kk > 0L && ncol(L) != kk)) {
      stop("factor_analytic_covariance: loadings must be n_env by n_factors")
    }
  }
  if (is.null(psi)) {
    P <- rep(1, m)
  } else {
    P <- .s03vec(psi)
    if (length(P) != m) stop("factor_analytic_covariance: psi must have n_env entries")
    if (any(P < 0)) {
      stop("factor_analytic_covariance: specific variances must be non-negative")
    }
  }
  S <- matrix(0, m, m)
  for (a in seq_len(m)) {
    for (b in seq_len(m)) {
      s <- 0
      if (kk > 0L) for (j in seq_len(kk)) s <- s + L[a, j] * L[b, j]
      S[a, b] <- s + (if (a == b) P[a] else 0)
    }
  }
  list(estimate = S[1L, 1L], Sigma = S, Lambda = L, Psi = P,
       n_params = m * kk - (kk * (kk - 1L)) %/% 2L + m, n = m,
       method = paste0("Sigma = Lambda Lambda^T + Psi, Smith, Cullis and ",
                       "Thompson (2001); not in the book"))
}
