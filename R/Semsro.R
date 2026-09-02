# SPDX-License-Identifier: AGPL-3.0-or-later
#' SEM residual covariance matrix and its summary fit indices
#'
#' The raw residual matrix is the model misfit in the covariance metric,
#' \code{E = S - Sigma_hat}, with S the sample covariance and
#' \code{Sigma_hat} the model-implied covariance.  Summaries run over the
#' lower triangle including the diagonal, \code{p (p + 1) / 2} distinct
#' elements:
#' \code{RMR = sqrt(2 / (p (p + 1)) * sum_{i <= j} (s_ij - sigma_ij)^2)} and
#' \code{SRMR = sqrt(2 / (p (p + 1)) * sum_{i <= j}
#' ((s_ij - sigma_ij) / sqrt(s_ii s_jj))^2)}.
#'
#' SRMR divides each residual by the product of the observed standard
#' deviations, so it is scale free; Hu & Bentler's conventional cutoff is
#' \code{SRMR <= 0.08}, applied by \code{srmr_acceptable}.  Bentler & Yuan
#' show that in small samples the residual-based statistics behave far
#' better than the likelihood-ratio chi-square, which is why the residual
#' matrix rather than the chi-square is the object returned here.
#'
#' Both matrices must be square, of the same order, and symmetric to within
#' 1e-8; a non-symmetric input is an error, not something to silently
#' symmetrise.
#'
#' @param sample_cov Observed covariance matrix S, p by p.
#' @param fitted_cov Model-implied covariance matrix Sigma-hat, p by p.
#' @return List with \code{estimate} (SRMR), \code{residual}, \code{srmr},
#'   \code{rmr}, \code{max_abs_residual}, \code{max_abs_standardised},
#'   \code{sum_sq_residual}, \code{mean_residual}, \code{trace_residual},
#'   \code{n_elements}, \code{p}, \code{srmr_acceptable}, \code{method}.
#' @references Bentler, P. M. & Yuan, K.-H. (1999). Structural equation
#'   modeling with small samples: test statistics. Multivariate Behavioral
#'   Research 34(2), 181-197. \doi{10.1207/s15327906mb340203};
#'   Hu, L. & Bentler, P. M. (1999). Cutoff criteria for fit indexes in
#'   covariance structure analysis. Structural Equation Modeling 6(1), 1-55.
#'   \doi{10.1080/10705519909540118}
#' @export
#' @examples
#' Semsro(sample_cov = 5L, fitted_cov = 5L)
Semsro <- function(sample_cov, fitted_cov) {
  S <- .s03mat(sample_cov)
  G <- .s03mat(fitted_cov)
  p <- nrow(S)
  if (p == 0L || ncol(S) != p) stop("sem_residual: sample_cov must be square")
  if (nrow(G) != p || ncol(G) != p)
    stop("sem_residual: fitted_cov must have the same order as sample_cov")
  for (i in seq_len(p)) for (j in seq_len(p)) {
    if (abs(S[i, j] - S[j, i]) > 1e-8) stop("sem_residual: sample_cov is not symmetric")
    if (abs(G[i, j] - G[j, i]) > 1e-8) stop("sem_residual: fitted_cov is not symmetric")
  }
  for (i in seq_len(p)) if (S[i, i] <= 0)
    stop("sem_residual: sample_cov has a non-positive variance")

  E <- S - G
  m <- p * (p + 1) / 2
  ss_raw <- 0; ss_std <- 0; max_abs <- 0; max_abs_std <- 0
  for (i in seq_len(p)) {
    for (j in seq_len(i)) {
      e <- E[i, j]
      ss_raw <- ss_raw + e * e
      z <- e / sqrt(S[i, i] * S[j, j])
      ss_std <- ss_std + z * z
      if (abs(e) > max_abs) max_abs <- abs(e)
      if (abs(z) > max_abs_std) max_abs_std <- abs(z)
    }
  }
  rmr <- sqrt(ss_raw / m)
  srmr <- sqrt(ss_std / m)
  tr <- sum(diag(E))

  .t1_result(estimate = srmr, residual = E, srmr = srmr, rmr = rmr,
             max_abs_residual = max_abs, max_abs_standardised = max_abs_std,
             sum_sq_residual = ss_raw, mean_residual = tr / p,
             trace_residual = tr, n_elements = m, p = p,
             srmr_acceptable = if (srmr <= 0.08) 1 else 0,
             method = "SEM residual matrix S - Sigma-hat with RMR/SRMR (Bentler & Yuan 1999; Hu & Bentler 1999)")
}
