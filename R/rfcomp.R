# SPDX-License-Identifier: AGPL-3.0-or-later
#' Robust factor analysis on a minimum covariance determinant scatter
#'
#' Pison, G., Rousseeuw, P. J., Filzmoser, P. and Croux, C. (2003), "Robust
#' factor analysis", Journal of Multivariate Analysis 84(1), 145-172.  The
#' proposal of that paper is the one named in the stub docstring: run the
#' classical factor-analytic machinery, but on a high-breakdown scatter matrix
#' rather than on the sample covariance, the MCD being the estimator used.  The
#' factor model itself is unchanged, Sigma = Lambda Lambda' + Psi, with Lambda
#' the p-by-k loading matrix and Psi the diagonal matrix of uniquenesses;
#' substituting the robust scatter for Sigma is what makes it resistant.
#'
#' Extraction here is principal factor analysis on the robust CORRELATION
#' matrix implied by that scatter: the loadings are the first k eigenvectors
#' scaled by the square roots of their eigenvalues, so Lambda Lambda' is the
#' rank-k eigen-approximation of the correlation matrix, and the uniquenesses
#' are the leftover diagonal.  The eigenproblem is the cyclic Jacobi routine
#' shared with the rest of this package, whose eigenvector signs are fixed so
#' the two language arms agree; without that fix the loadings would differ by a
#' sign column-wise and parity would fail on a correct implementation.
#'
#' Two exact consequences serve as anchors, neither running through the
#' extraction: at k = p the rank-k approximation is the whole spectral
#' decomposition, so Lambda Lambda' reproduces the correlation matrix exactly
#' and every uniqueness is zero; and for uncorrelated variables the robust
#' correlation matrix is the identity, whose eigenvalues are all 1, so the
#' communalities are k/p per variable and the reproduced off-diagonals vanish.
#'
#' @param X n-by-p data matrix.
#' @param k_factors number of factors, 1 <= k <= p.
#' @param h MCD subset size; defaults to \[(n + p + 1)/2\].
#' @param max_subsets passed to the MCD enumeration.
#' @return list: estimate, loadings, uniquenesses, communalities, correlation,
#'   reproduced, eigenvalues, center, k_factors, n, p, method.
#' @keywords internal
#' @examples
#' X <- cbind(c(1, 2, 3, 4, 5, 9), c(2, 1, 4, 3, 6, 1))
#' Rfcomp(X, 1, 6)$communalities
#' @export
Rfcomp <- function(X, k_factors = 1L, h = NULL, max_subsets = 200000) {
  Xm <- .s03mat(X)
  n <- nrow(Xm)
  if (n == 0L) stop("robust_factor_analysis: X is empty")
  p <- ncol(Xm)
  if (p == 0L) stop("robust_factor_analysis: X has no columns")
  kf <- as.integer(k_factors)
  if (is.na(kf) || kf < 1L || kf > p) stop("robust_factor_analysis: need 1 <= k_factors <= p")
  m <- Mcdv(Xm, h, NULL, max_subsets)
  S <- m$cov_raw
  sd <- numeric(p)
  for (a in seq_len(p)) {
    if (S[a, a] <= 0) stop("robust_factor_analysis: a variable has zero robust variance")
    sd[a] <- sqrt(S[a, a])
  }
  Cm <- matrix(0, p, p)
  for (a in seq_len(p)) for (b in seq_len(p)) Cm[a, b] <- S[a, b] / (sd[a] * sd[b])
  je <- .s03jacobi(Cm)
  ord <- seq(p, 1L)
  evals <- je$values[ord]
  L <- matrix(0, p, kf)
  for (cc in seq_len(kf)) {
    lam <- evals[cc]
    s <- if (lam > 0) sqrt(lam) else 0
    src <- ord[cc]
    for (a in seq_len(p)) L[a, cc] <- je$vectors[a, src] * s
  }
  comm <- numeric(p)
  for (a in seq_len(p)) {
    t <- 0
    for (cc in seq_len(kf)) t <- t + L[a, cc] * L[a, cc]
    comm[a] <- t
  }
  uniq <- numeric(p)
  for (a in seq_len(p)) uniq[a] <- Cm[a, a] - comm[a]
  rep_ <- matrix(0, p, p)
  for (a in seq_len(p)) for (b in seq_len(p)) {
    t <- 0
    for (cc in seq_len(kf)) t <- t + L[a, cc] * L[b, cc]
    rep_[a, b] <- t + if (a == b) uniq[a] else 0
  }
  list(estimate = comm[1], loadings = L, uniquenesses = uniq, communalities = comm,
       correlation = Cm, reproduced = rep_, eigenvalues = evals, center = m$center,
       k_factors = kf, n = n, p = p,
       method = "Pison-Rousseeuw-Filzmoser-Croux (2003) robust factor analysis: principal factors on the MCD correlation matrix")
}
