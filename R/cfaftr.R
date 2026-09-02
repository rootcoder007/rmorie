# SPDX-License-Identifier: AGPL-3.0-or-later

#' CFA one factor, ML estimation
#'
#' Formula: X = lambda * F + eps; ML estimation
#'
#' The single factor is standardised, so the model covariance is
#' lambda lambda' + Psi and the ML solution is reached by the
#' Rubin-Thayer EM algorithm.  For three items the ML solution is the
#' Spearman closed form lambda_i^2 = s_ij s_ik / s_jk, reported
#' alongside as an independent read of the same data.
#'
#' @param X An n x p data matrix, or a p x p item covariance matrix.
#' @param factor_structure Optional length-p 0/1 mask of which items
#'   load on the factor; NULL lets every item load.
#' @return List with \code{estimate} (variance explained),
#'   \code{loadings}, \code{uniquenesses}, \code{fml},
#'   \code{max_resid}, \code{communality}, \code{spearman},
#'   \code{n_iter}, \code{p}, \code{method}.
#' @references Joreskog (1969), Psychometrika 34(2):183-202;
#'   Spearman (1904), Am. J. Psychology 15(2):201-292.
#' @export
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(200), 50, 4)
#' Cfaftr(X)
Cfaftr <- function(X, factor_structure = NULL) {
  S <- .cfa_cov(X)
  p <- nrow(S)
  if (p < 3L) stop("a one-factor model needs at least three items")
  if (is.null(factor_structure)) {
    mask <- matrix(1L, p, 1L)
  } else {
    m <- .s03vec(factor_structure)
    if (length(m) != p) stop("factor_structure must have one entry per item")
    mask <- matrix(as.integer(abs(m) > 0), p, 1L)
    if (sum(mask) == 0L) stop("factor_structure frees no loading at all")
  }
  f <- .cfa_em(S, mask)
  load <- f$lam[, 1]
  comm <- load * load
  num <- S[1, 2] * S[1, 3]
  sp <- if (S[2, 3] != 0) sqrt(abs(num / S[2, 3])) else NaN
  if (load[1] < 0) sp <- -sp
  .t1_result(estimate = sum(comm) / sum(diag(S)), loadings = load,
             uniquenesses = f$psi, fml = f$fml, max_resid = f$resid,
             communality = comm, spearman = sp, n_iter = f$it, p = p,
             method = "one-factor CFA, ML by EM")
}
