# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bayesian ridge for genomic prediction -- the same method as brreg
#'
#' The specification here is "u_j ~ N(0, sigma^2) with conjugate prior", citing
#' Park and Casella (2008).  A normal prior with a common variance on every
#' marker effect, combined with a normal likelihood, has the ridge estimator as
#' its posterior mode, u_hat = (X'X + lambda I)^-1 X'y with
#' lambda = sigma_e^2 / sigma_u^2, which is RR-BLUP.  That is precisely what
#' \code{morie_bayesian_ridge_regression} in brreg.R already implements, with
#' both R arms present and verified in parity.
#'
#' There is therefore exactly one implementation: this function delegates.
#'
#' Note on the citation: Park and Casella (2008) is the Bayesian lasso, a
#' Laplace prior, not the ridge; the ridge with a conjugate normal prior is the
#' Lindley and Smith (1972) / Hoerl and Kennard (1970) construction.  The
#' stub's attribution is repeated here only to record that it was checked and
#' is wrong for the formula the stub states; the formula, not the label, is
#' what has been implemented.  Recorded in ledger/wave2/DUPMAP.tsv as
#' baysrr -> brreg.
#'
#' @param y the n phenotypes.
#' @param M the n-by-p marker matrix.
#' @param lam optional ridge parameter sigma_e^2 / sigma_u^2.
#' @return list: u, beta, estimate, lam, n, p, method.
#' @keywords internal
#' @examples
#' Baysrr(c(1.2, 0.8, 1.5, 0.3), rbind(c(1, 0), c(0, 1), c(1, 1), c(0, 0)), 0.5)$u
#' @export
Baysrr <- function(y, M = NULL, lam = NULL) {
  if (is.null(M)) stop("bayes_ridge: a marker matrix is required")
  r <- morie_bayesian_ridge_regression(M, y, lam)
  beta <- as.numeric(r$beta)
  list(u = beta, beta = beta, estimate = if (length(beta)) beta[1] else NA_real_,
       lam = r$lam, n = r$n, p = r$p,
       method = "posterior mode of a conjugate normal prior = ridge; shared implementation with morie.fn.brreg")
}
