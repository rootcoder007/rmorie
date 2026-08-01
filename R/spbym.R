# SPDX-License-Identifier: AGPL-3.0-or-later

#' Besag-York-Mollie convolution model for disease mapping
#'
#' For areas \eqn{i = 1, \ldots, n} with observed counts \eqn{y_i} and
#' expected counts \eqn{c_i},
#' \eqn{y_i | x_i \sim Poisson(c_i \exp\{x_i\})} with
#' \eqn{x_i = u_i + v_i}, where u carries spatially structured variation
#' under an intrinsic autoregression with scale kappa and v carries
#' unstructured heterogeneity with scale lambda. The relative risk is
#' \eqn{\exp\{u_i + v_i\}}, and the convolution of the two components is
#' what the model contributes.
#'
#' Returned are the conditional MAP estimates given kappa and lambda,
#' obtained by Newton. That is well posed rather than merely convenient: the
#' paper states the log posterior in u and v is "a strictly concave
#' differentiable function of u and v and therefore possesses a single
#' maximum".
#'
#' Two identities are reported and should be checked. Sec. 4 gives
#' \eqn{\sum_i v_i^* = 0} and
#' \eqn{\sum_i c_i \exp\{u_i^* + v_i^*\} = \sum_i y_i}, "so that the
#' fitted total number of cases matches the observed total". Neither is
#' imposed here; both follow from stationarity, because the structure matrix
#' has zero row sums. A fit that fails either is flagged, since that means no
#' maximum was reached.
#'
#' Only the sum enters the likelihood, so kappa and lambda are arguments
#' rather than estimates: the data cannot separate the two variance
#' components.
#'
#' @param counts Observed counts.
#' @param expected Expected counts, typically age-standardised; positive.
#' @param adjacency Symmetric 0/1 contiguity matrix with a zero diagonal.
#' @param kappa,lam Scales of the structured and unstructured components. The
#'   paper's own estimates were 0.129 and 0.011 for thyroid cancer across the
#'   94 departements of France.
#' @param max_iter,tol Newton controls.
#' @return A list with `u`, `v`, `x`, `relative_risk`, `fitted`, `smr`,
#'   `sum_v`, `fitted_total`, `observed_total`, `log_posterior`, `converged`
#'   and `identifiability`.
#' @references Besag, York and Mollie (1991), Ann. Inst. Statist. Math.
#'   43(1):1-59, Sec. 4, eqs (4.2)-(4.6); Schabenberger Ch 6, Sec 6.4.3.2
#' @export
spbym <- function(counts, expected, adjacency, kappa, lam, max_iter = 200L,
                  tol = 1e-11) {
  y <- as.numeric(counts); e <- as.numeric(expected)
  fit <- .schab_bym_map(y, e, adjacency, kappa, lam, max_iter = max_iter,
                        tol = tol)
  fit$smr <- .schab_smr(y, e)
  fit$kappa <- as.numeric(kappa)
  fit$lam <- as.numeric(lam)
  fit$identifiability <- .schab_bym_identifiability_note()
  fit$n_neighbours <- rowSums(as.matrix(adjacency))
  fit$median_log_prior <- .schab_bym_median_log_prior(fit$u, adjacency, kappa)
  problems <- character(0)
  if (!isTRUE(fit$converged)) problems <- c(problems, "Newton did not converge")
  if (abs(fit$sum_v) > 1e-6) {
    problems <- c(problems, sprintf("sum of v* is %.3g, not 0", fit$sum_v))
  }
  gap <- abs(fit$fitted_total - fit$observed_total)
  if (gap > 1e-5 * max(fit$observed_total, 1)) {
    problems <- c(problems,
                  sprintf("fitted total misses the observed total by %.3g", gap))
  }
  if (length(problems)) {
    fit$warning <- paste0(
      "the Sec. 4 stationarity identities do not hold, so this is not a ",
      "maximum of (4.5): ", paste(problems, collapse = "; "))
  }
  fit
}
