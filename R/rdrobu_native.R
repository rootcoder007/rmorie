# Robust confidence intervals for RD designs (front end).
# Calonico, S., Cattaneo, M. D., & Titiunik, R. (2014) "Robust Nonparametric
# Confidence Intervals for Regression-Discontinuity Designs", *Econometrica*
# 82(6), 2295-2326.
#
# This module is the *interval* half of the paper and shares one implementation
# with morie_causrddc, which carries the estimator, the MSE-optimal bandwidths
# of Lemma 1 and the full documentation. Nothing is reimplemented here -- two
# ledger entries point at the same paper, and duplicating the numerics would
# give two arms that could silently drift apart.
#
# What this front end adds is the comparison the paper is about, in one table:
# for a single fit it returns all three intervals side by side, together with
# the coverage-relevant quantities that distinguish them.
#
# I_SRD(h_n)     = tau_hat(h_n) +/- Phi^-1_{1-alpha/2} * sqrt(V(h_n))
# I^bc_SRD(...)  = tau_hat(h_n) - h_n^{p+1-nu} * B_hat +/- ... * sqrt(V(h_n))
# I^rbc_SRD(...) = tau_hat(h_n) - h_n^{p+1-nu} * B_hat +/- ... * sqrt(V(h_n) + C^bc(h_n, b_n))
#
# The first undercovers at an MSE-optimal bandwidth because the leading bias
# does not vanish; the second recentres but keeps a variance that ignores the
# bias estimate's own variability; only the third does both, which is what
# Theorem 1 buys and what Remark 2 calls robustness to "small" or "large"
# bandwidths.
#
# See morie_causrddc for the estimator, kernels, variance routes
# (nearest-neighbour or plug-in residuals), fuzzy and kink designs, and the
# bandwidth selectors.

#' morie_rdrobu
#'
#' A step of the rdrobu_native implementation. Called by \code{morie_calonico_cattaneo_titiunik}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Passed to \code{morie_causrddc}.
#' @param x Passed to \code{morie_causrddc}.
#' @param cutoff Passed to \code{morie_causrddc}. Defaults to \code{0}.
#' @param alpha Passed to \code{morie_causrddc}. Defaults to \code{0.05}.
#' @param ... Passed through.
#' @return A vector, from \code{c}.
#' @export
morie_rdrobu <- function(y, x, cutoff = 0.0, alpha = 0.05, ...) {
  fit <- morie_causrddc(y, x, cutoff = cutoff, alpha = alpha, ...)
  ci_c <- fit$ci_conventional
  ci_b <- fit$ci_bias_corrected
  ci_r <- fit$ci_robust
  se_c <- fit$se_conventional
  se_r <- fit$se_robust

  correction_factor <- if (isTRUE(se_c > 0)) se_r / se_c else NA_real_

  c(fit, list(
    estimate      = fit$estimate,
    intervals     = list(
      conventional   = ci_c,
      bias_corrected = ci_b,
      robust         = ci_r
    ),
    widths        = list(
      conventional   = ci_c[2] - ci_c[1],
      bias_corrected = ci_b[2] - ci_b[1],
      robust         = ci_r[2] - ci_r[1]
    ),
    correction_factor = correction_factor,
    bias_estimate     = fit$estimate - fit$bias_corrected,
    method = "robust bias-corrected RD confidence intervals (Calonico, Cattaneo & Titiunik 2014)",
    note   = "one implementation, shared with morie_causrddc; the conventional interval is the one the paper shows to undercover at an MSE-optimal bandwidth"
  ))
}

# Alias kept from the generated stub's signature.
#' Alias kept from the generated stub\'s signature
#'
#' A step of the rdrobu_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Passed to \code{morie_rdrobu}.
#' @param x Passed to \code{morie_rdrobu}.
#' @param cutoff Passed to \code{morie_rdrobu}. Defaults to \code{0}.
#' @param ... Passed through.
#' @return The value of \code{morie_rdrobu}.
#' @export
morie_calonico_cattaneo_titiunik <- function(y, x, cutoff = 0.0, ...) {
  morie_rdrobu(y, x, cutoff, ...)
}

# Compact alias per ledger/NAMING.md
morie_rd_confidence_intervals <- morie_rdrobu

#' .rdrobu_cheatsheet
#'
#' A step of the rdrobu_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .rdrobu_cheatsheet()
#' res
.rdrobu_cheatsheet <- function() {
  paste0(
    "rdrobu: the three RD intervals of Calonico, Cattaneo & Titiu",
    "nik (2014) side by side -- conventional, bias-corrected, and",
    " robust (recentred AND rescaled by V + C^bc). Shares its imp",
    "lementation with causrddc; see that module for the estimator",
    ", bandwidths and designs."
  )
}
