# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bayes factor between two models -- the same method as \code{Bayfac}
#'
#' This stub and bayfac declare the same public function name, the same formula
#' (the ratio of marginal likelihoods) and the same citation (Kass and Raftery
#' 1995).  They are one method under two module names; only the argument labels
#' differ, log_lik_a and log_lik_b here against log_evidence_1 and
#' log_evidence_2 there.
#'
#' Those labels are worth a word, because they are not interchangeable in
#' general.  A Bayes factor is a ratio of marginal likelihoods -- the
#' likelihood integrated over the prior -- not of maximised likelihoods.
#' Feeding in two maximised log-likelihoods gives a likelihood ratio, a
#' different quantity and one that always favours the larger model.  The
#' argument is treated here as what the formula requires, a log marginal
#' likelihood, whatever the stub happened to name it.
#'
#' There is exactly one implementation; this function delegates.  Recorded in
#' ledger/wave2/DUPMAP.tsv as bfac -> bayfac.  Distinct from bayesf and bbf
#' (BIC approximations) and from bfact (Savage-Dickey density ratio), which are
#' different estimators of the same target and must not be collapsed.
#'
#' @param log_lik_a,log_lik_b log marginal likelihoods of the two models.
#' @return the list returned by \code{Bayfac}.
#' @keywords internal
#' @examples
#' Bfac(-10.2, -14.7)$log_bf
#' @export
Bfac <- function(log_lik_a, log_lik_b) Bayfac(log_lik_a, log_lik_b)
