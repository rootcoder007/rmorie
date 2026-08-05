# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bayes factor between two models from their log marginal likelihoods
#'
#' Kass and Raftery (1995), "Bayes factors", Journal of the American
#' Statistical Association 90(430), 773-795,
#' doi:10.1080/01621459.1995.10476572 (citation verified against Crossref).
#'
#' The Bayes factor is the ratio of marginal likelihoods,
#' B_12 = p(D | M1) / p(D | M2), and the arithmetic is done on the log scale
#' throughout, log B = log p(D|M1) - log p(D|M2), exponentiating once at the
#' end.  That is not a style preference: marginal likelihoods routinely
#' underflow a double, and forming the ratio from the exponentials gives 0/0
#' for models that are perfectly well separated on the log scale.  log_bf is
#' returned first and bf may legitimately be Inf or 0.
#'
#' The evidence scale is the paper's own, on the 2 log_e B scale: 0 to 2 "not
#' worth more than a bare mention", 2 to 6 "positive", 6 to 10 "strong", above
#' 10 "very strong".  Note the factor of two -- the same table is often quoted
#' on the log10 scale with different cut-points, and mixing them up moves a
#' result two categories.  Both two_log_bf and log10_bf are returned so the
#' reader can see which scale is in play.
#'
#' The interpretation is symmetric: B_21 = 1/B_12, so evidence for M2 is read
#' from the same number with the sign of log B flipped.  favours says which
#' model and category grades the strength using |2 log B|.
#'
#' @param log_evidence_1,log_evidence_2 log p(D | M1) and log p(D | M2); logs,
#'   not likelihoods.
#' @return list: bf, estimate, log_bf, two_log_bf, log10_bf, bf_21, category,
#'   favours, log_evidence_1, log_evidence_2, method.
#' @keywords internal
#' @examples
#' Bayfac(-10.2, -14.7)$log_bf
#' @export
Bayfac <- function(log_evidence_1, log_evidence_2) {
  l1 <- as.numeric(log_evidence_1); l2 <- as.numeric(log_evidence_2)
  if (is.na(l1) || is.na(l2)) stop("bayes_factor: log evidences must not be NaN")
  if (is.infinite(l1) && is.infinite(l2) && ((l1 > 0) == (l2 > 0))) {
    stop("bayes_factor: both log evidences are the same infinity, B is undefined")
  }
  lb <- l1 - l2
  bf <- if (lb > 709) Inf else if (lb < -745) 0 else exp(lb)
  list(bf = bf, estimate = bf, log_bf = lb, two_log_bf = 2 * lb,
       log10_bf = lb / log(10),
       bf_21 = if (bf == 0) Inf else if (is.infinite(bf)) 0 else 1 / bf,
       category = .bayfac_category(2 * lb),
       favours = if (lb > 0) 1L else if (lb < 0) 2L else 0L,
       log_evidence_1 = l1, log_evidence_2 = l2,
       method = "B_12 = exp(log p(D|M1) - log p(D|M2)); Kass and Raftery (1995), 2 log_e scale")
}

#' @noRd
.bayfac_category <- function(two_log_bf) {
  a <- abs(as.numeric(two_log_bf))
  if (a < 2) "bare mention" else if (a < 6) "positive" else
    if (a < 10) "strong" else "very strong"
}
