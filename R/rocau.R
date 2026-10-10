# SPDX-License-Identifier: AGPL-3.0-or-later

#' ROC curve and AUC (R parity)
#'
#' Native empirical ROC curve, computed exactly as \code{pROC::roc} does
#' with \code{levels = c(0, 1)}, \code{direction = "<"}: candidate
#' thresholds are \code{-Inf}, the midpoints between consecutive distinct
#' scores and \code{+Inf} (with pROC's near-tie correction), a case is
#' called positive when its score is \code{>=} the threshold, and the AUC
#' is the trapezoid area under (specificity, sensitivity), which equals
#' the Mann-Whitney U probability with ties credited 0.5. Observations
#' with a missing label or score are dropped from the curve (pROC's
#' \code{na.rm = TRUE}).
#'
#' @param y_true Binary labels.
#' @param y_score Predicted scores for the positive class.
#' @return Named list: estimate, auc, fpr, tpr, thresholds, n,
#'   n_positive, n_negative, method.
#' @examples
#' set.seed(1)
#' y <- rbinom(50, 1, 0.5)
#' morie_roc_auc_score(y, y + rnorm(50))$auc
#' @export
morie_roc_auc_score <- function(y_true, y_score) {
  yt <- as.numeric(y_true)
  ys <- as.numeric(y_score)
  classes <- sort(unique(yt))
  if (length(classes) != 2) stop("morie_roc_auc_score requires binary y_true")
  pos <- classes[2]
  yt_b <- as.integer(yt == pos)
  ok <- !is.na(yt_b) & !is.na(ys)
  controls <- ys[ok & yt_b == 0L]
  cases <- ys[ok & yt_b == 1L]
  if (length(controls) == 0L || length(cases) == 0L) {
    stop("morie_roc_auc_score: no control or no case observations")
  }
  rc <- .morie_roc_lt(controls, cases)
  auc <- rc$auc
  fpr <- 1 - rc$specificities
  tpr <- rc$sensitivities
  # Sort by FPR ascending to match sklearn output order
  ord <- order(fpr, tpr)
  list(
    estimate    = as.numeric(auc),
    auc         = as.numeric(auc),
    fpr         = fpr[ord],
    tpr         = tpr[ord],
    thresholds  = rc$thresholds[ord],
    n           = length(yt),
    n_positive  = sum(yt_b == 1L),
    n_negative  = sum(yt_b == 0L),
    method      = "ROC AUC (Mann-Whitney U probability)"
  )
}

# Internal: empirical ROC for direction "<" (cases score higher).
# Thresholds follow pROC's rule: midpoints of the sorted distinct scores
# padded with -Inf/+Inf, halved before adding when the midpoint would
# overflow, and nudged up to the upper score whenever floating-point
# rounding makes a midpoint coincide with an observed score. Returned in
# ascending threshold order (specificity ascending), as pROC stores them.
# @noRd
.morie_roc_lt <- function(controls, cases) {
  predictor <- c(controls, cases)
  u <- sort(unique(predictor))
  th1 <- (c(-Inf, u) + c(u, Inf)) / 2
  th2 <- c(-Inf, u) / 2 + c(u, Inf) / 2
  thr <- ifelse(abs(th1) > 1e100, th2, th1)
  ties <- which(thr %in% predictor)
  for (k in ties) {
    if (thr[k] == u[k - 1L]) thr[k] <- u[k]
  }
  # Sensitivity / specificity at every threshold by sorted counting:
  # se(t) = #(cases >= t) / n1, sp(t) = #(controls < t) / n0.
  cs <- sort(cases)
  ct <- sort(controls)
  n1 <- length(cs)
  n0 <- length(ct)
  se <- (n1 - findInterval(thr, cs, left.open = TRUE)) / n1
  sp <- findInterval(thr, ct, left.open = TRUE) / n0
  auc <- sum((se[-1L] + se[-length(se)]) / 2 * (sp[-1L] - sp[-length(sp)]))
  list(thresholds = thr, sensitivities = se, specificities = sp, auc = auc)
}
