# SPDX-License-Identifier: AGPL-3.0-or-later
#' Generalized precision.
#'
#' Formula: P_i = TTP_all / (TTP_all + TFP_i) on a one-versus-all basis
#' (eq. 4.9), with TFP_i = sum_{j != i} n_ji from eq. (4.6).
#'
#' @param y_true Observed class labels, zero-based.
#' @param y_pred Predicted class labels, zero-based.
#' @param class_index Zero-based index of the focal class.
#' @param n_classes Number of classes; inferred when NULL.
#' @return List with estimate, TFP, TTP_all, pCCC, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (4.9) p.132. DOI 10.1007/978-3-030-89010-0.
#' @export
Msm007 <- function(y_true, y_pred, class_index = 0, n_classes = NULL) {
  m <- .gpclassmetrics(.gpconf(y_true, y_pred, n_classes), as.integer(class_index))
  list(estimate = m$precision, TFP = m$TFP, TTP_all = m$TTP_all, pCCC = m$pCCC,
       method = "generalized precision (MVSML 2022 eq. 4.9)")
}
