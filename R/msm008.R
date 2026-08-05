# SPDX-License-Identifier: AGPL-3.0-or-later
#' Generalized sensitivity and specificity.
#'
#' Formula: Se_i = TTP_all/(TTP_all + TFN_i) (eq. 4.10) and
#' Sp_i = TTN_i/(TTN_i + TFP_i) (eq. 4.11).
#'
#' @param y_true Observed class labels, zero-based.
#' @param y_pred Predicted class labels, zero-based.
#' @param class_index Zero-based index of the focal class.
#' @param n_classes Number of classes; inferred when NULL.
#' @return List with estimate, sensitivity, specificity, TFN, TTN, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eqs. (4.10)-(4.11) p.132. DOI 10.1007/978-3-030-89010-0.
#' @export
Msm008 <- function(y_true, y_pred, class_index = 0, n_classes = NULL) {
  m <- .gpclassmetrics(.gpconf(y_true, y_pred, n_classes), as.integer(class_index))
  list(estimate = m$sensitivity, sensitivity = m$sensitivity,
       specificity = m$specificity, TFN = m$TFN, TTN = m$TTN,
       method = "generalized sensitivity/specificity (MVSML 2022 eq. 4.10-4.11)")
}
