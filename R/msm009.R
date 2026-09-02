# SPDX-License-Identifier: AGPL-3.0-or-later
#' Brier score for categorical data.
#'
#' Formula: BS = T^-1 sum_i sum_c (pi_ic - d_ic)^2 (eq. 4.14) with d_ic the
#' indicator of the observed category. The categorical score lies in [0, 2];
#' halving puts it in [0, 1]. Lower is better.
#'
#' @param probs Matrix of predicted class probabilities, one row per case.
#' @param y_true Observed class labels, zero-based.
#' @param n_classes Number of classes; inferred when NULL.
#' @param halved If TRUE return BS/2.
#' @return List with estimate, brier, mean_log_loss, method.
#' @references Montesinos Lopez, Montesinos Lopez & Crossa (2022),
#'   Multivariate Statistical Machine Learning Methods for Genomic Prediction,
#'   Springer, eq. (4.14) p.136. DOI 10.1007/978-3-030-89010-0.
#' @export
#' @examples
#' Msm009(probs = c(1, 2, 3, 4, 5, 6, 7, 8), y_true = 5L)
Msm009 <- function(probs, y_true, n_classes = NULL, halved = FALSE) {
  bs <- .gpbrier(probs, y_true, n_classes, halved = halved)
  list(estimate = bs, brier = bs, mean_log_loss = .gpmll(probs, y_true, n_classes),
       method = "Brier score (MVSML 2022 eq. 4.14)")
}
