# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cohen's Kappa coefficient of agreement for a two-class confusion table
#'
#' Formula: kappa = (P0 - Pe) / (1 - Pe),  Pe = (tp+fn)/n*(tp+fp)/n + (fp+tn)/n*(fn+tn)/n
#'
#' @param tp True positives.
#' @param fp False positives.
#' @param fn False negatives.
#' @param tn True negatives.
#'
#' @return List with ``kappa``, ``p0``, ``pe``, ``n``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 4, Sect. 4.5.2, p. 134, which gives kappa = (P0 - Pe)/(1 - Pe) with P0 the proportion correctly classified and Pe as written above; the book attributes it to Cohen (1960).  Read from the chapter PDF, not recalled.
#' @export
Kappacoef <- function(tp, fp, fn, tn) {
  tp <- as.numeric(tp); fp <- as.numeric(fp)
  fn <- as.numeric(fn); tn <- as.numeric(tn)
  n <- tp + fp + fn + tn
  if (n <= 0) stop("the confusion table must have at least one observation")
  p0 <- (tp + tn) / n
  pe <- ((tp + fn) / n) * ((tp + fp) / n) + ((fp + tn) / n) * ((fn + tn) / n)
  if (pe == 1) stop("kappa is undefined when the chance agreement is 1")
  .t1_result(kappa = (p0 - pe) / (1 - pe), p0 = p0, pe = pe, n = n,
             method = "Cohen's kappa, MVSML Sect. 4.5.2")
}
