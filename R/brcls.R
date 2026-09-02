# SPDX-License-Identifier: AGPL-3.0-or-later
#' Brier score for a categorical predictive distribution
#'
#' Formula: BS = (1/T) sum_i sum_c (pihat_ic - d_ic)^2,  d_ic = 1 iff y_i = c
#'
#' @param P Predicted category probabilities, one row per observation.
#' @param y Observed category index, 1-based, length T.
#'
#' @return List with ``brier``, ``brier_scaled``, ``n``, ``C``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 4, Eq. (4.14) p. 136.  The book notes the categorical Brier score ranges over \[0, 2\] and suggests reporting BS/2; ``brier_scaled`` is that halved value.  Read from the chapter PDF, not recalled.
#' @export
#' @examples
#' set.seed(1)
#' P <- matrix(runif(30), 10, 3)
#' P <- P / rowSums(P)
#' y <- sample(1:3, 10, replace = TRUE)
#' Brierscore(P, y)
Brierscore <- function(P, y) {
  P <- as.matrix(P); y <- as.integer(y)
  Tn <- nrow(P); K <- ncol(P)
  if (Tn == 0L) stop("p must have at least one row")
  if (length(y) != Tn) stop("y must have one entry per row of p")
  if (any(y < 1L | y > K)) stop("y must be a 1-based category index")
  D <- matrix(0, Tn, K)
  D[cbind(seq_len(Tn), y)] <- 1
  bs <- sum((P - D)^2) / Tn
  .t1_result(brier = bs, brier_scaled = bs / 2, n = Tn, C = K,
             method = "Brier score, MVSML Eq. (4.14)")
}
