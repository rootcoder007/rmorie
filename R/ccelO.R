# SPDX-License-Identifier: AGPL-3.0-or-later
#' Categorical cross-entropy loss.
#'
#' Formula: L(w) = -sum_i sum_c y_ic log(yhat_ic)
#'
#' @param Y Observed class-indicator matrix, one row per record.
#' @param P Predicted class probabilities, strictly positive.
#'
#' @return List with ``loss``, ``mean_loss``, ``n``, ``C``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 10, Sect. 10.7, pp. 400-403.  Read from the chapter PDF, not recalled.  BOOK DEFECT: the display in Sect. 10.7.2 prints this loss without its leading minus sign, even though the same paragraph calls it the negative log-likelihood of a product of Bernoulli distributions and the Poisson loss two displays later does carry its sign.  What is implemented here is the quantity the surrounding text requires -- a loss that is minimised -- not the sign-dropped display.  The book has not been silently corrected elsewhere.
#' @export
Ccelo <- function(Y, P) {
  Y <- .t1_mat(Y); P <- .t1_mat(P)
  if (nrow(Y) == 0L || nrow(Y) != nrow(P) || ncol(Y) != ncol(P))
    stop("Y and P must be non-empty and the same shape")
  if (any(P <= 0)) stop("predicted probabilities must be strictly positive")
  loss <- -sum(Y * log(P))
  .t1_result(loss = loss, mean_loss = loss / nrow(Y),
             n = nrow(Y), C = ncol(Y),
             method = "Categorical cross-entropy loss, MVSML Sect. 10.7.2")
}
