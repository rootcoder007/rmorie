# SPDX-License-Identifier: AGPL-3.0-or-later
#' Binary cross-entropy (logistic) loss.
#'
#' Formula: L(w) = -sum_i sum_j [ y_ij log(yhat_ij) + (1 - y_ij) log(1 - yhat_ij) ]
#'
#' @param Y Observed 0/1 outcomes; a flat vector is read as one column.
#' @param P Predicted success probabilities, strictly inside (0, 1).
#'
#' @return List with ``loss``, ``mean_loss``, ``n``, ``L``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 10, Sect. 10.7, pp. 400-403.  Read from the chapter PDF, not recalled.  BOOK DEFECT: the display in Sect. 10.7.2 prints this loss without its leading minus sign, even though the same paragraph calls it the negative log-likelihood of a product of Bernoulli distributions and the Poisson loss two displays later does carry its sign.  What is implemented here is the quantity the surrounding text requires -- a loss that is minimised -- not the sign-dropped display.  The book has not been silently corrected elsewhere.
#' @export
#' @examples
#' Bcelo(Y = 5L, P = 0.5)
Bcelo <- function(Y, P) {
  Y <- .t1_mat(Y); P <- .t1_mat(P)
  if (nrow(Y) == 0L || nrow(Y) != nrow(P) || ncol(Y) != ncol(P))
    stop("Y and P must be non-empty and the same shape")
  if (any(P <= 0) || any(P >= 1))
    stop("predicted probabilities must lie strictly in (0, 1)")
  loss <- -sum(Y * log(P) + (1 - Y) * log(1 - P))
  .t1_result(loss = loss, mean_loss = loss / (nrow(Y) * ncol(Y)),
             n = nrow(Y), L = ncol(Y),
             method = "Binary cross-entropy loss, MVSML Sect. 10.7.2")
}
