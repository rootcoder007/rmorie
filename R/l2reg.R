# SPDX-License-Identifier: AGPL-3.0-or-later
#' Ridge (weight-decay) regularized loss
#'
#' Formula: L(w, lambda) = L(w) + 0.5 * lambda * w'w
#'
#' @param loss Unregularized loss L(w).
#' @param w Network weights.
#' @param lam Penalty strength lambda; must be non-negative.
#'
#' @return List with ``penalized_loss``, ``penalty``, ``ep``, ``lambda``, ``p``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 10, Sect. 10.7.3 p. 403: L(w, lambda) = L(w) + 0.5 * lambda * E_P, with E_P = w'w for the ridge (weight decay, L2) penalty.  Read from the chapter PDF, not recalled.
#' @export
L2pen <- function(loss, w, lam) {
  w <- .t1_vec(w); lam <- as.numeric(lam)
  if (lam < 0) stop("lambda must be non-negative")
  ep <- sum(w^2); pen <- 0.5 * lam * ep
  .t1_result(penalized_loss = as.numeric(loss) + pen, penalty = pen, ep = ep,
             lambda = lam, p = length(w),
             method = "L2 (ridge) regularized loss, MVSML Sect. 10.7.3")
}
