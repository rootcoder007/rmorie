# SPDX-License-Identifier: AGPL-3.0-or-later
#' One symmetric-normalised graph convolution
#'
#' The normalisation is the design. A plain adjacency multiply lets
#' high-degree nodes dominate and makes features explode over layers;
#' \code{D^-1/2 A D^-1/2} bounds the spectrum at one. The self-loop stops
#' a node forgetting itself at every step.
#'
#' Formula: \code{X^(k+1) = sigma(A_hat X^(k) W^(k))},
#' \code{A_hat = Dt^-1/2 (A + I) Dt^-1/2}.
#'
#' @param A_hat Adjacency; renormalised here, so pass the raw A.
#' @param X Node features.
#' @param W Layer weights.
#' @param activation "relu" or "none".
#' @return List with \code{X_next}, \code{estimate}, \code{A_norm},
#'   \code{n}, \code{f_out}.
#' @references Kipf, T. N. & Welling, M. (2017). ICLR 2017, equation (2).
#' @export
Sgtgrn <- function(A_hat, X, W, activation = "relu") {
  A <- as.matrix(A_hat); n <- nrow(A)
  Ai <- A + diag(1, n)
  d <- rowSums(Ai)
  An <- Ai / outer(sqrt(d), sqrt(d))
  An[!is.finite(An)] <- 0
  Z <- An %*% as.matrix(X) %*% as.matrix(W)
  if (identical(activation, "relu")) Z <- pmax(Z, 0)
  .t1_result(X_next = Z, estimate = sum(Z) / length(Z), A_norm = An, n = n,
             f_out = ncol(Z),
             method = "Symmetric-normalised graph convolution")
}
