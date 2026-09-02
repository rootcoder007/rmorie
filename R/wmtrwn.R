# SPDX-License-Identifier: AGPL-3.0-or-later
#' Row-normalised spatial weights
#'
#' Formula: w'_ij = w_ij / sum_j w_ij
#'
#' @param W Spatial weights matrix.

#' @param W See Usage.
#' @return List with ``W`` (normalised), ``row_sums``, ``islands``, ``n``.
#' @references Anselin (1988), Spatial Econometrics: Methods and Models, Kluwer. Not held locally; row standardisation w_ij / sum_j w_ij is the standard published convention and is what spdep's style = 'W' computes.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Rownorm(V)
Rownorm <- function(W) {
  W <- as.matrix(W)
  n <- nrow(W)
  rs <- rowSums(W)
  out <- W
  for (i in seq_len(n)) out[i, ] <- if (rs[i] != 0) W[i, ] / rs[i] else 0
  .t1_result(W = out, row_sums = rs, islands = which(rs == 0) - 1L, n = n,
             method = "Row-normalised spatial weights")
}
