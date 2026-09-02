# SPDX-License-Identifier: AGPL-3.0-or-later
#' Rectifier linear unit activation and its gradient
#'
#' Formula: g(z) = max(0, z);  g'(z) = 1 if z > 0, else 0
#'
#' @param z Pre-activation values.
#' @param slope Slope applied below the threshold; 0 gives the plain ReLU, a small positive value gives the leaky ReLU of Sect. 10.3.3.
#'
#' @return List with ``activation``, ``gradient``, ``n``.
#' @references Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate Statistical Machine Learning Methods for Genomic Prediction, Springer, doi:10.1007/978-3-030-89010-0.  Chapter 10, Sect. 10.3.2 p. 388: g(z) = max(0, z), flat below the threshold and linear above it; Sect. 10.3.3 p. 389 gives the leaky variant with slope alpha below zero (the figure uses alpha = 0.1).  Read from the chapter PDF, not recalled.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Reluact(V)
Reluact <- function(z, slope = 0) {
  z <- .t1_vec(z)
  slope <- as.numeric(slope)
  .t1_result(activation = ifelse(z > 0, z, slope * z),
             gradient = ifelse(z > 0, 1, slope), n = length(z),
             method = "ReLU activation, MVSML Sect. 10.3.2")
}
