# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mantel-Haenszel pooled odds ratio, given four parallel cell vectors
#'
#' An alias. The estimator is \code{\link{Mhors}}; the audit at
#' \code{ledger/wave2/DUPMAP.tsv} records \code{mamh} as a duplicate of
#' \code{mhors} and it is the same estimator with the same
#' Robins-Breslow-Greenland variance. Carrying the arithmetic twice would
#' mean two copies that agree with each other at 1e-9 forever and are
#' never checked against anything else, so this only adapts the calling
#' convention: four vectors in, one matrix of strata out.
#'
#' Formula: \code{OR_MH = sum(a_k d_k/n_k) / sum(b_k c_k/n_k)} -- Mantel
#' and Haenszel (1959); Robins, Breslow and Greenland (1986).
#'
#' @param a,b,c,d Per-stratum cells: exposed cases, exposed non-cases,
#'   unexposed cases, unexposed non-cases.
#' @param confidence Confidence level.
#' @return Whatever \code{\link{Mhors}} returns, unchanged.
#' @references Mantel, N. and Haenszel, W. (1959). Journal of the National
#'   Cancer Institute 22(4):719-748. \doi{10.1093/jnci/22.4.719}.
#' @seealso \code{\link{Mhors}}
#' @export
#' @examples
#' Mamh(a = c(1, 2, 3, 4, 5, 6, 7, 8), b = c(1, 2, 3, 4, 5, 6, 7, 8), c = c(1, 2, 3, 4, 5, 6, 7, 8), d = c(1, 2, 3, 4, 5, 6, 7, 8))
Mamh <- function(a, b, c, d, confidence = 0.95) {
  a <- as.numeric(a); b <- as.numeric(b)
  c <- as.numeric(c); d <- as.numeric(d)
  if (length(b) != length(a) || length(c) != length(a) ||
      length(d) != length(a))
    stop("the four cell vectors must have equal length")
  Mhors(cbind(a, b, c, d), confidence)
}
