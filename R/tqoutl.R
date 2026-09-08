# SPDX-License-Identifier: AGPL-3.0-or-later
#' Split coordinates into outlier and non-outlier sets, bits allocated apart
#'
#' The split is by MAGNITUDE RANK, not an absolute threshold, so the
#' outlier count is exactly ceiling(frac * d) regardless of scale. Ties
#' break on the lower index. The average bit-width comes out non-integer,
#' which is the point of the construction.
#'
#' Formula: O = the ceiling(frac d) coordinates of largest |x_j|;
#'   effective bits = (|O| b_out + (d - |O|) b_in) / d
#'
#' @param x The vector whose channels are split.
#' @param b_out Bits per outlier coordinate.
#' @param b_in Bits per non-outlier coordinate.
#' @param frac Fraction of coordinates treated as outliers, in (0, 1).
#' @return List with \code{outlier_index}, \code{n_outlier},
#'   \code{effective_bits}, \code{outlier_energy}, \code{threshold},
#'   \code{d}.
#' @references Zandieh et al., arXiv:2504.19874, on splitting channels
#'   into outlier and non-outlier sets and applying two independent
#'   TurboQuant instances with higher precision for outliers. Fetched from
#'   arXiv. The paper does not fix the split RULE; magnitude-rank
#'   selection is used here and documented as such.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Outsplit(V)
Outsplit <- function(x, b_out = 8, b_in = 2, frac = 0.01) {
  x <- .t1_vec(x)
  d <- length(x)
  if (d < 1L) stop("the vector must be non-empty")
  bo <- as.integer(b_out)
  bi <- as.integer(b_in)
  if (bo < 1L || bi < 1L) stop("both bit widths must be at least 1")
  if (bo < bi) stop("outliers must not get fewer bits than the bulk")
  f <- as.numeric(frac)
  if (f <= 0 || f >= 1) stop("frac must lie strictly between 0 and 1")
  k <- max(1L, as.integer(ceiling(f * d)))
  if (k >= d) stop("frac selects every coordinate as an outlier")
  ord <- order(-abs(x), seq_len(d))
  sel <- sort(ord[seq_len(k)])
  tot <- sum(x^2)
  .t1_result(outlier_index = sel, n_outlier = as.numeric(k),
             effective_bits = (k * bo + (d - k) * bi) / d,
             outlier_energy = if (tot > 0) sum(x[sel]^2) / tot else NaN,
             threshold = abs(x[ord[k]]), d = as.numeric(d),
             method = "Outlier channel split with per-set bit allocation")
}
