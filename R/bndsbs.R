# SPDX-License-Identifier: AGPL-3.0-or-later
#' Coordinate projection of a d-dimensional identified set
#'
#' Subset inference is a projection: what a joint identified set says about
#' one coordinate is the shadow the set casts on that axis. The projection
#' is always at least as wide as any conditional slice, which is exactly
#' why projection-based inference is conservative; the reported
#' per-coordinate widths make that visible.
#'
#' Formula: \code{proj_k(H) = [min h_k, max h_k]} over the points of
#' \code{H}, for each selected coordinate \code{k}.
#'
#' @param theta_full Points of the identified set, one per row of an
#'   (m, d) matrix.
#' @param subset_idx Zero-based coordinate indices to project onto.
#' @return List with \code{lower}, \code{upper}, \code{width} for the first
#'   selected coordinate, plus \code{total_width}, \code{max_width},
#'   \code{d_subset}, \code{m}, \code{d}.
#' @references Romano, J. P. and Shaikh, A. M. (2008). Inference for
#'   identifiable parameters in partially identified econometric models.
#'   Journal of Statistical Planning and Inference 138(9), 2786-2807.
#'   \doi{10.1016/j.jspi.2008.03.015}. The coverage-of-a-component
#'   distinction is Section 4.3.3 of Molinari, F. (2021), Handbook of
#'   Econometrics 7A (arXiv:2004.11751 p. 101).
#' @export
Bndsbs <- function(theta_full, subset_idx) {
  M <- as.matrix(theta_full)
  m <- nrow(M)
  if (m == 0L) stop("Bndsbs: theta_full is empty")
  d <- ncol(M)
  idx <- as.integer(unlist(subset_idx))
  if (length(idx) == 0L) stop("Bndsbs: subset_idx is empty")
  if (any(idx < 0L) || any(idx >= d))
    stop("Bndsbs: coordinate index out of range")
  tot <- 0
  mx <- 0
  lo0 <- 0
  hi0 <- 0
  for (pos in seq_along(idx)) {
    col <- M[, idx[pos] + 1L]
    lo <- min(col)
    hi <- max(col)
    w <- hi - lo
    tot <- tot + w
    if (w > mx) mx <- w
    if (pos == 1L) { lo0 <- lo; hi0 <- hi }
  }
  .t1_result(lower = lo0, upper = hi0, width = hi0 - lo0,
             total_width = tot, max_width = mx, d_subset = length(idx),
             m = m, d = d, method = "Subset-inference bound")
}
