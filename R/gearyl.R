# SPDX-License-Identifier: AGPL-3.0-or-later
#' Local Geary's c per location.
#'
#' Formula: c_i = sum_j w_ij (z_i - z_j)^2 with z the standardised x
#'
#' @param x Values at the n locations.
#' @param W Spatial weights.
#' @param scale Standardise x before computing c_i.

#' @return List with ``local``, ``global_c``, ``z``, ``n``.
#' @references Anselin (2019), A Local Indicator of Multivariate Spatial Association: Extending Geary's c, Geographical Analysis 51:133-150. Paywalled; the univariate form c_i = sum_j w_ij (x_i - x_j)^2 and the standardise-first convention are as documented by spdep::localC, the reference implementation.
#' @export
Localgeary <- function(x, W, scale = TRUE) {
  x <- .t1_vec(x); W <- as.matrix(W); n <- length(x)
  z <- if (isTRUE(scale)) {
    s <- stats::sd(x)
    if (s <= 0) stop("x has zero variance")
    (x - mean(x)) / s
  } else x
  loc <- vapply(seq_len(n), function(i) sum(W[i, ] * (z[i] - z)^2), numeric(1))
  s0 <- sum(W)
  .t1_result(local = loc, global_c = if (s0 != 0) sum(loc) / (2 * s0) else NA_real_,
             z = z, n = n, method = "Local Geary's c")
}
