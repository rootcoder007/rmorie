# SPDX-License-Identifier: AGPL-3.0-or-later
#' Local Moran's I (LISA).
#'
#' Formula: I_i = z_i sum_j w_ij z_j / m2, z_i = x_i - xbar, m2 = sum_i z_i^2 / n
#'
#' @param x Values at the n locations.
#' @param W Spatial weights.
#' @param mlvar Divide m2 by n (the default) rather than n-1.

#' @return List with ``local``, ``global_i``, ``m2``, ``z``, ``lag``, ``n``.
#' @references Anselin (1995), Local Indicators of Spatial Association -- LISA, Geographical Analysis 27(2):93-115, formula (12) p.99. The article is paywalled; the formula and the divide-by-n variance convention were taken from spdep::localmoran, the reference implementation, which cites that equation explicitly.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Localmoran(V, V)
Localmoran <- function(x, W, mlvar = TRUE) {
  x <- .t1_vec(x); W <- as.matrix(W); n <- length(x)
  z <- x - mean(x)
  m2 <- sum(z^2) / (if (isTRUE(mlvar)) n else n - 1)
  lag <- as.numeric(W %*% z)
  loc <- z * lag / m2
  s0 <- sum(W)
  .t1_result(local = loc, global_i = if (s0 != 0) sum(loc) / s0 else NA_real_,
             m2 = m2, z = z, lag = lag, n = n,
             method = "Local Moran's I (LISA)")
}
