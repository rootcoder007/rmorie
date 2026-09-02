# SPDX-License-Identifier: AGPL-3.0-or-later
#' Geometric median of a composition in clr coordinates
#'
#' clr(x)_j = log(x_j / (prod_l x_l)^(1/D)); the spatial median of the
#' clr scores minimises sum_i ||clr(x_i) - m|| and is found by the
#' Weiszfeld iteration m <- sum_i y_i/d_i / sum_i 1/d_i.  The estimate is
#' mapped back to the simplex by the inverse clr.
#'
#' @param X Strictly positive compositions, one per row.
#' @param steps Fixed Weiszfeld iteration count.
#' @param eps Distances below this are treated as coincident.
#'
#' @return List with median, clrmed, objective, clrmean, n, D, steps.
#' @references Filzmoser and Hron (2008), Mathematical Geosciences 40(3),
#'   233-248; clr is Aitchison's transform and the spatial median is
#'   Weiszfeld's (1937).  The article is paywalled and was not read; only
#'   the stated construction is claimed.
#' @export
#' @examples
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' Clrmedian(M)
Clrmedian <- function(X, steps = 100, eps = 1e-12) {
  M <- .t1_mat(X); n <- nrow(M); D <- ncol(M)
  if (n == 0 || D < 2)
    stop("need at least one composition with two parts")
  if (any(M <= 0)) stop("compositions must be strictly positive")
  L <- log(M)
  Y <- L - rowMeans(L)
  m <- colMeans(Y)
  cmean <- m
  for (k in seq_len(as.integer(steps))) {
    d <- sqrt(rowSums((Y - rep(m, each = n))^2))
    keep <- d >= eps
    if (!any(keep)) break
    den <- sum(1 / d[keep])
    if (den == 0) break
    m <- colSums(Y[keep, , drop = FALSE] / d[keep]) / den
  }
  obj <- sum(sqrt(rowSums((Y - rep(m, each = n))^2)))
  e <- exp(m)
  .t1_result(median = e / sum(e), clrmed = m, objective = obj,
             clrmean = cmean, n = n, D = D, steps = as.integer(steps),
             method = "Spatial median in clr coordinates (Weiszfeld iteration)")
}
