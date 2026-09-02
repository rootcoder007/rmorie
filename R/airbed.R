# SPDX-License-Identifier: AGPL-3.0-or-later
#' Activity-data times emission-factor inventory
#'
#' Emissions = AD * EF, summed over the cells of a sector-by-fuel table
#' and optionally weighted by per-gas global warming potentials.
#'
#' @param activity Activity data, one entry per sector-fuel cell.
#' @param factor Emission factors, same shape as activity.
#' @param gwp Per-column global warming potentials, or NULL.
#'
#' @return List with total, cell, bysector, byfuel, s, f.
#' @references IPCC (2006), 2006 IPCC Guidelines for National Greenhouse
#'   Gas Inventories, Volume 1, Chapter 1, Sect. 1.2.  Read from the
#'   official PDF at www.ipcc-nggip.iges.or.jp.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Emisinv(V, V)
Emisinv <- function(activity, factor, gwp = NULL) {
  A <- .t1_mat(activity); E <- .t1_mat(factor)
  if (nrow(A) != nrow(E) || ncol(A) != ncol(E))
    stop("activity and factor must have the same shape")
  s <- nrow(A); f <- ncol(A)
  g <- if (is.null(gwp)) rep(1, f) else .t1_vec(gwp)
  if (length(g) != f) stop("gwp must have one entry per column")
  cell <- A * E * rep(g, each = s)
  dim(cell) <- c(s, f)
  .t1_result(total = sum(cell), cell = cell, bysector = rowSums(cell),
             byfuel = colSums(cell), s = s, f = f,
             method = "IPCC inventory equation Emissions = AD * EF")
}
