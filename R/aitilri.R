# SPDX-License-Identifier: AGPL-3.0-or-later
#' Inverse ILR: back from coordinates to a closed composition
#'
#' Egozcue, Pawlowsky-Glahn, Mateu-Figueras and Barcelo-Vidal (2003),
#' "Isometric logratio transformations for compositional data analysis",
#' Mathematical Geology 35(3), 279-300, doi:10.1023/a:1023818214614 (citation
#' verified against Crossref).  The default basis is the sequential binary
#' partition printed as equation (11) of Mateu-Figueras, Pawlowsky-Glahn and
#' Egozcue, "The normal distribution in some constrained sample spaces",
#' p. 10, read as a rendered page image.
#'
#' Because the basis is orthonormal in the Aitchison inner product the inverse
#' is the perturbation-linear combination x = C(exp(V y)), with C the closure
#' to a unit total.  ilr(ilr^-1(y)) = y exactly.
#'
#' @param y the D-1 coordinates.
#' @param V optional D-by-(D-1) contrast matrix; defaults to the Egozcue et al.
#'   (2003) sequential binary partition.
#' @param kappa constant sum the result is closed to.
#' @return list: x, estimate, logx_unclosed, total, D, method.
#' @keywords internal
#' @examples
#' Aitilri(c(0, 0))$x
#' @export
Aitilri <- function(y, V = NULL, kappa = 1) {
  yy <- as.numeric(.s03vec(y))
  if (length(yy) == 0L) stop("aitchison_ilr_inverse: y is empty")
  if (!(kappa > 0)) stop("aitchison_ilr_inverse: kappa must be positive")
  Vm <- if (is.null(V)) .aitilri_basis(length(yy) + 1L) else
    matrix(as.numeric(as.matrix(V)), nrow = nrow(as.matrix(V)))
  D <- nrow(Vm); p <- ncol(Vm)
  if (p != length(yy)) {
    stop(sprintf("aitchison_ilr_inverse: V has %d columns but y has %d entries", p, length(yy)))
  }
  lx <- numeric(D)
  for (j in seq_len(D)) {
    s <- 0
    for (i in seq_len(p)) s <- s + Vm[j, i] * yy[i]
    lx[j] <- s
  }
  m <- max(lx)
  e <- exp(lx - m)
  tot <- 0
  for (v in e) tot <- tot + v
  x <- kappa * e / tot
  list(x = x, estimate = x[1], logx_unclosed = lx, total = kappa, D = D,
       method = "x = C(exp(V y)), V the Egozcue et al. (2003) SBP basis")
}

#' Contrast matrix of the default Egozcue (2003) sequential binary partition
#'
#' @param D number of parts.
#' @return a D-by-(D-1) numeric contrast matrix.
#' @keywords internal
#' @noRd
.aitilri_basis <- function(D) {
  if (D < 2L) stop("aitchison_ilr_inverse: a composition needs at least 2 parts")
  V <- matrix(0, nrow = D, ncol = D - 1L)
  for (i in seq_len(D - 1L)) {
    cc <- sqrt(i / (i + 1))
    for (j in seq_len(i)) V[j, i] <- cc / i
    V[i + 1L, i] <- -cc
  }
  V
}
