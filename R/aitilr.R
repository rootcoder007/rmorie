# SPDX-License-Identifier: AGPL-3.0-or-later
#' Isometric log-ratio (ILR) transform via a sequential binary partition
#'
#' Source consulted as a rendered page image, not the OCR text layer:
#' Mateu-Figueras, Pawlowsky-Glahn and Egozcue, "The normal distribution in
#' some constrained sample spaces", pp. 10-11, which prints the Aitchison
#' inner product <x, x*>_a = (1/D) sum_\{i<j\} ln(x_i/x_j) ln(x*_i/x*_j),
#' equation (10), and the default orthonormal basis of Egozcue et al. (2003),
#' y_i = 1/sqrt(i(i+1)) * ln((x_1 x_2 ... x_i) / x_\{i+1\}^i), i = 1..D-1,
#' equation (11).
#'
#' Primary reference verified against Crossref: Egozcue, Pawlowsky-Glahn,
#' Mateu-Figueras and Barcelo-Vidal (2003), "Isometric logratio
#' transformations for compositional data analysis", Mathematical Geology
#' 35(3), 279-300, doi:10.1023/a:1023818214614.
#'
#' Equation (11) is algebraically the contrast form used here,
#' y_i = sqrt(i/(i+1)) * ((1/i) sum_\{j<=i\} ln x_j - ln x_\{i+1\}) = v_i' clr(x),
#' with clr(x) = ln x - mean(ln x); the mean subtraction cancels because every
#' contrast column sums to zero, so the two routes agree exactly.
#'
#' @param x a D-part composition with strictly positive entries; it need not
#'   be closed, the transform being scale invariant.
#' @param V optional D-by-(D-1) contrast matrix whose columns are the clr
#'   coefficients of an orthonormal basis.  Defaults to the Egozcue et al.
#'   (2003) sequential binary partition of equation (11).
#' @return list: y, estimate, clr, norm, aitchison_norm, D, method.
#' @keywords internal
#' @examples
#' Aitilr(c(0.2, 0.3, 0.5))$y
#' @export
Aitilr <- function(x, V = NULL) {
  xx <- as.numeric(.s03vec(x))
  if (length(xx) < 2L) stop("aitchison_ilr: a composition needs at least 2 parts")
  if (any(!(xx > 0))) stop("aitchison_ilr: every part must be strictly positive")
  D <- length(xx)
  Vm <- if (is.null(V)) .aitilr_basis(D) else matrix(as.numeric(as.matrix(V)),
                                                     nrow = nrow(as.matrix(V)))
  if (nrow(Vm) != D) stop("aitchison_ilr: V has the wrong number of rows")
  p <- ncol(Vm)
  lg <- log(xx)
  z <- lg - sum(lg) / D
  y <- numeric(p)
  for (i in seq_len(p)) {
    s <- 0
    for (j in seq_len(D)) s <- s + Vm[j, i] * z[j]
    y[i] <- s
  }
  nrm <- 0
  for (v in y) nrm <- nrm + v * v
  nrm <- sqrt(nrm)
  a2 <- 0
  for (i in seq_len(D)) {
    for (j in seq_len(D)) {
      if (j > i) {
        d <- lg[i] - lg[j]
        a2 <- a2 + d * d
      }
    }
  }
  a2 <- a2 / D
  list(y = y, estimate = if (p > 0L) y[1] else NA_real_, clr = z, norm = nrm,
       aitchison_norm = sqrt(a2), D = D,
       method = "ilr(x) = V' clr(x), V the Egozcue et al. (2003) SBP basis, eq. (11)")
}

#' Contrast matrix of the default Egozcue (2003) sequential binary partition
#'
#' Column i holds +sqrt(i/(i+1))/i on rows 1..i, -sqrt(i/(i+1)) on row i+1 and
#' zero elsewhere, so every column sums to zero and has unit Euclidean norm.
#'
#' @param D number of parts.
#' @return a D-by-(D-1) numeric contrast matrix.
#' @noRd
.aitilr_basis <- function(D) {
  if (D < 2L) stop("aitchison_ilr: a composition needs at least 2 parts")
  V <- matrix(0, nrow = D, ncol = D - 1L)
  for (i in seq_len(D - 1L)) {
    cc <- sqrt(i / (i + 1))
    for (j in seq_len(i)) V[j, i] <- cc / i
    V[i + 1L, i] <- -cc
  }
  V
}
