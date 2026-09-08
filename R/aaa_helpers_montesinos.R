# SPDX-License-Identifier: AGPL-3.0-or-later

# Shared Montesinos-suite helpers.
#
# Sourced before gblpf.R / mtgbl.R / mrkvr.R so callers don't depend on R's
# alphabetical load order. Loaded first via the leading underscore in the
# filename and via the explicit Collate: field in DESCRIPTION.

#' VanRaden Genomic Relationship Matrix
#'
#' \describe{
#'   \item{`method = 1` / `"G_VR1"`}{VanRaden Method 1:
#'     \eqn{G = ZZ' / (2 \sum_j p_j(1 - p_j))}, with \eqn{Z = M - 2p}.}
#'   \item{`method = 2` / `"G_VR2"`}{VanRaden Method 2: \eqn{G = ZDZ'} with
#'     \eqn{D} diagonal, \eqn{D_{jj} = 1/(m \cdot 2p_j(1-p_j))}; equivalently
#'     each column of \eqn{Z} scaled by \eqn{\sqrt{2p_j(1-p_j)}} and divided
#'     by \eqn{m}.}
#'   \item{`method = 3` / `"G_XX"`}{Uncentred \eqn{G = MM'/m}. This is NOT one
#'     of VanRaden's three methods -- it is the first method of the Montesinos
#'     chapter, which renumbers VanRaden's scheme. Named `G_XX` rather than
#'     `G_VR*` for that reason.}
#' }
#'
#' VanRaden's own Method 3 regresses \eqn{MM'} on \eqn{A} and requires no
#' allele frequencies; it is not implemented here, so there is deliberately no
#' `"G_VR3"` alias.
#'
#' @param markers Numeric (n x m) genotype matrix coded 0/1/2. Must be a
#'   matrix or data frame: a bare vector is rejected rather than silently
#'   reshaped into an n x 1 matrix.
#' @param method  One of `1`, `2`, `3`, `"G_VR1"`, `"G_VR2"`, `"G_XX"`.
#'   Integer (`2L`) and double (`2`) forms are both accepted.
#' @return Named list with estimate (G matrix), diag_mean, off_mean, p, n, m, method.
#' @references VanRaden, P. M. (2008). Efficient methods to compute genomic
#'   predictions. J Dairy Sci 91(11):4414-4423, "Genomic Relationships and
#'   Inbreeding", p.4416. doi:10.3168/jds.2007-0980
#'
#'   Montesinos-Lopez et al., Multivariate Statistical Machine Learning Methods
#'   for Genomic Prediction, Sec. 2.4, pp.49-52 (secondary; renumbers the
#'   methods and contradicts its own worked example twice -- see the morie
#'   fixture tests/fn/fixtures/gmatv.json).
#' @examples
#' morie_grm_vanraden(markers = matrix(sample(0:2, 200, TRUE), 50, 4))
#' morie_grm_vanraden(matrix(sample(0:2, 200, TRUE), 50, 4), method = "G_VR2")
#' @export
morie_grm_vanraden <- function(markers, method = 1) {
  ## A bare vector used to be accepted: as.matrix(c(1,2,3)) yields a 3 x 1
  ## matrix, so the call silently became "three lines, one marker" instead of
  ## erroring. morie's Python raises for ndim != 2; match that.
  if (is.null(dim(markers))) {
    stop("`markers` must be a 2D (n x m) matrix or data frame, not a vector.")
  }
  M <- as.matrix(markers)
  storage.mode(M) <- "double"
  n <- nrow(M)
  m <- ncol(M)

  ## Dispatch used to be `identical(method, 2)`, which is FALSE for the
  ## integer literal 2L -- so morie_grm_vanraden(M, method = 2L), the natural
  ## way to write an integer in R, silently computed Method 1 and reported it
  ## as such. Any unrecognised value fell through to Method 1 as well. Both
  ## are now explicit.
  mode <- if (is.character(method)) {
    switch(method,
      G_VR1 = 1L,
      G_VR2 = 2L,
      G_XX = 3L,
      stop(
        "method must be one of: 1, 2, 3, 'G_VR1', 'G_VR2', 'G_XX' (got '",
        method, "')"
      )
    )
  } else if (is.numeric(method) && length(method) == 1L && method %in% c(1, 2, 3)) {
    as.integer(method)
  } else {
    stop(
      "method must be one of: 1, 2, 3, 'G_VR1', 'G_VR2', 'G_XX' (got ",
      paste(format(method), collapse = ", "), ")"
    )
  }

  p <- colMeans(M) / 2
  if (mode == 3L) {
    ## Montesinos Method 1: uncentred, divided by the marker count.
    Z <- M
    denom <- m
    method_str <- "G_XX (uncentred MM'/m; Montesinos Method 1, not VanRaden)"
  } else if (mode == 2L) {
    Z <- sweep(M, 2, 2 * p, "-")
    s <- sqrt(2 * p * (1 - p))
    s[s <= 0] <- 1
    Z <- sweep(Z, 2, s, "/")
    denom <- m
    method_str <- "VanRaden Method 2 / G_VR2 (ZDZ', per-locus scaled)"
  } else {
    Z <- sweep(M, 2, 2 * p, "-")
    denom <- 2 * sum(p * (1 - p))
    if (denom <= 0) denom <- 1
    method_str <- "VanRaden Method 1 / G_VR1 (ZZ'/2sum-pq)"
  }
  G <- tcrossprod(Z) / denom
  diag_mean <- mean(diag(G))
  off <- G
  diag(off) <- 0
  off_mean <- if (n > 1) sum(off) / (n * (n - 1)) else 0
  list(
    estimate = G, diag_mean = diag_mean, off_mean = off_mean,
    p = p, n = n, m = m, method = method_str
  )
}

# CANONICAL TEST
# set.seed(0); M <- matrix(sample(0:2, 20, TRUE), 4, 5)
# morie_grm_vanraden(M)$diag_mean  # ~1 in expectation
