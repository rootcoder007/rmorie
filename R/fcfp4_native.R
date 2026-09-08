# FCFP4: functional-class fingerprint, Morgan radius 2.
# Source: Rogers, D. and Hahn, M. (2010), Extended-connectivity
# fingerprints, Journal of Chemical Information and Modeling 50(5),
# 742-754, Sec. "Feature-class variants" (FCFP): the SAME iterative
# procedure as ECFP, but the round-0 atom identifiers encode
# pharmacophoric role rather than element identity, so that atoms
# playing the same role are interchangeable.
#
# Native implementation mirroring Python morie.fn.fcfp4.  The six
# feature classes are packed into a bit code in the fixed RDKit order
# below; that order sets the bit positions and must not be permuted.

#' Pharmacophoric feature classes used by FCFP, in RDKit bit order
#'
#' The order in which \code{\link{morie_fcfp4}} packs the six
#' functional-class flags of each atom into its round-0 identifier:
#' donor, acceptor, aromatic, halogen, basic, acidic.
#' @export
MORIE_FCFP_FEATURE_CLASSES <- c("donor", "acceptor", "aromatic",
                                "halogen", "basic", "acidic")

#' FCFP4 functional-class fingerprint
#'
#' Morgan fingerprint of radius 2 in which the round-0 atom invariant
#' is the atom's pharmacophoric role instead of its element (Rogers
#' and Hahn 2010).  Two molecules that place the same roles in the
#' same topology therefore share features even when the elements
#' differ, which is what makes FCFP the scaffold-hopping variant.
#'
#' @param adjacency Square symmetric matrix of bond orders.
#' @param features Either an (a x 6) 0/1 matrix of flags in the order
#'   of \code{MORIE_FCFP_FEATURE_CLASSES}, or a length-a vector of
#'   pre-packed integer codes.
#' @param nbits Folded fingerprint length, default 2048.
#' @param radius Number of iterations, default 2 (i.e. FCFP4).
#' @return A list with \code{bits}, \code{count}, \code{nset},
#'   \code{identifiers}, \code{nenv}, \code{featurecode}, \code{a},
#'   \code{nbits}, \code{radius}, \code{method}.
#' @references Rogers, D. and Hahn, M. (2010). Extended-connectivity
#'   fingerprints. Journal of Chemical Information and Modeling,
#'   50(5), 742-754.
#' @export
#' @examples
#' morie_fcfp4(adjacency = 5L, features = 5L)
morie_fcfp4 <- function(adjacency, features, nbits = 2048L, radius = 2L) {
  bd <- .mor_fp_bonds(adjacency)
  a <- bd$a
  if (is.matrix(features) || is.data.frame(features)) {
    fm <- as.matrix(features)
    if (nrow(fm) != a) stop("features must have one entry per atom")
    if (ncol(fm) != 6L) stop("features rows must have 6 flags")
    code <- vapply(seq_len(a), function(i)
      sum(ifelse(fm[i, ] != 0, 2^(seq_len(6L) - 1L), 0)), numeric(1))
  } else {
    code <- trunc(as.numeric(features))
    if (length(code) != a) stop("features must have one entry per atom")
  }
  m <- .mor_fp_morgan(a, bd, code, as.integer(radius), as.integer(nbits))
  list(bits = m$bits, count = m$count, nset = sum(m$bits),
       identifiers = sort(unique(m$ident)), nenv = length(m$ident),
       featurecode = code, a = a, nbits = as.integer(nbits),
       radius = as.integer(radius),
       method = "FCFP4 (functional-class Morgan radius 2)")
}
