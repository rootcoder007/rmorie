# ECFP6: extended-connectivity fingerprint at Morgan radius 3.
# Source: Rogers, D. and Hahn, M. (2010), Extended-connectivity
# fingerprints, Journal of Chemical Information and Modeling 50(5),
# 742-754.  Their naming convention is diameter-based: ECFP_n uses
# n/2 iterations, so ECFP6 is three iterations of the same procedure
# that ECFP4 (\code{\link{morie_ecfp4}}) runs twice.
#
# Native implementation mirroring Python morie.fn.ecfp6, which is the
# same call onto the shared Morgan machinery with radius 3.

#' ECFP6 extended-connectivity fingerprint
#'
#' Morgan fingerprint of radius 3 (Rogers and Hahn 2010).  Identical
#' to \code{\link{morie_ecfp4}} except for the extra iteration, so
#' every ECFP4 feature also appears here and the extra features
#' describe the third shell around each atom.
#'
#' @inheritParams morie_ecfp4
#' @param radius Number of iterations, default 3 (i.e. ECFP6).
#' @return A list with \code{bits}, \code{count}, \code{nset},
#'   \code{identifiers}, \code{nenv}, \code{a}, \code{nbits},
#'   \code{radius}, \code{method}.
#' @references Rogers, D. and Hahn, M. (2010). Extended-connectivity
#'   fingerprints. Journal of Chemical Information and Modeling,
#'   50(5), 742-754.
#' @export
#' @examples
#' morie_ecfp6(adjacency = 5L, atomnum = 5L)
morie_ecfp6 <- function(adjacency, atomnum, numhs = NULL, charge = NULL,
                        inring = NULL, isotope_delta = NULL,
                        nbits = 2048L, radius = 3L) {
  bd <- .mor_fp_bonds(adjacency)
  a <- bd$a
  at <- trunc(as.numeric(atomnum))
  if (length(at) != a) stop("atomnum must have one entry per atom")
  df <- .mor_fp_defaults(a, numhs, charge, inring, isotope_delta)
  inv <- .mor_fp_invariants(a, bd, at, df$numhs, df$charge, df$inring,
                            df$isotope_delta)
  m <- .mor_fp_morgan(a, bd, inv, as.integer(radius), as.integer(nbits))
  list(bits = m$bits, count = m$count, nset = sum(m$bits),
       identifiers = sort(unique(m$ident)), nenv = length(m$ident),
       a = a, nbits = as.integer(nbits), radius = as.integer(radius),
       method = "ECFP6 (Morgan radius 3), Rogers-Hahn / RDKit")
}
