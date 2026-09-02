# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Extended-connectivity fingerprint, Morgan radius 3 (Ecfp6).
# Bit-identical mirror of src/morie/fn/ecfp6.py; shares the internals
# defined in ecfp4.R.  Specification: RDKit MorganGenerator.cpp lines
# 395-495 and FingerprintUtil.cpp lines 242-265, master revision fetched
# 2026-08-09, stored at library/pdf/fetched-wave3/rdkit-reference-source/.
# Anchored against RDKit: environment and distinct-identifier counts
# reproduce RDKit exactly on benzene (4 / 19), ethanol, aspirin (32 / 42)
# and caffeine (34 / 46) at radius 3.

#' Extended-connectivity fingerprint, radius 3 (ECFP6)
#'
#' ECFP diameter 6 is Morgan radius 3.  The machinery is that of
#' \code{Ecfp4} -- the same round-0 Daylight invariants, the same
#' relabelling and the same retirement of duplicate environments -- run
#' for one further round, so the identifier set of ECFP6 contains that of
#' ECFP4 on the same molecule.  That containment is exact and is one of
#' the anchors for this function.
#'
#' The molecule arrives as a pre-parsed graph: \code{adjacency} is a
#' symmetric bond-order matrix with 0 no bond, 1 single, 2 double,
#' 3 triple and 4 aromatic, an encoding that is this implementation's own.
#' SMILES parsing and aromaticity perception are out of scope.  The hash
#' is the stated closed form \eqn{h \leftarrow (1000003 h + v) \bmod
#' (2^{31}-1)} rather than the Boost hash RDKit uses, so bit indices are
#' this implementation's own while the identifier partition is the
#' published one.
#'
#' @param adjacency Symmetric bond-order matrix; 0 means no bond.
#' @param atomnum Atomic number per atom.
#' @param numhs Attached hydrogen count per atom; default 0.
#' @param charge Formal charge per atom; default 0.
#' @param inring Ring-membership flag per atom; default 0.
#' @param isotope_delta Isotope mass delta per atom; default 0.
#' @param nbits Width of the folded fingerprint.
#' @param radius Morgan radius; 3 for ECFP6.
#' @return List with \code{bits}, \code{count}, \code{nset},
#'   \code{identifiers}, \code{nenv}, \code{a}, \code{nbits},
#'   \code{radius}, \code{method}.
#' @references Rogers, D. and Hahn, M. (2010), "Extended-connectivity
#'   fingerprints", Journal of Chemical Information and Modeling 50(5),
#'   742-754, doi:10.1021/ci100050t -- paywalled at ACS, not read for this
#'   implementation.  Specification followed: the RDKit reference
#'   implementation, Code/GraphMol/Fingerprints/MorganGenerator.cpp lines
#'   395-495 and Code/GraphMol/Fingerprints/FingerprintUtil.cpp lines
#'   242-265, master revision fetched 2026-08-09.  RDKit: Open-Source
#'   Cheminformatics, https://www.rdkit.org.
#' @export
#' @examples
#' Ecfp6(adjacency = 5L, atomnum = 5L)
Ecfp6 <- function(adjacency, atomnum, numhs = NULL, charge = NULL,
                  inring = NULL, isotope_delta = NULL, nbits = 2048,
                  radius = 3) {
  B <- .ecfp_bonds(adjacency)
  at <- as.numeric(.t1_vec(atomnum))
  if (length(at) != B$a) stop("atomnum must have one entry per atom", call. = FALSE)
  nh <- .ecfp_percol(numhs, B$a, 0)
  ch <- .ecfp_percol(charge, B$a, 0)
  ir <- .ecfp_percol(inring, B$a, 0)
  isd <- .ecfp_percol(isotope_delta, B$a, 0)
  inv <- .ecfp_conninv(B, at, nh, ch, ir, isd)
  M <- .ecfp_morgan(B, inv, radius, nbits)
  .t1_result(bits = M$bits, count = M$count, nset = sum(M$bits),
             identifiers = sort(unique(M$ident)), nenv = length(M$ident),
             a = B$a, nbits = as.integer(nbits), radius = as.integer(radius),
             method = "ECFP6 (Morgan radius 3), Rogers-Hahn / RDKit")
}
