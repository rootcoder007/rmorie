# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Functional-class fingerprint, Morgan radius 2 (Fcfp4).
# Bit-identical mirror of src/morie/fn/fcfp4.py; shares the internals
# defined in ecfp4.R.  Feature-class order and packing follow RDKit
# FingerprintUtil.cpp lines 172-192 (smartsPatterns, after Gobbi and
# Poppinger 1998) and lines 211-240 (getFeatureInvariants, mask 1 << i);
# rounds follow MorganGenerator.cpp lines 395-495.  Master revision
# fetched 2026-08-09, stored at
# library/pdf/fetched-wave3/rdkit-reference-source/.
# Anchored against RDKit: the packed feature codes reproduce
# rdMolDescriptors.GetFeatureInvariants exactly, and the environment and
# distinct-identifier counts reproduce RDKit's feature-invariant Morgan
# generator exactly on benzene, ethanol, aspirin (23 / 35) and caffeine
# (22 / 37).

#' Functional-class fingerprint, radius 2 (FCFP4)
#'
#' FCFP differs from ECFP only in the round-0 atom invariant.  Instead of
#' the Daylight connectivity components each atom carries a six-bit
#' pharmacophoric code: bit 0 hydrogen-bond donor, bit 1 hydrogen-bond
#' acceptor, bit 2 aromatic, bit 3 halogen, bit 4 basic, bit 5 acidic.
#' Everything after round 0 is shared with \code{Ecfp4}.
#'
#' The feature flags are an input.  Assigning them requires SMARTS
#' substructure matching against the six Gobbi-Poppinger patterns and a
#' SMARTS engine is out of scope here, so this function starts from the
#' per-atom flags such an engine would produce.  The flag order above is
#' the order of the RDKit pattern table and is what makes the codes
#' comparable with an RDKit-derived feature table.
#'
#' @param adjacency Symmetric bond-order matrix; 0 no bond, 1 single,
#'   2 double, 3 triple, 4 aromatic.
#' @param features Per-atom feature flags, a matrix with 6 columns in the
#'   order donor, acceptor, aromatic, halogen, basic, acidic; or a vector
#'   of already packed codes.
#' @param nbits Width of the folded fingerprint.
#' @param radius Morgan radius; 2 for FCFP4.
#' @return List with \code{bits}, \code{count}, \code{nset},
#'   \code{identifiers}, \code{nenv}, \code{featurecode}, \code{a},
#'   \code{nbits}, \code{radius}, \code{method}.
#' @references Rogers, D. and Hahn, M. (2010), "Extended-connectivity
#'   fingerprints", Journal of Chemical Information and Modeling 50(5),
#'   742-754, doi:10.1021/ci100050t, section on functional-class variants
#'   -- paywalled at ACS, not read for this implementation.  Feature
#'   classes: Gobbi, A. and Poppinger, D. (1998), "Genetic optimization of
#'   combinatorial libraries", Biotechnology and Bioengineering 61(1),
#'   47-54, as transcribed in the RDKit source,
#'   Code/GraphMol/Fingerprints/FingerprintUtil.cpp lines 172-192 and
#'   211-240; rounds from MorganGenerator.cpp lines 395-495.  RDKit master
#'   revision fetched 2026-08-09.  RDKit: Open-Source Cheminformatics,
#'   https://www.rdkit.org.
#' @export
#' @examples
#' Fcfp4(adjacency = 5L, features = 5L)
Fcfp4 <- function(adjacency, features, nbits = 2048, radius = 2) {
  B <- .ecfp_bonds(adjacency)
  if (is.matrix(features) || is.data.frame(features)) {
    F <- as.matrix(features); storage.mode(F) <- "double"
    if (nrow(F) != B$a) stop("features must have one entry per atom", call. = FALSE)
    if (ncol(F) != 6L) stop("features rows must have 6 flags", call. = FALSE)
    code <- numeric(B$a)
    for (i in seq_len(B$a)) {
      v <- 0
      for (k in seq_len(6L)) if (F[i, k] != 0) v <- v + 2^(k - 1L)
      code[i] <- v
    }
  } else {
    code <- as.numeric(.t1_vec(features))
    if (length(code) != B$a) stop("features must have one entry per atom", call. = FALSE)
  }
  M <- .ecfp_morgan(B, code, radius, nbits)
  .t1_result(bits = M$bits, count = M$count, nset = sum(M$bits),
             identifiers = sort(unique(M$ident)), nenv = length(M$ident),
             featurecode = code, a = B$a, nbits = as.integer(nbits),
             radius = as.integer(radius),
             method = "FCFP4 (functional-class Morgan radius 2)")
}
