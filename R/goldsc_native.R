# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of goldsc -- GoldScore, the genetic-algorithm docking fitness.
# Mirrors src/morie/fn/goldsc.py operation for operation, on the shared
# numerics in R/aaa_helpers_w3num.R.
#
# GoldScore is the older of GOLD's two main fitness functions and it is a
# different animal from ChemScore. ChemScore was fitted by regression
# against measured affinities and tries to predict them. GoldScore was
# tuned to predict the POSE -- where the ligand sits -- and its terms are
# energies rather than fitted contributions:
#
#     fitness = -( S_hbond_ext + w S_vdw_ext + S_vdw_int + S_torsion_int )
#
# Four components: the protein-ligand hydrogen bond energy, the
# protein-ligand van der Waals energy, the ligand's internal van der
# Waals energy and its torsional strain. The fitness is the NEGATIVE of
# their sum, so that larger is better, and the external van der Waals
# term is multiplied by w = 1.375 -- an empirical correction, stated as
# such in the GOLD documentation, whose purpose is to encourage
# protein-ligand hydrophobic contact. It is a weight on one term only;
# leaving it at one is a different scoring function and the module lets
# you see that.
#
# The van der Waals terms are Lennard-Jones, and WHICH Lennard-Jones is
# the interesting choice. Writing a potential with minimum depth eps at
# separation r0 in general (m, n) form,
#
#     E(r) = eps [ (m/(n-m)) (r0/r)^n - (n/(n-m)) (r0/r)^m ]
#
# GOLD applies 6-12 internally and 4-8 externally. The 4-8 is
# deliberately softer: a 12 rises so steeply that a genetic algorithm
# cannot get a ligand past a slightly-too-close protein atom to find the
# pose on the other side of it, and a docking search needs to be able to
# squeeze.
#
# For binding sites with a loop known to move, GOLD parameterises two
# SPLIT potentials, written "4-8 2-4" and "4-8 1-2": the long-range half
# is the ordinary 4-8, and below the minimum the short-range half takes
# over. The guide's condition is that the two halves agree at the
# change-over and that the minimum stays put, which is exactly what the
# general form above gives when both halves are built from the same r0
# and eps -- both are -eps at r0. So the split potential is not a
# separate formula here, it is the same formula with a softer exponent
# pair inside the minimum, which is why it is continuous by construction
# rather than by a fudge.
#
# Everything that is DATA rather than method -- atom radii, well depths,
# hydrogen bond energies, torsion potentials -- comes in as a parameter.
# GOLD keeps those in its gold.params file and they are not reproduced
# here; the module takes them from the caller, and the combining rule
# for a pair (arithmetic on the radii, geometric on the depths) is
# stated rather than assumed.
#
# References
#   Jones, G., Willett, P., Glen, R.C., Leach, A.R. and Taylor, R.
#     (1997) "Development and validation of a genetic algorithm for
#     flexible docking." Journal of Molecular Biology 267(3), 727-748.
#     doi:10.1006/jmbi.1996.0897.
#   Jones, G., Willett, P. and Glen, R.C. (1995) "Molecular recognition
#     of receptor sites using a genetic algorithm with a description of
#     desolvation." Journal of Molecular Biology 245(1), 43-53.
#   Verdonk, M.L., Cole, J.C., Hartshorn, M.J., Murray, C.W. and Taylor,
#     R.D. (2003) "Improved protein-ligand docking using GOLD."
#     Proteins 52(4), 609-623. doi:10.1002/prot.10465.
#   Cambridge Crystallographic Data Centre, "GOLD User Guide," section
#     8.3 for the four components and the 1.375 external van der Waals
#     factor, and section 5.4 for the 6-12 internal, 4-8 external
#     defaults and the two split potentials.

# The exponent pairs GOLD parameterises. The first two are the plain
# potentials; the split ones keep the 4-8 outside the minimum and soften
# the inside.
.GOLDSC_POTENTIALS <- c("4-8", "6-12", "split_2-4", "split_1-2")

# The empirical factor on the external van der Waals term, GOLD User
# Guide 8.3.1. It exists to encourage hydrophobic contact.
.GOLDSC_VDW_WEIGHT <- 1.375

.goldsc_outer <- function(p)
  switch(p, "4-8" = c(4, 8), "6-12" = c(6, 12), "split_2-4" = c(4, 8),
         "split_1-2" = c(4, 8), NULL)
.goldsc_inner <- function(p)
  switch(p, "split_2-4" = c(2, 4), "split_1-2" = c(1, 2), NULL)

# x to a small non-negative integer power, by repeated multiplication.
# Not the language's power operator: R raises to an integer exponent by
# repeated squaring while Python calls the C library's pow, and the two
# disagree in the last bit. A potential that is about to be summed over
# thousands of contacts cannot afford to be a different function in the
# two arms.
.goldsc_ipow <- function(x, k) {
  p <- 1
  k <- as.integer(k)
  if (k > 0L) for (i in seq_len(k)) p <- p * x
  p
}

#' A general (m, n) Lennard-Jones energy
#'
#' Minimum of -eps at r0, zero at infinity, and singular at zero. The
#' parameterisation is by the MINIMUM rather than by the zero crossing
#' because that is what a docking parameter file stores: a contact
#' radius and a well depth.
#'
#' @param r The separation.
#' @param r0 The separation at the minimum.
#' @param eps The well depth.
#' @param m The attractive exponent.
#' @param n The repulsive exponent.
#' @return The energy.
#' @export
morie_goldsc_lj <- function(r, r0, eps, m = 6, n = 12) {
  r <- as.numeric(r)
  if (r <= 0) return(Inf)
  if (n <= m)
    stop("the repulsive exponent must exceed the attractive one")
  q <- r0 / r
  eps * ((as.numeric(m) / (n - m)) * .goldsc_ipow(q, n) -
           (as.numeric(n) / (n - m)) * .goldsc_ipow(q, m))
}

#' The soft split potential: 4-8 outside the minimum, softer inside
#'
#' Both halves are built from the same minimum position and depth, so
#' both equal -eps at the change-over and the minimum does not move --
#' the continuity the guide asks for falls out of the construction
#' instead of being imposed afterwards.
#'
#' @param r The separation.
#' @param r0 The separation at the minimum.
#' @param eps The well depth.
#' @param outer The exponent pair used at or beyond the minimum.
#' @param inner The exponent pair used inside it.
#' @return The energy.
#' @export
morie_goldsc_split <- function(r, r0, eps, outer = c(4, 8),
                               inner = c(2, 4)) {
  if (as.numeric(r) >= r0)
    return(morie_goldsc_lj(r, r0, eps, outer[1], outer[2]))
  morie_goldsc_lj(r, r0, eps, inner[1], inner[2])
}

.goldsc_pair <- function(r, r0, eps, potential) {
  if (!(potential %in% .GOLDSC_POTENTIALS))
    stop("potential must be one of ",
         paste(.GOLDSC_POTENTIALS, collapse = ", "))
  inner <- .goldsc_inner(potential)
  if (!is.null(inner))
    return(morie_goldsc_split(r, r0, eps, .goldsc_outer(potential), inner))
  mn <- .goldsc_outer(potential)
  morie_goldsc_lj(r, r0, eps, mn[1], mn[2])
}

.goldsc_lookup <- function(table, key, what) {
  for (kv in table) if (kv[[1]] == key) return(as.numeric(kv[[2]]))
  stop("no ", what, " for atom type ", key)
}

#' Sum a Lennard-Jones potential over a list of contacts
#'
#' The combining rule is arithmetic on the radii and geometric on the
#' well depths, which is the ordinary Lorentz-Berthelot convention and is
#' stated here rather than hidden. A cutoff drops contacts beyond it
#' entirely rather than tapering them; that is a discontinuity, and it is
#' the caller's decision, so it is off by default.
#'
#' @param pairs A list of list(distance, type_i, type_j).
#' @param radii A list of list(type, radius).
#' @param depths A list of list(type, well depth).
#' @param potential A member of the potential list.
#' @param cutoff Drop contacts beyond this separation, or NULL.
#' @return A list with the total, the per-contact energies and the count.
#' @export
morie_goldsc_vdw <- function(pairs, radii, depths, potential = "4-8",
                             cutoff = NULL) {
  terms <- numeric(0)
  kept <- 0L
  for (pr in pairs) {
    r <- as.numeric(pr[[1]])
    if (!is.null(cutoff) && r > as.numeric(cutoff)) next
    r0 <- .goldsc_lookup(radii, pr[[2]], "radius") +
      .goldsc_lookup(radii, pr[[3]], "radius")
    eps <- sqrt(.goldsc_lookup(depths, pr[[2]], "well depth") *
                  .goldsc_lookup(depths, pr[[3]], "well depth"))
    terms <- c(terms, .goldsc_pair(r, r0, eps, potential))
    kept <- kept + 1L
  }
  list(total = if (length(terms)) .w3_csum(terms) else 0,
       terms = terms, n = kept)
}

#' Sum the tabulated hydrogen bond energies of the close pairs
#'
#' GOLD counts a bond towards the fitness only when the donor-hydrogen to
#' acceptor fitting-point distance is below the threshold, and anneals
#' that threshold down over a run so that poor bonds are tolerated early
#' and not at the end. The threshold is therefore a parameter of the
#' CALL, not a constant: a fitness computed at the starting threshold is
#' not the same number as one computed at the finishing threshold, and
#' the guide is explicit that only the final one means anything.
#'
#' @param bonds A list of list(distance, energy).
#' @param max_distance The distance threshold.
#' @return A list with the total, the counted energies and the count.
#' @export
morie_goldsc_hbond <- function(bonds, max_distance = 2.5) {
  terms <- numeric(0)
  for (b in bonds)
    if (as.numeric(b[[1]]) < as.numeric(max_distance))
      terms <- c(terms, as.numeric(b[[2]]))
  list(total = if (length(terms)) .w3_csum(terms) else 0,
       terms = terms, n = length(terms))
}

#' The ligand's internal torsional strain
#'
#' The cosine form a docking parameter file stores, one term per
#' rotatable bond, with the angle in degrees on the way in.
#'
#' @param torsions A list of c(phi, A, n, phi0).
#' @return A list with the total and the per-bond terms.
#' @export
morie_goldsc_torsion <- function(torsions) {
  terms <- vapply(torsions, function(t)
    as.numeric(t[2]) * (1 + cos(as.numeric(t[3]) * as.numeric(t[1]) *
                                  pi / 180 - as.numeric(t[4]))),
    numeric(1))
  list(total = if (length(terms)) .w3_csum(terms) else 0, terms = terms)
}

.goldsc_dist <- function(a, b) sqrt(.w3_csum((a - b) * (a - b)))

#' The GoldScore fitness of a pose
#'
#' @param receptor Atom rows: x, y, z, type. Every cross pair enters the
#'   external van der Waals sum.
#' @param ligand The ligand atom rows, likewise.
#' @param radii A list of list(type, contact radius).
#' @param depths A list of list(type, well depth).
#' @param hbonds A list of list(distance, energy) per candidate bond.
#' @param internal A list of list(distance, type_i, type_j) for the
#'   ligand's own non-bonded pairs.
#' @param torsions A list of c(phi, A, n, phi0) per rotatable bond.
#' @param potential A member of the potential list, for the external
#'   term.
#' @param internal_potential Likewise for the internal term.
#' @param vdw_weight The factor on the external van der Waals term.
#' @param max_distance The hydrogen-bond distance threshold.
#' @param cutoff Drop external contacts beyond this separation, or NULL.
#' @return A list with the fitness, each component, and the per-contact
#'   energies.
#' @export
morie_goldsc <- function(receptor, ligand, radii = list(),
                         depths = list(), hbonds = list(),
                         internal = list(), torsions = list(),
                         potential = "4-8",
                         internal_potential = "6-12",
                         vdw_weight = .GOLDSC_VDW_WEIGHT,
                         max_distance = 2.5, cutoff = NULL) {
  rec <- lapply(receptor, function(a)
    list(xyz = c(as.numeric(a[[1]]), as.numeric(a[[2]]),
                 as.numeric(a[[3]])), type = as.character(a[[4]])))
  lig <- lapply(ligand, function(a)
    list(xyz = c(as.numeric(a[[1]]), as.numeric(a[[2]]),
                 as.numeric(a[[3]])), type = as.character(a[[4]])))
  pairs <- list()
  for (ra in rec) for (la in lig)
    pairs[[length(pairs) + 1L]] <-
      list(.goldsc_dist(ra$xyz, la$xyz), ra$type, la$type)
  ext <- morie_goldsc_vdw(pairs, radii, depths, potential, cutoff)
  int <- morie_goldsc_vdw(internal, radii, depths, internal_potential,
                          cutoff)
  hb <- morie_goldsc_hbond(hbonds, max_distance)
  to <- morie_goldsc_torsion(torsions)

  w <- as.numeric(vdw_weight)
  total <- hb$total + w * ext$total + int$total + to$total
  list(fitness = -total, energy = total, hbond = hb$total,
       vdw_external = ext$total, vdw_external_weighted = w * ext$total,
       vdw_internal = int$total, torsion = to$total,
       internal = int$total + to$total, external_terms = ext$terms,
       internal_terms = int$terms, hbond_terms = hb$terms,
       torsion_terms = to$terms, n_external = ext$n, n_internal = int$n,
       n_hbond = hb$n, n_receptor = length(rec), n_ligand = length(lig),
       estimate = -total, se = NaN, vdw_weight = w,
       max_distance = as.numeric(max_distance), potential = potential,
       internal_potential = internal_potential,
       method = "GoldScore genetic-algorithm docking fitness")
}

#' One-line summary of the goldsc module
#'
#' @return A character scalar.
#' @export
morie_goldsc_cheatsheet <- function()
  paste0("goldsc: GoldScore docking fitness. potentials ",
         paste(.GOLDSC_POTENTIALS, collapse = ", "),
         "; external van der Waals weighted 1.375 (CCDC GOLD)")
