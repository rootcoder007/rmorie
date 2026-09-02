# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of pmfsc -- knowledge-based potentials of mean force for
# protein-ligand pairs. Mirrors src/morie/fn/pmfsc.py operation for
# operation, on the shared numerics in R/aaa_helpers_w3num.R.
#
# An empirical scoring function like ChemScore fits a handful of
# coefficients to measured affinities. A knowledge-based one asks a
# different question: forget affinities, just look at where atoms
# ACTUALLY sit in the several thousand protein-ligand complexes in the
# Protein Data Bank, and read the energy off the statistics. If a
# particular kind of protein atom is found next to a particular kind of
# ligand atom more often at 3.5 angstroms than chance would put it
# there, that separation is favourable, and how much more often says how
# favourable.
#
# Formally, inverting the Boltzmann distribution gives a Helmholtz free
# interaction energy for each atom-type pair as a function of
# separation:
#
#     A_ij(r) = -k_B T ln[ f_j(r) rho_ij(r) / rho_ij(bulk) ]
#
# rho_ij(r) is the observed number density of the pair at separation r,
# rho_ij(bulk) is its density in the reference state, and f_j(r) is the
# ligand volume correction, which exists because the ligand's own atoms
# occupy part of the shell and the raw density understates how crowded
# the available space really is. The score of a pose is the sum of A_ij
# over every pair inside a cutoff.
#
# The important thing about this module is what it does NOT ship. The
# PMF potential is a TABLE -- one curve per atom-type pair, derived from
# a structure database -- and a table copied out of a paper is data, not
# method. So the table is an OUTPUT here: give the module a set of
# observed contacts from complexes you have, and it derives the
# potential; give it the derived potential and a pose, and it scores.
# Nothing is hard-coded that a reader could not reproduce from their own
# structures.
#
# Two decisions are visible rather than buried. First, the reference
# state: "bulk" uses the observed overall density of that pair inside
# the cutoff, which is what makes A vanish where the pair is exactly as
# common as it is on average; "uniform" uses the density a completely
# structureless distribution would give. They differ, and which one you
# picked changes every number. Second, an UNOBSERVED bin. A pair never
# seen at some separation has zero density and a logarithm of minus
# infinity, and treating that as infinite repulsion is a statement the
# data does not support -- it may just be a rare pair. Those bins are
# capped at a stated value and COUNTED, so a score resting on twenty
# capped bins cannot look like a score resting on none.
#
# References
#   Muegge, I. and Martin, Y.C. (1999) "A general and fast scoring
#     function for protein-ligand interactions: a simplified potential
#     approach." Journal of Medicinal Chemistry 42(5), 791-804.
#     doi:10.1021/jm980536j.
#   Muegge, I. (2001) "Effect of ligand volume correction on PMF
#     scoring." Journal of Computational Chemistry 22(4), 418-425.
#   Muegge, I. (2006) "PMF scoring revisited." Journal of Medicinal
#     Chemistry 49(20), 5895-5902.
#   Sippl, M.J. (1990) "Calculation of conformational ensembles from
#     potentials of mean force." Journal of Molecular Biology 213(4),
#     859-883.
#
# A note on what is sourced. The functional form and the role of each
# factor above are from the papers. The particular realisation of the
# volume correction as an excluded-volume ratio, and the capping rule
# for unobserved bins, are this module's -- they are described in the
# sources in words rather than given as formulas, so they are named here
# as choices rather than attributed.

.PMFSC_REFERENCES <- c("bulk", "uniform")
.PMFSC_CORRECTIONS <- c("none", "excluded_volume")

# The separation beyond which pairs are not counted. Muegge and Martin
# use pair-specific cutoffs; those are data this module does not ship,
# so this is a single default the caller is expected to override with
# their own table.
.PMFSC_DEFAULT_CUTOFF <- 12.0

#' The volume of the spherical shell between two radii
#'
#' Written as the difference of two cubes rather than as a thin-shell
#' approximation, because the bins here are wide enough that
#' 4 pi r^2 dr is visibly wrong at the inner ones -- and the inner bins
#' are exactly where the interesting structure lives.
#'
#' @param r1 The inner radius.
#' @param r2 The outer radius.
#' @return The shell volume.
#' @export
morie_pmfsc_shell <- function(r1, r2) {
  if (r2 < r1) stop("the outer radius must not be inside the inner")
  (4 / 3) * pi * (r2 * r2 * r2 - r1 * r1 * r1)
}

#' Which bin a separation falls in, or -1 if it is past the cutoff
#'
#' A distance exactly on an edge goes to the UPPER bin, so the bins are
#' half-open on the left and the assignment cannot depend on which side
#' of an edge the arithmetic happens to land.
#'
#' @param r The separation.
#' @param r_max The cutoff.
#' @param n_bins The number of bins.
#' @return The zero-based bin index, or -1 past the cutoff.
#' @export
morie_pmfsc_bin <- function(r, r_max, n_bins) {
  r <- as.numeric(r)
  if (r < 0) stop("a separation cannot be negative")
  if (r >= r_max) return(-1L)
  k <- as.integer(floor(r * n_bins / r_max))
  if (k < 0L) k <- 0L
  if (k >= n_bins) k <- as.integer(n_bins) - 1L
  k
}

#' .pmfsc_key
#'
#' A step of the pmfsc_native implementation. Called by \code{morie_pmfsc_derive}, \code{morie_pmfsc_score}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Passed to \code{paste0}.
#' @param b Passed to \code{paste0}.
#' @return A character value.
#' @export
.pmfsc_key <- function(a, b) paste0(a, "|", b)

#' Turn observed contacts into a potential, one curve per type pair
#'
#' @param observations A list of list(type_i, type_j, separation) for
#'   every contact seen in the training complexes. Order within a pair
#'   matters only in that the two type labels are kept as given.
#' @param n_complexes How many complexes the observations came from. It
#'   scales every density identically and therefore cancels out of a
#'   bulk-referenced potential.
#' @param r_max The radial cutoff.
#' @param n_bins The number of radial bins.
#' @param reference A member of the reference list.
#' @param correction A member of the correction list.
#' @param occupied Per-bin ligand-occupied volume, or NULL.
#' @param kT The energy scale; one leaves the potential in units of kT.
#' @param cap The magnitude, in units of kT, at which an unobserved bin
#'   is held. Unobserved is not the same as forbidden.
#' @return A named list keyed by "type_i|type_j" with the counts, the
#'   density, the reference density, the potential and the count of
#'   capped bins.
#' @export
morie_pmfsc_derive <- function(observations, n_complexes = 1,
                               r_max = .PMFSC_DEFAULT_CUTOFF,
                               n_bins = 24, reference = "bulk",
                               correction = "none", occupied = NULL,
                               kT = 1, cap = 6) {
  if (!(reference %in% .PMFSC_REFERENCES))
    stop("reference must be one of ",
         paste(.PMFSC_REFERENCES, collapse = ", "))
  if (!(correction %in% .PMFSC_CORRECTIONS))
    stop("correction must be one of ",
         paste(.PMFSC_CORRECTIONS, collapse = ", "))
  n_bins <- as.integer(n_bins)
  if (n_bins < 1L) stop("need at least one radial bin")
  if (r_max <= 0) stop("the cutoff must be positive")
  nc <- as.numeric(n_complexes)
  if (nc <= 0) stop("need at least one complex")

  edges <- vapply(0:n_bins, function(t) r_max * t / n_bins, numeric(1))
  vol <- vapply(seq_len(n_bins), function(t)
    morie_pmfsc_shell(edges[t], edges[t + 1L]), numeric(1))
  fcorr <- rep(1, n_bins)
  if (correction == "excluded_volume") {
    if (is.null(occupied))
      stop("the excluded-volume correction needs the occupied volumes")
    for (t in seq_len(n_bins)) {
      free <- vol[t] - as.numeric(occupied[t])
      if (free <= 0)
        stop("bin ", t - 1L, " is entirely occupied by ligand atoms; ",
             "the correction is undefined there")
      # The available space is smaller than the shell, so the true
      # density is HIGHER than the raw count suggests, by exactly this
      # ratio.
      fcorr[t] <- vol[t] / free
    }
  }

  counts <- list()
  for (ob in observations) {
    k <- morie_pmfsc_bin(ob[[3]], r_max, n_bins)
    if (k < 0L) next
    key <- .pmfsc_key(as.character(ob[[1]]), as.character(ob[[2]]))
    if (is.null(counts[[key]])) counts[[key]] <- integer(n_bins)
    counts[[key]][k + 1L] <- counts[[key]][k + 1L] + 1L
  }

  out <- list()
  total_vol <- morie_pmfsc_shell(0, r_max)
  for (key in sort(names(counts), method = "radix")) {
    cc <- counts[[key]]
    dens <- vapply(seq_len(n_bins), function(t) cc[t] / (nc * vol[t]),
                   numeric(1))
    n_tot <- sum(cc)
    ref <- if (reference == "bulk") n_tot / (nc * total_vol)
           else 1 / total_vol
    a <- numeric(n_bins)
    capped <- 0L
    for (t in seq_len(n_bins)) {
      x <- fcorr[t] * dens[t]
      if (ref <= 0 || x <= 0) {
        # Unobserved, not forbidden. Hold it at the cap and say so
        # rather than letting a logarithm of zero decide.
        a[t] <- as.numeric(kT) * cap
        capped <- capped + 1L
      } else {
        a[t] <- -as.numeric(kT) * log(x / ref)
      }
    }
    out[[key]] <- list(counts = cc, density = dens, reference = ref,
                       potential = a, capped = capped, n = n_tot,
                       edges = edges, volume = vol, correction = fcorr)
  }
  out
}

#' Score a pose against a derived potential
#'
#' A pair whose type combination is absent from the potential
#' contributes the missing value and is counted separately -- silently
#' scoring it as zero would let a pose made entirely of unparameterised
#' atoms come out looking average.
#'
#' @param pairs A list of list(type_i, type_j, separation).
#' @param potential A potential from the derive function.
#' @param r_max The radial cutoff.
#' @param n_bins The number of radial bins.
#' @param missing The contribution of an unparameterised pair.
#' @return A list with the score and the three counts.
#' @export
morie_pmfsc_score <- function(pairs, potential,
                              r_max = .PMFSC_DEFAULT_CUTOFF,
                              n_bins = 24, missing = 0) {
  terms <- numeric(0)
  used <- 0L
  beyond <- 0L
  unknown <- 0L
  for (pr in pairs) {
    k <- morie_pmfsc_bin(pr[[3]], r_max, n_bins)
    if (k < 0L) { beyond <- beyond + 1L
    next }
    key <- .pmfsc_key(as.character(pr[[1]]), as.character(pr[[2]]))
    if (is.null(potential[[key]])) {
      unknown <- unknown + 1L
      terms <- c(terms, as.numeric(missing))
      next
    }
    terms <- c(terms, potential[[key]]$potential[k + 1L])
    used <- used + 1L
  }
  list(score = if (length(terms)) .w3_csum(terms) else 0, used = used,
       beyond = beyond, unknown = unknown)
}

#' .pmfsc_dist
#'
#' A step of the pmfsc_native implementation. Called by \code{morie_pmfsc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.pmfsc_dist <- function(a, b) sqrt(.w3_csum((a - b) * (a - b)))

#' Derive a potential if needed, then score the pose
#'
#' @param receptor Atom rows: x, y, z, type.
#' @param ligand The ligand atom rows, likewise.
#' @param potential A derived potential, or NULL to derive one.
#' @param observations Training contacts, or NULL.
#' @param n_complexes How many complexes the observations came from.
#' @param r_max The radial cutoff.
#' @param n_bins The number of radial bins.
#' @param reference A member of the reference list.
#' @param correction A member of the correction list.
#' @param occupied Per-bin ligand-occupied volume, or NULL.
#' @param kT The energy scale.
#' @param cap The unobserved-bin cap, in units of kT.
#' @param missing The contribution of an unparameterised pair.
#' @return A list with the score, the potential used, and the counts
#'   that say how much of the pose the potential actually covered.
#' @export
morie_pmfsc <- function(receptor, ligand, potential = NULL,
                        observations = NULL, n_complexes = 1,
                        r_max = .PMFSC_DEFAULT_CUTOFF, n_bins = 24,
                        reference = "bulk", correction = "none",
                        occupied = NULL, kT = 1, cap = 6, missing = 0) {
  if (is.null(potential)) {
    if (is.null(observations))
      stop("give either a derived potential or the observations to ",
           "derive one from")
    potential <- morie_pmfsc_derive(observations, n_complexes, r_max,
                                    n_bins, reference, correction,
                                    occupied, kT, cap)
  }
  rec <- lapply(receptor, function(a)
    list(xyz = c(as.numeric(a[[1]]), as.numeric(a[[2]]),
                 as.numeric(a[[3]])), type = as.character(a[[4]])))
  lig <- lapply(ligand, function(a)
    list(xyz = c(as.numeric(a[[1]]), as.numeric(a[[2]]),
                 as.numeric(a[[3]])), type = as.character(a[[4]])))
  pairs <- list()
  for (ra in rec) for (la in lig)
    pairs[[length(pairs) + 1L]] <-
      list(ra$type, la$type, .pmfsc_dist(ra$xyz, la$xyz))
  sc <- morie_pmfsc_score(pairs, potential, r_max, n_bins, missing)
  capped <- sum(vapply(potential, function(p) p$capped, integer(1)))
  list(score = sc$score, estimate = sc$score, se = NaN,
       n_scored = sc$used, n_beyond_cutoff = sc$beyond,
       n_unparameterised = sc$unknown, n_pairs = length(pairs),
       n_types = length(potential), n_capped_bins = capped,
       potential = potential, r_max = as.numeric(r_max),
       n_bins = as.integer(n_bins), kT = as.numeric(kT),
       cap = as.numeric(cap), reference = reference,
       correction = correction, method = "knowledge-based PMF scoring")
}

#' One-line summary of the pmfsc module
#'
#' @return A character scalar.
#' @export
morie_pmfsc_cheatsheet <- function()
  paste0("pmfsc: knowledge-based PMF scoring. references ",
         paste(.PMFSC_REFERENCES, collapse = ", "), "; corrections ",
         paste(.PMFSC_CORRECTIONS, collapse = ", "),
         "; the potential is derived, not shipped")
