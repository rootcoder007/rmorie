# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of scfhop -- scaffold hopping. Mirrors src/morie/fn/scfhop.py
# operation for operation, on the SMILES parser and graph helpers in
# R/avalon_native.R and the shared numerics in R/aaa_helpers_w3num.R.
#
# The medicinal-chemistry problem is that a lead compound is two things
# at once. It is a PHARMACOPHORE -- an arrangement of donors, acceptors,
# charges and greasy patches, which is what the protein recognises --
# and it is a SCAFFOLD, the ring-and-linker skeleton that holds them
# there, which is what the patent covers and what the metabolism
# attacks. A scaffold hop keeps the first and replaces the second.
# Schneider and colleagues' contribution was to make that searchable:
# describe a molecule by its pharmacophore alone, in a form that carries
# no memory of the skeleton, then look for molecules that match the
# description and do not match the skeleton.
#
#   THE DESCRIPTOR. CATS -- a topological pharmacophore correlation
#   vector. Each atom is assigned pharmacophore types; for every pair of
#   types and every topological distance up to a limit, count the atom
#   pairs of those types that many bonds apart. It says "there is a
#   donor five bonds from an acceptor" without saying what is in
#   between.
#
#   THE SCAFFOLD. Bemis and Murcko's framework: strip side chains by
#   repeatedly deleting terminal atoms that are not in a ring. A
#   molecule with no rings has no framework, and that is reported as an
#   empty scaffold rather than as the whole molecule.
#
#   THE HOP. A candidate is a hop when its descriptor is close and its
#   scaffold is different. Both halves are necessary and are reported
#   separately, because a candidate that is merely similar is an
#   analogue and one that is merely different is a different molecule.
#
# WHAT IS THIS MODULE'S OWN. The five CATS types are Schneider's; the
# rules assigning an atom to them are stated in the original work as
# SMARTS patterns, and the ones here are this module's operationalisation
# of the same five categories, written out in morie_scfhop_types so a
# reader can disagree with a specific rule rather than with a black box.
#
# Scaffold identity is decided by a Weisfeiler-Leman colour refinement
# over the framework subgraph. That is a graph INVARIANT, not a
# canonical form: isomorphic scaffolds always agree, different ones
# almost always disagree but are not guaranteed to. Everything below
# says "different by this invariant", never "different".
#
# References
#   Schneider, G., Neidhart, W., Giller, T. and Schmid, G. (1999)
#     "'Scaffold-hopping' by topological pharmacophore search."
#     Angewandte Chemie International Edition 38(19), 2894-2896.
#   Bemis, G.W. and Murcko, M.A. (1996) "The properties of known drugs.
#     1. Molecular frameworks." Journal of Medicinal Chemistry 39(15),
#     2887-2893. doi:10.1021/jm9602928.
#   Weisfeiler, B. and Leman, A.A. (1968) "The reduction of a graph to
#     canonical form and the algebra which appears therein."
#     Nauchno-Technicheskaya Informatsia 2(9), 12-16.
#   Shervashidze, N. et al. (2011) "Weisfeiler-Lehman graph kernels."
#     Journal of Machine Learning Research 12, 2539-2561.

# Schneider's five categories, in a fixed order so the vector's layout
# is a property of the module and not of a hash table's iteration.
.scfhop_types <- c("A", "D", "L", "N", "P")
.scfhop_scalings <- c("type", "count", "none")

#' Assign each atom its pharmacophore categories
#'
#' An atom may have several, or none. The rules, written out so they can
#' be argued with. D donor: nitrogen or oxygen carrying at least one
#' hydrogen. A acceptor: nitrogen or oxygen -- an atom can be both,
#' which is correct, since a hydroxyl is a donor and an acceptor. P
#' positive: an aliphatic nitrogen with no neighbouring carbonyl carbon,
#' an amine rather than an amide, which is the distinction that decides
#' whether it is protonated at physiological pH. N negative: the oxygens
#' of a carboxyl group, a carbon bearing both a doubly bonded oxygen and
#' a singly bonded one. L lipophilic: carbon with no nitrogen or oxygen
#' neighbour, and sulfur and the halogens.
#'
#' @param smiles The molecule.
#' @return A list of character vectors, one per atom.
#' @export
morie_scfhop_types <- function(smiles) {
  g <- morie_avalon_parse(smiles)
  el <- g$el
  arom <- g$arom
  n <- length(el)
  adj <- .avalon_adj(n, g$bonds)
  nh <- morie_avalon_h(el, arom, g$chg, g$hexp, g$bonds)
  # A carbon is a carbonyl carbon when it holds a doubly bonded oxygen,
  # and a carboxyl carbon when it also holds a single-bonded one. Both
  # are read off the bond orders, not guessed from the element.
  dbl_o <- integer(n)
  sng_o <- integer(n)
  for (i in seq_len(n)) for (e in adj[[i]]) {
    if (el[e[1] + 1L] == "O") {
      if (e[2] == 2) dbl_o[i] <- dbl_o[i] + 1L
      else if (e[2] == 1) sng_o[i] <- sng_o[i] + 1L
    }
  }
  out <- vector("list", n)
  for (i in seq_len(n)) {
    t <- character(0)
    e0 <- el[i]
    if (e0 %in% c("N", "O")) {
      t <- c(t, "A")
      if (nh[i] > 0L) t <- c(t, "D")
    }
    if (e0 == "N" && arom[i] != 1L) {
      amide <- FALSE
      for (e in adj[[i]])
        if (el[e[1] + 1L] == "C" && dbl_o[e[1] + 1L] > 0L) amide <- TRUE
      if (!amide) t <- c(t, "P")
    }
    if (e0 == "O") {
      for (e in adj[[i]])
        if (el[e[1] + 1L] == "C" && dbl_o[e[1] + 1L] > 0L &&
            sng_o[e[1] + 1L] > 0L) t <- c(t, "N")
    }
    if (e0 == "C") {
      het <- FALSE
      for (e in adj[[i]]) if (el[e[1] + 1L] %in% c("N", "O")) het <- TRUE
      if (!het) t <- c(t, "L")
    } else if (e0 %in% c("S", "F", "Cl", "Br", "I")) {
      t <- c(t, "L")
    }
    out[[i]] <- sort(unique(t), method = "radix")
  }
  out
}

#' .scfhop_pairs
#'
#' A step of the scfhop_native implementation. Called by \code{morie_scfhop_cats}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return The value of \code{out}, as built in the body.
#' @export
#' @examples
#' res <- .scfhop_pairs()
#' res
.scfhop_pairs <- function() {
  out <- list()
  for (a in seq_along(.scfhop_types))
    for (b in a:length(.scfhop_types))
      out[[length(out) + 1L]] <- c(.scfhop_types[a], .scfhop_types[b])
  out
}

#' The CATS correlation vector: type pairs by topological distance
#'
#' Laid out pair-major, distance-minor, so entry
#' \code{p * (maxdist + 1) + d} is the count of pairs of the p-th type
#' combination exactly d bonds apart. Distance zero is an atom with
#' itself, which is how an atom carrying two types registers at all.
#'
#' Scaling routes, all three defensible and all giving different
#' answers, so the choice is the caller's. \code{type} divides each
#' entry by the number of atoms carrying the two types, which is
#' Schneider's scaling and stops a large molecule dominating simply by
#' being large. \code{count} divides by the total pairs counted.
#' \code{none} leaves raw counts, which is what you want if you intend
#' to compare absolute frequencies.
#'
#' @param smiles The molecule.
#' @param maxdist The distance limit, in bonds.
#' @param scaling One of type, count or none.
#' @return A numeric vector.
#' @export
morie_scfhop_cats <- function(smiles, maxdist = 9L, scaling = "type") {
  if (!(scaling %in% .scfhop_scalings))
    stop("the scaling is type, count or none")
  maxdist <- as.integer(maxdist)
  if (maxdist < 0L) stop("a distance limit below zero counts nothing")
  ty <- morie_scfhop_types(smiles)
  g <- morie_avalon_parse(smiles)
  n <- length(g$el)
  D <- .avalon_dist(.avalon_adj(n, g$bonds), n)
  P <- .scfhop_pairs()
  pkey <- vapply(P, function(p) paste0(p[1], p[2]), character(1))
  v <- numeric(length(P) * (maxdist + 1L))
  have <- rep(0L, length(.scfhop_types))
  names(have) <- .scfhop_types
  for (i in seq_len(n)) for (t in ty[[i]]) have[t] <- have[t] + 1L
  for (i in seq_len(n)) for (j in i:n) {
    d <- D[i, j]
    if (d < 0L || d > maxdist) next
    for (a in ty[[i]]) for (b in ty[[j]]) {
      k <- if (.avalon_lte(a, b)) paste0(a, b) else paste0(b, a)
      p <- match(k, pkey)
      v[(p - 1L) * (maxdist + 1L) + d + 1L] <-
        v[(p - 1L) * (maxdist + 1L) + d + 1L] + 1
    }
  }
  if (scaling == "type") {
    for (p in seq_along(P)) {
      s <- have[[P[[p]][1]]] + have[[P[[p]][2]]]
      rng <- (p - 1L) * (maxdist + 1L) + seq_len(maxdist + 1L)
      v[rng] <- if (s > 0L) v[rng] / s else 0
    }
  } else if (scaling == "count") {
    tot <- .w3_csum(v)
    if (tot > 0) v <- v / tot
  }
  v
}

#' How close two CATS vectors are
#'
#' \code{tanimoto} is the continuous form, sum of minima over sum of
#' maxima: one exactly when the vectors are equal, zero when they share
#' no dimension. \code{euclidean} is reported as one over one plus the
#' distance so every route points the same way -- larger is closer --
#' and \code{cosine} ignores magnitude entirely.
#'
#' @param a,b Two descriptors of the same length.
#' @param metric One of tanimoto, euclidean or cosine.
#' @return A number, larger meaning closer.
#' @export
morie_scfhop_similarity <- function(a, b, metric = "tanimoto") {
  if (length(a) != length(b))
    stop("two descriptors of different lengths cannot be compared")
  if (metric == "tanimoto") {
    lo <- .w3_csum(pmin(a, b))
    hi <- .w3_csum(pmax(a, b))
    return(if (hi > 0) lo / hi else 0)
  }
  if (metric == "euclidean")
    return(1 / (1 + sqrt(.w3_csum((a - b) * (a - b)))))
  if (metric == "cosine") {
    num <- .w3_csum(a * b)
    na <- sqrt(.w3_csum(a * a))
    nb <- sqrt(.w3_csum(b * b))
    return(if (na > 0 && nb > 0) num / (na * nb) else 0)
  }
  stop("the metric is tanimoto, euclidean or cosine")
}

#' The Bemis-Murcko framework: ring systems and the linkers between
#'
#' Strip side chains by repeatedly deleting any atom that is not in a
#' ring and has at most one remaining neighbour. What survives is the
#' rings plus every atom on a path between two of them. A molecule with
#' no rings loses everything, and the empty scaffold is returned rather
#' than the whole molecule -- an acyclic lead has no skeleton to hop
#' away from and saying so is the useful answer.
#'
#' @param smiles The molecule.
#' @return A list with the surviving zero-based atom indices and the
#'   surviving bonds.
#' @export
morie_scfhop_murcko <- function(smiles) {
  g <- morie_avalon_parse(smiles)
  n <- length(g$el)
  rr <- morie_avalon_rings(n, g$bonds, g$closures)
  inring <- rr$inring
  keep <- rep(TRUE, n)
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    deg <- integer(n)
    for (b in g$bonds) if (keep[b[1] + 1L] && keep[b[2] + 1L]) {
      deg[b[1] + 1L] <- deg[b[1] + 1L] + 1L
      deg[b[2] + 1L] <- deg[b[2] + 1L] + 1L
    }
    for (i in seq_len(n))
      if (keep[i] && inring[i] != 1L && deg[i] <= 1L) {
        keep[i] <- FALSE
        changed <- TRUE
      }
  }
  atoms <- which(keep) - 1L
  kb <- list()
  for (b in g$bonds) if (keep[b[1] + 1L] && keep[b[2] + 1L])
    kb[[length(kb) + 1L]] <- b
  list(atoms = atoms, bonds = kb)
}

#' A Weisfeiler-Leman colour of the framework, as a fingerprint
#'
#' Each surviving atom starts coloured by its element, aromaticity and
#' charge; a round replaces the colour by a hash of it together with the
#' sorted multiset of its neighbours' colours and the bond orders
#' reaching them. The signature is the sorted multiset of final colours.
#'
#' An INVARIANT, not a canonical form: isomorphic scaffolds always
#' agree; different ones agree only in the rare cases the refinement
#' cannot separate.
#'
#' @param smiles The molecule.
#' @param rounds The refinement depth.
#' @return A sorted numeric vector, empty for an acyclic molecule.
#' @export
morie_scfhop_signature <- function(smiles, rounds = 3L) {
  g <- morie_avalon_parse(smiles)
  mk <- morie_scfhop_murcko(smiles)
  atoms <- mk$atoms
  if (!length(atoms)) return(numeric(0))
  m <- length(atoms)
  pos <- rep(NA_integer_, length(g$el))
  for (k in seq_len(m)) pos[atoms[k] + 1L] <- k
  nb <- vector("list", m)
  for (k in seq_len(m)) nb[[k]] <- list()
  for (b in mk$bonds) {
    p1 <- pos[b[1] + 1L]
    p2 <- pos[b[2] + 1L]
    nb[[p1]][[length(nb[[p1]]) + 1L]] <- c(p2, b[3])
    nb[[p2]][[length(nb[[p2]]) + 1L]] <- c(p1, b[3])
  }
  col <- vapply(seq_len(m), function(k)
    morie_avalon_fnv(sprintf("%s|%d|%d", g$el[atoms[k] + 1L],
                             g$arom[atoms[k] + 1L],
                             g$chg[atoms[k] + 1L])), numeric(1))
  for (it in seq_len(as.integer(rounds))) {
    nxt <- numeric(m)
    for (k in seq_len(m)) {
      around <- if (!length(nb[[k]])) character(0) else
        sort(vapply(nb[[k]], function(e)
          sprintf("%d:%.0f", as.integer(e[2]), col[e[1]]),
          character(1)), method = "radix")
      nxt[k] <- morie_avalon_fnv(
        sprintf("%.0f|%s", col[k], paste(around, collapse = ",")))
    }
    col <- nxt
  }
  sort(col)
}

#' Rank candidates by pharmacophore, and say which ones are hops
#'
#' @param lead_smiles The lead compound.
#' @param scaffold_db Candidate molecules, as SMILES.
#' @param maxdist The descriptor's distance limit.
#' @param scaling The descriptor's scaling.
#' @param metric The comparison.
#' @param rounds Weisfeiler-Leman refinement depth.
#' @param threshold Candidates below this similarity are ranked but not
#'   called hops. Zero calls every different-scaffold candidate a hop,
#'   which is the honest default: the cut-off is a decision about the
#'   screening campaign, not a property of the method.
#' @return A list with the ranked candidates, each with its similarity,
#'   whether its scaffold differs from the lead's, and whether it is a
#'   hop.
#' @export
morie_scfhop <- function(lead_smiles, scaffold_db, maxdist = 9L,
                         scaling = "type", metric = "tanimoto",
                         rounds = 3L, threshold = 0) {
  lead <- morie_scfhop_cats(lead_smiles, maxdist, scaling)
  lsig <- morie_scfhop_signature(lead_smiles, rounds)
  lmk <- morie_scfhop_murcko(lead_smiles)
  nq <- length(scaffold_db)
  sim <- numeric(nq)
  diff <- logical(nq)
  ssz <- integer(nq)
  hop <- logical(nq)
  for (q in seq_len(nq)) {
    sm <- scaffold_db[q]
    v <- morie_scfhop_cats(sm, maxdist, scaling)
    sim[q] <- morie_scfhop_similarity(lead, v, metric)
    sg <- morie_scfhop_signature(sm, rounds)
    mk <- morie_scfhop_murcko(sm)
    diff[q] <- !(length(sg) == length(lsig) && all(sg == lsig))
    ssz[q] <- length(mk$atoms)
    hop[q] <- diff[q] && sim[q] >= threshold
  }
  # Ranked by similarity, ties broken by the order they were given in,
  # so the ranking is a function of the input and not of a sort's
  # internal state.
  ord <- order(-sim, seq_len(nq))
  ranked <- lapply(ord, function(i)
    list(index = i - 1L, smiles = scaffold_db[i], similarity = sim[i],
         scaffold_differs = diff[i], scaffold_size = ssz[i],
         is_hop = hop[i]))
  list(lead = lead, lead_scaffold = lmk$atoms,
       lead_scaffold_size = length(lmk$atoms), lead_signature = lsig,
       ranked = ranked, similarity = sim[ord], is_hop = hop[ord],
       order = ord - 1L, n_candidates = nq, n_hops = sum(hop),
       n_dim = length(lead), maxdist = as.integer(maxdist),
       scaling = scaling, metric = metric, rounds = as.integer(rounds),
       threshold = as.numeric(threshold),
       method = paste0("CATS topological pharmacophore search with a ",
                       "Bemis-Murcko scaffold test"))
}

#' One-line summary of the scfhop module
#'
#' @return A character scalar.
#' @export
morie_scfhop_cheatsheet <- function()
  paste0("scfhop: scaffold hopping. CATS pharmacophore correlation ",
         "vector for what to keep, Bemis-Murcko framework for what to ",
         "change; a hop is close by the first and different by the second")
