# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of glides -- the Glide-style empirical docking score. Mirrors
# src/morie/fn/glides.py operation for operation, on the shared numerics
# in R/aaa_helpers_w3num.R.
#
# Glide's scoring function is a weighted sum of eight terms, and the
# published form fixes only the first two weights:
#
#     GScore = 0.065 EvdW + 0.130 Coul + Lipo + HBond + Metal
#              + BuryP + RotB + Site
#
# EvdW is the van der Waals energy and Coul the Coulomb energy, both
# computed with REDUCED net ionic charges on formally charged groups --
# metals, carboxylates, guanidiniums -- because a full formal charge on
# a solvent-exposed carboxylate overwhelms everything else in a
# non-solvated calculation. The remaining six enter with unit weight and
# carry their own internal scales: Lipo rewards hydrophobic contact;
# HBond is split by charge state, since a salt bridge and a
# hydroxyl-to-carbonyl bond are not the same event; Metal counts only
# anionic acceptors and only when the apo metal is positively charged;
# BuryP penalises a polar group buried without a partner; RotB
# penalises freezing a rotatable bond; and Site rewards polar but
# non-hydrogen-bonding atoms sitting in a hydrophobic region.
#
# The two coefficients above are the published ones and are the defaults
# here. The internal weights of the other six are NOT published in a
# form this module could quote, so they are parameters with unit
# defaults and this comment says so rather than inventing numbers -- the
# ledger calls this module a proxy for exactly that reason, and a proxy
# that is honest about which of its constants are real is worth more
# than one that is not.
#
# A word on the distance-dependent dielectric, since it is the one place
# a docking score quietly changes its physics. With a constant
# permittivity the Coulomb term falls off as 1/r; with the
# distance-dependent form used through most of the docking literature it
# falls off as 1/r squared, which crudely models the screening of a
# solvent that is not being simulated. The two give different rankings
# and the choice travels in the result.
#
# References
#   Friesner, R.A., Banks, J.L., Murphy, R.B., Halgren, T.A., Klicic,
#     J.J., Mainz, D.T., Repasky, M.P., Knoll, E.H., Shelley, M., Perry,
#     J.K., Shaw, D.E., Francis, P. and Shenkin, P.S. (2004) "Glide: a
#     new approach for rapid, accurate docking and scoring. 1. Method
#     and assessment of docking accuracy." Journal of Medicinal
#     Chemistry 47(7), 1739-1749. doi:10.1021/jm0306430.
#   Halgren, T.A., Murphy, R.B., Friesner, R.A., Beard, H.S., Frye,
#     L.L., Pollard, W.T. and Banks, J.L. (2004) "Glide ... 2.
#     Enrichment factors in database screening." Journal of Medicinal
#     Chemistry 47(7), 1750-1759.
#   Friesner, R.A., Murphy, R.B., Repasky, M.P., Frye, L.L., Greenwood,
#     J.R., Halgren, T.A., Sanschagrin, P.C. and Mainz, D.T. (2006)
#     "Extra precision Glide." Journal of Medicinal Chemistry 49(21),
#     6177-6196.
#   Eldridge, M.D. et al. (1997) "Empirical scoring functions: I."
#     Journal of Computer-Aided Molecular Design 11(5), 425-445.

.GLIDES_DIELECTRICS <- c("constant", "distance")
.GLIDES_HBOND_CLASSES <- c("neutral_neutral", "neutral_charged",
                           "charged_charged")

# The two published GScore coefficients, Friesner et al. (2004).
.GLIDES_COEFFICIENTS <- list(vdw = 0.065, coulomb = 0.130)

# Unit defaults for the six terms whose internal weights the papers do
# not state in a quotable form. They are parameters, not constants.
.GLIDES_WEIGHTS <- list(lipo = 1, hbond = 1, metal = 1, buryp = 1,
                        rotb = 1, site = 1,
                        hbond_neutral_neutral = 1,
                        hbond_neutral_charged = 1,
                        hbond_charged_charged = 1)

# Coulomb's constant in kcal/mol per elementary charge squared per
# angstrom -- the unit system a docking score works in.
.GLIDES_COULOMB_K <- 332.0637

# x to a small integer power, by repeated multiplication. Not the
# language's power operator: R uses repeated squaring for an integer
# exponent and Python calls the C library's pow, and they disagree in
# the last bit.
#' X to a small integer power, by repeated multiplication. Not the
#'
#' language\'s power operator: R uses repeated squaring for an integer
#' exponent and Python calls the C library\'s pow, and they disagree in
#' the last bit.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @param k A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{p}, as built in the body.
#' @export
.glides_ipow <- function(x, k) {
  p <- 1
  k <- as.integer(k)
  if (k > 0L) for (i in seq_len(k)) p <- p * x
  p
}

#' .glides_merge
#'
#' A step of the glides_native implementation. Called by \code{morie_glides_hbond}, \code{morie_glides_score}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param base A vector; indexed elementwise.
#' @param over Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @return The value of \code{base}, as built in the body.
#' @export
.glides_merge <- function(base, over) {
  if (is.null(over)) return(base)
  for (nm in names(over)) base[[nm]] <- over[[nm]]
  base
}

#' Lennard-Jones energy over a list of contacts
#'
#' Parameterised by the minimum, so r0 is where the well sits and eps is
#' how deep it is. The reduced ionic charges the paper describes affect
#' the Coulomb term, not this one.
#'
#' @param pairs A list of list(distance, r0, eps).
#' @param m The attractive exponent.
#' @param n The repulsive exponent.
#' @return A list with the total and the per-contact energies.
#' @export
morie_glides_vdw <- function(pairs, m = 6, n = 12) {
  terms <- vapply(pairs, function(p) {
    r <- as.numeric(p[[1]])
    if (r <= 0) return(Inf)
    q <- as.numeric(p[[2]]) / r
    as.numeric(p[[3]]) * ((as.numeric(m) / (n - m)) * .glides_ipow(q, n) -
                            (as.numeric(n) / (n - m)) * .glides_ipow(q, m))
  }, numeric(1))
  list(total = if (length(terms)) .w3_csum(terms) else 0, terms = terms)
}

#' Coulomb energy over a list of contacts
#'
#' With a constant permittivity the energy falls off as 1/r. With the
#' distance-dependent form -- the usual choice in docking, where no
#' solvent is present -- the permittivity is epsilon times r and the
#' energy falls off as 1/r squared. That is a different physics, not a
#' different constant, so it is a route and not a parameter tweak.
#'
#' @param pairs A list of list(distance, q_i, q_j).
#' @param dielectric A member of the dielectric list.
#' @param epsilon The permittivity scale.
#' @return A list with the total and the per-contact energies.
#' @export
morie_glides_coulomb <- function(pairs, dielectric = "constant",
                                 epsilon = 1) {
  if (!(dielectric %in% .GLIDES_DIELECTRICS))
    stop("dielectric must be one of ",
         paste(.GLIDES_DIELECTRICS, collapse = ", "))
  if (epsilon <= 0) stop("the permittivity must be positive")
  terms <- vapply(pairs, function(p) {
    r <- as.numeric(p[[1]])
    if (r <= 0) return(Inf)
    den <- if (dielectric == "constant") epsilon * r else epsilon * r * r
    .GLIDES_COULOMB_K * as.numeric(p[[2]]) * as.numeric(p[[3]]) / den
  }, numeric(1))
  list(total = if (length(terms)) .w3_csum(terms) else 0, terms = terms)
}

#' A ramped contact count over lipophilic atom pairs
#'
#' One inside the inner radius, zero past the outer, linear between --
#' the contact ramp of the empirical-scoring literature. Glide's own
#' lipophilic term is not published in a form this module could
#' reproduce, so this is a STAND-IN with stated parameters, and it is
#' the reason the ledger calls this module a proxy.
#'
#' @param distances The lipophilic pair separations.
#' @param r1 The inner radius.
#' @param r2 The outer radius.
#' @return A list with the total and the per-pair contributions.
#' @export
morie_glides_lipo <- function(distances, r1 = 4.1, r2 = 7.1) {
  if (r2 <= r1) stop("the outer radius must exceed the inner")
  terms <- vapply(distances, function(r) {
    r <- as.numeric(r)
    if (r <= r1) 1 else if (r >= r2) 0 else (r2 - r) / (r2 - r1)
  }, numeric(1))
  list(total = if (length(terms)) .w3_csum(terms) else 0, terms = terms)
}

#' Hydrogen bonds summed within their three charge classes
#'
#' The classes are weighted separately because a charged-charged bond is
#' a salt bridge and a neutral pair is not; collapsing them to one
#' number is the modelling error this split exists to prevent.
#'
#' @param bonds A list of list(class, strength).
#' @param weights Overrides for the class weights, or NULL.
#' @return A list with the total, the per-bond contributions and the
#'   three class subtotals.
#' @export
morie_glides_hbond <- function(bonds, weights = NULL) {
  w <- .glides_merge(.GLIDES_WEIGHTS, weights)
  by <- as.list(rep(0, length(.GLIDES_HBOND_CLASSES)))
  names(by) <- .GLIDES_HBOND_CLASSES
  terms <- numeric(0)
  for (b in bonds) {
    cl <- as.character(b[[1]])
    if (!(cl %in% .GLIDES_HBOND_CLASSES))
      stop("hydrogen bond class must be one of ",
           paste(.GLIDES_HBOND_CLASSES, collapse = ", "))
    v <- w[[paste0("hbond_", cl)]] * as.numeric(b[[2]])
    by[[cl]] <- by[[cl]] + v
    terms <- c(terms, v)
  }
  list(total = if (length(terms)) .w3_csum(terms) else 0, terms = terms,
       by_class = by)
}

#' Assemble the eight terms into a GScore
#'
#' The two published coefficients multiply the van der Waals and Coulomb
#' energies; the other six enter through weights that default to one.
#'
#' @param vdw The van der Waals energy.
#' @param coulomb The Coulomb energy.
#' @param lipo The lipophilic contact term.
#' @param hbond The hydrogen bonding term.
#' @param metal The metal binding term.
#' @param buryp The buried polar penalty.
#' @param rotb The rotatable bond penalty.
#' @param site The active site polar term.
#' @param coefficients Overrides for the two published coefficients.
#' @param weights Overrides for the six unit weights.
#' @return A list with the total, the weighted parts and their order.
#' @export
morie_glides_score <- function(vdw = 0, coulomb = 0, lipo = 0, hbond = 0,
                               metal = 0, buryp = 0, rotb = 0, site = 0,
                               coefficients = NULL, weights = NULL) {
  co <- .glides_merge(.GLIDES_COEFFICIENTS, coefficients)
  w <- .glides_merge(.GLIDES_WEIGHTS, weights)
  order <- c("vdw", "coulomb", "lipo", "hbond", "metal", "buryp", "rotb",
             "site")
  parts <- list(vdw = co$vdw * as.numeric(vdw),
                coulomb = co$coulomb * as.numeric(coulomb),
                lipo = w$lipo * as.numeric(lipo),
                hbond = w$hbond * as.numeric(hbond),
                metal = w$metal * as.numeric(metal),
                buryp = w$buryp * as.numeric(buryp),
                rotb = w$rotb * as.numeric(rotb),
                site = w$site * as.numeric(site))
  total <- .w3_csum(vapply(order, function(k) parts[[k]], numeric(1)))
  list(total = total, parts = parts, order = order)
}

#' .glides_dist
#'
#' A step of the glides_native implementation. Called by \code{morie_glides}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.glides_dist <- function(a, b) sqrt(.w3_csum((a - b) * (a - b)))

#' Score a pose in the Glide form
#'
#' @param receptor Atom rows: x, y, z, type.
#' @param ligand_pose The ligand atom rows, likewise.
#' @param radii A list of list(type, contact radius).
#' @param depths A list of list(type, well depth).
#' @param charges A list of list(type, partial charge), already REDUCED
#'   on formally charged groups as the paper requires -- that reduction
#'   is a chemical judgement and is the caller's.
#' @param lipophilic Atom types treated as lipophilic.
#' @param hbonds A list of list(class, strength) per hydrogen bond.
#' @param dielectric A member of the dielectric list.
#' @param epsilon The permittivity scale.
#' @param m The attractive exponent.
#' @param n The repulsive exponent.
#' @param r1 The inner lipophilic radius.
#' @param r2 The outer lipophilic radius.
#' @param metal The metal binding term.
#' @param buryp The buried polar penalty.
#' @param n_rot Frozen rotatable bonds.
#' @param rot_penalty The penalty per frozen bond; POSITIVE, so it
#'   raises the score of a floppy ligand.
#' @param site The active site polar term.
#' @param coefficients Overrides for the two published coefficients.
#' @param weights Overrides for the six unit weights.
#' @param cutoff Drop contacts beyond this separation, or NULL.
#' @return A list with the GScore, each weighted contribution, and the
#'   raw energies.
#' @export
morie_glides <- function(receptor, ligand_pose, radii = list(),
                         depths = list(), charges = list(),
                         lipophilic = character(0), hbonds = list(),
                         dielectric = "constant", epsilon = 1, m = 6,
                         n = 12, r1 = 4.1, r2 = 7.1, metal = 0,
                         buryp = 0, n_rot = 0, rot_penalty = 0.35,
                         site = 0, coefficients = NULL, weights = NULL,
                         cutoff = NULL) {
  look <- function(table, key, what) {
    for (kv in table) if (kv[[1]] == key) return(as.numeric(kv[[2]]))
    stop("no ", what, " for atom type ", key)
  }
  rec <- lapply(receptor, function(a)
    list(xyz = c(as.numeric(a[[1]]), as.numeric(a[[2]]),
                 as.numeric(a[[3]])), type = as.character(a[[4]])))
  lig <- lapply(ligand_pose, function(a)
    list(xyz = c(as.numeric(a[[1]]), as.numeric(a[[2]]),
                 as.numeric(a[[3]])), type = as.character(a[[4]])))
  lipset <- as.character(lipophilic)

  vp <- list(); cp <- list(); lp <- numeric(0)
  for (ra in rec) for (la in lig) {
    d <- .glides_dist(ra$xyz, la$xyz)
    if (!is.null(cutoff) && d > as.numeric(cutoff)) next
    vp[[length(vp) + 1L]] <- list(d,
                                  look(radii, ra$type, "radius") +
                                    look(radii, la$type, "radius"),
                                  sqrt(look(depths, ra$type, "well depth") *
                                         look(depths, la$type, "well depth")))
    cp[[length(cp) + 1L]] <- list(d, look(charges, ra$type, "charge"),
                                  look(charges, la$type, "charge"))
    if (ra$type %in% lipset && la$type %in% lipset) lp <- c(lp, d)
  }
  ev <- morie_glides_vdw(vp, m, n)
  ec <- morie_glides_coulomb(cp, dielectric, epsilon)
  el <- morie_glides_lipo(lp, r1, r2)
  eh <- morie_glides_hbond(hbonds, weights)
  er <- as.numeric(rot_penalty) * as.integer(n_rot)

  g <- morie_glides_score(ev$total, ec$total, el$total, eh$total, metal,
                          buryp, er, site, coefficients, weights)
  list(gscore = g$total, estimate = g$total, se = NaN,
       parts = vapply(g$order, function(k) g$parts[[k]], numeric(1)),
       part_names = g$order, vdw_energy = ev$total,
       coulomb_energy = ec$total, lipophilic_count = el$total,
       hbond_total = eh$total,
       hbond_by_class = vapply(.GLIDES_HBOND_CLASSES,
                               function(cl) eh$by_class[[cl]], numeric(1)),
       rot_penalty = er, metal = as.numeric(metal),
       buryp = as.numeric(buryp), site = as.numeric(site),
       vdw_terms = ev$terms, coulomb_terms = ec$terms,
       lipophilic_terms = el$terms, hbond_terms = eh$terms,
       n_contacts = length(vp), n_lipophilic = length(lp),
       n_hbond = length(eh$terms), n_rot = as.integer(n_rot),
       dielectric = dielectric, epsilon = as.numeric(epsilon),
       method = "Glide-style empirical docking score")
}

#' One-line summary of the glides module
#'
#' @return A character scalar.
#' @export
morie_glides_cheatsheet <- function()
  paste0("glides: Glide-style empirical docking score. dielectrics ",
         paste(.GLIDES_DIELECTRICS, collapse = ", "),
         "; GScore = 0.065 vdW + 0.130 Coulomb + six weighted terms")
