# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of frgrow -- fragment growing, judged by efficiency rather than
# by potency. Mirrors src/morie/fn/frgrow.py operation for operation, on
# the shared numerics in R/aaa_helpers_w3num.R.
#
# A fragment that binds at a millimolar dissociation constant looks like
# nothing next to a micromolar lead, and that comparison is the mistake
# fragment-based discovery exists to correct. A twelve-atom fragment
# binding at 1 mM is doing more per atom than a forty-atom compound
# binding at 1 uM, and it is the per-atom figure that says which one has
# room to grow.
#
# Ligand efficiency is that figure:
#
#     LE = -dG / HAC,   dG = R T ln Kd
#
# with HAC the heavy-atom count. Binding free energy is negative, so the
# minus sign makes LE positive and larger is better. There is a shortcut
# form, LE = 1.37 pKd / HAC, and the 1.37 is not a fitted constant: it
# is 2.303 R T at 298.15 K in kcal per mole, so the two agree at room
# temperature and diverge as soon as you leave it. Both routes are here
# and the temperature is a parameter, because a binding measured at
# 310 K and reported through a 298 K constant is off by four percent and
# nobody notices.
#
# Growing a fragment is then an arithmetic question. Adding a group of
# d_HAC atoms that contributes d_dG of binding has GROUP EFFICIENCY
# GE = -d_dG / d_HAC, and the grown compound's ligand efficiency is
# exactly the atom-weighted average of the two:
#
#     LE_new = (LE_parent HAC_parent + GE d_HAC) / (HAC_parent + d_HAC)
#
# which is worth writing down because it settles the decision rule
# without argument: an addition whose group efficiency beats the
# parent's ligand efficiency RAISES it, one that does not LOWERS it, and
# a tenfold potency gain bought with fifteen atoms is a step backwards
# even though the number in the assay improved. That identity is checked
# here rather than asserted.
#
# The other metrics answer "efficient in what?", and they disagree on
# purpose: LLE = pKd - logP is potency you did not buy with grease;
# LELP = logP / LE is lipophilicity per unit of efficiency, and unlike
# the others SMALLER is better; BEI = pKd / (MW/1000) counts mass rather
# than atoms, so a heavy halogen counts against you; SEI = pKd /
# (PSA/100) counts polar surface.
#
# Nothing here computes a descriptor from a structure. HAC, logP, MW and
# polar surface area come in as numbers, because deriving them needs a
# chemistry toolkit and inventing them would be worse than asking.
#
# References
#   Hopkins, A.L., Groom, C.R. and Alex, A. (2004) "Ligand efficiency: a
#     useful metric for lead selection." Drug Discovery Today 9(10),
#     430-431. doi:10.1016/S1359-6446(04)03069-7.
#   Verdonk, M.L. and Rees, D.C. (2008) "Group efficiency: a guideline
#     for hits-to-leads chemistry." ChemMedChem 3(8), 1179-1180.
#   Murray, C.W. and Rees, D.C. (2009) "The rise of fragment-based drug
#     discovery." Nature Chemistry 1(3), 187-192.
#   Shuker, S.B., Hajduk, P.J., Meadows, R.P. and Fesik, S.W. (1996)
#     "Discovering high-affinity ligands for proteins: SAR by NMR."
#     Science 274(5292), 1531-1534.
#   Leeson, P.D. and Springthorpe, B. (2007) "The influence of drug-like
#     concepts on decision-making in medicinal chemistry." Nature
#     Reviews Drug Discovery 6(11), 881-890.
#   Keseru, G.M. and Makara, G.M. (2009) "The influence of lead
#     discovery strategies on the properties of drug candidates."
#     Nature Reviews Drug Discovery 8(3), 203-212.
#   Abad-Zapatero, C. and Metz, J.T. (2005) "Ligand efficiency indices
#     as guideposts for drug discovery." Drug Discovery Today 10(7),
#     464-469.

.FRGROW_ENERGY_ROUTES <- c("rt", "shortcut")

# The gas constant in kcal per mole per kelvin, and the temperature the
# shortcut constant belongs to.
.FRGROW_R_KCAL <- 0.0019872041
.FRGROW_T_STANDARD <- 298.15
# 2.303 R T at 298.15 K, to two decimals: the constant the literature
# writes as 1.37. It is derived, not fitted, which is why the two energy
# routes agree at this temperature and nowhere else.
.FRGROW_LE_SHORTCUT <- 1.37

#' Binding free energy in kcal per mole, negative for binding
#'
#' Kd is a concentration in molar. A Kd of one molar gives zero, which
#' is the reference the whole scale hangs from.
#'
#' @param kd The dissociation constant, in molar.
#' @param temperature Kelvin.
#' @return The binding free energy.
#' @export
morie_frgrow_dg <- function(kd, temperature = .FRGROW_T_STANDARD) {
  kd <- as.numeric(kd)
  if (kd <= 0) stop("a dissociation constant must be positive")
  t <- as.numeric(temperature)
  if (t <= 0) stop("the temperature must be positive")
  .FRGROW_R_KCAL * t * log(kd)
}

#' Binding energy per heavy atom, positive and larger-is-better
#'
#' The "rt" route is the definition, -R T ln(Kd) / HAC. The "shortcut"
#' route is the literature's 1.37 pKd / HAC, which IS the definition at
#' 298.15 K and drifts from it elsewhere.
#'
#' @param kd The dissociation constant, in molar.
#' @param hac The heavy-atom count.
#' @param route A member of the energy-route list.
#' @param temperature Kelvin.
#' @return The ligand efficiency.
#' @export
morie_frgrow_le <- function(kd, hac, route = "rt",
                            temperature = .FRGROW_T_STANDARD) {
  if (!(route %in% .FRGROW_ENERGY_ROUTES))
    stop("route must be one of ",
         paste(.FRGROW_ENERGY_ROUTES, collapse = ", "))
  n <- as.numeric(hac)
  if (n <= 0) stop("the heavy-atom count must be positive")
  if (route == "rt")
    return(-morie_frgrow_dg(kd, temperature) / n)
  .FRGROW_LE_SHORTCUT * (-log10(as.numeric(kd))) / n
}

#' The efficiency of the atoms that were added, on their own
#'
#' Not the grown compound's efficiency -- the ADDED group's. A group
#' that contributes nothing has a group efficiency of zero however
#' potent the parent was, and one that makes the compound bind worse has
#' a negative one.
#'
#' @param kd_parent The parent's dissociation constant.
#' @param hac_parent The parent's heavy-atom count.
#' @param kd_grown The grown compound's dissociation constant.
#' @param hac_grown The grown compound's heavy-atom count.
#' @param route A member of the energy-route list.
#' @param temperature Kelvin.
#' @return The group efficiency.
#' @export
morie_frgrow_ge <- function(kd_parent, hac_parent, kd_grown, hac_grown,
                            route = "rt",
                            temperature = .FRGROW_T_STANDARD) {
  dn <- as.numeric(hac_grown) - as.numeric(hac_parent)
  if (dn <= 0)
    stop("growing must add heavy atoms; use the parent's own ",
         "efficiency for a compound that added none")
  if (route == "rt") {
    dg <- morie_frgrow_dg(kd_grown, temperature) -
      morie_frgrow_dg(kd_parent, temperature)
    return(-dg / dn)
  }
  dp <- (-log10(as.numeric(kd_grown))) - (-log10(as.numeric(kd_parent)))
  .FRGROW_LE_SHORTCUT * dp / dn
}

#' The efficiency metrics that the supplied descriptors allow
#'
#' A metric whose descriptor is absent is reported as NULL rather than
#' as zero: a compound with no measured logP has no
#' ligand-lipophilicity efficiency, and a zero there would rank it as
#' the best in the series.
#'
#' @param kd The dissociation constant, in molar.
#' @param hac The heavy-atom count.
#' @param logp The partition coefficient, or NULL.
#' @param mw The molecular weight, or NULL.
#' @param psa The polar surface area, or NULL.
#' @param route A member of the energy-route list.
#' @param temperature Kelvin.
#' @return A list with pKd, the free energy and the five metrics.
#' @export
morie_frgrow_metrics <- function(kd, hac, logp = NULL, mw = NULL,
                                 psa = NULL, route = "rt",
                                 temperature = .FRGROW_T_STANDARD) {
  kd <- as.numeric(kd)
  pkd <- -log10(kd)
  le <- morie_frgrow_le(kd, hac, route, temperature)
  out <- list(pkd = pkd, dg = morie_frgrow_dg(kd, temperature), le = le)
  out["lle"] <- list(NULL)
  out["lelp"] <- list(NULL)
  out["bei"] <- list(NULL)
  out["sei"] <- list(NULL)
  if (!is.null(logp)) {
    out$lle <- pkd - as.numeric(logp)
    # LELP is the one metric where smaller is better, and it is also the
    # one that can divide by zero -- a compound whose binding energy per
    # atom is nil has no defined lipophilicity per unit of it.
    if (le != 0) out$lelp <- as.numeric(logp) / le
  }
  if (!is.null(mw)) {
    m <- as.numeric(mw)
    if (m <= 0) stop("molecular weight must be positive")
    out$bei <- pkd / (m / 1000)
  }
  if (!is.null(psa)) {
    p <- as.numeric(psa)
    if (p <= 0) stop("polar surface area must be positive")
    out$sei <- pkd / (p / 100)
  }
  out
}

#' Score a set of grown analogues against the fragment they came from
#'
#' @param fragment The parent: list(kd, hac, logp, mw, psa). The last
#'   three may be NULL.
#' @param linker_lib A list of grown analogues in the same shape,
#'   optionally with a sixth entry naming each.
#' @param route A member of the energy-route list.
#' @param temperature Kelvin. Only the "rt" route uses it; the shortcut
#'   is fixed at 298.15 K by construction, which is the point of
#'   offering both.
#' @return A list with the parent's metrics, each analogue's metrics and
#'   group efficiency, the ranking by group efficiency, and which
#'   additions actually improved the ligand efficiency they inherited.
#' @export
morie_frgrow <- function(fragment, linker_lib, route = "rt",
                         temperature = .FRGROW_T_STANDARD) {
  unpack <- function(row) {
    kd <- as.numeric(row[[1]])
    hac <- as.numeric(row[[2]])
    lp <- if (length(row) < 3L || is.null(row[[3]])) NULL
          else as.numeric(row[[3]])
    mw <- if (length(row) < 4L || is.null(row[[4]])) NULL
          else as.numeric(row[[4]])
    ps <- if (length(row) < 5L || is.null(row[[5]])) NULL
          else as.numeric(row[[5]])
    nm <- if (length(row) >= 6L && !is.null(row[[6]]))
      as.character(row[[6]]) else ""
    list(kd = kd, hac = hac, lp = lp, mw = mw, ps = ps, nm = nm)
  }

  p <- unpack(fragment)
  parent <- morie_frgrow_metrics(p$kd, p$hac, p$lp, p$mw, p$ps, route,
                                 temperature)

  rows <- list()
  for (row in linker_lib) {
    a <- unpack(row)
    m <- morie_frgrow_metrics(a$kd, a$hac, a$lp, a$mw, a$ps, route,
                              temperature)
    ge <- morie_frgrow_ge(p$kd, p$hac, a$kd, a$hac, route, temperature)
    # The identity the decision rule rests on: the grown compound's
    # efficiency is the atom-weighted average of the parent's and the
    # added group's. Recomputing it here rather than trusting it is what
    # makes the check in the tests meaningful.
    blend <- (parent$le * p$hac + ge * (a$hac - p$hac)) / a$hac
    rows[[length(rows) + 1L]] <-
      list(name = a$nm, kd = a$kd, hac = a$hac, d_hac = a$hac - p$hac,
           ge = ge, blend = blend, improved = ge > parent$le, metrics = m)
  }

  n <- length(rows)
  ges <- vapply(rows, function(r) r$ge, numeric(1))
  order_idx <- if (n) order(-ges, seq_len(n)) - 1L else integer(0)
  improved <- if (n)
    which(vapply(rows, function(r) isTRUE(r$improved), logical(1))) - 1L
    else integer(0)
  best <- if (n) order_idx[1] else -1L
  pull <- function(f) if (n) vapply(rows, f, numeric(1)) else numeric(0)
  pull_opt <- function(nm) {
    if (!n) return(numeric(0))
    vapply(rows, function(r) {
      v <- r$metrics[[nm]]
      if (is.null(v)) NaN else as.numeric(v)
    }, numeric(1))
  }
  out <- list(parent_le = parent$le, parent_pkd = parent$pkd,
              parent_dg = parent$dg,
              name = if (n) vapply(rows, function(r) r$name,
                                   character(1)) else character(0),
              kd = pull(function(r) r$kd), hac = pull(function(r) r$hac),
              d_hac = pull(function(r) r$d_hac),
              group_efficiency = ges,
              le = pull(function(r) r$metrics$le),
              le_from_blend = pull(function(r) r$blend),
              pkd = pull(function(r) r$metrics$pkd),
              dg = pull(function(r) r$metrics$dg),
              lle = pull_opt("lle"), lelp = pull_opt("lelp"),
              bei = pull_opt("bei"), sei = pull_opt("sei"),
              improved = improved, n_improved = length(improved),
              ranking = order_idx, best = best,
              estimate = if (n) ges[best + 1L] else NaN, se = NaN,
              n = n, temperature = as.numeric(temperature),
              route = route,
              method = "fragment growing by ligand and group efficiency")
  # `$<-` with NULL deletes the element, so the optional parent metrics
  # go in through single-bracket assignment or they vanish whenever the
  # parent has no logP.
  out["parent_lle"] <- list(parent$lle)
  out["parent_lelp"] <- list(parent$lelp)
  out["parent_bei"] <- list(parent$bei)
  out["parent_sei"] <- list(parent$sei)
  out
}

#' One-line summary of the frgrow module
#'
#' @return A character scalar.
#' @export
morie_frgrow_cheatsheet <- function()
  paste0("frgrow: fragment growing by ligand and group efficiency. ",
         "routes ", paste(.FRGROW_ENERGY_ROUTES, collapse = ", "),
         "; LE = -RT ln(Kd)/HAC, GE on the added atoms only")
