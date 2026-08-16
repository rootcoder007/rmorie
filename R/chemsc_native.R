# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of chemsc -- ChemScore, the empirical protein-ligand docking
# score. Mirrors src/morie/fn/chemsc.py operation for operation, on the
# shared numerics in R/aaa_helpers_w3num.R.
#
# ChemScore estimates the free energy of binding as a sum of physical
# contributions, each multiplied by a coefficient fitted by regression
# against measured affinities for 82 protein-ligand complexes:
#
#     dG = dG0 + v1 Sum(hbond) + v2 Sum(metal) + v3 Sum(lipophilic)
#               + v4 Hrot
#
# with v1 = -3.34, v2 = -6.03, v3 = -0.117 and v4 = 2.56. The signs are
# the interesting part: hydrogen bonds, metal contacts and lipophilic
# contact all make binding more favourable, and freezing a rotatable
# bond makes it less -- the entropy you pay for holding the ligand
# still.
#
# Every geometric term is a BLOCK FUNCTION of a deviation from ideal:
# one inside the tolerance, zero past the maximum, a straight line
# between. The GOLD implementation convolves that block with a Gaussian
# to smooth the two corners, which matters when the score is being
# optimised: a genetic algorithm climbing a function with a kink gets
# stuck on the kink. Both are here, selectable, because the unsmoothed
# block is what the 1997 paper defines and the smoothed one is what a
# docking run should use. The convolution is done in closed form rather
# than numerically -- a trapezoid against a Gaussian integrates to
# normal distribution and density values, so there is no quadrature to
# disagree about.
#
# A hydrogen bond contributes the PRODUCT of three of these: one on the
# H...A distance, one on the D-H...A angle, and one on the H...A-X angle
# at the acceptor. An acceptor with several attached heavy atoms
# contributes the product over all of them. So a bond of ideal geometry
# contributes exactly one and anything else contributes less, which is
# why the term is a count of good hydrogen bonds rather than an energy.
#
# Parameter provenance. Every coefficient, radius, angle and sigma below
# is the default stated in the CCDC GOLD User Guide, which names each
# one by its identifier in the ChemScore parameter file; those
# identifiers are carried through into the constant names here so the
# two can be checked against each other. Two things are NOT taken from a
# source and are marked as this module's own: the shape of the clash
# penalty (the guide gives its radii but renders the function itself as
# an image), and the value of the intercept dG0 (the guide omits it,
# since a constant cannot change a ranking). dG0 therefore defaults to
# zero and is a parameter, not a hard-coded number.
#
# References
#   Eldridge, M.D., Murray, C.W., Auton, T.R., Paolini, G.V. and Mee,
#     R.P. (1997) "Empirical scoring functions: I. The development of a
#     fast empirical scoring function to estimate the binding affinity
#     of ligands in receptor complexes." Journal of Computer-Aided
#     Molecular Design 11(5), 425-445. doi:10.1023/A:1007996124545.
#   Baxter, C.A., Murray, C.W., Clark, D.E., Westhead, D.R. and
#     Eldridge, M.D. (1998) "Flexible docking using Tabu search and an
#     empirical estimate of binding affinity." Proteins 33(3), 367-382.
#   Cambridge Crystallographic Data Centre, "GOLD User Guide," sections
#     8.4.1 to 8.4.6. Every default parameter value used here.
#   Verdonk, M.L., Cole, J.C., Hartshorn, M.J., Murray, C.W. and Taylor,
#     R.D. (2003) "Improved protein-ligand docking using GOLD."
#     Proteins 52(4), 609-623.

.CHEMSC_SMOOTHINGS <- c("gaussian", "none")

# Regression coefficients, GOLD User Guide 8.4.3 to 8.4.5. The names are
# the identifiers in the ChemScore parameter file.
.CHEMSC_COEFFICIENTS <- list(HBOND_COEFFICIENT = -3.34,
                             METAL_COEFFICIENT = -6.03,
                             LIPO_COEFFICIENT = -0.117,
                             ROT_COEFFICIENT = 2.56)

# Hydrogen-bond geometry, GOLD User Guide 8.4.3. Distances in
# angstroms, angles in degrees.
.CHEMSC_HBOND <- list(R_IDEAL = 1.85, DELTA_R_IDEAL = 0.25,
                      DELTA_R_MAX = 0.65, HBOND_R_SIGMA = 0.1,
                      ALPHA_IDEAL = 180, DELTA_ALPHA_IDEAL = 30,
                      DELTA_ALPHA_MAX = 80, HBOND_ALPHA_SIGMA = 10,
                      BETA_IDEAL = 180, DELTA_BETA_IDEAL = 70,
                      DELTA_BETA_MAX = 80, HBOND_BETA_SIGMA = 10)

# Metal-binding geometry, GOLD User Guide 8.4.4.
.CHEMSC_METAL <- list(METAL_R1 = 2.6, METAL_R2 = 3.0, METAL_R_SIGMA = 0.1)

# Lipophilic contact geometry, GOLD User Guide 8.4.4. The lipophilic
# term is scored over a much longer range than the metal one, which is
# the whole difference between the two parameterisations.
.CHEMSC_LIPO <- list(LIPO_R1 = 4.1, LIPO_R2 = 7.1, LIPO_R_SIGMA = 0.1)

# Clash radii, GOLD User Guide 8.4.6.
.CHEMSC_CLASH <- list(CLASH_RADIUS_HBOND = 1.6, CLASH_RADIUS_METAL = 1.3,
                      CLASH_RADIUS_SULPHUR = 3.35,
                      CLASH_RADIUS_GENERAL = 3.10)

#' The ChemScore block function of a deviation from ideal
#'
#' One inside the tolerance, zero past the maximum, a straight line
#' between. Note it is a function of the DEVIATION, so it is already
#' folded about zero and never sees a sign.
#'
#' @param d The deviation.
#' @param d_ideal The tolerance window.
#' @param d_max The maximum deviation.
#' @return A value in the unit interval.
#' @export
morie_chemsc_block <- function(d, d_ideal, d_max) {
  d <- abs(as.numeric(d))
  if (d_max <= d_ideal)
    stop("the maximum deviation must exceed the ideal tolerance")
  if (d <= d_ideal) return(1)
  if (d >= d_max) return(0)
  (d_max - d) / (d_max - d_ideal)
}

#' The block function convolved with a Gaussian, in closed form
#'
#' A trapezoid against a normal density integrates to normal
#' distribution and density values, so this is exact arithmetic and not
#' a quadrature -- which matters here, because a numerical convolution
#' would be the one part of the score that two implementations could
#' disagree about while both being "right". With sigma at or below zero
#' this is the plain block function, which is the honest limit rather
#' than a special case.
#'
#' @param d The deviation.
#' @param d_ideal The tolerance window.
#' @param d_max The maximum deviation.
#' @param sigma The Gaussian smearing width.
#' @return A value in the unit interval.
#' @export
morie_chemsc_smooth_block <- function(d, d_ideal, d_max, sigma) {
  if (sigma <= 0) return(morie_chemsc_block(d, d_ideal, d_max))
  d <- abs(as.numeric(d))
  if (d_max <= d_ideal)
    stop("the maximum deviation must exceed the ideal tolerance")
  z1 <- (d_ideal - d) / sigma
  z2 <- (d_max - d) / sigma
  flat <- .w3_ncdf(z1)
  ramp <- ((d_max - d) * (.w3_ncdf(z2) - .w3_ncdf(z1)) -
             sigma * (.w3_npdf(z1) - .w3_npdf(z2))) / (d_max - d_ideal)
  v <- flat + ramp
  # The convolution of a function bounded in [0, 1] is bounded in
  # [0, 1]; only rounding can put it outside, and letting that leak into
  # a product of three terms would be a slow poison.
  if (v < 0) return(0)
  if (v > 1) return(1)
  v
}

.chemsc_B <- function(d, d_ideal, d_max, sigma, smoothing) {
  if (smoothing == "none") return(morie_chemsc_block(d, d_ideal, d_max))
  if (smoothing == "gaussian")
    return(morie_chemsc_smooth_block(d, d_ideal, d_max, sigma))
  stop("smoothing must be one of ",
       paste(.CHEMSC_SMOOTHINGS, collapse = ", "))
}

.chemsc_par <- function(base, over) {
  if (is.null(over)) return(base)
  for (nm in names(over)) base[[nm]] <- over[[nm]]
  base
}

#' One donor-acceptor pair's contribution, at most one
#'
#' The block values on the H...A distance, the D-H...A angle and every
#' H...A-X angle MULTIPLY: an acceptor with three neighbours has to
#' satisfy all three directions, not the best one.
#'
#' @param r The H...A distance.
#' @param alpha The D-H...A angle, in degrees.
#' @param betas The H...A-X angles, one per heavy atom attached to the
#'   acceptor.
#' @param smoothing A member of the smoothing list.
#' @param par Overrides for the hydrogen-bond parameters, or NULL.
#' @return A value in the unit interval.
#' @export
morie_chemsc_hbond <- function(r, alpha, betas, smoothing = "gaussian",
                               par = NULL) {
  p <- .chemsc_par(.CHEMSC_HBOND, par)
  v <- .chemsc_B(r - p$R_IDEAL, p$DELTA_R_IDEAL, p$DELTA_R_MAX,
                 p$HBOND_R_SIGMA, smoothing)
  v <- v * .chemsc_B(alpha - p$ALPHA_IDEAL, p$DELTA_ALPHA_IDEAL,
                     p$DELTA_ALPHA_MAX, p$HBOND_ALPHA_SIGMA, smoothing)
  for (b in betas)
    v <- v * .chemsc_B(b - p$BETA_IDEAL, p$DELTA_BETA_IDEAL,
                       p$DELTA_BETA_MAX, p$HBOND_BETA_SIGMA, smoothing)
  v
}

# How far past the ideal separation a contact is, never negative. The
# metal and lipophilic terms are RANGES, not windows: the guide calls R1
# the ideal separation and R2 "the maximum distance to be considered a
# binding interaction", so anything at or inside R1 is fully ideal and
# only the far side ramps down. Folding this about zero the way the
# hydrogen-bond deviations are folded would penalise a contact for being
# too close, which is the clash term's job and not this one's -- and it
# would also make a contact sitting exactly on R1 score a hair under one
# whenever the coordinate arithmetic put it a single bit on the wrong
# side.
.chemsc_over <- function(r, r1) {
  d <- as.numeric(r) - r1
  if (d > 0) d else 0
}

#' One acceptor-metal contact's contribution, at most one
#'
#' @param r The acceptor-metal distance.
#' @param smoothing A member of the smoothing list.
#' @param par Overrides for the metal parameters, or NULL.
#' @return A value in the unit interval.
#' @export
morie_chemsc_metal <- function(r, smoothing = "gaussian", par = NULL) {
  p <- .chemsc_par(.CHEMSC_METAL, par)
  .chemsc_B(.chemsc_over(r, p$METAL_R1), 0, p$METAL_R2 - p$METAL_R1,
            p$METAL_R_SIGMA, smoothing)
}

#' One lipophilic atom pair's contribution, at most one
#'
#' @param r The distance between the pair.
#' @param smoothing A member of the smoothing list.
#' @param par Overrides for the lipophilic parameters, or NULL.
#' @return A value in the unit interval.
#' @export
morie_chemsc_lipophilic <- function(r, smoothing = "gaussian",
                                    par = NULL) {
  p <- .chemsc_par(.CHEMSC_LIPO, par)
  .chemsc_B(.chemsc_over(r, p$LIPO_R1), 0, p$LIPO_R2 - p$LIPO_R1,
            p$LIPO_R_SIGMA, smoothing)
}

#' The frozen-rotatable-bond entropy term
#'
#' Hrot = 1 + (1 - 1/Nrot) Sum (P_nl + P'_nl) / 2, which is one unit for
#' the first frozen bond and progressively less for each one after it --
#' freezing a bond in an already rigid ligand costs less than freezing
#' the first. With no frozen bonds it is zero, not one: there is no
#' entropy to pay. The GOLD guide quotes the two fractions as
#' percentages; they are taken here as fractions in the unit interval.
#'
#' @param fractions A list of length-two vectors, the non-lipophilic
#'   fractions on the two sides of each FROZEN rotatable bond.
#' @return The entropy term.
#' @export
morie_chemsc_rot <- function(fractions) {
  n <- length(fractions)
  if (n == 0L) return(0)
  s <- .w3_csum(vapply(fractions, function(ab)
    0.5 * (as.numeric(ab[1]) + as.numeric(ab[2])), numeric(1)))
  1 + (1 - 1 / n) * s
}

#' The clash penalty for one too-close contact
#'
#' The radii are the GOLD defaults: 1.6 for a hydrogen-bonding contact,
#' 1.3 for a metal coordination contact, 3.35 to a protein sulphur and
#' 3.10 for anything else. A hydrogen bond is allowed much closer than a
#' general contact because it IS a close contact. The SHAPE of the
#' penalty is this module's, not a source's: the guide states the radii
#' and renders the function as an image. A linear ramp per angstrom of
#' overlap is used, which is monotone, zero at the radius and continuous
#' there -- the three properties a docking run actually needs from it.
#'
#' @param r The contact distance.
#' @param kind One of hbond, metal, sulphur, general.
#' @param slope The penalty per angstrom of overlap.
#' @param par Overrides for the clash radii, or NULL.
#' @return The penalty, zero at or beyond the radius.
#' @export
morie_chemsc_clash <- function(r, kind = "general", slope = 1,
                               par = NULL) {
  p <- .chemsc_par(.CHEMSC_CLASH, par)
  key <- switch(kind, hbond = "CLASH_RADIUS_HBOND",
                metal = "CLASH_RADIUS_METAL",
                sulphur = "CLASH_RADIUS_SULPHUR",
                general = "CLASH_RADIUS_GENERAL", NULL)
  if (is.null(key)) stop("kind must be hbond, metal, sulphur or general")
  rc <- p[[key]]
  r <- as.numeric(r)
  if (r >= rc) 0 else as.numeric(slope) * (rc - r)
}

#' One rotatable bond's internal torsional strain
#'
#' The cosine form the ChemScore parameter file carries, with A, n and
#' the phase read from lines like SP3_SP3_BOND. The angle is in degrees
#' on the way in, because that is how a torsion is measured.
#'
#' @param phi The torsion angle, in degrees.
#' @param A The barrier height.
#' @param n The periodicity.
#' @param phi0 The phase, in radians.
#' @return The strain contribution.
#' @export
morie_chemsc_torsion <- function(phi, A, n, phi0)
  as.numeric(A) * (1 + cos(as.numeric(n) * as.numeric(phi) * pi / 180 -
                             as.numeric(phi0)))

#' Assemble a ChemScore from its already-measured geometric terms
#'
#' This is the function the docking front end calls once it has turned
#' coordinates into contacts, and it is separately callable because a
#' reader who wants to check the arithmetic should not have to build a
#' protein first.
#'
#' @param hbonds A list of list(r, alpha, betas) per donor-acceptor pair.
#' @param metals Acceptor-metal distances.
#' @param lipophilic Lipophilic pair distances.
#' @param rotatable Non-lipophilic fraction pairs for frozen bonds.
#' @param clashes A list of list(r, kind) per close contact.
#' @param torsions A list of c(phi, A, n, phi0) per rotatable bond.
#' @param smoothing A member of the smoothing list.
#' @param dg0 The intercept.
#' @param clash_slope The clash penalty per angstrom of overlap.
#' @param intra_coefficient The weight on internal torsional strain.
#' @param coefficients Overrides for the regression coefficients.
#' @param par Overrides for the geometry parameters, as a named list with
#'   hbond, metal, lipo and clash entries.
#' @return A list with the free energy estimate, the fitness and every
#'   term separately so the total can be checked against its parts.
#' @export
morie_chemsc_score <- function(hbonds = list(), metals = numeric(0),
                               lipophilic = numeric(0),
                               rotatable = list(), clashes = list(),
                               torsions = list(), smoothing = "gaussian",
                               dg0 = 0, clash_slope = 1,
                               intra_coefficient = 1, coefficients = NULL,
                               par = NULL) {
  if (!(smoothing %in% .CHEMSC_SMOOTHINGS))
    stop("smoothing must be one of ",
         paste(.CHEMSC_SMOOTHINGS, collapse = ", "))
  co <- .chemsc_par(.CHEMSC_COEFFICIENTS, coefficients)
  hp <- if (is.null(par)) NULL else par$hbond
  mp <- if (is.null(par)) NULL else par$metal
  lp <- if (is.null(par)) NULL else par$lipo
  cp <- if (is.null(par)) NULL else par$clash

  hb <- vapply(hbonds, function(h)
    morie_chemsc_hbond(h[[1]], h[[2]], h[[3]], smoothing, hp), numeric(1))
  mt <- vapply(metals, function(r)
    morie_chemsc_metal(r, smoothing, mp), numeric(1))
  lpv <- vapply(lipophilic, function(r)
    morie_chemsc_lipophilic(r, smoothing, lp), numeric(1))
  s_hb <- if (length(hb)) .w3_csum(hb) else 0
  s_mt <- if (length(mt)) .w3_csum(mt) else 0
  s_lp <- if (length(lpv)) .w3_csum(lpv) else 0
  h_rot <- morie_chemsc_rot(rotatable)

  cl <- vapply(clashes, function(cc)
    morie_chemsc_clash(cc[[1]], cc[[2]], clash_slope, cp), numeric(1))
  to <- vapply(torsions, function(t)
    morie_chemsc_torsion(t[1], t[2], t[3], t[4]), numeric(1))
  s_cl <- if (length(cl)) .w3_csum(cl) else 0
  s_to <- if (length(to)) .w3_csum(to) else 0

  dg <- as.numeric(dg0) + co$HBOND_COEFFICIENT * s_hb +
    co$METAL_COEFFICIENT * s_mt + co$LIPO_COEFFICIENT * s_lp +
    co$ROT_COEFFICIENT * h_rot
  # The fitness is the negative of the free energy so that bigger is
  # better, with the penalties subtracted from it -- a clash makes a
  # pose worse whichever sign convention the energy is carrying.
  fitness <- -dg - s_cl - as.numeric(intra_coefficient) * s_to
  list(dg = dg, fitness = fitness, hbond = s_hb, metal = s_mt,
       lipophilic = s_lp, h_rot = h_rot, clash = s_cl, torsion = s_to,
       hbond_terms = hb, metal_terms = mt, lipophilic_terms = lpv,
       clash_terms = cl, torsion_terms = to, n_hbond = length(hb),
       n_metal = length(mt), n_lipophilic = length(lpv),
       n_rotatable = length(rotatable), n_clash = length(cl),
       estimate = dg, se = NaN, dg0 = as.numeric(dg0),
       smoothing = smoothing, method = "ChemScore empirical docking")
}

.chemsc_dist <- function(a, b) sqrt(.w3_csum((a - b) * (a - b)))

# The angle at b, in degrees, formed by a-b-c.
.chemsc_angle <- function(a, b, c) {
  u <- a - b
  v <- c - b
  nu <- sqrt(.w3_dot(u, u))
  nv <- sqrt(.w3_dot(v, v))
  if (nu <= 0 || nv <= 0) return(NaN)
  cc <- .w3_dot(u, v) / (nu * nv)
  if (cc > 1) cc <- 1
  if (cc < -1) cc <- -1
  acos(cc) * 180 / pi
}

#' Score a pose from coordinates and atom roles
#'
#' @param receptor A list of atom rows: x, y, z, role, then the
#'   coordinates of the attached atom the geometry needs -- the donor's
#'   hydrogen for a donor, the acceptor's attached heavy atom for an
#'   acceptor. Roles are "donor", "acceptor", "metal", "lipophilic",
#'   "sulphur" and "other". Rows whose partner coordinates are absent
#'   contribute no directional term.
#' @param ligand The ligand atom rows, likewise.
#' @param smoothing A member of the smoothing list.
#' @param dg0 The intercept.
#' @param clash_slope The clash penalty per angstrom of overlap.
#' @param intra_coefficient The weight on internal torsional strain.
#' @param rotatable Non-lipophilic fraction pairs for each FROZEN
#'   rotatable bond. Nothing in a set of coordinates says which bonds
#'   are frozen, so this is supplied rather than guessed.
#' @param torsions A list of c(phi, A, n, phi0) per rotatable bond.
#' @param coefficients Overrides for the regression coefficients.
#' @param par Overrides for the geometry parameters.
#' @return As the score function, with the contact lists it built.
#' @export
morie_chemsc <- function(receptor, ligand, smoothing = "gaussian",
                         dg0 = 0, clash_slope = 1, intra_coefficient = 1,
                         rotatable = list(), torsions = list(),
                         coefficients = NULL, par = NULL) {
  parse <- function(rows) lapply(rows, function(r) {
    xyz <- c(as.numeric(r[[1]]), as.numeric(r[[2]]), as.numeric(r[[3]]))
    role <- as.character(r[[4]])
    att <- NULL
    if (length(r) >= 7L && !is.na(r[[5]]))
      att <- c(as.numeric(r[[5]]), as.numeric(r[[6]]), as.numeric(r[[7]]))
    list(xyz = xyz, role = role, att = att)
  })
  rec <- parse(receptor)
  lig <- parse(ligand)

  hbonds <- list(); metals <- numeric(0); lipo <- numeric(0)
  clashes <- list()
  for (ra in rec) for (la in lig) {
    d <- .chemsc_dist(ra$xyz, la$xyz)
    pair <- NULL
    if (ra$role == "donor" && la$role == "acceptor") {
      # The donor's hydrogen is the atom the distance and the D-H...A
      # angle are both measured from, not the donor heavy atom.
      if (!is.null(ra$att)) {
        hb_r <- .chemsc_dist(ra$att, la$xyz)
        al <- .chemsc_angle(ra$xyz, ra$att, la$xyz)
        be <- if (!is.null(la$att))
          .chemsc_angle(ra$att, la$xyz, la$att) else numeric(0)
        hbonds[[length(hbonds) + 1L]] <- list(hb_r, al, be)
        pair <- list(hb_r, "hbond")
      }
    } else if (ra$role == "acceptor" && la$role == "donor") {
      if (!is.null(la$att)) {
        hb_r <- .chemsc_dist(la$att, ra$xyz)
        al <- .chemsc_angle(la$xyz, la$att, ra$xyz)
        be <- if (!is.null(ra$att))
          .chemsc_angle(la$att, ra$xyz, ra$att) else numeric(0)
        hbonds[[length(hbonds) + 1L]] <- list(hb_r, al, be)
        pair <- list(hb_r, "hbond")
      }
    } else if (ra$role == "metal" && la$role == "acceptor") {
      metals <- c(metals, d)
      pair <- list(d, "metal")
    } else if (ra$role == "acceptor" && la$role == "metal") {
      metals <- c(metals, d)
      pair <- list(d, "metal")
    } else if (ra$role %in% c("lipophilic", "sulphur") &&
               la$role == "lipophilic") {
      lipo <- c(lipo, d)
    }
    clashes[[length(clashes) + 1L]] <- if (!is.null(pair)) pair else
      list(d, if (ra$role == "sulphur") "sulphur" else "general")
  }
  morie_chemsc_score(hbonds, metals, lipo, rotatable, clashes, torsions,
                     smoothing, dg0, clash_slope, intra_coefficient,
                     coefficients, par)
}

#' One-line summary of the chemsc module
#'
#' @return A character scalar.
#' @export
morie_chemsc_cheatsheet <- function()
  paste0("chemsc: ChemScore empirical docking. smoothings ",
         paste(.CHEMSC_SMOOTHINGS, collapse = ", "),
         "; coefficients hbond -3.34, metal -6.03, lipo -0.117, ",
         "rot 2.56 (CCDC GOLD defaults)")
