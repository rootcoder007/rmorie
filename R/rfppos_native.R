# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of rfppos -- the reactive pose filter for covalent docking.
# Mirrors src/morie/fn/rfppos.py operation for operation, on the SMILES
# parser in R/avalon_native.R and the shared numerics in
# R/aaa_helpers_w3num.R.
#
# A covalent inhibitor binds twice. First it docks, held by the same
# shape and charge complementarity as any other ligand; then its warhead
# reacts with a cysteine and the complex stops being reversible. Docking
# software scores the first step. It does not, on its own, check the
# second -- and a pose can score beautifully while presenting the
# warhead to the wrong side of the cysteine, where no chemistry is
# possible.
#
# This is the filter for the second step, and it is pure geometry.
#
#   THE DISTANCE from the cysteine's sulfur to the electrophilic carbon.
#   The forming carbon-sulfur bond is about 1.8 angstroms; a pose whose
#   warhead is six angstroms away is not about to react no matter what
#   it scored.
#
#   THE ATTACK ANGLE at the electrophilic carbon. A nucleophile does not
#   approach from any direction. Buergi and Dunitz established from
#   crystal structures that attack on a carbonyl comes in at about 107
#   degrees to the carbon-oxygen axis, a property of the orbitals rather
#   than of the particular molecule. Michael addition to a conjugated
#   alkene is different: the sulfur attacks the beta carbon roughly
#   perpendicular to the double bond, so the two chemistries need two
#   criteria and this module has both as named routes rather than one
#   blurred average.
#
#   THE DIHEDRAL about the forming bond, reported for both routes,
#   because it distinguishes an approach that is merely at the right
#   angle from one that is at the right angle on the right FACE.
#
# WHAT IS PUBLISHED AND WHAT IS A SETTING. The Buergi-Dunitz angle is
# published and is the default. Every tolerance is a parameter with a
# stated default, because those are decisions about how permissive a
# screen should be and they belong to whoever runs it.
#
# The warhead is FOUND, not assumed: the module reads the bond orders
# and identifies the electrophilic carbon itself, and if there is no
# warhead it says so rather than measuring an angle at an arbitrary
# atom.
#
# References
#   Buergi, H.B., Dunitz, J.D. and Shefter, E. (1973) "Geometrical
#     reaction coordinates. II. Nucleophilic addition to a carbonyl
#     group." Journal of the American Chemical Society 95(15),
#     5065-5067. doi:10.1021/ja00796a058.
#   Bianco, G., Forli, S., Goodsell, D.S. and Olson, A.J. (2016)
#     "Covalent docking using autodock." Protein Science 25(1),
#     295-301. doi:10.1002/pro.2733.
#   Zhu, K. et al. (2014) "Docking covalent inhibitors: a parameter free
#     approach to pose prediction and scoring." Journal of Chemical
#     Information and Modeling 54(7), 1932-1940. CovDock, which the
#     ledger entry names.

# Buergi and Dunitz's approach angle for nucleophilic addition to a
# carbonyl, in degrees. A property of the orbitals, not a fitted value.
.rfppos_burgi_dunitz <- 107
.rfppos_modes <- c("burgi_dunitz", "michael")

#' .rfppos_cross
#'
#' A step of the rfppos_native implementation. Called by \code{morie_rfppos_dihedral}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; indexed elementwise.
#' @param b A vector; indexed elementwise.
#' @return A vector, from \code{c}.
#' @export
.rfppos_cross <- function(a, b)
  c(a[2] * b[3] - a[3] * b[2], a[3] * b[1] - a[1] * b[3],
    a[1] * b[2] - a[2] * b[1])

#' .rfppos_norm
#'
#' A step of the rfppos_native implementation. Called by \code{morie_rfppos_angle}, \code{morie_rfppos_dihedral}, \code{morie_rfppos_distance}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.rfppos_norm <- function(a) sqrt(.w3_csum(a * a))

#' The straight-line distance between two points
#'
#' @param a,b Coordinate triples.
#' @return A numeric scalar.
#' @export
morie_rfppos_distance <- function(a, b) .rfppos_norm(a - b)

#' The angle at b, in degrees, subtended by a and c
#'
#' The cosine is clamped into its own range before the arc cosine: two
#' nearly parallel vectors can produce a ratio a hair outside minus one
#' to one purely by rounding, and an unclamped arc cosine turns that
#' into a domain error rather than the zero degrees it obviously is.
#'
#' @param a,b,c Coordinate triples; b is the vertex.
#' @return Degrees.
#' @export
morie_rfppos_angle <- function(a, b, c) {
  u <- a - b; v <- c - b
  nu <- .rfppos_norm(u); nv <- .rfppos_norm(v)
  if (nu == 0 || nv == 0) stop("an angle needs three distinct points")
  t <- .w3_dot(u, v) / (nu * nv)
  if (t > 1) t <- 1
  if (t < -1) t <- -1
  acos(t) * 180 / pi
}

#' The torsion about the b-c axis, in degrees, signed
#'
#' Computed from the two plane normals with an arc tangent of two
#' arguments rather than an arc cosine, so the sign survives -- and the
#' sign is the whole point here, because it is what says which FACE the
#' nucleophile is approaching from.
#'
#' @param a,b,c,d Four coordinate triples.
#' @return Degrees, signed.
#' @export
morie_rfppos_dihedral <- function(a, b, c, d) {
  b1 <- b - a; b2 <- c - b; b3 <- d - c
  n2 <- .rfppos_norm(b2)
  if (n2 == 0) stop("a torsion needs a defined axis")
  u <- b2 / n2
  n1 <- .rfppos_cross(b1, b2)
  n3 <- .rfppos_cross(b2, b3)
  x <- .w3_dot(n1, n3)
  y <- .w3_dot(.rfppos_cross(n1, n3), u) * -1
  atan2(y, x) * 180 / pi
}

#' Locate the electrophilic carbon and the atom that orients it
#'
#' For the Buergi-Dunitz route: a carbon holding a double bond to oxygen
#' or nitrogen, or a triple bond to nitrogen. The electrophile is that
#' carbon and the reference is the heteroatom, because the approach
#' angle is measured to the carbon-heteroatom axis.
#'
#' For the Michael route: a carbon-carbon double bond with one end
#' attached to a carbonyl carbon. The electrophile is the FAR end -- the
#' beta carbon, which is where the sulfur adds -- and the reference is
#' the alpha carbon it is doubly bonded to. Getting these two the wrong
#' way round would measure a real angle at the wrong atom.
#'
#' @param smiles The ligand.
#' @param mode Either burgi_dunitz or michael.
#' @return A zero-based triple of electrophile, reference and torsion
#'   atom, or NULL when the molecule carries no such warhead.
#' @export
morie_rfppos_warhead <- function(smiles, mode = "burgi_dunitz") {
  if (!(mode %in% .rfppos_modes))
    stop("the mode is burgi_dunitz or michael")
  g <- morie_avalon_parse(smiles)
  el <- g$el; arom <- g$arom; n <- length(el)
  adj <- .avalon_adj(n, g$bonds)
  if (mode == "burgi_dunitz") {
    for (i in seq_len(n)) {
      if (el[i] != "C" || arom[i] == 1L) next
      for (e in adj[[i]]) {
        v <- e[1]; o <- e[2]
        if ((o == 2 && el[v + 1L] %in% c("O", "N")) ||
            (o == 3 && el[v + 1L] == "N")) {
          third <- NULL
          for (e2 in adj[[i]]) if (e2[1] != v) third <- e2[1]
          if (is.null(third)) next
          return(c(i - 1L, v, third))
        }
      }
    }
    return(NULL)
  }
  carbonyl <- rep(FALSE, n)
  for (i in seq_len(n)) {
    if (el[i] != "C") next
    for (e in adj[[i]])
      if (e[2] == 2 && el[e[1] + 1L] == "O") carbonyl[i] <- TRUE
  }
  for (bd in g$bonds) {
    if (bd[3] != 2 || el[bd[1] + 1L] != "C" || el[bd[2] + 1L] != "C")
      next
    for (pair in list(c(bd[1], bd[2]), c(bd[2], bd[1]))) {
      alpha <- pair[1]; beta <- pair[2]
      for (e in adj[[alpha + 1L]])
        if (e[1] != beta && carbonyl[e[1] + 1L])
          return(c(beta, alpha, e[1]))
    }
  }
  NULL
}

#' Does this pose present its warhead so the cysteine can attack?
#'
#' @param pose A list with \code{smiles} for the ligand and
#'   \code{coords}, one triple per atom in the order the SMILES lists
#'   them.
#' @param cys_residue A list with \code{SG} for the sulfur and
#'   \code{CB} for the beta carbon if the torsion is wanted.
#' @param mode Which chemistry, and therefore which ideal angle.
#' @param d_min,d_max The window the forming bond must fall in. The
#'   default upper bound is a near-attack conformation, not a bond.
#' @param ideal The approach angle. NULL takes 107 degrees for the
#'   Buergi-Dunitz route and 90 for the Michael route, which is
#'   perpendicular to the alkene.
#' @param angle_tol How far off the ideal still counts. A setting, not a
#'   constant.
#' @param warhead The electrophile, reference and torsion atoms, if the
#'   caller would rather name them than have them found.
#' @return A list with the measured geometry, each criterion separately,
#'   and whether the pose passes all of them.
#' @export
morie_rfppos <- function(pose, cys_residue, mode = "burgi_dunitz",
                         d_min = 1.5, d_max = 3.5, ideal = NULL,
                         angle_tol = 15, warhead = NULL) {
  if (!(mode %in% .rfppos_modes))
    stop("the mode is burgi_dunitz or michael")
  smiles <- pose$smiles
  coords <- lapply(pose$coords, as.numeric)
  g <- morie_avalon_parse(smiles)
  if (length(coords) != length(g$el))
    stop("one coordinate triple per atom, in the order the SMILES ",
         "lists them")
  sg <- as.numeric(cys_residue$SG)
  cb <- if (is.null(cys_residue$CB)) NULL else as.numeric(cys_residue$CB)

  if (is.null(warhead)) warhead <- morie_rfppos_warhead(smiles, mode)
  if (is.null(warhead))
    return(list(
      passes = FALSE,
      reason = sprintf(paste0("the ligand carries no %s warhead: there ",
                              "is no electrophilic carbon for the ",
                              "cysteine to attack, so there is no pose ",
                              "geometry to judge"), mode),
      electrophile = NULL, reference = NULL, torsion_atom = NULL,
      distance = NULL, angle = NULL, angle_error = NULL,
      dihedral = NULL, warhead_torsion = NULL,
      distance_ok = FALSE, angle_ok = FALSE,
      d_min = as.numeric(d_min), d_max = as.numeric(d_max),
      angle_tol = as.numeric(angle_tol), ideal = NULL, mode = mode,
      n_atoms = length(g$el),
      method = "covalent near-attack geometry filter"))
  e <- warhead[1]; r <- warhead[2]; t <- warhead[3]
  if (is.null(ideal))
    ideal <- if (mode == "burgi_dunitz") .rfppos_burgi_dunitz else 90
  ideal <- as.numeric(ideal)

  d <- morie_rfppos_distance(sg, coords[[e + 1L]])
  th <- morie_rfppos_angle(sg, coords[[e + 1L]], coords[[r + 1L]])
  di <- if (is.null(cb)) NULL else
    morie_rfppos_dihedral(cb, sg, coords[[e + 1L]], coords[[r + 1L]])
  tor <- morie_rfppos_dihedral(sg, coords[[e + 1L]], coords[[r + 1L]],
                               coords[[t + 1L]])

  dok <- d >= d_min && d <= d_max
  aok <- abs(th - ideal) <= angle_tol
  list(passes = dok && aok,
       reason = if (dok && aok) "" else
         paste0("the warhead is ",
                if (!dok) "too far or too close" else "at the wrong angle"),
       electrophile = e, reference = r, torsion_atom = t,
       distance = d, angle = th, angle_error = th - ideal,
       dihedral = di, warhead_torsion = tor,
       distance_ok = dok, angle_ok = aok, ideal = ideal,
       d_min = as.numeric(d_min), d_max = as.numeric(d_max),
       angle_tol = as.numeric(angle_tol), mode = mode,
       n_atoms = length(g$el),
       method = "covalent near-attack geometry filter")
}

#' One-line summary of the rfppos module
#'
#' @return A character scalar.
#' @export
morie_rfppos_cheatsheet <- function()
  paste0("rfppos: covalent pose filter. Sulfur-to-electrophile ",
         "distance, Buergi-Dunitz or perpendicular attack angle, and ",
         "the torsion; the warhead is found from the bond orders")
