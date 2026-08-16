# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of flexrd -- induced-fit docking with side-chain rotamers.
# Mirrors src/morie/fn/flexrd.py operation for operation, on the shared
# numerics in R/aaa_helpers_w3num.R.
#
# Rigid-receptor docking asks the wrong question. It takes a protein
# structure crystallised with one ligand, or with none, and asks which
# new ligand fits that exact shape -- when the thing being modelled is a
# protein that rearranges its side chains around whatever binds it. A
# ligand that would fit perfectly after a leucine rotates thirty degrees
# is thrown out for a clash that would not exist in reality.
#
# Sherman and colleagues' answer is a three-stage protocol.
#
#   STAGE ONE, SOFTEN AND DOCK. Shrink the van der Waals radii before
#   scoring. A softened receptor tolerates the near-clashes that a real
#   side chain would relieve by moving, so poses survive stage one that
#   a hard receptor would have discarded before the side chains ever got
#   a chance. The scale factor is the published mechanism of the stage.
#
#   STAGE TWO, REFINE THE SIDE CHAINS. For each surviving pose, rotate
#   the flexible residues' chi angles and keep what scores best. The
#   torsion is applied by Rodrigues' rotation about the bond axis, which
#   is exact and preserves every bond length in the side chain -- a
#   rotamer that stretched a bond would not be a rotamer.
#
#   STAGE THREE, RESCORE HARD. Score the refined complex at full radii.
#   This is the number comparable across poses, and it is reported
#   separately from the softened one, because a complex that looks good
#   only when softened is a complex that has not been shown to fit.
#
# TWO SEARCHES, BOTH HERE. "coordinate" optimises one residue at a time
# holding the others fixed, sweeping until nothing improves -- cheap,
# and it can stop at a local optimum where two side chains would have to
# move together. "exhaustive" tries every combination, which is the true
# optimum and is exponential in the number of chi angles, so it is for
# small cases and for checking that the cheap search did not go wrong.
#
# WHAT IS PUBLISHED AND WHAT IS A SETTING. The protocol and the
# softening are Sherman's. The chi grid defaults to the staggered
# positions -- minus sixty, sixty and one hundred and eighty degrees --
# which are elementary conformational analysis rather than a fitted
# library; a caller holding a real backbone-dependent rotamer library
# passes their own angles. The energy is a Lennard-Jones 12-6 with the
# radii the caller supplies, which is a functional form and not a force
# field.
#
# References
#   Sherman, W., Day, T., Jacobson, M.P., Friesner, R.A. and Farid, R.
#     (2006) "Novel procedure for modeling ligand/receptor induced fit
#     effects." Journal of Medicinal Chemistry 49(2), 534-553.
#     doi:10.1021/jm050540c.
#   Sherman, W., Beard, H.S. and Farid, R. (2006) "Use of an induced fit
#     receptor structure in virtual screening." Chemical Biology and
#     Drug Design 67(1), 83-84.
#   Jones, J.E. (1924) "On the determination of molecular fields. II."
#     Proceedings of the Royal Society A 106(738), 463-477.
#   Rodrigues, O. (1840) "Des lois geometriques qui regissent les
#     deplacements d'un systeme solide." Journal de Mathematiques Pures
#     et Appliquees 5, 380-440.

# The staggered torsions. Elementary conformational analysis, not a
# fitted rotamer library.
.flexrd_staggered <- c(-60, 60, 180)
.flexrd_searches <- c("coordinate", "exhaustive")

#' Rodrigues' rotation of a point about the axis from a to b
#'
#' Exact, and it preserves every distance to the axis -- which is what
#' makes a torsion a torsion rather than a distortion. A zero-length
#' axis has no direction to rotate about and is refused rather than
#' normalised into a division by zero.
#'
#' @param p The point to move.
#' @param a,b Two points defining the axis.
#' @param degrees The turn.
#' @return The rotated point.
#' @export
morie_flexrd_rotate <- function(p, a, b, degrees) {
  ax <- b - a
  n <- sqrt(.w3_csum(ax * ax))
  if (n == 0) stop("a torsion axis needs two distinct atoms")
  k <- ax / n
  v <- p - a
  t <- as.numeric(degrees) * pi / 180
  cc <- cos(t); ss <- sin(t)
  kv <- .w3_dot(k, v)
  cr <- c(k[2] * v[3] - k[3] * v[2], k[3] * v[1] - k[1] * v[3],
          k[1] * v[2] - k[2] * v[1])
  a + v * cc + cr * ss + k * kv * (1 - cc)
}

#' Turn one chi angle: rotate the atoms it moves, and only those
#'
#' The chi names the two atoms of the bond and the atoms distal to it.
#' Everything else in the receptor stays exactly where it was, which is
#' what makes the side chain flexible and the backbone not.
#'
#' @param coords The receptor coordinates, a list of triples.
#' @param chi A list with a, b and moves, all zero-based.
#' @param degrees The turn.
#' @return The receptor with that side chain moved.
#' @export
morie_flexrd_chi <- function(coords, chi, degrees) {
  out <- coords
  a <- out[[as.integer(chi$a) + 1L]]
  b <- out[[as.integer(chi$b) + 1L]]
  for (i in chi$moves)
    out[[as.integer(i) + 1L]] <-
      morie_flexrd_rotate(out[[as.integer(i) + 1L]], a, b, degrees)
  out
}

#' Lennard-Jones 12-6 between the ligand and the receptor
#'
#' The scale shrinks the summed radii, which is stage one's softening:
#' below one a contact has to be closer before it costs anything, so a
#' pose that a hard receptor would reject survives to the stage where
#' the side chains can move out of its way.
#'
#' Pairs beyond the cutoff are dropped. The cutoff is stated rather than
#' hidden because it makes the energy a sum over a finite neighbourhood,
#' which is what lets two arms of this package agree term for term.
#'
#' @param rec,lig Coordinate lists.
#' @param rec_r,lig_r The radii.
#' @param scale The softening.
#' @param epsilon The well depth.
#' @param cutoff The neighbourhood.
#' @return The energy.
#' @export
morie_flexrd_energy <- function(rec, lig, rec_r, lig_r, scale = 1,
                                epsilon = 1, cutoff = 8) {
  terms <- numeric(0)
  for (i in seq_along(lig)) for (j in seq_along(rec)) {
    d <- lig[[i]] - rec[[j]]
    r2 <- d[1] * d[1] + d[2] * d[2] + d[3] * d[3]
    if (r2 > cutoff * cutoff) next
    r <- sqrt(r2)
    if (r == 0)
      stop("two atoms are on top of each other, which is not a pose")
    sig <- (as.numeric(lig_r[i]) + as.numeric(rec_r[j])) *
      as.numeric(scale)
    q <- sig / r
    q6 <- q * q * q * q * q * q
    terms <- c(terms, 4 * as.numeric(epsilon) * (q6 * q6 - q6))
  }
  .w3_csum(terms)
}

#' .flexrd_grid
#'
#' A step of the flexrd_native implementation. Called by \code{morie_flexrd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param nchi A count; the body uses it as \code{seq_len(...)}.
#' @param angles See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.flexrd_grid <- function(nchi, angles) {
  out <- list(numeric(0))
  if (nchi > 0L) for (k in seq_len(nchi)) {
    nxt <- list()
    for (pre in out) for (a in angles)
      nxt[[length(nxt) + 1L]] <- c(pre, a)
    out <- nxt
  }
  out
}

#' Dock a ligand into a receptor whose side chains may move
#'
#' @param receptor A list with coords and radii, one per receptor atom.
#' @param ligand A list with coords and radii for a single pose, or
#'   poses for a list of candidate poses to try.
#' @param flex_residues One entry per movable chi angle: a and b name
#'   the bond it turns about and moves lists the atoms distal to it.
#' @param angles The chi values to try, as offsets from the input
#'   geometry. NULL is zero -- the structure as given -- together with
#'   the staggered positions, so the input rotamer is always in the
#'   search and flexibility can never score worse than rigidity.
#' @param soft Stage one's radius scale.
#' @param epsilon The well depth.
#' @param cutoff The neighbourhood.
#' @param search Either coordinate or exhaustive.
#' @param passes Sweeps of the coordinate search.
#' @param n_keep How many stage-one poses go forward.
#' @return A list with the chosen pose, the chosen chi angles, the
#'   softened and hard energies at each stage, and the refined receptor.
#' @export
morie_flexrd <- function(receptor, ligand, flex_residues, angles = NULL,
                         soft = 0.7, epsilon = 1, cutoff = 8,
                         search = "coordinate", passes = 3L,
                         n_keep = 3L) {
  if (!(search %in% .flexrd_searches))
    stop("the search is coordinate or exhaustive")
  rc <- lapply(receptor$coords, as.numeric)
  rr <- as.numeric(receptor$radii)
  if (length(rc) != length(rr)) stop("one radius per receptor atom")
  poses <- if (!is.null(ligand$poses))
    lapply(ligand$poses, function(p) lapply(p, as.numeric))
  else list(lapply(ligand$coords, as.numeric))
  lr <- as.numeric(ligand$radii)
  for (pose in poses) if (length(pose) != length(lr))
    stop("one radius per ligand atom")
  if (!length(poses))
    stop("a dock with no pose has nothing to score")
  chis <- flex_residues
  for (cc in chis) {
    if (is.null(cc$a) || is.null(cc$b) || is.null(cc$moves))
      stop("a chi needs a, b and moves")
    if (as.integer(cc$a) == as.integer(cc$b))
      stop("a torsion axis needs two distinct atoms")
  }
  if (is.null(angles)) angles <- c(0, .flexrd_staggered)
  angles <- as.numeric(angles)
  # Without zero the search could not return the structure it was given,
  # and "flexibility never hurts" would stop being true.
  if (!(0 %in% angles)) angles <- c(0, angles)

  soft_e <- vapply(poses, function(p)
    morie_flexrd_energy(rc, p, rr, lr, soft, epsilon, cutoff),
    numeric(1))
  ord <- order(soft_e, seq_along(poses))
  kept <- ord[seq_len(min(length(ord), max(1L, as.integer(n_keep))))]

  build <- function(chosen) {
    out <- rc
    if (length(chis)) for (k in seq_along(chis))
      if (chosen[k] != 0) out <- morie_flexrd_chi(out, chis[[k]],
                                                  chosen[k])
    out
  }

  best <- NULL
  for (pidx in kept) {
    pose <- poses[[pidx]]
    if (!length(chis)) {
      cand <- list(morie_flexrd_energy(rc, pose, rr, lr, 1, epsilon,
                                       cutoff),
                   pidx, numeric(0),
                   morie_flexrd_energy(rc, pose, rr, lr, soft, epsilon,
                                       cutoff), rc)
    } else if (search == "exhaustive") {
      bestc <- NULL
      for (combo in .flexrd_grid(length(chis), angles)) {
        rc2 <- build(combo)
        e <- morie_flexrd_energy(rc2, pose, rr, lr, soft, epsilon,
                                 cutoff)
        if (is.null(bestc) || e < bestc[[1]])
          bestc <- list(e, combo, rc2)
      }
      rc2 <- bestc[[3]]
      cand <- list(morie_flexrd_energy(rc2, pose, rr, lr, 1, epsilon,
                                       cutoff),
                   pidx, bestc[[2]], bestc[[1]], rc2)
    } else {
      chosen <- rep(0, length(chis))
      cur <- build(chosen)
      e <- morie_flexrd_energy(cur, pose, rr, lr, soft, epsilon, cutoff)
      for (it in seq_len(as.integer(passes))) {
        moved <- FALSE
        for (k in seq_along(chis)) for (a in angles) {
          trial <- chosen; trial[k] <- a
          rc2 <- build(trial)
          e2 <- morie_flexrd_energy(rc2, pose, rr, lr, soft, epsilon,
                                    cutoff)
          if (e2 < e) { e <- e2; chosen <- trial; cur <- rc2
                        moved <- TRUE }
        }
        if (!moved) break
      }
      cand <- list(morie_flexrd_energy(cur, pose, rr, lr, 1, epsilon,
                                       cutoff),
                   pidx, chosen, e, cur)
    }
    if (is.null(best) || cand[[1]] < best[[1]]) best <- cand
  }

  e_hard <- best[[1]]; pidx <- best[[2]]; chosen <- best[[3]]
  e_soft <- best[[4]]; refined <- best[[5]]
  rigid_soft <- morie_flexrd_energy(rc, poses[[pidx]], rr, lr, soft,
                                    epsilon, cutoff)
  rigid_hard <- morie_flexrd_energy(rc, poses[[pidx]], rr, lr, 1, epsilon,
                                    cutoff)
  list(pose_index = pidx - 1L, pose = poses[[pidx]], chi = chosen,
       receptor = refined, energy = e_hard, energy_soft = e_soft,
       rigid_energy = rigid_hard, rigid_energy_soft = rigid_soft,
       gain = rigid_hard - e_hard, stage1 = soft_e,
       stage1_order = ord - 1L, kept = kept - 1L, n_pose = length(poses),
       n_chi = length(chis), n_receptor = length(rc),
       n_ligand = length(lr), soft = as.numeric(soft),
       cutoff = as.numeric(cutoff), epsilon = as.numeric(epsilon),
       angles = angles, search = search,
       method = paste0("induced-fit docking: soften, refine side ",
                       "chains, rescore hard"))
}

#' One-line summary of the flexrd module
#'
#' @return A character scalar.
#' @export
morie_flexrd_cheatsheet <- function()
  paste0("flexrd: induced-fit docking. Soften the radii and rank poses, ",
         "turn the side-chain chi angles by Rodrigues rotation, rescore ",
         "at full radii")
