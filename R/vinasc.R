# SPDX-License-Identifier: AGPL-3.0-or-later
#' Score a docked pose with the Vina free-energy function
#'
#' Five heavy-atom pair terms, all functions of the surface distance
#' \code{d = r - R_i - R_j} rather than the centre distance. The
#' rotatable-bond term divides rather than adds, penalising the entropy
#' a flexible ligand gives up on binding.
#'
#' @param receptor Matrix, rows \code{[x, y, z, radius, type]}; type 1
#'   hydrophobic, 2 hydrogen-bond donor or acceptor, 0 otherwise.
#' @param ligand_pose Same layout for the ligand pose.
#' @param n_rot Active rotatable bonds between heavy atoms in the ligand.
#' @return List with \code{estimate}, \code{c_inter} and the five
#'   unweighted term sums.
#' @references Trott, O. & Olson, A. J. (2010). AutoDock Vina. Journal
#'   of Computational Chemistry 31:455-461. Weights are table 1;
#'   equation (9) is \code{g(c_inter) = c_inter / (1 + w N_rot)}.
#' @export
Vinasc <- function(receptor, ligand_pose, n_rot = 0) {
  R <- as.matrix(receptor); L <- as.matrix(ligand_pose)
  g1 <- 0; g2 <- 0; rep_ <- 0; hyd <- 0; hb <- 0
  for (i in seq_len(nrow(R))) {
    for (j in seq_len(nrow(L))) {
      r <- sqrt(sum((R[i, 1:3] - L[j, 1:3])^2))
      if (r > 8) next
      d <- r - R[i, 4] - L[j, 4]
      g1 <- g1 + exp(-((d / 0.5)^2))
      g2 <- g2 + exp(-(((d - 3) / 2)^2))
      if (d < 0) rep_ <- rep_ + d * d
      if (ncol(R) > 4 && ncol(L) > 4 && R[i, 5] == 1 && L[j, 5] == 1) {
        if (d < 0.5) hyd <- hyd + 1 else if (d < 1.5) hyd <- hyd + (1.5 - d)
      }
      if (ncol(R) > 4 && ncol(L) > 4 && R[i, 5] == 2 && L[j, 5] == 2) {
        if (d < -0.7) hb <- hb + 1 else if (d < 0) hb <- hb + (-d / 0.7)
      }
    }
  }
  c_inter <- -0.0356 * g1 - 0.00516 * g2 + 0.840 * rep_ - 0.0351 * hyd - 0.587 * hb
  .t1_result(estimate = c_inter / (1 + 0.0585 * as.numeric(n_rot)),
             c_inter = c_inter, gauss1 = g1, gauss2 = g2, repulsion = rep_,
             hydrophobic = hyd, hbond = hb,
             method = "AutoDock Vina scoring function")
}
