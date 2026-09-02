# SPDX-License-Identifier: AGPL-3.0-or-later
#' One coupling that has to satisfy the features and the structure at once
#'
#' A labelled graph carries two kinds of information, and the two usual
#' distances each throw one away: Wasserstein on the labels ignores the
#' edges, Gromov-Wasserstein on the edges ignores the labels. Fusing them
#' forces a single plan to explain both, and \code{alpha} says which
#' evidence dominates. Solved by conditional gradient with an exact
#' transport step, so no entropic blur enters the plan.
#'
#' Formula: \code{min_T (1-alpha) <T, M> + alpha sum_ijkl |Cx_ik -
#' Cy_jl|^2 T_ij T_kl} -- Vayer et al. (2020) eq. (3). The linearised cost
#' is \code{(1-alpha) M - 4 alpha Cx T Cy} and the step is
#' \code{gamma = 2/(k+2)}.
#'
#' @param M Feature cost between the two vertex sets, n by m.
#' @param Cx Structure matrix of the first object, n by n.
#' @param Cy Structure matrix of the second object, m by m.
#' @param a,b Marginals.
#' @param alpha Trade-off in \[0, 1\].
#' @param max_iter Conditional-gradient steps.
#' @return List with \code{T}, \code{cost}, \code{wass_part},
#'   \code{gromov_part}, \code{n}, \code{m}, \code{iters}.
#' @references Vayer, T., Chapel, L., Flamary, R., Tavenard, R. and
#'   Courty, N. (2020). Algorithms 13(9):212. \doi{10.3390/a13090212}.
#' @export
Otfgw <- function(M, Cx, Cy, a, b, alpha = 0.5, max_iter = 20) {
  Mm <- as.matrix(M); A <- as.matrix(Cx); B <- as.matrix(Cy)
  aa <- .ot_hist(a); bb <- .ot_hist(b)
  n <- length(aa); m <- length(bb)
  if (nrow(Mm) != n || ncol(Mm) != m) stop("M must be n by m")
  if (nrow(A) != n || nrow(B) != m)
    stop("structure matrices must match the marginals")
  al <- as.numeric(alpha)
  if (al < 0 || al > 1) stop("alpha must lie in [0, 1]")
  t1 <- sum(outer(aa, aa) * A^2); t3 <- sum(outer(bb, bb) * B^2)
  T <- outer(aa, bb)
  it <- as.integer(max_iter)
  for (k in seq_len(it) - 1L) {
    CT <- A %*% T %*% B
    G <- (1 - al) * Mm - 4 * al * CT
    Th <- .ot_emd(aa, bb, G)$T
    gam <- 2 / (k + 2)
    T <- (1 - gam) * T + gam * Th
  }
  gw <- t1 + t3 - 2 * sum((A %*% T %*% B) * T)
  if (gw < 0) gw <- 0
  wpart <- sum(T * Mm)
  .t1_result(T = T, cost = (1 - al) * wpart + al * gw, wass_part = wpart,
             gromov_part = gw, n = n, m = m, iters = it,
             method = "Fused Gromov-Wasserstein distance")
}
