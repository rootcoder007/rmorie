# SPDX-License-Identifier: AGPL-3.0-or-later
#' Ask whether the direct evidence on one comparison agrees with the rest
#'
#' A network estimate is a weighted blend of what the head-to-head trials
#' say and what the rest of the network implies. If those two disagree the
#' blend is meaningless, and no global fit statistic will show it, because
#' the blend absorbs the disagreement. Splitting the node computes them
#' separately and tests the difference, which is the only way to localise
#' inconsistency to an edge.
#'
#' Formula: \code{direct} is the inverse-variance pool of the studies
#' making that comparison; \code{indirect} is the consistency-model
#' estimate of the same contrast from all the other studies; the test is
#' \code{z = (direct - indirect)/sqrt(v_dir + v_ind)} -- Dias et al.
#' (2010) Section 3.
#'
#' @param yi Contrast estimates.
#' @param vi Their sampling variances, strictly positive.
#' @param design Baseline and comparator treatment labels, n by 2.
#' @param edge The comparison to split, as (baseline, comparator).
#' @return List with \code{direct}, \code{v_direct}, \code{indirect},
#'   \code{v_indirect}, \code{diff}, \code{z}, \code{p}, \code{k_direct},
#'   \code{k_indirect}.
#' @references Dias, S., Welton, N. J., Caldwell, D. M. and Ades, A. E.
#'   (2010). Statistics in Medicine 29(7-8):932-944. \doi{10.1002/sim.3767}.
#' @export
Manh2h <- function(yi, vi, design, edge) {
  y <- as.numeric(yi); v <- as.numeric(vi); n <- length(y)
  if (n == 0L) stop("no studies")
  if (length(v) != n) stop("yi and vi must have equal length")
  if (any(v <= 0)) stop("sampling variances must be strictly positive")
  D <- as.matrix(design)
  if (nrow(D) != n || ncol(D) != 2L) stop("design must be n by 2")
  e <- as.integer(edge)
  if (length(e) != 2L || e[1] == e[2])
    stop("edge must be two distinct treatment labels")
  sgn <- rep(0, n)
  sgn[D[, 1] == e[1] & D[, 2] == e[2]] <- 1
  sgn[D[, 1] == e[2] & D[, 2] == e[1]] <- -1
  di <- which(sgn != 0); rest <- which(sgn == 0)
  if (!length(di)) stop("no study makes that comparison directly")
  if (!length(rest))
    stop("no indirect evidence remains once the edge is split")
  sw <- sum(1 / v[di])
  direct <- sum(sgn[di] * y[di] / v[di]) / sw
  v_dir <- 1 / sw
  nd <- .ma_net_design(D[rest, , drop = FALSE])
  Xr <- nd$X; treats <- nd$treats; T <- nd$T
  if (!(e[1] %in% treats) || !(e[2] %in% treats))
    stop("the split edge is disconnected from the rest")
  p <- T - 1L
  w <- 1 / v[rest]; yr <- y[rest]
  fit <- .ma_wls(Xr, yr, w)
  beta <- fit$beta; cv <- fit$cov
  pos <- match(e, treats)
  cvec <- numeric(p)
  if (pos[1] > 1L) cvec[pos[1] - 1L] <- cvec[pos[1] - 1L] - 1
  if (pos[2] > 1L) cvec[pos[2] - 1L] <- cvec[pos[2] - 1L] + 1
  indirect <- sum(cvec * beta)
  v_ind <- as.numeric(t(cvec) %*% cv %*% cvec)
  diff <- direct - indirect
  sd <- sqrt(v_dir + v_ind)
  z <- if (sd > 0) diff / sd else NA_real_
  pv <- if (sd > 0) 2 * (1 - .s03pnorm(abs(z))) else NA_real_
  .t1_result(direct = direct, v_direct = v_dir, indirect = indirect,
             v_indirect = v_ind, diff = diff, z = z, p = pv,
             k_direct = length(di), k_indirect = length(rest),
             method = "Node-splitting inconsistency check")
}
