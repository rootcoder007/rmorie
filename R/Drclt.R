# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cluster-robust inference for the doubly robust DiD estimator
#'
#' The point estimate is the doubly robust moment of Sant'Anna and Zhao
#' (2020), equation (2.6).  The influence function is summed within
#' cluster before it is squared,
#' \code{V = a (1/n^2) sum_c (sum_{i in c} psi_i)^2} with the finite
#' cluster correction \code{a = [G/(G-1)][(n-1)/(n-k)]}.  With one
#' observation per cluster the sum collapses and \code{V} is the
#' independent variance times \code{a}.
#'
#' @param y Outcome change, one entry per unit.
#' @param D Binary treatment indicator.
#' @param X Optional baseline covariates.
#' @param cluster Cluster label per unit; \code{NULL} gives one unit per
#'   cluster.
#' @return List with \code{estimate}, \code{se}, \code{se_iid},
#'   \code{vif}, \code{n_clusters}, \code{dof_adj}, \code{k}, \code{n}.
#' @references Bertrand, M., Duflo, E. and Mullainathan, S. (2004).
#'   Quarterly Journal of Economics 119(1), 249-275.  Sant'Anna, P. H. C.
#'   and Zhao, J. (2020). Journal of Econometrics 219(1), 101-122.
#' @export
Drclt <- function(y, D, X = NULL, cluster = NULL) {
  yv <- .s03vec(y); dv <- .s03vec(D); n <- length(yv)
  if (n == 0L) stop("Drclt: empty input, y has no observations")
  if (length(dv) != n) stop("Drclt: y and D must have the same length")
  cl <- if (is.null(cluster)) as.character(seq_len(n)) else as.character(cluster)
  if (length(cl) != n) stop("Drclt: cluster must have the same length as y")
  fit <- .s03drdid(yv, dv, X)
  psi <- fit$inf
  labels <- unique(cl); G <- length(labels)
  nk <- 1L + if (is.null(X)) 0L else ncol(.s03mat(X))
  adj <- if (G < 2L || n <= nk) 1 else (G / (G - 1)) * ((n - 1) / (n - nk))
  v <- 0
  for (cc in labels) { s <- sum(psi[cl == cc]); v <- v + s * s }
  v_cr <- adj * v / (n * n)
  v_iid <- sum(psi * psi) / (n * n)
  .t1_result(estimate = fit$tau, se = sqrt(v_cr), se_iid = sqrt(v_iid),
             vif = if (v_iid > 0) v_cr / v_iid else NaN,
             n_clusters = G, dof_adj = adj, k = nk, n = n,
             method = "Cluster-robust DR-DiD")
}
