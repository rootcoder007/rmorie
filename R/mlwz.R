# SPDX-License-Identifier: AGPL-3.0-or-later
#' Standardise a level-1 variable inside each cluster
#'
#' \code{z_ij = (x_ij - xbar_j) / sd_j}.
#'
#' The centring step is not recomputed here: it is \code{Cwcenter} (module \code{cwcm}), the
#' single implementation of group-mean centring in this package. What
#' this module adds is the division by the within-cluster standard
#' deviation, which is what separates a z-score from a centred score:
#' centring removes the cluster's location, scaling also removes its
#' spread, so clusters with different variability become comparable and
#' every cluster ends with mean 0 and standard deviation 1 by
#' construction.
#'
#' That last property is also the cost. Between-cluster differences in
#' both level and spread are destroyed, so a cluster-level predictor
#' cannot be recovered from \code{z}; the cluster means and standard
#' deviations are returned alongside so they can be reintroduced as
#' level-2 predictors.
#'
#' A cluster with a single member, or with no variation, has
#' \code{sd_j = 0} and no defined z-score. Those positions are returned
#' as \code{NaN}, not 0: a unit that cannot be placed relative to its
#' cluster is not average, it is unmeasured.
#'
#' @param y The level-1 variable.
#' @param cluster Cluster identifier per observation.
#' @param ddof Denominator correction for the within-cluster SD; 1 gives
#'   the sample SD, 0 the population SD.
#' @return List with estimate (the z vector), z, cluster_means,
#'   cluster_sds, cluster_ids, n_undefined, n_clusters, n.
#' @references Raudenbush and Bryk (2002), Hierarchical Linear Models,
#'   2nd ed., Sage, ch. 5; Enders and Tofighi (2007), Psychological
#'   Methods 12(2), 121-138, \doi{10.1037/1082-989X.12.2.121}. Neither
#'   source was in the local corpus; the transformation is arithmetic and
#'   is implemented in its standard published form, exactly as printed
#'   above.
#' @export
Mlwz <- function(y, cluster, ddof = 1) {
  v <- .t1_vec(y); n <- length(v)
  if (n == 0L) stop("y is empty")
  g <- as.character(unlist(cluster))
  if (length(g) != n) stop("y and cluster must have the same length")
  dd <- as.integer(ddof)
  if (!dd %in% c(0L, 1L)) stop("ddof must be 0 or 1")
  base <- Cwcenter(v, g)
  cent <- as.numeric(base$centered)
  ids <- as.character(base$cluster_ids)
  means <- as.numeric(base$cluster_means)
  sds <- numeric(length(ids))
  for (j in seq_along(ids)) {
    idx <- which(g == ids[j])
    k <- length(idx) - dd
    sds[j] <- if (k <= 0) NaN else sqrt(sum(cent[idx]^2) / k)
  }
  s <- sds[match(g, ids)]
  bad <- is.na(s) | s <= 0
  z <- ifelse(bad, NaN, cent / s)
  .t1_result(estimate = z, z = z, cluster_means = means, cluster_sds = sds,
             cluster_ids = ids, n_undefined = sum(bad),
             n_clusters = length(ids), n = n,
             method = "Within-cluster standardisation (cluster z-score)")
}
