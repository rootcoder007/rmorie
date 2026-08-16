# SPDX-License-Identifier: AGPL-3.0-or-later

#' DBSCAN density-based clustering (R parity)
#'
#' Native implementation; the metric is honoured and the reported core points are the points meeting the min_samples rule.
#'
#' @param x Numeric matrix.
#' @param eps Neighbourhood radius.
#' @param min_samples Minimum points in eps-neighbourhood for a core point.
#' @param metric Distance metric (passed to dbscan).
#' @return Named list: estimate, labels, n_clusters, n_noise,
#'   core_sample_indices, eps, min_samples, n, method.
#' @examplesIf requireNamespace("dbscan", quietly = TRUE)
#' morie_dbscan_clustering(x = rnorm(50))
#' @export
#' Internal: native DBSCAN
#'
#' Density-based clustering, written out rather than delegated, so the
#' metric the caller asks for is the metric used and the core points
#' reported are the points that satisfy the min_samples rule. The
#' expansion order matches the Python arm exactly -- clusters are
#' seeded from core points in index order and the seed list is used as
#' a stack -- so the two arms agree on the cluster NUMBERING as well as
#' the partition.
#'
#' @param x A numeric matrix, one row per point.
#' @param eps Neighbourhood radius.
#' @param min_samples Points within eps needed to make a point a core
#'   point, counting the point itself.
#' @param metric One of \code{"euclidean"}, \code{"manhattan"},
#'   \code{"chebyshev"}.
#' @return A list with \code{labels} (0-based cluster ids, -1 for
#'   noise) and \code{core} (logical, one per point).
#' @keywords internal
.morie_dbscan_native <- function(x, eps = 0.5, min_samples = 5L,
                                 metric = "euclidean") {
  metrics <- c("euclidean", "manhattan", "chebyshev")
  if (!metric %in% metrics) {
    stop("unknown metric '", metric, "'; DBSCAN supports ",
         paste(metrics, collapse = ", "), ".", call. = FALSE)
  }
  x <- as.matrix(x)
  n <- nrow(x)
  min_samples <- as.integer(min_samples)
  if (n == 0L) return(list(labels = integer(0), core = logical(0)))

  dist_to <- function(i) {
    dif <- x - matrix(x[i, ], nrow = n, ncol = ncol(x), byrow = TRUE)
    switch(metric,
           manhattan = rowSums(abs(dif)),
           chebyshev = apply(abs(dif), 1L, max),
           sqrt(rowSums(dif * dif)))
  }

  nbrs <- vector("list", n)
  for (i in seq_len(n)) nbrs[[i]] <- which(dist_to(i) <= eps)
  core <- vapply(nbrs, function(v) length(v) >= min_samples, logical(1))

  labels <- rep(NA_integer_, n)
  cid <- 0L
  for (i in seq_len(n)) {
    if (!is.na(labels[i]) || !core[i]) next
    labels[i] <- cid
    seeds <- setdiff(nbrs[[i]], i)
    while (length(seeds)) {
      j <- seeds[length(seeds)]
      seeds <- seeds[-length(seeds)]
      if (!is.na(labels[j]) && labels[j] != -1L) next
      labels[j] <- cid
      if (core[j]) {
        add <- nbrs[[j]]
        add <- add[is.na(labels[add]) | labels[add] == -1L]
        seeds <- c(seeds, add)
      }
    }
    cid <- cid + 1L
  }
  labels[is.na(labels)] <- -1L
  list(labels = as.integer(labels), core = core)
}

morie_dbscan_clustering <- function(x, eps = 0.5, min_samples = 5L,
                                    metric = "euclidean") {
  if (is.null(dim(x))) x <- matrix(x, ncol = 1)
  x <- as.matrix(x)
  fit <- .morie_dbscan_native(x, eps = eps, min_samples = min_samples,
                              metric = metric)
  labels <- fit$labels
  n_clusters <- length(unique(labels[labels >= 0L]))
  list(
    estimate            = as.integer(n_clusters),
    labels              = as.integer(labels),
    n_clusters          = as.integer(n_clusters),
    n_noise             = as.integer(sum(labels == -1L)),
    # Core points, not merely clustered points: the two differ at every
    # cluster border, and the previous expression reported the latter.
    core_sample_indices = as.integer(which(fit$core) - 1L),
    eps                 = as.numeric(eps),
    min_samples         = as.integer(min_samples),
    metric              = metric,
    n                   = nrow(x),
    method              = "DBSCAN (Ester et al. 1996)"
  )
}
