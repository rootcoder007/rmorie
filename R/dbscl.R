# SPDX-License-Identifier: AGPL-3.0-or-later

#' .morie_dbscan_native
#'
#' Internal helper in dbscl.R; see the file header for
#' the source the module follows.
#'
#' @param x A matrix; indexed by row and column.
#' @param eps See Usage. Defaults to \code{0.5}.
#' @param min_samples See Usage. Defaults to \code{5L}.
#' @param metric See Usage. Defaults to \code{"euclidean"}.
#' @return A list with \code{labels}, \code{core}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .morie_dbscan_native(x = x)
#' res
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

  # within eps as dbscan::dbscan decides it (its ANN kd-tree search): squared Euclidean distance
  # summed one dimension at a time in double precision and compared with eps * eps; the
  # Manhattan and Chebyshev distances are accumulated the same way
  dist_to <- function(i) {
    .morie_dbscan_within(lapply(seq_len(ncol(x)), function(j) x[i, j] - x[, j]), eps, metric)
  }

  nbrs <- if (n > 300L && ncol(x) <= 3L && is.finite(eps) && eps > 0 && all(is.finite(x))) {
    .morie_dbscan_grid_nbrs(x, eps, metric)
  } else {
    lapply(seq_len(n), function(i) which(dist_to(i)))
  }
  core <- vapply(nbrs, function(v) length(v) >= min_samples, logical(1))

  # Each cluster grows from its first core point in index order. A point is labelled when it is
  # first reached and queued once, so the work is linear in the neighbour lists; every unlabelled
  # point reachable from the cluster's core points gets its id whatever the visiting order, as in
  # dbscan::dbscan (a border point keeps the first cluster that reaches it).
  labels <- rep(NA_integer_, n)
  queue <- integer(n)
  cid <- 0L
  for (i in seq_len(n)) {
    if (!is.na(labels[i]) || !core[i]) next
    labels[i] <- cid
    head <- 1L
    tail <- 1L
    queue[1L] <- i
    while (head <= tail) {
      j <- queue[head]
      head <- head + 1L
      if (!core[j]) next
      nb <- nbrs[[j]]
      nb <- nb[is.na(labels[nb])]
      if (length(nb)) {
        labels[nb] <- cid
        queue[tail + seq_along(nb)] <- nb
        tail <- tail + length(nb)
      }
    }
    cid <- cid + 1L
  }
  labels[is.na(labels)] <- -1L
  list(labels = as.integer(labels), core = core)
}

#' DBSCAN density-based clustering (R parity)
#'
#' Native implementation: the metric the caller asks for is the metric
#' used, and the core points reported are the points that meet the
#' min_samples rule -- not every clustered point, which is what the
#' previous wrapper reported.
#'
#' @param x Numeric matrix, one row per point (a vector is treated as
#'   one column).
#' @param eps Neighbourhood radius.
#' @param min_samples Points within \code{eps} needed to make a point a
#'   core point, counting the point itself.
#' @param metric One of \code{"euclidean"} (default),
#'   \code{"manhattan"} or \code{"chebyshev"}. Anything else is
#'   refused.
#' @return Named list: \code{estimate} (the cluster count),
#'   \code{labels} (0-based, \code{-1} for noise), \code{n_clusters},
#'   \code{n_noise}, \code{core_sample_indices} (0-based, the core
#'   points only), \code{eps}, \code{min_samples}, \code{metric},
#'   \code{n} and \code{method}.
#' @examples
#' set.seed(1)
#' x <- rbind(matrix(rnorm(80, 0, 0.2), ncol = 2), matrix(rnorm(80,
#'     5, 0.2), ncol = 2))
#' morie_dbscan_clustering(x, eps = 0.6, min_samples = 4L)
#' @export
#' @keywords internal
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

# Neighbours within eps by a grid of cells of side eps (1 to 3 dimensions): for every metric
# here |x_k - y_k| <= dist(x, y), so a neighbour lies in the same or an adjacent cell. Each
# list element is sorted, as which() over all points would give, so the clustering is unchanged.
.morie_dbscan_grid_nbrs <- function(x, eps, metric) {
  n <- nrow(x)
  d <- ncol(x)
  side <- eps * (1 + 1e-9)
  cell <- floor(sweep(x, 2L, apply(x, 2L, min)) / side)
  span <- apply(cell, 2L, max) + 3
  mult <- cumprod(c(1, span[-d]))
  key <- as.vector((cell + 1) %*% mult)
  members <- split(seq_len(n), key)
  offs <- as.matrix(expand.grid(rep(list(-1:1), d)))
  off_key <- as.vector(offs %*% mult)
  ukey <- as.numeric(names(members))
  adj <- matrix(match(outer(ukey, off_key, "+"), ukey), nrow = length(ukey))
  nbrs <- vector("list", n)
  for (c in seq_along(members)) {
    own <- members[[c]]
    cand <- sort(unlist(members[adj[c, !is.na(adj[c, ])]], use.names = FALSE))
    hit <- matrix(.morie_dbscan_within(
      lapply(seq_len(d), function(j) as.vector(outer(x[own, j], x[cand, j], "-"))), eps, metric),
      nrow = length(own))
    for (r in seq_along(own)) nbrs[[own[r]]] <- cand[hit[r, ]]
  }
  nbrs
}

# TRUE where a point is within eps, given its coordinate differences per dimension (a list of
# equal-length vectors, in dimension order). Euclidean: dbscan's sum of squares, left to right in
# double precision, <= eps * eps.
.morie_dbscan_within <- function(diffs, eps, metric) {
  switch(metric,
         manhattan = Reduce(`+`, lapply(diffs, abs)) <= eps,
         chebyshev = Reduce(pmax, lapply(diffs, abs)) <= eps,
         Reduce(`+`, lapply(diffs, function(v) v * v)) <= eps * eps)
}
