# Cluster-aware generalized random forests.
# Sources: Athey, S., Tibshirani, J. & Wager, S. (2019) "Generalized
# Random Forests", *The Annals of Statistics* 47(2), 1148-1178,
# doi:10.1214/18-AOS1709, arXiv:1610.01271 -- eq. (2)-(3) for the
# cluster-level GRF and the grf package's clustered sampling. Wager,
# S. & Athey, S. (2018) "Estimation and Inference of Heterogeneous
# Treatment Effects using Random Forests", *JASA* 113(523),
# 1228-1242, doi:10.1080/01621459.2017.1319839, eq. (8) for the
# infinitesimal jackknife this aggregates. Cameron, A. C. & Miller,
# D. L. (2015) "A Practitioner's Guide to Cluster-Robust Inference",
# *Journal of Human Resources* 50(2), 317-372, for why the cluster
# is the unit.
#
# Native implementation mirroring Python morie.fn.clrgrf exactly: the
# same cluster-level subsampling (via hntfst's grow_forest with the
# clusters argument), the same infinitesimal-jackknife covariance
# taken against the cluster indicator with the
# m(m-1)/(m - sc)^2 factor counting clusters, the same unit="cluster"
# default that weights each cluster equally inside a leaf, and the
# same diagnostic that flags row-level subsampling as the half-measure
# that violates honesty through the cluster while still producing a
# narrow interval.

.CLRGRF_EPS <- 1e-12

#' clrgrf_cluster_index
#'
#' A step of the clrgrf_native implementation. Called by \code{morie_clrgrf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param clusters Coerced to character by the body, with \code{as.character}.
#' @return A list with \code{groups}, \code{labels}.
#' @export
clrgrf_cluster_index <- function(clusters) {
  lab <- as.character(clusters)
  order <- character(0)
  groups <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_along(lab)) {
    c <- lab[i]
    if (exists(c, envir = groups, inherits = FALSE)) {
      groups[[c]] <- c(groups[[c]], i - 1L)  # 0-based row indices
    } else {
      groups[[c]] <- i - 1L
      order <- c(order, c)
    }
  }
  list(groups = lapply(order, function(k) groups[[k]]),
       labels = order)
}

#' clrgrf_cluster_jackknife
#'
#' A step of the clrgrf_native implementation. Called by \code{morie_clrgrf}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param preds A vector; its length is taken.
#' @param bags A vector; indexed elementwise.
#' @param groups A vector; its length is taken.
#' @param correction A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{variance}, \code{info}.
#' @export
clrgrf_cluster_jackknife <- function(preds, bags, groups,
                                      correction = TRUE) {
  B <- length(preds)
  m <- length(groups)
  if (B < 2L) stop(sprintf("clrgrf: need at least 2 trees, got %d", B))
  if (m < 3L) stop(sprintf("clrgrf: need at least 3 clusters, got %d", m))
  inbag_c <- lapply(seq_len(B), function(b) {
    vapply(groups, function(g) any(bags[[b]][g + 1L]), logical(1))
  })
  inbag_mat <- do.call(rbind, inbag_c)
  sc <- mean(rowSums(inbag_mat))
  subsampled <- sc < m - 0.5
  pbar <- mean(preds)
  total <- 0.0
  for (c in seq_len(m)) {
    nbar <- mean(inbag_mat[, c])
    cov <- sum((preds - pbar) * ((1.0 * inbag_mat[, c]) - nbar)) / B
    total <- total + cov * cov
  }
  if (isTRUE(correction) && subsampled) {
    total <- total * (m - 1.0) / m * (m / (m - sc))^2
  }
  list(variance = total,
       info = list(clusters_per_tree = sc, subsampled = subsampled, m = m))
}

#' morie_clrgrf
#'
#' A step of the clrgrf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param X A matrix; passed to \code{as.matrix}.
#' @param clusters A vector; its length is taken and its elements indexed.
#' @param at Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @param n_trees Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.
#' @param min_leaf Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5L}.
#' @param subsample_frac Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.5}.
#' @param seed Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0L}.
#' @param unit One of \code{"cluster"}, \code{"row"}. Defaults to \code{"cluster"}.
#' @param level Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.
#' @param cluster_sampling A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{estimate}, \code{fitted}, \code{se}, \code{ci}, \code{variance}, \code{n}, \code{n_clusters}, \code{clusters_per_tree}, \code{clusters_subsampled}, \code{cluster_sizes}, \code{cluster_labels}, \code{unit}, \code{cluster_sampling}, \code{n_trees}, \code{level}, \code{method}.
#' @export
morie_clrgrf <- function(y, X, clusters, at = NULL, n_trees = 200L,
                          min_leaf = 5L, subsample_frac = 0.5, seed = 0L,
                          unit = "cluster", level = 0.95,
                          cluster_sampling = TRUE) {
  if (!(unit %in% c("cluster", "row")))
    stop(sprintf("clrgrf: unit must be cluster or row, got %r", unit))
  yv <- as.numeric(y)
  n <- length(yv)
  Xm <- as.matrix(X)
  storage.mode(Xm) <- "double"
  if (nrow(Xm) != n)
    stop(sprintf("clrgrf: %d covariate rows for %d outcomes",
                 nrow(Xm), n))
  if (length(clusters) != n)
    stop(sprintf("clrgrf: %d cluster labels for %d rows",
                 length(clusters), n))
  ci <- clrgrf_cluster_index(clusters)
  groups <- ci$groups
  labels <- ci$labels
  m <- length(groups)
  if (m < 6L)
    stop(sprintf("clrgrf: need at least 6 clusters, got %d", m))

  gr <- grow_forest(Xm, yv, W = NULL, kind = "double-sample",
                    n_trees = as.integer(n_trees),
                    min_leaf = as.integer(min_leaf),
                    subsample_frac = as.numeric(subsample_frac),
                    seed = as.numeric(seed),
                    clusters = if (isTRUE(cluster_sampling)) clusters
                               else NULL)
  trees <- gr$trees
  bags <- gr$bags
  Q <- if (is.null(at)) Xm else as.matrix(at)
  fitted <- numeric(nrow(Q))
  var <- numeric(nrow(Q))
  info <- NULL
  for (q in seq_len(nrow(Q))) {
    per_tree <- numeric(length(trees))
    for (b in seq_along(trees)) {
      nd <- leaf_of(trees[[b]], Q[q, ])$node
      rows <- nd$I
      if (length(rows) == 0L) { per_tree[b] <- 0.0; next }
      if (unit == "row") {
        per_tree[b] <- mean(yv[rows + 1L])
      } else {
        # equal weight per cluster present in the leaf
        byc <- new.env(hash = TRUE, parent = emptyenv())
        for (i in rows) {
          k <- as.character(clusters[i + 1L])
          if (exists(k, envir = byc, inherits = FALSE)) {
            byc[[k]] <- c(byc[[k]], yv[i + 1L])
          } else {
            byc[[k]] <- yv[i + 1L]
          }
        }
        ks <- ls(byc, all.names = TRUE)
        per_tree[b] <- mean(vapply(ks, function(k) mean(byc[[k]]),
                                    numeric(1)))
      }
    }
    fitted[q] <- mean(per_tree)
    jj <- clrgrf_cluster_jackknife(per_tree, bags, groups)
    var[q] <- jj$variance
    info <- jj$info
  }
  se <- sqrt(pmax(var, 0.0))
  z <- qnorm(0.5 + 0.5 * as.numeric(level))
  ci_l <- fitted - z * se
  ci_u <- fitted + z * se
  list(estimate = fitted, fitted = fitted, se = se,
       ci = lapply(seq_along(fitted), function(q) c(ci_l[q], ci_u[q])),
       variance = var, n = n, n_clusters = m,
       clusters_per_tree = info$clusters_per_tree,
       clusters_subsampled = info$subsampled,
       cluster_sizes = vapply(groups, length, integer(1)),
       cluster_labels = labels, unit = unit,
       cluster_sampling = isTRUE(cluster_sampling),
       n_trees = as.integer(n_trees), level = as.numeric(level),
       method = paste0("cluster-aware generalized random forest, ",
                       "Athey, Tibshirani & Wager (2019) with eq. (8) ",
                       "aggregated to the cluster"))
}

#' clrgrf_cheatsheet
#'
#' A step of the clrgrf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
clrgrf_cheatsheet <- function() {
  paste0("clrgrf: draw whole CLUSTERS into the subsample -- row-wise ",
         "draws split clusters across the split and estimate halves, ",
         "violating honesty through the cluster -- and take the IJ ",
         "covariance against the cluster indicator with the ",
         "m(m-1)/(m-sc)^2 factor counting clusters.")
}
