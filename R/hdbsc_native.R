# HDBSCAN density clustering.
# Source: Campello, Moulavi & Sander (2013), PAKDD, LNCS 7819,
# 160-172, Defs. 5-8, Algorithm 1, Proposition 1
# (fetched-wave3/Density-Based Clustering Based on Hierarchical
# Density Estimates.pdf).  Mirrors Python morie.fn.hdbsc exactly
# (same Prim MST tie-breaking, same threshold sweep).

#' HDBSCAN* flat clustering via the mutual-reachability MST
#'
#' Core distance = distance to the mpts-nearest neighbor (Def. 5);
#' mutual reachability d_mreach = max(core_i, core_j, d) (Def. 7);
#' single-linkage MST (Prop. 1); the threshold cut yielding the most
#' min_cluster_size-valid connected components is returned, others
#' labelled noise (-1).
#'
#' @param X Data matrix (n x d).
#' @param min_pts mpts core-distance neighbor count.
#' @param min_cluster_size Minimum valid cluster size.
#' @return A list with elements \code{labels}, \code{core_distances},
#'   \code{n_clusters}, \code{mst_edges}, \code{min_pts},
#'   \code{method}.
#' @references Campello, R. J. G. B., Moulavi, D. and Sander, J.
#'   (2013). Density-based clustering based on hierarchical density
#'   estimates. PAKDD, LNCS 7819, 160-172.
#' @export
morie_hdbsc <- function(X, min_pts = 5, min_cluster_size = 5) {
  X <- as.matrix(X)
  n <- nrow(X)
  if (n < min_pts || n < 2) stop("need at least min_pts points")
  mp <- as.integer(min_pts)
  D <- as.matrix(stats::dist(X))
  core <- apply(D, 1, function(row) sort(row)[min(mp, n)])
  mreach <- function(i, j) max(core[i], core[j], D[i, j])
  in_tree <- rep(FALSE, n)
  key <- rep(Inf, n)
  parent <- rep(-1L, n)
  key[1] <- 0
  edges <- list()
  for (s in seq_len(n)) {
    cand <- which(!in_tree)
    u <- cand[which.min(key[cand])]
    in_tree[u] <- TRUE
    if (parent[u] != -1) {
      edges[[length(edges) + 1]] <- c(key[u], parent[u], u)
    }
    for (v in which(!in_tree)) {
      w <- mreach(u, v)
      if (w < key[v]) { key[v] <- w; parent[v] <- u }
    }
  }
  E <- if (length(edges)) do.call(rbind, edges) else
    matrix(numeric(0), 0, 3)
  thresholds <- sort(unique(E[, 1]))
  best_labels <- rep(-1L, n)
  best_k <- 0L
  best_score <- 0L
  find <- function(par, x) {
    while (par[x] != x) { par[x] <- par[par[x]]; x <- par[x] }
    x
  }
  for (t in thresholds) {
    par <- seq_len(n)
    for (r in seq_len(nrow(E))) {
      if (E[r, 1] <= t + 1e-12) {
        par[find(par, as.integer(E[r, 2]))] <- find(par, as.integer(E[r, 3]))
      }
    }
    roots <- vapply(seq_len(n), function(i) as.integer(find(par, i)),
                    integer(1))
    labels <- rep(-1L, n)
    k <- 0L
    for (rt in unique(roots)) {
      members <- which(roots == rt)
      if (length(members) >= min_cluster_size) {
        labels[members] <- k
        k <- k + 1L
      }
    }
    assigned <- sum(labels != -1)
    score <- assigned * 1000L + k
    if (score > best_score) {
      best_score <- score; best_k <- k; best_labels <- labels
    }
  }
  ord <- if (nrow(E)) order(E[, 1]) else integer(0)
  list(labels = best_labels, core_distances = core,
       n_clusters = best_k, mst_edges = E[ord, , drop = FALSE],
       min_pts = mp,
       method = "HDBSCAN* (Campello et al. 2013, Algorithm 1)")
}
