#' Resampling-based clustering: consensus, evidence accumulation, stability
#'
#' \code{ConsensusClustering}: Monti et al. consensus matrix over item
#' subsamples, average-linkage final partition, CDF area and cluster
#' consensus. \code{EvidenceAccumulation}: Fred-Jain co-association of many
#' k-means partitions, cut at k or at the largest lifetime.
#' \code{ClusterStability}: Hennig's bootstrap Jaccard stability of k-means
#' clusters. All draws are Philox. Identical to the Python arm
#' \code{morie.fn.clusterensemble}.
#'
#' @param X Data matrix (rows are points).
#' @param k Number of clusters (NULL: largest lifetime for
#'   \code{EvidenceAccumulation}).
#' @param n_resamples,p_item Subsamples and item fraction.
#' @param n_runs,k_range Partitions and range of their cluster numbers.
#' @param linkage Linkage of the final hierarchy.
#' @param n_boot Bootstrap replicates.
#' @param seed Philox seed.
#' @return List with \code{cluster}.
#' @references Monti, S., Tamayo, P., Mesirov, J. and Golub, T. (2003).
#'   Consensus clustering. Machine Learning 52, 91-118.
#'
#'   Fred, A. L. N. and Jain, A. K. (2005). Combining multiple clusterings
#'   using evidence accumulation. IEEE Transactions on Pattern Analysis and
#'   Machine Intelligence 27, 835-850.
#'
#'   Hennig, C. (2007). Cluster-wise assessment of cluster stability.
#'   Computational Statistics and Data Analysis 52, 258-271.
#' @examples
#' X <- rbind(c(0, 0), c(0, 1), c(1, 0), c(8, 8), c(8, 9), c(9, 8))
#' ConsensusClustering(X, 2, n_resamples = 20)$cluster
#' EvidenceAccumulation(X, n_runs = 20, k_range = c(2, 4))$cluster
#' @export
ConsensusClustering <- function(X, k, n_resamples = 100L, p_item = 0.8, seed = 1) {
  X <- as.matrix(X)
  n <- nrow(X)
  U <- .ce_unif(seed)
  m <- as.integer(round(p_item * n))
  if (k > m || m > n) stop("the subsample must hold at least k items")
  together <- matrix(0, n, n)
  both <- matrix(0, n, n)
  for (r in seq_len(n_resamples)) {
    pool <- seq_len(n)
    samp <- integer(0)
    for (t in seq_len(m)) {
      j <- U$index(length(pool))
      samp <- c(samp, pool[j])
      pool <- pool[-j]
    }
    samp <- sort(samp)
    lab <- .ce_kmeans(X[samp, , drop = FALSE], k, U)
    same <- outer(lab, lab, "==")
    both[samp, samp] <- both[samp, samp] + 1
    together[samp, samp] <- together[samp, samp] + same
  }
  M <- ifelse(both > 0, together / pmax(both, 1), 0)
  diag(M) <- 1
  h <- HierarchicalClustering(1 - M, "average")
  cl <- CutTree(h$merge, k)
  vals <- sort(M[upper.tri(M)])
  L <- length(vals)
  area <- 0
  for (t in seq_len(L - 1) + 1) area <- area + (vals[t] - vals[t - 1]) * (sum(vals <= vals[t]) / L)
  within <- vapply(seq_len(k), function(c) {
    mem <- which(cl == c)
    if (length(mem) < 2) return(1)
    pr <- numeric(0)
    for (x in seq_len(length(mem) - 1)) pr <- c(pr, M[mem[x], mem[(x + 1):length(mem)]])
    .ce_ss(pr) / length(pr)
  }, 0)
  list(cluster = cl, consensus = M, cdf_area = area, cluster_consensus = within)
}

.ce_ss <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

.ce_unif <- function(seed) {
  env <- new.env()
  env$block <- 0
  env$buf <- numeric(0)
  env$pos <- 0
  nxt <- function() {
    if (env$pos >= length(env$buf)) {
      env$buf <- .morie_random_uniform(4096, seed = seed, stream = env$block)
      env$block <- env$block + 1
      env$pos <- 0
    }
    env$pos <- env$pos + 1
    env$buf[env$pos]
  }
  list(nxt = nxt, index = function(n) min(floor(nxt() * n), n - 1) + 1)
}

.ce_kmeans <- function(P, k, U) {
  n <- nrow(P)
  idx <- integer(0)
  while (length(idx) < k) {
    j <- U$index(n)
    dup <- FALSE
    for (i in idx) if (all(P[j, ] == P[i, ])) dup <- TRUE
    if (!dup) idx <- c(idx, j)
  }
  KmeansLloyd(P, P[idx, , drop = FALSE])$cluster
}

#' @rdname ConsensusClustering
#' @export
EvidenceAccumulation <- function(X, k = NULL, n_runs = 50L, k_range = c(2, 10), seed = 1, linkage = "single") {
  X <- as.matrix(X)
  n <- nrow(X)
  U <- .ce_unif(seed)
  lo <- k_range[1]
  hi <- min(k_range[2], n)
  C <- matrix(0, n, n)
  for (r in seq_len(n_runs)) {
    kk <- lo + U$index(hi - lo + 1) - 1
    lab <- .ce_kmeans(X, kk, U)
    same <- outer(lab, lab, "==")
    C[same] <- C[same] + 1 / n_runs
  }
  Dm <- 1 - C
  diag(Dm) <- 0
  h <- HierarchicalClustering(Dm, linkage)
  if (is.null(k)) {
    hs <- h$height
    gaps <- diff(hs)
    k <- n - which.max(gaps)
  }
  list(cluster = CutTree(h$merge, k), coassociation = C, k = k)
}

#' @rdname ConsensusClustering
#' @export
ClusterStability <- function(X, k, n_boot = 50L, seed = 1) {
  X <- as.matrix(X)
  n <- nrow(X)
  U <- .ce_unif(seed)
  base <- .ce_kmeans(X, k, U)
  jac <- vector("list", k)
  for (b in seq_len(n_boot)) {
    samp <- vapply(seq_len(n), function(t) U$index(n), 1)
    distinct <- sort(unique(samp))
    if (nrow(unique(X[distinct, , drop = FALSE])) < k) next
    lab <- .ce_kmeans(X[samp, , drop = FALSE], k, U)
    boot <- lapply(seq_len(k), function(c) unique(samp[lab == c]))
    for (c in seq_len(k)) {
      cs <- intersect(which(base == c), distinct)
      if (!length(cs)) next
      best <- 0
      for (D in boot) if (length(D)) best <- max(best, length(intersect(cs, D)) / length(union(cs, D)))
      jac[[c]] <- c(jac[[c]], best)
    }
  }
  mean <- vapply(jac, function(v) if (length(v)) .ce_ss(v) / length(v) else 0, 0)
  list(cluster = base, mean_jaccard = mean, recovered = vapply(jac, function(v) sum(v > 0.5), 1L), jaccard = jac)
}
