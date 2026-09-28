#' Partitioning clustering: Lloyd k-means and PAM
#'
#' \code{KmeansLloyd}: Lloyd (1982) iterations from given centres, as
#' \code{kmeans(algorithm = "Lloyd")}. \code{PamMedoids}: BUILD and SWAP
#' (Kaufman and Rousseeuw 1990) as \code{cluster::pam}, clusters numbered by
#' first appearance. Identical to the Python arm \code{morie.fn.clusops}.
#'
#' @param X Data matrix.
#' @param centers Initial centres (matrix).
#' @param maxit Maximum iterations.
#' @param D Dissimilarity matrix.
#' @param k Number of medoids.
#' @return List.
#' @references Lloyd, S. P. (1982). Least squares quantization in PCM. IEEE
#'   Transactions on Information Theory 28, 129-137.
#'
#'   Kaufman, L. and Rousseeuw, P. J. (1990). Finding Groups in Data. Wiley.
#' @examples
#' KmeansLloyd(rbind(c(0, 0), c(0, 1), c(5, 5), c(5, 6)), rbind(c(0, 0), c(5, 5)))$cluster
#' PamMedoids(rbind(c(0, 1, 5, 6), c(1, 0, 4, 5), c(5, 4, 0, 1), c(6, 5, 1, 0)), 2)$medoids
#' @export
KmeansLloyd <- function(X, centers, maxit = 100L) {
  X <- as.matrix(X)
  C <- as.matrix(centers)
  k <- nrow(C)
  cl <- NULL
  it <- 0
  for (it in seq_len(maxit)) {
    d2 <- vapply(seq_len(k), function(j) rowSums(sweep(X, 2, C[j, ])^2), numeric(nrow(X)))
    new <- max.col(-matrix(d2, nrow(X)), ties.method = "first")
    if (!is.null(cl) && all(new == cl)) break
    cl <- new
    for (j in seq_len(k)) if (any(cl == j)) C[j, ] <- colMeans(X[cl == j, , drop = FALSE])
  }
  wss <- vapply(seq_len(k), function(j) sum(sweep(X[cl == j, , drop = FALSE], 2, C[j, ])^2), 0)
  tss <- sum(sweep(X, 2, colMeans(X))^2)
  list(cluster = cl, centers = C, withinss = wss, tot_withinss = sum(wss), betweenss = tss - sum(wss), iter = it)
}

#' @rdname KmeansLloyd
#' @export
PamMedoids <- function(D, k) {
  M <- as.matrix(D)
  n <- nrow(M)
  if (k < 1 || k >= n) stop("k must be in 1..n-1")
  cost <- function(meds) sum(apply(M[, meds, drop = FALSE], 1, min))
  meds <- unname(which.min(rowSums(M)))
  while (length(meds) < k) {
    cur <- cost(meds)
    cand <- setdiff(seq_len(n), meds)
    g <- vapply(cand, function(i) cur - cost(c(meds, i)), 0)
    meds <- c(meds, cand[which.max(g)])
  }
  build <- cost(meds) / n
  repeat {
    cur <- cost(meds)
    best <- NULL
    bgain <- 1e-10 * max(1, cur)
    for (a in seq_len(k)) {
      for (h in setdiff(seq_len(n), meds)) {
        trial <- meds
        trial[a] <- h
        g <- cur - cost(trial)
        if (g > bgain) {
          best <- trial
          bgain <- g
        }
      }
    }
    if (is.null(best)) break
    meds <- best
  }
  near <- apply(M[, meds, drop = FALSE], 1, which.min)
  first <- unique(near)
  list(medoids = meds[first], clustering = match(near, first), objective = c(build = build, swap = cost(meds) / n))
}

#' Silhouette widths and Dunn index
#'
#' \code{SilhouetteWidths}: \eqn{(b_i - a_i)/\max(a_i, b_i)} (Rousseeuw 1987),
#' singletons 0, as \code{cluster::silhouette}. \code{DunnIndex}: smallest
#' between-cluster distance over the largest diameter (Dunn 1974).
#'
#' @param D Dissimilarity matrix.
#' @param clusters Labels.
#' @return List or number.
#' @references Rousseeuw, P. J. (1987). Silhouettes: a graphical aid to the
#'   interpretation and validation of cluster analysis. Journal of
#'   Computational and Applied Mathematics 20, 53-65.
#' @examples
#' D <- rbind(c(0, 1, 5, 6), c(1, 0, 4, 5), c(5, 4, 0, 1), c(6, 5, 1, 0))
#' SilhouetteWidths(D, c(1, 1, 2, 2))$width
#' DunnIndex(D, c(1, 1, 2, 2))
#' @export
SilhouetteWidths <- function(D, clusters) {
  M <- as.matrix(D)
  n <- nrow(M)
  cls <- sort(unique(clusters))
  width <- numeric(n)
  neigh <- numeric(n)
  for (i in seq_len(n)) {
    own <- setdiff(which(clusters == clusters[i]), i)
    oth <- setdiff(cls, clusters[i])
    b <- vapply(oth, function(cc) mean(M[i, clusters == cc]), 0)
    neigh[i] <- oth[which.min(b)]
    if (!length(own)) next
    a <- mean(M[i, own])
    bb <- min(b)
    width[i] <- if (max(a, bb) > 0) (bb - a) / max(a, bb) else 0
  }
  list(width = width, neighbor = neigh, cluster_avg = vapply(cls, function(cc) mean(width[clusters == cc]), 0),
       avg_width = mean(width))
}

#' @rdname SilhouetteWidths
#' @export
DunnIndex <- function(D, clusters) {
  M <- as.matrix(D)
  same <- outer(clusters, clusters, `==`)
  diam <- max(M[same])
  if (diam > 0) min(M[!same]) / diam else Inf
}

#' Agglomerative hierarchical clustering and tree cutting (hclust conventions)
#'
#' Lance-Williams updates for single, complete, average and ward.D2 (Lance
#' and Williams 1967; Murtagh and Legendre 2014); merge matrix and heights as
#' \code{stats::hclust}; \code{CutTree} numbers clusters by first appearance
#' as \code{cutree}.
#'
#' @param D Dissimilarity matrix.
#' @param method Linkage.
#' @param merge Merge matrix.
#' @param k Number of groups.
#' @return List or labels.
#' @references Murtagh, F. and Legendre, P. (2014). Ward's hierarchical
#'   agglomerative clustering method: which algorithms implement Ward's
#'   criterion? Journal of Classification 31, 274-295.
#' @examples
#' h <- HierarchicalClustering(rbind(c(0, 1, 5, 6), c(1, 0, 4, 5), c(5, 4, 0, 1), c(6, 5, 1, 0)), "single")
#' CutTree(h$merge, 2)
#' @export
HierarchicalClustering <- function(D, method = "complete") {
  if (!method %in% c("single", "complete", "average", "ward.D2")) {
    stop("method must be single, complete, average or ward.D2")
  }
  M <- as.matrix(D)
  n <- nrow(M)
  ward <- method == "ward.D2"
  d <- if (ward) M^2 else M
  diag(d) <- Inf
  ids <- -seq_len(n)
  size <- rep(1, n)
  active <- rep(TRUE, n)
  merge <- matrix(0L, n - 1, 2)
  height <- numeric(n - 1)
  for (step in seq_len(n - 1)) {
    dd <- d
    dd[!active, ] <- Inf
    dd[, !active] <- Inf
    dd[lower.tri(dd, diag = TRUE)] <- Inf
    w <- which(dd == min(dd), arr.ind = TRUE)
    w <- w[order(w[, 1], w[, 2]), , drop = FALSE][1, ]
    a <- w[1]
    b <- w[2]
    la <- ids[a]
    lb <- ids[b]
    merge[step, ] <- if (la < 0 && lb < 0) sort(c(la, lb), decreasing = TRUE) else sort(c(la, lb))
    dab <- d[a, b]
    height[step] <- if (ward) sqrt(dab) else dab
    oth <- which(active & seq_len(n) != a & seq_len(n) != b)
    v <- switch(method,
      single = pmin(d[a, oth], d[b, oth]),
      complete = pmax(d[a, oth], d[b, oth]),
      average = (size[a] * d[a, oth] + size[b] * d[b, oth]) / (size[a] + size[b]),
      ward.D2 = ((size[a] + size[oth]) * d[a, oth] + (size[b] + size[oth]) * d[b, oth] - size[oth] * dab) /
        (size[a] + size[b] + size[oth])
    )
    d[a, oth] <- v
    d[oth, a] <- v
    active[b] <- FALSE
    size[a] <- size[a] + size[b]
    ids[a] <- step
  }
  list(merge = merge, height = height, method = method, n = n)
}

#' @rdname HierarchicalClustering
#' @export
CutTree <- function(merge, k) {
  merge <- as.matrix(merge)
  n <- nrow(merge) + 1
  if (k < 1 || k > n) stop("k must be in 1..n")
  g <- -seq_len(n)
  for (s in seq_len(n - k)) g[g %in% merge[s, ]] <- s
  match(g, unique(g))
}

#' Density-based clustering: DBSCAN and OPTICS (dbscan conventions)
#'
#' \code{DbscanClusters}: Ester et al. (1996), clusters grown from core points
#' in index order, 0 for noise, as \code{dbscan::dbscan}.
#' \code{OpticsOrdering}: Ankerst et al. (1999) ordering, reachability and
#' core distances, ties to the higher index as \code{dbscan::optics}.
#'
#' @param X Data matrix.
#' @param eps Radius.
#' @param min_pts Minimum points (itself included).
#' @param border_points Assign border points.
#' @return List.
#' @references Ester, M., Kriegel, H.-P., Sander, J. and Xu, X. (1996). A
#'   density-based algorithm for discovering clusters in large spatial
#'   databases with noise. KDD-96, 226-231.
#' @examples
#' X <- rbind(c(0, 0), c(0, 1), c(1, 0), c(9, 9), c(9, 8), c(8, 9), c(5, 5))
#' DbscanClusters(X, 1.5, 3)$cluster
#' OpticsOrdering(rbind(c(0, 0), c(0, 1), c(0, 3), c(10, 0)), 5, 2)$order
#' @export
DbscanClusters <- function(X, eps, min_pts = 5, border_points = TRUE) {
  D <- as.matrix(stats::dist(as.matrix(X)))
  n <- nrow(D)
  nb <- lapply(seq_len(n), function(i) which(D[i, ] <= eps))
  core <- lengths(nb) >= min_pts
  lab <- integer(n)
  cc <- 0L
  for (i in seq_len(n)) {
    if (lab[i] > 0 || !core[i]) next
    cc <- cc + 1L
    lab[i] <- cc
    queue <- i
    while (length(queue)) {
      q <- queue[1]
      queue <- queue[-1]
      if (!core[q]) next
      for (j in nb[[q]]) {
        if (lab[j] == 0) {
          if (core[j] || border_points) lab[j] <- cc
          if (core[j]) queue <- c(queue, j)
        }
      }
    }
  }
  list(cluster = lab, core = core)
}

#' @rdname DbscanClusters
#' @export
OpticsOrdering <- function(X, eps = Inf, min_pts = 5) {
  D <- as.matrix(stats::dist(as.matrix(X)))
  n <- nrow(D)
  core <- apply(D, 1, function(r) {
    s <- sort(r)
    if (length(s) >= min_pts) s[min_pts] else Inf
  })
  core <- unname(core)
  core[core > eps] <- Inf
  reach <- rep(Inf, n)
  done <- rep(FALSE, n)
  ord <- integer(0)
  for (s in seq_len(n)) {
    if (done[s]) next
    seeds <- integer(0)
    cur <- s
    repeat {
      done[cur] <- TRUE
      ord <- c(ord, cur)
      if (is.finite(core[cur])) {
        for (j in which(!done & D[cur, ] <= eps)) {
          r <- max(core[cur], D[cur, j])
          if (r < reach[j]) {
            reach[j] <- r
            seeds <- union(seeds, j)
          }
        }
      }
      if (!length(seeds)) break
      o <- order(reach[seeds], -seeds)
      cur <- seeds[o[1]]
      seeds <- setdiff(seeds, cur)
    }
  }
  list(order = ord, reachdist = reach, coredist = core)
}

#' Fuzzy c-means and Gaussian mean shift
#'
#' \code{FuzzyCmeans}: Bezdek (1981) iterations from given centres with the
#' Xie-Beni index (Xie and Beni 1991) and partition coefficient.
#' \code{MeanShift}: Gaussian-kernel mode seeking (Comaniciu and Meer 2002),
#' modes within \code{merge_tol} merged.
#'
#' @param X Data matrix.
#' @param centers Initial centres.
#' @param m Fuzzifier.
#' @param tol Tolerance.
#' @param maxit Maximum iterations.
#' @param bandwidth Kernel bandwidth.
#' @param merge_tol Mode merging distance (default bandwidth / 100).
#' @return List.
#' @references Bezdek, J. C. (1981). Pattern Recognition with Fuzzy Objective
#'   Function Algorithms. Plenum.
#'
#'   Comaniciu, D. and Meer, P. (2002). Mean shift: a robust approach toward
#'   feature space analysis. IEEE Transactions on Pattern Analysis and
#'   Machine Intelligence 24, 603-619.
#' @examples
#' FuzzyCmeans(matrix(c(0, 1, 9, 10)), matrix(c(0, 10)))$centers
#' MeanShift(matrix(c(0, 0.2, 5, 5.2)), 0.5)$cluster
#' @export
FuzzyCmeans <- function(X, centers, m = 2, tol = 1e-9, maxit = 1000L) {
  if (m <= 1) stop("m must exceed 1")
  X <- as.matrix(X)
  V <- as.matrix(centers)
  n <- nrow(X)
  k <- nrow(V)
  e <- 2 / (m - 1)
  it <- 0
  for (it in seq_len(maxit)) {
    d <- sqrt(vapply(seq_len(k), function(a) rowSums(sweep(X, 2, V[a, ])^2), numeric(n)))
    d <- matrix(d, n)
    U <- t(apply(d, 1, function(r) {
      if (any(r == 0)) return((r == 0) / sum(r == 0))
      1 / vapply(seq_len(k), function(a) sum((r[a] / r)^e), 0)
    }))
    U <- matrix(U, n)
    Um <- U^m
    newV <- t(Um) %*% X / colSums(Um)
    shift <- max(sqrt(rowSums((V - newV)^2)))
    V <- newV
    if (shift < tol) break
  }
  d2 <- matrix(vapply(seq_len(k), function(a) rowSums(sweep(X, 2, V[a, ])^2), numeric(n)), n)
  J <- sum(U^m * d2)
  sep <- min(as.matrix(stats::dist(V))[upper.tri(diag(k))])^2
  list(centers = unname(V), membership = U, objective = J, xie_beni = J / (n * sep),
       partition_coefficient = sum(U^2) / n, cluster = max.col(U, ties.method = "first"), iter = it)
}

#' @rdname FuzzyCmeans
#' @export
MeanShift <- function(X, bandwidth, tol = 1e-10, maxit = 1000L, merge_tol = NULL) {
  X <- as.matrix(X)
  mt <- if (is.null(merge_tol)) bandwidth / 100 else merge_tol
  ends <- t(apply(X, 1, function(y) {
    for (it in seq_len(maxit)) {
      w <- exp(-rowSums(sweep(X, 2, y)^2) / (2 * bandwidth^2))
      ny <- colSums(w * X) / sum(w)
      mv <- sqrt(sum((ny - y)^2))
      y <- ny
      if (mv < tol) break
    }
    y
  }))
  ends <- matrix(ends, nrow(X))
  modes <- NULL
  lab <- integer(nrow(X))
  for (i in seq_len(nrow(X))) {
    hit <- if (is.null(modes)) integer(0) else which(sqrt(colSums((t(modes) - ends[i, ])^2)) < mt)
    if (length(hit)) {
      lab[i] <- hit[1]
    } else {
      modes <- rbind(modes, ends[i, ])
      lab[i] <- nrow(modes)
    }
  }
  list(cluster = lab, modes = modes, endpoints = ends)
}
