.net_adj <- function(n, edges, directed) {
  A <- matrix(0, n, n)
  E <- as.matrix(edges)
  w <- if (ncol(E) > 2) E[, 3] else rep(1, nrow(E))
  for (k in seq_len(nrow(E))) {
    u <- E[k, 1] + 1
    v <- E[k, 2] + 1
    if (u == v) next
    A[u, v] <- A[u, v] + w[k]
    if (!directed) A[v, u] <- A[v, u] + w[k]
  }
  A
}

.net_dijkstra <- function(A, s, weighted) {
  n <- nrow(A)
  d <- rep(Inf, n)
  d[s] <- 0
  done <- rep(FALSE, n)
  repeat {
    cand <- which(!done & is.finite(d))
    if (!length(cand)) break
    u <- cand[which.min(d[cand])]
    done[u] <- TRUE
    nb <- which(A[u, ] > 0)
    nd <- d[u] + (if (weighted) A[u, nb] else 1)
    d[nb] <- pmin(d[nb], nd)
  }
  d
}

.net_brandes <- function(A, weighted) {
  n <- nrow(A)
  cb <- numeric(n)
  for (s in seq_len(n)) {
    d <- .net_dijkstra(A, s, weighted)
    reach <- which(is.finite(d))
    S <- reach[order(d[reach])]
    sigma <- numeric(n)
    sigma[s] <- 1
    P <- vector("list", n)
    for (w in S) {
      if (w == s) next
      pr <- which(A[, w] > 0 & is.finite(d))
      pr <- pr[abs(d[pr] + (if (weighted) A[pr, w] else 1) - d[w]) <= 1e-12 * max(1, abs(d[w]))]
      P[[w]] <- pr
      sigma[w] <- sum(sigma[pr])
    }
    delta <- numeric(n)
    for (w in rev(S)) {
      for (v in P[[w]]) delta[v] <- delta[v] + sigma[v] / sigma[w] * (1 + delta[w])
      if (w != s) cb[w] <- cb[w] + delta[w]
    }
  }
  cb
}

#' Network measures (igraph conventions)
#'
#' \code{ShortestPathLengths}: Dijkstra distances. \code{Centralities}:
#' degree, strength, closeness, Brandes betweenness, eigenvector (maximum 1),
#' PageRank (damping 0.85, uniform dangling redistribution) and
#' Stephenson-Zelen information centrality. \code{NetworkSummary}: density,
#' eccentricity, diameter, radius, transitivity, reciprocity, degree
#' assortativity, global efficiency and degree entropy.
#' \code{ModularityScore}: Newman-Girvan modularity. \code{NetworkConnectivity}:
#' edge and vertex connectivity. \code{NodeVulnerability}: Latora-Marchiori
#' vulnerability and Burt redundancy. Edges are 0-based node pairs
#' (optional third column weight). Identical to the Python arm
#' \code{morie.fn.netstat}.
#'
#' @param n Number of nodes.
#' @param edges Edge matrix with 0-based from and to columns and an optional weight column.
#' @param directed Directed graph.
#' @param weighted Use edge weights as lengths.
#' @param damping PageRank damping.
#' @param membership Community labels.
#' @param resolution Modularity resolution.
#' @return Matrix, list or number.
#' @references Brandes, U. (2001). A faster algorithm for betweenness
#'   centrality. Journal of Mathematical Sociology 25, 163-177.
#'
#'   Newman, M. E. J. (2002). Assortative mixing in networks. Physical Review
#'   Letters 89, 208701.
#'
#'   Stephenson, K. and Zelen, M. (1989). Rethinking centrality: methods and
#'   examples. Social Networks 11, 1-37.
#' @examples
#' Centralities(4, rbind(c(0, 1), c(1, 2), c(2, 3)))$betweenness
#' NetworkSummary(4, rbind(c(0, 1), c(1, 2), c(2, 0), c(2, 3)))$transitivity
#' @export
ShortestPathLengths <- function(n, edges, directed = FALSE, weighted = FALSE) {
  A <- .net_adj(n, edges, directed)
  t(vapply(seq_len(n), function(s) .net_dijkstra(A, s, weighted), numeric(n)))
}

#' @rdname ShortestPathLengths
#' @export
Centralities <- function(n, edges, directed = FALSE, weighted = FALSE, damping = 0.85) {
  A <- .net_adj(n, edges, directed)
  D <- ShortestPathLengths(n, edges, directed, weighted)
  clo <- vapply(seq_len(n), function(i) {
    d <- D[i, -i]
    tot <- sum(d[is.finite(d)])
    if (tot > 0) 1 / tot else NaN
  }, 0)
  bt <- .net_brandes(A, weighted)
  if (!directed) bt <- bt / 2
  x <- rep(1, n)
  for (it in 1:10000) {
    y <- as.vector(A %*% x)
    if (max(y) <= 0) break
    y <- y / max(y)
    if (max(abs(x - y)) < 1e-15) {
      x <- y
      break
    }
    x <- (x + y) / 2
  }
  outw <- rowSums(A)
  r <- rep(1 / n, n)
  for (it in 1:100000) {
    nr <- rep((1 - damping) / n + damping * sum(r[outw == 0]) / n, n)
    act <- outw > 0
    nr <- nr + damping * as.vector(t(A[act, , drop = FALSE] / outw[act]) %*% r[act])
    nr <- nr / sum(nr)
    if (max(abs(r - nr)) < 1e-16) {
      r <- nr
      break
    }
    r <- nr
  }
  info <- rep(NaN, n)
  if (!directed) {
    C <- tryCatch(solve(diag(rowSums(A)) - A + 1), error = function(e) NULL)
    if (!is.null(C)) info <- 1 / (diag(C) + (sum(diag(C)) - 2 * rowSums(C)) / n)
  }
  list(degree = rowSums(A > 0), strength = rowSums(A), closeness = clo, betweenness = bt, eigenvector = x,
       pagerank = r, information = info)
}

#' @rdname ShortestPathLengths
#' @export
NetworkSummary <- function(n, edges, directed = FALSE) {
  A <- (.net_adj(n, edges, directed) > 0) * 1
  U <- (.net_adj(n, edges, FALSE) > 0) * 1
  m <- if (directed) sum(A) else sum(A) / 2
  D <- ShortestPathLengths(n, edges, directed)
  ecc <- apply(D, 1, function(r) max(c(0, r[is.finite(r)])))
  tri <- diag(U %*% U %*% U) / 2
  k <- rowSums(U)
  triples <- k * (k - 1) / 2
  rec <- if (directed) sum(A * t(A)) / sum(A) else NaN
  if (directed) {
    ij <- which(A > 0, arr.ind = TRUE)
    xs <- rowSums(A)[ij[, 1]]
    ys <- colSums(A)[ij[, 2]]
  } else {
    ij <- which(U > 0, arr.ind = TRUE)
    xs <- k[ij[, 1]]
    ys <- k[ij[, 2]]
  }
  assort <- sum((xs - mean(xs)) * (ys - mean(ys))) / sqrt(sum((xs - mean(xs))^2) * sum((ys - mean(ys))^2))
  off <- row(D) != col(D) & is.finite(D)
  pk <- table(k) / n
  list(density = m / (if (directed) n * (n - 1) else n * (n - 1) / 2), eccentricity = ecc, diameter = max(ecc),
       radius = min(ecc), transitivity = if (sum(triples) > 0) sum(tri) / sum(triples) else NaN,
       local_transitivity = ifelse(k >= 2, tri / triples, NaN), reciprocity = rec, assortativity = assort,
       efficiency = sum(1 / D[off]) / (n * (n - 1)), degree_entropy = -sum(pk * log(pk)))
}

#' @rdname ShortestPathLengths
#' @export
ModularityScore <- function(n, edges, membership, resolution = 1) {
  A <- .net_adj(n, edges, FALSE)
  k <- rowSums(A)
  same <- outer(membership, membership, `==`)
  sum((A - resolution * outer(k, k) / sum(k))[same]) / sum(k)
}

.net_maxflow <- function(cap, s, t) {
  flow <- 0
  res <- cap
  n <- nrow(cap)
  repeat {
    par <- rep(0L, n)
    par[s] <- s
    q <- s
    while (length(q) && par[t] == 0) {
      u <- q[1]
      q <- q[-1]
      for (v in which(par == 0 & res[u, ] > 0)) {
        par[v] <- u
        q <- c(q, v)
      }
    }
    if (par[t] == 0) return(flow)
    b <- Inf
    v <- t
    while (v != s) {
      b <- min(b, res[par[v], v])
      v <- par[v]
    }
    v <- t
    while (v != s) {
      res[par[v], v] <- res[par[v], v] - b
      res[v, par[v]] <- res[v, par[v]] + b
      v <- par[v]
    }
    flow <- flow + b
  }
}

#' @rdname ShortestPathLengths
#' @export
NetworkConnectivity <- function(n, edges) {
  U <- (.net_adj(n, edges, FALSE) > 0) * 1
  edge <- if (n > 1) min(vapply(2:n, function(t) .net_maxflow(U, 1, t), 0)) else 0
  S <- matrix(0, 2 * n, 2 * n)
  for (i in seq_len(n)) {
    S[2 * i - 1, 2 * i] <- 1
    for (j in which(U[i, ] > 0)) S[2 * i, 2 * j - 1] <- n
  }
  vals <- c()
  for (s in seq_len(n)) for (t in seq_len(n)) if (s != t && U[s, t] == 0) vals <- c(vals, .net_maxflow(S, 2 * s, 2 * t - 1))
  list(edge = edge, vertex = if (length(vals)) min(vals) else n - 1)
}

#' @rdname ShortestPathLengths
#' @export
NodeVulnerability <- function(n, edges) {
  E <- as.matrix(edges)[, 1:2, drop = FALSE]
  eff <- function(ed) {
    D <- ShortestPathLengths(n, ed)
    off <- row(D) != col(D) & is.finite(D)
    sum(1 / D[off]) / (n * (n - 1))
  }
  E0 <- eff(E)
  vul <- vapply(seq_len(n) - 1, function(i) (E0 - eff(E[E[, 1] != i & E[, 2] != i, , drop = FALSE])) / E0, 0)
  U <- (.net_adj(n, edges, FALSE) > 0) * 1
  k <- rowSums(U)
  t2 <- diag(U %*% U %*% U)
  list(vulnerability = vul, redundancy = ifelse(k > 0, t2 / k, NaN), effective_size = ifelse(k > 0, k - t2 / k, NaN))
}
