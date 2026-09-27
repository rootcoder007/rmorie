.gn_edge_betweenness <- function(adj, n) {
  eb <- matrix(0, n, n)
  for (s in seq_len(n)) {
    stack <- integer(0)
    pred <- vector("list", n)
    sigma <- numeric(n)
    dist <- rep(-1L, n)
    sigma[s] <- 1
    dist[s] <- 0L
    q <- s
    head <- 1L
    while (head <= length(q)) {
      v <- q[head]
      head <- head + 1L
      stack <- c(stack, v)
      for (w in adj[[v]]) {
        if (dist[w] < 0L) {
          dist[w] <- dist[v] + 1L
          q <- c(q, w)
        }
        if (dist[w] == dist[v] + 1L) {
          sigma[w] <- sigma[w] + sigma[v]
          pred[[w]] <- c(pred[[w]], v)
        }
      }
    }
    delta <- numeric(n)
    for (w in rev(stack)) {
      for (v in pred[[w]]) {
        cc <- sigma[v] / sigma[w] * (1 + delta[w])
        a <- min(v, w)
        b <- max(v, w)
        eb[a, b] <- eb[a, b] + cc
        delta[v] <- delta[v] + cc
      }
    }
  }
  eb / 2
}

.gn_components <- function(adj, n) {
  lab <- rep(-1L, n)
  cnum <- 0L
  for (s in seq_len(n)) {
    if (lab[s] < 0L) {
      lab[s] <- cnum
      stack <- s
      while (length(stack)) {
        v <- stack[length(stack)]
        stack <- stack[-length(stack)]
        for (w in adj[[v]]) {
          if (lab[w] < 0L) {
            lab[w] <- cnum
            stack <- c(stack, w)
          }
        }
      }
      cnum <- cnum + 1L
    }
  }
  lab
}

.gn_modularity <- function(E, deg, m, lab) {
  if (m == 0) return(0)
  inside <- sum(lab[E[, 1]] == lab[E[, 2]])
  K <- vapply(sort(unique(lab)), function(c) sum(deg[lab == c]), 0)
  inside / m - sum(K^2) / (4 * m^2)
}

#' Girvan-Newman communities
#'
#' Repeatedly removes the edge of highest betweenness (recomputed after every
#' removal by Brandes' algorithm; ties go to the lexicographically smallest
#' edge) and scores each new split by the modularity of the original graph,
#' Q = sum_c (e_c / m - (K_c / 2m)^2). Returns the partition of maximum Q, or
#' the first with \code{n_communities} components. Any non-zero entry of the
#' symmetric matrix is an edge.
#'
#' @param G Adjacency matrix of an undirected graph.
#' @param n_communities Optional number of communities.
#' @return list(labels (0-based), estimate, modularity, n_communities,
#'   removed (edges, 0-based), path (components and modularity per split)).
#' @references Girvan, M. and Newman, M. E. J. (2002). Community structure in
#'   social and biological networks. PNAS 99, 7821-7826.
#'   Newman, M. E. J. and Girvan, M. (2004). Physical Review E 69, 026113.
#' @examples
#' A <- matrix(0, 6, 6)
#' A[cbind(c(1, 1, 2, 3, 4, 4, 5), c(2, 3, 3, 4, 5, 6, 6))] <- 1
#' GirvanNewman(A + t(A))$labels
#' @export
GirvanNewman <- function(G, n_communities = NULL) {
  W <- as.matrix(G)
  n <- nrow(W)
  if (n == 0 || ncol(W) != n) stop("G must be a non-empty square matrix", call. = FALSE)
  B <- W != 0
  diag(B) <- FALSE
  if (any(B != t(B))) stop("G must be symmetric", call. = FALSE)
  E <- which(B & upper.tri(B), arr.ind = TRUE)
  E <- E[order(E[, 1], E[, 2]), , drop = FALSE]
  m <- nrow(E)
  deg <- rowSums(B)
  adj <- lapply(seq_len(n), function(i) which(B[i, ]))
  lab <- .gn_components(adj, n)
  best_lab <- lab
  best_q <- .gn_modularity(E, deg, m, lab)
  path <- list(c(max(lab) + 1, best_q))
  removed <- matrix(integer(0), 0, 2)
  target <- if (is.null(n_communities)) NULL else as.integer(n_communities)
  ncomp <- max(lab) + 1L
  while ((is.null(target) || ncomp < target) && any(lengths(adj) > 0)) {
    eb <- .gn_edge_betweenness(adj, n)
    top <- max(eb)
    hit <- which(eb >= top - 1e-9 * max(1, top) & upper.tri(eb) & B, arr.ind = TRUE)
    hit <- hit[order(hit[, 1], hit[, 2]), , drop = FALSE]
    i <- hit[1, 1]
    j <- hit[1, 2]
    adj[[i]] <- setdiff(adj[[i]], j)
    adj[[j]] <- setdiff(adj[[j]], i)
    B[i, j] <- B[j, i] <- FALSE
    removed <- rbind(removed, c(i, j) - 1L)
    new <- .gn_components(adj, n)
    if (max(new) + 1L > ncomp) {
      ncomp <- max(new) + 1L
      q <- .gn_modularity(E, deg, m, new)
      path[[length(path) + 1]] <- c(ncomp, q)
      if (!is.null(target) || q > best_q + 1e-12) {
        best_lab <- new
        best_q <- q
      }
    }
  }
  if (!is.null(target) && ncomp < target) stop("the graph cannot be split into that many communities", call. = FALSE)
  list(labels = best_lab, estimate = best_q, modularity = best_q, n_communities = max(best_lab) + 1L,
       removed = removed, path = do.call(rbind, path))
}
