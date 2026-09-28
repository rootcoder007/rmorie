#' Community detection: fast greedy modularity and walktrap
#'
#' \code{GraphModularity}: Newman's modularity of a partition of a weighted
#' graph. \code{FastGreedyModularity}: Clauset-Newman-Moore greedy merging,
#' as \code{igraph::cluster_fast_greedy}. \code{WalktrapCommunities}:
#' Pons-Latapy random-walk distances with vertex loops, as
#' \code{igraph::cluster_walktrap}. Both return the partition of largest
#' modularity along the merge sequence. Identical to the Python arm
#' \code{morie.fn.communities}.
#'
#' @param A Symmetric non-negative weighted adjacency matrix.
#' @param membership Community labels.
#' @param steps Random-walk length.
#' @return Modularity (numeric) or list with \code{membership},
#'   \code{modularity}, \code{merges}, \code{max_modularity}.
#' @references Clauset, A., Newman, M. E. J. and Moore, C. (2004). Finding
#'   community structure in very large networks. Physical Review E 70, 066111.
#'
#'   Pons, P. and Latapy, M. (2006). Computing communities in large networks
#'   using random walks. Journal of Graph Algorithms and Applications 10,
#'   191-218.
#'
#'   Newman, M. E. J. (2004). Analysis of weighted networks. Physical Review
#'   E 70, 056131.
#' @examples
#' A <- matrix(0, 6, 6)
#' A[cbind(c(1, 1, 2, 3, 4, 4, 5), c(2, 3, 3, 4, 5, 6, 6))] <- 1
#' A <- A + t(A)
#' FastGreedyModularity(A)$membership
#' WalktrapCommunities(A)$membership
#' GraphModularity(A, c(1, 1, 1, 2, 2, 2))
#' @export
GraphModularity <- function(A, membership) {
  W <- .cm_adj(A)
  n <- nrow(W)
  m <- 0
  for (i in seq_len(n - 1)) for (j in (i + 1):n) m <- m + W[i, j]
  q <- 0
  for (cc in sort(unique(membership))) {
    mem <- which(membership == cc)
    win <- 0
    if (length(mem) > 1) {
      for (a in seq_len(length(mem) - 1)) for (b in (a + 1):length(mem)) win <- win + W[mem[a], mem[b]]
    }
    s <- 0
    for (i in mem) for (j in seq_len(n)) s <- s + W[i, j]
    q <- q + win / m - (s / (2 * m))^2
  }
  q
}

.cm_adj <- function(A) {
  W <- unname(as.matrix(A)) + 0
  if (nrow(W) != ncol(W)) stop("adjacency must be square")
  diag(W) <- 0
  if (any(W != t(W)) || any(W < 0)) stop("adjacency must be symmetric and non-negative")
  W
}

.cm_ss <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

#' @rdname GraphModularity
#' @export
FastGreedyModularity <- function(A) {
  W <- .cm_adj(A)
  n <- nrow(W)
  m2 <- .cm_ss(t(W))
  E <- W / m2
  a <- vapply(seq_len(n), function(i) .cm_ss(W[i, ]), 0) / m2
  q <- 0
  for (i in seq_len(n)) q <- q - a[i] * a[i]
  alive <- rep(TRUE, n)
  merges <- matrix(integer(0), 0, 2)
  qs <- q
  repeat {
    best <- NULL
    bdq <- -Inf
    for (i in which(alive)) {
      for (j in which(E[i, ] > 0)) {
        if (j > i) {
          dq <- 2 * (E[i, j] - a[i] * a[j])
          if (dq > bdq) {
            best <- c(i, j)
            bdq <- dq
          }
        }
      }
    }
    if (is.null(best)) break
    i <- best[1]
    j <- best[2]
    for (k in which(E[j, ] > 0)) {
      if (k == i) next
      E[i, k] <- E[i, k] + E[j, k]
      E[k, i] <- E[k, i] + E[j, k]
      E[k, j] <- 0
    }
    E[i, j] <- 0
    E[j, ] <- 0
    E[, j] <- 0
    a[i] <- a[i] + a[j]
    a[j] <- 0
    alive[j] <- FALSE
    q <- q + bdq
    merges <- rbind(merges, c(i, j))
    qs <- c(qs, q)
  }
  st <- which.max(qs) - 1
  comp <- seq_len(n)
  for (s in seq_len(st)) comp[comp == merges[s, 2]] <- merges[s, 1]
  list(membership = match(comp, unique(comp)), modularity = qs, merges = merges, max_modularity = qs[st + 1])
}

#' @rdname GraphModularity
#' @export
WalktrapCommunities <- function(A, steps = 4L) {
  W <- .cm_adj(A)
  n <- nrow(W)
  L <- W
  d <- numeric(n)
  for (i in seq_len(n)) {
    deg <- sum(W[i, ] > 0)
    s <- .cm_ss(W[i, ])
    L[i, i] <- if (deg == 0) 1 else s / deg
    d[i] <- s + L[i, i]
  }
  Pt <- matrix(0, n, n)
  for (v in seq_len(n)) {
    p <- numeric(n)
    p[v] <- 1
    for (t in seq_len(steps)) {
      q <- numeric(n)
      for (i in seq_len(n)) {
        if (p[i] != 0) {
          pi_ <- p[i] / d[i]
          for (j in which(L[i, ] > 0)) q[j] <- q[j] + pi_ * L[i, j]
        }
      }
      p <- q
    }
    Pt[v, ] <- p
  }
  comm <- lapply(seq_len(n), function(i) list(P = Pt[i, ], size = 1, members = i))
  names(comm) <- seq_len(n)
  nbr <- lapply(seq_len(n), function(i) setdiff(which(W[i, ] > 0), i))
  m <- 0
  for (i in seq_len(n - 1)) for (j in (i + 1):n) m <- m + W[i, j]
  win <- numeric(2 * n)
  stot <- numeric(2 * n)
  stot[seq_len(n)] <- vapply(seq_len(n), function(i) .cm_ss(W[i, ]), 0)
  q0 <- 0
  for (i in seq_len(n)) q0 <- q0 - (stot[i] / (2 * m))^2
  qs <- q0
  merges <- matrix(integer(0), 0, 3)
  cl <- vector("list", 2 * n)
  for (i in seq_len(n)) cl[[i]] <- comm[[i]]
  active <- seq_len(n)
  nb <- vector("list", 2 * n)
  for (i in seq_len(n)) nb[[i]] <- nbr[[i]]
  dsig <- function(a, b) {
    s <- 0
    for (k in seq_len(n)) {
      t <- cl[[a]]$P[k] - cl[[b]]$P[k]
      s <- s + t * t / d[k]
    }
    s * cl[[a]]$size * cl[[b]]$size / (cl[[a]]$size + cl[[b]]$size) / n
  }
  nxt <- n + 1
  repeat {
    best <- NULL
    bd <- Inf
    for (a in sort(active)) {
      for (b in sort(nb[[a]])) {
        if (b > a) {
          v <- dsig(a, b)
          if (v < bd) {
            best <- c(a, b)
            bd <- v
          }
        }
      }
    }
    if (is.null(best)) break
    a <- best[1]
    b <- best[2]
    sa <- cl[[a]]$size
    sb <- cl[[b]]$size
    P <- (sa * cl[[a]]$P + sb * cl[[b]]$P) / (sa + sb)
    cross <- 0
    for (i in cl[[a]]$members) for (j in cl[[b]]$members) cross <- cross + W[i, j]
    cl[[nxt]] <- list(P = P, size = sa + sb, members = c(cl[[a]]$members, cl[[b]]$members))
    win[nxt] <- win[a] + win[b] + cross
    stot[nxt] <- stot[a] + stot[b]
    nb[[nxt]] <- setdiff(union(nb[[a]], nb[[b]]), c(a, b))
    for (cc in nb[[nxt]]) nb[[cc]] <- union(setdiff(nb[[cc]], c(a, b)), nxt)
    active <- c(setdiff(active, c(a, b)), nxt)
    q <- 0
    for (cc in sort(active)) q <- q + win[cc] / m - (stot[cc] / (2 * m))^2
    qs <- c(qs, q)
    merges <- rbind(merges, c(a, b, nxt))
    nxt <- nxt + 1
  }
  st <- which.max(qs) - 1
  lab <- seq_len(n)
  for (s in seq_len(st)) lab[lab %in% merges[s, 1:2]] <- merges[s, 3]
  list(membership = match(lab, unique(lab)), modularity = qs, merges = merges[, 1:2, drop = FALSE],
       max_modularity = qs[st + 1])
}
