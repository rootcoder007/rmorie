#' Clustering algorithms: affinity propagation, CLARANS, CURE, DIANA, CHAMELEON
#'
#' \code{AffinityPropagation}: Frey-Dueck message passing with exemplar
#' refinement as in \code{apcluster}. \code{Clarans}: randomized medoid
#' search (Philox). \code{CureClustering}: hierarchical merging on shrunken
#' representative points. \code{Diana}: divisive analysis as
#' \code{cluster::diana}, with the divisive coefficient. \code{Chameleon}:
#' k-nearest-neighbour graph partitioning (spectral bisection) and merging
#' by relative interconnectivity and closeness. Identical to the Python arm
#' \code{morie.fn.clusteralgo}.
#'
#' @param X Data matrix (rows are points).
#' @param S Similarity matrix (default negative squared distances).
#' @param preference Diagonal preference (default median similarity).
#' @param damping Damping factor.
#' @param maxits,convits Maximum iterations and stable iterations to stop.
#' @param k Number of clusters (medoids).
#' @param numlocal,maxneighbor CLARANS restarts and neighbour budget.
#' @param seed Philox seed.
#' @param n_rep,alpha CURE representatives and shrink factor; for
#'   \code{Chameleon} alpha weights relative closeness.
#' @param D Dissimilarity matrix (or data when \code{is_distance = FALSE}).
#' @param is_distance Whether \code{D} is a dissimilarity matrix.
#' @param n_neighbors,min_size Graph degree and phase-1 part size.
#' @return List with 1-based \code{cluster} labels (numbered by first appearance).
#' @references Frey, B. J. and Dueck, D. (2007). Clustering by passing
#'   messages between data points. Science 315, 972-976.
#'
#'   Ng, R. T. and Han, J. (2002). CLARANS: a method for clustering objects
#'   for spatial data mining. IEEE Transactions on Knowledge and Data
#'   Engineering 14, 1003-1016.
#'
#'   Guha, S., Rastogi, R. and Shim, K. (1998). CURE: an efficient clustering
#'   algorithm for large databases. Proceedings of ACM SIGMOD, 73-84.
#'
#'   Kaufman, L. and Rousseeuw, P. J. (1990). Finding Groups in Data. Wiley.
#'
#'   Karypis, G., Han, E.-H. and Kumar, V. (1999). CHAMELEON: a hierarchical
#'   clustering algorithm using dynamic modeling. IEEE Computer 32, 68-75.
#' @examples
#' X <- rbind(c(0, 0), c(0, 1), c(1, 0), c(8, 8), c(8, 9), c(9, 8))
#' AffinityPropagation(X)$cluster
#' Clarans(X, 2)$cluster
#' CureClustering(X, 2)$cluster
#' Diana(as.matrix(dist(X)), 2)$cluster
#' @export
AffinityPropagation <- function(X = NULL, S = NULL, preference = NULL, damping = 0.9, maxits = 1000L, convits = 100L) {
  if (is.null(S)) {
    X <- as.matrix(X)
    S <- -as.matrix(stats::dist(X))^2
  }
  S <- unname(as.matrix(S)) + 0
  n <- nrow(S)
  if (is.null(preference)) {
    off <- sort(S[row(S) != col(S)])
    m <- length(off)
    preference <- if (m %% 2 == 1) off[m %/% 2 + 1] else 0.5 * (off[m %/% 2] + off[m %/% 2 + 1])
  }
  diag(S) <- preference
  lam <- damping
  R <- matrix(0, n, n)
  A <- matrix(0, n, n)
  hist <- list()
  converged <- FALSE
  it <- 0L
  for (it in seq_len(maxits)) {
    for (i in seq_len(n)) {
      AS <- A[i, ] + S[i, ]
      k1 <- which.max(AS)
      y1 <- AS[k1]
      y2 <- if (n > 1) max(AS[-k1]) else -Inf
      new <- S[i, ] - y1
      new[k1] <- S[i, k1] - y2
      R[i, ] <- lam * R[i, ] + (1 - lam) * new
    }
    for (k in seq_len(n)) {
      rp <- pmax(R[, k], 0)
      rp[k] <- R[k, k]
      col <- 0
      for (i in seq_len(n)) col <- col + rp[i]
      new <- col - rp
      new[-k] <- pmin(new[-k], 0)
      A[, k] <- lam * A[, k] + (1 - lam) * new
    }
    E <- diag(A) + diag(R) > 0
    hist[[length(hist) + 1]] <- E
    if (length(hist) > convits) hist[[1]] <- NULL
    if (it >= convits && all(vapply(hist, function(h) all(h == E), TRUE)) && any(E)) {
      converged <- TRUE
      break
    }
  }
  ex <- which(diag(A) + diag(R) > 0)
  if (!length(ex)) return(list(cluster = rep(0L, n), exemplars = integer(0), iterations = it, converged = converged))
  assign <- function(ex) {
    cl <- vapply(seq_len(n), function(i) which.max(S[i, ex]), 1L)
    cl[ex] <- seq_along(ex)
    cl
  }
  cl <- assign(ex)
  for (j in seq_along(ex)) {
    mem <- which(cl == j)
    cs <- vapply(mem, function(q) {
      s <- 0
      for (i in mem) s <- s + S[i, q]
      s
    }, 0)
    ex[j] <- mem[which.max(cs)]
  }
  cl <- assign(ex)
  exof <- ex[cl]
  ord <- sort(unique(exof))
  list(cluster = match(exof, ord), exemplars = ord, iterations = it, converged = converged, preference = preference)
}

.ca_first <- function(lab) match(lab, unique(lab))

.ca_unif <- function(seed) {
  env <- new.env()
  env$block <- 0
  env$buf <- numeric(0)
  env$pos <- 0
  function() {
    if (env$pos >= length(env$buf)) {
      env$buf <- .morie_random_uniform(4096, seed = seed, stream = env$block)
      env$block <- env$block + 1
      env$pos <- 0
    }
    env$pos <- env$pos + 1
    env$buf[env$pos]
  }
}

.ca_ss <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

#' @rdname AffinityPropagation
#' @export
Clarans <- function(X, k, numlocal = 2L, maxneighbor = NULL, seed = 1) {
  X <- as.matrix(X)
  n <- nrow(X)
  if (k < 1 || k >= n) stop("need 1 <= k < n")
  D <- as.matrix(stats::dist(X))
  if (is.null(maxneighbor)) maxneighbor <- max(250, floor(0.0125 * k * (n - k)))
  u <- .ca_unif(seed)
  cost <- function(med) .ca_ss(vapply(seq_len(n), function(i) min(D[i, med]), 0))
  best <- NULL
  best_cost <- Inf
  for (loc in seq_len(numlocal)) {
    cur <- integer(0)
    while (length(cur) < k) {
      j <- min(floor(u() * n), n - 1) + 1
      if (!j %in% cur) cur <- c(cur, j)
    }
    cc <- cost(cur)
    fails <- 0
    while (fails < maxneighbor) {
      pos <- min(floor(u() * k), k - 1) + 1
      others <- setdiff(seq_len(n), cur)
      cand <- others[min(floor(u() * length(others)), length(others) - 1) + 1]
      nb <- cur
      nb[pos] <- cand
      nc <- cost(nb)
      if (nc < cc) {
        cur <- nb
        cc <- nc
        fails <- 0
      } else {
        fails <- fails + 1
      }
    }
    if (cc < best_cost) {
      best <- sort(cur)
      best_cost <- cc
    }
  }
  lab <- vapply(seq_len(n), function(i) which.min(D[i, best]), 1L)
  list(medoids = best, cluster = .ca_first(lab), cost = best_cost)
}

#' @rdname AffinityPropagation
#' @export
CureClustering <- function(X, k, n_rep = 5L, alpha = 0.3) {
  X <- as.matrix(X)
  n <- nrow(X)
  if (k < 1 || k > n) stop("need 1 <= k <= n")
  dd <- function(a, b) sqrt(sum((a - b)^2))
  reps <- function(mem) {
    mu <- colMeans(X[mem, , drop = FALSE])
    chosen <- integer(0)
    for (r in seq_len(min(n_rep, length(mem)))) {
      if (!length(chosen)) {
        far <- mem[which.max(vapply(mem, function(i) dd(X[i, ], mu), 0))]
      } else {
        cand <- setdiff(mem, chosen)
        far <- cand[which.max(vapply(cand, function(i) min(vapply(chosen, function(c) dd(X[i, ], X[c, ]), 0)), 0))]
      }
      chosen <- c(chosen, far)
    }
    t(vapply(chosen, function(c) X[c, ] + alpha * (mu - X[c, ]), numeric(ncol(X))))
  }
  clusters <- as.list(seq_len(n))
  R <- lapply(clusters, function(c) matrix(reps(c), ncol = ncol(X)))
  while (length(clusters) > k) {
    best <- NULL
    bd <- Inf
    m <- length(clusters)
    for (a in seq_len(m - 1)) {
      for (b in (a + 1):m) {
        d <- Inf
        for (i in seq_len(nrow(R[[a]]))) for (j in seq_len(nrow(R[[b]]))) d <- min(d, dd(R[[a]][i, ], R[[b]][j, ]))
        if (d < bd) {
          best <- c(a, b)
          bd <- d
        }
      }
    }
    a <- best[1]
    b <- best[2]
    clusters[[a]] <- sort(c(clusters[[a]], clusters[[b]]))
    R[[a]] <- matrix(reps(clusters[[a]]), ncol = ncol(X))
    clusters[[b]] <- NULL
    R[[b]] <- NULL
  }
  lab <- integer(n)
  for (j in seq_along(clusters)) lab[clusters[[j]]] <- j
  list(cluster = .ca_first(lab), representatives = R)
}

#' @rdname AffinityPropagation
#' @export
Diana <- function(D, k = NULL, is_distance = TRUE) {
  M <- if (is_distance) unname(as.matrix(D)) + 0 else as.matrix(stats::dist(as.matrix(D)))
  n <- nrow(M)
  diam <- function(c) if (length(c)) max(M[c, c]) else 0
  splits <- list()
  last <- numeric(n)
  stack <- list(seq_len(n))
  while (length(stack)) {
    c <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    if (length(c) < 2) next
    dm <- diam(c)
    last[c] <- dm
    avg <- vapply(c, function(i) .ca_ss(M[i, setdiff(c, i)]) / (length(c) - 1), 0)
    s0 <- c[which.max(avg)]
    spl <- s0
    rest <- setdiff(c, s0)
    while (length(rest) > 1) {
      diff <- vapply(rest, function(i) .ca_ss(M[i, setdiff(rest, i)]) / (length(rest) - 1) - .ca_ss(M[i, spl]) / length(spl), 0)
      t <- which.max(diff)
      if (diff[t] <= 0) break
      spl <- c(spl, rest[t])
      rest <- rest[-t]
    }
    splits[[length(splits) + 1]] <- list(members = sort(c), diameter = dm, parts = list(sort(rest), sort(spl)))
    stack[[length(stack) + 1]] <- sort(spl)
    stack[[length(stack) + 1]] <- sort(rest)
  }
  full <- diam(seq_len(n))
  dc <- if (full > 0) .ca_ss(1 - last / full) / n else 0
  dmv <- vapply(splits, function(s) s$diameter, 0)
  splits <- splits[order(-dmv, seq_along(dmv))]
  out <- list(splits = splits, dc = dc)
  if (!is.null(k)) {
    if (k < 1 || k > n) stop("k must be in 1..n")
    lab <- integer(n)
    nxt <- 1L
    for (s in splits[seq_len(k - 1)]) {
      lab[s$parts[[2]]] <- nxt
      nxt <- nxt + 1L
    }
    out$cluster <- .ca_first(lab)
  }
  out
}

.ca_fiedler <- function(nodes, W) {
  m <- length(nodes)
  seen <- nodes[1]
  queue <- nodes[1]
  while (length(queue)) {
    u <- queue[1]
    queue <- queue[-1]
    for (v in nodes) {
      if (!v %in% seen && W[u, v] > 0) {
        seen <- c(seen, v)
        queue <- c(queue, v)
      }
    }
  }
  if (length(seen) < m) return(list(A = nodes[nodes %in% seen], B = nodes[!nodes %in% seen]))
  Wm <- W[nodes, nodes, drop = FALSE]
  L <- -Wm
  for (a in seq_len(m)) {
    L[a, a] <- 0
    L[a, a] <- .ca_ss(Wm[a, -a])
  }
  e <- .s03jacobi(L)
  f <- e$vectors[, 2]
  ord <- order(f, seq_len(m))
  half <- ord[seq_len(m %/% 2)]
  in_a <- seq_len(m) %in% half
  list(A = nodes[in_a], B = nodes[!in_a])
}

.ca_cut <- function(A, B, W) {
  w <- W[A, B]
  w <- w[w > 0]
  c(length(w), .ca_ss(t(W[A, B])[t(W[A, B]) > 0]))
}

#' @rdname AffinityPropagation
#' @export
Chameleon <- function(X, k, n_neighbors = 5L, min_size = 6L, alpha = 2) {
  X <- as.matrix(X)
  n <- nrow(X)
  if (k < 1 || k > n) stop("need 1 <= k <= n")
  D <- as.matrix(stats::dist(X))
  W <- matrix(0, n, n)
  for (i in seq_len(n)) {
    oth <- setdiff(seq_len(n), i)
    nn <- oth[order(D[i, oth], oth)][seq_len(n_neighbors)]
    for (j in nn) W[i, j] <- W[j, i] <- 1 / (1 + D[i, j])
  }
  parts <- list()
  stack <- list(seq_len(n))
  while (length(stack)) {
    c <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    if (length(c) <= min_size) {
      parts[[length(parts) + 1]] <- sort(c)
      next
    }
    ab <- .ca_fiedler(c, W)
    stack[[length(stack) + 1]] <- ab$B
    stack[[length(stack) + 1]] <- ab$A
  }
  internal <- function(c) {
    if (length(c) < 2) return(c(0, 0))
    ab <- .ca_fiedler(c, W)
    ct <- .ca_cut(ab$A, ab$B, W)
    c(ct[2], if (ct[1] > 0) ct[2] / ct[1] else 0)
  }
  info <- lapply(parts, internal)
  eps <- 1e-12
  while (length(parts) > k) {
    best <- NULL
    bs <- -Inf
    m <- length(parts)
    for (a in seq_len(m - 1)) {
      for (b in (a + 1):m) {
        ct <- .ca_cut(parts[[a]], parts[[b]], W)
        if (ct[1] == 0) next
        na <- length(parts[[a]])
        nb <- length(parts[[b]])
        ri <- ct[2] / ((info[[a]][1] + info[[b]][1]) / 2 + eps)
        rc <- (ct[2] / ct[1]) / (na / (na + nb) * info[[a]][2] + nb / (na + nb) * info[[b]][2] + eps)
        sc <- ri * rc^alpha
        if (sc > bs) {
          best <- c(a, b)
          bs <- sc
        }
      }
    }
    if (is.null(best)) break
    a <- best[1]
    b <- best[2]
    parts[[a]] <- sort(c(parts[[a]], parts[[b]]))
    info[[a]] <- internal(parts[[a]])
    parts[[b]] <- NULL
    info[[b]] <- NULL
  }
  lab <- integer(n)
  for (j in seq_along(parts)) lab[parts[[j]]] <- j
  list(cluster = .ca_first(lab), n_clusters = length(parts))
}
