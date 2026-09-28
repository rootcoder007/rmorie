# SPDX-License-Identifier: AGPL-3.0-or-later
# Graphs, CDCL satisfiability and linear-chain conditional random fields.
# Identical to the Python arm morie.fn.graphlogic.

#' Graphs, SAT and CRFs: degree centrality, diameter, betweenness, CDCL, linear-chain CRF
#'
#' \code{GraphDegreeCentrality}: Freeman \code{deg(v) / (n - 1)} (mode "out",
#' "in" or "all"). \code{GraphDiameter}: largest finite shortest-path distance
#' (BFS hop counts, or Dijkstra with \code{weighted}), eccentricities and a
#' farthest pair (1-based). \code{GraphBetweenness}: Brandes accumulation of
#' pair dependencies, halved for undirected graphs, optionally normalised as
#' igraph. \code{CdclSolve}: conflict-driven clause learning with scanning unit
#' propagation, VSIDS decisions (false first, ties to the lowest index),
#' first-UIP learning and non-chronological backjumping. \code{CrfMarginals},
#' \code{CrfViterbi} and \code{CrfFit}: linear-chain CRF with parameter vector
#' packing emission weights (row-major \code{K x d}), label biases and
#' transitions (row-major \code{K x K}); forward-backward marginals, Viterbi
#' decoding (0-based labels) and L2-penalised maximum likelihood by L-BFGS.
#'
#' @param A Adjacency (or edge-length) matrix; nonzero means an edge.
#' @param mode "all", "out" or "in".
#' @param weighted Use edge lengths (Dijkstra) instead of hop counts.
#' @param normalized Normalise betweenness.
#' @param cnf List of clauses (integer vectors of literals).
#' @param n_vars Number of variables, or NULL for the largest index used.
#' @param max_conflicts Conflict budget.
#' @param theta CRF parameter vector.
#' @param X Feature matrix of one sequence (rows are positions).
#' @param n_labels Number of labels.
#' @param sequences List of feature matrices.
#' @param labels List of 0-based label vectors.
#' @param l2 Ridge penalty.
#' @param max_iter L-BFGS iterations.
#' @return A vector or list.
#' @references Freeman, L. C. (1979). Social Networks 1, 215-239. Newman, M.
#'   E. J. (2010). Networks: An Introduction. Brandes, U. (2001). J. Math.
#'   Sociology 25, 163-177. Marques-Silva, J. P. and Sakallah, K. A. (1999).
#'   IEEE Trans. Computers 48, 506-521. Lafferty, J., McCallum, A. and Pereira,
#'   F. (2001). ICML 2001, 282-289.
#' @examples
#' GraphDegreeCentrality(rbind(c(0, 1, 1), c(1, 0, 0), c(1, 0, 0)))
#' CdclSolve(list(c(1, 2), c(-1, 2), c(1, -2), c(-1, -2, 3)))$model
#' @export
GraphDegreeCentrality <- function(A, mode = "all") {
  M <- unname(as.matrix(A)) * 1
  n <- nrow(M)
  sym <- isTRUE(all(M == t(M)))
  B <- (M != 0) * 1
  diag(B) <- 0
  o <- rowSums(B)
  cc <- colSums(B)
  d <- if (mode == "out") o else if (mode == "in") cc else if (sym) o else o + cc
  as.numeric(d / (n - 1))
}

.gl_sssp <- function(M, s, weighted) {
  n <- nrow(M)
  dist <- rep(Inf, n)
  sigma <- numeric(n)
  preds <- vector("list", n)
  for (i in seq_len(n)) preds[[i]] <- integer(0)
  dist[s] <- 0
  sigma[s] <- 1
  ord <- integer(0)
  if (!weighted) {
    queue <- s
    k <- 1
    while (k <= length(queue)) {
      v <- queue[k]
      k <- k + 1
      ord <- c(ord, v)
      for (w in seq_len(n)) if (M[v, w] != 0 && w != v) {
        if (dist[w] == Inf) {
          dist[w] <- dist[v] + 1
          queue <- c(queue, w)
        }
        if (dist[w] == dist[v] + 1) {
          sigma[w] <- sigma[w] + sigma[v]
          preds[[w]] <- c(preds[[w]], v)
        }
      }
    }
    return(list(dist = dist, sigma = sigma, preds = preds, order = ord))
  }
  done <- rep(FALSE, n)
  repeat {
    v <- -1
    best <- Inf
    for (u in seq_len(n)) if (!done[u] && dist[u] < best) {
      v <- u
      best <- dist[u]
    }
    if (v < 0) break
    done[v] <- TRUE
    ord <- c(ord, v)
    for (w in seq_len(n)) if (M[v, w] != 0 && w != v && !done[w]) {
      nd <- dist[v] + M[v, w]
      if (nd < dist[w]) {
        dist[w] <- nd
        sigma[w] <- sigma[v]
        preds[[w]] <- v
      } else if (nd == dist[w]) {
        sigma[w] <- sigma[w] + sigma[v]
        preds[[w]] <- c(preds[[w]], v)
      }
    }
  }
  list(dist = dist, sigma = sigma, preds = preds, order = ord)
}

#' @rdname GraphDegreeCentrality
#' @export
GraphDiameter <- function(A, weighted = FALSE) {
  M <- unname(as.matrix(A)) * 1
  n <- nrow(M)
  ecc <- numeric(n)
  best <- -1
  pair <- c(1, 1)
  for (s in seq_len(n)) {
    d <- .gl_sssp(M, s, weighted)$dist
    e <- max(d[is.finite(d)])
    ecc[s] <- e
    if (e > best) {
      best <- e
      pair <- c(s, which(d == e)[1])
    }
  }
  list(diameter = best, eccentricity = ecc, pair = pair)
}

#' @rdname GraphDegreeCentrality
#' @export
GraphBetweenness <- function(A, weighted = FALSE, normalized = FALSE) {
  M <- unname(as.matrix(A)) * 1
  n <- nrow(M)
  sym <- isTRUE(all(M == t(M)))
  cb <- numeric(n)
  for (s in seq_len(n)) {
    r <- .gl_sssp(M, s, weighted)
    delta <- numeric(n)
    for (w in rev(r$order)) {
      for (v in r$preds[[w]]) delta[v] <- delta[v] + r$sigma[v] / r$sigma[w] * (1 + delta[w])
      if (w != s) cb[w] <- cb[w] + delta[w]
    }
  }
  if (sym) cb <- cb / 2
  if (normalized && n > 2) cb <- cb / ((n - 1) * (n - 2) / (if (sym) 2 else 1))
  cb
}

#' @rdname GraphDegreeCentrality
#' @export
CdclSolve <- function(cnf, n_vars = NULL, max_conflicts = 100000) {
  st <- new.env()
  st$clauses <- lapply(cnf, as.integer)
  nv <- if (!is.null(n_vars)) n_vars else max(c(0, abs(unlist(st$clauses))))
  st$val <- integer(nv)
  st$level <- integer(nv)
  st$reason <- rep(-1L, nv)
  st$trail <- integer(0)
  act <- numeric(nv)
  stats <- list(decisions = 0, conflicts = 0, learned = 0)
  lit_val <- function(x) {
    v <- st$val[abs(x)]
    if (v == 0) 0L else if (x > 0) v else -v
  }
  assign <- function(x, lev, why) {
    st$val[abs(x)] <- if (x > 0) 1L else -1L
    st$level[abs(x)] <- lev
    st$reason[abs(x)] <- why
    st$trail <- c(st$trail, x)
  }
  propagate <- function(lev) {
    changed <- TRUE
    while (changed) {
      changed <- FALSE
      for (ci in seq_along(st$clauses)) {
        cl <- st$clauses[[ci]]
        unassigned <- integer(0)
        sat <- FALSE
        for (x in cl) {
          lv <- lit_val(x)
          if (lv == 1) {
            sat <- TRUE
            break
          }
          if (lv == 0) unassigned <- c(unassigned, x)
        }
        if (sat) next
        if (!length(unassigned)) return(ci)
        if (length(unassigned) == 1) {
          assign(unassigned, lev, ci)
          changed <- TRUE
        }
      }
    }
    -1L
  }
  analyze <- function(ci, lev) {
    learned <- st$clauses[[ci]]
    repeat {
      cur <- learned[st$level[abs(learned)] == lev]
      if (length(cur) <= 1) break
      for (t in rev(st$trail)) if ((-t) %in% learned && st$level[abs(t)] == lev) {
        pivot <- t
        break
      }
      r <- st$clauses[[st$reason[abs(pivot)]]]
      merged <- learned[learned != -pivot]
      for (x in r) if (x != pivot && !(x %in% merged)) merged <- c(merged, x)
      learned <- merged
    }
    others <- st$level[abs(learned)][st$level[abs(learned)] != lev]
    list(learned = learned, back = if (length(others)) max(others) else 0L)
  }
  done <- function(sat) {
    model <- if (sat) ifelse(st$val == 1L, seq_len(nv), -seq_len(nv)) else integer(0)
    c(list(satisfiable = sat, model = model), stats)
  }
  if (any(lengths(st$clauses) == 0)) return(done(FALSE))
  lev <- 0L
  repeat {
    ci <- propagate(lev)
    if (ci >= 0) {
      stats$conflicts <- stats$conflicts + 1
      if (lev == 0 || stats$conflicts > max_conflicts) return(done(FALSE))
      an <- analyze(ci, lev)
      learned <- an$learned
      for (x in learned) act[abs(x)] <- act[abs(x)] + 1
      act <- act * 0.95
      while (length(st$trail) && st$level[abs(st$trail[length(st$trail)])] > an$back) {
        x <- st$trail[length(st$trail)]
        st$trail <- st$trail[-length(st$trail)]
        st$val[abs(x)] <- 0L
        st$reason[abs(x)] <- -1L
      }
      st$clauses <- c(st$clauses, list(learned))
      stats$learned <- stats$learned + 1
      lev <- an$back
      unit <- learned[vapply(learned, lit_val, 0L) == 0]
      if (length(unit) == 1) assign(unit, lev, length(st$clauses))
      next
    }
    free <- which(st$val == 0L)
    if (!length(free)) return(done(TRUE))
    v <- free[which.max(act[free])]
    lev <- lev + 1L
    stats$decisions <- stats$decisions + 1
    assign(-v, lev, -1L)
  }
}

.gl_lse <- function(v) {
  m <- max(v)
  if (m == -Inf) return(m)
  m + log(sum(exp(v - m)))
}

.gl_unpack <- function(theta, K, d) {
  list(W = matrix(theta[seq_len(K * d)], K, d, byrow = TRUE), b = theta[K * d + seq_len(K)],
       T = matrix(theta[K * d + K + seq_len(K * K)], K, K, byrow = TRUE))
}

.gl_fb <- function(E, Tm) {
  n <- nrow(E)
  K <- ncol(E)
  al <- matrix(0, n, K)
  al[1, ] <- E[1, ]
  if (n > 1) for (t in 2:n) for (k in seq_len(K)) al[t, k] <- E[t, k] + .gl_lse(al[t - 1, ] + Tm[, k])
  be <- matrix(0, n, K)
  if (n > 1) for (t in (n - 1):1) for (k in seq_len(K)) be[t, k] <- .gl_lse(Tm[k, ] + E[t + 1, ] + be[t + 1, ])
  list(al = al, be = be, lz = .gl_lse(al[n, ]))
}

.gl_emit <- function(p, X) {
  X <- matrix(as.numeric(X), nrow = NROW(X))
  E <- matrix(0, nrow(X), nrow(p$W))
  for (t in seq_len(nrow(X))) for (k in seq_len(nrow(p$W))) E[t, k] <- sum(p$W[k, ] * X[t, ]) + p$b[k]
  E
}

#' @rdname GraphDegreeCentrality
#' @export
CrfMarginals <- function(theta, X, n_labels) {
  X <- matrix(as.numeric(unlist(X)), nrow = if (is.list(X)) length(X) else NROW(X), byrow = is.list(X))
  p <- .gl_unpack(as.numeric(theta), n_labels, ncol(X))
  E <- .gl_emit(p, X)
  fb <- .gl_fb(E, p$T)
  n <- nrow(X)
  node <- exp(fb$al + fb$be - fb$lz)
  edge <- lapply(seq_len(n - 1) + 1, function(t) {
    outer(seq_len(n_labels), seq_len(n_labels), function(i, j) exp(fb$al[t - 1, i] + p$T[cbind(i, j)] + E[t, j] + fb$be[t, j] - fb$lz))
  })
  list(node = node, edge = edge, log_z = fb$lz)
}

#' @rdname GraphDegreeCentrality
#' @export
CrfViterbi <- function(theta, X, n_labels) {
  X <- matrix(as.numeric(unlist(X)), nrow = if (is.list(X)) length(X) else NROW(X), byrow = is.list(X))
  p <- .gl_unpack(as.numeric(theta), n_labels, ncol(X))
  E <- .gl_emit(p, X)
  n <- nrow(X)
  K <- n_labels
  dp <- matrix(0, n, K)
  bp <- matrix(0L, n, K)
  dp[1, ] <- E[1, ]
  if (n > 1) for (t in 2:n) for (k in seq_len(K)) {
    cand <- dp[t - 1, ] + p$T[, k]
    i <- which.max(cand)
    dp[t, k] <- cand[i] + E[t, k]
    bp[t, k] <- i
  }
  last <- which.max(dp[n, ])
  path <- last
  if (n > 1) for (t in n:2) path <- c(bp[t, path[1]], path)
  list(labels = path - 1L, score = dp[n, last])
}

#' @rdname GraphDegreeCentrality
#' @export
CrfFit <- function(sequences, labels, n_labels, l2 = 1, max_iter = 500) {
  seqs <- lapply(sequences, function(X) matrix(as.numeric(unlist(X)), nrow = if (is.list(X)) length(X) else NROW(X),
                                               byrow = is.list(X)))
  K <- n_labels
  d <- ncol(seqs[[1]])
  P <- K * d + K + K * K
  fg <- function(theta) {
    p <- .gl_unpack(theta, K, d)
    f <- 0.5 * l2 * sum(theta * theta)
    g <- l2 * theta
    for (s in seq_along(seqs)) {
      X <- seqs[[s]]
      y <- labels[[s]] + 1
      E <- .gl_emit(p, X)
      fb <- .gl_fb(E, p$T)
      n <- nrow(X)
      obs <- sum(E[cbind(seq_len(n), y)]) + (if (n > 1) sum(p$T[cbind(y[-n], y[-1])]) else 0)
      f <- f + fb$lz - obs
      for (t in seq_len(n)) for (k in seq_len(K)) {
        pk <- exp(fb$al[t, k] + fb$be[t, k] - fb$lz) - (if (y[t] == k) 1 else 0)
        g[(k - 1) * d + seq_len(d)] <- g[(k - 1) * d + seq_len(d)] + pk * X[t, ]
        g[K * d + k] <- g[K * d + k] + pk
      }
      if (n > 1) for (t in 2:n) for (i in seq_len(K)) for (j in seq_len(K)) {
        pe <- exp(fb$al[t - 1, i] + p$T[i, j] + E[t, j] + fb$be[t, j] - fb$lz)
        idx <- K * d + K + (i - 1) * K + j
        g[idx] <- g[idx] + pe - (if (y[t - 1] == i && y[t] == j) 1 else 0)
      }
    }
    list(f = f, g = g)
  }
  cache <- new.env()
  get <- function(theta) {
    if (is.null(cache$key) || !identical(cache$key, theta)) {
      cache$key <- theta
      cache$val <- fg(theta)
    }
    cache$val
  }
  res <- LbfgsbMinimize(function(v) get(v)$f, numeric(P), grad = function(v) get(v)$g, pgtol = 1e-10, factr = 10,
                        max_iter = max_iter)
  list(theta = as.numeric(res$x), objective = res$fun, n_labels = K, n_features = d)
}
