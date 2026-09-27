# SPDX-License-Identifier: AGPL-3.0-or-later
.lsum <- function(x) {
  s <- 0
  for (v in x) s <- s + v
  s
}

.leiden_take <- function(st, n) {
  st$stream <- st$stream + 1
  if (n == 0) numeric(0) else .morie_random_uniform(n, seed = st$seed, stream = st$stream)
}

.leiden_move <- function(adj, w, r, comm, st) {
  n <- length(adj)
  size <- max(n, max(comm) + 1)
  tot <- numeric(size)
  occ <- logical(size)
  for (v in seq_len(n)) {
    tot[comm[v] + 1] <- tot[comm[v] + 1] + w[v]
    occ[comm[v] + 1] <- TRUE
  }
  free <- setdiff(0:(n - 1), which(occ) - 1)
  us <- .leiden_take(st, n)
  queue <- order(us, seq_len(n))
  inq <- rep(TRUE, n)
  head <- 1L
  while (head <= length(queue)) {
    v <- queue[head]
    head <- head + 1L
    inq[v] <- FALSE
    cur <- comm[v]
    tot[cur + 1] <- tot[cur + 1] - w[v]
    if (tot[cur + 1] <= 0 && !(cur %in% free)) free <- c(free, cur)
    lk <- numeric(0)
    lc <- numeric(0)
    for (q in seq_along(adj[[v]]$u)) {
      u <- adj[[v]]$u[q]
      if (u != v) {
        p <- match(comm[u], lc)
        if (is.na(p)) {
          lc <- c(lc, comm[u])
          lk <- c(lk, adj[[v]]$a[q])
        } else {
          lk[p] <- lk[p] + adj[[v]]$a[q]
        }
      }
    }
    p <- match(cur, lc)
    best <- cur
    bgain <- (if (is.na(p)) 0 else lk[p]) - r * w[v] * tot[cur + 1]
    for (i in order(lc)) {
      g <- lk[i] - r * w[v] * tot[lc[i] + 1]
      if (g > bgain + 1e-12) {
        best <- lc[i]
        bgain <- g
      }
    }
    if (bgain < -1e-12) best <- min(free)
    free <- free[free != best]
    comm[v] <- best
    tot[best + 1] <- tot[best + 1] + w[v]
    if (best != cur) {
      for (u in adj[[v]]$u) {
        if (u != v && comm[u] != best && !inq[u]) {
          queue <- c(queue, u)
          inq[u] <- TRUE
        }
      }
    }
  }
  comm
}

.leiden_refine <- function(adj, w, r, comm, theta, st) {
  n <- length(adj)
  ref <- 0:(n - 1)
  for (c in sort(unique(comm))) {
    S <- which(comm == c)
    inS <- logical(n)
    inS[S] <- TRUE
    WS <- .lsum(w[S])
    ext <- numeric(n)
    for (v in S) {
      e <- 0
      for (q in seq_along(adj[[v]]$u)) {
        u <- adj[[v]]$u[q]
        if (inS[u] && u != v) e <- e + adj[[v]]$a[q]
      }
      ext[v] <- e
    }
    cw <- w
    cext <- ext
    single <- rep(TRUE, n)
    us <- .leiden_take(st, 2 * length(S))
    ord <- order(us[seq_along(S)], seq_along(S))
    for (t in seq_along(ord)) {
      v <- S[ord[t]]
      if (!single[v] || ext[v] < r * w[v] * (WS - w[v]) - 1e-12) next
      lk <- numeric(0)
      lc <- numeric(0)
      for (q in seq_along(adj[[v]]$u)) {
        u <- adj[[v]]$u[q]
        if (inS[u] && u != v) {
          p <- match(ref[u], lc)
          if (is.na(p)) {
            lc <- c(lc, ref[u])
            lk <- c(lk, adj[[v]]$a[q])
          } else {
            lk[p] <- lk[p] + adj[[v]]$a[q]
          }
        }
      }
      cand <- ref[v]
      gains <- 0
      for (i in order(lc)) {
        C <- lc[i]
        if (cext[C + 1] < r * cw[C + 1] * (WS - cw[C + 1]) - 1e-12) next
        g <- lk[i] - r * w[v] * cw[C + 1]
        if (g >= 0) {
          cand <- c(cand, C)
          gains <- c(gains, g)
        }
      }
      gm <- max(gains)
      ps <- exp((gains - gm) / theta)
      x <- us[length(S) + t] * .lsum(ps)
      acc <- 0
      pick <- cand[length(cand)]
      for (i in seq_along(cand)) {
        acc <- acc + ps[i]
        if (x < acc) {
          pick <- cand[i]
          break
        }
      }
      old <- ref[v]
      if (pick == old) next
      cw[old + 1] <- cw[old + 1] - w[v]
      ref[v] <- pick
      single[v] <- FALSE
      single[S[ref[S] == pick]] <- FALSE
      cext[pick + 1] <- cext[pick + 1] + ext[v] - 2 * lk[match(pick, lc)]
      cw[pick + 1] <- cw[pick + 1] + w[v]
    }
  }
  ref
}

.leiden_canon <- function(lab) match(lab, unique(lab)) - 1L

.leiden_quality <- function(W, lab, gamma, quality) {
  n <- nrow(W)
  e <- 0
  for (i in seq_len(n)) for (j in seq_len(n)) if (lab[i] == lab[j]) e <- e + W[i, j]
  e <- e / 2
  if (quality == "cpm") {
    s <- tabulate(lab + 1L)
    s <- s[s > 0]
    return(e - gamma * .lsum(s * (s - 1) / 2))
  }
  m2 <- 0
  for (i in seq_len(n)) m2 <- m2 + .lsum(W[i, ])
  K <- numeric(max(lab) + 1)
  for (i in seq_len(n)) K[lab[i] + 1] <- K[lab[i] + 1] + .lsum(W[i, ])
  2 * e / m2 - gamma * .lsum(K^2) / (m2 * m2)
}

.leiden_connected <- function(W, lab) {
  for (c in unique(lab)) {
    mem <- which(lab == c)
    seen <- mem[1]
    stack <- mem[1]
    while (length(stack)) {
      u <- stack[length(stack)]
      stack <- stack[-length(stack)]
      for (v in mem) {
        if (!(v %in% seen) && W[u, v] > 0) {
          seen <- c(seen, v)
          stack <- c(stack, v)
        }
      }
    }
    if (length(seen) != length(mem)) return(FALSE)
  }
  TRUE
}

#' Leiden community detection
#'
#' Traag, Waltman and van Eck (2019), Algorithm A.2: fast local moving
#' (queue-based, random visiting order), refinement inside each community
#' (well-connected singletons join well-connected sub-communities with
#' probability proportional to exp(dH / theta)) and aggregation of the refined
#' partition, repeated until every community is one aggregate node. Quality is
#' modularity (node weight = strength, r = gamma / 2m) or the constant Potts
#' model of the paper's eq. (2) (node weight 1, r = gamma); moving v into D
#' gains k_vD - r w_v W_D.
#'
#' Randomness: the visiting orders and refinement draws come from the morie
#' Philox stream (a new stream per phase, same order as the Python arm), so
#' runs agree exactly across arms.
#'
#' @param graph Symmetric non-negative weighted adjacency matrix.
#' @param resolution The resolution parameter gamma.
#' @param quality "modularity" or "cpm".
#' @param max_iter Leiden iterations; stops once an iteration changes nothing.
#' @param theta Randomness of the refinement merge.
#' @param seed Philox seed.
#' @return list: labels, estimate, quality, n_communities, connected,
#'   passes, n, method.
#' @references Traag, V. A., Waltman, L. and van Eck, N. J. (2019). From Louvain
#'   to Leiden: guaranteeing well-connected communities. Scientific Reports 9,
#'   5233.
#' @keywords internal
#' @examples
#' A <- matrix(0, 6, 6)
#' A[cbind(c(1, 1, 2, 3, 4, 4, 5), c(2, 3, 3, 4, 5, 6, 6))] <- 1
#' Leidenclus(A + t(A))$labels
#' @export
Leidenclus <- function(graph, resolution = 1, quality = "modularity",
                       max_iter = 20, theta = 0.01, seed = 0) {
  quality <- match.arg(quality, c("modularity", "cpm"))
  W <- .s03mat(graph)
  W <- matrix(as.numeric(W), nrow(W))
  n <- nrow(W)
  if (n == 0 || ncol(W) != n) stop("graph must be a non-empty square matrix", call. = FALSE)
  if (any(W < 0) || max(abs(W - t(W))) > 1e-12) stop("graph must be symmetric with non-negative weights", call. = FALSE)
  g <- as.numeric(resolution)
  m2 <- 0
  for (i in seq_len(n)) m2 <- m2 + .lsum(W[i, ])
  lab <- 0:(n - 1)
  passes <- 0L
  if (m2 > 0) {
    st <- new.env()
    st$seed <- seed
    st$stream <- 0
    for (it in seq_len(as.integer(max_iter))) {
      passes <- passes + 1L
      adj <- lapply(seq_len(n), function(i) {
        u <- which(W[i, ] != 0)
        list(u = u, a = W[i, u])
      })
      w <- if (quality == "modularity") vapply(seq_len(n), function(i) .lsum(W[i, ]), 0) else rep(1, n)
      r <- if (quality == "modularity") g / m2 else g
      member <- as.list(seq_len(n))
      comm <- lab
      repeat {
        comm <- .leiden_move(adj, w, r, comm, st)
        if (length(unique(comm)) == length(adj)) break
        ref <- .leiden_refine(adj, w, r, comm, as.numeric(theta), st)
        rid <- match(ref, unique(ref))
        na <- max(rid)
        nadj <- replicate(na, list(u = integer(0), a = numeric(0)), simplify = FALSE)
        nw <- numeric(na)
        nmem <- vector("list", na)
        ncomm <- numeric(na)
        for (v in seq_along(adj)) {
          a <- rid[v]
          nw[a] <- nw[a] + w[v]
          nmem[[a]] <- c(nmem[[a]], member[[v]])
          ncomm[a] <- comm[v]
          for (q in seq_along(adj[[v]]$u)) {
            b <- rid[adj[[v]]$u[q]]
            p <- match(b, nadj[[a]]$u)
            if (is.na(p)) {
              nadj[[a]]$u <- c(nadj[[a]]$u, b)
              nadj[[a]]$a <- c(nadj[[a]]$a, adj[[v]]$a[q])
            } else {
              nadj[[a]]$a[p] <- nadj[[a]]$a[p] + adj[[v]]$a[q]
            }
          }
        }
        adj <- nadj
        w <- nw
        member <- nmem
        comm <- ncomm
      }
      new <- integer(n)
      for (i in seq_along(member)) new[member[[i]]] <- comm[i]
      new <- .leiden_canon(new)
      if (identical(as.integer(new), as.integer(lab))) break
      lab <- new
    }
  }
  lab <- as.integer(.leiden_canon(lab))
  q <- if (m2 > 0) .leiden_quality(W, lab, g, quality) else 0
  list(labels = lab, estimate = q, quality = q, n_communities = length(unique(lab)),
       connected = .leiden_connected(W, lab), passes = passes, n = n,
       method = "Leiden (Traag, Waltman and van Eck 2019, Algorithm A.2), Philox visiting order and refinement")
}
