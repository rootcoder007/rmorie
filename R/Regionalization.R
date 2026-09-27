.rg_ssd <- function(X, nodes, squared) {
  mu <- vapply(seq_len(ncol(X)), function(j) {
    s <- 0
    for (i in nodes) s <- s + X[i, j]
    s / length(nodes)
  }, 0)
  tot <- 0
  for (i in nodes) {
    q <- 0
    for (j in seq_len(ncol(X))) q <- q + (X[i, j] - mu[j])^2
    tot <- tot + (if (squared) q else sqrt(q))
  }
  tot
}

.rg_split <- function(edges, cut) {
  a <- edges[cut, 1]
  b <- edges[cut, 2]
  rest <- edges[-cut, , drop = FALSE]
  nodes <- sort(unique(c(as.vector(rest), a, b)))
  seen <- a
  stack <- a
  while (length(stack)) {
    u <- stack[length(stack)]
    stack <- stack[-length(stack)]
    nbrs <- c(rest[rest[, 1] == u, 2], rest[rest[, 2] == u, 1])
    for (v in nbrs) {
      if (!(v %in% seen)) {
        seen <- c(seen, v)
        stack <- c(stack, v)
      }
    }
  }
  inA <- rest[, 1] %in% seen
  list(list(nodes = sort(seen), edges = rest[inA, , drop = FALSE]),
       list(nodes = setdiff(nodes, seen), edges = rest[!inA, , drop = FALSE]))
}

.rg_nb <- function(adjacency, n) {
  if (is.matrix(adjacency)) lapply(seq_len(n), function(i) setdiff(which(adjacency[i, ] != 0), i)) else lapply(adjacency, as.integer)
}

#' SKATER regionalization
#'
#' Prunes the minimum spanning tree of the contiguity graph (edge weight = attribute
#' distance, Prim's algorithm from \code{start}) k - 1 times; each cut takes, over all
#' current subtrees, the edge whose removal most reduces the within-group
#' dissimilarity, in decreasing order until both parts respect \code{min_size} and
#' \code{min_weight}. The dissimilarity is the sum of Euclidean distances to the group
#' mean as in \code{spdep::skater}, or the sum of squared deviations when
#' \code{squared = TRUE}.
#'
#' @param X Attribute matrix (units in rows).
#' @param adjacency 0/1 contiguity matrix or list of 1-based neighbour vectors.
#' @param k Number of regions.
#' @param min_size Minimum units per region.
#' @param weights Per-unit weights for \code{min_weight}.
#' @param min_weight Minimum summed weight per region.
#' @param squared Use squared deviations.
#' @param start 1-based root of Prim's algorithm.
#' @return list(labels (1-based), mst, ssw, n_regions).
#' @references Assuncao, R. M., Neves, M. C., Camara, G. and da Costa Freitas, C.
#'   (2006). Efficient regionalization techniques for socio-economic geographical
#'   units using minimum spanning trees. International Journal of Geographical
#'   Information Science 20, 797-811.
#' @examples
#' X <- matrix(c(0, 0.1, 0.2, 5, 5.1, 5.2))
#' path <- list(2, c(1, 3), c(2, 4), c(3, 5), c(4, 6), 5)
#' Skater(X, path, 2)$labels
#' @export
Skater <- function(X, adjacency, k, min_size = 1, weights = NULL, min_weight = NULL, squared = FALSE, start = 1) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  nb <- .rg_nb(adjacency, n)
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  wmin <- if (is.null(min_weight)) -Inf else min_weight
  dd <- function(i, j) sqrt(sum((X[i, ] - X[j, ])^2))
  inside <- rep(FALSE, n)
  best <- rep(Inf, n)
  par <- integer(n)
  inside[start] <- TRUE
  for (j in nb[[start]]) {
    best[j] <- dd(start, j)
    par[j] <- start
  }
  mst <- matrix(0L, 0, 2)
  for (it in seq_len(n - 1)) {
    cand <- which(!inside)
    u <- cand[order(best[cand], cand)[1]]
    if (!is.finite(best[u])) stop("the contiguity graph is not connected", call. = FALSE)
    inside[u] <- TRUE
    mst <- rbind(mst, c(par[u], u))
    for (j in nb[[u]]) {
      dj <- dd(u, j)
      if (!inside[j] && dj < best[j]) {
        best[j] <- dj
        par[j] <- u
      }
    }
  }
  groups <- list(list(nodes = seq_len(n), edges = mst))
  ok <- function(nodes) {
    s <- 0
    for (i in nodes) s <- s + w[i]
    length(nodes) >= min_size && s >= wmin
  }
  hist <- .rg_ssd(X, seq_len(n), squared)
  while (length(groups) < k) {
    cands <- matrix(0, 0, 3)
    for (gi in seq_along(groups)) {
      E <- groups[[gi]]$edges
      if (!nrow(E)) next
      base <- .rg_ssd(X, groups[[gi]]$nodes, squared)
      for (ei in seq_len(nrow(E))) {
        sp <- .rg_split(E, ei)
        cands <- rbind(cands, c(base - .rg_ssd(X, sp[[1]]$nodes, squared) - .rg_ssd(X, sp[[2]]$nodes, squared), gi, ei))
      }
    }
    if (!nrow(cands)) break
    cands <- cands[order(-cands[, 1], cands[, 2], cands[, 3]), , drop = FALSE]
    done <- FALSE
    for (r in seq_len(nrow(cands))) {
      sp <- .rg_split(groups[[cands[r, 2]]]$edges, cands[r, 3])
      if (ok(sp[[1]]$nodes) && ok(sp[[2]]$nodes)) {
        groups[[cands[r, 2]]] <- sp[[1]]
        groups[[length(groups) + 1]] <- sp[[2]]
        done <- TRUE
        break
      }
    }
    if (!done) break
    tot <- 0
    for (g in groups) tot <- tot + .rg_ssd(X, g$nodes, squared)
    hist <- c(hist, tot)
  }
  labels <- integer(n)
  for (gi in seq_along(groups)) labels[groups[[gi]]$nodes] <- gi
  list(labels = labels, mst = mst, ssw = hist, n_regions = length(groups))
}

.rg_sqd <- function(a, b) {
  s <- 0
  for (q in seq_along(a)) s <- s + (a[q] - b[q])^2
  s
}

.rg_ssq <- function(X, nodes) {
  tot <- 0
  for (j in seq_len(ncol(X))) {
    m <- 0
    for (i in nodes) m <- m + X[i, j]
    m <- m / length(nodes)
    for (i in nodes) tot <- tot + (X[i, j] - m)^2
  }
  tot
}

.rg_canon <- function(lab) match(lab, unique(lab))

.rg_partition <- function(X, tree, k, ok) {
  n <- nrow(X)
  groups <- list(list(nodes = seq_len(n), edges = tree))
  hist <- .rg_ssq(X, seq_len(n))
  while (length(groups) < k) {
    cands <- matrix(0, 0, 3)
    for (gi in seq_along(groups)) {
      E <- groups[[gi]]$edges
      if (!nrow(E)) next
      base <- .rg_ssq(X, groups[[gi]]$nodes)
      for (ei in seq_len(nrow(E))) {
        sp <- .rg_split(E, ei)
        cands <- rbind(cands, c(base - .rg_ssq(X, sp[[1]]$nodes) - .rg_ssq(X, sp[[2]]$nodes), gi, ei))
      }
    }
    if (!nrow(cands)) break
    cands <- cands[order(-cands[, 1], cands[, 2], cands[, 3]), , drop = FALSE]
    done <- FALSE
    for (r in seq_len(nrow(cands))) {
      sp <- .rg_split(groups[[cands[r, 2]]]$edges, cands[r, 3])
      if (ok(sp[[1]]$nodes) && ok(sp[[2]]$nodes)) {
        groups[[cands[r, 2]]] <- sp[[1]]
        groups[[length(groups) + 1]] <- sp[[2]]
        done <- TRUE
        break
      }
    }
    if (!done) break
    tot <- 0
    for (g in groups) tot <- tot + .rg_ssq(X, g$nodes)
    hist <- c(hist, tot)
  }
  labels <- integer(n)
  for (gi in seq_along(groups)) labels[groups[[gi]]$nodes] <- gi
  list(labels = labels, ssd = hist)
}

.rg_bound <- function(n, weights, min_size, min_weight) {
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights)
  wmin <- if (is.null(min_weight)) -Inf else min_weight
  function(nodes) {
    s <- 0
    for (i in nodes) s <- s + w[i]
    length(nodes) >= min_size && s >= wmin
  }
}

.rg_agglomerate <- function(X, nb, linkage, order, stop) {
  n <- nrow(X)
  D <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) D[i, j] <- .rg_sqd(X[i, ], X[j, ])
  if (linkage == "average") D <- sqrt(D)
  E <- unique(do.call(rbind, lapply(seq_len(n), function(i) if (length(nb[[i]])) cbind(pmin(i, nb[[i]]), pmax(i, nb[[i]])) else NULL)))
  E <- E[order(D[E], E[, 1], E[, 2]), , drop = FALSE]
  isnb <- matrix(FALSE, n, n)
  isnb[E] <- TRUE
  isnb[E[, 2:1, drop = FALSE]] <- TRUE
  members <- lapply(seq_len(n), function(i) i)
  alive <- rep(TRUE, n)
  of <- seq_len(n)
  tree <- matrix(0L, 0, 2)
  lab_stop <- NULL
  link <- function(A, B) {
    if (order == "first") return(min(D[A, B][isnb[A, B]]))
    if (linkage == "single") return(min(D[A, B]))
    if (linkage == "complete") return(max(D[A, B]))
    if (linkage == "average") {
      s <- 0
      for (i in A) for (j in B) s <- s + D[i, j]
      return(s / (length(A) * length(B)))
    }
    ca <- vapply(seq_len(ncol(X)), function(q) {
      s <- 0
      for (i in A) s <- s + X[i, q]
      s / length(A)
    }, 0)
    cb <- vapply(seq_len(ncol(X)), function(q) {
      s <- 0
      for (j in B) s <- s + X[j, q]
      s / length(B)
    }, 0)
    length(A) * length(B) / (length(A) + length(B)) * .rg_sqd(ca, cb)
  }
  while (sum(alive) > 1) {
    if (sum(alive) == stop) lab_stop <- .rg_canon(of)
    pa <- unique(cbind(pmin(of[E[, 1]], of[E[, 2]]), pmax(of[E[, 1]], of[E[, 2]])))
    pa <- pa[pa[, 1] != pa[, 2], , drop = FALSE]
    if (!nrow(pa)) stop("the contiguity graph is not connected", call. = FALSE)
    lv <- vapply(seq_len(nrow(pa)), function(r) link(members[[pa[r, 1]]], members[[pa[r, 2]]]), 0)
    b <- order(lv, pa[, 1], pa[, 2])[1]
    a1 <- pa[b, 1]
    b1 <- pa[b, 2]
    hit <- which((of[E[, 1]] == a1 & of[E[, 2]] == b1) | (of[E[, 1]] == b1 & of[E[, 2]] == a1))[1]
    tree <- rbind(tree, E[hit, ])
    members[[a1]] <- c(members[[a1]], members[[b1]])
    of[members[[b1]]] <- a1
    alive[b1] <- FALSE
  }
  if (stop == 1) lab_stop <- rep(1L, n)
  list(tree = tree, labels = lab_stop)
}

#' REDCAP regionalization
#'
#' Contiguity-constrained agglomeration (clusters merge only when contiguous; the
#' linkage runs over all member pairs, or over the contiguity edges between them for
#' \code{order = "first"}) builds a spanning tree from the shortest contiguity edge of
#' each merge; the tree is cut k - 1 times at the edge of largest reduction in the
#' sum of squared deviations. Single, complete and Ward linkage use squared Euclidean
#' distances and average linkage Euclidean distances (the GeoDa convention).
#'
#' @inheritParams Skater
#' @param linkage "single", "complete", "average" or "ward".
#' @param order "full" or "first" (first-order needs single linkage).
#' @return list(labels, tree, ssd, n_regions).
#' @references Guo, D. (2008). Regionalization with dynamically constrained
#'   agglomerative clustering and partitioning (REDCAP). International Journal of
#'   Geographical Information Science 22, 801-823.
#' @examples
#' Redcap(matrix(c(0, 0.1, 0.2, 5, 5.1, 5.2)), list(2, c(1, 3), c(2, 4), c(3, 5), c(4, 6), 5), 2)$labels
#' @export
Redcap <- function(X, adjacency, k, linkage = c("complete", "single", "average", "ward"), order = c("full", "first"),
                   min_size = 1, weights = NULL, min_weight = NULL) {
  linkage <- match.arg(linkage)
  order <- match.arg(order)
  if (order == "first" && linkage != "single") stop('order "first" needs single linkage', call. = FALSE)
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  ag <- .rg_agglomerate(X, .rg_nb(adjacency, n), linkage, order, 0)
  pt <- .rg_partition(X, ag$tree, k, .rg_bound(n, weights, min_size, min_weight))
  list(labels = pt$labels, tree = ag$tree, ssd = pt$ssd, n_regions = max(pt$labels))
}

#' Spatially constrained hierarchical clustering
#'
#' The REDCAP full-order contiguity-constrained agglomeration stopped at k clusters.
#'
#' @inheritParams Redcap
#' @return list(labels (in order of first appearance), merges).
#' @references Guo, D. (2008). International Journal of Geographical
#'   Information Science 22, 801-823.
#' @examples
#' ConstrainedHierarchical(matrix(c(0, 0.1, 5, 5.1)), list(2, c(1, 3), c(2, 4), 3), 2)$labels
#' @export
ConstrainedHierarchical <- function(X, adjacency, k, linkage = c("ward", "single", "complete", "average")) {
  linkage <- match.arg(linkage)
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  if (k < 1 || k > n) stop("need 1 <= k <= number of units", call. = FALSE)
  ag <- .rg_agglomerate(X, .rg_nb(adjacency, n), linkage, "full", k)
  list(labels = if (is.null(ag$labels)) seq_len(n) else ag$labels, merges = ag$tree)
}

.rg_connected_without <- function(members, unit, nb) {
  rest <- members[members != unit]
  if (!length(rest)) return(FALSE)
  seen <- rest[1]
  stack <- rest[1]
  while (length(stack)) {
    u <- stack[length(stack)]
    stack <- stack[-length(stack)]
    for (v in nb[[u]]) {
      if (v %in% rest && !(v %in% seen)) {
        seen <- c(seen, v)
        stack <- c(stack, v)
      }
    }
  }
  length(seen) == length(rest)
}

.rg_objective <- function(X, weights, kind) {
  if (kind == "ssd") {
    return(function(regions) {
      t <- 0
      for (r in regions) t <- t + .rg_ssq(X, r)
      t
    })
  }
  w <- as.numeric(weights)
  function(regions) {
    tot <- vapply(regions, function(r) {
      s <- 0
      for (i in r) s <- s + w[i]
      s
    }, 0)
    m <- 0
    for (v in tot) m <- m + v
    m <- m / length(tot)
    out <- 0
    for (v in tot) out <- out + (v - m)^2
    out
  }
}

.rg_local_search <- function(labels, nb, objective, e, allowed, max_iter) {
  k <- max(labels)
  n <- length(labels)
  regions <- lapply(seq_len(k), function(r) which(labels == r))
  cur <- objective(regions)
  for (sweep in seq_len(max_iter)) {
    improved <- FALSE
    ord <- vapply(seq_len(k), function(q) .mh_u(e), 0)
    for (r in order(ord, seq_len(k))) {
      border <- sort(unique(unlist(lapply(regions[[r]], function(i) nb[[i]]))))
      border <- border[labels[border] != r]
      ob <- vapply(seq_along(border), function(q) .mh_u(e), 0)
      for (v in border[order(ob, border)]) {
        d <- labels[v]
        if (d == r || !.rg_connected_without(regions[[d]], v, nb) || !any(labels[nb[[v]]] == r)) next
        trial <- regions
        trial[[d]] <- trial[[d]][trial[[d]] != v]
        trial[[r]] <- sort(c(trial[[r]], v))
        if (!allowed(trial[[d]])) next
        val <- objective(trial)
        if (val < cur - 1e-12 * max(1, abs(cur))) {
          regions <- trial
          cur <- val
          labels[v] <- r
          improved <- TRUE
        }
      }
    }
    if (!improved) return(list(labels = labels, value = cur, sweeps = sweep))
  }
  list(labels = labels, value = cur, sweeps = max_iter)
}

#' Automatic zoning procedure (AZP)
#'
#' Aggregates contiguous units into k contiguous zones: a random contiguous start
#' (Philox-drawn seeds growing one random neighbour per round) or \code{init}, then
#' AZP local search moving border units while the donor stays contiguous and the
#' objective falls. \code{objective = "ssd"} minimises the within-zone sum of squared
#' deviations; \code{"balance"} minimises the squared deviations of zone weight
#' totals from their mean (redistricting).
#'
#' @inheritParams Skater
#' @param objective "ssd" or "balance".
#' @param init Initial zone labels (1-based), or NULL.
#' @param seed Philox seed.
#' @param max_iter Maximum sweeps.
#' @return list(labels, objective, initial, sweeps).
#' @references Openshaw, S. (1977). A geographical solution to scale and
#'   aggregation problems in region-building, partitioning and spatial modelling.
#'   Transactions of the Institute of British Geographers 2, 459-472.
#' @examples
#' AutomaticZoning(matrix(c(0, 0.1, 0.2, 5, 5.1, 5.2)), list(2, c(1, 3), c(2, 4), c(3, 5), c(4, 6), 5), 2)$labels
#' @export
AutomaticZoning <- function(X, adjacency, k, objective = c("ssd", "balance"), weights = NULL, init = NULL, seed = 0, max_iter = 1000) {
  objective <- match.arg(objective)
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  nb <- .rg_nb(adjacency, n)
  if (k < 1 || k > n) stop("need 1 <= k <= number of units", call. = FALSE)
  if (objective == "balance" && is.null(weights)) stop('objective "balance" needs weights', call. = FALSE)
  e <- .mh_rand(seed)
  if (is.null(init)) {
    lab <- rep(0L, n)
    pool <- seq_len(n)
    for (z in seq_len(k)) {
      i <- .mh_idx(e, length(pool))
      lab[pool[i]] <- z
      pool <- pool[-i]
    }
    while (any(lab == 0L)) {
      grew <- FALSE
      for (z in seq_len(k)) {
        front <- sort(unique(unlist(nb[lab == z])))
        front <- front[lab[front] == 0L]
        if (length(front)) {
          lab[front[.mh_idx(e, length(front))]] <- z
          grew <- TRUE
        }
      }
      if (!grew) stop("the contiguity graph is not connected", call. = FALSE)
    }
  } else {
    lab <- as.integer(init)
  }
  start <- .rg_canon(lab)
  ls <- .rg_local_search(start, nb, .rg_objective(X, weights, objective), e, function(r) length(r) > 0, max_iter)
  list(labels = ls$labels, objective = ls$value, initial = start, sweeps = ls$sweeps)
}

#' Max-p regionalization
#'
#' Largest number p of contiguous regions whose summed \code{weights} reach
#' \code{threshold}: repeated random constructions (Philox) grow regions from
#' shuffled seeds and attach enclaves to the adjacent region of least SSD increase,
#' keeping the largest p and then the smallest SSD; an AZP local search that keeps
#' every donor above the threshold then lowers the SSD.
#'
#' @inheritParams Skater
#' @param threshold Minimum summed weight per region.
#' @param n_construct Number of constructions.
#' @param seed Philox seed.
#' @param local Run the local search.
#' @param max_iter Maximum local-search sweeps.
#' @return list(labels, p, ssd, region_weights).
#' @references Duque, J. C., Anselin, L. and Rey, S. J. (2012). The
#'   max-p-regions problem. Journal of Regional Science 52, 397-419.
#' @examples
#' MaxPRegions(matrix(c(0, 0.1, 0.2, 5, 5.1, 5.2)), list(2, c(1, 3), c(2, 4), c(3, 5), c(4, 6), 5), rep(1, 6), 3)$p
#' @export
MaxPRegions <- function(X, adjacency, weights, threshold, n_construct = 50, seed = 0, local = TRUE, max_iter = 1000) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  nb <- .rg_nb(adjacency, n)
  w <- as.numeric(weights)
  wsum <- function(nodes) {
    s <- 0
    for (i in nodes) s <- s + w[i]
    s
  }
  if (wsum(seq_len(n)) < threshold) stop("the total weight is below the threshold", call. = FALSE)
  e <- .mh_rand(seed)
  best <- NULL
  for (it in seq_len(n_construct)) {
    lab <- rep(0L, n)
    regions <- list()
    ord <- vapply(seq_len(n), function(q) .mh_u(e), 0)
    for (i in order(ord, seq_len(n))) {
      if (lab[i] != 0L) next
      reg <- i
      lab[i] <- length(regions) + 1L
      while (wsum(reg) < threshold) {
        front <- sort(unique(unlist(nb[reg])))
        front <- front[lab[front] == 0L]
        if (!length(front)) break
        v <- front[.mh_idx(e, length(front))]
        lab[v] <- length(regions) + 1L
        reg <- c(reg, v)
      }
      if (wsum(reg) >= threshold) {
        regions[[length(regions) + 1]] <- sort(reg)
      } else {
        lab[reg] <- -1L
      }
    }
    if (!length(regions)) next
    lab[lab == -1L] <- 0L
    while (any(lab == 0L)) {
      moved <- FALSE
      for (u in seq_len(n)) {
        if (lab[u] != 0L) next
        cand <- sort(unique(lab[nb[[u]]]))
        cand <- cand[cand > 0L]
        if (length(cand)) {
          inc <- vapply(cand, function(q) .rg_ssq(X, c(regions[[q]], u)) - .rg_ssq(X, regions[[q]]), 0)
          r <- cand[order(inc, cand)[1]]
          regions[[r]] <- sort(c(regions[[r]], u))
          lab[u] <- r
          moved <- TRUE
        }
      }
      if (!moved) stop("the contiguity graph is not connected", call. = FALSE)
    }
    val <- 0
    for (r in regions) val <- val + .rg_ssq(X, r)
    if (is.null(best) || length(regions) > best$p || (length(regions) == best$p && val < best$v)) best <- list(p = length(regions), v = val, lab = lab)
  }
  if (is.null(best)) stop("no region reaches the threshold", call. = FALSE)
  labels <- best$lab
  if (local) labels <- .rg_local_search(labels, nb, .rg_objective(X, NULL, "ssd"), e, function(r) wsum(r) >= threshold, max_iter)$labels
  p <- max(labels)
  regions <- lapply(seq_len(p), function(r) which(labels == r))
  total <- 0
  for (r in regions) total <- total + .rg_ssq(X, r)
  list(labels = labels, p = p, ssd = total, region_weights = vapply(regions, wsum, 0))
}
