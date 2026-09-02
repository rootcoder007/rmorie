# Multilevel k-way graph partitioning.
# Sources: Karypis, G. & Kumar, V. (1998) A Fast and High Quality
# Multilevel Scheme for Partitioning Irregular Graphs, SIAM Journal on
# Scientific Computing 20(1), 359-392 (the METIS paper). The Python
# reference uses the same matching rules (HEM, RM, LEM), the same
# region-growing initial partitions (GGGP greedy with 4 starts, GGP
# breadth-first with 10 starts), and the same KL/BKL refinement with
# patience = 50, stopping once no improvement is found.
#
# Native R port mirroring morie.fn.grclus exactly. The Python arm draws
# random integers through numpy's default_rng; we draw from the shared
# generator (see .ghc_rng) so the same seed reproduces the same matching
# order, the same RM partner, and the same HEM/LEM tie-break.

#' Multilevel k-way graph partitioning (METIS)
#'
#' Splits the vertices into \code{k} nearly-balanced parts whose
#' between-part edge weight is as small as possible. Following
#' Karypis & Kumar (1998) the graph is coarsened by matching
#' (\code{"hem"}, \code{"rm"} or \code{"lem"}), cut while small, and
#' the cut is carried back up and refined with KL or BKL at every
#' level. \code{k} parts are obtained by recursive bisection.
#'
#' @param A Square adjacency matrix (matrix or list of numeric vectors),
#'   symmetric, non-negative weights, no self-loops, no non-finite
#'   values.
#' @param k Number of parts.
#' @param weights Optional positive vertex weights.
#' @param matching One of \code{"hem"} (heavy edge, default), \code{"rm"}
#'   (random) or \code{"lem"} (light edge).
#' @param initial One of \code{"gggp"} (greedy region growth, default) or
#'   \code{"ggp"} (breadth-first).
#' @param refinement One of \code{"bkl"} (boundary KL, default) or
#'   \code{"kl"} (all vertices).
#' @param tolerance Balance tolerance in \code{[0, 1)}.
#' @param coarsest Coarsening stops when the graph is this small.
#' @param seed Seed for the shared generator.
#' @return A list with \code{estimate} (= edge-cut), \code{partition},
#'   \code{edge_cut}, \code{k}, \code{sizes}, \code{part_weights},
#'   \code{balance}, \code{total_edge_weight}, \code{cut_fraction},
#'   \code{matching}, \code{initial}, \code{refinement}, \code{tolerance},
#'   \code{n}, \code{method}, \code{note}.
#' @references Karypis, G. & Kumar, V. (1998). A Fast and High Quality
#'   Multilevel Scheme for Partitioning Irregular Graphs. SIAM Journal
#'   on Scientific Computing, 20(1), 359-392.
#' @export
morie_grclus <- function(A, k = 2L, weights = NULL, matching = "hem",
                         initial = "gggp", refinement = "bkl",
                         tolerance = 0.03, coarsest = 20L, seed = 17) {
  adj <- .grclus_as_graph(A)
  n <- length(adj)
  k <- as.integer(k)
  if (k < 1L) stop("grclus: k must be at least 1")
  if (k > n) stop("grclus: k = ", k, " exceeds the ", n, " vertices")
  if (!(matching %in% c("hem", "rm", "lem")))
    stop("grclus: matching must be 'hem', 'rm' or 'lem'")
  if (!(initial %in% c("gggp", "ggp")))
    stop("grclus: initial must be 'gggp' or 'ggp'")
  if (!(refinement %in% c("bkl", "kl")))
    stop("grclus: refinement must be 'bkl' or 'kl'")
  if (tolerance < 0 || tolerance >= 1)
    stop("grclus: tolerance must lie in [0, 1)")
  if (is.null(weights)) {
    vw <- rep(1.0, n)
  } else {
    vw <- as.numeric(weights)
    if (length(vw) != n)
      stop("grclus: weights has ", length(vw), " entries for ", n, " vertices")
    if (any(vw <= 0)) stop("grclus: vertex weights must be positive")
  }

  parts <- integer(n)

  rec <- function(members, n_parts, label, depth) {
    if (n_parts <= 1L || length(members) <= 1L) {
      parts[members + 1L] <<- label
      return(invisible(NULL))
    }
    left_parts <- as.integer(n_parts %/% 2L)
    sub <- .grclus_subgraph(adj, vw, members)
    target <- sum(sub$vw) * left_parts / as.numeric(n_parts)
    cut <- .grclus_bisect(sub$adj, sub$vw, target, matching, initial,
                          refinement, tolerance, as.integer(coarsest),
                          seed + depth)
    left <- members[cut == 0L]
    right <- members[cut == 1L]
    if (length(left) == 0L || length(right) == 0L) {
      half <- max(1L, as.integer(length(members) * left_parts / n_parts))
      left <- members[seq_len(half)]
      right <- members[-seq_len(half)]
    }
    rec(left, left_parts, label, depth + 1L)
    rec(right, n_parts - left_parts, label + left_parts, depth + 1L)
  }

  rec(seq_len(n) - 1L, k, 0L, 0L)

  sizes <- integer(k)
  part_w <- numeric(k)
  for (u in seq_len(n) - 1L) {
    sizes[parts[u + 1L] + 1L] <- sizes[parts[u + 1L] + 1L] + 1L
    part_w[parts[u + 1L] + 1L] <- part_w[parts[u + 1L] + 1L] + vw[u + 1L]
  }
  cut <- .grclus_edge_cut(adj, parts)
  total <- .grclus_total_edge_weight(adj)
  ideal <- sum(vw) / as.numeric(k)
  list(estimate = cut, partition = parts, edge_cut = cut, k = k,
       sizes = sizes, part_weights = part_w,
       balance = if (ideal > 0) max(part_w) / ideal else 1.0,
       total_edge_weight = total,
       cut_fraction = if (total > 0) cut / total else 0.0,
       matching = matching, initial = initial, refinement = refinement,
       tolerance = tolerance, n = n,
       method = paste0("multilevel recursive bisection (Karypis & Kumar ",
                       "1998): ", toupper(matching), " matching, ",
                       toupper(initial), " initial partition, ",
                       toupper(refinement), " refinement"),
       note = paste0("edge_cut is the total weight of edges between parts; ",
                     "balance is the heaviest part divided by the ideal ",
                     "equal share, so 1.0 is perfect"))
}

# ---- graph representation ----
# Internally an adjacency list is a list of numeric vectors of
# destination vertex indices (1-based), one column per edge. Parallel
# edges are allowed and their weights are stored positionally.

#' .grclus_rows_to_mat
#'
#' A step of the grclus_native implementation. Called by \code{.grclus_as_graph}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; the body checks with \code{is.matrix}.
#' @return Nothing; this branch always raises.
#' @export
.grclus_rows_to_mat <- function(A) {
  if (is.matrix(A)) return(A)
  if (is.list(A)) return(do.call(rbind, lapply(A, as.numeric)))
  stop("grclus: A must be a matrix or list of numeric vectors")
}

#' .grclus_as_graph
#'
#' A step of the grclus_native implementation. Called by \code{morie_grclus}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A Passed to \code{.grclus_rows_to_mat}.
#' @return The value of \code{adj}, as built in the body.
#' @export
.grclus_as_graph <- function(A) {
  M <- .grclus_rows_to_mat(A)
  if (nrow(M) == 0L) stop("grclus: A is empty")
  if (ncol(M) != nrow(M))
    stop("grclus: A must be a square adjacency matrix (got ", nrow(M),
         " x ", ncol(M), ")")
  if (any(!is.finite(M)))
    stop("grclus: A contains a non-finite value")
  if (any(M < 0))
    stop("grclus: edge weights must be non-negative")
  for (i in seq_len(nrow(M))) {
    for (j in seq_len(ncol(M))) {
      w <- M[i, j]
      if (i == j || w == 0) next
      if (abs(w - M[j, i]) > 1e-9 * max(1, abs(w)))
        stop("grclus: A must be symmetric; entry (", i, ", ", j,
             ") is ", w, " but (", j, ", ", i, ") is ", M[j, i])
    }
  }
  adj <- vector("list", nrow(M))
  for (i in seq_len(nrow(M))) {
    dest <- integer(0); wts <- numeric(0)
    for (j in seq_len(ncol(M))) {
      w <- M[i, j]
      if (i == j || w == 0) next
      dest <- c(dest, j); wts <- c(wts, w)
    }
    adj[[i]] <- list(v = dest, w = wts)
  }
  adj
}

#' .grclus_edge_cut
#'
#' A step of the grclus_native implementation. Called by \code{.grclus_grow_partition}, \code{.grclus_kl}, \code{morie_grclus}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adj A vector; its length is taken and its elements indexed.
#' @param parts A vector; indexed elementwise.
#' @return A numeric value.
#' @export
.grclus_edge_cut <- function(adj, parts) {
  cut <- 0.0
  for (u in seq_along(adj)) {
    v <- adj[[u]]$v; w <- adj[[u]]$w
    for (k in seq_along(v)) if (parts[u] != parts[[v[k]]]) cut <- cut + w[k]
  }
  cut / 2.0
}

#' .grclus_total_edge_weight
#'
#' A step of the grclus_native implementation. Called by \code{morie_grclus}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adj See Usage.
#' @return A numeric value.
#' @export
.grclus_total_edge_weight <- function(adj) {
  s <- 0.0
  for (nbr in adj) s <- s + sum(nbr$w)
  s / 2.0
}

#' Members: 0-based
#'
#' A step of the grclus_native implementation. Called by \code{morie_grclus}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adj A vector; indexed elementwise.
#' @param vw A vector; indexed elementwise.
#' @param members A vector; its length is taken and its elements indexed.
#' @return A list with \code{adj}, \code{vw}.
#' @export
.grclus_subgraph <- function(adj, vw, members) {
  # members: 0-based
  index <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_along(members)) index[[as.character(members[i])]] <- i
  m <- length(members)
  sub <- vector("list", m)
  for (i in seq_len(m)) sub[[i]] <- list(v = integer(0), w = numeric(0))
  for (i in seq_along(members)) {
    u <- members[i] + 1L
    src <- adj[[u]]
    for (kk in seq_along(src$v)) {
      j <- index[[as.character(src$v[kk] - 1L)]]
      if (!is.null(j)) {
        sub[[i]]$v <- c(sub[[i]]$v, j)
        sub[[i]]$w <- c(sub[[i]]$w, src$w[kk])
      }
    }
  }
  list(adj = sub, vw = vw[members + 1L])
}

# ---- RNG helpers (shared) ----
#' RNG helpers (shared) ----
#'
#' A step of the grclus_native implementation. Called by \code{.grclus_grow_partition}, \code{.grclus_match_vertices}, \code{.grclus_shuffled}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param e Passed to \code{.ghc_unif}.
#' @param lo Numeric; combined arithmetically in the body.
#' @param hi Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.grclus_int_in <- function(e, lo, hi) {
  # numpy rng.integers(0, k): low inclusive, high exclusive
  if (hi <= lo) return(lo)
  as.integer(.ghc_unif(e, 1L) * (hi - lo))[1L] + lo
}

#' .grclus_shuffled
#'
#' A step of the grclus_native implementation. Called by \code{.grclus_match_vertices}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param seed Passed to \code{.ghc_rng}.
#' @return The value of \code{idx}, as built in the body.
#' @export
.grclus_shuffled <- function(n, seed) {
  e <- .ghc_rng(seed)
  idx <- as.integer(seq_len(n) - 1L)
  if (n < 2L) return(idx)
  for (i in n:2L) {
    j <- .grclus_int_in(e, 0L, i)         # 0..i-1
    tmp <- idx[i]; idx[i] <- idx[j + 1L]; idx[j + 1L] <- tmp
  }
  idx
}

# ---- coarsening ----
#' Coarsening ----
#'
#' A step of the grclus_native implementation. Called by \code{.grclus_bisect}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adj A vector; its length is taken and its elements indexed.
#' @param scheme One of \code{"hem"}, \code{"rm"}.
#' @param seed Numeric; combined arithmetically in the body.
#' @return The value of \code{mate}, as built in the body.
#' @export
.grclus_match_vertices <- function(adj, scheme, seed) {
  n <- length(adj)
  mate <- as.integer(seq_len(n) - 1L)
  matched <- rep(FALSE, n)
  order <- .grclus_shuffled(n, seed)
  e <- .ghc_rng(seed + 1L)
  for (uu in seq_along(order)) {
    u <- order[uu] + 1L
    if (matched[u]) next
    keep <- !matched[adj[[u]]$v]
    cands <- adj[[u]]$v[keep]
    cws <- adj[[u]]$w[keep]
    if (length(cands) == 0L) next
    if (scheme == "rm") {
      v <- cands[.grclus_int_in(e, 0L, length(cands)) + 1L]
    } else if (scheme == "hem") {
      best <- max(cws)
      tie <- which(cws == best)
      v <- if (length(tie) == 1L) cands[tie[1L]] else
        cands[tie[.grclus_int_in(e, 0L, length(tie)) + 1L]]
    } else {
      best <- min(cws)
      tie <- which(cws == best)
      v <- if (length(tie) == 1L) cands[tie[1L]] else
        cands[tie[.grclus_int_in(e, 0L, length(tie)) + 1L]]
    }
    mate[u] <- v - 1L; mate[v] <- u - 1L
    matched[u] <- matched[v] <- TRUE
  }
  mate
}

#' Mate: 0-based, mate\[u\] = v means u matched with v
#'
#' A step of the grclus_native implementation. Called by \code{.grclus_bisect}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adj A vector; its length is taken and its elements indexed.
#' @param vw A vector; indexed elementwise.
#' @param mate A vector; indexed elementwise.
#' @return A list with \code{adj}, \code{vw}, \code{mapping}.
#' @export
.grclus_coarsen <- function(adj, vw, mate) {
  # mate: 0-based, mate[u] = v means u matched with v
  n <- length(adj)
  mapping <- rep(-1L, n)
  nxt <- 0L
  for (u in seq_len(n) - 1L) {
    if (mapping[u + 1L] >= 0L) next
    v <- mate[u + 1L] + 1L
    mapping[u + 1L] <- nxt
    if (v != u + 1L) mapping[v] <- nxt
    nxt <- nxt + 1L
  }
  m <- nxt
  vw2 <- numeric(m)
  sums <- vector("list", m)
  for (i in seq_len(m)) sums[[i]] <- new.env(hash = TRUE, parent = emptyenv())
  adj2 <- vector("list", m)
  for (i in seq_len(m)) adj2[[i]] <- list(v = integer(0), w = numeric(0))
  for (u in seq_len(n) - 1L) {
    cu <- mapping[u + 1L] + 1L
    vw2[cu] <- vw2[cu] + vw[u + 1L]
    src <- adj[[u + 1L]]
    for (kk in seq_along(src$v)) {
      cv <- mapping[src$v[kk]] + 1L
      if (cu == cv) next
      key <- as.character(cv)
      if (is.null(sums[[cu]][[key]])) {
        sums[[cu]][[key]] <- src$w[kk]
        adj2[[cu]]$v <- c(adj2[[cu]]$v, cv)
        adj2[[cu]]$w <- c(adj2[[cu]]$w, src$w[kk])
      } else {
        sums[[cu]][[key]] <- sums[[cu]][[key]] + src$w[kk]
        # the Python arm stores only the running sum as the parallel
        # edge; collapse the running total into a single representative
        # edge whose weight is the sum
        adj2[[cu]]$w[length(adj2[[cu]]$w)] <- sums[[cu]][[key]]
      }
    }
  }
  list(adj = adj2, vw = vw2, mapping = mapping)
}

# ---- region growing ----
#' Region growing ----
#'
#' A step of the grclus_native implementation. Called by \code{.grclus_balance_bisection}, \code{.grclus_kl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adj A vector; its length is taken and its elements indexed.
#' @param parts A vector; indexed elementwise.
#' @return The value of \code{g}, as built in the body.
#' @export
.grclus_gains <- function(adj, parts) {
  n <- length(adj)
  g <- numeric(n)
  for (u in seq_len(n)) {
    ext <- 0.0; ins <- 0.0
    src <- adj[[u]]
    for (k in seq_along(src$v)) {
      if (parts[[src$v[k]]] == parts[u]) ins <- ins + src$w[k]
      else ext <- ext + src$w[k]
    }
    g[u] <- ext - ins
  }
  g
}

#' .grclus_grow_partition
#'
#' A step of the grclus_native implementation. Called by \code{.grclus_bisect}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adj A vector; its length is taken and its elements indexed.
#' @param vw A vector; indexed elementwise.
#' @param target Passed to \code{<}.
#' @param seed Passed to \code{.ghc_rng}.
#' @param greedy A flag; the body branches on it.
#' @param n_starts Optional; may be \code{NULL}. A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{best_parts}, as built in the body.
#' @export
.grclus_grow_partition <- function(adj, vw, target, seed, greedy,
                                   n_starts = NULL) {
  n <- length(adj)
  if (is.null(n_starts)) n_starts <- if (greedy) 4L else 10L
  e <- .ghc_rng(seed)
  best_parts <- NULL; best_cut <- NULL
  n_starts <- min(n_starts, n)
  starts <- rep(0L, n_starts)
  for (i in seq_len(n_starts)) starts[i] <- .grclus_int_in(e, 0L, n)
  for (s in starts) {
    parts <- rep(1L, n)
    parts[s + 1L] <- 0L
    weight <- vw[s + 1L]
    frontier <- new.env(hash = TRUE, parent = emptyenv())
    for (kk in adj[[s + 1L]]$v)
      frontier[[as.character(kk)]] <- 0L
    while (weight < target && length(ls(frontier)) > 0L) {
      v <- if (greedy) {
        best_v <- NA; best_g <- NA
        keys <- ls(frontier)
        for (kk in keys) {
          v0 <- as.integer(kk)
          ins <- 0.0; out <- 0.0
          src <- adj[[v0]]
          for (m in seq_along(src$v)) {
            if (parts[[src$v[m]]] == 0L) ins <- ins + src$w[m]
            else out <- out + src$w[m]
          }
          g <- ins - out
          if (is.na(best_g) || g > best_g) { best_g <- g; best_v <- v0 }
        }
        best_v
      } else {
        as.integer(ls(frontier)[1L])
      }
      rm(list = as.character(v), envir = frontier)
      parts[v] <- 0L
      weight <- weight + vw[v]
      src <- adj[[v]]
      for (m in seq_along(src$v)) {
        if (parts[[src$v[m]]] == 1L &&
            is.null(frontier[[as.character(src$v[m])]]))
          frontier[[as.character(src$v[m])]] <- 0L
      }
    }
    c <- .grclus_edge_cut(adj, parts)
    if (is.null(best_cut) || c < best_cut) {
      best_cut <- c; best_parts <- parts
    }
  }
  if (is.null(best_parts)) {
    best_parts <- ifelse((seq_len(n) - 1L) * 2L < n, 0L, 1L)
  }
  best_parts
}

# ---- refinement ----
#' Refinement ----
#'
#' A step of the grclus_native implementation. Called by \code{.grclus_bisect}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adj A vector; its length is taken and its elements indexed.
#' @param vw A vector; indexed elementwise.
#' @param parts A vector; indexed elementwise.
#' @param target Numeric; combined arithmetically in the body.
#' @param tolerance Numeric; combined arithmetically in the body.
#' @param boundary A flag; the body branches on it.
#' @param max_passes Coerced to integer by the body, with \code{as.integer}.
#' @param patience Passed to \code{>=}.
#' @return A list with \code{parts}, \code{cut}.
#' @export
.grclus_kl <- function(adj, vw, parts, target, tolerance, boundary,
                       max_passes, patience) {
  n <- length(adj)
  parts <- as.integer(parts)
  total_w <- sum(vw)
  lo <- target - tolerance * total_w
  hi <- target + tolerance * total_w
  best_cut <- .grclus_edge_cut(adj, parts)
  for (pass in seq_len(as.integer(max_passes))) {
    g <- .grclus_gains(adj, parts)
    locked <- rep(FALSE, n)
    cur_cut <- best_cut
    w0 <- sum(vw[parts == 0L])
    seen_best <- cur_cut
    best_state <- parts
    since <- 0L
    moved_any <- FALSE
    repeat {
      cands <- integer(0); cands_g <- numeric(0)
      for (v in seq_len(n)) {
        if (locked[v]) next
        if (boundary) {
          src <- adj[[v]]$v
          has <- FALSE
          for (u in src) if (parts[u] != parts[v]) { has <- TRUE; break }
          if (!has) next
        }
        nw0 <- if (parts[v] == 0L) w0 - vw[v] else w0 + vw[v]
        if (!(lo <= nw0 && nw0 <= hi)) {
          if (abs(nw0 - target) >= abs(w0 - target)) next
        }
        cands <- c(cands, v); cands_g <- c(cands_g, g[v])
      }
      if (length(cands) == 0L) break
      pick <- which.max(cands_g)
      v <- cands[pick]
      cur_cut <- cur_cut - g[v]
      w0 <- if (parts[v] == 0L) w0 - vw[v] else w0 + vw[v]
      parts[v] <- 1L - parts[v]
      locked[v] <- TRUE
      moved_any <- TRUE
      src <- adj[[v]]
      for (m in seq_along(src$v)) {
        if (parts[[src$v[m]]] == parts[v]) g[src$v[m]] <- g[src$v[m]] - 2.0 * src$w[m]
        else g[src$v[m]] <- g[src$v[m]] + 2.0 * src$w[m]
      }
      if (cur_cut < seen_best - 1e-12) {
        seen_best <- cur_cut; best_state <- parts; since <- 0L
      } else {
        since <- since + 1L
        if (since >= patience) break
      }
    }
    parts <- best_state
    if (!moved_any || seen_best >= best_cut - 1e-12) {
      best_cut <- min(best_cut, seen_best)
      break
    }
    best_cut <- seen_best
  }
  list(parts = parts, cut = best_cut)
}

#' .grclus_balance_bisection
#'
#' A step of the grclus_native implementation. Called by \code{.grclus_bisect}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adj A vector; its length is taken.
#' @param vw A vector; indexed elementwise.
#' @param parts A vector; indexed elementwise.
#' @param target Numeric; combined arithmetically in the body.
#' @param tolerance Numeric; combined arithmetically in the body.
#' @return The value of \code{parts}, as built in the body.
#' @export
.grclus_balance_bisection <- function(adj, vw, parts, target, tolerance) {
  n <- length(adj)
  parts <- as.integer(parts)
  total_w <- sum(vw)
  lo <- target - tolerance * total_w
  hi <- target + tolerance * total_w
  guard <- 0L
  while (guard < n) {
    guard <- guard + 1L
    w0 <- sum(vw[parts == 0L])
    if (lo <= w0 && w0 <= hi) break
    heavy <- if (w0 > hi) 0L else 1L
    g <- .grclus_gains(adj, parts)
    cands <- which(parts == heavy)
    if (length(cands) <= 1L) break
    v <- cands[which.max(g[cands])]
    parts[v] <- 1L - parts[v]
  }
  parts
}

#' .grclus_bisect
#'
#' A step of the grclus_native implementation. Called by \code{morie_grclus}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adj Passed to \code{.grclus_balance_bisection}.
#' @param vw Numeric; passed to \code{sum}.
#' @param target Numeric; combined arithmetically in the body.
#' @param matching Passed to \code{.grclus_match_vertices}.
#' @param initial Compared against \code{"gggp"}.
#' @param refinement Compared against \code{"bkl"}.
#' @param tolerance Passed to \code{.grclus_kl}.
#' @param coarsest Coerced to integer by the body, with \code{as.integer}.
#' @param seed Numeric; combined arithmetically in the body.
#' @return The value of \code{.grclus_balance_bisection}.
#' @export
.grclus_bisect <- function(adj, vw, target, matching, initial, refinement,
                           tolerance, coarsest, seed) {
  levels <- list()
  cur_adj <- adj; cur_vw <- as.numeric(vw)
  guard <- 0L
  while (length(cur_adj) > max(as.integer(coarsest), 3L) && guard < 100L) {
    guard <- guard + 1L
    mate <- .grclus_match_vertices(cur_adj, matching, seed + guard)
    c <- .grclus_coarsen(cur_adj, cur_vw, mate)
    if (length(c$adj) >= length(cur_adj)) break
    levels[[length(levels) + 1L]] <- list(adj = cur_adj, vw = cur_vw,
                                          mapping = c$mapping)
    cur_adj <- c$adj; cur_vw <- c$vw
  }
  sum0 <- sum(vw)
  scale <- if (sum0 > 0) sum(cur_vw) / sum0 else 1.0
  parts <- .grclus_grow_partition(cur_adj, cur_vw, target * scale, seed,
                                  greedy = (initial == "gggp"))
  kl <- .grclus_kl(cur_adj, cur_vw, parts, target * scale, tolerance,
                   boundary = (refinement == "bkl"),
                   max_passes = 10L, patience = 50L)
  parts <- kl$parts
  for (lvl in rev(levels)) {
    # Uncoarsen: lvl is the FINE level we stored, lvl$mapping maps
    # fine -> coarse. Expand the current coarse partition to the fine
    # level.
    new_parts <- rep(0L, length(lvl$adj))
    for (u in seq_along(lvl$mapping)) {
      new_parts[u] <- parts[[lvl$mapping[u] + 1L]]
    }
    sum0 <- sum(vw)
    scale <- if (sum0 > 0) sum(lvl$vw) / sum0 else 1.0
    new_parts <- .grclus_balance_bisection(lvl$adj, lvl$vw, new_parts,
                                            target * scale, tolerance)
    kl <- .grclus_kl(lvl$adj, lvl$vw, new_parts, target * scale, tolerance,
                     boundary = (refinement == "bkl"),
                     max_passes = 10L, patience = 50L)
    parts <- kl$parts
  }
  .grclus_balance_bisection(adj, vw, parts, target, tolerance)
}
