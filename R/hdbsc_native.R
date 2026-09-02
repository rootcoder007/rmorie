# HDBSCAN* density clustering by excess-of-mass extraction.
# Source: Campello, Moulavi & Sander (2013), PAKDD, LNCS 7819,
# 160-172, Defs. 5-8, Algorithms 1-3, Eqs. 1-5
# (fetched-wave3/Density-Based Clustering Based on Hierarchical
# Density Estimates.pdf).  Mirrors Python morie.fn.hdbsc exactly:
# same Prim MST tie-breaking, same single-linkage merge order,
# same condense/stability/selection, same label assignment.

#' .hdb_core
#'
#' A step of the hdbsc_native implementation. Called by \code{morie_hdbsc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param D A matrix; indexed by row and column.
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param mp Numeric; passed to \code{min}.
#' @return A vector, from \code{vapply}.
#' @export
.hdb_core <- function(D, n, mp) {
  vapply(seq_len(n), function(i) sort(D[i, ])[min(mp, n)], numeric(1))
}

#' .hdb_mst
#'
#' A step of the hdbsc_native implementation. Called by \code{morie_hdbsc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param D A matrix; indexed by row and column.
#' @param core A vector; indexed elementwise.
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{edges}, as built in the body.
#' @export
.hdb_mst <- function(D, core, n) {
  in_tree <- rep(FALSE, n)
  key <- rep(Inf, n)
  parent <- rep(-1L, n)
  key[1] <- 0
  edges <- list()
  for (s in seq_len(n)) {
    u <- -1L; best <- Inf
    for (k in seq_len(n)) {
      if (!in_tree[k] && key[k] < best) { best <- key[k]; u <- k }
    }
    in_tree[u] <- TRUE
    if (parent[u] != -1L) {
      edges[[length(edges) + 1L]] <- c(key[u], parent[u] - 1L, u - 1L)
    }
    for (v in seq_len(n)) {
      if (!in_tree[v]) {
        w <- max(core[u], core[v], D[u, v])
        if (w < key[v]) { key[v] <- w; parent[v] <- u }
      }
    }
  }
  edges
}

# single linkage (Prop. 1): 0-based node ids to mirror Python.
#' Single linkage (Prop. 1): 0-based node ids to mirror Python
#'
#' A step of the hdbsc_native implementation. Called by \code{morie_hdbsc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param edges A vector; its length is taken.
#' @param n Numeric; combined arithmetically in the body.
#' @return A list with \code{root}, \code{children}, \code{node_size}.
#' @export
.hdb_linkage <- function(edges, n) {
  m <- length(edges)
  E <- if (m) do.call(rbind, edges) else matrix(numeric(0), 0, 3)
  ord <- if (m) order(E[, 1], E[, 2], E[, 3]) else integer(0)
  par <- 0:(n - 1)                       # union-find over 0-based ids
  node_of <- 0:(n - 1)
  find <- function(x) {                  # x is 0-based
    while (par[x + 1L] != x) {
      par[x + 1L] <- par[par[x + 1L] + 1L]
      x <- par[x + 1L]
    }
    x
  }
  children <- list()                     # key = as.character(node id)
  node_size <- rep(1L, 2 * n)            # index by (id+1)
  nid <- n
  for (oi in ord) {
    w <- E[oi, 1]; a <- as.integer(E[oi, 2]); b <- as.integer(E[oi, 3])
    ra <- find(a); rb <- find(b)
    na <- node_of[ra + 1L]; nb <- node_of[rb + 1L]
    children[[as.character(nid)]] <- list(l = na, r = nb, w = w)
    node_size[nid + 1L] <- node_size[na + 1L] + node_size[nb + 1L]
    par[ra + 1L] <- rb
    node_of[rb + 1L] <- nid
    nid <- nid + 1L
  }
  list(root = nid - 1L, children = children, node_size = node_size)
}

#' .hdb_points_under
#'
#' A step of the hdbsc_native implementation. Called by \code{morie_hdbsc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node Passed to \code{c}.
#' @param children A vector; indexed elementwise.
#' @param n Passed to \code{<}.
#' @return The value of \code{out}, as built in the body.
#' @export
.hdb_points_under <- function(node, children, n) {
  out <- integer(0)
  stack <- c(node)
  while (length(stack)) {
    v <- stack[length(stack)]; stack <- stack[-length(stack)]
    if (v < n) {
      out <- c(out, v)
    } else {
      ch <- children[[as.character(v)]]
      stack <- c(stack, ch$l, ch$r)
    }
  }
  out
}

#' HDBSCAN* flat clustering by excess-of-mass stability extraction
#'
#' Core distance = distance to the mpts-nearest neighbour (Def. 5);
#' mutual reachability d_mreach = max(core_i, core_j, d) (Def. 7);
#' single-linkage density hierarchy (Prop. 1); condensed with a
#' minimum cluster size (Algorithm 2); cluster stability
#' S(C) = sum(lambda_max - lambda_min) (Eq. 3); flat partition by the
#' bottom-up max-stability recursion (Algorithm 3, Eqs. 4-5).
#' Clusters selected at different density levels separate
#' differing-density clusters no single DBSCAN* threshold can.
#'
#' @param X Data matrix (n x d).
#' @param min_pts mpts core-distance neighbour count.
#' @param min_cluster_size Minimum cluster size (mclSize).
#' @param selection See Usage.
#' @param verbose See Usage.
#' @return A list with elements \code{labels} (-1 = noise),
#'   \code{core_distances}, \code{n_clusters}, \code{stabilities},
#'   \code{condensed_tree}, \code{cluster_tree}, \code{min_pts},
#'   \code{min_cluster_size}, \code{method}.
#' @references Campello, R. J. G. B., Moulavi, D. and Sander, J.
#'   (2013). Density-based clustering based on hierarchical density
#'   estimates. PAKDD, LNCS 7819, 160-172.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_hdbsc(V)
morie_hdbsc <- function(X, min_pts = 5, min_cluster_size = 5,
                        selection = "eom", verbose = FALSE) {
  if (!selection %in% c("eom", "leaf"))
    stop("selection must be 'eom' or 'leaf'")
  X <- as.matrix(X)
  n <- nrow(X)
  mcs <- as.integer(min_cluster_size)
  if (n < min_pts || n < 2) stop("need at least min_pts points")
  mp <- as.integer(min_pts)
  say <- function(msg) if (isTRUE(verbose)) message("[hdbsc] ", msg)
  say(sprintf("distances (n=%d)", n))
  D <- as.matrix(stats::dist(X))
  dimnames(D) <- NULL
  say("core distances")
  core <- .hdb_core(D, n, mp)
  say("mutual-reachability MST")
  edges <- .hdb_mst(D, core, n)
  say("single linkage")
  sl <- .hdb_linkage(edges, n)
  root <- sl$root; children <- sl$children; node_size <- sl$node_size

  # condense (Algorithm 2).  Cluster ids 0-based to mirror Python:
  # root cluster id = n, new ids > n.
  next_cluster <- n + 1L
  node_to_cluster <- list(); node_to_cluster[[as.character(root)]] <- n
  birth <- list();       birth[[as.character(n)]] <- 0
  birth_node <- list();  birth_node[[as.character(n)]] <- root
  cluster_tree <- list(); cluster_tree[[as.character(n)]] <- integer(0)
  parent_of <- list();   parent_of[[as.character(n)]] <- NA_integer_
  rows <- list()
  stack <- c(root)
  while (length(stack)) {
    node <- stack[length(stack)]; stack <- stack[-length(stack)]
    cid <- node_to_cluster[[as.character(node)]]
    if (node < n) next
    ch <- children[[as.character(node)]]
    l <- ch$l; r <- ch$r; dist <- ch$w
    lam <- if (dist <= 0) Inf else 1 / dist
    szl <- node_size[l + 1L]; szr <- node_size[r + 1L]
    ok_l <- szl >= mcs; ok_r <- szr >= mcs
    if (ok_l && ok_r) {
      for (pair in list(c(l, szl), c(r, szr))) {
        side <- pair[1]; sz <- pair[2]
        c_ <- next_cluster; next_cluster <- next_cluster + 1L
        node_to_cluster[[as.character(side)]] <- c_
        birth[[as.character(c_)]] <- lam
        birth_node[[as.character(c_)]] <- side
        cluster_tree[[as.character(c_)]] <- integer(0)
        cluster_tree[[as.character(cid)]] <-
          c(cluster_tree[[as.character(cid)]], c_)
        parent_of[[as.character(c_)]] <- cid
        rows[[length(rows) + 1L]] <- c(cid, c_, lam, sz)
        stack <- c(stack, side)
      }
    } else if (!ok_l && !ok_r) {
      for (p in .hdb_points_under(node, children, n)) {
        rows[[length(rows) + 1L]] <- c(cid, p, lam, 1)
      }
    } else {
      if (ok_l) { big <- l; small <- r } else { big <- r; small <- l }
      for (p in .hdb_points_under(small, children, n)) {
        rows[[length(rows) + 1L]] <- c(cid, p, lam, 1)
      }
      node_to_cluster[[as.character(big)]] <- cid
      stack <- c(stack, big)
    }
  }

  cl_ids <- as.integer(names(birth))
  # stability (Eq. 3)
  stability <- setNames(rep(0, length(cl_ids)), as.character(cl_ids))
  for (row in rows) {
    parent <- row[1]; lam <- row[3]; sz <- row[4]
    if (is.infinite(lam)) next
    key <- as.character(parent)
    stability[key] <- stability[key] + sz * (lam - birth[[key]])
  }

  say(sprintf("extract (%s)", selection))
  subtree <- function(c_) {
    out <- c_
    for (ch in cluster_tree[[as.character(c_)]]) out <- c(out, subtree(ch))
    out
  }
  if (selection == "leaf") {
    # leaf selection (Campello 2013): every leaf of the condensed tree
    selected <- setNames(vapply(cl_ids, function(c_)
      c_ != n && !length(cluster_tree[[as.character(c_)]]), logical(1)),
      as.character(cl_ids))
  } else {
    # Algorithm 3: bottom-up max excess-of-mass selection (deepest first)
    order_c <- cl_ids[order(-vapply(cl_ids, function(c_)
      birth[[as.character(c_)]], numeric(1)))]
    selected <- setNames(cl_ids != n, as.character(cl_ids))
    s_hat <- list()
    for (c_ in order_c) {
      if (c_ == n) next
      kids <- cluster_tree[[as.character(c_)]]
      if (!length(kids)) {
        s_hat[[as.character(c_)]] <- stability[[as.character(c_)]]
      } else {
        sub <- sum(vapply(kids, function(k) s_hat[[as.character(k)]],
                          numeric(1)))
        if (stability[[as.character(c_)]] < sub) {
          s_hat[[as.character(c_)]] <- sub
          selected[as.character(c_)] <- FALSE
        } else {
          s_hat[[as.character(c_)]] <- stability[[as.character(c_)]]
          for (k in kids) for (d in subtree(k))
            selected[as.character(d)] <- FALSE
        }
      }
    }
  }

  chosen <- sort(cl_ids[cl_ids != n & selected[as.character(cl_ids)]])
  label_of <- setNames(seq_along(chosen) - 1L, as.character(chosen))
  labels <- rep(-1L, n)
  contains <- lapply(cl_ids, function(c_)
    .hdb_points_under(birth_node[[as.character(c_)]], children, n))
  names(contains) <- as.character(cl_ids)
  for (p in 0:(n - 1)) {
    cand <- cl_ids[vapply(cl_ids, function(c_)
      p %in% contains[[as.character(c_)]], logical(1))]
    deep <- cand[which.max(vapply(cand, function(c_)
      birth[[as.character(c_)]], numeric(1)))]
    cur <- deep
    repeat {
      if (is.na(cur)) break
      if (isTRUE(selected[as.character(cur)])) break
      cur <- parent_of[[as.character(cur)]]
    }
    if (!is.na(cur) && as.character(cur) %in% names(label_of)) {
      labels[p + 1L] <- label_of[[as.character(cur)]]
    }
  }

  condensed <- if (length(rows)) do.call(rbind, rows) else
    matrix(numeric(0), 0, 4)
  list(labels = labels, core_distances = core,
       n_clusters = length(chosen),
       stabilities = stability,
       condensed_tree = condensed,
       cluster_tree = cluster_tree,
       min_pts = mp, min_cluster_size = mcs, selection = selection,
       method = sprintf("HDBSCAN* %s extraction (Campello 2013)",
         if (selection == "eom") "excess-of-mass" else "leaf"))
}
