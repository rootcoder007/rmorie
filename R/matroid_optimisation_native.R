# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Matroids and combinatorial optimisation.
# Mirrors morie.fn.matrdt and morie.fn.cmbopt.
#
# The organising result is Rado and Edmonds': the greedy algorithm
# returns a maximum-weight independent set for EVERY weighting if and
# only if the independence system is a matroid. Because that is an
# equivalence, the parity tests exercise both directions -- greedy
# confirmed optimal on matroids, and confirmed to FAIL on a hereditary
# system that violates exchange. Checking only the forward direction
# would pass on code with no notion of a matroid at all.
#
# The optimisation side is all min-max dualities, so each function
# returns BOTH sides and the residual between them rather than one side
# and a promise.
#
# Oxley JG (2011), Matroid Theory, 2nd ed. Whitney H (1935);
# Edmonds J (1971); Kruskal JB (1956); Hall P (1935); Konig D (1931);
# Ford LR, Fulkerson DR (1956).

#' .morie_subsets
#'
#' Part of the matroid_optimisation_native implementation; see the file
#' header for the source it follows.
#'
#' @param g A vector; its length is taken.
#' @return The value of \code{out}, as built in the body.
#' @export
#' @examples
#' g <- c(0L, 1L, 0L, 1L, 1L, 0L, 1L, 0L)
#' res <- .morie_subsets(g = g)
#' res
.morie_subsets <- function(g) {
  n <- length(g)
  out <- list(integer(0))
  if (n >= 1L) {
    for (r in seq_len(n)) {
      cmb <- utils::combn(g, r, simplify = FALSE)
      out <- c(out, cmb)
    }
  }
  out
}

#' .morie_key
#'
#' Part of the matroid_optimisation_native implementation; see the file
#' header for the source it follows.
#'
#' @param s Coerced to integer by the body, with \code{as.integer}.
#' @return A character value.
#' @export
#' @examples
#' txt <- c('alpha', 'beta', 'gamma', 'delta')
#' res <- .morie_key(s = txt)
#' res
.morie_key <- function(s) paste(sort(as.integer(s)), collapse = ",")

#' Check the two matroid axioms directly
#'
#' Heredity: every subset of an independent set is independent.
#' Exchange: if \eqn{|A| < |B|} with both independent, some element of
#' \eqn{B \setminus A} can be added to \eqn{A} keeping it independent.
#'
#' Both are checked exhaustively and the first violating pair is
#' returned, because "not a matroid" without a witness is not a useful
#' answer.
#'
#' @param ground Integer vector of ground-set elements.
#' @param independent List of integer vectors.
#' @return A list with `is_matroid`, `hereditary`, `exchange`,
#'   `heredity_violation`, `exchange_violation`, `warnings`.
#' @references Whitney H (1935) \emph{Amer J Math} 57(3):509-533.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_is_matroid(V, V)
morie_is_matroid <- function(ground, independent) {
  g <- as.integer(ground)
  fam <- lapply(independent, function(s) sort(as.integer(s)))
  keys <- vapply(fam, .morie_key, character(1))
  has <- function(s) .morie_key(s) %in% keys
  warns <- character(0)
  if (!has(integer(0))) {
    return(list(is_matroid = FALSE, hereditary = FALSE, exchange = NULL,
                heredity_violation = "the empty set is not independent",
                exchange_violation = NULL, n_independent = length(fam),
                n = length(g), warnings = "the empty set is not independent",
                method = "Matroid structure and greedy optimality"))
  }
  hered_bad <- NULL
  for (s in fam) {
    for (e in s) {
      if (!has(setdiff(s, e))) {
        hered_bad <- list(s, setdiff(s, e))
        break
      }
    }
    if (!is.null(hered_bad)) break
  }
  exch_bad <- NULL
  if (is.null(hered_bad)) {
    for (a in fam) {
      for (b in fam) {
        if (length(a) >= length(b)) next
        extra <- setdiff(b, a)
        if (!any(vapply(extra, function(x) has(c(a, x)), logical(1)))) {
          exch_bad <- list(a, b)
          break
        }
      }
      if (!is.null(exch_bad)) break
    }
  }
  ok <- is.null(hered_bad) && is.null(exch_bad)
  if (!is.null(hered_bad)) {
    warns <- c(warns, sprintf(paste(
      "Heredity fails: {%s} is independent but its subset {%s} is not.",
      "This is not even an independence system."),
      paste(hered_bad[[1]], collapse = ", "),
      paste(hered_bad[[2]], collapse = ", ")))
  }
  if (!is.null(exch_bad)) {
    warns <- c(warns, sprintf(paste(
      "Exchange fails: {%s} and {%s} are both independent with the first",
      "smaller, yet no element of the second can be added to the first. The",
      "greedy algorithm is therefore not guaranteed optimal on this system."),
      paste(exch_bad[[1]], collapse = ", "),
      paste(exch_bad[[2]], collapse = ", ")))
  }
  list(is_matroid = ok, hereditary = is.null(hered_bad),
       exchange = if (is.null(hered_bad)) is.null(exch_bad) else NULL,
       heredity_violation = hered_bad, exchange_violation = exch_bad,
       n_independent = length(fam), n = length(g), warnings = warns,
       method = "Matroid structure and greedy optimality")
}

#' Rank of a subset: the size of its largest independent subset
#'
#' @param ground,independent The matroid.
#' @param subset Subset to rank; defaults to the whole ground set.
#' @return Integer rank.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_matroid_rank(V, V)
morie_matroid_rank <- function(ground, independent, subset = NULL) {
  target <- if (is.null(subset)) as.integer(ground) else as.integer(subset)
  best <- 0L
  for (s in independent) {
    si <- as.integer(s)
    if (all(si %in% target)) best <- max(best, length(si))
  }
  best
}

#' The maximal independent sets
#'
#' All have the same size in a matroid, which is a consequence of
#' exchange rather than an assumption.
#'
#' @param ground,independent The matroid.
#' @return A list of integer vectors.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_matroid_bases(V, V)
morie_matroid_bases <- function(ground, independent) {
  r <- morie_matroid_rank(ground, independent)
  out <- Filter(function(s) length(s) == r, independent)
  lapply(out, function(s) sort(as.integer(s)))
}

#' The minimal dependent sets
#'
#' @param ground,independent The matroid.
#' @return A list of integer vectors.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_matroid_circuits(V, V)
morie_matroid_circuits <- function(ground, independent) {
  g <- as.integer(ground)
  keys <- vapply(independent, .morie_key, character(1))
  all_s <- .morie_subsets(g)
  dep <- Filter(function(s) !(.morie_key(s) %in% keys), all_s)
  is_min <- function(d) {
    !any(vapply(dep, function(c) {
      length(c) < length(d) && all(c %in% d)
    }, logical(1)))
  }
  out <- Filter(is_min, dep)
  out <- lapply(out, function(s) sort(as.integer(s)))
  out[order(vapply(out, length, integer(1)),
            vapply(out, function(s) paste(s, collapse = ","), character(1)))]
}

#' The dual matroid
#'
#' \eqn{X} is independent in \eqn{M^*} exactly when \eqn{E \setminus X}
#' contains a basis of \eqn{M}. Duality is an involution, which the
#' tests confirm by dualising twice.
#'
#' @param ground,independent The matroid.
#' @return A list of integer vectors.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_matroid_dual(V, V)
morie_matroid_dual <- function(ground, independent) {
  g <- as.integer(ground)
  bases <- morie_matroid_bases(g, independent)
  out <- Filter(function(s) {
    rest <- setdiff(g, as.integer(s))
    any(vapply(bases, function(b) all(b %in% rest), logical(1)))
  }, .morie_subsets(g))
  lapply(out, function(s) sort(as.integer(s)))
}

#' The uniform matroid U(k, n)
#'
#' Every subset of size at most \eqn{k} is independent.
#'
#' @param n,k Ground size and rank.
#' @return A list with `ground`, `independent`, `rank`.
#' @export
#' @examples
#' morie_uniform_matroid(n = 5L, k = 5L)
morie_uniform_matroid <- function(n, k) {
  n <- as.integer(n)
  k <- as.integer(k)
  if (n < 0L || k < 0L) stop("n and k must be non-negative.", call. = FALSE)
  # 1-based, like every other ground set in this package. The Python
  # side is 0-based; counts and weights are index-free and must agree,
  # element labels differ by one and the parity test shifts them.
  g <- if (n >= 1L) seq_len(n) else integer(0)
  ind <- Filter(function(s) length(s) <= k, .morie_subsets(g))
  list(ground = g, independent = ind, rank = min(k, n),
       name = sprintf("U(%d,%d)", k, n))
}

#' The cycle matroid of a graph
#'
#' A set of edges is independent exactly when it is acyclic. This is the
#' matroid that makes Kruskal's algorithm correct: minimum spanning tree
#' IS the greedy algorithm here.
#'
#' @param edges List or matrix of vertex pairs, vertices from 1.
#' @param n_vertices Number of vertices.
#' @return A list with `ground`, `independent`, `edges`.
#' @export
#' @examples
#' morie_graphic_matroid(edges = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), n_vertices = 5L)
morie_graphic_matroid <- function(edges, n_vertices) {
  E <- lapply(edges, as.integer)
  m <- length(E)
  n <- as.integer(n_vertices)
  # NOTE the variable name and the assignment operator. An earlier
  # version used `par` with `par[a] <<- b` at this scope; `<<-` skips
  # the current frame, so it never found the local `par` and resolved
  # to graphics::par instead, failing with "object of type 'closure' is
  # not subsettable". Plain `<-` at the owning scope, and a name that
  # does not collide with a base function.
  acyclic <- function(sub) {
    uf <- seq_len(n)
    find <- function(x) {
      while (uf[x] != x) x <- uf[x]
      x
    }
    for (i in sub) {
      a <- find(E[[i]][1])
      b <- find(E[[i]][2])
      if (a == b) return(FALSE)
      uf[a] <- b
    }
    TRUE
  }
  g <- if (m >= 1L) seq_len(m) else integer(0)
  ind <- Filter(acyclic, .morie_subsets(g))
  list(ground = g, independent = ind, edges = E, n_vertices = n,
       name = "graphic")
}

#' The greedy algorithm on an independence system
#'
#' Sort by weight, take what keeps independence. Only positive weights
#' are taken, since a negative element can never improve a
#' maximum-weight independent set.
#'
#' @param ground,independent The system.
#' @param weights Numeric vector aligned with `ground`.
#' @return A list with `set` and `weight`.
#' @references Edmonds J (1971) \emph{Math Prog} 1:127-136.
#' @export
#' @examples
#' morie_greedy_independent_set(ground = c(1, 2, 3, 4, 5, 6, 7, 8), independent = c(1, 2,
#' 3, 4, 5, 6, 7, 8), weights = c(1, 2, 3, 4, 5, 6, 7, 8))
morie_greedy_independent_set <- function(ground, independent, weights) {
  g <- as.integer(ground)
  w <- stats::setNames(as.numeric(weights), as.character(g))
  keys <- vapply(independent, .morie_key, character(1))
  has <- function(s) .morie_key(s) %in% keys
  ord <- g[order(-w[as.character(g)], g)]
  cur <- integer(0)
  for (e in ord) {
    if (w[as.character(e)] <= 0) next
    if (has(c(cur, e))) cur <- sort(c(cur, e))
  }
  list(set = cur, weight = sum(w[as.character(cur)]))
}

#' Maximum-weight independent set by exhaustive search
#'
#' @param ground,independent The system.
#' @param weights Numeric vector aligned with `ground`.
#' @return A list with `set` and `weight`.
#' @export
#' @examples
#' morie_brute_force_max_weight(ground = c(1, 2, 3, 4, 5, 6, 7, 8), independent = c(1, 2,
#' 3, 4, 5, 6, 7, 8), weights = c(1, 2, 3, 4, 5, 6, 7, 8))
morie_brute_force_max_weight <- function(ground, independent, weights) {
  g <- as.integer(ground)
  w <- stats::setNames(as.numeric(weights), as.character(g))
  best <- NULL
  best_set <- NULL
  for (s in independent) {
    si <- sort(as.integer(s))
    tot <- if (length(si)) sum(w[as.character(si)]) else 0
    if (is.null(best) || tot > best) { best <- tot
    best_set <- si }
  }
  list(set = best_set, weight = if (is.null(best)) 0 else best)
}

#' Kruskal's algorithm: greedy on the cycle matroid
#'
#' A disconnected graph has no spanning TREE, so `connected` is reported
#' and the result is a spanning forest. Returning its weight as though
#' it were a tree's would be quietly wrong.
#'
#' @param edges List of vertex pairs, vertices numbered from 1.
#' @param n_vertices Number of vertices.
#' @param weights Edge weights; default all 1.
#' @return A list with `weight`, `tree_edges`, `connected`,
#'   `n_components`, `warnings`.
#' @references Kruskal JB (1956) \emph{Proc AMS} 7(1):48-50.
#' @export
#' @examples
#' K4E <- list(c(1, 2), c(2, 3), c(3, 4), c(4, 1), c(1, 3))
#' morie_minimum_spanning_tree(K4E, 4, c(4, 1, 3, 2, 5))
morie_minimum_spanning_tree <- function(edges, n_vertices, weights = NULL) {
  E <- lapply(edges, as.integer)
  m <- length(E)
  n <- as.integer(n_vertices)
  if (n < 1L) stop(sprintf("n_vertices must be positive; got %d", n),
                   call. = FALSE)
  w <- if (is.null(weights)) rep(1, m) else as.numeric(weights)
  if (length(w) != m) {
    stop(sprintf("weights has length %d but there are %d edges.",
                 length(w), m), call. = FALSE)
  }
  for (e in E) {
    if (any(e < 1L | e > n)) {
      stop(sprintf("edge (%s) leaves 1 .. %d", paste(e, collapse = ", "), n),
           call. = FALSE)
    }
  }
  uf <- seq_len(n)
  find <- function(x) {
    while (uf[x] != x) x <- uf[x]
    x
  }
  ord <- if (m >= 1L) order(w, seq_len(m)) else integer(0)
  chosen <- integer(0)
  total <- 0
  for (i in ord) {
    a <- find(E[[i]][1])
    b <- find(E[[i]][2])
    if (a != b) {
      uf[a] <- b
      chosen <- c(chosen, i)
      total <- total + w[i]
    }
  }
  comps <- length(unique(vapply(seq_len(n), find, integer(1))))
  connected <- comps == 1L
  warns <- character(0)
  if (!connected) {
    warns <- sprintf(paste(
      "The graph has %d components, so no spanning TREE exists. The %d edges",
      "returned are a spanning forest and the weight is the forest's, not a",
      "tree's."), comps, length(chosen))
  }
  list(weight = total, tree_edges = chosen, connected = connected,
       n_components = comps, n_edges_chosen = length(chosen),
       expected_tree_edges = n - 1L, n = n, warnings = warns,
       method = "Kruskal's algorithm = greedy on the cycle matroid")
}

#' Maximum bipartite matching by augmenting paths
#'
#' @param left_n,right_n Side sizes.
#' @param edges List of (left, right) pairs, numbered from 1.
#' @return A list with `size`, `pairs`, `match_right`.
#' @export
morie_bipartite_matching <- function(left_n, right_n, edges) {
  ln <- as.integer(left_n)
  rn <- as.integer(right_n)
  if (ln < 0L || rn < 0L) stop("both sides must be non-negative.",
                               call. = FALSE)
  adj <- vector("list", max(ln, 1L))
  for (i in seq_len(ln)) adj[[i]] <- integer(0)
  for (e in edges) {
    a <- as.integer(e[1])
    b <- as.integer(e[2])
    if (a < 1L || a > ln) {
      stop(sprintf("left endpoint %d leaves 1 .. %d", a, ln), call. = FALSE)
    }
    if (b < 1L || b > rn) {
      stop(sprintf("right endpoint %d leaves 1 .. %d", b, rn), call. = FALSE)
    }
    adj[[a]] <- c(adj[[a]], b)
  }
  # An environment rather than <<- on a recursion parameter. `<<-`
  # inside a recursive call walks OUT of each frame looking for the
  # name, which on deep recursion exhausts the node stack; an
  # environment is a reference and needs no assignment operator games.
  st <- new.env(parent = emptyenv())
  st$match_r <- rep(0L, rn)
  augment <- function(u) {
    for (v in adj[[u]]) {
      if (!st$seen[v]) {
        st$seen[v] <- TRUE
        if (st$match_r[v] == 0L || augment(st$match_r[v])) {
          st$match_r[v] <- u
          return(TRUE)
        }
      }
    }
    FALSE
  }
  size <- 0L
  for (u in seq_len(ln)) {
    st$seen <- rep(FALSE, rn)
    if (augment(u)) size <- size + 1L
  }
  match_r <- st$match_r
  pairs <- which(match_r != 0L)
  list(size = size,
       pairs = if (length(pairs)) cbind(match_r[pairs], pairs) else
         matrix(integer(0), 0, 2),
       match_right = match_r, left_n = ln, right_n = rn)
}

#' Konig's theorem: maximum matching equals minimum vertex cover
#'
#' Both sides are computed and compared, and the cover is separately
#' VERIFIED to cover every edge -- a construction returning the right
#' size while missing an edge would otherwise pass.
#'
#' @param left_n,right_n Side sizes.
#' @param edges List of (left, right) pairs, numbered from 1.
#' @return A list with `matching_size`, `cover_size`, `cover_is_valid`,
#'   `konig_holds`, `warnings`.
#' @references Konig D (1931). Grafok es matrixok. \emph{Matematikai es
#'   Fizikai Lapok}, 38, 116-119.
#' @export
#' @examples
#' E <- list(c(1, 1), c(1, 2), c(2, 2), c(3, 1), c(3, 3))
#' morie_konig_theorem(3, 3, E)
morie_konig_theorem <- function(left_n, right_n, edges) {
  ln <- as.integer(left_n)
  rn <- as.integer(right_n)
  m <- morie_bipartite_matching(ln, rn, edges)
  match_r <- m$match_right
  match_l <- rep(0L, ln)
  for (v in seq_len(rn)) if (match_r[v] != 0L) match_l[match_r[v]] <- v
  adj <- vector("list", max(ln, 1L))
  for (i in seq_len(ln)) adj[[i]] <- integer(0)
  for (e in edges) {
    adj[[as.integer(e[1])]] <- c(adj[[as.integer(e[1])]], as.integer(e[2]))
  }
  vis_l <- rep(FALSE, ln)
  vis_r <- rep(FALSE, rn)
  stack <- which(match_l == 0L)
  vis_l[stack] <- TRUE
  while (length(stack)) {
    u <- stack[length(stack)]
    stack <- stack[-length(stack)]
    for (v in adj[[u]]) {
      if (v != match_l[u] && !vis_r[v]) {
        vis_r[v] <- TRUE
        nu <- match_r[v]
        if (nu != 0L && !vis_l[nu]) { vis_l[nu] <- TRUE
        stack <- c(stack, nu) }
      }
    }
  }
  cover_left <- which(!vis_l)
  cover_right <- which(vis_r)
  cover_size <- length(cover_left) + length(cover_right)
  uncovered <- Filter(function(e) {
    !(as.integer(e[1]) %in% cover_left) && !(as.integer(e[2]) %in% cover_right)
  }, edges)
  valid <- length(uncovered) == 0L
  warns <- character(0)
  if (!valid) {
    warns <- c(warns, sprintf(paste(
      "The constructed cover misses %d edges, so it is not a vertex cover at",
      "all."), length(uncovered)))
  }
  if (m$size != cover_size) {
    warns <- c(warns, sprintf(paste(
      "Matching (%d) and cover (%d) differ, contradicting Konig's theorem."),
      m$size, cover_size))
  }
  list(matching_size = m$size, matching = m$pairs, cover_size = cover_size,
       cover_left = cover_left, cover_right = cover_right,
       cover_is_valid = valid, n_uncovered = length(uncovered),
       konig_holds = m$size == cover_size && valid, n = ln + rn,
       warnings = warns, method = "Konig's theorem (Konig 1931)")
}

#' Hall's marriage theorem
#'
#' A matching saturating the left side exists exactly when every subset
#' \eqn{S} satisfies \eqn{|N(S)| \ge |S|}. Checked exhaustively and
#' cross-checked against the matching. When it fails the VIOLATING SET
#' is returned: Hall's theorem is most useful as a certificate of
#' impossibility, and a bare FALSE is not one.
#'
#' @param left_n,right_n Side sizes.
#' @param edges List of (left, right) pairs, numbered from 1.
#' @return A list with `holds`, `violating_set`, `deficiency`,
#'   `matching_size`, `agrees_with_matching`, `warnings`.
#' @references Hall P (1935) \emph{J London Math Soc} 10:26-30.
#' @export
#' @examples
#' morie_hall_condition(3, 2, list(c(1, 1), c(2, 1), c(3, 2)))
morie_hall_condition <- function(left_n, right_n, edges) {
  ln <- as.integer(left_n)
  rn <- as.integer(right_n)
  nbr <- vector("list", max(ln, 1L))
  for (i in seq_len(ln)) nbr[[i]] <- integer(0)
  for (e in edges) {
    a <- as.integer(e[1])
    b <- as.integer(e[2])
    if (a < 1L || a > ln || b < 1L || b > rn) {
      stop("an edge endpoint is out of range.", call. = FALSE)
    }
    nbr[[a]] <- union(nbr[[a]], b)
  }
  holds <- TRUE
  worst <- NULL
  worst_def <- 0L
  if (ln >= 1L) {
    for (r in seq_len(ln)) {
      cmb <- utils::combn(ln, r, simplify = FALSE)
      for (S in cmb) {
        un <- unique(unlist(nbr[S]))
        deficit <- length(S) - length(un)
        if (deficit > 0L) {
          holds <- FALSE
          if (deficit > worst_def) { worst_def <- deficit
          worst <- S }
        }
      }
    }
  }
  m <- morie_bipartite_matching(ln, rn, edges)
  agrees <- (m$size == ln) == holds
  warns <- character(0)
  if (!agrees) {
    warns <- paste("Hall's condition and the computed matching disagree",
                   "about whether the left side can be saturated.")
  }
  list(holds = holds, violating_set = worst, deficiency = worst_def,
       matching_size = m$size, perfect_on_left = m$size == ln,
       agrees_with_matching = agrees, n = ln + rn, warnings = warns,
       method = "Hall's marriage theorem (Hall 1935)")
}

#' Max-flow min-cut by Ford-Fulkerson with breadth-first augmentation
#'
#' Returns both the flow value and a minimum cut. The cut is derived
#' from the residual graph and its capacity RECOMPUTED from the original
#' matrix rather than assumed equal, so the theorem is a check rather
#' than a restatement.
#'
#' @param capacity Square non-negative matrix; `capacity\[i, j\]` is the
#'   arc capacity from i to j.
#' @param source,sink Node indices, numbered from 1.
#' @return A list with `flow`, `cut_capacity`, `min_cut_source_side`,
#'   `theorem_holds`, `warnings`.
#' @references Ford LR, Fulkerson DR (1956) \emph{Canad J Math}
#'   8:399-404.
#' @export
#' @examples
#' C <- matrix(c(0, 3, 2, 0, 0, 0, 5, 2, 0, 0, 0, 3, 0, 0, 0, 0),
#'     4, 4, byrow = TRUE)
#' morie_max_flow_min_cut(C, 1, 4)
morie_max_flow_min_cut <- function(capacity, source = 1L, sink = NULL) {
  C <- as.matrix(capacity)
  storage.mode(C) <- "double"
  n <- nrow(C)
  if (ncol(C) != n) stop("capacity must be square.", call. = FALSE)
  if (any(C < 0)) stop("capacities must be non-negative.", call. = FALSE)
  s <- as.integer(source)
  t <- if (is.null(sink)) n else as.integer(sink)
  if (s < 1L || s > n || t < 1L || t > n) {
    stop(sprintf("source and sink must lie in 1 .. %d", n), call. = FALSE)
  }
  if (s == t) stop("source and sink must differ.", call. = FALSE)

  R <- C
  flow <- 0
  repeat {
    parent <- rep(0L, n)
    parent[s] <- s
    queue <- s
    while (length(queue) && parent[t] == 0L) {
      u <- queue[1]
      queue <- queue[-1]
      for (v in seq_len(n)) {
        if (parent[v] == 0L && R[u, v] > 1e-12) {
          parent[v] <- u
          queue <- c(queue, v)
        }
      }
    }
    if (parent[t] == 0L) break
    push <- Inf
    v <- t
    while (v != s) { u <- parent[v]
    push <- min(push, R[u, v])
    v <- u }
    v <- t
    while (v != s) {
      u <- parent[v]
      R[u, v] <- R[u, v] - push
      R[v, u] <- R[v, u] + push
      v <- u
    }
    flow <- flow + push
  }
  reach <- rep(FALSE, n)
  reach[s] <- TRUE
  queue <- s
  while (length(queue)) {
    u <- queue[1]
    queue <- queue[-1]
    for (v in seq_len(n)) {
      if (!reach[v] && R[u, v] > 1e-12) { reach[v] <- TRUE
      queue <- c(queue, v) }
    }
  }
  cut_cap <- 0
  cut_edges <- list()
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (reach[i] && !reach[j] && C[i, j] > 0) {
        cut_cap <- cut_cap + C[i, j]
        cut_edges[[length(cut_edges) + 1L]] <- c(i, j)
      }
    }
  }
  holds <- abs(flow - cut_cap) < 1e-9
  warns <- character(0)
  if (!holds) {
    warns <- sprintf(paste(
      "The flow (%g) and the cut capacity (%g) differ, contradicting the",
      "max-flow min-cut theorem."), flow, cut_cap)
  }
  list(flow = flow, cut_capacity = cut_cap,
       min_cut_source_side = which(reach), cut_edges = cut_edges,
       theorem_holds = holds, residual_gap = abs(flow - cut_cap),
       source = s, sink = t, n = n, warnings = warns,
       method = "Ford-Fulkerson with a verified minimum cut")
}
