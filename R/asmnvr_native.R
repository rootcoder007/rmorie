# De novo fragment assembly by the Eulerian-path (de Bruijn) approach.
# Sources: Pevzner, P. A., Tang, H. & Waterman, M. S. (2001) "An
# Eulerian path approach to DNA fragment assembly", Proceedings of
# the National Academy of Sciences 98(17), 9748-9753 (the de Bruijn
# graph G(S, l) and the Eulerian-superpath idea for repeat
# resolution); Hierholzer, C. (1873) "Ueber die Moeglichkeit, einen
# Linienzug ohne Wiederholung und ohne Unterbrechung zu umfahren",
# Mathematische Annalen 6(1), 30-32, communicated posthumously by
# Chr. Wiener (the existence condition and the constructive
# circuit-splicing proof that eulerian_path implements).
#
# Native implementation mirroring Python morie.fn.asmnvr exactly:
# the same de Bruijn construction, the same Hierholzer traversal,
# the same unitigs as the unambiguous fallback when no Eulerian
# path exists, and the same payload keys.

#' De novo assembly via the de Bruijn graph and an Eulerian path
#'
#' Reads are broken into k-mers; vertices are (k-1)-mers and each
#' k-mer is an edge. The overlap-layout-consensus paradigm asks for
#' a Hamiltonian path (NP-complete); this asks for an Eulerian
#' path (easy, by Hierholzer 1873). Repeat resolution via Eulerian
#' superpaths and error correction are NOT implemented here -- a
#' branching graph is reported ambiguous and only the unambiguous
#' unitigs are returned, matching the Python arm's honesty about
#' the boundary.
#'
#' @param reads Character vector of reads (need not be equal length).
#' @param k Integer k-mer length, >= 2. NULL means the shortest read
#'   length.
#' @param multiplicity "set" (default) gives one edge per DISTINCT
#'   k-mer (the Idury-Waterman / SBH construction); "count" gives
#'   one edge per OCCURRENCE for callers who already know their
#'   multiplicities.
#' @return A named list with keys matching the Python payload:
#'   estimate, sequence, path, contigs, unambiguous,
#'   length_is_lower_bound, branching, n_kmers, n_vertices, graph,
#'   k, multiplicity, note, method.
#' @references Pevzner, P. A., Tang, H. & Waterman, M. S. (2001).
#'   An Eulerian path approach to DNA fragment assembly. PNAS,
#'   98(17), 9748-9753.
#' @export
morie_asmnvr <- function(reads, k = NULL, multiplicity = "set") {
  rs <- as.character(reads)
  if (length(rs) == 0L)
    stop("asmnvr: reads must be non-empty")
  if (is.null(k))
    k <- min(nchar(rs))
  parts <- de_bruijn_graph(rs, k, multiplicity)
  edges <- parts$edges
  indeg <- parts$indeg
  outdeg <- parts$outdeg
  path <- eulerian_path(edges, indeg, outdeg)
  lower_bound <- (multiplicity == "set")
  unitig_paths <- .unitigs(edges, indeg, outdeg)
  contigs <- vapply(unitig_paths, .spell, character(1))
  verts <- unique(c(ls(indeg, all.names = TRUE),
                    ls(outdeg, all.names = TRUE)))
  branching_mask <- vapply(verts, function(v) {
    iv <- if (is.null(indeg[[v]])) 0L else indeg[[v]]
    ov <- if (is.null(outdeg[[v]])) 0L else outdeg[[v]]
    ov > 1L || iv > 1L
  }, logical(1))
  branching <- sort(verts[branching_mask], method = "radix")
  graph_list <- list()
  for (k2 in sort(ls(edges, all.names = TRUE), method = "radix"))
    graph_list[[k2]] <- edges[[k2]]
  n_kmers <- sum(vapply(ls(edges, all.names = TRUE),
                        function(v) length(edges[[v]]), integer(1)))
  list(
    estimate = if (is.null(path)) NULL else .spell(path),
    sequence = if (is.null(path)) NULL else .spell(path),
    path = path,
    contigs = contigs,
    unambiguous = !is.null(path) && length(branching) == 0L,
    length_is_lower_bound = lower_bound && !is.null(path),
    branching = branching,
    n_kmers = n_kmers,
    n_vertices = length(verts),
    graph = graph_list,
    k = as.integer(k),
    multiplicity = multiplicity,
    note = paste0(
      "repeat resolution (Eulerian superpaths) and error ",
      "correction are NOT implemented; a branching graph is ",
      "reported as ambiguous rather than resolved. With ",
      "multiplicity='set' a k-mer that repeats in the source ",
      "is one edge and is traversed once, so the assembled ",
      "length is a LOWER BOUND on the truth ",
      "(length_is_lower_bound); the collapse cannot be ",
      "detected from the k-mer set, since read coverage ",
      "repeats k-mers too"),
    method = "Eulerian-path assembly (Pevzner, Tang & Waterman 2001)"
  )
}

# Build G(S, l) with l = k. Vertices are (k-1)-mers; each k-mer is
# an edge from its prefix to its suffix. multiplicity="set" gives one
# edge per distinct k-mer (the Idury-Waterman construction);
# "count" gives one per occurrence for callers who already know
# their multiplicities. Returns a list (edges, indeg, outdeg);
# edges/indeg/outdeg are environments keyed by vertex (string),
# paralleling the Python dict-of-lists + counter-dicts.
#' Build G(S, l) with l = k. Vertices are (k-1)-mers; each k-mer is
#'
#' an edge from its prefix to its suffix. multiplicity="set" gives one
#' edge per distinct k-mer (the Idury-Waterman construction); "count"
#' gives one per occurrence for callers who already know their
#' multiplicities. Returns a list (edges, indeg, outdeg);
#' edges/indeg/outdeg are environments keyed by vertex (string),
#' paralleling the Python dict-of-lists + counter-dicts.
#'
#' @param reads Coerced to character by the body, with \code{as.character}.
#' @param k Numeric; combined arithmetically in the body.
#' @param multiplicity One of \code{"count"}, \code{"set"}. Defaults to \code{"set"}.
#' @return A list with \code{edges}, \code{indeg}, \code{outdeg}.
#' @export
de_bruijn_graph <- function(reads, k, multiplicity = "set") {
  if (!(multiplicity %in% c("set", "count")))
    stop(sprintf("asmnvr: multiplicity must be 'set' or 'count', got '%s'",
                 multiplicity))
  k <- as.integer(k)
  if (k < 2L)
    stop("asmnvr: k must be >= 2 (a k-mer needs a (k-1)-mer prefix and suffix)")
  rs <- as.character(reads)
  if (length(rs) == 0L)
    stop("asmnvr: reads must be non-empty")
  edges <- new.env(hash = TRUE, parent = emptyenv())
  indeg <- new.env(hash = TRUE, parent = emptyenv())
  outdeg <- new.env(hash = TRUE, parent = emptyenv())
  seen <- new.env(hash = TRUE, parent = emptyenv())
  kmers <- character(0)
  for (r in rs) {
    if (nchar(r) < k) next
    nkm <- nchar(r) - k + 1L
    for (i in seq_len(nkm)) {
      kmer <- substr(r, i, i + k - 1L)
      if (multiplicity == "set") {
        if (!is.null(seen[[kmer]])) next
        seen[[kmer]] <- TRUE
      }
      kmers <- c(kmers, kmer)
    }
  }
  n_kmers <- 0L
  for (kmer in kmers) {
    v <- substr(kmer, 1L, k - 1L)
    w <- substr(kmer, 2L, k)
    edges[[v]] <- c(edges[[v]], w)
    cur_out <- outdeg[[v]]
    outdeg[[v]] <- if (is.null(cur_out)) 1L else cur_out + 1L
    cur_in <- indeg[[w]]
    indeg[[w]] <- if (is.null(cur_in)) 1L else cur_in + 1L
    if (is.null(indeg[[v]])) indeg[[v]] <- 0L
    if (is.null(outdeg[[w]])) outdeg[[w]] <- 0L
    n_kmers <- n_kmers + 1L
  }
  if (n_kmers == 0L)
    stop(sprintf("asmnvr: no read is at least k = %d long", k))
  list(edges = edges, indeg = indeg, outdeg = outdeg)
}

# Weak connectivity over vertices that carry at least one edge, used
# by eulerian_path to enforce the "connected on its non-isolated
# vertices" half of the Eulerian existence condition.
#' Weak connectivity over vertices that carry at least one edge, used
#'
#' by eulerian_path to enforce the "connected on its non-isolated
#' vertices" half of the Eulerian existence condition.
#'
#' @param edges A vector; indexed elementwise.
#' @param verts See Usage.
#' @return A logical value.
#' @export
.connected <- function(edges, verts) {
  edge_keys <- ls(edges, all.names = TRUE)
  v_with_out <- character(0)
  ws_all <- list()
  for (v in edge_keys) {
    ws <- edges[[v]]
    if (!is.null(ws) && length(ws) > 0L) {
      v_with_out <- c(v_with_out, v)
      ws_all[[v]] <- ws
    }
  }
  v_with_in <- if (length(ws_all) > 0L)
    unique(unlist(ws_all, use.names = FALSE)) else character(0)
  active <- sort(unique(c(v_with_out, v_with_in)), method = "radix")
  if (length(active) == 0L) return(TRUE)
  adj <- list()
  for (v in v_with_out) {
    for (w in ws_all[[v]]) {
      adj[[v]] <- unique(c(adj[[v]], w))
      adj[[w]] <- unique(c(adj[[w]], v))
    }
  }
  start <- active[1L]
  seen <- character(0)
  stack <- start
  while (length(stack) > 0L) {
    x <- stack[length(stack)]
    stack <- stack[-length(stack)]
    if (x %in% seen) next
    seen <- c(seen, x)
    nbrs <- adj[[x]]
    if (!is.null(nbrs))
      stack <- c(stack, nbrs[!(nbrs %in% seen)])
  }
  all(active %in% seen)
}

# Hierholzer's algorithm with the exact existence condition checked:
# connected, and either all in/out degrees equal, or exactly one
# vertex with out - in = 1 (start) and one with in - out = 1 (end).
# Returns NULL when no Eulerian path exists -- ambiguous or
# disconnected graphs are reported, not guessed at.
#' Hierholzer\'s algorithm with the exact existence condition checked:
#'
#' connected, and either all in/out degrees equal, or exactly one vertex
#' with out - in = 1 (start) and one with in - out = 1 (end). Returns
#' NULL when no Eulerian path exists -- ambiguous or disconnected graphs
#' are reported, not guessed at.
#'
#' @param edges A vector; indexed elementwise.
#' @param indeg A vector; indexed elementwise.
#' @param outdeg A vector; indexed elementwise.
#' @return The value of \code{path}, as built in the body.
#' @export
eulerian_path <- function(edges, indeg, outdeg) {
  verts <- unique(c(ls(indeg, all.names = TRUE),
                    ls(outdeg, all.names = TRUE)))
  starts <- character(0)
  ends <- character(0)
  odd <- character(0)
  for (v in verts) {
    iv <- if (is.null(indeg[[v]])) 0L else indeg[[v]]
    ov <- if (is.null(outdeg[[v]])) 0L else outdeg[[v]]
    if (ov - iv == 1L) starts <- c(starts, v)
    if (iv - ov == 1L) ends <- c(ends, v)
    if (abs(iv - ov) > 1L) odd <- c(odd, v)
  }
  if (length(odd) > 0L || length(starts) > 1L ||
      length(ends) > 1L || length(starts) != length(ends))
    return(NULL)
  if (!.connected(edges, verts)) return(NULL)
  if (length(starts) > 0L) {
    start <- starts[1L]
  } else {
    cand <- sort(verts[!vapply(verts,
                               function(v) is.null(edges[[v]]),
                               logical(1))],
                 method = "radix")
    if (length(cand) == 0L) return(NULL)
    start <- cand[1L]
  }
  # Mutable copies of the edge lists, with a per-vertex pointer so we
  # can pop from the end without mutating the caller's edges env.
  nxt <- new.env(hash = TRUE, parent = emptyenv())
  ptr <- new.env(hash = TRUE, parent = emptyenv())
  for (v in ls(edges, all.names = TRUE)) {
    ws <- edges[[v]]
    if (!is.null(ws) && length(ws) > 0L) {
      nxt[[v]] <- as.list(ws)
      ptr[[v]] <- length(ws)
    }
  }
  has_next <- function(v) {
    p <- ptr[[v]]
    !is.null(p) && p >= 1L
  }
  pop_next <- function(v) {
    p <- ptr[[v]] - 1L
    ptr[[v]] <- p
    nxt[[v]][[p + 1L]]
  }
  stack <- c(start)
  path <- character(0)
  while (length(stack) > 0L) {
    v <- stack[length(stack)]
    if (has_next(v)) {
      stack <- c(stack, pop_next(v))
    } else {
      path <- c(path, stack[length(stack)])
      stack <- stack[-length(stack)]
    }
  }
  path <- rev(path)
  total <- sum(vapply(ls(edges, all.names = TRUE),
                      function(v) length(edges[[v]]), integer(1)))
  if (length(path) != total + 1L) return(NULL)
  path
}

# Maximal non-branching paths: the part of the assembly that is
# unambiguous even when the whole graph is not. Walks each edge out
# of every non-simple vertex (in sorted order) and through every
# simple vertex until a branch point; a graph that is one pure
# cycle contributes the cycle itself.
#' Maximal non-branching paths: the part of the assembly that is
#'
#' unambiguous even when the whole graph is not. Walks each edge out of
#' every non-simple vertex (in sorted order) and through every simple
#' vertex until a branch point; a graph that is one pure cycle
#' contributes the cycle itself.
#'
#' @param edges A vector; indexed elementwise.
#' @param indeg A vector; indexed elementwise.
#' @param outdeg A vector; indexed elementwise.
#' @return The value of \code{out}, as built in the body.
#' @export
.unitigs <- function(edges, indeg, outdeg) {
  simple <- function(v) {
    iv <- if (is.null(indeg[[v]])) 0L else indeg[[v]]
    ov <- if (is.null(outdeg[[v]])) 0L else outdeg[[v]]
    iv == 1L && ov == 1L
  }
  get_e <- function(v) {
    x <- edges[[v]]
    if (is.null(x)) character(0) else x
  }
  used <- new.env(hash = TRUE, parent = emptyenv())
  mark <- function(v, i)
    used[[paste0(v, "\r", as.character(i))]] <- TRUE
  marked <- function(v, i)
    !is.null(used[[paste0(v, "\r", as.character(i))]])
  out <- list()
  for (v in sort(ls(edges, all.names = TRUE), method = "radix")) {
    if (simple(v)) next
    ws <- get_e(v)
    for (idx in seq_along(ws)) {
      w <- ws[idx]
      if (marked(v, idx)) next
      mark(v, idx)
      walk <- c(v, w)
      cur <- w
      while (simple(cur) && length(get_e(cur)) > 0L) {
        nxt_v <- get_e(cur)[1L]
        mark(cur, 0L)
        walk <- c(walk, nxt_v)
        cur <- nxt_v
      }
      out[[length(out) + 1L]] <- walk
    }
  }
  if (length(out) == 0L && length(ls(edges, all.names = TRUE)) > 0L) {
    v <- sort(ls(edges, all.names = TRUE), method = "radix")[1L]
    walk <- v
    cur <- v
    seen <- new.env(hash = TRUE, parent = emptyenv())
    seen_set <- function(c, i)
      !is.null(seen[[paste0(c, "\r", as.character(i))]])
    while (length(get_e(cur)) > 0L && !seen_set(cur, 0L)) {
      seen[[paste0(cur, "\r", "0")]] <- TRUE
      cur <- get_e(cur)[1L]
      walk <- c(walk, cur)
      if (cur == v) break
    }
    out[[length(out) + 1L]] <- walk
  }
  out
}

# Spell a walk of (k-1)-mers back into the sequence: the first
# (k-1)-mer in full, then the last character of each subsequent one.
#' Spell a walk of (k-1)-mers back into the sequence: the first
#'
#' (k-1)-mer in full, then the last character of each subsequent one.
#'
#' @param path A vector; its length is taken and its elements indexed.
#' @return A character value.
#' @export
.spell <- function(path) {
  if (length(path) == 0L) return("")
  paste0(path[1L],
         paste0(substring(path[-1L], nchar(path[-1L])), collapse = ""))
}
