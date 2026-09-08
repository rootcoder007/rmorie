# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of metsem -- metagenome assembly. Mirrors src/morie/fn/metsem.py
# operation for operation, on the shared numerics in
# R/aaa_helpers_w3num.R.
#
# Assembling one genome is a graph problem. Assembling a community is
# the same graph problem with the assumption that breaks it removed. A
# single-genome assembler leans on uniform coverage: a stretch of the
# graph seen far less often than the rest is an error, and can be cut
# away. In a metagenome that reasoning destroys the data, because a
# species at one percent abundance is genuinely covered a hundred times
# less than the dominant one, and it is not an error -- it is the
# finding.
#
# metaSPAdes' answer, and this module's, is to make every coverage test
# RELATIVE. A dead end is removed when it is much thinner than the path
# it hangs off, not when it is thin in absolute terms. A rare organism's
# contigs are thin everywhere, so nothing about them is locally
# anomalous, and they survive.
#
#   THE DE BRUIJN GRAPH. Nodes are the k-1-mers, edges are the k-mers,
#   and an edge's weight is how many times that k-mer was read.
#
#   THE UNITIGS. Maximal non-branching paths -- walk forward while the
#   next node has exactly one way in and one way out. Where the walk has
#   to stop is where the data genuinely stops determining the sequence.
#   Circular components with no branch at all are found separately,
#   because a cycle has no starting node to walk from and an assembler
#   that only walked from branch points would silently drop every
#   plasmid.
#
#   THE CLEANING, both relative. A TIP is a short dead-end whose
#   coverage is a small fraction of the branch it leaves; a BUBBLE is
#   two paths between the same two nodes, which is what a single-base
#   difference between two strains looks like, and the thinner side
#   goes. Both thresholds are parameters and both counts are reported,
#   because an assembler that cleaned silently would be
#   indistinguishable from one that lost data.
#
# Contigs come back sorted longest first, ties broken by sequence in
# byte order, so the output is a function of the reads and the
# parameters and of nothing else.
#
# References
#   Nurk, S., Meleshko, D., Korobeynikov, A. and Pevzner, P.A. (2017)
#     "metaSPAdes: a new versatile metagenomic assembler." Genome
#     Research 27(5), 824-834. doi:10.1101/gr.213959.116.
#   Pevzner, P.A., Tang, H. and Waterman, M.S. (2001) "An Eulerian path
#     approach to DNA fragment assembly." PNAS 98(17), 9748-9753.
#   Bankevich, A. et al. (2012) "SPAdes: a new genome assembly
#     algorithm and its applications to single-cell sequencing."
#     Journal of Computational Biology 19(5), 455-477.
#     doi:10.1089/cmb.2012.0021.

#' Every k-mer of a sequence, in order, with repeats kept
#'
#' @param seq A sequence.
#' @param k The k-mer length.
#' @return A character vector.
#' @export
morie_metsem_kmers <- function(seq, k) {
  k <- as.integer(k)
  if (k < 2L) stop("a de Bruijn graph needs k of at least two")
  n <- nchar(seq) - k + 1L
  if (n < 1L) return(character(0))
  vapply(seq_len(n), function(i) substr(seq, i, i + k - 1L),
         character(1))
}

#' .metsem_index
#'
#' A step of the metsem_native implementation. Called by \code{.metsem_drop},
#' \code{morie_metsem_graph}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param edges Passed to \code{names}.
#' @param k Numeric; combined arithmetically in the body.
#' @return A list with \code{out}, \code{inc}, \code{nodes}.
#' @export
.metsem_index <- function(edges, k) {
  out <- list()
  inc <- list()
  nodes <- character(0)
  for (km in sort(names(edges), method = "radix")) {
    a <- substr(km, 1L, k - 1L)
    b <- substr(km, 2L, k)
    nodes <- c(nodes, a, b)
    out[[a]] <- c(out[[a]], km)
    inc[[b]] <- c(inc[[b]], km)
  }
  list(out = out, inc = inc,
       nodes = sort(unique(nodes), method = "radix"))
}

#' Nodes are the k-1-mers, edges the k-mers, weights the counts
#'
#' Reads shorter than k contribute nothing and are counted rather than
#' dropped in silence: a run where most reads are shorter than the
#' chosen k has a k problem, and the caller should be told.
#'
#' @param reads The reads.
#' @param k The k-mer length.
#' @return A list with the edge weights, the adjacency, the nodes and
#'   the read counts.
#' @export
morie_metsem_graph <- function(reads, k) {
  k <- as.integer(k)
  edges <- list()
  short <- 0L
  used <- 0L
  for (r in as.character(reads)) {
    if (nchar(r) < k) { short <- short + 1L
    next }
    used <- used + 1L
    for (km in morie_metsem_kmers(r, k))
      edges[[km]] <- if (is.null(edges[[km]])) 1L else edges[[km]] + 1L
  }
  ix <- .metsem_index(edges, k)
  list(edges = edges, out = ix$out, inc = ix$inc, nodes = ix$nodes,
       k = k, short = short, used = used)
}

#' .metsem_outdeg
#'
#' A step of the metsem_native implementation. Called by \code{.metsem_walk},
#' \code{morie_metsem}, \code{morie_metsem_unitigs}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$out} from it.
#' @param v Passed to \code{\%in\%}.
#' @return One of two values, depending on the branch taken.
#' @export
.metsem_outdeg <- function(g, v)
  if (v %in% names(g$out)) length(g$out[[v]]) else 0L
#' .metsem_indeg
#'
#' A step of the metsem_native implementation. Called by \code{.metsem_walk},
#' \code{morie_metsem}, \code{morie_metsem_unitigs}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$inc} from it.
#' @param v Passed to \code{\%in\%}.
#' @return One of two values, depending on the branch taken.
#' @export
.metsem_indeg <- function(g, v)
  if (v %in% names(g$inc)) length(g$inc[[v]]) else 0L

#' .metsem_walk
#'
#' A step of the metsem_native implementation. Called by \code{morie_metsem_unitigs}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$out} from it.
#' @param first A vector; its length is taken.
#' @return The value of \code{path}, as built in the body.
#' @export
.metsem_walk <- function(g, first) {
  path <- first
  v <- substr(first, 2L, nchar(first))
  while (.metsem_outdeg(g, v) == 1L && .metsem_indeg(g, v) == 1L) {
    nxt <- g$out[[v]][1]
    if (nxt %in% path) break
    path <- c(path, nxt)
    v <- substr(nxt, 2L, nchar(nxt))
  }
  path
}

#' .metsem_seq
#'
#' A step of the metsem_native implementation. Called by \code{morie_metsem_unitigs}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param path A vector; its length is taken and its elements indexed.
#' @return A character value.
#' @export
.metsem_seq <- function(path) {
  if (length(path) == 1L) return(path[1])
  paste0(path[1],
         paste(vapply(path[-1], function(e)
           substr(e, nchar(e), nchar(e)), character(1)),
           collapse = ""))
}

#' Maximal non-branching paths, plus the pure cycles
#'
#' A branch point is any node that is not exactly one-in one-out. Every
#' walk starts at an edge leaving one. What is left over after that is
#' made only of one-in one-out nodes, which means it is a cycle; those
#' are picked up separately so a circular replicon does not vanish for
#' want of a place to start.
#'
#' @param g A graph from the graph builder.
#' @return A list of unitigs, longest first.
#' @export
morie_metsem_unitigs <- function(g) {
  seen <- character(0)
  paths <- list()
  for (v in g$nodes) {
    od <- .metsem_outdeg(g, v)
    id <- .metsem_indeg(g, v)
    if (od > 0L && !(od == 1L && id == 1L)) {
      for (e in g$out[[v]]) {
        if (e %in% seen) next
        p <- .metsem_walk(g, e)
        seen <- c(seen, p)
        paths[[length(paths) + 1L]] <- p
      }
    }
  }
  for (km in sort(names(g$edges), method = "radix")) {
    if (km %in% seen) next
    p <- .metsem_walk(g, km)
    seen <- c(seen, p)
    paths[[length(paths) + 1L]] <- p
  }
  if (!length(paths)) return(list())
  rows <- lapply(paths, function(p) {
    w <- vapply(p, function(e) as.numeric(g$edges[[e]]), numeric(1))
    s <- .metsem_seq(p)
    list(path = p, seq = s, start = substr(p[1], 1L, nchar(p[1]) - 1L),
         end = substr(p[length(p)], 2L, nchar(p[length(p)])),
         length = nchar(s), coverage = .w3_csum(w) / length(w),
         n_edges = length(p))
  })
  lens <- vapply(rows, function(r) r$length, numeric(1))
  sqs <- vapply(rows, function(r) r$seq, character(1))
  rows[order(-lens, sqs, method = "radix")]
}

#' The length at which half the assembly is in contigs that long
#'
#' Sorted longest first, accumulate until the running total reaches half
#' the assembly; the contig you were on is the answer. Reported for an
#' empty assembly as zero rather than as an error, because an assembly
#' with no contigs is a result and not a mistake.
#'
#' @param lengths The contig lengths.
#' @return An integer.
#' @export
morie_metsem_n50 <- function(lengths) {
  ls <- sort(as.integer(lengths), decreasing = TRUE)
  total <- sum(ls)
  if (total == 0L) return(0L)
  run <- 0L
  for (v in ls) {
    run <- run + v
    if (run * 2L >= total) return(v)
  }
  ls[length(ls)]
}

#' .metsem_drop
#'
#' A step of the metsem_native implementation. Called by \code{morie_metsem}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A list; the body reads \code{$edges}, \code{$inc}, \code{$k}, \code{$nodes},
#' \code{$out} from it.
#' @param es See Usage.
#' @return The value of \code{g}, as built in the body.
#' @export
.metsem_drop <- function(g, es) {
  for (e in es) if (!is.null(g$edges[[e]])) g$edges[[e]] <- NULL
  ix <- .metsem_index(g$edges, g$k)
  g$out <- ix$out
  g$inc <- ix$inc
  g$nodes <- ix$nodes
  g
}

#' Assemble a community from its reads
#'
#' @param reads The reads.
#' @param k The k-mer length.
#' @param tip_length A dead-end shorter than this is a tip candidate.
#'   NULL is twice k, the usual choice, stated rather than hidden.
#' @param tip_ratio A tip goes only if its coverage is below this
#'   fraction of the best branch it leaves -- RELATIVE, which is the
#'   whole point: an absolute cut-off deletes the rare organisms.
#' @param bubble_ratio The thinner side of a bubble goes if it is below
#'   this fraction of the thicker.
#' @param rounds Cleaning passes.
#' @param min_length Contigs shorter than this are reported separately
#'   rather than thrown away.
#' @return A list with the contigs longest first, their coverage, the
#'   N50, and what the cleaning removed.
#' @export
morie_metsem <- function(reads, k, tip_length = NULL, tip_ratio = 0.2,
                         bubble_ratio = 0.5, rounds = 2L,
                         min_length = NULL) {
  k <- as.integer(k)
  if (k < 2L) stop("a de Bruijn graph needs k of at least two")
  rs <- as.character(reads)
  if (!length(rs)) stop("an assembly needs reads")
  if (is.null(tip_length)) tip_length <- 2L * k
  g <- morie_metsem_graph(rs, k)
  n_edges0 <- length(g$edges)
  tips <- 0L
  bubbles <- 0L
  for (it in seq_len(as.integer(rounds))) {
    us <- morie_metsem_unitigs(g)
    keys <- vapply(us, function(u) paste0(u$start, "|", u$end),
                   character(1))
    drop <- character(0)
    for (key in sort(unique(keys), method = "radix")) {
      grp <- us[keys == key]
      if (length(grp) < 2L) next
      cv <- vapply(grp, function(r) r$coverage, numeric(1))
      sq <- vapply(grp, function(r) r$seq, character(1))
      grp <- grp[order(-cv, sq, method = "radix")]
      for (q in 2:length(grp))
        if (grp[[q]]$coverage <= bubble_ratio * grp[[1]]$coverage) {
          drop <- c(drop, grp[[q]]$path)
          bubbles <- bubbles + 1L
        }
    }
    if (length(drop)) { g <- .metsem_drop(g, drop)
    next }
    drop <- character(0)
    for (ui in seq_along(us)) {
      u <- us[[ui]]
      dead <- .metsem_outdeg(g, u$end) == 0L ||
        .metsem_indeg(g, u$start) == 0L
      if (!dead || u$length >= tip_length) next
      neigh <- numeric(0)
      for (oi in seq_along(us)) {
        if (oi == ui) next
        o <- us[[oi]]
        if (o$end == u$start || o$start == u$end ||
            o$start == u$start || o$end == u$end)
          neigh <- c(neigh, o$coverage)
      }
      if (!length(neigh)) next
      best <- neigh[1]
      for (v in neigh) if (v > best) best <- v
      if (u$coverage <= tip_ratio * best) {
        drop <- c(drop, u$path)
        tips <- tips + 1L
      }
    }
    if (!length(drop)) break
    g <- .metsem_drop(g, drop)
  }

  us <- morie_metsem_unitigs(g)
  if (is.null(min_length)) {
    keep <- us
    short <- list()
  } else {
    ok <- vapply(us, function(u) u$length >= as.integer(min_length),
                 logical(1))
    keep <- us[ok]
    short <- us[!ok]
  }
  lens <- if (length(keep)) vapply(keep, function(u) u$length,
                                   numeric(1)) else numeric(0)
  list(contigs = vapply(keep, function(u) u$seq, character(1)),
       lengths = lens,
       coverage = if (length(keep))
         vapply(keep, function(u) u$coverage, numeric(1))
       else numeric(0),
       starts = vapply(keep, function(u) u$start, character(1)),
       ends = vapply(keep, function(u) u$end, character(1)),
       short_contigs = vapply(short, function(u) u$seq, character(1)),
       n_contigs = length(keep), n_short = length(short),
       total_length = sum(lens),
       longest = if (length(lens)) lens[1] else 0,
       n50 = morie_metsem_n50(lens), n_tips_removed = tips,
       n_bubbles_removed = bubbles, n_kmers = length(g$edges),
       n_kmers_initial = n_edges0, n_nodes = length(g$nodes),
       n_reads = length(rs), n_reads_used = g$used,
       n_reads_too_short = g$short, k = k,
       tip_length = as.integer(tip_length),
       tip_ratio = as.numeric(tip_ratio),
       bubble_ratio = as.numeric(bubble_ratio),
       method = paste0("de Bruijn assembly with relative tip and ",
                       "bubble removal"))
}

#' One-line summary of the metsem module
#'
#' @return A character scalar.
#' @export
morie_metsem_cheatsheet <- function()
  paste0("metsem: metagenome assembly. de Bruijn graph, maximal ",
         "non-branching unitigs, tips and bubbles removed on RELATIVE ",
         "coverage so rare organisms survive")
