# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Kraken-style taxonomic classification (Taxass). Bit-identical
# mirror of src/morie/fn/taxass.py. Anchored on hand-computed RTL
# path scores over a five-taxon toy taxonomy, including the tie ->
# LCA rule.

#' Kraken-style taxonomic classification by RTL path scoring
#'
#' Each query k-mer maps (via the database) to the lowest common
#' ancestor taxon of the genomes containing it; unmapped k-mers are
#' ignored. Hit taxa and their ancestors form a pruned
#' classification tree weighted by hit counts. Every root-to-leaf
#' (RTL) path is scored by the sum of node weights along it; the
#' query is assigned the leaf of the maximal RTL path, with ties
#' resolved to the LCA of the tied leaves. No hits leaves the query
#' unclassified (taxon 0). Kraken 2 applies the same classification
#' step to minimizer-based LCA hits.
#'
#' @param kmer_taxa Integer taxon id hit by each k-mer (0 = no hit).
#' @param parent Named vector or list mapping taxon id to parent id;
#'   the root maps to itself.
#' @return List with \code{taxon}, \code{leaf_scores},
#'   \code{weights}, \code{n_kmers}, \code{n_hit}, \code{method}.
#' @references Wood, D. E. and Salzberg, S. L. (2014), Kraken:
#'   ultrafast metagenomic sequence classification using exact
#'   alignments, Genome Biology 15(3), R46. Sequence classification
#'   algorithm (RTL scoring, tie to LCA), p. 8 and Figure 1. Wood,
#'   D. E., Lu, J. and Langmead, B. (2019), Improved metagenomic
#'   analysis with Kraken 2, Genome Biology 20, 257, Methods. Local
#'   sources:
#'   library/pdf/fetched-wave3/Wood-Salzberg-2014-Kraken1-GenomeBiology.pdf
#'   and Wood-Lu-Langmead-2019-Kraken2-GenomeBiology.pdf.
#' @export
Taxass <- function(kmer_taxa, parent) {
  hits <- as.integer(kmer_taxa)
  pn <- as.integer(unlist(parent))
  names(pn) <- names(unlist(parent))
  par_of <- function(t) {
    v <- pn[[as.character(t)]]
    if (is.null(v)) t else v
  }
  path_to_root <- function(t) {
    path <- t
    while (par_of(t) != t) {
      t <- par_of(t)
      path <- c(path, t)
    }
    path
  }
  for (k in seq_along(pn)) {
    v <- pn[[k]]
    if (v != as.integer(names(pn)[k]) &&
        is.null(pn[[as.character(v)]])) {
      stop(sprintf("parent map is missing taxon %d", v), call. = FALSE)
    }
  }
  weights <- new.env(hash = TRUE)
  for (t in hits) {
    if (t == 0L) next
    if (is.null(pn[[as.character(t)]])) {
      stop(sprintf("hit taxon %d not in parent map", t), call. = FALSE)
    }
    key <- as.character(t)
    old <- if (exists(key, envir = weights, inherits = FALSE)) {
      get(key, envir = weights)
    } else 0L
    assign(key, old + 1L, envir = weights)
  }
  wkeys <- ls(weights)
  n_hit <- if (length(wkeys)) {
    sum(vapply(wkeys, function(k) get(k, envir = weights), 0L))
  } else 0L
  if (n_hit == 0L) {
    return(list(taxon = 0L, leaf_scores = list(), weights = list(),
                n_kmers = length(hits), n_hit = 0L,
                method = "Kraken RTL-path classification (Wood-Salzberg 2014)"))
  }
  tree <- integer(0)
  for (k in wkeys) tree <- union(tree, path_to_root(as.integer(k)))
  nchild <- stats::setNames(rep(0L, length(tree)), as.character(tree))
  for (t in tree) {
    p <- par_of(t)
    if (p != t && as.character(p) %in% names(nchild)) {
      nchild[[as.character(p)]] <- nchild[[as.character(p)]] + 1L
    }
  }
  leaves <- sort(tree[nchild[as.character(tree)] == 0L])
  wt <- function(t) {
    key <- as.character(t)
    if (exists(key, envir = weights, inherits = FALSE)) {
      get(key, envir = weights)
    } else 0L
  }
  scores <- vapply(leaves,
                   function(lf) sum(vapply(path_to_root(lf), wt, 0L)),
                   0L)
  best <- max(scores)
  top <- leaves[scores == best]
  lca2 <- function(a, b) {
    pa <- path_to_root(a)
    x <- b
    while (!(x %in% pa)) x <- par_of(x)
    x
  }
  label <- top[1]
  if (length(top) > 1L) {
    for (o in top[-1]) label <- lca2(label, o)
  }
  list(taxon = as.integer(label),
       leaf_scores = stats::setNames(as.list(as.integer(scores)),
                                     as.character(leaves)),
       weights = stats::setNames(
         lapply(wkeys, function(k) get(k, envir = weights)), wkeys),
       n_kmers = length(hits), n_hit = as.integer(n_hit),
       method = "Kraken RTL-path classification (Wood-Salzberg 2014)")
}
