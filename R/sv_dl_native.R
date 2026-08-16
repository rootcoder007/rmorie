# morie.fn -- function file (rootcoder007/morie)
# DELLY: structural variant discovery from paired-end and split reads.
#
# Rausch, T., Zichner, T., Schlattl, A., Stuetz, A. M., Benes, V., &
# Korbel, J. O. (2012) "DELLY: structural variant discovery by
# integrated paired-end and split-read analysis", Bioinformatics
# 28(18), i333-i339. doi:10.1093/bioinformatics/bts378
#
# Two components, run in that order (the paper's Figure 1).
#
# Paired-end mapping analysis (Section 2.1): each library has a default
# read-pair orientation and an insert size distribution; a pair is
# discordant if its orientation is wrong or its insert size is more
# than n_sd standard deviations above the median (three by default).
# Signatures: deletion (default orientation, insert far too large),
# tandem duplication (reads swapped order, strands kept), inversion
# (one read flipped; left- and right-spanning clustered apart),
# translocation (mates on different chromosomes; four classes).
# Discordant pairs of one signature are the nodes of a weighted graph;
# an edge joins two pairs that could support the same breakpoint and
# carries the disagreement between the implied SV sizes (for
# translocations, the summed shift of the two left-most positions).
# Within each connected component a clique is grown from the
# lowest-weight edge; the call interval is the intersection all
# supporting pairs agree on.
#
# Split-read analysis (Section 2.2): every non-deletion type is first
# rewritten so a deletion-type search works (Figure 4); reads are
# mapped by k-mer diagonals (k = 7), the gap between consecutive
# diagonals is the SV size a read implies, a gapless majority-vote
# consensus is built, and forward and reverse Gotoh score vectors are
# split at argmax_{i<j} f_i + r_j -- the slack i < j is where a
# non-template microinsertion goes. The refinement is accepted only if
# the split-read length agrees with the paired-end estimate to within
# max_length_diff (10%).
#
# insert_size_stats note: Section 2.1 characterises the library by
# "the median and standard deviation", but the deletion-spanning pairs
# are the large-insert outliers and inflate the SD that is supposed to
# find them; spread="mad" (default) uses 1.4826 * MAD instead, and
# spread="sd" gives the literal reading.
#
# Everything works on sequences and coordinates in memory; a pair is a
# list with chrom1/pos1/strand1 and the same for mate 2.

.sv_dl_TYPES <- c("DEL", "DUP", "INV", "TRA")
.sv_dl_COMPLEMENT <- c(A="T", C="G", G="C", T="A", N="N")

# ------------------------------------------------------------- helpers

#' Normalise one read pair; mate 1 is the left-most alignment
#'
#' A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_classify_pair}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p A list; the body reads \code{$chrom1}, \code{$chrom2}, \code{$id}, \code{$len1}, \code{$len2}, \code{$pos1}, \code{$pos2}, \code{$read_length}, \code{$seq}, \code{$strand1}, \code{$strand2} from it.
#' @return A list with \code{chrom1}, \code{pos1}, \code{strand1}, \code{len1}, \code{chrom2}, \code{pos2}, \code{strand2}, \code{len2}, \code{seq}, \code{id}.
#' @export
.sv_dl_pair <- function(p) {
  # Normalise one read pair; mate 1 is the left-most alignment.
  need <- c("chrom1", "pos1", "strand1", "chrom2", "pos2", "strand2")
  if (any(vapply(need, function(k) is.null(p[[k]]), logical(1)))) {
    stop(paste0("sv_dl: a pair needs chrom1/pos1/strand1 and ",
                "chrom2/pos2/strand2"))
  }
  c1 <- as.character(p[["chrom1"]]); p1 <- as.integer(p[["pos1"]])
  s1 <- as.character(p[["strand1"]])
  c2 <- as.character(p[["chrom2"]]); p2 <- as.integer(p[["pos2"]])
  s2 <- as.character(p[["strand2"]])
  rl <- if (!is.null(p[["read_length"]])) p[["read_length"]] else 100L
  l1 <- as.integer(if (!is.null(p[["len1"]])) p[["len1"]] else rl)
  l2 <- as.integer(if (!is.null(p[["len2"]])) p[["len2"]] else rl)
  if (l1 < 1L || l2 < 1L) {
    stop("sv_dl: read lengths must be positive")
  }
  if (p1 < 0L || p2 < 0L) {
    stop("sv_dl: alignment positions must be non-negative")
  }
  if (!(s1 %in% c("+", "-")) || !(s2 %in% c("+", "-"))) {
    stop("sv_dl: strands must be '+' or '-'")
  }
  swap <- (c2 < c1) || (c2 == c1 && p2 < p1)
  if (swap) {
    tmp <- list(c1, p1, s1, l1)
    c1 <- c2; p1 <- p2; s1 <- s2; l1 <- l2
    c2 <- tmp[[1L]]; p2 <- tmp[[2L]]; s2 <- tmp[[3L]]; l2 <- tmp[[4L]]
  }
  list(chrom1=c1, pos1=p1, strand1=s1, len1=l1,
       chrom2=c2, pos2=p2, strand2=s2, len2=l2,
       seq=p[["seq"]], id=p[["id"]])
}

#' .sv_dl_median
#'
#' A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_insert_size_stats}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; passed to \code{sort}.
#' @return One of two values, depending on the branch taken.
#' @export
.sv_dl_median <- function(v) {
  s <- sort(v)
  n <- length(s)
  if (n == 0L) {
    stop("sv_dl: no values to take a median of")
  }
  if (n %% 2L == 1L) {
    s[(n %/% 2L) + 1L]
  } else {
    0.5 * (s[n %/% 2L] + s[n %/% 2L + 1L])
  }
}

#' Outer distance: left-most start to right-most end
#'
#' A step of the sv_dl_native implementation. Called by \code{.sv_dl_size}, \code{morie_sv_dl_classify_pair}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p A list; the body reads \code{$len2}, \code{$pos1}, \code{$pos2} from it.
#' @return A numeric value.
#' @export
.sv_dl_insert <- function(p) {
  # Outer distance: left-most start to right-most end.
  (p$pos2 + p$len2) - p$pos1
}

#' morie_sv_dl_insert_size_stats
#'
#' A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_paired_end_calls}, \code{morie_sv_dl_structural_variant}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param pairs Iterated over elementwise, with \code{lapply}.
#' @param orientation Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param spread One of \code{"mad"}, \code{"sd"}. Defaults to \code{"mad"}.
#' @return A list with \code{median}, \code{sd}, \code{spread}, \code{orientation}, \code{n}.
#' @export
morie_sv_dl_insert_size_stats <- function(pairs, orientation=NULL,
                                          spread="mad") {
  # Median and spread of the library insert size, and its orientation.
  # spread="mad" (default) uses 1.4826 * MAD, unmoved by the
  # deletion-spanning outliers; spread="sd" gives the paper's literal
  # reading.
  ps <- lapply(pairs, .sv_dl_pair)
  same <- Filter(function(p) p$chrom1 == p$chrom2, ps)
  if (length(same) == 0L) {
    stop(paste0("sv_dl: no same-chromosome pairs to estimate the insert ",
                "size distribution from"))
  }
  if (is.null(orientation)) {
    keys <- vapply(same, function(p) paste(p$strand1, p$strand2),
                   character(1))
    tab <- table(keys)
    best <- names(tab)[order(-as.integer(tab), names(tab))][1L]
    orientation <- strsplit(best, " ")[[1L]]
  }
  orientation <- as.character(orientation)
  if (!(orientation[1L] %in% c("+", "-")) ||
      !(orientation[2L] %in% c("+", "-"))) {
    stop("sv_dl: orientation must be a pair of strands")
  }
  concordant <- vapply(
    Filter(function(p) p$strand1 == orientation[1L] &&
                       p$strand2 == orientation[2L], same),
    .sv_dl_insert, numeric(1))
  if (length(concordant) == 0L) {
    stop("sv_dl: no pairs in the default orientation")
  }
  if (!(spread %in% c("mad", "sd"))) {
    stop("sv_dl: spread must be 'mad' or 'sd'")
  }
  med <- .sv_dl_median(concordant)
  if (length(concordant) > 1L) {
    if (spread == "mad") {
      sd_ <- 1.4826 * .sv_dl_median(abs(concordant - med))
    } else {
      mu <- mean(concordant)
      sd_ <- sqrt(sum((concordant - mu) ^ 2) / (length(concordant) - 1.0))
    }
  } else {
    sd_ <- 0.0
  }
  list(median=as.numeric(med), sd=as.numeric(sd_), spread=spread,
       orientation=orientation, n=length(concordant))
}

#' morie_sv_dl_classify_pair
#'
#' A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_paired_end_calls}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p A list; the body reads \code{$chrom1}, \code{$chrom2}, \code{$strand1}, \code{$strand2} from it.
#' @param median Numeric; combined arithmetically in the body.
#' @param sd Numeric; combined arithmetically in the body.
#' @param orientation A vector; indexed elementwise. Defaults to \code{c("+", "-")}.
#' @param n_sd Numeric; combined arithmetically in the body. Defaults to \code{3}.
#' @return Nothing; the function is called for its effect.
#' @export
morie_sv_dl_classify_pair <- function(p, median, sd, orientation=c("+", "-"),
                                      n_sd=3.0) {
  # The signature of one pair (Section 2.1, Figure 2). Returns NULL
  # for a concordant pair, otherwise c(type, subtype).
  p <- .sv_dl_pair(p)
  d1 <- orientation[1L]
  d2 <- orientation[2L]
  if (p$chrom1 != p$chrom2) {
    # four translocation classes; chromosomes are already in sorted
    # order, so the class is fixed by which strands departed from the
    # library orientation
    t <- 2L * (if (p$strand1 != d1) 1L else 0L) +
      (if (p$strand2 != d2) 1L else 0L)
    return(c("TRA", as.character(t)))
  }
  s1 <- p$strand1
  s2 <- p$strand2
  if (s1 == s2) {
    # one read flipped: an inversion; left- and right-spanning pairs
    # are clustered separately
    return(c("INV", if (s1 == d1) "left" else "right"))
  }
  if (s1 == d2 && s2 == d1) {
    # strands kept but order swapped: tandem duplication
    return(c("DUP", ""))
  }
  if (s1 == d1 && s2 == d2) {
    if (.sv_dl_insert(p) > median + n_sd * sd) {
      return(c("DEL", ""))
    }
    return(NULL)
  }
  NULL
}

#' The SV size this pair implies, used as the clustering weight
#'
#' A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_build_sv_graph}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p Passed to \code{.sv_dl_insert}.
#' @param label A vector; indexed elementwise.
#' @param median Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.sv_dl_size <- function(p, label, median) {
  # The SV size this pair implies, used as the clustering weight.
  if (label[1L] == "TRA") {
    return(NA_real_)
  }
  .sv_dl_insert(p) - median
}

#' morie_sv_dl_build_sv_graph
#'
#' A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_paired_end_calls}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param pairs Iterated over elementwise, with \code{lapply}.
#' @param median Numeric; combined arithmetically in the body.
#' @param sd Numeric; combined arithmetically in the body.
#' @param label A vector; indexed elementwise.
#' @param orientation Defaults to \code{c("+", "-")}.
#' @param n_sd Numeric; combined arithmetically in the body. Defaults to \code{3}.
#' @param window Defaults to \code{NULL}.
#' @return A list with \code{nodes}, \code{edges}, \code{sizes}, \code{label}.
#' @export
morie_sv_dl_build_sv_graph <- function(pairs, median, sd, label,
                                       orientation=c("+", "-"), n_sd=3.0,
                                       window=NULL) {
  # Nodes are pairs of one signature; edges join pairs that agree. An
  # edge requires both left and right ends within the expected insert
  # range; its weight is the disagreement between implied SV sizes.
  # Edges are rows (weight, i, j) with 1-based node indices.
  if (is.null(window)) {
    window <- median + n_sd * sd
  }
  ps <- lapply(pairs, .sv_dl_pair)
  sizes <- vapply(ps, function(p) .sv_dl_size(p, label, median), numeric(1))
  edges <- matrix(numeric(0), 0L, 3L)
  n <- length(ps)
  for (i in seq_len(max(0L, n - 1L))) {
    for (j in seq.int(i + 1L, n)) {
      a <- ps[[i]]
      b <- ps[[j]]
      if (a$chrom1 != b$chrom1 || a$chrom2 != b$chrom2) {
        next
      }
      if (abs(a$pos1 - b$pos1) > window) {
        next
      }
      if (abs(a$pos2 - b$pos2) > window) {
        next
      }
      w <- if (label[1L] == "TRA") {
        abs(a$pos1 - b$pos1) + abs(a$pos2 - b$pos2)
      } else {
        abs(sizes[i] - sizes[j])
      }
      edges <- rbind(edges, c(as.numeric(w), i, j))
    }
  }
  list(nodes=ps, edges=edges, sizes=sizes, label=label)
}

#' Connected components (union-find); singletons dropped
#'
#' A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_paired_end_calls}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param edges A matrix; indexed by row and column.
#' @return The value of \code{out}, as built in the body.
#' @export
.sv_dl_components <- function(n, edges) {
  # Connected components (union-find); singletons dropped.
  parent <- seq_len(n)
  find <- function(a) {
    while (parent[a] != a) {
      parent[a] <<- parent[parent[a]]
      a <- parent[a]
    }
    a
  }
  for (e in seq_len(nrow(edges))) {
    ri <- find(as.integer(edges[e, 2L]))
    rj <- find(as.integer(edges[e, 3L]))
    if (ri != rj) {
      parent[ri] <- rj
    }
  }
  roots <- vapply(seq_len(n), find, integer(1))
  out <- list()
  for (r in unique(roots)) {
    grp <- sort(which(roots == r))
    if (length(grp) > 1L) {
      out <- c(out, list(grp))
    }
  }
  out
}

#' Grow a clique from the lowest-weight edge (Section 2.1): the seed
#'
#' is e_min; then repeatedly the lowest-weight edge with exactly one
#' endpoint inside, whose other endpoint is adjacent to every member.
#'
#' @param members See Usage.
#' @param edges A matrix; indexed by row and column.
#' @return A vector, from \code{sort}.
#' @export
morie_sv_dl_maximal_clique <- function(members, edges) {
  # Grow a clique from the lowest-weight edge (Section 2.1): the seed
  # is e_min; then repeatedly the lowest-weight edge with exactly one
  # endpoint inside, whose other endpoint is adjacent to every member.
  keep <- members
  sel <- edges[edges[, 2L] %in% keep & edges[, 3L] %in% keep, , drop=FALSE]
  if (nrow(sel) == 0L) {
    return(integer(0))
  }
  sel <- sel[order(sel[, 1L], sel[, 2L], sel[, 3L]), , drop=FALSE]
  adj <- list()
  akey <- function(i, j) paste(i, j)
  for (e in seq_len(nrow(sel))) {
    i <- as.integer(sel[e, 2L])
    j <- as.integer(sel[e, 3L])
    adj[[akey(i, j)]] <- sel[e, 1L]
    adj[[akey(j, i)]] <- sel[e, 1L]
  }
  clique <- c(as.integer(sel[1L, 2L]), as.integer(sel[1L, 3L]))
  repeat {
    best_w <- NULL
    best_v <- NULL
    for (e in seq_len(nrow(sel))) {
      i <- as.integer(sel[e, 2L])
      j <- as.integer(sel[e, 3L])
      inside <- (i %in% clique) + (j %in% clique)
      if (inside != 1L) {
        next
      }
      outside <- if (i %in% clique) j else i
      ok <- all(vapply(clique,
                       function(m) !is.null(adj[[akey(outside, m)]]),
                       logical(1)))
      if (ok && (is.null(best_w) || sel[e, 1L] < best_w)) {
        best_w <- sel[e, 1L]
        best_v <- outside
      }
    }
    if (is.null(best_v)) {
      break
    }
    clique <- c(clique, best_v)
  }
  sort(unique(clique))
}

#' morie_sv_dl_paired_end_calls
#'
#' A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_structural_variant}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param pairs Iterated over elementwise, with \code{lapply}.
#' @param median Optional; may be \code{NULL}. Passed to \code{morie_sv_dl_classify_pair}.
#' @param sd Optional; may be \code{NULL}. Passed to \code{morie_sv_dl_classify_pair}.
#' @param orientation Optional; may be \code{NULL}. Passed to \code{morie_sv_dl_insert_size_stats}.
#' @param n_sd Passed to \code{morie_sv_dl_classify_pair}. Defaults to \code{3}.
#' @param min_support Defaults to \code{2}.
#' @param window Passed to \code{morie_sv_dl_build_sv_graph}.
#' @param spread Passed to \code{morie_sv_dl_insert_size_stats}. Defaults to \code{"mad"}.
#' @return The value of \code{calls}, as built in the body.
#' @export
morie_sv_dl_paired_end_calls <- function(pairs, median=NULL, sd=NULL,
                                         orientation=NULL, n_sd=3.0,
                                         min_support=2, window=NULL,
                                         spread="mad") {
  # Cluster the discordant pairs into paired-end SV calls.
  ps <- lapply(pairs, .sv_dl_pair)
  if (length(ps) == 0L) {
    stop("sv_dl: no read pairs given")
  }
  if (is.null(median) || is.null(sd) || is.null(orientation)) {
    st <- morie_sv_dl_insert_size_stats(ps, orientation, spread)
    if (is.null(median)) median <- st$median
    if (is.null(sd)) sd <- st$sd
    if (is.null(orientation)) orientation <- st$orientation
  }
  orientation <- as.character(orientation)
  if (n_sd < 0) {
    stop("sv_dl: n_sd must be non-negative")
  }
  if (min_support < 1) {
    stop("sv_dl: min_support must be at least 1")
  }
  by_label <- list()
  for (p in ps) {
    lab <- morie_sv_dl_classify_pair(p, median, sd, orientation, n_sd)
    if (!is.null(lab)) {
      key <- paste(lab[1L], lab[2L], sep="\r")
      by_label[[key]] <- c(by_label[[key]], list(p))
    }
  }
  calls <- list()
  for (key in sort(names(by_label))) {
    lab <- strsplit(key, "\r")[[1L]]
    if (length(lab) < 2L) {
      lab <- c(lab, "")
    }
    group <- by_label[[key]]
    g <- morie_sv_dl_build_sv_graph(group, median, sd, lab, orientation,
                                    n_sd, window)
    for (comp in .sv_dl_components(length(g$nodes), g$edges)) {
      members <- morie_sv_dl_maximal_clique(comp, g$edges)
      if (length(members) < min_support) {
        next
      }
      sel <- g$nodes[members]
      start <- max(vapply(sel, function(p) p$pos1 + p$len1, numeric(1)))
      end <- min(vapply(sel, function(p) p$pos2, numeric(1)))
      size <- if (lab[1L] != "TRA") {
        mean(g$sizes[members])
      } else {
        NULL
      }
      calls <- c(calls, list(list(
        type=lab[1L], subtype=lab[2L],
        chrom=sel[[1L]]$chrom1, chrom2=sel[[1L]]$chrom2,
        start=as.integer(start), end=as.integer(end),
        size=if (is.null(size)) NULL else as.numeric(size),
        support=length(members), pairs=sel, precise=FALSE)))
    }
  }
  if (length(calls) > 1L) {
    keys <- vapply(calls, function(c) paste(c$type, c$chrom,
                                            sprintf("%012d", c$start)),
                   character(1))
    calls <- calls[order(keys)]
  }
  calls
}

# ------------------------------------------------------- split reads

#' .sv_dl_revcomp
#'
#' A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_deletion_type_reference}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s Character; passed to \code{toupper}.
#' @return A character value.
#' @export
.sv_dl_revcomp <- function(s) {
  chars <- rev(strsplit(toupper(s), "")[[1L]])
  comp <- ifelse(chars %in% names(.sv_dl_COMPLEMENT),
                 .sv_dl_COMPLEMENT[chars], "N")
  paste(comp, collapse="")
}

#' Rewrite the region so a deletion-type search works (Figure 4): a
#'
#' tandem duplication has its two halves swapped, an inversion has its
#' second half reverse complemented, a translocation gets both.
#'
#' @param ref Coerced to character by the body, with \code{as.character}.
#' @param sv_type One of \code{"DEL"}, \code{"DUP"}, \code{"INV"}.
#' @return A character value.
#' @export
morie_sv_dl_deletion_type_reference <- function(ref, sv_type) {
  # Rewrite the region so a deletion-type search works (Figure 4): a
  # tandem duplication has its two halves swapped, an inversion has
  # its second half reverse complemented, a translocation gets both.
  if (!(sv_type %in% .sv_dl_TYPES)) {
    stop(sprintf("sv_dl: sv_type must be one of %s",
                 paste(.sv_dl_TYPES, collapse=", ")))
  }
  s <- toupper(as.character(ref))
  if (sv_type == "DEL") {
    return(s)
  }
  h <- nchar(s) %/% 2L
  a <- substr(s, 1L, h)
  b <- substr(s, h + 1L, nchar(s))
  if (sv_type == "DUP") {
    return(paste0(b, a))
  }
  if (sv_type == "INV") {
    return(paste0(a, .sv_dl_revcomp(b)))
  }
  paste0(.sv_dl_revcomp(b), a)
}

#' morie_sv_dl_kmer_diagonals
#'
#' A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_refine_breakpoint}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param read Coerced to character by the body, with \code{as.character}.
#' @param ref Coerced to character by the body, with \code{as.character}.
#' @param k Numeric; combined arithmetically in the body. Defaults to \code{7}.
#' @param k_min Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.
#' @param require_half A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return The value of \code{[}.
#' @export
morie_sv_dl_kmer_diagonals <- function(read, ref, k=7, k_min=3,
                                       require_half=TRUE) {
  # Bin the read's k-mer hits by alignment diagonal (Section 2.2).
  # Diagonals are taken in decreasing hit count and each read k-mer is
  # charged to its best diagonal only; diagonals under k_min hits are
  # dropped, and the read is rejected unless two survive holding at
  # least half its k-mers. Returns a matrix (diagonal, hits) sorted by
  # position in the read, or NULL. Diagonals/offsets are 0-based.
  r <- toupper(as.character(read))
  g <- toupper(as.character(ref))
  k <- as.integer(k)
  if (k < 1L) {
    stop("sv_dl: k must be at least 1")
  }
  if (nchar(r) < k) {
    return(NULL)
  }
  index <- list()
  for (pos in seq_len(nchar(g) - k + 1L) - 1L) {
    km <- substr(g, pos + 1L, pos + k)
    if (grepl("N", km, fixed=TRUE)) {
      next
    }
    index[[km]] <- c(index[[km]], pos)
  }
  per_diag <- list()
  total <- 0L
  for (off in seq_len(nchar(r) - k + 1L) - 1L) {
    km <- substr(r, off + 1L, off + k)
    if (grepl("N", km, fixed=TRUE)) {
      next
    }
    total <- total + 1L
    for (pos in index[[km]]) {
      dk <- as.character(pos - off)
      per_diag[[dk]] <- c(per_diag[[dk]], off)
    }
  }
  if (length(per_diag) == 0L) {
    return(NULL)
  }
  diags <- as.integer(names(per_diag))
  counts <- vapply(per_diag, length, integer(1))
  ord <- order(-counts, diags)
  used <- integer(0)
  kept <- matrix(numeric(0), 0L, 3L)  # diag, hits, min offset
  for (t in ord) {
    offs <- setdiff(per_diag[[t]], used)
    if (length(offs) < as.integer(k_min)) {
      next
    }
    used <- c(used, offs)
    kept <- rbind(kept, c(diags[t], length(offs), min(offs)))
  }
  if (nrow(kept) < 2L) {
    return(NULL)
  }
  top2 <- sum(sort(kept[, 2L], decreasing=TRUE)[1:2])
  if (require_half && total > 0L && top2 * 2 < total) {
    return(NULL)
  }
  kept <- kept[order(kept[, 3L]), , drop=FALSE]  # order along the read
  kept[, 1:2, drop=FALSE]
}

#' Gapless majority-vote consensus over the aligned reads. starts
#'
#' places each read in a common frame. Returns list(consensus, start).
#'
#' @param reads Coerced to character by the body, with \code{as.character}.
#' @param starts Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{consensus}, \code{start}.
#' @export
morie_sv_dl_split_read_consensus <- function(reads, starts=NULL) {
  # Gapless majority-vote consensus over the aligned reads. starts
  # places each read in a common frame. Returns list(consensus, start).
  rs <- toupper(as.character(Filter(function(r) nchar(r) > 0L,
                                    as.character(reads))))
  if (length(rs) == 0L) {
    stop("sv_dl: no reads to build a consensus from")
  }
  if (is.null(starts)) {
    st <- rep(0L, length(rs))
  } else {
    st <- as.integer(starts)
    if (length(st) != length(rs)) {
      stop("sv_dl: one start per read is required")
    }
  }
  lo <- min(st)
  hi <- max(st + nchar(rs))
  out <- character(0)
  for (col in seq.int(lo, hi - 1L)) {
    counts <- list()
    for (z in seq_along(rs)) {
      a <- st[z]
      if (a <= col && col < a + nchar(rs[z])) {
        base <- substr(rs[z], col - a + 1L, col - a + 1L)
        counts[[base]] <- (if (is.null(counts[[base]])) 0L
                           else counts[[base]]) + 1L
      }
    }
    if (length(counts) == 0L) {
      break                    # the consensus stays contiguous
    }
    nm <- sort(names(counts))
    cts <- vapply(nm, function(b) counts[[b]], integer(1))
    out <- c(out, nm[which.max(cts)])
  }
  list(consensus=paste(out, collapse=""), start=lo)
}

#' .sv_dl_gotoh
#'
#' A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_gotoh_score_vectors}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param query Character; passed to \code{strsplit}.
#' @param ref Character; passed to \code{strsplit}.
#' @param match Defaults to \code{1}.
#' @param mismatch Defaults to \code{-2}.
#' @param gap_open Numeric; combined arithmetically in the body. Defaults to \code{-4}.
#' @param gap_extend Numeric; combined arithmetically in the body. Defaults to \code{-1}.
#' @return A list with \code{best}, \code{best_at}.
#' @export
.sv_dl_gotoh <- function(query, ref, match=1.0, mismatch=-2.0,
                         gap_open=-4.0, gap_extend=-1.0) {
  # Affine-gap DP; returns, for each query prefix, its best score and
  # where it ended in the reference (0-based). The query is aligned
  # from its start but may end anywhere in the reference.
  q <- strsplit(query, "")[[1L]]
  g <- strsplit(ref, "")[[1L]]
  n <- length(q)
  m <- length(g)
  neg <- -Inf
  Mrow <- rep(neg, m + 1L)
  Irow <- rep(neg, m + 1L)
  Drow <- rep(0.0, m + 1L)  # free leading gap in the query
  Mrow[1L] <- 0.0
  best <- numeric(n)
  best_at <- integer(n)
  for (i in seq_len(n)) {
    nM <- rep(neg, m + 1L)
    nI <- rep(neg, m + 1L)
    nD <- rep(neg, m + 1L)
    nI[1L] <- max(Mrow[1L] + gap_open, Irow[1L] + gap_extend)
    for (j in seq_len(m)) {
      s <- if (q[i] == g[j]) match else mismatch
      prev <- max(Mrow[j], Irow[j], Drow[j])
      nM[j + 1L] <- prev + s
      nI[j + 1L] <- max(Mrow[j + 1L] + gap_open, Irow[j + 1L] + gap_extend)
      nD[j + 1L] <- max(nM[j] + gap_open, nD[j] + gap_extend)
    }
    Mrow <- nM
    Irow <- nI
    Drow <- nD
    col <- pmax(Mrow, Irow, Drow)
    bj <- which.max(col)
    best[i] <- col[bj]
    best_at[i] <- bj - 1L
  }
  list(best=best, best_at=best_at)
}

#' morie_sv_dl_gotoh_score_vectors
#'
#' A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_refine_breakpoint}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param consensus Coerced to character by the body, with \code{as.character}.
#' @param ref Coerced to character by the body, with \code{as.character}.
#' @param match Passed to \code{.sv_dl_gotoh}. Defaults to \code{1}.
#' @param mismatch Passed to \code{.sv_dl_gotoh}. Defaults to \code{-2}.
#' @param gap_open Passed to \code{.sv_dl_gotoh}. Defaults to \code{-4}.
#' @param gap_extend Passed to \code{.sv_dl_gotoh}. Defaults to \code{-1}.
#' @return A list with \code{f}, \code{f_at}, \code{r}, \code{r_at}.
#' @export
morie_sv_dl_gotoh_score_vectors <- function(consensus, ref, match=1.0,
                                            mismatch=-2.0, gap_open=-4.0,
                                            gap_extend=-1.0) {
  # The paper's f and r (Section 2.2): f_i the best score for the
  # prefix c_1..c_i, r_j the best for the suffix c_n..c_j. Returns
  # list(f, f_at, r, r_at) with 0-based reference positions.
  c_ <- toupper(as.character(consensus))
  g <- toupper(as.character(ref))
  if (nchar(c_) == 0L || nchar(g) == 0L) {
    stop("sv_dl: consensus and reference must be non-empty")
  }
  fw <- .sv_dl_gotoh(c_, g, match, mismatch, gap_open, gap_extend)
  revstr <- function(s) paste(rev(strsplit(s, "")[[1L]]), collapse="")
  bw <- .sv_dl_gotoh(revstr(c_), revstr(g), match, mismatch, gap_open,
                     gap_extend)
  n <- nchar(c_)
  m <- nchar(g)
  r <- numeric(n)
  r_at <- integer(n)
  for (t in seq_len(n) - 1L) {
    # bw$best[t+1] is the suffix of length t+1, i.e. it starts at c_{n-t}
    r[n - t] <- bw$best[t + 1L]
    r_at[n - t] <- m - bw$best_at[t + 1L]
  }
  list(f=fw$best, f_at=fw$best_at, r=r, r_at=r_at)
}

#' Argmax_{i<j} f_i + r_j -- the split with a microinsertion gap
#'
#' Indices are 1-based over the consensus, as in the paper.
#'
#' @param f A vector; its length is taken and its elements indexed.
#' @param r A vector; its length is taken and its elements indexed.
#' @return A list with \code{i}, \code{j}, \code{score}.
#' @export
morie_sv_dl_optimal_split <- function(f, r) {
  # argmax_{i<j} f_i + r_j -- the split with a microinsertion gap.
  # Indices are 1-based over the consensus, as in the paper.
  n <- length(f)
  if (n != length(r)) {
    stop("sv_dl: f and r must have the same length")
  }
  if (n < 2L) {
    stop("sv_dl: the consensus is too short to split")
  }
  best <- NULL
  for (i in seq_len(n - 1L)) {
    for (j in seq.int(i + 1L, n)) {
      v <- f[i] + r[j]
      if (is.null(best) || v > best[1L]) {
        best <- c(v, i, j)
      }
    }
  }
  list(i=as.integer(best[2L]), j=as.integer(best[3L]), score=best[1L])
}

#' morie_sv_dl_refine_breakpoint
#'
#' A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_structural_variant}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param call A list; the body reads \code{$size}, \code{$type} from it.
#' @param reference Passed to \code{morie_sv_dl_deletion_type_reference}.
#' @param reads A vector; its length is taken and its elements indexed.
#' @param k Passed to \code{morie_sv_dl_kmer_diagonals}. Defaults to \code{7}.
#' @param k_min Passed to \code{morie_sv_dl_kmer_diagonals}. Defaults to \code{3}.
#' @param min_split_support Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2}.
#' @param max_length_diff Numeric; combined arithmetically in the body. Defaults to \code{0.1}.
#' @param match Passed to \code{morie_sv_dl_gotoh_score_vectors}. Defaults to \code{1}.
#' @param mismatch Passed to \code{morie_sv_dl_gotoh_score_vectors}. Defaults to \code{-2}.
#' @param gap_open Passed to \code{morie_sv_dl_gotoh_score_vectors}. Defaults to \code{-4}.
#' @param gap_extend Passed to \code{morie_sv_dl_gotoh_score_vectors}. Defaults to \code{-1}.
#' @return A list with \code{start}, \code{end}, \code{size}, \code{split_support}, \code{consensus}, \code{score}, \code{microinsertion}, \code{microhomology}, \code{kmer_offset}.
#' @export
morie_sv_dl_refine_breakpoint <- function(call, reference, reads, k=7,
                                          k_min=3, min_split_support=2,
                                          max_length_diff=0.10, match=1.0,
                                          mismatch=-2.0, gap_open=-4.0,
                                          gap_extend=-1.0) {
  # Take one paired-end call to single-nucleotide resolution. Returns
  # NULL if the read support or the length check fails -- the call then
  # stays imprecise rather than being invented.
  region <- morie_sv_dl_deletion_type_reference(reference, call$type)
  offsets <- list()
  per_read <- list()
  first_diag <- list()
  for (idx in seq_along(reads)) {
    diags <- morie_sv_dl_kmer_diagonals(reads[[idx]], region, k, k_min)
    if (is.null(diags)) {
      next
    }
    first_diag[[as.character(idx)]] <- diags[1L, 1L]
    # the gap between consecutive diagonals is the size this read implies
    for (a in seq_len(nrow(diags) - 1L)) {
      off <- diags[a + 1L, 1L] - diags[a, 1L]
      if (off == 0) {
        next
      }
      ok <- as.character(off)
      offsets[[ok]] <- (if (is.null(offsets[[ok]])) 0L
                        else offsets[[ok]]) + 1L
      per_read[[ok]] <- c(per_read[[ok]], idx)
    }
  }
  if (length(offsets) == 0L) {
    return(NULL)
  }
  onames <- as.numeric(names(offsets))
  ocnt <- vapply(names(offsets), function(o) offsets[[o]], integer(1))
  ord <- order(onames)
  best_off <- onames[ord][which.max(ocnt[ord])]
  support <- per_read[[as.character(best_off)]]
  if (length(support) < as.integer(min_split_support)) {
    return(NULL)
  }
  starts <- vapply(support, function(i) first_diag[[as.character(i)]],
                   numeric(1))
  cons <- morie_sv_dl_split_read_consensus(reads[support], starts)
  consensus <- cons$consensus
  gv <- morie_sv_dl_gotoh_score_vectors(consensus, region, match, mismatch,
                                        gap_open, gap_extend)
  sp <- morie_sv_dl_optimal_split(gv$f, gv$r)
  i <- sp$i
  j <- sp$j
  left_ref <- gv$f_at[i]
  right_ref <- gv$r_at[j]
  size <- right_ref - left_ref
  if (!is.null(call$size) && call$size > 0) {
    if (abs(size - call$size) > max_length_diff * abs(call$size)) {
      return(NULL)
    }
  }
  # Microhomology: bases shared by the two breakpoint flanks. The
  # alignment places the junction as far left as it will go, so the
  # room left over runs to the right.
  hom <- 0L
  while (left_ref + hom < nchar(region) && right_ref + hom < nchar(region) &&
         substr(region, left_ref + hom + 1L, left_ref + hom + 1L) ==
         substr(region, right_ref + hom + 1L, right_ref + hom + 1L)) {
    hom <- hom + 1L
  }
  micro <- if (j - 1L >= i + 1L) substr(consensus, i + 1L, j - 1L) else ""
  list(start=as.integer(left_ref), end=as.integer(right_ref),
       size=as.integer(size), split_support=length(support),
       consensus=consensus, score=as.numeric(sp$score),
       microinsertion=micro, microhomology=as.integer(hom),
       kmer_offset=as.integer(best_off))
}

# ------------------------------------------------------------- driver

#' morie_sv_dl_structural_variant
#'
#' A step of the sv_dl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param pairs Passed to \code{morie_sv_dl_paired_end_calls}.
#' @param reference Optional; may be \code{NULL}. A vector; its length is taken.
#' @param split_reads Optional; may be \code{NULL}. A vector; its length is taken.
#' @param orientation Passed to \code{morie_sv_dl_paired_end_calls}.
#' @param median Passed to \code{morie_sv_dl_paired_end_calls}.
#' @param sd Passed to \code{morie_sv_dl_paired_end_calls}.
#' @param n_sd Passed to \code{morie_sv_dl_paired_end_calls}. Defaults to \code{3}.
#' @param min_support Passed to \code{morie_sv_dl_paired_end_calls}. Defaults to \code{2}.
#' @param k Passed to \code{morie_sv_dl_refine_breakpoint}. Defaults to \code{7}.
#' @param k_min Passed to \code{morie_sv_dl_refine_breakpoint}. Defaults to \code{3}.
#' @param min_split_support Passed to \code{morie_sv_dl_refine_breakpoint}. Defaults to \code{2}.
#' @param max_length_diff Passed to \code{morie_sv_dl_refine_breakpoint}. Defaults to \code{0.1}.
#' @param window Passed to \code{morie_sv_dl_paired_end_calls}.
#' @param spread Passed to \code{morie_sv_dl_paired_end_calls}. Defaults to \code{"mad"}.
#' @return A list with \code{estimate}, \code{calls}, \code{n_calls}, \code{n_precise}, \code{insert_median}, \code{insert_sd}, \code{spread}, \code{orientation}, \code{n_sd}, \code{min_support}, \code{method}, \code{note}.
#' @export
morie_sv_dl_structural_variant <- function(pairs, reference=NULL,
                                           split_reads=NULL,
                                           orientation=NULL, median=NULL,
                                           sd=NULL, n_sd=3.0, min_support=2,
                                           k=7, k_min=3,
                                           min_split_support=2,
                                           max_length_diff=0.10,
                                           window=NULL, spread="mad") {
  # Call structural variants from read pairs, refined by split reads.
  # Give reference and split_reads to get single-nucleotide
  # breakpoints; without them the calls come back at paired-end
  # resolution with precise=FALSE.
  calls <- morie_sv_dl_paired_end_calls(pairs, median, sd, orientation,
                                        n_sd, min_support, window, spread)
  st <- morie_sv_dl_insert_size_stats(pairs, orientation, spread)
  refined <- 0L
  if (!is.null(reference) && !is.null(split_reads) &&
      length(split_reads) > 0L) {
    for (ci in seq_along(calls)) {
      call <- calls[[ci]]
      lo <- max(0L, call$start - as.integer(st$median))
      hi <- min(nchar(reference), call$end + as.integer(st$median))
      region <- substr(as.character(reference), lo + 1L, hi)
      if (nchar(region) == 0L) {
        next
      }
      got <- morie_sv_dl_refine_breakpoint(call, region, split_reads, k,
                                           k_min, min_split_support,
                                           max_length_diff)
      if (is.null(got)) {
        next
      }
      call$start <- got$start + lo
      call$end <- got$end + lo
      call$size <- as.numeric(got$size)
      call$split_support <- got$split_support
      call$consensus <- got$consensus
      call$microinsertion <- got$microinsertion
      call$microhomology <- got$microhomology
      call$precise <- TRUE
      calls[[ci]] <- call
      refined <- refined + 1L
    }
  }
  list(
    estimate=calls, calls=calls, n_calls=length(calls),
    n_precise=refined, insert_median=st$median, insert_sd=st$sd,
    spread=spread, orientation=st$orientation, n_sd=as.numeric(n_sd),
    min_support=as.integer(min_support),
    method=paste0("DELLY (Rausch et al. 2012): discordant paired-end ",
                  "clustering by maximal clique, refined by k-mer ",
                  "split-read search and a double-dynamic-programming ",
                  "split alignment"),
    note=paste0("calls are imprecise (paired-end resolution) unless a ",
                "reference and split reads are supplied and the ",
                "split-read length agrees with the paired-end estimate ",
                "to within max_length_diff"))
}

#' morie_sv_dl_cheatsheet
#'
#' A step of the sv_dl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_sv_dl_cheatsheet <- function() {
  paste0(
    "sv_dl: DELLY (Rausch et al. 2012). Discordant pairs are ",
    "typed by orientation and insert size (DEL, DUP, INV ",
    "left/right, four TRA classes), made into a weighted graph ",
    "where the weight is the disagreement in implied SV size, ",
    "and each component yields a maximal clique grown from its ",
    "lowest-weight edge. Split reads are then found by k-mer ",
    "diagonal counting, a majority-vote consensus is built, and ",
    "forward and reverse Gotoh score vectors are split at ",
    "argmax_{i<j} f_i + r_j to give the breakpoint to a base."
  )
}

# compact alias per ledger/NAMING.md
morie_sv_dl_sv_delly <- morie_sv_dl_structural_variant

#' @export
morie_sv_dl <- morie_sv_dl_structural_variant
