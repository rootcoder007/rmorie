# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of longrd -- long-read consensus polishing. Mirrors
# src/morie/fn/longrd.py operation for operation, on the shared numerics
# in R/aaa_helpers_w3num.R.
#
# A long-read assembly is right about the big things and wrong about the
# small ones. The reads are long enough to span repeats, so the layout
# is correct; but each read carries several percent error, concentrated
# in homopolymers -- a nanopore signal cannot count how many identical
# bases went through the pore, so a run of six adenines reads back as
# five or seven. Those errors survive into the draft, and a draft with a
# spurious insertion in a homopolymer produces a frameshift in every
# gene that crosses it.
#
# Polishing fixes it by voting. Align the reads back to the draft, and
# at each position let them outvote the error, which is random, while
# the truth, which is not, accumulates.
#
#   THE ALIGNMENT. Needleman and Wunsch's global alignment, written out
#   with an explicit traceback and a fixed tie-break -- diagonal, then
#   up, then left -- so the alignment is a function of the two sequences
#   and the scoring and of nothing else.
#
#   THE PILEUP. What each read says at each draft position, including
#   what it says between positions. Insertions are counted separately
#   because an insertion is not a substitution, and collapsing them
#   would lose exactly the errors that matter most here.
#
#   THE CONSENSUS. Two routes, both published. "pileup" is the
#   column-wise majority, given an insertion channel from the insertion
#   counts. "poa" is partial-order alignment built progressively, which
#   is what Racon does; it is ORDER DEPENDENT, a real property of
#   progressive construction and not a bug, and the reads are sorted by
#   default so the answer is at least a function of the SET.
#
#   THE RUN-LENGTH VIEW. Homopolymer length is where the errors are, so
#   the run-length encoding of draft and polished sequence is reported.
#   A polish that shortened a run from seven to six is visible there and
#   invisible in a base-by-base diff.
#
# A position with too little coverage is LEFT ALONE rather than called
# from two reads. The depth floor is a parameter and the count of
# positions it protected is reported, because a polisher that silently
# rewrites thin regions is worse than one that does not polish them.
#
# References
#   Vaser, R., Sovic, I., Nagarajan, N. and Sikic, M. (2017) "Fast and
#     accurate de novo genome assembly from long uncorrected reads."
#     Genome Research 27(5), 737-746. doi:10.1101/gr.214270.116.
#   Lee, C., Grasso, C. and Sharlow, M.F. (2002) "Multiple sequence
#     alignment using partial order graphs." Bioinformatics 18(3),
#     452-464. doi:10.1093/bioinformatics/18.3.452.
#   Needleman, S.B. and Wunsch, C.D. (1970) "A general method applicable
#     to the search for similarities in the amino acid sequence of two
#     proteins." Journal of Molecular Biology 48(3), 443-453.
#   Wick, R.R., Judd, L.M. and Holt, K.E. (2019) "Performance of neural
#     network basecalling tools for Oxford Nanopore sequencing." Genome
#     Biology 20, 129. doi:10.1186/s13059-019-1727-y.

.longrd_bases <- c("A", "C", "G", "T")
.longrd_methods <- c("pileup", "poa")

#' Needleman-Wunsch global alignment, with the traceback written out
#'
#' Returns the score and the two gapped strings. Ties are broken in a
#' fixed order -- diagonal, then up, then left -- so the alignment is a
#' function of its arguments. Any other order gives an alignment of the
#' same score and a different pileup, which is why the order is stated
#' rather than left to whichever branch ran first.
#'
#' @param a,b The two sequences.
#' @param match,mismatch,gap The scoring.
#' @return A list with the score and the two gapped sequences.
#' @export
morie_longrd_align <- function(a, b, match = 1, mismatch = -1,
                               gap = -2) {
  av <- if (nchar(a)) strsplit(a, "")[[1]] else character(0)
  bv <- if (nchar(b)) strsplit(b, "")[[1]] else character(0)
  n <- length(av); m <- length(bv)
  s <- matrix(0, n + 1L, m + 1L)
  if (n > 0L) for (i in seq_len(n)) s[i + 1L, 1] <- s[i, 1] + gap
  if (m > 0L) for (j in seq_len(m)) s[1, j + 1L] <- s[1, j] + gap
  if (n > 0L && m > 0L) for (i in seq_len(n)) for (j in seq_len(m)) {
    d <- s[i, j] + (if (av[i] == bv[j]) match else mismatch)
    u <- s[i, j + 1L] + gap
    l <- s[i + 1L, j] + gap
    best <- d
    if (u > best) best <- u
    if (l > best) best <- l
    s[i + 1L, j + 1L] <- best
  }
  ga <- character(0); gb <- character(0)
  i <- n; j <- m
  while (i > 0L || j > 0L) {
    if (i > 0L && j > 0L &&
        s[i + 1L, j + 1L] == s[i, j] +
          (if (av[i] == bv[j]) match else mismatch)) {
      ga <- c(av[i], ga); gb <- c(bv[j], gb); i <- i - 1L; j <- j - 1L
    } else if (i > 0L && s[i + 1L, j + 1L] == s[i, j + 1L] + gap) {
      ga <- c(av[i], ga); gb <- c("-", gb); i <- i - 1L
    } else {
      ga <- c("-", ga); gb <- c(bv[j], gb); j <- j - 1L
    }
  }
  list(score = s[n + 1L, m + 1L],
       a = paste(ga, collapse = ""), b = paste(gb, collapse = ""))
}

#' Run-length encoding: the bases and how many of each in a row
#'
#' Homopolymer length is where nanopore error lives, so this is the view
#' in which a polish either fixed something or did not.
#'
#' @param seq A sequence.
#' @return A list of base and count pairs.
#' @export
morie_longrd_rle <- function(seq) {
  if (!nchar(seq)) return(list())
  v <- strsplit(seq, "")[[1]]
  out <- list()
  for (ch in v) {
    k <- length(out)
    if (k > 0L && out[[k]][[1]] == ch) {
      out[[k]][[2]] <- out[[k]][[2]] + 1L
    } else {
      out[[k + 1L]] <- list(ch, 1L)
    }
  }
  out
}

#' Expand a run-length encoding back to the sequence
#'
#' @param runs A list of base and count pairs.
#' @return A character scalar.
#' @export
morie_longrd_unrle <- function(runs) {
  if (!length(runs)) return("")
  paste(vapply(runs, function(r)
    paste(rep(r[[1]], as.integer(r[[2]])), collapse = ""),
    character(1)), collapse = "")
}

#' What every read says at every draft position, and between them
#'
#' Returns, per draft position, a count of each base and of deletions,
#' and separately a count of the sequences each read inserts AFTER that
#' position. The insertions are kept apart because an insertion is not a
#' substitution: merging them would throw away the homopolymer errors
#' this whole exercise is about.
#'
#' @param draft The draft sequence.
#' @param reads The reads.
#' @param match,mismatch,gap The scoring.
#' @return A list with the columns and the insertion counts.
#' @export
morie_longrd_pileup <- function(draft, reads, match = 1, mismatch = -1,
                                gap = -2) {
  n <- nchar(draft)
  cols <- vector("list", n)
  for (i in seq_len(n))
    cols[[i]] <- c(A = 0L, C = 0L, G = 0L, T = 0L, `-` = 0L)
  ins <- vector("list", n + 1L)
  for (i in seq_len(n + 1L)) ins[[i]] <- list()
  for (read in reads) {
    al <- morie_longrd_align(draft, read, match, mismatch, gap)
    gd <- strsplit(al$a, "")[[1]]
    gr <- strsplit(al$b, "")[[1]]
    pos <- 0L; pend <- ""
    for (k in seq_along(gd)) {
      if (gd[k] == "-") { pend <- paste0(pend, gr[k]); next }
      if (nzchar(pend)) {
        slot <- ins[[pos + 1L]]
        slot[[pend]] <- if (is.null(slot[[pend]])) 1L else
          slot[[pend]] + 1L
        ins[[pos + 1L]] <- slot
        pend <- ""
      }
      cc <- gr[k]
      if (!(cc %in% names(cols[[pos + 1L]]))) {
        cols[[pos + 1L]][["-"]] <- cols[[pos + 1L]][["-"]] + 1L
      } else {
        cols[[pos + 1L]][[cc]] <- cols[[pos + 1L]][[cc]] + 1L
      }
      pos <- pos + 1L
    }
    if (nzchar(pend)) {
      slot <- ins[[n + 1L]]
      slot[[pend]] <- if (is.null(slot[[pend]])) 1L else
        slot[[pend]] + 1L
      ins[[n + 1L]] <- slot
    }
  }
  list(cols = cols, ins = ins)
}

#' Ties go to the draft base if it is among the leaders, and otherwise
#'
#' to the first base in alphabetical order -- an arbitrary rule, but a
#' STATED arbitrary rule, which is what makes the two arms agree.
#'
#' @param col A list; the body reads \code{$-}, \code{$A}, \code{$C}, \code{$G}, \code{$T} from it.
#' @param draft_base Carried through into a list the body builds.
#' @param min_depth Passed to \code{<}.
#' @param min_frac Passed to \code{<}.
#' @return The value of \code{list}.
#' @export
.longrd_call <- function(col, draft_base, min_depth, min_frac) {
  # Ties go to the draft base if it is among the leaders, and otherwise
  # to the first base in alphabetical order -- an arbitrary rule, but a
  # STATED arbitrary rule, which is what makes the two arms agree.
  depth <- col[["A"]] + col[["C"]] + col[["G"]] + col[["T"]] +
    col[["-"]]
  if (depth < min_depth)
    return(list(draft_base, depth, 0, TRUE))
  best <- NULL
  for (b in c(.longrd_bases, "-"))
    if (is.null(best) || col[[b]] > col[[best]]) best <- b
  top <- col[[best]]
  if (draft_base %in% names(col) && col[[draft_base]] == top)
    best <- draft_base
  frac <- if (depth > 0L) top / depth else 0
  if (frac < min_frac) return(list(draft_base, depth, frac, TRUE))
  list(if (best == "-") "" else best, depth, frac, FALSE)
}

#' Progressive partial-order consensus: the heaviest path
#'
#' The first read is the seed. Each later read is aligned to the current
#' consensus and every aligned column votes; the consensus is recomputed
#' as the column-wise majority, which for a partial order graph built
#' this way IS the heaviest path, because every node's weight is the
#' number of reads passing through it.
#'
#' Progressive construction depends on the order the reads arrive. That
#' is a property of the method and not an accident, so the reads are
#' sorted first by default -- which does not make the answer
#' order-INDEPENDENT, it makes it a function of the SET rather than of
#' the sequence, and those are different guarantees.
#'
#' @param reads The reads.
#' @param match,mismatch,gap The scoring.
#' @param sort_reads Whether to sort first.
#' @return The consensus sequence.
#' @export
morie_longrd_poa <- function(reads, match = 1, mismatch = -1, gap = -2,
                             sort_reads = TRUE) {
  rs <- as.character(reads)
  if (!length(rs)) stop("a consensus needs at least one read")
  if (isTRUE(sort_reads)) rs <- sort(rs, method = "radix")
  cons <- rs[1]
  if (length(rs) > 1L) for (k in 2:length(rs)) {
    al <- morie_longrd_align(cons, rs[k], match, mismatch, gap)
    gc <- strsplit(al$a, "")[[1]]
    gr <- strsplit(al$b, "")[[1]]
    out <- character(length(gc))
    for (q in seq_along(gc)) {
      out[q] <- if (gc[q] == "-") gr[q] else gc[q]
    }
    cons <- paste(out, collapse = "")
  }
  cons
}

#' Polish a draft assembly with the reads it was built from
#'
#' @param assembly The draft.
#' @param reads The reads, already known to belong to this contig.
#' @param method Either pileup or poa.
#' @param min_depth Below this many reads a position is left alone. The
#'   count of positions this protected is reported.
#' @param min_frac A call needs this share of the column, or the draft
#'   stands.
#' @param ins_frac An insertion needs this share of the depth to be
#'   accepted.
#' @param match,mismatch,gap The alignment scoring.
#' @param sort_reads Whether the partial-order route sorts first.
#' @return A list with the polished sequence, the per-position depth and
#'   support, the run-length view of both sequences, and how much
#'   changed.
#' @export
morie_longrd <- function(assembly, reads, method = "pileup",
                         min_depth = 3L, min_frac = 0.5,
                         ins_frac = 0.5, match = 1, mismatch = -1,
                         gap = -2, sort_reads = TRUE) {
  if (!(method %in% .longrd_methods))
    stop("the method is pileup or poa")
  draft <- as.character(assembly)
  rs <- as.character(reads)
  if (!nchar(draft)) stop("an empty draft has nothing to polish")
  if (!length(rs)) stop("polishing needs reads")

  pu <- morie_longrd_pileup(draft, rs, match, mismatch, gap)
  cols <- pu$cols; ins <- pu$ins
  dv <- strsplit(draft, "")[[1]]
  n <- length(dv)
  depth <- integer(n); support <- numeric(n); called <- character(n)
  protected <- 0L
  for (p in seq_len(n)) {
    r <- .longrd_call(cols[[p]], dv[p], min_depth, min_frac)
    called[p] <- r[[1]]; depth[p] <- r[[2]]; support[p] <- r[[3]]
    if (isTRUE(r[[4]])) protected <- protected + 1L
  }

  if (method == "pileup") {
    out <- character(0)
    for (p in 0:n) {
      slot <- ins[[p + 1L]]
      if (length(slot)) {
        bestk <- NULL
        for (k in sort(names(slot), method = "radix"))
          if (is.null(bestk) || slot[[k]] > slot[[bestk]]) bestk <- k
        d <- if (p < n) depth[p + 1L] else
          if (n > 0L) depth[n] else 0L
        if (d > 0L && slot[[bestk]] / d >= ins_frac)
          out <- c(out, bestk)
      }
      if (p < n) out <- c(out, called[p + 1L])
    }
    polished <- paste(out, collapse = "")
  } else {
    polished <- morie_longrd_poa(rs, match, mismatch, gap, sort_reads)
  }

  pv <- if (nchar(polished)) strsplit(polished, "")[[1]] else
    character(0)
  changed <- 0L
  lim <- min(n, length(pv))
  if (lim > 0L) for (p in seq_len(lim))
    if (dv[p] != pv[p]) changed <- changed + 1L
  changed <- changed + abs(n - length(pv))
  list(polished = polished, draft = draft, called = called,
       depth = depth, support = support, n_protected = protected,
       n_changed = changed, identical = identical(polished, draft),
       draft_rle = morie_longrd_rle(draft),
       polished_rle = morie_longrd_rle(polished),
       n_draft_runs = length(morie_longrd_rle(draft)),
       n_polished_runs = length(morie_longrd_rle(polished)),
       mean_depth = if (n > 0L) .w3_csum(as.numeric(depth)) / n else 0,
       n_reads = length(rs), length = length(pv), draft_length = n,
       method = method, min_depth = as.integer(min_depth),
       min_frac = as.numeric(min_frac),
       ins_frac = as.numeric(ins_frac))
}

#' One-line summary of the longrd module
#'
#' @return A character scalar.
#' @export
morie_longrd_cheatsheet <- function()
  paste0("longrd: long-read consensus polishing. Needleman-Wunsch ",
         "pileup with a column majority, or a progressive ",
         "partial-order consensus; homopolymers reported run-length")
