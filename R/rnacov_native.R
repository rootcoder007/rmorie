# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of rnacov -- RNA covariance models. Mirrors
# src/morie/fn/rnacov.py operation for operation, on the shared numerics
# in R/aaa_helpers_w3num.R.
#
# RNA structure is held together by base pairs, and base pairs leave a
# signature in an alignment that sequence conservation does not. If
# position 12 pairs with position 40, then a mutation at 12 that would
# break the pair is only tolerated when position 40 mutates to restore
# it -- G:C becomes A:U, never G:U-and-stay. So the two columns VARY
# TOGETHER while each varies freely on its own.
#
# That is the whole idea, and the thing it corrects is the intuition
# that conserved means important. A column pair that is G:C in every
# single sequence tells you nothing about whether the two positions
# interact: they might be conserved for entirely separate reasons.
# Covariation is evidence of pairing; conservation is not. Mutual
# information says exactly this and says it in bits, and it is zero when
# the columns are independent AND zero when either is constant -- the
# second being the case the eye gets wrong.
#
# Raw mutual information has a known bias: with few sequences, two
# columns look dependent by chance, and the bias grows with the number
# of letters actually seen. The Miller-Madow correction subtracts the
# leading term of that bias, and it is offered as a route rather than
# applied silently, because a corrected and an uncorrected score are
# different numbers and the reader should know which they have.
#
# Two structure routes: score the base pairs of a consensus structure in
# dot-bracket notation, or fold by maximum base pairing first. The
# second knows nothing about covariation, so comparing the two says
# whether the proposed structure is doing better than pairing counts
# alone.
#
# Gaps are their own problem. A column pair whose sequences are mostly
# gapped has almost no data behind it, and the effective number of
# ungapped sequences per pair is reported so a high score on four
# sequences cannot be mistaken for a high score on four hundred.
#
# References
#   Eddy, S.R. and Durbin, R. (1994) "RNA sequence analysis using
#     covariance models." Nucleic Acids Research 22(11), 2079-2088.
#     doi:10.1093/nar/22.11.2079.
#   Nawrocki, E.P. and Eddy, S.R. (2013) "Infernal 1.1: 100-fold faster
#     RNA homology searches." Bioinformatics 29(22), 2933-2935.
#   Rivas, E., Clements, J. and Eddy, S.R. (2017) "A statistical test
#     for conserved RNA structure shows lack of evidence for structure
#     in lncRNAs." Nature Methods 14(1), 45-48.
#   Nussinov, R. and Jacobson, A.B. (1980) "Fast algorithm for
#     predicting the secondary structure of single-stranded RNA." PNAS
#     77(11), 6309-6313.
#   Miller, G.A. (1955) "Note on the bias of information estimates." In
#     Information Theory in Psychology, 95-100.

.RNACOV_ALPHABET <- c("A", "C", "G", "U")
# Watson-Crick plus the wobble, which is a real pair and leaving it out
# would score two thirds of a stem as unpaired.
.RNACOV_PAIRS <- list(c("A", "U"), c("U", "A"), c("C", "G"),
                      c("G", "C"), c("G", "U"), c("U", "G"))
.RNACOV_STRUCTURES <- c("given", "nussinov")
.RNACOV_GAPS <- c("-", ".", "~")

#' Joint and marginal letter counts for two columns, gaps dropped
#'
#' A sequence gapped in either column contributes to neither margin.
#' Counting it in one and not the other would make the marginals and the
#' joint disagree about how many sequences there were, and the mutual
#' information would stop being a mutual information.
#'
#' @param alignment A character vector of aligned sequences.
#' @param i First column, one-based.
#' @param j Second column, one-based.
#' @return A list with the joint counts, both margins and the count.
#' @export
morie_rnacov_counts <- function(alignment, i, j) {
  joint <- matrix(0L, 4L, 4L)
  n <- 0L
  for (s in alignment) {
    a <- substr(s, i, i); b <- substr(s, j, j)
    if (a %in% .RNACOV_GAPS || b %in% .RNACOV_GAPS) next
    ia <- match(a, .RNACOV_ALPHABET); ib <- match(b, .RNACOV_ALPHABET)
    if (is.na(ia) || is.na(ib)) next
    joint[ia, ib] <- joint[ia, ib] + 1L
    n <- n + 1L
  }
  list(joint = joint, mi = rowSums(joint), mj = colSums(joint), n = n)
}

#' Mutual information between two alignment columns, in bits
#'
#' Zero for independent columns and zero when either column is constant.
#' The second is the point: a perfectly conserved pair is perfectly
#' uninformative about whether the positions interact.
#'
#' @param alignment A character vector of aligned sequences.
#' @param i First column, one-based.
#' @param j Second column, one-based.
#' @param correction "none" or "miller_madow".
#' @return A list with the information, the support and the cells seen.
#' @export
morie_rnacov_mi <- function(alignment, i, j, correction = "none") {
  cc <- morie_rnacov_counts(alignment, i, j)
  n <- cc$n
  if (n == 0L) return(list(mi = 0, n = 0L, seen = 0L))
  terms <- numeric(0)
  seen <- 0L
  for (a in 1:4) for (b in 1:4) {
    c0 <- cc$joint[a, b]
    if (c0 == 0L) next
    seen <- seen + 1L
    pab <- c0 / n
    pa <- cc$mi[a] / n
    pb <- cc$mj[b] / n
    terms <- c(terms, pab * log(pab / (pa * pb)) / log(2))
  }
  v <- if (length(terms)) .w3_csum(terms) else 0
  if (correction == "miller_madow") {
    # The leading bias term: (cells seen - rows seen - cols seen + 1)
    # over 2 n ln 2. It is subtracted, so a small sample's spurious
    # dependence is charged for rather than reported as signal.
    rows <- sum(cc$mi > 0L); cols <- sum(cc$mj > 0L)
    v <- v - (seen - rows - cols + 1) / (2 * n * log(2))
  } else if (correction != "none") {
    stop("correction must be none or miller_madow")
  }
  list(mi = v, n = n, seen = seen)
}

#' Dot-bracket to a list of base pairs
#'
#' An unbalanced string is an error, not a structure. Silently dropping
#' an unmatched bracket would give a plausible-looking pair list for a
#' string that never described a structure at all.
#'
#' @param s A dot-bracket string.
#' @return A two-column matrix of zero-based pair positions.
#' @export
morie_rnacov_parse <- function(s) {
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  stack <- integer(0)
  pi <- integer(0); pj <- integer(0)
  for (k in seq_along(chars)) {
    ch <- chars[k]
    if (ch %in% c("(", "<", "[", "{")) {
      stack <- c(stack, k - 1L)
    } else if (ch %in% c(")", ">", "]", "}")) {
      if (!length(stack))
        stop("closing bracket at ", k - 1L, " has nothing to close")
      pi <- c(pi, stack[length(stack)]); pj <- c(pj, k - 1L)
      stack <- stack[-length(stack)]
    } else if (!(ch %in% c(".", ":", "_", "-", ","))) {
      stop("character ", ch, " at ", k - 1L, " is not dot-bracket")
    }
  }
  if (length(stack))
    stop(length(stack), " bracket(s) never closed, first at ", stack[1])
  if (!length(pi)) return(matrix(integer(0), 0L, 2L))
  ord <- order(pi, pj)
  cbind(pi[ord], pj[ord])
}

#' .rnacov_can_pair
#'
#' A step of the rnacov_native implementation. Called by \code{morie_rnacov_nussinov}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return A logical value.
#' @export
.rnacov_can_pair <- function(a, b) {
  for (p in .RNACOV_PAIRS) if (a == p[1] && b == p[2]) return(TRUE)
  FALSE
}

#' Maximum base pairing by the Nussinov dynamic program
#'
#' Fills the upper triangle with the best number of pairs on each
#' subsequence, then traces back. The minimum loop length is a physical
#' constraint, not a tuning knob: a hairpin cannot close on fewer than
#' about three unpaired bases.
#'
#' @param seq A sequence string.
#' @param min_loop The minimum hairpin loop.
#' @return A list with the zero-based pair matrix and the pair count.
#' @export
morie_rnacov_nussinov <- function(seq, min_loop = 3L) {
  ch <- strsplit(seq, "", fixed = TRUE)[[1]]
  n <- length(ch)
  m <- matrix(0L, max(n, 1L), max(n, 1L))
  if (n > min_loop + 1L) for (span in (min_loop + 1L):(n - 1L))
    for (i in seq_len(n - span)) {
      j <- i + span
      best <- m[i, j - 1L]
      if (.rnacov_can_pair(ch[i], ch[j])) {
        inner <- if (i + 1L <= j - 1L) m[i + 1L, j - 1L] else 0L
        if (inner + 1L > best) best <- inner + 1L
      }
      for (k in i:(j - 1L)) {
        v <- m[i, k] + m[k + 1L, j]
        if (v > best) best <- v
      }
      m[i, j] <- best
    }
  pi <- integer(0); pj <- integer(0)
  stack <- list(c(1L, n))
  while (length(stack)) {
    cur <- stack[[length(stack)]]; stack <- stack[-length(stack)]
    i <- cur[1]; j <- cur[2]
    if (j - i <= min_loop) next
    if (m[i, j] == m[i, j - 1L]) {
      stack[[length(stack) + 1L]] <- c(i, j - 1L)
      next
    }
    done <- FALSE
    if (.rnacov_can_pair(ch[i], ch[j])) {
      inner <- if (i + 1L <= j - 1L) m[i + 1L, j - 1L] else 0L
      if (m[i, j] == inner + 1L) {
        pi <- c(pi, i - 1L); pj <- c(pj, j - 1L)
        stack[[length(stack) + 1L]] <- c(i + 1L, j - 1L)
        done <- TRUE
      }
    }
    if (!done) for (k in i:(j - 1L))
      if (m[i, j] == m[i, k] + m[k + 1L, j]) {
        stack[[length(stack) + 1L]] <- c(i, k)
        stack[[length(stack) + 1L]] <- c(k + 1L, j)
        break
      }
  }
  pairs <- if (length(pi)) {
    ord <- order(pi, pj); cbind(pi[ord], pj[ord])
  } else matrix(integer(0), 0L, 2L)
  list(pairs = pairs, total = if (n) m[1L, n] else 0L)
}

#' Score the covariation supporting a structure in an alignment
#'
#' @param alignment A character vector of aligned sequences.
#' @param structure A dot-bracket consensus structure, or NULL.
#' @param correction "none" or "miller_madow".
#' @param mode A member of the structure list.
#' @param min_loop The minimum hairpin loop for folding.
#' @param min_sequences Pairs with fewer ungapped sequences than this
#'   are reported as unsupported rather than scored as if they were.
#' @return A list with per-pair mutual information and support, the
#'   total, the pairs used, and how many were too sparsely covered to
#'   judge.
#' @export
morie_rnacov <- function(alignment, structure = NULL,
                         correction = "none", mode = "given",
                         min_loop = 3L, min_sequences = 4L) {
  if (!(mode %in% .RNACOV_STRUCTURES))
    stop("mode must be one of ", paste(.RNACOV_STRUCTURES, collapse = ", "))
  seqs <- toupper(gsub("T", "U", as.character(alignment), fixed = TRUE))
  if (!length(seqs)) stop("the alignment is empty")
  L <- nchar(seqs[1])
  if (any(nchar(seqs) != L))
    stop("every sequence must have the same length")

  folded <- 0L
  if (mode == "given") {
    if (is.null(structure)) stop("the given mode needs a structure")
    pairs <- morie_rnacov_parse(structure)
  } else {
    # Fold the first ungapped sequence: the dynamic program works on a
    # sequence, not an alignment, and using the first one is a stated
    # choice rather than a silent consensus nobody defined.
    base <- gsub("[-.]", "", seqs[1])
    nu <- morie_rnacov_nussinov(base, min_loop)
    pairs <- nu$pairs; folded <- nu$total
  }

  if (nrow(pairs)) for (k in seq_len(nrow(pairs)))
    if (pairs[k, 1] < 0 || pairs[k, 2] >= L || pairs[k, 1] >= pairs[k, 2])
      stop("pair (", pairs[k, 1], ", ", pairs[k, 2],
           ") is outside the alignment")

  np <- nrow(pairs)
  mis <- numeric(np); sup <- integer(np); cells <- integer(np)
  weak <- 0L
  if (np) for (k in seq_len(np)) {
    r <- morie_rnacov_mi(seqs, pairs[k, 1] + 1L, pairs[k, 2] + 1L,
                         correction)
    mis[k] <- r$mi; sup[k] <- r$n; cells[k] <- r$seen
    if (r$n < as.integer(min_sequences)) weak <- weak + 1L
  }
  total <- if (np) .w3_csum(mis) else 0
  strong <- if (np)
    which(sup >= as.integer(min_sequences) & mis > 0) - 1L else integer(0)
  list(pair_i = if (np) pairs[, 1] else integer(0),
       pair_j = if (np) pairs[, 2] else integer(0),
       mutual_information = mis, support = sup, cells_seen = cells,
       n_pairs = np, n_weak = weak, n_covarying = length(strong),
       covarying = strong, total = total,
       estimate = if (np) total / np else NaN, se = NaN,
       max_mi = if (np) max(mis) else NaN, n_sequences = length(seqs),
       length = L, folded_pairs = folded, correction = correction,
       mode = mode, method = "RNA covariance model scoring")
}

#' One-line summary of the rnacov module
#'
#' @return A character scalar.
#' @export
morie_rnacov_cheatsheet <- function()
  paste0("rnacov: RNA covariance model scoring. modes ",
         paste(.RNACOV_STRUCTURES, collapse = ", "),
         "; mutual information in bits, conservation is not covariation")
