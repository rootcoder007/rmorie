# SPDX-License-Identifier: AGPL-3.0-or-later
#' BLEU: n-gram precision score for machine translation
#'
#' Papineni, Roukos, Ward and Zhu (2002), "BLEU: a method for automatic
#' evaluation of machine translation", Proceedings of the 40th Annual Meeting
#' of the ACL, 311-318, read from the ACL Anthology PDF (P02-1040).  Sections
#' 2.1-2.3 give, verbatim, the modified precision
#' p_n = sum_C sum_ngram Count_clip(ngram) / sum_C2 sum_ngram2 Count(ngram2),
#' the brevity penalty BP = 1 if c > r and exp(1 - r/c) if c <= r, and
#' BLEU = BP exp(sum_n w_n log p_n) with N = 4 and w_n = 1/N.
#'
#' Two details carry the whole method.  Clipping: Count_clip is the candidate
#' count truncated at the largest count the n-gram attains in any single
#' reference; without it the paper's own Example 2, candidate
#' "the the the the the the the" against "The cat is on the mat.", scores a
#' unigram precision of 7/7 instead of the 2/7 the paper prints, and that
#' printed 2/7 is the anchor.  And the brevity penalty is one-sided:
#' candidates longer than the reference are already punished by precision, so
#' BP only bites when c <= r, with r the "best match length", the reference
#' length closest to c, ties going to the shorter.
#'
#' Case folding is the only normalisation the paper performs and the only one
#' done here.  A zero at any order sends the geometric mean to zero and BLEU is
#' honestly reported as 0 rather than smoothed; the per-order precisions are
#' returned so the cause is visible.
#'
#' @param candidate the candidate translation, a string or token vector.
#' @param references one reference or a list of them.
#' @param max_n highest n-gram order, N in the paper.
#' @return list: bleu, estimate, p_n, clipped, total, bp, c, r, log_geo_mean,
#'   max_n, n_ref, method.
#' @keywords internal
#' @examples
#' BleuS("the the the the the the the", "The cat is on the mat", max_n = 1)$p_n
#' @export
BleuS <- function(candidate, references, max_n = 4) {
  N <- as.integer(max_n)
  if (N < 1L) stop("bleu: max_n must be at least one")
  cand <- .bleu_tok(candidate)
  if (length(cand) == 0L) stop("bleu: the candidate is empty")
  refs <- if (is.list(references)) lapply(references, .bleu_tok) else
    lapply(as.list(references), .bleu_tok)
  if (length(refs) == 0L) stop("bleu: no references given")
  for (r in refs) if (length(r) == 0L) stop("bleu: a reference is empty")
  pn <- numeric(N); num <- numeric(N); den <- numeric(N)
  for (n in seq_len(N)) {
    ab <- .bleu_mp(cand, refs, n)
    num[n] <- ab[1]; den[n] <- ab[2]
    pn[n] <- if (ab[2] > 0) ab[1] / ab[2] else 0
  }
  cc <- length(cand)
  rlens <- sort(vapply(refs, length, 0L))
  best <- rlens[1]
  for (rr in rlens) if (abs(rr - cc) < abs(best - cc)) best <- rr
  bp <- if (cc > best) 1 else exp(1 - best / cc)
  w <- 1 / N
  if (min(pn) <= 0) {
    sc <- 0; logsum <- -Inf
  } else {
    logsum <- 0
    for (p in pn) logsum <- logsum + w * log(p)
    sc <- bp * exp(logsum)
  }
  list(bleu = sc, estimate = sc, p_n = pn, clipped = num, total = den, bp = bp,
       c = cc, r = best, log_geo_mean = logsum, max_n = N, n_ref = length(refs),
       method = "Papineni et al. (2002) BLEU, clipped n-gram precision, one-sided brevity penalty")
}

#' @noRd
.bleu_tok <- function(s) {
  if (is.character(s) && length(s) == 1L) {
    t <- strsplit(tolower(s), "[[:space:]]+")[[1]]
    t[nzchar(t)]
  } else tolower(as.character(s))
}

#' @noRd
.bleu_ng <- function(toks, n) {
  k <- length(toks) - n + 1L
  if (k < 1L) return(character(0))
  vapply(seq_len(k), function(i) paste(toks[i:(i + n - 1L)], collapse = " "), "")
}

#' @noRd
.bleu_mp <- function(cand, refs, n) {
  cg <- .bleu_ng(cand, n)
  if (length(cg) == 0L) return(c(0, 0))
  ct <- table(cg)
  mx <- list()
  for (r in refs) {
    rg <- .bleu_ng(r, n)
    if (length(rg) == 0L) next
    rt <- table(rg)
    for (g in names(rt)) {
      prev <- if (is.null(mx[[g]])) 0 else mx[[g]]
      if (as.numeric(rt[[g]]) > prev) mx[[g]] <- as.numeric(rt[[g]])
    }
  }
  clip <- 0
  for (g in names(ct)) {
    m <- if (is.null(mx[[g]])) 0 else mx[[g]]
    clip <- clip + min(as.numeric(ct[[g]]), m)
  }
  c(clip, length(cg))
}
