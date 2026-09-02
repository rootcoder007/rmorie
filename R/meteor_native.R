# SPDX-License-Identifier: AGPL-3.0-or-later
#
# METEOR sentence metric, exact-match stage (Meteor). Bit-identical
# mirror of src/morie/fn/meteor.py. Anchored on the paper's own
# two-chunk worked example and hand fraction arithmetic.

#' METEOR machine-translation metric (exact-match stage)
#'
#' Candidate unigrams align to reference unigrams by exact match
#' (left to right, earliest unmatched reference occurrence). With m
#' matches, precision P = m/len(candidate), recall
#' R = m/len(reference), and
#' \eqn{Fmean = 10 P R / (R + 9 P)}. Matched unigrams group into the
#' fewest chunks of contiguous candidate positions mapped to
#' contiguous reference positions, giving
#' \eqn{Penalty = 0.5 (chunks/m)^3} and
#' \eqn{Score = Fmean (1 - Penalty)}. No matches gives score 0.
#'
#' @param candidate System translation (string split on whitespace,
#'   or character vector of tokens).
#' @param reference Reference translation.
#' @param lowercase Case-fold before matching (default TRUE).
#' @return List with \code{score}, \code{fmean}, \code{penalty},
#'   \code{precision}, \code{recall}, \code{matches}, \code{chunks},
#'   \code{len_candidate}, \code{len_reference}, \code{method}.
#' @references Banerjee, S. and Lavie, A. (2005), METEOR: An
#'   automatic metric for MT evaluation with improved correlation
#'   with human judgments, Proceedings of the ACL Workshop on
#'   Intrinsic and Extrinsic Evaluation Measures for Machine
#'   Translation and/or Summarization, Ann Arbor, 65-72. Fmean,
#'   penalty and score formulas with the worked example, Section
#'   2.1, p. 68. Local source:
#'   library/pdf/fetched-wave3/Banerjee-Lavie-2005-METEOR-ACL.pdf.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Meteor(V, V)
Meteor <- function(candidate, reference, lowercase = TRUE) {
  tok <- function(x) {
    if (length(x) == 1L && is.character(x)) {
      strsplit(trimws(x), "[[:space:]]+")[[1]]
    } else {
      as.character(x)
    }
  }
  cand <- tok(candidate)
  ref <- tok(reference)
  if (lowercase) { cand <- tolower(cand)
  ref <- tolower(ref) }
  if (length(cand) == 0L || length(ref) == 0L) {
    stop("candidate and reference must be nonempty", call. = FALSE)
  }
  used <- rep(FALSE, length(ref))
  ci <- integer(0)
  rj <- integer(0)
  for (i in seq_along(cand)) {
    for (j in seq_along(ref)) {
      if (!used[j] && ref[j] == cand[i]) {
        used[j] <- TRUE
        ci <- c(ci, i)
        rj <- c(rj, j)
        break
      }
    }
  }
  m <- length(ci)
  if (m == 0L) {
    return(list(score = 0, fmean = 0, penalty = 0, precision = 0,
                recall = 0, matches = 0L, chunks = 0L,
                len_candidate = length(cand),
                len_reference = length(ref),
                method = "METEOR exact-match stage (Banerjee-Lavie 2005)"))
  }
  prec <- m / length(cand)
  rec <- m / length(ref)
  fmean <- 10 * prec * rec / (rec + 9 * prec)
  ch <- 1L
  if (m > 1L) {
    for (k in 2:m) {
      if (!(ci[k] == ci[k - 1] + 1L && rj[k] == rj[k - 1] + 1L)) {
        ch <- ch + 1L
      }
    }
  }
  penalty <- 0.5 * (ch / m)^3
  list(score = fmean * (1 - penalty), fmean = fmean,
       penalty = penalty, precision = prec, recall = rec,
       matches = m, chunks = ch, len_candidate = length(cand),
       len_reference = length(ref),
       method = "METEOR exact-match stage (Banerjee-Lavie 2005)")
}
