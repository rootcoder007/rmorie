# SPDX-License-Identifier: AGPL-3.0-or-later
#
# FIMO-style PWM scan with exact Staden p-values (Motfom).
# Bit-identical mirror of src/morie/fn/motfom.py. Anchored against
# brute-force enumeration of all 4^w words under the background.

#' FIMO-style position weight matrix scan
#'
#' For each position i of the sequence the log-likelihood ratio score
#' of the length-w window is
#' \eqn{score(i) = \sum_j \log_2(p(b_{i+j-1}, j) / bg(b))}, where
#' p(b, j) is the motif probability of base b at column j and bg the
#' zero-order background. Scores are converted to p-values
#' \eqn{P(S \ge score)} with the Staden (1994) dynamic-programming
#' distribution of the discretized score (integer grid of 1/scale
#' bits), as in FIMO. Scanning is single-strand.
#'
#' @param sequence DNA string over ACGT.
#' @param pwm Motif probability matrix, w rows, 4 columns (A, C, G, T).
#' @param background Optional 4 background frequencies (default
#'   uniform).
#' @param pseudocount Added to every pwm cell before normalization.
#' @param scale Discretization grid, scores rounded to 1/scale bits.
#' @return List with \code{scores}, \code{pvalues},
#'   \code{best_score}, \code{best_pvalue}, \code{best_position}
#'   (0-based), \code{width}, \code{n_windows}, \code{method}.
#' @references Grant, C. E., Bailey, T. L. and Noble, W. S. (2011),
#'   FIMO: scanning for occurrences of a given motif, Bioinformatics
#'   27(7), 1017-1018 (score and dynamic-programming p-values,
#'   Methods). Staden, R. (1994), Searching for motifs in nucleic
#'   acid sequences, Methods in Molecular Biology 25, 93-102. Local
#'   source: library/pdf/fetched-wave3/Grant-2011-FIMO-Bioinformatics.pdf.
#' @export
Motfom <- function(sequence, pwm, background = NULL, pseudocount = 0,
                   scale = 1000L) {
  seq_ <- toupper(as.character(sequence)[1])
  P <- as.matrix(pwm)
  if (ncol(P) != 4L) stop("pwm must be (w, 4)", call. = FALSE)
  w <- nrow(P)
  bg <- if (is.null(background)) rep(0.25, 4) else {
    b <- as.numeric(background); b / sum(b)
  }
  llr <- matrix(0, w, 4)
  for (j in seq_len(w)) {
    tot <- sum(P[j, ]) + 4 * pseudocount
    for (a in 1:4) {
      pa <- (P[j, a] + pseudocount) / tot
      if (pa <= 0 || bg[a] <= 0) {
        stop("zero probability; use a pseudocount", call. = FALSE)
      }
      llr[j, a] <- log2(pa / bg[a])
    }
  }
  illr <- matrix(as.integer(round(llr * scale)), w, 4)
  lo <- sum(apply(illr, 1, min))
  hi <- sum(apply(illr, 1, max))
  pdf <- new.env(hash = TRUE)
  assign("0", 1.0, envir = pdf)
  for (j in seq_len(w)) {
    nxt <- new.env(hash = TRUE)
    for (key in ls(pdf)) {
      s <- as.integer(key)
      pr <- get(key, envir = pdf)
      for (a in 1:4) {
        k2 <- as.character(s + illr[j, a])
        old <- if (exists(k2, envir = nxt, inherits = FALSE)) {
          get(k2, envir = nxt)
        } else 0.0
        assign(k2, old + pr * bg[a], envir = nxt)
      }
    }
    pdf <- nxt
  }
  dense <- numeric(hi - lo + 1)
  for (key in ls(pdf)) {
    dense[as.integer(key) - lo + 1] <- get(key, envir = pdf)
  }
  surv <- numeric(length(dense) + 1)
  for (s in length(dense):1) surv[s] <- surv[s + 1] + dense[s]
  chars <- strsplit(seq_, "")[[1]]
  code <- match(chars, c("A", "C", "G", "T"))
  n_win <- length(chars) - w + 1
  if (n_win < 1) stop("sequence shorter than motif", call. = FALSE)
  scores <- numeric(n_win); pvals <- numeric(n_win)
  for (i in seq_len(n_win)) {
    a <- code[i:(i + w - 1)]
    if (anyNA(a)) { scores[i] <- NaN; pvals[i] <- NaN; next }
    s_bits <- 0; s_int <- 0L
    for (j in seq_len(w)) {
      s_bits <- s_bits + llr[j, a[j]]
      s_int <- s_int + illr[j, a[j]]
    }
    scores[i] <- s_bits
    pvals[i] <- surv[s_int - lo + 1]
  }
  finite <- which(!is.nan(pvals))
  if (length(finite) == 0L) {
    stop("no scorable window (non-ACGT sequence)", call. = FALSE)
  }
  best <- finite[which.max(scores[finite])]
  list(scores = scores, pvalues = pvals,
       best_score = scores[best], best_pvalue = pvals[best],
       best_position = best - 1L, width = w, n_windows = n_win,
       method = "FIMO PWM scan, Staden DP p-values (Grant et al. 2011)")
}
