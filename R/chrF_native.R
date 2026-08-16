# Sources:
#   Popovic, M. (2015) "chrF: character n-gram F-score for automatic MT
#   evaluation", Proc. 10th Workshop on Statistical Machine Translation
#   (WMT15), 392-395.

#' .chrf_char_ngrams
#'
#' A step of the chrF_native implementation. Called by \code{chrf_score}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s A vector; its length is taken.
#' @param n Numeric; combined arithmetically in the body.
#' @return The value of \code{out}, as built in the body.
#' @export
.chrf_char_ngrams <- function(s, n) {
  L <- nchar(s)
  if (L < n) return(list())
  out <- vector("list", L - n + 1L)
  for (i in seq_len(L - n + 1L)) out[[i]] <- substr(s, i, i + n - 1L)
  out
}

#' .chrf_word_ngrams
#'
#' A step of the chrF_native implementation. Called by \code{chrf_score}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ws A vector; its length is taken and its elements indexed.
#' @param n Numeric; combined arithmetically in the body.
#' @return The value of \code{out}, as built in the body.
#' @export
.chrf_word_ngrams <- function(ws, n) {
  L <- length(ws)
  if (L < n) return(list())
  out <- vector("list", L - n + 1L)
  for (i in seq_len(L - n + 1L)) out[[i]] <- ws[i:(i + n - 1L)]
  out
}

#' .chrf_counts
#'
#' A step of the chrF_native implementation. Called by \code{.chrf_pr}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param seq See Usage.
#' @return The value of \code{d}, as built in the body.
#' @export
.chrf_counts <- function(seq) {
  d <- list()
  for (g in seq) {
    key <- if (is.character(g)) g else paste(g, collapse = "\r")
    d[[key]] <- (d[[key]] %||% 0L) + 1L
  }
  d
}

#' .chrf_pr
#'
#' A step of the chrF_native implementation. Called by \code{chrf_score}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param hyp_grams A vector; its length is taken.
#' @param ref_grams A vector; its length is taken.
#' @return A vector, from \code{c}.
#' @export
.chrf_pr <- function(hyp_grams, ref_grams) {
  if (length(hyp_grams) == 0L || length(ref_grams) == 0L) return(NULL)
  hc <- .chrf_counts(hyp_grams)
  rc <- .chrf_counts(ref_grams)
  match <- 0L
  for (g in names(hc)) {
    v <- hc[[g]]
    rv <- if (is.null(rc[[g]])) 0L else rc[[g]]
    match <- match + min(v, rv)
  }
  c(match / as.numeric(length(hyp_grams)), match / as.numeric(length(ref_grams)))
}

#' chrf_score
#'
#' A step of the chrF_native implementation. Called by \code{morie_chrF}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param hypothesis Coerced to character by the body, with \code{as.character}.
#' @param reference A vector; its length is taken.
#' @param n_char Coerced to integer by the body, with \code{as.integer}. Defaults to \code{6L}.
#' @param beta Numeric; combined arithmetically in the body. Defaults to \code{2}.
#' @param remove_whitespace A flag; the body branches on it. Defaults to \code{TRUE}.
#' @param word_order Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.
#' @return The value of \code{best}, as built in the body.
#' @export
chrf_score <- function(hypothesis, reference, n_char = 6L, beta = 2.0,
                       remove_whitespace = TRUE, word_order = 0L) {
  N <- as.integer(n_char)
  if (N < 1L) stop(sprintf("chrf_score: n_char must be at least 1, got %r", n_char))
  beta <- as.numeric(beta)
  if (beta <= 0.0) stop(sprintf("chrf_score: beta must be positive, got %r", beta))
  w_order <- as.integer(word_order)
  if (w_order < 0L) stop("chrf_score: word_order must be >= 0")

  refs <- if (is.character(reference) && length(reference) >= 1L && !is.null(names(reference)) == FALSE) {
    if (length(reference) == 1L) list(reference) else as.list(reference)
  } else if (is.list(reference)) {
    reference
  } else {
    list(as.character(reference))
  }
  hyp <- as.character(hypothesis)

  best <- NULL
  for (ref in refs) {
    ref <- as.character(ref)
    h_words <- strsplit(hyp, "\\s+")[[1L]]
    r_words <- strsplit(ref, "\\s+")[[1L]]
    h_words <- h_words[nzchar(h_words)]
    r_words <- r_words[nzchar(r_words)]
    h_chars <- if (isTRUE(remove_whitespace)) paste(h_words, collapse = "") else hyp
    r_chars <- if (isTRUE(remove_whitespace)) paste(r_words, collapse = "") else ref

    precs <- numeric(0)
    recs <- numeric(0)
    per_order <- list()
    for (n in seq_len(N)) {
      pr <- .chrf_pr(.chrf_char_ngrams(h_chars, n), .chrf_char_ngrams(r_chars, n))
      if (is.null(pr)) {
        per_order[[length(per_order) + 1L]] <- list(n = n, precision = NA_real_, recall = NA_real_)
        next
      }
      precs <- c(precs, pr[1L])
      recs <- c(recs, pr[2L])
      per_order[[length(per_order) + 1L]] <- list(n = n, precision = pr[1L], recall = pr[2L])
    }
    for (n in seq_len(w_order)) {
      pr <- .chrf_pr(.chrf_word_ngrams(h_words, n), .chrf_word_ngrams(r_words, n))
      if (is.null(pr)) next
      precs <- c(precs, pr[1L])
      recs <- c(recs, pr[2L])
    }
    chrP <- if (length(precs)) sum(precs) / length(precs) else 0.0
    chrR <- if (length(recs)) sum(recs) / length(recs) else 0.0
    b2 <- beta * beta
    denom <- b2 * chrP + chrR
    f <- if (denom > 0.0) (1.0 + b2) * chrP * chrR / denom else 0.0
    cand <- list(
      estimate = f, chrf = f, chrP = chrP, chrR = chrR,
      per_order = per_order, n_char = N, beta = beta,
      word_order = w_order,
      remove_whitespace = isTRUE(remove_whitespace)
    )
    if (is.null(best) || cand$chrf > best$chrf) best <- cand
  }
  best$method <- sprintf("chrF%g, arithmetic mean over character n-gram orders (Popovic 2015)", beta)
  best
}

chrF <- chrf_score

#' morie_chrF
#'
#' A step of the chrF_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param hypothesis See Usage.
#' @param reference See Usage.
#' @param n_char Defaults to \code{6L}.
#' @param beta Defaults to \code{2}.
#' @param remove_whitespace Defaults to \code{TRUE}.
#' @param word_order Defaults to \code{0L}.
#' @return The value of \code{chrf_score}.
#' @export
morie_chrF <- function(hypothesis, reference, n_char = 6L, beta = 2.0,
                       remove_whitespace = TRUE, word_order = 0L) {
  chrf_score(hypothesis, reference, n_char, beta, remove_whitespace, word_order)
}
