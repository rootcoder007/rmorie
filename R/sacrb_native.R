# morie.fn -- function file (rootcoder007/morie)
# r"""BLEU, and why the number alone is not comparable.
#
# **The metric.** Modified n-gram precision clips each candidate n-gram
# count at the maximum seen in any reference, so repeating a correct
# word cannot inflate the score. Precision alone would reward
# short output, so a multiplicative brevity penalty is applied. With
# :math:`c` the total candidate length and :math:`r` the effective
# reference length,
#
# .. math:: BP = \begin{cases} 1 & c > r\\
#           e^{(1 - r/c)} & c \le r \end{cases}, \qquad
#           BLEU = BP \cdot \exp\Big(\sum_{n=1}^{N} w_n \log p_n\Big).
#
# Two details in that definition matter and are easy to get wrong.
#
# *The penalty is computed over the whole corpus, not per sentence.*
# Averaging per-sentence penalties would punish length deviation on
# short sentences far too harshly, so :math:`r` is the sum of best-match
# lengths across the corpus and :math:`c` the total candidate length.
# A sentence may be short if another is long.
#
# *The best match length is the closest reference length, not the
# shortest.* With references of 12, 15 and 17 words and a candidate of
# 12, the penalty is exactly 1.
#
# **Why long candidates are not penalised twice.** Modified precision
# already falls when the candidate is too long -- the extra n-grams are
# unmatched. So :math:`BP = 1` for :math:`c > r` by design, not by
# oversight.
#
# **And now the part that makes the number usable.** BLEU is a function
# of a *tokenisation*, and papers rarely report which one. The same
# system, the same output and the same references produce materially
# different BLEU depending on whether the text was tokenised with
# Moses, split on whitespace, lowercased, or had punctuation detached --
# and on whether the references were detokenised first. Scores from two
# papers are therefore not comparable unless both name a scheme, which
# they usually do not.
#
# sacreBLEU's answer is not a better metric but a **protocol**: score
# detokenised output against detokenised references, apply one named
# tokenisation internally, and emit a version string recording every
# choice. ``signature`` produces that string here. Two BLEU numbers are
# comparable exactly when their signatures match, and the anchor
# demonstrates the divergence rather than asserting it.
#
# References
# ----------
# Papineni, K., Roukos, S., Ward, T. & Zhu, W.-J. (2002) "BLEU: a
# Method for Automatic Evaluation of Machine Translation", *Proceedings
# of the 40th Annual Meeting of the Association for Computational
# Linguistics (ACL 2002)*, 311-318, doi:10.3115/1073083.1073135.
# Sec. 2.1 (modified n-gram precision with clipping), Sec. 2.2.2 (the
# sentence brevity penalty, the "best match length" as the closest
# reference length, and the argument for computing the penalty over the
# corpus rather than per sentence), and Sec. 2.3 (the geometric mean
# and the BP formula reproduced above).
#
# Post, M. (2018) "A Call for Clarity in Reporting BLEU Scores",
# *Proceedings of the Third Conference on Machine Translation (WMT18)*,
# 186-191, doi:10.18653/v1/W18-6319, arXiv:1804.08771. That BLEU depends on tokenisation and
# preprocessing, that these are usually unreported, and that scores are
# therefore not comparable across papers; the proposal to score
# detokenised output with a single internal tokenisation and to publish
# a version signature.
# """

.sacrb_TOKENIZERS <- c("13a", "intl", "none")
.sacrb_PUNCT <- ".,!?;:()\"'`-[]{}<>/\\|@#$%^&*+=~"
.sacrb_PUNCT_CHARS <- strsplit(.sacrb_PUNCT, "")[[1]]
.sacrb_EPS <- 1e-12

.sacrb_tokenize_13a <- function(text, lowercase = FALSE) {
  s <- as.character(text)
  if (lowercase) {
    s <- tolower(s)
  }
  n <- nchar(s)
  if (n == 0L) {
    return(character(0))
  }
  chars <- strsplit(s, "")[[1]]
  parts <- ifelse(chars %in% .sacrb_PUNCT_CHARS,
                  paste0(" ", chars, " "), chars)
  joined <- paste0(parts, collapse = "")
  out <- strsplit(trimws(joined), "\\s+")[[1]]
  if (length(out) == 1L && identical(out, "")) {
    out <- character(0)
  }
  out
}

.sacrb_tokenize_intl <- function(text, lowercase = FALSE) {
  s <- as.character(text)
  if (lowercase) {
    s <- tolower(s)
  }
  n <- nchar(s)
  if (n == 0L) {
    return(character(0))
  }
  chars <- strsplit(s, "")[[1]]
  out <- character(0)
  cur <- character(0)
  for (ch in chars) {
    if (grepl("[a-zA-Z0-9]", ch)) {
      cur <- c(cur, ch)
    } else {
      if (length(cur) > 0L) {
        out <- c(out, paste0(cur, collapse = ""))
        cur <- character(0)
      }
      if (!grepl("[[:space:]]", ch)) {
        out <- c(out, ch)
      }
    }
  }
  if (length(cur) > 0L) {
    out <- c(out, paste0(cur, collapse = ""))
  }
  out
}

.sacrb_tok <- function(text, scheme, lowercase) {
  if (scheme == "13a") {
    return(.sacrb_tokenize_13a(text, lowercase))
  }
  if (scheme == "intl") {
    return(.sacrb_tokenize_intl(text, lowercase))
  }
  if (scheme == "none") {
    s <- as.character(text)
    if (lowercase) {
      s <- tolower(s)
    }
    parts <- strsplit(trimws(s), "\\s+")[[1]]
    if (length(parts) == 1L && identical(parts, "")) {
      return(character(0))
    }
    return(parts)
  }
  stop(sprintf("sacrb: tokenizer must be one of %s, got %s",
               paste(.sacrb_TOKENIZERS, collapse = ", "),
               as.character(scheme)))
}

.sacrb_ngram_counts <- function(tokens, n) {
  if (as.integer(n) < 1L) {
    stop("sacrb: n must be at least 1")
  }
  n <- as.integer(n)
  L <- length(tokens)
  c <- list()
  if (L >= n) {
    for (i in seq_len(L - n + 1L)) {
      g <- tokens[i:(i + n - 1L)]
      key <- paste(g, collapse = "\r")
      if (is.null(c[[key]])) {
        c[[key]] <- 1L
      } else {
        c[[key]] <- c[[key]] + 1L
      }
    }
  }
  c
}

.sacrb_modified_precision <- function(cand_tokens, refs_tokens, n) {
  cc <- .sacrb_ngram_counts(cand_tokens, n)
  if (length(cc) == 0L) {
    return(list(numerator = 0L, denominator = 0L, precision = 0.0))
  }
  total <- sum(unlist(cc, use.names = FALSE))
  if (total == 0L) {
    return(list(numerator = 0L, denominator = 0L, precision = 0.0))
  }
  best <- list()
  for (rt in refs_tokens) {
    rc <- .sacrb_ngram_counts(rt, n)
    for (key in names(rc)) {
      v <- rc[[key]]
      if (is.null(best[[key]])) {
        best[[key]] <- v
      } else if (v > best[[key]]) {
        best[[key]] <- v
      }
    }
  }
  clipped <- 0L
  for (key in names(cc)) {
    v <- cc[[key]]
    b <- if (is.null(best[[key]])) 0L else best[[key]]
    clipped <- clipped + min(v, b)
  }
  list(
    numerator = clipped,
    denominator = total,
    precision = clipped / as.numeric(total)
  )
}

.sacrb_brevity_penalty <- function(c, r) {
  cv <- as.numeric(c)
  rv <- as.numeric(r)
  if (cv <= 0.0) {
    return(0.0)
  }
  if (cv > rv) {
    return(1.0)
  }
  exp(1.0 - rv / cv)
}

.sacrb_best_match <- function(clen, rlens) {
  if (length(rlens) == 0L) {
    return(0L)
  }
  diffs <- abs(rlens - clen)
  min_diff <- min(diffs)
  candidates <- rlens[diffs == min_diff]
  min(candidates)
}

morie_sacrb_bleu <- function(candidates, references, max_n = 4L,
                             weights = NULL, tokenizer = "13a",
                             lowercase = FALSE) {
  C <- as.character(candidates)
  R <- lapply(references, function(refs) as.character(refs))
  if (length(C) != length(R)) {
    stop(sprintf("sacrb: %d candidates but %d reference sets",
                 length(C), length(R)))
  }
  if (length(C) == 0L) {
    stop("sacrb: no candidates given")
  }
  if (any(lengths(R) == 0L)) {
    stop("sacrb: every candidate needs at least one reference")
  }
  N <- as.integer(max_n)
  if (N < 1L) {
    stop("sacrb: max_n must be at least 1")
  }
  if (is.null(weights)) {
    w <- rep(1.0 / N, N)
  } else {
    w <- as.numeric(weights)
  }
  if (length(w) != N) {
    stop(sprintf("sacrb: %d weights for max_n = %d", length(w), N))
  }
  if (abs(sum(w) - 1.0) > 1e-9) {
    stop(sprintf("sacrb: the weights must sum to 1, got %.6f", sum(w)))
  }
  num <- integer(N)
  den <- integer(N)
  c_total <- 0L
  r_total <- 0L
  for (i in seq_along(C)) {
    ct <- .sacrb_tok(C[i], tokenizer, lowercase)
    rt <- lapply(R[[i]], function(x) .sacrb_tok(x, tokenizer, lowercase))
    c_total <- c_total + length(ct)
    r_total <- r_total + .sacrb_best_match(
      length(ct),
      vapply(rt, length, integer(1L))
    )
    for (n in seq_len(N)) {
      mp <- .sacrb_modified_precision(ct, rt, n)
      num[n] <- num[n] + mp$numerator
      den[n] <- den[n] + mp$denominator
    }
  }
  precisions <- numeric(N)
  for (n in seq_len(N)) {
    precisions[n] <- if (den[n] > 0L) num[n] / den[n] else 0.0
  }
  bp <- .sacrb_brevity_penalty(c_total, r_total)
  if (any(precisions <= 0.0)) {
    score <- 0.0
  } else {
    score <- bp * exp(sum(w * log(precisions)))
  }
  list(
    estimate = score,
    bleu = score,
    score = 100.0 * score,
    precisions = precisions,
    bp = bp,
    candidate_length = c_total,
    reference_length = r_total,
    ratio = c_total / max(r_total, 1L),
    tokenizer = tokenizer,
    lowercase = as.logical(lowercase),
    max_n = N,
    signature = .sacrb_signature(tokenizer, lowercase, N,
                                 length(R[[1]])),
    method = paste0("corpus BLEU; Papineni et al. (2002) Sec. 2.3, ",
                    "reported with a sacreBLEU-style signature ",
                    "(Post 2018)")
  )
}

.sacrb_signature <- function(tokenizer = "13a", lowercase = FALSE,
                            max_n = 4L, n_refs = 1L,
                            version = "morie-sacrb-1") {
  paste0("nrefs:", as.integer(n_refs),
         "|case:", if (isTRUE(lowercase)) "lc" else "mixed",
         "|tok:", as.character(tokenizer),
         "|ngram:", as.integer(max_n),
         "|version:", version)
}

.sacrb_cheatsheet <- function() {
  paste0("sacrb: BLEU = BP * exp(sum w_n log p_n), with clipped ",
         "n-gram precision and BP = 1 if c > r else exp(1 - r/c). ",
         "The penalty is computed over the WHOLE CORPUS, not per ",
         "sentence, and r uses the CLOSEST reference length, not ",
         "the shortest. Long candidates are not penalised twice -- ",
         "modified precision already handles them. The number ",
         "depends on TOKENISATION, so two BLEU scores are ",
         "comparable only when their signatures match.")
}

# compact alias per ledger/NAMING.md
morie_sacrb_sacrebleu <- morie_sacrb_bleu

#' @rdname morie_sacrb_bleu
#' @export
morie_sacrb <- morie_sacrb_bleu
