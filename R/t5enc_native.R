# morie.fn -- function file (rootcoder007/morie)
# T5: every task as text-to-text.
#
# The unifying claim is a framing, not an architecture: **every** NLP
# problem -- translation, classification, regression, summarisation --
# is cast as feeding the model text and training it to produce text.
# Classification emits the class *name*; regression emits a number as a
# string, rounded to a fixed increment. One model, one loss
# (teacher-forced maximum likelihood), one decoding procedure, and a
# task prefix telling the model which job it is doing.
#
# **What the framing buys, and what it costs.** It buys the ability to
# mix every task in one training mixture and to transfer between them.
# It costs the guarantee that the output is well formed: a
# classification head cannot emit an invalid class, but a decoder can,
# and the paper handles this by treating any output that is not a valid
# label as wrong. ``parse_prediction`` reproduces that rule rather than
# snapping to the nearest label, because snapping would hide the failure
# mode the framing introduces.
#
# **The pre-training objective is span corruption.** Rather than masking
# single tokens, contiguous **spans** are dropped and replaced by a
# single sentinel, and the target is the concatenation of the dropped
# spans with their sentinels. That makes the target much shorter than
# the input, which is a computational argument as much as a modelling
# one -- and the paper's ablations pick 15% corruption with mean span
# length 3.
#
# **Relative position embeddings, shared across layers.** Position is
# encoded as a learned scalar added to the attention logits, bucketed by
# the *relative* offset between query and key, with buckets growing
# logarithmically so distant positions share parameters. There is no
# absolute position signal at all, which is what lets the model
# generalise past the training length.
#
# References
# ----------
# Raffel, C., Shazeer, N., Roberts, A., Lee, K., Narang, S., Matena, M.,
# Zhou, Y., Li, W. & Liu, P. J. (2020) "Exploring the Limits of Transfer
# Learning with a Unified Text-to-Text Transformer", *Journal of Machine
# Learning Research* 21(140), 1-67, arXiv:1910.10683. The text-to-text
# framework in which every task is fed text and produces text, with task
# prefixes; the treatment of classification by emitting the label text
# and of regression by emitting a rounded number as a string, with
# outputs that do not match any label counted as wrong; the span
# corruption pre-training objective with sentinels and the ablations
# selecting 15% corruption and mean span length 3; and the simplified
# relative position embeddings shared across layers, bucketed by
# relative offset with logarithmically growing buckets.
#
# Vaswani, A., Shazeer, N., Parmar, N., Uszkoreit, J., Jones, L.,
# Gomez, A. N., Kaiser, L. & Polosukhin, I. (2017) "Attention Is All You
# Need", *NIPS 2017*, 5998-6008, arXiv:1706.03762. The encoder-decoder
# being modified.
#
# Devlin, J., Chang, M.-W., Lee, K. & Toutanova, K. (2019) "BERT:
# Pre-training of Deep Bidirectional Transformers for Language
# Understanding", *NAACL-HLT 2019*, 4171-4186,
# doi:10.18653/v1/N19-1423. The single-token masking objective span
# corruption replaces.

#' t5enc_task_prefix
#'
#' A step of the t5enc_native implementation. Called by \code{morie_t5enc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param task Coerced to character by the body, with \code{as.character}.
#' @param text Coerced to character by the body, with \code{as.character}.
#' @return A character value.
#' @export
t5enc_task_prefix <- function(task, text) {
  t <- trimws(as.character(task))
  if (nchar(t) == 0L) {
    stop("t5enc: the task prefix cannot be empty -- the model has no other signal of which job it is doing")
  }
  sprintf("%s: %s", t, as.character(text))
}

#' t5enc_span_corruption
#'
#' A step of the t5enc_native implementation. Called by \code{morie_t5enc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tokens Coerced to character by the body, with \code{as.character}.
#' @param rate Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.15}.
#' @param mean_span Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{3}.
#' @param seed Passed to \code{.ghc_rng}. Defaults to \code{0}.
#' @param sentinel Defaults to \code{"<extra_id_\\\\\\\%d>"}.
#' @return A list with \code{input}, \code{target}, \code{n_spans}, \code{corrupted_tokens}, \code{corruption_rate}, \code{target_shorter_by}, \code{note}.
#' @export
t5enc_span_corruption <- function(tokens, rate = 0.15, mean_span = 3.0,
                                   seed = 0,
                                   sentinel = "<extra_id_%d>") {
  toks <- as.character(tokens)
  n <- length(toks)
  r <- as.numeric(rate)
  if (r <= 0 || r >= 1) {
    stop(sprintf("t5enc: the corruption rate must lie in (0,1), got %s",
                 format(rate)))
  }
  ms <- as.numeric(mean_span)
  if (ms < 1.0) {
    stop("t5enc: the mean span must be at least 1")
  }
  n_corrupt <- max(1L, as.integer(round(n * r)))
  n_spans <- max(1L, as.integer(round(n_corrupt / ms)))

  e_rng <- .ghc_rng(seed)
  u_vec <- .ghc_unif(e_rng, n_spans * 3L)
  candidates <- as.integer(u_vec * n) %% n
  sorted_unique <- sort(unique(candidates))
  starts <- sorted_unique[seq_len(min(n_spans, length(sorted_unique)))]

  used_logical <- logical(n)
  span_starts <- integer(0)
  span_ends <- integer(0)
  per <- max(1L, n_corrupt %/% max(length(starts), 1L))
  for (s in starts) {
    e_end <- min(n, s + per)
    r_idx <- seq.int(s, e_end - 1L) + 1L
    if (any(used_logical[r_idx])) next
    used_logical[r_idx] <- TRUE
    span_starts <- c(span_starts, as.integer(s))
    span_ends <- c(span_ends, as.integer(e_end))
  }
  if (length(span_starts) > 0L) {
    ord <- order(span_starts)
    span_starts <- span_starts[ord]
    span_ends <- span_ends[ord]
  }

  src <- character(0)
  tgt <- character(0)
  idx <- 0L
  pos <- 0L
  for (k in seq_along(span_starts)) {
    s <- span_starts[k]
    e_end <- span_ends[k]
    if (pos < s) {
      src <- c(src, toks[(pos + 1L):s])
    }
    src <- c(src, sprintf(sentinel, idx))
    tgt <- c(tgt, sprintf(sentinel, idx))
    if (s < e_end) {
      tgt <- c(tgt, toks[(s + 1L):e_end])
    }
    idx <- idx + 1L
    pos <- e_end
  }
  if (pos < n) {
    src <- c(src, toks[(pos + 1L):n])
  }
  tgt <- c(tgt, sprintf(sentinel, idx))

  corrupted_tokens <- sum(used_logical)
  list(
    input = src,
    target = tgt,
    n_spans = length(span_starts),
    corrupted_tokens = corrupted_tokens,
    corruption_rate = corrupted_tokens / n,
    target_shorter_by = length(src) - length(tgt),
    note = "one sentinel per SPAN, so the target is much shorter than the input"
  )
}

#' t5enc_relative_bucket
#'
#' A step of the t5enc_native implementation. Called by \code{morie_t5enc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param relative_position Coerced to integer by the body, with \code{as.integer}.
#' @param bidirectional A flag; the body branches on it. Defaults to \code{TRUE}.
#' @param num_buckets Coerced to integer by the body, with \code{as.integer}. Defaults to \code{32L}.
#' @param max_distance Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{128}.
#' @return A numeric value.
#' @export
t5enc_relative_bucket <- function(relative_position, bidirectional = TRUE,
                                   num_buckets = 32L, max_distance = 128) {
  nb <- as.integer(num_buckets)
  if (nb < 2L) {
    stop("t5enc: at least 2 buckets are needed")
  }
  rp <- as.integer(relative_position)
  ret <- 0L
  if (bidirectional) {
    nb <- nb %/% 2L
    ret <- ret + if (rp > 0L) nb else 0L
    rp <- abs(rp)
  } else {
    rp <- -min(rp, 0L)
  }
  exact <- nb %/% 2L
  if (rp < exact) {
    return(ret + rp)
  }
  v <- exact + as.integer(
    log(rp / as.numeric(exact)) / log(as.numeric(max_distance) / exact) * (nb - exact)
  )
  ret + min(v, nb - 1L)
}

#' t5enc_format_regression
#'
#' A step of the t5enc_native implementation. Called by \code{morie_t5enc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param value Coerced to numeric by the body, with \code{as.numeric}.
#' @param increment Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.2}.
#' @param lo Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @param hi Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{5}.
#' @return A character value.
#' @export
t5enc_format_regression <- function(value, increment = 0.2, lo = 1.0, hi = 5.0) {
  v <- min(max(as.numeric(value), as.numeric(lo)), as.numeric(hi))
  inc <- as.numeric(increment)
  if (inc <= 0) {
    stop("t5enc: the increment must be positive")
  }
  sprintf("%.1f", round(v / inc) * inc)
}

#' t5enc_parse_prediction
#'
#' A step of the t5enc_native implementation. Called by \code{morie_t5enc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param text Coerced to character by the body, with \code{as.character}.
#' @param labels Optional; may be \code{NULL}. Coerced to character by the body, with \code{as.character}.
#' @return A list with \code{label}, \code{valid}, \code{note}.
#' @export
t5enc_parse_prediction <- function(text, labels = NULL) {
  s <- trimws(as.character(text))
  if (is.null(labels)) {
    num <- suppressWarnings(as.numeric(s))
    if (!is.na(num)) {
      return(list(value = num, valid = TRUE))
    } else {
      return(list(value = NULL, valid = FALSE,
                  note = "not a number; counted as wrong"))
    }
  }
  ok <- s %in% as.character(labels)
  list(
    label = if (ok) s else NULL,
    valid = ok,
    note = "an output matching no label is counted as WRONG, not snapped to the nearest one"
  )
}

#' t5enc_cheatsheet
#'
#' A step of the t5enc_native implementation. Called by \code{morie_t5enc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
t5enc_cheatsheet <- function() {
  paste(
    "t5enc: EVERY task as text-to-text -- classification emits ",
    "the label TEXT, regression emits a rounded number as a ",
    "string, and a task prefix says which job it is. One ",
    "model, one loss, one decoder, and tasks can be mixed. The ",
    "cost: the decoder can emit something that is not a valid ",
    "label, and that counts as WRONG rather than being snapped ",
    "to the nearest one. Pre-training is SPAN corruption -- ",
    "contiguous spans replaced by ONE sentinel each, so the ",
    "target is far shorter (15%, mean span 3). Positions are ",
    "RELATIVE, log-bucketed, shared across layers; there is no ",
    "absolute position signal.",
    sep = ""
  )
}

# compact alias per ledger/NAMING.md
t5encoder <- t5enc_span_corruption

# public names resolved by fn/_lazy_map.json
t5 <- t5enc_span_corruption

# entry point
#' Entry point
#'
#' A step of the t5enc_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param method Defaults to \code{c("task_prefix", "span_corruption", "relative_bucket", "format_regression",     "parse_prediction", "cheatsheet")}.
#' @param ... Passed through.
#' @return The value of \code{switch}.
#' @export
morie_t5enc <- function(method = c("task_prefix", "span_corruption",
                                    "relative_bucket", "format_regression",
                                    "parse_prediction", "cheatsheet"), ...) {
  method <- match.arg(method)
  switch(method,
    task_prefix = t5enc_task_prefix(...),
    span_corruption = t5enc_span_corruption(...),
    relative_bucket = t5enc_relative_bucket(...),
    format_regression = t5enc_format_regression(...),
    parse_prediction = t5enc_parse_prediction(...),
    cheatsheet = t5enc_cheatsheet()
  )
}
