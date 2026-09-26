# SentencePiece-style subword tokeniser.
# Sources: Kudo, T. & Richardson, J. (2018) "SentencePiece: A simple
# and language independent subword tokenizer and detokenizer for
# Neural Text Processing", EMNLP 2018 Sys. Demos, 66-71,
# doi:10.18653/v1/D18-2012; Sennrich, R., Haddow, B. & Birch, A.
# (2016) "Neural Machine Translation of Rare Words with Subword
# Units", ACL 2016, 1715-1725, arXiv:1508.07909; Kudo, T. (2018)
# "Subword Regularization", arXiv:1804.10959.
#
# Native implementation mirroring morie.fn.sentpc exactly: the
# U+2581 escape/unescape round-trip, the BPE merge loop, and the
# unigram Viterbi over the lattice of all splits.

.SENT_SPACE <- "\u2581"

#' Replace spaces with the U+2581 marker
#'
#' Nothing is dropped, which is the whole point: the transform is
#' invertible by string replacement alone.
#'
#' @param text Character.
#' @param add_prefix Logical; prepend a marker so leading spaces
#'   survive the round-trip.
#' @return Character.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_sentpc_escape_whitespace(V)
#' @keywords internal
morie_sentpc_escape_whitespace <- function(text, add_prefix = TRUE) {
  out <- gsub(" ", .SENT_SPACE, as.character(text), fixed = TRUE)
  if (isTRUE(add_prefix)) out <- paste0(.SENT_SPACE, out)
  out
}

#' Invert \code{escape_whitespace} exactly
#'
#' @param text Character.
#' @param strip_prefix Logical; remove the leading marker if present.
#' @return Character.
#' @export
#' @examples
#' morie_sentpc_unescape_whitespace(text = 5L)
#' @keywords internal
morie_sentpc_unescape_whitespace <- function(text,
                                              strip_prefix = TRUE) {
  s <- as.character(text)
  if (isTRUE(strip_prefix) && startsWith(s, .SENT_SPACE)) s <- substr(s, 2L, nchar(s))
  gsub(.SENT_SPACE, " ", s, fixed = TRUE)
}

#' Split an escaped string on the marker
#'
#' Every unit after the first begins with U+2581 so a naive
#' \code{strsplit} on the marker (and re-attaching one) would collapse
#' runs of spaces, breaking the lossless identity.
#'
#' @param escaped Character.
#' @return Character vector of units.
#' @keywords internal
#' @noRd
.sentpc_units <- function(escaped) {
  s <- strsplit(escaped, "")[[1]]
  out <- character(0)
  cur <- ""
  for (ch in s) {
    if (ch == .SENT_SPACE) {
      if (nzchar(cur)) out <- c(out, cur)
      cur <- .SENT_SPACE
    } else {
      cur <- paste0(cur, ch)
    }
  }
  if (nzchar(cur)) out <- c(out, cur)
  out
}

#' Join pieces and unescape
#'
#' @param pieces Character vector of pieces.
#' @param strip_prefix Logical; drop a leading U+2581.
#' @return Character.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_sentpc_decode(V)
#' @keywords internal
morie_sentpc_decode <- function(pieces, strip_prefix = TRUE) {
  morie_sentpc_unescape_whitespace(paste0(as.character(pieces), collapse = ""),
                                    strip_prefix = strip_prefix)
}

#' Train a greedy BPE model
#'
#' Merge the most frequent adjacent pair until the vocabulary budget
#' is met. Greedy and deterministic: the merge list fully determines
#' every future segmentation.
#'
#' @param corpus Character vector of training sentences.
#' @param vocab_size Integer target vocabulary size.
#' @param add_prefix Logical; escape-and-prefix before counting.
#' @return List with \code{merges}, \code{vocab}, \code{vocab_size},
#'   \code{requested}, \code{algorithm}, \code{note}.
#' @export
#' @examples
#' morie_sentpc_train_bpe(corpus = c(1, 2, 3, 4, 5, 6, 7, 8), vocab_size = 5L)
#' @keywords internal
morie_sentpc_train_bpe <- function(corpus, vocab_size, add_prefix = TRUE) {
  V <- as.integer(vocab_size)
  if (V < 1L) stop("sentpc: vocab_size must be at least 1")
  words <- list()
  for (line in corpus) {
    for (w in .sentpc_units(morie_sentpc_escape_whitespace(line, add_prefix))) {
      key <- paste0(w, "\r")
      words[[key]] <- if (is.null(words[[key]])) 1L
                      else words[[key]] + 1L
    }
  }
  if (length(words) == 0L)
    stop("sentpc: the corpus produced no tokens")
  # rebuild keys: store raw character vectors
  words2 <- list()
  for (k in names(words)) {
    words2[[k]] <- list(rep = strsplit(substr(k, 1L, nchar(k) - 1L), "")[[1]],
                       f = words[[k]])
  }
  alphabet <- sort(unique(unlist(lapply(words2, function(x) x$rep), use.names = FALSE)))
  merges <- list()
  vocab <- unique(alphabet)
  while (length(vocab) < V) {
    pairs <- list()
    for (w in words2) {
      r <- w$rep
      f <- w$f
      if (length(r) < 2L) next
      for (i in seq_len(length(r) - 1L)) {
        key <- paste0(c(r[i], r[i + 1L]), collapse = "\r")
        pairs[[key]] <- (if (is.null(pairs[[key]])) 0L else pairs[[key]]) + f
      }
    }
    if (length(pairs) == 0L) break
    counts <- unlist(pairs)
    keys <- names(pairs)
    best_i <- which.max(counts)
    best_key <- keys[best_i]
    best <- strsplit(best_key, "\r", fixed = TRUE)[[1]]
    merges[[length(merges) + 1L]] <- best
    vocab <- unique(c(vocab, paste0(best, collapse = "")))
    nw <- list()
    for (w in words2) {
      r <- w$rep
      f <- w$f
      out <- character(0)
      i <- 1L
      while (i <= length(r)) {
        if (i < length(r) && r[i] == best[1] && r[i + 1L] == best[2]) {
          out <- c(out, paste0(best, collapse = ""))
          i <- i + 2L
        } else { out <- c(out, r[i])
        i <- i + 1L }
      }
      key <- paste0(out, collapse = "\r")
      if (is.null(nw[[key]])) nw[[key]] <- f else nw[[key]] <- nw[[key]] + f
    }
    words2 <- lapply(names(nw), function(k) list(rep = strsplit(k, "\r", fixed = TRUE)[[1]], f = nw[[k]]))
    names(words2) <- names(nw)
  }
  list(merges = merges, vocab = sort(vocab),
       vocab_size = length(vocab), requested = V, algorithm = "bpe",
       note = "greedy and deterministic -- the merge list fixes every later segmentation")
}

#' Apply a trained BPE model
#'
#' @param text Character.
#' @param model Output of \code{train_bpe}.
#' @param add_prefix Logical; escape-and-prefix.
#' @return Character vector of pieces.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' morie_sentpc_encode_bpe(V, D)
#' @keywords internal
morie_sentpc_encode_bpe <- function(text, model, add_prefix = TRUE) {
  esc <- morie_sentpc_escape_whitespace(text, add_prefix)
  out <- character(0)
  for (w in .sentpc_units(esc)) {
    toks <- strsplit(w, "", fixed = TRUE)[[1]]
    for (ab in model$merges) {
      a <- ab[1]
      b <- ab[2]
      ab_join <- paste0(ab, collapse = "")
      i <- 1L
      new <- character(0)
      while (i <= length(toks)) {
        if (i < length(toks) && toks[i] == a && toks[i + 1L] == b) {
          new <- c(new, ab_join)
          i <- i + 2L
        } else { new <- c(new, toks[i])
        i <- i + 1L }
      }
      toks <- new
    }
    out <- c(out, toks)
  }
  out
}

#' Unigram LM segmentation via Viterbi
#'
#' Maximises \code{sum(log p(piece))} over every split of the escaped
#' input. Unlike BPE this scores every segmentation, so the best is a
#' maximisation rather than the by-product of a construction.
#'
#' @param text Character.
#' @param piece_logp Named numeric vector of log probabilities.
#' @param add_prefix Logical; escape-and-prefix.
#' @return List with \code{pieces}, \code{logp}, \code{n_pieces},
#'   \code{algorithm}.
#' @export
#' @keywords internal
morie_sentpc_viterbi_segment <- function(text, piece_logp,
                                          add_prefix = TRUE) {
  s <- morie_sentpc_escape_whitespace(text, add_prefix)
  n <- nchar(s)
  if (n == 0L) return(list(pieces = character(0), logp = 0,
                             n_pieces = 0L,
                             algorithm = "unigram (Viterbi)"))
  maxlen <- max(c(1L, nchar(names(piece_logp))))
  best <- rep(-Inf, n + 1L)
  back <- vector("list", n + 1L)
  best[1L] <- 0
  # substr vectorised over start positions in s
  for (i in 2:(n + 1L)) {
    Lmax <- min(maxlen, i - 1L)
    for (L in 1:Lmax) {
      piece <- substr(s, i - L, i - 1L)
      lp <- piece_logp[[piece]]
      if (is.null(lp)) next
      cand <- best[i - L] + lp
      if (cand > best[i]) {
        best[i] <- cand
        back[[i]] <- c(i - L, piece)
      }
    }
  }
  if (is.infinite(best[n + 1L]) && best[n + 1L] < 0)
    stop("sentpc: no segmentation covers the input -- the piece set must include every character")
  pieces <- character(0)
  i <- n + 1L
  while (i > 1L) {
    st <- back[[i]]
    pieces <- c(st[2], pieces)
    i <- as.integer(st[1])
  }
  list(pieces = pieces, logp = best[n + 1L], n_pieces = length(pieces),
       algorithm = "unigram (Viterbi)")
}

# house entry point: the package exports one morie_<module>
morie_sentpc <- morie_sentpc_escape_whitespace

# -- restored: morie-only definition kept through the rmorie sync --
#' .escape_whitespace
#'
#' A step of the sentpc_native implementation. Called by \code{encode_bpe},
#' \code{escape_whitespace}, \code{train_bpe} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param text Coerced to character by the body, with \code{as.character}.
#' @param add_prefix A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return The value of \code{out}, as built in the body.
#' @export
#' @examples
#' txt <- c('alpha', 'beta', 'gamma', 'delta')
#' res <- .escape_whitespace(text = txt)
#' res
.escape_whitespace <- function(text, add_prefix = TRUE) {
  s <- as.character(text)
  out <- gsub(" ", .SPACE, s, fixed = TRUE)
  if (isTRUE(add_prefix)) out <- paste0(.SPACE, out)
  out
}

# -- restored: morie-only definition kept through the rmorie sync --
#' .unescape_whitespace
#'
#' A step of the sentpc_native implementation. Called by \code{morie_sentpc_decode},
#' \code{unescape_whitespace}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param text Coerced to character by the body, with \code{as.character}.
#' @param strip_prefix A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return The value of \code{gsub}.
#' @export
.unescape_whitespace <- function(text, strip_prefix = TRUE) {
  s <- as.character(text)
  if (isTRUE(strip_prefix) && startsWith(s, .SPACE))
    s <- substring(s, 2L)
  gsub(.SPACE, " ", s, fixed = TRUE)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Split an escaped string so every unit after the first begins with
#'
#' U+2581; joining reproduces the input exactly so runs of spaces do not
#' collapse.
#'
#' @param escaped Character; passed to \code{strsplit}.
#' @return The value of \code{out}, as built in the body.
#' @export
.units <- function(escaped) {
  out <- character(0)
  cur <- ""
  chars <- strsplit(escaped, "")[[1]]
  for (ch in chars) {
    if (ch == .SPACE) {
      if (nzchar(cur)) out <- c(out, cur)
      cur <- .SPACE
    } else {
      cur <- paste0(cur, ch)
    }
  }
  if (nzchar(cur)) out <- c(out, cur)
  out
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Encode text with a trained BPE model
#' @param text See Usage.
#' @param model See Usage.
#' @param add_prefix See Usage.
#' @export
encode_bpe <- function(text, model, add_prefix = TRUE) {
  esc <- .escape_whitespace(text, add_prefix)
  out <- character(0)
  for (w in .units(esc)) {
    toks <- strsplit(w, "")[[1]]
    for (m in model$merges) {
      a <- m[1L]
      b <- m[2L]
      i <- 1L
      neww <- character(0)
      while (i <= length(toks)) {
        if (i < length(toks) && toks[i] == a && toks[i + 1L] == b) {
          neww <- c(neww, paste0(toks[i], toks[i + 1L]))
          i <- i + 2L
        } else {
          neww <- c(neww, toks[i])
          i <- i + 1L
        }
      }
      toks <- neww
    }
    out <- c(out, toks)
  }
  out
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Escape whitespace as U+2581, optionally prefixing the marker
#' @param text See Usage.
#' @param add_prefix See Usage.
#' @export
escape_whitespace <- function(text, add_prefix = TRUE) {
  .escape_whitespace(text, add_prefix)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Train a BPE model: merge the most frequent adjacent pair greedily
#' @param corpus See Usage.
#' @param vocab_size See Usage.
#' @param add_prefix See Usage.
#' @export
train_bpe <- function(corpus, vocab_size, add_prefix = TRUE) {
  V <- as.integer(vocab_size)
  if (V < 1L) stop("sentpc: vocab_size must be at least 1")
  words <- list()
  for (line in corpus) {
    for (w in .units(.escape_whitespace(line, add_prefix))) {
      key <- w
      if (is.null(words[[key]])) words[[key]] <- 0L
      words[[key]] <- words[[key]] + 1L
    }
  }
  if (length(words) == 0L) {
    stop("sentpc: the corpus produced no tokens")
  }
  alphabet <- sort(unique(unlist(strsplit(unlist(names(words)), ""))))
  merges <- list()
  vocab <- alphabet
  while (length(vocab) < V) {
    pairs <- list()
    for (key in names(words)) {
      w <- strsplit(key, "")[[1]]
      f <- words[[key]]
      if (length(w) < 2L) next
      for (i in seq_len(length(w) - 1L)) {
        pk <- paste0(w[i], "|", w[i + 1L])
        if (is.null(pairs[[pk]])) pairs[[pk]] <- 0L
        pairs[[pk]] <- pairs[[pk]] + f
      }
    }
    if (length(pairs) == 0L) break
    cnts <- unlist(pairs)
    best_key <- names(which.max(cnts))
    best <- strsplit(best_key, "|", fixed = TRUE)[[1]]
    merges[[length(merges) + 1L]] <- best
    new_token <- paste0(best[1L], best[2L])
    vocab <- c(vocab, new_token)
    nw <- list()
    for (key in names(words)) {
      w <- strsplit(key, "")[[1]]
      f <- words[[key]]
      out <- character(0)
      i <- 1L
      while (i <= length(w)) {
        if (i < length(w) && w[i] == best[1L] && w[i + 1L] == best[2L]) {
          out <- c(out, paste0(w[i], w[i + 1L]))
          i <- i + 2L
        } else {
          out <- c(out, w[i])
          i <- i + 1L
        }
      }
      nk <- paste0(out, collapse = "")
      if (is.null(nw[[nk]])) nw[[nk]] <- 0L
      nw[[nk]] <- nw[[nk]] + f
    }
    words <- nw
  }
  list(merges = merges, vocab = sort(vocab),
       vocab_size = length(vocab), requested = V,
       algorithm = "bpe",
       note = paste("greedy and deterministic -- the merge list fixes ",
                    "every later segmentation"))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Invert the whitespace escape
#' @param text See Usage.
#' @param strip_prefix See Usage.
#' @export
unescape_whitespace <- function(text, strip_prefix = TRUE) {
  .unescape_whitespace(text, strip_prefix)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Unigram LM segmentation by Viterbi over the split lattice
#' @param text See Usage.
#' @param piece_logp See Usage.
#' @param add_prefix See Usage.
#' @export
viterbi_segment <- function(text, piece_logp, add_prefix = TRUE) {
  s <- .escape_whitespace(text, add_prefix)
  n <- nchar(s)
  if (nchar(s) == 0L) return(list(pieces = character(0), logp = 0))
  if (length(piece_logp) == 0L) maxlen <- 1L
  else maxlen <- max(nchar(names(piece_logp)))
  best <- rep(-Inf, n + 1L)
  back <- vector("list", n + 1L)
  best[1L] <- 0
  for (i in 2:(n + 1L)) {
    for (L in seq_len(min(maxlen, i - 1L))) {
      piece <- substr(s, i - L, i - 1L)
      lp <- piece_logp[[piece]]
      if (is.null(lp)) next
      if (best[i - L] + lp > best[i]) {
        best[i] <- best[i - L] + lp
        back[[i]] <- c(i - L, piece)
      }
    }
  }
  if (is.infinite(best[n + 1L]) && best[n + 1L] < 0) {
    stop("sentpc: no segmentation covers the input -- the piece set ",
         "must include every character")
  }
  pieces <- character(0)
  i <- n + 1L
  while (i > 1L) {
    st <- back[[i]]
    pieces <- c(st[2L], pieces)
    i <- as.integer(st[1L])
  }
  list(pieces = pieces, logp = best[n + 1L],
       n_pieces = length(pieces),
       algorithm = "unigram (Viterbi)")
}

# -- restored: morie-only objects kept through the rmorie sync --
.SPACE <- "\u2581"
