# SPDX-License-Identifier: AGPL-3.0-or-later
#
# tokenizer.R -- BPE / SentencePiece tokenizer for MORIE's R surface.
#
# R port of src/morie/tokenizer.py. Either an explicit vocab/scores
# list, a GGUF metadata list, or a SentencePiece `.model` file path
# (loaded via reticulate) is required. The pure-R path implements
# greedy longest-match BPE with UTF-8 byte fallback identical to the
# Python implementation.

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Construct a MORIE tokenizer
#'
#' Returns an environment holding tokenizer state (vocab, scores,
#' merges, special-token IDs, optional SentencePiece processor).
#'
#' @param vocab Character vector of vocabulary tokens. Optional.
#' @param scores Numeric vector of vocab scores. Optional.
#' @param merges Character vector of "a b" BPE merge lines. Optional.
#' @param bos_id Beginning-of-sequence integer ID. Default 1.
#' @param eos_id End-of-sequence integer ID. Default 2.
#' @param gguf_metadata Optional named list from a GGUF file (keys
#'   `tokenizer.ggml.tokens`, `.scores`, `.merges`, `.bos_token_id`,
#'   `.eos_token_id`).
#' @param sp_model_path Optional `.model` file path; loads SentencePiece
#'   via `reticulate::import("sentencepiece")`.
#' @return Tokenizer environment of class `morie_tokenizer`.
#' @export
#' @examples
#' vocab <- c("<unk>", "<s>", "</s>", "\u2581", "hi", "world")
#' morie_tokenizer_new(vocab = vocab, bos_id = 1L, eos_id = 2L)
morie_tokenizer_new <- function(vocab = NULL, scores = NULL, merges = NULL,
                                bos_id = 1L, eos_id = 2L,
                                gguf_metadata = NULL,
                                sp_model_path = NULL) {
  self <- new.env(parent = emptyenv())
  self$vocab <- character()
  self$scores <- numeric()
  self$merges <- list()
  self$token_to_id <- new.env(parent = emptyenv())
  self$bos_id <- as.integer(bos_id)
  self$eos_id <- as.integer(eos_id)
  self$sp <- NULL

  if (!is.null(sp_model_path)) {
    if (!requireNamespace("reticulate", quietly = TRUE)) {
      stop("SentencePiece path requires the 'reticulate' package.")
    }
    spm <- reticulate::import("sentencepiece", convert = TRUE)
    self$sp <- spm$SentencePieceProcessor()
    self$sp$Load(as.character(sp_model_path))
    n <- self$sp$GetPieceSize()
    self$vocab <- vapply(seq_len(n) - 1L,
                         function(i) self$sp$IdToPiece(as.integer(i)),
                         character(1))
    self$bos_id <- as.integer(self$sp$bos_id())
    self$eos_id <- as.integer(self$sp$eos_id())
  } else if (!is.null(gguf_metadata)) {
    tokens <- gguf_metadata[["tokenizer.ggml.tokens"]]
    if (is.null(tokens) || !length(tokens)) {
      stop("GGUF metadata has no tokenizer.ggml.tokens entry.")
    }
    self$vocab <- vapply(tokens,
      function(t) if (is.raw(t)) rawToChar(t) else as.character(t),
      character(1))
    self$scores <- as.numeric(
      gguf_metadata[["tokenizer.ggml.scores"]] %||% rep(0, length(self$vocab)))
    self$bos_id <- as.integer(
      gguf_metadata[["tokenizer.ggml.bos_token_id"]] %||% 1L)
    self$eos_id <- as.integer(
      gguf_metadata[["tokenizer.ggml.eos_token_id"]] %||% 2L)
    merges_raw <- gguf_metadata[["tokenizer.ggml.merges"]] %||% character()
    for (m in merges_raw) {
      parts <- strsplit(as.character(m), " ", fixed = TRUE)[[1]]
      if (length(parts) == 2L) {
        self$merges[[length(self$merges) + 1L]] <- parts
      }
    }
  } else if (!is.null(vocab)) {
    self$vocab <- as.character(vocab)
    self$scores <- as.numeric(scores %||% rep(0, length(vocab)))
    if (!is.null(merges)) {
      for (m in merges) {
        parts <- strsplit(as.character(m), " ", fixed = TRUE)[[1]]
        if (length(parts) == 2L) {
          self$merges[[length(self$merges) + 1L]] <- parts
        }
      }
    }
  } else {
    stop("Provide vocab, gguf_metadata, or sp_model_path.")
  }

  for (i in seq_along(self$vocab)) {
    assign(self$vocab[i], i - 1L, envir = self$token_to_id)
  }
  class(self) <- c("morie_tokenizer", "environment")
  self
}

#' Vocabulary size
#' @param tok Tokenizer environment.
#' @return Integer scalar.
#' @export
#' @examples
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' morie_tokenizer_vocab_size(D)
morie_tokenizer_vocab_size <- function(tok) {
  if (!is.null(tok$sp)) tok$sp$GetPieceSize() else length(tok$vocab)
}

#' Encode text to a vector of token IDs
#'
#' SentencePiece when loaded; otherwise greedy longest-match BPE with
#' UTF-8 byte fallback (`<0xHH>` tokens), matching the Python reference.
#'
#' @param tok Tokenizer environment.
#' @param text Character scalar.
#' @param add_bos Prepend BOS ID. Default TRUE.
#' @return Integer vector of token IDs.
#' @export
#' @examples
#' vocab <- c("<unk>", "<s>", "</s>", "\u2581", "hi", "world")
#' tok <- morie_tokenizer_new(vocab = vocab, bos_id = 1L, eos_id = 2L)
#' morie_tokenizer_encode(tok, "hi")
morie_tokenizer_encode <- function(tok, text, add_bos = TRUE) {
  text <- as.character(text)
  if (!is.null(tok$sp)) {
    ids <- as.integer(tok$sp$Encode(text))
    if (add_bos && (length(ids) == 0L || ids[1] != tok$bos_id)) {
      ids <- c(tok$bos_id, ids)
    }
    return(ids)
  }
  toks <- .morie_tokenizer_bpe_encode(tok, text)
  ids <- vapply(toks,
    function(t) {
      v <- mget(t, envir = tok$token_to_id, ifnotfound = list(NULL))[[1]]
      if (is.null(v)) 0L else as.integer(v)
    },
    integer(1))
  if (add_bos) ids <- c(tok$bos_id, ids)
  ids
}

#' Decode token IDs back to text
#' @param tok Tokenizer environment.
#' @param ids Integer vector.
#' @return Character scalar.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' morie_tokenizer_decode(D, V)
morie_tokenizer_decode <- function(tok, ids) {
  ids <- as.integer(ids)
  if (!is.null(tok$sp)) {
    return(tok$sp$Decode(as.list(ids)))
  }
  pieces <- character()
  for (i in ids) {
    if (i >= 0L && i < length(tok$vocab)) {
      piece <- tok$vocab[i + 1L]
      piece <- gsub("\u2581", " ", piece, fixed = TRUE)
      if (startsWith(piece, "<0x") && endsWith(piece, ">")) {
        hex <- substr(piece, 4L, nchar(piece) - 1L)
        b <- suppressWarnings(strtoi(hex, base = 16L))
        if (!is.na(b)) piece <- rawToChar(as.raw(b))
      }
      pieces <- c(pieces, piece)
    }
  }
  out <- paste0(pieces, collapse = "")
  if (startsWith(out, " ")) out <- substr(out, 2L, nchar(out))
  out
}

#' .morie_tokenizer_bpe_encode
#'
#' A step of the tokenizer implementation. Called by \code{morie_tokenizer_encode}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param tok A list; the body reads \code{$token_to_id} from it.
#' @param text A vector; its length is taken.
#' @return The value of \code{tokens}, as built in the body.
#' @export
.morie_tokenizer_bpe_encode <- function(tok, text) {
  text <- paste0("\u2581", gsub(" ", "\u2581", text, fixed = TRUE))
  tokens <- character()
  i <- 1L
  n <- nchar(text)
  while (i <= n) {
    best <- NULL
    best_len <- 0L
    max_len <- min(32L, n - i + 1L)
    for (len in seq.int(max_len, 1L, by = -1L)) {
      cand <- substr(text, i, i + len - 1L)
      if (exists(cand, envir = tok$token_to_id, inherits = FALSE)) {
        best <- cand
        best_len <- len
        break
      }
    }
    if (!is.null(best)) {
      tokens <- c(tokens, best)
      i <- i + best_len
    } else {
      char_bytes <- charToRaw(substr(text, i, i))
      for (b in char_bytes) {
        tokens <- c(tokens, sprintf("<0x%02X>", as.integer(b)))
      }
      i <- i + 1L
    }
  }
  tokens
}

#' Print method for morie tokenizer
#' @param x Tokenizer.
#' @param ... Unused.
#' @return Invisibly `x`.
#' @export
#' @examples
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' rmorie:::print.morie_tokenizer(D)
print.morie_tokenizer <- function(x, ...) {
  src <- if (!is.null(x$sp)) "sentencepiece" else "gguf/vocab"
  cat(sprintf("morie_tokenizer(vocab_size=%d, source=%s)\n",
              morie_tokenizer_vocab_size(x), src))
  invisible(x)
}
