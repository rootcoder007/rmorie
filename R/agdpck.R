# SPDX-License-Identifier: AGPL-3.0-or-later
#' Canonical serialisation of AlphaZero training data
#'
#' No equation here: Silver et al. (2018), arXiv:1712.01815 (FETCHED),
#' says only that the (s, pi, z) triples are stored and sampled uniformly
#' from the most recent games.  What this contributes is a canonical
#' encoding, so the same buffer always yields the same bytes and the same
#' digest whichever language wrote it.  The digest is a Rabin-Karp
#' polynomial hash, h <- (131 h + c) mod (2^31 - 1), exact in double
#' precision because every intermediate stays below 2^53.  It is not a
#' cryptographic hash and is not presented as one.  Writing to disk is
#' opt-in; by default nothing touches the filesystem.
#'
#' @param replay_buffer rows of (s, pi..., z), or any nested numeric.
#' @param path optional file to write the canonical text to.
#' @return list: estimate, digest, n_rows, n_values, text_len, written,
#'   method.
#' @keywords internal
#' @examples
#' Replaypack(matrix(c(1, 0.5, 0.5, 1), 1, 4))$digest
#' @export
Replaypack <- function(replay_buffer, path = NULL) {
  digest1 <- function(text) {
    h <- 0
    if (nchar(text) > 0L) for (cp in utf8ToInt(text)) h <- (131 * h + cp) %% 2147483647
    h
  }
  rows <- .s03mat(replay_buffer)
  parts <- character(0)
  nv <- 0L
  for (i in seq_len(nrow(rows))) {
    parts <- c(parts, paste(sprintf("%.17g", rows[i, ]), collapse = ","))
    nv <- nv + ncol(rows)
  }
  text <- paste(parts, collapse = "\n")
  written <- FALSE
  if (!is.null(path)) {
    writeLines(text, path, sep = "")
    written <- TRUE
  }
  list(
    estimate = digest1(text), digest = digest1(text), n_rows = nrow(rows),
    n_values = nv, text_len = nchar(text), written = written,
    method = "Canonical (s, pi, z) encoding with a Rabin-Karp digest"
  )
}
