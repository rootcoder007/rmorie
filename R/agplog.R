# SPDX-License-Identifier: AGPL-3.0-or-later
#' Structured play log for one game
#'
#' No equation here either: Silver et al. (2018), arXiv:1712.01815
#' (FETCHED), reports games and outcomes but specifies no log format, so
#' nothing is attributed to it.  What this provides is a canonical,
#' order-stable record -- move index, action, root visit count, value
#' estimate -- with the statistics a log is read for: length, total and
#' mean value, and the entropy of the realised move distribution.  The
#' digest is the same Rabin-Karp hash as Replaypack.  Writing to disk is
#' opt-in.
#'
#' @param game the actions played, in order.
#' @param path optional file to write the canonical text to.
#' @param values value estimate at each move.
#' @param visits root visit count of the played move.
#' @return list: estimate, digest, moves, total_value, mean_value,
#'   action_entropy, written, method.
#' @keywords internal
#' @examples
#' Gamelog(c(0, 1, 1, 0))$action_entropy
#' @export
Gamelog <- function(game, path = NULL, values = NULL, visits = NULL) {
  digest1 <- function(text) {
    h <- 0
    if (nchar(text) > 0L) for (cp in utf8ToInt(text)) h <- (131 * h + cp) %% 2147483647
    h
  }
  acts <- .s03vec(game)
  n <- length(acts)
  v <- if (!is.null(values)) .s03vec(values) else numeric(n)
  vis <- if (!is.null(visits)) .s03vec(visits) else numeric(n)
  lines <- character(0)
  for (i in seq_len(n)) {
    lines <- c(lines, sprintf("%d,%.17g,%.17g,%.17g", i - 1L, acts[i], vis[i], v[i]))
  }
  text <- paste(lines, collapse = "\n")
  written <- FALSE
  if (!is.null(path)) { writeLines(text, path, sep = ""); written <- TRUE }
  lv <- sort(unique(acts))
  h <- 0
  for (key in lv) {
    q <- if (n) sum(acts == key) / n else 0
    if (q > 0) h <- h - q * log(q)
  }
  tot <- 0
  for (x in v) tot <- tot + x
  list(estimate = digest1(text), digest = digest1(text), moves = n,
       total_value = tot, mean_value = if (n) tot / n else NaN,
       action_entropy = h, written = written,
       method = "Canonical game log with a Rabin-Karp digest")
}
