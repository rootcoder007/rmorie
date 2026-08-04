# SPDX-License-Identifier: AGPL-3.0-or-later
#' Amalgamate a subset of parts into one, keeping the rest.
#'
#' The amalgamated part is appended last; the retained parts keep their
#' original order. \code{parts} is one-based.
#'
#' Formula: amalg(x; S) = C( (x_i : i not in S), sum_{j in S} x_j )
#'
#' @param x Strictly positive vector of parts.
#' @param parts One-based indices of the parts summed together.
#' @param total Constant the result sums to.
#' @return List with \code{composition}, \code{amalgamated}, \code{parts},
#'   \code{kept}, \code{D}.
#' @references Aitchison (1986), The Statistical Analysis of Compositional
#'   Data, Chapter 2, which defines amalgamation and notes that it does
#'   not preserve the log-ratio structure.
#' @export
Amalgam <- function(x, parts, total = 1) {
  x <- .t1_vec(x)
  if (any(x <= 0)) stop("compositions must be strictly positive")
  D <- length(x)
  idx <- as.integer(parts)
  if (length(idx) < 2L) stop("an amalgamation needs at least two parts")
  if (any(idx < 1L | idx > D)) stop("parts must be one-based indices in 1..D")
  keep <- setdiff(seq_len(D), idx)
  amal <- sum(x[idx])
  raw <- c(x[keep], amal)
  k <- as.numeric(total)
  .t1_result(composition = k * raw / sum(raw), amalgamated = amal,
             parts = idx, kept = keep, D = length(raw),
             method = "Amalgamation")
}
