# SPDX-License-Identifier: AGPL-3.0-or-later
#' Pointwise mutual information, the association ratio
#'
#' I(x, y) = log2(P(x, y) / (P(x) P(y))) with the probabilities estimated by
#' counting co-occurrences.  Source consulted: Church and Hanks (1990), Word
#' Association Norms, Mutual Information, and Lexicography, Computational
#' Linguistics 16(1), 22-29 (ACL Anthology J90-1003).
#'
#' @param x integer codes of the first pair member.
#' @param y integer codes of the second pair member, same length as x.
#' @param window optional window width; log2(window - 1) is subtracted.
#' @return list: estimate, mi_bits, pmimax, pmimin, pmimean, pmi, npairs, n, method.
#' @keywords internal
#' @examples
#' pmiwd(c(0, 0, 1, 1), c(0, 0, 1, 1))
#' @export
pmiwd <- function(x, y, window = NULL) {
  xs <- as.numeric(x); ys <- as.numeric(y)
  n <- min(length(xs), length(ys))
  xs <- xs[seq_len(n)]; ys <- ys[seq_len(n)]
  fx <- table(xs); fy <- table(ys)
  key <- paste(xs, ys, sep = "|")
  fxy <- table(key)
  adj <- if (!is.null(window) && window > 1) log2(window - 1) else 0
  nn <- as.numeric(n)
  nm <- sort(names(fxy))
  pmis <- numeric(length(nm)); mi <- 0
  for (i in seq_along(nm)) {
    parts <- strsplit(nm[i], "|", fixed = TRUE)[[1]]
    pxy <- as.numeric(fxy[[nm[i]]]) / nn
    px <- as.numeric(fx[[parts[1]]]) / nn
    py <- as.numeric(fy[[parts[2]]]) / nn
    v <- log2(pxy / (px * py)) - adj
    pmis[i] <- v
    mi <- mi + pxy * v
  }
  list(estimate = as.numeric(mi), mi_bits = as.numeric(mi),
       pmimax = max(pmis), pmimin = min(pmis), pmimean = mean(pmis),
       pmi = pmis, npairs = length(pmis), n = as.integer(n),
       method = "Pointwise mutual information / association ratio (Church & Hanks 1990)")
}

# CANONICAL TEST
# r <- pmiwd(c(0, 0, 1, 1), c(0, 0, 1, 1)); stopifnot(abs(r$estimate - 1) < 1e-12)

#' @rdname pmiwd
#' @keywords internal
#' @export
morie_pointwise_mutual_info <- pmiwd
