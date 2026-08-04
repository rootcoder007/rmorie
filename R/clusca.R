# SPDX-License-Identifier: AGPL-3.0-or-later
#' Local clustering coefficient
#'
#' Watts and Strogatz (1998), Collective dynamics of 'small-world'
#' networks, Nature 393, 440-442: for a vertex v with k_v neighbours, C_v
#' is the number of edges among those neighbours over k_v(k_v - 1)/2, and
#' the network coefficient is the average of C_v.  The Nature paper is
#' paywalled; the definition is quoted in its standard published form.
#' The global transitivity of Barrat and Weigt (2000) -- three times the
#' triangles over the connected triples -- is a DIFFERENT quantity and is
#' returned separately, because the two are routinely confused and
#' disagree on any graph with an uneven degree distribution.
#'
#' @param y the adjacency matrix (first slot, for signature stability).
#' @param A the adjacency matrix; wins over y.
#' @param node vertex whose C_v is returned as estimate (zero-based).
#' @return list: estimate, local, average, transitivity, n, method.
#' @keywords internal
#' @examples
#' A <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' Clustcoef(A)$transitivity
#' @export
Clustcoef <- function(y, A = NULL, node = NULL) {
  W <- .s03mat(if (!is.null(A)) A else y)
  n <- nrow(W)
  loc <- numeric(n); tri <- 0; trip <- 0
  for (v in seq_len(n)) {
    nb <- integer(0)
    for (u in seq_len(n)) if (u != v && W[v, u] != 0) nb <- c(nb, u)
    kv <- length(nb)
    links <- 0
    if (kv > 1L) for (a in seq_len(kv - 1L)) for (b in seq(a + 1L, kv)) {
      if (W[nb[a], nb[b]] != 0) links <- links + 1
    }
    tri <- tri + links
    trip <- trip + kv * (kv - 1) / 2
    loc[v] <- if (kv > 1L) 2 * links / (kv * (kv - 1)) else 0
  }
  keep <- logical(n)
  for (v in seq_len(n)) {
    kv <- 0L
    for (u in seq_len(n)) if (u != v && W[v, u] != 0) kv <- kv + 1L
    keep[v] <- kv > 1L
  }
  avg <- if (any(keep)) .s03mean(loc[keep]) else NaN
  trans <- if (trip > 0) tri / trip else NaN
  est <- if (!is.null(node)) loc[as.integer(node) + 1L] else avg
  list(estimate = est, local = loc, average = avg, transitivity = trans,
       n = n,
       method = "Watts-Strogatz local clustering coefficient, with global transitivity")
}
