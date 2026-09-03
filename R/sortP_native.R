# morie.fn -- function file (rootcoder007/morie)
# The under-stated problem in graph classification is how to read
# vertices in a MEANINGFUL AND CONSISTENT order so an ordinary
# network can be trained on graphs. Summing is invariant and forgets
# who contributed what; SortPooling ARRANGES vertices instead,
# sorting by the last convolution channel -- a continuous WL colour,
# so the order comes from the GRAPH, not the input file -- then
# truncates or pads to a fixed k. Relabel the vertices and the
# output must not move. k is chosen for coverage of the size
# distribution.

.sortP_eps <- 1e-12

#' wl_colours
#'
#' A step of the sortP_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adj A vector; indexed elementwise.
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param rounds Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2}.
#' @param initial Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @return The value of \code{c}, as built in the body.
#' @export
wl_colours <- function(adj, n, rounds = 2, initial = NULL) {
  n <- as.integer(n)
  c <- if (is.null(initial)) rep(1.0, n) else as.numeric(initial)
  if (length(c) != n)
    stop(sprintf("sortP: %d initial colours for %d vertices",
                 length(c), n))
  nb_of <- function(v) {
    a <- adj[[as.character(v)]]
    if (is.null(a)) integer(0) else sort(unique(setdiff(a, v)))
  }
  for (r in seq_len(as.integer(rounds))) {
    nc <- vapply(seq_len(n) - 1L, function(v) {
      nb <- nb_of(v)
      c[v + 1L] + sum(c[nb + 1L])
    }, numeric(1))
    m <- mean(nc)
    c <- if (m > .sortP_eps) nc / m else nc
  }
  c
}

#' sort_pooling
#'
#' A step of the sortP_native implementation. Called by \code{order_is_graph_determined}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param features A matrix; the body checks with \code{is.matrix}.
#' @param k_keep Coerced to integer by the body, with \code{as.integer}.
#' @param sort_channel Coerced to integer by the body, with \code{as.integer}. Defaults
#' to \code{-1}.
#' @return A list with \code{pooled}, \code{order}, \code{n_truncated}, \code{n_padded},
#' \code{k}, \code{sort_channel}, \code{note}.
#' @export
sort_pooling <- function(features, k_keep, sort_channel = -1) {
  X <- if (is.matrix(features)) features else
    do.call(rbind, lapply(features, function(r) as.numeric(r)))
  n <- nrow(X)
  d <- ncol(X)
  kk <- as.integer(k_keep)
  if (kk < 1L) stop("sortP: k must be at least 1")
  ch <- as.integer(sort_channel)
  ch <- ((ch %% d) + d) %% d + 1L
  ord <- order(-X[, ch], seq_len(n))
  kept <- ord[seq_len(min(kk, n))]
  out <- X[kept, , drop = FALSE]
  if (nrow(out) < kk) {
    pad <- matrix(0.0, nrow = kk - nrow(out), ncol = d)
    out <- rbind(out, pad)
  }
  list(pooled = out, order = as.integer(kept),
       n_truncated = max(0L, n - kk),
       n_padded = max(0L, kk - n), k = kk,
       sort_channel = ch,
       note = "a fixed-size, ordered representation, so an ordinary CNN can read it")
}

#' choose_k
#'
#' A step of the sortP_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param graph_sizes Coerced to integer by the body, with \code{as.integer}.
#' @param coverage Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.6}.
#' @return A list with \code{k}, \code{coverage}, \code{fraction_untruncated}, \code{note}.
#' @export
choose_k <- function(graph_sizes, coverage = 0.6) {
  s <- sort(as.integer(graph_sizes))
  c <- as.numeric(coverage)
  if (c <= 0 || c > 1)
    stop(sprintf("sortP: the coverage must lie in (0,1], got %r", coverage))
  if (length(s) == 0L) stop("sortP: no graph sizes given")
  idx <- min(length(s) - 1L, as.integer(ceiling(c * length(s))) - 1L)
  idx <- max(idx, 0L)
  kk <- s[idx + 1L]
  list(k = kk, coverage = c,
       fraction_untruncated = sum(s <= kk) / length(s),
       note = "k is the coverage quantile of the size distribution")
}

#' order_is_graph_determined
#'
#' A step of the sortP_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param features A matrix; the body checks with \code{is.matrix}.
#' @param adj Accepted by the signature and not used anywhere in the body.
#' @param perm A vector; indexed elementwise.
#' @param k_keep Passed to \code{sort_pooling}.
#' @param tol Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-09}.
#' @return A list with \code{max_deviation}, \code{invariant}, \code{note}.
#' @export
order_is_graph_determined <- function(features, adj, perm, k_keep,
                                      tol = 1e-9) {
  X <- if (is.matrix(features)) features else
    do.call(rbind, lapply(features, function(r) as.numeric(r)))
  n <- nrow(X)
  base <- sort_pooling(X, k_keep)$pooled
  inv <- integer(n)
  for (i in seq_len(n)) inv[perm[i]] <- i
  Xp <- X[inv, , drop = FALSE]
  other <- sort_pooling(Xp, k_keep)$pooled
  dev <- max(abs(base - other))
  list(max_deviation = dev, invariant = dev < as.numeric(tol),
       note = "the sort key must be a function of the GRAPH, not of the vertex listing")
}

#' .sortP_cheatsheet
#'
#' A step of the sortP_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .sortP_cheatsheet()
#' res
.sortP_cheatsheet <- function() {
  paste("sortP: the under-stated problem in graph classification is",
        "how to read vertices in a MEANINGFUL AND CONSISTENT",
        "order so an ordinary network can be trained on graphs.",
        "Summing is invariant and forgets who contributed what;",
        "SortPooling ARRANGES vertices instead, sorting by the",
        "last convolution channel -- a continuous WL colour, so",
        "the order comes from the GRAPH, not the input file --",
        "then truncates or pads to a fixed k. Relabel the vertices",
        "and the output must not move. k is chosen for coverage of",
        "the size distribution.")
}

sortpooling <- sort_pooling
sortpool <- sort_pooling

# house entry point: the package exports one morie_<module>
morie_sortP <- sort_pooling
