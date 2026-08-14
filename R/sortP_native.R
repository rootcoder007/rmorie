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

sort_pooling <- function(features, k_keep, sort_channel = -1) {
  X <- if (is.matrix(features)) features else
    do.call(rbind, lapply(features, function(r) as.numeric(r)))
  n <- nrow(X); d <- ncol(X)
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

cheatsheet <- function() {
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
