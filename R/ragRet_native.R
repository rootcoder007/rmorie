# Retrieval for RAG: top-k by inner product, and what it costs.
# Sources: Lewis, P. et al. (2020) "Retrieval-Augmented Generation for
# Knowledge-Intensive NLP Tasks", NeurIPS 33, 9459-9474, arXiv
# 2005.11401. Johnson, Douze, Jegou (2019) IEEE TBD 7(3) (IVF index,
# arXiv 1702.08734). Karpukhin et al. (2020) EMNLP (DPR, arXiv
# 2004.04906). Native R mirroring morie.fn.ragRet: same exact and IVF
# search routes, same recall measurement, same RAG-Sequence and
# RAG-Token marginalisations.

.EPS <- 1e-12
.METRICS <- c("inner_product", "cosine")

morie_ragRet_normalise <- function(v) {
  x <- as.numeric(v); n <- sqrt(sum(x^2))
  if (n <= .EPS) stop("ragRet: a zero vector has no direction")
  x / n
}

morie_ragRet_top_k <- function(query, corpus, k.top = 5L,
                               metric = "inner_product") {
  if (!(metric %in% .METRICS))
    stop(paste0("ragRet: metric must be one of ",
                paste(.METRICS, collapse = ", "), ", got ", metric))
  q <- as.numeric(query)
  D <- lapply(corpus, as.numeric)
  if (length(D) == 0L) stop("ragRet: the corpus is empty")
  if (any(lengths(D) != length(q)))
    stop("ragRet: a document has a different width from the query")
  if (metric == "cosine") {
    q <- morie_ragRet_normalise(q)
    D <- lapply(D, morie_ragRet_normalise)
  }
  s <- vapply(seq_along(D), function(j) sum(q * D[[j]]), numeric(1))
  order <- order(-s)
  kk <- min(as.integer(k.top), length(order))
  list(indices = order[seq_len(kk)] - 1L,
       scores = s[order[seq_len(kk)]],
       all_scores = s, metric = metric,
       comparisons = length(D),
       note = "exact, and linear in the corpus -- which is the reason approximate indexes exist")
}

morie_ragRet_ivf_index <- function(corpus, n.cells = 4L, iters = 25L,
                                   seed = 0L) {
  D <- lapply(corpus, as.numeric); n <- length(D)
  c <- as.integer(n.cells)
  if (n < 1L || c < 1L)
    stop("ragRet: need a non-empty corpus and at least one cell")
  if (c > n)
    stop(paste0("ragRet: ", c, " cells for ", n, " vectors"))
  e <- .ghc_rng(as.integer(seed))
  cent <- vector("list", c)
  for (k in seq_len(c)) {
    j <- as.integer(.ghc_unif(e, 1L) * n) + 1L
    cent[[k]] <- D[[j]]
  }
  assign <- integer(n)
  d <- length(D[[1]])
  for (it in seq_len(as.integer(iters))) {
    for (j in seq_len(n)) {
      best <- 1L; bestd <- Inf
      for (tt in seq_len(c)) {
        d2 <- sum((D[[j]] - cent[[tt]])^2)
        if (d2 < bestd) { bestd <- d2; best <- tt }
      }
      assign[j] <- best
    }
    for (tt in seq_len(c)) {
      mem <- which(assign == tt)
      if (length(mem) > 0L) {
        cent[[tt]] <- vapply(seq_len(d), function(a)
          sum(vapply(mem, function(j) D[[j]][a], numeric(1))) / length(mem),
          numeric(1))
      }
    }
  }
  lists <- vector("list", c)
  for (j in seq_len(n)) {
    a <- assign[j]
    if (is.null(lists[[a]])) lists[[a]] <- integer(0)
    lists[[a]] <- c(lists[[a]], j)
  }
  list(centroids = cent, lists = lists, assign = assign - 1L,
       n.cells = c, n = n,
       note = "the inverted file: which vectors live in which cell")
}

morie_ragRet_ivf_search <- function(query, corpus, index, k.top = 5L,
                                    nprobe = 1L, metric = "inner_product") {
  q <- as.numeric(query)
  cent <- index$centroids
  d2 <- vapply(cent, function(ct) sum((q - ct)^2), numeric(1))
  order.cells <- order(d2)
  probe <- head(order.cells, max(1L, as.integer(nprobe)))
  cand <- integer(0)
  for (tt in probe) cand <- c(cand, index$lists[[tt]])
  if (length(cand) == 0L)
    return(list(indices = integer(0), scores = numeric(0),
                comparisons = 0L, probed = probe - 1L,
                note = "the probed cells were empty"))
  sub <- corpus[cand]
  r <- morie_ragRet_top_k(q, sub, min(as.integer(k.top), length(sub)),
                           metric)
  list(indices = cand[r$indices + 1L] - 1L,
       scores = r$scores, comparisons = length(cand),
       probed = probe - 1L, n.cells = index$n.cells,
       fraction.scanned = length(cand) / index$n,
       note = "approximate: the true nearest neighbour may sit in a cell that was not probed")
}

morie_ragRet_recall_at_k <- function(approximate, exact) {
  A <- as.integer(approximate)
  E <- as.integer(exact)
  if (length(E) == 0L) stop("ragRet: the exact result is empty")
  hits <- sum(E %in% A)
  list(recall = hits / length(E), hits = hits, k = length(E),
       missed = E[!(E %in% A)])
}

morie_ragRet_marginalise <- function(doc.scores, token.probs,
                                     mode = "sequence") {
  p <- as.numeric(doc.scores)
  if (length(p) == 0L) stop("ragRet: no retrieved documents")
  if (any(p < 0)) stop("ragRet: the document scores must be non-negative probabilities")
  z <- sum(p)
  if (z <= .EPS) stop("ragRet: the document weights are all zero")
  w <- p / z
  T <- lapply(token.probs, as.numeric)
  if (length(T) != length(w))
    stop(paste0("ragRet: ", length(w), " documents but ",
                length(T), " token distributions"))
  if (mode == "sequence") {
    seq <- vapply(seq_along(w), function(d)
      exp(sum(log(pmax(T[[d]], .EPS)))), numeric(1))
    return(list(estimate = sum(w * seq),
                probability = sum(w * seq),
                per_document = seq, weights = w, mode = "sequence",
                method = "RAG-Sequence marginalisation; Lewis et al. (2020)",
                note = "ONE document conditions the whole output"))
  }
  if (mode == "token") {
    n.tok <- length(T[[1]])
    if (any(lengths(T) != n.tok))
      stop("ragRet: the token distributions differ in length")
    per.tok <- vapply(seq_len(n.tok), function(t)
      sum(vapply(seq_along(w), function(d) w[d] * T[[d]][t],
                 numeric(1))), numeric(1))
    return(list(estimate = exp(sum(log(pmax(per.tok, .EPS)))),
                probability = exp(sum(log(pmax(per.tok, .EPS)))),
                per_token = per.tok, weights = w, mode = "token",
                method = "RAG-Token marginalisation; Lewis et al. (2020)",
                note = "each token may draw on a DIFFERENT document, so facts can be composed across passages"))
  }
  stop(paste0("ragRet: mode must be sequence or token, got ", mode))
}

# house entry point: the package exports one morie_<module>
morie_ragRet <- morie_ragRet_normalise
