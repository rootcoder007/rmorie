# morie.fn -- function file (rootcoder007/morie)
# Sources:
#   Hotho, A., Jaschke, R., Schmitz, C. & Stumme, G. (2006) "Information
#   Retrieval in Folksonomies: Search and Ranking", The Semantic Web:
#   Research and Applications (ESWC 2006), LNCS 4011, 411-426,
#   doi:10.1007/11762256_31.
#   Brin, S. & Page, L. (1998) "The anatomy of a large-scale hypertextual
#   Web search engine", Computer Networks and ISDN Systems 30(1-7),
#   107-117, doi:10.1016/S0169-7552(98)00110-X.
#   Haveliwala, T. H. (2002) "Topic-sensitive PageRank", WWW '02,
#   517-526, doi:10.1145/511446.511513.

.EPS <- 1e-12

tripartite_graph <- function(triples) {
  nodes <- character(0)
  edges <- list()
  for (tr in triples) {
    u <- tr[[1]]; t <- tr[[2]]; r <- tr[[3]]
    nu <- paste0("u:", u); nt <- paste0("t:", t); nr <- paste0("r:", r)
    nodes <- c(nodes, nu, nt, nr)
    pairs <- list(c(nu, nt), c(nt, nr), c(nu, nr))
    for (pr in pairs) {
      a <- pr[1]; b <- pr[2]
      key <- paste(a, b, sep = "\r")
      if (is.null(edges[[key]])) edges[[key]] <- 0
      edges[[key]] <- edges[[key]] + 1
      key2 <- paste(b, a, sep = "\r")
      if (is.null(edges[[key2]])) edges[[key2]] <- 0
      edges[[key2]] <- edges[[key2]] + 1
    }
  }
  nodes <- sort(unique(nodes))
  adj <- list()
  for (k in names(edges)) {
    parts <- strsplit(k, "\r", fixed = TRUE)[[1]]
    a <- parts[1]; b <- parts[2]
    if (is.null(adj[[a]])) adj[[a]] <- list()
    adj[[a]][[b]] <- as.numeric(edges[[k]])
  }
  n_triples <- length(triples)
  list(adjacency = adj, nodes = nodes, n_nodes = length(nodes),
       n_triples = n_triples,
       note = paste("a triadic hyperedge flattened to three",
                    "undirected edges, so weight flows both ways"))
}

preference_vector <- function(nodes, focus, weight = 0.9) {
  N <- as.list(nodes)
  F <- intersect(focus, N)
  if (length(F) == 0) {
    stop("tagRC: none of the focus nodes are in the graph")
  }
  w <- as.numeric(weight)
  if (!(w > 0 && w < 1)) {
    stop("tagRC: the focus weight must lie in (0,1)")
  }
  rest <- length(N) - length(F)
  p <- list()
  for (n in N) {
    if (n %in% F) {
      p[[n]] <- w / length(F)
    } else {
      p[[n]] <- if (rest > 0) (1.0 - w) / rest else 0.0
    }
  }
  mass <- 0.0
  for (v in p) mass <- mass + v
  list(p = p, focus = F, mass = mass)
}

adapted_pagerank <- function(adjacency, nodes, p = NULL, d = 0.7,
                             iters = 200, tol = 1e-12) {
  N <- as.list(nodes)
  n <- length(N)
  if (n == 0) {
    stop("tagRC: the graph is empty")
  }
  if (!(as.numeric(d) > 0 && as.numeric(d) < 1)) {
    stop("tagRC: the damping factor must lie in (0,1)")
  }
  pref <- list()
  if (is.null(p)) {
    for (u in N) pref[[u]] <- 1.0 / n
  } else {
    for (u in N) {
      v <- p[[u]]
      pref[[u]] <- if (is.null(v)) 0.0 else v
    }
  }
  w <- list()
  for (u in N) w[[u]] <- 1.0 / n
  deg <- list()
  for (u in N) {
    nb <- adjacency[[u]]
    s <- 0.0
    if (!is.null(nb)) for (vv in nb) s <- s + as.numeric(vv)
    deg[[u]] <- s
  }
  dd <- as.numeric(d)
  ttol <- as.numeric(tol)
  niters <- as.integer(iters)
  for (i in seq_len(niters)) {
    nxt <- list()
    for (u in N) {
      s <- 0.0
      for (v in N) {
        adjv <- adjacency[[v]]
        if (is.null(adjv)) next
        a <- adjv[[u]]
        if (is.null(a)) next
        a <- as.numeric(a)
        if (a > 0.0 && deg[[v]] > .EPS) {
          s <- s + w[[v]] * a / deg[[v]]
        }
      }
      nxt[[u]] <- dd * s + (1.0 - dd) * pref[[u]]
    }
    tot <- 0.0
    for (u in N) tot <- tot + nxt[[u]]
    if (tot == 0) tot <- 1.0
    for (u in N) nxt[[u]] <- nxt[[u]] / tot
    delta <- 0.0
    for (u in N) {
      diff <- abs(nxt[[u]] - w[[u]])
      if (diff > delta) delta <- diff
    }
    w <- nxt
    if (delta < ttol) break
  }
  ranking <- N[order(sapply(N, function(u) -w[[u]]))]
  list(w = w, ranking = ranking)
}

folkrank <- function(triples, focus, d = 0.7, weight = 0.9, iters = 200) {
  g <- tripartite_graph(triples)
  N <- g$nodes
  pv <- preference_vector(N, focus, weight)
  with_p <- adapted_pagerank(g$adjacency, N, pv$p, d, iters)
  without <- adapted_pagerank(g$adjacency, N, NULL, d, iters)
  diff <- list()
  for (u in N) diff[[u]] <- with_p$w[[u]] - without$w[[u]]
  order <- N[order(sapply(N, function(u) -diff[[u]]))]
  list(estimate = order, ranking = order, difference = diff,
       with_preference = with_p$w,
       without_preference = without$w,
       undifferenced_ranking = with_p$ranking,
       baseline_ranking = without$ranking,
       focus = pv$focus, n_nodes = g$n_nodes,
       method = paste("FolkRank differential ranking; Hotho, Jaschke,",
                      "Schmitz & Stumme (2006)"),
       note = paste("PageRank cannot be applied directly -- undirected",
                    "triadic hyperedges, not directed binary edges --",
                    "and on this graph degree dominates, which is what",
                    "the difference removes"))
}

folkrank_search <- folkrank
tag_aware_rec <- folkrank
tagawarerec <- folkrank

.tagRC_cheatsheet <- function() {
  paste("tagRC: a folksonomy is (user, tag, resource) TRIPLES, so",
        "the structure is an undirected triadic HYPEREDGE, not a",
        "directed binary link -- PageRank cannot be applied directly.",
        "Flatten each triple to three undirected edges; because they",
        "are undirected, DEGREE dominates the stationary distribution",
        "and a topic preference vector still returns the globally",
        "popular nodes. FOLKRANK is the difference of two runs, WITH",
        "and WITHOUT the preference vector: whatever is popular",
        "regardless of the query cancels, and what the preference",
        "actually pulled up survives. Keep ||p||_1 = ||w||_1.")
}

morie_tagRC <- function(triples, focus, d = 0.7, weight = 0.9,
                        iters = 200) {
  folkrank(triples, focus, d = d, weight = weight, iters = iters)
}
