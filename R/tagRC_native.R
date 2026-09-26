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

.tagRC_EPS <- 1e-12

#' tripartite_graph
#'
#' A step of the tagRC_native implementation. Called by \code{folkrank}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param triples A vector; its length is taken.
#' @return A list with \code{adjacency}, \code{nodes}, \code{n_nodes}, \code{n_triples},
#' \code{note}.
#' @export
#' @examples
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' tripartite_graph(D)
#' @keywords internal
tripartite_graph <- function(triples) {
  nodes <- character(0)
  edges <- list()
  for (tr in triples) {
    u <- tr[[1]]
    t <- tr[[2]]
    r <- tr[[3]]
    nu <- paste0("u:", u)
    nt <- paste0("t:", t)
    nr <- paste0("r:", r)
    nodes <- c(nodes, nu, nt, nr)
    pairs <- list(c(nu, nt), c(nt, nr), c(nu, nr))
    for (pr in pairs) {
      a <- pr[1]
      b <- pr[2]
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
    a <- parts[1]
    b <- parts[2]
    if (is.null(adj[[a]])) adj[[a]] <- list()
    adj[[a]][[b]] <- as.numeric(edges[[k]])
  }
  n_triples <- length(triples)
  list(adjacency = adj, nodes = nodes, n_nodes = length(nodes),
       n_triples = n_triples,
       note = paste("a triadic hyperedge flattened to three",
                    "undirected edges, so weight flows both ways"))
}

#' preference_vector
#'
#' A step of the tagRC_native implementation. Called by \code{folkrank}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param nodes Coerced to list by the body, with \code{as.list}.
#' @param focus The body requires: tagRC: none of the focus nodes are in the graph.
#' @param weight Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.9}.
#' @return A list with \code{p}, \code{focus}, \code{mass}.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' preference_vector(V, V)
#' @keywords internal
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

#' adapted_pagerank
#'
#' A step of the tagRC_native implementation. Called by \code{folkrank}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adjacency A vector; indexed elementwise.
#' @param nodes Coerced to list by the body, with \code{as.list}.
#' @param p Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @param d Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.7}.
#' @param iters Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.
#' @param tol Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-12}.
#' @return A list with \code{w}, \code{ranking}.
#' @export
#' @examples
#' D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
#' S <- c("a", "b", "c")
#' adapted_pagerank(D, S)
#' @keywords internal
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
        if (a > 0.0 && deg[[v]] > .tagRC_EPS) {
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

#' folkrank
#'
#' A step of the tagRC_native implementation. Called by \code{morie_tagRC}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param triples Passed to \code{tripartite_graph}.
#' @param focus Passed to \code{preference_vector}.
#' @param d Passed to \code{adapted_pagerank}. Defaults to \code{0.7}.
#' @param weight Passed to \code{preference_vector}. Defaults to \code{0.9}.
#' @param iters Passed to \code{adapted_pagerank}. Defaults to \code{200}.
#' @return A list with \code{estimate}, \code{ranking}, \code{difference},
#' \code{with_preference}, \code{without_preference}, \code{undifferenced_ranking},
#' \code{baseline_ranking}, \code{focus}, \code{n_nodes}, \code{method}, \code{note}.
#' @export
#' @examples
#' triples <- list(c("u1", "t1", "r1"), c("u1", "t2", "r2"),
#'                 c("u2", "t1", "r1"), c("u2", "t3", "r3"))
#' folkrank(triples, focus = "t:t1")
#' @keywords internal
folkrank <- function(triples, focus, d = 0.7, weight = 0.9, iters = 200) {
  g <- tripartite_graph(triples)
  N <- g$nodes
  pv <- preference_vector(N, focus, weight)
  with_p <- adapted_pagerank(g$adjacency, N, pv$p, d, iters)
  # baseline: the fixed point of Eq. (1) with beta = 1 (Hotho et al.
  # 2006, sec. 4.1, step 2) -- on an undirected graph, the degree
  # distribution
  degs <- vapply(N, function(u) {
    nb <- g$adjacency[[u]]
    if (is.null(nb)) 0.0 else sum(as.numeric(unlist(nb)))
  }, numeric(1))
  wo <- list()
  for (u in N) wo[[u]] <- degs[[u]] / sum(degs)
  without <- list(w = wo, ranking = N[order(-degs)])
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

#' .tagRC_cheatsheet
#'
#' A step of the tagRC_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .tagRC_cheatsheet()
#' res
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

#' morie_tagRC
#'
#' A step of the tagRC_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param triples Passed to \code{folkrank}.
#' @param focus Passed to \code{folkrank}.
#' @param d Passed to \code{folkrank}. Defaults to \code{0.7}.
#' @param weight Passed to \code{folkrank}. Defaults to \code{0.9}.
#' @param iters Passed to \code{folkrank}. Defaults to \code{200}.
#' @return The value of \code{folkrank}.
#' @export
#' @examples
#' triples <- list(c("u1", "t1", "r1"), c("u1", "t2", "r2"),
#'                 c("u2", "t1", "r1"), c("u2", "t3", "r3"))
#' morie_tagRC(triples, focus = "t:t1")
#' @keywords internal
morie_tagRC <- function(triples, focus, d = 0.7, weight = 0.9,
                        iters = 200) {
  folkrank(triples, focus, d = d, weight = weight, iters = iters)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' tagRC_adapted_pagerank
#'
#' A step of the tagRC_native implementation. Called by \code{morie_tagRC}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adjacency A vector; indexed elementwise.
#' @param nodes Coerced to character by the body, with \code{as.character}.
#' @param p Optional; may be \code{NULL}. Coerced to list by the body, with \code{as.list}.
#' @param d Numeric; combined arithmetically in the body. Defaults to \code{0.7}.
#' @param iters A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200}.
#' @param tol Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-12}.
#' @return A list with \code{w}, \code{ranking}.
#' @export
tagRC_adapted_pagerank <- function(adjacency, nodes, p = NULL,
                                   d = 0.7, iters = 200,
                                   tol = 1e-12) {
  N <- as.character(nodes)
  n <- length(N)
  if (n == 0L)
    stop("tagRC: the graph is empty")
  d <- as.numeric(d)
  if (!is.finite(d) || !(d > 0) || !(d < 1))
    stop("tagRC: the damping factor must lie in (0,1)")
  tol <- as.numeric(tol)
  iters <- as.integer(iters)
  if (is.null(p)) {
    pref <- list()
    for (u in N) pref[[u]] <- 1.0 / n
  } else {
    pref <- as.list(p)
  }
  w <- list()
  for (u in N) w[[u]] <- 1.0 / n
  deg <- list()
  for (u in N) {
    inner <- adjacency[[u]]
    if (is.null(inner)) inner <- list()
    s <- 0
    for (v in inner) s <- s + v
    deg[[u]] <- s
  }
  for (.i in seq_len(iters)) {
    nxt <- list()
    for (u in N) {
      s <- 0.0
      for (v in N) {
        a <- 0.0
        inner_v <- adjacency[[v]]
        if (!is.null(inner_v) && !is.null(inner_v[[u]]))
          a <- inner_v[[u]]
        if (a > 0.0 && deg[[v]] > .tagRC.EPS)
          s <- s + w[[v]] * a / deg[[v]]
      }
      pref_u <- 0.0
      if (!is.null(pref[[u]])) pref_u <- pref[[u]]
      nxt[[u]] <- d * s + (1.0 - d) * pref_u
    }
    tot <- 0
    for (v in nxt) tot <- tot + v
    if (tot == 0) tot <- 1.0
    for (u in N) nxt[[u]] <- nxt[[u]] / tot
    delta <- 0
    for (u in N) {
      diff_u <- abs(nxt[[u]] - w[[u]])
      if (diff_u > delta) delta <- diff_u
    }
    w <- nxt
    if (delta < tol) break
  }
  ranking <- N[order(-unlist(w, use.names = FALSE))]
  list(w = w, ranking = ranking)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' tagRC_cheatsheet
#'
#' A step of the tagRC_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
tagRC_cheatsheet <- function() {
  paste("tagRC: a folksonomy is (user, tag, resource) TRIPLES, so ",
        "the structure is an undirected triadic HYPEREDGE, not a ",
        "directed binary link -- PageRank cannot be applied ",
        "directly. Flatten each triple to three undirected edges; ",
        "because they are undirected, DEGREE dominates the ",
        "stationary distribution and a topic preference vector ",
        "still returns the globally popular nodes. FOLKRANK is the ",
        "difference of two runs, WITH and WITHOUT the preference ",
        "vector: whatever is popular regardless of the query ",
        "cancels, and what the preference actually pulled up ",
        "survives. Keep ||p||_1 = ||w||_1.", sep = "")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' tagRC_preference_vector
#'
#' A step of the tagRC_native implementation. Called by \code{morie_tagRC}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param nodes Coerced to character by the body, with \code{as.character}.
#' @param focus Coerced to character by the body, with \code{as.character}.
#' @param weight Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.9}.
#' @return A list with \code{p}, \code{focus}, \code{mass}.
#' @export
tagRC_preference_vector <- function(nodes, focus, weight = 0.9) {
  N <- as.list(nodes)
  Nset <- unique(as.character(nodes))
  F <- as.character(focus)
  F <- F[F %in% Nset]
  if (length(F) == 0L)
    stop("tagRC: none of the focus nodes are in the graph")
  w <- as.numeric(weight)
  if (!is.finite(w) || !(w > 0) || !(w < 1))
    stop("tagRC: the focus weight must lie in (0,1)")
  rest <- length(Nset) - length(F)
  p <- list()
  for (n in Nset) {
    if (n %in% F)
      p[[n]] <- w / length(F)
    else
      p[[n]] <- if (rest > 0L) (1.0 - w) / rest else 0.0
  }
  mass <- 0
  for (v in p) mass <- mass + v
  list(p = p, focus = F, mass = mass)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' tagRC_tripartite_graph
#'
#' A step of the tagRC_native implementation. Called by \code{morie_tagRC}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param triples See Usage.
#' @return A list with \code{adjacency}, \code{nodes}, \code{n_nodes}, \code{n_triples},
#' \code{note}.
#' @export
tagRC_tripartite_graph <- function(triples) {
  nodes <- character(0)
  edges <- new.env(hash = TRUE, parent = emptyenv())
  n_triples <- 0L
  for (tr in triples) {
    u <- tr[[1]]
    t <- tr[[2]]
    r <- tr[[3]]
    nu <- paste0("u:", u)
    nt <- paste0("t:", t)
    nr <- paste0("r:", r)
    nodes <- c(nodes, nu, nt, nr)
    pairs <- list(c(nu, nt), c(nt, nr), c(nu, nr))
    for (ab in pairs) {
      a <- ab[1]
      b <- ab[2]
      key_ab <- paste0(a, "|", b)
      key_ba <- paste0(b, "|", a)
      cur_ab <- if (exists(key_ab, envir = edges, inherits = FALSE))
        get(key_ab, envir = edges) else 0
      cur_ba <- if (exists(key_ba, envir = edges, inherits = FALSE))
        get(key_ba, envir = edges) else 0
      assign(key_ab, cur_ab + 1, envir = edges)
      assign(key_ba, cur_ba + 1, envir = edges)
    }
    n_triples <- n_triples + 1L
  }
  nodes <- sort(unique(nodes))
  adj <- new.env(hash = TRUE, parent = emptyenv())
  for (nm in ls(edges)) {
    ab <- strsplit(nm, "|", fixed = TRUE)[[1]]
    a <- ab[1]
    b <- ab[2]
    w <- get(nm, envir = edges)
    if (exists(a, envir = adj, inherits = FALSE)) {
      inner <- get(a, envir = adj)
      inner[[b]] <- as.numeric(w)
      assign(a, inner, envir = adj)
    } else {
      inner <- new.env(hash = TRUE, parent = emptyenv())
      inner[[b]] <- as.numeric(w)
      assign(a, inner, envir = adj)
    }
  }
  adj_list <- list()
  for (a in nodes) {
    if (exists(a, envir = adj, inherits = FALSE)) {
      inner <- get(a, envir = adj)
      adj_list[[a]] <- as.list(inner)
    } else {
      adj_list[[a]] <- list()
    }
  }
  list(adjacency = adj_list, nodes = nodes,
       n_nodes = length(nodes), n_triples = n_triples,
       note = paste("a triadic hyperedge flattened to three",
                    "undirected edges, so weight flows both ways"))
}

# -- restored: morie-only objects kept through the rmorie sync --
.tagRC.EPS <- 1e-12

# -- restored: pre-sync definition (tagRC_folkrank) --
#' @noRd
tagRC_folkrank <- morie_tagRC

# -- restored: pre-sync definition (tagRC_folkrank_search) --
#' @noRd
tagRC_folkrank_search <- morie_tagRC

# -- restored: pre-sync definition (tagRC_tag_aware_rec) --
#' @noRd
tagRC_tag_aware_rec <- morie_tagRC

# -- restored: pre-sync definition (tagRC_tagawarerec) --
#' @noRd
tagRC_tagawarerec <- morie_tagRC
