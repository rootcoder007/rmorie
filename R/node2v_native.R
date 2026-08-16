# node2vec: the neighbourhood definition is the model.
# Sources: Grover, A. & Leskovec, J. (2016) "node2vec: Scalable
# Feature Learning for Networks", KDD '16, 855-864,
# doi:10.1145/2939672.2939754, arXiv:1607.00653. Sec. 2 (the
# document/sentence analogy and the observation that no single
# sampling strategy wins across networks and tasks). Sec. 3 (the
# maximum likelihood objective of eq. (1) under conditional
# independence and a softmax over the dot product; Sec. 3.1 on BFS
# giving a low-variance microscopic view versus DFS giving a
# macroscopic community view, and that real networks mix both; Sec.
# 3.2.2's second-order walk with alpha_pq keyed on the shortest-path
# distance d_tx from the previous node). Mikolov, T., Sutskever, I.,
# Chen, K., Corrado, G. & Dean, J. (2013) "Distributed Representations
# of Words and Phrases and their Compositionality", NIPS 2013, for
# skip-gram with negative sampling. Perozzi, B., Al-Rfou, R. & Skiena,
# S. (2014) "DeepWalk: Online Learning of Social Representations",
# KDD '14, for the uniform random walk node2vec generalises.

# Base R only, faithful translation of node2v_python_reference.py.

#' node2v_check_pq
#'
#' A step of the node2v_native implementation. Called by \code{alpha_pq}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p See Usage.
#' @param q See Usage.
#' @return A list with \code{p}, \code{q}.
#' @export
node2v_check_pq <- function(p, q) {
  p <- as.numeric(p)
  q <- as.numeric(q)
  if (!is.finite(p) || !is.finite(q) || p <= 0 || q <= 0)
    stop("node2v: p and q must be positive")
  list(p = p, q = q)
}

#' alpha_pq
#'
#' A step of the node2v_native implementation. Called by \code{transition_probabilities}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param d_tx See Usage.
#' @param p See Usage.
#' @param q See Usage.
#' @return Nothing; this branch always raises.
#' @export
alpha_pq <- function(d_tx, p, q) {
  d <- as.integer(d_tx)
  pq <- node2v_check_pq(p, q)
  if (d == 0L) return(1.0 / pq$p)
  if (d == 1L) return(1.0)
  if (d == 2L) return(1.0 / pq$q)
  stop("node2v: d_tx must be 0, 1 or 2 for a second-order walk, got ",
       format(d))
}

# Internal helper: shortest-path distance from t to x in the unweighted
# adjacency dict, restricted to 0/1/2.
#' Internal helper: shortest-path distance from t to x in the unweighted
#'
#' adjacency dict, restricted to 0/1/2.
#'
#' @param adj A vector; indexed elementwise.
#' @param t See Usage.
#' @param x See Usage.
#' @return A numeric value.
#' @export
.node2v_dist <- function(adj, t, x) {
  if (identical(t, x)) return(0L)
  nb_t <- adj[[t]]
  if (is.null(nb_t)) nb_t <- list()
  if (!is.null(nb_t[[as.character(x)]])) return(1L)
  2L
}

#' transition_probabilities
#'
#' A step of the node2v_native implementation. Called by \code{walk}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adj A vector; indexed elementwise.
#' @param t Optional; may be \code{NULL}. Passed to \code{.node2v_dist}.
#' @param v See Usage.
#' @param p See Usage.
#' @param q See Usage.
#' @param weights Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @return A list with \code{nodes}, \code{probabilities}, \code{unnormalized}, \code{Z}.
#' @export
transition_probabilities <- function(adj, t, v, p, q, weights = NULL) {
  nb <- adj[[v]]
  if (is.null(nb) || length(nb) == 0L) {
    stop("node2v: node ", v, " has no neighbours")
  }
  nb_names <- sort(names(nb))
  if (is.null(weights)) {
    ws <- rep(1.0, length(nb_names))
  } else {
    ws <- vapply(nb_names, function(x) {
      key <- paste(v, x, sep = "\r")
      wv <- weights[[key]]
      if (is.null(wv)) 1.0 else as.numeric(wv)
    }, numeric(1L))
  }
  if (is.null(t)) {
    a_vals <- rep(1.0, length(nb_names))
  } else {
    a_vals <- vapply(nb_names, function(x) {
      alpha_pq(.node2v_dist(adj, t, x), p, q)
    }, numeric(1L))
  }
  pi_vals <- as.numeric(ws) * a_vals
  Z <- sum(pi_vals)
  list(
    nodes = nb_names,
    probabilities = as.numeric(pi_vals / Z),
    unnormalized = as.numeric(pi_vals),
    Z = Z
  )
}

#' walk
#'
#' A step of the node2v_native implementation. Called by \code{.avalon_paths}, \code{.depth_counts}, \code{.dmlqs_count_totters} and 13 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adj See Usage.
#' @param start See Usage.
#' @param length See Usage.
#' @param p Defaults to \code{1}.
#' @param q Defaults to \code{1}.
#' @param seed Defaults to \code{0}.
#' @param weights Defaults to \code{NULL}.
#' @return The value of \code{path}, as built in the body.
#' @export
walk <- function(adj, start, length, p = 1.0, q = 1.0, seed = 0,
                 weights = NULL) {
  len <- as.integer(length)
  e <- .ghc_rng(as.numeric(seed))
  path <- as.character(start)
  prev <- NULL
  if (len > 1L) {
    for (k in seq_len(len - 1L)) {
      tp <- transition_probabilities(adj, prev, path[length(path)],
                                     p, q, weights)
      u <- .ghc_unif(e, 1L)
      acc <- 0.0
      nxt <- tp$nodes[length(tp$nodes)]
      for (i in seq_along(tp$nodes)) {
        acc <- acc + tp$probabilities[i]
        if (u <= acc) {
          nxt <- tp$nodes[i]
          break
        }
      }
      prev <- path[length(path)]
      path <- c(path, nxt)
    }
  }
  path
}

#' generate_walks
#'
#' A step of the node2v_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adj See Usage.
#' @param num_walks Defaults to \code{10}.
#' @param length Defaults to \code{10}.
#' @param p Defaults to \code{1}.
#' @param q Defaults to \code{1}.
#' @param seed Defaults to \code{0}.
#' @param weights Defaults to \code{NULL}.
#' @return A list with \code{estimate}, \code{walks}, \code{p}, \code{q}, \code{n_walks}, \code{length}, \code{method}, \code{note}.
#' @export
generate_walks <- function(adj, num_walks = 10, length = 10, p = 1.0,
                           q = 1.0, seed = 0, weights = NULL) {
  nw <- as.integer(num_walks)
  len <- as.integer(length)
  rng_seed <- as.numeric(seed)
  adj_names <- sort(names(adj))
  out <- vector("list", nw * length(adj_names))
  idx <- 0L
  for (j in seq_len(nw)) {
    for (v in adj_names) {
      idx <- idx + 1L
      out[[idx]] <- walk(adj, v, len, p, q, rng_seed, weights)
    }
  }
  list(
    estimate = out,
    walks = out,
    p = as.numeric(p),
    q = as.numeric(q),
    n_walks = length(out),
    length = len,
    method = "second-order biased random walk; Grover & Leskovec (2016) Sec. 3.2.2",
    note = "large q keeps the walk local (BFS-like), small q pushes it outward (DFS-like); p prices returning"
  )
}

# compact alias per ledger/NAMING.md
node2vec <- generate_walks

#' skipgram_pairs
#'
#' A step of the node2v_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param walks See Usage.
#' @param window Defaults to \code{2}.
#' @return The value of \code{do.call}.
#' @export
skipgram_pairs <- function(walks, window = 2) {
  w <- as.integer(window)
  if (w < 1L)
    stop("node2v: the window must be at least 1")
  pairs <- list()
  for (path in walks) {
    n <- length(path)
    for (i in seq_len(n)) {
      lo <- max(1L, i - w)
      hi <- min(n, i + w)
      for (j in lo:hi) {
        if (j != i) {
          pairs[[length(pairs) + 1L]] <- c(path[i], path[j])
        }
      }
    }
  }
  if (length(pairs) == 0L) return(list())
  do.call(rbind, pairs)
}

#' .node2v_cheatsheet
#'
#' A step of the node2v_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.node2v_cheatsheet <- function() {
  paste("node2v: graph as document, walk as sentence, skip-gram on ",
        "top. The point is that NO sampling strategy wins everywhere: ",
        "BFS gives a low-variance local structural view, DFS a ",
        "macroscopic community view, and real networks mix both. A ",
        "SECOND-ORDER walk interpolates -- having come from t, the ",
        "bias to x is 1/p if returning, 1 if x neighbours t, 1/q ",
        "otherwise. Large q stays local, small q roams. A first-order ",
        "walk cannot express this.", sep = "")
}

morie_node2v <- generate_walks
