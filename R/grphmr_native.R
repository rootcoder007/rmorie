# Graphormer: making a standard Transformer work on graphs.
# Sources: Ying, C., Cai, T., Luo, S., Zheng, S., Ke, G., He, D., Shen,
# Y. and Liu, T.-Y. (2021), Do Transformers Really Perform Bad for
# Graph Representation?, NeurIPS 2021 (arXiv:2106.05234) -- the
# centrality, spatial and edge encodings; Vaswani, A. et al. (2017),
# Attention Is All You Need, NIPS 2017 -- the standard Transformer
# that Graphormer extends; Dwivedi, V. P. and Bresson, X. (2020), A
# Generalization of Transformer Networks to Graphs, arXiv:2012.09699
# -- the neighbour-only alternative.
#
# Native implementation mirroring Python morie.fn.grphmr exactly: the
# same degree-indexed learnable vectors added at the input layer, the
# same BFS all-pairs shortest-path matrix with UNREACHABLE pairs
# getting a learnable bias, the same averaged edge feature along the
# path, and the same QK^T/sqrt(d) + bias + edge_bias attention logits.

# Mirrors the constant in the Python arm.
.GRPHMR_UNREACHABLE <- -1L

#' Learnable vector per degree, added at the input layer
#'
#' @param adj Adjacency list.
#' @param n Number of vertices.
#' @param z_in List of degree-indexed learnable vectors (in-degree).
#' @param z_out Optional list of out-degree vectors.
#' @param directed If TRUE, combine in- and out-degree vectors.
#' @return List with encoding, degrees, note.
#' @export
centrality_encoding <- function(adj, n, z_in, z_out = NULL,
                                 directed = FALSE) {
  N <- as.integer(n)
  deg_in <- rep(0L, N)
  deg_out <- rep(0L, N)
  for (k in names(adj)) {
    v <- as.integer(k) + 1L
    nb <- adj[[k]]
    for (nk in names(nb)) {
      w <- as.integer(nk) + 1L
      if (v == w) next
      deg_out[v] <- deg_out[v] + 1L
      deg_in[w] <- deg_in[w] + 1L
    }
  }
  if (!directed) {
    deg_in <- vapply(seq_len(N), function(v) {
      nb <- adj[[as.character(v - 1L)]]
      if (is.null(nb)) 0L
      else length(setdiff(names(nb), as.character(v - 1L)))
    }, integer(1))
    deg_out <- deg_in
  }
  out <- vector("list", N)
  for (v in seq_len(N)) {
    d <- min(deg_in[v], length(z_in) - 1L)
    vec <- as.numeric(z_in[[d + 1L]])
    if (directed && !is.null(z_out)) {
      o <- min(deg_out[v], length(z_out) - 1L)
      vec <- vec + as.numeric(z_out[[o + 1L]])
    }
    out[[v]] <- vec
  }
  list(encoding = out, degrees = as.integer(deg_in),
       note = "indexed by degree, added at the INPUT layer")
}

#' All-pairs shortest path lengths by BFS
#'
#' @param adj Adjacency list.
#' @param n Number of vertices.
#' @return List with distance matrix, unreachable, n_unreachable.
#' @export
shortest_path_matrix <- function(adj, n) {
  N <- as.integer(n)
  D <- matrix(.GRPHMR_UNREACHABLE, nrow = N, ncol = N)
  for (s in seq_len(N)) {
    D[s, s] <- 0L
    frontier <- s
    d <- 0L
    seen <- new.env(hash = TRUE, parent = emptyenv())
    seen[[as.character(s - 1L)]] <- TRUE
    while (length(frontier) > 0L) {
      d <- d + 1L
      nxt <- integer(0)
      for (v in frontier) {
        nb <- adj[[as.character(v - 1L)]]
        if (is.null(nb)) next
        nbks <- sort(setdiff(names(nb), as.character(v - 1L)))
        for (k in nbks) {
          if (is.null(seen[[k]])) {
            seen[[k]] <- TRUE
            w <- as.integer(k) + 1L
            D[s, w] <- d
            nxt <- c(nxt, w)
          }
        }
      }
      frontier <- nxt
    }
  }
  list(distance = D, unreachable = .GRPHMR_UNREACHABLE,
       n_unreachable = sum(D == .GRPHMR_UNREACHABLE))
}

#' Turn the distance matrix into the attention bias
#'
#' @param distance Square integer distance matrix.
#' @param b_table Learnable bias table indexed by distance.
#' @param unreachable_bias Bias for unreachable pairs.
#' @return List with bias, unreachable_bias, note.
#' @export
spatial_bias <- function(distance, b_table, unreachable_bias = NULL) {
  D <- matrix(as.integer(distance), nrow = nrow(distance))
  ub <- if (is.null(unreachable_bias)) -10.0 else as.numeric(unreachable_bias)
  out <- matrix(0, nrow = nrow(D), ncol = ncol(D))
  for (i in seq_len(nrow(D))) {
    for (j in seq_len(ncol(D))) {
      if (D[i, j] == .GRPHMR_UNREACHABLE) out[i, j] <- ub
      else {
        k <- min(D[i, j], length(b_table) - 1L)
        out[i, j] <- as.numeric(b_table[[k + 1L]])
      }
    }
  }
  list(bias = out, unreachable_bias = ub,
       note = "a bias inside the softmax keeps distant nodes reachable but discouraged")
}

#' Average edge features along the shortest path
#'
#' @param paths List of (i, j) -> ordered edge tuples along the path.
#' @param edge_features Edge feature lookup (e or its reverse).
#' @param w_table Learnable weight table indexed by step.
#' @return List with edge_bias and note.
#' @export
edge_encoding <- function(paths, edge_features, w_table) {
  out <- list()
  for (key in names(paths)) {
    path <- paths[[key]]
    if (length(path) == 0L) { out[[key]] <- 0; next }
    acc <- 0
    for (step in seq_along(path)) {
      e <- path[[step]]
      ekey <- if (is.character(e)) e
              else paste0("(", e[1], ", ", e[2], ")")
      f <- edge_features[[ekey]]
      if (is.null(f)) {
        rekey <- if (is.character(e)) e
                 else paste0("(", e[2], ", ", e[1], ")")
        f <- edge_features[[rekey]]
      }
      if (is.null(f))
        stop(paste0("grphmr: no features for edge ", ekey))
      w <- w_table[[min(step, length(w_table))]]
      fv <- as.numeric(f); wv <- as.numeric(w)
      acc <- acc + sum(fv * wv)
    }
    out[[key]] <- acc / length(path)
  }
  list(edge_bias = out,
       note = "edge information cannot reach the model through node features")
}

#' Full attention with the structural biases added to the logits
#'
#' @param H Node feature matrix (n x d).
#' @param WQ Query projection matrix.
#' @param WK Key projection matrix.
#' @param WV Value projection matrix.
#' @param bias Spatial bias matrix.
#' @param edge_bias Optional list of edge biases keyed by (i, j).
#' @return List with output, weights, method, note.
#' @export
graphormer_attention <- function(H, WQ, WK, WV, bias, edge_bias = NULL) {
  X <- as.matrix(H); storage.mode(X) <- "double"
  n <- nrow(X); dk <- ncol(WQ)
  WQ <- as.matrix(WQ); storage.mode(WQ) <- "double"
  WK <- as.matrix(WK); storage.mode(WK) <- "double"
  WV <- as.matrix(WV); storage.mode(WV) <- "double"
  B <- as.matrix(bias); storage.mode(B) <- "double"
  out <- matrix(0, nrow = n, ncol = nrow(WV))
  weights <- matrix(0, nrow = n, ncol = n)
  for (i in seq_len(n)) {
    q <- as.numeric(WQ %*% X[i, ])
    sc <- numeric(n)
    for (j in seq_len(n)) {
      kj <- as.numeric(WK %*% X[j, ])
      s <- sum(q * kj) / sqrt(dk) + B[i, j]
      if (!is.null(edge_bias)) {
        key <- paste0("(", i - 1L, ", ", j - 1L, ")")
        s <- s + as.numeric(edge_bias[[key]])
      }
      sc[j] <- s
    }
    m <- max(sc)
    e <- exp(sc - m)
    z <- sum(e)
    w <- e / z
    weights[i, ] <- w
    Vproj <- WV %*% t(X)
    out[i, ] <- as.numeric(w %*% Vproj)
  }
  list(estimate = out, output = out, weights = weights,
       method = "Graphormer attention with centrality, spatial and edge encodings; Ying et al. (2021)",
       note = "the architecture is a STANDARD Transformer; the structural encodings are what was missing")
}

# Compact alias
#' @export
graphormer <- graphormer_attention

# house entry point: the package exports one morie_<module>
morie_grphmr <- graphormer_attention
