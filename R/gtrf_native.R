# Graph transformer: attention that respects the graph.
# Sources: Dwivedi, V. P. and Bresson, X. (2020), A Generalization of
# Transformer Networks to Graphs, AAAI Workshop on Deep Learning on
# Graphs (arXiv:2012.09699) -- neighbour-restricted attention,
# Laplacian-eigenvector positional encoding, batch normalisation
# instead of layer normalisation, and the edge-feature pipeline;
# Vaswani, A. et al. (2017), Attention Is All You Need, NIPS 2017 --
# the architecture being generalised; Belkin, M. and Niyogi, M.
# (2003), Laplacian Eigenmaps for Dimensionality Reduction and Data
# Representation, Neural Computation 15(6) -- the eigenvectors used
# as the positional encoding.
#
# Native implementation mirroring Python morie.fn.gtrf exactly: the
# same normalised Laplacian L = I - D^{-1/2} A D^{-1/2}, the same k
# smallest non-trivial eigenvectors as positional encoding, the same
# random sign flip during training, the same neighbour-restricted
# softmax attention with optional edge bias, and the same
# attention-residual-norm, feed-forward-residual-norm block.

.GTRF_EPS <- 1e-12

#' Build the graph Laplacian
#'
#' @param adj Adjacency list.
#' @param n Number of vertices.
#' @param normalized If TRUE, return I - D^\{-1/2\} A D^\{-1/2\}.
#' @return Square Laplacian matrix.
#' @export
laplacian <- function(adj, n, normalized = TRUE) {
  N <- as.integer(n)
  A <- matrix(0, nrow = N, ncol = N)
  for (k in names(adj)) {
    v <- as.integer(k) + 1L
    for (nk in names(adj[[k]])) {
      w <- as.integer(nk) + 1L
      A[v, w] <- 1; A[w, v] <- 1
    }
  }
  d <- rowSums(A)
  L <- matrix(0, nrow = N, ncol = N)
  for (i in seq_len(N)) for (j in seq_len(N)) {
    if (normalized) {
      if (d[i] <= .GTRF_EPS || d[j] <= .GTRF_EPS)
        L[i, j] <- if (i == j) 1 else 0
      else
        L[i, j] <- (if (i == j) 1 else 0) - A[i, j] / sqrt(d[i] * d[j])
    } else {
      L[i, j] <- (if (i == j) d[i] else 0) - A[i, j]
    }
  }
  L
}

#' Laplacian eigenvector positional encoding
#'
#' @param adj Adjacency list.
#' @param n Number of vertices.
#' @param dim Number of non-trivial eigenvectors.
#' @param normalized If TRUE, use the normalised Laplacian.
#' @return List with encoding, eigenvalues, caveat.
#' @export
laplacian_positional_encoding <- function(adj, n, dim = 2L,
                                           normalized = TRUE) {
  L <- laplacian(adj, n, normalized)
  ee <- eigen(L, symmetric = TRUE)
  vals <- ee$values; vecs <- ee$vectors
  order <- order(vals)
  take <- order[seq_len(as.integer(dim) + 1L)[-1L]]
  if (length(take) < as.integer(dim))
    stop(paste0("gtrf: the graph has only ", length(take),
                " non-trivial eigenvectors, ", as.integer(dim),
                " were asked for"))
  pe <- vecs[, take, drop = FALSE]
  list(encoding = pe, eigenvalues = as.numeric(vals[take]),
       caveat = "eigenvectors are defined up to SIGN, so the encoding is not unique -- the sign is flipped at random during training")
}

#' Random sign flip on the positional encoding
#'
#' @param pe Positional encoding matrix (n x d).
#' @param rng Generator environment.
#' @return Sign-flipped encoding.
#' @export
random_sign_flip <- function(pe, rng) {
  pe <- as.matrix(pe); storage.mode(pe) <- "double"
  d <- ncol(pe)
  s <- ifelse(.ghc_unif(rng, d) < 0.5, 1, -1)
  pe * matrix(s, nrow = nrow(pe), ncol = d, byrow = TRUE)
}

#' @keywords internal
#' @noRd
.gtrf_normalize <- function(X, how) {
  if (how == "none") return(X)
  X <- as.matrix(X); storage.mode(X) <- "double"
  n <- nrow(X); d <- ncol(X)
  if (how == "batch") {
    mu <- colSums(X) / n
    sd <- sqrt(colSums((X - matrix(mu, nrow = n, ncol = d,
                                   byrow = TRUE))^2) / n + 1e-5)
    return((X - matrix(mu, nrow = n, ncol = d, byrow = TRUE)) /
             matrix(sd, nrow = n, ncol = d, byrow = TRUE))
  }
  out <- matrix(0, nrow = n, ncol = d)
  for (i in seq_len(n)) {
    mu <- sum(X[i, ]) / d
    sd <- sqrt(sum((X[i, ] - mu)^2) / d + 1e-5)
    out[i, ] <- (X[i, ] - mu) / sd
  }
  out
}

#' Sparse (neighbour-restricted) attention
#'
#' @param H Node feature matrix.
#' @param adj Adjacency list keyed by character 0..n-1.
#' @param WQ Query projection.
#' @param WK Key projection.
#' @param WV Value projection.
#' @param edge_bias Optional list of edge biases keyed by (i, j).
#' @return List with output, note.
#' @export
morie_gtrf_sparse_attention <- function(H, adj, WQ, WK, WV, edge_bias = NULL) {
  H <- as.matrix(H); storage.mode(H) <- "double"
  WQ <- as.matrix(WQ); WK <- as.matrix(WK); WV <- as.matrix(WV)
  storage.mode(WQ) <- "double"; storage.mode(WK) <- "double"
  storage.mode(WV) <- "double"
  dk <- ncol(WQ)
  Q <- H %*% t(WQ)
  K <- H %*% t(WK)
  V <- H %*% t(WV)
  n <- nrow(H)
  out <- matrix(0, nrow = n, ncol = nrow(WV))
  for (i in seq_len(n) - 1L) {
    nk <- sort(as.integer(names(adj[[as.character(i)]])))
    if (length(nk) == 0L)
      stop(paste0("gtrf: node ", i, " has no neighbours"))
    nk1 <- nk + 1L
    sc <- as.numeric((Q[i + 1L, , drop = FALSE] %*% K[nk1, , drop = FALSE]) /
                        sqrt(dk))
    if (!is.null(edge_bias)) {
      for (kk in seq_along(nk)) {
        key <- paste0("(", i, ", ", nk[kk], ")")
        rev <- paste0("(", nk[kk], ", ", i, ")")
        eb <- edge_bias[[key]]
        if (is.null(eb)) eb <- edge_bias[[rev]]
        if (is.null(eb)) eb <- 0
        sc[kk] <- sc[kk] + as.numeric(eb)
      }
    }
    m <- max(sc)
    e <- exp(sc - m)
    z <- sum(e)
    w <- e / z
    out[i + 1L, ] <- as.numeric(w %*% V[nk1, , drop = FALSE])
  }
  list(output = out,
       note = "attention is a function of the NEIGHBOURHOOD, not of an arbitrary node ordering")
}

#' One graph transformer layer
#'
#' @param H Node feature matrix.
#' @param adj Adjacency list.
#' @param WQ,WK,WV Attention projection matrices.
#' @param W1,W2 Feed-forward matrices.
#' @param edge_bias Optional list of edge biases keyed by (i, j).
#' @param norm One of "batch", "layer", "none".
#' @return Updated node feature matrix.
#' @export
graph_transformer_layer <- function(H, adj, WQ, WK, WV, W1, W2,
                                    edge_bias = NULL,
                                    norm = "batch") {
  if (!(norm %in% c("batch", "layer", "none")))
    stop(paste0("gtrf: norm must be batch, layer or none, got '",
                norm, "'"))
  H <- as.matrix(H); storage.mode(H) <- "double"
  att <- morie_gtrf_sparse_attention(H, adj, WQ, WK, WV, edge_bias)$output
  res <- H + att
  res <- .gtrf_normalize(res, norm)
  W1 <- as.matrix(W1); W2 <- as.matrix(W2)
  storage.mode(W1) <- "double"; storage.mode(W2) <- "double"
  H1 <- pmax(res %*% t(W1), 0)
  ff <- H1 %*% t(W2)
  out <- res + ff
  .gtrf_normalize(out, norm)
}

# Compact aliases
#' @export
graphtransformer <- graph_transformer_layer
#' @export
graph_transformer <- graph_transformer_layer

# house entry point: the package exports one morie_<module>
morie_gtrf <- graph_transformer_layer
