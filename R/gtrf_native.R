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
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' laplacian(V, V)
#' @keywords internal
laplacian <- function(adj, n, normalized = TRUE) {
  N <- as.integer(n)
  A <- matrix(0, nrow = N, ncol = N)
  for (k in names(adj)) {
    v <- as.integer(k) + 1L
    for (nk in names(adj[[k]])) {
      w <- as.integer(nk) + 1L
      A[v, w] <- 1
      A[w, v] <- 1
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
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' laplacian_positional_encoding(V, V)
#' @keywords internal
laplacian_positional_encoding <- function(adj, n, dim = 2L,
                                           normalized = TRUE) {
  L <- laplacian(adj, n, normalized)
  ee <- eigen(L, symmetric = TRUE)
  vals <- ee$values
  vecs <- ee$vectors
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
#' @examples
#' random_sign_flip(pe = c(1, 2, 3, 4, 5, 6, 7, 8), rng = list(a = 1, b = 2))
#' @keywords internal
random_sign_flip <- function(pe, rng) {
  pe <- as.matrix(pe)
  storage.mode(pe) <- "double"
  d <- ncol(pe)
  s <- ifelse(.ghc_unif(rng, d) < 0.5, 1, -1)
  pe * matrix(s, nrow = nrow(pe), ncol = d, byrow = TRUE)
}

#' @keywords internal
#' @noRd
.gtrf_normalize <- function(X, how) {
  if (how == "none") return(X)
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- nrow(X)
  d <- ncol(X)
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
#' Graph transformer: attention that respects the graph. Transformer Networks to
#' Graphs, AAAI Workshop on Deep Learning on Graphs (arXiv:2012.09699) --
#' neighbour-restricted attention, Laplacian-eigenvector positional encoding,
#' batch normalisation instead of layer normalisation, and the edge-feature
#' pipeline; the architecture being generalised; Belkin, M. and Niyogi, M.
#' Representation, Neural Computation 15(6) -- the eigenvectors used as the
#' positional encoding. Native implementation mirroring Python morie.fn.gtrf
#' exactly: the same normalised Laplacian L = I - D^\{-1/2\} A D^\{-1/2\}, the
#' same k smallest non-trivial eigenvectors as positional encoding, the same
#' random sign flip during training, the same neighbour-restricted softmax
#' attention with optional edge bias, and the same attention-residual-norm,
#' feed-forward-residual-norm block.
#'
#' @param H Node feature matrix.
#' @param adj Adjacency list keyed by character 0..n-1.
#' @param WQ Query projection.
#' @param WK Key projection.
#' @param WV Value projection.
#' @param edge_bias Optional list of edge biases keyed by (i, j).
#' @return List with output, note.
#' @export
#' @examples
#' set.seed(1)
#' H <- matrix(rnorm(6), 3, 2)
#' adj <- list("0" = c("1" = 1, "2" = 1),
#'             "1" = c("0" = 1, "2" = 1),
#'             "2" = c("0" = 1, "1" = 1))
#' W <- matrix(rnorm(4), 2, 2)
#' morie_gtrf_sparse_attention(H, adj, W, W, W)
#' @keywords internal
morie_gtrf_sparse_attention <- function(H, adj, WQ, WK, WV, edge_bias = NULL) {
  H <- as.matrix(H)
  storage.mode(H) <- "double"
  WQ <- as.matrix(WQ)
  WK <- as.matrix(WK)
  WV <- as.matrix(WV)
  storage.mode(WQ) <- "double"
  storage.mode(WK) <- "double"
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
#' @examples
#' set.seed(1)
#' H <- matrix(rnorm(6), 3, 2)
#' adj <- list("0" = c("1" = 1, "2" = 1), "1" = c("0" = 1, "2" = 1),
#'             "2" = c("0" = 1, "1" = 1))
#' W <- matrix(rnorm(4), 2, 2)
#' W1 <- matrix(rnorm(8), 4, 2)
#' W2 <- matrix(rnorm(8), 2, 4)
#' graph_transformer_layer(H, adj, W, W, W, W1, W2)
#' @keywords internal
graph_transformer_layer <- function(H, adj, WQ, WK, WV, W1, W2,
                                    edge_bias = NULL,
                                    norm = "batch") {
  if (!(norm %in% c("batch", "layer", "none")))
    stop(paste0("gtrf: norm must be batch, layer or none, got '",
                norm, "'"))
  H <- as.matrix(H)
  storage.mode(H) <- "double"
  att <- morie_gtrf_sparse_attention(H, adj, WQ, WK, WV, edge_bias)$output
  res <- H + att
  res <- .gtrf_normalize(res, norm)
  W1 <- as.matrix(W1)
  W2 <- as.matrix(W2)
  storage.mode(W1) <- "double"
  storage.mode(W2) <- "double"
  H1 <- pmax(res %*% t(W1), 0)
  ff <- H1 %*% t(W2)
  out <- res + ff
  .gtrf_normalize(out, norm)
}

# Compact aliases
#' @rdname graph_transformer_layer
#' @export
graphtransformer <- graph_transformer_layer
#' @rdname graph_transformer_layer
#' @export
graph_transformer <- graph_transformer_layer

# house entry point: the package exports one morie_<module>
morie_gtrf <- graph_transformer_layer

# -- restored: morie-only definition kept through the rmorie sync --
#' Neighbourhood-restricted attention
#'
#' Standard scaled dot-product softmax attention, but only over the
#' neighbours: dense attention would throw the graph away.
#'
#' @param H Node feature matrix (n x d).
#' @param adj Adjacency list keyed by node id.
#' @param WQ,WK,WV Projection matrices.
#' @param edge_bias Optional per-pair scalar biases.
#' @return A list with \code{output} and \code{note}.
#' @references Dwivedi, V. P. and Bresson, X. (2020).
#' @export
morie_gtrf_attention <- function(H, adj, WQ, WK, WV, edge_bias = NULL) {
  rows <- apply(H, c(1L, 2L), as.numeric)
  dk <- ncol(WQ)
  project <- function(W, x) as.numeric(W %*% x)
  n <- nrow(rows)
  out <- matrix(0.0, nrow = n, ncol = nrow(WV))
  for (i in seq_len(n) - 1L) {
    nb <- sort(as.integer(adj[[as.character(i)]]))
    if (length(nb) == 0L) stop(paste0("gtrf: node ", i, " has no neighbours"))
    q <- project(WQ, rows[i + 1L, ])
    sc <- numeric(length(nb))
    for (jj in seq_along(nb)) {
      j <- nb[jj]
      kk <- project(WK, rows[j + 1L, ])
      s <- sum(q * kk) / sqrt(dk)
      if (!is.null(edge_bias)) {
        eb <- edge_bias[[paste0(i, ", ", j)]]
        if (is.null(eb)) eb <- edge_bias[[paste0(j, ", ", i)]]
        if (is.null(eb)) eb <- 0.0
        s <- s + as.numeric(eb)
      }
      sc[jj] <- s
    }
    m <- max(sc)
    e <- exp(sc - m)
    z <- sum(e)
    w <- e / z
    Vs <- rows[nb + 1L, , drop = FALSE] %*% t(WV)
    out[i + 1L, ] <- as.numeric(crossprod(w, Vs))
  }
  list(output = out,
       note = paste0("attention is a function of the NEIGHBOURHOOD, ",
                     "not of an arbitrary node ordering"))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Graph Laplacian
#'
#' \eqn{L = I - D^{-1/2} A D^{-1/2}} (normalised) or \eqn{D - A}
#' (unnormalised).
#'
#' @param adj Adjacency list.
#' @param n Number of nodes.
#' @param normalized Use the symmetric normalised Laplacian.
#' @return Square numeric matrix.
#' @references Belkin, M. and Niyogi, P. (2003).
#' @export
morie_gtrf_laplacian <- function(adj, n, normalized = TRUE) {
  N <- as.integer(n)
  A <- matrix(0.0, nrow = N, ncol = N)
  for (v in seq_len(N) - 1L) {
    nbrs <- as.integer(adj[[v + 1L]])
    for (w in nbrs) {
      if (v == w) next
      A[v + 1L, w + 1L] <- 1.0
      A[w + 1L, v + 1L] <- 1.0
    }
  }
  d <- rowSums(A)
  L <- matrix(0.0, nrow = N, ncol = N)
  for (i in seq_len(N)) for (j in seq_len(N)) {
    if (normalized) {
      if (d[i] <= .GTRF_EPS || d[j] <= .GTRF_EPS) {
        L[i, j] <- if (i == j) 1.0 else 0.0
      } else {
        L[i, j] <- (if (i == j) 1.0 else 0.0) -
          A[i, j] / sqrt(d[i] * d[j])
      }
    } else {
      L[i, j] <- (if (i == j) d[i] else 0.0) - A[i, j]
    }
  }
  L
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Laplacian positional encoding
#'
#' The smallest non-trivial eigenvectors of the Laplacian: on a path
#' graph these are sinusoids, so the NLP positional encoding is the
#' special case.
#'
#' @param adj Adjacency list.
#' @param n Number of nodes.
#' @param dim Encoding dimension (number of eigenvectors).
#' @param normalized Use the symmetric normalised Laplacian.
#' @return A list with \code{encoding} (n x dim), \code{eigenvalues}
#'   and the sign caveat.
#' @references Dwivedi, V. P. and Bresson, X. (2020).
#' @export
morie_gtrf_lap_pe <- function(adj, n, dim = 2L, normalized = TRUE) {
  L <- morie_gtrf_laplacian(adj, n, normalized)
  ev <- eigen(L, symmetric = TRUE)
  vals <- ev$values
  vecs <- ev$vectors
  order <- order(vals)
  take <- order[seq_len(as.integer(dim) + 1L)[-1L]]
  if (length(take) < as.integer(dim))
    stop(paste0("gtrf: the graph has only ", length(take),
                " non-trivial eigenvectors, ", as.integer(dim),
                " were asked for"))
  pe <- vecs[, take, drop = FALSE]
  list(encoding = pe,
       eigenvalues = vals[take],
       caveat = paste0("eigenvectors are defined up to SIGN, so the ",
                       "encoding is not unique -- the sign is flipped ",
                       "at random during training"))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Graph transformer layer
#'
#' Attention, residual, normalisation, feed-forward, residual,
#' normalisation, with batch normalisation by default.
#'
#' @param H Node feature matrix (n x d).
#' @param adj Adjacency list.
#' @param WQ,WK,WV Attention projections.
#' @param W1,W2 Feed-forward projections.
#' @param edge_bias Optional per-pair scalar biases.
#' @param norm "batch", "layer" or "none".
#' @return New node feature matrix.
#' @references Dwivedi, V. P. and Bresson, X. (2020).
#' @export
morie_gtrf_layer <- function(H, adj, WQ, WK, WV, W1, W2,
                             edge_bias = NULL, norm = "batch") {
  if (!(norm %in% c("batch", "layer", "none")))
    stop(paste0("gtrf: norm must be batch, layer or none, got ",
                deparse(norm)))
  att <- morie_gtrf_attention(H, adj, WQ, WK, WV, edge_bias)$output
  res <- H + att
  res <- .gtrf_normalize(res, norm)
  ff <- matrix(0.0, nrow = nrow(res), ncol = nrow(W2))
  for (i in seq_len(nrow(res)) - 1L) {
    h1 <- pmax(0.0, as.numeric(W1 %*% res[i + 1L, ]))
    ff[i + 1L, ] <- as.numeric(W2 %*% h1)
  }
  out <- res + ff
  .gtrf_normalize(out, norm)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Random sign flip
#'
#' Flips each eigenvector's sign with probability 1/2, per the paper.
#'
#' @param pe Positional encoding matrix (n x d).
#' @param rng Generator environment (shared with the Python arm).
#' @return Sign-flipped encoding.
#' @references Dwivedi, V. P. and Bresson, X. (2020).
#' @export
morie_gtrf_sign_flip <- function(pe, rng) {
  d <- ncol(pe)
  sgn <- ifelse(.ghc_unif(rng, d) < 0.5, 1.0, -1.0)
  pe * matrix(sgn, nrow = nrow(pe), ncol = d, byrow = TRUE)
}
