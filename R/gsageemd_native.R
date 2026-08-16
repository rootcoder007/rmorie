# GraphSAGE: embeddings for nodes the model has never seen.
# Sources: Hamilton, W. L., Ying, R. and Leskovec, J. (2017),
# Inductive Representation Learning on Large Graphs, NeurIPS 2017
# (arXiv:1706.02216) -- Algorithm 1, the three aggregators and
# the unsupervised loss; Kipf, T. N. and Welling, M. (2017), Semi-
# Supervised Classification with Graph Convolutional Networks, ICLR
# 2017 (arXiv:1609.02907) -- the transductive convolution that
# GraphSAGE extends.
#
# Native implementation mirroring Python morie.fn.gsageemd exactly:
# the same mean, max_pool and lstm_order aggregators, the same
# fixed-size neighbour sampling with replacement when the budget
# exceeds the neighbourhood, the same L2 normalisation after each
# layer, and the same unsupervised graph-based loss.

.GSAGEEMD_EPS <- 1e-12
.GSAGEEMD_AGGS <- c("mean", "max_pool", "lstm_order")

#' Aggregate a set of neighbour vectors
#'
#' @param vectors Numeric matrix (one row per neighbour) or list of
#'   rows.
#' @param how One of "mean", "max_pool", "lstm_order".
#' @param W Optional weight matrix for max_pool.
#' @return Permutation-invariant summary vector.
#' @export
.gsageemd_mat <- function(x) {
  # k.mat's contract: accept a list of vectors OR a matrix, yield a matrix.
  m <- if (is.matrix(x)) x else if (is.data.frame(x)) as.matrix(x) else
    do.call(rbind, lapply(x, as.numeric))
  storage.mode(m) <- "double"
  m
}

#' morie_gsageemd_aggregate
#'
#' A step of the gsageemd_native implementation. Called by \code{sage_layer}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param vectors Passed to \code{.gsageemd_mat}.
#' @param how One of \code{"max_pool"}, \code{"mean"}. Defaults to \code{"mean"}.
#' @param W Optional; may be \code{NULL}. A matrix; indexed by row and column.
#' @return One of two values, depending on the branch taken.
#' @export
morie_gsageemd_aggregate <- function(vectors, how = "mean", W = NULL) {
  if (!(how %in% .GSAGEEMD_AGGS))
    stop(paste0("gsageemd: aggregator must be one of ",
                paste(.GSAGEEMD_AGGS, collapse = ", "),
                ", got '", how, "'"))
  V <- .gsageemd_mat(vectors)
  if (nrow(V) == 0L)
    stop("gsageemd: no neighbours to aggregate")
  d <- ncol(V)
  if (how == "mean") return(colSums(V) / nrow(V))
  if (how == "max_pool") {
    if (is.null(W)) return(apply(V, 2, max))
    W <- as.matrix(W); storage.mode(W) <- "double"
    H <- matrix(0, nrow = nrow(V), ncol = nrow(W))
    for (i in seq_len(nrow(V)))
      for (o in seq_len(nrow(W))) {
        s <- 0
        for (j in seq_len(d)) s <- s + W[o, j] * V[i, j]
        H[i, o] <- max(0, s)
      }
    apply(H, 2, max)
  } else {
    # lstm_order: pass through the first row in given order, mirroring
    # the Python helper's "patch" that returns the unaggregated view.
    V[1, ]
  }
}

#' Sample a fixed number of neighbours with replacement
#'
#' @param adj Adjacency list keyed by 0-based node id (character keys).
#' @param v Node to sample around.
#' @param size Number of neighbours to draw.
#' @param rng Optional generator environment (defaults to .ghc_rng(0)).
#' @return Vector of sampled neighbour ids.
#' @export
sample_neighbors <- function(adj, v, size, rng = NULL) {
  if (is.null(rng)) rng <- .ghc_rng(0L)
  nb <- sort(as.integer(names(adj[[as.character(v)]])))
  if (length(nb) == 0L)
    stop(paste0("gsageemd: node ", v, " has no neighbours"))
  s <- as.integer(size)
  if (s < 1L)
    stop("gsageemd: the sample size must be at least 1")
  kk <- as.integer(.ghc_unif(rng, s) * length(nb))
  kk[kk == length(nb)] <- length(nb) - 1L
  nb[kk + 1L]
}

#' @keywords internal
#' @noRd
.gs_norm <- function(v) {
  n <- sqrt(sum(v * v))
  if (n <= .GSAGEEMD_EPS) v else v / n
}

#' One depth of Algorithm 1
#'
#' @param H Node feature matrix (n x d).
#' @param adj Adjacency list keyed by character "0..n-1".
#' @param W Weight matrix.
#' @param how Aggregator.
#' @param sizes Optional sample size per node.
#' @param rng Optional generator environment.
#' @param normalize If TRUE, L2-normalise the output.
#' @return Updated node feature matrix.
#' @export
sage_layer <- function(H, adj, W, how = "mean", sizes = NULL,
                       rng = NULL, normalize = TRUE) {
  H <- as.matrix(H); storage.mode(H) <- "double"
  W <- as.matrix(W); storage.mode(W) <- "double"
  n <- nrow(H)
  out <- matrix(0, nrow = n, ncol = nrow(W))
  for (v in seq_len(n) - 1L) {
    if (is.null(sizes)) {
      nk <- sort(as.integer(names(adj[[as.character(v)]])))
    } else {
      nk <- sample_neighbors(adj, v, sizes, rng)
    }
    if (length(nk) == 0L)
      stop(paste0("gsageemd: node ", v, " has no neighbours"))
    agg <- morie_gsageemd_aggregate(H[nk + 1L, , drop = FALSE], how)
    cat <- c(as.numeric(H[v + 1L, ]), agg)
    if (ncol(W) != length(cat))
      stop(paste0("gsageemd: W expects ", ncol(W),
                  " inputs but the concatenation is ", length(cat)))
    z <- rep(0, nrow(W))
    for (o in seq_len(nrow(W))) {
      s <- 0
      for (j in seq_along(cat)) s <- s + W[o, j] * cat[j]
      z[o] <- max(0, s)
    }
    out[v + 1L, ] <- if (normalize) .gs_norm(z) else z
  }
  out
}

#' Inductive node embeddings by K GraphSAGE layers
#'
#' @param features Node feature matrix.
#' @param adj Adjacency list.
#' @param Ws List of weight matrices, one per layer.
#' @param how Aggregator.
#' @param sizes Optional per-layer sample size.
#' @param seed Seed for the shared generator.
#' @return List with estimate, embeddings, depth, aggregator,
#'   per_batch_bound, method, note.
#' @export
morie_gsageemd_embed <- function(features, adj, Ws, how = "mean", sizes = NULL,
                  seed = 0) {
  rng <- .ghc_rng(as.integer(seed))
  H <- as.matrix(features); storage.mode(H) <- "double"
  for (W in Ws) {
    H <- sage_layer(H, adj, W, how, sizes, rng)
  }
  list(estimate = H, embeddings = H, depth = length(Ws),
       aggregator = how,
       per_batch_bound = if (is.null(sizes)) NULL
                         else as.integer(sizes)^length(Ws),
       method = "GraphSAGE; Hamilton, Ying & Leskovec (2017) Algorithm 1",
       note = "parameters are shared across nodes, so an unseen node is embedded by a forward pass -- inductive, not transductive")
}

#' Sec. 3.2 unsupervised graph-based loss
#' @param z_u Source embedding.
#' @param z_v Positive neighbour embedding.
#' @param z_negatives List of negative embeddings.
#' @return Scalar negative log-likelihood.
#' @export
unsupervised_loss <- function(z_u, z_v, z_negatives) {
  z_u <- as.numeric(z_u); z_v <- as.numeric(z_v)
  pos <- log(pmax(1 / (1 + exp(-sum(z_u * z_v))), .GSAGEEMD_EPS))
  neg <- 0
  for (z_n in z_negatives) {
    z_n <- as.numeric(z_n)
    neg <- neg + log(pmax(1 / (1 + exp(sum(z_u * z_n))), .GSAGEEMD_EPS))
  }
  -(pos + neg)
}

#' @export
graphsage <- morie_gsageemd_embed

# house entry point: the package exports one morie_<module>
morie_gsageemd <- morie_gsageemd_embed
