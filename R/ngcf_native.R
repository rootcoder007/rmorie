# morie.fn -- function file (rootcoder007/morie)
# r"""Neural graph collaborative filtering: the signal is in the graph.
#
# Standard collaborative filtering learns an embedding per user and item
# and then scores by an interaction function. The collaborative signal
# -- the behavioural similarity encoded in *who consumed what* -- never
# enters the embedding itself; it is only present in the objective. NGCF
# puts it into the embedding by propagating over the user-item bipartite
# graph.
#
# **Message construction, with a term that is not standard.** For a
# connected pair,
#
# .. math:: m_{u \leftarrow i} = \frac{1}{\sqrt{|N_u||N_i|}}
#           \big(W_1 e_i + W_2 (e_i \odot e_u)\big).
#
# The first term is ordinary graph convolution. The second,
# :math:`e_i \odot e_u`, makes the message depend on the **affinity**
# between the two embeddings, so more is passed from items similar to
# the user. That element-wise term is NGCF's addition, and dropping it
# degrades the model to a plain GCN -- the anchor separates the two
# rather than trusting the description.
#
# **The Laplacian coefficient is a decay, not a normaliser.**
# :math:`p_{ui} = 1/\sqrt{|N_u||N_i|}` can be read two ways: as how much
# a historical item contributes to the user's preference, or as a
# discount reflecting that messages should weaken with path length.
#
# **Aggregation keeps the node's own signal.**
#
# .. math:: e_u^{(1)} = \mathrm{LeakyReLU}\Big(m_{u\leftarrow u}
#           + \sum_{i \in N_u} m_{u \leftarrow i}\Big),
#
# with the self-message retaining the original features.
#
# **Stacking layers is stacking orders of connectivity.** Two layers
# capture :math:`u_1 \leftarrow i_2 \leftarrow u_2` -- behavioural
# similarity between users. Three capture
# :math:`u_1 \leftarrow i_2 \leftarrow u_2 \leftarrow i_4` -- a
# recommendation path. The final representation concatenates the
# embeddings from all layers, so each order contributes explicitly, and
# the trainable weights between layers determine the strength of that
# flow.
#
# References
# ----------
# Wang, X., He, X., Wang, M., Feng, F. & Chua, T.-S. (2019) "Neural
# Graph Collaborative Filtering", *Proceedings of the 42nd
# International ACM SIGIR Conference on Research and Development in
# Information Retrieval (SIGIR '19)*, 165-174,
# doi:10.1145/3331184.3331267. Sec. 1 (the collaborative signal is
# absent from the embedding in conventional CF; the interpretation of
# two- and three-layer propagation as behavioural similarity and
# potential recommendations). Sec. 2.2.1 (message construction eq. (3)
# including the e_i (*) e_u affinity term, the Laplacian coefficient
# p_ui = 1/sqrt(|N_u||N_i|) read both as contribution and as a
# path-length discount, and message aggregation eq. (4) with the
# self-connection). Sec. 2.3 (concatenating the per-layer embeddings).
#
# Kipf, T. N. & Welling, M. (2017) "Semi-Supervised Classification with
# Graph Convolutional Networks", *ICLR 2017*, arXiv:1609.02907. The
# graph convolution being extended.
#
# He, X., Liao, L., Zhang, H., Nie, L., Hu, X. & Chua, T.-S. (2017)
# "Neural Collaborative Filtering", *WWW '17*, 173-182,
# doi:10.1145/3038912.3052569. The framework NGCF is measured against;
# implemented in :mod:`ncfRS`.
# """

#' .ngcf_leaky
#'
#' A step of the ngcf_native implementation. Called by \code{ngcf_propagate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @param slope Numeric; combined arithmetically in the body. Defaults to \code{0.2}.
#' @return One of two values, depending on the branch taken.
#' @export
.ngcf_leaky <- function(x, slope=0.2) {
  if (x >= 0.0) x else slope * x
}

#' ngcf_laplacian_coefficient
#'
#' A step of the ngcf_native implementation. Called by \code{ngcf_propagate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n_u Coerced to integer by the body, with \code{as.integer}.
#' @param n_i Coerced to integer by the body, with \code{as.integer}.
#' @return A numeric value.
#' @export
ngcf_laplacian_coefficient <- function(n_u, n_i) {
  a <- as.integer(n_u)
  b <- as.integer(n_i)
  if (a < 1L || b < 1L) {
    stop(sprintf("ngcf: both nodes need at least one neighbour, got (%d, %d)", a, b))
  }
  1.0 / sqrt(as.numeric(a * b))
}

#' ngcf_message
#'
#' A step of the ngcf_native implementation. Called by \code{ngcf_propagate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param e_i Coerced to numeric by the body, with \code{as.numeric}.
#' @param e_u Coerced to numeric by the body, with \code{as.numeric}.
#' @param W1 A matrix; indexed by row and column.
#' @param W2 A matrix; indexed by row and column.
#' @param p_ui Numeric; combined arithmetically in the body.
#' @param affinity A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return The value of \code{out}, as built in the body.
#' @export
ngcf_message <- function(e_i, e_u, W1, W2, p_ui, affinity=TRUE) {
  ei <- as.numeric(e_i)
  eu <- as.numeric(e_u)
  d <- nrow(W1)
  out <- numeric(d)
  for (o in seq_len(d)) {
    s <- sum(W1[o, ] * ei)
    if (isTRUE(affinity)) {
      s <- s + sum(W2[o, ] * ei * eu)
    }
    out[o] <- p_ui * s
  }
  out
}

#' ngcf_propagate
#'
#' A step of the ngcf_native implementation. Called by \code{ngcf_stack_layers}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param E A matrix; indexed by row and column.
#' @param adjacency A vector; indexed elementwise.
#' @param W1 Passed to \code{ngcf_message}.
#' @param W2 Passed to \code{ngcf_message}.
#' @param affinity Passed to \code{ngcf_message}. Defaults to \code{TRUE}.
#' @param slope Passed to \code{.ngcf_leaky}. Defaults to \code{0.2}.
#' @return The value of \code{out}, as built in the body.
#' @export
ngcf_propagate <- function(E, adjacency, W1, W2, affinity=TRUE, slope=0.2) {
  n <- nrow(E)
  d <- ncol(E)
  deg <- sapply(seq_len(n), function(v) length(adjacency[[v]]))
  out <- matrix(0, nrow=n, ncol=d)
  for (v in seq_len(n)) {
    nb <- adjacency[[v]]
    if (length(nb) == 0L) {
      stop(sprintf("ngcf: node %d has no neighbours", v))
    }
    acc <- ngcf_message(E[v, , drop=FALSE], E[v, , drop=FALSE], W1, W2,
                        ngcf_laplacian_coefficient(deg[v], deg[v]),
                        affinity)
    for (w in nb) {
      m <- ngcf_message(E[w, , drop=FALSE], E[v, , drop=FALSE], W1, W2,
                        ngcf_laplacian_coefficient(deg[v], deg[w]),
                        affinity)
      acc <- acc + m
    }
    out[v, ] <- vapply(seq_along(acc), function(i) .ngcf_leaky(acc[i], slope), numeric(1))
  }
  out
}

#' ngcf_stack_layers
#'
#' A step of the ngcf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param E0 A matrix; passed to \code{as.matrix}.
#' @param adjacency Passed to \code{ngcf_propagate}.
#' @param Ws A vector; its length is taken.
#' @param affinity Coerced to logical by the body, with \code{as.logical}. Defaults to \code{TRUE}.
#' @param slope Passed to \code{ngcf_propagate}. Defaults to \code{0.2}.
#' @return A list with \code{estimate}, \code{final}, \code{layers}, \code{n_layers}, \code{affinity}, \code{method}, \code{note}.
#' @export
ngcf_stack_layers <- function(E0, adjacency, Ws, affinity=TRUE, slope=0.2) {
  E <- as.matrix(E0)
  storage.mode(E) <- "double"
  n <- nrow(E)
  layers <- list(E)
  for (W_pair in Ws) {
    W1 <- as.matrix(W_pair[[1]])
    W2 <- as.matrix(W_pair[[2]])
    E <- ngcf_propagate(E, adjacency, W1, W2, affinity, slope)
    layers[[length(layers) + 1L]] <- E
  }
  L_total <- length(layers)
  d_emb <- ncol(layers[[1]])
  final <- matrix(0, nrow=n, ncol=d_emb * L_total)
  for (v in seq_len(n)) {
    final[v, ] <- unlist(lapply(layers, function(layer) layer[v, ]))
  }
  list(
    estimate = final,
    final = final,
    layers = layers,
    n_layers = length(Ws),
    affinity = as.logical(affinity),
    method = "embedding propagation; Wang et al. (2019) eqs. (3)-(4) with per-layer concatenation",
    note = "2 layers reach user-user behavioural similarity, 3 reach a recommendation path"
  )
}

#' ngcf_score
#'
#' A step of the ngcf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param final A matrix; indexed by row and column.
#' @param u Coerced to integer by the body, with \code{as.integer}.
#' @param i Coerced to integer by the body, with \code{as.integer}.
#' @return A numeric value.
#' @export
ngcf_score <- function(final, u, i) {
  a <- final[as.integer(u), ]
  b <- final[as.integer(i), ]
  if (length(a) != length(b)) {
    stop("ngcf: representations differ in length")
  }
  sum(a * b)
}

#' ngcf_cheatsheet
#'
#' A step of the ngcf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
ngcf_cheatsheet <- function() {
  paste0("ngcf: conventional CF never puts the COLLABORATIVE ",
         "SIGNAL into the embedding -- only into the objective. ",
         "NGCF propagates over the user-item graph: ",
         "m_{u<-i} = p_ui (W1 e_i + W2 (e_i * e_u)), where the ",
         "elementwise AFFINITY term is NGCF's addition and dropping ",
         "it leaves a plain GCN. p_ui = 1/sqrt(|N_u||N_i|) doubles ",
         "as a path-length discount. Two layers reach user-user ",
         "similarity, three reach a recommendation path; all ",
         "layers are concatenated.")
}

# compact alias per ledger/NAMING.md
neuralgraphcf <- ngcf_stack_layers

# public names resolved by fn/_lazy_map.json
# Main entry point
morie_ngcf <- ngcf_stack_layers

















