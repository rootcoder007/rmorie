# HAN: hierarchical attention on a heterogeneous graph.
#
# Sources:
#   Wang, X., Ji, H., Shi, C., Wang, B., Cui, P., Yu, P. & Ye, Y.
#   (2019) "Heterogeneous Graph Attention Network", WWW '19,
#   2022-2032, arXiv:1903.07293.
#   Velickovic, P. et al. (2018) "Graph Attention Networks", ICLR 2018,
#   arXiv:1710.10903.
#   Dong, Y., Chawla, N. V. & Swami, A. (2017) "metapath2vec: Scalable
#   Representation Learning for Heterogeneous Networks", KDD '17,
#   135-144.

#' metapath_neighbours
#'
#' A step of the hetgnn_native implementation. Called by \code{han_forward}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param edges A vector; indexed elementwise.
#' @param types A vector; indexed elementwise.
#' @param metapath Coerced to character by the body, with \code{as.character}.
#' @return A list with \code{neighbours}, \code{metapath}, \code{note}.
#' @export
metapath_neighbours <- function(edges, types, metapath) {
  mp <- as.character(metapath)
  if (length(mp) < 2L) {
    stop("hetgnn: a meta-path needs at least 2 types")
  }
  nb <- list()
  starts <- names(types)[vapply(names(types),
                                function(v) identical(types[[v]], mp[1L]),
                                logical(1))]
  for (s in starts) {
    frontier <- s
    for (step in 2:length(mp)) {
      nxt <- character(0)
      for (v in frontier) {
        ws <- if (is.null(edges[[v]])) character(0) else edges[[v]]
        for (w in ws) {
          if (!is.null(types[[w]]) && identical(types[[w]], mp[step])) {
            nxt <- c(nxt, w)
          }
        }
      }
      nxt <- unique(nxt)
      frontier <- nxt
    }
    nb[[s]] <- sort(setdiff(frontier, s))
  }
  list(neighbours = nb, metapath = mp,
       note = paste("the relation between two nodes DEPENDS on the meta-path followed"))
}

#' node_attention
#'
#' A step of the hetgnn_native implementation. Called by \code{han_forward}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param h_i Passed to \code{proj}.
#' @param neighbours A vector; its length is taken and its elements indexed.
#' @param H A vector; indexed elementwise.
#' @param a_vec Numeric; combined arithmetically in the body.
#' @param W A matrix; passed to \code{\%*\%}.
#' @param slope Numeric; combined arithmetically in the body. Defaults to \code{0.2}.
#' @return A list with \code{embedding}, \code{alpha}, \code{neighbours}.
#' @export
node_attention <- function(h_i, neighbours, H, a_vec, W, slope = 0.2) {
  proj <- function(x) as.numeric(W %*% as.numeric(x))
  hi <- proj(h_i)
  if (length(neighbours) == 0L) {
    stop("hetgnn: the node has no meta-path neighbours; the meta-path does not apply here")
  }
  sc <- numeric(length(neighbours))
  for (tt in seq_along(neighbours)) {
    j <- neighbours[tt]
    hj <- proj(H[[j]])
    z <- hi + hj
    s <- sum(a_vec * z)
    sc[tt] <- if (s >= 0) s else slope * s
  }
  m <- max(sc)
  e <- exp(sc - m)
  tot <- sum(e)
  al <- e / tot
  d <- length(hi)
  z <- numeric(d)
  for (a in 1:d) {
    acc <- 0.0
    for (tt in seq_along(neighbours)) {
      acc <- acc + al[tt] * proj(H[[neighbours[tt]]])[a]
    }
    z[a] <- acc
  }
  list(embedding = z, alpha = al, neighbours = neighbours)
}

#' semantic_attention
#'
#' A step of the hetgnn_native implementation. Called by \code{han_forward}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param Z_per_metapath A vector; indexed elementwise.
#' @param W A matrix; passed to \code{\%*\%}.
#' @param b Coerced to numeric by the body, with \code{as.numeric}.
#' @param q Numeric; combined arithmetically in the body.
#' @return A list with \code{beta}, \code{scores}, \code{metapaths}, \code{note}.
#' @export
semantic_attention <- function(Z_per_metapath, W, b, q) {
  names_v <- sort(names(Z_per_metapath))
  if (length(names_v) == 0L) {
    stop("hetgnn: no meta-path embeddings given")
  }
  w <- numeric(length(names_v))
  for (idx in seq_along(names_v)) {
    nm <- names_v[idx]
    Z <- as.matrix(Z_per_metapath[[nm]])
    acc <- 0.0
    for (i in seq_len(nrow(Z))) {
      proj <- tanh(as.numeric(b) + as.numeric(W %*% Z[i, ]))
      acc <- acc + sum(q * proj)
    }
    w[idx] <- acc / nrow(Z)
  }
  m <- max(w)
  e <- exp(w - m)
  tot <- sum(e)
  beta <- e / tot
  names(beta) <- names_v
  names(w) <- names_v
  list(beta = as.list(beta), scores = as.list(w),
       metapaths = names_v,
       note = paste("averaged over nodes, so the weight describes the META-PATH, not a node"))
}

#' han_forward
#'
#' A step of the hetgnn_native implementation. Called by \code{morie_hetgnn}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param H A matrix; indexed by row and column.
#' @param edges Passed to \code{metapath_neighbours}.
#' @param types Passed to \code{metapath_neighbours}.
#' @param metapaths A vector; indexed elementwise.
#' @param a_vec Passed to \code{node_attention}.
#' @param W_node A matrix; passed to \code{nrow}.
#' @param W_sem Passed to \code{semantic_attention}.
#' @param b_sem Passed to \code{semantic_attention}.
#' @param q_sem Passed to \code{semantic_attention}.
#' @param slope Passed to \code{node_attention}. Defaults to \code{0.2}.
#' @return A list with \code{estimate}, \code{embeddings}, \code{semantic_weights}, \code{per_metapath}, \code{method}, \code{note}.
#' @export
han_forward <- function(H, edges, types, metapaths, a_vec, W_node,
                        W_sem, b_sem, q_sem, slope = 0.2) {
  if (is.matrix(H)) {
    feats <- lapply(seq_len(nrow(H)), function(i) as.numeric(H[i, ]))
  } else {
    feats <- lapply(H, as.numeric)
  }
  per <- list()
  for (name in names(metapaths)) {
    mp <- metapaths[[name]]
    nb <- metapath_neighbours(edges, types, mp)$neighbours
    Z <- vector("list", length(feats))
    for (i in seq_along(feats)) {
      n <- nb[[as.character(i)]]
      if (length(n) == 0L) {
        Z[[i]] <- rep(0.0, nrow(W_node))
        next
      }
      Z[[i]] <- node_attention(feats[[i]], n, feats, a_vec,
                               W_node, slope)$embedding
    }
    per[[name]] <- Z
  }
  sem <- semantic_attention(per, W_sem, b_sem, q_sem)
  names_v <- sem$metapaths
  d <- length(per[[names_v[1L]]][[1L]])
  final <- matrix(0.0, length(feats), d)
  for (i in seq_along(feats)) {
    for (a in 1:d) {
      acc <- 0.0
      for (nm in names_v) {
        acc <- acc + sem$beta[[nm]] * per[[nm]][[i]][a]
      }
      final[i, a] <- acc
    }
  }
  list(estimate = final, embeddings = final,
       semantic_weights = sem$beta,
       per_metapath = per,
       method = paste("hierarchical attention on a heterogeneous graph; Wang et al. (2019)"),
       note = paste("two attentions answering different questions: which NEIGHBOUR, and which META-PATH"))
}

heterogeneousattention <- han_forward
heterogeneous_gnn <- han_forward

#' morie_hetgnn
#'
#' A step of the hetgnn_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param H Passed to \code{han_forward}.
#' @param edges Passed to \code{han_forward}.
#' @param types Passed to \code{han_forward}.
#' @param metapaths Passed to \code{han_forward}.
#' @param a_vec Passed to \code{han_forward}.
#' @param W_node Passed to \code{han_forward}.
#' @param W_sem Passed to \code{han_forward}.
#' @param b_sem Passed to \code{han_forward}.
#' @param q_sem Passed to \code{han_forward}.
#' @param slope Passed to \code{han_forward}. Defaults to \code{0.2}.
#' @return The value of \code{han_forward}.
#' @export
morie_hetgnn <- function(H, edges, types, metapaths, a_vec, W_node,
                         W_sem, b_sem, q_sem, slope = 0.2) {
  han_forward(H, edges, types, metapaths, a_vec, W_node, W_sem, b_sem,
              q_sem, slope)
}

#' .hetgnn_cheatsheet
#'
#' A step of the hetgnn_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.hetgnn_cheatsheet <- function() {
  paste("hetgnn: in a heterogeneous graph the relation between two nodes depends on the META-PATH -- Movie-Actor-Movie is co-actor, Movie-Director-Movie is shared-director, and a homogeneous GNN cannot say which it followed. TWO attentions in a hierarchy: NODE-level picks which meta-path neighbours matter, SEMANTIC-level picks which meta-paths matter, averaged OVER NODES so the weight describes the relation rather than a node. Collapsing them loses the distinction; keeping them makes both readable, which is the paper's interpretability claim.")
}
