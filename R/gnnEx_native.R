# morie.fn -- function file (rootcoder007/morie)
# GNNExplainer: which subgraph and which features drove this prediction.
#
# A trained GNN's prediction for a node depends on its computation graph
# (the L-hop neighbourhood) and on the node features within it.
# GNNExplainer asks which *small* part of each actually mattered, and
# formulates that as an optimisation rather than a heuristic:
#
#   max_{G_S}  MI(Y, (G_S, X_S)) = H(Y) - H(Y | G=G_S, X=X_S)
#
# H(Y) is fixed once the model is trained, so maximising the mutual
# information is minimising the conditional entropy. A mean-field
# variational relaxation replaces the discrete choice with a real-valued
# graph mask on the edges and a feature mask on the dimensions, both
# learned by gradient descent and squashed through a sigmoid. Size and
# entropy penalties keep the explanation small and near-binary.
#
# References
# ----------
# Ying, R., Bourgeois, D., You, J., Zitnik, M. & Leskovec, J. (2019)
# "GNNExplainer: Generating Explanations for Graph Neural Networks",
# Advances in Neural Information Processing Systems 32 (NeurIPS 2019),
# 9240-9251, arXiv:1903.03894. Sec. 4.1 (the MI objective, H(Y) being
# constant for a trained GNN so the problem reduces to minimising
# conditional entropy, and the mean-field variational approximation
# learning a real-valued graph mask). Sec. 4.2 (the feature mask).
#
# Kipf, T. N. & Welling, M. (2017) "Semi-Supervised Classification with
# Graph Convolutional Networks", ICLR 2017, arXiv:1609.02907. The model
# class being explained.

.gnnEx_EPS <- 1e-12

.gnnEx_sig <- function(x) {
  # Vectorised sigmoid with overflow protection (matches Python _sig).
  out <- numeric(length(x))
  big_neg <- x <= -700
  out[!big_neg] <- 1.0 / (1.0 + exp(-x[!big_neg]))
  out[big_neg] <- 0.0
  out
}

.gnnEx_get_neighbors <- function(adj, u) {
  # adj is a list: names are node IDs (as character or integer), values
  # are integer vectors of neighbours. Falls back to 1-based integer
  # indexing when the list has no names.
  if (is.null(adj)) return(integer(0))
  if (!is.null(names(adj))) {
    key <- as.character(u)
    if (key %in% names(adj)) {
      return(as.integer(adj[[key]]))
    }
  }
  u_idx <- as.integer(u)
  if (u_idx >= 1L && u_idx <= length(adj)) {
    return(as.integer(adj[[u_idx]]))
  }
  integer(0)
}

gnnEx_computation_graph <- function(adj, v, L) {
  # The L-hop neighbourhood -- everything the prediction could depend on.
  v_int <- as.integer(v)
  seen <- v_int
  frontier <- v_int
  for (iter in seq_len(as.integer(L))) {
    nxt <- integer(0)
    if (length(frontier) > 0L) {
      for (u in frontier) {
        nbrs <- .gnnEx_get_neighbors(adj, u)
        nxt <- union(nxt, nbrs)
      }
    }
    frontier <- setdiff(nxt, seen)
    seen <- union(seen, nxt)
  }
  edges <- list()
  for (a in sort(seen)) {
    nbrs <- .gnnEx_get_neighbors(adj, a)
    if (length(nbrs) == 0L) next
    for (b in sort(nbrs)) {
      if (b %in% seen && a < b) {
        edges[[length(edges) + 1L]] <- c(as.integer(a), as.integer(b))
      }
    }
  }
  list(
    nodes = sort(seen),
    edges = edges,
    hops = as.integer(L),
    size = length(edges)
  )
}

gnnEx_conditional_entropy <- function(probs) {
  # H(Y | .) for a predicted distribution.
  p <- as.numeric(probs)
  s <- sum(p)
  if (s <= .gnnEx_EPS) {
    stop("gnnEx: the prediction has no mass")
  }
  p <- p / s
  -sum(p * log(pmax(p, .gnnEx_EPS)))
}

gnnEx_mask_objective <- function(predict, edges, edge_logits, feature_logits, y,
                                 size_coef = 0.005, entropy_coef = 1.0) {
  # Minimise -log p_theta(y) plus size and entropy penalties.
  em <- .gnnEx_sig(edge_logits)
  fm <- .gnnEx_sig(feature_logits)
  p <- predict(edges, em, fm)
  y_idx <- as.integer(y) + 1L
  loss <- -log(max(as.numeric(p[y_idx]), .gnnEx_EPS))
  size <- size_coef * (sum(em) + sum(fm))
  n_em <- max(length(em), 1L)
  ent <- entropy_coef * (
    sum(-(em * log(pmax(em, .gnnEx_EPS)) +
         (1 - em) * log(pmax(1 - em, .gnnEx_EPS)))) / n_em)
  list(
    loss = loss + size + ent,
    fit = loss,
    size = size,
    entropy = ent,
    edge_mask = em,
    feature_mask = fm,
    prediction = p
  )
}

gnnEx_explain_node <- function(predict, adj, v, y, n_features, L = 2,
                               iters = 300, lr = 0.1, size_coef = 0.005,
                               entropy_coef = 1.0, seed = 0, penalize = TRUE) {
  # Learn the edge and feature masks by gradient descent.
  cg <- gnnEx_computation_graph(adj, v, L)
  edges <- cg$edges
  if (length(edges) == 0L) {
    stop(sprintf("gnnEx: node %s has an empty computation graph",
                 as.character(v)))
  }
  st <- .ghc_rng(seed)
  n_total <- length(edges) + as.integer(n_features)
  all_u <- .ghc_unif(st, n_total)
  el <- (all_u[1:length(edges)] - 0.5) * 0.1
  fl <- (all_u[(length(edges) + 1L):n_total] - 0.5) * 0.1
  sc <- if (isTRUE(penalize)) size_coef else 0.0
  ec <- if (isTRUE(penalize)) entropy_coef else 0.0
  h <- 1e-4
  hist <- numeric(0)
  for (iter in seq_len(as.integer(iters))) {
    base <- gnnEx_mask_objective(predict, edges, el, fl, y, sc, ec)
    hist <- c(hist, base$loss)
    # Every partial derivative is taken against the SAME point: updating
    # in place mid-sweep would mix a stale baseline with already-moved
    # coordinates and descend in the wrong direction.
    ge <- numeric(length(el))
    for (i in seq_along(el)) {
      up <- el
      up[i] <- up[i] + h
      obj <- gnnEx_mask_objective(predict, edges, up, fl, y, sc, ec)
      ge[i] <- (obj$loss - base$loss) / h
    }
    gf <- numeric(length(fl))
    for (i in seq_along(fl)) {
      up <- fl
      up[i] <- up[i] + h
      obj <- gnnEx_mask_objective(predict, edges, el, up, y, sc, ec)
      gf[i] <- (obj$loss - base$loss) / h
    }
    for (i in seq_along(el)) {
      el[i] <- el[i] - lr * ge[i]
    }
    for (i in seq_along(fl)) {
      fl[i] <- fl[i] - lr * gf[i]
    }
  }
  final <- gnnEx_mask_objective(predict, edges, el, fl, y, sc, ec)
  order_idx <- order(-final$edge_mask)
  list(
    estimate = lapply(order_idx, function(i) edges[[i]]),
    edges_ranked = lapply(order_idx, function(i) {
      list(edge = edges[[i]], mask = final$edge_mask[i])
    }),
    edge_mask = final$edge_mask,
    feature_mask = final$feature_mask,
    loss_history = hist,
    final = final,
    computation_graph = cg,
    penalized = isTRUE(penalize),
    method = "GNNExplainer; Ying et al. (2019) Sec. 4",
    note = paste("maximising MI(Y, (G_S, X_S)) is minimising the",
                 "conditional entropy, since H(Y) is fixed once the",
                 "model is trained")
  )
}

gnnEx_cheatsheet <- function() {
  paste("gnnEx: explanation = a SMALL SUBGRAPH plus a SMALL FEATURE",
        "SUBSET, chosen by maximising MI(Y, (G_S, X_S)). Since H(Y) is",
        "fixed for a trained model, that is MINIMISING CONDITIONAL",
        "ENTROPY -- find the subgraph under which the model is least",
        "uncertain. Combinatorial search is replaced by a mean-field",
        "relaxation: continuous edge and feature masks learned by",
        "gradient descent, with size and entropy penalties WITHOUT",
        "WHICH the mask stays diffuse. Both masks matter: edges alone",
        "cannot name a feature, features alone cannot name a neighbour.")
}

# Entry point
morie_gnnEx <- gnnEx_explain_node

# Compact aliases per ledger/NAMING.md
gnnexplainer <- gnnEx_explain_node
gnn_explainer <- gnnEx_explain_node

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph

#' @rdname gnnEx_computation_graph
#' @export
morie_gnnEx <- gnnEx_computation_graph
