# SPDX-License-Identifier: AGPL-3.0-or-later
#' Discrete Bayesian network inference by variable elimination
#'
#' Posterior P(query given evidence) by sum-product variable
#' elimination: every conditional probability table becomes a factor,
#' evidence rows are selected out, each hidden non-query variable is
#' summed out of the product of the factors that mention it, and the
#' surviving factors are multiplied and normalized. The elimination
#' order is lexicographic over the hidden variables -- deterministic,
#' identical to the Python arm, and the order affects only cost, never
#' the result.
#'
#' @param graph Named list: node -> character vector of parent names
#'   (ordered as in the CPT axes).
#' @param cpts Named list: node -> nested list or array with axes
#'   (parent_1, ..., parent_k, node).
#' @param evidence Named list: node -> observed state index (0-based,
#'   matching the Python arm).
#' @param query The query node name.
#' @return List with posterior, states, estimate (0-based argmax),
#'   normalizer, query.
#' @references Zhang, N. L. and Poole, D. (1994). A simple approach to
#'   Bayesian network computations. Proc. 10th Canadian Conference on
#'   AI, 171-178. Archived:
#'   fetched-wave3/zhang-poole-1994-simple-approach-bn.pdf.
#'
#'   Pearl, J. (1988). Probabilistic Reasoning in Intelligent Systems.
#'   Morgan Kaufmann.
#'
#'   Koller, D. and Friedman, N. (2009). Probabilistic Graphical
#'   Models. MIT Press, Ch. 9.
#' @examples
#' graph <- list(C = character(0), R = "C")
#' cpts <- list(C = c(0.5, 0.5), R = list(c(0.8, 0.2), c(0.2, 0.8)))
#' Baynet(graph, cpts, list(), "R")
#' @export
Baynet <- function(graph, cpts, evidence = list(), query = NULL) {
  if (is.null(query)) stop("`query` is required")
  nodes <- sort(names(graph))
  card <- integer(0)
  for (v in nodes) {
    t <- cpts[[v]]
    for (p in graph[[v]]) t <- t[[1]]
    card[v] <- length(unlist(t))
  }
  if (!query %in% names(card)) stop("unknown query node")
  ev <- list()
  for (v in names(evidence)) {
    if (!v %in% names(card)) stop("unknown evidence node")
    s <- as.integer(evidence[[v]])
    if (s < 0L || s >= card[v]) stop("evidence state out of range")
    ev[[v]] <- s
  }

  flatten_cpt <- function(child) {
    scope <- c(graph[[child]], child)
    dims <- card[scope]
    flat <- numeric(0)
    walk <- function(t, depth) {
      if (depth == length(dims)) {
        flat[length(flat) + 1L] <<- as.numeric(t)
        return(invisible(NULL))
      }
      tt <- if (is.list(t)) t else as.list(t)
      if (length(tt) != dims[depth + 1L]) stop("CPT has wrong extent")
      for (item in tt) walk(item, depth + 1L)
      invisible(NULL)
    }
    walk(cpts[[child]], 0L)
    list(scope = scope, table = flat)
  }

  f_index <- function(scope, assign) {
    idx <- 0L
    for (v in scope) idx <- idx * card[v] + assign[[v]]
    idx
  }

  enum_assign <- function(scope, fun) {
    dims <- card[scope]
    total <- prod(dims)
    assign <- list()
    for (flat in 0:(total - 1L)) {
      rem <- flat
      for (pos in rev(seq_along(scope))) {
        assign[[scope[pos]]] <- rem %% dims[pos]
        rem <- rem %/% dims[pos]
      }
      fun(flat, assign)
    }
  }

  f_multiply <- function(f1, f2) {
    scope <- c(f1$scope, setdiff(f2$scope, f1$scope))
    out <- numeric(prod(card[scope]))
    enum_assign(scope, function(flat, assign) {
      out[flat + 1L] <<- f1$table[f_index(f1$scope, assign) + 1L] *
        f2$table[f_index(f2$scope, assign) + 1L]
    })
    list(scope = scope, table = out)
  }

  f_marginalize <- function(f, var) {
    if (!var %in% f$scope) return(f)
    new_scope <- setdiff(f$scope, var)
    out <- numeric(max(1, prod(card[new_scope])))
    enum_assign(f$scope, function(flat, assign) {
      k <- if (length(new_scope)) f_index(new_scope, assign) + 1L else 1L
      out[k] <<- out[k] + f$table[flat + 1L]
    })
    list(scope = new_scope, table = out)
  }

  f_reduce <- function(f) {
    hits <- intersect(f$scope, names(ev))
    if (!length(hits)) return(f)
    new_scope <- setdiff(f$scope, hits)
    out <- numeric(max(1, prod(card[new_scope])))
    enum_assign(f$scope, function(flat, assign) {
      ok <- TRUE
      for (v in hits) if (assign[[v]] != ev[[v]]) ok <- FALSE
      if (ok) {
        k <- if (length(new_scope)) f_index(new_scope, assign) + 1L else 1L
        out[k] <<- out[k] + f$table[flat + 1L]
      }
    })
    list(scope = new_scope, table = out)
  }

  factors <- lapply(nodes, function(v) f_reduce(flatten_cpt(v)))
  hidden <- setdiff(nodes, c(query, names(ev)))
  for (z in hidden) {
    touches <- vapply(factors, function(f) z %in% f$scope, TRUE)
    if (!any(touches)) next
    touch <- factors[touches]
    keep <- factors[!touches]
    prod_f <- touch[[1L]]
    if (length(touch) > 1L) {
      for (idx in 2:length(touch)) prod_f <- f_multiply(prod_f, touch[[idx]])
    }
    keep[[length(keep) + 1L]] <- f_marginalize(prod_f, z)
    factors <- keep
  }
  result <- list(scope = character(0), table = 1)
  for (f in factors) result <- f_multiply(result, f)
  if (!length(result$scope)) stop("query eliminated; check inputs")
  for (v in setdiff(result$scope, query)) result <- f_marginalize(result, v)
  norm <- sum(result$table)
  if (norm <= 0) stop("zero-probability evidence")
  post <- result$table / norm
  est <- 1L
  for (i in seq_along(post)) if (post[i] > post[est] + 1e-15) est <- i
  list(posterior = post, states = card[[query]], estimate = est - 1L,
       normalizer = norm, query = query,
       method = "Variable elimination (Zhang-Poole 1994), lexicographic order")
}

#' @rdname Baynet
#' @export
bayes_network <- Baynet
