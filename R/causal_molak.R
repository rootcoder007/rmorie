# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Causal-inference / causal-discovery shelf -- R mirror of
# morie/fn/_molak.py and the fourteen shelf modules.
#
# Spec: Molak, A., Causal Inference and Discovery in Python, Packt.
# The copy in the corpus carries the 2023 first-edition copyright page,
# so every locator is a first-edition page; the backlog labels the
# shelf as the 2025 second edition. Where a construct is only NAMED in
# that copy and no formula is printed, the comment says so rather than
# attributing a formula to the book.
#
# Collision scan: causal_molak.R and all fourteen exported names were
# free in both R trees (morie/r-package/morie and r-morie-oss).
#
# The path enumeration and blocking rules below mirror
# morie/fn/bdcrt._paths and ._blocked exactly, including their
# depth-first stack order, so path counts match the Python arm.

#' .morie_ml_pinv
#'
#' A step of the causal_molak implementation. Called by \code{morie_bicdag}, \code{morie_rlearn}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A matrix; passed to \code{dim}.
#' @return The value of \code{%*%}.
#' @export
.morie_ml_pinv <- function(a) {
  a <- as.matrix(a)
  s <- svd(a)
  tol <- max(dim(a)) * max(s$d) * .Machine$double.eps
  dinv <- ifelse(s$d > tol, 1 / s$d, 0)
  s$v %*% (dinv * t(s$u))
}

# --- graph plumbing ---------------------------------------------------

# A DAG is a two-column character matrix / data frame of edges, or a
# list(node = c(children)). Both spellings normalize to a sorted
# character matrix of edges.
#' A DAG is a two-column character matrix / data frame of edges, or a
#'
#' list(node = c(children)). Both spellings normalize to a sorted
#' character matrix of edges.
#'
#' @param dag A matrix; passed to \code{as.matrix}.
#' @return The value of \code{[}.
#' @export
.morie_ml_edges <- function(dag) {
  if (is.list(dag) && !is.data.frame(dag)) {
    nm <- names(dag)
    e <- do.call(rbind, lapply(seq_along(dag), function(i) {
      ch <- as.character(dag[[i]])
      if (length(ch) == 0L) return(NULL)
      cbind(rep(nm[i], length(ch)), ch)
    }))
  } else {
    e <- as.matrix(dag)
  }
  if (is.null(e) || nrow(e) == 0L) {
    return(matrix(character(0), ncol = 2))
  }
  e <- matrix(as.character(e), ncol = 2)
  e <- e[order(e[, 1], e[, 2]), , drop = FALSE]
  e[!duplicated(paste(e[, 1], e[, 2], sep = "\r")), , drop = FALSE]
}

#' .morie_ml_nodes
#'
#' A step of the causal_molak implementation. Called by \code{.morie_ml_acyclic}, \code{.morie_ml_dsep}, \code{morie_docalc} and 3 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param edges Coerced to character by the body, with \code{as.character}.
#' @param extra Coerced to character by the body, with \code{as.character}. Defaults to \code{character(0)}.
#' @return A vector, from \code{sort}.
#' @export
.morie_ml_nodes <- function(edges, extra = character(0)) {
  sort(unique(c(as.character(edges), as.character(extra))))
}

#' .morie_ml_children
#'
#' A step of the causal_molak implementation. Called by \code{.morie_ml_acyclic}, \code{.morie_ml_dsep}, \code{morie_docalc} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param edges A matrix; indexed by row and column.
#' @param nodes A vector; its length is taken.
#' @return The value of \code{lapply}.
#' @export
.morie_ml_children <- function(edges, nodes) {
  out <- setNames(vector("list", length(nodes)), nodes)
  for (n in nodes) out[[n]] <- character(0)
  if (nrow(edges)) {
    for (i in seq_len(nrow(edges))) {
      out[[edges[i, 1]]] <- c(out[[edges[i, 1]]], edges[i, 2])
    }
  }
  lapply(out, sort)
}

#' .morie_ml_parents
#'
#' A step of the causal_molak implementation. Called by \code{.morie_ml_dsep}, \code{morie_dseptest}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param edges A matrix; indexed by row and column.
#' @param nodes A vector; its length is taken.
#' @return The value of \code{lapply}.
#' @export
.morie_ml_parents <- function(edges, nodes) {
  out <- setNames(vector("list", length(nodes)), nodes)
  for (n in nodes) out[[n]] <- character(0)
  if (nrow(edges)) {
    for (i in seq_len(nrow(edges))) {
      out[[edges[i, 2]]] <- c(out[[edges[i, 2]]], edges[i, 1])
    }
  }
  lapply(out, sort)
}

#' .morie_ml_desc
#'
#' A step of the causal_molak implementation. Called by \code{.morie_ml_blocked}, \code{morie_docalc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param node See Usage.
#' @param children A vector; indexed elementwise.
#' @return The value of \code{seen}, as built in the body.
#' @export
.morie_ml_desc <- function(node, children) {
  seen <- character(0)
  stack <- node
  while (length(stack)) {
    cur <- stack[length(stack)]
    stack <- stack[-length(stack)]
    for (c in children[[cur]]) {
      if (!(c %in% seen)) {
        seen <- c(seen, c)
        stack <- c(stack, c)
      }
    }
  }
  seen
}

#' .morie_ml_paths
#'
#' A step of the causal_molak implementation. Called by \code{.morie_ml_dsep}, \code{morie_dseptest}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x See Usage.
#' @param y See Usage.
#' @param children A vector; indexed elementwise.
#' @param parents A vector; indexed elementwise.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_ml_paths <- function(x, y, children, parents) {
  out <- list()
  stack <- list(list(cur = x, path = x, dirs = character(0)))
  while (length(stack)) {
    top <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    if (top$cur == y) {
      out[[length(out) + 1L]] <- list(path = top$path, dirs = top$dirs)
      next
    }
    for (nx in children[[top$cur]]) {
      if (!(nx %in% top$path)) {
        stack[[length(stack) + 1L]] <- list(cur = nx, path = c(top$path, nx),
                                            dirs = c(top$dirs, "->"))
      }
    }
    for (nx in parents[[top$cur]]) {
      if (!(nx %in% top$path)) {
        stack[[length(stack) + 1L]] <- list(cur = nx, path = c(top$path, nx),
                                            dirs = c(top$dirs, "<-"))
      }
    }
  }
  out
}

#' .morie_ml_blocked
#'
#' A step of the causal_molak implementation. Called by \code{.morie_ml_dsep}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param path A vector; its length is taken and its elements indexed.
#' @param dirs A vector; indexed elementwise.
#' @param z See Usage.
#' @param children Passed to \code{.morie_ml_desc}.
#' @return A logical value.
#' @export
.morie_ml_blocked <- function(path, dirs, z, children) {
  n <- length(path)
  if (n < 3L) return(FALSE)
  for (i in 2:(n - 1L)) {
    node <- path[i]
    is_col <- dirs[i - 1L] == "->" && dirs[i] == "<-"
    if (is_col) {
      if (!(node %in% z) &&
          length(intersect(.morie_ml_desc(node, children), z)) == 0L) {
        return(TRUE)
      }
    } else if (node %in% z) {
      return(TRUE)
    }
  }
  FALSE
}

#' .morie_ml_dsep
#'
#' A step of the causal_molak implementation. Called by \code{morie_docalc}, \code{morie_dseptest}, \code{morie_faithchk} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param edges Passed to \code{.morie_ml_nodes}.
#' @param x Passed to \code{.morie_ml_paths}.
#' @param y Passed to \code{.morie_ml_paths}.
#' @param z Passed to \code{.morie_ml_blocked}. Defaults to \code{character(0)}.
#' @param nodes Optional; may be \code{NULL}. Passed to \code{.morie_ml_children}.
#' @return A logical value.
#' @export
.morie_ml_dsep <- function(edges, x, y, z = character(0), nodes = NULL) {
  nodes <- if (is.null(nodes)) .morie_ml_nodes(edges, c(x, y, z)) else nodes
  ch <- .morie_ml_children(edges, nodes)
  pa <- .morie_ml_parents(edges, nodes)
  for (nn in c(x, y, z)) {
    if (!(nn %in% nodes)) stop(sprintf("node %s not in the graph.", nn), call. = FALSE)
  }
  ps <- .morie_ml_paths(x, y, ch, pa)
  if (length(ps) == 0L) return(TRUE)
  all(vapply(ps, function(p) .morie_ml_blocked(p$path, p$dirs, z, ch), logical(1)))
}

#' .morie_ml_cutin
#'
#' A step of the causal_molak implementation. Called by \code{morie_docalc}, \code{morie_dointerv}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param edges A matrix; indexed by row and column.
#' @param targets See Usage.
#' @return The value of \code{[}.
#' @export
.morie_ml_cutin <- function(edges, targets) {
  if (!nrow(edges)) return(edges)
  edges[!(edges[, 2] %in% targets), , drop = FALSE]
}

#' .morie_ml_cutout
#'
#' A step of the causal_molak implementation. Called by \code{morie_docalc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param edges A matrix; indexed by row and column.
#' @param sources See Usage.
#' @return The value of \code{[}.
#' @export
.morie_ml_cutout <- function(edges, sources) {
  if (!nrow(edges)) return(edges)
  edges[!(edges[, 1] %in% sources), , drop = FALSE]
}

#' .morie_ml_skeleton
#'
#' A step of the causal_molak implementation. Called by \code{morie_mectest}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param edges A matrix; passed to \code{nrow}.
#' @return A vector, from \code{sort}.
#' @export
.morie_ml_skeleton <- function(edges) {
  if (!nrow(edges)) return(character(0))
  sort(unique(apply(edges, 1, function(r) paste(sort(r), collapse = "\r"))))
}

#' .morie_ml_adj
#'
#' A step of the causal_molak implementation. Called by \code{.morie_ml_colliders}, \code{morie_bowarc}, \code{morie_collider} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param edges A matrix; indexed by row and column.
#' @param a See Usage.
#' @param b See Usage.
#' @return A logical value.
#' @export
.morie_ml_adj <- function(edges, a, b) {
  if (!nrow(edges)) return(FALSE)
  any(edges[, 1] == a & edges[, 2] == b) || any(edges[, 1] == b & edges[, 2] == a)
}

#' .morie_ml_acyclic
#'
#' A step of the causal_molak implementation. Called by \code{morie_bowarc}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param edges Passed to \code{.morie_ml_nodes}.
#' @return A logical value.
#' @export
.morie_ml_acyclic <- function(edges) {
  nodes <- .morie_ml_nodes(edges)
  ch <- .morie_ml_children(edges, nodes)
  colour <- setNames(rep(0L, length(nodes)), nodes)
  visit <- function(n) {
    colour[[n]] <<- 1L
    for (c in ch[[n]]) {
      if (colour[[c]] == 1L) return(TRUE)
      if (colour[[c]] == 0L && visit(c)) return(TRUE)
    }
    colour[[n]] <<- 2L
    FALSE
  }
  for (n in nodes) {
    if (colour[[n]] == 0L && visit(n)) return(FALSE)
  }
  TRUE
}

#' .morie_ml_colliders
#'
#' A step of the causal_molak implementation. Called by \code{morie_collider}, \code{morie_mectest}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param edges A matrix; indexed by row and column.
#' @return A vector, from \code{sort}.
#' @export
.morie_ml_colliders <- function(edges) {
  if (!nrow(edges)) return(character(0))
  out <- character(0)
  for (cnode in sort(unique(edges[, 2]))) {
    ps <- sort(unique(edges[edges[, 2] == cnode, 1]))
    if (length(ps) < 2L) next
    for (i in seq_len(length(ps) - 1L)) {
      for (j in (i + 1L):length(ps)) {
        if (!.morie_ml_adj(edges, ps[i], ps[j])) {
          out <- c(out, paste(ps[i], cnode, ps[j], sep = "\r"))
        }
      }
    }
  }
  sort(out)
}

# --- ch. 13, p. 348: GES scoring --------------------------------------

#' Gaussian BIC score of a DAG
#'
#' The corpus copy only NAMES the Bayesian Information Criterion as a
#' gCastle GES scoring option (ch. 13, p. 348, citing Chickering 2003);
#' no BIC formula is printed there. The formula used here is the
#' standard Gaussian one, stated so nothing is attributed to the book
#' that the book does not say:
#' score(G) = sum_j \[-n/2 (log(2 pi s2_j) + 1)\] - (log n / 2) k.
#' @param data numeric matrix, one row per observation
#' @param dag edge matrix or list(node = children)
#' @param names column names matching the DAG node labels
#' @return list(score, loglik, k, penalty, n)
#' @export
morie_bicdag <- function(data, dag, names = NULL) {
  d <- as.matrix(data)
  storage.mode(d) <- "double"
  n <- nrow(d)
  if (n < 2L) stop("need at least 2 rows of data.", call. = FALSE)
  p <- ncol(d)
  nms <- if (is.null(names)) as.character(seq_len(p) - 1L) else as.character(names)
  if (length(nms) != p) stop("names must have one entry per column.", call. = FALSE)
  edges <- .morie_ml_edges(dag)
  pars <- setNames(vector("list", p), nms)
  for (nm in nms) pars[[nm]] <- character(0)
  if (nrow(edges)) {
    for (i in seq_len(nrow(edges))) {
      if (!(edges[i, 1] %in% nms) || !(edges[i, 2] %in% nms)) {
        stop("DAG node has no data column.", call. = FALSE)
      }
      pars[[edges[i, 2]]] <- c(pars[[edges[i, 2]]], edges[i, 1])
    }
  }
  total <- 0
  k <- 0L
  for (j in seq_len(p)) {
    nm <- nms[j]
    y <- d[, j]
    ps <- sort(pars[[nm]])
    xm <- cbind(rep(1, n))
    for (q in ps) xm <- cbind(xm, d[, which(nms == q)])
    beta <- .morie_ml_pinv(xm) %*% y
    rss <- sum((y - as.numeric(xm %*% beta))^2)
    s2 <- max(rss / n, 1e-300)
    total <- total - 0.5 * n * (log(2 * pi * s2) + 1)
    k <- k + length(ps) + 2L
  }
  list(score = total - 0.5 * log(n) * k, loglik = total, k = k,
       penalty = 0.5 * log(n) * k, n = n)
}

# --- bow arcs ---------------------------------------------------------

#' Whether a pair forms a bow
#'
#' NOT LOCATED IN THE EXTRACTED TEXT of the corpus copy of Molak. The
#' definition and theorem are taken from the primary source: "A bow-arc
#' is a pair of variables, one of which is a direct function of the
#' other, whose error terms are correlated"; "Theorem 4. (Brito and
#' Pearl, 2002b) (Bow-free Rule) Every acyclic model whose path diagram
#' lacks bow-arcs is identified." -- Chen, B. and Pearl, J. (2015),
#' Graphical Tools for Linear Structural Equation Modeling, UCLA
#' Technical Report R-432, p. 15, citing Brito, C. and Pearl, J. (2002),
#' Structural Equation Modeling 9(4):459-474. identified reports
#' Theorem 4 itself: acyclic AND bow-free.
#' @param dag directed edge matrix
#' @param bidirected two-column matrix of bidirected edges
#' @param x,y the ordered pair to test
#' @return list(isbow, direct, confounded, nbows, bowfree, acyclic,
#'   identified, bows)
#' @export
morie_bowarc <- function(dag, bidirected, x, y) {
  edges <- .morie_ml_edges(dag)
  bid <- as.matrix(bidirected)
  bkey <- if (nrow(bid)) {
    sort(unique(apply(bid, 1, function(r) paste(sort(as.character(r)), collapse = "\r"))))
  } else {
    character(0)
  }
  direct <- .morie_ml_adj(matrix(c(x, y), ncol = 2), x, y) &&
    any(edges[, 1] == x & edges[, 2] == y)
  confounded <- paste(sort(c(x, y)), collapse = "\r") %in% bkey
  bows <- character(0)
  if (nrow(edges)) {
    for (i in seq_len(nrow(edges))) {
      if (paste(sort(edges[i, ]), collapse = "\r") %in% bkey) {
        bows <- c(bows, paste(edges[i, ], collapse = "\r"))
      }
    }
  }
  bows <- sort(bows)
  acyclic <- .morie_ml_acyclic(edges)
  list(isbow = direct && confounded, direct = direct, confounded = confounded,
       nbows = length(bows), bowfree = length(bows) == 0L, acyclic = acyclic,
       identified = acyclic && length(bows) == 0L, bows = bows)
}

# --- ch. 5, pp. 82-85: colliders and Markov equivalence ---------------

#' Collider structures (immoralities, v-structures), ch. 5 p. 82
#' @param dag directed edge matrix
#' @param triple optional c(a, c, b) to test as a collider at c
#' @return list(ncolliders, colliders, iscollider, shielded, nedges)
#' @export
morie_collider <- function(dag, triple = NULL) {
  edges <- .morie_ml_edges(dag)
  cols <- .morie_ml_colliders(edges)
  hit <- FALSE
  shielded <- FALSE
  if (!is.null(triple)) {
    a <- triple[1]; cc <- triple[2]; b <- triple[3]
    hit <- paste(a, cc, b, sep = "\r") %in% cols ||
      paste(b, cc, a, sep = "\r") %in% cols
    shielded <- any(edges[, 1] == a & edges[, 2] == cc) &&
      any(edges[, 1] == b & edges[, 2] == cc) && .morie_ml_adj(edges, a, b)
  }
  list(ncolliders = length(cols), colliders = cols, iscollider = hit,
       shielded = shielded, nedges = nrow(edges))
}

#' Markov equivalence, ch. 5 p. 85 (Verma and Pearl, 1991)
#'
#' Two DAGs are Markov equivalent iff they share a skeleton and a set
#' of colliders.
#' @param dag1,dag2 directed edge matrices
#' @return list(equivalent, sameskeleton, samecolliders, nskeleton,
#'   ncolliders1, ncolliders2)
#' @export
morie_mectest <- function(dag1, dag2) {
  e1 <- .morie_ml_edges(dag1); e2 <- .morie_ml_edges(dag2)
  s1 <- .morie_ml_skeleton(e1); s2 <- .morie_ml_skeleton(e2)
  c1 <- .morie_ml_colliders(e1); c2 <- .morie_ml_colliders(e2)
  list(equivalent = identical(s1, s2) && identical(c1, c2),
       sameskeleton = identical(s1, s2), samecolliders = identical(c1, c2),
       nskeleton = length(s1), ncolliders1 = length(c1),
       ncolliders2 = length(c2))
}

# --- ch. 6, p. 119: the three rules of do-calculus ---------------------

#' Which of the three rules of do-calculus applies (p. 119)
#'
#' Notation as printed: an overline on X drops every edge INTO X, an
#' underline on Z drops every edge OUT OF Z.
#' Rule 1 (ignore an observation): (Y indep Z | X, W) in G_Xbar.
#' Rule 2 (intervention as observation): in G_Xbar,Zunder.
#' Rule 3 (ignore an intervention): in G_Xbar,Zbar(W), where Z(W) is
#' the subset of Z that are not ancestors of any W node in G_Xbar.
#' @param dag directed edge matrix
#' @param y outcome node
#' @param z the single node being added or dropped
#' @param x intervened nodes
#' @param w further conditioning nodes
#' @return list(rule1, rule2, rule3, nrules, zwsize)
#' @export
morie_docalc <- function(dag, y, z, x = character(0), w = character(0)) {
  edges <- .morie_ml_edges(dag)
  x <- as.character(x); w <- as.character(w); zs <- as.character(z)
  if (length(zs) != 1L) {
    stop("this implementation checks one z node at a time.", call. = FALSE)
  }
  cond <- sort(unique(c(x, w)))
  allnodes <- .morie_ml_nodes(edges, c(y, zs, x, w))

  g1 <- .morie_ml_cutin(edges, x)
  r1 <- .morie_ml_dsep(g1, y, zs, cond, allnodes)

  g2 <- .morie_ml_cutout(.morie_ml_cutin(edges, x), zs)
  r2 <- .morie_ml_dsep(g2, y, zs, cond, allnodes)

  ch1 <- .morie_ml_children(g1, allnodes)
  anc <- character(0)
  for (nd in allnodes) {
    if (length(intersect(.morie_ml_desc(nd, ch1), w)) > 0L) anc <- c(anc, nd)
  }
  zw <- setdiff(zs, anc)
  g3 <- .morie_ml_cutin(.morie_ml_cutin(edges, x), zw)
  r3 <- .morie_ml_dsep(g3, y, zs, cond, allnodes)

  list(rule1 = r1, rule2 = r2, rule3 = r3,
       nrules = as.integer(r1) + as.integer(r2) + as.integer(r3),
       zwsize = length(zw))
}

#' The do-operator as graph surgery (modularity, ch. 7 p. 154)
#'
#' do(X = x) deletes every edge into X and leaves every other
#' structural equation untouched.
#' @param dag directed edge matrix
#' @param x intervened node(s)
#' @return list(edges, removed, nremoved, nkept, nnodes)
#' @export
morie_dointerv <- function(dag, x) {
  edges <- .morie_ml_edges(dag)
  xs <- as.character(x)
  kept <- .morie_ml_cutin(edges, xs)
  list(edges = kept, removed = nrow(edges) - nrow(kept),
       nremoved = nrow(edges) - nrow(kept), nkept = nrow(kept),
       nnodes = length(.morie_ml_nodes(edges, xs)))
}

# --- ch. 6: d-separation ----------------------------------------------

#' d-separation of x and y given z (ch. 6)
#'
#' Mirrors morie.fn._dsep.d_separated; the counts come from the same
#' path enumeration so a caller can see why the answer came out as it
#' did.
#' @param dag directed edge matrix
#' @param x,y the two nodes
#' @param z conditioning set
#' @return list(dseparated, npaths, nnodes, ncond)
#' @export
morie_dseptest <- function(dag, x, y, z = character(0)) {
  edges <- .morie_ml_edges(dag)
  nodes <- .morie_ml_nodes(edges, c(x, y, z))
  ch <- .morie_ml_children(edges, nodes)
  pa <- .morie_ml_parents(edges, nodes)
  ps <- .morie_ml_paths(x, y, ch, pa)
  list(dseparated = .morie_ml_dsep(edges, x, y, as.character(z), nodes),
       npaths = length(ps), nnodes = length(nodes),
       ncond = length(as.character(z)))
}

# --- ch. 5, p. 77: the faithfulness assumption -------------------------

#' Faithfulness for one triple (ch. 5 p. 77)
#'
#' The printed formulation is X indep_P Y | Z implies X indep_G Y | Z:
#' an independence in the DISTRIBUTION must be reflected in the GRAPH.
#' @param dag directed edge matrix
#' @param x,y the two nodes
#' @param z conditioning set
#' @param indep the observed distributional independence
#' @return list(dseparated, indep, faithful, markov, violation)
#' @export
morie_faithchk <- function(dag, x, y, z = character(0), indep = TRUE) {
  sep <- .morie_ml_dsep(.morie_ml_edges(dag), x, y, as.character(z))
  indep <- isTRUE(indep)
  list(dseparated = sep, indep = indep, faithful = (!indep) || sep,
       markov = (!sep) || indep, violation = indep && !sep)
}

# --- ch. 13, p. 354: HSIC ---------------------------------------------

#' .morie_ml_gram
#'
#' A step of the causal_molak implementation. Called by \code{morie_hsicstat}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; its length is taken.
#' @param sigma Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.morie_ml_gram <- function(a, sigma = NULL) {
  a <- as.numeric(a)
  d2 <- outer(a, a, function(p, q) (p - q)^2)
  if (is.null(sigma)) {
    n <- length(a)
    off <- d2[upper.tri(d2)]
    med <- if (length(off)) stats::median(off) else 1
    sigma <- sqrt(max(med, 1e-12) / 2)
  }
  exp(-d2 / (2 * sigma^2))
}

#' Hilbert-Schmidt independence criterion for an ANM residual test
#'
#' The corpus copy only CALLS gCastle's hsic_test (ch. 13, p. 354) and
#' prints no formula; the book's own citation for the criterion is
#' Gretton et al. (2007). The estimator is the biased V-statistic
#' tr(K H L H)/n^2 with RBF Gram matrices and the median-heuristic
#' bandwidth, mirroring morie.fn.anmod.hsic. threshold is
#' caller-supplied; no null distribution is simulated.
#' @param a,b the two samples
#' @param sigma_a,sigma_b RBF bandwidths; NULL uses the median heuristic
#' @param threshold cutoff below which the samples are called independent
#' @return list(hsic, nhsic, independent, n)
#' @export
morie_hsicstat <- function(a, b, sigma_a = NULL, sigma_b = NULL,
                           threshold = 0.01) {
  a <- as.numeric(a); b <- as.numeric(b)
  if (length(a) != length(b)) {
    stop("a and b must be the same length.", call. = FALSE)
  }
  n <- length(a)
  if (n < 4L) stop("HSIC needs at least 4 observations.", call. = FALSE)
  k <- .morie_ml_gram(a, sigma_a)
  l <- .morie_ml_gram(b, sigma_b)
  h <- diag(n) - 1 / n
  stat <- sum(diag(k %*% h %*% l %*% h)) / (n * n)
  list(hsic = stat, nhsic = stat * n, independent = stat < threshold, n = n)
}

# --- ch. 2, p. 15: the Ladder of Causation -----------------------------

#' One rung of the Ladder of Causation (Table 2.1, ch. 2 p. 15)
#'
#' The rung names, actions, and questions are transcribed from the
#' printed table. needsgraph and needsscm follow the book's own account
#' of what each rung requires.
#' @param rung 1, 2, or 3
#' @return list(level, name, action, question, needsgraph, needsscm)
#' @export
morie_causrung <- function(rung) {
  rung <- as.integer(rung)
  tab <- list(
    "1" = c("Association", "Observing",
            "How does observing X change my belief in Y?"),
    "2" = c("Intervention", "Doing", "What will happen to Y if I do X?"),
    "3" = c("Counterfactual", "Imagining",
            "If I had done X, what would Y be?"))
  key <- as.character(rung)
  if (!(key %in% names(tab))) {
    stop("rung must be 1, 2 or 3.", call. = FALSE)
  }
  r <- tab[[key]]
  list(level = rung, name = r[1], action = r[2], question = r[3],
       needsgraph = rung >= 2L, needsscm = rung >= 3L)
}

# --- ch. 7, p. 157: the positivity assumption --------------------------

#' Positivity: every treatment value has positive probability in every
#' covariate stratum (ch. 7 p. 157)
#' @param treat treatment values, one per unit
#' @param stratum covariate stratum labels, one per unit
#' @param tol the threshold a cell probability must exceed
#' @return list(minprob, holds, ncells, nstrata, nlevels)
#' @export
morie_poschk <- function(treat, stratum, tol = 0) {
  treat <- as.character(treat); stratum <- as.character(stratum)
  if (length(treat) != length(stratum) || length(treat) == 0L) {
    stop("treat and stratum must be non-empty and equal length.", call. = FALSE)
  }
  levels_ <- sort(unique(treat))
  strata <- sort(unique(stratum))
  cells <- numeric(0)
  for (s in strata) {
    idx <- which(stratum == s)
    for (lv in levels_) {
      cells <- c(cells, sum(treat[idx] == lv) / length(idx))
    }
  }
  mn <- min(cells)
  list(minprob = mn, holds = mn > tol, ncells = length(cells),
       nstrata = length(strata), nlevels = length(levels_))
}

# --- the R-learner ----------------------------------------------------

#' Residualized (Robinson-style) CATE estimator
#'
#' NOT LOCATED IN THE EXTRACTED TEXT of the corpus copy of Molak, which
#' covers the S-, T-, X- and DR-learners but has no R-learner section.
#' The estimator is taken from the primary source. Robinson
#' decomposition, eq. (1): "Y_i - m*(X_i) = {W_i - e*(X_i)} tau*(X_i) +
#' eps_i"; R-learner objective, eq. (4): "tau_hat(.) = argmin_tau
#' \[L_hat_n{tau(.)} + Lambda_n{tau(.)}\]" with "L_hat_n{tau(.)} = (1/n)
#' sum_i \[{Y_i - m_hat^(-q(i))(X_i)} - {W_i - e_hat^(-q(i))(X_i)}
#' tau(X_i)]^2" -- Nie, X. and Wager, S. (2021), Quasi-Oracle
#' Estimation of Heterogeneous Treatment Effects, Biometrika
#' 108(2):299-319 (arXiv:1712.04912). Computed with Lambda_n = 0 and
#' the cross-fitted nuisances supplied BY THE CALLER, so the fold
#' assignment -- the only random ingredient -- lives outside.
#' @param y outcomes
#' @param t treatments
#' @param m caller-supplied outcome predictions
#' @param e caller-supplied propensity predictions
#' @param x optional basis matrix for a linear tau(X)
#' @return list(tau, ate, loss, n, k)
#' @export
morie_rlearn <- function(y, t, m, e, x = NULL) {
  y <- as.numeric(y); t <- as.numeric(t)
  m <- as.numeric(m); e <- as.numeric(e)
  n <- length(y)
  if (n == 0L || length(t) != n || length(m) != n || length(e) != n) {
    stop("y, t, m, e must be non-empty and the same length.", call. = FALSE)
  }
  ry <- y - m
  rt <- t - e
  if (is.null(x)) {
    den <- sum(rt * rt)
    if (den <= 0) stop("residualized treatment has no variation.", call. = FALSE)
    tau <- sum(rt * ry) / den
    pred <- rep(tau, n)
  } else {
    basis <- cbind(rep(1, n), as.matrix(x))
    if (nrow(basis) != n) {
      stop("x must have one row per observation.", call. = FALSE)
    }
    design <- basis * rt
    coefs <- as.numeric(.morie_ml_pinv(design) %*% ry)
    tau <- coefs
    pred <- as.numeric(basis %*% coefs)
  }
  list(tau = tau, ate = sum(pred) / n, loss = sum((ry - rt * pred)^2),
       n = n, k = length(tau))
}

# --- separating sets ---------------------------------------------------

#' .morie_ml_combn
#'
#' A step of the causal_molak implementation. Called by \code{morie_sepset}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param seq_ A vector; its length is taken and its elements indexed.
#' @param k Numeric; combined arithmetically in the body.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_ml_combn <- function(seq_, k) {
  if (k == 0L) return(list(character(0)))
  if (length(seq_) < k) return(list())
  out <- list()
  for (i in seq_len(length(seq_) - k + 1L)) {
    for (rest in .morie_ml_combn(seq_[-seq_len(i)], k - 1L)) {
      out[[length(out) + 1L]] <- c(seq_[i], rest)
    }
  }
  out
}

#' Smallest set that d-separates x from y, searched in a fixed order
#'
#' The corpus copy discusses conditioning sets that block paths (ch. 6)
#' but prints no named separating-set definition, so the search rule is
#' stated here: candidates are drawn from the union of the adjacencies
#' of x and y, in sorted order and increasing size, and the FIRST
#' separating set found is returned.
#' @param dag directed edge matrix
#' @param x,y the two nodes
#' @param maxsize largest candidate set considered
#' @return list(found, size, sepset, ntested, nnodes)
#' @export
morie_sepset <- function(dag, x, y, maxsize = 3L) {
  edges <- .morie_ml_edges(dag)
  nodes <- .morie_ml_nodes(edges, c(x, y))
  adj <- character(0)
  if (nrow(edges)) {
    adj <- sort(unique(c(edges[edges[, 1] %in% c(x, y), 2],
                         edges[edges[, 2] %in% c(x, y), 1])))
  }
  cand <- setdiff(adj, c(x, y))
  if (.morie_ml_adj(edges, x, y)) {
    return(list(found = FALSE, size = -1L, sepset = character(0),
                ntested = 0L, nnodes = length(nodes)))
  }
  tested <- 0L
  top <- min(as.integer(maxsize), length(cand))
  for (size in 0:top) {
    for (comb in .morie_ml_combn(cand, size)) {
      tested <- tested + 1L
      if (.morie_ml_dsep(edges, x, y, comb, nodes)) {
        return(list(found = TRUE, size = as.integer(size), sepset = comb,
                    ntested = tested, nnodes = length(nodes)))
      }
    }
  }
  list(found = FALSE, size = -1L, sepset = character(0), ntested = tested,
       nnodes = length(nodes))
}

# --- ch. 7, p. 164: SUTVA ----------------------------------------------

#' SUTVA: no interference between units, one version of treatment
#'
#' The printed statement (ch. 7 p. 164) is that the fact that one unit
#' receives treatment does not influence any other units.
#' @param interference square matrix whose off-diagonal (i, j) is how
#'   much unit i's treatment moves unit j's outcome
#' @param versions number of distinct versions of the treatment
#' @param tol largest tolerated off-diagonal magnitude
#' @return list(maxinterference, nointerference, consistent, holds, n)
#' @export
morie_sutvachk <- function(interference, versions = 1L, tol = 0) {
  mat <- as.matrix(interference)
  storage.mode(mat) <- "double"
  n <- nrow(mat)
  if (n == 0L || ncol(mat) != n) {
    stop("interference must be a square matrix.", call. = FALSE)
  }
  off <- abs(mat[row(mat) != col(mat)])
  mx <- if (length(off)) max(off) else 0
  versions <- as.integer(versions)
  list(maxinterference = mx, nointerference = mx <= tol,
       consistent = versions == 1L, holds = (mx <= tol) && versions == 1L,
       n = n)
}
