# SPDX-License-Identifier: AGPL-3.0-or-later
#' Weisfeiler-Lehman subtree graph kernel
#'
#' Shervashidze, Schweitzer, van Leeuwen, Mehlhorn and Borgwardt (2011),
#' Weisfeiler-Lehman graph kernels, JMLR 12, 2539-2561 (FETCHED as PDF
#' from jmlr.org).  Algorithm 1 gives one iteration of the 1-dimensional
#' Weisfeiler-Lehman test: assign each node the multiset of its
#' neighbours' previous labels, sort it, prepend the node's own previous
#' label, and compress to a fresh label.  Equation (2) is the kernel,
#' k^(h)(G, G') = <phi^(h)(G), phi^(h)(G')>, whose coordinates are the
#' counts of every original and compressed label over iterations 0..h --
#' the kernel "counts common original and compressed labels in two
#' graphs".  The label alphabet is shared between the graphs, and
#' compressed labels are allocated in order of first appearance, so the
#' run is deterministic.
#'
#' @param G1,G2 adjacency matrices.
#' @param K number of WL iterations h.
#' @param labels1,labels2 initial node labels; all-equal by default.
#' @param normalize divide by sqrt(k(G1,G1) k(G2,G2)).
#' @return list: estimate, kernel, per_iter, n_labels, method.
#' @keywords internal
#' @examples
#' A <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
#' Wlkernel(A, A, 1)$kernel
#' @export
Wlkernel <- function(G1, G2, K = 3, labels1 = NULL, labels2 = NULL,
                     normalize = FALSE) {
  adj <- function(G) {
    A <- .s03mat(G)
    n <- nrow(A)
    nb <- vector("list", n)
    for (i in seq_len(n)) {
      v <- integer(0)
      for (j in seq_len(n)) if (i != j && A[i, j] != 0) v <- c(v, j)
      nb[[i]] <- v
    }
    nb
  }
  nb1 <- adj(G1)
  nb2 <- adj(G2)
  n1 <- length(nb1)
  n2 <- length(nb2)
  l1 <- if (!is.null(labels1)) as.character(labels1) else rep("0", n1)
  l2 <- if (!is.null(labels2)) as.character(labels2) else rep("0", n2)
  alpha <- character(0)
  code <- function(s) {
    i <- match(s, alpha)
    if (is.na(i)) { alpha[[length(alpha) + 1L]] <<- s
    i <- length(alpha) }
    i - 1L
  }
  l1 <- vapply(l1, function(s) as.character(code(s)), "")
  l2 <- vapply(l2, function(s) as.character(code(s)), "")
  total <- 0
  per <- numeric(0)
  for (it in seq_len(as.integer(K) + 1L) - 1L) {
    u1 <- sort(unique(l1))
    u2 <- sort(unique(l2))
    keys <- sort(unique(c(u1, u2)))
    dot <- 0
    for (s in keys) dot <- dot + sum(l1 == s) * sum(l2 == s)
    per <- c(per, dot)
    total <- total + dot
    if (it == as.integer(K)) break
    n1l <- character(n1)
    n2l <- character(n2)
    for (v in seq_len(n1)) {
      ms <- sort(l1[nb1[[v]]])
      n1l[v] <- as.character(code(paste0(l1[v], ",", paste(ms, collapse = "|"))))
    }
    for (v in seq_len(n2)) {
      ms <- sort(l2[nb2[[v]]])
      n2l[v] <- as.character(code(paste0(l2[v], ",", paste(ms, collapse = "|"))))
    }
    l1 <- n1l
    l2 <- n2l
  }
  est <- total
  if (normalize) {
    s1 <- Wlkernel(G1, G1, K, labels1, labels1)$estimate
    s2 <- Wlkernel(G2, G2, K, labels2, labels2)$estimate
    d <- sqrt(s1 * s2)
    est <- if (d > 0) total / d else NaN
  }
  list(estimate = est, kernel = total, per_iter = per,
       n_labels = length(alpha),
       method = "Weisfeiler-Lehman subtree kernel (Shervashidze et al. 2011, eq. 2)")
}
