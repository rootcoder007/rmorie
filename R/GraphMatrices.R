#' Adjacency and non-backtracking matrices of a graph
#'
#' R arm of \code{morie.fn.sgtadj} and \code{sgtnbe} (\code{sgtsbnd} and
#' \code{prnkpg} have their own files). \code{Sgtadj}: adjacency matrix of an edge list (labels in
#' sorted order, or integer labels as indices when \code{n} is given;
#' duplicate edges collapse, undirected edges are symmetrised).
#' \code{Sgtnbe}: the Hashimoto non-backtracking matrix on the directed edge
#' set, \eqn{B_{ef} = 1} when f continues where e ends without walking back.
#'
#' @param edges List of length-2 vectors, or a two-column matrix, of node
#'   labels.
#' @param n Number of nodes (optional).
#' @param directed Keep edges one way.
#' @return A list (the Python payload).
#' @references Hashimoto, K. (1989). Zeta functions of finite graphs and
#'   representations of p-adic groups. Advanced Studies in Pure Mathematics
#'   15, 211-280.
#'
#'   Krzakala, F., Moore, C., Mossel, E., Neeman, J., Sly, A., Zdeborova, L.
#'   and Zhang, P. (2013). Spectral redemption in clustering sparse
#'   networks. PNAS 110, 20935-20940.
#' @examples
#' Sgtadj(list(c("A", "B"), c("B", "C")))$A
#' Sgtnbe(list(c(0, 1), c(1, 2)))$B
#' @export
Sgtadj <- function(edges, n = NULL, directed = FALSE) {
  if (is.matrix(edges)) edges <- lapply(seq_len(nrow(edges)), function(i) edges[i, ])
  if (any(lengths(edges) != 2)) stop("every edge must be a pair")
  labs <- unique(unlist(edges))
  labs <- labs[order(as.character(labs), method = "radix")]
  if (is.null(n)) {
    idx <- stats::setNames(seq_along(labs) - 1L, as.character(labs))
    size <- length(labs)
  } else {
    size <- as.integer(n)
    if (is.numeric(labs)) {
      if (length(labs) && (min(labs) < 0 || max(labs) >= size)) stop(sprintf("integer labels must lie in [0, %d]", size - 1))
      idx <- stats::setNames(as.integer(labs), as.character(labs))
    } else {
      if (length(labs) > size) stop(sprintf("n=%d but %d distinct labels", size, length(labs)))
      idx <- stats::setNames(seq_along(labs) - 1L, as.character(labs))
    }
  }
  A <- matrix(0, size, size)
  for (e in edges) {
    u <- idx[[as.character(e[[1]])]] + 1L
    v <- idx[[as.character(e[[2]])]] + 1L
    A[u, v] <- 1
    if (!directed) A[v, u] <- 1
  }
  key <- vapply(edges, function(e) {
    s <- as.character(unlist(e))
    if (directed) paste(s, collapse = "\r") else paste(sort(unique(s), method = "radix"), collapse = "\r")
  }, "")
  list(A = A, nodes = idx, degree = rowSums(A), n = size, m = length(unique(key)), directed = directed)
}

#' @rdname Sgtadj
#' @export
Sgtnbe <- function(edges, n = NULL) {
  A <- Sgtadj(edges, n = n, directed = FALSE)$A
  size <- nrow(A)
  de <- which(t(A) > 0 & t(row(A) != col(A)), arr.ind = TRUE)[, 2:1, drop = FALSE]
  de <- de[order(de[, 1], de[, 2]), , drop = FALSE]
  m2 <- nrow(de)
  B <- matrix(0, m2, m2)
  key <- paste(de[, 1], de[, 2])
  for (i in seq_len(m2)) {
    u <- de[i, 1]
    v <- de[i, 2]
    for (w in seq_len(size)) {
      if (A[v, w] > 0 && w != u && w != v) B[i, match(paste(v, w), key)] <- 1
    }
  }
  list(B = B, directed_edges = lapply(seq_len(m2), function(i) de[i, ] - 1L), n = size, m = m2 %/% 2)
}
