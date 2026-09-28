.gs_adj <- function(A) {
  M <- as.matrix(A)
  storage.mode(M) <- "double"
  if (nrow(M) != ncol(M)) stop("adjacency must be square", call. = FALSE)
  M
}

#' Spectral graph indices and label-propagation communities
#'
#' \code{EstradaIndex}: \eqn{\sum_i e^{\lambda_i} = \mathrm{tr}\, e^A} (Estrada
#' 2000). \code{Communicability}: \eqn{G = e^A} from the eigen-decomposition,
#' with subgraph centralities (Estrada and Hatano 2008).
#' \code{ModularityMatrix}: \eqn{B = A - kk^\top/2m}, its leading eigenpair and
#' spectral bisection, and the modularity of a partition (Newman 2006).
#' \code{PerronFrobenius}: spectral radius and unit positive eigenvector.
#' \code{RandicIndex}: \eqn{\sum_{uv}(d_u d_v)^\alpha} (Randic 1975).
#' \code{LabelPropagation}: Raghavan et al. (2007) communities with Philox
#' visiting orders and tie-breaks. Identical to the Python arm
#' \code{morie.fn.graphspec}; memberships are 0-based as in Python.
#'
#' @param A Symmetric (weighted) adjacency matrix.
#' @param membership Community labels for the modularity.
#' @param alpha Randic exponent.
#' @param seed Philox seed.
#' @param max_iter Maximum sweeps.
#' @return Numeric or list.
#' @references Estrada, E. and Hatano, N. (2008). Communicability in complex
#'   networks. Physical Review E 77, 036111.
#'
#'   Newman, M. E. J. (2006). Modularity and community structure in networks.
#'   PNAS 103, 8577-8582.
#'
#'   Randic, M. (1975). Characterization of molecular branching. Journal of the
#'   American Chemical Society 97, 6609-6615.
#'
#'   Raghavan, U. N., Albert, R. and Kumara, S. (2007). Near linear time
#'   algorithm to detect community structures in large-scale networks. Physical
#'   Review E 76, 036106.
#' @examples
#' EstradaIndex(matrix(c(0, 1, 1, 0), 2))
#' PerronFrobenius(1 - diag(3))$eigenvalue
#' @export
EstradaIndex <- function(A) sum(exp(eigen(.gs_adj(A), symmetric = TRUE, only.values = TRUE)$values))

#' @rdname EstradaIndex
#' @export
Communicability <- function(A) {
  e <- eigen(.gs_adj(A), symmetric = TRUE)
  G <- e$vectors %*% diag(exp(e$values), length(e$values)) %*% t(e$vectors)
  list(matrix = G, subgraph_centrality = diag(G), estrada_index = sum(diag(G)))
}

#' @rdname EstradaIndex
#' @export
ModularityMatrix <- function(A, membership = NULL) {
  M <- .gs_adj(A)
  k <- rowSums(M)
  two_m <- sum(k)
  B <- M - outer(k, k) / two_m
  e <- eigen(B, symmetric = TRUE)
  lead <- e$vectors[, 1]
  if (lead[which.max(abs(lead))] < 0) lead <- -lead
  out <- list(matrix = B, leading_eigenvalue = e$values[1], leading_eigenvector = lead,
              bisection = as.integer(lead >= 0))
  if (!is.null(membership)) out$modularity <- sum(B[outer(membership, membership, "==")]) / two_m
  out
}

#' @rdname EstradaIndex
#' @export
PerronFrobenius <- function(A) {
  M <- .gs_adj(A)
  if (any(M < 0)) stop("matrix must be non-negative", call. = FALSE)
  e <- eigen(M, symmetric = TRUE)
  v <- e$vectors[, 1]
  if (sum(v) < 0) v <- -v
  v <- v / sqrt(sum(v^2))
  v[abs(v) < 1e-14] <- pmax(0, v[abs(v) < 1e-14])
  n <- length(e$values)
  second <- if (n > 1) max(abs(e$values[2]), abs(e$values[n])) else 0
  list(eigenvalue = e$values[1], eigenvector = v, ratio = if (second > 0) e$values[1] / second else Inf)
}

#' @rdname EstradaIndex
#' @export
RandicIndex <- function(A, alpha = -0.5) {
  M <- .gs_adj(A)
  d <- rowSums(M)
  idx <- which(upper.tri(M) & M != 0, arr.ind = TRUE)
  sum((d[idx[, 1]] * d[idx[, 2]])^alpha)
}

#' @rdname EstradaIndex
#' @export
LabelPropagation <- function(A, seed = 1L, max_iter = 100L) {
  M <- .gs_adj(A)
  n <- nrow(M)
  lab <- seq_len(n) - 1L
  it <- 0L
  for (it in seq_len(max_iter)) {
    u <- .morie_random_uniform(n, seed = seed, stream = 2 * it)
    ord <- seq_len(n)
    for (t in seq_len(n - 1)) {
      k <- t + floor(u[t] * (n - t + 1))
      tmp <- ord[t]
      ord[t] <- ord[k]
      ord[k] <- tmp
    }
    tie <- .morie_random_uniform(n, seed = seed, stream = 2 * it + 1)
    changed <- FALSE
    for (pos in seq_len(n)) {
      i <- ord[pos]
      nb <- which(M[i, ] != 0 & seq_len(n) != i)
      if (!length(nb)) next
      sc <- tapply(M[i, nb], lab[nb], sum)
      best <- max(sc)
      cand <- sort(as.integer(names(sc)[sc == best]))
      if (lab[i] %in% cand) next
      lab[i] <- cand[min(length(cand), floor(tie[pos] * length(cand)) + 1)]
      changed <- TRUE
    }
    if (!changed) break
  }
  mem <- match(lab, unique(lab)) - 1L
  list(membership = mem, n_communities = length(unique(lab)), modularity = ModularityMatrix(M, mem)$modularity,
       iterations = it)
}
