# Areal difference boundaries by Dirichlet-process clustering.
# Sources: Li, P., Banerjee, S., Hanson, T. A. and McBean, A. M.
# (2015), Bayesian Models for Detecting Difference Boundaries in
# Areal Data, Statistica Sinica 25(1), 385-402 (areal data,
# conditional autoregressive models, difference boundaries and
# wombling; the Dirichlet process mixture prior on the areal random
# effects, with G ~ DP(alpha, G_0); the stick-breaking representation
# of the DP (Sethuraman, 1994) and the Blackwell-MacQueen generalized
# Polya urn scheme giving an explicit prediction rule and effective
# sampling strategies; and the detection of difference boundaries
# through the induced clustering of neighbouring regions); Sethuraman,
# J. (1994), A Constructive Definition of Dirichlet Priors, Statistica
# Sinica 4(2), 639-650 (the stick-breaking representation);
# Besag, J. (1974), Spatial Interaction and the Statistical Analysis
# of Lattice Systems, JRSS-B 36(2), 192-236 (the conditional
# autoregressive model); Womble, W. H. (1951), Differential
# Systematics, Science 114(2961), 315-322 (the boundary problem the
# method is named for).
#
# Native implementation mirroring morie.fn.dpgrf exactly: the same
# adjacency-pair enumeration (only upper-triangular nonzeros are
# adjacent pairs), the same precision matrix tau*(D - rho*W) with the
# same validity checks, the same coclustering estimator (a count of
# draws where two regions share a label, divided by the number of
# draws), and the same posterior boundary probabilities
# 1 - coclustering[i, j] for adjacent pairs, ranked by descending
# probability.

#' Adjacent pairs of regions
#'
#' Enumerates the unordered pairs of regions with a nonzero entry in
#' the (symmetric) adjacency matrix. Only \code{i < j} pairs are
#' returned, so each boundary sits on exactly one pair.
#'
#' @param W Square symmetric adjacency matrix (n x n).
#' @return A list with \code{pairs} (list of \code{c(i, j)} with
#'   \code{i < j}), \code{n_pairs}, \code{n_regions}, \code{degrees}
#'   (row sums).
#' @keywords internal
#' @noRd
.dpgrf_adjacency_pairs <- function(W) {
  A <- matrix(as.numeric(W), nrow = nrow(as.matrix(W)))
  n <- nrow(A)
  if (n != ncol(A))
    stop("dpgrf: the adjacency matrix is not square")
  if (any(abs(A - t(A)) > 1e-12))
    stop("dpgrf: the adjacency matrix must be symmetric")
  pairs <- list()
  for (i in seq_len(n - 1L)) {
    for (j in seq.int(i + 1L, n)) {
      if (A[i, j] != 0) pairs[[length(pairs) + 1L]] <- c(i, j)
    }
  }
  list(pairs = pairs, n_pairs = length(pairs), n_regions = n,
       degrees = as.numeric(rowSums(A)))
}

#' Conditional autoregressive precision matrix
#'
#' Returns \code{tau * (D - rho * W)} with \code{D} the diagonal of
#' row sums of \code{W}. With \code{rho = 1} the matrix is the
#' intrinsic CAR and is singular, which the model has to live with
#' rather than hide.
#'
#' @param W Square symmetric adjacency matrix (n x n).
#' @param rho Spatial correlation in \code{\[0, 1\]}.
#' @param tau Positive scale.
#' @return A list with \code{Q}, \code{degrees}, \code{rho},
#'   \code{min_eigenvalue}, \code{singular}, \code{note}.
#' @keywords internal
#' @noRd
.dpgrf_car_precision <- function(W, rho = 0.99, tau = 1.0) {
  A <- matrix(as.numeric(W), nrow = nrow(as.matrix(W)))
  n <- nrow(A)
  if (n != ncol(A))
    stop("dpgrf: the adjacency matrix is not square")
  if (any(abs(A - t(A)) > 1e-12))
    stop("dpgrf: the adjacency matrix must be symmetric")
  r_ <- as.numeric(rho)
  if (r_ < 0 || r_ > 1)
    stop("dpgrf: rho must lie in [0,1]")
  tau_ <- as.numeric(tau)
  D <- as.numeric(rowSums(A))
  if (any(D <= 0))
    stop("dpgrf: a region has no neighbours, so its conditional variance is undefined")
  Q <- tau_ * (diag(D) - r_ * A)
  vals <- eigen(Q, symmetric = TRUE, only.values = TRUE)$values
  list(Q = Q, degrees = D, rho = r_,
       min_eigenvalue = min(vals),
       singular = min(vals) < 1e-9,
       note = "rho = 1 is the intrinsic CAR and IS singular; it is a proper prior only up to a constant")
}

#' Sample cluster labels from the Polya urn
#'
#' Mirrors \code{urn.sample_urn}: at each step either start a new
#' cluster with probability \code{alpha / (alpha + n)} or repeat an
#' existing one with probability proportional to its current count.
#' Ties occur with POSITIVE probability, which is the whole reason
#' the DP clusters.
#'
#' @param n_regions Number of regions (positive integer).
#' @param alpha Positive concentration.
#' @param rng Generator environment from \code{.ghc_rng} (or
#'   \code{NULL} to build one with \code{seed = 0}).
#' @param seed Seed for the generator shared with the Python arm.
#' @return A list with \code{labels}, \code{counts},
#'   \code{n_clusters}, \code{alpha}, \code{note}.
#' @keywords internal
#' @noRd
.dpgrf_sample_labels <- function(n_regions, alpha, rng = NULL,
                                 seed = 0) {
  N <- as.integer(n_regions)
  if (N < 1L) stop("dpgrf: n_regions must be at least 1")
  a <- as.numeric(alpha)
  if (a <= 0) stop("dpgrf: alpha must be positive")
  e <- if (is.null(rng)) .ghc_rng(seed) else rng
  counts <- numeric(0)
  labels <- integer(0)
  for (k in seq_len(N)) {
    if (length(counts) == 0L) {
      counts <- c(counts, 1)
      labels <- c(labels, 0L)
      next
    }
    n_so_far <- sum(counts)
    weights <- c(counts, a)
    wsum <- sum(weights)
    u <- .ghc_unif(e, 1L) * wsum
    acc <- 0.0
    chosen <- NA_integer_
    cs <- cumsum(weights)
    for (j in seq_along(cs)) {
      acc <- cs[j]
      if (u <= acc) { chosen <- j - 1L
      break }
    }
    if (is.na(chosen)) {
      counts <- c(counts, 1)
      labels <- c(labels, length(counts) - 1L)
    } else {
      counts[chosen + 1L] <- counts[chosen + 1L] + 1
      labels <- c(labels, as.integer(chosen))
    }
  }
  list(labels = as.integer(labels), counts = as.numeric(counts),
       n_clusters = length(counts), alpha = a,
       note = "ties occur with POSITIVE probability, which is why the DP clusters")
}

#' Posterior coclustering probabilities from sampled labels
#'
#' For each pair \code{(i, j)} returns the fraction of label draws
#' in which regions \code{i} and \code{j} share a label. The result
#' is symmetric with a unit diagonal by construction -- a cheap
#' invariant that catches an indexing error.
#'
#' @param label_draws List of integer label vectors, each of length n.
#' @return A list with \code{matrix} (n x n), \code{n_draws}, \code{n},
#'   \code{symmetric}, \code{unit_diagonal}.
#' @keywords internal
#' @noRd
.dpgrf_coclustering <- function(label_draws) {
  if (length(label_draws) == 0L)
    stop("dpgrf: no label draws given")
  L <- lapply(label_draws, as.integer)
  n <- length(L[[1L]])
  if (n == 0L)
    stop("dpgrf: no label draws given")
  if (any(vapply(L, length, integer(1)) != n))
    stop("dpgrf: the draws differ in length")
  M <- matrix(0, nrow = n, ncol = n)
  for (d in L) {
    same <- outer(d, d, "==") * 1.0
    M <- M + same
  }
  nd <- length(L)
  M <- M / nd
  sym <- all(abs(M - t(M)) < 1e-12)
  diag_ok <- all(abs(diag(M) - 1) < 1e-12)
  list(matrix = M, n_draws = nd, n = n,
       symmetric = sym, unit_diagonal = diag_ok)
}

#' Continuous-prior tie probability
#'
#' The whole problem in one number: under any continuous prior the
#' posterior probability that two regions are exactly equal is 0, so
#' a boundary can only be defined by thresholding a difference.
#'
#' @return A list with \code{probability} and \code{note}.
#' @keywords internal
#' @noRd
.dpgrf_continuous_prior_tie_probability <- function() {
  list(probability = 0,
       note = "a continuous prior assigns no mass to a point, so 'are these two equal?' cannot be answered -- which is why the DP is used here")
}

#' Areal difference boundaries by Dirichlet-process clustering
#'
#' Computes the posterior probability that two adjacent regions
#' differ -- \code{P(phi_i != phi_j)} -- from a set of sampled
#' cluster labels, then flags the pairs whose probability exceeds a
#' threshold. The probability is a posterior statement about cluster
#' membership under a Dirichlet-process mixture prior on the areal
#' random effects, NOT a thresholded difference of continuous values,
#' which is the whole point of using a DP rather than a plain CAR.
#'
#' @param W Square symmetric adjacency matrix (n x n).
#' @param label_draws List of integer label vectors, each of length n.
#' @param threshold Probability threshold above which a pair is
#'   declared a boundary.
#' @return A list with \code{estimate} (boundary pairs), \code{boundaries}
#'   (same as \code{estimate}), \code{ranked} (list of one entry per
#'   adjacent pair, sorted by descending \code{p_difference}),
#'   \code{n_adjacent}, \code{n_boundaries}, \code{threshold},
#'   \code{method}, \code{note}.
#' @references Li, P. et al. (2015). Bayesian Models for Detecting
#'   Difference Boundaries in Areal Data. Statistica Sinica, 25(1),
#'   385-402.
#' @export
morie_dpgrf <- function(W, label_draws, threshold = 0.5) {
  pairs <- .dpgrf_adjacency_pairs(W)$pairs
  co <- .dpgrf_coclustering(label_draws)$matrix
  thr <- as.numeric(threshold)
  rows <- lapply(pairs, function(p) {
    p_diff <- 1 - co[p[1L], p[2L]]
    list(pair = p, p_difference = p_diff,
         boundary = p_diff > thr)
  })
  rows <- rows[order(-vapply(rows, function(r) r$p_difference,
                              numeric(1)))]
  flagged <- vapply(rows, function(r) r$boundary, logical(1))
  list(estimate = lapply(rows[flagged], function(r) r$pair),
       boundaries = lapply(rows[flagged], function(r) r$pair),
       ranked = rows,
       n_adjacent = length(pairs),
       n_boundaries = sum(flagged),
       threshold = thr,
       method = "areal difference boundaries by DP clustering; Li, Banerjee, Hanson & McBean (2015)",
       note = "each number is a posterior probability that two adjacent regions DIFFER, not a rescaled gap")
}
