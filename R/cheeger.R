# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P3: a hot-spot boundary and the spectral gap of the street graph
# (research/lean/P3Cheeger.lean; Chung, Four proofs of the Cheeger inequality, Thm 1).
#
#   Research.P3.testVec_orth / testVec_dnorm / testVec_dirichlet
#        f = 1_S / vol S - 1_{S^c} / vol S^c is degree-orthogonal to constants,
#        D(f) = 1/vol S + 1/vol S^c, E(f) = cut S (1/vol S + 1/vol S^c)^2
#   Research.P3.rayleigh_testVec              E(f)/D(f) = cut S (1/vol S + 1/vol S^c)
#   Research.P3.rayleigh_le_two_conductance   <= 2 h(S)
#   Research.P3.lambda2_le_rayleigh_testVec / cheeger_easy   lambda_2 <= E(f)/D(f) <= 2 h(S)

#' Conductance of a hot-spot set and the Cheeger bound on the spectral gap
#'
#' For a weighted street graph (symmetric non-negative adjacency) and a set
#' of places \code{S}, reports the conductance
#' \eqn{h(S) = \mathrm{cut}(S)/\min(\mathrm{vol}\,S, \mathrm{vol}\,S^c)}, the
#' Rayleigh quotient of the two-valued test vector, which equals
#' \eqn{\mathrm{cut}(S)(1/\mathrm{vol}\,S + 1/\mathrm{vol}\,S^c)}
#' (\code{Research.P3.rayleigh_testVec}), and the second eigenvalue
#' \eqn{\lambda_2} of the normalised Laplacian. The proved chain is
#' \eqn{\lambda_2 \le E(f)/D(f) \le 2h(S)} (\code{cheeger_easy}): a set that
#' few patrol-crossings leave relative to its volume forces a small spectral
#' gap, so anything that diffuses along the graph (displacement, spillover)
#' leaves such a set slowly. With \code{exhaustive = TRUE} on a small graph
#' the Cheeger constant \eqn{h_G = \min_S h(S)} is found by enumeration.
#' @param adjacency Square symmetric matrix of non-negative weights.
#' @param S Indices (or a logical vector) of the places in the set.
#' @param exhaustive Also minimise \eqn{h(S)} over every non-trivial subset
#'   (graphs with at most 16 places).
#' @return A list with \code{cut}, \code{vol_S}, \code{vol_complement},
#'   \code{conductance}, \code{rayleigh_test}, \code{lambda2},
#'   \code{bound_holds} (\eqn{\lambda_2 \le E(f)/D(f) \le 2h(S)}),
#'   \code{cheeger_constant} and \code{argmin_set} when exhaustive, and
#'   \code{theorems}.
#' @examples
#' # two dense blocks joined by one edge
#' A <- matrix(0, 6, 6)
#' A[1, 2] <- A[1, 3] <- A[2, 3] <- A[4, 5] <- A[4, 6] <- A[5, 6] <- A[3, 4] <- 1
#' A <- A + t(A)
#' morie_cheeger_bound(A, S = 1:3)[c("conductance", "rayleigh_test", "lambda2", "bound_holds")]
#' @export
morie_cheeger_bound <- function(adjacency, S, exhaustive = FALSE) {
  A <- as.matrix(adjacency)
  n <- nrow(A)
  if (ncol(A) != n) stop("adjacency must be square", call. = FALSE)
  if (any(A < 0) || max(abs(A - t(A))) > 1e-12) stop("adjacency must be symmetric and non-negative", call. = FALSE)
  in_s <- if (is.logical(S)) S else seq_len(n) %in% S
  if (length(in_s) != n || !any(in_s) || all(in_s)) stop("S must be a non-empty proper subset of the places", call. = FALSE)
  d <- rowSums(A)
  if (any(d <= 0)) stop("every place needs positive degree", call. = FALSE)
  stats_of <- function(in_s) {
    vol_s <- sum(d[in_s])
    vol_c <- sum(d[!in_s])
    cut_s <- sum(A[in_s, !in_s])
    list(cut = cut_s, vol_s = vol_s, vol_c = vol_c, h = cut_s / min(vol_s, vol_c),
         rayleigh = cut_s * (1 / vol_s + 1 / vol_c))
  }
  st <- stats_of(in_s)
  # normalised Laplacian: its eigenvalues are the Rayleigh quotients E(f)/D(f) at the eigenvectors
  dis <- 1 / sqrt(d)
  L <- diag(n) - (dis %o% dis) * A
  lam <- sort(eigen((L + t(L)) / 2, symmetric = TRUE, only.values = TRUE)$values)
  lambda2 <- lam[2]
  out <- list(cut = st$cut, vol_S = st$vol_s, vol_complement = st$vol_c, conductance = st$h,
              rayleigh_test = st$rayleigh, lambda2 = lambda2,
              bound_holds = lambda2 <= st$rayleigh + 1e-10 && st$rayleigh <= 2 * st$h + 1e-10)
  if (isTRUE(exhaustive)) {
    if (n > 16L) stop("exhaustive search is limited to 16 places", call. = FALSE)
    best <- Inf
    arg <- NULL
    for (code in seq_len(2^(n - 1) - 1)) {            # subsets containing place 1 are omitted by symmetry
      m <- as.logical(bitwAnd(code, 2^(0:(n - 1))))
      h <- stats_of(m)$h
      if (h < best) {
        best <- h
        arg <- which(m)
      }
    }
    out$cheeger_constant <- best
    out$argmin_set <- arg
    out$cheeger_bound_holds <- lambda2 <= 2 * best + 1e-10
  }
  out$theorems <- c("Research.P3.testVec_orth", "Research.P3.testVec_dnorm", "Research.P3.testVec_dirichlet",
                    "Research.P3.rayleigh_testVec", "Research.P3.rayleigh_le_two_conductance",
                    "Research.P3.lambda2_le_rayleigh_testVec", "Research.P3.cheeger_easy")
  out
}
