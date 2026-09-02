# SPDX-License-Identifier: AGPL-3.0-or-later

#' GEM stick-breaking weights
#'
#' Formula: GEM(alpha) = stick-breaking with V_k ~ Beta(1, alpha)
#'
#' w_1 = V_1 and w_k = V_k prod_\{j<k\} (1 - V_j), so the weights and the
#' unbroken remainder sum to exactly one at every truncation.  The
#' marginal mean of the first weight is 1/(1 + alpha), and the expected
#' remaining mass after K breaks is (alpha/(1 + alpha))^K.
#'
#' @param alpha Concentration, strictly positive.
#' @param K Truncation level.
#' @param seed Seed of the deterministic stream.
#' @return List with \code{estimate} (largest weight), \code{weights},
#'   \code{V}, \code{remaining}, \code{expected_remaining}, \code{K},
#'   \code{method}.
#' @references Sethuraman (1994), Statistica Sinica 4(2):639-650;
#'   Pitman (2002), Poisson-Dirichlet and GEM invariant distributions,
#'   Technical Report 621, U.C. Berkeley.
#' @export
#' @examples
#' Dpgem()
Dpgem <- function(alpha = 1, K = 10, seed = 42) {
  if (!(alpha > 0)) stop("alpha must be strictly positive")
  K <- as.integer(K)
  if (K < 1L) stop("K must be at least 1")
  e <- .ghc_rng(seed)
  V <- numeric(K); w <- numeric(K)
  rest <- 1
  for (k in seq_len(K)) {
    v <- .ghc_beta1(e, 1, alpha)
    V[k] <- v
    w[k] <- v * rest
    rest <- rest * (1 - v)
  }
  .t1_result(estimate = max(w), weights = w, V = V, remaining = rest,
             expected_remaining = (alpha / (1 + alpha))^K, K = K,
             method = "GEM stick-breaking weights")
}
