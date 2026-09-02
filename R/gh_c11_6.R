# SPDX-License-Identifier: AGPL-3.0-or-later
#' Brownian-motion prior
#'
#' Brownian motion has E W(t) = 0 and Cov(W(s), W(t)) = min(s, t), and
#' its sample paths are Holder-alpha for every alpha < 1/2.  Those path
#' properties are exactly what bound its concentration function, so the
#' covariance identity is worth checking directly: the empirical
#' covariance of simulated random walks on a grid must reproduce
#' min(s, t) and, on the diagonal, the variance s.
#'
#' Formula: W(t_i) = sum_\{j <= i\} Z_j / sqrt(n_grid), Z_j iid N(0, 1);
#'   Cov(W(s), W(t)) = min(s, t).
#'
#' @param n_grid Number of grid steps on \[0, 1\].
#' @param n_sim Number of simulated paths.
#' @param seed Seed for the deterministic draws.
#' @return List with \code{estimate} (empirical covariance),
#'   \code{theory_min_st}, \code{cov_gap}, \code{var_gap},
#'   \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, Example 11.5.
#' @export
#' @examples
#' Ghosalbmprior()
Ghosalbmprior <- function(n_grid = 200, n_sim = 400, seed = 42) {
  n_grid <- as.integer(n_grid)
  n_sim <- as.integer(n_sim)
  if (n_grid < 4L) stop("n_grid must be at least 4")
  if (n_sim < 1L) stop("n_sim must be positive")
  e <- .ghc_rng(seed)
  s_idx <- n_grid %/% 4L
  t_idx <- n_grid %/% 2L
  acc_st <- 0
  acc_ss <- 0
  for (it in seq_len(n_sim)) {
    w <- cumsum(.ghc_norm(e, n_grid) / sqrt(n_grid))
    ws <- if (s_idx >= 1L) w[s_idx] else 0
    wt <- if (t_idx >= 1L) w[t_idx] else 0
    acc_st <- acc_st + ws * wt / n_sim
    acc_ss <- acc_ss + ws * ws / n_sim
  }
  s <- s_idx / n_grid
  .t1_result(estimate = acc_st, theory_min_st = s,
             cov_gap = abs(acc_st - s), var_gap = abs(acc_ss - s),
             method = "Brownian motion prior (GvdV 2017 Ex 11.5)")
}
