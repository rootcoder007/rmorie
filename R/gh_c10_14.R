# SPDX-License-Identifier: AGPL-3.0-or-later
#' Parametric versus nonparametric Bayes factor
#'
#' BF is roughly exp(-n KL(P0, P-hat)) divided by the prior mass of a KL
#' ball of radius eps_n: the parametric model wins on prior concentration
#' when it actually holds, and loses to the nonparametric one otherwise.
#' The multinomial version is exact -- H0 is the uniform on four cells,
#' H1 the full Dirichlet(1,1,1,1) simplex -- so no approximation clouds
#' the comparison.
#'
#' Formula: log m_0 = sum_j c_j log(1/4);
#'   log m_1 = lgamma(4) - lgamma(4 + n) + sum_j lgamma(1 + c_j).
#'
#' @param n Number of multinomial trials.
#' @param parametric_truth If TRUE the cells are truly uniform.
#' @param seed Seed for the deterministic draw.
#' @return List with \code{estimate} (log Bayes factor for the
#'   nonparametric model), \code{nonparametric_wins}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 10.5.3.
#' @export
#' @examples
#' Ghosalparamnpbf()
Ghosalparamnpbf <- function(n = 1500, parametric_truth = TRUE, seed = 42) {
  n <- as.integer(n)
  if (n < 1L) stop("n must be positive")
  e <- .ghc_rng(seed)
  p0 <- if (isTRUE(parametric_truth)) rep(0.25, 4) else c(0.4, 0.3, 0.2, 0.1)
  acc <- cumsum(p0)
  counts <- integer(4)
  for (i in seq_len(n)) {
    u <- .ghc_unif(e, 1L)
    j <- which(u <= acc)[1]
    if (!is.na(j)) counts[j] <- counts[j] + 1L
  }
  l0 <- sum(counts * log(0.25))
  l1 <- lgamma(4) - lgamma(4 + n) + sum(lgamma(1 + counts))
  log_bf_np <- l1 - l0
  .t1_result(estimate = log_bf_np, nonparametric_wins = log_bf_np > 0,
             method = "parametric-vs-NP Bayes factor (GvdV 2017 sec. 10.5.3)")
}
