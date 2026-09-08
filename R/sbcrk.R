# SPDX-License-Identifier: AGPL-3.0-or-later
#' Rank statistics for simulation-based calibration, and a uniformity test
#'
#' The SHAPE of the departure names the fault: U means the posterior is
#' too narrow, a hump too wide, a slope biased. The bin count must divide
#' L + 1 exactly or the expected counts are unequal.
#'
#' Formula: rank_j = #\{ l : theta^\{(l)\}_j < theta_j^prior \};
#'   under calibration rank ~ Uniform\{0, ..., L\};
#'   chi^2 = sum_b (O_b - E_b)^2 / E_b on (bins - 1) df
#'
#' @param prior_draw One prior draw per replication.
#' @param post_draws Matrix of posterior draws, one row per replication.
#' @param bins Number of histogram bins; must divide L + 1.
#' @return List with \code{rank}, \code{histogram}, \code{expected},
#'   \code{statistic}, \code{p_value}, \code{df}, \code{bins}, \code{J},
#'   \code{L}.
#' @references Talts, Betancourt, Simpson, Vehtari & Gelman (2018),
#'   arXiv:1804.06788 -- the primary source. Bayesian Data Analysis, 3rd
#'   edition, was fetched in full and searched; it predates
#'   simulation-based calibration and does NOT contain it.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Sbcrank(V, V)
Sbcrank <- function(prior_draw, post_draws, bins = NULL) {
  pd <- .t1_vec(prior_draw)
  P <- as.matrix(post_draws)
  J <- length(pd)
  if (nrow(P) != J) stop("one row of posterior draws per prior draw")
  L <- ncol(P)
  if (J < 1L || L < 1L)
    stop("at least one replication and one draw are needed")
  rk <- vapply(seq_len(J), function(j) sum(P[j, ] < pd[j]), 0)
  K <- if (is.null(bins)) L + 1L else as.integer(bins)
  if (K < 1L || (L + 1L) %% K != 0L) stop("bins must divide L + 1 exactly")
  width <- (L + 1L) %/% K
  idx <- pmin(K - 1L, rk %/% width)
  hist <- as.numeric(tabulate(idx + 1L, nbins = K))
  ex <- J / K
  chi <- sum((hist - ex)^2 / ex)
  df <- K - 1L
  .t1_result(rank = as.numeric(rk), histogram = hist, expected = ex,
             statistic = chi,
             p_value = if (df >= 1L) stats::pchisq(chi, df, lower.tail = FALSE) else NaN,
             df = as.numeric(df), bins = as.numeric(K), J = as.numeric(J),
             L = as.numeric(L),
             method = "Simulation-based calibration ranks (Talts et al. 2018)")
}
