# SPDX-License-Identifier: AGPL-3.0-or-later
#' Posterior predictive mean, sd and interval from replicated datasets.
#'
#' \code{sd} is across replicate means, \code{sd_pooled} across all
#' replicated observations; the second is the predictive spread and is
#' always the wider.
#'
#' Formula: E[ytilde | y] ~= (1/S) sum_s ytilde^s, interval from
#'   empirical quantiles of the pooled draws
#'
#' @param yrep Matrix whose row s is one replicated dataset.
#' @return List with \code{estimate}, \code{sd}, \code{sd_pooled},
#'   \code{ci_lower}, \code{ci_upper}, \code{rep_mean}, \code{S},
#'   \code{n}.
#' @references Gelman, Carlin, Stern, Dunson, Vehtari & Rubin (2013),
#'   Bayesian Data Analysis, 3rd edition, Section 6.3. Fetched as the full
#'   text of the book from the author's own copy.
#' @export
Ppmean <- function(yrep) {
  Y <- as.matrix(yrep); S <- nrow(Y); n <- ncol(Y)
  if (S < 2L) stop("at least two replicated datasets are required")
  rm_ <- rowMeans(Y)
  est <- mean(rm_)
  pooled <- as.numeric(t(Y))
  q <- sort(pooled); N <- length(q)
  lo <- q[max(1L, floor(0.025 * (N - 1)) + 1L)]
  hi <- q[min(N, ceiling(0.975 * (N - 1)) + 1L)]
  .t1_result(estimate = est, sd = stats::sd(rm_),
             sd_pooled = stats::sd(pooled), ci_lower = lo, ci_upper = hi,
             rep_mean = rm_, S = as.numeric(S), n = as.numeric(n),
             method = "Posterior predictive summary, BDA3 Section 6.3")
}
