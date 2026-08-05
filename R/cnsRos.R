# SPDX-License-Identifier: AGPL-3.0-or-later

#' Rosenbaum sensitivity bounds, Wilcoxon signed-rank
#'
#' Formula: vary Gamma; compute the upper p-value
#'
#' Under a bias of at most Gamma in the odds of treatment within a
#' matched pair, the null distribution of the signed-rank statistic is
#' bounded by the one where each pair contributes its rank with
#' probability p+ = Gamma/(1+Gamma).  The upper p-value uses the normal
#' approximation with mean p+ sum(q) and variance p+(1-p+) sum(q^2).  At
#' Gamma = 1 that reduces to the ordinary Wilcoxon signed-rank test.
#'
#' @param pairs Within-pair differences; exact zeros are dropped.
#' @param Gamma Sensitivity parameter, at least 1.
#' @return List with \code{estimate}, \code{p_upper}, \code{p_lower},
#'   \code{W}, \code{mu_plus}, \code{sigma_plus}, \code{z_upper},
#'   \code{n_pairs}, \code{Gamma}, \code{method}.
#' @references Rosenbaum (2002), Observational Studies, 2nd ed.,
#'   Springer, section 4.3.
#' @export
CnsRos <- function(pairs, Gamma = 1) {
  d <- .s03vec(pairs)
  d <- d[d != 0]
  n <- length(d)
  if (n == 0L) stop("empty input: no non-zero pair differences")
  G <- as.numeric(Gamma)
  if (G < 1) stop("Gamma must be at least 1")
  ranks <- .s03rank(abs(d))
  W <- 0
  for (i in seq_len(n)) if (d[i] > 0) W <- W + ranks[i]
  pp <- G / (1 + G)
  pm <- 1 / (1 + G)
  sq <- sum(ranks)
  sq2 <- sum(ranks * ranks)
  mu_p <- pp * sq
  sd_p <- sqrt(pp * (1 - pp) * sq2)
  mu_m <- pm * sq
  sd_m <- sqrt(pm * (1 - pm) * sq2)
  z_up <- if (sd_p > 0) (W - mu_p) / sd_p else NaN
  z_lo <- if (sd_m > 0) (W - mu_m) / sd_m else NaN
  .t1_result(estimate = 1 - .s03pnorm(z_up), p_upper = 1 - .s03pnorm(z_up),
             p_lower = 1 - .s03pnorm(z_lo), W = W, mu_plus = mu_p,
             sigma_plus = sd_p, z_upper = z_up, n_pairs = n, Gamma = G,
             method = "Rosenbaum sensitivity bounds, Wilcoxon signed-rank")
}
