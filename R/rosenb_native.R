# Rosenbaum sensitivity bounds over a grid of Gamma.
# Source: Rosenbaum, P. R. (2002), Observational Studies, 2nd ed.,
# Springer, Ch. 4 (sensitivity analysis): under a bias of at most
# Gamma in the odds of treatment within a matched pair, the null
# distribution of Wilcoxon's signed-rank statistic is bounded between
# two distributions whose success probabilities are Gamma/(1+Gamma)
# and 1/(1+Gamma); the resulting p-value bounds are what a sceptic
# needs in order to overturn the finding.  Gamma = 1 is the
# randomisation-inference case with no hidden bias.
#
# Native implementation mirroring Python morie.fn.rosenb (and the
# morie.fn.cnsRos bound it calls) exactly: the same average ranks on
# absolute non-zero differences, the same normal approximation, and
# the same "largest Gamma whose upper bound is still significant"
# convention for the critical Gamma.

# Wilcoxon signed-rank sensitivity bound at one Gamma (Python cnsRos).
.mor_ros_signed <- function(pairs, Gamma = 1) {
  d <- as.numeric(pairs)
  d <- d[d != 0]
  n <- length(d)
  if (n == 0L) stop("empty input: no non-zero pair differences")
  G <- as.numeric(Gamma)
  if (G < 1) stop("Gamma must be at least 1")
  ranks <- rank(abs(d))                 # average ranks for ties
  W <- sum(ranks[d > 0])
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
  list(p_upper = 1 - pnorm(z_up), p_lower = 1 - pnorm(z_lo), W = W,
       mu_plus = mu_p, sigma_plus = sd_p, z_upper = z_up, n_pairs = n,
       Gamma = G)
}

#' Rosenbaum sensitivity bounds over a Gamma grid
#'
#' For each \eqn{\Gamma} in the grid, reports the largest and smallest
#' p-values consistent with a hidden bias of that magnitude
#' (Rosenbaum 2002, Ch. 4).  \code{gamma_critical} is the largest grid
#' value at which the UPPER bound is still at or below \code{alpha} --
#' the point past which an unmeasured confounder of that strength
#' could explain the result away.
#'
#' @param matched_pairs Within-pair outcome differences.
#' @param Gamma_grid Ascending grid of bias magnitudes, each at least
#'   1.
#' @param alpha Significance level for \code{gamma_critical}.
#' @return A list with \code{Gamma}, \code{p_upper}, \code{p_lower},
#'   \code{gamma_critical} (\code{NULL} if none qualifies),
#'   \code{n_pairs}, \code{W}, \code{alpha}, \code{method}.
#' @references Rosenbaum, P. R. (2002). Observational Studies, 2nd
#'   ed. Springer, Chapter 4.
#' @export
morie_rosenb <- function(matched_pairs, Gamma_grid = c(1, 1.5, 2, 3),
                         alpha = 0.05) {
  gs <- as.numeric(Gamma_grid)
  if (length(gs) == 0L) stop("rosenb: Gamma_grid is empty")
  pu <- numeric(length(gs)); pl <- numeric(length(gs))
  W <- NA_real_; n_pairs <- NA_integer_
  for (k in seq_along(gs)) {
    r <- .mor_ros_signed(matched_pairs, Gamma = gs[k])
    pu[k] <- r$p_upper; pl[k] <- r$p_lower
    W <- r$W; n_pairs <- r$n_pairs
  }
  crit <- NULL
  for (k in seq_along(gs)) if (pu[k] <= as.numeric(alpha)) crit <- gs[k]
  list(Gamma = gs, p_upper = pu, p_lower = pl, gamma_critical = crit,
       n_pairs = n_pairs, W = W, alpha = as.numeric(alpha),
       method = "Rosenbaum signed-rank sensitivity bounds over Gamma")
}
