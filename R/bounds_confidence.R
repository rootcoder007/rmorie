# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P11: confidence sets for an identification region
# (research/lean/P11Coverage.lean; Manski sec. 2.7; Imbens & Manski 2004).
#
#   Research.P11.region_coverage_le     theta in H  =>  P[H subset of C] <= P[theta in C]
#   Research.P11.coverage_strictMono_c  F(c + D) - F(-c) is strictly increasing in c
#   Research.P11.im_cutoff_antitone     a wider region needs a smaller cutoff
#   Research.P11.im_cutoff_between      the cutoff is at most the two-sided z and F(-c) <= alpha
#   Research.P11.two_sided_overcovers   at the two-sided z a region of positive width is over-covered

#' Imbens-Manski confidence interval for a partially identified parameter
#'
#' Given estimated bounds \eqn{[\hat L, \hat U]} with standard errors, the
#' interval \eqn{[\hat L - c\,\sigma_L,\; \hat U + c\,\sigma_U]} covers every
#' point of the identification region with probability \eqn{1-\alpha}
#' (uniformly over the region's width) when \eqn{c} solves
#' \eqn{\Phi(c + \Delta) - \Phi(-c) = 1 - \alpha} with
#' \eqn{\Delta = (\hat U - \hat L)/\max(\sigma_L, \sigma_U)} (Imbens & Manski
#' 2004). The cutoff is non-increasing in \eqn{\Delta}
#' (\code{Research.P11.im_cutoff_antitone}), lies between the one-sided and
#' the two-sided normal quantiles (\code{im_cutoff_between}), and the
#' two-sided quantile over-covers whenever the region has positive width
#' (\code{two_sided_overcovers}). An interval built to cover the whole
#' region is at least as conservative for the parameter
#' (\code{region_coverage_le}); the function reports both.
#' @param lower,upper Estimated bounds (e.g. from
#'   \code{\link{morie_sentence_effect_bounds}} with bootstrap standard errors).
#' @param se_lower,se_upper Their standard errors, positive.
#' @param level Confidence level.
#' @return A list with \code{cutoff} (\eqn{c}), \code{delta}, \code{interval}
#'   (the Imbens-Manski interval), \code{region_interval} (the interval that
#'   covers the whole region, using the two-sided quantile), \code{z_one_sided},
#'   \code{z_two_sided}, \code{coverage_two_sided} (what the two-sided cutoff
#'   would deliver for a point at one end, above \code{level}), and \code{theorems}.
#' @examples
#' morie_bounds_confidence(lower = 0.10, upper = 0.35, se_lower = 0.03, se_upper = 0.04)
#' @export
morie_bounds_confidence <- function(lower, upper, se_lower, se_upper, level = 0.95) {
  for (v in list(lower, upper, se_lower, se_upper, level)) {
    if (!is.numeric(v) || length(v) != 1L || is.na(v)) stop("all arguments must be single numbers", call. = FALSE)
  }
  if (upper < lower) stop("upper must be at least lower", call. = FALSE)
  if (se_lower <= 0 || se_upper <= 0) stop("standard errors must be positive", call. = FALSE)
  if (level <= 0 || level >= 1) stop("level must lie in (0, 1)", call. = FALSE)
  alpha <- 1 - level
  sigma <- max(se_lower, se_upper)
  delta <- (upper - lower) / sigma
  z1 <- stats::qnorm(1 - alpha)
  z2 <- stats::qnorm(1 - alpha / 2)
  f <- function(c_) stats::pnorm(c_ + delta) - stats::pnorm(-c_) - (1 - alpha)
  cutoff <- stats::uniroot(f, c(z1 - 1e-9, z2 + 1e-9), tol = 1e-12)$root
  list(
    cutoff = cutoff, delta = delta, level = level,
    interval = c(lower = lower - cutoff * se_lower, upper = upper + cutoff * se_upper),
    region_interval = c(lower = lower - z2 * se_lower, upper = upper + z2 * se_upper),
    z_one_sided = z1, z_two_sided = z2,
    coverage_two_sided = stats::pnorm(z2 + delta) - stats::pnorm(-z2),
    theorems = c("Research.P11.region_coverage_le", "Research.P11.coverage_strictMono_c",
                 "Research.P11.im_cutoff_antitone", "Research.P11.im_cutoff_between",
                 "Research.P11.two_sided_overcovers")
  )
}
