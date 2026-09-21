# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P12: ecological versus individual correlation
# (research/lean/P12Ecological.lean; Freedman, Pisani & Purves, Statistics 4e, ch. 9 sec. 4; Robinson 1950).
#
#   Research.P12.within_orth     the within-group residual is orthogonal to every group-level function
#   Research.P12.cov_decomp      cov(x, y) = cov(between) + cov(within)
#   Research.P12.var_decomp      var(x) = var(between) + var(within)
#   Research.P12.ecological_ge   within covariance zero  =>  corr(x, y)^2 <= corr(group means)^2

#' Between/within decomposition of a correlation across groups
#'
#' Splits each variable into its group mean (between part) and the residual
#' (within part). Covariances and variances decompose exactly
#' (\code{Research.P12.cov_decomp}, \code{var_decomp}); when the within-group
#' covariance is zero the group-level (ecological) correlation is at least as
#' large in magnitude as the individual one (\code{ecological_ge}). When the
#' within-group covariance is not zero the ecological correlation can be
#' smaller, or of the opposite sign (Robinson 1950), so a neighbourhood-level
#' regression of crime on composition constrains individual behaviour only
#' through the within term, which this function reports.
#'
#' @param x,y Numeric vectors, one entry per individual.
#' @param group Group label per individual.
#' @return A list with \code{cov} (individual, between, within),
#'   \code{var_x}, \code{var_y} (same split), \code{corr_individual},
#'   \code{corr_ecological} (correlation of the group means, weighted by
#'   group size), \code{within_share_of_cov}, \code{bound_applies} (whether the
#'   within covariance is zero, in which case the proved inequality holds),
#'   \code{sign_reversed} and \code{theorems}. Population (divide by n)
#'   conventions throughout, matching the Lean definitions.
#' @examples
#' # Robinson's reversal: group means anti-aligned, individuals positively related
#' x <- c(0, 2, 1, 3); y <- c(1, 3, 0, 2); g <- c("a", "a", "b", "b")
#' morie_ecological_decompose(x, y, g)[c("corr_individual", "corr_ecological", "sign_reversed")]
#' @export
morie_ecological_decompose <- function(x, y, group) {
  n <- length(x)
  if (length(y) != n || length(group) != n) stop("x, y and group must have equal length", call. = FALSE)
  if (n < 2) stop("need at least two individuals", call. = FALSE)
  group <- as.character(group)
  bx <- stats::ave(x, group); by_ <- stats::ave(y, group)
  wx <- x - bx; wy <- y - by_
  pcov <- function(a, b) sum(a * b) / n - mean(a) * mean(b)
  cv <- c(individual = pcov(x, y), between = pcov(bx, by_), within = pcov(wx, wy))
  vx <- c(individual = pcov(x, x), between = pcov(bx, bx), within = pcov(wx, wx))
  vy <- c(individual = pcov(y, y), between = pcov(by_, by_), within = pcov(wy, wy))
  ci <- if (vx["individual"] > 0 && vy["individual"] > 0) unname(cv["individual"] / sqrt(vx["individual"] * vy["individual"])) else NA_real_
  ce <- if (vx["between"] > 0 && vy["between"] > 0) unname(cv["between"] / sqrt(vx["between"] * vy["between"])) else NA_real_
  list(cov = cv, var_x = vx, var_y = vy, corr_individual = ci, corr_ecological = ce,
       within_share_of_cov = if (cv["individual"] != 0) unname(cv["within"] / cv["individual"]) else NA_real_,
       bound_applies = abs(cv["within"]) < 1e-12 * max(1, abs(cv["individual"])),
       sign_reversed = isTRUE(sign(ci) != sign(ce) && ci != 0 && ce != 0),
       theorems = c("Research.P12.within_orth", "Research.P12.cov_decomp", "Research.P12.var_decomp",
                    "Research.P12.ecological_ge"))
}
