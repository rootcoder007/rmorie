# SPDX-License-Identifier: AGPL-3.0-or-later
#' How often the NUTS trajectory hit the tree-depth cap.
#'
#' Saturation is an EFFICIENCY warning, not a validity one: unlike a
#' divergence it does not bias the draws.
#'
#' Formula: saturated = #\{ i : depth_i >= max_depth \} / n;
#'   leapfrog steps per iteration ~ 2^depth_i
#'
#' @param depths Tree depth reached at each iteration.
#' @param max_depth The configured maximum tree depth.
#' @return List with \code{saturated}, \code{n_saturated},
#'   \code{mean_depth}, \code{max_observed}, \code{mean_leapfrog},
#'   \code{total_leapfrog}, \code{warn}, \code{n}.
#' @references Hoffman & Gelman (2014), Journal of Machine Learning
#'   Research 15, 1593-1623; Betancourt (2017), arXiv:1701.02434.
#'   Bayesian Data Analysis, 3rd edition, was fetched in full and
#'   searched; it describes HMC but not the tree-depth diagnostic.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Treedepth(V)
Treedepth <- function(depths, max_depth = 10) {
  d <- .t1_vec(depths); n <- length(d)
  if (n < 1L) stop("at least one iteration is required")
  if (any(d < 0)) stop("tree depths must be non-negative")
  md <- as.integer(max_depth)
  if (md < 0L) stop("max_depth must be non-negative")
  k <- sum(d >= md)
  lf <- 2^d
  .t1_result(saturated = k / n, n_saturated = as.numeric(k),
             mean_depth = mean(d), max_observed = max(d),
             mean_leapfrog = mean(lf), total_leapfrog = sum(lf),
             warn = as.numeric(k > 0), n = as.numeric(n),
             method = "NUTS tree-depth saturation diagnostic")
}
