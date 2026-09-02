# SPDX-License-Identifier: AGPL-3.0-or-later
# Post-stratification as a weight adjustment
#
# The same method as \eqn{link{Poststs}} seen from the weight side:
# with unit design weights the two return the same number. The adjusted
# weights sum to \eqn{_h} within every stratum exactly, which is the
# prime calibration property that makes the adjustment worth doing.
# prime
# prime Formula: \code{w_i prime = w_i N_h / sum_{j in h} w_j}; the estimate is the
#' weighted mean.
#'
#' @param y Observed values.
#' @param weights Design weights, positive.
#' @param stratum Stratum label per observation.
#' @param N_h Population size per stratum.
#' @return List with \code{estimate}, \code{weights}, \code{factors},
#'   \code{N}, \code{n}, \code{strata}.
#' @references Holt, D. & Smith, T. M. F. (1979). Post stratification.
#'   Journal of the Royal Statistical Society Series A 142(1):33-46.
#'   \doi{10.2307/2344652}. Standard form, as for \code{Poststs}.
#' @export
#' @examples
#' Postrt(y = c(1, 2, 3, 4, 5, 6, 7, 8), weights = c(1, 2, 3, 4, 5, 6, 7, 8), stratum = c(1, 2, 3, 4, 5, 6, 7, 8), N_h = c(1, 2, 3, 4, 5, 6, 7, 8))
Postrt <- function(y, weights, stratum, N_h) {
  d <- .ps_strata(y, stratum, N_h, "Postrt")
  w <- as.numeric(unlist(weights))
  if (length(w) != length(d$y))
    stop("Postrt: weights must have one entry per observation")
  if (any(w <= 0)) stop("Postrt: weights must be positive")
  wa <- w; fac <- numeric(0)
  for (k in d$order) {
    idx <- which(d$s == k)
    tot <- sum(w[idx])
    if (tot <= 0) stop(paste0("Postrt: stratum ", k, " has no weight"))
    f <- d$sizes[[k]] / tot
    fac <- c(fac, f)
    wa[idx] <- w[idx] * f
  }
  .t1_result(estimate = sum(wa * d$y) / sum(wa), weights = wa, factors = fac,
             N = sum(wa), n = length(d$y), strata = length(d$order),
             method = "Post-stratification weight adjustment w_i N_h / sum_h w")
}
