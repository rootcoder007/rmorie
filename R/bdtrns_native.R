# Manski worst-case bound for partially observed outcomes.
# Source: Manski (2007), Identification for Prediction and Decision,
# Harvard UP, Secs. 1.4 and 2.1, Eqs. 2.8-2.9
# (fetched-wave3/Identification_for_Prediction_and_Decision..pdf).
# Mirrors Python morie.fn.bdtrns exactly.

#' Manski no-assumptions identification bound for a mean
#'
#' With observation probability p and bounded outcomes \[g0, g1\], the
#' identification region for E[y] is
#' \[m p + g0 (1-p), m p + g1 (1-p)\] with m the observed-subsample
#' mean; the width is exactly (g1 - g0)(1 - p).
#'
#' @param y_obs Observed outcomes.
#' @param p_obs Observation probability in \[0, 1\].
#' @param y_lo,y_hi Logical outcome bounds.
#' @return A list with elements \code{lower}, \code{upper},
#'   \code{width}, \code{observed_mean}, \code{p_obs},
#'   \code{method}.
#' @references Manski, C. F. (2007). Identification for Prediction
#'   and Decision. Harvard University Press.
#' @export
#' @examples
#' morie_bdtrns(y_obs = c(1, 0, 1), p_obs = 0.6, y_lo = 0, y_hi = 1)
morie_bdtrns <- function(y_obs, p_obs, y_lo, y_hi) {
  yv <- as.numeric(y_obs)
  if (!length(yv)) stop("need at least one observed outcome")
  p <- as.numeric(p_obs)
  if (p < 0 || p > 1) stop("p_obs must be in [0, 1]")
  if (y_hi < y_lo) stop("y_hi must be >= y_lo")
  if (any(yv < y_lo - 1e-12 | yv > y_hi + 1e-12)) {
    stop("observed outcomes violate the stated bounds")
  }
  m <- mean(yv)
  lo <- m * p + y_lo * (1 - p)
  hi <- m * p + y_hi * (1 - p)
  list(lower = lo, upper = hi, width = hi - lo, observed_mean = m,
       p_obs = p,
       method = "Manski (2007) worst-case bound (Eqs. 2.8-2.9)")
}
