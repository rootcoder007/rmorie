# SPDX-License-Identifier: AGPL-3.0-or-later
#' Weight truncation at a percentile of the weight distribution
#'
#' Potter's simplest procedure: pick a cut point from the empirical
#' weight distribution and pull every larger weight down onto it.
#' Truncation trades bias for variance, so the removed mass is reported
#' and a rescaled copy restoring the original total is returned too.  The
#' cut is the type-7 quantile.
#'
#' Formula: w_i' = min(w_i, Q_q(w)).
#'
#' @param weights Non-negative design weights.
#' @param quantile Upper percentile in (0, 1], default 0.99.
#' @return List with \code{estimate} (cut point), \code{weights},
#'   \code{rescaled}, \code{n_trimmed}, \code{mass_removed}, \code{sumw},
#'   \code{sumw_trimmed}, \code{cv_before}, \code{cv_after}, \code{n},
#'   \code{method}.
#' @references Potter, F. J. (1990). A study of procedures to identify and
#'   trim extreme sampling weights. Proceedings of the Section on Survey
#'   Research Methods, American Statistical Association, 225-230.
#' @examples
#' Trmwgt(c(1, 1, 1, 100), 0.75)
#' @export
Trmwgt <- function(weights, quantile = 0.99) {
  w <- .s03vec(weights); n <- length(w)
  if (n == 0L) stop("trim_weights: weights is empty")
  if (any(w < 0)) stop("trim_weights: weights must be non-negative")
  q <- as.numeric(quantile)
  if (!(q > 0 && q <= 1)) stop("trim_weights: quantile must lie in (0, 1]")
  cut <- .s03quantile7(w, q)
  tw <- pmin(w, cut)
  s0 <- sum(w); s1 <- sum(tw)
  resc <- if (s1 > 0) tw * (s0 / s1) else tw
  list(estimate = as.numeric(cut), weights = tw, rescaled = resc,
       n_trimmed = as.integer(sum(w > cut)),
       mass_removed = as.numeric(s0 - s1), sumw = as.numeric(s0),
       sumw_trimmed = as.numeric(s1), cv_before = .trmwgt_cv(w),
       cv_after = .trmwgt_cv(tw), n = as.integer(n),
       method = "weight truncation at the type-7 q-th percentile [Potter 1990]")
}

#' .trmwgt_cv
#'
#' A step of the Trmwgt implementation. Called by \code{Trmwgt}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param w A vector; its length is taken.
#' @return A numeric value.
#' @export
.trmwgt_cv <- function(w) {
  n <- length(w); m <- sum(w) / n
  if (m == 0) return(NaN)
  v <- if (n > 1) sum((w - m)^2) / (n - 1) else 0
  sqrt(v) / m
}

# CANONICAL TEST
# r <- Trmwgt(c(1, 1, 1, 100), 0.75)
# stopifnot(r$n_trimmed == 1L, abs(sum(r$rescaled) - 103) < 1e-12)
