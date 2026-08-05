# SPDX-License-Identifier: AGPL-3.0-or-later
#' Truncated product of treatment and censoring weights
#'
#' A marginal structural model fitted under both non-random treatment and
#' non-random censoring uses the product of the two stabilized weights.
#' The product has a much heavier right tail than either factor; Cole and
#' Hernan truncate it, accepting a little bias for a large variance
#' reduction.  The cut is the type-7 percentile of the product.
#'
#' Formula: sw_i = min(sw_A_i sw_C_i, Q_q(sw_A sw_C)).
#'
#' @param sw_A Positive stabilized treatment weights.
#' @param sw_C Positive stabilized censoring weights, same length.
#' @param quantile Upper percentile in (0, 1], default 0.99.
#' @return List with \code{estimate} (mean truncated weight),
#'   \code{weights}, \code{cut}, \code{n_truncated}, \code{max_before},
#'   \code{max_after}, \code{sd}, \code{mean_untruncated}, \code{n},
#'   \code{method}.
#' @references Cole, S. R. and Hernan, M. A. (2008). Constructing inverse
#'   probability weights for marginal structural models. American Journal
#'   of Epidemiology 168(6):656-664. \doi{10.1093/aje/kwn164}
#' @examples
#' Trcwgt(c(1, 1, 1, 1), c(1, 1, 1, 1), 0.99)
#' @export
Trcwgt <- function(sw_A, sw_C, quantile = 0.99) {
  a <- .s03vec(sw_A); c <- .s03vec(sw_C); n <- length(a)
  if (n == 0L) stop("truncated_combined_weights: sw_A is empty")
  if (length(c) != n)
    stop("truncated_combined_weights: sw_A and sw_C differ in length")
  if (any(a <= 0) || any(c <= 0))
    stop("truncated_combined_weights: weights must be positive")
  q <- as.numeric(quantile)
  if (!(q > 0 && q <= 1))
    stop("truncated_combined_weights: quantile must lie in (0, 1]")
  prod <- a * c
  cut <- .s03quantile7(prod, q)
  tw <- pmin(prod, cut)
  m1 <- sum(tw) / n
  v1 <- if (n > 1) sum((tw - m1)^2) / (n - 1) else 0
  list(estimate = as.numeric(m1), weights = tw, cut = as.numeric(cut),
       n_truncated = as.integer(sum(prod > cut)),
       max_before = as.numeric(max(prod)), max_after = as.numeric(max(tw)),
       sd = as.numeric(sqrt(v1)), mean_untruncated = as.numeric(sum(prod) / n),
       n = as.integer(n),
       method = "sw = min(sw_A sw_C, q-th percentile) [Cole & Hernan 2008]")
}

# CANONICAL TEST
# r <- Trcwgt(rep(1, 4), rep(1, 4), 0.99)
# stopifnot(abs(r$estimate - 1) < 1e-12, r$n_truncated == 0L)
