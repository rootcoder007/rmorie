# SPDX-License-Identifier: AGPL-3.0-or-later

#' Sample quantile from the empirical distribution function
#'
#' \deqn{\hat Q(p) = \inf\{t : F_n(t) \ge p\},}{Qhat(p) = inf{t : F_n(t) >= p},}
#' the generalised inverse of the empirical df -- concretely the order
#' statistic `X_(ceiling(n p))`, and the estimator Chapter 3 measures the
#' kernel quantile estimator against.
#'
#' Its asymptotic variance is `p(1-p) / (n f^2(Q(p)))` (Eq. 3.2), the same
#' first-order quantity the kernel estimator attains -- Remark 3.3 is explicit
#' that a kernel can only MATCH the sample quantile to first order, so any
#' gain must show up in the Edgeworth term. That is why Chapter 3 exists.
#'
#' No interpolation and no plotting-position convention: the definition is a
#' strict infimum, so this is `stats::quantile(type = 1)` and NOT the default
#' `type = 7`. An interpolating quantile would silently change the estimand.
#'
#' @param x Sample.
#' @param p Probabilities in `(0, 1]`.
#' @return Named list with ``estimate``, ``index``, ``p``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Eq. (3.1) and the display defining the sample
#' quantile in Sec. 3.2.
#' @examples
#' Smpqnt(c(1, 2, 3, 4), p = 0.5)
#' @export
Smpqnt <- function(x, p = 0.5) {
  x <- sort(as.numeric(x))
  n <- length(x)
  if (n < 1L) stop("need at least one observation.")
  p <- as.numeric(p)
  if (any(p <= 0) || any(p > 1)) stop("probabilities must lie in (0, 1].")
  idx <- pmax(1L, pmin(n, as.integer(ceiling(p * n))))
  list(estimate = x[idx], index = idx, p = p, n = n,
       method = "sample quantile, inf{t: F_n(t) >= p}")
}

# CANONICAL TEST
# stopifnot(Smpqnt(c(1, 2, 3, 4), p = 0.5)$estimate == 2)

#' @rdname Smpqnt
#' @keywords internal
#' @export
morie_fauzi_sample_quantile <- Smpqnt
