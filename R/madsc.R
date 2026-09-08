# SPDX-License-Identifier: AGPL-3.0-or-later
#' Median absolute deviation scale estimate
#'
#' median(|x - median(x)|) times 1.4826 = 1/qnorm(3/4), the factor that makes
#' it consistent for sigma at the normal.  Source consulted: Hampel (1974),
#' JASA 69(346), 383-393, section 5.  Matches stats::mad.
#'
#' @param x sample.
#' @param constant consistency factor.
#' @param center optional centre; the median if absent.
#' @return list: estimate, raw_mad, center, constant, n, method.
#' @keywords internal
#' @examples
#' madsc(c(2.1, 3.4, 1.9, 5.6))$estimate
#' @export
madsc <- function(x, constant = 1.4826, center = NULL) {
  v <- as.numeric(x)
  ctr <- if (is.null(center)) stats::median(v) else as.numeric(center)
  raw <- stats::median(abs(v - ctr))
  list(estimate = constant * raw, raw_mad = raw, center = ctr,
       constant = as.numeric(constant), n = length(v),
       method = "Median absolute deviation scale estimate (Hampel 1974)")
}

# CANONICAL TEST
# r <- madsc(c(2.1,3.4,1.9,5.6,2.8,3.1,9.9,2.5,3.3,2.7))
# stopifnot(abs(r$estimate - 0.66717) < 1e-12)

#' @rdname madsc
#' @keywords internal
#' @export
morie_madsc <- madsc
