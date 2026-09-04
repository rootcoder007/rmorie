# SPDX-License-Identifier: AGPL-3.0-or-later

#' Berry-Esseen bound for the kernel quantile estimator
#'
#' Eq. (3.5) and Remark 3.1:
#' \deqn{P(\sqrt{n}|\hat Q_{p,h} - Q(p)| \le x\sigma_n) = 2\Phi(x) - 1 +
#' O(n^{-r}).}{P(sqrt(n)|Qhat - Q(p)| <= x sigma_n) = 2 Phi(x) - 1 + O(n^-r).}
#'
#' The rate `r` depends on the ORDER of the kernel, not on the sample:
#' `m = 2` gives \code{r = 1/3}; `m = 3` gives `5/13`; `m = 4` gives `7/17` by the
#' earlier literature, improved to `1/2` by Remark 3.1 of this book. Those five
#' numbers are quoted verbatim from the text.
#'
#' The book also says plainly that `o(n^-1/2)` is unreachable for ANY kernel
#' order without adding the next Edgeworth term, so \code{r = 1/2} is the ceiling of
#' this approach, not a stepping stone.
#'
#' The two-sided normal probability and the bound term `n^(-r)` come back
#' separately: the second is an order symbol with no constant, and adding it to
#' the first would be arithmetic on something with no numerical value.
#'
#' @param x Argument, in units of `sigma_n`.
#' @param n Sample size.
#' @param m Kernel order; 2, 3 or 4.
#' @param improved For `m = 4`, use Remark 3.1's \code{r = 1/2} instead of the
#'   literature's `7/17`.
#' @return Named list with ``estimate``, ``bound``, ``rate``, ``m``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Eq. (3.5), Remark 3.1.
#' @examples
#' Qbebnd(x = 1.96, n = 100)
#' @export
Qbebnd <- function(x, n, m = 4L, improved = TRUE) {
  if (n < 1) stop("sample size must be at least 1.")
  rate <- if (m == 2L) 1 / 3 else if (m == 3L) 5 / 13 else if (m == 4L) {
    if (isTRUE(improved)) 0.5 else 7 / 17
  } else stop("the book states rates for m = 2, 3, 4 only.")
  x <- as.numeric(x)
  if (any(x < 0)) stop("(3.5) bounds an absolute deviation; x must be >= 0.")
  list(estimate = 2 * stats::pnorm(x) - 1, bound = n^(-rate), rate = rate,
       m = m, n = n,
       method = "Berry-Esseen bound for the kernel quantile estimator (3.5)")
}

# CANONICAL TEST
# r <- Qbebnd(x = 1.96, n = 100)
# stopifnot(abs(r$estimate - 0.95) < 1e-3, r$rate == 0.5)

#' @rdname Qbebnd
#' @keywords internal
#' @export
morie_fauzi_berry_esseen_quantile <- Qbebnd
