# SPDX-License-Identifier: AGPL-3.0-or-later
#' Pettitt's non-parametric single change-point test.
#'
#' Formula: with \eqn{r_i} the midranks of \code{x},
#' \eqn{U_k = 2\sum_{i\le k} r_i - k(n+1)}, statistic
#' \eqn{U^* = \max_k |U_k|}, change point at the maximising \eqn{k}, and
#' \eqn{p = \min(1, 2 e^{-6 U^{*2} / (n^3 + n^2)})}.  The rank form is
#' algebraically the Mann-Whitney form but O(n) per split.  The
#' approximation is only trustworthy for \eqn{p \le 0.5}, hence the
#' clamp.
#'
#' @param x Series in time order.
#' @return List with \code{statistic}, \code{p_value},
#'   \code{changepoint} (1-based), \code{U}, \code{n}, \code{method}.
#' @references Pettitt (1979), JRSS C 28:126-135.  Paywalled; the coded form was read from Pohlert's CRAN package trend (R/pettitt.test.R), which follows Verstraeten et al. (2006).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Pettitt(V)
Pettitt <- function(x) {
  x <- .t4_vec(x); n <- length(x)
  if (n < 2) stop("need at least 2 observations")
  r <- .t4_ranks(x)
  k <- seq_len(n)
  Uk <- 2 * cumsum(r) - k * (n + 1)
  ustar <- max(abs(Uk))
  kstar <- min(k[abs(Uk) == ustar])
  p <- min(1, 2 * exp(-6 * ustar^2 / (n^3 + n^2)))
  .t4_result(statistic = ustar, p_value = p, changepoint = as.integer(kstar),
             U = Uk, n = as.integer(n),
             method = "Pettitt single change-point test")
}
