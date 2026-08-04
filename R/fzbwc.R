# SPDX-License-Identifier: AGPL-3.0-or-later

#' Bandwidth condition for the kernel quantile Edgeworth expansion
#'
#' Eq. (3.8): the expansion is proved under
#' \deqn{h = o(n^{-1/4})\quad\text{and}\quad \lim_n (n^{1/4}h)^{-k}n^{-\beta} = 0}{h = o(n^-1/4) and lim (n^1/4 h)^-k n^-beta = 0}
#' for every `beta > 0` and integer `k`.
#'
#' The first half is a rate. The second is not -- it says `h` may not shrink
#' FASTER than any negative power of `n`, so a bandwidth like `exp(-n)` is
#' excluded even though it satisfies the first condition comfortably. The
#' window is genuinely two-sided, which is why the book's working choice is
#' `h = n^(-1/4) / log(n)`: it beats `n^(-1/4)` by a logarithm, and only by one.
#'
#' Checked as `h < n^(-1/4)` for the upper side and `h n^(1/4 + eps)` large for
#' the lower, both finite-sample proxies for limit statements, reported
#' separately with the book's reference bandwidth so a caller can see which
#' side a bandwidth fails on.
#'
#' @param h Bandwidth.
#' @param n Sample size.
#' @param eps The margin at which the lower-side condition is probed.
#' @return Named list with ``ok``, ``upper``, ``lower``, ``hbook``, ``ratio``, ``method``.
#' @references Fauzi and Maesono (2023), Eq. (3.8).
#' @examples
#' Qbwcheck(h = 0.05, n = 1000)
#' @export
Qbwcheck <- function(h, n, eps = 0.05) {
  if (h <= 0) stop("bandwidth must be positive.")
  if (n < 2) stop("sample size must be at least 2.")
  cap <- n^-0.25
  upper <- h < cap
  lower <- h * n^(0.25 + eps) > 1
  list(ok = upper && lower, upper = upper, lower = lower,
       hbook = cap / log(n), ratio = h / cap,
       method = "bandwidth window (3.8) for the quantile Edgeworth expansion")
}

# CANONICAL TEST
# r <- Qbwcheck(h = 0.05, n = 1000); stopifnot(r$upper, r$lower)

#' @rdname Qbwcheck
#' @keywords internal
#' @export
morie_fauzi_quantile_bw_condition <- Qbwcheck
