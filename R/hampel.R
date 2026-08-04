# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hampel three-part redescending psi function
#'
#' Hampel, F. R. (1974), "The influence curve and its role in robust
#' estimation", \emph{Journal of the American Statistical Association} 69(346),
#' 383-393, doi:10.1080/01621459.1974.10482962, is the shelf citation; it is
#' closed access with no open copy in any repository (Unpaywall reports
#' oa_status "closed"), so the exact piecewise form was taken from the
#' reference implementation shipped with R, MASS::psi.hampel (Venables and
#' Ripley, \emph{Modern Applied Statistics with S}, 4th ed., 2002), whose body
#' was printed in this session.  MASS returns the weight psi(u)/u; multiplying
#' it back by u gives
#'
#' \deqn{\psi(r) = r,\ |r| \le a; \quad a\,\mathrm{sign}(r),\ a < |r| \le b; \quad a(c-|r|)\mathrm{sign}(r)/(c-b),\ b < |r| \le c; \quad 0,\ |r| > c,}{psi(r) = r for |r| <= a; a sign(r) for a < |r| <= b; a (c - |r|) sign(r)/(c - b) for b < |r| <= c; 0 for |r| > c,}
#'
#' linear, then flat, then descending to zero, then zero.  Its derivative,
#' which MASS returns for deriv = 1, is 1, 0, -a/(c-b), 0 on the same four
#' pieces and is returned here as psi_deriv.  Rejecting |r| > c outright is
#' what makes this psi redescending rather than merely bounded like Huber's.
#'
#' @param r Residuals, usually already scaled by a robust sigma.
#' @param a,b,c The three bend points, 0 < a <= b < c.
#' @return list: estimate (mean psi), psi, psi_deriv, n_reject, n, a, b, c,
#'   method.
#' @keywords internal
#' @examples
#' Hampel(c(0, 1, 3, 5, 9))$psi
#' @export
Hampel <- function(r, a = 2, b = 4, c = 8) {
  x <- .s03vec(r)
  if (length(x) == 0L) stop("hampel_redescend: r is empty")
  ck <- .hampel_check(a, b, c, "hampel_redescend")
  a <- ck[1L]; b <- ck[2L]; c <- ck[3L]
  ps <- numeric(length(x)); dp <- numeric(length(x)); nrej <- 0L
  for (i in seq_along(x)) {
    e <- x[i]; u <- abs(e); sg <- if (e >= 0) 1 else -1
    if (u <= a) { ps[i] <- e; dp[i] <- 1 }
    else if (u <= b) { ps[i] <- a * sg; dp[i] <- 0 }
    else if (u <= c) { ps[i] <- a * (c - u) / (c - b) * sg; dp[i] <- -a / (c - b) }
    else { ps[i] <- 0; dp[i] <- 0; nrej <- nrej + 1L }
  }
  list(estimate = sum(ps) / length(x), psi = ps, psi_deriv = dp,
       n_reject = nrej, n = length(x), a = a, b = b, c = c,
       method = "Hampel three-part redescender")
}
