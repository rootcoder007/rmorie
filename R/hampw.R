# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hampel three-part redescending weight function
#'
#' Hampel, F. R. (1974), "The influence curve and its role in robust
#' estimation", \emph{Journal of the American Statistical Association} 69(346),
#' 383-393, doi:10.1080/01621459.1974.10482962, is the shelf citation and the
#' source of the influence-curve argument these constants come from.  That
#' paper is closed access with no open copy in any repository (Unpaywall
#' reports oa_status "closed"), so the exact piecewise form was taken from the
#' reference implementation that ships with R, MASS::psi.hampel, from Venables,
#' W. N. and Ripley, B. D. (2002), \emph{Modern Applied Statistics with S},
#' 4th ed., Springer, whose body was printed in this session as
#'
#' \preformatted{U <- pmin(abs(u) + 1e-50, c)
#' ifelse(U <= a, U, ifelse(U <= b, a, a * (c - U)/(c - b))) / U}
#'
#' i.e. with r = |u|, w(r) = 1 for r <= a, a/r for a < r <= b,
#' a(c-r)/((c-b)r) for b < r <= c and 0 beyond c: the weight that multiplies
#' each residual in an IRLS M-estimator.  MASS's default constants a = 2,
#' b = 4, c = 8 are kept.  The 1e-50 of the R source guards 0/0 at r = 0; this
#' arm returns the limit w(0) = 1 directly instead.
#'
#' @param y Residuals, usually already scaled by a robust sigma.
#' @param a,b,c The three bend points, 0 < a <= b < c.
#' @return list: estimate (mean weight), weights, n_zero, n, a, b, c, method.
#' @keywords internal
#' @examples
#' Hampw(c(0, 1, 3, 5, 9))$weights
#' @export
Hampw <- function(y, a = 2, b = 4, c = 8) {
  r <- .s03vec(y)
  if (length(r) == 0L) stop("hampel_three_part: y is empty")
  ck <- .hampel_check(a, b, c, "hampel_three_part")
  a <- ck[1L]
  b <- ck[2L]
  c <- ck[3L]
  w <- numeric(length(r))
  nz <- 0L
  for (i in seq_along(r)) {
    u <- abs(r[i])
    wi <- if (u <= a) 1 else if (u <= b) a / u else if (u <= c) {
      a * (c - u) / ((c - b) * u)
    } else { nz <- nz + 1L
    0 }
    w[i] <- wi
  }
  list(estimate = sum(w) / length(r), weights = w, n_zero = nz,
       n = length(r), a = a, b = b, c = c,
       method = "Hampel three-part redescending weight")
}

#' .hampel_check
#'
#' A step of the hampw implementation. Called by \code{Hampel}, \code{Hampw}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param a Coerced to numeric by the body, with \code{as.numeric}.
#' @param b Coerced to numeric by the body, with \code{as.numeric}.
#' @param c Coerced to numeric by the body, with \code{as.numeric}.
#' @param who Passed to \code{paste0}.
#' @return A vector, from \code{c}.
#' @export
.hampel_check <- function(a, b, c, who) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  c <- as.numeric(c)
  if (!(a > 0 && a <= b && b < c)) {
    stop(paste0(who, ": the constants must satisfy 0 < a <= b < c"))
  }
  c(a, b, c)
}
