# SPDX-License-Identifier: AGPL-3.0-or-later

#' .tl_lchoose
#'
#' A step of the evtlmom implementation. Called by \code{Evtlmom}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Numeric; combined arithmetically in the body.
#' @param k Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.tl_lchoose <- function(n, k) {
  if (k < 0 || k > n) return(-Inf)
  lgamma(n + 1) - lgamma(k + 1) - lgamma(n - k + 1)
}

#' Trimmed L-moments TL(s,t) of a sample
#'
#' Formula: lambda_r^(s,t) = (1/r) sum_k (-1)^k C(r-1,k) E[X_(r+s-k):(r+s+t)]
#'
#' with the order statistic expectation estimated by the unbiased
#' combinatorial weights
#' E[X_(j:m)] = sum_i C(i-1,j-1) C(n-i,m-j) / C(n,m) x_(i).  Trimming s
#' observations from the left and t from the right makes the moments
#' exist for heavy tails where ordinary L-moments do not.  With
#' s = t = 0 they reduce to Hosking's L-moments, so lambda_1 is the
#' sample mean and lambda_2 the L-scale.
#'
#' @param x Sample.
#' @param s Left trimming.
#' @param t Right trimming.
#' @param order Highest order r to compute.
#' @return List with \code{lambda}, \code{estimate}, \code{tau},
#'   \code{n}, \code{method}.
#' @references Elamir & Seheult (2003), Comput. Statist. Data Anal.
#'   43(3):299-314.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Evtlmom(V)
Evtlmom <- function(x, s = 0, t = 0, order = 2) {
  xs <- sort(.s03vec(x))
  n <- length(xs)
  s <- as.integer(s); t <- as.integer(t); order <- as.integer(order)
  if (n == 0L) stop("empty input: x has no observations")
  if (s < 0L || t < 0L) stop("trimming parameters must be non-negative")
  if (order < 1L) stop("order must be at least 1")
  if (n < order + s + t)
    stop("sample too small for the requested order and trimming")
  lam <- numeric(order)
  for (r in seq_len(order)) {
    m <- r + s + t
    tot <- 0
    for (k in 0:(r - 1)) {
      j <- r + s - k
      w <- 0
      for (i in seq_len(n)) {
        a <- .tl_lchoose(i - 1, j - 1) + .tl_lchoose(n - i, m - j) -
          .tl_lchoose(n, m)
        if (a > -Inf) w <- w + exp(a) * xs[i]
      }
      sgn <- if (k %% 2L == 1L) -1 else 1
      tot <- tot + sgn * exp(.tl_lchoose(r - 1, k)) * w
    }
    lam[r] <- tot / r
  }
  tau <- rep(NaN, length(lam))
  if (length(lam) >= 2L && lam[2] != 0) {
    for (r in seq_along(lam)) if (r >= 3L) tau[r] <- lam[r] / lam[2]
  }
  .t1_result(lambda = lam, estimate = lam[length(lam)], tau = tau, n = n,
             method = "trimmed L-moments TL(s,t)")
}
