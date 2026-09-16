# SPDX-License-Identifier: AGPL-3.0-or-later
#' Get a proportion back out of the double-arcsine scale
#'
#' The double arcsine stabilises the variance of a proportion, which is
#' what makes it poolable, but the pooled value is then on a scale nobody
#' can read. Back-transforming is not a matter of inverting the formula
#' twice: the transform depends on the sample size, and Miller's inverse
#' substitutes the harmonic mean of the study sizes for it. Using the
#' arithmetic mean instead is a known way to get a proportion outside
#' \code{\[0, 1\]}.
#'
#' Formula: \code{p = 0.5 (1 - sgn(cos t) sqrt(1 - (sin t + (sin t - 1/sin
#' t)/n)^2))} for \code{t} the double-arcsine value and \code{n} the
#' harmonic mean sample size -- Miller (1978).
#'
#' @param ft Value(s) on the double-arcsine scale.
#' @param n_harmonic Harmonic mean of the study sample sizes, positive.
#' @return List with \code{p}, \code{n_harmonic}, \code{clamped}.
#' @references Miller, J. J. (1978). The American Statistician 32(4):138.
#'   \doi{10.1080/00031305.1978.10479283}.
#' @export
#' @examples
#' Mafrti(ft = c(1, 2, 3, 4, 5, 6, 7, 8), n_harmonic = 5L)
Mafrti <- function(ft, n_harmonic) {
  n <- as.numeric(n_harmonic)
  if (n <= 0) stop("the harmonic mean sample size must be positive")
  vals <- as.numeric(ft)
  out <- numeric(length(vals))
  clamped <- 0L
  for (i in seq_along(vals)) {
    t <- vals[i]
    st <- sin(t)
    ct <- cos(t)
    if (abs(st) < 1e-12) {
      out[i] <- if (ct > 0) 0 else 1
      clamped <- clamped + 1L
      next
    }
    inner <- st + (st - 1 / st) / n
    q <- 1 - inner^2
    if (q < 0) q <- 0
    sgn <- if (ct > 0) 1 else if (ct < 0) -1 else 0
    p <- 0.5 * (1 - sgn * sqrt(q))
    if (p < 0) { p <- 0
    clamped <- clamped + 1L }
    else if (p > 1) { p <- 1
    clamped <- clamped + 1L }
    out[i] <- p
  }
  .t1_result(p = out, n_harmonic = n, clamped = clamped,
             method = "Freeman-Tukey double arcsine back-transformation")
}
