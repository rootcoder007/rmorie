# SPDX-License-Identifier: AGPL-3.0-or-later
#' RMSNorm, root-mean-square layer normalisation
#'
#' Zhang and Sennrich (2019), Root mean square layer normalization,
#' NeurIPS 32 (arXiv:1910.07467 -- FETCHED), equation (4): abar_i = a_i /
#' RMS(a) * g_i with RMS(a) = sqrt((1/n) sum a_i^2) -- to be contrasted
#' with LayerNorm's eq. (2), abar_i = (a_i - mu)/sigma * g_i.  RMSNorm
#' drops the re-centering entirely, which is the paper's whole hypothesis.
#' pRMSNorm, in which the RMS is estimated from the first p per cent of
#' the units, is available as `p` and is NOT the default, because it
#' changes the statistic.
#'
#' @param y the summed inputs a (first slot, for signature stability).
#' @param x the summed inputs; wins over y.
#' @param g the gain; ones by default.
#' @param eps added inside the square root; 0 is the paper's expression.
#' @param p fraction of units used for the RMS (pRMSNorm).
#' @param b offset added after scaling.
#' @return list: estimate, out, rms, k_partial, n, method.
#' @keywords internal
#' @examples
#' Rmsnorm(c(1, -2, 3))$rms
#' @export
Rmsnorm <- function(y, x = NULL, g = NULL, eps = 0, p = 1, b = NULL) {
  a <- .s03vec(if (!is.null(x)) x else y)
  n <- length(a)
  kp <- as.integer(n * as.numeric(p))
  if (kp < 1L) kp <- 1L
  if (kp > n) kp <- n
  s <- 0
  for (i in seq_len(kp)) s <- s + a[i] * a[i]
  rms <- sqrt(s / kp + as.numeric(eps))
  gg <- if (!is.null(g)) .s03vec(g) else rep(1, n)
  bb <- if (!is.null(b)) .s03vec(b) else numeric(n)
  out <- numeric(n)
  for (i in seq_len(n)) out[i] <- if (rms > 0) (a[i] / rms) * gg[i] + bb[i] else 0
  list(estimate = if (n) out[1] else NaN, out = out, rms = rms,
       k_partial = kp, n = n,
       method = "RMSNorm (Zhang and Sennrich 2019, eq. 4)")
}
