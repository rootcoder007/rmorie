# SPDX-License-Identifier: AGPL-3.0-or-later
#' Fractional Brownian motion prior
#'
#' fBm has E(W_s W_t) = (s^(2H) + t^(2H) - |t - s|^(2H)) / 2.  The Hurst
#' index H sets the path smoothness directly, so fBm is the natural prior
#' when the smoothness is to be chosen rather than inherited; the kernel
#' is evaluated on a grid and checked for the two properties the theory
#' needs, the diagonal E W_t^2 = t^(2H) and positive definiteness (all
#' leading principal minors positive, by Gaussian elimination).
#'
#' Formula: K(s, t) = (s^(2H) + t^(2H) - |t - s|^(2H)) / 2.
#'
#' @param H Hurst index, in (0, 1).
#' @param ts Grid of time points, all positive.
#' @return List with \code{estimate} (K(t1, t1)), \code{kernel},
#'   \code{var_gap}, \code{positive_definite}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, Example 11.9, eq. (11.6).
#' @export
#' @examples
#' Ghosalfbmprior()
Ghosalfbmprior <- function(H = 0.7, ts = c(0.25, 0.5, 0.75)) {
  H <- as.numeric(H); ts <- as.numeric(ts)
  if (H <= 0 || H >= 1) stop("H must lie strictly between 0 and 1")
  if (length(ts) == 0L) stop("ts must be non-empty")
  if (any(ts <= 0)) stop("every time point must be positive")
  k <- length(ts)
  G <- outer(ts, ts, function(a, b)
    0.5 * (a^(2 * H) + b^(2 * H) - abs(b - a)^(2 * H)))
  var_gap <- max(abs(diag(G) - ts^(2 * H)))
  m <- G
  minors <- numeric(k)
  det <- 1
  for (i in seq_len(k)) {
    det <- det * m[i, i]
    minors[i] <- det
    if (i < k) for (r in (i + 1):k) {
      f <- m[r, i] / m[i, i]
      m[r, ] <- m[r, ] - f * m[i, ]
    }
  }
  .t1_result(estimate = G[1, 1], kernel = G, var_gap = var_gap,
             positive_definite = all(minors > 0),
             method = "fBm covariance (GvdV 2017 eq. 11.6)")
}
