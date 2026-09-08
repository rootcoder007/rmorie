#' LMS adaptive noise canceller -- Rangayyan Ch 11
#'
#' Widrow-Hoff least-mean-squares adaptive filter. Returns the cleaned
#' residual `signal = x - LMS(reference)` and the noise estimate.
#'
#' @param x Numeric vector (target + correlated noise).
#' @param reference Numeric vector of reference noise (same length).
#' @param mu Step size (0 < mu).
#' @param order Number of taps (default 16).
#' @return Named list `signal`, `noise_estimate`, `weights`, `mu`, `order`.
#' @references Widrow, B., & Stearns, S. D. (1985). Adaptive Signal
#'   Processing. Prentice-Hall. Rangayyan, R. M., & Krishnan, S. (2024).
#'   Biomedical Signal Analysis, 3rd ed. Wiley-IEEE Press. Sec 3.10.2 "The
#'   least-mean-squares adaptive filter", p.184. The previous reference said
#'   Ch 11.
#' @export
#' @examples
#' set.seed(0)
#' n <- rnorm(200)
#' x <- sin(2 * pi * seq_len(200) / 20) + n
#' r <- rgadp(x, reference = n, mu = 0.01, order = 8)
#' length(r$signal)
rgadp <- function(x, reference, mu = 0.01, order = 16L) {
  if (length(x) != length(reference)) stop("x and reference must have equal length.")
  M <- as.integer(order)
  N <- length(x)
  w <- numeric(M)
  y <- numeric(N)
  e <- numeric(N)
  ## Start at n = 1 with a zero-padded reference history rather than at
  ## n = M. The old loop left y[1..M-1] and e[1..M-1] at zero, so the first
  ## M-1 samples of the returned signal were not the cancelled input, they
  ## were nothing at all -- 15 samples at the default order = 16. It also
  ## broke the identity that defines a noise canceller: signal +
  ## noise_estimate must reconstruct the primary input everywhere.
  for (n in seq_len(N)) {
    lo <- max(1L, n - M + 1L)
    seg <- rev(reference[lo:n])
    rv <- numeric(M)
    rv[seq_along(seg)] <- seg
    y[n] <- sum(w * rv)
    e[n] <- x[n] - y[n]
    w <- w + 2 * mu * e[n] * rv
  }
  list(
    signal = e, noise_estimate = y, weights = w,
    mu = mu, order = M
  )
}

#' @rdname rgadp
#' @keywords internal
#' @export
morie_rangayyan_adaptive_filter <- rgadp
