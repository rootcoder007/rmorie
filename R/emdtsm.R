# SPDX-License-Identifier: AGPL-3.0-or-later

#' Empirical Mode Decomposition
#'
#' Formula: iterative sifting into intrinsic mode functions.  Given the
#' running residual r, the sifting loop of Huang et al (1998) sec. 4 is
#' \enumerate{
#'   \item locate the local maxima and minima of h;
#'   \item cubic-spline the maxima into an upper envelope u and the
#'         minima into a lower envelope l;
#'   \item m = (u + l) / 2 ; h <- h - m;
#'   \item stop when SD = sum (h_k - h_{k-1})^2 / sum h_{k-1}^2 < sd_tol
#'         (Huang eq. 5.5, recommended 0.2-0.3).
#' }
#' The extracted h is an IMF; it is subtracted from r and the loop
#' repeats until the residual has fewer than three extrema or
#' \code{max_imf} IMFs have been taken.  Endpoints are appended to both
#' extremum sets so the envelopes span the record.
#'
#' @param y Signal.
#' @param max_imf Maximum number of IMFs to extract.
#' @param max_sift Maximum sifting iterations per IMF.
#' @param sd_tol Cauchy-type stopping threshold SD.
#' @return List with \code{estimate}, \code{n_imf}, \code{imfs}
#'   (row-major), \code{residual}, \code{completeness}, \code{n},
#'   \code{method}.
#' @references Huang, Shen, Long, Wu, Shih, Zheng, Yen, Tung & Liu
#'   (1998), Proc. R. Soc. Lond. A 454(1971):903-995,
#'   doi:10.1098/rspa.1998.0193.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Emdtsm(V)
Emdtsm <- function(y, max_imf = 10, max_sift = 50, sd_tol = 0.2) {
  .nspline <- function(xk, yk, xq) {
    m <- length(xk)
    if (m == 1L) return(rep(yk[1], length(xq)))
    if (m == 2L) {
      s <- (yk[2] - yk[1]) / (xk[2] - xk[1])
      return(yk[1] + s * (xq - xk[1]))
    }
    h <- xk[2:m] - xk[1:(m - 1L)]
    a <- numeric(m); b <- numeric(m); cc <- numeric(m); d <- numeric(m)
    b[1] <- 1; b[m] <- 1
    for (i in 2:(m - 1L)) {
      a[i] <- h[i - 1L]
      b[i] <- 2 * (h[i - 1L] + h[i])
      cc[i] <- h[i]
      d[i] <- 3 * ((yk[i + 1L] - yk[i]) / h[i] - (yk[i] - yk[i - 1L]) / h[i - 1L])
    }
    cp <- numeric(m); dp <- numeric(m)
    cp[1] <- cc[1] / b[1]; dp[1] <- d[1] / b[1]
    for (i in 2:m) {
      den <- b[i] - a[i] * cp[i - 1L]
      cp[i] <- cc[i] / den
      dp[i] <- (d[i] - a[i] * dp[i - 1L]) / den
    }
    C <- numeric(m); C[m] <- dp[m]
    for (i in seq(m - 1L, 1L)) C[i] <- dp[i] - cp[i] * C[i + 1L]
    B <- numeric(m - 1L); D <- numeric(m - 1L)
    for (i in seq_len(m - 1L)) {
      B[i] <- (yk[i + 1L] - yk[i]) / h[i] - h[i] * (2 * C[i] + C[i + 1L]) / 3
      D[i] <- (C[i + 1L] - C[i]) / (3 * h[i])
    }
    out <- numeric(length(xq))
    for (q in seq_along(xq)) {
      t <- xq[q]
      if (t <= xk[1]) i <- 1L
      else if (t >= xk[m]) i <- m - 1L
      else {
        i <- 1L
        while (i < m - 1L && xk[i + 1L] <= t) i <- i + 1L
      }
      u <- t - xk[i]
      out[q] <- yk[i] + B[i] * u + C[i] * u * u + D[i] * u * u * u
    }
    out
  }
  .ext <- function(v) {
    n <- length(v)
    hi <- integer(0); lo <- integer(0)
    if (n > 2L) for (i in 2:(n - 1L)) {
      if (v[i] > v[i - 1L] && v[i] >= v[i + 1L]) hi <- c(hi, i)
      if (v[i] < v[i - 1L] && v[i] <= v[i + 1L]) lo <- c(lo, i)
    }
    list(hi = hi, lo = lo)
  }
  y <- as.numeric(y)
  n <- length(y)
  if (n == 0L) stop("empty input: y has no observations")
  max_imf <- as.integer(max_imf)
  if (max_imf < 1L) stop("max_imf must be positive")
  if (as.numeric(sd_tol) <= 0) stop("sd_tol must be positive")
  x <- as.numeric(seq_len(n) - 1L)
  r <- y
  imfs <- list()
  while (length(imfs) < max_imf) {
    e <- .ext(r)
    if (length(e$hi) + length(e$lo) < 3L) break
    h <- r
    for (.s in seq_len(as.integer(max_sift))) {
      e <- .ext(h)
      if (length(e$hi) == 0L || length(e$lo) == 0L) break
      xu <- c(1L, e$hi, n)
      xl <- c(1L, e$lo, n)
      up <- .nspline(x[xu], h[xu], x)
      dn <- .nspline(x[xl], h[xl], x)
      prev <- h
      h <- prev - 0.5 * (up + dn)
      den <- sum(prev * prev)
      if (den <= 0) break
      sd <- sum((h - prev)^2) / den
      if (sd < as.numeric(sd_tol)) break
    }
    imfs[[length(imfs) + 1L]] <- h
    r <- r - h
  }
  tot <- r
  for (f in imfs) tot <- tot + f
  comp <- max(abs(tot - y))
  .t1_result(estimate = as.numeric(length(imfs)), n_imf = length(imfs),
             imfs = if (length(imfs)) unlist(imfs) else numeric(0),
             residual = r, completeness = comp, n = n,
             method = "Empirical Mode Decomposition")
}
