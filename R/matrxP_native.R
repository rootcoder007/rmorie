# Matrix profile of a series (self-join).
# Source: Yeh, C.-C. M., Zhu, Y., Ulanova, L., Begum, N., Ding, Y.,
# Dau, H. A., Silva, D. F., Mueen, A. and Keogh, E. (2016), Matrix
# profile I: all pairs similarity joins for time series, ICDM 2016,
# 1317-1322.  Definitions 4-6 (distance profile, matrix profile,
# matrix profile index), the O(1) z-normalised Euclidean distance
# from the dot product and cached means/standard deviations of their
# Sec. III.A, and the trivial-match exclusion zone of their Sec. III.
# The largest profile value is the discord, the smallest is one half
# of the motif pair (their Sec. IV.A-B).
#
# Native implementation mirroring Python morie.fn.matrxP exactly:
# same exclusion zone m %/% 2, same strict "<" update (earliest
# neighbour wins ties), same constant-subsequence fallback.

#' Matrix profile of a time series
#'
#' For each subsequence of length \code{window}, the z-normalised
#' Euclidean distance to its nearest non-trivial neighbour, computed
#' by the exact self-join of Yeh et al. (2016).  The profile's maximum
#' locates the discord and its minimum locates the motif pair.
#'
#' @param x Numeric series.
#' @param window Subsequence length; needs \code{2 <= window <=
#'   length(x)/2}.
#' @return A list with \code{profile}, \code{index} (1-based nearest
#'   neighbour of each subsequence), \code{discord},
#'   \code{discord_distance}, \code{motif} (the pair of 1-based
#'   starts), \code{motif_distance}, \code{window}, \code{estimate},
#'   \code{n}, \code{method}.
#' @references Yeh, C.-C. M. et al. (2016). Matrix profile I: all
#'   pairs similarity joins for time series. ICDM 2016, 1317-1322.
#' @export
morie_matrxP <- function(x, window) {
  xs <- as.numeric(x)
  nlen <- length(xs)
  m <- as.integer(window)
  if (m < 2L || m > nlen %/% 2L) stop("need 2 <= window <= len(x)/2")
  nsub <- nlen - m + 1L
  cs <- numeric(nlen + 1L); css <- numeric(nlen + 1L)
  for (i in seq_len(nlen)) {
    cs[i + 1L] <- cs[i] + xs[i]
    css[i + 1L] <- css[i] + xs[i] * xs[i]
  }
  mu <- numeric(nsub); sdv <- numeric(nsub)
  for (i in seq_len(nsub)) {
    s <- cs[i + m] - cs[i]
    ss <- css[i + m] - css[i]
    mu[i] <- s / m
    v <- ss / m - mu[i] * mu[i]
    sdv[i] <- if (v > 0) sqrt(v) else 0
  }
  excl <- m %/% 2L
  P <- rep(Inf, nsub)
  I <- rep(0L, nsub)
  for (i in seq_len(nsub - 1L)) {
    for (j in seq.int(i + 1L, nsub)) {
      if (j - i <= excl) next
      qt <- sum(xs[i:(i + m - 1L)] * xs[j:(j + m - 1L)])
      if (sdv[i] <= 0 || sdv[j] <= 0) {
        d <- if (sdv[i] <= 0 && sdv[j] <= 0) 0 else sqrt(2 * m)
      } else {
        arg <- 1 - (qt - m * mu[i] * mu[j]) / (m * sdv[i] * sdv[j])
        if (arg < 0) arg <- 0
        d <- sqrt(2 * m * arg)
      }
      if (d < P[i]) { P[i] <- d; I[i] <- j }
      if (d < P[j]) { P[j] <- d; I[j] <- i }
    }
  }
  ib <- 1L; iw <- 1L
  for (i in seq_len(nsub)) {
    if (P[i] > P[ib]) ib <- i
    if (P[i] < P[iw]) iw <- i
  }
  list(profile = P, index = I, discord = ib, discord_distance = P[ib],
       motif = c(iw, I[iw]), motif_distance = P[iw],
       window = m, estimate = ib, n = nlen,
       method = "Matrix profile self-join (Yeh et al. 2016)")
}
