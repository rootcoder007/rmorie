# SPDX-License-Identifier: AGPL-3.0-or-later
#' Rate-distortion function by the Blahut-Arimoto algorithm
#'
#' Blahut (1972), IEEE Trans. Inf. Theory 18(4), 460-473, and Arimoto
#' (1972), ibid. 18(1), 14-20; the fixed point is reproduced verbatim in
#' Tishby, Pereira and Bialek (1999), The information bottleneck method,
#' Allerton 37, 368-377 (physics/0004057 -- FETCHED), section 2:
#' p(xtilde|x) = p(xtilde)/Z(x, beta) exp\[-beta d(x, xtilde)\], alternated
#' with p(xtilde) = sum_x p(x) p(xtilde|x).  The Lagrangian printed there
#' is F = I(X; Xtilde) + beta <d>, so beta traces out R(D) and -1/beta is
#' the slope of R(D).  The 1972 papers are paywalled; the fixed point is
#' quoted from the fetched 1999 source.  beta is bisected so the returned
#' point sits at the requested distortion rather than at an arbitrary beta.
#'
#' @param px the source distribution.
#' @param distortion the distortion matrix; Hamming by default.
#' @param D the target distortion.
#' @param beta_hi upper end of the beta bracket.
#' @param iters inner Blahut-Arimoto iterations.
#' @return list: estimate, rate, bits, distortion_achieved, beta, slope, q,
#'   method.
#' @keywords internal
#' @examples
#' Ratedist(c(0.5, 0.5), NULL, 0.1)$rate
#' @export
Ratedist <- function(px, distortion = NULL, D = 0.1, beta_hi = 1e4,
                     iters = 500) {
  p <- .s03vec(px)
  tot <- 0
  for (v in p) tot <- tot + v
  p <- p / tot
  n <- length(p)
  Dm <- if (is.null(distortion)) {
    m <- matrix(1, n, n)
    for (i in seq_len(n)) m[i, i] <- 0
    m
  } else .s03mat(distortion)
  ba <- function(beta) {
    m <- ncol(Dm)
    q <- rep(1 / m, m)
    Q <- matrix(0, n, m)
    for (it in seq_len(as.integer(iters))) {
      for (i in seq_len(n)) {
        lw <- numeric(m)
        for (j in seq_len(m)) {
          lw[j] <- log(if (q[j] > 1e-300) q[j] else 1e-300) - beta * Dm[i, j]
        }
        z <- .s03logsumexp(lw)
        for (j in seq_len(m)) Q[i, j] <- exp(lw[j] - z)
      }
      nq <- numeric(m)
      for (j in seq_len(m)) {
        s <- 0
        for (i in seq_len(n)) s <- s + p[i] * Q[i, j]
        nq[j] <- s
      }
      delta <- 0
      for (j in seq_len(m)) delta <- delta + abs(nq[j] - q[j])
      q <- nq
      if (delta < 1e-14) break
    }
    R <- 0
    dist <- 0
    for (i in seq_len(n)) for (j in seq_len(m)) {
      if (Q[i, j] > 0 && q[j] > 0) R <- R + p[i] * Q[i, j] * log(Q[i, j] / q[j])
      dist <- dist + p[i] * Q[i, j] * Dm[i, j]
    }
    list(R = R, dist = dist, q = q)
  }
  lo <- 0
  hi <- as.numeric(beta_hi)
  for (it in seq_len(120L)) {
    mid <- 0.5 * (lo + hi)
    res <- ba(mid)
    if (res$dist > as.numeric(D)) lo <- mid else hi <- mid
    if (hi - lo < 1e-12 * max(1, hi)) break
  }
  beta <- 0.5 * (lo + hi)
  res <- ba(beta)
  list(estimate = res$R, rate = res$R, bits = res$R / log(2),
       distortion_achieved = res$dist, beta = beta,
       slope = if (beta > 0) -1 / beta else NaN, q = res$q,
       method = "Blahut-Arimoto rate-distortion, beta bisected to the target distortion")
}
