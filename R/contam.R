# SPDX-License-Identifier: AGPL-3.0-or-later

# Solve 2 phi(k)/k - 2 Phi(-k) = eps/(1-eps) for k, by bisection.
.huber_k <- function(eps) {
  if (eps <= 0) return(Inf)
  if (eps >= 1) return(0)
  target <- eps / (1 - eps)
  g <- function(k) 2 * exp(-0.5 * k * k) / sqrt(2 * pi) / k - 2 * .s03pnorm(-k)
  lo <- 1e-8; hi <- 40
  for (i in seq_len(300)) {
    mid <- 0.5 * (lo + hi)
    if (g(mid) > target) lo <- mid else hi <- mid
  }
  0.5 * (lo + hi)
}

#' Huber's epsilon-contamination neighbourhood
#'
#' Formula: F = (1 - eps) Phi + eps H
#'
#' The gross-error model: a fraction eps of the data comes from an
#' arbitrary H instead of the nominal normal.  The least favourable
#' member of this neighbourhood has a density normal in the middle and
#' exponential in the tails, whose score is Huber's psi clipped at k,
#' where k solves 2 phi(k)/k - 2 Phi(-k) = eps/(1-eps).  That k is
#' reported alongside the mixture itself.
#'
#' @param epsilon Contamination fraction in [0, 1).
#' @param H Sample from the contaminating distribution, used as its
#'   empirical distribution function.
#' @param x Points at which the mixture cdf is evaluated, or NULL for
#'   -3, -2, ..., 3.
#' @return List with \code{estimate} (Huber k), \code{k}, \code{F},
#'   \code{mean}, \code{var}, \code{eps}, \code{n_H}, \code{method}.
#' @references Huber (1964), Ann. Math. Statist. 35(1):73-101.
#' @export
Contam <- function(epsilon, H, x = NULL) {
  eps <- as.numeric(epsilon)
  if (!(eps >= 0 && eps < 1)) stop("epsilon must lie in [0, 1)")
  h <- .s03vec(H)
  m <- length(h)
  if (m == 0L) stop("empty input: H has no observations")
  if (is.null(x)) x <- as.numeric(-3:3) else x <- .s03vec(x)
  hs <- sort(h)
  FF <- numeric(length(x))
  for (q in seq_along(x)) {
    cnt <- 0L
    for (w in hs) if (w <= x[q]) cnt <- cnt + 1L
    FF[q] <- (1 - eps) * .s03pnorm(x[q]) + eps * cnt / m
  }
  mh <- sum(hs) / m
  vh <- sum((hs - mh)^2) / m
  mean <- eps * mh
  var <- (1 - eps) * 1 + eps * (vh + mh * mh) - mean * mean
  k <- .huber_k(eps)
  .t1_result(estimate = k, k = k, F = FF, mean = mean, var = var,
             eps = eps, n_H = m,
             method = "Huber epsilon-contamination neighbourhood")
}
