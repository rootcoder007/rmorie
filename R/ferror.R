# SPDX-License-Identifier: AGPL-3.0-or-later

#' Renewal-equation forecast
#'
#' Formula: I_t = R_t sum_\{s>=1\} I_\{t-s\} w_s, where w is the generation
#' interval distribution (normalised to sum to one) and
#' Lambda_t = sum_s I_\{t-s\} w_s is the total infectiousness at time t.
#' Forward projection feeds each simulated incidence back into Lambda for
#' the following step.  Read backwards the same identity gives the
#' instantaneous reproduction number, Rhat_t = I_t / Lambda_t.
#'
#' @param incidence Observed incidence I_1..I_n.
#' @param Rt Reproduction number per forecast step; a scalar is held
#'   constant over a one-step horizon.
#' @param gen_int Generation-interval weights w_1..w_S at lags 1..S;
#'   non-negative, renormalised internally.
#' @return List with \code{estimate}, \code{forecast}, \code{lambda_},
#'   \code{total}, \code{Rt_implied}, \code{horizon}, \code{n},
#'   \code{method}.
#' @references Fraser (2007), PLoS ONE 2(8):e758,
#'   doi:10.1371/journal.pone.0000758.
#' @export
#' @examples
#' Ferror(incidence = c(1, 2, 3, 4, 5, 6, 7, 8), Rt = c(1, 2, 3, 4, 5, 6, 7, 8), gen_int
#' = c(1, 2, 3, 4, 5, 6, 7, 8))
Ferror <- function(incidence, Rt, gen_int) {
  inc <- as.numeric(incidence)
  n <- length(inc)
  if (n == 0L) stop("empty input: incidence has no observations")
  if (any(inc < 0)) stop("incidence must be non-negative")
  w <- as.numeric(gen_int)
  S <- length(w)
  if (S == 0L) stop("gen_int must have at least one lag")
  if (any(w < 0)) stop("gen_int weights must be non-negative")
  sw <- sum(w)
  if (sw <= 0) stop("gen_int weights must not all be zero")
  w <- w / sw
  R <- as.numeric(Rt)
  H <- length(R)
  if (H == 0L) stop("Rt must have at least one step")
  if (any(R < 0)) stop("Rt must be non-negative")
  hist <- inc
  fc <- numeric(H)
  lam <- numeric(H)
  for (k in seq_len(H)) {
    L <- 0
    for (s in seq_len(S)) {
      j <- length(hist) - s + 1L
      if (j >= 1L) L <- L + hist[j] * w[s]
    }
    lam[k] <- L
    v <- R[k] * L
    fc[k] <- v
    hist <- c(hist, v)
  }
  Rimp <- numeric(0)
  if (n > S) for (t in (S + 1L):n) {
    L <- 0
    for (s in seq_len(S)) L <- L + inc[t - s] * w[s]
    Rimp <- c(Rimp, if (L > 0) inc[t] / L else NaN)
  }
  .t1_result(estimate = sum(fc), forecast = fc, lambda_ = lam,
             total = sum(fc), Rt_implied = Rimp, horizon = H, n = n,
             method = "Renewal-equation forecast")
}
