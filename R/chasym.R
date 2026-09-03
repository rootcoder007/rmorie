# SPDX-License-Identifier: AGPL-3.0-or-later

# Park-Miller multiplicative LCG: a bootstrap needs an INDEPENDENT
# stream, not a low-discrepancy one -- a shared quasi-random sequence
# correlates the resample positions and shrinks the bootstrap spread.
#' Park-Miller multiplicative LCG: a bootstrap needs an INDEPENDENT
#'
#' stream, not a low-discrepancy one -- a shared quasi-random sequence
#' correlates the resample positions and shrinks the bootstrap spread.
#'
#' @param seed Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{function}.
#' @export
#' @examples
#' res <- .msm_lcg(seed = 1L)
#' res
.msm_lcg <- function(seed) {
  st <- as.numeric(seed) %% 2147483647
  if (st == 0) st <- 1
  function() {
    st <<- (st * 48271) %% 2147483647
    (st - 1) / 2147483646
  }
}

#' Asymptotic-variance check for an IPW marginal structural model
#'
#' Formula: compare E\[psi^2\]/n to the bootstrap
#'
#' The influence function of the IPW mean is psi_i = w_i (Y_i - theta),
#' so the sandwich variance is
#' sum w_i^2 (Y_i - theta)^2 / (sum w_i)^2.  If the estimator really is
#' asymptotically linear that number matches the bootstrap variance; a
#' systematic gap is the signal that the weights are too unstable for
#' the normal approximation.  With unit weights the sandwich reduces to
#' the plain sample variance over n.
#'
#' @param y Outcome.
#' @param A Treatment indicator, or NULL.
#' @param H IPW weights, or NULL for unit weights.
#' @param B Bootstrap replicates.
#' @param seed Seed of the deterministic bootstrap stream.
#' @return List with \code{estimate}, \code{theta}, \code{var_if},
#'   \code{var_boot}, \code{ratio}, \code{ess}, \code{agree}, \code{n},
#'   \code{B}, \code{method}.
#' @references van der Laan & Rose (2011), Targeted Learning, Springer,
#'   ch. 5; Funk et al. (2011), Am. J. Epidemiology 173(7):761-767.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Chasym(V)
Chasym <- function(y, A = NULL, H = NULL, B = 200, seed = 42) {
  yv <- .s03vec(y)
  n <- length(yv)
  if (n < 2L) stop("need at least two observations")
  w <- if (is.null(H)) rep(1, n) else .s03vec(H)
  if (length(w) != n) stop("y and H must have the same length")
  if (any(w < 0)) stop("weights must be non-negative")
  if (!is.null(A)) {
    av <- .s03vec(A)
    if (length(av) != n) stop("y and A must have the same length")
  }
  sw <- 0
  for (v in w) sw <- sw + v
  if (sw <= 0) stop("weights must not sum to zero")
  num <- 0
  for (i in seq_len(n)) num <- num + w[i] * yv[i]
  theta <- num / sw
  var_if <- 0
  for (i in seq_len(n)) var_if <- var_if + (w[i] * (yv[i] - theta))^2
  var_if <- var_if / (sw * sw)
  B <- as.integer(B)
  if (B < 2L) stop("B must be at least 2")
  u <- .msm_lcg(seed)
  reps <- numeric(B)
  for (b in seq_len(B)) {
    sw_b <- 0
    sy_b <- 0
    for (i in seq_len(n)) {
      k <- as.integer(u() * n) + 1L
      if (k > n) k <- n
      sw_b <- sw_b + w[k]
      sy_b <- sy_b + w[k] * yv[k]
    }
    reps[b] <- if (sw_b > 0) sy_b / sw_b else NaN
  }
  mb <- 0
  for (v in reps) mb <- mb + v
  mb <- mb / B
  var_b <- 0
  for (v in reps) var_b <- var_b + (v - mb)^2
  var_b <- var_b / (B - 1)
  s2 <- 0
  for (v in w) s2 <- s2 + v * v
  ess <- sw * sw / s2
  ratio <- if (var_if > 0) var_b / var_if else NaN
  .t1_result(estimate = ratio, theta = theta, var_if = var_if,
             var_boot = var_b, ratio = ratio, ess = ess,
             agree = as.integer(!is.nan(ratio) && ratio >= 0.5 && ratio <= 2),
             n = n, B = B,
             method = "asymptotic-variance check for an IPW MSM")
}
