# SPDX-License-Identifier: AGPL-3.0-or-later
#' PPS design quantities for a frame of units
#'
#' \code{y} and \code{size} describe the frame -- every unit that could be
#' drawn -- and \code{n} is the number of draws. That is the only reading
#' under which \code{pi_i = n x_i / sum_k x_k} is a probability: the
#' denominator is the population size total, not a sample total. A unit
#' with \code{pi_i > 1} must be taken with certainty, so it is an error
#' here rather than a silently truncated probability.
#'
#' The Hansen-Hurwitz design variance is available in closed form from
#' the frame and is exactly zero when \code{y} is proportional to
#' \code{size} -- the case PPS is designed for, and the anchor used here.
#'
#' Formula: \code{p_i = x_i / X}; \code{pi_i = n p_i};
#' \code{Var(Yhat_HH) = (1/n) sum_i p_i (y_i / p_i - Y)^2}.
#'
#' @param y Values for the frame units.
#' @param size Positive size measure, same length as \code{y}.
#' @param n Number of draws, at least 1.
#' @return List with \code{pi}, \code{p}, \code{estimate}, \code{total},
#'   \code{hh_variance}, \code{hh_se}, \code{srs_variance}, \code{deff},
#'   \code{X}, \code{N}, \code{n}.
#' @references Hansen, M. H. & Hurwitz, W. N. (1943). On the theory of
#'   sampling from finite populations. Annals of Mathematical Statistics
#'   14(4):333-362. \doi{10.1214/aoms/1177731356}.
#' @export
Ppsamp <- function(y, size, n) {
  y <- as.numeric(unlist(y)); x <- as.numeric(unlist(size))
  if (length(y) == 0L) stop("Ppsamp: y is empty")
  if (length(x) != length(y)) stop("Ppsamp: size must have one entry per unit")
  if (any(x <= 0)) stop("Ppsamp: sizes must be positive")
  n <- as.integer(n)
  if (n < 1L) stop("Ppsamp: n must be at least 1")
  N <- length(y); X <- sum(x); p <- x / X; pi <- n * p
  if (any(pi > 1))
    stop("Ppsamp: an inclusion probability exceeds 1; that unit must be selected with certainty")
  Y <- sum(y)
  v <- sum(p * (y / p - Y)^2) / n
  sv <- sum((N * y - Y)^2 / N) / n
  .t1_result(pi = pi, p = p, estimate = Y, total = Y,
             hh_variance = v, hh_se = sqrt(v), srs_variance = sv,
             deff = if (sv > 0) v / sv else NA_real_,
             X = X, N = N, n = n,
             method = "PPS selection probabilities and Hansen-Hurwitz design variance")
}
