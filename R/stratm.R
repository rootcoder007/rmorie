# SPDX-License-Identifier: AGPL-3.0-or-later
#' Stratified estimate of the population mean, with its variance
#'
#' Because the strata are sampled independently the variance is a
#' weighted sum of WITHIN-stratum variances only. ybar_st equals the
#' sample mean only under proportional allocation, so the unweighted mean
#' is returned alongside for contrast. \code{h} holds one-based labels.
#'
#' Formula: W_h = N_h/N, ybar_st = sum_h W_h ybar_h,
#'   V(ybar_st) = sum_h W_h^2 (1 - f_h) s_h^2 / n_h, f_h = n_h/N_h
#'
#' @param y Observations.
#' @param h One-based stratum label of each observation.
#' @param Nh Population size of each stratum, length L.
#' @param level Confidence level.
#' @return List with \code{estimate}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}, \code{stratum_mean}, \code{stratum_var}, \code{nh},
#'   \code{Wh}, \code{unweighted_mean}, \code{N}, \code{n}, \code{L}.
#' @references Cochran (1977), Sampling Techniques, 3rd edition, Section
#'   5.3, Theorem 5.3: V(ybar_st) = sum W_h^2 S_h^2 (1 - f_h)/n_h, the
#'   cross terms vanishing because the strata are drawn independently.
#'   Chapter 5 read from the scanned original. Cross-checked against the
#'   reference implementation in the CRAN package samplingbook 1.2.4,
#'   whose stratamean forms sum(Meanh*wh) and sum(Varh*wh^2).
#' @export
Stratmean <- function(y, h, Nh, level = 0.95) {
  y <- .t1_vec(y); h <- as.integer(.t1_vec(h)); Nh <- .t1_vec(Nh)
  if (length(y) != length(h)) stop("y and h must have the same length")
  L <- length(Nh)
  if (L < 1L) stop("at least one stratum is required")
  if (any(h < 1L | h > L))
    stop("h must hold one-based stratum labels in 1..L")
  if (level <= 0 || level >= 1)
    stop("level must lie strictly between 0 and 1")
  N <- sum(Nh); W <- Nh / N
  mh <- vh <- nh <- numeric(L)
  for (s in seq_len(L)) {
    ys <- y[h == s]; m <- length(ys)
    if (m < 2L) stop("every stratum needs at least two observations")
    if (m > Nh[s]) stop("a stratum sample cannot exceed its population")
    nh[s] <- m
    mh[s] <- mean(ys)
    vh[s] <- (Nh[s] - m) / Nh[s] * stats::var(ys) / m
  }
  est <- sum(W * mh); var <- sum(W^2 * vh); se <- sqrt(var)
  z <- stats::qnorm((1 + level) / 2)
  .t1_result(estimate = est, se = se, ci_lower = est - z * se,
             ci_upper = est + z * se, stratum_mean = mh, stratum_var = vh,
             nh = nh, Wh = W, unweighted_mean = mean(y), N = N,
             n = length(y), L = L,
             method = "Stratified mean, Cochran Theorem 5.3")
}
