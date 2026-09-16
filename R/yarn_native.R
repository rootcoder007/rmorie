# SPDX-License-Identifier: AGPL-3.0-or-later
#
# YaRN context-window scaling (Yarn). Bit-identical mirror of
# src/morie/fn/yarn.py. NOTE kmyarn implements only the uniform
# NTK-aware rescaling -- a DIFFERENT interpolation from the same
# paper; not aliased.

#' YaRN context-window scaling: NTK-by-parts + ramp + temperature
#'
#' Peng, Quesnelle, Fan and Shippole (2023), "YaRN: Efficient Context
#' Window Extension of Large Language Models", arXiv:2309.00071 (ICLR
#' 2024). Eq 17 rotation counts r(d) = L theta_d / (2 pi); Eq 18 ramp
#' gamma; Eq 20 blend h(theta_d) = (1 - gamma) theta_d / s +
#' gamma theta_d; Eq 22 temperature sqrt(1/t) = 0.1 ln(s) + 1.
#'
#' @param base RoPE base b (scalar > 1), or the d/2 frequencies
#'   directly (vector).
#' @param s Context extension scale factor (> 0).
#' @param d Head embedding width, positive even integer.
#' @param L Original context length (> 0).
#' @param beta_fast,beta_slow Rotation-count bounds (paper Eq 18 with
#'   alpha = beta_slow, beta = beta_fast; defaults 32 and 1).
#' @return List with \code{theta}, \code{theta_new}, \code{rotations},
#'   \code{gamma}, \code{temperature}, \code{logit_scale},
#'   \code{scale}, \code{estimate}, \code{n}, \code{method}.
#' @references Peng, B., Quesnelle, J., Fan, H. and Shippole, E.
#'   (2023), arXiv:2309.00071, Sections 3.2-3.4, Eqs 17/18/20/22.
#'   Local source: fetched-wave3/peng-etal-2023-yarn-arxiv2309.00071.pdf.
#' @export
Yarn <- function(base, s, d, L, beta_fast = 32, beta_slow = 1) {
  d <- as.integer(d)
  if (d < 2L || d %% 2L != 0L) stop(sprintf("Yarn: d must be a positive even width, got %d", d), call. = FALSE)
  s <- as.numeric(s)[1]
  if (s <= 0) stop(sprintf("Yarn: scale factor must be positive, got %g", s), call. = FALSE)
  L <- as.numeric(L)[1]
  if (L <= 0) stop(sprintf("Yarn: original context length must be positive, got %g", L), call. = FALSE)
  alpha <- as.numeric(beta_slow)[1]
  beta <- as.numeric(beta_fast)[1]
  if (!(alpha < beta)) stop(sprintf("Yarn: need beta_slow < beta_fast, got %g and %g", alpha, beta), call. = FALSE)
  half <- d %/% 2L
  if (length(base) == 1L) {
    b <- as.numeric(base)
    if (b <= 1) stop(sprintf("Yarn: a scalar base must exceed 1, got %g", b), call. = FALSE)
    freqs <- b^(-2 * (0:(half - 1L)) / d)
  } else {
    freqs <- as.numeric(base)
    if (length(freqs) != half) stop(sprintf("Yarn: need the d/2 = %d frequencies, got %d", half, length(freqs)), call. = FALSE)
    if (any(freqs <= 0)) stop("Yarn: frequencies must be positive", call. = FALSE)
  }
  rot <- L * freqs / (2 * pi)
  gam <- ifelse(rot < alpha, 0, ifelse(rot > beta, 1, (rot - alpha) / (beta - alpha)))
  new <- (1 - gam) * freqs / s + gam * freqs
  sqrt_inv_t <- 0.1 * log(s) + 1
  inv_t <- sqrt_inv_t * sqrt_inv_t
  list(theta = freqs, theta_new = new, rotations = rot, gamma = gam,
       temperature = 1 / inv_t, logit_scale = inv_t, scale = s,
       estimate = new[half], n = half,
       method = "YaRN NTK-by-parts + ramp + temperature (Peng et al. 2023, Eqs 17/18/20/22)")
}
