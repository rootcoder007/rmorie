# SPDX-License-Identifier: AGPL-3.0-or-later
#' Gaussian mechanism for differential privacy
#'
#' The bound is only valid for epsilon below one, which is checked
#' rather than assumed; the noise is drawn from the deterministic
#' inverse-normal stream so the released value is reproducible across
#' language arms.
#'
#' Formula: M(D) = f(D) + N(0, sigma^2) with
#'   sigma = sqrt(2 log(1.25/delta)) Delta_2 f / epsilon.
#'
#' @param f_value Query value(s) to release.
#' @param l2_sens L2 sensitivity of the query, positive.
#' @param epsilon Privacy parameter in (0, 1).
#' @param delta Failure probability in (0, 1).
#' @param draw Index into the deterministic noise stream.
#' @return List with \code{estimate}, \code{released}, \code{noise},
#'   \code{sigma}, \code{c}, \code{epsilon}, \code{delta}, \code{n},
#'   \code{method}.
#' @references Dwork and Roth (2014), The Algorithmic Foundations of
#'   Differential Privacy, Foundations and Trends in Theoretical
#'   Computer Science 9(3-4):211-407, Theorem A.1.
#'   \doi{10.1561/0400000042}
#' @export
#' @examples
#' Gaussm(c(1, 2, 3), l2_sens = 1, epsilon = 0.5, delta = 1e-5)
Gaussm <- function(f_value, l2_sens, epsilon, delta, draw = 1) {
  fv <- .s03vec(f_value)
  if (length(fv) == 0L) stop("gaussian_mechanism: f_value is empty")
  s <- as.numeric(l2_sens); e <- as.numeric(epsilon); d <- as.numeric(delta)
  if (s <= 0) stop("gaussian_mechanism: the L2 sensitivity must be positive")
  if (!(e > 0 && e < 1)) stop("gaussian_mechanism: Theorem A.1 requires 0 < epsilon < 1")
  if (!(d > 0 && d < 1)) stop("gaussian_mechanism: delta must lie in (0, 1)")
  cc <- sqrt(2 * log(1.25 / d))
  sigma <- cc * s / e
  k <- as.integer(draw)
  noise <- vapply(seq_along(fv) - 1L, function(i) sigma * .s03qnorm(.s03vdc(k + i)), 0)
  out <- fv + noise
  .t1_result(estimate = out[1], released = out, noise = noise, sigma = sigma,
             c = cc, epsilon = e, delta = d, n = length(fv),
             method = "f(D) + N(0, sigma^2) with sigma = sqrt(2 ln(1.25/delta)) Delta_2 f / eps, Dwork & Roth (2014) Thm A.1")
}
