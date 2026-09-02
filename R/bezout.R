# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bezout coefficients by the extended Euclidean algorithm.
#'
#' Formula: a x + b y = gcd(a, b)
#'
#' @param a First integer.
#' @param b Second integer.

#' @return List with ``gcd``, ``x``, ``y``, ``check`` (a x + b y), ``a``, ``b``.
#' @references Bezout (1779), Theorie generale des equations algebriques. Not held locally; the extended Euclidean algorithm and the identity a x + b y = gcd(a, b) are standard published results.
#' @export
#' @examples
#' Bezout(240, 46)
Bezout <- function(a, b) {
  a0 <- as.numeric(a); b0 <- as.numeric(b)
  old_r <- a0; r <- b0; old_s <- 1; s <- 0; old_t <- 0; t <- 1
  while (r != 0) {
    q <- (old_r - (old_r %% r)) / r
    tmp <- old_r - q * r; old_r <- r; r <- tmp
    tmp <- old_s - q * s; old_s <- s; s <- tmp
    tmp <- old_t - q * t; old_t <- t; t <- tmp
  }
  if (old_r < 0) { old_r <- -old_r; old_s <- -old_s; old_t <- -old_t }
  .t1_result(gcd = old_r, x = old_s, y = old_t,
             check = a0 * old_s + b0 * old_t, a = a0, b = b0,
             method = "Bezout coefficients (extended Euclid)")
}
