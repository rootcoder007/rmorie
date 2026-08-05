# SPDX-License-Identifier: AGPL-3.0-or-later
#' Stationary GP via Bochner's theorem
#'
#' Bochner: every continuous positive-definite kernel of a stationary
#' process is the Fourier transform of a finite spectral measure,
#' K(s - t) = int exp(-i <s - t, lambda>) dmu(lambda).  In one dimension
#' the squared-exponential process has spectral density
#' (2 sqrt(pi))^(-1) exp(-lambda^2 / 4), and inverting that integral must
#' return K(h) = exp(-h^2).  Doing the inversion numerically is the check
#' that the spectral representation used throughout the chapter is the
#' right one.
#'
#' Formula: K(h) = (2 sqrt(pi))^(-1) int cos(h lambda) exp(-lambda^2/4)
#'   dlambda, by midpoint rule over [-lam_max, lam_max].
#'
#' @param h Lag at which the kernel is evaluated.
#' @param n_grid Number of midpoint cells.
#' @param lam_max Truncation of the spectral integral.
#' @return List with \code{estimate} (numeric kernel),
#'   \code{kernel_exact}, \code{bochner_gap}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, Example 11.8,
#'   eqs. (11.3)-(11.4).
#' @export
Ghosalstatgpspec <- function(h = 0.6, n_grid = 4000, lam_max = 30) {
  n_grid <- as.integer(n_grid)
  if (n_grid < 1L) stop("n_grid must be positive")
  if (lam_max <= 0) stop("lam_max must be positive")
  step <- 2 * lam_max / n_grid
  lam <- -lam_max + (seq_len(n_grid) - 0.5) * step
  K_num <- sum(cos(h * lam) * exp(-lam * lam / 4) * step) / (2 * sqrt(pi))
  K_true <- exp(-h * h)
  .t1_result(estimate = K_num, kernel_exact = K_true,
             bochner_gap = abs(K_num - K_true),
             method = "Bochner spectral kernel (GvdV 2017 eq. 11.3-11.4)")
}
