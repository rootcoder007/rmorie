# SPDX-License-Identifier: AGPL-3.0-or-later
#' Self-similarity of fractional Brownian motion
#'
#' fBm satisfies f(lambda t) equal in distribution to lambda^H f(t), so
#' its variances obey E f(lambda t)^2 = lambda^(2H) E f(t)^2 EXACTLY --
#' this is an identity read straight off the kernel (11.6), not a limit.
#' Self-similarity is what makes the length-scale rescaling of section
#' 11.5 equivalent to a change of smoothness index.
#'
#' Formula: ((lambda t)^(2H)) / (t^(2H)) = lambda^(2H).
#'
#' @param H Hurst index, in (0, 1).
#' @param lam Scaling factor, positive.
#' @param t Time point, positive.
#' @return List with \code{estimate} (the variance ratio),
#'   \code{expected}, \code{gap}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 11.5.1.
#' @export
#' @examples
#' Ghosalselfsimgp()
Ghosalselfsimgp <- function(H = 0.6, lam = 3, t = 0.2) {
  H <- as.numeric(H); lam <- as.numeric(lam); t <- as.numeric(t)
  if (H <= 0 || H >= 1) stop("H must lie strictly between 0 and 1")
  if (lam <= 0) stop("lam must be positive")
  if (t <= 0) stop("t must be positive")
  ratio <- (lam * t)^(2 * H) / t^(2 * H)
  .t1_result(estimate = ratio, expected = lam^(2 * H),
             gap = abs(ratio - lam^(2 * H)),
             method = "fBm self-similarity (GvdV 2017 sec. 11.5.1)")
}
