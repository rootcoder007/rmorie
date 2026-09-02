# SPDX-License-Identifier: AGPL-3.0-or-later
#' beta-VAE objective with an optional capacity target.
#'
#' L = E_q\[log p(x|z)\] - beta D_KL(q||p) with
#' D_KL = 0.5 sum_j (mu_j^2 + sigma_j^2 - 1 - log sigma_j^2); the
#' capacity variant uses gamma |D_KL - C| in place of beta D_KL.
#'
#' @param x Observation.
#' @param xhat Decoder mean.
#' @param mu Posterior means.
#' @param logvar Posterior log variances.
#' @param beta Weight on the divergence.
#' @param capacity Target C, or NULL.
#' @param gamma Weight on |D_KL - C|; NULL reuses beta.
#' @param noisevar Decoder variance, strictly positive.
#'
#' @return List with objective, recon, kl, klper, penalty, beta, J, d.
#' @references Higgins et al. (2017), ICLR; capacity form from Burgess et
#'   al. (2018), arXiv:1804.03599; Gaussian KL from Kingma and Welling
#'   (2014), Appendix B.  Standard published form; the ICLR paper is not
#'   in the local corpus and was not read.
#' @export
#' @examples
#' Betavae(x = c(1, 2, 3, 4, 5, 6, 7, 8), xhat = c(1, 2, 3, 4, 5, 6, 7, 8), mu = c(1, 2, 3, 4, 5, 6, 7, 8), logvar = c(1, 2, 3, 4, 5, 6, 7, 8))
Betavae <- function(x, xhat, mu, logvar, beta = 4, capacity = NULL,
                    gamma = NULL, noisevar = 1) {
  x <- .t1_vec(x); xh <- .t1_vec(xhat); m <- .t1_vec(mu); lv <- .t1_vec(logvar)
  if (length(x) != length(xh)) stop("x and xhat must have the same length")
  if (length(m) != length(lv)) stop("mu and logvar must have the same length")
  nv <- as.numeric(noisevar)
  if (nv <= 0) stop("noisevar must be strictly positive")
  d <- length(x); J <- length(m)
  rec <- -sum((x - xh)^2) / (2 * nv) - 0.5 * d * log(2 * pi * nv)
  per <- 0.5 * (m^2 + exp(lv) - 1 - lv)
  kl <- sum(per)
  b <- as.numeric(beta)
  pen <- if (is.null(capacity)) b * kl else {
    g <- if (is.null(gamma)) b else as.numeric(gamma)
    g * abs(kl - as.numeric(capacity))
  }
  .t1_result(objective = rec - pen, recon = rec, kl = kl, klper = per,
             penalty = pen, beta = b, J = J, d = d,
             method = "beta-VAE objective (Higgins et al. 2017)")
}
