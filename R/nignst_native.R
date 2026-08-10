# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Conjugate normal model, N-Inv-chi2 update (Nignst). Bit-identical
# mirror of src/morie/fn/nignst.py.

#' Conjugate N-Inv-chi2 posterior update for a normal sample
#'
#' Exact conjugate update for iid N(mu, sigma^2) data under the prior
#' sigma^2 distributed Inv-chi2(nu0, sigma0^2) and mu given sigma^2
#' distributed N(mu0, sigma^2/kappa0) -- the (mu, sigma^2)
#' parameterization of the normal-inverse-gamma family. Posterior
#' parameters (Gelman et al., BDA3, Section 3.3, Eq. 3.8):
#' \eqn{\mu_n = (\kappa_0 \mu_0 + n \bar y)/(\kappa_0 + n)},
#' \eqn{\kappa_n = \kappa_0 + n}, \eqn{\nu_n = \nu_0 + n},
#' \eqn{\nu_n \sigma_n^2 = \nu_0 \sigma_0^2 + (n-1) s^2 +
#' \kappa_0 n (\bar y - \mu_0)^2 / (\kappa_0 + n)}.
#' The marginal posterior of mu is t with nu_n degrees of freedom,
#' centre mu_n and scale^2 sigma_n^2/kappa_n; the posterior predictive
#' for one new observation has scale^2 sigma_n^2 (kappa_n + 1)/kappa_n.
#'
#' @param y Observations.
#' @param mu0 Prior location of mu.
#' @param kappa0 Prior pseudo-count for the mean.
#' @param nu0 Prior degrees of freedom for sigma^2.
#' @param sigma0_sq Prior scale of sigma^2.
#' @return List with \code{estimate}, \code{mu_n}, \code{kappa_n},
#'   \code{nu_n}, \code{sigma_n_sq}, \code{mu_scale_sq},
#'   \code{pred_scale_sq}, \code{n}, \code{ybar}, \code{s_sq},
#'   \code{method}.
#' @references Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B.,
#'   Vehtari, A. and Rubin, D. B. (2013), Bayesian Data Analysis,
#'   3rd ed., Chapman and Hall/CRC, Section 3.3, Eqs. 3.7-3.8; local
#'   copy fetched-wave3/gelman-etal-2013-bayesian-data-analysis-3ed.pdf.
#' @export
Nignst <- function(y, mu0, kappa0, nu0, sigma0_sq) {
  yv <- as.numeric(y)
  n <- length(yv)
  if (n < 1L) stop("need at least one observation", call. = FALSE)
  mu0 <- as.numeric(mu0); kappa0 <- as.numeric(kappa0)
  nu0 <- as.numeric(nu0); sigma0_sq <- as.numeric(sigma0_sq)
  if (kappa0 <= 0 || nu0 <= 0 || sigma0_sq <= 0) {
    stop("kappa0, nu0, sigma0_sq must be positive", call. = FALSE)
  }
  ybar <- mean(yv)
  s_sq <- if (n > 1L) stats::var(yv) else 0
  kappa_n <- kappa0 + n
  nu_n <- nu0 + n
  mu_n <- (kappa0 * mu0 + n * ybar) / kappa_n
  nusq <- nu0 * sigma0_sq + (n - 1) * s_sq +
    kappa0 * n * (ybar - mu0)^2 / kappa_n
  sigma_n_sq <- nusq / nu_n
  list(estimate = mu_n, mu_n = mu_n, kappa_n = kappa_n, nu_n = nu_n,
       sigma_n_sq = sigma_n_sq,
       mu_scale_sq = sigma_n_sq / kappa_n,
       pred_scale_sq = sigma_n_sq * (kappa_n + 1) / kappa_n,
       n = n, ybar = ybar, s_sq = s_sq,
       method = "BDA3 Section 3.3 N-Inv-chi2 conjugate update")
}
