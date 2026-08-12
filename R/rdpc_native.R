# Renyi differential privacy budget of the Gaussian mechanism.
# Source: Mironov, I. (2017), Renyi differential privacy, IEEE CSF
# 2017 (arXiv 1702.07476): Proposition 7, which gives the closed-form
# Renyi divergence between a Gaussian and its offset,
# D_alpha(N(0, sigma^2) || N(mu, sigma^2)) = alpha mu^2 / (2 sigma^2),
# and the Corollary 3 that follows immediately from it -- for a
# sensitivity-1 function the Gaussian mechanism satisfies
# (alpha, alpha / (2 sigma^2))-RDP.  The general-sensitivity form used
# here puts mu = Delta into Proposition 7.
#
# Native implementation mirroring Python morie.fn.rdpc.

#' Renyi-DP budget of the Gaussian mechanism
#'
#' Returns \eqn{\varepsilon = \alpha \Delta^2 / (2\sigma^2)}, the
#' Renyi differential privacy budget at order \eqn{\alpha} of adding
#' \eqn{N(0, \sigma^2)} noise to a query of \eqn{\ell_2} sensitivity
#' \eqn{\Delta} (Mironov 2017, Proposition 7 and Corollary 3).  The
#' budget curve is linear in \eqn{\alpha}, which is what makes RDP
#' compose by simple addition.
#'
#' @param alpha Renyi order, strictly greater than 1.
#' @param sigma Noise standard deviation, positive.
#' @param sensitivity L2 sensitivity of the query, positive.
#' @return A list with \code{epsilon_rdp}, \code{estimate},
#'   \code{alpha}, \code{sigma}, \code{sensitivity}, \code{method}.
#' @references Mironov, I. (2017). Renyi differential privacy. IEEE
#'   Computer Security Foundations Symposium, 263-275.
#' @export
morie_rdpc <- function(alpha, sigma, sensitivity = 1) {
  alpha <- as.numeric(alpha); sigma <- as.numeric(sigma)
  sensitivity <- as.numeric(sensitivity)
  if (!(alpha > 1)) stop("alpha must exceed 1")
  if (sigma <= 0) stop("sigma must be positive")
  if (sensitivity <= 0) stop("sensitivity must be positive")
  eps <- alpha * sensitivity * sensitivity / (2 * sigma * sigma)
  list(epsilon_rdp = eps, estimate = eps, alpha = alpha, sigma = sigma,
       sensitivity = sensitivity,
       method = "Gaussian-mechanism RDP (Mironov 2017, Corollary 3)")
}
