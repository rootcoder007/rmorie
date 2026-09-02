# SPDX-License-Identifier: AGPL-3.0-or-later
#' Renyi differential privacy of the Gaussian mechanism
#'
#' RDP budget curve of the Gaussian mechanism: for a query of L2
#' sensitivity \eqn{\Delta} and noise scale \eqn{\sigma}, the mechanism
#' satisfies \eqn{(\alpha, \alpha \Delta^2 / (2 \sigma^2))}-RDP for
#' every \eqn{\alpha > 1} (Mironov 2017, Proposition 7 and Corollary 3).
#' The curve is a straight line in \eqn{\alpha}; n-fold composition
#' equals a single mechanism with scale \eqn{\sigma / \sqrt{n}} (remark
#' after Corollary 3). Conversion to (epsilon, delta)-DP is Proposition 3
#' and ships separately in \code{morie_renyi_dp_composition}.
#'
#' @param alpha Renyi order, must exceed 1.
#' @param sigma Noise standard deviation, positive.
#' @param sensitivity L2 sensitivity of the query, positive.
#' @return List with \code{epsilon_rdp}, \code{estimate}, \code{alpha},
#'   \code{sigma}, \code{sensitivity}, \code{method}.
#' @references Mironov, I. (2017). Renyi differential privacy. IEEE CSF
#'   2017, 263-275. arXiv:1702.07476. Proposition 7, Corollary 3,
#'   Table II.
#'   Local source: fetched-wave3/mironov-2017-renyi-differential-privacy-arxiv1702.07476.pdf
#' @export
#' @examples
#' Rdpc(alpha = 2, sigma = 1.0)
Rdpc <- function(alpha, sigma, sensitivity = 1) {
  alpha <- as.numeric(alpha)
  sigma <- as.numeric(sigma)
  sensitivity <- as.numeric(sensitivity)
  if (!(alpha > 1)) stop("alpha must exceed 1", call. = FALSE)
  if (sigma <= 0) stop("sigma must be positive", call. = FALSE)
  if (sensitivity <= 0) stop("sensitivity must be positive", call. = FALSE)
  eps <- alpha * sensitivity * sensitivity / (2 * sigma * sigma)
  .t1_result(epsilon_rdp = eps, estimate = eps, alpha = alpha,
             sigma = sigma, sensitivity = sensitivity,
             method = "Gaussian-mechanism RDP (Mironov 2017, Corollary 3)")
}
