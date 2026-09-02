# SPDX-License-Identifier: AGPL-3.0-or-later
#' GP density estimation contraction rate via the concentration function
#'
#' The density is f = exp(psi) / int exp(psi) with psi a Gaussian
#' process, which forces positivity and normalisation automatically --
#' the reason for the exponential link rather than a prior on f itself.
#' The rate then follows from the concentration function, and the KERNEL
#' decides the answer: a Matern process with smoothness matched to psi0
#' attains the minimax n^(-s/(2s+1)), while a squared-exponential process
#' has analytic sample paths, is far too smooth for a merely s-smooth
#' truth, and contracts only LOGARITHMICALLY unless its length scale is
#' itself given a prior.  That contrast is returned rather than buried.
#'
#' Formula: minimax = n^(-s/(2s+1)); rescaled SE picks up
#'   (log n)^((s+1)/(2s+1)); plain SE gives (log n)^(-s).
#'
#' @param x Observations; used for the sample size.
#' @param s Smoothness of the log-density; 1 when NULL.
#' @param n Sample size; taken from \code{x} when NULL.
#' @param kernel One of "squared_exponential", "matern", "rescaled_se".
#' @return List with \code{n}, \code{smoothness}, \code{kernel},
#'   \code{rate}, \code{minimax_rate}, \code{attains_minimax},
#'   \code{rate_kind}, \code{ratio_to_minimax}, \code{link},
#'   \code{driver}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 11.3.1 and ch. 11;
#'   van der Vaart & van Zanten.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ghosalgpdenscrt(V)
Ghosalgpdenscrt <- function(x, s = NULL, n = NULL,
                            kernel = "squared_exponential") {
  nn <- if (is.null(n)) length(as.numeric(x)) else as.integer(n)
  if (nn < 2) stop(sprintf("n must be at least 2, got %d.", nn))
  sv <- if (is.null(s)) 1 else as.numeric(s)
  if (sv <= 0) stop(sprintf("smoothness must be positive, got %g.", sv))
  if (!kernel %in% c("squared_exponential", "matern", "rescaled_se"))
    stop("kernel must be 'squared_exponential', 'matern' or 'rescaled_se'.")
  mm <- .morie_gh_minimax_rate(nn, sv)
  if (kernel == "matern") {
    rate <- mm; kind <- "polynomial (minimax)"; attains <- TRUE
  } else if (kernel == "rescaled_se") {
    rate <- mm * log(nn)^((sv + 1) / (2 * sv + 1))
    kind <- "polynomial up to a log factor"; attains <- FALSE
  } else {
    rate <- log(nn)^(-sv); kind <- "LOGARITHMIC"; attains <- FALSE
  }
  .t1_result(n = nn, smoothness = sv, kernel = kernel, rate = rate,
             minimax_rate = mm, attains_minimax = attains,
             rate_kind = kind, ratio_to_minimax = rate / mm,
             link = "f = exp(psi) / int exp(psi): positivity and normalisation for free",
             driver = "the concentration function: RKHS approximation + small-ball probability",
             method = "GP density contraction (Sec. 11.3.1); the kernel's smoothness decides the rate")
}
