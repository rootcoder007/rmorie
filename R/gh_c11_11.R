# SPDX-License-Identifier: AGPL-3.0-or-later
#' Rescaled Gaussian process
#'
#' f_l(x) = f(x / l) changes the kernel to K(s/l, t/l): shrinking the
#' length scale l roughens the sample paths and lengthening it smooths
#' them.  Because the smoothness of a Gaussian prior is what fixes its
#' contraction rate, putting a prior on l is exactly how a single process
#' adapts to an unknown smoothness -- the mechanism of section 11.5.  For
#' the squared-exponential kernel the correlation at lag h is
#' exp(-(h/l)^2), which falls as l shrinks.
#'
#' Formula: rho_l(h) = exp(-(h / l)^2).
#'
#' @param lengths Vector of length scales, all positive.
#' @param h Lag.
#' @return List with \code{estimate} (correlation at the last length),
#'   \code{correlation_by_length}, \code{roughens_as_l_shrinks},
#'   \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, Example 11.11 and
#'   section 11.5.
#' @export
Ghosalrescalgp <- function(lengths = c(2, 1, 0.25), h = 0.3) {
  lengths <- as.numeric(lengths)
  if (length(lengths) == 0L) stop("lengths must be non-empty")
  if (any(lengths <= 0)) stop("every length scale must be positive")
  cors <- exp(-(h / lengths)^2)
  mono <- if (length(cors) < 2L) TRUE else
    all(cors[-1] <= cors[-length(cors)] + 1e-15)
  .t1_result(estimate = cors[length(cors)],
             correlation_by_length = cors,
             roughens_as_l_shrinks = mono,
             method = "rescaled GP (GvdV 2017 Ex 11.11, sec. 11.5)")
}
