# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mixtures of beta processes
#'
#' H ~ int BP(c, H0_lambda) dPi(lambda) mixes over the base hazard.  The
#' prior mean is then the weighted mixture of the component means,
#' E H(t) = sum_j w_j H0_{lambda_j}(t) -- mixing enlarges the support
#' without disturbing the mean structure, which is why it is the standard
#' way to make a beta process prior less committal.
#'
#' Formula: E H(t) = sum_j w_j lambda_j t, with w the normalised
#'   weights.
#'
#' @param lambdas Component hazard rates.
#' @param weights Mixing weights; uniform when NULL.
#' @param c Concentration (does not enter the mean).
#' @param t Time point.
#' @return List with \code{estimate} (mixture mean hazard),
#'   \code{component_means}, \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 13.3.4.
#' @export
#' @examples
#' Ghosalmixbp()
Ghosalmixbp <- function(lambdas = c(0.5, 1, 2), weights = NULL, c = 3,
                        t = 1) {
  ls <- as.numeric(lambdas)
  if (length(ls) == 0L) stop("lambdas must be non-empty")
  if (is.null(weights)) weights <- rep(1 / length(ls), length(ls))
  w <- as.numeric(weights)
  if (length(w) != length(ls))
    stop("weights and lambdas must have the same length")
  if (any(w < 0)) stop("weights must be non-negative")
  if (sum(w) <= 0) stop("weights must sum to a positive value")
  w <- w / sum(w)
  .t1_result(estimate = sum(w * ls * t), component_means = ls * t,
             method = "mixture of beta processes (GvdV 2017 sec. 13.3.4)")
}
