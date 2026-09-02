# SPDX-License-Identifier: AGPL-3.0-or-later
#' Test a null value against the Imbens-Manski confidence interval
#'
#' The decision is whether the interval that covers the true parameter with
#' probability \code{1 - alpha} contains \code{theta_0}. The critical value
#' is the one already implemented in \code{morie_bnd_imbens_manski} and
#' reached through \code{Bndfre}; nothing about the construction is
#' re-derived here.
#'
#' @param lower,upper Replicated estimates of the two bounds, same length.
#' @param se The null value \code{theta_0}, default 0.
#' @param cdf The level \code{alpha}, default 0.05.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{covers}, \code{reject}, \code{c}, \code{theta_0}, \code{n}.
#' @section Note: the parameter names \code{se} and \code{cdf} are
#'   inherited generator boilerplate, kept so existing positional calls do
#'   not break; they carry the meanings documented above. Stoye's (2009)
#'   refinement, which pre-tests the width of the estimated bounds and
#'   switches between a one- and a two-sided critical value, is NOT
#'   implemented: its pre-test threshold could not be verified against an
#'   accessible copy.
#' @references Imbens, G. W. and Manski, C. F. (2004). Confidence intervals
#'   for partially identified parameters. Econometrica 72(6), 1845-1857,
#'   equation (6). \doi{10.1111/j.1468-0262.2004.00555.x}. Stoye, J.
#'   (2009). More on confidence intervals for partially identified
#'   parameters. Econometrica 77(4), 1299-1315. \doi{10.3982/ECTA7347}.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Bnstst(V, V)
Bnstst <- function(lower, upper, se = 0, cdf = 0.05) {
  r <- Bndfre(lower, upper, as.numeric(cdf)[1])
  t0 <- as.numeric(se)[1]
  covers <- if (r$lower <= t0 && t0 <= r$upper) 1 else 0
  .t1_result(lower = r$lower, upper = r$upper, width = r$width,
             covers = covers, reject = 1 - covers, c = r$c,
             theta_0 = t0, n = r$n,
             method = "Inference on an interval-identified parameter")
}
