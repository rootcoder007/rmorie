# SPDX-License-Identifier: AGPL-3.0-or-later
#' Confidence interval for a partially identified parameter (alias)
#'
#' The stub set carried two module names for one construction:
#' "frequentist bound with valid coverage" and "confidence interval for a
#' partially identified parameter" are both the Imbens-Manski (2004)
#' equation (6) interval. This name is kept working and forwards to
#' \code{Bndfre}; it is not a second implementation.
#'
#' @param lower,upper Replicated estimates of the two bounds.
#' @param alpha Miss probability, default 0.05.
#' @return The payload of \code{Bndfre}, with \code{method} naming this
#'   entry point.
#' @references Imbens, G. W. and Manski, C. F. (2004). Confidence intervals
#'   for partially identified parameters. Econometrica 72(6), 1845-1857,
#'   equation (6). \doi{10.1111/j.1468-0262.2004.00555.x}.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Bnscrf(V, V)
Bnscrf <- function(lower, upper, alpha = 0.05) {
  r <- Bndfre(lower, upper, alpha)
  r$method <- "Confidence interval for partially identified parameter"
  r
}
