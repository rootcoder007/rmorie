# SPDX-License-Identifier: AGPL-3.0-or-later

#' Linear approximation to the exponential
#'
#' 1 + x beside exp(x).
#'
#' @param x argument.
#' @return list(exact, approx, abs_error).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (7.9).
#' @examples
#' ExpLinApx(0.01)$abs_error
#' @export
ExpLinApx <- function(x) {
  x <- as.numeric(x)
  if (length(x) != 1L || is.na(x)) stop("x must be a single value.", call. = FALSE)
  list(exact = exp(x), approx = 1 + x, abs_error = abs(exp(x) - (1 + x)))
}
