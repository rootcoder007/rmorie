# SPDX-License-Identifier: AGPL-3.0-or-later

#' Unbiased sample variance
#'
#' s^2 = (1/(n-1)) sum (xi - xbar)^2, reported beside the 1/n form.
#'
#' @param x numeric data, n >= 2.
#' @return list(sample_variance, population_variance).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (3.73).
#' @examples
#' SampVar(c(2, 4, 4, 4, 5, 5, 7, 9))$sample_variance
#' @export
SampVar <- function(x) {
  x <- as.numeric(x)
  if (length(x) < 2L || any(is.na(x))) {
    stop("sample variance needs n >= 2.", call. = FALSE)
  }
  xbar <- mean(x)
  list(sample_variance = sum((x - xbar)^2) / (length(x) - 1),
       population_variance = mean((x - xbar)^2))
}
