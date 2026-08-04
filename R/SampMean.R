# SPDX-License-Identifier: AGPL-3.0-or-later

#' Sample mean
#'
#' X-bar = (X1 + ... + Xn)/n.
#'
#' @param x numeric sample, non-empty.
#' @return list(mean, n).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (3.54).
#' @examples
#' SampMean(c(1, 2, 3, 4))$mean
#' @export
SampMean <- function(x) {
  x <- as.numeric(x)
  if (length(x) == 0L || any(is.na(x))) {
    stop("x must be a non-empty numeric vector.", call. = FALSE)
  }
  list(mean = mean(x), n = length(x))
}
