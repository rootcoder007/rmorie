# SPDX-License-Identifier: AGPL-3.0-or-later

#' Group-average relation
#'
#' yavg = m xavg.
#'
#' @param m slope.
#' @param xavg group average of X.
#' @return list(yavg).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (6.39).
#' @examples
#' GroupAvg(0.5, 4)$yavg
#' @export
GroupAvg <- function(m, xavg) {
  m <- as.numeric(m)
  xavg <- as.numeric(xavg)
  if (length(m) != 1L || length(xavg) != 1L || is.na(m) || is.na(xavg)) {
    stop("m and xavg must be single values.", call. = FALSE)
  }
  list(yavg = m * xavg)
}
