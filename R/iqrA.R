# SPDX-License-Identifier: AGPL-3.0-or-later
#' Flag points outside the Tukey fences
#'
#' The rule is deliberately not a test: no null, no p-value, no
#' distributional assumption, only a pair of fences drawn from the middle
#' half of the data. The hinges are used rather than sample quantiles,
#' because the hinges are what Tukey defined and what a boxplot draws.
#'
#' Formula: flag \code{x < H_L - k (H_U - H_L)} or
#' \code{x > H_U + k (H_U - H_L)}.
#'
#' @param x Sample.
#' @param k Fence multiplier; 1.5 for outliers, 3 for far-out points.
#' @return List with \code{estimate}, \code{n_out}, \code{lower},
#'   \code{upper}, \code{iqr}, \code{flags}, \code{n}.
#' @references Tukey, J. W. (1977). Exploratory Data Analysis.
#'   Addison-Wesley, chapters 2 and 3.
#' @export
IqrA <- function(x, k = 1.5) {
  v <- as.numeric(unlist(x)); n <- length(v)
  s <- sort(v)
  n4 <- floor((n + 3) / 2) / 2
  at <- function(d) 0.5 * (s[floor(d)] + s[ceiling(d)])
  hl <- at(n4); hu <- at(n + 1 - n4)
  spread <- hu - hl
  lo <- hl - k * spread; hi <- hu + k * spread
  flags <- as.numeric(v < lo | v > hi)
  nout <- sum(flags)
  .t1_result(estimate = nout / n, n_out = nout, lower = lo, upper = hi,
             iqr = spread, flags = flags, n = n,
             method = "Tukey fences on the hinges")
}
