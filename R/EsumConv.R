# SPDX-License-Identifier: AGPL-3.0-or-later

#' Expectation of a sum from the convolved pmf
#'
#' Convolves the two pmfs, takes the expectation of the sum, and checks
#' it against E(X) + E(Y).
#'
#' @param values_x,probs_x the pmf of X; probs must sum to 1.
#' @param values_y,probs_y the pmf of Y, independent of X.
#' @return list(e_sum, e_x_plus_e_y).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (3.11)-(3.12).
#' @examples
#' EsumConv(1:2, c(0.5, 0.5), 1:3, rep(1 / 3, 3))$e_sum
#' @export
EsumConv <- function(values_x, probs_x, values_y, probs_y) {
  vx <- as.numeric(values_x)
  px <- as.numeric(probs_x)
  vy <- as.numeric(values_y)
  py <- as.numeric(probs_y)
  if (length(vx) != length(px) || length(vy) != length(py) ||
        length(vx) == 0L || length(vy) == 0L) {
    stop("values and probs must be equal-length, non-empty.", call. = FALSE)
  }
  if (any(px < 0) || any(py < 0) || abs(sum(px) - 1) > 1e-9 ||
        abs(sum(py) - 1) > 1e-9) {
    stop("each pmf must be >= 0 and sum to 1.", call. = FALSE)
  }
  sums <- as.vector(outer(vx, vy, "+"))
  ps <- as.vector(outer(px, py))
  agg <- tapply(ps, sums, sum)
  vals <- as.numeric(names(agg))
  e_sum <- sum(vals * as.numeric(agg))
  e_parts <- sum(vx * px) + sum(vy * py)
  if (abs(e_sum - e_parts) > 1e-9) stop("E(X+Y) != E(X) + E(Y).", call. = FALSE)
  list(e_sum = e_sum, e_x_plus_e_y = e_parts)
}
