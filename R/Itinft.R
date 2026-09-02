# SPDX-License-Identifier: AGPL-3.0-or-later
#' Item information function
#'
#' For the two-parameter logistic the information peaks at theta = b
#' with value a^2 / 4; test information is the sum over items and the
#' asymptotic standard error of the ability estimate is its reciprocal
#' square root.
#'
#' Formula: I_j(theta) = a_j^2 P_j(theta) (1 - P_j(theta)).
#'
#' @param theta Ability values.
#' @param a Item slopes, positive.
#' @param b Item difficulties, same length as a.
#' @return List with \code{estimate} (largest test information),
#'   \code{information}, \code{test_information}, \code{se}, \code{n},
#'   \code{method}.
#' @references Lord (1980), Applications of Item Response Theory to
#'   Practical Testing Problems, Lawrence Erlbaum, ch. 5.
#' @export
#' @examples
#' Itinft(theta = 0.5, a = c(1, 2, 3, 4, 5, 6, 7, 8), b = c(1, 2, 3, 4, 5, 6, 7, 8))
Itinft <- function(theta, a, b) {
  th <- .s03vec(theta)
  av <- .s03vec(a)
  bv <- .s03vec(b)
  if (length(th) == 0L) stop("item_information_function: theta is empty")
  if (length(av) != length(bv)) stop("item_information_function: a and b have different lengths")
  if (length(av) == 0L) stop("item_information_function: no item parameters")
  if (any(av <= 0)) stop("item_information_function: slopes must be positive")
  info <- matrix(0, length(th), length(av))
  for (i in seq_along(th)) for (j in seq_along(av)) {
    p <- .s03sigmoid(av[j] * (th[i] - bv[j]))
    info[i, j] <- av[j] * av[j] * p * (1 - p)
  }
  ti <- rowSums(info)
  se <- ifelse(ti <= 0, Inf, 1 / sqrt(ti))
  .t1_result(estimate = max(ti), information = info, test_information = ti,
             se = se, n = length(th),
             method = "I_j(theta) = a_j^2 P (1 - P), Lord (1980) ch. 5")
}
