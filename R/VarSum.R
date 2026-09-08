# SPDX-License-Identifier: AGPL-3.0-or-later

#' Variance of a sum of independent variables
#'
#' The running partial sums make the recursion of eq (3.31) explicit:
#' Var(X1+...+Xn) = Var(X1+...+X_\{n-1\}) + Var(Xn).
#'
#' @param variances per-variable variances, each >= 0.
#' @return list(var_sum, partial_sums).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs (3.25), (3.30)-(3.31).
#' @examples
#' VarSum(c(1, 4, 9))$var_sum
#' @export
VarSum <- function(variances) {
  v <- as.numeric(variances)
  if (length(v) == 0L || any(is.na(v)) || any(v < 0)) {
    stop("variances must be non-empty and >= 0.", call. = FALSE)
  }
  steps <- cumsum(v)
  running <- steps[length(steps)]
  if (abs(running - sum(v)) > 1e-12 * max(1, running)) {
    stop("recursion disagrees with the direct sum.", call. = FALSE)
  }
  list(var_sum = running, partial_sums = steps)
}
