# SPDX-License-Identifier: AGPL-3.0-or-later

#' Multinomial coefficient
#'
#' N!/(n1! n2! ... nk!).  When sum(ns) < N the leftover items form one
#' extra implicit committee (Morin's remark below eq (1.35)).
#'
#' @param ns committee sizes, all >= 0.
#' @param N total items; defaults to sum(ns).
#' @return list(ns, N, coefficient, assignments); assignments is an alias
#'   of coefficient kept for eq (1.35) callers.
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs. (1.35), (1.37).
#' @examples
#' Multinom(c(3, 2, 5))$coefficient
#' @export
Multinom <- function(ns, N = NULL) {
  ns <- as.integer(ns)
  if (length(ns) == 0L || any(is.na(ns)) || any(ns < 0L)) {
    stop("ns must be a non-empty vector of integers >= 0.", call. = FALSE)
  }
  if (is.null(N)) N <- sum(ns)
  N <- as.integer(N)
  if (is.na(N) || N < 0L) stop("N must be an integer >= 0.", call. = FALSE)
  if (sum(ns) > N) stop("sum(ns) exceeds N.", call. = FALSE)
  full <- if (sum(ns) < N) c(ns, N - sum(ns)) else ns
  value <- round(prod(choose(cumsum(full), full)))
  list(ns = ns, N = N, coefficient = value, assignments = value)
}
