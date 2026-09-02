# SPDX-License-Identifier: AGPL-3.0-or-later
#' Effective simple-random-sample size of a weighted design
#'
#' deff = 1 + cv^2(w) = n * sum(w^2) / (sum w)^2 and
#' n_eff = n / deff = (sum w)^2 / sum(w^2).
#'
#' @param w Positive selection weights, one per unit.
#'
#' @return List with neff, deff, cv2, n, sumw, sumw2.
#' @references Kish, L. (1965), Survey Sampling, Wiley, Sect. 11.7.
#'   Standard published form; the monograph is not in the local corpus and
#'   was not read.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Neffsrs(V)
Neffsrs <- function(w) {
  w <- .t1_vec(w)
  n <- length(w)
  if (n == 0) stop("w must be non-empty")
  if (any(w <= 0)) stop("weights must be strictly positive")
  s1 <- sum(w)
  s2 <- sum(w^2)
  deff <- n * s2 / (s1 * s1)
  .t1_result(
    neff = s1 * s1 / s2, deff = deff, cv2 = deff - 1,
    n = n, sumw = s1, sumw2 = s2,
    method = "Kish effective sample size, deff = 1 + cv^2(w)"
  )
}
