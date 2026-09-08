# SPDX-License-Identifier: AGPL-3.0-or-later

#' Mode of the Poisson distribution
#'
#' Both neighbours of the claimed mode are checked, so a wrong
#' tie-breaking rule fails loudly rather than silently.
#'
#' @param a expected count, > 0.
#' @return list(mode, p_mode).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq (4.89).
#' @examples
#' PoisMode(3.7)$mode
#' @export
PoisMode <- function(a) {
  a <- as.numeric(a)
  if (length(a) != 1L || is.na(a) || a <= 0) {
    stop("a must be a single value > 0.", call. = FALSE)
  }
  k_star <- max(as.integer(ceiling(a)) - 1L, 0L)
  p_star <- PoisPmf(k_star, a)$probability
  lo <- if (k_star >= 1L) PoisPmf(k_star - 1L, a)$probability else 0
  hi <- PoisPmf(k_star + 1L, a)$probability
  if (lo > p_star + 1e-15 || hi > p_star + 1e-15) {
    stop("neighbour beats the claimed mode.", call. = FALSE)
  }
  list(mode = k_star, p_mode = p_star)
}
