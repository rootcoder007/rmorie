# SPDX-License-Identifier: AGPL-3.0-or-later

#' Intersection of k independent events of common probability
#'
#' P(all of k independent events, each of probability p) = p^k.  The
#' k = 2 and k = 3 cases are the pairwise and triple intersection terms
#' of the book's inclusion-exclusion dice example.
#'
#' @param p common event probability, in [0, 1].
#' @param k number of events, >= 0.
#' @return list(p, k, p_intersection).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs. (2.93)-(2.96).
#' @examples
#' PiidInt(1 / 6, 3)$p_intersection
#' @export
PiidInt <- function(p, k = 2) {
  p <- as.numeric(p)
  if (length(p) != 1L || is.na(p) || p < 0 || p > 1) {
    stop("p must be a single value in [0, 1].", call. = FALSE)
  }
  k <- as.integer(k)
  if (length(k) != 1L || is.na(k) || k < 0L) {
    stop("k must be a single integer >= 0.", call. = FALSE)
  }
  list(p = p, k = k, p_intersection = p^k)
}
