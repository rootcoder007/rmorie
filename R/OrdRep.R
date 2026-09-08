# SPDX-License-Identifier: AGPL-3.0-or-later

#' Ordered sampling with replacement
#'
#' The number of outcomes of n picks from N objects, with replacement
#' and with the order mattering, is N^n (not n^N).
#'
#' @param N number of distinct objects in the box.
#' @param n number of picks.
#' @return list(n_objects, n_picks, count, log_count).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eq. (1.4).
#' @examples
#' OrdRep(6, 5)$count
#' @export
OrdRep <- function(N, n) {
  N <- as.integer(N)
  n <- as.integer(n)
  if (is.na(N) || is.na(n) || N < 0L || n < 0L) {
    stop("N and n must be non-negative integers.", call. = FALSE)
  }
  count <- N^n
  log_count <- if (N > 0L && n > 0L) n * log(N) else 0
  list(n_objects = as.numeric(N), n_picks = as.numeric(n),
       count = as.numeric(count), log_count = as.numeric(log_count))
}
