# SPDX-License-Identifier: AGPL-3.0-or-later

#' Counting process for survival data
#'
#' N(infty) = the count of events (sum of indicator delta_i = 1).
#'
#' @param t Numeric vector of observed times.
#' @param event Integer/logical vector (1 = event, 0 = censored).
#' @return Named list with estimate (total events), n, method.
#' @references Kosorok (2008), Ch 8.
#' @examples
#' morie_ksr17_kosorok_counting_process(t = seq(0, 1, length.out = 50), event = rbinom(50, 1, 0.8))
#' @export
morie_ksr17_kosorok_counting_process <- function(t, event) {
  t <- as.numeric(t)
  event <- as.integer(event)
  list(
    estimate = as.integer(sum(event)),
    n        = length(t),
    method   = "Counting process N(infty) = sum 1{T_i finite, delta_i=1}"
  )
}

# CANONICAL TEST
# morie_ksr17_kosorok_counting_process(1:10, c(1,1,0,1,1,0,1,1,1,0))

#' @rdname morie_ksr17_kosorok_counting_process
#' @keywords internal
#' @export
morie_kosorok_counting_process <- morie_ksr17_kosorok_counting_process
