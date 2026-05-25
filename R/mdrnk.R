# SPDX-License-Identifier: AGPL-3.0-or-later

#' Midrank vector with tie summary (Gibbons Ch 5.6.2)
#'
#' Identical to `rank(x, ties.method = "average")` plus a tie-
#' correction term `sum t_j^3 - t_j` over tied groups.
#'
#' @param x Numeric vector.
#' @return Named list: midranks, n, ties, tie_correction.
#' @examples
#' midranks(x = rnorm(50))
#' @export
midranks <- function(x) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 1) {
    return(list(
      midranks = numeric(0), n = 0L, ties = list(),
      tie_correction = 0,
      method = "Midranks"
    ))
  }
  mr <- rank(x, ties.method = "average")
  tab <- table(x)
  ties <- Filter(function(z) z[[2]] > 1, Map(
    function(v, c) list(as.numeric(v), as.integer(c)),
    names(tab), tab
  ))
  tied <- as.numeric(tab[tab > 1])
  tie_correction <- if (length(tied) == 0L) 0 else sum(tied^3 - tied)
  list(
    midranks = mr,
    n = n,
    ties = ties,
    tie_correction = as.numeric(tie_correction),
    method = "Midranks (Gibbons 5.6.2)"
  )
}
