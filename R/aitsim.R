# SPDX-License-Identifier: AGPL-3.0-or-later
#' Gini-Simpson diversity of a composition
#'
#' Simpson, E. H. (1949), Measurement of diversity, Nature 163, 688,
#' defines the concentration lambda = sum p_i^2, the probability that two
#' individuals drawn at random belong to the same class; 1 - lambda is the
#' Gini-Simpson index.  The 1949 note is paywalled; both expressions are
#' quoted in their standard published form.  The input is closed (divided
#' by its total) if it does not already sum to one.
#'
#' @param x proportions or counts by class; negative entries are dropped.
#' @return list: D, estimate, lambda, inv_simpson, p, n, method.
#' @keywords internal
#' @examples
#' Ginisimp(c(5, 3, 2))$D
#' @export
Ginisimp <- function(x) {
  raw <- .s03vec(x)
  raw <- raw[raw >= 0]
  tot <- 0
  for (v in raw) tot <- tot + v
  if (tot <= 0) {
    return(list(
      D = NaN, estimate = NaN, lambda = NaN, inv_simpson = NaN,
      p = numeric(0), n = 0L,
      method = "Gini-Simpson diversity index"
    ))
  }
  p <- raw / tot
  lam <- 0
  for (v in p) lam <- lam + v * v
  d <- 1 - lam
  list(
    D = d, estimate = d, lambda = lam,
    inv_simpson = if (lam > 0) 1 / lam else Inf,
    p = p, n = length(p), method = "Gini-Simpson diversity index"
  )
}
