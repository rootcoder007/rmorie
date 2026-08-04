# SPDX-License-Identifier: AGPL-3.0-or-later
#' Stick-breaking weights of a Dirichlet process
#'
#' Sethuraman (1994), A constructive definition of Dirichlet priors,
#' Statistica Sinica 4(2), 639-650: V_k ~ Beta(1, alpha) independently and
#' pi_k = V_k prod_(j<k)(1 - V_j), so sum_k pi_k = 1 almost surely.  The
#' 1994 paper is free but was not retrievable here; the construction is
#' quoted in its standard published form and is reproduced in Teh et al.
#' (2006), JASA 101, 1566-1581, eqs. (5)-(6), which WAS fetched.
#'
#' Determinism: a Beta(1, alpha) draw is not taken from a generator -- its
#' quantile is 1 - (1 - u)^(1/alpha) in closed form, evaluated at van der
#' Corput points.  The truncation remainder prod_k (1 - V_k) is returned
#' rather than absorbed silently.
#'
#' @param alpha the DP concentration.
#' @param truncation number of sticks.
#' @param V optional stick fractions supplied by the caller.
#' @param base van der Corput base.
#' @return list: estimate, pi, V, remainder, mass, method.
#' @keywords internal
#' @examples
#' Stickw(1, 5)$pi
#' @export
Stickw <- function(alpha = 1, truncation = 10, V = NULL, base = 2) {
  a <- as.numeric(alpha); K <- as.integer(truncation)
  Vs <- if (!is.null(V)) .s03vec(V) else {
    out <- numeric(K)
    for (i in seq_len(K)) out[i] <- 1 - (1 - .s03vdc(i - 1L, as.integer(base)))^(1 / a)
    out
  }
  pi_ <- numeric(length(Vs)); rest <- 1
  for (i in seq_along(Vs)) { pi_[i] <- Vs[i] * rest; rest <- rest * (1 - Vs[i]) }
  tot <- 0
  for (x in pi_) tot <- tot + x
  list(estimate = if (length(pi_)) pi_[1] else NaN, pi = pi_, V = Vs,
       remainder = rest, mass = tot,
       method = "Sethuraman (1994) stick-breaking, Beta quantiles at low-discrepancy points")
}
