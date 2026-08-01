# SPDX-License-Identifier: AGPL-3.0-or-later
#' Moments of Moran's I under Gaussianity and under randomization
#'
#' Both assumptions give the same mean, \eqn{E[I] = -1/(n-1)} (p. 22), but
#' the variances differ and both are returned. `variance_normal` is
#' \eqn{(n^2 S_1 - n S_2 + 3 S_0^2)/\{S_0^2(n^2-1)\} - E[I]^2}, which does
#' not involve the data at all. `variance_randomization` is Problem 1.8's
#' \eqn{E_r[I^2]} less \eqn{E[I]^2}, which enters the data only through the
#' sample kurtosis \eqn{b}.
#'
#' Two corrections against the placeholder this replaces. It printed the
#' *normality* variance and labelled it randomization, and it was missing the
#' \eqn{-E[I]^2} term. Separately, the \eqn{E_r[I^2]} printed in Problem 1.8
#' is missing a bracket: \eqn{n} multiplies the whole first group,
#' \eqn{n[(n^2-3n+3)S_1 - nS_2 + 3w_{..}^2]}, which is the grouping the
#' book's own Example 1.7 confirms (sd 0.0732 against the literal reading's
#' 0.0740; the Gaussian column reproduces the printed 0.0731 exactly).
#'
#' @param x Attribute values on the lattice, length n.
#' @param w Spatial connectivity weights, n by n, zero diagonal.
#' @return A list with `I`, `expectation`, `variance_normal`,
#'   `variance_randomization`, `sd_normal`, `sd_randomization`, `z_normal`,
#'   `z_randomization`, `kurtosis_b`, `S0`, `S1`, `S2`, `geary_c`,
#'   `geary_expectation`, `n` and `moments` (the same list, for callers that
#'   expect that key).
#' @references Schabenberger Ch 1, Sec 1.3.2, eqs (1.14)-(1.15), pp. 21-23;
#'   Example 1.7 p. 22; Problem 1.8 p. 39, quoting Cliff and Ord (1981),
#'   Spatial Processes: Models and Applications, Pion, Ch. 2.
#' @export
spmenv <- function(x, w) {
  m <- .schab_moran_moments(x, w)
  m$moments <- m[c("I", "expectation", "variance_normal",
                   "variance_randomization")]
  m
}

