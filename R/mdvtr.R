# SPDX-License-Identifier: AGPL-3.0-or-later

#' Median voter theorem (Black 1948)
#'
#' With single-peaked preferences on one dimension the Condorcet winner
#' is the median ideal point.
#'
#' This is the compact front-end. It delegates to
#' [morie_median_voter_ci()] and therefore reports the general
#' density-based standard error \eqn{1/(2 f(m)\sqrt{n})}, NOT the
#' familiar \eqn{1.2533\,s/\sqrt{n}}, which is
#' \eqn{\sqrt{\pi/2}\,\sigma/\sqrt{n}} and holds only under normality.
#' The normal-theory value is still returned as `se_normal` so the two
#' can be compared; for a heavy-tailed electorate it badly overstates
#' the uncertainty, because it reads the tails as spread when the
#' median responds only to the density at the centre. This front-end
#' used to report the normal-theory value as `se` outright, which made
#' its intervals wrong on every non-normal electorate without saying so.
#'
#' @param x Numeric vector of voter ideal points.
#' @param alpha Two-sided level for the intervals.
#' @return Everything [morie_median_voter_ci()] returns: `estimate`,
#'   `se`, `se_normal`, `ci_lower`, `ci_upper`, the distribution-free
#'   `ci_exact_lower` / `ci_exact_upper`, `median_interval`,
#'   `unique_winner`, `n`, `warnings`, `method`.
#' @references Black D (1948) \emph{Journal of Political Economy}
#'   56(1):23-34, \doi{10.1086/256633}.
#' @examples
#' mdvtr(x = rnorm(50))
#' @export
mdvtr <- function(x, alpha = 0.05) {
  morie_median_voter_ci(x, alpha = alpha)
}

#' @keywords internal
#' @rdname mdvtr
#' @export
morie_median_voter <- mdvtr
