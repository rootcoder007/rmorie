# SPDX-License-Identifier: AGPL-3.0-or-later
#' Natural indirect effect (Pearl 2001)
#'
#' DUPLICATE: the contrast is already implemented as \code{Nieff} in
#' \code{unclr.R}; per ledger/wave2/DUPMAP.tsv this is an alias, not a
#' second copy.
#'
#' Formula: \code{NIE = E[Y(1, M(1))] - E[Y(1, M(0))]}, taken within
#' unit so the standard error is the paired one.
#'
#' @param y11 Unit-level \code{Y(1, M(1))}.
#' @param y10 Unit-level \code{Y(1, M(0))}, the cross-world outcome.
#' @return List with \code{estimate}, \code{se}, \code{mean_y11},
#'   \code{mean_y10}, \code{n}.
#' @references Pearl, J. (2001). Direct and indirect effects.
#'   Proceedings of the Seventeenth Conference on Uncertainty in
#'   Artificial Intelligence, 411-420. Morgan Kaufmann.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Nie(V, V)
Nie <- function(y11, y10) {
  r <- Nieff(y11, y10)
  .t1_result(estimate = r$estimate, se = r$se, mean_y11 = r$mean_y11,
             mean_y10 = r$mean_y10, n = r$n)
}
