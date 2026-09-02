# SPDX-License-Identifier: AGPL-3.0-or-later
#' Masters' partial credit model
#'
#' The Rasch member of the partial-credit family: the slope is fixed at
#' one, so the raw score is a sufficient statistic for theta, which is
#' what distinguishes it from Muraki's GPCM.  The category
#' probabilities are therefore taken from the GPCM kernel evaluated at
#' a = 1 with the step vector prefixed by a zero, rather than by writing
#' a second softmax.
#'
#' Formula: P(X = k) = exp(sum_{v=1}^{k} (theta - delta_v)) /
#'   sum_{c=0}^{m} exp(sum_{v=1}^{c} (theta - delta_v)), empty sum zero.
#'
#' @param y Observed categories, 0-based (0 .. m).
#' @param theta Person abilities, same length as y.
#' @param delta_j Step difficulties delta_1 .. delta_m.
#' @return List with \code{estimate}, \code{p_observed},
#'   \code{probs_first}, \code{loglik}, \code{categories}, \code{n},
#'   \code{method}.
#' @references Masters (1982), A Rasch model for partial credit
#'   scoring, Psychometrika 47(2):149-174. \doi{10.1007/BF02296272}
#' @export
#' @examples
#' Irtprc(y = 5L, theta = 0.5, delta_j = c(1, 2, 3, 4, 5, 6, 7, 8))
Irtprc <- function(y, theta, delta_j) {
  ys <- as.integer(.s03vec(y)); th <- .s03vec(theta); dl <- .s03vec(delta_j)
  if (length(ys) == 0L) stop("partial_credit: y is empty")
  if (length(th) != length(ys)) stop("partial_credit: y and theta have different lengths")
  if (length(dl) < 1L) stop("partial_credit: delta_j needs at least one step")
  ncat <- length(dl) + 1L
  if (any(ys < 0L | ys >= ncat)) stop("partial_credit: y outside the category range")
  b <- c(0, dl)
  pr <- function(theta) {
    z <- cumsum(theta - b)
    e <- exp(z - max(z))
    e / sum(e)
  }
  pobs <- vapply(seq_along(ys), function(i) pr(th[i])[ys[i] + 1L], 0)
  .t1_result(estimate = mean(pobs), p_observed = pobs,
             probs_first = pr(th[1]), loglik = sum(log(pobs)),
             categories = ncat, n = length(ys),
             method = "P(X=k) = exp(sum_{v<=k}(theta - delta_v)) / normaliser, Masters (1982) eq. (7)")
}
