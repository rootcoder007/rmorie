# SPDX-License-Identifier: AGPL-3.0-or-later
#' Generalized partial credit model
#'
#' Muraki's model exponentiates the running sum of the step logits.  The
#' v = 0 term is common to every numerator and cancels, which is why the
#' first step parameter is arbitrary; it is kept so the printed formula
#' is followed literally.
#'
#' Formula: P_k = exp(sum_{v=0}^{k} a (theta - b_v)) /
#'   sum_c exp(sum_{v=0}^{c} a (theta - b_v)).
#'
#' @param y Observed categories, 0-based, one per person.
#' @param theta Person abilities, same length as y.
#' @param a Item slope, positive.
#' @param b_j Step parameters; their count sets the category count.
#' @return List with \code{estimate} (mean probability of the observed
#'   response), \code{p_observed}, \code{probs_first}, \code{loglik},
#'   \code{categories}, \code{n}, \code{method}.
#' @references Muraki (1992), A generalized partial credit model,
#'   Applied Psychological Measurement 16(2):159-176.
#'   \doi{10.1177/014662169201600206}
#' @export
Gpcm <- function(y, theta, a, b_j) {
  ys <- as.integer(.s03vec(y))
  th <- .s03vec(theta)
  b <- .s03vec(b_j)
  if (length(ys) == 0L) stop("generalized_partial_credit: y is empty")
  if (length(th) != length(ys)) stop("generalized_partial_credit: y and theta have different lengths")
  if (length(b) < 2L) stop("generalized_partial_credit: b_j needs at least two categories")
  av <- as.numeric(a)
  if (av <= 0) stop("generalized_partial_credit: a must be positive")
  m <- length(b)
  probs <- function(t) {
    z <- cumsum(av * (t - b))
    e <- exp(z - max(z))
    e / sum(e)
  }
  pobs <- numeric(length(ys))
  ll <- 0
  for (i in seq_along(ys)) {
    if (ys[i] < 0L || ys[i] >= m) stop("generalized_partial_credit: response outside the category range")
    p <- probs(th[i])
    pobs[i] <- p[ys[i] + 1L]
    ll <- ll + log(pobs[i])
  }
  .t1_result(estimate = mean(pobs), p_observed = pobs,
             probs_first = probs(th[1]), loglik = ll, categories = m,
             n = length(ys), method = "GPCM eq. (1) of Muraki (1992)")
}
