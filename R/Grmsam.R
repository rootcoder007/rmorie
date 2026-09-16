# SPDX-License-Identifier: AGPL-3.0-or-later
#' Samejima graded response model
#'
#' The model is stated through the cumulative operating characteristics;
#' the category probability is a difference of two of them, so the
#' thresholds must strictly increase or a probability goes negative.
#' The constraint is checked rather than assumed.
#'
#' Formula: P*_k = 1/(1 + exp(-a(theta - b_k))), P*_0 = 1, P*_{m+1} = 0,
#'   P_k = P*_k - P*_{k+1}.
#'
#' @param y Observed categories, 0-based (0 .. m).
#' @param theta Person abilities, same length as y.
#' @param a Item slope, positive.
#' @param b_k The m strictly increasing thresholds.
#' @return List with \code{estimate}, \code{p_observed},
#'   \code{probs_first}, \code{loglik}, \code{categories}, \code{n},
#'   \code{method}.
#' @references Samejima (1969), Estimation of latent ability using a
#'   response pattern of graded scores, Psychometrika Monograph
#'   Supplement 34(4, Pt. 2). \doi{10.1007/BF03372160}
#' @export
#' @examples
#' Grmsam(y = c(0, 1, 2, 1), theta = c(0, 0.5, -0.5, 1), a = 1.2, b_k = c(-1, 0, 1))
Grmsam <- function(y, theta, a, b_k) {
  ys <- as.integer(.s03vec(y))
  th <- .s03vec(theta)
  b <- .s03vec(b_k)
  if (length(ys) == 0L) stop("graded_response_samejima: y is empty")
  if (length(th) != length(ys)) stop("graded_response_samejima: y and theta have different lengths")
  if (length(b) == 0L) stop("graded_response_samejima: b_k is empty")
  if (length(b) > 1L) for (k in 2:length(b)) if (b[k] <= b[k - 1L])
    stop("graded_response_samejima: thresholds must strictly increase")
  av <- as.numeric(a)
  if (av <= 0) stop("graded_response_samejima: a must be positive")
  m <- length(b)
  probs <- function(t) {
    star <- c(1, vapply(b, function(bb) .s03sigmoid(av * (t - bb)), 0), 0)
    star[seq_len(m + 1L)] - star[seq_len(m + 1L) + 1L]
  }
  pobs <- numeric(length(ys))
  ll <- 0
  for (i in seq_along(ys)) {
    if (ys[i] < 0L || ys[i] > m) stop("graded_response_samejima: response outside the category range")
    p <- probs(th[i])
    pobs[i] <- p[ys[i] + 1L]
    ll <- ll + log(pobs[i])
  }
  .t1_result(estimate = mean(pobs), p_observed = pobs,
             probs_first = probs(th[1]), loglik = ll, categories = m + 1L,
             n = length(ys),
             method = "P_k = P*_k - P*_{k+1} with logistic P*, Samejima (1969)")
}
