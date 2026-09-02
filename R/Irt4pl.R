# SPDX-License-Identifier: AGPL-3.0-or-later
#' Four-parameter logistic item response model
#'
#' The lower asymptote absorbs guessing and the upper asymptote
#' slipping; c = 0 and d = 1 recovers the two-parameter logistic exactly.
#'
#' Formula: P = c + (d - c) / (1 + exp(-a(theta - b))).
#'
#' @param y Observed 0/1 responses.
#' @param theta Person abilities, same length as y.
#' @param a Slope, positive.
#' @param b Difficulty.
#' @param c Lower asymptote.
#' @param d Upper asymptote, greater than c.
#' @return List with \code{estimate}, \code{p}, \code{loglik}, \code{a},
#'   \code{b}, \code{c}, \code{d}, \code{n}, \code{method}.
#' @references Lord (1980), Applications of Item Response Theory to
#'   Practical Testing Problems, Lawrence Erlbaum, ch. 2; Barton and
#'   Lord (1981), ETS Research Report RR-81-20.
#'   \doi{10.1002/j.2333-8504.1981.tb01255.x}
#' @export
#' @examples
#' Irt4pl(y = c(1, 0, 1), theta = c(0.5, -0.5, 1), a = 1, b = 0, c = 0.1, d = 0.95)
Irt4pl <- function(y, theta, a, b, c = 0, d = 1) {
  ys <- as.integer(.s03vec(y))
  th <- .s03vec(theta)
  if (length(ys) == 0L) stop("four_parameter_logistic: y is empty")
  if (length(th) != length(ys)) stop("four_parameter_logistic: y and theta have different lengths")
  if (any(!(ys %in% c(0L, 1L)))) stop("four_parameter_logistic: responses must be 0 or 1")
  av <- as.numeric(a); bv <- as.numeric(b); cv <- as.numeric(c); dv <- as.numeric(d)
  if (av <= 0) stop("four_parameter_logistic: a must be positive")
  if (cv < 0 || dv > 1 || cv >= dv) stop("four_parameter_logistic: need 0 <= c < d <= 1")
  p <- cv + (dv - cv) * vapply(av * (th - bv), .s03sigmoid, 0)
  ll <- sum(ifelse(ys == 1L, log(p), log(1 - p)))
  .t1_result(estimate = mean(p), p = p, loglik = ll, a = av, b = bv,
             c = cv, d = dv, n = length(ys),
             method = "P = c + (d - c)/(1 + exp(-a(theta - b)))")
}
