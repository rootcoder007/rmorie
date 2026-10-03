# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P17: incapacitation (research/lean/P17Incapacitation.lean; Avi-Itzhak & Shinnar 1973;
# Blumstein, Cohen & Nagin 1978).
#
#   Research.P17.steady_state_rate         lambda (1/(lambda q)) / (1/(lambda q) + S) = lambda / (1 + lambda q S)
#   Research.P17.cycle_rate                the realised rate on N cycles is lambda fbar / (fbar + S)
#   Research.P17.prevented_share_eq / prevented_share_lt_one   share removed = lambda q S / (1 + lambda q S) < 1
#   Research.P17.rate_antitone_in_S / rate_antitone_in_q       longer or surer sentences lower the rate
#   Research.P17.marginal_prevention_eq / marginal_prevention_pos  one more year prevents lambda^2 q / ((1+lambda q S)(1+lambda q (S+1)))
#   Research.P17.high_rate_more_prevented  a uniform sentence removes a larger share from the higher-rate group

#' Incapacitation arithmetic: crime rate, prevented share and the marginal year
#'
#' An offender commits crimes at rate \code{lambda} while free; each crime
#' leads to a sentence of length \code{S} with probability \code{q}. The
#' long-run crime rate is \eqn{\lambda/(1 + \lambda q S)}
#' (\code{Research.P17.steady_state_rate}); the share of free-state offending
#' removed is \eqn{\lambda q S/(1 + \lambda q S)}, always below one
#' (\code{prevented_share_lt_one}); longer or surer sentences lower the rate
#' (\code{rate_antitone_in_S}, \code{rate_antitone_in_q}); and each extra year
#' prevents \eqn{\lambda^2 q/((1+\lambda q S)(1+\lambda q (S+1)))} crimes per
#' year, less than the year before (\code{marginal_prevention_eq}). With
#' several offender groups the aggregate rate is the share-weighted sum and a
#' uniform sentence removes the larger share from the higher-rate group
#' (\code{high_rate_more_prevented}). The model holds \eqn{\lambda} constant
#' and replaces nobody; those are the assumptions, not the theorems.
#' @param lambda Offending rate(s) while free (crimes per year), positive.
#' @param q Probability per crime of a sentence, in (0, 1].
#' @param S Sentence length(s) in years, non-negative.
#' @param shares Optional group shares (one per \code{lambda}), summing to one.
#' @return A data frame with one row per group: \code{lambda}, \code{q}, \code{S},
#'   \code{rate}, \code{prevented_share}, \code{marginal_prevention}; with
#'   \code{shares}, the attribute \code{"aggregate"} holds the share-weighted
#'   free rate, incapacitated rate and prevented share; attribute \code{"theorems"}.
#' @examples
#' morie_incapacitation(lambda = c(2, 10), q = 0.1, S = 1, shares = c(0.8, 0.2))
#' @export
morie_incapacitation <- function(lambda, q, S, shares = NULL) {
  n <- max(length(lambda), length(q), length(S))
  lambda <- rep_len(lambda, n); q <- rep_len(q, n); S <- rep_len(S, n)
  if (any(lambda <= 0) || any(q <= 0 | q > 1) || any(S < 0)) stop("lambda must be positive, q in (0, 1] and S non-negative", call. = FALSE)
  rate <- lambda / (1 + lambda * q * S)
  prevented <- lambda * q * S / (1 + lambda * q * S)
  marginal <- lambda^2 * q / ((1 + lambda * q * S) * (1 + lambda * q * (S + 1)))
  out <- data.frame(lambda = lambda, q = q, S = S, rate = rate, prevented_share = prevented, marginal_prevention = marginal)
  if (!is.null(shares)) {
    if (length(shares) != n || any(shares < 0) || abs(sum(shares) - 1) > 1e-10) stop("shares must be one non-negative number per group, summing to one", call. = FALSE)
    free <- sum(shares * lambda); inc <- sum(shares * rate)
    attr(out, "aggregate") <- c(free_rate = free, incapacitated_rate = inc, prevented_share = 1 - inc / free)
  }
  attr(out, "theorems") <- c("Research.P17.steady_state_rate", "Research.P17.cycle_rate", "Research.P17.prevented_share_eq",
                             "Research.P17.prevented_share_lt_one", "Research.P17.rate_antitone_in_S", "Research.P17.rate_antitone_in_q",
                             "Research.P17.marginal_prevention_eq", "Research.P17.high_rate_more_prevented")
  out
}
