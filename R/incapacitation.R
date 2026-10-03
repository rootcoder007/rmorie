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
  lambda <- rep_len(lambda, n)
  q <- rep_len(q, n)
  S <- rep_len(S, n)
  if (any(lambda <= 0) || any(q <= 0 | q > 1) || any(S < 0)) stop("lambda must be positive, q in (0, 1] and S non-negative", call. = FALSE)
  rate <- lambda / (1 + lambda * q * S)
  prevented <- lambda * q * S / (1 + lambda * q * S)
  marginal <- lambda^2 * q / ((1 + lambda * q * S) * (1 + lambda * q * (S + 1)))
  out <- data.frame(lambda = lambda, q = q, S = S, rate = rate, prevented_share = prevented, marginal_prevention = marginal)
  if (!is.null(shares)) {
    if (length(shares) != n || any(shares < 0) || abs(sum(shares) - 1) > 1e-10) stop("shares must be one non-negative number per group, summing to one", call. = FALSE)
    free <- sum(shares * lambda)
    inc <- sum(shares * rate)
    attr(out, "aggregate") <- c(free_rate = free, incapacitated_rate = inc, prevented_share = 1 - inc / free)
  }
  attr(out, "theorems") <- c("Research.P17.steady_state_rate", "Research.P17.cycle_rate", "Research.P17.prevented_share_eq",
                             "Research.P17.prevented_share_lt_one", "Research.P17.rate_antitone_in_S", "Research.P17.rate_antitone_in_q",
                             "Research.P17.marginal_prevention_eq", "Research.P17.high_rate_more_prevented")
  out
}

#   Research.P17Replacement.replaced_le / replaced_antitone / replaced_full   (1 - r) x prevented share: lower, monotone, zero at r = 1
#   Research.P17Replacement.prevented_le_const      sum_{s<S} lam(t0+s) <= S lam(t0)
#   Research.P17Replacement.prevented_ge_const      sum_{s<S} lam(t0+s) >= S lam(t0+S)
#   Research.P17Replacement.later_sentence_prevents_less   the same sentence later in a career prevents fewer crimes
#   Research.P17Replacement.prevented_net_le        (1 - r) sum <= S lam(t0) for r in [0, 1]

#' Incapacitation under desistance and replacement
#'
#' Crimes prevented by a sentence of \code{S} periods served from career age
#' \code{t0} when the offending rate is a non-increasing path
#' \eqn{\lambda(t)} (desistance) and a share \code{replacement} of the
#' prevented crimes is committed by others. The sum
#' \eqn{\sum_{s<S}\lambda(t_0+s)} is at most \eqn{S\lambda(t_0)}
#' (\code{Research.P17Replacement.prevented_le_const}) and at least
#' \eqn{S\lambda(t_0+S)} (\code{prevented_ge_const}); the same sentence
#' served later prevents fewer crimes (\code{later_sentence_prevents_less});
#' replacement scales the result by \eqn{1 - r} (\code{replaced_le},
#' \code{replaced_antitone}, \code{replaced_full}), so the constant-rate
#' estimate from the entry-age rate is an upper bound under both
#' (\code{prevented_net_le}).
#' @param lambda_path Non-increasing offending rates by career age, the first
#'   element being age 0; length at least \code{t0 + S + 1}.
#' @param t0 Career age (0-based) at which the sentence starts.
#' @param S Sentence length in periods, a positive integer.
#' @param replacement Replacement share \eqn{r \in [0, 1]}.
#' @return A list with \code{prevented}, \code{prevented_net}
#'   (\eqn{(1-r)} times it), \code{upper} (\eqn{S\lambda(t_0)}),
#'   \code{lower} (\eqn{S\lambda(t_0+S)}), \code{later} (the sum for a start
#'   one period later, \code{NA} when the path is too short),
#'   \code{replacement}, \code{factor} (\eqn{1-r}) and \code{theorems}.
#' @examples
#' lam <- c(12, 10, 8, 6, 5, 4, 3, 2, 2, 1)
#' r <- morie_incapacitation_career(lam, t0 = 2, S = 3, replacement = 0.25)
#' c(prevented = r$prevented, net = r$prevented_net, upper = r$upper, lower = r$lower, later = r$later)
#' @export
morie_incapacitation_career <- function(lambda_path, t0, S, replacement = 0) {
  if (!is.numeric(lambda_path) || anyNA(lambda_path) || any(lambda_path < 0)) stop("lambda_path must be non-negative numbers", call. = FALSE)
  if (any(diff(lambda_path) > 0)) stop("lambda_path must be non-increasing (desistance)", call. = FALSE)
  if (length(t0) != 1L || t0 < 0 || t0 != floor(t0)) stop("t0 must be a non-negative integer", call. = FALSE)
  if (length(S) != 1L || S < 1 || S != floor(S)) stop("S must be a positive integer", call. = FALSE)
  if (length(replacement) != 1L || replacement < 0 || replacement > 1) stop("replacement must lie in [0, 1]", call. = FALSE)
  if (length(lambda_path) < t0 + S + 1) stop("lambda_path must have length at least t0 + S + 1", call. = FALSE)
  prevented <- sum(lambda_path[t0 + seq_len(S)])
  later <- if (length(lambda_path) >= t0 + S + 2) sum(lambda_path[t0 + 1 + seq_len(S)]) else NA_real_
  list(
    prevented = prevented,
    prevented_net = (1 - replacement) * prevented,
    upper = S * lambda_path[t0 + 1],
    lower = S * lambda_path[t0 + S + 1],
    later = later,
    replacement = replacement,
    factor = 1 - replacement,
    theorems = c("Research.P17Replacement.prevented_le_const", "Research.P17Replacement.prevented_ge_const",
                 "Research.P17Replacement.later_sentence_prevents_less", "Research.P17Replacement.replaced_antitone",
                 "Research.P17Replacement.prevented_net_le")
  )
}
