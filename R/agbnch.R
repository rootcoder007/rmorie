# SPDX-License-Identifier: AGPL-3.0-or-later
#' Elo rating from a match pool
#'
#' Silver et al. (2018), arXiv:1712.01815 (FETCHED), states its own
#' convention verbatim: "We estimate the probability that player a will
#' defeat player b by a logistic function p(a defeats b) = 1 / (1 +
#' exp(c_elo (e(b) - e(a)))) ... using the standard constant c_elo =
#' 1/400."  That is the natural exponential logistic, NOT the classical
#' curve of Elo (1978), The Rating of Chessplayers, Past and Present,
#' which is 1 / (1 + 10^((R_b - R_a)/400)).  The two disagree: a
#' 200-point gap gives 0.622 under the paper's constant and 0.760 under
#' Elo's.  base = "e" is AlphaZero's convention and the default; base = 10
#' is Elo's.  Ratings are inverted in closed form from a score against an
#' anchor, not fitted by Bayesian logistic regression, and `method` says
#' so.
#'
#' @param games a score in \[0, 1\], or a triple (wins, draws, losses).
#' @param ladder optional ratings of other players.
#' @param anchor rating of the opponent the score was achieved against.
#' @param base "e" or 10.
#' @param c_elo the constant, 1/400.
#' @return list: estimate, rating, score, expected, base, method.
#' @keywords internal
#' @examples
#' Elorating(c(60, 10, 30), anchor = 3000)$rating
#' @export
Elorating <- function(games, ladder = NULL, anchor = 0, base = "e",
                      c_elo = 1 / 400) {
  g <- .s03vec(games)
  if (length(g) >= 3L) {
    w <- g[1]
    d <- g[2]
    l <- g[3]
    tot <- w + d + l
    score <- if (tot > 0) (w + 0.5 * d) / tot else NaN
  } else {
    score <- if (length(g)) g[1] else NaN
  }
  if (score <= 0 || score >= 1) {
    rating <- if (score <= 0) -Inf else Inf
  } else {
    odds <- log(score / (1 - score))
    rating <- as.numeric(anchor) +
      (if (identical(base, "e")) odds / c_elo else odds / (log(10) * c_elo))
  }
  exp_ <- numeric(0)
  lad <- if (!is.null(ladder)) .s03vec(ladder) else numeric(0)
  for (r in lad) {
    dd <- c_elo * (r - rating)
    exp_ <- c(exp_, 1 / (1 + (if (identical(base, "e")) exp(dd) else 10^dd)))
  }
  list(
    estimate = rating, rating = rating, score = score, expected = exp_,
    base = base,
    method = paste0(
      "Elo rating inverted in closed form from a score against an ",
      "anchor; AlphaZero's exp convention by default, Elo's ",
      "base-10 curve with base=10"
    )
  )
}
