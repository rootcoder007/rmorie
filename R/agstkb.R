# SPDX-License-Identifier: AGPL-3.0-or-later
#' Head-to-head tally against a rating ladder
#'
#' Silver et al. (2018), arXiv:1712.01815 (FETCHED): "Elo ratings were
#' computed from the results of a 1 second per move tournament between
#' iterations of AlphaZero during training, and also a baseline player:
#' either Stockfish, Elmo or AlphaGo Lee respectively.  The Elo rating of
#' the baseline players was anchored to publicly available values."  Each
#' baseline's rating is therefore fixed and known, and the candidate's
#' follows from its score against each; the rungs are combined weighted by
#' games played.  The logistic convention is the paper's own; see
#' Elorating for why that is not the classical base-10 Elo curve.
#'
#' @param games matrix, one row per rung: (wins, draws, losses).
#' @param ladder anchored rating of each rung.
#' @param base "e" or 10.
#' @param c_elo the constant, 1/400.
#' @return list: estimate, rating, per_rung, scores, n_games, wins, draws,
#'   losses, method.
#' @keywords internal
#' @examples
#' Elomatch(matrix(c(30, 60, 10), 1, 3), 3300)$rating
#' @export
Elomatch <- function(games, ladder, base = "e", c_elo = 1 / 400) {
  rows <- .s03mat(games)
  anchors <- .s03vec(ladder)
  nr <- nrow(rows)
  per <- numeric(nr)
  scores <- numeric(nr)
  ns <- numeric(nr)
  num <- 0
  den <- 0
  for (i in seq_len(nr)) {
    w <- rows[i, 1]
    d <- rows[i, 2]
    l <- rows[i, 3]
    n <- w + d + l
    ns[i] <- n
    s <- if (n > 0) (w + 0.5 * d) / n else NaN
    scores[i] <- s
    if (!is.na(s) && s > 0 && s < 1) {
      odds <- log(s / (1 - s))
      r <- anchors[i] +
        (if (identical(base, "e")) odds / c_elo else odds / (log(10) * c_elo))
    } else {
      r <- if (!is.na(s) && s <= 0) -Inf else Inf
    }
    per[i] <- r
    if (!is.na(r) && is.finite(r)) {
      num <- num + n * r
      den <- den + n
    }
  }
  est <- if (den > 0) num / den else NaN
  wins <- 0
  draws <- 0
  losses <- 0
  for (i in seq_len(nr)) {
    wins <- wins + rows[i, 1]
    draws <- draws + rows[i, 2]
    losses <- losses + rows[i, 3]
  }
  list(
    estimate = est, rating = est, per_rung = per, scores = scores,
    n_games = ns, wins = wins, draws = draws, losses = losses,
    method = "Elo anchored to a ladder of baselines, games-weighted"
  )
}
