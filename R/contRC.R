# SPDX-License-Identifier: AGPL-3.0-or-later

#' Content-based recommendation
#'
#' Formula: score = sim(item profile, user profile)
#'
#' The user profile is the rating-weighted mean of the feature vectors
#' of the items they liked, and every item is then scored by cosine
#' similarity to it.  A user who has rated exactly one item therefore
#' has that item's feature vector as their profile, so the top
#' recommendation is its nearest neighbour in feature space -- the
#' degenerate case that pins the weighting.
#'
#' @param item_feat An n_items x f matrix of item feature vectors.
#' @param user_profile Either a length-f profile vector or a
#'   length-n_items rating vector (0 = unrated).
#' @param ratings An explicit rating vector, or NULL.
#' @param topn Length of the returned list.
#' @return List with \code{estimate}, \code{scores}, \code{ranking},
#'   \code{recommended}, \code{profile}, \code{n_items}, \code{f},
#'   \code{method}.
#' @references Pazzani & Billsus (2007), Content-Based Recommendation
#'   Systems, in The Adaptive Web, LNCS 4321:325-341.
#' @export
ContRC <- function(item_feat, user_profile, ratings = NULL, topn = 3) {
  F <- .s03mat(item_feat)
  ni <- nrow(F)
  if (ni == 0L) stop("empty input: item_feat has no rows")
  f <- ncol(F)
  up <- .s03vec(if (is.null(ratings)) user_profile else ratings)
  rated <- integer(0)
  if (!is.null(ratings) || length(up) == ni) {
    if (length(up) != ni)
      stop("the rating vector must have one entry per item")
    rated <- which(up != 0)
    if (!length(rated))
      stop("the user has rated nothing; no profile exists")
    w <- sum(up[rated])
    prof <- numeric(f)
    for (t in seq_len(f)) {
      s <- 0
      for (j in rated) s <- s + up[j] * F[j, t]
      prof[t] <- s / w
    }
  } else {
    if (length(up) != f)
      stop("user_profile must be a length-f profile or a length-n_items rating vector")
    prof <- up
  }
  pn <- sqrt(sum(prof * prof))
  if (pn <= 0) stop("the user profile has zero norm")
  scores <- numeric(ni)
  for (j in seq_len(ni)) {
    fn <- sqrt(sum(F[j, ] * F[j, ]))
    s <- 0
    for (t in seq_len(f)) s <- s + prof[t] * F[j, t]
    scores[j] <- if (fn > 0) s / (pn * fn) else 0
  }
  order_ <- order(-scores, seq_len(ni))
  rec <- setdiff(order_, rated)
  rec <- rec[seq_len(min(as.integer(topn), length(rec)))]
  .t1_result(estimate = if (length(rec)) scores[rec[1]] else NaN,
             scores = scores, ranking = order_ - 1L,
             recommended = rec - 1L, profile = prof, n_items = ni, f = f,
             method = "content-based recommendation")
}
