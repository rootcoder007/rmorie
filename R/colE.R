# SPDX-License-Identifier: AGPL-3.0-or-later

#' Cold-start user fallback
#'
#' Formula: fallback to popular / content / metadata
#'
#' A user with fewer than min_ratings observed interactions cannot be
#' served by collaborative filtering at all, because their row of the
#' rating matrix carries no signal.  The three fallbacks are: rank by
#' global popularity, rank by similarity between the item features and
#' whatever the user has rated, and rank by similarity to users with
#' matching metadata.  The cold/warm decision is reported so it can be
#' audited separately from the ranking.
#'
#' @param user Zero-based index of the target user.
#' @param mode One of popular, content, metadata.
#' @param R An n_users x n_items rating matrix; 0 means unobserved.
#' @param item_features An n_items x f matrix for the content fallback.
#' @param user_features An n_users x g matrix for the metadata fallback.
#' @param min_ratings Threshold below which the user counts as cold.
#' @param topn Length of the returned recommendation list.
#' @return List with \code{estimate}, \code{is_cold}, \code{n_rated},
#'   \code{scores}, \code{recommended}, \code{mode}, \code{n_users},
#'   \code{n_items}, \code{method}.
#' @references Schein, Popescul, Ungar & Pennock (2002), Methods and
#'   Metrics for Cold-Start Recommendations, SIGIR 2002:253-260.
#' @export
#' @examples
#' R <- matrix(c(5, 3, 0, 4, 0, 2, 0, 5, 3, 4, 4, 5), 4, 3, byrow = TRUE)
#' ColE(user = 1, mode = "popular", R = R, min_ratings = 1)
ColE <- function(user, mode = "popular", R = NULL, item_features = NULL,
                 user_features = NULL, min_ratings = 3, topn = 3) {
  if (is.null(R)) stop("R is required")
  Rm <- .s03mat(R)
  nu <- nrow(Rm)
  if (nu == 0L) stop("empty input: R has no rows")
  ni <- ncol(Rm)
  u <- as.integer(user) + 1L
  if (u < 1L || u > nu) stop("user index out of range")
  mode <- tolower(as.character(mode))
  if (!(mode %in% c("popular", "content", "metadata")))
    stop("mode must be popular, content or metadata")
  rated <- which(Rm[u, ] != 0)
  is_cold <- as.integer(length(rated) < as.integer(min_ratings))
  if (mode == "popular") {
    scores <- numeric(ni)
    for (j in seq_len(ni)) scores[j] <- sum(Rm[, j] != 0)
  } else if (mode == "content") {
    if (is.null(item_features)) stop("content mode needs item_features")
    F <- .s03mat(item_features)
    if (nrow(F) != ni) stop("item_features must have one row per item")
    f <- ncol(F)
    prof <- numeric(f)
    if (length(rated)) {
      w <- sum(Rm[u, rated])
      for (t in seq_len(f)) {
        s <- 0
        for (j in rated) s <- s + Rm[u, j] * F[j, t]
        prof[t] <- s / w
      }
    } else {
      for (t in seq_len(f)) prof[t] <- sum(F[, t]) / ni
    }
    pn <- sqrt(sum(prof * prof))
    scores <- numeric(ni)
    for (j in seq_len(ni)) {
      fn <- sqrt(sum(F[j, ] * F[j, ]))
      s <- 0
      for (t in seq_len(f)) s <- s + prof[t] * F[j, t]
      scores[j] <- if (pn > 0 && fn > 0) s / (pn * fn) else 0
    }
  } else {
    if (is.null(user_features)) stop("metadata mode needs user_features")
    U <- .s03mat(user_features)
    if (nrow(U) != nu) stop("user_features must have one row per user")
    g <- ncol(U)
    un <- sqrt(sum(U[u, ] * U[u, ]))
    sim <- numeric(nu)
    for (i in seq_len(nu)) {
      vn <- sqrt(sum(U[i, ] * U[i, ]))
      s <- 0
      for (t in seq_len(g)) s <- s + U[u, t] * U[i, t]
      sim[i] <- if (un > 0 && vn > 0) s / (un * vn) else 0
    }
    tot <- 0
    for (i in seq_len(nu)) if (i != u) tot <- tot + sim[i]
    scores <- numeric(ni)
    for (j in seq_len(ni)) {
      s <- 0
      for (i in seq_len(nu)) if (i != u) s <- s + sim[i] * Rm[i, j]
      scores[j] <- s / (if (tot != 0) tot else 1)
    }
  }
  order_ <- order(-scores, seq_len(ni))
  rec <- setdiff(order_, rated)
  rec <- rec[seq_len(min(as.integer(topn), length(rec)))]
  .t1_result(estimate = if (length(rec)) scores[rec[1]] else NaN,
             is_cold = is_cold, n_rated = length(rated), scores = scores,
             recommended = rec - 1L, mode = mode, n_users = nu,
             n_items = ni, method = "cold-start recommendation fallback")
}
