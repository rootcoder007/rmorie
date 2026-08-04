# SPDX-License-Identifier: AGPL-3.0-or-later
#' Item-based collaborative filtering
#'
#' Sarwar, Karypis, Konstan and Riedl (2001), Item-based collaborative
#' filtering recommendation algorithms, WWW 10, 285-295.  The weighted-sum
#' prediction of their section 3.2.1 is P(u, i) = sum_j s(i,j) R(u,j) /
#' sum_j |s(i,j)| over the k items most similar to i that u has rated,
#' with s the adjusted cosine of section 3.1.3, which subtracts each
#' USER's mean rather than each item's -- that is what removes the
#' rating-scale differences between users.  The 2001 proceedings were not
#' retrievable here; both expressions are quoted in their standard
#' published form.  Unrated entries must be NA, not zero: zero is a
#' rating.
#'
#' @param R ratings, users in rows, items in columns; NA where unrated.
#' @param u,i the user and item to predict (zero-based).
#' @param k_nn neighbourhood size.
#' @param similarity "adjusted" or "cosine".
#' @return list: estimate, prediction, neighbours, sims, method.
#' @keywords internal
#' @examples
#' R <- matrix(c(5, 3, 4, 4, 1, 5, NA, 2), 2, 4, byrow = TRUE)
#' Itemcf(R, 0, 2, 2)$prediction
#' @export
Itemcf <- function(R, u = 0, i = 0, k_nn = 2, similarity = "adjusted") {
  M <- .s03mat(R); nu <- nrow(M); ni <- ncol(M)
  umean <- numeric(nu)
  for (a in seq_len(nu)) {
    vals <- M[a, !is.na(M[a, ])]
    umean[a] <- if (length(vals)) .s03mean(vals) else 0
  }
  sim <- function(p, q) {
    num <- 0; d1 <- 0; d2 <- 0
    for (a in seq_len(nu)) {
      if (is.na(M[a, p]) || is.na(M[a, q])) next
      cen <- if (identical(similarity, "adjusted")) umean[a] else 0
      xp <- M[a, p] - cen; xq <- M[a, q] - cen
      num <- num + xp * xq; d1 <- d1 + xp * xp; d2 <- d2 + xq * xq
    }
    d <- sqrt(d1 * d2)
    if (d > 0) num / d else 0
  }
  ii <- as.integer(i) + 1L; uu <- as.integer(u) + 1L
  cand <- integer(0)
  for (j in seq_len(ni)) if (j != ii && !is.na(M[uu, j])) cand <- c(cand, j)
  sims <- vapply(cand, function(j) sim(ii, j), 0)
  ord <- order(-abs(sims), cand)
  take <- ord[seq_len(min(as.integer(k_nn), length(ord)))]
  num <- 0; den <- 0
  for (t in take) { num <- num + sims[t] * M[uu, cand[t]]; den <- den + abs(sims[t]) }
  pred <- if (den > 0) num / den else NaN
  list(estimate = pred, prediction = pred, neighbours = cand[take] - 1L,
       sims = sims[take],
       method = "Item-based CF with an adjusted-cosine weighted sum (Sarwar et al. 2001)")
}
