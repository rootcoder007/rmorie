# SPDX-License-Identifier: AGPL-3.0-or-later
#' Aitchison-distance k-nearest-neighbour classifier for compositions
#'
#' The distance is Aitchison's, taken from the rendered page 10 of
#' Mateu-Figueras, Pawlowsky-Glahn and Egozcue, "The normal distribution in
#' some constrained sample spaces", which prints
#' d_a^2(x, x*) = (1/D) sum_\{i<j\} (ln(x_i/x_j) - ln(x*_i/x*_j))^2 immediately
#' below inner product (10).  The classifier is the rule named in the
#' specification: take the k training points nearest x* in d_a and label x* by
#' the group whose members inside that neighbourhood have the smallest total
#' distance, ghat(x*) = argmin_g sum_\{i in N_k(x*), y_i = g\} d_a(x*, x_i).
#'
#' Groups with no member in the neighbourhood are not candidates, since their
#' empty sum would be zero and would win every time.  Ties go to the smaller
#' label, so the rule is a function rather than a coin toss and both language
#' arms agree.  This is a total-distance vote, not a majority vote: one very
#' close neighbour can outweigh three middling ones.  That is the rule as
#' specified; the majority-vote label is reported alongside as yhat_majority so
#' the disagreement is visible rather than hidden.
#'
#' Because ilr is an isometry, d_a equals the ordinary Euclidean distance
#' between ilr coordinates, which gives an independent route to the same
#' neighbour ordering.  Reference for k-NN on the simplex: Pawlowsky-Glahn,
#' Egozcue and Tolosana-Delgado (2015), Modeling and Analysis of Compositional
#' Data, Wiley.
#'
#' @param X N-by-D matrix of strictly positive training compositions.
#' @param y N group labels, coerced to numeric.
#' @param x_new one composition, or a matrix of them.
#' @param k neighbourhood size, between 1 and the sample size.
#' @return list: yhat, estimate, yhat_majority, dist, k, n, D, method.
#' @keywords internal
#' @examples
#' Aitcap(rbind(c(.6, .2, .2), c(.2, .6, .2)), c(1, 2), c(.5, .3, .2), 1)$yhat
#' @export
Aitcap <- function(X, y, x_new, k) {
  rows <- as.matrix(X)
  storage.mode(rows) <- "double"
  if (nrow(rows) == 0L || length(rows) == 0L) stop("compositional_classifyAP: no training data")
  D <- ncol(rows)
  if (D < 2L) stop("compositional_classifyAP: a composition needs at least 2 parts")
  if (any(!(rows > 0))) stop("compositional_classifyAP: every part must be positive")
  lab <- as.numeric(.s03vec(y))
  if (length(lab) != nrow(rows)) stop("compositional_classifyAP: X and y have different lengths")
  kk <- as.integer(k)
  if (kk < 1L || kk > nrow(rows)) {
    stop("compositional_classifyAP: k must lie between 1 and the sample size")
  }
  many <- is.matrix(x_new) || is.data.frame(x_new)
  news <- if (many) as.matrix(x_new) else matrix(as.numeric(x_new), nrow = 1L)
  storage.mode(news) <- "double"
  if (nrow(news) == 0L) stop("compositional_classifyAP: x_new is empty")
  if (ncol(news) != D) stop("compositional_classifyAP: x_new has the wrong number of parts")
  if (any(!(news > 0))) stop("compositional_classifyAP: every part must be positive")
  yhat <- numeric(nrow(news))
  ymaj <- numeric(nrow(news))
  d0 <- NULL
  for (t in seq_len(nrow(news))) {
    xs <- news[t, ]
    dist <- numeric(nrow(rows))
    for (i in seq_len(nrow(rows))) dist[i] <- .aitcap_dist(xs, rows[i, ])
    ord <- order(dist, seq_along(dist))[seq_len(kk)]
    gs <- sort(unique(lab[ord]))
    tot <- numeric(length(gs))
    cnt <- numeric(length(gs))
    for (i in ord) {
      w <- which(gs == lab[i])
      tot[w] <- tot[w] + dist[i]
      cnt[w] <- cnt[w] + 1
    }
    yhat[t] <- gs[which.min(tot)]
    ymaj[t] <- gs[which.max(cnt)]
    if (t == 1L) d0 <- dist
  }
  list(
    yhat = if (many) yhat else yhat[1], estimate = yhat[1],
    yhat_majority = if (many) ymaj else ymaj[1], dist = d0,
    k = kk, n = nrow(rows), D = D,
    method = "argmin_g sum_{i in N_k, y_i = g} d_a(x*, x_i), Aitchison distance"
  )
}

#' @noRd
.aitcap_dist <- function(a, b) {
  D <- length(a)
  la <- log(a)
  lb <- log(b)
  s <- 0
  for (i in seq_len(D)) {
    for (j in seq_len(D)) {
      if (j > i) {
        d <- (la[i] - la[j]) - (lb[i] - lb[j])
        s <- s + d * d
      }
    }
  }
  sqrt(s / D)
}
