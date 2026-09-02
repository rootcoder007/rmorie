# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bootstrap aggregating (bagging) ensemble prediction
#'
#' Breiman (1996), "Bagging predictors", Machine Learning 24(2), 123-140,
#' doi:10.1007/BF00058655.  Breiman gives two aggregation rules and they are
#' not interchangeable: numerical prediction averages the ensemble,
#' ghat_bag(x) = (1/B) sum_b ghat_b(x), while classification votes,
#' ghat_bag(x) = argmax_k #\{b : ghat_b(x) = k\}.
#'
#' Averaging class labels is the classic bug: it returns 1.5 for a two-one
#' split between classes 1 and 2, a label that does not exist.  This function
#' refuses to guess and takes kind explicitly.  Ties in the vote go to the
#' smaller label, so the rule is a function.
#'
#' models is a B-by-m matrix of predictions, one row per bootstrap replicate
#' and one column per new case: the fitted learners arrive already evaluated,
#' which is what lets the two language arms be compared at all.  X_new is used
#' only to check the column count.
#'
#' @param models B-by-m matrix of per-replicate predictions.
#' @param X_new optional new cases; only the length is used, as a check.
#' @param kind "regression" to average or "classification" to vote.
#' @return list: y_pred, estimate, vote_share, B, m, kind, method.
#' @keywords internal
#' @examples
#' Btbg(rbind(c(1, 2), c(1, 3), c(2, 2)), kind = "regression")$y_pred
#' @export
Btbg <- function(models, X_new = NULL, kind = "regression") {
  rows <- as.matrix(models)
  storage.mode(rows) <- "double"
  B <- nrow(rows)
  if (B == 0L || length(rows) == 0L) stop("boot_bagging_predict: no models")
  m <- ncol(rows)
  if (m == 0L) stop("boot_bagging_predict: no cases to predict")
  if (!is.null(X_new)) {
    nx <- if (is.matrix(X_new) || is.data.frame(X_new)) nrow(X_new) else length(X_new)
    if (nx != m) {
      stop("boot_bagging_predict: X_new and the model matrix disagree on the case count")
    }
  }
  kd <- tolower(as.character(kind))
  if (!(kd %in% c("regression", "classification"))) {
    stop("boot_bagging_predict: kind must be regression or classification")
  }
  yp <- numeric(m)
  share <- numeric(m)
  for (j in seq_len(m)) {
    col <- rows[, j]
    if (kd == "regression") {
      s <- 0
      for (v in col) s <- s + v
      mu <- s / B
      yp[j] <- mu
      ss <- 0
      for (v in col) ss <- ss + (v - mu)^2
      share[j] <- if (B > 1L) sqrt(ss / (B - 1)) else 0
    } else {
      gs <- sort(unique(col))
      cnt <- numeric(length(gs))
      for (v in col) cnt[which(gs == v)] <- cnt[which(gs == v)] + 1
      w <- which.max(cnt)
      yp[j] <- gs[w]
      share[j] <- cnt[w] / B
    }
  }
  list(y_pred = yp, estimate = yp[1], vote_share = share, B = B, m = m, kind = kd,
       method = "Breiman (1996) bagging: average for regression, plurality vote for classification")
}
