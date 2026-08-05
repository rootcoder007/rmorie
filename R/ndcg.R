# SPDX-License-Identifier: AGPL-3.0-or-later
#' Normalised discounted cumulative gain at k
#'
#' \code{DCG@k = sum_{i=1..k} (2^rel_i - 1) / log2(i + 1)} and
#' \code{NDCG@k = DCG@k / IDCG@k}, where \code{IDCG@k} re-sorts the same
#' relevances descending. An all-zero relevance list has \code{IDCG = 0};
#' NDCG is then undefined and is refused rather than reported as a
#' perfect 1.
#'
#' @param pred_rank Items in predicted order, best first.
#' @param relevant Either a named numeric vector of graded relevances
#'   (absent items score 0) or a plain vector of relevant items, in which
#'   case membership is used as binary relevance.
#' @param k Cut-off.
#' @return List with estimate (NDCG@k), dcg, idcg, k, n.
#' @references Jarvelin and Kekalainen (2002), ACM TOIS 20(4), 422-446,
#'   \doi{10.1145/582415.582418}.
#' @export
Ndcg <- function(pred_rank, relevant, k) {
  items <- as.character(unlist(pred_rank))
  if (length(items) == 0L) stop("pred_rank is empty")
  if (!is.null(names(relevant))) {
    g <- as.numeric(relevant)[match(items, names(relevant))]
    g[is.na(g)] <- 0
  } else {
    g <- as.numeric(items %in% as.character(unlist(relevant)))
  }
  k <- as.integer(k)
  if (k < 1L) stop("k must be positive.")
  if (any(g < 0)) stop("graded relevances must be non-negative.")
  dcg <- function(v) {
    v <- v[seq_len(min(k, length(v)))]
    sum((2^v - 1) / log2(seq_along(v) + 1))
  }
  got <- dcg(g)
  ideal <- dcg(sort(g, decreasing = TRUE))
  if (ideal == 0) {
    stop(paste("every relevance is 0, so IDCG is 0 and NDCG is undefined;",
               "declaring the ranking perfect there would be a lie."))
  }
  .t1_result(estimate = got / ideal, dcg = got, idcg = ideal, k = k,
             n = length(g),
             method = "NDCG@k (Jarvelin and Kekalainen 2002)")
}
