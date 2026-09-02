# SPDX-License-Identifier: AGPL-3.0-or-later
#' k-anonymity check on a set of quasi-identifiers
#'
#' A release satisfies k-anonymity when every combination of
#' quasi-identifier values that appears in it appears at least k times.
#' The equivalence classes are the distinct rows of the quasi-identifier
#' block; the smallest is the binding constraint, and the rows in
#' classes below k are exactly the records that would have to be
#' suppressed or generalised.
#'
#' Formula: min over equivalence classes of |class| >= k.
#'
#' @param y Records; only the length is used, to check it against the
#'   quasi-identifier block.
#' @param quasi_ids Quasi-identifier block, one row per record.
#' @param k Required anonymity level.
#' @return List with \code{estimate} (smallest class size),
#'   \code{min_class_size}, \code{max_class_size},
#'   \code{mean_class_size}, \code{k}, \code{satisfies},
#'   \code{n_classes}, \code{n_violating}, \code{n_unique}, \code{n},
#'   \code{method}.
#' @references Sweeney (2002), k-anonymity: a model for protecting
#'   privacy, International Journal of Uncertainty, Fuzziness and
#'   Knowledge-Based Systems 10(5):557-570.
#'   \doi{10.1142/S0218488502001648}
#' @export
#' @examples
#' Kanon(y = c(1, 2, 3, 4, 5, 6, 7, 8), quasi_ids = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L)
Kanon <- function(y, quasi_ids, k) {
  yv <- .s03vec(y); n <- length(yv)
  if (n == 0L) stop("k_anonymity_check: y is empty")
  rows <- .s03mat(quasi_ids)
  if (nrow(rows) != n) stop("k_anonymity_check: quasi_ids and y have different lengths")
  kk <- as.integer(k)
  if (kk < 1L) stop("k_anonymity_check: k must be at least 1")
  keys <- apply(rows, 1L, function(z) paste(sprintf("%.12g", z), collapse = "|"))
  ord <- unique(keys)
  sizes <- vapply(ord, function(z) sum(keys == z), 0)
  mn <- min(sizes)
  viol <- sum(sizes[sizes < kk])
  .t1_result(estimate = mn, min_class_size = mn, max_class_size = max(sizes),
             mean_class_size = mean(sizes), k = kk,
             satisfies = if (mn >= kk) 1 else 0, n_classes = length(sizes),
             n_violating = viol, n_unique = sum(sizes == 1), n = n,
             method = "min over equivalence classes of |class| >= k, Sweeney (2002)")
}
