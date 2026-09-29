#' Aggregate proportional reduction in error (APRE)
#'
#' Poole and Rosenthal's APRE pools classification errors over roll calls,
#' \eqn{\sum_j (m_j - e_j) / \sum_j m_j}, which is the minority-weighted mean
#' of the per-vote PREs. Without minority sizes every roll call is weighted
#' equally. Identical to the Python arm \code{morie.fn.apres.apre_statistic}.
#'
#' @param all_pre Numeric vector of per-roll-call PREs.
#' @param minority Optional positive minority-side sizes, one per roll call.
#' @return A list with \code{value} (APRE), \code{mean_pre},
#'   \code{median_pre}, \code{min_pre}, \code{max_pre}, \code{n_roll_calls}
#'   and \code{weighted}.
#' @references Poole, K. T. and Rosenthal, H. (1997). Congress: A
#'   Political-Economic History of Roll Call Voting. Oxford University Press.
#' @examples
#' apre_statistic(c(0.8, 0.5), minority = c(40, 10))$value
#' @export
apre_statistic <- function(all_pre, minority = NULL) {
  pre <- as.numeric(all_pre)
  k <- length(pre)
  if (k == 0) stop("all_pre is empty")
  w <- if (is.null(minority)) rep(1, k) else as.numeric(minority)
  if (length(w) != k) stop("minority must have one entry per roll call")
  if (any(!(w > 0))) stop("minority sizes must be positive")
  list(value = sum(w * pre) / sum(w), mean_pre = sum(pre) / k,
       median_pre = stats::median(pre), min_pre = min(pre), max_pre = max(pre),
       n_roll_calls = k, weighted = !is.null(minority))
}
