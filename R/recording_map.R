# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P9: crime recording as a linear map (research/lean/P9Recording.lean).
#
#   Research.P9.total_invariant_of_colStochastic   column-stochastic M  =>  sum(M c) = sum(c)
#   Research.P9.total_le_of_colSubstochastic       column-substochastic M =>  sum(M c) = sum(c) - dropped mass <= sum(c)
#   Research.P9.reclassification_moves_ratio       share q of j moved into i: total fixed, r_i/r_j = c_i/c_j + q/(1-q) (1 + c_i/c_j)
#   Research.P9.detection_rate_rises               moving share q of a class (rate d) into a disposal detected w.p. 1
#                                                  raises the aggregate detection rate by q n (1-d) / N, N fixed

#' Recorded counts from true counts through a recording matrix
#'
#' A police service turns true offence counts \code{c} (one per category)
#' into recorded counts \code{M \%*\% c}, where \code{M[i, j]} is the share
#' of true category-\code{j} offences recorded as category \code{i}. If
#' every column of \code{M} sums to one (pure re-classification, e.g.
#' robbery recorded as theft) the recorded total equals the true total
#' (\code{Research.P9.total_invariant_of_colStochastic}): downgrading is
#' invisible in totals and visible only in category ratios. If columns sum
#' to less than one (offences "cuffed" out of the notifiable set) the
#' total falls by exactly the dropped mass
#' (\code{Research.P9.total_le_of_colSubstochastic}).
#'
#' @param M Square non-negative matrix with column sums at most one.
#' @param counts True counts per category (length \code{ncol(M)}).
#' @return A list with \code{recorded}, \code{true_total},
#' \code{recorded_total}, \code{dropped} (per category and total),
#' \code{regime} (\code{"reclassification"} when every column sums to one,
#' \code{"cuffing"} otherwise), \code{ratio_true} and \code{ratio_recorded}
#' (each category over the first) and \code{theorems}.
#' @examples
#' M <- rbind(c(0.7, 0), c(0.3, 1))          # 30 percent of robberies recorded as theft
#' rownames(M) <- colnames(M) <- c("robbery", "theft")
#' morie_recording_map(M, c(robbery = 100, theft = 400))
#' @export
morie_recording_map <- function(M, counts) {
  M <- as.matrix(M)
  if (nrow(M) != ncol(M) || length(counts) != ncol(M)) stop("M must be square with one column per category", call. = FALSE)
  if (any(M < 0) || any(colSums(M) > 1 + 1e-12)) stop("M must be non-negative with column sums at most one", call. = FALSE)
  if (any(counts < 0)) stop("counts must be non-negative", call. = FALSE)
  r <- as.numeric(M %*% counts)
  names(r) <- if (!is.null(rownames(M))) rownames(M) else names(counts)
  dropped <- (1 - colSums(M)) * counts
  stochastic <- all(abs(colSums(M) - 1) < 1e-12)
  list(
    recorded = r, true_total = sum(counts), recorded_total = sum(r),
    dropped = list(by_category = dropped, total = sum(dropped)),
    regime = if (stochastic) "reclassification" else "cuffing",
    ratio_true = counts / counts[1], ratio_recorded = r / r[1],
    theorems = c("Research.P9.total_invariant_of_colStochastic", "Research.P9.total_le_of_colSubstochastic")
  )
}

#' Detection-rate arithmetic of downgrade-and-caution
#'
#' Moving a share \code{q} of an offence class of size \code{n} with
#' detection rate \code{d} into a disposal detected with probability one
#' raises the aggregate detection rate by exactly \eqn{q n (1 - d) / N} while
#' the recorded total \eqn{N} is unchanged
#' (\code{Research.P9.detection_rate_rises}). An aggregate clearance rate
#' therefore cannot serve as a performance signal without the split by
#' disposal type.
#'
#' @param detected Aggregate detections before the move.
#' @param total Recorded total \eqn{N}.
#' @param n Size of the class being moved.
#' @param d Detection rate of that class before the move, in [0, 1).
#' @param q Share of the class moved, in (0, 1].
#' @return A list with \code{rate_before}, \code{rate_after}, \code{rise}
#' and \code{theorem}.
#' @examples
#' morie_detection_rate_shift(detected = 2000, total = 10000, n = 1500, d = 0.2, q = 0.5)
#' @export
morie_detection_rate_shift <- function(detected, total, n, d, q) {
  if (total <= 0 || n <= 0 || n > total || detected < 0 || detected > total) stop("counts must satisfy 0 <= detected <= total and 0 < n <= total", call. = FALSE)
  if (d < 0 || d >= 1 || q <= 0 || q > 1) stop("d must lie in [0, 1) and q in (0, 1]", call. = FALSE)
  rise <- q * n * (1 - d) / total
  list(rate_before = detected / total, rate_after = (detected + q * n * (1 - d)) / total, rise = rise,
       theorem = "Research.P9.detection_rate_rises")
}
