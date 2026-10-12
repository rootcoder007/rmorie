# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P9: crime recording as a linear map (research/lean/P9Recording.lean).
#
#   Research.P9.total_invariant_of_colStochastic   column-stochastic M  =>  sum(M c) = sum(c)
#   Research.P9.total_le_of_colSubstochastic       column-substochastic M =>  sum(M c) = sum(c) - dropped mass <= sum(c)
#   Research.P9.reclassification_moves_ratio       share q of j moved into i: total fixed, r_i/r_j = c_i/c_j + q/(1-q) (1 + c_i/c_j)
#   Research.P9.detection_rate_rises               moving share q of a class (rate d) into a disposal detected w.p. 1
#                                                  raises the aggregate detection rate by q n (1-d) / N, N fixed
#   Research.P9.regimes_not_identified          one recorded series, two regimes (identity vs (1-eps) I); second_source_separates: a true total tells them apart

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

#' Two recording regimes from one recorded series
#'
#' A recorded vector alone cannot say whether it came from pure
#' re-classification or from cuffing: the same series is produced by the
#' identity map on true counts equal to the series and by the map
#' \eqn{(1-\epsilon) I} on true counts \eqn{r/(1-\epsilon)}, whose true
#' total is larger (\code{Research.P9.regimes_not_identified}). A second
#' source that fixes the true total separates the two: re-classification
#' reproduces the total exactly, cuffing a positive share records strictly
#' less (\code{Research.P9.second_source_separates}).
#'
#' @param recorded Non-negative recorded counts per category.
#' @param cuffed_share The share \eqn{\epsilon} in (0, 1) used for the cuffing
#'   construction.
#' @param true_total Optional true total from a second source (a
#'   victimisation survey, an audit). When given, the regime the pair of
#'   numbers allows is reported.
#' @return A list with \code{recorded_total}, two constructions
#'   (\code{reclassification} and \code{cuffing}, each with \code{M},
#'   \code{counts} and \code{true_total}), \code{identified_from_series}
#'   (always \code{FALSE}), \code{second_source} (\code{NULL}, or the
#'   regime, the dropped mass and whether the pair is consistent) and
#'   \code{theorems}.
#' @examples
#' r <- c(robbery = 70, theft = 430)
#' two <- morie_recording_regimes(r, cuffed_share = 0.2)
#' two$reclassification$true_total; two$cuffing$true_total
#' morie_recording_regimes(r, true_total = 550)$second_source
#' @export
morie_recording_regimes <- function(recorded, cuffed_share = 0.1, true_total = NULL) {
  r <- as.numeric(recorded)
  if (!length(r) || anyNA(r) || any(r < 0)) stop("recorded must be non-negative counts", call. = FALSE)
  if (!is.numeric(cuffed_share) || length(cuffed_share) != 1L || cuffed_share <= 0 || cuffed_share >= 1) {
    stop("cuffed_share must lie in (0, 1)", call. = FALSE)
  }
  k <- length(r)
  nm <- names(recorded)
  id <- diag(k)
  dimnames(id) <- list(nm, nm)
  cuff <- (1 - cuffed_share) * id
  counts_cuff <- r / (1 - cuffed_share)
  names(counts_cuff) <- nm
  recorded_total <- sum(r)
  second <- NULL
  if (!is.null(true_total)) {
    if (!is.numeric(true_total) || length(true_total) != 1L || is.na(true_total) || true_total < 0) {
      stop("true_total must be a single non-negative number", call. = FALSE)
    }
    tol <- 1e-9 * max(1, true_total)
    second <- list(
      true_total = true_total, dropped_mass = max(0, true_total - recorded_total),
      regime = if (abs(recorded_total - true_total) <= tol) "reclassification" else if (recorded_total < true_total) "cuffing" else "inconsistent",
      consistent = recorded_total <= true_total + tol
    )
  }
  list(
    recorded = stats::setNames(r, nm), recorded_total = recorded_total,
    reclassification = list(M = id, counts = stats::setNames(r, nm), true_total = recorded_total),
    cuffing = list(M = cuff, counts = counts_cuff, true_total = sum(counts_cuff), share = cuffed_share),
    identified_from_series = FALSE, second_source = second,
    theorems = c("Research.P9.regimes_not_identified", "Research.P9.second_source_separates")
  )
}
