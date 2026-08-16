# SPDX-License-Identifier: AGPL-3.0-or-later
#' Weighted frequency distribution
#'
#' Each sampled unit contributes its design weight to the cell it falls
#' in, so a cell total estimates the population count.  With unit weights
#' the totals are exactly the ordinary contingency counts.
#'
#' Formula: f_k = sum over i in cell k of w_i; p_k = f_k / sum(f).
#'
#' @param y Category labels, compared as character.
#' @param weights Optional design weights; equal weights if NULL.
#' @param cells Optional vector of levels in reporting order; defaults to
#'   the sorted distinct labels of \code{y}.
#' @return List with \code{estimate} (largest cell total), \code{levels},
#'   \code{freq}, \code{prop}, \code{sumw}, \code{n}, \code{k},
#'   \code{method}.
#' @references Lohr, S. L. (2010). Sampling: Design and Analysis, 2nd ed.
#'   Brooks/Cole, section 7.2.
#' @examples
#' Wfrep(c(1, 1, 2, 3, 3, 3))
#' @export
Wfrep <- function(y, weights = NULL, cells = NULL) {
  lab <- .wfrep_lab(y); n <- length(lab)
  if (n == 0L) stop("weighted_frequency: y is empty")
  w <- if (is.null(weights)) rep(1, n) else .s03vec(weights)
  if (length(w) != n) stop("weighted_frequency: y and weights differ in length")
  lv <- if (is.null(cells)) sort(unique(lab)) else .wfrep_lab(cells)
  freq <- numeric(length(lv))
  for (j in seq_along(lv)) freq[j] <- sum(w[lab == lv[j]])
  tot <- sum(freq)
  prop <- if (tot > 0) freq / tot else rep(NaN, length(lv))
  list(estimate = if (length(freq)) as.numeric(max(freq)) else NaN,
       levels = lv, freq = freq, prop = prop,
       sumw = as.numeric(sum(w)), n = as.integer(n),
       k = as.integer(length(lv)),
       method = "weighted cell frequencies, f_k = sum_{i in k} w_i [Lohr 2010]")
}

#' .wfrep_lab
#'
#' Part of the Wfrep implementation; see the file header for the source
#' it follows.
#'
#' @param v See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.wfrep_lab <- function(v) {
  if (is.numeric(v)) {
    ifelse(v == trunc(v), format(trunc(v), scientific = FALSE, trim = TRUE),
           as.character(v))
  } else as.character(v)
}

# CANONICAL TEST
# r <- Wfrep(c(1, 1, 2, 3, 3, 3))
# stopifnot(all(r\$freq == as.vector(table(c(1, 1, 2, 3, 3, 3)))))
