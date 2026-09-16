# SPDX-License-Identifier: AGPL-3.0-or-later

#' Matthews correlation coefficient from confusion-matrix counts
#'
#' \deqn{MCC = \frac{TP \cdot TN - FP \cdot FN}{\sqrt{(TP+FP)(TP+FN)(TN+FP)(TN+FN)}}}
#'
#' The coefficient stays informative under class imbalance, where
#' accuracy does not: it uses all four cells of the confusion matrix
#' rather than only the diagonal. It ranges over \[-1, 1\], with 0 meaning
#' no better than chance and negative values meaning the predictions run
#' opposite to the truth.
#'
#' If any of the four marginal sums is zero the denominator vanishes.
#' The coefficient is then undefined, and the convention followed here
#' is to report 0 and warn, rather than return \code{NaN}.
#'
#' Mirrors \code{morie.fn.mcc} on the Python side. See
#' \code{\link{morie_matthews_corrcoef}} for the label-vector form.
#'
#' @param tp,tn,fp,fn Non-negative integer counts of true positives,
#'   true negatives, false positives and false negatives.
#' @return Named list with \code{value}, \code{statistic} (the same
#'   number), \code{accuracy}, \code{tp}, \code{tn}, \code{fp},
#'   \code{fn}, \code{n}, \code{method}.
#' @references Matthews BW (1975). Comparison of the predicted and
#'   observed secondary structure of T4 phage lysozyme.
#'   \emph{Biochimica et Biophysica Acta}, 405(2), 442-451.
#' @examples
#' morie_mcc_counts(tp = 20, tn = 30, fp = 5, fn = 5)$value
#' @export
morie_mcc_counts <- function(tp, tn, fp, fn) {
  cnt <- c(tp = tp, tn = tn, fp = fp, fn = fn)
  if (any(!is.finite(cnt)) || any(cnt < 0)) {
    stop("Counts must be finite and non-negative.", call. = FALSE)
  }
  # Coerce to double before multiplying. The denominator is a product of
  # four marginals, so with integer counts it overflows R's 32-bit
  # integer at roughly n = 800 and silently becomes NA. Python's
  # arbitrary-precision integers hide this failure mode entirely, which
  # is why it only appears on this side of the mirror.
  tp <- as.numeric(tp)
  tn <- as.numeric(tn)
  fp <- as.numeric(fp)
  fn <- as.numeric(fn)
  num <- tp * tn - fp * fn
  denom <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  if (denom == 0) {
    value <- 0
    warning("A marginal sum is zero, so MCC is undefined; reporting 0. ",
            "Check class imbalance.", call. = FALSE)
  } else {
    value <- num / denom
  }
  n <- tp + tn + fp + fn
  list(
    value = value,
    statistic = value,
    accuracy = if (n > 0) (tp + tn) / n else NA_real_,
    tp = tp, tn = tn, fp = fp, fn = fn, n = n,
    method = "Matthews correlation coefficient"
  )
}

#' Matthews correlation coefficient from label vectors
#'
#' Label-vector form of \code{\link{morie_mcc_counts}}, matching the
#' calling convention of \code{sklearn.metrics.matthews_corrcoef} and of
#' \code{morie.fn.mcc}'s \code{matthews_corrcoef}. Returns a bare
#' numeric so the result compares and orders like a number.
#'
#' Binary labels only. The two levels are taken from the values actually
#' present rather than assumed to be 0/1, so \code{c(-1, 1)} and
#' character labels work; the second level in sorted order is treated as
#' the positive class. Counts are derived and handed to
#' \code{morie_mcc_counts}, so both entry points share one
#' implementation of the formula.
#'
#' @param y_true Vector of observed labels.
#' @param y_pred Vector of predicted labels, same length as
#'   \code{y_true}.
#' @return A single numeric in \[-1, 1\].
#' @references Matthews BW (1975). Comparison of the predicted and
#'   observed secondary structure of T4 phage lysozyme.
#'   \emph{Biochimica et Biophysica Acta}, 405(2), 442-451.
#' @seealso \code{\link{morie_mcc_counts}}
#' @examples
#' morie_matthews_corrcoef(c(0, 0, 1, 1), c(0, 0, 1, 1))
#' morie_matthews_corrcoef(c(0, 0, 1, 1), c(1, 1, 0, 0))
#' @export
morie_matthews_corrcoef <- function(y_true, y_pred) {
  yt <- as.vector(y_true)
  yp <- as.vector(y_pred)
  if (length(yt) != length(yp)) {
    stop("y_true and y_pred must be the same length; got ",
         length(yt), ", ", length(yp), ".", call. = FALSE)
  }
  if (length(yt) == 0L) {
    stop("y_true and y_pred must not be empty.", call. = FALSE)
  }
  levels_present <- sort(unique(c(yt, yp)))
  if (length(levels_present) > 2L) {
    stop("morie_matthews_corrcoef handles binary labels only; got ",
         length(levels_present), " distinct values.", call. = FALSE)
  }

  # With a single level present there is no positive class, so every
  # observation falls in the negative one and the denominator vanishes.
  pos <- if (length(levels_present) == 2L) levels_present[2L] else NULL
  t <- if (is.null(pos)) rep(FALSE, length(yt)) else yt == pos
  p <- if (is.null(pos)) rep(FALSE, length(yp)) else yp == pos

  morie_mcc_counts(
    tp = sum(t & p),
    tn = sum(!t & !p),
    fp = sum(!t & p),
    fn = sum(t & !p)
  )$value
}
