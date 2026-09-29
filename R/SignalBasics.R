#' Basic biomedical signal measures
#'
#' R arm of \code{morie.fn.dcsub}, \code{prdur} and \code{sactv}. \code{Dcsub}:
#' DC removal (subtract the mean). \code{Prdur}: PR intervals (QRS onset minus
#' P onset) in seconds. \code{Sactv}: Hjorth activity, the population variance
#' of the signal.
#'
#' @param x Signal.
#' @param p_on,qrs_on Sample indices of P-wave and QRS onsets.
#' @param fs Sampling frequency.
#' @return A named list (the Python result's fields).
#' @references Hjorth, B. (1970). EEG analysis based on time domain properties. Electroencephalography and Clinical Neurophysiology 29, 306-310.
#'
#'   Rangayyan, R. M. (2015). Biomedical Signal Analysis, 2nd ed. Wiley-IEEE Press.
#' @examples
#' Dcsub(c(1, 2, 6))$filtered
#' Prdur(c(100, 900), c(260, 1070), fs = 500)$value
#' Sactv(c(1, 2, 6))$value
#' @export
Dcsub <- function(x) {
  dc <- mean(x)
  list(filtered = x - dc, dc_value = dc, n_samples = length(x))
}

#' @rdname Dcsub
#' @export
Prdur <- function(p_on, qrs_on, fs = 1) {
  n <- min(length(p_on), length(qrs_on))
  if (n == 0) return(list(value = 0, pr_intervals = numeric(0)))
  pr <- (qrs_on[seq_len(n)] - p_on[seq_len(n)]) / fs
  list(value = mean(pr), pr_intervals = pr, mean_pr = mean(pr), std_pr = if (n > 1) stats::sd(pr) else 0,
       n_beats = n, fs = fs)
}

#' @rdname Dcsub
#' @export
Sactv <- function(x) {
  act <- mean((x - mean(x))^2)
  list(value = act, activity = act, n = length(x))
}
