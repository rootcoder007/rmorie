# Effective reproduction number by Wallinga-Teunis (2004).
# Source: Wallinga & Teunis (2004), Am. J. Epidemiol. 160(6),
# 509-516 (fetched-wave3/wallinga-teunis-2004-rt.pdf):
# p_ij = w(t_i - t_j)/sum_{k!=i} w(t_i - t_k); R_j = sum_i p_ij.
# Mirrors Python morie.fn.rtwall exactly.

#' Wallinga-Teunis case reproduction numbers
#'
#' Likelihood-based pairs method: each case i distributes one unit of
#' infection probability over its possible infectors j in proportion
#' to the generation-interval density at the onset-time difference;
#' R_j is the total probability received by case j.  sum(R_j) equals
#' the number of cases with at least one possible infector, exactly.
#'
#' @param onset_times Integer vector of symptom-onset days.
#' @param gi_pmf Non-negative generation-interval masses w(1), w(2),
#'   ... (w(0) = 0; lags beyond the vector are 0).
#' @return A list with elements \code{r_case}, \code{r_daily} (named
#'   by day), \code{n_cases}, \code{mass_check}, \code{method}.
#' @references Wallinga, J. and Teunis, P. (2004). Different epidemic
#'   curves for severe acute respiratory syndrome reveal similar
#'   impacts of control measures. American Journal of Epidemiology,
#'   160(6), 509-516.
#' @export
morie_rtwall <- function(onset_times, gi_pmf) {
  t <- as.integer(onset_times)
  w <- as.numeric(gi_pmf)
  n <- length(t)
  if (n < 2) stop("need at least two cases")
  if (!length(w) || any(w < 0)) {
    stop("gi_pmf must be non-negative and non-empty")
  }
  wfun <- function(lag) {
    ifelse(lag >= 1 & lag <= length(w), w[pmax(lag, 1)], 0)
  }
  r <- numeric(n)
  n_with_infector <- 0L
  for (i in seq_len(n)) {
    lags <- t[i] - t[-i]
    wk <- ifelse(lags >= 1 & lags <= length(w), w[pmax(pmin(lags, length(w)), 1)], 0)
    denom <- sum(wk)
    if (denom <= 0) next
    n_with_infector <- n_with_infector + 1L
    r[-i] <- r[-i] + wk / denom
  }
  days <- sort(unique(t))
  r_daily <- vapply(days, function(d) mean(r[t == d]), numeric(1))
  names(r_daily) <- days
  list(r_case = r, r_daily = r_daily, n_cases = n,
       mass_check = sum(r) - n_with_infector,
       method = "Wallinga-Teunis (2004) case reproduction numbers")
}
