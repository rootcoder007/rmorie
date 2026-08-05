# SPDX-License-Identifier: AGPL-3.0-or-later
#' (IPCW) Brier score for survival models
#'
#' The Brier score is the mean squared error of a survival probability
#' prediction at a fixed horizon.  Censoring makes the naive average
#' biased, because subjects censored before the horizon have unknown
#' status; Graf et al. reweight the observed subjects by the inverse of
#' the censoring survival at their own event time, which restores
#' unbiasedness.  The scaled score, 1 - BS(model)/BS(null) with the
#' Kaplan-Meier marginal as the null, is the index of prediction accuracy.
#'
#' R arm of the existing Python \code{brier} module.
#'
#' Formula: BS(t) = (1/n) \[ sum_i w_i I(T_i <= t, d_i = 1) S(t|x_i)^2
#'                        + sum_i I(T_i > t) (1 - S(t|x_i))^2 ]
#' with w_i = 1 / G(T_i), G the Kaplan-Meier estimate of censoring.
#'
#' @param time Observed event or censoring times.
#' @param event Event indicator, 1 = event, 0 = censored.
#' @param predicted_survival Predicted survival at \code{eval_time}: a
#'   vector of length n, or an n by k matrix for k evaluation times.
#' @param eval_time Scalar or vector of evaluation times.
#' @param method "ipcw" (default) or "naive".
#' @return List with \code{brier_score}, \code{scaled_brier},
#'   \code{integrated_brier}, \code{eval_time}.
#' @references Graf, E., Schmoor, C., Sauerbrei, W. and Schumacher, M.
#'   (1999). Assessment and comparison of prognostic classification
#'   schemes for survival data. Statistics in Medicine 18(17-18):
#'   2529-2545.
#'   Gerds, T. A. and Schumacher, M. (2006). Biometrical Journal
#'   48(6):1029-1040. \doi{10.1002/bimj.200610301}
#' @examples
#' Brier(c(1, 2, 3, 4), c(1, 1, 0, 1), c(0.9, 0.7, 0.5, 0.3), 2.5)
#' @export
Brier <- function(time, event, predicted_survival, eval_time, method = "ipcw") {
  time <- .s03vec(time); event <- .s03vec(event)
  n <- length(time)
  if (length(event) != n) stop("time and event must have the same length.")
  if (!all(event %in% c(0, 1))) stop("event must be binary (0/1).")
  if (!(method %in% c("ipcw", "naive"))) stop("method must be 'ipcw' or 'naive'.")

  scalar_eval <- (length(eval_time) == 1L)
  eval_times <- as.numeric(eval_time)
  pred_2d <- if (is.matrix(predicted_survival)) predicted_survival else
    matrix(as.numeric(predicted_survival), ncol = 1L)
  if (!is.matrix(predicted_survival) && length(eval_times) > 1L)
    stop("predicted_survival must be 2D for multiple eval_times.")
  if (nrow(pred_2d) != n) stop("predicted_survival must have n rows.")
  if (ncol(pred_2d) != length(eval_times))
    stop("predicted_survival columns must match number of eval_times.")

  cen <- as.numeric(event == 0)
  ord <- order(time)
  t_s <- time[ord]; c_s <- cen[ord]; e_o <- event[ord]

  S_c <- 1; km_c_times <- numeric(0); km_c_vals <- numeric(0)
  for (t_j in sort(unique(t_s[c_s == 1]))) {
    n_r <- sum(t_s >= t_j); n_c <- sum(t_s == t_j & c_s == 1)
    km_c_times <- c(km_c_times, t_j); km_c_vals <- c(km_c_vals, S_c)
    S_c <- S_c * (if (n_r > 0) 1 - n_c / n_r else 1)
  }
  if (!length(km_c_times)) { km_c_times <- 0; km_c_vals <- 1 }
  get_km_c <- function(t) {
    idx <- sum(km_c_times < t) - 1L
    if (idx < 0L) return(1)
    km_c_vals[min(idx, length(km_c_vals) - 1L) + 1L]
  }

  S_km <- 1; ko_times <- numeric(0); ko_vals <- numeric(0)
  for (t_j in sort(unique(t_s[c_s == 0]))) {
    n_r <- sum(t_s >= t_j)
    n_e <- sum(t_s == t_j & c_s == 0 & e_o == 1)
    if (n_e == 0) next
    # The null reference is S(t), the survival AFTER the drop at t_j.
    # Appending before the update stored S(t_j-) and made the marginal KM
    # one event-step stale, which biased scaled_brier / IPA.
    S_km <- S_km * (if (n_r > 0) 1 - n_e / n_r else 1)
    ko_times <- c(ko_times, t_j); ko_vals <- c(ko_vals, S_km)
  }
  get_km_surv <- function(t) {
    if (!length(ko_times)) return(1)
    idx <- sum(ko_times <= t) - 1L
    if (idx < 0L) return(1)
    ko_vals[min(idx, length(ko_vals) - 1L) + 1L]
  }

  k_n <- length(eval_times)
  bs_vals <- numeric(k_n); bs_null_vals <- numeric(k_n)
  for (k in seq_len(k_n)) {
    t_eval <- eval_times[k]; s_hat <- pred_2d[, k]; s_null <- get_km_surv(t_eval)
    bs <- 0; bs_null <- 0
    for (i in seq_len(n)) {
      if (time[i] <= t_eval && event[i] == 1) {
        w <- if (method == "ipcw") 1 / max(get_km_c(time[i]), 1e-10) else 1
        bs <- bs + w * s_hat[i]^2
        bs_null <- bs_null + w * s_null^2
      } else if (time[i] > t_eval) {
        bs <- bs + (1 - s_hat[i])^2
        bs_null <- bs_null + (1 - s_null)^2
      }
    }
    bs_vals[k] <- bs / n; bs_null_vals[k] <- bs_null / n
  }
  scaled_bs <- ifelse(bs_null_vals > 0, 1 - bs_vals / bs_null_vals, 0)
  ibs <- if (k_n > 1L) {
    tr <- sum(diff(eval_times) * (utils::head(bs_vals, -1) + utils::tail(bs_vals, -1)) / 2)
    tr / (eval_times[k_n] - eval_times[1L])
  } else bs_vals[1L]

  if (scalar_eval)
    list(brier_score = as.numeric(bs_vals[1L]),
         scaled_brier = as.numeric(scaled_bs[1L]),
         integrated_brier = as.numeric(bs_vals[1L]),
         eval_time = as.numeric(eval_times[1L]))
  else
    list(brier_score = bs_vals, scaled_brier = scaled_bs,
         integrated_brier = as.numeric(ibs), eval_time = eval_times)
}

# CANONICAL TEST
# perfect prediction of survivors: BS is 0 when S_hat = 0 for events, 1 for survivors
