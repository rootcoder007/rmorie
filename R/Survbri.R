# SPDX-License-Identifier: AGPL-3.0-or-later
#' Brier score for survival prediction
#'
#' E[(I(T > t) - S(t|X))^2] with inverse-probability-of-censoring
#' weights.  A thin alias for \code{Brier}; the estimator already exists
#' and is not duplicated here.
#'
#' @param time Observed event or censoring times.
#' @param event Event indicator, 1 = event, 0 = censored.
#' @param predicted_survival Predicted survival at \code{eval_time}.
#' @param eval_time Evaluation time.
#' @return List with \code{estimate}, \code{brier_score},
#'   \code{scaled_brier}, \code{integrated_brier}, \code{eval_time},
#'   \code{method}.
#' @references Graf, E., Schmoor, C., Sauerbrei, W. and Schumacher, M.
#'   (1999). Statistics in Medicine 18(17-18):2529-2545.
#'   Gerds, T. A. and Schumacher, M. (2006). Biometrical Journal
#'   48(6):1029-1040. \doi{10.1002/bimj.200610301}
#' @examples
#' Survbri(c(1, 2, 3, 4), c(1, 1, 0, 1), c(0.9, 0.7, 0.5, 0.3), 2.5)
#' @export
Survbri <- function(time, event, predicted_survival, eval_time) {
  r <- Brier(time, event, predicted_survival, eval_time)
  c(list(estimate = as.numeric(r$brier_score)), r,
    list(method = "IPCW Brier score [Graf et al. 1999; Gerds & Schumacher 2006]"))
}
