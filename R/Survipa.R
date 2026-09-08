# SPDX-License-Identifier: AGPL-3.0-or-later
#' Index of prediction accuracy (scaled Brier score)
#'
#' IPA = 1 - BS(model) / BS(null), the null being the Kaplan-Meier
#' marginal survival.  It is exactly the \code{scaled_brier} already
#' returned by \code{Brier}, so this is a thin alias rather than a second
#' implementation.  IPA is 1 for a perfect model, 0 for a model no better
#' than the marginal, and negative for one that is worse.
#'
#' @param time Observed event or censoring times.
#' @param event Event indicator, 1 = event, 0 = censored.
#' @param predicted_survival Predicted survival at \code{eval_time}.
#' @param eval_time Evaluation time.
#' @return List with \code{estimate} (IPA), \code{brier_score},
#'   \code{scaled_brier}, \code{eval_time}, \code{method}.
#' @references Kattan, M. W. and Gerds, T. A. (2018). The index of
#'   prediction accuracy: an intuitive measure useful for evaluating risk
#'   prediction models. Diagnostic and Prognostic Research 2:7.
#'   \doi{10.1186/s41512-018-0029-2}
#' @examples
#' Survipa(c(1, 2, 3, 4), c(1, 1, 0, 1), c(0.9, 0.7, 0.5, 0.3), 2.5)
#' @export
Survipa <- function(time, event, predicted_survival, eval_time) {
  r <- Brier(time, event, predicted_survival, eval_time)
  list(estimate = as.numeric(r$scaled_brier),
       brier_score = as.numeric(r$brier_score),
       scaled_brier = as.numeric(r$scaled_brier),
       eval_time = as.numeric(r$eval_time),
       method = "IPA = 1 - BS_model/BS_null [Kattan & Gerds 2018]")
}
