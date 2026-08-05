# SPDX-License-Identifier: AGPL-3.0-or-later
#' Uno C-index for censored data
#'
#' The truncated, inverse-probability-of-censoring-weighted concordance
#' index.  A thin alias for \code{Cstat(method = "uno")}; the estimator
#' already exists and is not duplicated here.
#'
#' @param time Observed event or censoring times.
#' @param event Event indicator, 1 = event, 0 = censored.
#' @param risk_score Predicted risk; higher means shorter survival.
#' @return List with \code{estimate}, \code{c_statistic}, \code{se},
#'   \code{ci_lower}, \code{ci_upper}, \code{comparable}, \code{method}.
#' @references Uno, H., Cai, T., Pencina, M. J., D'Agostino, R. B. and
#'   Wei, L. J. (2011). On the C-statistics for evaluating overall
#'   adequacy of risk prediction procedures with censored survival data.
#'   Statistics in Medicine 30(10):1105-1117. \doi{10.1002/sim.4154}
#' @examples
#' Survci2(c(1, 2, 3, 4), c(1, 1, 1, 0), c(4, 3, 2, 1))
#' @export
Survci2 <- function(time, event, risk_score) {
  r <- Cstat(time, event, risk_score, method = "uno")
  c(list(estimate = as.numeric(r$c_statistic)), r,
    list(method = "Uno IPCW-weighted truncated C-statistic [Uno et al. 2011]"))
}
