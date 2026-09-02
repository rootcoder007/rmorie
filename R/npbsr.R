# SPDX-License-Identifier: AGPL-3.0-or-later
#' Nonparametric Bayes survival via a beta process
#'
#' DUPLICATE: Hjort's beta-process posterior for right-censored
#' survival is already implemented as
#' \code{morie_ghosal_survival_beta_process} in \code{ghsrv.R}; per
#' ledger/wave2/DUPMAP.tsv this is an alias, not a second copy.
#'
#' Formula: with \code{H ~ BP(c, H_0)} the posterior is again a beta
#' process and \code{dH_post(t) = (c dH_0(t) + dN(t)) / (c + Y(t-))},
#' so \code{S_hat(t) = prod (1 - dH_post(s))}. As \code{c -> 0} this
#' becomes the Kaplan-Meier estimator.
#'
#' @param time Observation times, possibly censored.
#' @param event 1 = event, 0 = censored; all events if \code{NULL}.
#' @param c Prior concentration.
#' @param lam0 Exponential base hazard rate; \code{1 / mean(time)} if
#'   \code{NULL}.
#' @return List with \code{estimate}, \code{times}, \code{S_post},
#'   \code{H_post}, \code{c}, \code{lam0}, \code{n}.
#' @references Hjort, N. L. (1990). Nonparametric Bayes estimators
#'   based on beta processes in models for life history data. Annals of
#'   Statistics, 18(3), 1259-1294. doi:10.1214/aos/1176347749
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Npbsr(V)
Npbsr <- function(time, event = NULL, c = 1.0, lam0 = NULL) {
  morie_ghosal_survival_beta_process(time, event = event, c = c, lam0 = lam0)
}
