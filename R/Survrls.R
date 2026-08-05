# SPDX-License-Identifier: AGPL-3.0-or-later
#' Restricted mean survival time over a fixed horizon
#'
#' RMST answers what a hazard ratio does not: how much longer, on average,
#' over a stated window, and it needs no proportional-hazards assumption.
#' The horizon is not optional in substance, because past the last
#' observed time the curve is a plateau the data cannot pin down, so
#' \code{t_star} is required here rather than defaulted.
#'
#' A thin alias for \code{Rmst}; the estimator already exists and is not
#' duplicated here.
#'
#' Formula: RMST(t*) = integral_0^{t*} S(u) du, the survival curve being a
#' step function integrated as summed rectangles.
#'
#' @param fit Observed event or censoring times.
#' @param event Event indicator, 1 = event, 0 = censored.
#' @param t_star Restriction horizon, positive.
#' @return List with \code{estimate}, \code{tau}, \code{n_events},
#'   \code{method}.
#' @references Royston, P. and Parmar, M. K. B. (2013). Restricted mean
#'   survival time. BMC Medical Research Methodology 13:152.
#'   \doi{10.1186/1471-2288-13-152}
#'   Klein, J. P. and Moeschberger, M. L. (2003). Survival Analysis, 2nd
#'   ed. Springer, section 4.5.
#' @examples
#' Survrls(c(1, 2, 3), c(1, 1, 1), 3)
#' @export
Survrls <- function(fit, event, t_star) {
  if (as.numeric(t_star) <= 0) stop("restricted_lifetime: t_star must be positive")
  r <- Rmst(fit, event, tau = as.numeric(t_star))
  list(estimate = as.numeric(r$rmst), tau = as.numeric(r$tau),
       n_events = as.integer(r$n_events),
       method = "RMST(t*) = area under the KM curve [Royston & Parmar 2013]")
}

# CANONICAL TEST
# stopifnot(abs(Survrls(c(1, 2, 3), c(1, 1, 1), 3)$estimate - 2) < 1e-12)
