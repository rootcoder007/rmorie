# SPDX-License-Identifier: AGPL-3.0-or-later
#' Nugget effect in the semivariogram: discontinuity at the origin.
#'
#' gamma_z(h) = c0 + sigma0^2 gamma_2(h), with Var(Z(s)) = c0 + sigma0^2.
#' The point of the model is the JUMP: gamma(0) is 0 by definition while
#' the limit as h approaches 0 from above is c0. Both are returned.
#'
#' @param h Numeric vector of non-negative lag distances.
#' @param nugget Nugget c0.
#' @param sill Partial sill sigma0^2.
#' @param range Practical range of the nested continuous component.
#' @param model Unit-sill component: "exponential", "gaussian" or "spherical".
#' @return Named list: gamma, gamma_at_zero, limit_at_zero_plus, nugget,
#'   sill, total_sill, model.
#' @references Schabenberger & Gotway (2005), Sec 4.3.6, p. 150.
#' @examples
#' spnug(h = c(0, 0.5, 2), nugget = 0.3, sill = 1, range = 1)
#' @export
spnug <- function(h, nugget = 0, sill = 1, range = 1, model = "exponential") {
  g <- .sp_semivariogram(h, nugget, sill, range, model)
  list(gamma = g, gamma_at_zero = 0, limit_at_zero_plus = as.numeric(nugget),
       nugget = as.numeric(nugget), sill = as.numeric(sill),
       total_sill = as.numeric(nugget) + as.numeric(sill), model = model)
}
