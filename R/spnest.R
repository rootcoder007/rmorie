# SPDX-License-Identifier: AGPL-3.0-or-later
#' Nested semivariogram: the linear model of regionalization
#'
#' With Z(s) = mu + sum_j a_j U_j(s) and the U_j orthogonal,
#' gamma_z(h) = sum_j a_j^2 gamma_j(h). Validity relies on the components
#' being orthogonal, which the book notes is hard to justify except when
#' a white-noise measurement-error term is nested with one other model to
#' create a nugget.
#'
#' @param h Numeric vector of non-negative lag distances.
#' @param components List of components, each a list with `model`, `sill`
#'   and `range`. Use `model = "nugget"` for a pure white-noise component.
#' @return Named list: gamma, components, total_sill.
#' @references Schabenberger & Gotway (2005), Sec 4.3.6, eqs (4.22)-(4.23), p. 150.
#' @examples
#' spnest(h = c(0, 0.5, 2), components = list(
#'   list(model = "nugget", sill = 0.2),
#'   list(model = "spherical", sill = 1, range = 1.5)))
#' @export
spnest <- function(h, components) {
  h <- as.numeric(h)
  if (any(h < 0)) stop("lag distances `h` must be non-negative")
  if (missing(components) || !length(components)) {
    stop("`components` must list at least one component")
  }
  total <- numeric(length(h))
  parts <- vector("list", length(components))
  sill_sum <- 0
  for (k in seq_along(components)) {
    cc <- components[[k]]
    model <- if (is.null(cc$model)) "exponential" else cc$model
    sill <- if (is.null(cc$sill)) 1 else as.numeric(cc$sill)
    if (sill < 0) stop("component ", k, ": `sill` must be >= 0")
    g <- if (identical(model, "nugget")) {
      ifelse(h > 0, sill, 0)
    } else {
      .sp_semivariogram(h, 0, sill, if (is.null(cc$range)) 1 else cc$range, model)
    }
    parts[[k]] <- list(model = model, sill = sill, gamma = g)
    total <- total + g
    sill_sum <- sill_sum + sill
  }
  list(gamma = total, components = parts, total_sill = sill_sum)
}
