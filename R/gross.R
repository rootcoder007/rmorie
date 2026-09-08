# SPDX-License-Identifier: AGPL-3.0-or-later
#' Gross-error sensitivity (Hampel et al. 1986)
#'
#' gamma* = sup_x |IF(x; T, F)|, the worst influence a small amount of
#' contamination can have on the estimator.  Source consulted: Hampel,
#' Ronchetti, Rousseeuw and Stahel (1986), Robust Statistics, section 2.1c.
#'
#' @param IF numeric vector of influence-function values on a grid.
#' @param x optional numeric grid the influence function was evaluated on.
#' @return list: estimate, gamma_star, xmax, imax, brobust, n, method.
#' @keywords internal
#' @examples
#' gross(c(0.1, -0.9, 0.4), x = c(1, 2, 3))
#' @export
gross <- function(IF, x = NULL) {
  IF <- as.numeric(IF)
  n <- length(IF)
  a <- abs(IF)
  imax <- which.max(a) - 1L
  gamma <- as.numeric(a[imax + 1L])
  xmax <- if (is.null(x)) as.numeric(imax) else as.numeric(as.numeric(x)[imax + 1L])
  list(estimate = gamma, gamma_star = gamma, xmax = xmax,
       imax = as.integer(imax), brobust = is.finite(gamma),
       n = as.integer(n),
       method = "Gross-error sensitivity (Hampel et al. 1986)")
}

# CANONICAL TEST
# r <- gross(c(0.1, -0.9, 0.4), x = c(1, 2, 3))
# stopifnot(abs(r$estimate - 0.9) < 1e-12, abs(r$xmax - 2) < 1e-12)

#' @rdname gross
#' @keywords internal
#' @export
morie_gross_error_sensitivity <- gross
