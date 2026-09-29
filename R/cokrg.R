# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cokriging of a primary variable from co-located primary and secondary data
#'
#' Exponential direct covariances (sill minus nugget, plus the nugget at
#' distance zero) and an exponential cross-covariance, written as a sum of
#' coregionalization structures and solved by \code{\link{LmcCokriging}}:
#' simple cokriging with known \code{means} (default zero means, the former
#' behaviour) or ordinary cokriging with \code{means = NULL}. Python parity:
#' \code{morie.fn.cokrg.cokriging}.
#'
#' @param x Primary variable (n,).
#' @param y Secondary variable (n,).
#' @param coords Co-located coord matrix.
#' @param target Target coords (m by d) or (d,).
#' @param sill_p,range_p Primary auto-covariance parameters.
#' @param sill_s,range_s Secondary auto-covariance parameters.
#' @param cross_sill,cross_range Cross-covariance parameters.
#' @param nugget Nugget.
#' @param means Known means c(m1, m2) for simple cokriging; NULL for ordinary
#'   cokriging.
#' @return Named list: estimate, se, n, method.
#' @references Wackernagel, H. (2003). Multivariate Geostatistics, 3rd edn.
#'   Springer, ch. 24-25.
#'
#'   Schabenberger, O. and Gotway, C. A. (2005). Statistical Methods for
#'   Spatial Data Analysis. Chapman and Hall/CRC.
#' @examples
#' set.seed(1)
#' cokrg(x = rnorm(50), y = rnorm(50), coords = matrix(runif(100), 50, 2), target = rnorm(50))
#' @export
cokrg <- function(x, y, coords, target,
                  sill_p = 1, range_p = 1,
                  sill_s = 1, range_s = 1,
                  cross_sill = 0.5, cross_range = 1,
                  nugget = 0, means = c(0, 0)) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  n <- length(x)
  coords <- if (is.matrix(coords)) {
    coords
  } else {
    matrix(as.numeric(unlist(coords)), nrow = n)
  }
  if (!is.matrix(target)) {
    tv <- as.numeric(unlist(target))
    if (length(tv) %% ncol(coords) != 0L) {
      stop("target/coords dim mismatch")
    }
    target <- matrix(tv, ncol = ncol(coords), byrow = TRUE)
  }
  if (length(y) != n || nrow(coords) != n) {
    stop("x, y, and coords must have matching n")
  }
  if (ncol(target) != ncol(coords)) stop("target/coords dim mismatch")
  lmc <- list(
    list(model = "Nug", B = diag(nugget, 2)),
    list(model = "Exp", range = range_p, B = matrix(c(sill_p - nugget, 0, 0, 0), 2)),
    list(model = "Exp", range = range_s, B = matrix(c(0, 0, 0, sill_s - nugget), 2)),
    list(model = "Exp", range = cross_range, B = matrix(c(0, cross_sill, cross_sill, 0), 2))
  )
  r <- LmcCokriging(c(x, y), rbind(coords, coords), rep(0:1, each = n), target, lmc, target = 0L, means = means)
  m <- nrow(target)
  ses <- sqrt(pmax(r$variance, 0))
  list(
    estimate = if (m == 1) r$prediction[1] else r$prediction,
    se = if (m == 1) ses[1] else ses, n = n,
    method = paste(if (is.null(means)) "Ordinary" else "Simple",
                   "cokriging (exponential direct and cross covariances)")
  )
}

#' @rdname cokrg
#' @keywords internal
#' @export
morie_cokrg <- cokrg
