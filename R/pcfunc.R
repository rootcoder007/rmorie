# SPDX-License-Identifier: AGPL-3.0-or-later
#' Pair correlation function g(r) from the derivative of Ripley's K
#'
#' Formula: \code{g(r) = K'(r) / (2 pi r)}. \code{K} is estimated with
#' the reduced-sample (border) correction and differentiated by a
#' central difference of half-width \code{h},
#' \code{g_hat(r) = (K_hat(r + h) - K_hat(r - h)) / (2 h) / (2 pi r)},
#' which is deterministic -- no kernel smoothing and no data-dependent
#' bandwidth rule. Under complete spatial randomness \code{K(r) = pi
#' r^2} so \code{g(r) = 1} at every radius; above one means clustering
#' at that scale and below one regularity.
#'
#' @param points Point coordinates, n by 2.
#' @param window \code{c(xmin, ymin, xmax, ymax)} or an (m, 2) vertex
#'   matrix; \code{NULL} takes the bounding box of \code{points}.
#' @param r Radii, strictly positive.
#' @param h Half-width of the central difference; defaults to one
#'   quarter of the smallest radius, which keeps \code{r - h} positive.
#' @return List with \code{g}, \code{r}, \code{K}, \code{h},
#'   \code{estimate}, \code{lambda_hat}, \code{n}.
#' @references Stoyan, D. & Stoyan, H. (1994). Fractals, Random Shapes
#'   and Point Fields, Wiley, chapter 14; Diggle, P. J. (2003).
#'   Statistical Analysis of Spatial Point Patterns, 2nd edition,
#'   Arnold, section 4.3.
#' @export
Pcfunc <- function(points, window, r, h = NULL) {
  p <- as.matrix(points)
  n <- nrow(p)
  if (n < 2L) stop("Pcfunc: need at least two points")
  region <- .sp_region(window, p)
  rs <- as.numeric(r)
  if (!length(rs)) stop("Pcfunc: r is empty")
  if (any(rs <= 0)) stop("Pcfunc: r must be strictly positive")
  hh <- if (is.null(h)) min(rs) / 4 else as.numeric(h)
  if (hh <= 0) stop("Pcfunc: h must be positive")
  Klo <- .sp_k(p, region, rs - hh, correction = "border")
  Khi <- .sp_k(p, region, rs + hh, correction = "border")
  Kat <- .sp_k(p, region, rs, correction = "border")
  g <- (Khi - Klo) / (2 * hh) / (2 * pi * rs)
  .t1_result(g = g, r = rs, K = Kat, h = hh, estimate = g[1],
             lambda_hat = .sp_intensity(p, region), n = n,
             method = "Pair correlation function from K'(r) / (2 pi r)")
}
