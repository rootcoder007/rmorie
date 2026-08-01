# SPDX-License-Identifier: AGPL-3.0-or-later

#' GWR kernel weight functions
#'
#' Geographical weights \eqn{w_i(u)} for a geographically weighted regression
#' kernel. The kernel decides how fast an observation's influence on the local
#' fit at \eqn{u} decays with distance. Schabenberger & Gotway require only
#' that "the influence of an observation decreases with increasing distance
#' from s0" (Sec. 6.1.3.1, p. 317) and refer the reader to the kernels of
#' Sec. 5.3.2 and to Fotheringham et al. (2002); the four kernels named here
#' come from the GWR literature rather than from the book.
#'
#' \describe{
#'   \item{gaussian}{\eqn{\exp(-0.5 (d/h)^2)}, positive at every distance.}
#'   \item{bisquare}{\eqn{(1 - (d/h)^2)^2} for \eqn{d < h}, otherwise 0.}
#'   \item{tricube}{\eqn{(1 - (d/h)^3)^3} for \eqn{d < h}, otherwise 0.}
#'   \item{boxcar}{1 for \eqn{d < h}, otherwise 0.}
#' }
#'
#' Bisquare and tricube reach zero *smoothly* -- their first derivative
#' vanishes at the support edge -- which is what the GWR white paper means by
#' calling the bisquare "near-Gaussian". The boxcar drops discontinuously and
#' is the only one of the four that does.
#'
#' Sec. 5.3.2 writes its Gaussian as a probability density,
#' \eqn{(1/(h \sqrt{2\pi})) \exp(-0.5 (d/h)^2)}, where the GWR literature
#' drops the leading constant. Both are available through `normalized`, and
#' they are not in conflict: weighted least squares is invariant to a positive
#' scalar applied to every weight, so the two produce an identical hat matrix
#' and identical local coefficients. Only the printed weights differ.
#'
#' @param distance Distances \eqn{d_i(u)} from the regression point.
#'   Must be non-negative.
#' @param bandwidth With `adaptive = FALSE`, the bandwidth \eqn{h} in
#'   coordinate units. With `adaptive = TRUE`, a neighbour count: \eqn{h}
#'   becomes the distance to the `bandwidth`-th nearest point, so the same
#'   number of observations enters every local fit however unevenly the sample
#'   is spread. The regression point counts as its own first neighbour.
#' @param kernel_type One of "gaussian", "bisquare", "tricube", "boxcar".
#' @param adaptive Logical; use a nearest-neighbour bandwidth.
#' @param normalized Logical; return Sec. 5.3.2's density form. Gaussian only.
#' @return A list with `weights`, `bandwidth` (the distance actually used),
#'   `kernel`, `adaptive`, `normalized`, `truncated` and `n_nonzero`.
#' @references Schabenberger Ch 5, Sec 5.3.2, pp. 240-241, and Ch 6,
#'   Sec 6.1.3.1, p. 317. Cleveland, W. S. (1979), JASA 74:829-836, for the
#'   tri-cube. Charlton, M., Geographically Weighted Regression White Paper,
#'   pp. 6-7. Fotheringham, Brunsdon & Charlton (2002), pp. 56-57, as cited by
#'   the GWmodel package, which is the source consulted for the boxcar.
#' @export
spgwrk <- function(distance, bandwidth, kernel_type = "gaussian",
                   adaptive = FALSE, normalized = FALSE) {
  h <- if (adaptive) {
    .schab_adaptive_bandwidth(distance, bandwidth)
  } else {
    as.numeric(bandwidth)[1L]
  }
  w <- .schab_kernel_weights(distance, h, kernel_type, normalized = normalized)
  list(
    weights = w,
    bandwidth = as.numeric(h),
    kernel = kernel_type,
    adaptive = isTRUE(adaptive),
    normalized = isTRUE(normalized),
    truncated = kernel_type != "gaussian",
    n_nonzero = sum(as.numeric(w) > 0)
  )
}
