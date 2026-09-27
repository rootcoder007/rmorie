#' Gaussian kernel intensity of a point pattern with edge corrections
#'
#' With the isotropic Gaussian kernel of standard deviation \eqn{\sigma} and
#' the edge mass \eqn{e(u)} (the share of the kernel centred at u inside the
#' rectangular window, a product of normal CDF differences):
#' \code{"none"} gives \eqn{\sum_j w_j k(u - x_j)}, \code{"uniform"} divides
#' by \eqn{e(u)} (Diggle 1985) and \code{"diggle"} divides each term by
#' \eqn{e(x_j)} (Jones 1993). Evaluated at the data points (\code{at =
#' NULL}, leaving each point out of its own sum by default) or at given
#' locations; these are \code{spatstat.explore::density.ppp} with \code{at
#' = "points"} and its \code{edge} and \code{diggle} options.
#'
#' @param points Event locations (n x 2) inside \code{window}.
#' @param window Rectangle \code{c(xmin, xmax, ymin, ymax)}.
#' @param sigma Kernel standard deviation.
#' @param at Optional (m x 2) evaluation locations.
#' @param correction \code{"none"}, \code{"uniform"} or \code{"diggle"}.
#' @param leaveoneout At the data points, omit each point's own kernel.
#' @param weights Optional point weights.
#' @return List with \code{intensity}, \code{edge}, \code{correction},
#'   \code{sigma}.
#' @references Diggle, P. J. (1985). A kernel method for smoothing point
#'   process data. Applied Statistics 34, 138-147.
#'
#'   Jones, M. C. (1993). Simple boundary correction for kernel density
#'   estimation. Statistics and Computing 3, 135-146.
#' @examples
#' KernelIntensity(rbind(c(0.2, 0.3), c(0.4, 0.5), c(0.7, 0.2)), c(0, 1, 0, 1), 0.2)$intensity
#' @export
KernelIntensity <- function(points, window, sigma, at = NULL, correction = c("uniform", "none", "diggle"),
                            leaveoneout = TRUE, weights = NULL) {
  correction <- match.arg(correction)
  P <- as.matrix(points)
  w4 <- as.numeric(window)
  w <- if (is.null(weights)) rep(1, nrow(P)) else as.numeric(weights)
  edge <- function(U) {
    (stats::pnorm((w4[2] - U[, 1]) / sigma) - stats::pnorm((w4[1] - U[, 1]) / sigma)) *
      (stats::pnorm((w4[4] - U[, 2]) / sigma) - stats::pnorm((w4[3] - U[, 2]) / sigma))
  }
  if (correction == "diggle") w <- w / edge(P)
  U <- if (is.null(at)) P else as.matrix(at)
  own <- is.null(at) && leaveoneout
  cc <- 1 / (2 * pi * sigma^2)
  out <- numeric(nrow(U))
  for (k in seq_len(nrow(U))) {
    k2 <- w * cc * exp(-((U[k, 1] - P[, 1])^2 + (U[k, 2] - P[, 2])^2) / (2 * sigma^2))
    if (own) k2[k] <- 0
    out[k] <- sum(k2)
  }
  e <- edge(U)
  if (correction == "uniform") out <- out / e
  list(intensity = out, edge = e, correction = correction, sigma = sigma)
}
