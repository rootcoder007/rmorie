#' Cox process given its realised intensity field
#'
#' A Cox process is a Poisson process with a random intensity; given the
#' realised intensity it is an inhomogeneous Poisson process.
#' \code{CoxProcess} treats \code{intensity_field} (an ny by nx lattice over
#' \code{window}, row iy the iy-th band from ymin) as that realisation and
#' draws the conditional Poisson process: a Poisson count with mean
#' max(lambda, 0) times the cell area per cell, placed uniformly (Philox
#' draws as \code{\link{LgcpSimulate}}, streams 1000 and up). The
#' unconditional log-Gaussian Cox process is \code{LgcpSimulate}. Identical to
#' the Python arm \code{morie.fn.sgcox.cox_process}.
#'
#' @param intensity_field Matrix of intensities (ny rows, nx columns).
#' @param window Rectangle c(xmin, xmax, ymin, ymax).
#' @param seed Philox seed.
#' @return List: points (two-column matrix), n_points, value, mean_intensity,
#'   expected_count.
#' @references Cox, D. R. (1955). Some statistical methods connected with
#'   series of events. Journal of the Royal Statistical Society B 17, 129-164.
#'
#'   Moller, J., Syversveen, A. R. and Waagepetersen, R. P. (1998). Log
#'   Gaussian Cox processes. Scandinavian Journal of Statistics 25, 451-482.
#' @examples
#' CoxProcess(rbind(c(2, 5), c(1, 8)), c(0, 2, 0, 2), seed = 3)$n_points
#' @export
CoxProcess <- function(intensity_field, window, seed = 1) {
  lam <- as.matrix(intensity_field)
  ny <- nrow(lam)
  nx <- ncol(lam)
  dx <- (window[2] - window[1]) / nx
  dy <- (window[4] - window[3]) / ny
  U <- .cx_stream(seed, 1000, FALSE)
  pts <- matrix(0, 0, 2)
  for (iy in seq_len(ny)) {
    for (ix in seq_len(nx)) {
      k <- .cx_poisson(max(lam[iy, ix], 0) * dx * dy, U)
      for (t in seq_len(k)) {
        px <- window[1] + (ix - 1) * dx + dx * U()
        py <- window[3] + (iy - 1) * dy + dy * U()
        pts <- rbind(pts, c(px, py))
      }
    }
  }
  list(
    points = pts, n_points = nrow(pts), value = nrow(pts), mean_intensity = mean(lam),
    expected_count = sum(pmax(lam, 0)) * dx * dy
  )
}
