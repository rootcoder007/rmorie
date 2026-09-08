# SPDX-License-Identifier: AGPL-3.0-or-later

#' Z-transform of a discrete-time sequence
#'
#' The two-sided z-transform \eqn{X(z) = \sum_n x_n z^{-n}} of the
#' sequence \code{x}, evaluated at the complex point or points \code{z}.
#' \code{n0} is the time index of the first sample, so the sum runs over
#' \eqn{n = n_0, \ldots, n_0 + N - 1}; \code{n0 = 0} gives the causal
#' finite-length case \eqn{X(z) = \sum_{n=0}^{N-1} x_n z^{-n}}, which is
#' the transfer function of an FIR system.
#'
#' A finite-length sequence converges everywhere except possibly at
#' \eqn{z = 0} (samples at \eqn{n > 0} put a pole there) and at infinity,
#' so no region of convergence has to be supplied. An FIR sequence has
#' all its poles at the origin and is therefore always stable; a
#' recursive-filter stability test in the sense of Jury's criterion needs
#' the denominator polynomial, which this function does not take.
#'
#' Delegates to \code{\link{Ztrans}}, the canonical implementation in
#' morie. Mirrors \code{morie.fn.zfm} on the Python side.
#'
#' @param x Numeric or complex vector of samples
#'   \eqn{x(n_0), \ldots, x(n_0 + N - 1)}.
#' @param z Complex scalar or vector of evaluation points.
#' @param n0 Integer time index of the first sample. Default 0 (causal).
#' @return Named list with \code{X}, \code{z}, \code{coefficients},
#'   \code{n}, \code{causal}, \code{degree}, \code{method}.
#' @references Jury E I (1964). \emph{Theory and Application of the
#'   z-Transform Method}. Wiley, New York, Chapter 1.
#' @examples
#' Zfm(c(1, 2, 3), z = 2)$X
#' @export
Zfm <- function(x, z, n0 = 0) {
  if (is.null(z)) {
    stop("z must be given; use Ztrans for the coefficients-only form",
         call. = FALSE)
  }
  Ztrans(x, z = z, n0 = n0)
}
