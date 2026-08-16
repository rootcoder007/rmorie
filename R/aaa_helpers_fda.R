# SPDX-License-Identifier: AGPL-3.0-or-later
# Private numeric helpers shared by the functional-data function files.
#
# This is the mirror of morie.fn._fdacore on the Python side.  Every routine
# performs the same floating-point operations in the same order as its Python
# counterpart, which is what lets the parity harness assert agreement at 1e-9.
#
# The integration rule here always runs over the WHOLE grid.  A sibling module
# once integrated over [a+h, b-h], dropping both end intervals, and returned
# 3.8667 where the closed form is 4; both arms had the same defect, so parity
# was green and only a closed-form anchor caught it.  Nothing in this file may
# narrow the interval.
#
# Nothing here is exported.

#' .fdtrapz
#'
#' Part of the helpers_fda implementation; see the file header for the
#' source it follows.
#'
#' @param t See Usage.
#' @param v See Usage.
#' @return The value of \code{s}, as built in the body.
#' @export
.fdtrapz <- function(t, v) {
  s <- 0
  n <- length(t)
  if (n < 2L) return(s)
  for (i in seq_len(n - 1L)) s <- s + 0.5 * (v[i] + v[i + 1L]) * (t[i + 1L] - t[i])
  s
}

#' .fdgrid
#'
#' Part of the helpers_fda implementation; see the file header for the
#' source it follows.
#'
#' @param n See Usage.
#' @return A numeric value.
#' @export
.fdgrid <- function(n) (seq_len(n) - 1) / (n - 1)

#' .fdcolmeans
#'
#' Part of the helpers_fda implementation; see the file header for the
#' source it follows.
#'
#' @param A See Usage.
#' @param nr See Usage.
#' @param nc See Usage.
#' @return A numeric value.
#' @export
.fdcolmeans <- function(A, nr, nc) {
  m <- numeric(nc)
  for (i in seq_len(nr)) for (j in seq_len(nc)) m[j] <- m[j] + A[i, j]
  m / nr
}
