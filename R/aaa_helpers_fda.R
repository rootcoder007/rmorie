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
#' A step of the helpers_fda implementation. Called by \code{Scfd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t A vector; its length is taken and its elements indexed.
#' @param v A vector; indexed elementwise.
#' @return The value of \code{s}, as built in the body.
#' @export
.fdtrapz <- function(t, v) {
  s <- 0
  n <- length(t)
  if (n < 2L) {
    return(s)
  }
  for (i in seq_len(n - 1L)) s <- s + 0.5 * (v[i] + v[i + 1L]) * (t[i + 1L] - t[i])
  s
}

#' .fdgrid
#'
#' A step of the helpers_fda implementation. Called by \code{Scfd}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @return A numeric value.
#' @export
.fdgrid <- function(n) (seq_len(n) - 1) / (n - 1)

#' .fdcolmeans
#'
#' A step of the helpers_fda implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A A matrix; indexed by row and column.
#' @param nr A count; the body uses it as \code{seq_len(...)}.
#' @param nc A count; the body uses it as \code{seq_len(...)}.
#' @return A numeric value.
#' @export
.fdcolmeans <- function(A, nr, nc) {
  m <- numeric(nc)
  for (i in seq_len(nr)) for (j in seq_len(nc)) m[j] <- m[j] + A[i, j]
  m / nr
}
