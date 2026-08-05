# SPDX-License-Identifier: AGPL-3.0-or-later
#' PageRank by power iteration -- alias of \code{\link{Pgrank}}
#'
#' \code{Pgrank} already runs a fixed number of power iterations in all
#' three arms. \code{tol} is accepted for signature compatibility and
#' ignored: an early stop on a tolerance is exactly what makes two arms
#' disagree in the last digits.
#'
#' Formula: \code{p = (1 - d)/n + d M^T p}, iterated \code{max_iter} times.
#'
#' @param A Adjacency matrix.
#' @param d Damping factor.
#' @param max_iter Power iterations.
#' @param tol Ignored; accepted for signature compatibility.
#' @return The value of \code{\link{Pgrank}}.
#' @references Page, L., Brin, S., Motwani, R. & Winograd, T. (1999).
#'   Stanford InfoLab technical report 1999-66.
#' @export
Sgtpgr <- function(A, d = 0.85, max_iter = 100, tol = NULL) {
  Pgrank(A, d = d, n_iter = max_iter)
}
