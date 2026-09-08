# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hellinger distance (alias of ecccen-style duplicate entry)
#'
#' Thin alias for \code{\link{hellie}}, which carries the implementation and
#' the source discussion (Hellinger 1909).  The duplicate exists because the
#' extraction pipeline created two entries for the same method.
#'
#' @param p,q non-negative masses over a common support.
#' @param normalise rescale each argument to sum to one first.
#' @return list: estimate, h2, bc, affinity, n, method.
#' @keywords internal
#' @examples
#' hellngd(c(0.25, 0.75), c(0.25, 0.75))
#' @export
hellngd <- function(p, q, normalise = TRUE) hellie(p, q, normalise)
