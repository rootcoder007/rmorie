# SPDX-License-Identifier: AGPL-3.0-or-later
#' Generalized partial credit model (alias of Gpcm)
#'
#' Muraki's GPCM is already implemented as \code{\link{Gpcm}}; this is
#' the same estimator exported under the catalogue's second spelling and
#' delegates to it rather than carrying a second copy.
#'
#' @inheritParams Gpcm
#' @return As \code{\link{Gpcm}}.
#' @references Muraki (1992), A generalized partial credit model,
#'   Applied Psychological Measurement 16(2):159-176.
#'   \doi{10.1177/014662169201600206}
#' @export
Irtgpc <- function(y, theta, a, b_j) Gpcm(y, theta, a, b_j)
