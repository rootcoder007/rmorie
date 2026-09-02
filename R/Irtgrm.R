# SPDX-License-Identifier: AGPL-3.0-or-later
#' Samejima graded response model (alias of Grmsam)
#'
#' Samejima's GRM is already implemented as \code{\link{Grmsam}}; this
#' is the same estimator exported under the catalogue's second spelling
#' and delegates to it rather than carrying a second copy.
#'
#' @inheritParams Grmsam
#' @return As \code{\link{Grmsam}}.
#' @references Samejima (1969), Estimation of latent ability using a
#'   response pattern of graded scores, Psychometrika Monograph
#'   Supplement 34(4, Pt. 2). \doi{10.1007/BF03372160}
#' @export
#' @examples
#' Irtgrm(c(0, 1, 2), theta = c(0, 0.5, -0.5), a = 1, b_k = c(-1, 0, 1))
Irtgrm <- function(y, theta, a, b_k) Grmsam(y, theta, a, b_k)
