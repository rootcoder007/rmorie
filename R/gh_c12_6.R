# SPDX-License-Identifier: AGPL-3.0-or-later
#' Semiparametric efficiency bound
#'
#' var >= (grad psi)' I^(-1) (grad psi) is the Cramer-Rao lower bound in
#' its semiparametric form.  Written this way it makes plain why the
#' bound depends only on the gradient of the functional and the
#' information operator, so a single information matrix serves every
#' functional of the model at once.
#'
#' Formula: bound = g' I^(-1) g, obtained by solving I x = g.
#'
#' @param grad_psi Gradient of the functional, length k.
#' @param info_matrix Information matrix, k by k and invertible.
#' @return List with \code{estimate} (the bound), \code{positive},
#'   \code{method}.
#' @references Ghosal & van der Vaart (2017), Fundamentals of
#'   Nonparametric Bayesian Inference, CUP, section 12.3.
#' @export
Ghosalsemiparaeff <- function(grad_psi, info_matrix) {
  g <- as.numeric(grad_psi)
  I <- as.matrix(info_matrix)
  k <- length(g)
  if (!all(dim(I) == c(k, k)))
    stop("info_matrix must be square and match grad_psi")
  bound <- sum(g * solve(I, g))
  .t1_result(estimate = bound, positive = bound > 0,
             method = "semiparametric efficiency bound (GvdV 2017 sec. 12.3)")
}
