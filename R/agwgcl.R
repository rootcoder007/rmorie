# SPDX-License-Identifier: AGPL-3.0-or-later
#' Global-norm gradient clipping
#'
#' Pascanu, Mikolov and Bengio (2013), On the difficulty of training
#' recurrent neural networks, ICML 28, 1310-1318 (arXiv:1211.5063),
#' algorithm 1: if ||g|| >= threshold then g <- threshold g / ||g||, with
#' the norm taken over the concatenation of every parameter's gradient.
#' AlphaZero (Silver et al., arXiv:1712.01815 -- FETCHED) does not state a
#' clipping threshold, so the routine is documented as the standard
#' stabiliser it is rather than attributed to that paper.
#'
#' @param grad the gradient, of any nesting; flattened for the norm.
#' @param max_norm the clipping threshold.
#' @return list: estimate, clipped, norm, scale, was_clipped, method.
#' @keywords internal
#' @examples
#' Gradclip(c(3, 4), 1)$norm
#' @export
Gradclip <- function(grad, max_norm = 1) {
  g <- .s03vec(grad)
  s2 <- 0
  for (x in g) s2 <- s2 + x * x
  nrm <- sqrt(s2)
  mx <- as.numeric(max_norm)
  if (nrm >= mx && nrm > 0) { scale <- mx / nrm; was <- TRUE } else { scale <- 1; was <- FALSE }
  out <- g * scale
  s2b <- 0
  for (x in out) s2b <- s2b + x * x
  list(estimate = sqrt(s2b), clipped = out, norm = nrm, scale = scale,
       was_clipped = was,
       method = "Global-norm gradient clipping (Pascanu et al. 2013, alg. 1)")
}
