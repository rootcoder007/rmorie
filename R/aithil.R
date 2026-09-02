# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hill numbers of order q.
#'
#' qD = (sum_i p_i^q)^(1/(1-q)) for q != 1, and
#' 1D = exp(-sum_i p_i log p_i) in the limit q -> 1.
#'
#' @param x Non-negative abundances or proportions; closed internally.
#' @param q Order of the diversity number.
#'
#' @return List with hill, q, prop, richness, shannon, simpson, D.
#' @references Hill, M. O. (1973), Ecology 54(2), 427-432, Equation (2)
#'   and Sect. 2.  Standard published form; the article is paywalled and
#'   was not read.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Hillq(V)
Hillq <- function(x, q = 1) {
  x <- .t1_vec(x); D <- length(x)
  if (D == 0) stop("x must be non-empty")
  if (any(x < 0)) stop("abundances must be non-negative")
  tot <- sum(x)
  if (tot <= 0) stop("abundances must not all be zero")
  p <- x / tot
  pos <- p[p > 0]
  sh <- -sum(pos * log(pos))
  si <- sum(pos^2)
  q <- as.numeric(q)
  h <- if (abs(q - 1) < 1e-12) exp(sh) else sum(pos^q)^(1 / (1 - q))
  .t1_result(hill = h, q = q, prop = p, richness = length(pos),
             shannon = sh, simpson = si, D = D,
             method = "Hill number of order q (Hill 1973 eq. 2)")
}
