# SPDX-License-Identifier: AGPL-3.0-or-later
#' SPDX-License-Identifier: AGPL-3.0-or-later
#'
#' A step of the jsdivg implementation. Called by \code{Jsdiv}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param p A vector; its length is taken and its elements indexed.
#' @param q A vector; indexed elementwise.
#' @param base Numeric; passed to \code{log}.
#' @return A numeric value.
#' @export
.t4_jsd <- function(p, q, base) {
  lg <- log(base)
  s1 <- 0
  s2 <- 0
  for (i in seq_along(p)) {
    m <- p[i] + q[i]
    if (p[i] != 0 && m != 0) s1 <- s1 + p[i] * log(2 * p[i] / m) / lg
    if (q[i] != 0 && m != 0) s2 <- s2 + q[i] * log(2 * q[i] / m) / lg
  }
  0.5 * (s1 + s2)
}

#' Jensen-Shannon divergence between two discrete distributions
#'
#' Formula: with \eqn{M = (P+Q)/2},
#' \eqn{JSD(P,Q) = \tfrac12 KL(P\|M) + \tfrac12 KL(Q\|M) =
#' \tfrac12 \sum p \log(2p/(p+q)) + \tfrac12 \sum q \log(2q/(p+q))}.
#' Unlike KL it is symmetric, always finite and bounded by \eqn{\log 2}
#' -- the finiteness is the point of averaging into \eqn{M}, since
#' \eqn{KL(P\|Q)} is infinite wherever Q puts no mass and P does.  Terms
#' with a zero contribute nothing, by \eqn{0\log 0 = 0}.  Its square
#' root is a metric.
#'
#' @param p,q Non-negative weights over the same support.
#' @param base Logarithm base; 2 gives bits, exp(1) nats.
#' @param normalize Rescale p and q to sum to one first.
#' @return List with \code{estimate}, \code{distance}, \code{bound},
#'   \code{base}, \code{n}, \code{method}.
#' @references Lin (1991), IEEE Transactions on Information Theory 37:145-151.  Paywalled at IEEE; the coded form was read from Drost's philentropy, src/distances_internal.h::jensen_shannon_internal (tarball philentropy_0.10.0 from CRAN), with exactly the zero-guards used here.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Jsdiv(V, V)
Jsdiv <- function(p, q, base = 2, normalize = TRUE) {
  p <- .t4_vec(p)
  q <- .t4_vec(q)
  if (length(p) != length(q)) stop("p and q must have the same length")
  if (any(p < 0) || any(q < 0)) stop("p and q must be non-negative")
  if (normalize) {
    if (sum(p) <= 0 || sum(q) <= 0) stop("p and q must have positive total mass")
    p <- p / sum(p)
    q <- q / sum(q)
  }
  d <- .t4_jsd(p, q, base)
  if (d < 0) d <- 0
  .t4_result(estimate = d, distance = sqrt(d), bound = log(2) / log(base),
             base = base, n = length(p), method = "Jensen-Shannon divergence")
}
