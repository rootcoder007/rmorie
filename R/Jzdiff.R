# SPDX-License-Identifier: AGPL-3.0-or-later
#' Jensen-Shannon divergence
#'
#' Symmetric, always finite (unlike Kullback-Leibler), bounded above by
#' log 2, and equal to log 2 exactly when the two distributions have
#' disjoint support -- all four checked in the tests.  Its square root
#' is a metric (Endres and Schindelin 2003).  The stub this function
#' replaces carried the label "Jensen-Zhang (1986)", which corresponds
#' to no traceable paper on this divergence; the attribution below is
#' Lin's, verified against the DOI.
#'
#' Formula: JS(P, Q) = H(M) - (H(P) + H(Q))/2 with M = (P + Q)/2.
#'
#' @param y Support labels; only the length is used.
#' @param p Non-negative weights, normalised internally.
#' @param q Non-negative weights of the same length.
#' @return List with \code{estimate}, \code{divergence},
#'   \code{distance}, \code{entropy_p}, \code{entropy_q},
#'   \code{entropy_m}, \code{n}, \code{method}.
#' @references Lin (1991), Divergence measures based on the Shannon
#'   entropy, IEEE Transactions on Information Theory 37(1):145-151,
#'   \doi{10.1109/18.61115}; Endres and Schindelin (2003), IEEE
#'   Transactions on Information Theory 49(7):1858-1860.
#'   \doi{10.1109/TIT.2003.813506}
#' @export
#' @examples
#' Jzdiff(y = 5L, p = 0.5, q = 0.5)
Jzdiff <- function(y, p, q) {
  pv <- .s03vec(p)
  qv <- .s03vec(q)
  if (length(pv) == 0L) stop("jenson_zhang_disparity: p is empty")
  if (length(qv) != length(pv)) stop("jenson_zhang_disparity: p and q have different lengths")
  if (!is.null(y)) {
    yv <- .s03vec(y)
    if (length(yv) != length(pv)) stop("jenson_zhang_disparity: y and p have different lengths")
  }
  if (any(c(pv, qv) < 0)) stop("jenson_zhang_disparity: weights must be non-negative")
  if (sum(pv) <= 0 || sum(qv) <= 0) stop("jenson_zhang_disparity: weights must sum to something positive")
  P <- pv / sum(pv)
  Q <- qv / sum(qv)
  M <- (P + Q) / 2
  H <- function(p) -sum(p[p > 0] * log(p[p > 0]))
  hp <- H(P)
  hq <- H(Q)
  hm <- H(M)
  js <- max(hm - (hp + hq) / 2, 0)
  .t1_result(estimate = js, divergence = js, distance = sqrt(js),
             entropy_p = hp, entropy_q = hq, entropy_m = hm, n = length(P),
             method = "JS = H(M) - (H(P) + H(Q))/2 with M = (P+Q)/2, Lin (1991) eq. (3.6)")
}
