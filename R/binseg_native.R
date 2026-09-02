# Greedy binary segmentation for changepoints.
# Sources: Scott, A. J. and Knott, M. (1974), A cluster analysis method
# for grouping means in the analysis of variance, Biometrics 30(3),
# 507-512; Killick, R., Fearnhead, P. and Eckley, I. A. (2012), JASA
# 107(500), 1590-1598, Sec. 2.1 (binary segmentation as the
# approximate alternative to the exact segment-neighbourhood search
# that PELT solves).
#
# Native implementation mirroring Python morie.fn.binseg: same segment
# scan order, same strictly-greater ">" tie-break (earliest maximiser
# wins), and the same segment-cost tables as morie_pelt.

#' Binary segmentation for changepoints
#'
#' Recursively splits the series at the position giving the largest
#' reduction in total segment cost, stopping after \code{K} splits or
#' as soon as no split improves the cost by more than \code{penalty}.
#' This is the greedy approximation of Killick et al. (2012), Sec.
#' 2.1; \code{\link{morie_pelt}} solves the same objective exactly and
#' is the natural cross-check.
#'
#' @param x Numeric series.
#' @param K Maximum number of changepoints.
#' @param cost Segment cost, \code{"mean"} (default) or
#'   \code{"meanvar"}; see \code{\link{morie_pelt}}.
#' @param penalty Cost improvement a split must beat, default 0.
#' @param min_seglen Minimum segment length.
#' @return A list with \code{changepoints} (sorted, 0-based),
#'   \code{order} (in the order they were found),
#'   \code{improvements}, \code{n_changepoints},
#'   \code{segment_means}, \code{estimate}, \code{n}, \code{method}.
#' @references Scott, A. J. and Knott, M. (1974). A cluster analysis
#'   method for grouping means in the analysis of variance.
#'   Biometrics, 30(3), 507-512.
#' @export
#' @examples
#' morie_binseg(x = c(1, 2, 3, 4, 5, 6, 7, 8), K = 5L)
morie_binseg <- function(x, K, cost = "mean", penalty = 0,
                         min_seglen = 1L) {
  xs <- as.numeric(x)
  n <- length(xs)
  K <- as.integer(K)
  min_seglen <- as.integer(min_seglen)
  if (n < 2L * min_seglen) stop("series too short")
  penalty <- as.numeric(penalty)
  C <- .mor_cp_cost(.mor_cp_tables(xs), cost)

  best_split <- function(a, b) {
    best_gain <- -Inf
    best_tau <- -1L
    base <- C(a, b)
    lo <- a + min_seglen
    hi <- b - min_seglen
    if (hi >= lo) for (tau in seq.int(lo, hi)) {
      g <- base - (C(a, tau) + C(tau, b)) - penalty
      if (g > best_gain) { best_gain <- g; best_tau <- tau }
    }
    list(tau = best_tau, gain = best_gain)
  }

  seg_a <- 0L
  seg_b <- n
  order <- integer(0)
  gains <- numeric(0)
  while (length(order) < K) {
    ca <- NA_integer_; cb <- NA_integer_
    cg <- NULL; ct <- NA_integer_
    for (i in seq_along(seg_a)) {
      a <- seg_a[i]; b <- seg_b[i]
      if (b - a < 2L * min_seglen) next
      s <- best_split(a, b)
      if (s$tau > 0L && (is.null(cg) || s$gain > cg)) {
        ca <- a; cb <- b; cg <- s$gain; ct <- s$tau
      }
    }
    if (is.null(cg) || cg <= 0) break
    order <- c(order, ct)
    gains <- c(gains, cg)
    drop <- which(seg_a == ca & seg_b == cb)[1L]
    seg_a <- seg_a[-drop]; seg_b <- seg_b[-drop]
    seg_a <- c(seg_a, ca, ct); seg_b <- c(seg_b, ct, cb)
  }
  taus <- sort(order)
  bounds <- c(0L, taus, n)
  list(changepoints = as.numeric(taus),
       order = as.numeric(order),
       improvements = gains,
       n_changepoints = length(taus),
       segment_means = .mor_cp_segmeans(xs, bounds),
       estimate = as.numeric(taus),
       n = n,
       method = paste("Binary segmentation (Scott-Knott 1974;",
                      "Killick et al. 2012 Sec. 2.1)"))
}
