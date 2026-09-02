# Gabriel haplotype blocks.
# Source: Gabriel, S. B. et al. (2002), Science 296(5576),
# 2225-2229, strong-LD definitions p. 2226
# (fetched-wave3/The structure of haplotype blocks in the human
# genome.pdf).  Mirrors Python morie.fn.hapblk exactly (same grid
# profile-likelihood CI).

#' .hapblk_ci
#'
#' A step of the hapblk_native implementation. Called by \code{morie_hapblk}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param h A vector; indexed elementwise.
#' @param grid Numeric; combined arithmetically in the body. Defaults to \code{200}.
#' @return A vector, from \code{c}.
#' @export
.hapblk_ci <- function(h, grid = 200) {
  n <- sum(h)
  if (n == 0) return(c(0, 0, 0))
  pA <- (h[1] + h[2]) / n
  pB <- (h[1] + h[3]) / n
  if (pA %in% c(0, 1) || pB %in% c(0, 1)) return(c(0, 0, 0))
  p00 <- h[1] / n
  D <- p00 - pA * pB
  dmax <- if (D > 0) min(pA * (1 - pB), (1 - pA) * pB) else
    min(pA * pB, (1 - pA) * (1 - pB))
  if (dmax <= 0) return(c(0, 0, 0))
  dprime <- abs(D) / dmax
  sgn <- if (D >= 0) 1 else -1
  gs <- 0:grid
  logl <- vapply(gs, function(g) {
    dp <- g / grid
    Dg <- sgn * dp * dmax
    p <- c(pA * pB + Dg, pA * (1 - pB) - Dg,
           (1 - pA) * pB - Dg, (1 - pA) * (1 - pB) + Dg)
    if (any(p < -1e-12)) return(-1e18)
    sum(h * log(pmax(p, 1e-12)))
  }, numeric(1))
  w <- exp(logl - max(logl))
  cdf <- cumsum(w / sum(w))
  lo <- gs[which(cdf >= 0.05)[1]] / grid
  hi <- gs[which(cdf >= 0.95)[1]] / grid
  c(dprime, lo, hi)
}

#' Gabriel et al. (2002) haplotype blocks
#'
#' Pairs are strong-LD when the upper 95% profile-likelihood bound
#' on D' exceeds 0.98 and the lower bound exceeds 0.7; strong
#' recombination when the upper bound is below 0.9; blocks are
#' maximal spans with >= 95% strong-LD informative pairs.
#'
#' @param H Binary haplotype matrix (n x m).
#' @param strong_hi,strong_lo,recomb_hi Paper thresholds.
#' @param frac Required strong-LD fraction (0.95).
#' @return A list with elements \code{blocks} (list of inclusive
#'   0-based (start, end)), \code{dprime}, \code{ci_lo},
#'   \code{ci_hi}, \code{pair_class}, \code{method}.
#' @references Gabriel, S. B. et al. (2002). The structure of
#'   haplotype blocks in the human genome. Science, 296(5576),
#'   2225-2229.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' morie_hapblk(V)
morie_hapblk <- function(H, strong_hi = 0.98, strong_lo = 0.70,
                         recomb_hi = 0.90, frac = 0.95) {
  H <- as.matrix(H)
  n <- nrow(H)
  m <- ncol(H)
  if (n < 4) stop("need >= 4 haplotypes")
  dp <- lo_m <- hi_m <- matrix(0, m, m)
  cls <- matrix("", m, m)
  for (a in seq_len(m - 1)) {
    for (b in (a + 1):m) {
      h <- c(sum(H[, a] == 0 & H[, b] == 0),
             sum(H[, a] == 0 & H[, b] == 1),
             sum(H[, a] == 1 & H[, b] == 0),
             sum(H[, a] == 1 & H[, b] == 1))
      ci <- .hapblk_ci(h)
      dp[a, b] <- dp[b, a] <- ci[1]
      lo_m[a, b] <- lo_m[b, a] <- ci[2]
      hi_m[a, b] <- hi_m[b, a] <- ci[3]
      c_ <- if (ci[3] > strong_hi && ci[2] > strong_lo) "S"
        else if (ci[3] < recomb_hi) "R" else "U"
      cls[a, b] <- cls[b, a] <- c_
    }
  }
  blocks <- list()
  start <- 1
  while (start < m) {
    best_end <- -1
    for (end in m:(start + 1)) {
      ns <- nr <- 0
      for (a in start:(end - 1)) {
        for (b in (a + 1):end) {
          if (cls[a, b] == "S") ns <- ns + 1
          else if (cls[a, b] == "R") nr <- nr + 1
        }
      }
      inf <- ns + nr
      if (inf > 0 && ns / inf >= frac) {
        best_end <- end
        break
      }
    }
    if (best_end > start) {
      blocks[[length(blocks) + 1]] <- c(start - 1, best_end - 1)
      start <- best_end + 1
    } else {
      start <- start + 1
    }
  }
  list(blocks = blocks, dprime = dp, ci_lo = lo_m, ci_hi = hi_m,
       pair_class = cls,
       method = "Gabriel et al. (2002) confidence-bound blocks")
}
