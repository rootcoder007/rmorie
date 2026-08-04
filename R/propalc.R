# SPDX-License-Identifier: AGPL-3.0-or-later
#' Allocate n units to strata in proportion to stratum size.
#'
#' Largest-remainder apportionment is used rather than rounding each n_h
#' independently, because rounding can miss the target total by several
#' units; ties break on the lowest stratum index so the two language arms
#' agree exactly.
#'
#' Formula: n_h = n W_h = n N_h / N, apportioned by largest remainder
#'
#' @param Nh Population size of each stratum.
#' @param n Total sample size to allocate.
#' @return List with \code{nh}, \code{nh_exact}, \code{Wh},
#'   \code{fraction}, \code{N}, \code{n}, \code{L}.
#' @references Cochran (1977), Sampling Techniques, 3rd edition, Section
#'   5.3, Corollary 2, "stratification with proportional allocation of
#'   the n_h", n_h/n = N_h/N. Chapter 5 read from the scanned original.
#'   Cross-checked against the reference implementation in the CRAN
#'   package samplingbook 1.2.4, whose stratasamp(type = "prop") sets
#'   wh <- Nh/N.
#' @export
Propalloc <- function(Nh, n) {
  Nh <- .t1_vec(Nh); n <- as.integer(n)
  L <- length(Nh)
  if (L < 1L) stop("at least one stratum is required")
  if (any(Nh <= 0)) stop("stratum sizes must be positive")
  if (n < 0L) stop("n must be non-negative")
  N <- sum(Nh); W <- Nh / N
  exact <- n * W
  base <- as.integer(floor(exact))
  rem <- n - sum(base)
  ord <- order(-(exact - base), seq_len(L))
  if (rem > 0) base[ord[seq_len(rem)]] <- base[ord[seq_len(rem)]] + 1L
  .t1_result(nh = base, nh_exact = exact, Wh = W, fraction = n / N,
             N = N, n = n, L = L,
             method = "Proportional allocation (largest remainder)")
}
