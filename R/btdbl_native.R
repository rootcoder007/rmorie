# Double-bootstrap prepivoted confidence interval.
# Source: Beran (1987), Biometrika 74(3), 457-468, Eqs. 2.6-2.7 and
# the Monte Carlo algorithm of Eqs. 2.11-2.12
# (fetched-wave3/Prepivoting_to_reduce_level_error_of_confidence_
# sets..pdf).  Mirrors Python morie.fn.btdbl exactly: identical
# SplitMix64 uniform stream drives every resampling index.

#' Beran prepivoted double-bootstrap confidence interval
#'
#' Root R = sqrt(n)|t(X*) - t(X)|; inner bootstraps give the
#' prepivoted values u_b = H*_b(R_b); c1 is their (1-alpha)
#' quantile; the critical root is the c1 quantile of the outer root
#' distribution (Beran Eq. 2.7 two-step algorithm).
#'
#' @param x Numeric sample.
#' @param statistic Function(x) -> scalar (default mean).
#' @param alpha 1 - confidence level.
#' @param B_outer,B_inner Bootstrap sizes.
#' @param seed SplitMix64 seed (mirrors the Python arm).
#' @return A list with elements \code{estimate}, \code{lower},
#'   \code{upper}, \code{critical_root}, \code{c_level},
#'   \code{alpha}, \code{B_outer}, \code{B_inner}, \code{seed},
#'   \code{method}.
#' @references Beran, R. (1987). Prepivoting to reduce level error
#'   of confidence sets. Biometrika, 74(3), 457-468.
#' @export
morie_btdbl <- function(x, statistic = NULL, alpha = 0.05,
                        B_outer = 400, B_inner = 200, seed = 0) {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 5) stop("need at least five observations")
  if (is.null(statistic)) statistic <- mean
  if (alpha <= 0 || alpha >= 1) stop("alpha must be in (0, 1)")
  e <- .ghc_rng(seed)
  that <- as.numeric(statistic(xv))
  sqn <- sqrt(n)
  resample <- function(base) {
    idx <- pmin(floor(.ghc_unif(e, n) * n), n - 1) + 1
    base[idx]
  }
  outer_roots <- numeric(B_outer)
  prepiv <- numeric(B_outer)
  for (b in seq_len(B_outer)) {
    xb <- resample(xv)
    tb <- as.numeric(statistic(xb))
    rb <- sqn * abs(tb - that)
    outer_roots[b] <- rb
    le <- 0L
    for (cc in seq_len(B_inner)) {
      xc <- resample(xb)
      tc <- as.numeric(statistic(xc))
      if (sqn * abs(tc - tb) <= rb) le <- le + 1L
    }
    prepiv[b] <- le / B_inner
  }
  sp <- sort(prepiv)
  idx <- max(min(ceiling((1 - alpha) * B_outer), B_outer), 1)
  c1 <- sp[idx]
  so <- sort(outer_roots)
  j <- max(min(ceiling(c1 * B_outer), B_outer), 1)
  crit <- so[j]
  half <- crit / sqn
  list(estimate = that, lower = that - half, upper = that + half,
       critical_root = crit, c_level = c1, alpha = alpha,
       B_outer = as.integer(B_outer), B_inner = as.integer(B_inner),
       seed = seed,
       method = "Beran (1987) prepivoted double bootstrap (Eq. 2.7)")
}
