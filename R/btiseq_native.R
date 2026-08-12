# Iterated prepivoted bootstrap test.
# Source: Beran (1988), JASA 83(403), 687-697, Secs. 1-2
# (fetched-wave3/Prepivoting_Test_Statistics_A_Bootstrap_View_of_
# Asymptotic_Refinements.pdf).  Mirrors Python morie.fn.btiseq
# exactly (identical SplitMix64 index stream).

#' Beran prepivoted and twice-prepivoted bootstrap mean test
#'
#' p_B = 1 - H_n(T_n) with H_n the bootstrap null cdf of the
#' studentized statistic (null resampling from x - xbar + mu0);
#' p_B1 is the fraction of outer resamples whose inner-prepivoted
#' statistic H*_b(T*_b) is at least H_n(T_n) (double prepivoting).
#'
#' @param x Numeric sample.
#' @param mu0 Null mean value.
#' @param B_outer,B_inner Bootstrap sizes.
#' @param seed SplitMix64 seed (mirrors the Python arm).
#' @return A list with elements \code{statistic}, \code{p_boot},
#'   \code{p_iterated}, \code{prepivoted_value}, \code{mu0},
#'   \code{B_outer}, \code{B_inner}, \code{seed}, \code{method}.
#' @references Beran, R. (1988). Prepivoting test statistics: a
#'   bootstrap view of asymptotic refinements. JASA, 83(403),
#'   687-697.
#' @export
morie_btiseq <- function(x, mu0 = 0, B_outer = 300, B_inner = 150,
                         seed = 0) {
  xv <- as.numeric(x)
  n <- length(xv)
  if (n < 5) stop("need at least five observations")
  e <- .ghc_rng(seed)
  tstat <- function(s, center) {
    m <- mean(s)
    sd_ <- sqrt(sum((s - m)^2) / (n - 1))
    if (sd_ <= 0) return(0)
    sqrt(n) * abs(m - center) / sd_
  }
  resample <- function(base) {
    idx <- pmin(floor(.ghc_unif(e, n) * n), n - 1) + 1
    base[idx]
  }
  t_obs <- tstat(xv, mu0)
  null_x <- xv - mean(xv) + mu0
  outer_t <- numeric(B_outer)
  prepiv <- numeric(B_outer)
  for (b in seq_len(B_outer)) {
    xb <- resample(null_x)
    tb <- tstat(xb, mu0)
    outer_t[b] <- tb
    null_b <- xb - mean(xb) + mu0
    le <- 0L
    for (cc in seq_len(B_inner)) {
      xc <- resample(null_b)
      if (tstat(xc, mu0) <= tb) le <- le + 1L
    }
    prepiv[b] <- le / B_inner
  }
  h_obs <- sum(outer_t <= t_obs) / B_outer
  list(statistic = t_obs,
       p_boot = 1 - h_obs,
       p_iterated = sum(prepiv >= h_obs) / B_outer,
       prepivoted_value = h_obs,
       mu0 = mu0, B_outer = as.integer(B_outer),
       B_inner = as.integer(B_inner), seed = seed,
       method = "Beran (1988) prepivoted / twice-prepivoted test")
}
