# SPDX-License-Identifier: AGPL-3.0-or-later
#' MuZero n-step bootstrapped value targets.
#'
#' z_t = sum_{j=0}^{n-1} gamma^j u_{t+j} + gamma^n nu_{t+n}, with the
#' bootstrap term dropped past the end of the trajectory.
#'
#' @param rewards Environment rewards, one per transition.
#' @param values Search values at the same time steps.
#' @param n Bootstrap horizon, >= 1.
#' @param gamma Discount factor.
#'
#' @return List with target, T, n, gamma, mean.
#' @references Schrittwieser et al. (2020), Nature 588, 604-609;
#'   arXiv:1911.08265, Methods (training targets) and Equation (3).  Read
#'   from the ar5iv rendering of the arXiv source.
#' @export
Mznstep <- function(rewards, values, n = 5, gamma = 0.997) {
  u <- .t1_vec(rewards); v <- .t1_vec(values); T <- length(u)
  if (length(v) != T) stop("rewards and values must have the same length")
  n <- as.integer(n)
  if (n < 1L) stop("n must be at least 1")
  g <- as.numeric(gamma)
  z <- numeric(T)
  for (t in seq_len(T)) {
    s <- 0
    for (j in 0:(n - 1L)) if (t + j <= T) s <- s + g^j * u[t + j]
    if (t + n <= T) s <- s + g^n * v[t + n]
    z[t] <- s
  }
  .t1_result(target = z, T = T, n = n, gamma = g,
             mean = if (T > 0) sum(z) / T else NA_real_,
             method = "MuZero n-step value target (Schrittwieser et al. 2020)")
}
