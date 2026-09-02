# SPDX-License-Identifier: AGPL-3.0-or-later
#' Join-count statistics for binary spatial data (Cliff and Ord 1981)
#'
#' Source FETCHED (reference implementation): \code{spdep::joincount.test}
#' (Bivand, spdep 1.4-2, \code{R/jc.R}), whose comments cite Cliff, A. D.
#' and Ord, J. K. (1981), Spatial Processes: Models and Applications,
#' Pion, page 20, equations (1.31) and (1.32) for nonfree sampling:
#' \code{E\[J_kk\] = S0 n_k (n_k - 1) / (2 N (N-1))} and
#' \code{4 Var + 4 E^2 = S1 n_k(n_k-1)/(N(N-1))
#' + (S2 - 2 S1) n_k(n_k-1)(n_k-2)/(N(N-1)(N-2))
#' + (S0^2 + S1 - S2) n_k(n_k-1)(n_k-2)(n_k-3)/(N(N-1)(N-2)(N-3))},
#' with \code{S0 = sum w_ij}, \code{S1 = 0.5 sum (w_ij + w_ji)^2} and
#' \code{S2 = sum_i (rowsum_i + colsum_i)^2}.  The variance is defined
#' only for \code{n_k >= 4}; NaN is returned below that.
#'
#' @param x Binary vector of length N coded 0/1.
#' @param W N x N spatial weights matrix; the diagonal is ignored.
#' @return list: BB, WW, BW, E_BB, E_WW, V_BB, V_WW, z_BB, z_WW, p_BB,
#'   p_WW, S0, S1, S2, n, method.
#' @examples
#' W <- matrix(0, 8, 8)
#' for (i in 1:7) {
#'   W[i, i + 1] <- 1
#'   W[i + 1, i] <- 1
#' }
#' Joincnt(c(1, 1, 1, 1, 0, 0, 0, 0), W)$BB
#' @export
Joincnt <- function(x, W) {
  x <- as.numeric(x)
  N <- length(x)
  W <- as.matrix(W)
  if (!identical(dim(W), c(N, N))) stop("W must be N x N with N = length(x)")
  diag(W) <- 0
  if (!all(x == 0 | x == 1)) stop("x must be coded 0/1")
  s0 <- sum(W)
  s1 <- 0.5 * sum((W + t(W))^2)
  s2 <- sum((rowSums(W) + colSums(W))^2)
  z0 <- 1 - x
  bb <- 0.5 * as.numeric(t(x) %*% W %*% x)
  ww <- 0.5 * as.numeric(t(z0) %*% W %*% z0)
  d <- outer(x, x, "-")
  bw <- 0.5 * sum(W * d * d)
  mom <- function(nk) {
    if (nk < 4 || N < 4) {
      return(c(NaN, NaN))
    }
    d1 <- N - 1
    d2 <- N - 2
    d3 <- N - 3
    a2 <- nk * (nk - 1)
    a3 <- a2 * (nk - 2)
    a4 <- a3 * (nk - 3)
    e <- s0 * a2 / (2 * N * d1)
    v <- s1 * a2 / (N * d1)
    v <- v + (s2 - 2 * s1) * a3 / (N * d1 * d2)
    v <- v + (s0^2 + s1 - s2) * a4 / (N * d1 * d2 * d3)
    c(e, 0.25 * v - e^2)
  }
  n1 <- sum(x)
  m1 <- mom(n1)
  m0 <- mom(N - n1)
  zb <- if (!is.nan(m1[2]) && m1[2] > 0) (bb - m1[1]) / sqrt(m1[2]) else NaN
  zw <- if (!is.nan(m0[2]) && m0[2] > 0) (ww - m0[1]) / sqrt(m0[2]) else NaN
  list(
    BB = bb, WW = ww, BW = bw,
    E_BB = m1[1], E_WW = m0[1], V_BB = m1[2], V_WW = m0[2],
    z_BB = zb, z_WW = zw,
    p_BB = if (is.nan(zb)) NaN else stats::pnorm(zb, lower.tail = FALSE),
    p_WW = if (is.nan(zw)) NaN else stats::pnorm(zw, lower.tail = FALSE),
    S0 = s0, S1 = s1, S2 = s2, n = N,
    method = paste(
      "Join-count statistics, nonfree sampling",
      "(Cliff and Ord 1981 eqs 1.31-1.32)"
    )
  )
}
