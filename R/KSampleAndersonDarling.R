.ksa_solve3 <- function(S, r) as.vector(solve(S, r))

#' K-sample Anderson-Darling test
#'
#' R arm of \code{morie.fn.ksamp}: the Scholz-Stephens (1987) k-sample
#' Anderson-Darling statistic in its midrank (ties) form \eqn{A^2_{akN}},
#' standardised \eqn{T = (A^2_{akN} - (k - 1))/\sigma_N} with the exact
#' finite-sample variance, and the p-value interpolated as in scipy: a
#' least-squares quadratic of the log significance levels in the Scholz-
#' Stephens critical values, clipped to the range 0.001 to 0.25.
#'
#' @param ... Two or more numeric samples.
#' @return List with \code{statistic} (T), \code{p_value}, \code{A2akN},
#'   \code{critical_values}, \code{k}, \code{sample_sizes}.
#' @references Scholz, F. W. and Stephens, M. A. (1987). K-sample
#'   Anderson-Darling tests. Journal of the American Statistical Association
#'   82, 918-924.
#' @examples
#' Ksamp(c(0.1, 1.2, 0.5, 2.2, 1.9), c(3.1, 2.4, 4.2, 3.3, 2.9))$statistic
#' @export
Ksamp <- function(...) {
  gs <- lapply(list(...), function(g) sort(as.numeric(g)))
  k <- length(gs)
  if (k < 2) stop("Need at least 2 samples.")
  ns <- lengths(gs)
  allv <- sort(unlist(gs))
  n <- length(allv)
  zs <- sort(unique(allv))
  lj <- vapply(zs, function(z) sum(allv == z), 0)
  bj <- vapply(zs, function(z) sum(allv < z), 0) + lj / 2
  a2 <- 0
  for (i in seq_len(k)) {
    fij <- vapply(zs, function(z) sum(gs[[i]] == z), 0)
    mij <- vapply(zs, function(z) sum(gs[[i]] <= z), 0) - fij / 2
    den <- bj * (n - bj) - n * lj / 4
    ok <- den > 0
    a2 <- a2 + sum((lj / n * (n * mij - bj * ns[i])^2 / den)[ok]) / ns[i]
  }
  A2 <- a2 * (n - 1) / n - (k - 1)
  H <- sum(1 / ns)
  hs <- sum(1 / seq_len(n - 1))
  gsum <- 0
  for (i in seq_len(n - 2)) gsum <- gsum + sum(1 / ((n - i) * ((i + 1):(n - 1))))
  a <- (4 * gsum - 6) * (k - 1) + (10 - 6 * gsum) * H
  b <- (2 * gsum - 4) * k^2 + 8 * hs * k + (2 * gsum - 14 * hs - 4) * H - 8 * hs + 4 * gsum - 6
  cc <- (6 * hs + 2 * gsum - 2) * k^2 + (4 * hs - 4 * gsum + 6) * k + (2 * hs - 6) * H + 4 * hs
  d <- (2 * hs + 6) * k^2 - 4 * hs * k
  sigsq <- (a * n^3 + b * n^2 + cc * n + d) / ((n - 1) * (n - 2) * (n - 3))
  tn <- A2 / sqrt(sigsq)
  m <- k - 1
  tm <- c(0.675, 1.281, 1.645, 1.960, 2.326, 2.573, 3.085) + c(-0.245, 0.250, 0.678, 1.149, 1.822, 2.364, 3.615) / sqrt(m) +
    c(-0.105, -0.305, -0.362, -0.391, -0.396, -0.345, -0.154) / m
  ls <- log(c(0.25, 0.10, 0.05, 0.025, 0.01, 0.005, 0.001))
  p <- if (tn < tm[1]) 0.25 else if (tn > tm[7]) 0.001 else {
    S <- outer(0:2, 0:2, Vectorize(function(i, j) sum(tm^(i + j))))
    co <- .ksa_solve3(S, vapply(0:2, function(i) sum(ls * tm^i), 0))
    exp(co[1] + co[2] * tn + co[3] * tn^2)
  }
  list(statistic = tn, p_value = p, A2akN = A2, critical_values = tm, k = k, sample_sizes = ns)
}
