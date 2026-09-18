# SPDX-License-Identifier: AGPL-3.0-or-later
#' Gaussian elimination with partial pivoting on a small dense system (internal)
#'
#' Written out rather than routed through \code{.t1_lstsq}: that helper is
#' an SVD and the Python arm's is a modified Gram-Schmidt QR, and two
#' different factorisations of the same design do not agree to the last
#' digits. IK's algorithm is itself in normal-equation form, so this is
#' the arithmetic the paper prescribes, and the identical loop in both
#' arms makes the two agree exactly.
#'
#' @param A Coefficient matrix.
#' @param b Right-hand side.
#' @return Solution vector.
#' @keywords internal
.ik_gesolve <- function(A, b) {
  n <- length(b)
  M <- cbind(as.matrix(A), b)
  for (k in seq_len(n)) {
    piv <- k
    best <- abs(M[k, k])
    if (k < n) for (i in (k + 1L):n) if (abs(M[i, k]) > best) { best <- abs(M[i, k])
    piv <- i }
    if (best < 1e-300)
      stop("mse_optimal_bandwidth_rdd: singular design in a pilot regression")
    if (piv != k) { tmp <- M[k, ]
    M[k, ] <- M[piv, ]
    M[piv, ] <- tmp }
    pk <- M[k, k]
    if (k < n) for (i in (k + 1L):n) {
      f <- M[i, k] / pk
      if (f != 0) for (j in k:(n + 1L)) M[i, j] <- M[i, j] - f * M[k, j]
    }
  }
  x <- numeric(n)
  for (i in n:1L) {
    s <- M[i, n + 1L]
    if (i < n) for (j in (i + 1L):n) s <- s - M[i, j] * x[j]
    x[i] <- s / M[i, i]
  }
  x
}

#' Least squares through the normal equations, small p (internal)
#'
#' @param rows Design matrix.
#' @param y Response.
#' @return Coefficient vector.
#' @keywords internal
#' @examples
#' set.seed(1)
#' r <- rmorie:::.ik_ols(rows = rnorm(10), y = rnorm(10)); TRUE
.ik_ols <- function(rows, y) {
  rows <- as.matrix(rows)
  y <- as.numeric(y)
  p <- ncol(rows)
  A <- matrix(0, p, p)
  b <- numeric(p)
  for (r in seq_len(nrow(rows))) {
    ri <- rows[r, ]
    yi <- y[r]
    for (i in seq_len(p)) {
      b[i] <- b[i] + ri[i] * yi
      for (j in seq_len(p)) A[i, j] <- A[i, j] + ri[i] * ri[j]
    }
  }
  .ik_gesolve(A, b)
}

#' Median with the even case averaged (internal)
#'
#' @param v Numeric vector.
#' @return The median, IK (2012) p.9 convention.
#' @keywords internal
#' @examples
#' set.seed(1)
#' r <- rmorie:::.ik_median(v = rnorm(10)); TRUE
.ik_median <- function(v) {
  s <- sort(as.numeric(v))
  m <- length(s)
  if (m == 0L) stop("mse_optimal_bandwidth_rdd: median of an empty side")
  h <- m %/% 2L
  if (m %% 2L == 1L) s[h + 1L] else 0.5 * (s[h] + s[h + 1L])
}

#' Step 3 of Imbens-Kalyanaraman (2012): combine the plug-ins (internal)
#'
#' Formula, IK (2012) eq. (4.7) p.8:
#' \code{h_opt = C_K * ((2 sigma2 / f) / ((m2p - m2m)^2 + rp + rm))^(1/5) * N^(-1/5)}.
#'
#' Exposed separately so it can be checked against the paper's own worked
#' example (pp.15-16) without re-running the plug-in steps.
#'
#' @param sigma2 Conditional variance at the cutoff.
#' @param f_hat Density of the running variable at the cutoff.
#' @param m2_plus,m2_minus Curvatures either side.
#' @param r_plus,r_minus Regularisation terms.
#' @param n Sample size.
#' @param ck Kernel constant.
#' @return The bandwidth.
#' @keywords internal
.ik_hopt <- function(sigma2, f_hat, m2_plus, m2_minus, r_plus, r_minus, n, ck = 3.4375) {
  denom <- (m2_plus - m2_minus)^2 + (r_plus + r_minus)
  if (f_hat <= 0 || denom <= 0 || n <= 0)
    stop("mse_optimal_bandwidth_rdd: degenerate bandwidth criterion")
  ck * ((2 * sigma2 / f_hat) / denom)^0.2 * n^-0.2
}

#' Plug-in MSE-optimal bandwidth for a sharp RD design
#'
#' A local-linear RD estimator trades squared bias, which grows with the
#' window, against variance, which shrinks with it. IK's selector is the
#' minimiser of that sum with the six unknown population quantities
#' replaced by plug-ins, plus a regularisation term that keeps the answer
#' finite when the two curvatures happen to cancel -- without it the
#' criterion has a pole at \code{m2_plus == m2_minus} and the selected
#' window runs away to the whole support.
#'
#' The three steps are the paper's own (sec. 4.2, pp.9-10): a
#' Silverman-type pilot \code{h1 = 1.84 * S_X * N^(-1/5)} giving
#' \code{f(c)} and \code{sigma^2(c)}; a global cubic with a jump giving
#' \code{m3} and hence the pilot bandwidths of eq. (4.11), then a local
#' quadratic on each for the curvatures; and the regularisation terms of
#' eq. (4.12) fed into eq. (4.7).
#'
#' Note on eq. (4.8). The density estimator is printed on p.9 as
#' \code{(N_h1- + N_h1+) / (N * h1)}, but the paper's own worked example
#' on p.15 evaluates \code{(836 + 862) / (2 * 6558 * 0.1445) = 0.8962}.
#' The factor two is required -- the window has width \code{2 * h1} --
#' and the printed eq. (4.8) is missing it. This implementation follows
#' the worked example, which reproduces the paper's reported numbers;
#' without the two the density comes out at 1.79 and the reported
#' \code{h_opt = 0.2649} is not recoverable.
#'
#' @param y Outcome.
#' @param x Running variable, same length as \code{y}.
#' @param cutoff Threshold \code{c}.
#' @param kernel_constant \code{C_K}; 3.4375 is the edge (triangular)
#'   kernel value stated on p.10, 5.4 the uniform-kernel value.
#' @return List with \code{estimate} and \code{h_opt}, \code{h_no_reg},
#'   \code{h1}, \code{f_hat}, \code{sigma2}, \code{m3}, \code{h2_plus},
#'   \code{h2_minus}, \code{m2_plus}, \code{m2_minus}, \code{r_plus},
#'   \code{r_minus}, \code{n_plus}, \code{n_minus}, \code{n2_plus},
#'   \code{n2_minus}, \code{n1_plus}, \code{n1_minus}, \code{n}, \code{ck}.
#' @references Imbens, G. & Kalyanaraman, K. (2012). Optimal bandwidth
#'   choice for the regression discontinuity estimator. Review of
#'   Economic Studies 79(3):933-959. \doi{10.1093/restud/rdr043}.
#'   Equations and worked example read from the NBER working paper w14726
#'   version, pp.8-10 and pp.15-16.
#' @export
Rdmcbw <- function(y, x, cutoff = 0, kernel_constant = 3.4375) {
  y <- as.numeric(unlist(y))
  x <- as.numeric(unlist(x))
  n <- length(y)
  if (n == 0L) stop("mse_optimal_bandwidth_rdd: y is empty")
  if (length(x) != n) stop("mse_optimal_bandwidth_rdd: x must have one entry per observation")
  c0 <- as.numeric(cutoff)
  ck <- as.numeric(kernel_constant)
  r <- x - c0

  sx <- stats::sd(x)
  h1 <- 1.84 * sx * n^-0.2
  if (!is.finite(h1) || h1 <= 0) stop("mse_optimal_bandwidth_rdd: the running variable is constant")
  ip <- which(r >= 0 & r <= h1)
  im <- which(r >= -h1 & r < 0)
  n1p <- length(ip)
  n1m <- length(im)
  if (n1p < 2L || n1m < 2L)
    stop("mse_optimal_bandwidth_rdd: too few points inside the pilot window h1")
  ybp <- sum(y[ip]) / n1p
  ybm <- sum(y[im]) / n1m
  f_hat <- (n1p + n1m) / (2 * n * h1)
  ss <- sum((y[ip] - ybp)^2) + sum((y[im] - ybm)^2)
  sigma2 <- ss / (n1p + n1m)
  if (sigma2 <= 0) stop("mse_optimal_bandwidth_rdd: zero residual variance at the cutoff")

  right <- which(r >= 0)
  left <- which(r < 0)
  n_plus <- length(right)
  n_minus <- length(left)
  if (n_plus < 4L || n_minus < 4L)
    stop("mse_optimal_bandwidth_rdd: each side needs at least four observations")
  med_p <- .ik_median(x[right])
  med_m <- .ik_median(x[left])
  keep <- which(x >= med_m & x <= med_p)
  if (length(keep) < 5L) stop("mse_optimal_bandwidth_rdd: too few points for the global cubic")
  rows <- cbind(1, ifelse(r[keep] >= 0, 1, 0), r[keep], r[keep]^2, r[keep]^3)
  g <- .ik_ols(rows, y[keep])
  m3 <- 6 * g[5]

  base <- (sigma2 / (f_hat * max(m3 * m3, 0.01)))^(1 / 7)
  h2p <- 3.56 * base * n_plus^(-1 / 7)
  h2m <- 3.56 * base * n_minus^(-1 / 7)

  .curv <- function(idx, who) {
    if (length(idx) < 3L)
      stop(paste0("mse_optimal_bandwidth_rdd: too few points for the ", who, " local quadratic"))
    q <- .ik_ols(cbind(1, r[idx], r[idx]^2), y[idx])
    2 * q[3]
  }
  i2p <- which(r >= 0 & r <= h2p)
  i2m <- which(r >= -h2m & r < 0)
  n2p <- length(i2p)
  n2m <- length(i2m)
  m2p <- .curv(i2p, "right")
  m2m <- .curv(i2m, "left")

  r_plus <- 720 * sigma2 / (n2p * h2p^4)
  r_minus <- 720 * sigma2 / (n2m * h2m^4)
  h_opt <- .ik_hopt(sigma2, f_hat, m2p, m2m, r_plus, r_minus, n, ck)
  h_no_reg <- .ik_hopt(sigma2, f_hat, m2p, m2m, 0, 0, n, ck)

  .t1_result(estimate = h_opt, h_opt = h_opt, h_no_reg = h_no_reg, h1 = h1,
             f_hat = f_hat, sigma2 = sigma2, m3 = m3,
             h2_plus = h2p, h2_minus = h2m, m2_plus = m2p, m2_minus = m2m,
             r_plus = r_plus, r_minus = r_minus,
             n_plus = n_plus, n_minus = n_minus, n2_plus = n2p, n2_minus = n2m,
             n1_plus = n1p, n1_minus = n1m, n = n, ck = ck,
             method = "IK (2012) MSE-optimal RDD bandwidth")
}
