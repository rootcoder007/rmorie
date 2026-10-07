#' Resampling for spatially dependent data
#'
#' \code{BlockBootstrapGrid}: moving-block bootstrap of a grid from
#' overlapping \code{block} by \code{block} blocks (Hall 1985; Lahiri 2003).
#' \code{StationaryBootstrap}: Politis-Romano stationary bootstrap with
#' geometric block lengths of mean \eqn{1/p} on the circularly wrapped
#' series. \code{BootstrapBands}: pointwise percentile and simultaneous
#' (max-|t|) bands from replicate fields. \code{ToroidalShiftTest}:
#' correlation of two maps against all (or random) toroidal shifts of the
#' second. Identical to the Python arm \code{morie.fn.spboot}; indices are
#' 1-based here.
#'
#' @param grid Matrix.
#' @param block Block side.
#' @param nboot Replicates.
#' @param seed Philox seed.
#' @param statistic Function of a vector (default \code{mean}).
#' @param alpha Level.
#' @param x Series.
#' @param p Block restart probability.
#' @param replicates Matrix of replicate fields (rows).
#' @param estimate Centre of the bands.
#' @param a,b Maps (matrices).
#' @param exact Use all shifts.
#' @param nshift Random shifts.
#' @return List.
#' @references Lahiri, S. N. (2003). Resampling Methods for Dependent Data.
#'   Springer.
#'
#'   Politis, D. N. and Romano, J. P. (1994). The stationary bootstrap. JASA
#'   89, 1303-1313.
#'
#'   Upton, G. J. G. and Fingleton, B. (1985). Spatial Data Analysis by
#'   Example, Vol. 1. Wiley.
#' @examples
#' ToroidalShiftTest(matrix(1:4, 2, byrow = TRUE), matrix(1:4, 2, byrow = TRUE))$statistic
#' @export
BlockBootstrapGrid <- function(grid, block, nboot = 200L, seed = 1, statistic = mean, alpha = 0.05) {
  .morie_arg(grid, "m")
  G <- as.matrix(grid) * 1
  nr <- nrow(G)
  nc <- ncol(G)
  pr <- nr - block + 1
  pc <- nc - block + 1
  br <- ceiling(nr / block)
  bc <- ceiling(nc / block)
  reps <- vapply(seq_len(nboot) - 1, function(r) {
    u <- .morie_random_uniform(2 * br * bc, seed = seed, stream = r)
    out <- matrix(0, nr, nc)
    k <- 0
    for (bi in seq_len(br) - 1) {
      for (bj in seq_len(bc) - 1) {
        i0 <- min(floor(u[2 * k + 1] * pr), pr - 1)
        j0 <- min(floor(u[2 * k + 2] * pc), pc - 1)
        k <- k + 1
        for (a in seq_len(block) - 1) {
          for (b in seq_len(block) - 1) {
            x <- bi * block + a
            y <- bj * block + b
            if (x < nr && y < nc) out[x + 1, y + 1] <- G[i0 + a + 1, j0 + b + 1]
          }
        }
      }
    }
    statistic(as.vector(t(out)))
  }, 0)
  list(replicates = reps, se = stats::sd(reps), ci = unname(stats::quantile(reps, c(alpha / 2, 1 - alpha / 2))),
       estimate = statistic(as.vector(t(G))))
}

#' @rdname BlockBootstrapGrid
#' @export
StationaryBootstrap <- function(x, p, nboot = 200L, seed = 1, statistic = mean, alpha = 0.05) {
  n <- length(x)
  res <- lapply(seq_len(nboot) - 1, function(r) {
    u <- .morie_random_uniform(2 * n, seed = seed, stream = r)
    ix <- integer(n)
    cur <- min(floor(u[1] * n), n - 1)
    for (t in seq_len(n) - 1) {
      if (t > 0) cur <- if (u[2 * t + 1] < p) min(floor(u[2 * t + 2] * n), n - 1) else (cur + 1) %% n
      ix[t + 1] <- cur + 1
    }
    list(ix = ix, s = statistic(x[ix]))
  })
  reps <- vapply(res, `[[`, 0, "s")
  list(replicates = reps, se = stats::sd(reps), ci = unname(stats::quantile(reps, c(alpha / 2, 1 - alpha / 2))),
       indices = lapply(res, `[[`, "ix"), estimate = statistic(x))
}

#' @rdname BlockBootstrapGrid
#' @export
BootstrapBands <- function(replicates, estimate = NULL, alpha = 0.05) {
  R <- as.matrix(replicates)
  est <- if (is.null(estimate)) colMeans(R) else estimate
  sd <- apply(R, 2, stats::sd)
  lo <- apply(R, 2, stats::quantile, alpha / 2, names = FALSE)
  hi <- apply(R, 2, stats::quantile, 1 - alpha / 2, names = FALSE)
  ok <- sd > 0
  tmax <- if (any(ok)) apply(R, 1, function(r) max(abs(r[ok] - est[ok]) / sd[ok])) else rep(0, nrow(R))
  cc <- unname(stats::quantile(tmax, 1 - alpha))
  list(estimate = est, pointwise_lower = lo, pointwise_upper = hi, simultaneous_lower = est - cc * sd,
       simultaneous_upper = est + cc * sd, critical_value = cc)
}

#' @rdname BlockBootstrapGrid
#' @export
ToroidalShiftTest <- function(a, b, exact = TRUE, nshift = 199L, seed = 1) {
  .morie_arg(a, "m")
  A <- as.matrix(a) * 1
  B <- as.matrix(b) * 1
  nr <- nrow(A)
  nc <- ncol(A)
  fa <- as.vector(t(A))
  shift <- function(di, dj) B[((seq_len(nr) - 1 + di) %% nr) + 1, ((seq_len(nc) - 1 + dj) %% nc) + 1, drop = FALSE]
  r0 <- stats::cor(fa, as.vector(t(B)))
  sh <- if (exact) {
    g <- expand.grid(dj = 0:(nc - 1), di = 0:(nr - 1))
    g[g$di != 0 | g$dj != 0, c("di", "dj")]
  } else {
    u <- .morie_random_uniform(2 * nshift, seed = seed, stream = 0)
    data.frame(di = pmin(floor(u[2 * seq_len(nshift) - 1] * nr), nr - 1), dj = pmin(floor(u[2 * seq_len(nshift)] * nc), nc - 1))
  }
  sims <- vapply(seq_len(nrow(sh)), function(k) stats::cor(fa, as.vector(t(shift(sh$di[k], sh$dj[k])))), 0)
  list(statistic = r0, simulated = sims, n_shifts = length(sims),
       p_value = (1 + sum(abs(sims) >= abs(r0) - 1e-12)) / (1 + length(sims)))
}
