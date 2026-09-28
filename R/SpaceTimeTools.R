.st_perm <- function(n, seed, stream) {
  u <- .morie_random_uniform(n, seed = seed, stream = stream)
  p <- seq_len(n)
  if (n > 1) {
    for (i in n:2) {
      j <- floor(u[i] * i) + 1
      tmp <- p[i]
      p[i] <- p[j]
      p[j] <- tmp
    }
  }
  p
}

.st_close <- function(D, Tt, s0, t0, perm) {
  up <- upper.tri(D)
  cs <- D <= s0
  ct <- abs(outer(Tt[perm], Tt[perm], `-`)) <= t0
  c(sum((cs & ct)[up]), sum(cs[up]), sum(ct[up]))
}

#' Space-time interaction, Mantel, geographic profiling and related tools
#'
#' \code{KnoxTest}: Knox (1964) count with Poisson and Philox-permutation
#' p-values. \code{NearRepeatTable}: banded observed/expected pair counts
#' (Townsley et al. 2003). \code{MantelTest}: \code{vegan::mantel} statistic
#' with Philox permutations. \code{GeographicProfile}: Rossmo (1995) CGT.
#' \code{AoristicWeights}: Ratcliffe (2002). \code{Kde2d}: as
#' \code{MASS::kde2d}. \code{CrossCorrelation}: as \code{stats::ccf}.
#' \code{EofAnalysis}: EOFs of a times x locations field. Identical to the
#' Python arm \code{morie.fn.sttools}.
#'
#' @param coords Coordinates.
#' @param times Event times.
#' @param s0,t0 Space and time closeness thresholds.
#' @param nsim Permutations.
#' @param seed Philox seed.
#' @param space_breaks,time_breaks Band upper limits.
#' @param D1,D2 Distance matrices.
#' @param method \code{"pearson"} or \code{"spearman"}.
#' @param crimes Crime sites.
#' @param grid Grid points.
#' @param B,f,g Rossmo buffer and exponents.
#' @param starts,ends Event windows.
#' @param breaks Time bins.
#' @param x,y Data.
#' @param h Bandwidths.
#' @param n Grid size.
#' @param lims Grid limits.
#' @param lag_max Maximum lag.
#' @param Z Times x locations field.
#' @param k Number of EOFs.
#' @return List or vector.
#' @references Knox, E. G. (1964). The detection of space-time interactions.
#'   Journal of the Royal Statistical Society C 13, 25-30.
#'
#'   Mantel, N. (1967). The detection of disease clustering and a generalized
#'   regression approach. Cancer Research 27, 209-220.
#'
#'   Ratcliffe, J. H. (2002). Aoristic signatures and the spatio-temporal
#'   analysis of high volume crime patterns. Journal of Quantitative
#'   Criminology 18, 23-43.
#' @examples
#' KnoxTest(rbind(c(0, 0), c(0.5, 0), c(5, 5), c(5.2, 5)), c(1, 2, 10, 11), 1, 2, nsim = 0)$observed
#' AoristicWeights(c(0.5, 2), c(2.5, 2), 0:3)$totals
#' @export
KnoxTest <- function(coords, times, s0, t0, nsim = 999L, seed = 1L) {
  D <- as.matrix(stats::dist(as.matrix(coords)))
  n <- nrow(D)
  r <- .st_close(D, times, s0, t0, seq_len(n))
  E <- r[2] * r[3] / (n * (n - 1) / 2)
  out <- list(observed = r[1], expected = E, n_space = r[2], n_time = r[3],
              poisson_p = if (r[1] > 0) 1 - stats::ppois(r[1] - 1, E) else 1)
  if (nsim > 0) {
    sims <- vapply(seq_len(nsim), function(s) .st_close(D, times, s0, t0, .st_perm(n, seed, s))[1], 0)
    out$mc_p <- (1 + sum(sims >= r[1])) / (nsim + 1)
  }
  out
}

#' @rdname KnoxTest
#' @export
NearRepeatTable <- function(coords, times, space_breaks, time_breaks, nsim = 99L, seed = 1L) {
  D <- as.matrix(stats::dist(as.matrix(coords)))
  n <- nrow(D)
  up <- which(upper.tri(D), arr.ind = TRUE)
  band <- function(v, br) {
    k <- findInterval(v, br, left.open = TRUE) + 1
    ifelse(k > length(br), NA, k)
  }
  tab <- function(perm) {
    a <- band(D[up], space_breaks)
    b <- band(abs(times[perm][up[, 1]] - times[perm][up[, 2]]), time_breaks)
    M <- matrix(0, length(space_breaks), length(time_breaks))
    ok <- !is.na(a) & !is.na(b)
    for (i in which(ok)) M[a[i], b[i]] <- M[a[i], b[i]] + 1
    M
  }
  obs <- tab(seq_len(n))
  if (nsim == 0) return(list(observed = obs, expected = NULL, ratio = NULL, p = NULL))
  sims <- lapply(seq_len(nsim), function(s) tab(.st_perm(n, seed, s)))
  ex <- Reduce(`+`, sims) / nsim
  pv <- (1 + Reduce(`+`, lapply(sims, function(S) (S >= obs) * 1))) / (nsim + 1)
  list(observed = obs, expected = ex, ratio = ifelse(ex > 0, obs / ex, NaN), p = pv)
}

#' @rdname KnoxTest
#' @export
MantelTest <- function(D1, D2, method = "pearson", nsim = 999L, seed = 1L) {
  if (!method %in% c("pearson", "spearman")) stop("method must be pearson or spearman")
  A <- as.matrix(D1)
  B <- as.matrix(D2)
  lt <- lower.tri(A)
  r <- stats::cor(A[lt], B[lt], method = method)
  out <- list(statistic = r)
  if (nsim > 0) {
    sims <- vapply(seq_len(nsim), function(s) {
      p <- .st_perm(nrow(A), seed, s)
      stats::cor(A[p, p][lt], B[lt], method = method)
    }, 0)
    out$pvalue <- (1 + sum(sims >= r)) / (nsim + 1)
  }
  out
}

#' @rdname KnoxTest
#' @export
GeographicProfile <- function(crimes, grid, B, f = 1.2, g = 1.2) {
  C <- as.matrix(crimes)
  G <- as.matrix(grid)
  raw <- apply(G, 1, function(q) {
    d <- abs(q[1] - C[, 1]) + abs(q[2] - C[, 2])
    sum(ifelse(d > B, 1 / d^f, B^(g - f) / (2 * B - d)^g))
  })
  raw / sum(raw)
}

#' @rdname KnoxTest
#' @export
AoristicWeights <- function(starts, ends, breaks) {
  nb <- length(breaks) - 1
  W <- t(vapply(seq_along(starts), function(i) {
    s <- starts[i]
    e <- ends[i]
    if (e == s) return(as.numeric(breaks[-length(breaks)] <= s & s < breaks[-1]))
    pmax(0, pmin(e, breaks[-1]) - pmax(s, breaks[-length(breaks)])) / (e - s)
  }, numeric(nb)))
  W <- matrix(W, length(starts))
  list(weights = W, totals = colSums(W))
}

#' @rdname KnoxTest
#' @export
Kde2d <- function(x, y, h = NULL, n = 25L, lims = c(range(x), range(y))) {
  bw <- function(v) 4 * 1.06 * min(stats::sd(v), stats::IQR(v) / 1.34) * length(v)^(-0.2)
  hh <- if (is.null(h)) c(bw(x), bw(y)) else rep(h, length.out = 2)
  hh <- hh / 4
  gx <- seq(lims[1], lims[2], length.out = n)
  gy <- seq(lims[3], lims[4], length.out = n)
  ax <- outer(gx, x, `-`) / hh[1]
  ay <- outer(gy, y, `-`) / hh[2]
  list(x = gx, y = gy, z = tcrossprod(stats::dnorm(ax), stats::dnorm(ay)) / (length(x) * hh[1] * hh[2]))
}

#' @rdname KnoxTest
#' @export
CrossCorrelation <- function(x, y, lag_max = NULL) {
  n <- length(x)
  K <- if (is.null(lag_max)) floor(10 * log10(n)) else lag_max
  xa <- x - mean(x)
  ya <- y - mean(y)
  s <- sqrt(mean(xa^2) * mean(ya^2))
  lags <- -K:K
  r <- vapply(lags, function(k) {
    t <- max(1, 1 - k):min(n, n - k)
    sum(xa[t + k] * ya[t]) / n / s
  }, 0)
  list(lag = lags, acf = r)
}

#' @rdname KnoxTest
#' @export
EofAnalysis <- function(Z, k = NULL) {
  A <- sweep(as.matrix(Z), 2, colMeans(as.matrix(Z)))
  sv <- svd(A)
  kk <- if (is.null(k)) length(sv$d) else min(k, length(sv$d))
  for (j in seq_len(kk)) {
    if (sv$v[which.max(abs(sv$v[, j])), j] < 0) {
      sv$v[, j] <- -sv$v[, j]
      sv$u[, j] <- -sv$u[, j]
    }
  }
  list(eofs = sv$v[, seq_len(kk), drop = FALSE], pcs = sv$u[, seq_len(kk), drop = FALSE] %*% diag(sv$d[seq_len(kk)], kk),
       explained = (sv$d^2 / sum(sv$d^2))[seq_len(kk)], sdev = sv$d[seq_len(kk)] / sqrt(nrow(A) - 1))
}
