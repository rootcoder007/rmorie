#' Berkeley Earth regional temperature average
#'
#' Model T_i(t) = theta(t) + b_i + W(x_i, t) (Rohde et al. 2013). Starting
#' from station means, each iteration fits the weather correlation
#' R(d) = c exp(-d / L) by weighted least squares to pairwise residual
#' correlations (weights the overlap lengths; grid then golden-section search
#' on L), estimates theta(t) by ordinary block kriging of the domain mean over
#' a grid, re-estimates the baselines and centres theta. Identical to the
#' Python arm \code{morie.fn.berkeley}.
#'
#' @param stations Matrix of coordinates (x, y), or (lat, lon) with spherical.
#' @param series Matrix of station temperatures (stations by times, NA missing).
#' @param grid Optional matrix of domain points (default 10 x 10 on the bounding box).
#' @param spherical Great-circle kilometres from latitude and longitude.
#' @param n_iter,tol Iteration cap and convergence tolerance.
#' @return List: theta, theta_se, baselines, correlation_c, correlation_length,
#'   nugget, correlation_sse, iterations.
#' @references Rohde, R. et al. (2013). Berkeley Earth temperature averaging
#'   process. Geoinformatics and Geostatistics: An Overview 1(2).
#' @examples
#' st <- rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1), c(0.5, 0.5))
#' th <- c(0, 0.3, -0.2, 0.5, 0.1, 0.4)
#' T <- outer(10 + 2 * (0:4), th, "+") + 0.05 * sin(outer(1:5, 1:6))
#' BerkeleyEarth(st, T)$theta
#' @export
BerkeleyEarth <- function(stations, series, grid = NULL, spherical = FALSE, n_iter = 20L, tol = 1e-10) {
  X <- as.matrix(stations) + 0
  S <- as.matrix(series) + 0
  m <- nrow(X)
  nt <- ncol(S)
  if (nrow(S) != m) stop("series must have one row per station, all of the same length")
  if (is.null(grid)) {
    ab <- expand.grid(b = 0:9, a = 0:9)
    grid <- cbind(min(X[, 1]) + (max(X[, 1]) - min(X[, 1])) * (ab$a + 0.5) / 10,
                  min(X[, 2]) + (max(X[, 2]) - min(X[, 2])) * (ab$b + 0.5) / 10)
  }
  G <- as.matrix(grid) + 0
  ng <- nrow(G)
  dd <- function(a, b) .be_dist(a, b, spherical)
  D <- outer(seq_len(m), seq_len(m), Vectorize(function(i, j) dd(X[i, ], X[j, ])))
  DG <- outer(seq_len(m), seq_len(ng), Vectorize(function(i, j) dd(X[i, ], G[j, ])))
  DGG <- outer(seq_len(ng), seq_len(ng), Vectorize(function(i, j) dd(G[i, ], G[j, ])))
  b <- vapply(seq_len(m), function(i) {
    v <- S[i, !is.na(S[i, ])]
    if (!length(v)) stop("every station needs at least one value")
    .be_lsum(v) / length(v)
  }, 0)
  theta <- numeric(nt)
  se <- numeric(nt)
  cc <- 1
  L <- 1
  sse <- 0
  it <- 0
  for (it in seq_len(n_iter)) {
    resid <- (S - matrix(theta, m, nt, byrow = TRUE)) - b
    pairs <- .be_pair_corr(resid, D)
    if (!nrow(pairs)) stop("no station pair overlaps in three or more times")
    fit <- .be_fit_corr(pairs)
    cc <- fit$c
    L <- fit$L
    sse <- fit$sse
    allr <- as.vector(t(resid))
    allr <- allr[!is.na(allr)]
    mr <- .be_lsum(allr) / length(allr)
    s2 <- .be_lsum((allr - mr)^2) / max(length(allr) - 1, 1)
    rgg <- 0
    for (i in seq_len(ng)) for (j in seq_len(ng)) rgg <- rgg + .be_corr(DGG[i, j], cc, L)
    rgg <- rgg / ng^2
    new <- numeric(nt)
    for (t in seq_len(nt)) {
      av <- which(!is.na(S[, t]))
      if (!length(av)) {
        new[t] <- NaN
        se[t] <- NaN
        next
      }
      k <- length(av)
      A <- matrix(0, k + 1, k + 1)
      for (p in seq_len(k)) for (q in seq_len(k)) A[p, q] <- .be_corr(D[av[p], av[q]], cc, L)
      A[seq_len(k), k + 1] <- 1
      A[k + 1, seq_len(k)] <- 1
      rbar <- vapply(av, function(i) {
        s <- 0
        for (d in DG[i, ]) s <- s + .be_corr(d, cc, L)
        s / ng
      }, 0)
      sol <- solve(A, c(rbar, 1))
      w <- sol[seq_len(k)]
      mu <- sol[k + 1]
      th <- 0
      wr <- 0
      for (q in seq_len(k)) {
        th <- th + w[q] * (S[av[q], t] - b[av[q]])
        wr <- wr + w[q] * rbar[q]
      }
      new[t] <- th
      se[t] <- sqrt(max(s2 * (rgg - wr - mu), 0))
    }
    ok <- new[!is.nan(new)]
    new <- new - .be_lsum(ok) / length(ok)
    nb <- vapply(seq_len(m), function(i) {
      sel <- !is.na(S[i, ]) & !is.nan(new)
      if (!any(sel)) return(b[i])
      .be_lsum(S[i, sel] - new[sel]) / sum(sel)
    }, 0)
    fin <- !is.nan(new)
    change <- max(max(abs(new[fin] - theta[fin])), max(abs(nb - b)))
    theta <- new
    b <- nb
    if (change < tol) break
  }
  list(theta = theta, theta_se = se, baselines = b, correlation_c = cc, correlation_length = L, nugget = 1 - cc,
       correlation_sse = sse, iterations = it)
}

.be_lsum <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

.be_dist <- function(a, b, spherical) {
  if (!spherical) return(sqrt((a[1] - b[1])^2 + (a[2] - b[2])^2))
  r <- a * pi / 180
  q <- b * pi / 180
  h <- sin((q[1] - r[1]) / 2)^2 + cos(r[1]) * cos(q[1]) * sin((q[2] - r[2]) / 2)^2
  2 * 6371 * asin(min(1, sqrt(h)))
}

.be_corr <- function(d, cc, L) if (d == 0) 1 else cc * exp(-d / L)

.be_pair_corr <- function(resid, D) {
  m <- nrow(resid)
  out <- matrix(0, 0, 3)
  for (i in seq_len(m - 1)) for (j in (i + 1):m) {
    idx <- which(!is.na(resid[i, ]) & !is.na(resid[j, ]))
    if (length(idx) < 3) next
    xa <- resid[i, idx]
    xb <- resid[j, idx]
    ma <- .be_lsum(xa) / length(xa)
    mb <- .be_lsum(xb) / length(xb)
    sab <- 0
    saa <- 0
    sbb <- 0
    for (q in seq_along(idx)) {
      sab <- sab + (xa[q] - ma) * (xb[q] - mb)
      saa <- saa + (xa[q] - ma)^2
      sbb <- sbb + (xb[q] - mb)^2
    }
    if (saa > 0 && sbb > 0) out <- rbind(out, c(D[i, j], sab / sqrt(saa * sbb), length(idx)))
  }
  out
}

.be_fit_corr <- function(pairs) {
  ds <- pairs[pairs[, 1] > 0, 1]
  lo <- min(ds) / 10
  hi <- max(ds) * 10
  sse_c <- function(L) {
    num <- 0
    den <- 0
    for (r in seq_len(nrow(pairs))) {
      e <- exp(-pairs[r, 1] / L)
      num <- num + pairs[r, 3] * pairs[r, 2] * e
      den <- den + pairs[r, 3] * e * e
    }
    cc <- if (den > 0) min(max(num / den, 0), 1) else 0
    s <- 0
    for (r in seq_len(nrow(pairs))) {
      v <- pairs[r, 2] - cc * exp(-pairs[r, 1] / L)
      s <- s + pairs[r, 3] * v * v
    }
    c(s, cc)
  }
  K <- 60
  grid <- exp(log(lo) + (log(hi) - log(lo)) * (0:(K - 1)) / (K - 1))
  vals <- vapply(grid, function(L) sse_c(L)[1], 0)
  kb <- which.min(vals)
  a <- grid[max(kb - 1, 1)]
  b <- grid[min(kb + 1, K)]
  g <- (sqrt(5) - 1) / 2
  x1 <- b - g * (b - a)
  x2 <- a + g * (b - a)
  f1 <- sse_c(x1)[1]
  f2 <- sse_c(x2)[1]
  for (s in 1:80) {
    if (f1 <= f2) {
      b <- x2
      x2 <- x1
      f2 <- f1
      x1 <- b - g * (b - a)
      f1 <- sse_c(x1)[1]
    } else {
      a <- x1
      x1 <- x2
      f1 <- f2
      x2 <- a + g * (b - a)
      f2 <- sse_c(x2)[1]
    }
  }
  L <- (a + b) / 2
  r <- sse_c(L)
  list(c = r[2], L = L, sse = r[1])
}
