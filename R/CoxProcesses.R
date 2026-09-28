#' Cox point processes
#'
#' \code{LgcpSimulate}: log-Gaussian Cox process on a lattice (Cholesky
#' field, Poisson counts, uniform positions). \code{LgcpMoments}: intensity,
#' expected count and pair correlation. \code{ThomasSimulate}: Thomas
#' cluster process. \code{ThomasPcf}: its pair correlation and K function.
#' \code{VoronoiResiduals}: Voronoi residuals of a log-linear intensity.
#' Philox draws; identical to the Python arm \code{morie.fn.coxproc}.
#'
#' @param window Rectangle (xmin, xmax, ymin, ymax).
#' @param n_grid Lattice size per side.
#' @param mu Mean of the Gaussian field, or mean offspring number.
#' @param model Covariance model (\code{\link{KrigingCovariance}}).
#' @param seed Philox seed.
#' @param sigma2 Field variance.
#' @param area Window area.
#' @param r Distance.
#' @param kappa Parent intensity.
#' @param scale Offspring dispersal standard deviation.
#' @param points Two-column matrix of points.
#' @param bbox Box (xmin, ymin, xmax, ymax).
#' @param beta Log-linear intensity coefficients (intercept, x, y).
#' @return List.
#' @references Moller, J., Syversveen, A. R. and Waagepetersen, R. P. (1998).
#'   Log Gaussian Cox processes. Scandinavian Journal of Statistics 25,
#'   451-482.
#'
#'   Diggle, P. J. (2013). Statistical Analysis of Spatial and Spatio-Temporal
#'   Point Patterns, 3rd edn. CRC Press.
#'
#'   Bray, A., Wong, K., Barr, C. D. and Schoenberg, F. P. (2014). Voronoi
#'   residual analysis of spatial point process models. Annals of Applied
#'   Statistics 8, 2247-2267.
#' @examples
#' ThomasPcf(0.1, 10, 0.05)
#' VoronoiResiduals(rbind(c(0.25, 0.5), c(0.75, 0.5)), c(0, 0, 1, 1), c(log(2), 0, 0))$residuals
#' @export
LgcpSimulate <- function(window, n_grid, mu, model, seed = 1) {
  g <- n_grid
  dx <- (window[2] - window[1]) / g
  dy <- (window[4] - window[3]) / g
  cen <- cbind(rep(window[1] + (seq_len(g) - 0.5) * dx, times = g), rep(window[3] + (seq_len(g) - 0.5) * dy, each = g))
  m <- nrow(cen)
  C <- matrix(0, m, m)
  for (a in seq_len(m)) for (b in seq_len(m)) C[a, b] <- KrigingCovariance(sqrt(sum((cen[a, ] - cen[b, ])^2)), model) + (if (a == b) 1e-10 else 0)
  L <- .s03chol(C)
  e <- .morie_random_normal(m, seed = seed)
  Z <- vapply(seq_len(m), function(a) {
    s <- 0
    for (k in seq_len(a)) s <- s + L[a, k] * e[k]
    mu + s
  }, 0)
  U <- .cx_stream(seed, 1000, FALSE)
  pts <- matrix(0, 0, 2)
  for (a in seq_len(m)) {
    k <- .cx_poisson(exp(Z[a]) * dx * dy, U)
    for (t in seq_len(k)) {
      px <- cen[a, 1] - dx / 2 + dx * U()
      py <- cen[a, 2] - dy / 2 + dy * U()
      pts <- rbind(pts, c(px, py))
    }
  }
  list(points = pts, field = Z, centres = cen)
}

.cx_stream <- function(seed, stream0, normal) {
  env <- new.env()
  env$block <- stream0
  env$buf <- numeric(0)
  env$pos <- 0
  function() {
    if (env$pos >= length(env$buf)) {
      env$buf <- if (normal) .morie_random_normal(4096, seed = seed, stream = env$block) else
        .morie_random_uniform(4096, seed = seed, stream = env$block)
      env$block <- env$block + 1
      env$pos <- 0
    }
    env$pos <- env$pos + 1
    env$buf[env$pos]
  }
}

.cx_poisson <- function(m, U) {
  if (m > 600) stop("Poisson mean too large for inversion")
  u <- U()
  k <- 0
  p <- exp(-m)
  F <- p
  while (u > F && p > 0) {
    k <- k + 1
    p <- p * m / k
    F <- F + p
  }
  k
}

#' @rdname LgcpSimulate
#' @export
LgcpMoments <- function(mu, sigma2, area, r = 0, model = NULL) {
  lam <- exp(mu + sigma2 / 2)
  out <- list(intensity = lam, expected_count = lam * area)
  if (!is.null(model)) out$pcf <- exp(KrigingCovariance(r, model))
  out
}

#' @rdname LgcpSimulate
#' @export
ThomasSimulate <- function(kappa, scale, mu, window, seed = 1) {
  d <- 4 * scale
  ex <- c(window[1] - d, window[2] + d)
  ey <- c(window[3] - d, window[4] + d)
  U <- .cx_stream(seed, 0, FALSE)
  npar <- .cx_poisson(kappa * diff(ex) * diff(ey), U)
  par <- matrix(0, npar, 2)
  for (i in seq_len(npar)) {
    par[i, 1] <- ex[1] + diff(ex) * U()
    par[i, 2] <- ey[1] + diff(ey) * U()
  }
  Z <- .cx_stream(seed, 5000, TRUE)
  pts <- matrix(0, 0, 2)
  for (i in seq_len(npar)) {
    k <- .cx_poisson(mu, U)
    for (t in seq_len(k)) {
      x <- par[i, 1] + scale * Z()
      y <- par[i, 2] + scale * Z()
      if (x >= window[1] && x <= window[2] && y >= window[3] && y <= window[4]) pts <- rbind(pts, c(x, y))
    }
  }
  list(points = pts, parents = par)
}

#' @rdname LgcpSimulate
#' @export
ThomasPcf <- function(r, kappa, scale) {
  s2 <- scale^2
  list(pcf = 1 + exp(-r^2 / (4 * s2)) / (4 * pi * kappa * s2), K = pi * r^2 + (1 - exp(-r^2 / (4 * s2))) / kappa)
}

.cx_dun <- rbind(
  c(0.225, 1 / 3, 1 / 3, 1 / 3),
  c(0.132394152788506, 0.059715871789770, 0.470142064105115, 0.470142064105115),
  c(0.132394152788506, 0.470142064105115, 0.059715871789770, 0.470142064105115),
  c(0.132394152788506, 0.470142064105115, 0.470142064105115, 0.059715871789770),
  c(0.125939180544827, 0.797426985353087, 0.101286507323456, 0.101286507323456),
  c(0.125939180544827, 0.101286507323456, 0.797426985353087, 0.101286507323456),
  c(0.125939180544827, 0.101286507323456, 0.101286507323456, 0.797426985353087)
)

.cx_cellint <- function(cell, beta) {
  s <- 0
  a <- cell[1, ]
  for (i in 2:(nrow(cell) - 1)) {
    b <- cell[i, ]
    cc <- cell[i + 1, ]
    area <- 0.5 * abs((b[1] - a[1]) * (cc[2] - a[2]) - (cc[1] - a[1]) * (b[2] - a[2]))
    q <- 0
    for (k in 1:7) {
      x <- .cx_dun[k, 2] * a[1] + .cx_dun[k, 3] * b[1] + .cx_dun[k, 4] * cc[1]
      y <- .cx_dun[k, 2] * a[2] + .cx_dun[k, 3] * b[2] + .cx_dun[k, 4] * cc[2]
      q <- q + .cx_dun[k, 1] * exp(beta[1] + beta[2] * x + beta[3] * y)
    }
    s <- s + area * q
  }
  s
}

#' @rdname LgcpSimulate
#' @export
VoronoiResiduals <- function(points, bbox, beta) {
  P <- as.matrix(points)
  n <- nrow(P)
  res <- numeric(n)
  areas <- numeric(n)
  for (i in seq_len(n)) {
    cell <- rbind(c(bbox[1], bbox[2]), c(bbox[3], bbox[2]), c(bbox[3], bbox[4]), c(bbox[1], bbox[4]))
    for (j in seq_len(n)) {
      if (j == i || all(P[j, ] == P[i, ])) next
      cell <- .ss_clip(cell, P[j, 1] - P[i, 1], P[j, 2] - P[i, 2], 0.5 * (P[j, 1]^2 + P[j, 2]^2 - P[i, 1]^2 - P[i, 2]^2))
      if (nrow(cell) == 0) break
    }
    ok <- nrow(cell) >= 3
    areas[i] <- if (ok) .ss_area(cell) else 0
    res[i] <- 1 - (if (ok) .cx_cellint(cell, beta) else 0)
  }
  list(residuals = res, areas = areas)
}
