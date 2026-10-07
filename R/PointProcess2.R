# SPDX-License-Identifier: AGPL-3.0-or-later
# Point processes: area-interaction, Thomas, LGCP, adaptive intensity, Berman-Turner, space-time K/G/J.
# Identical to the Python arm morie.fn.pointproc2.

#' Point processes: area-interaction and cluster simulation, LGCP, adaptive intensity, Berman-Turner fits, space-time K, G, J
#'
#' \code{AreaInteractionSimulate}: birth-death Metropolis-Hastings for
#' \code{beta^n eta^(-C(x))} with the union of discs counted on a grid.
#' \code{ThomasSimulate} and \code{ThomasK}: Thomas cluster process and its K
#' function. \code{LgcpSimulateGrid}: log-Gaussian Cox process on a grid.
#' \code{AbramsonIntensity}: adaptive Gaussian kernel with Abramson's
#' square-root bandwidths. \code{BermanTurnerFit}: penalised log-linear
#' intensity by Berman-Turner quadrature with grid counting weights.
#' \code{StKFunction}, \code{StGFunction}, \code{StJFunction}: space-time
#' second-order and nearest-neighbour summaries. Philox streams as the
#' Python arm.
#'
#' @param beta,eta,r Area-interaction activity, interaction and radius.
#' @param window c(x0, x1, y0, y1).
#' @param n_steps Birth-death steps.
#' @param grid Union-area lattice size.
#' @param seed Philox seed.
#' @param nx,ny Grid size.
#' @param points Point coordinates (rows; x, y, t for space-time).
#' @param at Evaluation locations.
#' @param h0 Pilot bandwidth.
#' @param trim Bandwidth trimming factor.
#' @param covariates Function of (x, y) returning the covariate vector.
#' @param ridge Ridge penalty (intercept unpenalised).
#' @param max_iter Newton iterations.
#' @param r_values,t_values Spatial and temporal distances.
#' @param area,time_length Window area and time length.
#' @param method "diggle" (n(n - 1) denominator) or "stpp" (n^2, as stpp::STIKhat).
#' @param time_range c(t0, t1).
#' @param n_grid,n_time Reference grid sizes.
#' @return A list or numeric vector.
#' @references Baddeley, A. J. and van Lieshout, M. N. M. (1995). Ann. Inst.
#'   Statist. Math. 47, 601-619. Geyer, C. J. and Moller, J. (1994). Scand. J.
#'   Statist. 21, 359-373. Moller, J. and Waagepetersen, R. P. (2004).
#'   Statistical Inference and Simulation for Spatial Point Processes. Moller,
#'   J., Syversveen, A. R. and Waagepetersen, R. P. (1998). Scand. J. Statist.
#'   25, 451-482. Abramson, I. S. (1982). Ann. Statist. 10, 1217-1223.
#'   Berman, M. and Turner, T. R. (1992). Applied Statistics 41, 31-38. Diggle,
#'   P. J. et al. (1995). Statistical Methods in Medical Research 4, 124-136.
#'   van Lieshout, M. N. M. (2011). Statistica Neerlandica 65, 183-201.
#' @examples
#' xy <- AreaInteractionSimulate(beta = 50, eta = 2, r = 0.05, window = c(0, 1, 0, 1),
#'                               n_steps = 200, grid = 40, seed = 1)
#' xy$n
#' head(xy$points)
#' @export
AreaInteractionSimulate <- function(beta, eta, r, window, n_steps, grid = 100, seed = 0) {
  x0 <- window[1]
  x1 <- window[2]
  y0 <- window[3]
  y1 <- window[4]
  area <- (x1 - x0) * (y1 - y0)
  gx0 <- x0 - r
  gy0 <- y0 - r
  dx <- (x1 - x0 + 2 * r) / grid
  dy <- (y1 - y0 + 2 * r) / grid
  cover <- matrix(0L, grid, grid)
  unit <- dx * dy / (pi * r * r)
  cells <- function(px, py) {
    i0 <- max(0, as.integer((px - r - gx0) / dx))
    i1 <- min(grid - 1, as.integer((px + r - gx0) / dx))
    j0 <- max(0, as.integer((py - r - gy0) / dy))
    j1 <- min(grid - 1, as.integer((py + r - gy0) / dy))
    out <- matrix(0L, 0, 2)
    for (i in i0:i1) {
      cx <- gx0 + (i + 0.5) * dx
      for (j in j0:j1) {
        cy <- gy0 + (j + 0.5) * dy
        if ((cx - px)^2 + (cy - py)^2 <= r * r) out <- rbind(out, c(i + 1L, j + 1L))
      }
    }
    out
  }
  pts <- matrix(0, 0, 2)
  for (s in seq_len(n_steps) - 1) {
    u <- .morie_random_uniform(4, seed = seed, stream = s)
    if (u[1] < 0.5) {
      px <- x0 + u[2] * (x1 - x0)
      py <- y0 + u[3] * (y1 - y0)
      cl <- cells(px, py)
      dC <- sum(cover[cl] == 0) * unit
      if (u[4] < beta * area * eta^(-dC) / (nrow(pts) + 1)) {
        pts <- rbind(pts, c(px, py))
        cover[cl] <- cover[cl] + 1L
      }
    } else if (nrow(pts)) {
      k <- min(as.integer(u[2] * nrow(pts)), nrow(pts) - 1) + 1
      cl <- cells(pts[k, 1], pts[k, 2])
      dC <- sum(cover[cl] == 1) * unit
      if (u[4] < nrow(pts) / (beta * area) * eta^dC) {
        cover[cl] <- cover[cl] - 1L
        pts <- pts[-k, , drop = FALSE]
      }
    }
  }
  list(points = pts, n = nrow(pts), union_area_units = sum(cover > 0) * unit)
}

.pp2_poisson <- function(lam, u) {
  k <- 0
  p <- exp(-lam)
  cc <- p
  while (u > cc && k < 100000) {
    k <- k + 1
    p <- p * lam / k
    cc <- cc + p
  }
  k
}

#' @rdname AreaInteractionSimulate
#' @export
AbramsonIntensity <- function(points, at, h0, trim = 5) {
  P <- matrix(as.numeric(unlist(points)), ncol = 2, byrow = is.list(points))
  A <- matrix(as.numeric(unlist(at)), ncol = 2, byrow = is.list(at))
  gk <- function(d2, h) exp(-d2 / (2 * h * h)) / (2 * pi * h * h)
  D2 <- as.matrix(stats::dist(P))^2
  pilot <- rowSums(gk(D2, h0))
  G <- exp(sum(log(pilot)) / length(pilot))
  h <- h0 * pmin((pilot / G)^-0.5, trim)
  lam <- vapply(seq_len(nrow(A)), function(k) sum(gk((A[k, 1] - P[, 1])^2 + (A[k, 2] - P[, 2])^2, h)), 0)
  list(intensity = lam, bandwidths = h, pilot = pilot)
}

#' @rdname AreaInteractionSimulate
#' @export
BermanTurnerFit <- function(points, window, covariates, nx, ny, ridge = 0, max_iter = 100) {
  P <- matrix(as.numeric(unlist(points)), ncol = 2, byrow = is.list(points))
  dx <- (window[2] - window[1]) / nx
  dy <- (window[4] - window[3]) / ny
  g <- expand.grid(i = 0:(nx - 1), j = 0:(ny - 1))
  Q <- rbind(P, cbind(window[1] + (g$i + 0.5) * dx, window[3] + (g$j + 0.5) * dy))
  z <- c(rep(1, nrow(P)), rep(0, nx * ny))
  ix <- pmin(pmax(ceiling(nx * (Q[, 1] - window[1]) / (window[2] - window[1])), 1), nx) - 1
  iy <- pmin(pmax(ceiling(ny * (Q[, 2] - window[3]) / (window[4] - window[3])), 1), ny) - 1
  ti <- ix + nx * iy
  w <- dx * dy / as.numeric(table(ti)[as.character(ti)])
  Z <- cbind(1, t(vapply(seq_len(nrow(Q)), function(q) as.numeric(covariates(Q[q, 1], Q[q, 2])), numeric(length(covariates(Q[1, 1], Q[1, 2]))))))
  Z <- matrix(Z, nrow(Q))
  k <- ncol(Z)
  pen <- c(0, rep(ridge, k - 1))
  theta <- c(log(nrow(P) / ((window[2] - window[1]) * (window[4] - window[3]))), rep(0, k - 1))
  for (it in seq_len(max_iter)) {
    mu <- exp(as.numeric(Z %*% theta))
    gr <- as.numeric(crossprod(Z, z - w * mu)) - pen * theta
    H <- crossprod(Z, Z * (w * mu)) + diag(pen, k)
    step <- as.numeric(solve(H, gr))
    theta <- theta + step
    if (max(abs(step)) <= 1e-12) break
  }
  list(coefficients = theta, se = sqrt(diag(solve(H))), weights = w, n_quad = nrow(Q))
}

#' @rdname AreaInteractionSimulate
#' @export
StKFunction <- function(points, r_values, t_values, area, time_length, method = "diggle") {
  P <- matrix(as.numeric(unlist(points)), ncol = 3, byrow = is.list(points))
  n <- nrow(P)
  D <- as.matrix(stats::dist(P[, 1:2]))
  Tm <- abs(outer(P[, 3], P[, 3], "-"))
  diag(D) <- Inf
  den <- if (method == "stpp") n * n else n * (n - 1)
  K <- outer(r_values, t_values, Vectorize(function(r, t) area * time_length * sum(D <= r & Tm <= t) / den))
  L <- sqrt(K / (2 * pi * matrix(t_values, length(r_values), length(t_values), byrow = TRUE)))
  list(K = K, L = L)
}

#' @rdname AreaInteractionSimulate
#' @export
StGFunction <- function(points, r_values, t_values) {
  P <- matrix(as.numeric(unlist(points)), ncol = 3, byrow = is.list(points))
  D <- as.matrix(stats::dist(P[, 1:2]))
  Tm <- abs(outer(P[, 3], P[, 3], "-"))
  diag(D) <- Inf
  outer(r_values, t_values, Vectorize(function(r, t) mean(rowSums(D <= r & Tm <= t) > 0)))
}

#' @rdname AreaInteractionSimulate
#' @export
StJFunction <- function(points, r_values, t_values, window, time_range, n_grid = 10, n_time = 10) {
  P <- matrix(as.numeric(unlist(points)), ncol = 3, byrow = is.list(points))
  g <- expand.grid(i = 0:(n_grid - 1), j = 0:(n_grid - 1), k = 0:(n_time - 1))
  ref <- cbind(window[1] + (g$i + 0.5) * (window[2] - window[1]) / n_grid,
               window[3] + (g$j + 0.5) * (window[4] - window[3]) / n_grid,
               time_range[1] + (g$k + 0.5) * (time_range[2] - time_range[1]) / n_time)
  G <- StGFunction(P, r_values, t_values)
  Ds <- sqrt(outer(ref[, 1], P[, 1], "-")^2 + outer(ref[, 2], P[, 2], "-")^2)
  Dt <- abs(outer(ref[, 3], P[, 3], "-"))
  F_ <- outer(r_values, t_values, Vectorize(function(r, t) mean(rowSums(Ds <= r & Dt <= t) > 0)))
  J <- ifelse(F_ < 1, (1 - G) / (1 - F_), NaN)
  list(G = G, F = F_, J = J)
}
