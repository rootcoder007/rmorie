.ppf_terms <- function(x, y, degree) {
  z <- 1
  if (degree >= 1) for (d in 1:degree) for (k in 0:d) z <- c(z, x^(d - k) * y^k)
  z
}

#' Log-linear Poisson point process: Berman-Turner fit and simulation
#'
#' \eqn{\log \lambda(u) = \theta^\top z(u)} with z the monomials in the
#' coordinates up to \code{degree} (\code{degree = 0} is the homogeneous
#' process). The likelihood is approximated by the Berman-Turner quadrature
#' (data points plus the \code{nx} by \code{ny} grid of tile centres, each
#' weighted by its tile's area shared among the quadrature points in that
#' tile) and maximised by Newton's method; this is
#' \code{spatstat.model::ppm} on the same quadrature scheme. With
#' \code{simulate > 0}, patterns are drawn by Lewis-Shedler thinning of a
#' homogeneous process at the largest fitted intensity over the corners and
#' quadrature points (Philox streams, identical to the Python arm).
#'
#' @param points Event locations (n x 2) inside \code{window}.
#' @param window Rectangle \code{c(xmin, xmax, ymin, ymax)}.
#' @param degree Polynomial degree of the log intensity.
#' @param nx,ny Dummy grid dimensions.
#' @param simulate Number of patterns to simulate from the fit.
#' @param seed Philox key.
#' @param max_iter Newton iterations.
#' @return List with \code{theta}, \code{se}, \code{loglik},
#'   \code{expected_count}, \code{simulated}.
#' @references Berman, M. and Turner, T. R. (1992). Approximating point
#'   process likelihoods with GLIM. Applied Statistics 41, 31-38.
#'
#'   Lewis, P. A. W. and Shedler, G. S. (1979). Simulation of
#'   nonhomogeneous Poisson processes by thinning. Naval Research Logistics
#'   Quarterly 26, 403-413.
#' @examples
#' PoissonProcessFit(rbind(c(0.1, 0.2), c(0.5, 0.5), c(0.9, 0.7)), c(0, 1, 0, 2), degree = 0)$theta
#' @export
PoissonProcessFit <- function(points, window, degree = 1L, nx = 12L, ny = 12L, simulate = 0L, seed = 0,
                              max_iter = 100L) {
  P <- as.matrix(points)
  w4 <- as.numeric(window)
  n <- nrow(P)
  g <- expand.grid(a = seq_len(nx) - 1, b = seq_len(ny) - 1)
  D <- cbind(w4[1] + (g$a + 0.5) * (w4[2] - w4[1]) / nx, w4[3] + (g$b + 0.5) * (w4[4] - w4[3]) / ny)
  Q <- rbind(P, D)
  ind <- c(rep(1, n), rep(0, nrow(D)))
  ta <- pmin(floor((Q[, 1] - w4[1]) / (w4[2] - w4[1]) * nx), nx - 1)
  tb <- pmin(floor((Q[, 2] - w4[3]) / (w4[4] - w4[3]) * ny), ny - 1)
  key <- ta * ny + tb
  cnt <- table(key)
  w <- (w4[2] - w4[1]) * (w4[4] - w4[3]) / (nx * ny) / as.numeric(cnt[as.character(key)])
  Z <- t(vapply(seq_len(nrow(Q)), function(i) .ppf_terms(Q[i, 1], Q[i, 2], degree), numeric(length(.ppf_terms(0, 0, degree)))))
  Z <- matrix(Z, nrow(Q))
  theta <- c(log(max(n, 1) / ((w4[2] - w4[1]) * (w4[4] - w4[3]))), rep(0, ncol(Z) - 1))
  for (it in seq_len(max_iter)) {
    lam <- exp(as.vector(Z %*% theta))
    step <- solve(crossprod(Z, Z * (w * lam)), crossprod(Z, ind - w * lam))
    theta <- theta + as.vector(step)
    if (max(abs(step)) < 1e-12) break
  }
  lam <- exp(as.vector(Z %*% theta))
  H <- crossprod(Z, Z * (w * lam))
  sims <- list()
  if (simulate > 0) {
    corners <- rbind(c(w4[1], w4[3]), c(w4[1], w4[4]), c(w4[2], w4[3]), c(w4[2], w4[4]))
    allp <- rbind(corners, Q)
    lmax <- max(vapply(seq_len(nrow(allp)), function(i) exp(sum(theta * .ppf_terms(allp[i, 1], allp[i, 2], degree))), 0))
    mu <- lmax * (w4[2] - w4[1]) * (w4[4] - w4[3])
    parts <- max(1, ceiling(mu / 500))
    for (s in seq_len(simulate) - 1L) {
      k <- 0L
      for (u in .morie_random_uniform(parts, seed = seed, stream = 2 * s)) {
        j <- 0L
        prob <- exp(-mu / parts)
        cum <- prob
        while (u > cum && prob > 0) {
          j <- j + 1L
          prob <- prob * mu / parts / j
          cum <- cum + prob
        }
        k <- k + j
      }
      pat <- matrix(0, 0, 2)
      if (k > 0) {
        v <- .morie_random_uniform(3 * k, seed = seed, stream = 2 * s + 1)
        for (j in seq_len(k)) {
          px <- w4[1] + v[3 * j - 2] * (w4[2] - w4[1])
          py <- w4[3] + v[3 * j - 1] * (w4[4] - w4[3])
          if (v[3 * j] <= exp(sum(theta * .ppf_terms(px, py, degree))) / lmax) pat <- rbind(pat, c(px, py))
        }
      }
      sims[[s + 1L]] <- pat
    }
  }
  list(theta = theta, se = sqrt(diag(solve(H))), loglik = sum(log(lam[seq_len(n)])) - sum(w * lam),
       expected_count = sum(w * lam), simulated = sims)
}
