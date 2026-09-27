.gp_stat <- function(interaction, d, par, near, P, skip) {
  if (interaction == "strauss") return(sum(d <= par$r))
  if (interaction == "softcore") {
    pr <- -((d[is.finite(d)] / par$sigma0)^(-2 / par$kappa))
    return(if (any(pr < -25)) -Inf else sum(pr))
  }
  if (interaction == "diggle_gratton") {
    if (any(d <= par$delta)) return(-Inf)
    dd <- d[d <= par$rho]
    return(sum(log((dd - par$delta) / (par$rho - par$delta))))
  }
  if (interaction == "geyer") {
    nb <- which(d <= par$r)
    v <- min(par$sat, length(nb))
    for (j in nb) {
      tj <- near[j] - if (!is.null(skip) && sqrt(sum((P[j, ] - P[skip, ])^2)) <= par$r) 1 else 0
      v <- v + min(par$sat, tj + 1) - min(par$sat, tj)
    }
    return(v)
  }
  stop("interaction must be 'strauss', 'geyer', 'softcore' or 'diggle_gratton'", call. = FALSE)
}

#' Maximum pseudolikelihood for Gibbs point processes
#'
#' The conditional intensity is \eqn{\log\lambda(u | x) = \log\beta + \theta
#' V(u | x)} with the canonical statistic V of the Strauss (count within
#' \code{r}), Geyer (change in \eqn{\sum_i \min(sat, t_r(x_i, x))}), soft-core
#' (\eqn{-\sum_j (d_j/\sigma_0)^{-2/\kappa}}, zero intensity once a term falls
#' below -25; \eqn{\sigma_0} defaults to the smallest nearest-neighbour
#' distance) or Diggle-Gratton (\eqn{\sum_j \log((d_j - \delta)/(\rho -
#' \delta))}, \eqn{-\infty} inside the hard core) model. The Besag
#' pseudolikelihood is approximated by the Berman-Turner quadrature (data
#' points plus the nx by ny grid of tile centres, counting weights) and
#' maximised by Newton's method on the standardised statistic; this is
#' \code{spatstat.model::ppm} with \code{correction = "none"} on that
#' quadrature scheme.
#'
#' @param points Event locations (n x 2) inside \code{window}.
#' @param window Rectangle \code{c(xmin, xmax, ymin, ymax)}.
#' @param interaction \code{"strauss"}, \code{"geyer"}, \code{"softcore"}
#'   or \code{"diggle_gratton"}.
#' @param r Interaction radius (Strauss, Geyer).
#' @param sat Saturation (Geyer).
#' @param kappa Soft-core index.
#' @param sigma0 Soft-core scale.
#' @param delta,rho Diggle-Gratton hard core and range.
#' @param nx,ny Dummy grid dimensions.
#' @param max_iter Newton iterations.
#' @return List with \code{theta} (log beta, interaction coefficient),
#'   \code{se}, \code{beta}, \code{gamma} or \code{sigma}, \code{loglik},
#'   \code{n_quadrature}.
#' @references Besag, J. (1977). Some methods of statistical analysis for
#'   spatial data. Bulletin of the International Statistical Institute 47,
#'   77-92.
#'
#'   Baddeley, A. and Turner, R. (2000). Practical maximum pseudolikelihood
#'   for spatial point patterns. Australian and New Zealand Journal of
#'   Statistics 42, 283-322.
#'
#'   Diggle, P. J. and Gratton, R. J. (1984). Monte Carlo methods of
#'   inference for implicit statistical models. Journal of the Royal
#'   Statistical Society B 46, 193-227.
#' @examples
#' P <- rbind(c(0.1, 0.1), c(0.3, 0.2), c(0.8, 0.7), c(0.5, 0.9), c(0.2, 0.6), c(0.7, 0.3))
#' GibbsPseudolikelihood(P, c(0, 1, 0, 1), "strauss", r = 0.25, nx = 4, ny = 4)$theta
#' @export
GibbsPseudolikelihood <- function(points, window, interaction, r = NULL, sat = NULL, kappa = NULL, sigma0 = NULL,
                                  delta = NULL, rho = NULL, nx = 12L, ny = 12L, max_iter = 100L) {
  P <- as.matrix(points)
  w4 <- as.numeric(window)
  n <- nrow(P)
  DM <- as.matrix(stats::dist(P))
  if (interaction == "softcore" && is.null(sigma0)) sigma0 <- min(DM[upper.tri(DM)])
  par <- list(r = r, sat = sat, kappa = kappa, sigma0 = sigma0, delta = delta, rho = rho)
  near <- if (interaction == "geyer") rowSums(DM <= r) - 1 else NULL
  g <- expand.grid(a = seq_len(nx) - 1, b = seq_len(ny) - 1)
  D <- cbind(w4[1] + (g$a + 0.5) * (w4[2] - w4[1]) / nx, w4[3] + (g$b + 0.5) * (w4[4] - w4[3]) / ny)
  Q <- rbind(P, D)
  ta <- pmin(floor((Q[, 1] - w4[1]) / (w4[2] - w4[1]) * nx), nx - 1)
  tb <- pmin(floor((Q[, 2] - w4[3]) / (w4[4] - w4[3]) * ny), ny - 1)
  key <- ta * ny + tb
  cnt <- table(key)
  w <- (w4[2] - w4[1]) * (w4[4] - w4[3]) / (nx * ny) / as.numeric(cnt[as.character(key)])
  V <- vapply(seq_len(nrow(Q)), function(i) {
    d <- sqrt((P[, 1] - Q[i, 1])^2 + (P[, 2] - Q[i, 2])^2)
    if (i <= n) d[i] <- Inf
    .gp_stat(interaction, d, par, near, P, if (i <= n) i else NULL)
  }, 0)
  if (any(!is.finite(V[seq_len(n)]))) stop("the data violate the hard core", call. = FALSE)
  keep <- which(is.finite(V))
  m <- mean(V[keep])
  sd <- sqrt(mean((V[keep] - m)^2))
  if (sd == 0) sd <- 1
  Z <- cbind(1, (V[keep] - m) / sd)
  y <- as.numeric(keep <= n)
  wk <- w[keep]
  th <- c(log(n / ((w4[2] - w4[1]) * (w4[4] - w4[3]))), 0)
  for (it in seq_len(max_iter)) {
    lam <- exp(as.vector(Z %*% th))
    step <- solve(crossprod(Z, Z * (wk * lam)), crossprod(Z, y - wk * lam))
    th <- th + as.vector(step)
    if (max(abs(step)) < 1e-12) break
  }
  lam <- exp(as.vector(Z %*% th))
  C <- solve(crossprod(Z, Z * (wk * lam)))
  A <- rbind(c(1, -m / sd), c(0, 1 / sd))
  theta <- as.vector(A %*% th)
  out <- list(theta = theta, se = sqrt(diag(A %*% C %*% t(A))), beta = exp(theta[1]),
              loglik = sum(Z[y == 1, ] %*% th) - sum(wk * lam), n_quadrature = length(keep))
  if (interaction %in% c("strauss", "geyer")) out$gamma <- exp(theta[2])
  if (interaction == "softcore") {
    out$sigma <- if (theta[2] > 0) theta[2]^(kappa / 2) * sigma0 else NaN
    out$sigma0 <- sigma0
  }
  out
}
#' Simple sequential inhibition process
#'
#' Points are proposed uniformly in the window, one at a time, and a
#' proposal is kept only if it lies at least \code{r} from every point kept
#' so far (Diggle, Besag and Gleaves 1976). The process stops when \code{n}
#' points are kept or, if \code{n} is \code{NULL}, after
#' \code{max_failures} consecutive rejections (an approximation to the
#' jammed state; the jamming coverage of random sequential adsorption of
#' discs is about 0.547, Feder 1980). Proposal k uses Philox uniforms 2k and
#' 2k + 1 of stream \code{2k \%/\% 4096}, as in the Python arm.
#'
#' @param r Inhibition distance.
#' @param window Rectangle \code{c(xmin, xmax, ymin, ymax)}.
#' @param n Target number of points (\code{NULL} to saturate).
#' @param max_failures Consecutive rejections allowed.
#' @param seed Philox key.
#' @return List with \code{points}, \code{proposals}, \code{n},
#'   \code{saturated}, \code{coverage}.
#' @references Diggle, P. J., Besag, J. E. and Gleaves, J. T. (1976).
#'   Statistical analysis of spatial point patterns by means of distance
#'   methods. Biometrics 32, 659-667.
#'
#'   Feder, J. (1980). Random sequential adsorption. Journal of Theoretical
#'   Biology 87, 237-254.
#' @examples
#' SequentialInhibition(0.2, c(0, 1, 0, 1), n = 5, seed = 1)$points
#' @export
SequentialInhibition <- function(r, window, n = NULL, max_failures = 1000L, seed = 0) {
  w <- as.numeric(window)
  px <- numeric(0)
  py <- numeric(0)
  k <- 0
  fails <- 0
  block <- -1
  buf <- NULL
  rr <- r^2
  while ((is.null(n) || length(px) < n) && fails < max_failures) {
    b <- (2 * k) %/% 4096
    if (b != block) {
      block <- b
      buf <- .morie_random_uniform(4096, seed = seed, stream = block)
    }
    off <- 2 * k - 4096 * block
    ux <- w[1] + buf[off + 1] * (w[2] - w[1])
    uy <- w[3] + buf[off + 2] * (w[4] - w[3])
    k <- k + 1
    if (all((ux - px)^2 + (uy - py)^2 >= rr)) {
      px <- c(px, ux)
      py <- c(py, uy)
      fails <- 0
    } else {
      fails <- fails + 1
    }
  }
  list(points = cbind(px, py, deparse.level = 0), proposals = k, n = length(px),
       saturated = is.null(n) || length(px) < n,
       coverage = length(px) * pi * (r / 2)^2 / ((w[2] - w[1]) * (w[4] - w[3])))
}
