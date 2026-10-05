#' Random walks, Brownian motion, Brownian bridges and percolation
#'
#' \code{LatticeRandomWalk}: simple random walks on the square lattice.
#' \code{CorrelatedRandomWalk}: constant steps with von Mises turning angles
#' (Best-Fisher rejection) and \code{CrwMsd} the Kareiva-Shigesada mean
#' squared displacement. \code{BrownianMotion}: planar Brownian paths.
#' \code{BrownianBridgeUd}: Brownian bridge movement model utilisation
#' distribution (Horne et al. 2007). \code{SitePercolation}: site
#' percolation clusters and spanning. Identical to the Python arm
#' \code{morie.fn.movement}.
#'
#' @param steps,n Number of steps.
#' @param nwalk Number of walks.
#' @param seed Philox seed.
#' @param step_length Step length.
#' @param kappa von Mises concentration.
#' @param dt Time step.
#' @param sigma Diffusion scale.
#' @param track Two-column fixes.
#' @param times Fix times.
#' @param xs,ys Grid coordinates.
#' @param sig1,sig2 Motion variance scale and location error.
#' @param nalpha Integration nodes per segment.
#' @param nrow,ncol Grid size.
#' @param p Site occupation probability.
#' @param nsim Realisations.
#' @return List (numeric vector for \code{CrwMsd}).
#' @references Kareiva, P. M. and Shigesada, N. (1983). Analyzing insect
#'   movement as a correlated random walk. Oecologia 56, 234-238.
#'
#'   Horne, J. S., Garton, E. O., Krone, S. M. and Lewis, J. S. (2007).
#'   Analyzing animal movements using Brownian bridges. Ecology 88, 2354-2363.
#'
#'   Stauffer, D. and Aharony, A. (1994). Introduction to Percolation Theory,
#'   2nd edn. Taylor and Francis.
#' @examples
#' CrwMsd(3, kappa = 2)
#' SitePercolation(3, 3, 1)$spanning
#' @export
LatticeRandomWalk <- function(steps, nwalk = 1L, seed = 1) {
  moves <- rbind(c(1, 0), c(0, 1), c(-1, 0), c(0, -1))
  paths <- lapply(seq_len(nwalk) - 1, function(w) {
    u <- .morie_random_uniform(steps, seed = seed, stream = w)
    m <- moves[pmin(floor(u * 4), 3) + 1, , drop = FALSE]
    rbind(c(0, 0), apply(m, 2, cumsum))
  })
  list(paths = paths, msd = rowMeans(vapply(paths, function(p) rowSums(p^2), numeric(steps + 1))))
}

.mv_ratio <- function(kappa) {
  if (kappa >= 700) return(1 - 1 / (2 * kappa))
  besselI(kappa, 1, expon.scaled = TRUE) / besselI(kappa, 0, expon.scaled = TRUE)
}

#' @rdname LatticeRandomWalk
#' @export
CrwMsd <- function(steps, step_length = 1, kappa = 2) {
  cc <- .mv_ratio(kappa)
  n <- 0:steps
  if (cc < 1) n * step_length^2 + 2 * step_length^2 * cc / (1 - cc) * (n - (1 - cc^n) / (1 - cc)) else (n * step_length)^2
}

#' @rdname LatticeRandomWalk
#' @export
CorrelatedRandomWalk <- function(steps, step_length = 1, kappa = 2, nwalk = 1L, seed = 1) {
  tau <- 1 + sqrt(1 + 4 * kappa^2)
  rho <- (tau - sqrt(2 * tau)) / (2 * kappa)
  rr <- (1 + rho^2) / (2 * rho)
  paths <- lapply(seq_len(nwalk) - 1, function(w) {
    u <- .morie_random_uniform(1 + 3 * 64 * steps, seed = seed, stream = w)
    pos <- 2
    head <- 2 * pi * u[1]
    P <- matrix(0, steps + 1, 2)
    for (t in seq_len(steps)) {
      repeat {
        z <- cos(pi * u[pos])
        f <- (1 + rr * z) / (rr + z)
        cc <- kappa * (rr - f)
        u2 <- u[pos + 1]
        u3 <- u[pos + 2]
        pos <- pos + 3
        if (cc * (2 - cc) - u2 > 0 || log(cc / u2) + 1 - cc >= 0) break
      }
      head <- head + sign(u3 - 0.5) * acos(max(-1, min(1, f)))
      P[t + 1, ] <- P[t, ] + step_length * c(cos(head), sin(head))
    }
    P
  })
  list(paths = paths, msd = rowMeans(vapply(paths, function(p) rowSums(p^2), numeric(steps + 1))),
       theory = CrwMsd(steps, step_length, kappa))
}

#' @rdname LatticeRandomWalk
#' @export
BrownianMotion <- function(n, dt, sigma = 1, nwalk = 1L, seed = 1) {
  .morie_arg(n, "i1")
  paths <- lapply(seq_len(nwalk) - 1, function(w) {
    z <- matrix(.morie_random_normal(2 * n, seed = seed, stream = w), n, 2, byrow = TRUE)
    rbind(c(0, 0), apply(sigma * sqrt(dt) * z, 2, cumsum))
  })
  list(paths = paths, msd = rowMeans(vapply(paths, function(p) rowSums(p^2), numeric(n + 1))))
}

#' @rdname LatticeRandomWalk
#' @export
BrownianBridgeUd <- function(track, times, xs, ys, sig1, sig2, nalpha = 25L) {
  P <- as.matrix(track)
  G <- expand.grid(x = xs, y = ys)
  dens <- numeric(nrow(G))
  total <- times[length(times)] - times[1]
  for (k in seq_len(nrow(P) - 1)) {
    dur <- times[k + 1] - times[k]
    for (m in seq_len(nalpha) - 1) {
      al <- (m + 0.5) / nalpha
      mu <- P[k, ] + al * (P[k + 1, ] - P[k, ])
      v <- dur * al * (1 - al) * sig1^2 + ((1 - al)^2 + al^2) * sig2^2
      dens <- dens + dur / total / nalpha * exp(-((G$x - mu[1])^2 + (G$y - mu[2])^2) / (2 * v)) / (2 * pi * v)
    }
  }
  D <- matrix(dens, length(ys), length(xs), byrow = TRUE)
  list(ud = D / sum(D), density = D)
}

#' @rdname LatticeRandomWalk
#' @export
SitePercolation <- function(nrow, ncol, p, seed = 1, nsim = 1L) {
  N <- nrow * ncol
  res <- lapply(seq_len(nsim) - 1, function(r) {
    occ <- .morie_random_uniform(N, seed = seed, stream = r) < p
    par <- seq_len(N)
    find <- function(a) {
      while (par[a] != a) {
        par[a] <<- par[par[a]]
        a <- par[a]
      }
      a
    }
    for (idx in which(occ)) {
      i <- (idx - 1) %/% ncol
      j <- (idx - 1) %% ncol
      for (o in list(c(1, 0), c(0, 1))) {
        x <- i + o[1]
        y <- j + o[2]
        if (x < nrow && y < ncol && occ[x * ncol + y + 1]) {
          ra <- find(idx)
          rb <- find(x * ncol + y + 1)
          if (ra != rb) par[max(ra, rb)] <- min(ra, rb)
        }
      }
    }
    lab <- ifelse(occ, vapply(seq_len(N), find, 0), -1)
    L <- matrix(lab, nrow, ncol, byrow = TRUE)
    left <- L[, 1][L[, 1] >= 0]
    right <- L[, ncol][L[, ncol] >= 0]
    tab <- table(lab[lab >= 0])
    list(L = L, nc = length(tab), span = length(intersect(left, right)) > 0,
         lf = if (length(tab)) max(tab) / N else 0)
  })
  span <- vapply(res, `[[`, TRUE, "span")
  list(labels = lapply(res, `[[`, "L"), n_clusters = vapply(res, `[[`, 0L, "nc"), spanning = span,
       largest_fraction = vapply(res, `[[`, 0, "lf"), spanning_probability = mean(span))
}
