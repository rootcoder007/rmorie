#' Point-pattern extras: nearest neighbours, quadrats, Morisita, cross-K, segregation
#'
#' \code{KnnDistances}: distance to the \code{k}-th nearest neighbour (ties by
#' index), as \code{spatstat.geom::nndist(X, k = k)}. \code{QuadratTest}:
#' Pearson \eqn{X^2} of \eqn{n_x \times n_y} equal tiles of a rectangle, with
#' upper-tail and two-sided p-values, as \code{spatstat.explore::quadrat.test}.
#' \code{MorisitaIndex}: Morisita's index, the critical indices and the
#' standardised index, as \code{vegan::dispindmorisita}. \code{CrossK}:
#' cross-type K and L with Ripley's isotropic correction, as \code{Kcross}.
#' \code{SegregationTest}: \eqn{T = \sum_i \sum_m (p_m(x_i) - \bar p_m)^2} with
#' leave-one-out Gaussian-kernel type probabilities and a random-relabelling
#' Monte Carlo p-value, as \code{segregation.test}. Identical to the Python arm
#' \code{morie.fn.pppextra}; indices are 1-based here.
#'
#' @param points Two-column coordinates.
#' @param k Neighbour order.
#' @param window Rectangle \code{c(x0, x1, y0, y1)}.
#' @param nx,ny Tiles along x and y.
#' @param counts Quadrat counts.
#' @param crit Significance level of the critical indices.
#' @param marks Type labels.
#' @param i,j Types.
#' @param r Distances.
#' @param sigma Gaussian kernel bandwidth.
#' @param nsim Number of relabellings.
#' @param seed Philox seed.
#' @return List.
#' @references Thompson, H. R. (1956). Distribution of distance to nth
#'   neighbour in a population of randomly distributed individuals. Ecology
#'   37, 391-394.
#'
#'   Morisita, M. (1959). Measuring of the dispersion of individuals and
#'   analysis of the distributional patterns. Memoirs of the Faculty of
#'   Science, Kyushu University, Series E 2, 215-235.
#'
#'   Lotwick, H. W. and Silverman, B. W. (1982). Methods for analysing spatial
#'   processes of several types of points. JRSS B 44, 406-413.
#'
#'   Diggle, P. J., Zheng, P. and Durr, P. (2005). Nonparametric estimation of
#'   spatial segregation in a multivariate point process. Applied Statistics
#'   54, 645-658.
#' @examples
#' KnnDistances(rbind(c(0, 0), c(1, 0), c(3, 0)), k = 2)$distance
#' MorisitaIndex(c(0, 0, 0, 10))$imor
#' @export
KnnDistances <- function(points, k = 1L) {
  P <- as.matrix(points)
  n <- nrow(P)
  if (k < 1 || k >= n) stop("k must be between 1 and n - 1", call. = FALSE)
  D <- as.matrix(stats::dist(P))
  w <- vapply(seq_len(n), function(a) {
    o <- setdiff(seq_len(n), a)
    o[order(D[a, o], o)][k]
  }, 0L)
  d <- D[cbind(seq_len(n), w)]
  list(distance = d, which = w, mean = mean(d), k = k)
}

#' @rdname KnnDistances
#' @export
QuadratTest <- function(points, window, nx, ny) {
  P <- as.matrix(points)
  i <- pmin(floor((P[, 1] - window[1]) / (window[2] - window[1]) * nx), nx - 1)
  j <- pmin(floor((P[, 2] - window[3]) / (window[4] - window[3]) * ny), ny - 1)
  C <- matrix(0L, ny, nx)
  for (a in seq_len(nrow(P))) C[ny - j[a], i[a] + 1] <- C[ny - j[a], i[a] + 1] + 1L
  k <- nx * ny
  E <- nrow(P) / k
  X2 <- sum((C - E)^2 / E)
  up <- stats::pchisq(X2, k - 1, lower.tail = FALSE)
  list(counts = C, statistic = X2, df = k - 1, p_upper = up, p_value = min(1, 2 * min(up, 1 - up)),
       intensity = nrow(P) / ((window[2] - window[1]) * (window[4] - window[3])))
}

#' @rdname KnnDistances
#' @export
MorisitaIndex <- function(counts, crit = 0.05) {
  x <- as.numeric(counts)
  n <- length(x)
  s <- sum(x)
  imor <- n * (sum(x^2) - s) / (s^2 - s)
  chicr <- stats::qchisq(c(crit / 2, 1 - crit / 2), n - 1, lower.tail = FALSE)
  muni <- (chicr[2] - n + s) / (s - 1)
  mclu <- (chicr[1] - n + s) / (s - 1)
  smor <- imor
  if (s > 1) {
    if (imor >= mclu && mclu > 1) smor <- 0.5 + 0.5 * (imor - mclu) / (n - mclu)
    if (mclu > imor && imor >= 1) smor <- 0.5 * (imor - 1) / (mclu - 1)
    if (1 > imor && imor > muni) smor <- -0.5 * (imor - 1) / (muni - 1)
    if (1 > muni && muni > imor) smor <- -0.5 + 0.5 * (imor - muni) / muni
  }
  list(imor = imor, mclu = mclu, muni = muni, imst = smor,
       p_value = stats::pchisq(imor * (s - 1) + n - s, n - 1, lower.tail = FALSE))
}

#' @rdname KnnDistances
#' @export
CrossK <- function(points, marks, i, j, window, r) {
  P <- as.matrix(points)
  I <- P[marks == i, , drop = FALSE]
  J <- P[marks == j, , drop = FALSE]
  area <- (window[2] - window[1]) * (window[4] - window[3])
  D <- sqrt(outer(I[, 1], J[, 1], "-")^2 + outer(I[, 2], J[, 2], "-")^2)
  E <- matrix(0, nrow(I), nrow(J))
  for (a in seq_len(nrow(I))) {
    for (b in seq_len(nrow(J))) {
      if (D[a, b] > 0) E[a, b] <- 1 / .ripk_weight(I[a, 1], I[a, 2], D[a, b], window[1], window[2], window[3], window[4])
    }
  }
  K <- vapply(r, function(rr) area * sum(E[D > 0 & D <= rr]) / (nrow(I) * nrow(J)), 0)
  list(r = r, K = K, L = sqrt(K / pi), theo = pi * r^2)
}

#' @rdname KnnDistances
#' @export
SegregationTest <- function(points, marks, sigma, nsim = 19L, seed = 1) {
  P <- as.matrix(points)
  n <- nrow(P)
  types <- sort(unique(marks))
  K <- exp(-as.matrix(stats::dist(P))^2 / (2 * sigma^2))
  diag(K) <- 0
  den <- rowSums(K)
  stat <- function(lab) {
    sum(vapply(types, function(m) sum((as.vector(K %*% (lab == m)) / den - mean(lab == m))^2), 0))
  }
  obs <- stat(marks)
  sims <- vapply(seq_len(nsim) - 1, function(s) {
    u <- .morie_random_uniform(n, seed = seed, stream = s)
    lab <- marks
    for (t in seq_len(n - 1)) {
      k <- t + floor(u[t] * (n - t + 1))
      tmp <- lab[t]
      lab[t] <- lab[k]
      lab[k] <- tmp
    }
    stat(lab)
  }, 0)
  list(statistic = obs, simulated = sims, p_value = (1 + sum(sims >= obs)) / (1 + nsim))
}

.potts_nb <- function(nr, nc) {
  lapply(seq_len(nr * nc) - 1, function(s) {
    i <- s %/% nc
    j <- s %% nc
    cand <- rbind(c(i - 1, j), c(i + 1, j), c(i, j - 1), c(i, j + 1))
    ok <- cand[, 1] >= 0 & cand[, 1] < nr & cand[, 2] >= 0 & cand[, 2] < nc
    cand[ok, 1] * nc + cand[ok, 2] + 1
  })
}

#' Potts and Ising lattice models
#'
#' \code{PottsGibbs}: single-site raster-scan Gibbs sampler of
#' \eqn{P(x) \propto \exp(\beta \sum_{i \sim j} 1(x_i = x_j))} on a
#' free-boundary four-neighbour lattice (states \code{0..q-1}; Ising with
#' coupling \eqn{J} is \code{q = 2}, \eqn{\beta = 2J}), full conditionals
#' drawn by inversion of Philox uniforms (stream = sweep); returns the final
#' lattice and per-sweep like-pair counts and state-0 shares.
#' \code{PottsExact}: log partition function and moments of the like-pair
#' count by enumeration (small lattices). Identical to the Python arm
#' \code{morie.fn.pppextra} (Potts 1952; Geman and Geman 1984).
#'
#' @param nrow,ncol Lattice size.
#' @param q Number of states.
#' @param beta Interaction.
#' @param sweeps Number of sweeps.
#' @param seed Philox seed.
#' @param init Optional initial lattice (matrix of states).
#' @return List.
#' @references Potts, R. B. (1952). Some generalized order-disorder
#'   transformations. Mathematical Proceedings of the Cambridge Philosophical
#'   Society 48, 106-109.
#'
#'   Geman, S. and Geman, D. (1984). Stochastic relaxation, Gibbs
#'   distributions, and the Bayesian restoration of images. IEEE PAMI 6,
#'   721-741.
#' @examples
#' PottsExact(1, 2, 2, 0)$mean_like_pairs
#' PottsGibbs(3, 3, 2, 50, sweeps = 5, init = matrix(0, 3, 3))$like_pairs
#' @export
PottsGibbs <- function(nrow, ncol, q, beta, sweeps = 100L, seed = 1, init = NULL) {
  nb <- .potts_nb(nrow, ncol)
  N <- nrow * ncol
  x <- if (is.null(init)) pmin(q - 1, floor(.morie_random_uniform(N, seed = seed, stream = 1e6) * q)) else
    as.vector(t(as.matrix(init)))
  like <- integer(sweeps)
  share <- numeric(sweeps)
  for (s in seq_len(sweeps)) {
    u <- .morie_random_uniform(N, seed = seed, stream = s - 1)
    for (i in seq_len(N)) {
      cnt <- tabulate(x[nb[[i]]] + 1, q)
      w <- exp(beta * (cnt - max(cnt)))
      t <- u[i] * sum(w)
      a <- 1
      acc <- w[1]
      while (acc < t && a < q) {
        a <- a + 1
        acc <- acc + w[a]
      }
      x[i] <- a - 1
    }
    like[s] <- sum(vapply(seq_len(N), function(i) sum(nb[[i]] > i & x[nb[[i]]] == x[i]), 0L))
    share[s] <- mean(x == 0)
  }
  list(lattice = matrix(x, nrow, ncol, byrow = TRUE), like_pairs = like, share_state0 = share)
}

#' @rdname PottsGibbs
#' @export
PottsExact <- function(nrow, ncol, q, beta) {
  nb <- .potts_nb(nrow, ncol)
  N <- nrow * ncol
  pairs <- do.call(rbind, lapply(seq_len(N), function(i) {
    j <- nb[[i]][nb[[i]] > i]
    if (length(j)) cbind(i, j) else NULL
  }))
  st <- as.matrix(expand.grid(rep(list(0:(q - 1)), N)))
  S <- rowSums(st[, pairs[, 1], drop = FALSE] == st[, pairs[, 2], drop = FALSE])
  m <- max(beta * S)
  w <- exp(beta * S - m)
  z <- sum(w)
  e1 <- sum(S * w) / z
  list(log_z = m + log(z), mean_like_pairs = e1, var_like_pairs = sum(S^2 * w) / z - e1^2, n_pairs = nrow(pairs))
}
