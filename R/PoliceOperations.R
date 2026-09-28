#' Police operations research models
#'
#' \code{HypercubeQueue}: Larson (1974) hypercube queueing model (zero or
#' infinite line) with unit workloads, dispatch fractions, loss or waiting
#' probability and travel times. \code{SquareRootLaw}: Kolesar-Blum response
#' distance. \code{ShortCrimeLattice}: Short et al. (2008) agent burglary
#' model on a periodic lattice. \code{ShortCrimePde}: its continuum
#' reaction-diffusion version with hotspot suppression. Units and atoms are
#' 0-based in \code{preferences}, as in the Python arm \code{morie.fn.policeops}.
#'
#' @param arrival_rates Call rates by atom.
#' @param preferences List of unit dispatch orders (0-based) per atom.
#' @param service_rate Service rate per unit.
#' @param queue \code{"zero"} or \code{"infinite"}.
#' @param travel_time Units x atoms travel times.
#' @param area,available_units Area and available units.
#' @param metric \code{"euclidean"} or \code{"rectilinear"}.
#' @param speed Travel speed.
#' @param n Lattice side.
#' @param steps Time steps.
#' @param A0,eta,omega,gamma Model parameters.
#' @param B0 Initial dynamic attractiveness (length \code{n^2}).
#' @param criminals Initial criminal sites (0-based).
#' @param seed Philox seed.
#' @param B,rho Initial fields (matrices).
#' @param dx,dt Grid spacing and time step.
#' @param D,eps,z Continuum constants.
#' @param suppress 0/1 matrix of policed cells.
#' @return List.
#' @references Larson, R. C. (1974). A hypercube queuing model for facility
#'   location and redistricting in urban emergency services. Computers and
#'   Operations Research 1, 67-95.
#'
#'   Kolesar, P. and Blum, E. H. (1973). Square root laws for fire engine
#'   response distances. Management Science 19, 1368-1378.
#'
#'   Short, M. B. et al. (2008). A statistical model of criminal behavior.
#'   Mathematical Models and Methods in Applied Sciences 18, 1249-1267.
#' @examples
#' HypercubeQueue(1, list(c(0, 1)))$loss
#' SquareRootLaw(100, 4)$distance
#' @export
HypercubeQueue <- function(arrival_rates, preferences, service_rate = 1, queue = "zero", travel_time = NULL) {
  lam <- arrival_rates
  N <- max(unlist(preferences)) + 1
  if (N > 10) stop("at most 10 units (2^N states)")
  mu <- service_rate
  S <- 2^N
  full <- S - 1
  Lam <- sum(lam)
  rho <- Lam / (N * mu)
  if (queue == "infinite" && rho >= 1) stop("infinite-line model needs Lambda < N mu")
  busy <- function(b, u) bitwAnd(b, 2^u) != 0
  first_free <- function(b, j) {
    p <- preferences[[j]]
    p[!vapply(p, function(u) busy(b, u), TRUE)][1]
  }
  Q <- matrix(0, S, S)
  for (b in 0:(S - 1)) {
    if (b != full) {
      for (j in seq_along(lam)) {
        u <- first_free(b, j)
        Q[b + 1, b + 2^u + 1] <- Q[b + 1, b + 2^u + 1] + lam[j]
      }
    }
    sc <- if (b == full && queue == "infinite") 1 - rho else 1
    for (i in 0:(N - 1)) if (busy(b, i)) Q[b + 1, b - 2^i + 1] <- Q[b + 1, b - 2^i + 1] + mu * sc
    Q[b + 1, b + 1] <- -sum(Q[b + 1, -(b + 1)])
  }
  A <- t(Q)
  A[S, ] <- 1
  pi <- solve(A, c(rep(0, S - 1), 1))
  work <- vapply(0:(N - 1), function(n) sum(pi[vapply(0:(S - 1), function(b) busy(b, n), TRUE)]), 0)
  disp <- matrix(0, N, length(lam))
  for (b in 0:(S - 2)) {
    for (j in seq_along(lam)) {
      u <- first_free(b, j)
      disp[u + 1, j] <- disp[u + 1, j] + lam[j] * pi[b + 1] / Lam
    }
  }
  out <- list(state_prob = pi, workload = work, dispatch = disp)
  if (queue == "zero") {
    out$loss <- pi[S]
  } else {
    out$wait <- pi[S]
    out$mean_queue <- pi[S] * rho / (1 - rho)
  }
  if (!is.null(travel_time)) {
    Tm <- as.matrix(travel_time)
    out$mean_travel_time <- sum(disp * Tm) / sum(disp)
    out$atom_travel_time <- colSums(disp * Tm) / colSums(disp)
  }
  out
}

#' @rdname HypercubeQueue
#' @export
SquareRootLaw <- function(area, available_units, metric = "euclidean", speed = NULL) {
  cc <- switch(metric, euclidean = 0.5, rectilinear = sqrt(2 * pi) / 4, stop("metric must be euclidean or rectilinear"))
  d <- cc * sqrt(area / available_units)
  out <- list(distance = d, constant = cc)
  if (!is.null(speed)) out$time <- d / speed
  out
}

.pc_poisson_inv <- function(u, lam) {
  k <- 0
  p <- exp(-lam)
  cc <- p
  while (u > cc && k < 1000) {
    k <- k + 1
    p <- p * lam / k
    cc <- cc + p
  }
  k
}

#' @rdname HypercubeQueue
#' @export
ShortCrimeLattice <- function(n, steps, A0 = 1 / 30, eta = 0.03, omega = 1 / 15, gamma = 0.002, B0 = NULL,
                              criminals = NULL, seed = 1) {
  N <- n * n
  B <- if (is.null(B0)) numeric(N) else B0
  crim <- if (is.null(criminals)) integer(0) else criminals
  nb <- function(s) {
    i <- s %/% n
    j <- s %% n
    c(((i - 1) %% n) * n + j, ((i + 1) %% n) * n + j, i * n + (j - 1) %% n, i * n + (j + 1) %% n)
  }
  counts <- numeric(0)
  meanB <- numeric(0)
  for (t in seq_len(steps) - 1) {
    u <- .morie_random_uniform(2 * length(crim) + N, seed = seed, stream = t)
    E <- numeric(N)
    k <- 1
    surv <- integer(0)
    for (s in crim) {
      if (u[k] < 1 - exp(-(A0 + B[s + 1]))) E[s + 1] <- E[s + 1] + 1 else surv <- c(surv, s)
      k <- k + 1
    }
    moved <- integer(0)
    for (s in surv) {
      nbs <- nb(s)
      w <- A0 + B[nbs + 1]
      r <- u[k] * sum(w)
      dest <- nbs[which(r < cumsum(w))[1]]
      if (is.na(dest)) dest <- nbs[4]
      moved <- c(moved, dest)
      k <- k + 1
    }
    k <- 2 * length(crim)
    for (s in 0:(N - 1)) moved <- c(moved, rep(s, .pc_poisson_inv(u[k + s + 1], gamma)))
    B <- vapply(0:(N - 1), function(s) ((1 - eta) * B[s + 1] + eta / 4 * sum(B[nb(s) + 1])) * (1 - omega) + E[s + 1], 0)
    crim <- moved
    counts <- c(counts, sum(E))
    meanB <- c(meanB, mean(B))
  }
  list(B = B, criminals = crim, burglaries = counts, mean_B = meanB)
}

#' @rdname HypercubeQueue
#' @export
ShortCrimePde <- function(B, rho, dx = 1, dt = 0.01, steps = 100, eta = 0.03, omega = 1 / 15, A0 = 1 / 30,
                          gamma = 0.002, D = 1, eps = 1, z = 4, suppress = NULL) {
  Bg <- as.matrix(B)
  Rg <- as.matrix(rho)
  ny <- nrow(Bg)
  nx <- ncol(Bg)
  Sp <- if (is.null(suppress)) matrix(0, ny, nx) else as.matrix(suppress)
  up <- c(ny, seq_len(ny - 1))
  dn <- c(seq(2, ny), 1)
  lf <- c(nx, seq_len(nx - 1))
  rt <- c(seq(2, nx), 1)
  for (s in seq_len(steps)) {
    A <- A0 + Bg
    shifts <- list(Bg[up, ], Bg[dn, ], Bg[, lf], Bg[, rt])
    lapB <- (Reduce(`+`, shifts) - 4 * Bg) / dx^2
    lapR <- (Rg[up, ] + Rg[dn, ] + Rg[, lf] + Rg[, rt] - 4 * Rg) / dx^2
    q <- Rg / A
    adv <- ((q + q[up, ]) / 2 * (A[up, ] - A) + (q + q[dn, ]) / 2 * (A[dn, ] - A) +
              (q + q[, lf]) / 2 * (A[, lf] - A) + (q + q[, rt]) / 2 * (A[, rt] - A)) / dx^2
    crime <- ifelse(Sp != 0, 0, Rg * A)
    nB <- Bg + dt * (eta * D / z * lapB - omega * Bg + eps * D * crime)
    nR <- Rg + dt * (D / z * (lapR - 2 * adv) - crime + gamma)
    Bg <- nB
    Rg <- nR
  }
  bstar <- if (omega > 0) eps * D * gamma / omega else NaN
  list(B = Bg, rho = Rg, B_star = bstar, rho_star = gamma / (A0 + bstar))
}
