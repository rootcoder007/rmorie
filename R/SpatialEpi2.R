# SPDX-License-Identifier: AGPL-3.0-or-later
# Spatial epidemiology: ZIP Gibbs, kernel relative risk, Oden's Ipop, prospective scan and more.
# Identical to the Python arm morie.fn.spepi.

#' Spatial epidemiology: ZIP Gibbs, kernel relative risk, Oden Ipop, prospective scan, ecological regression, wombling
#'
#' \code{ZipGibbs}: Bayesian zero-inflated Poisson relative risks by data
#' augmentation with Marsaglia-Tsang gammas on Philox streams.
#' \code{KernelRelativeRisk}: Kelsall-Diggle Gaussian-kernel log relative risk.
#' \code{OdenIpop}: Oden's population-adjusted Moran statistic with a
#' multinomial Monte Carlo test. \code{ProspectiveScan}: Kulldorff's
#' prospective space-time scan (Poisson, windows ending at the last time).
#' \code{PoissonEcological}: IRLS Poisson regression with offset and
#' quasi-Poisson scale. \code{ArealWombling}: boundary likelihood values on
#' adjacent pairs and barrier edges. \code{BufferRateRatio}: exposure-zone
#' rate ratio with a conditional binomial test. \code{NeedBasedAllocation}:
#' weighted capitation. \code{FunnelControlLimits}: Spiegelhalter funnel
#' limits. \code{GhoseDrugFilter}: the Ghose drug-likeness rule. Indices
#' are 0-based, as the Python arm.
#'
#' @param y,cases Case counts.
#' @param expected Expected counts.
#' @param n_iter,burn Gibbs sweeps and burn-in.
#' @param a,b Gamma prior shape and rate.
#' @param seed Philox seed.
#' @param controls Control point coordinates.
#' @param points Evaluation points.
#' @param bandwidth Kernel bandwidth.
#' @param population Populations.
#' @param W Spatial weights (diagonal allowed).
#' @param nsim Monte Carlo replicates.
#' @param counts Times by regions case counts.
#' @param coords Region centroids.
#' @param max_window Longest time window.
#' @param max_pop_frac Largest share of the expected total in a zone.
#' @param X Covariates (an intercept is added).
#' @param values Area values.
#' @param A Adjacency matrix.
#' @param quantile Barrier quantile.
#' @param sources Point-source coordinates.
#' @param radius Buffer radius.
#' @param budget Total budget.
#' @param need_index Need indices.
#' @param floor_share Share of the budget split equally.
#' @param target Target proportion.
#' @param sizes Denominators or expected counts.
#' @param z Normal quantiles of the limits.
#' @param kind "proportion" or "smr".
#' @param phi Overdispersion factor.
#' @param mw,logp,mr,n_atoms Molecular weight, logP, molar refractivity and atom count.
#' @return A list, vector or logical vector.
#' @references Lambert, D. (1992). Technometrics 34, 1-14. Kelsall, J. E. and
#'   Diggle, P. J. (1995). Statistics in Medicine 14, 2335-2342. Oden, N.
#'   (1995). Statistics in Medicine 14, 17-26. Kulldorff, M. (2001). JRSS A
#'   164, 61-72. Lu, H. and Carlin, B. P. (2005). Geographical Analysis 37,
#'   265-285. Spiegelhalter, D. J. (2005). Statistics in Medicine 24,
#'   1185-1202. Ghose, A. K. et al. (1999). J. Comb. Chem. 1, 55-68.
#' @examples
#' PoissonEcological(c(2, 4, 8), c(1, 1, 1), matrix(0:2))$beta
#' FunnelControlLimits(0.1, 100, z = 2)$upper
#' @export
ZipGibbs <- function(y, expected, n_iter, a = 1, b = 1, burn = 0, seed = 0) {
  y <- as.numeric(y)
  E <- as.numeric(expected)
  n <- length(y)
  theta <- rep(1, n)
  pi_ <- 0.5
  z <- integer(n)
  keep <- 0
  s_theta <- numeric(n)
  s_pi <- 0
  for (t in seq_len(n_iter) - 1) {
    base <- t * (n + 3)
    u <- .morie_random_uniform(n, seed = seed + 7919, stream = t)
    for (i in seq_len(n)) {
      z[i] <- if (y[i] == 0) as.integer(u[i] < pi_ / (pi_ + (1 - pi_) * exp(-E[i] * theta[i]))) else 0L
    }
    for (i in seq_len(n)) theta[i] <- .se_rgamma(a + y[i] * (1 - z[i]), seed, base + i - 1) / (b + E[i] * (1 - z[i]))
    sz <- sum(z)
    g1 <- .se_rgamma(1 + sz, seed, base + n)
    g2 <- .se_rgamma(1 + n - sz, seed, base + n + 1)
    pi_ <- g1 / (g1 + g2)
    if (t >= burn) {
      keep <- keep + 1
      s_pi <- s_pi + pi_
      s_theta <- s_theta + theta
    }
  }
  list(theta = s_theta / keep, pi = s_pi / keep, draws = keep)
}

.se_rgamma <- function(shape, seed, stream) {
  boost <- 1
  a <- shape
  if (a < 1) {
    boost <- .morie_random_uniform(1, seed = seed, stream = 2 * (stream * 64 + 63))^(1 / a)
    a <- a + 1
  }
  d <- a - 1 / 3
  cc <- 1 / sqrt(9 * d)
  for (blk in 0:62) {
    z <- .morie_random_normal(8, seed = seed, stream = 2 * (stream * 64 + blk))
    u <- .morie_random_uniform(8, seed = seed, stream = 2 * (stream * 64 + blk) + 1)
    for (q in 1:8) {
      v <- (1 + cc * z[q])^3
      if (v <= 0) next
      if (log(u[q]) < 0.5 * z[q]^2 + d - d * v + d * log(v)) return(d * v * boost)
    }
  }
  d * boost
}

.se_pts <- function(x) matrix(as.numeric(unlist(x)), ncol = 2, byrow = is.list(x))

#' @rdname ZipGibbs
#' @export
KernelRelativeRisk <- function(cases, controls, points, bandwidth) {
  P1 <- .se_pts(cases)
  P0 <- .se_pts(controls)
  X <- .se_pts(points)
  dens <- function(P) vapply(seq_len(nrow(X)), function(k) {
    sum(exp(-((X[k, 1] - P[, 1])^2 + (X[k, 2] - P[, 2])^2) / (2 * bandwidth^2))) / (nrow(P) * 2 * pi * bandwidth^2)
  }, 0)
  f1 <- dens(P1)
  f0 <- dens(P0)
  list(log_rr = log(f1 / f0), f_cases = f1, f_controls = f0, p_case = nrow(P1) * f1 / (nrow(P1) * f1 + nrow(P0) * f0))
}

#' @rdname ZipGibbs
#' @export
OdenIpop <- function(cases, population, W, nsim = 0, seed = 0) {
  cs <- as.numeric(cases)
  pop <- as.numeric(population)
  W <- unname(as.matrix(W)) * 1
  C <- sum(cs)
  m <- sum(pop)
  b <- C / m
  p <- pop / m
  den <- b * (1 - b) * (m^2 * sum(W * outer(p, p)) - m * sum(diag(W) * p))
  stat <- function(cc) {
    r <- cc / sum(cc)
    (m^2 * sum(W * outer(r - p, r - p)) - m * (1 - 2 * b) * sum(diag(W) * r) - m * b * sum(diag(W) * p)) / den
  }
  obs <- stat(cs)
  cum <- cumsum(p)
  sims <- vapply(seq_len(nsim), function(s) {
    u <- .morie_random_uniform(C, seed = seed, stream = s)
    k <- vapply(u, function(uu) min(which(c(uu <= cum[-length(cum)], TRUE))), 0L)
    stat(tabulate(k, length(p)))
  }, 0)
  list(statistic = obs, p_value = if (nsim) (1 + sum(sims >= obs)) / (nsim + 1) else NaN, simulated = sims)
}

.se_llr <- function(c, e, C) {
  if (c <= e) return(0)
  c * log(c / e) + (if (C - c > 0) (C - c) * log((C - c) / (C - e)) else 0)
}

#' @rdname ZipGibbs
#' @export
ProspectiveScan <- function(counts, expected, coords, max_window = 3, max_pop_frac = 0.5, nsim = 99, seed = 0) {
  Cm <- as.matrix(counts) * 1
  T_ <- nrow(Cm)
  n <- ncol(Cm)
  Ctot <- sum(Cm)
  E <- as.matrix(expected) * Ctot / sum(expected)
  xy <- .se_pts(coords)
  area_e <- colSums(E)
  zones <- list()
  for (i in seq_len(n)) {
    d <- sqrt((xy[, 1] - xy[i, 1])^2 + (xy[, 2] - xy[i, 2])^2)
    o <- order(d, seq_len(n))
    z <- integer(0)
    tot <- 0
    for (j in o) {
      if (tot + area_e[j] > max_pop_frac * Ctot && length(z)) break
      z <- c(z, j)
      tot <- tot + area_e[j]
      zones[[length(zones) + 1]] <- sort(z)
    }
  }
  zones <- unique(zones)
  best <- function(M) {
    tot <- sum(M)
    bl <- 0
    bz <- zones[[1]]
    bw <- 1
    for (z in zones) for (w in seq_len(min(max_window, T_))) {
      rows <- (T_ - w + 1):T_
      v <- .se_llr(sum(M[rows, z]), sum(E[rows, z]), tot)
      if (v > bl + 1e-12) {
        bl <- v
        bz <- z
        bw <- w
      }
    }
    list(llr = bl, zone = bz, w = bw)
  }
  ob <- best(Cm)
  cum <- cumsum(as.numeric(t(E)) / Ctot)
  sims <- vapply(seq_len(nsim), function(s) {
    u <- .morie_random_uniform(Ctot, seed = seed, stream = s)
    k <- vapply(u, function(uu) min(which(c(uu <= cum[-length(cum)], TRUE))), 0L)
    M <- matrix(tabulate(k, T_ * n), T_, n, byrow = TRUE)
    best(M)$llr
  }, 0)
  list(llr = ob$llr, cluster = ob$zone - 1, window = ob$w, p_value = (1 + sum(sims >= ob$llr)) / (nsim + 1))
}

#' @rdname ZipGibbs
#' @export
PoissonEcological <- function(y, expected, X) {
  y <- as.numeric(y)
  off <- log(as.numeric(expected))
  Xr <- cbind(1, unname(as.matrix(X)) * 1)
  n <- nrow(Xr)
  p <- ncol(Xr)
  beta <- c(log(max(sum(y), 0.5) / sum(exp(off))), rep(0, p - 1))
  for (it in seq_len(100)) {
    eta <- off + as.numeric(Xr %*% beta)
    mu <- exp(eta)
    zw <- eta - off + (y - mu) / mu
    nw <- as.numeric(solve(crossprod(Xr, Xr * mu), crossprod(Xr, mu * zw)))
    done <- max(abs(nw - beta)) <= 1e-12
    beta <- nw
    if (done) break
  }
  mu <- exp(off + as.numeric(Xr %*% beta))
  V <- solve(crossprod(Xr, Xr * mu))
  phi <- if (n > p) sum((y - mu)^2 / mu) / (n - p) else NaN
  dev <- 2 * sum(ifelse(y > 0, y * log(y / mu), 0) - (y - mu))
  list(beta = beta, se = sqrt(diag(V)), se_quasi = sqrt(phi * diag(V)), dispersion = phi, deviance = dev, fitted = mu)
}

#' @rdname ZipGibbs
#' @export
ArealWombling <- function(values, A, quantile = 0.8) {
  y <- as.numeric(values)
  idx <- which(upper.tri(A) & A != 0, arr.ind = TRUE)
  idx <- idx[order(idx[, 1], idx[, 2]), , drop = FALSE]
  blv <- abs(y[idx[, 1]] - y[idx[, 2]])
  s <- sort(blv)
  h <- (length(s) - 1) * quantile
  lo <- floor(h)
  w <- h - lo
  thr <- if (w > 0) (1 - w) * s[lo + 1] + w * s[min(lo + 2, length(s))] else s[lo + 1]
  edges <- unname(idx) - 1
  list(edges = edges, blv = blv, threshold = thr, barriers = edges[blv >= thr, , drop = FALSE])
}

#' @rdname ZipGibbs
#' @export
BufferRateRatio <- function(cases, population, coords, sources, radius) {
  xy <- .se_pts(coords)
  S <- .se_pts(sources)
  ins <- vapply(seq_len(nrow(xy)), function(i) any(sqrt((S[, 1] - xy[i, 1])^2 + (S[, 2] - xy[i, 2])^2) <= radius), TRUE)
  ci <- sum(cases[ins])
  co <- sum(cases[!ins])
  ni <- sum(population[ins])
  no <- sum(population[!ins])
  rr <- (ci / ni) / (co / no)
  se <- if (ci > 0 && co > 0) sqrt(1 / ci + 1 / co) else Inf
  C <- ci + co
  q <- ni / (ni + no)
  k <- ci:C
  pv <- sum(exp(lgamma(C + 1) - lgamma(k + 1) - lgamma(C - k + 1) + k * log(q) + (C - k) * log(1 - q)))
  list(rate_ratio = rr, ci = rr * exp(c(-1.96, 1.96) * se), p_value = pv, inside = ins)
}

#' @rdname ZipGibbs
#' @export
NeedBasedAllocation <- function(budget, population, need_index, floor_share = 0) {
  wv <- as.numeric(population) * as.numeric(need_index)
  floor_share * budget / length(wv) + (1 - floor_share) * budget * wv / sum(wv)
}

#' @rdname ZipGibbs
#' @export
FunnelControlLimits <- function(target, sizes, z = c(1.959963984540054, 3.090232306167813), kind = "proportion", phi = 1) {
  n <- as.numeric(sizes)
  lower <- upper <- list()
  for (zz in z) {
    if (kind == "smr") {
      h <- zz * sqrt(phi / n)
      lower[[length(lower) + 1]] <- pmax(1 - h, 0)
      upper[[length(upper) + 1]] <- 1 + h
    } else {
      h <- zz * sqrt(phi * target * (1 - target) / n)
      lower[[length(lower) + 1]] <- pmax(target - h, 0)
      upper[[length(upper) + 1]] <- pmin(target + h, 1)
    }
  }
  list(lower = lower, upper = upper)
}

#' @rdname ZipGibbs
#' @export
GhoseDrugFilter <- function(mw, logp, mr, n_atoms) {
  mw >= 160 & mw <= 480 & logp >= -0.4 & logp <= 5.6 & mr >= 40 & mr <= 130 & n_atoms >= 20 & n_atoms <= 70
}
