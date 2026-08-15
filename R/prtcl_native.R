# Sequential Monte Carlo: the bootstrap particle filter.
# Sources: King, A. A., Nguyen, D. & Ionides, E. L. (2016) "Statistical
# Inference for Partially Observed Markov Processes: The R Package
# pomp", Journal of Statistical Software 69(12), 1-43,
# doi:10.18637/jss.v069.i12 (Algorithm 1 and Algorithm 2, systematic
# resampling). Doucet & Johansen (2011) Handbook of Nonlinear
# Filtering, Ch. 1. Kalman (1960) J. Basic Eng. 82(1) (the closed
# form the filter must reproduce on a linear-Gaussian model). Mirroring
# morie.fn.prtcl: same Algorithm 1 / Algorithm 2 with the same ESS rule
# and the same downward bias in log mean weight.

.prtcl_EPS <- 1e-300

morie_prtcl_effective_sample_size <- function(weights) {
  s1 <- sum(weights); s2 <- sum(weights^2)
  if (s2 <= 0) return(0)
  s1 * s1 / s2
}

morie_prtcl_systematic_resample <- function(weights, u = NULL, e = NULL) {
  J <- length(weights)
  tot <- sum(weights)
  if (tot <= 0) stop("prtcl: all particle weights are zero; the filter has lost the signal")
  w <- weights / tot
  if (is.null(u)) {
    u <- if (is.null(e)) 0.5 else .ghc_unif(e, 1L)
  }
  if (!(u >= 0 && u < 1))
    stop(paste0("prtcl: the offset must lie in [0, 1), got ", u))
  idx <- integer(J); cum <- w[1]; j <- 1L
  for (m in seq_len(J)) {
    pos <- (m - 1L + u) / J
    while (pos > cum && j < J) {
      j <- j + 1L
      cum <- cum + w[j]
    }
    idx[m] <- j
  }
  idx
}

.scalar <- function(state) {
  if (is.list(state)) state[[1]] else if (length(state) > 1L) state[1] else as.numeric(state)
}

.multinomial <- function(w, e) {
  tot <- sum(w)
  J <- length(w)
  cum <- cumsum(w / tot)
  out <- integer(J)
  for (k in seq_len(J)) {
    u <- .ghc_unif(e, 1L)
    j <- 1L
    while (j < J && u > cum[j]) j <- j + 1L
    out[k] <- j
  }
  out
}

morie_prtcl_particle_filter <- function(y, n.particles, init, step, loglik,
                                       seed = 0L, resample.threshold = 1.0,
                                       systematic = TRUE) {
  obs <- as.numeric(y); N <- length(obs)
  J <- as.integer(n.particles)
  if (J < 2L) stop(paste0("prtcl: need at least 2 particles, got ", J))
  if (N == 0L) stop("prtcl: no observations")
  if (!(resample.threshold > 0 && resample.threshold <= 1))
    stop(paste0("prtcl: resample_threshold must be in (0, 1], got ",
                resample.threshold))
  e <- .ghc_rng(as.integer(seed))
  parts <- vector("list", J)
  for (j in seq_len(J)) parts[[j]] <- init(e)
  ll <- 0
  means <- numeric(N); esss <- numeric(N); resampled <- logical(N)
  for (n in seq_len(N)) {
    for (j in seq_len(J)) parts[[j]] <- step(parts[[j]], n - 1L, e)
    lw <- vapply(seq_len(J), function(j) loglik(parts[[j]], obs[n], n - 1L),
                 numeric(1))
    mx <- max(lw)
    if (mx == -Inf)
      stop(paste0("prtcl: every particle has zero likelihood at observation ",
                  n - 1L))
    w <- exp(lw - mx)
    tot <- sum(w)
    ll <- ll + mx + log(tot / J)
    ess <- morie_prtcl_effective_sample_size(w)
    esss[n] <- ess
    means[n] <- sum(vapply(seq_len(J), function(j) w[j] * .scalar(parts[[j]]),
                           numeric(1))) / tot
    if (ess < resample.threshold * J) {
      idx <- if (systematic) morie_prtcl_systematic_resample(w, e = e)
             else .multinomial(w, e)
      parts <- parts[idx]
      resampled[n] <- TRUE
    } else resampled[n] <- FALSE
  }
  list(estimate = means, filtered.mean = means, loglik = ll,
       ess = esss, min.ess = min(esss), resampled = resampled,
       n.particles = J, n.obs = N, systematic = systematic,
       particles = parts,
       method = ("bootstrap particle filter, King, Nguyen & Ionides (2016) Algorithm 1 with systematic resampling (Algorithm 2)"))
}

morie_prtcl_kalman_filter_1d <- function(y, a, q, c, r, m0 = 0, p0 = 1) {
  m <- as.numeric(m0); p <- as.numeric(p0)
  N <- length(y); means <- numeric(N); ll <- 0
  for (n in seq_len(N)) {
    m <- a * m
    p <- a * a * p + q
    s <- c * c * p + r
    v <- y[n] - c * m
    ll <- ll - 0.5 * (log(2 * pi * s) + v * v / s)
    gain <- p * c / s
    m <- m + gain * v
    p <- (1 - gain * c) * p
    means[n] <- m
  }
  list(means = means, loglik = ll)
}

# house entry point: the package exports one morie_<module>
morie_prtcl <- morie_prtcl_effective_sample_size
