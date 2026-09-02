# SPDX-License-Identifier: AGPL-3.0-or-later
#
# MCMC/SMC shelf, wave3 w5_09 batch 3: ABC rejection (Abcrej), SIR
# importance resampling (Bayisr), Meng-Wong bridge sampling (Bridgs),
# parallel tempering (Ptmcmc). Bit-identical mirrors of
# src/morie/fn/{abcrej,bayisr,bridgs,ptmcmc}.py. Stochastic arms
# consume the shared SplitMix64 stream via .ghc_rng/.ghc_unif/
# .ghc_norm (aaa_helpers_ghc_rng.R), so parity is bit-exact given the
# same seed and stream-aligned callables.

#' ABC rejection sampler
#'
#' Draw theta from a uniform prior box, simulate summaries, accept iff
#' the Euclidean distance to the observed summaries is <= eps
#' (Pritchard et al. 1999; algorithm as stated in Sisson-Fan-Beaumont
#' 2018, Sec 1.5).
#'
#' @param sim function(theta, e) returning a summary vector; e is the
#'   shared RNG environment (use .ghc_* helpers for stochastic sims).
#' @param obs Observed summary vector.
#' @param eps Acceptance tolerance (> 0).
#' @param prior List of c(low, high) pairs (uniform box).
#' @param n_draws Number of prior draws.
#' @param seed SplitMix64 seed.
#' @return List with \code{samples}, \code{n_accepted},
#'   \code{acceptance_rate}, \code{distances}, \code{posterior_mean},
#'   \code{eps}, \code{n_draws}, \code{seed}, \code{method}.
#' @references Pritchard, J. K., Seielstad, M. T., Perez-Lezaun, A.
#'   and Feldman, M. W. (1999), Molecular Biology and Evolution
#'   16(12), 1791-1798; Sisson, S. A., Fan, Y. and Beaumont, M. A.
#'   (2018), arXiv:1802.09720, Sec 1.5 (local:
#'   fetched-wave3/sisson-2018-abc-overview.pdf).
#' @export
#' @examples
#' sim <- function(theta, e) theta[1] + 0.1 * .ghc_norm(e, 1L)
#' r <- Abcrej(sim, obs = 1.5, eps = 0.3, prior = list(c(0, 3)),
#'             n_draws = 400)
#' str(r, max.level = 1)
Abcrej <- function(sim, obs, eps, prior, n_draws = 1000L, seed = 0L) {
  obs <- as.numeric(obs)
  eps <- as.numeric(eps)[1]
  if (eps <= 0) stop("eps must be positive", call. = FALSE)
  bounds <- lapply(prior, function(p) c(as.numeric(p[1]), as.numeric(p[2])))
  if (any(vapply(bounds, function(b) b[2] <= b[1], logical(1)))) {
    stop("each prior pair must satisfy low < high", call. = FALSE)
  }
  e <- .ghc_rng(seed)
  accepted <- list(); dists <- numeric(0)
  for (i in seq_len(as.integer(n_draws))) {
    theta <- vapply(bounds, function(b) .ghc_unif(e, 1L, b[1], b[2]), 0)
    s <- as.numeric(sim(theta, e))
    if (length(s) != length(obs)) {
      stop("sim() must return summaries matching obs", call. = FALSE)
    }
    d <- sqrt(sum((s - obs)^2))
    if (d <= eps) {
      accepted[[length(accepted) + 1L]] <- theta
      dists <- c(dists, d)
    }
  }
  k <- length(accepted)
  pm <- if (k) {
    p <- length(bounds)
    vapply(seq_len(p), function(j) sum(vapply(accepted, `[`, 0, j)) / k, 0)
  } else rep(NaN, length(bounds))
  list(samples = accepted, n_accepted = k,
       acceptance_rate = k / as.integer(n_draws), distances = dists,
       posterior_mean = pm, eps = eps, n_draws = as.integer(n_draws),
       seed = as.integer(seed),
       method = "ABC rejection (Pritchard et al. 1999)")
}

#' Sampling-importance-resampling (SIR)
#'
#' Importance weights w_i = exp(log p - log g) (max-shifted), then m
#' resampled draws with replacement, P(i) proportional to w_i, via
#' one uniform per draw and inverse-CDF on the unnormalized cumulative
#' weights (bit-identical to the Python arm).
#'
#' @param samples Draws from the proposal (vector or list of vectors).
#' @param log_target,log_proposal Log densities up to constants.
#' @param m Resample size.
#' @param seed SplitMix64 seed.
#' @return List with \code{resample}, \code{indices} (0-based, matching
#'   the Python arm), \code{weights}, \code{ess}, \code{n}, \code{m},
#'   \code{seed}, \code{method}.
#' @references Rubin, D. B. (1988), in Bayesian Statistics 3, Oxford
#'   UP, 395-402; Smith, A. F. M. and Gelfand, A. E. (1992), The
#'   American Statistician 46(2), 84-88.
#' @export
#' @examples
#' set.seed(1)
#' xs <- rnorm(500, 0, 2)
#' r <- Bayisr(xs, log_target = function(x) -0.5 * x^2,
#'             log_proposal = function(x) -0.5 * (x / 2)^2 - log(2),
#'             m = 100)
#' str(r, max.level = 1)
Bayisr <- function(samples, log_target, log_proposal, m, seed = 0L) {
  xs <- if (is.list(samples)) samples else as.list(as.numeric(samples))
  n <- length(xs)
  if (n == 0L) stop("`samples` must be non-empty", call. = FALSE)
  m <- as.integer(m)
  if (m < 1L) stop("m must be a positive integer", call. = FALSE)
  logw <- vapply(xs, function(x) as.numeric(log_target(x)) - as.numeric(log_proposal(x)), 0)
  mx <- max(logw)
  w <- exp(logw - mx)
  tot <- sum(w)
  wbar <- w / tot
  ess <- 1 / sum(wbar * wbar)
  e <- .ghc_rng(seed)
  cum <- cumsum(w)
  idx <- integer(m)
  for (t in seq_len(m)) {
    u <- .ghc_unif(e, 1L) * tot
    j <- n
    for (s in seq_len(n)) {
      if (u <= cum[s]) { j <- s; break }
    }
    idx[t] <- j
  }
  list(resample = xs[idx], indices = idx - 1L, weights = wbar,
       ess = ess, n = n, m = m, seed = as.integer(seed),
       method = "SIR weighted bootstrap (Rubin 1988; Smith-Gelfand 1992)")
}

#' Bridge sampling ratio of normalizing constants (Meng-Wong)
#'
#' Iterative optimal-bridge estimate of r = c1/c2 (Meng and Wong 1996,
#' eq. 3.5 and Sec. 4), with a common log-shift for overflow safety
#' that cancels exactly.
#'
#' @param draws1,draws2 Draws from p1 and p2.
#' @param log_q1,log_q2 Log unnormalized densities.
#' @param tol Relative fixed-point tolerance on log r.
#' @param max_iter Iteration cap.
#' @return List with \code{estimate} (r), \code{log_r},
#'   \code{n_iter}, \code{converged}, \code{method}.
#' @references Meng, X.-L. and Wong, W. H. (1996), Statistica Sinica
#'   6, 831-860, eq. (3.5), Sec. 4 (local:
#'   fetched-wave3/meng-wong-1996-bridge-sampling-statsinica.pdf).
#' @export
#' @examples
#' set.seed(2)
#' d1 <- rnorm(400, 0, 1)
#' d2 <- rnorm(400, 0.5, 1.2)
#' r <- Bridgs(d1, d2,
#'             log_q1 = function(x) -0.5 * x^2,
#'             log_q2 = function(x) -0.5 * ((x - 0.5) / 1.2)^2)
#' str(r, max.level = 1)
Bridgs <- function(draws1, draws2, log_q1, log_q2, tol = 1e-12,
                   max_iter = 1000L) {
  d1 <- if (is.list(draws1)) draws1 else as.list(as.numeric(draws1))
  d2 <- if (is.list(draws2)) draws2 else as.list(as.numeric(draws2))
  n1 <- length(d1); n2 <- length(d2)
  if (n1 == 0L || n2 == 0L) stop("draws must be non-empty", call. = FALSE)
  l1 <- vapply(d1, function(x) as.numeric(log_q1(x)) - as.numeric(log_q2(x)), 0)
  l2 <- vapply(d2, function(x) as.numeric(log_q1(x)) - as.numeric(log_q2(x)), 0)
  shift <- max(c(l1, l2))
  e1 <- exp(l1 - shift); e2 <- exp(l2 - shift)
  s1 <- n1 / (n1 + n2); s2 <- n2 / (n1 + n2)
  r <- 1
  converged <- FALSE
  it <- 0L
  for (it in seq_len(as.integer(max_iter))) {
    num <- sum(e2 / (s1 * e2 + s2 * r)) / n2
    den <- sum(1 / (s1 * e1 + s2 * r)) / n1
    r_new <- num / den
    if (abs(log(r_new) - log(r)) <= tol * (1 + abs(log(r_new)))) {
      r <- r_new; converged <- TRUE; break
    }
    r <- r_new
  }
  log_ratio <- log(r) + shift
  list(ratio = exp(log_ratio), log_ratio = log_ratio, iterations = it,
       converged = converged, n1 = n1, n2 = n2,
       method = "Meng-Wong iterative optimal bridge (eq. 3.5 / Sec. 4)")
}

#' Parallel tempering (replica exchange) MCMC
#'
#' K random-walk Metropolis replicas at ascending temperatures with
#' neighbour swaps accepted with probability
#' min(1, exp((1/T_k - 1/T_{k+1})(lp_{k+1} - lp_k))) (Earl-Deem 2005
#' eq. 4; Hukushima-Nemoto 1996). Proposal sd scales with sqrt(T_k).
#' Bit-identical stream consumption with the Python arm: one normal
#' and one uniform per replica per sweep, one uniform per swap
#' attempt.
#'
#' @param log_p Log target density (the T = 1 target).
#' @param temperatures Ascending positive temperatures, first 1.0.
#' @param x0 Scalar start.
#' @param n_iter Sweeps.
#' @param step Base proposal sd.
#' @param seed SplitMix64 seed.
#' @param swap_every Swap attempt period.
#' @return List with \code{chain} (cold trace), \code{chains_last},
#'   \code{accept_rate}, \code{swap_accept_rate},
#'   \code{temperatures}, \code{n_iter}, \code{seed}, \code{method}.
#' @references Earl, D. J. and Deem, M. W. (2005), Phys. Chem. Chem.
#'   Phys. 7, 3910-3916, eq. 4 (local: fetched-wave3/
#'   earl-deem-2005-parallel-tempering.pdf); Hukushima, K. and Nemoto,
#'   K. (1996), J. Phys. Soc. Japan 65(6), 1604-1608 (local:
#'   fetched-wave3/hukushima-nemoto-1996-exchange-mc.pdf); Metropolis
#'   et al. (1953), J. Chem. Phys. 21, 1087-1092.
#' @export
#' @examples
#' r <- Ptmcmc(function(x) -0.5 * ((x - 1)^2) * ((x + 1)^2),
#'             temperatures = c(1, 2, 4), x0 = 0, n_iter = 400)
#' str(r, max.level = 1)
Ptmcmc <- function(log_p, temperatures, x0, n_iter = 1000L, step = 1,
                   seed = 0L, swap_every = 1L) {
  temps <- as.numeric(temperatures)
  K <- length(temps)
  if (K < 2L) stop("need at least two temperatures", call. = FALSE)
  if (any(temps <= 0) || any(diff(temps) <= 0)) {
    stop("temperatures must be positive and ascending", call. = FALSE)
  }
  n_iter <- as.integer(n_iter)
  e <- .ghc_rng(seed)
  x <- rep(as.numeric(x0), K)
  lp <- vapply(x, function(v) as.numeric(log_p(v)), 0)
  acc <- rep(0L, K)
  swap_try <- rep(0L, K - 1L); swap_acc <- rep(0L, K - 1L)
  cold <- numeric(n_iter)
  for (sweep in seq_len(n_iter)) {
    for (k in seq_len(K)) {
      prop <- x[k] + step * sqrt(temps[k]) * .ghc_norm(e, 1L)
      lpp <- as.numeric(log_p(prop))
      u <- .ghc_unif(e, 1L)
      if (log(u) < (lpp - lp[k]) / temps[k]) {
        x[k] <- prop; lp[k] <- lpp; acc[k] <- acc[k] + 1L
      }
    }
    if (sweep %% as.integer(swap_every) == 0L) {
      for (k in seq_len(K - 1L)) {
        swap_try[k] <- swap_try[k] + 1L
        delta <- (1 / temps[k] - 1 / temps[k + 1L]) * (lp[k + 1L] - lp[k])
        u <- .ghc_unif(e, 1L)
        if (log(u) < min(0, delta)) {
          tmp <- x[k]; x[k] <- x[k + 1L]; x[k + 1L] <- tmp
          tmp <- lp[k]; lp[k] <- lp[k + 1L]; lp[k + 1L] <- tmp
          swap_acc[k] <- swap_acc[k] + 1L
        }
      }
    }
    cold[sweep] <- x[1L]
  }
  list(chain = cold, chains_last = x,
       accept_rate = acc / n_iter,
       swap_accept_rate = ifelse(swap_try > 0, swap_acc / swap_try, NaN),
       temperatures = temps, n_iter = n_iter, seed = as.integer(seed),
       method = "Parallel tempering (Earl-Deem 2005 eq. 4; Hukushima-Nemoto 1996)")
}
