# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of snpest -- sequential (on-line) density estimation under a
# Dirichlet-process mixture filter. Mirrors src/morie/fn/snpest.py
# operation for operation, on the shared numerics in
# R/aaa_helpers_w3num.R and the generator in R/aaa_helpers_ghc_rng.R.
#
# Batch MCMC for a DP mixture sees the whole sample before it says
# anything. A filter sees y_1, then y_2, and must have an answer after
# each one, never revisiting what it already processed. That constraint
# is not a weakness to apologise for -- it is the whole point when the
# data arrives as a stream and a decision is due before the next
# observation.
#
# The state is the ALLOCATION of the points seen so far. The component
# parameters are not part of it: with a conjugate normal / inverse-gamma
# prior they integrate out exactly, so a cluster is carried as three
# sufficient statistics (count, sum, sum of squares) and its predictive
# density for the next point is a Student t. That is
# Rao-Blackwellisation, and it is what makes a particle filter over
# allocations feasible at all -- particles over (allocation, parameters)
# would need a Metropolis move per step and would degenerate.
#
# At step t a particle holding an allocation of y_1..y_{t-1} extends it
# by assigning y_t. The Polya urn gives the prior:
#
#     P(join cluster j) proportional to n_j
#     P(start a new one) proportional to alpha
#
# and the conjugate predictive gives the likelihood. Two proposals:
#
#   "optimal"  sample from the EXACT conditional -- prior times
#              likelihood, normalised -- and multiply the weight by that
#              normalising constant. The minimum-variance proposal for
#              this model (MacEachern, Clyde and Liu; Fearnhead). Every
#              particle survives every step with a well-behaved weight.
#   "prior"    sample from the Polya urn alone and weight by the
#              likelihood. Cheaper per step and much worse: a particle
#              that guesses badly gets a tiny weight, so the population
#              degenerates and the filter spends its life resampling.
#              Kept because the difference between the two is the most
#              instructive thing about this algorithm, and it should be
#              measurable rather than asserted -- the harness anchors on
#              the effective sample size of each.
#
# Resampling fires when the effective sample size falls below a
# threshold. Three schemes, all exact: "multinomial" (n independent
# draws, highest variance), "stratified" (one draw per stratum of width
# 1/n) and "systematic" (ONE draw then a regular comb -- lowest variance
# of the three, and the correlation it introduces between selections is
# exactly what removes the variance).
#
# References
#   MacEachern, S.N., Clyde, M. and Liu, J.S. (1999) "Sequential
#     importance sampling for nonparametric Bayes models: the next
#     generation." Canadian Journal of Statistics 27(2), 251-267.
#   Fearnhead, P. (2004) "Particle filters for mixture models with an
#     unknown number of components." Statistics and Computing 14(1),
#     11-21.
#   Caron, F., Doucet, A. and Gottardo, R. (2012) "On-line changepoint
#     detection and parameter estimation with application to genomic
#     data." Statistics and Computing 22(2), 579-595. The ledger cites
#     this as 2017; the paper is 2012, and the year is corrected here
#     rather than repeated.
#   Escobar, M.D. and West, M. (1995) JASA 90(430), 577-588.

.SNPEST_PROPOSALS <- c("optimal", "prior")
.SNPEST_RESAMPLERS <- c("systematic", "stratified", "multinomial")

#' log density of a Student t
#'
#' Written out rather than taken from dt(): the two languages carry
#' separate implementations and they do not agree in the last digits.
#'
#' @param x The point.
#' @param df Degrees of freedom.
#' @param loc Location.
#' @param scale2 Squared scale.
#' @return The log density.
#' @export
morie_snpest_t_logpdf <- function(x, df, loc, scale2) {
  z <- (x - loc) * (x - loc) / (df * scale2)
  .w3_lgamma(0.5 * (df + 1)) - .w3_lgamma(0.5 * df) -
    0.5 * log(df * pi * scale2) - 0.5 * (df + 1) * log(1 + z)
}

#' log p(x | cluster with sufficient statistics)
#'
#' n = 0 gives the prior predictive, which is what a brand-new cluster
#' uses -- the same expression, so there is no separate code path to get
#' wrong.
#'
#' @param x The point.
#' @param n Cluster size.
#' @param s Sum of its members.
#' @param ss Sum of their squares.
#' @param m0 Prior mean.
#' @param kappa0 Prior precision scaling.
#' @param a0 Inverse-gamma shape.
#' @param b0 Inverse-gamma scale.
#' @return The log predictive density.
#' @export
morie_snpest_predictive <- function(x, n, s, ss, m0, kappa0, a0, b0) {
  if (n > 0) {
    ybar <- s / n
    sse <- ss - s * s / n
    kn <- kappa0 + n
    mn <- (kappa0 * m0 + s) / kn
    an <- a0 + 0.5 * n
    bn <- b0 + 0.5 * sse + 0.5 * kappa0 * n * (ybar - m0) * (ybar - m0) / kn
  } else {
    kn <- kappa0
    mn <- m0
    an <- a0
    bn <- b0
  }
  df <- 2 * an
  scale2 <- bn * (kn + 1) / (an * kn)
  morie_snpest_t_logpdf(x, df, mn, scale2)
}

# Indices of the resampled particles; weights assumed normalised.
#' Indices of the resampled particles; weights assumed normalised
#'
#' A step of the snpest_native implementation. Called by \code{morie_snpest}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param e Passed to \code{.ghc_unif}.
#' @param weights A vector; its length is taken and its elements indexed.
#' @param scheme One of \code{"multinomial"}, \code{"stratified"}, \code{"systematic"}.
#' @return The value of \code{out}, as built in the body.
#' @export
.snpest_resample <- function(e, weights, scheme) {
  n <- length(weights)
  if (scheme == "multinomial")
    us <- sort(vapply(seq_len(n), function(i) .ghc_unif(e, 1L), numeric(1)))
  else if (scheme == "stratified")
    us <- vapply(seq_len(n), function(k) (k - 1 + .ghc_unif(e, 1L)) / n,
                 numeric(1))
  else if (scheme == "systematic") {
    u0 <- .ghc_unif(e, 1L)
    us <- vapply(seq_len(n), function(k) (k - 1 + u0) / n, numeric(1))
  } else
    stop("resampler must be one of ",
         paste(.SNPEST_RESAMPLERS, collapse = ", "))
  out <- integer(0)
  acc <- 0
  j <- 1L
  for (k in seq_len(n)) {
    acc <- acc + weights[k]
    while (j <= n && us[j] <= acc) {
      out <- c(out, k)
      j <- j + 1L
    }
  }
  while (length(out) < n) out <- c(out, n)
  out
}

# Effective sample size, 1 / sum w^2 for normalised weights.
#' Effective sample size, 1 / sum w^2 for normalised weights
#'
#' A step of the snpest_native implementation. Called by \code{morie_snpest}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param w Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .snpest_ess(w = x)
#' res
.snpest_ess <- function(w) 1 / .w3_csum(w * w)

#' On-line DP-mixture filter over a stream of observations
#'
#' @param y_stream The observations, in arrival order. The order MATTERS:
#'   this is a filter, not a batch method, and the answer after t points
#'   is a function of the first t.
#' @param alpha Dirichlet-process concentration.
#' @param n_particles Particles carried.
#' @param proposal "optimal" or "prior".
#' @param resampler "systematic", "stratified" or "multinomial".
#' @param ess_threshold Resample when the effective sample size falls
#'   below this fraction of the particle count.
#' @param m0 Prior mean; the stream mean by default.
#' @param kappa0 Prior precision scaling on the mean.
#' @param a0 Inverse-gamma shape.
#' @param b0 Inverse-gamma scale; from the stream variance by default.
#' @param seed Seed for the generator shared with the Python arm.
#' @param grid Points at which the filtered predictive density is
#'   recorded after the last observation.
#' @param seed_stats Take m0 and b0 from the whole stream when not given.
#'   FALSE uses 0 and 1 instead and keeps the run strictly causal.
#' @return A list with the running log marginal likelihood, the expected
#'   number of clusters after each point, the ESS trace, the resampling
#'   times, and the filtered predictive density on the grid.
#' @export
morie_snpest <- function(y_stream, alpha = 1, n_particles = 100L,
                         proposal = "optimal", resampler = "systematic",
                         ess_threshold = 0.5, m0 = NULL, kappa0 = 0.01,
                         a0 = 2, b0 = NULL, seed = 1, grid = NULL,
                         seed_stats = TRUE) {
  if (!(proposal %in% .SNPEST_PROPOSALS))
    stop("proposal must be one of ", paste(.SNPEST_PROPOSALS, collapse = ", "))
  if (!(resampler %in% .SNPEST_RESAMPLERS))
    stop("resampler must be one of ",
         paste(.SNPEST_RESAMPLERS, collapse = ", "))
  if (alpha <= 0) stop("alpha must be positive")
  ys <- as.numeric(y_stream)
  Tn <- length(ys)
  if (Tn < 2L) stop("need at least two observations")
  P <- as.integer(n_particles)
  if (P < 1L) stop("need at least one particle")
  if (seed_stats) {
    mean_ <- .w3_csum(ys) / Tn
    var_ <- .w3_csum((ys - mean_) * (ys - mean_)) / (Tn - 1)
  } else {
    mean_ <- 0
    var_ <- 1
  }
  if (is.null(m0)) m0 <- mean_
  if (is.null(b0)) b0 <- if (a0 > 1) var_ * (a0 - 1) else var_

  e <- .ghc_rng(seed)

  # Each particle is a list of clusters, each c(count, sum, sumsq).
  parts <- lapply(seq_len(P), function(i) list())
  logw <- numeric(P)
  loglik_trace <- numeric(0)
  ess_trace <- numeric(0)
  clusters_trace <- numeric(0)
  resampled_at <- integer(0)

  for (t in seq_len(Tn)) {
    x <- ys[t]
    incr <- numeric(P)
    for (p in seq_len(P)) {
      cl <- parts[[p]]
      K <- length(cl)
      lp <- numeric(K + 1L)
      if (K > 0L)
        for (j in seq_len(K))
          lp[j] <- log(cl[[j]][1]) +
            morie_snpest_predictive(x, cl[[j]][1], cl[[j]][2], cl[[j]][3],
                                    m0, kappa0, a0, b0)
      lp[K + 1L] <- log(alpha) +
        morie_snpest_predictive(x, 0, 0, 0, m0, kappa0, a0, b0)
      norm <- .w3_logsumexp(lp)
      if (proposal == "optimal") {
        # Draw from the exact conditional; the weight increment is the
        # normalising constant, which is p(y_t | past).
        probs <- exp(lp - norm)
        u <- .ghc_unif(e, 1L)
        acc <- 0
        pick <- K
        for (j in seq_len(K + 1L)) {
          acc <- acc + probs[j]
          if (u <= acc) { pick <- j - 1L
          break }
        }
        incr[p] <- norm - log(alpha + (t - 1))
      } else {
        # Draw from the Polya urn alone; the weight increment is the
        # likelihood of the cluster that was drawn.
        tot <- alpha + (t - 1)
        u <- .ghc_unif(e, 1L) * tot
        acc <- 0
        pick <- K
        if (K > 0L)
          for (j in seq_len(K)) {
            acc <- acc + cl[[j]][1]
            if (u <= acc) { pick <- j - 1L
            break }
          }
        incr[p] <- lp[pick + 1L] -
          log(if (pick < K) cl[[pick + 1L]][1] else alpha)
      }
      if (pick == K) {
        cl[[K + 1L]] <- c(1, x, x * x)
      } else {
        cl[[pick + 1L]][1] <- cl[[pick + 1L]][1] + 1
        cl[[pick + 1L]][2] <- cl[[pick + 1L]][2] + x
        cl[[pick + 1L]][3] <- cl[[pick + 1L]][3] + x * x
      }
      parts[[p]] <- cl
    }

    logw <- logw + incr
    lnorm <- .w3_logsumexp(logw)
    wts <- exp(logw - lnorm)
    # lnorm IS the running log marginal likelihood: the weights start at
    # zero and a resample rewrites them as lnorm - log(P) each, which
    # leaves their total untouched. Recording it here, before the
    # resample, is what keeps it the right quantity.
    loglik_trace <- c(loglik_trace, lnorm)
    ee <- .snpest_ess(wts)
    ess_trace <- c(ess_trace, ee)
    clusters_trace <- c(clusters_trace,
                        .w3_csum(vapply(seq_len(P), function(p)
                          wts[p] * length(parts[[p]]), numeric(1))))

    if (ee < ess_threshold * P) {
      idx <- .snpest_resample(e, wts, resampler)
      parts <- lapply(idx, function(i) parts[[i]])
      logw <- rep(lnorm - log(P), P)
      resampled_at <- c(resampled_at, t - 1L)
    }
  }

  lnorm <- .w3_logsumexp(logw)
  wts <- exp(logw - lnorm)

  if (is.null(grid)) {
    lo <- min(ys) - sqrt(var_)
    hi <- max(ys) + sqrt(var_)
    grid <- vapply(0:20, function(k) lo + (hi - lo) * k / 20, numeric(1))
  }
  grid <- as.numeric(grid)

  dens <- vapply(grid, function(g) {
    acc <- vapply(seq_len(P), function(p) {
      cl <- parts[[p]]
      K <- length(cl)
      lp <- numeric(K + 1L)
      if (K > 0L)
        for (j in seq_len(K))
          lp[j] <- log(cl[[j]][1]) +
            morie_snpest_predictive(g, cl[[j]][1], cl[[j]][2], cl[[j]][3],
                                    m0, kappa0, a0, b0)
      lp[K + 1L] <- log(alpha) +
        morie_snpest_predictive(g, 0, 0, 0, m0, kappa0, a0, b0)
      wts[p] * exp(.w3_logsumexp(lp) - log(alpha + Tn))
    }, numeric(1))
    .w3_csum(acc)
  }, numeric(1))

  list(grid = grid, density = dens,
       log_marginal = loglik_trace[length(loglik_trace)],
       log_marginal_trace = loglik_trace, ess = ess_trace,
       final_ess = ess_trace[length(ess_trace)],
       mean_ess = .w3_csum(ess_trace) / Tn,
       clusters = clusters_trace,
       final_clusters = clusters_trace[length(clusters_trace)],
       resampled_at = resampled_at, n_resamples = length(resampled_at),
       T = Tn, n_particles = P, proposal = proposal, resampler = resampler,
       alpha = as.numeric(alpha),
       prior = list(m0 = m0, kappa0 = kappa0, a0 = a0, b0 = b0),
       seed = as.integer(seed),
       estimate = clusters_trace[length(clusters_trace)],
       method = "sequential DP-mixture particle filter")
}

#' One-line summary of the snpest module
#'
#' @return A character scalar.
#' @export
morie_snpest_cheatsheet <- function()
  paste0("snpest: on-line DP-mixture particle filter. proposals ",
         paste(.SNPEST_PROPOSALS, collapse = ", "), "; resamplers ",
         paste(.SNPEST_RESAMPLERS, collapse = ", "))
