# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of slbpdg -- slice samplers for Dirichlet-process mixtures
# (Walker 2007; Kalli, Griffin and Walker 2011). Mirrors
# src/morie/fn/slbpdg.py operation for operation, on the shared
# numerics in R/aaa_helpers_w3num.R and the generator in
# R/aaa_helpers_ghc_rng.R, which is the R side of
# _array_core._SplitMix64 -- so both arms draw the same numbers.
#
# A Dirichlet-process mixture has infinitely many components. Every
# practical sampler has to make that finite somehow, and the older
# answer -- truncate the stick after K terms and hope K was big enough
# -- changes the model to make the computation possible. The slice
# sampler does the opposite: it adds an auxiliary variable that makes
# the number of components needed at each sweep FINITE AND RANDOM while
# leaving the posterior exactly the one the infinite model implies.
# Nothing is truncated; the truncation point is resampled.
#
# The trick. Write the mixture as sum_k w_k f(y | theta_k) with
# stick-breaking weights w_k = v_k prod_{l<k} (1 - v_l), v_k ~ Beta(1,
# alpha). Introduce u_i and define the joint
#
#     p(y_i, u_i, d_i) = 1{u_i < w_{d_i}} f(y_i | theta_{d_i})
#
# Marginalising u_i over (0, w_{d_i}) gives back w_{d_i} f(y_i |
# theta_{d_i}), so the model is unchanged. But CONDITIONAL on u_i only
# components with w_k > u_i can be chosen, and since the weights sum to
# one only finitely many qualify. That is Walker's sampler.
#
# Kalli, Griffin and Walker replace the random bound w_{d_i} with a
# DETERMINISTIC decreasing sequence xi_k = (1 - kappa) kappa^(k-1):
#
#     p(y_i, u_i, d_i) = 1{u_i < xi_{d_i}} (w_{d_i} / xi_{d_i})
#                          f(y_i | theta_{d_i})
#
# The number of components needed is then a deterministic function of
# min(u), so it cannot be driven to something huge by one unlucky small
# weight -- the failure mode of the original when a component's weight
# is tiny but its slice variable lands under it. The price is the ratio
# w/xi in the label weights, and kappa is a knob: small kappa carries
# fewer components and mixes worse.
#
# Both are implemented, both are exact, and `route` chooses. They target
# the same posterior, so on the same data with the same prior they must
# agree in distribution -- which the parity harness checks by comparing
# posterior summaries rather than paths.
#
# The component model is the conjugate normal / inverse-gamma, so every
# parameter draw is closed form and no Metropolis step appears anywhere.
# alpha may be held fixed or given Escobar and West's gamma prior.
#
# References
#   Walker, S.G. (2007) "Sampling the Dirichlet mixture model with
#     slices." Communications in Statistics -- Simulation and
#     Computation 36(1), 45-54.
#   Kalli, M., Griffin, J.E. and Walker, S.G. (2011) "Slice sampling
#     mixture models." Statistics and Computing 21(1), 93-105.
#   Escobar, M.D. and West, M. (1995) "Bayesian density estimation and
#     inference using mixtures." JASA 90(430), 577-588, section 6.
#   Ishwaran, H. and James, L.F. (2001) "Gibbs sampling methods for
#     stick-breaking priors." JASA 96(453), 161-173.

.SLBPDG_ROUTES <- c("walker", "kalli_griffin_walker")

#' Stick-breaking weights from the beta variates
#'
#' w_k = v_k times the product of (1 - v_l) over l < k, accumulated as
#' a running product rather than recomputed -- both the cheap way and
#' the way the Python arm can be matched term for term.
#'
#' @param v The beta variates.
#' @return A list with the weights and the unbroken remainder.
#' @export
morie_slbpdg_weights <- function(v) {
  n <- length(v)
  w <- numeric(n)
  rest <- 1
  for (k in seq_len(n)) {
    w[k] <- v[k] * rest
    rest <- rest * (1 - v[k])
  }
  list(w = w, rest = rest)
}

#' .slbpdg_dnorm
#'
#' A step of the slbpdg_native implementation. Called by \code{morie_slbpdg}, \code{morie_slbpdg_density}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @param mu Numeric; combined arithmetically in the body.
#' @param s2 Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.slbpdg_dnorm <- function(x, mu, s2) {
  d <- x - mu
  exp(-0.5 * d * d / s2) / sqrt(2 * pi * s2)
}

#' Mixture density at a point
#'
#' @param x The point.
#' @param w Component weights.
#' @param mu Component means.
#' @param s2 Component variances.
#' @return The density value.
#' @export
morie_slbpdg_density <- function(x, w, mu, s2)
  .w3_csum(vapply(seq_along(w), function(k)
    w[k] * .slbpdg_dnorm(x, mu[k], s2[k]), numeric(1)))

# Conjugate normal / inverse-gamma draw for one component. With no
# members this is a draw from the PRIOR, which is what the slice sampler
# needs for components it has to invent to cover the slice -- they are
# not fitted to anything, so their parameters must come from the prior
# and not from some neighbour.
#' Conjugate normal / inverse-gamma draw for one component. With no
#'
#' members this is a draw from the PRIOR, which is what the slice
#' sampler needs for components it has to invent to cover the slice --
#' they are not fitted to anything, so their parameters must come from
#' the prior and not from some neighbour.
#'
#' @param e Passed to \code{.ghc_gamma1}.
#' @param ys A vector; its length is taken.
#' @param m0 Numeric; combined arithmetically in the body.
#' @param kappa0 Numeric; combined arithmetically in the body.
#' @param a0 Numeric; combined arithmetically in the body.
#' @param b0 Numeric; combined arithmetically in the body.
#' @return A vector, from \code{c}.
#' @export
.slbpdg_theta <- function(e, ys, m0, kappa0, a0, b0) {
  n <- length(ys)
  if (n > 0L) {
    ybar <- .w3_csum(ys) / n
    ss <- .w3_csum((ys - ybar) * (ys - ybar))
    kn <- kappa0 + n
    mn <- (kappa0 * m0 + n * ybar) / kn
    an <- a0 + 0.5 * n
    bn <- b0 + 0.5 * ss + 0.5 * kappa0 * n * (ybar - m0) * (ybar - m0) / kn
  } else {
    kn <- kappa0; mn <- m0; an <- a0; bn <- b0
  }
  # s2 first, then mu: the mean's variance depends on the draw of s2, so
  # the order is forced, and it fixes the stream position too.
  s2 <- bn / .ghc_gamma1(e, an, 1)
  mu <- mn + sqrt(s2 / kn) * .ghc_norm(e, 1L)
  c(mu, s2)
}

# Inverse-CDF draw on UNNORMALISED weights, one uniform each. The
# cumulative sum is a plain running sum in both arms rather than a
# compensated one, because the comparison is against a uniform scaled by
# the same total -- consistency between the two sums is what matters,
# not their accuracy.
#' Inverse-CDF draw on UNNORMALISED weights, one uniform each. The
#'
#' cumulative sum is a plain running sum in both arms rather than a
#' compensated one, because the comparison is against a uniform scaled
#' by the same total -- consistency between the two sums is what
#' matters, not their accuracy.
#'
#' @param e Passed to \code{.ghc_unif}.
#' @param weights A vector; its length is taken and its elements indexed.
#' @return A numeric value.
#' @export
.slbpdg_categorical <- function(e, weights) {
  tot <- .w3_csum(weights)
  if (tot <= 0) return(-1L)
  u <- .ghc_unif(e, 1L) * tot
  acc <- 0
  for (k in seq_along(weights)) {
    acc <- acc + weights[k]
    if (u <= acc) return(k - 1L)
  }
  length(weights) - 1L
}

# The deterministic bound xi_k = (1 - kappa) kappa^(k-1), built by
# repeated multiplication rather than kappa^(k-1): R's `^` on an integer
# exponent is repeated squaring and Python's `**` calls pow(), and the
# two part company in the last bit exactly where the comparison
# xi_k > u_i decides how many components to carry.
#' The deterministic bound xi_k = (1 - kappa) kappa^(k-1), built by
#'
#' repeated multiplication rather than kappa^(k-1): R\'s `^` on an
#' integer exponent is repeated squaring and Python\'s `**` calls pow(),
#' and the two part company in the last bit exactly where the comparison
#' xi_k > u_i decides how many components to carry.
#'
#' @param kappa Numeric; combined arithmetically in the body.
#' @param k A count; the body uses it as \code{seq_len(...)}.
#' @return A numeric value.
#' @export
.slbpdg_xi <- function(kappa, k) {
  p <- 1
  if (k > 0L) for (i in seq_len(k)) p <- p * kappa
  (1 - kappa) * p
}

# Linear interpolation, used only to integrate the density grid.
#' Linear interpolation, used only to integrate the density grid
#'
#' A step of the slbpdg_native implementation. Called by \code{morie_slbpdg}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param xs A vector; its length is taken and its elements indexed.
#' @param ys A vector; indexed elementwise.
#' @param x Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.slbpdg_interp <- function(xs, ys, x) {
  n <- length(xs)
  if (x <= xs[1]) return(ys[1])
  if (x >= xs[n]) return(ys[n])
  lo <- 1L; hi <- n
  while (hi - lo > 1L) {
    mid <- (lo + hi) %/% 2L
    if (xs[mid] <= x) lo <- mid else hi <- mid
  }
  t <- (x - xs[lo]) / (xs[hi] - xs[lo])
  ys[lo] + t * (ys[hi] - ys[lo])
}

#' Slice-sampled Dirichlet-process mixture of normals
#'
#' @param y The data.
#' @param alpha Dirichlet-process concentration; the starting value when
#'   it is being updated.
#' @param n_iter Total sweeps, including burn-in.
#' @param burn Sweeps discarded; half of n_iter by default.
#' @param thin Keep every thin-th sweep after burn-in.
#' @param route "walker" for the random bound u_i below the weight of
#'   the point's own component, or "kalli_griffin_walker" for the
#'   deterministic xi_k.
#' @param kappa The geometric rate of xi_k, used only by the second
#'   route. Smaller carries fewer components and mixes worse.
#' @param m0 Prior mean; the sample mean by default.
#' @param kappa0 Prior precision scaling on the mean.
#' @param a0 Inverse-gamma shape.
#' @param b0 Inverse-gamma scale; set from the sample variance by
#'   default, which puts the prior on the scale of the data instead of
#'   on the scale of whatever units it arrived in.
#' @param seed Seed for the generator shared with the Python arm.
#' @param max_components A hard ceiling on the components carried in one
#'   sweep. Reaching it is reported in hit_ceiling rather than silently
#'   truncating the model into a different one.
#' @param grid Points at which the posterior mean density is
#'   accumulated.
#' @param alpha_update NULL to hold alpha fixed, or "escobar_west".
#' @param alpha_a Shape of the gamma prior on alpha.
#' @param alpha_b Rate of that prior.
#' @param keep_draws Also return the component state of every retained
#'   sweep -- weights, means and variances. Downstream modules needing a
#'   posterior of some functional of the mixture (a quantile, a tail
#'   probability) work from these rather than re-running a sampler of
#'   their own.
#' @return A list of posterior summaries: the mean density on the grid,
#'   the distribution of the number of occupied clusters, the retained
#'   alpha draws and per-sweep diagnostics.
#' @export
morie_slbpdg <- function(y, alpha = 1, n_iter = 500L, burn = NULL,
                         thin = 1L, route = "walker", kappa = 0.5,
                         m0 = NULL, kappa0 = 0.01, a0 = 2, b0 = NULL,
                         seed = 1, max_components = 200L, grid = NULL,
                         alpha_update = NULL, alpha_a = 2, alpha_b = 1,
                         keep_draws = FALSE) {
  if (!(route %in% .SLBPDG_ROUTES))
    stop("route must be one of ", paste(.SLBPDG_ROUTES, collapse = ", "))
  if (!(kappa > 0 && kappa < 1))
    stop("kappa must lie strictly inside (0, 1)")
  if (alpha <= 0) stop("alpha must be positive")
  ys <- as.numeric(y)
  n <- length(ys)
  if (n < 2L) stop("need at least two observations")
  n_iter <- as.integer(n_iter)
  if (is.null(burn)) burn <- n_iter %/% 2L
  burn <- as.integer(burn); thin <- as.integer(thin)
  ybar <- .w3_csum(ys) / n
  var <- .w3_csum((ys - ybar) * (ys - ybar)) / (n - 1)
  if (is.null(m0)) m0 <- ybar
  if (is.null(b0)) b0 <- if (a0 > 1) var * (a0 - 1) else var
  if (is.null(grid)) {
    lo <- min(ys) - sqrt(var)
    hi <- max(ys) + sqrt(var)
    grid <- vapply(0:20, function(k) lo + (hi - lo) * k / 20, numeric(1))
  }
  grid <- as.numeric(grid)

  e <- .ghc_rng(seed)

  # One component to start, everybody in it.
  v <- .ghc_beta1(e, 1, alpha)
  th <- .slbpdg_theta(e, ys, m0, kappa0, a0, b0)
  mus <- th[1]; s2s <- th[2]
  d <- integer(n)

  dens <- numeric(length(grid))
  kept <- 0L
  n_clusters <- integer(0)
  n_components <- integer(0)
  alphas <- numeric(0)
  hit_ceiling <- FALSE
  max_carried <- 0L
  draws <- list()

  for (it in seq_len(n_iter)) {
    sw <- morie_slbpdg_weights(v)
    w <- sw$w; rest <- sw$rest

    # 1. the slice variables
    u <- numeric(n)
    for (i in seq_len(n))
      u[i] <- .ghc_unif(e, 1L) *
        (if (route == "walker") w[d[i] + 1L] else .slbpdg_xi(kappa, d[i]))
    umin <- min(u)

    # 2. grow the stick until no component beyond it can be chosen.
    #    Walker: until the leftover mass is below the smallest slice.
    #    KGW: until xi_K itself is, which does not depend on the weights
    #    at all and is the point of the variant.
    repeat {
      if (route == "walker") {
        if (rest <= umin || length(v) >= max_components) break
      } else {
        if (.slbpdg_xi(kappa, length(v)) <= umin ||
            length(v) >= max_components) break
      }
      v <- c(v, .ghc_beta1(e, 1, alpha))
      tk <- .slbpdg_theta(e, numeric(0), m0, kappa0, a0, b0)
      mus <- c(mus, tk[1]); s2s <- c(s2s, tk[2])
      sw <- morie_slbpdg_weights(v)
      w <- sw$w; rest <- sw$rest
    }
    if (length(v) >= max_components) hit_ceiling <- TRUE
    K <- length(v)
    if (K > max_carried) max_carried <- K

    # 3. labels, from the components the slice admits
    for (i in seq_len(n)) {
      wt <- numeric(K)
      for (k in seq_len(K)) {
        if (route == "walker") {
          if (w[k] > u[i]) wt[k] <- .slbpdg_dnorm(ys[i], mus[k], s2s[k])
        } else {
          xk <- .slbpdg_xi(kappa, k - 1L)
          if (xk > u[i])
            wt[k] <- (w[k] / xk) * .slbpdg_dnorm(ys[i], mus[k], s2s[k])
        }
      }
      j <- .slbpdg_categorical(e, wt)
      if (j >= 0L) d[i] <- j
    }

    # 4. the sticks, from the counts above and beyond each index
    counts <- integer(K)
    for (i in seq_len(n)) counts[d[i] + 1L] <- counts[d[i] + 1L] + 1L
    after <- integer(K)
    run <- 0L
    for (k in seq(K, 1L)) {
      after[k] <- run
      run <- run + counts[k]
    }
    for (k in seq_len(K)) v[k] <- .ghc_beta1(e, 1 + counts[k], alpha + after[k])

    # 5. the component parameters
    for (k in seq_len(K)) {
      tk <- .slbpdg_theta(e, ys[d == (k - 1L)], m0, kappa0, a0, b0)
      mus[k] <- tk[1]; s2s[k] <- tk[2]
    }

    occupied <- sum(counts > 0L)

    # 6. alpha, by Escobar and West's two-component mixture
    if (!is.null(alpha_update)) {
      if (alpha_update != "escobar_west")
        stop('alpha_update must be NULL or "escobar_west"')
      eta <- .ghc_beta1(e, alpha + 1, n)
      odds <- (alpha_a + occupied - 1) / (n * (alpha_b - log(eta)))
      pi_eta <- odds / (1 + odds)
      if (.ghc_unif(e, 1L) < pi_eta)
        alpha <- .ghc_gamma1(e, alpha_a + occupied, 1 / (alpha_b - log(eta)))
      else
        alpha <- .ghc_gamma1(e, alpha_a + occupied - 1, 1 / (alpha_b - log(eta)))
    }

    if (it - 1L >= burn && (it - 1L - burn) %% thin == 0L) {
      kept <- kept + 1L
      n_clusters <- c(n_clusters, occupied)
      n_components <- c(n_components, K)
      alphas <- c(alphas, alpha)
      sw <- morie_slbpdg_weights(v)
      for (gi in seq_along(grid))
        dens[gi] <- dens[gi] + morie_slbpdg_density(grid[gi], sw$w, mus, s2s)
      if (keep_draws)
        draws[[length(draws) + 1L]] <- list(w = sw$w, mu = mus, s2 = s2s,
                                            rest = sw$rest)
    }
  }

  if (kept == 0L) stop("burn-in consumed every sweep")
  dens <- dens / kept
  mean_clusters <- .w3_csum(as.numeric(n_clusters)) / kept
  mean_alpha <- .w3_csum(alphas) / kept
  lv <- sort(unique(n_clusters))
  cnt <- vapply(lv, function(c) sum(n_clusters == c), integer(1))
  modal <- lv[order(-cnt, lv)[1]]

  out <- list(grid = grid, density = dens,
       density_integral = .w3_simpson(
         function(x) .slbpdg_interp(grid, dens, x), grid[1],
         grid[length(grid)], 200L),
       n_clusters = n_clusters, mean_clusters = mean_clusters,
       modal_clusters = modal,
       cluster_table = lapply(seq_along(lv), function(i) c(lv[i], cnt[i])),
       n_components = n_components, max_components_carried = max_carried,
       hit_ceiling = hit_ceiling, alpha_draws = alphas,
       mean_alpha = mean_alpha, kept = kept, n = n, route = route,
       kappa = as.numeric(kappa),
       prior = list(m0 = m0, kappa0 = kappa0, a0 = a0, b0 = b0),
       seed = as.integer(seed), estimate = mean_clusters,
       method = "slice-sampled Dirichlet-process mixture")
  if (keep_draws) out$draws <- draws
  out
}

#' One-line summary of the slbpdg module
#'
#' @return A character scalar.
#' @export
morie_slbpdg_cheatsheet <- function()
  paste0("slbpdg: slice-sampled Dirichlet-process mixture. routes ",
         paste(.SLBPDG_ROUTES, collapse = ", "))
