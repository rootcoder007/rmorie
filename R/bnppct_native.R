# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of bnppct -- the nonparametric Bayes posterior of F^-1(q).
# Mirrors src/morie/fn/bnppct.py operation for operation, on the shared
# numerics in R/aaa_helpers_w3num.R and the generator in
# R/aaa_helpers_ghc_rng.R.
#
# A quantile estimate carrying a standard error borrowed from
# asymptotics answers a different question from the one usually asked.
# A Bayesian nonparametric model gives the POSTERIOR of the quantile: F
# is a random distribution, F^-1(q) is a functional of it, and every
# posterior draw of F carries a draw of the quantile. The spread of
# those draws is the uncertainty, with no appeal to a limit and no
# assumption that the sampling distribution is symmetric -- which for an
# extreme quantile it is not.
#
# Three routes, because there are three genuinely different objects and
# calling any one of them "the" Bayesian quantile would be a choice
# hidden inside an implementation:
#
#   "mixture" (default)
#       For each retained sweep of the Dirichlet-process mixture, invert
#       that sweep's mixture CDF at q. The result is a sample from the
#       posterior of F^-1(q).
#
#   "predictive"
#       Average the CDF over sweeps FIRST and invert once. That is the
#       quantile of the posterior predictive distribution, and it is NOT
#       the posterior mean of the quantile -- inversion does not commute
#       with averaging. It answers "what value will the next observation
#       fall below with probability q", a prediction rather than an
#       inference about F.
#
#   "bayesian_bootstrap"
#       Rubin's Bayesian bootstrap: a Dirichlet(1, ..., 1) posterior on
#       the weights of the observed points, read off as the weighted
#       empirical quantile. No mixture, no smoothing, no prior on the
#       shape of F. It is the limiting case of the DP posterior as the
#       concentration goes to zero, so it is the honest baseline: if the
#       mixture route says something very different, that difference is
#       the smoothing, and it should be visible rather than assumed away.
#
# The mixture routes work from the retained component states of the
# slice sampler in slbpdg -- the SAME sampler, called with keep_draws,
# not a second one written here. Two samplers for one model is two
# places for a bug to live.
#
# One honest limitation is reported rather than hidden. The slice
# sampler carries finitely many components per sweep and leaves an
# unbroken remainder of the stick, so the mixture CDF integrates to
# 1 - rest rather than to 1. The routes normalise by the carried mass
# and report min_mass_carried, so a run where the remainder was not
# negligible announces itself instead of quietly shifting the tails.
#
# References
#   Kottas, A. and Krnjajic, M. (2009) "Bayesian semiparametric
#     modelling in quantile regression." Scandinavian Journal of
#     Statistics 36(2), 297-319.
#   Rubin, D.B. (1981) "The Bayesian bootstrap." Annals of Statistics
#     9(1), 130-134.
#   Walker (2007); Kalli, Griffin and Walker (2011) -- the sampler,
#     through morie_slbpdg.

.BNPPCT_ROUTES <- c("mixture", "predictive", "bayesian_bootstrap")

#' Unnormalised mixture CDF
#'
#' @param x The point.
#' @param w Component weights.
#' @param mu Component means.
#' @param s2 Component variances.
#' @return sum_k w_k Phi((x - mu_k)/sqrt(s2_k)).
#' @export
morie_bnppct_cdf <- function(x, w, mu, s2)
  .w3_csum(vapply(seq_along(w), function(k)
    w[k] * .w3_ncdf((x - mu[k]) / sqrt(s2[k])), numeric(1)))

#' Widen a bracket by doubling until f changes sign across it
#'
#' A fixed doubling schedule, not a search: both arms then visit exactly
#' the same endpoints and the bisection that follows starts from the
#' same interval.
#'
#' @param f The function.
#' @param lo Lower end.
#' @param hi Upper end.
#' @param iters Maximum doublings.
#' @return A list with the widened lo and hi.
#' @export
morie_bnppct_expand <- function(f, lo, hi, iters = 60L) {
  flo <- f(lo)
  fhi <- f(hi)
  k <- 0L
  while ((flo > 0) == (fhi > 0) && k < as.integer(iters)) {
    mid <- 0.5 * (lo + hi)
    half <- hi - mid
    lo <- mid - 2 * half
    hi <- mid + 2 * half
    flo <- f(lo)
    fhi <- f(hi)
    k <- k + 1L
  }
  list(lo = lo, hi = hi)
}

#' Invert the mixture CDF at q, normalising by the carried mass
#'
#' Bisection rather than Newton: the CDF is monotone but its derivative
#' is a mixture of narrow normals, and a Newton step off a flat stretch
#' between two well-separated components lands anywhere.
#'
#' The bracket is taken from the COMPONENTS, not from the data. A slice
#' sampler instantiates components from the prior to cover the slice,
#' and an inverse-gamma prior draw can be enormous; such a component
#' leaves the CDF far short of one anywhere near the data, so a
#' data-width bracket fails to contain the root. That is not a numerical
#' nuisance -- it is the model saying this draw of F has a very heavy
#' tail -- so the bracket follows the components and is then widened
#' until it genuinely brackets.
#'
#' @param q The probability.
#' @param w Component weights.
#' @param mu Component means.
#' @param s2 Component variances.
#' @param lo Lower end of the bracket, or NULL to take it from the
#'   components.
#' @param hi Upper end of the bracket, or NULL.
#' @return The quantile.
#' @export
morie_bnppct_quantile <- function(q, w, mu, s2, lo = NULL, hi = NULL) {
  mass <- .w3_csum(w)
  if (mass <= 0) return(NaN)
  f <- function(x) morie_bnppct_cdf(x, w, mu, s2) / mass - q
  if (is.null(lo) || is.null(hi)) {
    sds <- sqrt(s2)
    blo <- min(mu - 40 * sds)
    bhi <- max(mu + 40 * sds)
    lo <- if (is.null(lo)) blo else min(lo, blo)
    hi <- if (is.null(hi)) bhi else max(hi, bhi)
  }
  br <- morie_bnppct_expand(f, lo, hi)
  .w3_bisect(f, br$lo, br$hi)
}

# Weighted empirical quantile: the smallest point whose cumulative
# weight reaches q. The inverse of the weighted ECDF, which is a step
# function, so nothing is interpolated between the steps.
#' Weighted empirical quantile: the smallest point whose cumulative
#'
#' weight reaches q. The inverse of the weighted ECDF, which is a step
#' function, so nothing is interpolated between the steps.
#'
#' @param ys_sorted A vector; its length is taken and its elements indexed.
#' @param weights A vector; indexed elementwise.
#' @param q Passed to \code{>=}.
#' @return The value of \code{[}.
#' @export
.bnppct_wq <- function(ys_sorted, weights, q) {
  acc <- 0
  for (i in seq_along(ys_sorted)) {
    acc <- acc + weights[i]
    if (acc >= q) return(ys_sorted[i])
  }
  ys_sorted[length(ys_sorted)]
}

#' Posterior of the quantile function of a nonparametric F
#'
#' @param y The data.
#' @param quantile One or more probabilities in (0, 1).
#' @param route "mixture", "predictive" or "bayesian_bootstrap".
#' @param alpha Dirichlet-process concentration for the mixture routes.
#' @param n_iter Sampler length.
#' @param burn Burn-in.
#' @param thin Thinning.
#' @param seed Seed for the generator shared with the Python arm.
#' @param cred Credible level for the reported interval.
#' @param sampler_route Which slice sampler morie_slbpdg should use.
#' @param kappa Geometric rate for the Kalli-Griffin-Walker route.
#' @param m0 Prior mean.
#' @param kappa0 Prior precision scaling on the mean.
#' @param a0 Inverse-gamma shape.
#' @param b0 Inverse-gamma scale.
#' @param n_bootstrap Replicates for the Bayesian-bootstrap route.
#' @param alpha_update Passed through to the sampler.
#' @return A list with, per quantile, the posterior mean, standard
#'   deviation, median and credible bounds, plus the draws themselves.
#' @export
morie_bnppct <- function(y, quantile = 0.5, route = "mixture", alpha = 1,
                         n_iter = 500L, burn = NULL, thin = 1L, seed = 1,
                         cred = 0.9, sampler_route = "walker", kappa = 0.5,
                         m0 = NULL, kappa0 = 0.01, a0 = 2, b0 = NULL,
                         n_bootstrap = 500L, alpha_update = NULL) {
  if (!(route %in% .BNPPCT_ROUTES))
    stop("route must be one of ", paste(.BNPPCT_ROUTES, collapse = ", "))
  qs <- as.numeric(quantile)
  if (any(qs <= 0 | qs >= 1))
    stop("quantiles must lie strictly inside (0, 1)")
  if (!(cred > 0 && cred < 1)) stop("cred must lie strictly inside (0, 1)")
  ys <- as.numeric(y)
  n <- length(ys)
  if (n < 2L) stop("need at least two observations")
  ybar <- .w3_csum(ys) / n
  sd <- sqrt(.w3_csum((ys - ybar) * (ys - ybar)) / (n - 1))
  lo <- min(ys) - 10 * sd
  hi <- max(ys) + 10 * sd

  draws <- lapply(qs, function(q) numeric(0))
  min_mass <- 1
  fit <- NULL

  if (route == "bayesian_bootstrap") {
    e <- .ghc_rng(seed)
    ysort <- ys[order(ys, seq_len(n))]
    for (b in seq_len(as.integer(n_bootstrap))) {
      # Dirichlet(1,...,1) as normalised unit exponentials, Rubin's
      # construction, which needs no Dirichlet primitive.
      g <- vapply(seq_len(n), function(i) .ghc_gamma1(e, 1, 1), numeric(1))
      tot <- .w3_csum(g)
      wts <- g / tot
      for (j in seq_along(qs))
        draws[[j]] <- c(draws[[j]], .bnppct_wq(ysort, wts, qs[j]))
    }
  } else {
    fit <- morie_slbpdg(ys, alpha = alpha, n_iter = n_iter, burn = burn,
                        thin = thin, route = sampler_route, kappa = kappa,
                        m0 = m0, kappa0 = kappa0, a0 = a0, b0 = b0,
                        seed = seed, alpha_update = alpha_update,
                        keep_draws = TRUE)
    for (d in fit$draws) {
      mass <- .w3_csum(d$w)
      if (mass < min_mass) min_mass <- mass
      if (route == "mixture")
        for (j in seq_along(qs))
          draws[[j]] <- c(draws[[j]],
                          morie_bnppct_quantile(qs[j], d$w, d$mu, d$s2,
                                                NULL, NULL))
    }
    if (route == "predictive") {
      # Average the CDF over sweeps, then invert ONCE. The order
      # matters: inversion does not commute with averaging, and doing it
      # the other way round would silently return the mixture route's
      # answer under a different name.
      sweeps <- fit$draws
      m <- length(sweeps)
      fbar <- function(x)
        .w3_csum(vapply(sweeps, function(d)
          morie_bnppct_cdf(x, d$w, d$mu, d$s2) / .w3_csum(d$w),
          numeric(1))) / m
      for (j in seq_along(qs)) {
        qq <- qs[j]
        g <- function(x) fbar(x) - qq
        br <- morie_bnppct_expand(g, lo, hi)
        draws[[j]] <- c(draws[[j]], .w3_bisect(g, br$lo, br$hi))
      }
    }
  }

  out <- lapply(seq_along(qs), function(j) {
    v <- sort(draws[[j]])
    m <- length(v)
    mean <- .w3_csum(v) / m
    sdq <- if (m > 1L) sqrt(.w3_csum((v - mean) * (v - mean)) / (m - 1)) else 0
    a <- (1 - cred) / 2
    list(q = qs[j], estimate = mean, sd = sdq,
         median = .bnppct_wq(v, rep(1 / m, m), 0.5),
         lower = .bnppct_wq(v, rep(1 / m, m), a),
         upper = .bnppct_wq(v, rep(1 / m, m), 1 - a),
         draws = draws[[j]])
  })

  res <- list(quantiles = out, estimate = out[[1]]$estimate,
              se = out[[1]]$sd, n = n, route = route,
              cred = as.numeric(cred), n_draws = length(draws[[1]]),
              min_mass_carried = min_mass, seed = as.integer(seed),
              method = "nonparametric Bayes posterior of the quantile function")
  if (!is.null(fit)) {
    res$mean_clusters <- fit$mean_clusters
    res$sampler_route <- fit$route
  }
  res
}

#' One-line summary of the bnppct module
#'
#' @return A character scalar.
#' @export
morie_bnppct_cheatsheet <- function()
  paste0("bnppct: nonparametric Bayes posterior of the quantile ",
         "function. routes ", paste(.BNPPCT_ROUTES, collapse = ", "))
