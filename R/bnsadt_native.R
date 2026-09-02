# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of bnsadt -- publication-bias correction and its adversarial
# bound (Andrews and Kasy 2019). Mirrors src/morie/fn/bnsadt.py
# operation for operation, on the shared numerics in
# R/aaa_helpers_w3num.R.
#
# A literature is not a random sample of the studies that were run. If
# whether a result gets published depends on the result, the published
# estimates are draws from a TILTED distribution, and averaging them --
# however carefully -- averages the tilt along with the effect.
#
# Andrews and Kasy's move is to write the tilt down as a parameter. Let
# p(z) be the probability a study with z-statistic z is published. For a
# study with standard error sigma the density of what we observe is
#
#     f(x | sigma) = p(x/sigma) * phi((x - mu)/s) / s / E[p]
#
# with s^2 = tau^2 + sigma^2 for a latent effect Theta ~ N(mu, tau^2).
# Because p is a step function of z, E[p] is a finite sum of normal
# increments -- no quadrature, no approximation:
#
#     E[p] = sum_k beta_k [ Phi((sigma c_{k+1} - mu)/s)
#                           - Phi((sigma c_k - mu)/s) ]
#
# That closed form is why the model is estimable from a meta-study
# alone: the SAME mu and tau must explain studies with very different
# sigma, and only the selection can bend the relationship between them.
#
# Two things follow, and both are reported.
#
#   The point estimate. Maximise the likelihood over (mu, tau, beta) and
#   correct a single study to the MEDIAN-UNBIASED value: the theta for
#   which the published X is equally likely to fall above and below the
#   observed x. Median-unbiased rather than mean-unbiased because the
#   truncated normal's mean has no closed form and its median does;
#   Andrews and Kasy make the same choice.
#
#   The direction of the correction is toward zero at EVERY theta, not
#   only for significant results. With symmetric selection and theta > 0
#   the surviving upper tail carries more mass than the surviving lower
#   tail, so the published median sits above theta wherever theta is; an
#   insignificant published estimate is therefore shrunk too, not
#   inflated. Only theta = 0 is a fixed point, and it is one exactly.
#
#   The adversarial bound. The point estimate is conditional on ONE
#   selection function. The honest object is what happens as p ranges
#   over a family: the correction is monotone in the selection strength,
#   so sweeping beta gives an interval, and that interval -- not the
#   point -- is what the data plus the family assumption supports. This
#   is the "max over F" the module is named for.
#
# Families, all selectable
#
#   "none"             p == 1. The corrected estimate is the observation
#                      itself, which is the check that the machinery is
#                      not inventing a correction.
#   "symmetric_step"   p = beta for |z| < 1.96, 1 above. One parameter.
#   "symmetric_step2"  p = beta1 for |z| < 1.645, beta2 on the band up
#                      to 1.96, 1 above. Sees selection on the 10% level
#                      as well as the 5% one.
#   "signed_step"      the paper's own minimum-wage specification: a
#                      separate beta below -1.96, on (-1.96, 0), on
#                      (0, 1.96), and 1 above. The only family here that
#                      can see selection on the SIGN.
#
# Reference
#   Andrews, I. and Kasy, M. (2019) "Identification of and Correction
#     for Publication Bias." American Economic Review 109(8), 2766-2794.
#     (Working paper arXiv:1711.10527.) The step-function
#     specification, the meta-study likelihood and the median-unbiased
#     correction are from there. The sweep over the family, and the
#     reporting of the resulting interval, is this module's framing of
#     their identification argument.

# Each family is a list of signed cutoffs plus one group index per
# interval. A group index of -1 means the interval's probability is
# fixed at 1: p is identified only up to scale, so the interval a
# significant result lands in carries the normalisation.
.BNSADT_FAMILIES <- list(
  none = list(cuts = numeric(0), groups = c(-1L)),
  symmetric_step = list(cuts = c(-1.96, 1.96), groups = c(-1L, 0L, -1L)),
  symmetric_step2 = list(cuts = c(-1.96, -1.645, 1.645, 1.96),
                         groups = c(-1L, 1L, 0L, 1L, -1L)),
  signed_step = list(cuts = c(-1.96, 0, 1.96), groups = c(0L, 1L, 2L, -1L))
)

#' .bnsadt_nfree
#'
#' A step of the bnsadt_native implementation. Called by \code{morie_bnsadt}, \code{morie_bnsadt_fit}, \code{morie_bnsadt_group_counts}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param family See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.bnsadt_nfree <- function(family) {
  g <- .BNSADT_FAMILIES[[family]]$groups
  g <- g[g >= 0L]
  if (!length(g)) 0L else max(g) + 1L
}

# Per-interval publication probabilities from the free parameters.
#' Per-interval publication probabilities from the free parameters
#'
#' A step of the bnsadt_native implementation. Called by \code{.bnsadt_expected_p}, \code{.bnsadt_pub_cdf}, \code{morie_bnsadt_p}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param family See Usage.
#' @param params A vector; indexed elementwise.
#' @return A vector, from \code{vapply}.
#' @export
.bnsadt_betas <- function(family, params) {
  g <- .BNSADT_FAMILIES[[family]]$groups
  vapply(g, function(k) if (k < 0L) 1 else params[k + 1L], numeric(1))
}

#' Publication probability of a study with z-statistic z
#'
#' @param z The z-statistic.
#' @param family A name of the selection family.
#' @param params The free probabilities, in group order.
#' @return A single probability.
#' @export
morie_bnsadt_p <- function(z, family = "symmetric_step", params = numeric(0)) {
  cuts <- .BNSADT_FAMILIES[[family]]$cuts
  betas <- .bnsadt_betas(family, params)
  k <- 0L
  while (k < length(cuts) && z >= cuts[k + 1L]) k <- k + 1L
  betas[k + 1L]
}

# E[p(X/sigma)] under the marginal law of an unselected study. A finite
# sum of normal increments because p is a step function -- the reason no
# quadrature appears anywhere in this module.
#' E\[p(X/sigma)\] under the marginal law of an unselected study. A finite
#'
#' sum of normal increments because p is a step function -- the reason
#' no quadrature appears anywhere in this module.
#'
#' @param sigma Numeric; combined arithmetically in the body.
#' @param mu Numeric; combined arithmetically in the body.
#' @param tau Numeric; combined arithmetically in the body.
#' @param family Passed to \code{.bnsadt_betas}.
#' @param params Passed to \code{.bnsadt_betas}.
#' @return The value of \code{.w3_csum}.
#' @export
.bnsadt_expected_p <- function(sigma, mu, tau, family, params) {
  cuts <- .BNSADT_FAMILIES[[family]]$cuts
  betas <- .bnsadt_betas(family, params)
  s <- sqrt(tau * tau + sigma * sigma)
  edges <- c(-Inf, sigma * cuts, Inf)
  terms <- vapply(seq_along(betas), function(k) {
    lo <- edges[k]; hi <- edges[k + 1L]
    plo <- if (lo == -Inf) 0 else .w3_ncdf((lo - mu) / s)
    phi <- if (hi == Inf) 1 else .w3_ncdf((hi - mu) / s)
    betas[k] * (phi - plo)
  }, numeric(1))
  .w3_csum(terms)
}

#' Log-likelihood of the published estimates under the selection model
#'
#' @param x Published point estimates.
#' @param sigma Their standard errors.
#' @param mu Mean of the latent effect distribution.
#' @param tau Its standard deviation.
#' @param family A name of the selection family.
#' @param params The free publication probabilities.
#' @return The log-likelihood.
#' @export
morie_bnsadt_loglik <- function(x, sigma, mu, tau,
                                family = "symmetric_step",
                                params = numeric(0)) {
  n <- length(x)
  terms <- numeric(n)
  for (i in seq_len(n)) {
    s <- sqrt(tau * tau + sigma[i] * sigma[i])
    p <- morie_bnsadt_p(x[i] / sigma[i], family, params)
    if (p <= 0) return(-Inf)
    d <- .bnsadt_expected_p(sigma[i], mu, tau, family, params)
    if (d <= 0) return(-Inf)
    z <- (x[i] - mu) / s
    terms[i] <- log(p) - 0.5 * z * z - log(s * sqrt(2 * pi)) - log(d)
  }
  .w3_csum(terms)
}

#' Published studies falling in each free group's region
#'
#' A group with NO observations is not identified from below: nothing in
#' the data distinguishes "such studies are published one time in ten"
#' from "one time in a billion", because none were seen either way. The
#' likelihood is then monotone in that beta all the way to zero, and a
#' maximiser sent after it walks off into the denormals -- which is
#' exactly what the two arms did differently before this existed.
#'
#' @param x Published point estimates.
#' @param sigma Their standard errors.
#' @param family A name of the selection family.
#' @return An integer count per free group.
#' @export
morie_bnsadt_group_counts <- function(x, sigma, family) {
  groups <- .BNSADT_FAMILIES[[family]]$groups
  cuts <- .BNSADT_FAMILIES[[family]]$cuts
  k <- .bnsadt_nfree(family)
  counts <- integer(k)
  for (i in seq_along(x)) {
    z <- x[i] / sigma[i]
    j <- 0L
    while (j < length(cuts) && z >= cuts[j + 1L]) j <- j + 1L
    g <- groups[j + 1L]
    if (g >= 0L) counts[g + 1L] <- counts[g + 1L] + 1L
  }
  counts
}

#' Maximum likelihood over the selection model, by Nelder-Mead
#'
#' tau and every beta are optimised on the log scale, so the simplex
#' cannot step to a negative variance or a negative probability and the
#' run needs no penalty term to stay inside the parameter space. Betas
#' are squashed by the logistic, which bounds them by 1 -- a publication
#' probability above the normalisation would be the likelihood saying
#' the significant region is SUPPRESSED, which the family does not
#' describe.
#'
#' Two guards, both statements about identification and not numerical
#' tape: a group with no observed studies is HELD at 1 and reported in
#' `unidentified`, because its beta is bounded only from above; and tau
#' is floored at 1e-6 times the mean standard error, below which the
#' between-study spread is far under the within-study noise and the
#' likelihood is flat in it. The floor is reported in `tau_at_floor`
#' rather than hidden.
#'
#' @param x Published point estimates.
#' @param sigma Their standard errors.
#' @param family A name of the selection family.
#' @param mu0 Starting mean; the sample mean by default.
#' @param tau0 Starting heterogeneity; a method-of-moments value by
#'   default.
#' @param beta0 Starting publication probability.
#' @param iters Nelder-Mead iterations.
#' @return A list with mu, tau, the betas and the log-likelihood.
#' @export
morie_bnsadt_fit <- function(x, sigma, family = "symmetric_step",
                             mu0 = NULL, tau0 = NULL, beta0 = 0.5,
                             iters = 600L) {
  if (!(family %in% names(.BNSADT_FAMILIES)))
    stop("family must be one of ", paste(sort(names(.BNSADT_FAMILIES)),
                                         collapse = ", "))
  n <- length(x)
  if (is.null(mu0)) mu0 <- .w3_csum(x) / n
  if (is.null(tau0)) {
    v <- .w3_csum((x - mu0) * (x - mu0)) / n
    w <- .w3_csum(sigma * sigma) / n
    tau0 <- if (v > w) sqrt(v - w) else 0.5 * sqrt(v)
    if (tau0 <= 0) tau0 <- 0.1
  }
  k <- .bnsadt_nfree(family)
  counts <- morie_bnsadt_group_counts(x, sigma, family)
  active <- if (k > 0L) which(counts > 0L) - 1L else integer(0)
  unident <- if (k > 0L) which(counts == 0L) - 1L else integer(0)
  tau_floor <- 1e-6 * (.w3_csum(sigma) / n)

  expand <- function(par) {
    ps <- rep(1, k)
    if (length(active))
      for (j in seq_along(active))
        ps[active[j] + 1L] <- 1 / (1 + exp(-par[2L + j]))
    tau <- exp(par[2])
    list(mu = par[1], tau = if (tau > tau_floor) tau else tau_floor, ps = ps)
  }

  start <- c(mu0, log(tau0), rep(log(beta0 / (1 - beta0)), length(active)))

  neg <- function(par) {
    e <- expand(par)
    ll <- morie_bnsadt_loglik(x, sigma, e$mu, e$tau, family, e$ps)
    if (is.nan(ll) || ll == -Inf) 1e100 else -ll
  }

  r <- .w3_nelder_mead(neg, start, iters = iters)
  e <- expand(r$x)
  list(mu = e$mu, tau = e$tau, betas = e$ps, loglik = -r$value,
       family = family, n_free = length(active) + 2L, counts = counts,
       unidentified = unident, tau_at_floor = e$tau <= tau_floor,
       tau_floor = tau_floor)
}

# P(X <= x | Theta = theta) for a PUBLISHED study. Numerator and
# denominator are both finite sums of normal increments, so this is
# exact rather than quadrature.
#' P(X <= x | Theta = theta) for a PUBLISHED study. Numerator and
#'
#' denominator are both finite sums of normal increments, so this is
#' exact rather than quadrature.
#'
#' @param x Passed to \code{>}.
#' @param theta Numeric; combined arithmetically in the body.
#' @param sigma Numeric; combined arithmetically in the body.
#' @param family Passed to \code{.bnsadt_betas}.
#' @param params Passed to \code{.bnsadt_betas}.
#' @return A numeric value.
#' @export
.bnsadt_pub_cdf <- function(x, theta, sigma, family, params) {
  cuts <- .BNSADT_FAMILIES[[family]]$cuts
  betas <- .bnsadt_betas(family, params)
  edges <- c(-Inf, sigma * cuts, Inf)
  num <- numeric(0)
  den <- numeric(length(betas))
  for (k in seq_along(betas)) {
    lo <- edges[k]; hi <- edges[k + 1L]
    clo <- if (lo == -Inf) 0 else .w3_ncdf((lo - theta) / sigma)
    chi <- if (hi == Inf) 1 else .w3_ncdf((hi - theta) / sigma)
    den[k] <- betas[k] * (chi - clo)
    if (x > lo) {
      top <- if (hi < x) hi else x
      ctop <- if (top == Inf) 1 else .w3_ncdf((top - theta) / sigma)
      num <- c(num, betas[k] * (ctop - clo))
    }
  }
  d <- .w3_csum(den)
  if (d <= 0) return(NaN)
  .w3_csum(num) / d
}

#' Median-unbiased correction of a single published estimate
#'
#' The theta for which the published X has median x, solved by
#' bisection; the published CDF is decreasing in theta, so the root is
#' unique.
#'
#' @param x The published estimate.
#' @param sigma Its standard error.
#' @param family A name of the selection family.
#' @param params The free publication probabilities.
#' @param lo Lower end of the bracket.
#' @param hi Upper end of the bracket.
#' @return The corrected estimate.
#' @export
morie_bnsadt_median_unbiased <- function(x, sigma,
                                         family = "symmetric_step",
                                         params = numeric(0),
                                         lo = NULL, hi = NULL) {
  if (is.null(lo)) lo <- x - 20 * sigma
  if (is.null(hi)) hi <- x + 20 * sigma
  # The published CDF decreases in theta, so f(lo) > 0 and f(hi) < 0;
  # bisect only needs a sign change, not a direction.
  .w3_bisect(function(th) .bnsadt_pub_cdf(x, th, sigma, family, params) - 0.5,
             lo, hi)
}

#' Publication-bias correction with its adversarial bound
#'
#' @param y Published point estimates, one per study.
#' @param D Their standard errors. Required and positive: the model is
#'   identified BY the variation in sigma across studies, so a constant
#'   or absent sigma is not a smaller version of this problem, it is a
#'   different one.
#' @param family A name of the selection family.
#' @param grid Publication probabilities to sweep for the adversarial
#'   bound. The default runs from 1 down to 0.05, spanning "nothing was
#'   suppressed" to "insignificant work was almost never published".
#' @param target The study to correct; the largest absolute z by
#'   default, which is the one selection distorts most and so the one
#'   the bound is most informative about.
#' @param target_se Its standard error; taken from D when target is
#'   defaulted.
#' @param fit Estimate the selection parameters by maximum likelihood as
#'   well as sweeping the family.
#' @param iters Nelder-Mead iterations.
#' @return A list with the fit, the corrected estimate under it, and the
#'   adversarial interval over the family with the beta attaining each
#'   end.
#' @export
morie_bnsadt <- function(y, D, family = "symmetric_step", grid = NULL,
                         target = NULL, target_se = NULL, fit = TRUE,
                         iters = 600L) {
  if (!(family %in% names(.BNSADT_FAMILIES)))
    stop("family must be one of ", paste(sort(names(.BNSADT_FAMILIES)),
                                         collapse = ", "))
  x <- as.numeric(y)
  sigma <- as.numeric(D)
  n <- length(x)
  if (length(sigma) != n) stop("y and D must have the same length")
  if (any(sigma <= 0)) stop("standard errors must be positive")
  if (n < 2L) stop("a meta-study needs at least two studies")

  if (is.null(target)) {
    j <- 1L
    for (i in seq_len(n))
      if (abs(x[i] / sigma[i]) > abs(x[j] / sigma[j])) j <- i
    target <- x[j]; target_se <- sigma[j]
  } else if (is.null(target_se))
    stop("target_se is required when target is given")

  k <- .bnsadt_nfree(family)
  res <- list(family = family, n = n, target = as.numeric(target),
              target_se = as.numeric(target_se),
              target_z = as.numeric(target) / as.numeric(target_se),
              method = paste("Andrews-Kasy publication-bias correction",
                             "with an adversarial bound over the",
                             "selection family"))

  if (fit) {
    f <- morie_bnsadt_fit(x, sigma, family, iters = iters)
    res$mu <- f$mu
    res$tau <- f$tau
    res$betas <- f$betas
    res$group_counts <- f$counts
    res$unidentified <- f$unidentified
    res$tau_at_floor <- f$tau_at_floor
    res$loglik <- f$loglik
    res$estimate <- morie_bnsadt_median_unbiased(target, target_se, family,
                                                 f$betas)
    # The naive number the literature would report, for contrast.
    res$uncorrected <- as.numeric(target)
    res$correction <- res$estimate - as.numeric(target)
    # A likelihood-ratio statistic against no selection at all: the
    # nested model fixes every beta at 1.
    ll0 <- morie_bnsadt_loglik(x, sigma, f$mu, f$tau, "none", numeric(0))
    res$loglik_no_selection <- ll0
    res$lr_statistic <- 2 * (f$loglik - ll0)
    res$lr_df <- k - length(f$unidentified)
    res$lr_p <- if (res$lr_df == 0L) NaN else
      .w3_gammq(res$lr_df / 2, res$lr_statistic / 2)
  }

  if (is.null(grid)) grid <- c(1, 0.8, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.05)
  sweep <- lapply(grid, function(b) {
    ps <- rep(as.numeric(b), k)
    list(beta = as.numeric(b),
         estimate = morie_bnsadt_median_unbiased(target, target_se, family, ps))
  })
  ests <- vapply(sweep, function(s) s$estimate, numeric(1))
  lo_i <- order(ests, seq_along(ests))[1]
  hi_i <- order(-ests, seq_along(ests))[1]
  res$sweep <- sweep
  res$bound_lower <- ests[lo_i]
  res$bound_upper <- ests[hi_i]
  res$bound_lower_beta <- sweep[[lo_i]]$beta
  res$bound_upper_beta <- sweep[[hi_i]]$beta
  res$bound_width <- ests[hi_i] - ests[lo_i]
  res$se <- as.numeric(target_se)
  res
}

#' One-line summary of the bnsadt module
#'
#' @return A character scalar.
#' @export
morie_bnsadt_cheatsheet <- function()
  paste0("bnsadt: Andrews-Kasy publication-bias correction plus the ",
         "adversarial bound over the selection family. families ",
         paste(sort(names(.BNSADT_FAMILIES)), collapse = ", "))
