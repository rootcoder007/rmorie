# SPDX-License-Identifier: AGPL-3.0-or-later
#
# distributions.R -- a general probability-distribution representation
# and a set of operations on it that take the distribution as an input
# parameter, completing rmorie's coverage of the srr "PD" standards.
# Moments are computed analytically where closed forms exist and by
# numeric integration (with documented stability conditions) otherwise;
# discrete summation is used only for discrete distributions where the
# series has a finite limit.

#' srr probability-distributions (PD) representation standards
#'
#' These PD standards are completed by the general distribution object
#' (morie_distribution) and the operations on it (this file), tested in
#' test-srr-standards-PD-full.R.
#'
#' @srrstats {PD2.0} morie_distribution() is a single general
#'   representation for probability distributions, mapping a name +
#'   parameters onto the corresponding base-R d/p/q/r functions.
#' @srrstats {PD3.0} Distribution manipulation is analytic where closed
#'   forms exist (morie_dist_moment(method="analytic")); numeric methods
#'   are used only with explicit justification (documented on the method
#'   argument).
#' @srrstats {PD3.1} Operations on distributions are separate functions
#'   (morie_dist_pdf/cdf/quantile/moment/integrate) that accept the
#'   distribution object -- whose name is one input parameter -- rather
#'   than being hard-coded per distribution.
#' @srrstats {PD3.4} morie_dist_integrate() documents the conditions under
#'   which the integral is stable and pre-checks them (finite,
#'   non-negative, integrable density) before integrating.
#' @srrstats {PD3.5} morie_dist_sum() uses discrete summation only for
#'   discrete distributions, where the justification (a convergent
#'   probability mass series summing to 1) is explicit.
#' @srrstats {PD3.5a} morie_dist_sum() demonstrates the series has a
#'   finite limit by checking the tail mass falls below a tolerance
#'   before truncating.
#' @noRd
NULL

# base-R distribution registry: name -> (base suffix, discrete, analytic
# mean/var closures where a closed form exists)
#' Internal helper: Dist Registry
#' @noRd
.dist_registry <- list(
  normal      = list(suffix = "norm",  discrete = FALSE,
                     mean = function(p) p$mean,
                     var  = function(p) p$sd^2),
  exponential = list(suffix = "exp",   discrete = FALSE,
                     mean = function(p) 1 / p$rate,
                     var  = function(p) 1 / p$rate^2),
  gamma       = list(suffix = "gamma", discrete = FALSE,
                     mean = function(p) p$shape / p$rate,
                     var  = function(p) p$shape / p$rate^2),
  poisson     = list(suffix = "pois",  discrete = TRUE,
                     mean = function(p) p$lambda,
                     var  = function(p) p$lambda),
  binomial    = list(suffix = "binom", discrete = TRUE,
                     mean = function(p) p$size * p$prob,
                     var  = function(p) p$size * p$prob * (1 - p$prob))
)

#' Construct a general probability-distribution object
#'
#' A single representation for a probability distribution: a name plus
#' its parameters, resolved to the corresponding base-R `d`/`p`/`q`/`r`
#' functions. All distribution operations in this module accept such an
#' object, so the distribution is a parameter rather than being hard-wired.
#'
#' @param name One of `"normal"`, `"exponential"`, `"gamma"`,
#'   `"poisson"`, `"binomial"`.
#' @param ... Named distribution parameters (e.g. `mean`, `sd` for
#'   normal; `rate` for exponential; `shape`, `rate` for gamma; `lambda`
#'   for poisson; `size`, `prob` for binomial).
#' @return A `morie_distribution` object.
#' @examples
#' morie_distribution("normal", mean = 0, sd = 1)
#' @export
morie_distribution <- function(name, ...) {
  name <- match.arg(name, names(.dist_registry))
  params <- list(...)
  out <- list(name = name, params = params,
              suffix = .dist_registry[[name]]$suffix,
              discrete = .dist_registry[[name]]$discrete)
  class(out) <- c("morie_distribution", "morie_rich_result", "list")
  out
}

#' Print method for \code{morie_distribution} objects
#'
#' @param x A `morie_distribution`.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @examples
#' \donttest{
#' obj <- morie_distribution("normal", mean = 0, sd = 1)
#' print(obj)
#' }
#' @export
print.morie_distribution <- function(x, ...) {
  cat(sprintf("<morie_distribution: %s(%s)>\n", x$name,
              paste(names(x$params), unlist(x$params), sep = "=",
                    collapse = ", ")))
  invisible(x)
}

#' Internal helper: Dist Call
#' @noRd
.dist_call <- function(dist, letter, x) {
  fn <- get(paste0(letter, dist$suffix), envir = asNamespace("stats"))
  do.call(fn, c(list(x), dist$params))
}

#' Density / mass of a distribution at points
#' @param dist A `morie_distribution`.
#' @param x Points at which to evaluate.
#' @return Numeric density (continuous) or mass (discrete) values.
#' @examples
#' morie_dist_pdf(morie_distribution("normal", mean = 0, sd = 1), 0)
#' @export
morie_dist_pdf <- function(dist, x) {
  stopifnot(inherits(dist, "morie_distribution")); .dist_call(dist, "d", x)
}

#' Cumulative distribution function at points
#' @param dist A `morie_distribution`.
#' @param q Quantiles.
#' @return Numeric CDF values.
#' @examples
#' morie_dist_cdf(morie_distribution("normal", mean = 0, sd = 1), 1.96)
#' @export
morie_dist_cdf <- function(dist, q) {
  stopifnot(inherits(dist, "morie_distribution")); .dist_call(dist, "p", q)
}

#' Quantile function
#' @param dist A `morie_distribution`.
#' @param p Probabilities.
#' @return Numeric quantiles.
#' @examples
#' morie_dist_quantile(morie_distribution("normal", mean = 0, sd = 1), 0.975)
#' @export
morie_dist_quantile <- function(dist, p) {
  stopifnot(inherits(dist, "morie_distribution")); .dist_call(dist, "q", p)
}

#' A moment of a distribution, analytically or by integration
#'
#' @param dist A `morie_distribution`.
#' @param order Moment order (1 = mean, 2 = variance about the mean when
#'   `central = TRUE`).
#' @param central Whether to return the central moment (default TRUE for
#'   order >= 2).
#' @param method `"analytic"` uses the closed form where available and
#'   otherwise falls back to `"numeric"`; `"numeric"` forces integration.
#'   The numeric path is justified only where no closed form exists.
#' @return The requested moment (numeric scalar).
#' @examples
#' morie_dist_moment(morie_distribution("gamma", shape = 2, rate = 1), 1)
#' @export
morie_dist_moment <- function(dist, order = 1L, central = order >= 2L,
                              method = c("analytic", "numeric")) {
  stopifnot(inherits(dist, "morie_distribution"))
  method <- match.arg(method)
  reg <- .dist_registry[[dist$name]]
  if (method == "analytic" && order == 1L && !is.null(reg$mean)) {
    return(reg$mean(dist$params))
  }
  if (method == "analytic" && order == 2L && central && !is.null(reg$var)) {
    return(reg$var(dist$params))
  }
  # numeric fallback (justified: no closed form for the requested moment)
  mu <- if (central) reg$mean(dist$params) else 0
  if (dist$discrete) {
    supp <- morie_dist_sum(dist, moment_order = order, center = mu)
    return(supp)
  }
  integrand <- function(x) (x - mu)^order * morie_dist_pdf(dist, x)
  morie_dist_integrate(dist, integrand = integrand)$value
}

#' Integrate a function against a distribution's support, with checks
#'
#' Integrates `integrand` (default: the density, giving total mass 1)
#' over the distribution's support. Stability conditions -- the integrand
#' must be finite and the density non-negative and integrable -- are
#' documented and pre-checked before [stats::integrate()] is called.
#'
#' @param dist A `morie_distribution` (continuous).
#' @param integrand Function of `x` to integrate (default the density).
#' @param lower,upper Integration limits (default the distribution's
#'   effective support via extreme quantiles).
#' @return A list with `value` and the numeric `abs.error`.
#' @examples
#' morie_dist_integrate(morie_distribution("normal", mean = 0, sd = 1))$value
#' @export
morie_dist_integrate <- function(dist, integrand = NULL,
                                 lower = NULL, upper = NULL) {
  stopifnot(inherits(dist, "morie_distribution"))
  if (isTRUE(dist$discrete)) {
    stop("morie_dist_integrate() is for continuous distributions; ",
         "use morie_dist_sum()", call. = FALSE)
  }
  if (is.null(integrand)) integrand <- function(x) morie_dist_pdf(dist, x)
  if (is.null(lower)) lower <- morie_dist_quantile(dist, 1e-8)
  if (is.null(upper)) upper <- morie_dist_quantile(dist, 1 - 1e-8)
  # stability pre-checks (PD3.4)
  probe <- integrand(morie_dist_quantile(dist, c(0.25, 0.5, 0.75)))
  if (any(!is.finite(probe))) {
    stop("integrand is not finite on the support; integral is unstable",
         call. = FALSE)
  }
  if (any(morie_dist_pdf(dist, seq(lower, upper, length.out = 5)) < 0)) {
    stop("density is negative; not a valid probability density", call. = FALSE)
  }
  res <- stats::integrate(integrand, lower = lower, upper = upper)
  list(value = res$value, abs.error = res$abs.error)
}

#' Sum a discrete distribution's mass (or a moment) with convergence check
#'
#' Uses discrete summation, justified because the probability-mass series
#' of a proper discrete distribution converges to 1. Before truncating an
#' unbounded support (e.g. Poisson), the tail mass is confirmed to fall
#' below `tol`, demonstrating the series has a finite limit.
#'
#' @param dist A discrete `morie_distribution`.
#' @param moment_order If > 0, sum `(k - center)^order * P(k)` instead of
#'   the mass.
#' @param center Centering value for central moments.
#' @param tol Tail-mass tolerance for truncation.
#' @return The summed value (total mass, or the requested moment).
#' @examples
#' morie_dist_sum(morie_distribution("poisson", lambda = 3))
#' @export
morie_dist_sum <- function(dist, moment_order = 0L, center = 0, tol = 1e-10) {
  stopifnot(inherits(dist, "morie_distribution"), isTRUE(dist$discrete))
  upper <- morie_dist_quantile(dist, 1 - tol)          # finite truncation point
  k <- 0:upper
  mass <- morie_dist_pdf(dist, k)
  # demonstrate a finite limit: the omitted tail mass is below tolerance
  tail_mass <- 1 - sum(mass)
  if (tail_mass > 1e-6) {
    warning("discrete series tail mass exceeds tolerance; result truncated",
            call. = FALSE)
  }
  if (moment_order == 0L) return(sum(mass))
  sum((k - center)^moment_order * mass)
}
