# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of tcmech -- truncated concentrated differential privacy for an
# unbounded query. Mirrors src/morie/fn/tcmech.py operation for
# operation, on the shared numerics in R/aaa_helpers_w3num.R and the
# matched random stream in R/aaa_helpers_ghc_rng.R.
#
# A query with no bound on its output has no sensitivity, and a
# mechanism with no sensitivity has no privacy guarantee at all.
# Clipping is what buys one: hold every contribution inside [-C, C] and
# a single record can move the answer by at most the amount C allows.
# The clipping is not a detail, it IS the privacy argument, so the
# number of records it actually bound is reported -- a C so loose that
# nothing was clipped is a C that was chosen to look harmless.
#
# The accounting is concentrated differential privacy, stated through
# the Renyi divergence between the mechanism's output on neighbouring
# inputs:
#
#     (xi, rho)-zCDP:   D_alpha(M(x) || M(x')) <= xi + rho alpha
#                       for all alpha in (1, infinity)
#
# TRUNCATED CDP restricts that quantifier to alpha in (1, omega).
# Bounding the divergence only up to a finite order is a weaker promise,
# and it is the right one for mechanisms whose divergence is well
# behaved near one and blows up further out; the price is that the
# conversion to (epsilon, delta) can no longer optimise alpha freely,
# and this module shows that price rather than hiding it.
#
# Three published facts do all the work: the Gaussian mechanism
# releasing N(q(x), sigma^2) for a sensitivity-Delta query satisfies
# (Delta^2 / 2 sigma^2)-zCDP; rho-zCDP implies
# (rho + 2 sqrt(rho log(1/delta)), delta)-DP; and epsilon-DP implies
# (epsilon^2 / 2)-zCDP. Running the conversion backwards gives the rho a
# target (epsilon, delta) can afford, and the Gaussian proposition turns
# that into a noise scale. The inversion is a fixed-step bisection on a
# monotone function rather than a closed form, so it takes the same
# number of steps on every input and cannot iterate differently in two
# implementations.
#
# With a finite omega the free-alpha conversion is unavailable whenever
# the optimising alpha would exceed it, and the module falls back to the
# fixed-order Renyi bound, epsilon = rho omega + log(1/delta)/(omega-1).
# That branch is Mironov's RDP conversion at a fixed order, cited below;
# it is not from the tCDP paper and is labelled here rather than passed
# off as such.
#
# What this module does NOT implement is the sinh-normal mechanism,
# which is the reason tCDP was introduced -- its parameterisation is not
# something this implementation could reproduce faithfully from a
# description, so it is absent rather than guessed at.
#
# References
#   Bun, M., Dwork, C., Rothblum, G.N. and Steinke, T. (2018)
#     "Composable and versatile privacy via truncated CDP." Proceedings
#     of the 50th Annual ACM SIGACT Symposium on Theory of Computing
#     (STOC), 74-86. doi:10.1145/3188745.3188946.
#   Bun, M. and Steinke, T. (2016) "Concentrated differential privacy:
#     simplifications, extensions, and lower bounds." Theory of
#     Cryptography Conference (TCC), 635-658. arXiv:1605.02065.
#     Definition 1.1, Proposition 1.3, Proposition 1.4, Definition 1.5
#     and Proposition 1.6.
#   Mironov, I. (2017) "Renyi differential privacy." IEEE Computer
#     Security Foundations Symposium (CSF), 263-275.
#   Dwork, C. and Rothblum, G.N. (2016) "Concentrated differential
#     privacy." arXiv:1603.01887.

#' The (epsilon, delta) guarantee a rho-tCDP mechanism gives
#'
#' With no truncation this is Bun and Steinke's Proposition 1.3,
#' exactly. With a finite omega the optimising order may lie outside the
#' range where the divergence is bounded, and the fixed-order Renyi
#' conversion is used instead -- which is strictly worse, as it must be,
#' because a weaker premise cannot give a stronger promise.
#'
#' @param rho The concentrated-privacy budget.
#' @param delta The failure probability.
#' @param omega The truncation order, or NULL for untruncated zCDP.
#' @return The epsilon achieved.
#' @export
morie_tcmech_eps <- function(rho, delta, omega = NULL) {
  rho <- as.numeric(rho); delta <- as.numeric(delta)
  if (rho < 0) stop("rho cannot be negative")
  if (!(delta > 0 && delta < 1))
    stop("delta must lie strictly inside (0, 1)")
  l <- log(1 / delta)
  if (rho == 0) return(0)
  free <- rho + 2 * sqrt(rho * l)
  if (is.null(omega)) return(free)
  w <- as.numeric(omega)
  if (w <= 1) stop("the truncation order must exceed one")
  # The order that minimises the fixed-order bound is 1 + sqrt(l/rho);
  # inside the truncation it reproduces the free conversion, outside it
  # the best available order is omega itself.
  star <- 1 + sqrt(l / rho)
  if (star <= w) return(free)
  rho * w + l / (w - 1)
}

#' The epsilon a truncated guarantee cannot get below, at any rho
#'
#' On the fixed-order branch the bound is rho omega + log(1/delta) /
#' (omega - 1), and the second term does not depend on rho at all. So a
#' tight truncation puts a FLOOR under epsilon that no amount of noise
#' removes -- spending less budget shrinks the first term towards zero
#' and leaves the second exactly where it was. This is a real property
#' of truncating the divergence, not a defect of the implementation, and
#' it is the thing that has to be checked before inverting.
#'
#' @param delta The failure probability.
#' @param omega The truncation order, or NULL.
#' @return The floor, zero when untruncated.
#' @export
morie_tcmech_floor <- function(delta, omega = NULL) {
  if (is.null(omega)) return(0)
  w <- as.numeric(omega)
  if (w <= 1) stop("the truncation order must exceed one")
  log(1 / as.numeric(delta)) / (w - 1)
}

#' The largest rho whose guarantee still meets a target (eps, delta)
#'
#' A fixed-step bisection on a strictly increasing function, so it runs
#' the same number of steps whatever the inputs. The upper bracket is
#' widened by doubling first, which is a fixed schedule and not a
#' search.
#'
#' @param epsilon The target epsilon.
#' @param delta The target delta.
#' @param omega The truncation order, or NULL.
#' @param iters Bisection steps.
#' @return The affordable rho.
#' @export
morie_tcmech_rho <- function(epsilon, delta, omega = NULL, iters = 200L) {
  epsilon <- as.numeric(epsilon)
  if (epsilon <= 0) stop("epsilon must be positive")
  fl <- morie_tcmech_floor(delta, omega)
  if (epsilon <= fl)
    stop("no rho can reach epsilon ", epsilon, " at delta ", delta,
         " with truncation ", omega, ": the fixed-order bound has an ",
         "irreducible term log(1/delta)/(omega - 1) = ", fl,
         " that does not depend on rho. Loosen the truncation, loosen ",
         "delta, or ask for a larger epsilon.")
  lo <- 0; hi <- 1
  for (i in seq_len(60L)) {
    if (morie_tcmech_eps(hi, delta, omega) >= epsilon) break
    hi <- hi * 2
  }
  for (i in seq_len(as.integer(iters))) {
    mid <- 0.5 * (lo + hi)
    if (morie_tcmech_eps(mid, delta, omega) <= epsilon) lo <- mid
    else hi <- mid
  }
  lo
}

#' The noise scale a rho budget buys, from Proposition 1.6
#'
#' rho = Delta^2 / (2 sigma^2), so sigma = Delta / sqrt(2 rho).
#'
#' @param sensitivity The query sensitivity.
#' @param rho The budget.
#' @return The Gaussian noise scale.
#' @export
morie_tcmech_sigma <- function(sensitivity, rho) {
  s <- as.numeric(sensitivity); r <- as.numeric(rho)
  if (s <= 0) stop("the sensitivity must be positive")
  if (r <= 0) stop("rho must be positive to release anything")
  s / sqrt(2 * r)
}

#' Proposition 1.6 the other way round
#'
#' @param sensitivity The query sensitivity.
#' @param sigma The noise scale.
#' @return The implied rho.
#' @export
morie_tcmech_rho_from_sigma <- function(sensitivity, sigma) {
  s <- as.numeric(sensitivity); g <- as.numeric(sigma)
  if (g <= 0) stop("the noise scale must be positive")
  s * s / (2 * g * g)
}

#' Proposition 1.4: epsilon-DP implies (epsilon^2 / 2)-zCDP
#'
#' @param epsilon The pure differential privacy parameter.
#' @return The implied rho.
#' @export
morie_tcmech_rho_from_pure <- function(epsilon) {
  e <- as.numeric(epsilon)
  if (e < 0) stop("epsilon cannot be negative")
  0.5 * e * e
}

#' Composition: the rhos add and the truncation is the tightest one
#'
#' Renyi divergence is additive over independent releases at each order,
#' so the budgets add. A composed mechanism is only bounded at orders
#' where EVERY part is bounded, which is why the truncation takes the
#' minimum -- taking the maximum would claim a guarantee at orders one
#' of the parts never had.
#'
#' @param rhos The per-release budgets.
#' @param omegas The per-release truncations, or NULL.
#' @return A list with the total rho and the composed truncation.
#' @export
morie_tcmech_compose <- function(rhos, omegas = NULL) {
  total <- if (length(rhos)) .w3_csum(as.numeric(rhos)) else 0
  if (is.null(omegas)) return(list(rho = total, omega = NULL))
  live <- as.numeric(omegas[!vapply(omegas, is.null, logical(1))])
  list(rho = total, omega = if (length(live)) min(live) else NULL)
}

#' Release a clipped query under a target (epsilon, delta) budget
#'
#' @param y The per-record contributions. Clipping them is what gives
#'   the query a sensitivity at all.
#' @param f_value The query value being protected.
#' @param C The clipping bound, and hence the sensitivity of one record.
#' @param epsilon The target epsilon.
#' @param delta The target delta.
#' @param omega The truncation order, or NULL.
#' @param seed The random stream.
#' @param n_release How many releases the budget is split across.
#' @return A list with the private answer, the noise scale, the budget
#'   in rho, how many records the clipping bound, and the guarantee
#'   actually achieved.
#' @export
morie_tcmech <- function(y, f_value, C, epsilon, delta, omega = NULL,
                         seed = 0, n_release = 1L) {
  vals <- as.numeric(y)
  c0 <- as.numeric(C)
  if (c0 <= 0) stop("the clipping bound must be positive")
  k <- as.integer(n_release)
  if (k < 1L) stop("there must be at least one release")
  clipped <- numeric(length(vals))
  n_clipped <- 0L
  for (i in seq_along(vals)) {
    if (vals[i] > c0) { clipped[i] <- c0; n_clipped <- n_clipped + 1L }
    else if (vals[i] < -c0) { clipped[i] <- -c0; n_clipped <- n_clipped + 1L }
    else clipped[i] <- vals[i]
  }

  rho_total <- morie_tcmech_rho(epsilon, delta, omega)
  rho_each <- rho_total / k
  sigma <- morie_tcmech_sigma(c0, rho_each)
  e <- .ghc_rng(seed)
  noise <- .ghc_norm(e, 1L, 0, sigma)
  private <- as.numeric(f_value) + noise
  achieved <- morie_tcmech_eps(rho_total, delta, omega)
  free <- morie_tcmech_eps(rho_total, delta, NULL)
  list(private_value = private, estimate = private, se = sigma,
       noise = noise, sigma = sigma, rho_total = rho_total,
       rho_per_release = rho_each, epsilon_target = as.numeric(epsilon),
       epsilon_achieved = achieved, delta = as.numeric(delta),
       omega = omega,
       truncation_binds = !is.null(omega) && achieved >= free - 1e-15 &&
         achieved != free,
       clipped = clipped, n_clipped = n_clipped, n = length(vals),
       sensitivity = c0, n_release = k, f_value = as.numeric(f_value),
       method = "truncated CDP Gaussian mechanism")
}

#' One-line summary of the tcmech module
#'
#' @return A character scalar.
#' @export
morie_tcmech_cheatsheet <- function()
  paste0("tcmech: truncated CDP Gaussian mechanism. clip to bound the ",
         "sensitivity, rho from the target (eps, delta), sigma from ",
         "Delta / sqrt(2 rho)")
