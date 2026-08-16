# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of baysmplr -- sampler dispatch plus Metropolis, Gibbs, HMC and
# NUTS. Mirrors src/morie/fn/baysmplr.py operation for operation, on the
# shared numerics in R/aaa_helpers_w3num.R and the generator in
# R/aaa_helpers_ghc_rng.R.
#
# The dispatcher is the point, but a dispatcher that cannot run what it
# recommends is a lookup table, so all four samplers are here and the
# choice is made on properties of the problem rather than on taste.
#
# What actually distinguishes them:
#
#   Metropolis-Hastings  needs only the log density. Its random walk
#                        explores at a rate that degrades with dimension
#                        -- the optimal scaling result is that the step
#                        must shrink like 1/sqrt(d) and the acceptance
#                        rate settle near 0.234 -- so the cost of an
#                        effectively independent draw grows like d^2.
#                        Fine in three dimensions, hopeless in three
#                        hundred.
#   Gibbs                needs conditional samplers, which usually means
#                        conjugacy. When they exist it accepts
#                        everything and has no tuning at all; when the
#                        coordinates are strongly correlated it still
#                        crawls, because every move is axis-aligned.
#   HMC                  needs the GRADIENT. It follows Hamiltonian
#                        dynamics, so a proposal can travel a long way
#                        and still be accepted, and the cost per
#                        independent draw grows like d^(5/4) rather than
#                        d^2. The price is two numbers to tune, and the
#                        trajectory length is genuinely hard -- too few
#                        steps and it is a random walk again, too many
#                        and it doubles back on itself.
#   NUTS                 removes the second of those. It doubles the
#                        trajectory until it starts to turn back on
#                        itself, then samples a point from what it
#                        built. Same requirement as HMC, no trajectory
#                        length to choose.
#
# The dispatch rule, stated so it can be disagreed with:
#
#     gradient and d >= 20        -> NUTS
#     gradient and d <  20        -> HMC
#     no gradient, conditionals   -> Gibbs
#     no gradient, d <= 5         -> Metropolis-Hastings
#     no gradient, d >  5         -> Metropolis-Hastings, with a warning
#                                    that it will mix badly and that a
#                                    gradient is worth more than any
#                                    amount of tuning
#
# The threshold at 20 is a judgement, not a theorem: below it HMC's
# fixed trajectory is easy enough to set and cheaper per draw than
# NUTS's doubling, above it choosing the length by hand stops being
# reasonable. It is exposed as nuts_threshold so it can be moved. Every
# result carries `reason`, because a dispatcher that will not say why it
# chose is not usable in an argument.
#
# Diagnostics are computed the same way for every sampler -- acceptance
# rate, effective sample size by the initial-positive-sequence rule --
# so the samplers can be compared rather than each scored on its own
# terms.
#
# References
#   Metropolis, N. et al. (1953) J. Chem. Phys. 21(6), 1087-1092.
#   Hastings, W.K. (1970) Biometrika 57(1), 97-109.
#   Geman, S. and Geman, D. (1984) IEEE PAMI 6(6), 721-741.
#   Duane, S. et al. (1987) Physics Letters B 195(2), 216-222.
#   Neal, R.M. (2011) "MCMC using Hamiltonian dynamics." Handbook of
#     Markov Chain Monte Carlo, chapter 5.
#   Hoffman, M.D. and Gelman, A. (2014) "The No-U-Turn Sampler." JMLR
#     15, 1593-1623. Algorithms 3 and 6.
#   Roberts, G.O., Gelman, A. and Gilks, W.R. (1997) Ann. Appl. Probab.
#     7(1), 110-120.
#   Geyer, C.J. (1992) Statist. Sci. 7(4), 473-483.

.BAYSMPLR_SAMPLERS <- c("mh", "gibbs", "hmc", "nuts")

#' The dispatch decision and the sentence that explains it
#'
#' @param dim Dimension of the parameter.
#' @param has_grad Whether a gradient is available.
#' @param has_conditionals Whether exact conditionals are available.
#' @param nuts_threshold Dimension at or above which NUTS is preferred.
#' @return A list with the sampler name and the reason.
#' @export
morie_baysmplr_choose <- function(dim, has_grad, has_conditionals = FALSE,
                                  nuts_threshold = 20L) {
  if (has_grad && dim >= nuts_threshold)
    return(list(sampler = "nuts",
                reason = sprintf(paste("gradient available and dimension %d at",
                                       "or above the threshold %d, where",
                                       "choosing a trajectory length by hand",
                                       "stops being reasonable"),
                                 as.integer(dim), as.integer(nuts_threshold))))
  if (has_grad)
    return(list(sampler = "hmc",
                reason = sprintf(paste("gradient available and dimension %d",
                                       "below the threshold %d, where a fixed",
                                       "trajectory is easy to set and cheaper",
                                       "per draw than NUTS doubling"),
                                 as.integer(dim), as.integer(nuts_threshold))))
  if (has_conditionals)
    return(list(sampler = "gibbs",
                reason = paste("no gradient, but exact conditionals are",
                               "available, so every move is accepted and",
                               "nothing needs tuning")))
  if (dim <= 5L)
    return(list(sampler = "mh",
                reason = sprintf(paste("no gradient and dimension %d is small,",
                                       "so a random walk is adequate"),
                                 as.integer(dim))))
  list(sampler = "mh",
       reason = sprintf(paste("no gradient and dimension %d is large; a random",
                              "walk will mix badly, and supplying a gradient",
                              "would be worth more than any amount of tuning"),
                        as.integer(dim)))
}

#' Random-walk Metropolis with an isotropic normal proposal
#'
#' The default scale is 2.38 / sqrt(d), the optimal-scaling value for a
#' product target; adapt then tunes it towards target_accept by
#' Robbins-Monro on the log scale during the first half of the run,
#' after which it is frozen so the chain that is kept is homogeneous.
#'
#' @param log_p The log density.
#' @param x0 Starting point.
#' @param n_iter Iterations.
#' @param e A generator environment from .ghc_rng.
#' @param scale Proposal scale, or NULL for the default.
#' @param adapt Tune the scale during the first half.
#' @param target_accept Target acceptance rate.
#' @return A list with the draws, the acceptance rate and the settings.
#' @export
morie_baysmplr_mh <- function(log_p, x0, n_iter, e, scale = NULL,
                              adapt = FALSE, target_accept = 0.234) {
  d <- length(x0)
  if (is.null(scale)) scale <- 2.38 / sqrt(d)
  x <- as.numeric(x0)
  lp <- log_p(x)
  n_iter <- as.integer(n_iter)
  draws <- vector("list", n_iter)
  acc <- 0L
  half <- n_iter %/% 2L
  for (it in seq_len(n_iter)) {
    prop <- x + scale * vapply(seq_len(d), function(j) .ghc_norm(e, 1L),
                               numeric(1))
    lq <- log_p(prop)
    a <- lq - lp
    if (a >= 0 || log(.ghc_unif(e, 1L)) < a) {
      x <- prop; lp <- lq; acc <- acc + 1L; ar <- 1
    } else ar <- 0
    if (adapt && (it - 1L) < half)
      scale <- exp(log(scale) + (ar - target_accept) / sqrt(it))
    draws[[it]] <- x
  }
  list(draws = draws, accept = acc / n_iter, info = list(scale = scale))
}

#' Gibbs on a multivariate normal, using its exact conditionals
#'
#' The conditional of coordinate j given the rest is normal with
#' precision Q_jj and mean mu_j minus the weighted deviations of the
#' others. Written from the precision matrix because that is the form in
#' which the conditionals are trivial -- inverting a covariance to get
#' them and then inverting back is the usual way to make this slower and
#' less accurate than it needs to be.
#'
#' @param mean The mean vector.
#' @param cov_inv The precision matrix.
#' @param x0 Starting point.
#' @param n_iter Iterations.
#' @param e A generator environment from .ghc_rng.
#' @return A list with the draws, the acceptance rate and the settings.
#' @export
morie_baysmplr_gibbs <- function(mean, cov_inv, x0, n_iter, e) {
  d <- length(x0)
  x <- as.numeric(x0)
  n_iter <- as.integer(n_iter)
  draws <- vector("list", n_iter)
  for (it in seq_len(n_iter)) {
    for (j in seq_len(d)) {
      ks <- setdiff(seq_len(d), j)
      s <- .w3_csum(vapply(ks, function(k) cov_inv[j, k] * (x[k] - mean[k]),
                           numeric(1)))
      mj <- mean[j] - s / cov_inv[j, j]
      sj <- sqrt(1 / cov_inv[j, j])
      x[j] <- mj + sj * .ghc_norm(e, 1L)
    }
    draws[[it]] <- x
  }
  # Gibbs accepts everything by construction; reporting 1 is a statement
  # about the algorithm, not a measurement.
  list(draws = draws, accept = 1, info = list())
}

#' .baysmplr_leapfrog
#'
#' A step of the baysmplr_native implementation. Called by \code{.baysmplr_build_tree}, \code{morie_baysmplr_hmc}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param grad See Usage.
#' @param q Numeric; combined arithmetically in the body.
#' @param p Numeric; combined arithmetically in the body.
#' @param eps Numeric; combined arithmetically in the body.
#' @param steps Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{q}, \code{p}, \code{g}.
#' @export
.baysmplr_leapfrog <- function(grad, q, p, eps, steps) {
  g <- grad(q)
  for (t in seq_len(as.integer(steps))) {
    p <- p + 0.5 * eps * g
    q <- q + eps * p
    g <- grad(q)
    p <- p + 0.5 * eps * g
  }
  list(q = q, p = p, g = g)
}

#' Hamiltonian Monte Carlo with a fixed trajectory
#'
#' The momentum is refreshed from a standard normal each iteration and
#' the leapfrog integrator is used because it is symplectic and
#' reversible: the acceptance step corrects only its energy error, and a
#' non-reversible integrator would leave a bias no acceptance rule can
#' remove.
#'
#' @param log_p The log density.
#' @param grad Its gradient.
#' @param x0 Starting point.
#' @param n_iter Iterations.
#' @param e A generator environment from .ghc_rng.
#' @param eps Step size.
#' @param steps Leapfrog steps per iteration.
#' @return A list with the draws, the acceptance rate and the settings.
#' @export
morie_baysmplr_hmc <- function(log_p, grad, x0, n_iter, e, eps = 0.1,
                               steps = 10L) {
  d <- length(x0)
  x <- as.numeric(x0)
  lp <- log_p(x)
  n_iter <- as.integer(n_iter)
  draws <- vector("list", n_iter)
  acc <- 0L
  for (it in seq_len(n_iter)) {
    p0 <- vapply(seq_len(d), function(j) .ghc_norm(e, 1L), numeric(1))
    k0 <- 0.5 * .w3_csum(p0 * p0)
    lf <- .baysmplr_leapfrog(grad, x, p0, eps, steps)
    lq <- log_p(lf$q)
    k1 <- 0.5 * .w3_csum(lf$p * lf$p)
    a <- (lq - k1) - (lp - k0)
    if (a >= 0 || log(.ghc_unif(e, 1L)) < a) {
      x <- lf$q; lp <- lq; acc <- acc + 1L
    }
    draws[[it]] <- x
  }
  list(draws = draws, accept = acc / n_iter,
       info = list(eps = eps, steps = steps))
}

# Hoffman and Gelman's recursive doubling, Algorithm 3.
#' Hoffman and Gelman\'s recursive doubling, Algorithm 3
#'
#' A step of the baysmplr_native implementation. Called by \code{morie_baysmplr_nuts}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param log_p Passed to \code{.baysmplr_build_tree}.
#' @param grad Passed to \code{.baysmplr_leapfrog}.
#' @param q Passed to \code{.baysmplr_leapfrog}.
#' @param p Passed to \code{.baysmplr_leapfrog}.
#' @param u Numeric; passed to \code{log}.
#' @param v Numeric; combined arithmetically in the body.
#' @param j Numeric; combined arithmetically in the body.
#' @param eps Numeric; combined arithmetically in the body.
#' @param e Passed to \code{.baysmplr_build_tree}.
#' @param h0 Numeric; combined arithmetically in the body.
#' @param dmax Numeric; combined arithmetically in the body. Defaults to \code{1000}.
#' @return The value of \code{r}, as built in the body.
#' @export
.baysmplr_build_tree <- function(log_p, grad, q, p, u, v, j, eps, e, h0,
                                 dmax = 1000) {
  if (j == 0L) {
    lf <- .baysmplr_leapfrog(grad, q, p, v * eps, 1L)
    h <- log_p(lf$q) - 0.5 * .w3_csum(lf$p * lf$p)
    n <- if (u <= exp(h)) 1L else 0L
    s <- if (h > log(u) - dmax) 1L else 0L
    return(list(qm = lf$q, pm = lf$p, qp = lf$q, pp = lf$p, q2 = lf$q,
                n = n, s = s, a = min(1, exp(h - h0)), na = 1L))
  }
  r <- .baysmplr_build_tree(log_p, grad, q, p, u, v, j - 1L, eps, e, h0)
  if (r$s == 1L) {
    if (v == -1) {
      r2 <- .baysmplr_build_tree(log_p, grad, r$qm, r$pm, u, v, j - 1L, eps,
                                 e, h0)
      r$qm <- r2$qm; r$pm <- r2$pm
    } else {
      r2 <- .baysmplr_build_tree(log_p, grad, r$qp, r$pp, u, v, j - 1L, eps,
                                 e, h0)
      r$qp <- r2$qp; r$pp <- r2$pp
    }
    if (r$n + r2$n > 0L && .ghc_unif(e, 1L) < r2$n / (r$n + r2$n))
      r$q2 <- r2$q2
    r$a <- r$a + r2$a
    r$na <- r$na + r2$na
    # The no-U-turn condition: stop when the trajectory's two ends are
    # moving towards each other rather than apart.
    dq <- r$qp - r$qm
    r$s <- r2$s * (if (.w3_dot(dq, r$pm) >= 0) 1L else 0L) *
      (if (.w3_dot(dq, r$pp) >= 0) 1L else 0L)
    r$n <- r$n + r2$n
  }
  r
}

#' The No-U-Turn Sampler, with optional dual-averaging warm-up
#'
#' The trajectory doubles until the two ends start approaching each
#' other, which is what removes the trajectory-length parameter. With
#' dual_average the step size is tuned during warm-up by Hoffman and
#' Gelman's Algorithm 6 and then frozen, so the retained chain has a
#' single fixed step size rather than a moving one.
#'
#' @param log_p The log density.
#' @param grad Its gradient.
#' @param x0 Starting point.
#' @param n_iter Iterations.
#' @param e A generator environment from .ghc_rng.
#' @param eps Step size.
#' @param max_depth Doubling limit.
#' @param dual_average Tune the step size during warm-up.
#' @param target_accept Target acceptance statistic.
#' @param warmup Warm-up iterations; half of n_iter by default.
#' @return A list with the draws, the mean acceptance statistic and the
#'   settings.
#' @export
morie_baysmplr_nuts <- function(log_p, grad, x0, n_iter, e, eps = 0.25,
                                max_depth = 8L, dual_average = FALSE,
                                target_accept = 0.8, warmup = NULL) {
  d <- length(x0)
  x <- as.numeric(x0)
  n_iter <- as.integer(n_iter)
  draws <- vector("list", n_iter)
  depths <- integer(n_iter)
  accs <- numeric(n_iter)
  if (is.null(warmup)) warmup <- n_iter %/% 2L
  mu <- log(10 * eps)
  log_eps_bar <- 0
  hbar <- 0
  gamma <- 0.05; t0 <- 10; kappa <- 0.75
  for (it in seq_len(n_iter)) {
    p0 <- vapply(seq_len(d), function(j) .ghc_norm(e, 1L), numeric(1))
    h0 <- log_p(x) - 0.5 * .w3_csum(p0 * p0)
    u <- .ghc_unif(e, 1L) * exp(h0)
    if (u <= 0) u <- 1e-300
    qm <- x; qp <- x; pm <- p0; pp <- p0
    xnew <- x
    n <- 1L; s <- 1L; j <- 0L
    a_sum <- 0; na <- 1L
    while (s == 1L && j < as.integer(max_depth)) {
      v <- if (.ghc_unif(e, 1L) < 0.5) -1 else 1
      if (v == -1) {
        r <- .baysmplr_build_tree(log_p, grad, qm, pm, u, v, j, eps, e, h0)
        qm <- r$qm; pm <- r$pm
      } else {
        r <- .baysmplr_build_tree(log_p, grad, qp, pp, u, v, j, eps, e, h0)
        qp <- r$qp; pp <- r$pp
      }
      if (r$s == 1L && n > 0L && .ghc_unif(e, 1L) < r$n / n) xnew <- r$q2
      a_sum <- a_sum + r$a
      na <- na + r$na
      n <- n + r$n
      dq <- qp - qm
      s <- r$s * (if (.w3_dot(dq, pm) >= 0) 1L else 0L) *
        (if (.w3_dot(dq, pp) >= 0) 1L else 0L)
      j <- j + 1L
    }
    x <- xnew
    depths[it] <- j
    accs[it] <- if (na > 0L) a_sum / na else 0
    if (dual_average && (it - 1L) < warmup) {
      m <- it
      hbar <- (1 - 1 / (m + t0)) * hbar + (target_accept - a_sum / na) / (m + t0)
      log_eps <- mu - sqrt(m) / gamma * hbar
      w <- m^(-kappa)
      log_eps_bar <- w * log_eps + (1 - w) * log_eps_bar
      eps <- exp(log_eps)
    } else if (dual_average && (it - 1L) == warmup) {
      eps <- exp(log_eps_bar)
    }
    draws[[it]] <- x
  }
  list(draws = draws, accept = .w3_csum(accs) / length(accs),
       info = list(eps = eps,
                   mean_depth = .w3_csum(as.numeric(depths)) / length(depths)))
}

#' Geyer's initial positive sequence estimator, per coordinate
#'
#' Sums the autocorrelations in adjacent PAIRS and stops when a pair
#' turns negative. The pairing is not decoration: for a reversible chain
#' the sum of two adjacent autocorrelations is positive, so truncating on
#' a single negative lag stops too early on a noisy tail and inflates the
#' answer.
#'
#' Lags beyond max_lag are not computed. The estimator is meant to stop
#' long before that -- if it has not, the chain has an autocorrelation
#' time comparable to its own length and the number it would return is
#' not trustworthy anyway.
#'
#' @param chain A list of draws.
#' @param max_lag Largest lag computed.
#' @return One effective sample size per coordinate.
#' @export
morie_baysmplr_ess <- function(chain, max_lag = 200L) {
  n <- length(chain)
  d <- length(chain[[1]])
  vapply(seq_len(d), function(cc) {
    v <- vapply(chain, function(row) row[cc], numeric(1))
    mu <- .w3_csum(v) / n
    dev <- v - mu
    var <- .w3_csum(dev * dev) / n
    if (var <= 0) return(as.numeric(n))
    top <- min(n - 1L, as.integer(max_lag))
    rho <- numeric(top)
    for (lag in seq_len(top))
      rho[lag] <- .w3_csum(dev[seq_len(n - lag)] * dev[(lag + 1L):n]) /
        (n * var)
    total <- 0
    k <- 1L
    while (k + 1L <= length(rho)) {
      pair <- rho[k] + rho[k + 1L]
      if (pair <= 0) break
      total <- total + pair
      k <- k + 2L
    }
    if (1 + 2 * total > 0) n / (1 + 2 * total) else as.numeric(n)
  }, numeric(1))
}

#' Pick a sampler for this problem and run it
#'
#' @param log_p Log density, up to a constant, of a numeric vector.
#' @param grad_p Its gradient. Supplying it is what makes HMC and NUTS
#'   available, and the dispatcher will take them when it can.
#' @param x0 Starting point; its length is the dimension.
#' @param n_iter Iterations.
#' @param burn Burn-in; half by default.
#' @param seed Seed for the generator shared with the Python arm.
#' @param sampler Force a sampler instead of dispatching. The reason
#'   field then records that the choice was overridden.
#' @param cov_inv A normal target's precision matrix.
#' @param mean Its mean, which with cov_inv makes Gibbs available.
#' @param eps HMC or NUTS step size.
#' @param steps HMC trajectory length.
#' @param scale Metropolis proposal scale.
#' @param adapt Tune the Metropolis scale during the first half.
#' @param nuts_threshold Dimension at or above which NUTS is preferred.
#' @param dual_average Tune the NUTS step size during warm-up.
#' @param max_depth NUTS doubling limit.
#' @return A list with the chosen sampler and the reason, the posterior
#'   mean and standard deviation per coordinate, the acceptance rate,
#'   the effective sample size and the retained draws.
#' @export
morie_baysmplr <- function(log_p, grad_p = NULL, x0 = NULL, n_iter = 500L,
                           burn = NULL, seed = 1, sampler = NULL,
                           cov_inv = NULL, mean = NULL, eps = 0.1,
                           steps = 10L, scale = NULL, adapt = FALSE,
                           nuts_threshold = 20L, dual_average = FALSE,
                           max_depth = 8L) {
  if (is.null(x0)) stop("x0 is required; its length is the dimension")
  x0 <- as.numeric(x0)
  d <- length(x0)
  if (d < 1L) stop("x0 must be non-empty")
  n_iter <- as.integer(n_iter)
  if (is.null(burn)) burn <- n_iter %/% 2L
  burn <- as.integer(burn)
  if (burn >= n_iter) stop("burn-in consumes every iteration")
  has_cond <- !is.null(cov_inv) && !is.null(mean)
  if (is.null(sampler)) {
    ch <- morie_baysmplr_choose(d, !is.null(grad_p), has_cond, nuts_threshold)
    sampler <- ch$sampler
    reason <- ch$reason
  } else {
    if (!(sampler %in% .BAYSMPLR_SAMPLERS))
      stop("sampler must be one of ",
           paste(.BAYSMPLR_SAMPLERS, collapse = ", "))
    reason <- "forced by the caller, dispatch not consulted"
  }
  if (sampler %in% c("hmc", "nuts") && is.null(grad_p))
    stop(sampler, " needs a gradient")
  if (sampler == "gibbs" && !has_cond) stop("gibbs needs mean and cov_inv")

  e <- .ghc_rng(seed)
  r <- if (sampler == "mh")
    morie_baysmplr_mh(log_p, x0, n_iter, e, scale, adapt)
  else if (sampler == "gibbs")
    morie_baysmplr_gibbs(mean, cov_inv, x0, n_iter, e)
  else if (sampler == "hmc")
    morie_baysmplr_hmc(log_p, grad_p, x0, n_iter, e, eps, steps)
  else
    morie_baysmplr_nuts(log_p, grad_p, x0, n_iter, e, eps, max_depth,
                        dual_average)

  kept <- r$draws[(burn + 1L):n_iter]
  m <- length(kept)
  means <- vapply(seq_len(d), function(cc)
    .w3_csum(vapply(kept, function(row) row[cc], numeric(1))) / m, numeric(1))
  sds <- vapply(seq_len(d), function(cc) {
    if (m > 1L) {
      v <- vapply(kept, function(row) row[cc], numeric(1))
      sqrt(.w3_csum((v - means[cc]) * (v - means[cc])) / (m - 1))
    } else 0
  }, numeric(1))
  ess <- morie_baysmplr_ess(kept)

  list(sampler = sampler, reason = reason, mean = means, sd = sds,
       ess = ess, min_ess = min(ess), ess_per_draw = min(ess) / m,
       accept_rate = r$accept, draws = kept, kept = m, dim = d,
       n_iter = n_iter, burn = burn, info = r$info,
       seed = as.integer(seed), estimate = means[1],
       method = "MCMC sampler dispatch")
}

#' One-line summary of the baysmplr module
#'
#' @return A character scalar.
#' @export
morie_baysmplr_cheatsheet <- function()
  paste0("baysmplr: MCMC sampler dispatch and the samplers themselves. ",
         paste(.BAYSMPLR_SAMPLERS, collapse = ", "))
