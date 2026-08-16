# morie native arm -- likemc
#
# Likelihood-based Metropolis-Hastings for a closed SIR epidemic.
# New infections in step t are lambda_t = beta * S_t * I_t * dt / N,
# observed with Poisson error; the sampler is a random walk on
# (log beta, log gamma), so positivity is automatic and the proposal
# is symmetric, leaving the Hastings ratio equal to the posterior
# ratio.
#
# beta and gamma are badly identified from incidence alone -- the
# early curve pins down R0 = beta/gamma, not either rate separately --
# which is why the posterior is sampled rather than maximised.
#
# Every draw comes from the shared SplitMix64 stream in the same order
# as the Python arm, so the two produce the identical chain draw for
# draw rather than merely chains that agree in distribution.
#
# O'Neill, P. D. & Roberts, G. O. (1999) "Bayesian inference for
# partially observed stochastic epidemics", Journal of the Royal
# Statistical Society: Series A 162(1), 121-129,
# doi:10.1111/1467-985X.00125.

.morie_likemc_incidence <- function(beta, gamma, S0, I0, N, n_steps,
                                    dt = 1) {
  if (beta <= 0 || gamma <= 0) {
    stop("likemc: beta and gamma must be positive")
  }
  if (N <= 0) stop("likemc: the population size must be positive")
  S <- S0
  I <- I0
  out <- numeric(n_steps)
  for (k in seq_len(n_steps)) {
    lam <- max(beta * S * I / N * dt, 1e-12)
    rem <- gamma * I * dt
    out[k] <- lam
    S <- max(S - lam, 0)
    I <- max(I + lam - rem, 0)
  }
  out
}

.morie_likemc_poisll <- function(observed, expected) {
  y <- as.numeric(observed)
  lam <- as.numeric(expected)
  if (length(y) != length(lam)) {
    stop(sprintf("likemc: %d observations but %d expected counts",
                 length(y), length(lam)))
  }
  if (any(y < 0)) stop("likemc: a count cannot be negative")
  L <- pmax(lam, 1e-12)
  sum(y * log(L) - L - lgamma(y + 1))
}

.morie_likemc_lnorm <- function(x, mu, sigma) {
  if (x <= 0) return(-Inf)
  z <- (log(x) - mu) / sigma
  -log(x * sigma * sqrt(2 * pi)) - 0.5 * z * z
}

#' morie_likemc
#'
#' Part of the likemc_native implementation; see the file header for the
#' source it follows.
#'
#' @param model See Usage.
#' @param data See Usage.
#' @param priors See Usage.
#' @param n_iter See Usage.
#' @param seed Defaults to \code{1}.
#' @param step Defaults to \code{0.15}.
#' @param burn Defaults to \code{0}.
#' @return A list with \code{estimate}, \code{beta_mean}, \code{gamma_mean}, \code{chain}, \code{n_draws}, \code{n_iter}, \code{acceptance_rate}, \code{R0_mean}, \code{R0_q025}, \code{R0_median}, \code{R0_q975}, \code{logpost_final}, \code{seed}, \code{step}, \code{method}.
#' @export
morie_likemc <- function(model, data, priors, n_iter, seed = 1,
                         step = 0.15, burn = 0) {
  y <- as.numeric(data)
  if (length(y) < 2) stop("likemc: need at least two observed counts")
  S0 <- as.numeric(model$S0)
  I0 <- as.numeric(model$I0)
  N <- as.numeric(model$N)
  dt <- if (is.null(model$dt)) 1 else as.numeric(model$dt)
  gv <- function(p, nm, d) if (is.null(p[[nm]])) d else as.numeric(p[[nm]])
  bm <- gv(priors, "beta_mu", log(0.5))
  bs <- gv(priors, "beta_sigma", 1)
  gm <- gv(priors, "gamma_mu", log(0.2))
  gs <- gv(priors, "gamma_sigma", 1)
  if (bs <= 0 || gs <= 0) stop("likemc: prior sigmas must be positive")
  it <- as.integer(n_iter)
  if (it < 1L) stop("likemc: need at least one iteration")
  if (step <= 0) stop("likemc: the proposal step must be positive")

  e <- .ghc_rng(as.integer(seed))
  unif <- function() .ghc_unif(e, 1L)
  # Box-Muller from two uniforms, drawn in the Python arm's order
  rnorm1 <- function() {
    u1 <- unif()
    while (u1 <= 0) u1 <- unif()
    u2 <- unif()
    sqrt(-2 * log(u1)) * cos(2 * pi * u2)
  }
  logpost <- function(b, g) {
    if (b <= 0 || g <= 0) return(-Inf)
    lam <- .morie_likemc_incidence(b, g, S0, I0, N, length(y), dt)
    .morie_likemc_poisll(y, lam) + .morie_likemc_lnorm(b, bm, bs) +
      .morie_likemc_lnorm(g, gm, gs)
  }

  b <- exp(bm)
  g <- exp(gm)
  lp <- logpost(b, g)
  chain <- vector("list", it)
  n_acc <- 0L
  for (k in seq_len(it)) {
    pb <- b * exp(step * rnorm1())
    pg <- g * exp(step * rnorm1())
    plp <- logpost(pb, pg)
    if (log(unif()) < plp - lp) {
      b <- pb
      g <- pg
      lp <- plp
      n_acc <- n_acc + 1L
    }
    chain[[k]] <- c(b, g, lp)
  }
  kept <- if (burn > 0) chain[-seq_len(as.integer(burn))] else chain
  if (length(kept) == 0) {
    stop("likemc: the burn-in consumed the whole chain")
  }
  nb <- length(kept)
  mb <- mean(vapply(kept, function(r) r[1], numeric(1)))
  mg <- mean(vapply(kept, function(r) r[2], numeric(1)))
  r0 <- vapply(kept, function(r) r[1] / r[2], numeric(1))
  sr <- sort(r0)
  q <- function(p) sr[min(nb, max(1L, as.integer(p * (nb - 1)) + 1L))]

  list(
    estimate = c(mb, mg), beta_mean = mb, gamma_mean = mg,
    chain = kept, n_draws = nb, n_iter = it,
    acceptance_rate = n_acc / it,
    R0_mean = mean(r0),
    R0_q025 = q(0.025), R0_median = q(0.5), R0_q975 = q(0.975),
    logpost_final = lp, seed = as.integer(seed), step = step,
    method = paste("random-walk Metropolis on (log beta, log gamma)",
                   "with a Poisson SIR incidence likelihood",
                   "(O'Neill & Roberts 1999)")
  )
}
