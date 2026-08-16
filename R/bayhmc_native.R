# No-U-Turn Sampler, with dual-averaging step-size adaptation.
# Sources: Hoffman, M. D. and Gelman, A. (2014) "The No-U-Turn
# Sampler: Adaptively Setting Path Lengths in Hamiltonian Monte
# Carlo", Journal of Machine Learning Research 15, 1593-1623 (the
# NUTS recursion of Algorithm 3 with progressive sampling, the
# Nesterov dual-averaging step-size adaptation of Equation 6, the
# find-reasonable-epsilon scheme of Algorithm 4, and the cap
# Delta_max = 1000 on the simulation error); Metropolis, N.,
# Rosenbluth, A. W., Rosenbluth, M. N., Teller, A. H. and Teller,
# E. (1953) "Equation of State Calculations by Fast Computing
# Machines", Journal of Chemical Physics 21(6), 1087-1092 (the
# accept-reject rule the leapfrog energy error is corrected with
# -- NUTS replaces the fixed-length trajectory and the hand-tuned
# step size, not this).
# Native implementation mirroring Python morie.fn.bayhmc exactly:
# the same leapfrog integrator, the same NUTS recursion with the
# same progressive-sampling scheme, the same dual-averaging
# step-size adaptation during warmup only, the same find-
# reasonable-epsilon scheme, the same cap on the simulation error,
# and the same shared RNG stream so both arms draw the same
# uniforms.

#' No-U-Turn Sampler with dual-averaging step-size adaptation
#'
#' Draws from an unnormalised target by Hamiltonian Monte Carlo,
#' with the trajectory length set by the No-U-Turn criterion and
#' the step size set by Nesterov dual averaging (Hoffman and Gelman
#' 2014). \code{logp} is the log density up to an additive
#' constant; supply \code{grad} or fall back to a central-
#' difference numerical gradient. Set \code{sampler = "hmc"} for
#' the fixed-length Algorithm 1 baseline.
#'
#' @param logp Function \code{(theta)} returning the log density
#'   up to an additive constant.
#' @param theta0 Numeric vector, the starting state.
#' @param n_iter Total number of iterations (warmup + sampling).
#' @param warmup Number of warmup iterations; defaults to
#'   \code{n_iter \%/\% 2} when \code{NULL}.
#' @param grad Function \code{(theta)} returning the gradient of
#'   \code{logp}; when \code{NULL} a central-difference numerical
#'   gradient is used.
#' @param sampler One of \code{"nuts"} (the default) or
#'   \code{"hmc"} (fixed-length Algorithm 1, needs
#'   \code{n_steps}).
#' @param delta Target average Metropolis acceptance, in (0, 1).
#'   Default 0.65, the paper's recommended value.
#' @param eps Optional step size; if \code{NULL} it is set by
#'   Algorithm 4 (find-reasonable-epsilon) before the chain
#'   starts.
#' @param n_steps Leapfrog steps per iteration for the HMC
#'   baseline.
#' @param max_depth Maximum tree depth (doublings) for NUTS.
#' @param gamma,t0,kappa Dual-averaging hyperparameters; the
#'   paper's defaults are 0.05, 10, 0.75.
#' @param seed Seed for the shared generator.
#' @return A list with \code{estimate}, \code{samples},
#'   \code{mean}, \code{variance}, \code{sd}, \code{acceptance},
#'   \code{eps}, \code{eps_trace}, \code{depths}, \code{n_samples},
#'   \code{warmup}, \code{sampler}, \code{delta}, \code{method},
#'   \code{note}.
#' @references Hoffman, M. D. and Gelman, A. (2014). The No-U-Turn
#'   Sampler. Journal of Machine Learning Research, 15, 1593-1623.
#' @references Metropolis, N. et al. (1953). Equation of State
#'   Calculations by Fast Computing Machines. Journal of Chemical
#'   Physics, 21(6), 1087-1092.
#' @export
morie_bayhmc <- function(logp, theta0, n_iter = 1000L, warmup = NULL,
                          grad = NULL, sampler = "nuts", delta = 0.65,
                          eps = NULL, n_steps = 10L, max_depth = 10L,
                          gamma = 0.05, t0 = 10.0, kappa = 0.75,
                          seed = 0L) {
  if (!(sampler %in% c("nuts", "hmc")))
    stop("bayhmc: sampler must be one of ('nuts', 'hmc')")
  theta <- as.numeric(theta0)
  if (length(theta) == 0L) stop("bayhmc: theta0 is empty")
  if (n_iter < 1L) stop("bayhmc: n_iter must be at least 1")
  if (!(delta > 0 && delta < 1))
    stop("bayhmc: delta must be in (0, 1)")
  if (max_depth < 1L || n_steps < 1L)
    stop("bayhmc: max_depth and n_steps must be positive")
  if (!is.null(eps) && eps <= 0) stop("bayhmc: eps must be positive")
  warm <- if (is.null(warmup)) as.integer(n_iter %/% 2L)
          else as.integer(warmup)
  if (warm < 0L || warm > n_iter)
    stop("bayhmc: warmup must be between 0 and n_iter")

  g <- if (!is.null(grad)) grad else function(t) .bayhmc_num_grad(logp, t)
  e <- .ghc_rng(seed)
  norm_one <- function() .ghc_norm(e, 1L)
  unif_one <- function() .ghc_unif(e, 1L)

  if (is.null(eps)) eps <- find_reasonable_epsilon(theta, logp, g, norm_one)
  mu <- log(10.0 * eps)
  h_bar <- 0.0
  log_eps_bar <- 0.0
  draws <- list()
  accepts <- numeric(0)
  depths <- integer(0)
  eps_trace <- numeric(0)

  for (m in seq_len(as.integer(n_iter))) {
    r0 <- vapply(seq_along(theta), function(i) norm_one(), numeric(1))
    joint0 <- .joint(logp, theta, r0)
    if (sampler == "hmc") {
      t2 <- theta
      r2 <- r0
      for (k in seq_len(as.integer(n_steps))) {
        lf <- leapfrog(t2, r2, eps, g)
        t2 <- lf$theta
        r2 <- lf$r
      }
      alpha <- min(1.0, exp(min(.joint(logp, t2, r2) - joint0, 700.0)))
      if (unif_one() < alpha) theta <- t2
      depth <- as.integer(n_steps)
    } else {
      logu <- joint0 + log(max(unif_one(), 1e-300))
      tm <- theta
      tp <- theta
      rm <- r0
      rp <- r0
      j <- 0L
      n <- 1L
      s <- 1L
      alpha <- 0.0
      n_alpha <- 1L
      while (s == 1L && j < as.integer(max_depth)) {
        v <- if (unif_one() < 0.5) 1L else -1L
        if (v == -1L) {
          sub <- build_tree(tm, rm, logu, v, j, eps, logp, g,
                            unif_one, joint0)
          tm <- sub$tm
          rm <- sub$rm
          t2 <- sub$t_p
          n2 <- sub$n
          s2 <- sub$s
          a <- sub$alpha
          na <- sub$na
        } else {
          sub <- build_tree(tp, rp, logu, v, j, eps, logp, g,
                            unif_one, joint0)
          tp <- sub$tp
          rp <- sub$rp
          t2 <- sub$t_p
          n2 <- sub$n
          s2 <- sub$s
          a <- sub$alpha
          na <- sub$na
        }
        if (s2 == 1L && n > 0L && unif_one() < min(1.0, n2 / n)) {
          theta <- t2
        }
        n <- n + n2
        alpha <- a
        n_alpha <- na
        s <- if (no_u_turn(tm, tp, rm, rp)) s2 else 0L
        j <- j + 1L
      }
      depth <- j
      alpha <- alpha / max(as.numeric(n_alpha), 1.0)
    }
    accepts <- c(accepts, alpha)
    depths <- c(depths, depth)
    if (m <= warm) {
      da <- dual_averaging_update(m, h_bar, log_eps_bar,
                                   delta - alpha, mu, gamma, t0, kappa)
      eps <- da$eps
      h_bar <- da$h_bar
      log_eps_bar <- da$log_eps_bar
    } else if (m == warm + 1L) {
      if (warm > 0L) eps <- exp(log_eps_bar)
    }
    eps_trace <- c(eps_trace, eps)
    if (m > warm) draws[[length(draws) + 1L]] <- as.numeric(theta)
  }
  if (length(draws) == 0L) draws <- list(as.numeric(theta))
  d <- length(theta)
  n_post <- length(draws)
  if (n_post > 0L) {
    mat <- do.call(rbind, draws)
    mean_theta <- colMeans(mat)
    var_theta <- colSums((mat -
      matrix(mean_theta, nrow = n_post, ncol = d, byrow = TRUE))^2) /
      max(n_post - 1L, 1L)
  } else {
    mean_theta <- rep(NaN, d)
    var_theta <- rep(NaN, d)
  }
  post <- if (length(accepts) > warm) accepts[(warm + 1L):length(accepts)]
          else accepts
  list(
    estimate = mean_theta,
    samples = draws,
    mean = mean_theta,
    variance = var_theta,
    sd = sqrt(var_theta),
    acceptance = sum(post) / length(post),
    eps = eps,
    eps_trace = eps_trace,
    depths = depths,
    n_samples = as.integer(n_post),
    warmup = as.integer(warm),
    sampler = sampler,
    delta = as.numeric(delta),
    method = if (sampler == "nuts")
      "NUTS (Hoffman & Gelman 2014) with dual-averaging step size"
    else "Hamiltonian Monte Carlo (Algorithm 1)",
    note = paste("adaptation runs during warmup only and the",
                 "averaged epsilon is used afterwards, so the sampled",
                 "chain is a proper Markov chain; gamma=0.05, t0=10,",
                 "kappa=0.75 and delta=0.65 are the paper's values,",
                 "Delta_max=1000")
  )
}

# --- helpers used by morie_bayhmc -------------------------------------
# Defined after the entry point so the file reads top-down; R looks
# them up at call time, so the order does not affect behaviour.

# The paper's recommended cap on the simulation error.
DELTA_MAX <- 1000.0

# Central-difference numerical gradient, used when the caller does
# not supply one. Same step h = 1e-5 as the Python arm.
#' Central-difference numerical gradient, used when the caller does
#'
#' not supply one. Same step h = 1e-5 as the Python arm.
#'
#' @param logp See Usage.
#' @param theta A vector; its length is taken.
#' @param h Numeric; combined arithmetically in the body. Defaults to \code{1e-05}.
#' @return The value of \code{out}, as built in the body.
#' @export
.bayhmc_num_grad <- function(logp, theta, h = 1e-5) {
  d <- length(theta)
  out <- numeric(d)
  for (i in seq_len(d)) {
    up <- theta
    up[i] <- up[i] + h
    dn <- theta
    dn[i] <- dn[i] - h
    out[i] <- (logp(up) - logp(dn)) / (2 * h)
  }
  out
}

# The joint density in (theta, r) up to a constant: log p(theta) +
# log p(r), with p(r) the standard normal so log p(r) = -r.r/2.
#' The joint density in (theta, r) up to a constant: log p(theta) +
#'
#' log p(r), with p(r) the standard normal so log p(r) = -r.r/2.
#'
#' @param logp See Usage.
#' @param theta See Usage.
#' @param r Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.joint <- function(logp, theta, r) {
  logp(theta) - 0.5 * sum(r * r)
}

# One leapfrog step: half-kick, drift, half-kick. Reversible and
# volume-preserving, which lets a single Metropolis test on the
# energy correct the discretisation error exactly.
#' One leapfrog step: half-kick, drift, half-kick. Reversible and
#'
#' volume-preserving, which lets a single Metropolis test on the energy
#' correct the discretisation error exactly.
#'
#' @param theta Numeric; combined arithmetically in the body.
#' @param r Numeric; combined arithmetically in the body.
#' @param eps Numeric; combined arithmetically in the body.
#' @param grad See Usage.
#' @return A list with \code{theta}, \code{r}.
#' @export
leapfrog <- function(theta, r, eps, grad) {
  g <- grad(theta)
  r_half <- r + 0.5 * eps * g
  new_theta <- theta + eps * r_half
  g2 <- grad(new_theta)
  new_r <- r_half + 0.5 * eps * g2
  list(theta = new_theta, r = new_r)
}

# Algorithm 4: double or halve the step size until the Metropolis
# acceptance probability crosses one half. The argument `rnd` is
# the single-value normal generator (the closure around the
# shared RNG).
#' Algorithm 4: double or halve the step size until the Metropolis
#'
#' acceptance probability crosses one half. The argument `rnd` is the
#' single-value normal generator (the closure around the shared RNG).
#'
#' @param theta A vector; its length is taken.
#' @param logp Passed to \code{.joint}.
#' @param grad See Usage.
#' @param rnd See Usage.
#' @param eps Numeric; combined arithmetically in the body. Defaults to \code{1}.
#' @param max_doublings Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.
#' @return The value of \code{eps}, as built in the body.
#' @export
find_reasonable_epsilon <- function(theta, logp, grad, rnd,
                                     eps = 1.0,
                                     max_doublings = 100) {
  r <- vapply(seq_along(theta), function(i) rnd(), numeric(1))
  lf <- leapfrog(theta, r, eps, grad)
  t2 <- lf$theta
  r2 <- lf$r
  log_ratio <- .joint(logp, t2, r2) - .joint(logp, theta, r)
  a <- if (log_ratio > log(0.5)) 1.0 else -1.0
  for (kk in seq_len(as.integer(max_doublings))) {
    if (a * log_ratio <= a * log(0.5)) break
    eps <- eps * (2.0 ^ a)
    lf <- leapfrog(theta, r, eps, grad)
    t2 <- lf$theta
    r2 <- lf$r
    log_ratio <- .joint(logp, t2, r2) - .joint(logp, theta, r)
  }
  eps
}

# Equation 6, one iteration of the Nesterov dual-averaging scheme
# for the log step size. Returns the new step size and the running
# averages carried between calls.
#' Equation 6, one iteration of the Nesterov dual-averaging scheme
#'
#' for the log step size. Returns the new step size and the running
#' averages carried between calls.
#'
#' @param t Numeric; passed to \code{sqrt}.
#' @param h_bar Numeric; combined arithmetically in the body.
#' @param log_eps_bar Numeric; combined arithmetically in the body.
#' @param h_new Numeric; combined arithmetically in the body.
#' @param mu Numeric; combined arithmetically in the body.
#' @param gamma Numeric; combined arithmetically in the body. Defaults to \code{0.05}.
#' @param t0 Numeric; combined arithmetically in the body. Defaults to \code{10}.
#' @param kappa Numeric; combined arithmetically in the body. Defaults to \code{0.75}.
#' @return A list with \code{eps}, \code{h_bar}, \code{log_eps_bar}.
#' @export
dual_averaging_update <- function(t, h_bar, log_eps_bar, h_new, mu,
                                  gamma = 0.05, t0 = 10.0,
                                  kappa = 0.75) {
  if (t < 1)
    stop("bayhmc: the dual averaging step must start at 1")
  if (gamma <= 0 || t0 < 0 || !(0.5 < kappa && kappa <= 1.0))
    stop(paste("bayhmc: gamma must be positive, t0 non-negative",
                "and kappa in (0.5, 1]"))
  eta <- 1.0 / (t + t0)
  h_bar <- (1.0 - eta) * h_bar + eta * h_new
  log_eps <- mu - sqrt(t) / gamma * h_bar
  w <- t ^ (-kappa)
  log_eps_bar <- w * log_eps + (1.0 - w) * log_eps_bar
  list(eps = exp(log_eps), h_bar = h_bar, log_eps_bar = log_eps_bar)
}

# The NUTS stopping rule: has the trajectory begun to double back?
# True (no U-turn) while (theta+ - theta-).r is non-negative at
# both ends.
#' The NUTS stopping rule: has the trajectory begun to double back?
#'
#' True (no U-turn) while (theta+ - theta-).r is non-negative at both
#' ends.
#'
#' @param theta_minus Numeric; combined arithmetically in the body.
#' @param theta_plus Numeric; combined arithmetically in the body.
#' @param r_minus Numeric; combined arithmetically in the body.
#' @param r_plus Numeric; combined arithmetically in the body.
#' @return A logical value.
#' @export
no_u_turn <- function(theta_minus, theta_plus, r_minus, r_plus) {
  d <- theta_plus - theta_minus
  sum(d * r_minus) >= 0 && sum(d * r_plus) >= 0
}

# The recursion of Algorithm 3, returning the paper's nine values:
# the new trajectory ends (theta_minus, r_minus, theta_plus,
# r_plus), the candidate sample t_p, and the tree's bookkeeping
# (count, indicator, summed alpha, alpha normaliser).
#' The recursion of Algorithm 3, returning the paper\'s nine values:
#'
#' the new trajectory ends (theta_minus, r_minus, theta_plus, r_plus),
#' the candidate sample t_p, and the tree\'s bookkeeping (count,
#' indicator, summed alpha, alpha normaliser).
#'
#' @param theta See Usage.
#' @param r See Usage.
#' @param logu Numeric; combined arithmetically in the body.
#' @param v Numeric; combined arithmetically in the body.
#' @param j Numeric; combined arithmetically in the body.
#' @param eps Numeric; combined arithmetically in the body.
#' @param logp Passed to \code{.joint}.
#' @param grad See Usage.
#' @param rnd See Usage.
#' @param joint0 Numeric; combined arithmetically in the body.
#' @return A list, whose contents depend on the branch taken; across the branches its names are \code{tm}, \code{rm}, \code{tp}, \code{rp}, \code{t_p}, \code{n}, \code{s}, \code{alpha}, \code{na}.
#' @export
build_tree <- function(theta, r, logu, v, j, eps, logp, grad, rnd,
                       joint0) {
  if (j == 0L) {
    lf <- leapfrog(theta, r, v * eps, grad)
    t2 <- lf$theta
    r2 <- lf$r
    jj <- .joint(logp, t2, r2)
    n <- if (logu <= jj) 1L else 0L
    s <- if (jj > logu - DELTA_MAX) 1L else 0L
    alpha <- min(1.0, exp(min(jj - joint0, 700.0)))
    list(tm = t2, rm = r2, tp = t2, rp = r2, t_p = t2,
         n = n, s = s, alpha = alpha, na = 1L)
  } else {
    sub <- build_tree(theta, r, logu, v, j - 1L, eps, logp, grad,
                      rnd, joint0)
    tm <- sub$tm
    rm <- sub$rm
    tp <- sub$tp
    rp <- sub$rp
    t_p <- sub$t_p
    n_p <- sub$n
    s_p <- sub$s
    a_p <- sub$alpha
    na_p <- sub$na
    if (s_p == 1L) {
      if (v == -1L) {
        sub2 <- build_tree(tm, rm, logu, v, j - 1L, eps, logp,
                           grad, rnd, joint0)
        tm <- sub2$tm
        rm <- sub2$rm
        t2 <- sub2$t_p
        n2 <- sub2$n
        s2 <- sub2$s
        a2 <- sub2$alpha
        na2 <- sub2$na
      } else {
        sub2 <- build_tree(tp, rp, logu, v, j - 1L, eps, logp,
                           grad, rnd, joint0)
        tp <- sub2$tp
        rp <- sub2$rp
        t2 <- sub2$t_p
        n2 <- sub2$n
        s2 <- sub2$s
        a2 <- sub2$alpha
        na2 <- sub2$na
      }
      if ((n_p + n2) > 0L && rnd() < n2 / (n_p + n2)) {
        t_p <- t2
      }
      a_p <- a_p + a2
      na_p <- na_p + na2
      s_p <- if (no_u_turn(tm, tp, rm, rp)) s2 else 0L
      n_p <- n_p + n2
    }
    list(tm = tm, rm = rm, tp = tp, rp = rp, t_p = t_p,
         n = n_p, s = s_p, alpha = a_p, na = na_p)
  }
}
