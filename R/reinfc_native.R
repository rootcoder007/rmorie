# Williams, R. J. (1992) "Simple Statistical Gradient-Following Algorithms
# for Connectionist Reinforcement Learning", Machine Learning 8, 229-256.
#
# REINFORCE: episodic and immediate-reinforcement policy gradients.
# Eq. 2:  Delta w_ij = alpha_ij (r - b_ij) d ln g_i / d w_ij
# Units:  bernoulli (eq. 5, L_R-I), bernoulli-logistic (eqs. 7-9),
#         gaussian (eqs. 13-14).
# Baselines: none, comparison (eq. 10), mean.
# Modes:  immediate (Thm 1), episodic (Thm 2).
# Theorem 1: E{Delta W}' grad E{r} >= 0 for any baseline b.

#' .reinfc_logistic
#'
#' Part of the reinfc_native implementation; see the file header for the
#' source it follows.
#'
#' @param s See Usage.
#' @return A numeric value.
#' @export
.reinfc_logistic <- function(s) {
  if (s >= 0.0) {
    return(1.0 / (1.0 + exp(-s)))
  }
  e <- exp(s)
  return(e / (1.0 + e))
}

#' .reinfc_as_matrix
#'
#' Part of the reinfc_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param name See Usage.
#' @return The value of \code{x_mat}, as built in the body.
#' @export
.reinfc_as_matrix <- function(x, name) {
  if (is.null(x)) {
    stop(sprintf("reinfc: %s must be non-empty", name))
  }
  if (is.null(dim(x))) {
    x_vec <- as.numeric(x)
    if (length(x_vec) == 0) {
      stop(sprintf("reinfc: %s must be non-empty", name))
    }
    x_mat <- matrix(x_vec, nrow = 1)
  } else {
    x_mat <- as.matrix(x)
    storage.mode(x_mat) <- "double"
  }
  if (nrow(x_mat) == 0 || ncol(x_mat) == 0) {
    stop(sprintf("reinfc: %s must be non-empty", name))
  }
  return(x_mat)
}

#' .reinfc_baseline_series
#'
#' Part of the reinfc_native implementation; see the file header for the
#' source it follows.
#'
#' @param rewards See Usage.
#' @param baseline See Usage.
#' @param gamma See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.reinfc_baseline_series <- function(rewards, baseline, gamma) {
  n <- length(rewards)
  if (baseline == "none") {
    return(rep(0.0, n))
  }
  out <- rep(0.0, n)
  if (baseline == "comparison") {
    rbar <- 0.0
    for (t in seq_len(n)) {
      out[t] <- rbar
      rbar <- gamma * rbar + (1.0 - gamma) * rewards[t]
    }
    return(out)
  }
  total <- 0.0
  for (t in seq_len(n)) {
    out[t] <- if (t > 1) total / (t - 1) else 0.0
    total <- total + rewards[t]
  }
  return(out)
}

#' .reinfc_finish
#'
#' Part of the reinfc_native implementation; see the file header for the
#' source it follows.
#'
#' @param param See Usage.
#' @param rewards See Usage.
#' @param bs See Usage.
#' @param traj See Usage.
#' @return A list with \code{estimate}, \code{rewards}, \code{baseline}, \code{trajectory}, \code{n_trials}, \code{mean_reward_first}, \code{mean_reward_last}, \code{method}.
#' @export
.reinfc_finish <- function(param, rewards, bs, traj) {
  n <- length(rewards)
  tenth <- max(1, n %/% 10)
  first_part <- rewards[1:tenth]
  last_part <- rewards[(n - tenth + 1):n]
  list(
    estimate = as.list(param),
    rewards = as.numeric(rewards),
    baseline = as.numeric(bs),
    trajectory = traj,
    n_trials = as.integer(n),
    mean_reward_first = as.numeric(sum(first_part) / tenth),
    mean_reward_last = as.numeric(sum(last_part) / tenth),
    method = "REINFORCE (Williams 1992)"
  )
}

#' .reinfc_running_baseline
#'
#' Part of the reinfc_native implementation; see the file header for the
#' source it follows.
#'
#' @param state See Usage.
#' @param baseline See Usage.
#' @param gamma See Usage.
#' @return A numeric value.
#' @export
.reinfc_running_baseline <- function(state, baseline, gamma) {
  if (baseline == "none") {
    return(0.0)
  }
  if (baseline == "comparison") {
    return(state$v1)
  }
  if (state$v2 == 0) {
    return(0.0)
  }
  return(state$v1 / state$v2)
}

#' .reinfc_advance_baseline
#'
#' Part of the reinfc_native implementation; see the file header for the
#' source it follows.
#'
#' @param state See Usage.
#' @param baseline See Usage.
#' @param gamma See Usage.
#' @param r See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.reinfc_advance_baseline <- function(state, baseline, gamma, r) {
  if (baseline == "comparison") {
    state$v1 <- gamma * state$v1 + (1.0 - gamma) * r
  } else if (baseline == "mean") {
    state$v1 <- state$v1 + r
    state$v2 <- state$v2 + 1.0
  }
}

#' .reinfc_run_bernoulli
#'
#' Part of the reinfc_native implementation; see the file header for the
#' source it follows.
#'
#' @param reward_fn See Usage.
#' @param pv See Usage.
#' @param baseline See Usage.
#' @param mode See Usage.
#' @param rho See Usage.
#' @param gamma See Usage.
#' @param k See Usage.
#' @param trials See Usage.
#' @param rng See Usage.
#' @return The value of \code{.reinfc_finish}.
#' @export
.reinfc_run_bernoulli <- function(reward_fn, pv, baseline, mode, rho, gamma, k, trials, rng) {
  n <- length(pv)
  p <- pv
  state <- new.env()
  state$v1 <- 0.0
  state$v2 <- 0.0
  rewards <- numeric(trials)
  bs <- numeric(trials)
  traj <- vector("list", trials)
  for (trial_idx in seq_len(trials)) {
    elig <- numeric(n)
    ys <- vector("list", k)
    for (step in seq_len(k)) {
      u <- .ghc_unif(rng, n)
      y <- as.numeric(u < p)
      ys[[step]] <- y
      for (i in seq_len(n)) {
        elig[i] <- elig[i] + (y[i] - p[i])
      }
    }
    if (mode == "episodic") {
      r <- as.numeric(reward_fn(ys, NULL))
    } else {
      r <- as.numeric(reward_fn(ys[[1]], NULL))
    }
    b <- .reinfc_running_baseline(state, baseline, gamma)
    for (i in seq_len(n)) {
      p[i] <- p[i] + rho * (r - b) * elig[i]
      p[i] <- min(1.0 - 1e-12, max(1e-12, p[i]))
    }
    .reinfc_advance_baseline(state, baseline, gamma, r)
    rewards[trial_idx] <- r
    bs[trial_idx] <- b
    traj[[trial_idx]] <- as.list(p)
  }
  .reinfc_finish(p, rewards, bs, traj)
}

#' .reinfc_run_logistic
#'
#' Part of the reinfc_native implementation; see the file header for the
#' source it follows.
#'
#' @param reward_fn See Usage.
#' @param xs See Usage.
#' @param wm See Usage.
#' @param baseline See Usage.
#' @param mode See Usage.
#' @param alpha See Usage.
#' @param gamma See Usage.
#' @param k See Usage.
#' @param trials See Usage.
#' @param eligibility See Usage.
#' @param rng See Usage.
#' @return The value of \code{result}, as built in the body.
#' @export
.reinfc_run_logistic <- function(reward_fn, xs, wm, baseline, mode, alpha, gamma, k, trials,
                                 eligibility, rng) {
  n_units <- nrow(wm)
  n_in <- ncol(wm)
  w <- wm
  state <- new.env()
  state$v1 <- 0.0
  state$v2 <- 0.0
  ybar <- rep(0.5, n_units)
  rewards <- numeric(trials)
  bs <- numeric(trials)
  traj <- vector("list", trials)
  for (t in seq_len(trials)) {
    elig <- matrix(0.0, nrow = n_units, ncol = n_in)
    ys <- vector("list", k)
    for (step in seq_len(k)) {
      xrow_idx <- (((t - 1) * k + (step - 1)) %% nrow(xs)) + 1
      xrow <- xs[xrow_idx, ]
      s_vec <- as.numeric(w %*% xrow)
      pi_vec <- sapply(s_vec, .reinfc_logistic)
      u <- .ghc_unif(rng, n_units)
      y <- as.numeric(u < pi_vec)
      ys[[step]] <- y
      for (i in seq_len(n_units)) {
        ref <- if (eligibility == "ybar") ybar[i] else pi_vec[i]
        for (j in seq_len(n_in)) {
          elig[i, j] <- elig[i, j] + (y[i] - ref) * xrow[j]
        }
      }
    }
    xrow_idx <- (((t - 1) * k) %% nrow(xs)) + 1
    x_input <- xs[xrow_idx, ]
    if (mode == "episodic") {
      r <- as.numeric(reward_fn(ys, x_input))
    } else {
      r <- as.numeric(reward_fn(ys[[1]], x_input))
    }
    b <- .reinfc_running_baseline(state, baseline, gamma)
    w <- w + alpha * (r - b) * elig
    if (eligibility == "ybar") {
      for (i in seq_len(n_units)) {
        mean_y <- mean(sapply(ys, function(yy) yy[i]))
        ybar[i] <- gamma * ybar[i] + (1.0 - gamma) * mean_y
      }
    }
    .reinfc_advance_baseline(state, baseline, gamma, r)
    rewards[t] <- r
    bs[t] <- b
    traj[[t]] <- as.list(as.vector(t(w)))
  }
  flat <- as.vector(t(w))
  result <- .reinfc_finish(flat, rewards, bs, traj)
  result$weights <- lapply(seq_len(nrow(w)), function(i) as.numeric(w[i, ]))
  result
}

#' .reinfc_run_gaussian
#'
#' Part of the reinfc_native implementation; see the file header for the
#' source it follows.
#'
#' @param reward_fn See Usage.
#' @param mu See Usage.
#' @param sigma See Usage.
#' @param baseline See Usage.
#' @param mode See Usage.
#' @param alpha See Usage.
#' @param gamma See Usage.
#' @param k See Usage.
#' @param trials See Usage.
#' @param rate_scaling See Usage.
#' @param rng See Usage.
#' @return The value of \code{result}, as built in the body.
#' @export
.reinfc_run_gaussian <- function(reward_fn, mu, sigma, baseline, mode, alpha, gamma, k,
                                 trials, rate_scaling, rng) {
  if (!(rate_scaling %in% c("sigma2", "none"))) {
    stop("reinfc: rate_scaling must be 'sigma2' or 'none'")
  }
  state <- new.env()
  state$v1 <- 0.0
  state$v2 <- 0.0
  rewards <- numeric(trials)
  bs <- numeric(trials)
  traj <- vector("list", trials)
  for (trial_idx in seq_len(trials)) {
    e_mu <- 0.0
    e_sig <- 0.0
    ys <- numeric(k)
    for (step in seq_len(k)) {
      u1 <- .ghc_unif(rng, 1)
      u2 <- .ghc_unif(rng, 1)
      z <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
      y <- mu + sigma * z
      ys[step] <- y
      e_mu <- e_mu + (y - mu) / (sigma * sigma)
      e_sig <- e_sig + ((y - mu)^2 - sigma * sigma) / (sigma^3)
    }
    if (mode == "episodic") {
      r <- as.numeric(reward_fn(ys, NULL))
    } else {
      r <- as.numeric(reward_fn(ys[1], NULL))
    }
    b <- .reinfc_running_baseline(state, baseline, gamma)
    rate <- if (rate_scaling == "sigma2") alpha * sigma * sigma else alpha
    mu <- mu + rate * (r - b) * e_mu
    sigma <- sigma + rate * (r - b) * e_sig
    if (sigma <= 1e-12) {
      sigma <- 1e-12
    }
    .reinfc_advance_baseline(state, baseline, gamma, r)
    rewards[trial_idx] <- r
    bs[trial_idx] <- b
    traj[[trial_idx]] <- list(mu, sigma)
  }
  result <- .reinfc_finish(c(mu, sigma), rewards, bs, traj)
  result$mu <- as.numeric(mu)
  result$sigma <- as.numeric(sigma)
  result
}

#' morie_reinfc
#'
#' Part of the reinfc_native implementation; see the file header for the
#' source it follows.
#'
#' @param reward_fn See Usage.
#' @param x Defaults to \code{NULL}.
#' @param w Defaults to \code{NULL}.
#' @param p Defaults to \code{NULL}.
#' @param mu Defaults to \code{0}.
#' @param sigma Defaults to \code{1}.
#' @param unit Defaults to \code{"bernoulli-logistic"}.
#' @param baseline Defaults to \code{"comparison"}.
#' @param mode Defaults to \code{"immediate"}.
#' @param alpha Defaults to \code{0.1}.
#' @param gamma Defaults to \code{0.9}.
#' @param rho Defaults to \code{0.1}.
#' @param episode_length Defaults to \code{1}.
#' @param trials Defaults to \code{100}.
#' @param eligibility Defaults to \code{"p"}.
#' @param rate_scaling Defaults to \code{"sigma2"}.
#' @param seed Defaults to \code{0}.
#' @return The value of \code{.reinfc_run_logistic}.
#' @export
morie_reinfc <- function(reward_fn, x = NULL, w = NULL, p = NULL, mu = 0.0, sigma = 1.0,
                         unit = "bernoulli-logistic", baseline = "comparison", mode = "immediate",
                         alpha = 0.1, gamma = 0.9, rho = 0.1, episode_length = 1, trials = 100,
                         eligibility = "p", rate_scaling = "sigma2", seed = 0) {
  units_ok <- c("bernoulli", "bernoulli-logistic", "gaussian")
  baselines_ok <- c("none", "comparison", "mean")
  modes_ok <- c("immediate", "episodic")
  if (!(unit %in% units_ok)) {
    stop(sprintf("reinfc: unit must be one of %s, got %s",
                 paste(units_ok, collapse = ", "), unit))
  }
  if (!(baseline %in% baselines_ok)) {
    stop(sprintf("reinfc: baseline must be one of %s, got %s",
                 paste(baselines_ok, collapse = ", "), baseline))
  }
  if (!(mode %in% modes_ok)) {
    stop(sprintf("reinfc: mode must be one of %s, got %s",
                 paste(modes_ok, collapse = ", "), mode))
  }
  if (!(eligibility %in% c("p", "ybar"))) {
    stop(sprintf("reinfc: eligibility must be 'p' or 'ybar', got %s", eligibility))
  }
  if (!is.function(reward_fn)) {
    stop("reinfc: reward_fn must be callable")
  }
  trials <- as.integer(trials)
  if (trials < 1) {
    stop("reinfc: trials must be >= 1")
  }
  k <- as.integer(episode_length)
  if (k < 1) {
    stop("reinfc: episode_length must be >= 1")
  }
  if (mode == "immediate") {
    k <- 1
  }
  rng <- .ghc_rng(seed)
  if (unit == "gaussian") {
    sigma <- as.numeric(sigma)
    if (sigma <= 0.0) {
      stop("reinfc: sigma must be > 0")
    }
    return(.reinfc_run_gaussian(reward_fn, as.numeric(mu), sigma, baseline, mode,
                                as.numeric(alpha), as.numeric(gamma), k, trials,
                                rate_scaling, rng))
  }
  if (unit == "bernoulli") {
    if (is.null(p)) {
      pv <- 0.5
    } else {
      pv <- as.numeric(p)
    }
    if (any(pv <= 0 | pv >= 1)) {
      stop("reinfc: p must lie strictly in (0, 1)")
    }
    return(.reinfc_run_bernoulli(reward_fn, pv, baseline, mode, as.numeric(rho),
                                 as.numeric(gamma), k, trials, rng))
  }
  if (is.null(x)) {
    xs <- matrix(1.0, nrow = 1, ncol = 1)
  } else {
    xs <- .reinfc_as_matrix(x, "x")
  }
  n_in <- ncol(xs)
  if (is.null(w)) {
    wm <- matrix(0.0, nrow = 1, ncol = n_in)
  } else {
    wm <- .reinfc_as_matrix(w, "w")
    if (ncol(wm) != n_in) {
      stop(sprintf("reinfc: w has %d columns but x has %d", ncol(wm), n_in))
    }
  }
  .reinfc_run_logistic(reward_fn, xs, wm, baseline, mode, as.numeric(alpha),
                       as.numeric(gamma), k, trials, eligibility, rng)
}

morie_reinforce <- morie_reinfc

#' morie_reinfc_expected_update
#'
#' Part of the reinfc_native implementation; see the file header for the
#' source it follows.
#'
#' @param p See Usage.
#' @param r0 See Usage.
#' @param r1 See Usage.
#' @param alpha Defaults to \code{1}.
#' @param b Defaults to \code{0}.
#' @return The value of \code{list}.
#' @export
morie_reinfc_expected_update <- function(p, r0, r1, alpha = 1.0, b = 0.0) {
  p <- as.numeric(p)
  if (p <= 0 || p >= 1) {
    stop("expected_update: p must lie strictly in (0, 1)")
  }
  upd <- alpha * ((1.0 - p) * (r0 - b) * (0.0 - p) + p * (r1 - b) * (1.0 - p))
  grad <- alpha * p * (1.0 - p) * (r1 - r0)
  list(as.numeric(upd), as.numeric(grad))
}

#' morie_reinfc_cheatsheet
#'
#' Part of the reinfc_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
morie_reinfc_cheatsheet <- function() {
  paste("reinfc: REINFORCE, Delta w = alpha (r - b) dln g/dw ",
        "(Williams 1992 eq. 2). Units bernoulli (eq. 5, L_R-I), ",
        "bernoulli-logistic (eqs. 7-9), gaussian (eqs. 13-14); ",
        "baselines none/comparison (eq. 10)/mean; modes immediate ",
        "(Thm 1) and episodic (Thm 2, eligibilities summed over the ",
        "episode). E{dW}'grad E{r} >= 0 for every baseline.",
        sep = "")
}
