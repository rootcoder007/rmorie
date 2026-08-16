# Random Network Distillation: a novelty bonus from a random target.
#
# Burda, Y., Edwards, H., Storkey, A., & Klimov, O. (2019) "Exploration by
# Random Network Distillation", ICLR, arXiv:1810.12894.
#
# Two networks. A target f : O -> R^k, randomly initialised and then frozen
# forever. A predictor fhat(.; theta), trained by gradient descent on the
# agent's own observation stream to minimise ||fhat(x; theta) - f(x)||^2.
# The exploration bonus is that same error, r^i_t = ||fhat(x_t; theta) - f(x_t)||^2.
#
# Observation normalisation (whitening + clip to +-5) and intrinsic-reward
# normalisation (divide by running std of intrinsic returns) are both
# implemented as described in section 2.4. combine_returns implements the
# two-value-head decomposition V = V_E + V_I of section 2.3.

# Private: frozen random target net x -> tanh(W1 x + b1) W2, W fixed at init.
#' Private: frozen random target net x -> tanh(W1 x + b1) W2, W fixed at
#' init
#'
#' Part of the rndnet_native implementation; see the file header for the
#' source it follows.
#'
#' @param n_in See Usage.
#' @param n_hidden See Usage.
#' @param n_out See Usage.
#' @param rng See Usage.
#' @param scale Defaults to \code{1}.
#' @return The value of \code{e}, as built in the body.
#' @export
.rndnet_random_features_new <- function(n_in, n_hidden, n_out, rng, scale=1.0) {
  s1 <- scale / sqrt(max(1, n_in))
  s2 <- scale / sqrt(max(1, n_hidden))

  e <- new.env()
  e$n_in <- n_in
  e$n_hidden <- n_hidden
  e$n_out <- n_out

  # W1 : n_in x n_hidden, filled row-by-row to match the Python iteration
  # (outer j, inner i) so the random-number stream is consumed identically.
  u <- .ghc_unif(rng, n_in * n_hidden)
  e$W1 <- matrix(u * 2.0 - 1.0, nrow=n_in, ncol=n_hidden, byrow=TRUE) * s1

  u <- .ghc_unif(rng, n_hidden)
  e$b1 <- (u * 2.0 - 1.0) * s1

  u <- .ghc_unif(rng, n_hidden * n_out)
  e$W2 <- matrix(u * 2.0 - 1.0, nrow=n_hidden, ncol=n_out, byrow=TRUE) * s2

  e$hidden <- function(x) {
    h <- e$b1
    for (j in 1:e$n_in) {
      xj <- x[j]
      if (xj == 0.0) next
      for (i in 1:e$n_hidden) {
        h[i] <- h[i] + e$W1[j, i] * xj
      }
    }
    tanh(h)
  }

  e$call <- function(x) {
    h <- e$hidden(x)
    out <- rep(0.0, e$n_out)
    for (i in 1:e$n_hidden) {
      hi <- h[i]
      if (hi == 0.0) next
      for (o in 1:e$n_out) {
        out[o] <- out[o] + e$W2[i, o] * hi
      }
    }
    out
  }

  e
}

# Private: predictor with the same random feature layer but a trainable
# output weight matrix W, updated by plain SGD on the MSE.
#' Private: predictor with the same random feature layer but a trainable
#'
#' output weight matrix W, updated by plain SGD on the MSE.
#'
#' @param n_in See Usage.
#' @param n_hidden See Usage.
#' @param n_out See Usage.
#' @param rng See Usage.
#' @param scale Defaults to \code{1}.
#' @return The value of \code{e}, as built in the body.
#' @export
.rndnet_predictor_new <- function(n_in, n_hidden, n_out, rng, scale=1.0) {
  e <- new.env()
  e$n_in <- n_in
  e$n_hidden <- n_hidden
  e$n_out <- n_out

  e$feat <- .rndnet_random_features_new(n_in, n_hidden, n_out, rng, scale)
  e$W <- matrix(0.0, nrow=n_hidden, ncol=n_out)

  e$call <- function(x) {
    h <- e$feat$hidden(x)
    out <- rep(0.0, e$n_out)
    for (i in 1:e$n_hidden) {
      hi <- h[i]
      if (hi == 0.0) next
      for (o in 1:e$n_out) {
        out[o] <- out[o] + e$W[i, o] * hi
      }
    }
    list(output=out, features=h)
  }

  e$update <- function(h, err, lr) {
    for (i in 1:e$n_hidden) {
      hi <- h[i]
      if (hi == 0.0) next
      for (o in 1:e$n_out) {
        e$W[i, o] <- e$W[i, o] - lr * 2.0 * err[o] * hi
      }
    }
  }

  e
}

# Private: Welford running mean and variance for the normalisers.
#' Private: Welford running mean and variance for the normalisers
#'
#' Part of the rndnet_native implementation; see the file header for the
#' source it follows.
#'
#' @param n See Usage.
#' @return The value of \code{e}, as built in the body.
#' @export
.rndnet_running_stats_new <- function(n) {
  e <- new.env()
  e$n <- 0
  e$mean <- rep(0.0, n)
  e$m2 <- rep(0.0, n)

  e$update <- function(x) {
    e$n <- e$n + 1
    for (i in seq_along(x)) {
      d <- x[i] - e$mean[i]
      e$mean[i] <- e$mean[i] + d / e$n
      e$m2[i] <- e$m2[i] + d * (x[i] - e$mean[i])
    }
  }

  e$std <- function(eps=1e-8) {
    if (e$n < 2) {
      return(rep(1.0, length(e$mean)))
    }
    sqrt(e$m2 / (e$n - 1)) + eps
  }

  e
}

# Public entry point: compute the RND exploration bonus along an
# observation stream.
#' Public entry point: compute the RND exploration bonus along an
#'
#' observation stream.
#'
#' @param observations See Usage.
#' @param n_hidden Defaults to \code{64}.
#' @param n_out Defaults to \code{8}.
#' @param lr Defaults to \code{0.05}.
#' @param clip Defaults to \code{5}.
#' @param normalize_obs Defaults to \code{TRUE}.
#' @param normalize_reward Defaults to \code{TRUE}.
#' @param init_steps Defaults to \code{0}.
#' @param gamma_int Defaults to \code{0.99}.
#' @param seed Defaults to \code{0}.
#' @param target Defaults to \code{NULL}.
#' @param predictor Defaults to \code{NULL}.
#' @param update Defaults to \code{TRUE}.
#' @return A list with \code{estimate}, \code{intrinsic_reward}, \code{raw_error}, \code{returns}, \code{mse}, \code{mean_first}, \code{mean_last}, \code{n}, \code{target}, \code{predictor}, \code{method}.
#' @export
morie_rndnet <- function(observations, n_hidden=64, n_out=8, lr=0.05, clip=5.0,
                         normalize_obs=TRUE, normalize_reward=TRUE,
                         init_steps=0, gamma_int=0.99, seed=0,
                         target=NULL, predictor=NULL, update=TRUE) {
  # Coerce observations to a (T, d) numeric matrix, accepting the common
  # R idioms (matrix, data.frame, list-of-vectors, numeric vector).
  if (is.matrix(observations)) {
    X <- observations
    storage.mode(X) <- "double"
  } else if (is.data.frame(observations)) {
    X <- as.matrix(observations)
    storage.mode(X) <- "double"
  } else if (is.list(observations)) {
    lens <- lengths(observations)
    if (length(lens) > 0L) {
      d0 <- lens[1L]
      if (any(lens != d0)) {
        stop("rndnet: observations must be rectangular")
      }
    }
    X <- do.call(rbind, lapply(observations, as.numeric))
  } else if (is.numeric(observations)) {
    X <- matrix(as.numeric(observations), nrow=1L)
  } else {
    stop("rndnet: observations must be a matrix, data.frame, list, or numeric vector")
  }

  if (nrow(X) == 0L) stop("rndnet: observations must be non-empty")
  d <- ncol(X)

  init_steps <- as.integer(init_steps)
  if (init_steps < 0L || init_steps >= nrow(X)) {
    stop("rndnet: init_steps must lie in [0, T)")
  }
  clip <- as.numeric(clip)
  if (!(clip > 0.0)) stop("rndnet: clip must be > 0")

  rng1 <- .ghc_rng(seed)
  rng2 <- .ghc_rng(seed + 1L)

  if (is.null(target)) {
    tgt <- .rndnet_random_features_new(d, n_hidden, n_out, rng1)
  } else {
    tgt <- target
  }
  if (is.null(predictor)) {
    prd <- .rndnet_predictor_new(d, n_hidden, n_out, rng2)
  } else {
    prd <- predictor
  }

  obs_stats <- .rndnet_running_stats_new(d)
  for (t in seq_len(init_steps)) {
    obs_stats$update(X[t, , drop=FALSE])
  }

  ret_stats <- .rndnet_running_stats_new(1L)
  running_return <- 0.0
  raw <- numeric(0L)
  rewards <- numeric(0L)
  returns <- numeric(0L)

  for (t in (init_steps + 1L):nrow(X)) {
    x <- X[t, , drop=FALSE]

    if (normalize_obs) {
      obs_stats$update(x)
      sd <- obs_stats$std()
      z <- (x - obs_stats$mean) / sd
      z <- pmax(-clip, pmin(clip, z))
    } else {
      z <- x
    }

    if (is.environment(tgt)) {
      ft <- tgt$call(z)
    } else {
      ft <- tgt(z)
    }

    if (is.environment(prd)) {
      pr <- prd$call(z)
    } else {
      pr <- prd(z)
    }
    fh <- pr$output
    h  <- pr$features

    err <- fh - ft
    e2  <- sum(err * err)
    raw <- c(raw, e2)
    running_return <- gamma_int * running_return + e2
    returns <- c(returns, running_return)
    ret_stats$update(c(running_return))
    if (normalize_reward) {
      rewards <- c(rewards, e2 / ret_stats$std()[1L])
    } else {
      rewards <- c(rewards, e2)
    }
    if (update) {
      prd$update(h, err, lr)
    }
  }

  n <- length(rewards)
  tenth <- max(1L, n %/% 10L)
  list(
    estimate=rewards,
    intrinsic_reward=rewards,
    raw_error=raw,
    returns=returns,
    mse=sum(raw) / n,
    mean_first=sum(rewards[1L:tenth]) / tenth,
    mean_last=sum(rewards[(n - tenth + 1L):n]) / tenth,
    n=n,
    target=tgt,
    predictor=prd,
    method="RND (Burda et al. 2019)"
  )
}

# Public: section 2.3, R = R_E + R_I as two value heads. The extrinsic
# stream is episodic (truncated at each `done`), the intrinsic stream is
# non-episodic.
#' Public: section 2.3, R = R_E + R_I as two value heads. The extrinsic
#'
#' stream is episodic (truncated at each `done`), the intrinsic stream
#' is non-episodic.
#'
#' @param reward_ext See Usage.
#' @param reward_int See Usage.
#' @param gamma_ext Defaults to \code{0.999}.
#' @param gamma_int Defaults to \code{0.99}.
#' @param done Defaults to \code{NULL}.
#' @return A list with \code{estimate}, \code{return_ext}, \code{return_int}, \code{return_total}, \code{gamma_ext}, \code{gamma_int}, \code{method}.
#' @export
morie_rndnet_combine_returns <- function(reward_ext, reward_int,
                                         gamma_ext=0.999, gamma_int=0.99,
                                         done=NULL) {
  re <- as.numeric(reward_ext)
  ri <- as.numeric(reward_int)
  if (length(re) != length(ri)) {
    stop("combine_returns: the two reward streams must have the same length")
  }
  T_len <- length(re)
  if (is.null(done)) {
    d <- rep(FALSE, T_len)
  } else {
    d <- as.logical(done)
  }
  if (length(d) != T_len) {
    stop("combine_returns: done must match the reward length")
  }
  return_ext <- rep(0.0, T_len)
  return_int <- rep(0.0, T_len)
  acc_e <- 0.0
  acc_i <- 0.0
  # Iterate backwards over 1..T_len safely even when T_len == 0.
  for (k in seq_len(T_len)) {
    t <- T_len - k + 1L
    acc_e <- re[t] + (if (d[t]) 0.0 else gamma_ext * acc_e)
    acc_i <- ri[t] + gamma_int * acc_i
    return_ext[t] <- acc_e
    return_int[t] <- acc_i
  }
  list(
    estimate=return_ext + return_int,
    return_ext=return_ext,
    return_int=return_int,
    return_total=return_ext + return_int,
    gamma_ext=gamma_ext,
    gamma_int=gamma_int,
    method="RND two value heads (Burda et al. 2019 sec. 2.3)"
  )
}

# Private one-line reminder of what this module is for.
#' Private one-line reminder of what this module is for
#'
#' Part of the rndnet_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
.rndnet_cheatsheet <- function() {
  paste("rndnet: RND bonus r^i = ||fhat(x) - f(x)||^2 with f a",
        "FROZEN random net (Burda 2019). Random deterministic",
        "target kills the noisy-TV problem. Obs whitened and",
        "clipped to +-5, r^i divided by the running std of",
        "intrinsic RETURNS (sec 2.4). combine_returns is V = V_E +",
        "V_I with the intrinsic stream non-episodic (sec 2.3).")
}

# Compact alias mirroring `random_network_distillation = rndnet`.
morie_rndnet_random_network_distillation <- morie_rndnet
