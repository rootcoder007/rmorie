# Adversarial Inverse Reinforcement Learning: recovering a *reward*,
# not just a policy.
#
# Sources: Fu, J., Luo, K., & Levine, S. (2018) "Learning Robust Rewards
# with Adversarial Inverse Reinforcement Learning", ICLR, arXiv:1710.11248
# (eq. 4, Algorithm 1, Theorem C.1); plus the Python reference
# implementation airl_python_reference.py in this directory.
#
# Native R implementation mirroring the Python arm of morie.fn.airl
# exactly: same logistic discriminator with the AIRL f = g(s) +
# gamma*h(s') - h(s) parameterisation, same tabular compress-then-fit
# gradient ascent, same line-6 reward r = log D - log(1 - D), the same
# soft value iteration, and the same RichResult payload keys.

# Internal helpers: floor-protected log and canonical (state, action)
# keys. Python's _key returns a hashable (int / str / tuple of floats);
# R has no tuples-as-keys, so we collapse to a string at full double
# precision and join components with U+001F, which cannot appear in any
# sprintf("%.17g", .) output. The one behaviour difference between the
# two arms is that an int action and a float action with the same value
# share a key here but not in Python; in practice users pass one type.

#' .log
#'
#' A step of the airl_native implementation. Called by \code{morie_airl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; passed to \code{max}.
#' @return A numeric value.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .log(x = x)
#' res
.log <- function(x) log(max(x, 1e-300))

#' .state_key
#'
#' A step of the airl_native implementation. Called by \code{morie_soft_value_iteration}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s A vector; its length is taken.
#' @return A character value.
#' @export
#' @examples
#' txt <- c('alpha', 'beta', 'gamma', 'delta')
#' res <- .state_key(s = txt)
#' res
.state_key <- function(s) {
  if (is.character(s)) {
    return(s)
  }
  if (is.list(s)) s <- unlist(s, use.names = FALSE)
  s <- as.numeric(s)
  if (length(s) == 0L) {
    return("")
  }
  paste0(sprintf("%.17g", s), collapse = "\x1f")
}

#' .action_key
#'
#' A step of the airl_native implementation. Called by \code{morie_airl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; its length is taken.
#' @return A character value.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' res <- .action_key(a = A)
#' res
.action_key <- function(a) {
  if (is.character(a)) {
    return(a)
  }
  if (is.list(a)) a <- unlist(a, use.names = FALSE)
  if (is.numeric(a) && length(a) == 1L) {
    return(sprintf("%.17g", as.numeric(a)))
  }
  as.character(a)
}

#' Adversarial Inverse Reinforcement Learning
#'
#' Fits the AIRL discriminator (Fu, Luo & Levine 2018, eq. 4) on expert and
#' policy transitions and reads off the recovered reward `log D - log(1 - D)` for
#' each policy transition (line 6 of Algorithm 1). The discriminator `D = exp(f)
#' / (exp(f) + pi)` with `f = g(s) + gamma h(s') - h(s)` separates expert (`D ->
#' 1`) from policy (`D -> 0`) data. `state_only = TRUE` (the default, Theorem
#' C.1) parameterises `g` on the state alone so the recovered `g*` is the ground
#' truth reward up to a constant. `state_only = FALSE` uses the `g(s, a)` of eq.
#' 4 as written, which fits at least as well but recovers the advantage rather
#' than a transferable reward. AIRL has no random draws: the result is fully
#' determined by the inputs, so no RNG is consulted and `set.seed()` is neither
#' needed nor called.
#'
#' Fits the AIRL discriminator (Fu, Luo & Levine 2018, eq. 4) on
#' expert and policy transitions and reads off the recovered reward
#' \code{log D - log(1 - D)} for each policy transition (line 6 of
#' Algorithm 1). The discriminator
#' \code{D = exp(f) / (exp(f) + pi)} with
#' \eqn{f = g(s) + gamma h(s') - h(s)} separates expert
#' (\code{D -> 1}) from policy (\code{D -> 0}) data.
#'
#' \code{state_only = TRUE} (the default, Theorem C.1) parameterises
#' \code{g} on the state alone so the recovered \code{g*} is the ground
#' truth reward up to a constant. \code{state_only = FALSE} uses the
#' \code{g(s, a)} of eq. 4 as written, which fits at least as well but
#' recovers the advantage rather than a transferable reward.
#'
#' AIRL has no random draws: the result is fully determined by the
#' inputs, so no RNG is consulted and \code{set.seed()} is neither
#' needed nor called.
#'
#' @param expert_states,expert_actions,expert_next Expert transitions
#'   \eqn{(s, a, s')}. States may be numeric (matrix / vector / list),
#'   strings, or integers; actions may be any value.
#' @param expert_log_policy \code{log pi(a | s)} under the CURRENT
#'   policy for each expert transition. AIRL's discriminator is
#'   explicitly a function of \code{pi}, so this is not optional
#'   bookkeeping.
#' @param policy_states,policy_actions,policy_next Transitions sampled
#'   from the current policy.
#' @param policy_log_policy \code{log pi(a | s)} under the current
#'   policy for each policy transition.
#' @param gamma Discount used in the shaping term of eq. 4.
#' @param state_only If \code{TRUE} (default, Theorem C.1) parameterise
#'   \code{g} on the state alone; if \code{FALSE} (eq. 4 as written)
#'   parameterise on \code{(s, a)}.
#' @param lr Learning rate for full-batch gradient ascent.
#' @param epochs Number of gradient-ascent iterations.
#' @param l2 Optional ridge penalty on \code{g} and \code{h}.
#' @return A named list with \code{estimate}, \code{reward}, \code{g},
#'   \code{h}, \code{f_policy}, \code{f_expert}, \code{D_policy},
#'   \code{D_expert}, \code{accuracy}, \code{log_likelihood},
#'   \code{gamma}, \code{state_only}, \code{method}.
#' @references Fu, J., Luo, K., & Levine, S. (2018). Learning Robust
#'   Rewards with Adversarial Inverse Reinforcement Learning. ICLR.
#'   arXiv:1710.11248.
#' @export
#' @examples
#' set.seed(1)
#' r <- morie_airl(expert_states = rnorm(10), expert_actions = rnorm(10), expert_next = rnorm(10),
#'   expert_log_policy = rnorm(10), policy_states = rnorm(10), policy_actions = rnorm(10),
#'   policy_next = rnorm(10), policy_log_policy = rnorm(10))
#' TRUE
#' @keywords internal
morie_airl <- function(expert_states, expert_actions, expert_next,
                       expert_log_policy,
                       policy_states, policy_actions, policy_next,
                       policy_log_policy,
                       gamma = 0.99, state_only = TRUE, lr = 0.1,
                       epochs = 500L, l2 = 0.0) {
  to_rows <- function(x) {
    if (is.matrix(x) || is.data.frame(x)) {
      lapply(seq_len(nrow(x)), function(i) x[i, ])
    } else {
      as.list(x)
    }
  }
  prep <- function(S, A, S1, LP, name) {
    S <- lapply(to_rows(S), .state_key)
    S1 <- lapply(to_rows(S1), .state_key)
    A <- as.list(A)
    LP <- as.numeric(LP)
    n <- length(S)
    if (!(length(A) == length(S1) && length(A) == length(LP) &&
      length(A) == n) || n == 0L) {
      stop(paste0(
        "airl: ", name,
        " states, actions, next states and log_policy must be non-empty and the same length"
      ))
    }
    Map(
      function(s, a, s1, lp) list(s = s, a = a, s1 = s1, lp = lp),
      S, A, S1, LP
    )
  }

  E <- prep(
    expert_states, expert_actions, expert_next,
    expert_log_policy, "expert"
  )
  P <- prep(
    policy_states, policy_actions, policy_next,
    policy_log_policy, "policy"
  )

  states <- sort(unique(c(
    vapply(E, function(t) t$s, character(1)),
    vapply(E, function(t) t$s1, character(1)),
    vapply(P, function(t) t$s, character(1)),
    vapply(P, function(t) t$s1, character(1))
  )))
  if (state_only) {
    gkeys <- states
  } else {
    gkeys <- sort(unique(vapply(c(E, P), function(t) {
      paste0(t$s, "\x1f", .action_key(t$a))
    }, character(1))))
  }
  gi <- setNames(seq_along(gkeys), gkeys)
  hi <- setNames(seq_along(states), states)
  ng <- length(gkeys)
  nh <- length(states)
  g <- rep(0, ng)
  h <- rep(0, nh)
  gamma <- as.numeric(gamma)
  lr <- as.numeric(lr)
  l2 <- as.numeric(l2)

  gkey_of <- function(t) {
    if (state_only) {
      t$s
    } else {
      paste0(t$s, "\x1f", .action_key(t$a))
    }
  }
  f_of <- function(t) {
    g[match(gkey_of(t), names(gi), nomatch = 0L)] +
      gamma * h[match(t$s1, names(hi), nomatch = 0L)] -
      h[match(t$s, names(hi), nomatch = 0L)]
  }
  d_of <- function(t) {
    z <- f_of(t) - t$lp
    if (z >= 0) {
      1 / (1 + exp(-z))
    } else {
      e <- exp(z)
      e / (1 + e)
    }
  }

  # The model is tabular, so identical transitions contribute identical
  # gradients. Collapse to weighted unique rows: the gradient is
  # unchanged and the fit stops being quadratic in the number of samples.
  full_key <- function(t) {
    paste0(
      gkey_of(t), "\x1f", t$s1, "\x1f", t$s, "\x1f",
      sprintf("%.17g", t$lp)
    )
  }
  compress <- function(rows) {
    keys <- vapply(rows, full_key, character(1))
    uniq_keys <- unique(keys)
    counts <- tabulate(match(keys, uniq_keys))
    n <- length(rows)
    list(rows = rows[match(uniq_keys, keys)], wgt = counts / n)
  }

  Ec <- compress(E)
  Pc <- compress(P)
  n_epochs <- max(1L, as.integer(epochs))
  for (iter in seq_len(n_epochs)) {
    dg <- rep(0, ng)
    dh <- rep(0, nh)
    # Binary logistic regression, expert labelled 1, policy 0.
    # d/df log D       =  1 - D    (expert)
    # d/df log (1 - D) =    - D    (policy)
    for (k in seq_along(Ec$rows)) {
      t <- Ec$rows[[k]]
      gi_k <- match(gkey_of(t), names(gi), nomatch = 0L)
      hi_s1 <- match(t$s1, names(hi), nomatch = 0L)
      hi_s <- match(t$s, names(hi), nomatch = 0L)
      c_val <- (1 - d_of(t)) * Ec$wgt[k]
      dg[gi_k] <- dg[gi_k] + c_val
      dh[hi_s1] <- dh[hi_s1] + c_val * gamma
      dh[hi_s] <- dh[hi_s] - c_val
    }
    for (k in seq_along(Pc$rows)) {
      t <- Pc$rows[[k]]
      gi_k <- match(gkey_of(t), names(gi), nomatch = 0L)
      hi_s1 <- match(t$s1, names(hi), nomatch = 0L)
      hi_s <- match(t$s, names(hi), nomatch = 0L)
      c_val <- -d_of(t) * Pc$wgt[k]
      dg[gi_k] <- dg[gi_k] + c_val
      dh[hi_s1] <- dh[hi_s1] + c_val * gamma
      dh[hi_s] <- dh[hi_s] - c_val
    }
    g <- g + lr * (dg - l2 * g)
    h <- h + lr * (dh - l2 * h)
  }

  de <- vapply(E, d_of, numeric(1))
  dp <- vapply(P, d_of, numeric(1))
  # line 6: r = log D - log(1 - D), which equals f - log pi exactly.
  reward <- vapply(dp, function(v) .log(v) - .log(1 - v), numeric(1))
  ll <- (sum(vapply(de, .log, numeric(1))) / length(de) +
    sum(vapply(dp, function(v) .log(1 - v), numeric(1))) / length(dp))
  acc <- (sum(de > 0.5) + sum(dp <= 0.5)) / (length(de) + length(dp))

  list(
    estimate = reward,
    reward = reward,
    g = setNames(g, gkeys),
    h = setNames(h, states),
    f_policy = vapply(P, f_of, numeric(1)),
    f_expert = vapply(E, f_of, numeric(1)),
    D_policy = dp,
    D_expert = de,
    accuracy = as.numeric(acc),
    log_likelihood = as.numeric(ll),
    gamma = gamma,
    state_only = as.logical(state_only),
    method = "AIRL (Fu, Luo & Levine 2018, eq. 4 + Alg. 1)"
  )
}

#' MaxEnt (soft) value iteration on a deterministic tabular MDP
#'
#' \eqn{Q(s, a) = r(s) + gamma V(s')},
#' \code{V(s) = log sum_a exp Q(s, a)}, with the soft-optimal policy
#' \code{pi(a | s) = exp(Q(s, a) - V(s))}. AIRL is derived in the
#' maximum-entropy IRL setting, so this is the \code{V*} that Theorem
#' C.1's \code{h* = V* + const} refers to. Provided so a caller -- and
#' the anchors -- can compute the ground truth independently of
#' anything AIRL fitted.
#'
#' @param states Iterable of states.
#' @param actions Iterable of actions.
#' @param step Function \eqn{(s, a) -> s'}.
#' @param reward Function \code{s -> r(s)}.
#' @param gamma Discount.
#' @param iters Maximum number of value-iteration sweeps.
#' @param tol Convergence tolerance on the sup-norm of \code{V}.
#' @return A list with \code{V} (named numeric vector keyed by state)
#'   and \code{pi} (named numeric vector keyed by
#'   \code{"<state>|<action>"}).
#' @references Fu, J., Luo, K., & Levine, S. (2018). Learning Robust
#'   Rewards with Adversarial Inverse Reinforcement Learning. ICLR.
#'   arXiv:1710.11248.
#' @export
#' @keywords internal
morie_soft_value_iteration <- function(states, actions, step, reward,
                                       gamma = 0.9, iters = 2000L,
                                       tol = 1e-14) {
  S <- as.list(states)
  A <- as.list(actions)
  S_keys <- vapply(S, .state_key, character(1))
  V <- setNames(rep(0, length(S_keys)), S_keys)
  gamma <- as.numeric(gamma)
  for (iter in seq_len(as.integer(iters))) {
    newV <- numeric(length(S_keys))
    for (i in seq_along(S_keys)) {
      s <- S[[i]]
      qs <- vapply(A, function(a) {
        as.numeric(reward(s)) + gamma * V[[.state_key(step(s, a))]]
      }, numeric(1))
      m <- max(qs)
      newV[i] <- m + log(sum(exp(qs - m)))
    }
    delta <- max(abs(newV - V))
    V <- setNames(newV, S_keys)
    if (delta < tol) break
  }
  pi_keys <- character(0)
  pi_vals <- numeric(0)
  for (i in seq_along(S_keys)) {
    s <- S[[i]]
    qs <- vapply(A, function(a) {
      as.numeric(reward(s)) + gamma * V[[.state_key(step(s, a))]]
    }, numeric(1))
    new_keys <- paste0(S_keys[i], "\x1f", vapply(A, .action_key, character(1)))
    pi_keys <- c(pi_keys, new_keys)
    pi_vals <- c(pi_vals, exp(qs - V[[S_keys[i]]]))
  }
  list(V = V, pi = setNames(pi_vals, pi_keys))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Adversarial Inverse Reinforcement Learning
#'
#' Fit the AIRL discriminator and return the recovered reward,
#' following Fu, Luo & Levine (2018) eq. 4 and Algorithm 1.
#' @param expert_states,expert_actions,expert_next Expert transitions.
#' @param expert_log_policy log pi(a|s) under the *current* policy.
#' @param policy_states,policy_actions,policy_next Policy transitions.
#' @param policy_log_policy log pi(a|s) for the policy transitions.
#' @param gamma Discount used in the shaping term.
#' @param state_only TRUE: parameterise g on s alone (Theorem C.1);
#'   FALSE: g(s, a).
#' @param lr,epochs,l2 Full-batch gradient ascent on the logistic
#'   log-likelihood with optional ridge penalty.
#' @return List with reward (line 6 of Algorithm 1), g, h, f_policy,
#'   f_expert, D_policy, D_expert, accuracy, log_likelihood, gamma,
#'   state_only, method.
#' @export
airl <- function(expert_states, expert_actions, expert_next,
                 expert_log_policy, policy_states, policy_actions,
                 policy_next, policy_log_policy, gamma = 0.99,
                 state_only = TRUE, lr = 0.1, epochs = 500L, l2 = 0) {
  Eprep <- .airl_prep(expert_states, expert_actions, expert_next,
                      expert_log_policy, "expert")
  Pprep <- .airl_prep(policy_states, policy_actions, policy_next,
                      policy_log_policy, "policy")
  nE <- length(Eprep$S)
  nP <- length(Pprep$S)
  Es <- lapply(seq_len(nE), function(k) list(S = Eprep$S[[k]],
                                             A = Eprep$A[[k]],
                                             S1 = Eprep$S1[[k]],
                                             LP = Eprep$LP[k]))
  Ps <- lapply(seq_len(nP), function(k) list(S = Pprep$S[[k]],
                                             A = Pprep$A[[k]],
                                             S1 = Pprep$S1[[k]],
                                             LP = Pprep$LP[k]))
  all_states <- unique(c(lapply(Es, function(t) t$S),
                         lapply(Es, function(t) t$S1),
                         lapply(Ps, function(t) t$S),
                         lapply(Ps, function(t) t$S1)))
  if (state_only) {
    gkeys <- all_states
  } else {
    gkeys <- unique(c(lapply(Es, function(t) list(t$S, t$A)),
                      lapply(Ps, function(t) list(t$S, t$A))))
  }
  gi <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_along(gkeys)) assign(deparse(gkeys[[i]], control = "useSource"),
                                     i, envir = gi)
  hi <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_along(all_states)) assign(deparse(all_states[[i]],
                                                  control = "useSource"),
                                          i, envir = hi)
  ng <- length(gkeys)
  nh <- length(all_states)
  g <- rep(0, ng)
  h <- rep(0, nh)
  gamma <- as.numeric(gamma)
  lr <- as.numeric(lr)
  l2 <- as.numeric(l2)

  gkey_of <- function(t) if (state_only) t$S else list(t$S, t$A)
  gi_idx <- function(k)
    get(deparse(k, control = "useSource"), envir = gi, inherits = FALSE)
  hi_idx <- function(k)
    get(deparse(k, control = "useSource"), envir = hi, inherits = FALSE)
  f_of <- function(t) g[gi_idx(gkey_of(t))] + gamma * h[hi_idx(t$S1)] -
    h[hi_idx(t$S)]
  d_of <- function(t) {
    z <- f_of(t) - t$LP
    if (z >= 0) 1 / (1 + exp(-z)) else { ez <- exp(z)
    ez / (1 + ez) }
  }

  # Compress to unique transitions: identical rows contribute identical
  # gradients, so the fit is unchanged and the cost stays bounded.
  Ec <- list()
  Pc <- list()
  Ecount <- new.env(hash = TRUE, parent = emptyenv())
  for (t in Es) {
    key <- paste0(deparse(t$S, control = "useSource"), "|",
                  deparse(t$A, control = "useSource"), "|",
                  deparse(t$S1, control = "useSource"), "|", t$LP)
    assign(key, (get(key, envir = Ecount, inherits = FALSE) %||% 0) + 1,
           envir = Ecount)
  }
  Ekeys <- ls(Ecount)
  for (k in Ekeys) {
    parts <- strsplit(k, "|", fixed = TRUE)[[1]]
    Ec[[length(Ec) + 1L]] <- list(t = list(S = .airl_key_from_str(parts[1]),
                                           A = .airl_key_from_str(parts[2]),
                                           S1 = .airl_key_from_str(parts[3]),
                                           LP = as.numeric(parts[4])),
                                  w = get(k, envir = Ecount) / nE)
  }
  Pcount <- new.env(hash = TRUE, parent = emptyenv())
  for (t in Ps) {
    key <- paste0(deparse(t$S, control = "useSource"), "|",
                  deparse(t$A, control = "useSource"), "|",
                  deparse(t$S1, control = "useSource"), "|", t$LP)
    assign(key, (get(key, envir = Pcount, inherits = FALSE) %||% 0) + 1,
           envir = Pcount)
  }
  Pkeys <- ls(Pcount)
  for (k in Pkeys) {
    parts <- strsplit(k, "|", fixed = TRUE)[[1]]
    Pc[[length(Pc) + 1L]] <- list(t = list(S = .airl_key_from_str(parts[1]),
                                           A = .airl_key_from_str(parts[2]),
                                           S1 = .airl_key_from_str(parts[3]),
                                           LP = as.numeric(parts[4])),
                                  w = get(k, envir = Pcount) / nP)
  }

  for (ep in seq_len(max(1L, as.integer(epochs)))) {
    dg <- rep(0, ng)
    dh <- rep(0, nh)
    for (row in Ec) {
      t <- row$t
      wgt <- row$w
      c <- (1 - d_of(t)) * wgt
      dg[gi_idx(gkey_of(t))] <- dg[gi_idx(gkey_of(t))] + c
      dh[hi_idx(t$S1)] <- dh[hi_idx(t$S1)] + c * gamma
      dh[hi_idx(t$S)] <- dh[hi_idx(t$S)] - c
    }
    for (row in Pc) {
      t <- row$t
      wgt <- row$w
      c <- -d_of(t) * wgt
      dg[gi_idx(gkey_of(t))] <- dg[gi_idx(gkey_of(t))] + c
      dh[hi_idx(t$S1)] <- dh[hi_idx(t$S1)] + c * gamma
      dh[hi_idx(t$S)] <- dh[hi_idx(t$S)] - c
    }
    g <- g + lr * (dg - l2 * g)
    h <- h + lr * (dh - l2 * h)
  }

  de <- vapply(Es, function(t) d_of(t), numeric(1))
  dp <- vapply(Ps, function(t) d_of(t), numeric(1))
  # line 6: r = log D - log(1 - D), equivalent to f - log pi.
  reward <- .airl_log(dp) - .airl_log(1 - dp)
  ll <- mean(.airl_log(de)) + mean(.airl_log(1 - dp))
  acc <- (sum(de > 0.5) + sum(dp <= 0.5)) / (length(de) + length(dp))

  g_map <- list()
  for (k in gkeys) g_map[[length(g_map) + 1L]] <-
    setNames(list(g[gi_idx(k)]), deparse(k, control = "useSource"))
  h_map <- list()
  for (k in all_states) h_map[[length(h_map) + 1L]] <-
    setNames(list(h[hi_idx(k)]), deparse(k, control = "useSource"))

  list(estimate = reward, reward = reward, g = g_map, h = h_map,
       f_policy = vapply(Ps, f_of, numeric(1)),
       f_expert = vapply(Es, f_of, numeric(1)),
       D_policy = dp, D_expert = de, accuracy = as.numeric(acc),
       log_likelihood = as.numeric(ll), gamma = gamma,
       state_only = as.logical(state_only),
       method = "AIRL (Fu, Luo & Levine 2018, eq. 4 + Alg. 1)")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' .airl_key
#'
#' A step of the airl_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s A vector; its length is taken.
#' @return One of two values, depending on the branch taken.
#' @export
.airl_key <- function(s) {
  if (is.numeric(s) && length(s) == 1L) s
  else as.numeric(s)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' .airl_key_from_str
#'
#' A step of the airl_native implementation. Called by \code{airl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param s Character; passed to \code{grepl}.
#' @return One of two values, depending on the branch taken.
#' @export
.airl_key_from_str <- function(s) {
  if (grepl("^list\\(", s)) {
    inner <- sub("^list\\((.*)\\)$", "\\1", s)
    parts <- strsplit(inner, ", ", fixed = TRUE)[[1]]
    as.numeric(parts)
  } else {
    as.numeric(s)
  }
}

# -- restored: morie-only definition kept through the rmorie sync --
#' .airl_log
#'
#' A step of the airl_native implementation. Called by \code{airl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; passed to \code{max}.
#' @param floor Numeric; passed to \code{max}. Defaults to \code{1e-300}.
#' @return A numeric value.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .airl_log(x = x)
#' res
.airl_log <- function(x, floor = 1e-300) log(max(x, floor))


# -- restored: morie-only definition kept through the rmorie sync --
#' .airl_prep
#'
#' A step of the airl_native implementation. Called by \code{airl}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param S A vector; its length is taken.
#' @param A A vector; its length is taken.
#' @param S1 A vector; its length is taken.
#' @param LP A vector; its length is taken.
#' @param name Passed to \code{stop}.
#' @return A list with \code{S}, \code{A}, \code{S1}, \code{LP}.
#' @export
.airl_prep <- function(S, A, S1, LP, name) {
  if (length(S) == 0L || length(A) == 0L || length(S1) == 0L ||
        length(LP) == 0L)
    stop("airl: ", name, " states, actions, next states and log_policy ",
         "must be non-empty and the same length")
  if (!(length(S) == length(A) && length(A) == length(S1) &&
        length(S1) == length(LP)))
    stop("airl: ", name, " states, actions, next states and log_policy ",
         "must be non-empty and the same length")
  list(S = lapply(S, .airl_key),
       A = lapply(A, function(a) a),
       S1 = lapply(S1, .airl_key),
       LP = as.numeric(LP))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' Soft value iteration on a deterministic tabular MDP
#'
#' The maximum-entropy \eqn{V^*} of eq. Q(s, a) = r(s) + gamma V(s'),
#' V(s) = log sum_a exp Q(s, a). Provided so callers can validate
#' AIRL's recovered reward independently of the fit.
#' @param states See Usage.
#' @param actions See Usage.
#' @param step See Usage.
#' @param reward See Usage.
#' @param gamma See Usage.
#' @param iters See Usage.
#' @param tol See Usage.
#' @export
soft_value_iteration <- function(states, actions, step, reward, gamma = 0.9,
                                 iters = 2000L, tol = 1e-14) {
  S <- as.list(states)
  A <- as.list(actions)
  V <- new.env(hash = TRUE, parent = emptyenv())
  for (s in S) assign(deparse(s, control = "useSource"), 0, envir = V)
  for (it in seq_len(as.integer(iters))) {
    newV <- new.env(hash = TRUE, parent = emptyenv())
    for (s in S) {
      qs <- vapply(A, function(a) reward(s) + gamma *
                     get(deparse(step(s, a), control = "useSource"),
                         envir = V), numeric(1))
      m <- max(qs)
      assign(deparse(s, control = "useSource"),
             m + log(sum(exp(qs - m))), envir = newV)
    }
    delta <- max(vapply(S, function(s)
      abs(get(deparse(s, control = "useSource"), envir = newV) -
            get(deparse(s, control = "useSource"), envir = V)), numeric(1)))
    V <- newV
    if (delta < tol) break
  }
  pi <- list()
  for (s in S) {
    qs <- vapply(A, function(a) reward(s) + gamma *
                   get(deparse(step(s, a), control = "useSource"),
                       envir = V), numeric(1))
    Vs <- get(deparse(s, control = "useSource"), envir = V)
    for (i in seq_along(A))
      pi[[length(pi) + 1L]] <-
        setNames(list(exp(qs[i] - Vs)),
                 paste0(deparse(s, control = "useSource"), "|",
                        deparse(A[[i]], control = "useSource")))
  }
  V_out <- list()
  for (s in S) V_out[[length(V_out) + 1L]] <-
    setNames(list(get(deparse(s, control = "useSource"), envir = V)),
             deparse(s, control = "useSource"))
  list(V = V_out, pi = pi)
}
