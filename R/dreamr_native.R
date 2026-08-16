# dreamr.R - Dreamer: value estimation and actor-critic learning in latent
# imagination.
#
# Hafner, D., Lillicrap, T., Ba, J., & Norouzi, M. (2020) "Dream to
# Control: Learning Behaviors by Latent Imagination", *ICLR*,
# arXiv:1912.01603.
#
# Dreamer learns a latent dynamics model and then learns its *behaviour*
# entirely inside that model -- it imagines trajectories forward from
# model states and never touches the environment while doing so. The
# latent model has three components (eq. 1):
#
# .. math:: \text{representation } p(s_t \mid s_{t-1}, a_{t-1}, o_t),
#           \qquad
#           \text{transition } q(s_t \mid s_{t-1}, a_{t-1}),
#           \qquad
#           \text{reward } q(r_t \mid s_t).
#
# The transition model is what makes imagination possible: it predicts
# future model states *without* seeing the observations that would cause
# them.
#
# Behaviour learning happens over imagined trajectories
# :math:`\{s_\tau, a_\tau, r_\tau\}_{\tau=t}^{t+H}`, and the paper gives
# three value estimators trading bias against variance:
#
# .. math:: V_R(s_\tau) = \mathbb{E}\Big[\sum_{n=\tau}^{t+H} r_n\Big],
#          \tag{4}
#
# which simply sums rewards to the horizon and ignores everything beyond
# it -- so it needs no value model at all, and the paper uses it as an
# ablation;
#
# .. math:: V_N^k(s_\tau) = \mathbb{E}\Big[
#          \sum_{n=\tau}^{h-1} \gamma^{n-\tau} r_n
#          + \gamma^{h-\tau} v_\psi(s_h)\Big],
#          \quad h = \min(\tau + k,\ t + H),
#          \tag{5}
#
# the :math:`k`-step estimate bootstrapping from the learned value; and
# the one Dreamer actually uses,
#
# .. math:: V_\lambda(s_\tau) = (1-\lambda)
#          \sum_{n=1}^{H-1} \lambda^{n-1} V_N^n(s_\tau)
#          + \lambda^{H-1} V_N^H(s_\tau),
#          \tag{6}
#
# an exponentially weighted average over horizons. Note the
# :math:`\min` in eq. 5: past the imagination horizon the estimate stops
# extending, so every :math:`V_N^k` with :math:`\tau + k \ge t+H`
# collapses to the same value -- which is exactly why the last term of
# eq. 6 carries the remaining weight :math:`\lambda^{H-1}` rather than
# continuing the sum.
#
# The updates (Algorithm 1) are then
#
# .. math:: \phi \leftarrow \phi + \alpha \nabla_\phi
#          \sum_{\tau=t}^{t+H} V_\lambda(s_\tau),
#          \qquad
#          \psi \leftarrow \psi - \alpha \nabla_\psi
#          \sum_{\tau=t}^{t+H} \tfrac12
#          \big\|v_\psi(s_\tau) - V_\lambda(s_\tau)\big\|^2 .
#
# The action model is updated by *propagating gradients of the value
# estimates back through the learned dynamics* -- that is the analytic
# gradient Dreamer gets and a model-free method does not, and it is why
# the paper can solve long-horizon tasks robustly with respect to
# :math:`H`.
#
# Implemented here: the three estimators of eqs. 4-6 on an imagined
# trajectory, :func:`imagine` to roll one out through a supplied
# transition and reward model, and :func:`value_update` for the critic's
# regression target. Bring your own :math:`q(s' \mid s,a)`,
# :math:`q(r \mid s)` and :math:`v_\psi`, learned or exact -- exact ones
# make the estimators checkable against closed forms, which is what the
# anchors do.

#' .dreamr_vec
#'
#' A step of the dreamr_native implementation. Called by \code{morie_dreamr_lambda_return}, \code{morie_dreamr_value_update}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x See Usage.
#' @param name See Usage.
#' @return The value of \code{v}, as built in the body.
#' @export
.dreamr_vec <- function(x, name) {
  v <- as.numeric(x)
  if (length(v) == 0L) stop(sprintf("dreamr: %s must be non-empty", name))
  v
}

#' .dreamr_pack
#'
#' A step of the dreamr_native implementation. Called by \code{morie_dreamr_lambda_return}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param vals A vector; its length is taken.
#' @param name See Usage.
#' @return A list with \code{estimate}, \code{returns}, \code{n}, \code{method}.
#' @export
.dreamr_pack <- function(vals, name) {
  list(
    estimate = vals,
    returns = vals,
    n = length(vals),
    method = sprintf("Dreamer %s (Hafner et al. 2020)", name)
  )
}

#' morie_dreamr_imagine
#'
#' A step of the dreamr_native implementation. Called by \code{morie_dreamr}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param state See Usage.
#' @param action_model See Usage.
#' @param transition See Usage.
#' @param reward_model See Usage.
#' @param horizon See Usage.
#' @param value_model Defaults to \code{NULL}.
#' @return A list with \code{estimate}, \code{states}, \code{actions}, \code{rewards}, \code{values}, \code{horizon}, \code{method}.
#' @export
morie_dreamr_imagine <- function(state, action_model, transition, reward_model,
                                  horizon, value_model = NULL) {
  H <- as.integer(horizon)
  if (H < 1L) stop("dreamr: horizon must be >= 1")
  fns <- list(c(action_model, "action_model"),
              c(transition, "transition"),
              c(reward_model, "reward_model"))
  for (pair in fns) {
    fn <- pair[[1]]
    name <- pair[[2]]
    if (!is.function(fn)) stop(sprintf("dreamr: %s must be callable", name))
  }
  states <- list(state)
  actions <- list()
  rewards <- numeric(0)
  s <- state
  for (i in seq_len(H)) {
    a <- action_model(s)
    r <- as.numeric(reward_model(s))
    actions[[length(actions) + 1L]] <- a
    rewards <- c(rewards, r)
    s <- transition(s, a)
    states[[length(states) + 1L]] <- s
  }
  values <- if (!is.null(value_model)) {
    vapply(states, function(x) as.numeric(value_model(x)), numeric(1))
  } else {
    NULL
  }
  list(
    estimate = rewards,
    states = states,
    actions = actions,
    rewards = rewards,
    values = values,
    horizon = H,
    method = "Dreamer latent imagination (Hafner et al. 2020 eq. 1)"
  )
}

#' morie_dreamr_lambda_return
#'
#' A step of the dreamr_native implementation. Called by \code{morie_dreamr}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param rewards Passed to \code{.dreamr_vec}.
#' @param values Passed to \code{.dreamr_vec}.
#' @param gamma Numeric; combined arithmetically in the body. Defaults to \code{0.99}.
#' @param lam Numeric; combined arithmetically in the body. Defaults to \code{0.95}.
#' @param estimator One of \code{"k-step"}, \code{"lambda"}, \code{"reward"}. Defaults to \code{"lambda"}.
#' @param k Defaults to \code{1}.
#' @return The value of \code{.dreamr_pack}.
#' @export
morie_dreamr_lambda_return <- function(rewards, values, gamma = 0.99, lam = 0.95,
                                        estimator = "lambda", k = 1) {
  if (!is.character(estimator) || length(estimator) != 1L ||
      !estimator %in% c("lambda", "k-step", "reward")) {
    stop(sprintf("dreamr: estimator must be 'lambda', 'k-step' or 'reward', got '%s'",
                 estimator))
  }
  r <- .dreamr_vec(rewards, "rewards")
  H <- length(r)
  v <- .dreamr_vec(values, "values")
  if (length(v) != H + 1L) {
    stop(sprintf("dreamr: values must have one more entry than rewards (got %d and %d)",
                 length(v), H))
  }
  gamma <- as.numeric(gamma)
  lam <- as.numeric(lam)
  if (lam < 0 || lam > 1) stop("dreamr: lam must lie in [0, 1]")

  if (estimator == "reward") {
    # eq. 4: undiscounted sum to the horizon, no value model.
    out <- numeric(H)
    for (tau in seq_len(H) - 1L) {
      s <- 0
      for (n in tau:(H - 1L)) {
        s <- s + r[n + 1L]
      }
      out[tau + 1L] <- s
    }
    return(.dreamr_pack(out, "V_R (eq. 4)"))
  }

  vn <- function(tau, kk) {
    # eq. 5, with h = min(tau + k, t + H). tau is 0-based.
    h <- min(tau + as.integer(kk), H)
    tot <- 0
    if (h > tau) {
      for (n in tau:(h - 1L)) {
        tot <- tot + (gamma^(n - tau)) * r[n + 1L]
      }
    }
    tot + (gamma^(h - tau)) * v[h + 1L]
  }

  if (estimator == "k-step") {
    if (as.integer(k) < 1L) stop("dreamr: k must be >= 1")
    out <- numeric(H)
    for (tau in seq_len(H) - 1L) {
      out[tau + 1L] <- vn(tau, as.integer(k))
    }
    return(.dreamr_pack(out, "V_N^k (eq. 5)"))
  }

  # eq. 6
  out <- numeric(H)
  for (tau in seq_len(H) - 1L) {
    acc <- 0
    if (H > 1L) {
      for (n in seq_len(H - 1L)) {
        acc <- acc + (1 - lam) * (lam^(n - 1L)) * vn(tau, n)
      }
    }
    acc <- acc + (lam^(H - 1L)) * vn(tau, H)
    out[tau + 1L] <- acc
  }
  .dreamr_pack(out, "V_lambda (eq. 6)")
}

#' morie_dreamr_value_update
#'
#' A step of the dreamr_native implementation. Called by \code{morie_dreamr}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param values Passed to \code{.dreamr_vec}.
#' @param targets Passed to \code{.dreamr_vec}.
#' @return A list with \code{estimate}, \code{loss}, \code{residual}, \code{grad}, \code{method}.
#' @export
morie_dreamr_value_update <- function(values, targets) {
  v <- .dreamr_vec(values, "values")
  t <- .dreamr_vec(targets, "targets")
  if (length(v) != length(t)) {
    stop(sprintf("dreamr: values and targets must be the same length (got %d and %d)",
                 length(v), length(t)))
  }
  resid <- v - t
  loss <- 0.5 * sum(resid * resid)
  list(
    estimate = as.numeric(loss),
    loss = as.numeric(loss),
    residual = as.numeric(resid),
    grad = as.numeric(resid),
    method = "Dreamer value loss (Hafner et al. 2020, Alg. 1)"
  )
}

#' morie_dreamr
#'
#' A step of the dreamr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param state Passed to \code{morie_dreamr_imagine}.
#' @param action_model Passed to \code{morie_dreamr_imagine}.
#' @param transition Passed to \code{morie_dreamr_imagine}.
#' @param reward_model Passed to \code{morie_dreamr_imagine}.
#' @param value_model Passed to \code{morie_dreamr_imagine}.
#' @param horizon Passed to \code{morie_dreamr_imagine}. Defaults to \code{15}.
#' @param gamma Passed to \code{morie_dreamr_lambda_return}. Defaults to \code{0.99}.
#' @param lam Passed to \code{morie_dreamr_lambda_return}. Defaults to \code{0.95}.
#' @param estimator Passed to \code{morie_dreamr_lambda_return}. Defaults to \code{"lambda"}.
#' @param k Passed to \code{morie_dreamr_lambda_return}. Defaults to \code{1}.
#' @return A list with \code{estimate}, \code{returns}, \code{objective}, \code{value_loss}, \code{residual}, \code{states}, \code{actions}, \code{rewards}, \code{values}, \code{horizon}, \code{gamma}, \code{lam}, \code{estimator}, \code{method}.
#' @export
morie_dreamr <- function(state, action_model, transition, reward_model, value_model,
                          horizon = 15, gamma = 0.99, lam = 0.95,
                          estimator = "lambda", k = 1) {
  traj <- morie_dreamr_imagine(state, action_model, transition, reward_model,
                                horizon, value_model = value_model)
  ret <- morie_dreamr_lambda_return(traj$rewards, traj$values, gamma = gamma,
                                     lam = lam, estimator = estimator, k = k)
  upd <- morie_dreamr_value_update(traj$values[-length(traj$values)], ret$returns)
  list(
    estimate = ret$returns,
    returns = ret$returns,
    objective = as.numeric(sum(ret$returns)),
    value_loss = upd$loss,
    residual = upd$residual,
    states = traj$states,
    actions = traj$actions,
    rewards = traj$rewards,
    values = traj$values,
    horizon = as.integer(horizon),
    gamma = as.numeric(gamma),
    lam = as.numeric(lam),
    estimator = estimator,
    method = "Dreamer behaviour step (Hafner et al. 2020, Alg. 1)"
  )
}

morie_dreamer <- morie_dreamr

#' morie_dreamr_cheatsheet
#'
#' A step of the dreamr_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_dreamr_cheatsheet <- function() {
  paste0("dreamr: learn behaviour inside a latent world model ",
         "(Hafner 2020). Imagine H steps with the TRANSITION model ",
         "(no observations), then V_R (eq. 4, no value model), ",
         "V_N^k (eq. 5, h = min(tau+k, t+H)) or V_lambda (eq. 6, ",
         "the exponentially weighted average Dreamer uses). Actor ",
         "ascends sum_tau V_lambda through the dynamics; critic ",
         "regresses v_psi onto V_lambda.")
}
