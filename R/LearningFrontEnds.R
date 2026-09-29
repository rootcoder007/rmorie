#' Actor-critic, DDPG and beam-search front ends
#'
#' R arm of \code{morie.fn.a2cv}, \code{ddpgc} and \code{gptas}, thin
#' wrappers of the Geron-shelf implementations. \code{A2cv}: advantage
#' actor-critic with linear softmax policy and value baseline
#' (\code{morie_geron_a2c}; \code{n_steps} caps each rollout). \code{Ddpgc}:
#' deep deterministic policy gradient with linear actor and critic,
#' target networks and Ornstein-Uhlenbeck exploration
#' (\code{morie_geron_ddpg}). \code{Gptas}: beam search decoding of width
#' \code{k} (\code{morie_geron_beam_search}).
#'
#' @param env Environment (see the wrapped functions).
#' @param actor,critic Initial actor and critic weights.
#' @param n_steps Rollout cap.
#' @param epochs Training episodes.
#' @param lr Learning rate.
#' @param gamma Discount factor.
#' @param critic_lr Critic learning rate.
#' @param seed Seed.
#' @param tau Soft target-update rate.
#' @param ou_theta,ou_sigma Ornstein-Uhlenbeck parameters.
#' @param s0 Initial state.
#' @param model Next-token scorer \code{model(src, prefix)}.
#' @param prompt Source sequence.
#' @param k Beam width.
#' @param max_len Maximum length.
#' @param eos End-of-sequence token.
#' @param length_penalty Length penalty.
#' @return The wrapped function's result.
#' @references Mnih, V. et al. (2016). Asynchronous methods for deep
#'   reinforcement learning. ICML, 1928-1937.
#'
#'   Lillicrap, T. P. et al. (2016). Continuous control with deep
#'   reinforcement learning. ICLR.
#'
#'   Sutskever, I., Vinyals, O. and Le, Q. V. (2014). Sequence to sequence
#'   learning with neural networks. NeurIPS, 3104-3112.
#' @examples
#' m <- function(src, prefix) if (length(prefix) %% 2 == 0) log(c(0.6, 0.4)) else log(c(0.3, 0.7))
#' Gptas(m, "hi", k = 2, max_len = 3)$sequence
#' @export
A2cv <- function(env, actor, critic, n_steps = 200, epochs = 100, lr = 0.1, gamma = 0.99, critic_lr = NULL, seed = 0) {
  morie_geron_a2c(env, actor, critic, epochs = epochs, lr = lr, gamma = gamma, critic_lr = critic_lr,
                  max_steps = n_steps, seed = seed)
}

#' @rdname A2cv
#' @export
Ddpgc <- function(env, actor, critic, tau = 0.01, epochs = 20, lr = 0.01, gamma = 0.95, ou_theta = 0.15,
                  ou_sigma = 0.2, seed = 0, s0 = NULL) {
  morie_geron_ddpg(env, actor, critic, epochs = epochs, lr = lr, gamma = gamma, tau = tau, ou_theta = ou_theta,
                   ou_sigma = ou_sigma, seed = seed, s0 = s0)
}

#' @rdname A2cv
#' @export
Gptas <- function(model, prompt, k = 3, max_len = 10, eos = NULL, length_penalty = 0) {
  morie_geron_beam_search(model, prompt, beam_width = k, max_len = max_len, eos = eos, length_penalty = length_penalty)
}
