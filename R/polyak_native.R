# morie.fn -- function file (rootcoder007/morie)
#
# Polyak averaging and soft target updates.
#
# Two related ideas about not trusting the latest iterate.
#
# **Averaging the iterates.** Stochastic approximation with a slowly
# decaying step size produces iterates that rattle around the optimum.
# Averaging them,
# \bar\theta_T = (1/T) \sum_{t \le T} \theta_t, gives an estimator that
# is asymptotically optimal -- the same rate a second-order method
# achieves, with no second derivatives computed. The requirement is that
# the step size decay *slowly* (slower than 1/t); with too fast a decay
# the iterates stop moving before averaging can help. A running form
# with a fixed decay is also given, so nothing needs storing.
#
# **Soft target updates.** Q-learning with a function approximator uses
# the network being trained to compute its own regression target, which
# makes the update prone to divergence. DQN's answer is a target network
# copied every C steps. For actor-critic the copy is replaced by a slow
# track,
#
# \theta' <- \tau \theta + (1 - \tau) \theta',  \tau \ll 1,
#
# with \tau = 10^{-3} reported. The target values are then constrained
# to change slowly, which moves an unstable problem closer to supervised
# learning -- where the targets do not move at all. The price is delay:
# the target lags the online network by roughly 1/\tau steps, and that
# trade is the point rather than a side-effect. lag_halflife computes
# it, and the anchor checks the geometric convergence against the
# closed form.
#
# References
# ----------
# Polyak, B. T. & Juditsky, A. B. (1992) "Acceleration of Stochastic
# Approximation by Averaging", SIAM Journal on Control and
# Optimization 30(4), 838-855, doi:10.1137/0330046. Averaging the
# iterates of a slowly-decaying stochastic approximation attains the
# asymptotically optimal rate.
#
# Ruppert, D. (1988) Efficient Estimations from a Slowly Convergent
# Robbins-Monro Process, Technical Report 781, School of Operations
# Research and Industrial Engineering, Cornell University. The same
# averaging idea, independently.
#
# Lillicrap, T. P., Hunt, J. J., Pritzel, A., Heess, N., Erez, T.,
# Tassa, Y., Silver, D. & Wierstra, D. (2016) "Continuous control with
# deep reinforcement learning", International Conference on Learning
# Representations (ICLR 2016), arXiv:1509.02971. Sec. 3: the Q update
# is prone to divergence because the network being updated also computes
# the target; "soft" target updates theta' <- tau theta + (1 - tau)
# theta' with tau << 1 replace DQN's periodic copy, constraining target
# values to change slowly and moving the problem closer to supervised
# learning; the supplementary details give tau = 0.001.
#
# Mnih, V., Kavukcuoglu, K., Silver, D. et al. (2015) "Human-level
# control through deep reinforcement learning", Nature 518, 529-533,
# doi:10.1038/nature14236. The periodic-copy target network;
# implemented in dqnv.

#' morie_polyak
#'
#' Part of the polyak_native implementation; see the file header for the
#' source it follows.
#'
#' @param iterates See Usage.
#' @param burn_in Defaults to \code{0}.
#' @return A list with \code{average}, \code{n_averaged}, \code{burn_in}.
#' @export
morie_polyak <- function(iterates, burn_in = 0) {
  X <- lapply(iterates, function(t) as.numeric(t))

  if (length(X) == 0L) {
    stop("polyak: no iterates given")
  }

  b <- as.integer(burn_in)
  if (b >= length(X)) {
    stop(sprintf("polyak: the burn-in of %d discards all %d iterates",
                 b, length(X)))
  }

  # Keep iterates after burn-in: Python X[b:] corresponds to R X[(b+1):length(X)]
  keep <- X[(b + 1L):length(X)]
  d <- length(keep[[1]])

  # Stack iterates as rows of a matrix, then take the column means.
  mat <- matrix(unlist(keep), nrow = length(keep), byrow = TRUE)
  avg <- colMeans(mat)

  list(
    average = avg,
    n_averaged = length(keep),
    burn_in = b
  )
}

#' .polyak_running_average
#'
#' Part of the polyak_native implementation; see the file header for the
#' source it follows.
#'
#' @param prev See Usage.
#' @param new See Usage.
#' @param decay Defaults to \code{0.999}.
#' @return A numeric value.
#' @export
.polyak_running_average <- function(prev, new, decay = 0.999) {
  a <- as.numeric(decay)
  if (a <= 0 || a >= 1) {
    stop(sprintf("polyak: the decay must lie in (0,1), got %g", decay))
  }

  p <- as.numeric(prev)
  n <- as.numeric(new)

  if (length(p) != length(n)) {
    stop(sprintf("polyak: the parameter vectors differ in length (%d, %d)",
                 length(p), length(n)))
  }

  a * p + (1 - a) * n
}

#' .polyak_soft_update
#'
#' Part of the polyak_native implementation; see the file header for the
#' source it follows.
#'
#' @param target See Usage.
#' @param online See Usage.
#' @param tau Defaults to \code{0.001}.
#' @return A numeric value.
#' @export
.polyak_soft_update <- function(target, online, tau = 0.001) {
  t <- as.numeric(tau)
  if (t <= 0 || t > 1) {
    stop(sprintf("polyak: tau must lie in (0,1], got %g", tau))
  }

  a <- as.numeric(target)
  b <- as.numeric(online)

  if (length(a) != length(b)) {
    stop(sprintf("polyak: the networks differ in size (%d, %d)",
                 length(a), length(b)))
  }

  t * b + (1 - t) * a
}

#' Step %% C == 0 : a copy is taken at multiples of C
#'
#' Part of the polyak_native implementation; see the file header for the
#' source it follows.
#'
#' @param target See Usage.
#' @param online See Usage.
#' @param step See Usage.
#' @param C Defaults to \code{10000}.
#' @return A list, whose contents depend on the branch taken; across the branches its names are \code{target}, \code{copied}.
#' @export
.polyak_hard_update <- function(target, online, step, C = 10000) {
  # step %% C == 0 : a copy is taken at multiples of C
  if (as.integer(step) %% as.integer(C) == 0L) {
    list(
      target = as.numeric(online),
      copied = TRUE
    )
  } else {
    list(
      target = as.numeric(target),
      copied = FALSE
    )
  }
}

#' .polyak_lag_halflife
#'
#' Part of the polyak_native implementation; see the file header for the
#' source it follows.
#'
#' @param tau See Usage.
#' @return A list with \code{halflife}, \code{approx}, \code{tau}, \code{note}.
#' @export
.polyak_lag_halflife <- function(tau) {
  t <- as.numeric(tau)
  if (t <= 0 || t >= 1) {
    stop(sprintf("polyak: tau must lie in (0,1) for a half-life, got %g", tau))
  }

  list(
    halflife = log(0.5) / log(1 - t),
    approx = log(2) / t,
    tau = t,
    note = "the target lags the online network; that delay IS the stabiliser"
  )
}

#' .polyak_cheatsheet
#'
#' Part of the polyak_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
.polyak_cheatsheet <- function() {
  "polyak: (1) averaging the iterates of a SLOWLY decaying stochastic approximation is asymptotically optimal -- the second-order rate without second derivatives, provided the step decays slower than 1/t. (2) Q-learning diverges because the network computes its own target; DQN copies the weights every C steps, DDPG instead TRACKS them, theta' <- tau theta + (1-tau) theta' with tau = 1e-3, so targets move slowly and the problem resembles supervised learning. The lag, about 0.69/tau steps, is the price."
}

# Aliases polyakaveraging, polyak_target, polyaktarget all point to the
# same polyak_average; in this R translation they are absorbed by
# morie_polyak, which is the single public entry point.
