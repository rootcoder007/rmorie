# Intrinsic Curiosity Module: curiosity as forward-model error in a
# learned, action-relevant feature space.
# Sources: Pathak, D., Agrawal, P., Efros, A. A., & Darrell, T.
# (2017) "Curiosity-driven Exploration by Self-supervised
# Prediction", *ICML*, arXiv:1705.05363. Eq. (2)-(3) for the inverse
# model, eq. (4)-(5) for the forward model, eq. (6) for the
# intrinsic reward, eq. (7) for the joint policy/inverse/forward
# objective with beta in [0, 1] and lambda > 0.
#
# Native implementation mirroring Python morie.fn.explor exactly: the
# same ICM structure (feature map phi trained ONLY through the
# inverse model, inverse dynamics g(phi(s), phi(s')) -> a, forward
# dynamics f(phi(s), a) -> phi(s')), the same softmax inverse model
# for discrete actions and squared-error inverse model for continuous
# actions, the same intrinsic reward r^i = eta * L_F (so the eta/2
# factor is absorbed into L_F as the Python code does), the same
# 0.1/sqrt(d) tanh-friendly small init for the feature encoder, the
# same (1-beta) L_I + beta L_F training signal, and the same
# features="identity" baseline that falls for the noisy TV.

.EXPLOR_FEATURES <- c("inverse", "identity")

.mat <- function(x, name) {
  X <- as.matrix(x)
  if (nrow(X) == 0L || ncol(X) == 0L)
    stop("explor: ", name, " must be non-empty")
  lapply(seq_len(nrow(X)), function(i) as.numeric(X[i, ]))
}

.matvec <- function(W, x) {
  W <- as.matrix(W)
  as.numeric(as.vector(x) %*% W)
}

.explor_softmax <- function(z) {
  m <- max(z)
  e <- exp(z - m)
  e / sum(e)
}

#' explor
#'
#' Part of the explor_native implementation; see the file header for the
#' source it follows.
#'
#' @param states See Usage.
#' @param actions See Usage.
#' @param next_states See Usage.
#' @param n_actions Defaults to \code{NULL}.
#' @param n_features Defaults to \code{8}.
#' @param eta Defaults to \code{1}.
#' @param beta Defaults to \code{0.2}.
#' @param lr Defaults to \code{0.05}.
#' @param epochs Defaults to \code{1}.
#' @param features Defaults to \code{"inverse"}.
#' @param discrete Defaults to \code{TRUE}.
#' @param seed Defaults to \code{0}.
#' @return The value of \code{payload}, as built in the body.
#' @export
explor <- function(states, actions, next_states, n_actions = NULL,
                   n_features = 8, eta = 1, beta = 0.2, lr = 0.05,
                   epochs = 1, features = "inverse", discrete = TRUE,
                   seed = 0) {
  if (!(features %in% .EXPLOR_FEATURES))
    stop("explor: features must be one of ",
         paste(sQuote(.EXPLOR_FEATURES), collapse = ", "), ", got ",
         deparse(features))
  eta <- as.numeric(eta)
  if (!(eta > 0))
    stop("explor: eta must be > 0")
  beta <- as.numeric(beta)
  if (!(beta >= 0 && beta <= 1))
    stop("explor: beta must lie in [0, 1]")
  S <- .mat(states, "states")
  S1 <- .mat(next_states, "next_states")
  if (length(S) != length(S1))
    stop("explor: states and next_states must have the same length")
  if (length(S[[1]]) != length(S1[[1]]))
    stop("explor: states and next_states must have the same width")
  T <- length(S)
  d <- length(S[[1]])

  if (discrete) {
    A <- as.integer(as.numeric(actions))
    if (length(A) != T)
      stop("explor: got ", length(A), " actions for ", T, " transitions")
    nA <- if (is.null(n_actions)) max(A) + 1L else as.integer(n_actions)
    if (nA < 2L)
      stop("explor: need at least 2 discrete actions")
    if (min(A) < 0L || max(A) >= nA)
      stop("explor: action index out of range")
    a_dim <- nA
  } else {
    Ac <- .mat(actions, "actions")
    if (length(Ac) != T)
      stop("explor: got ", length(Ac), " actions for ", T, " transitions")
    a_dim <- length(Ac[[1]])
  }

  e <- .ghc_rng(as.numeric(seed))
  if (features == "identity") {
    k <- d
    Wphi <- NULL
  } else {
    k <- as.integer(n_features)
    if (k < 1L)
      stop("explor: n_features must be >= 1")
    # Small init: tanh saturated at initialisation has no gradient,
    # and phi is trained, so it must start in its linear regime.
    s <- 0.1 / sqrt(d)
    u <- .ghc_unif(e, d * k, low = -1, high = 1)
    Wphi <- matrix(u * s, nrow = d, ncol = k)
  }

  phi <- function(x) {
    if (is.null(Wphi)) return(as.numeric(x))
    tanh(.matvec(Wphi, x))
  }

  # Inverse model g: (phi(s), phi(s')) -> action.  Forward model f:
  # (phi(s), a) -> phi(s').  Both linear in their inputs.
  Winv <- matrix(0, nrow = 2L * k, ncol = a_dim)
  Wfwd <- matrix(0, nrow = k + a_dim, ncol = k)

  curve <- numeric(0)
  rewards <- numeric(0)
  lf_tot <- 0
  li_tot <- 0
  n_correct <- 0
  for (ep in seq_len(max(1L, as.integer(epochs)))) {
    rewards <- numeric(T)
    lf_tot <- 0
    li_tot <- 0
    n_correct <- 0
    for (t in seq_len(T)) {
      p <- phi(S[[t]])
      p1 <- phi(S1[[t]])
      if (discrete) {
        avec <- rep(0, a_dim)
        avec[A[t] + 1L] <- 1
      } else {
        avec <- as.numeric(Ac[[t]])
      }

      # --- inverse model, eqs. 2-3
      inp_i <- c(p, p1)
      zi <- .matvec(Winv, inp_i)
      if (discrete) {
        pr <- .explor_softmax(zi)
        li <- -log(max(pr[A[t] + 1L], 1e-300))
        gi <- pr - avec
        if (which.max(pr) - 1L == A[t]) n_correct <- n_correct + 1L
      } else {
        li <- 0.5 * sum((zi - avec)^2)
        gi <- zi - avec
      }
      li_tot <- li_tot + li

      # --- forward model, eqs. 4-5
      inp_f <- c(p, avec)
      ph <- .matvec(Wfwd, inp_f)
      ef <- ph - p1
      lf <- 0.5 * sum(ef * ef)
      lf_tot <- lf_tot + lf
      rewards[t] <- eta * lf     # eq. 6: (eta/2)||.||^2 == eta*L_F

      # --- SGD on (1-beta) L_I + beta L_F  (the eq. 7 terms that do
      #     not involve the policy).
      #
      # phi's parameters belong to theta_I: the feature encoder is
      # trained through the INVERSE loss only. That is not an
      # implementation shortcut, it is the mechanism -- phi is never
      # asked to reconstruct anything, only to support predicting the
      # agent's own action, so dimensions the agent cannot influence
      # carry no gradient and fall out of the representation. Letting
      # L_F train phi as well would give it an incentive to collapse
      # phi to a constant, which drives the curiosity reward to zero
      # while learning nothing.
      if (!is.null(Wphi)) {
        dphi <- rep(0, 2L * k)
        for (j in seq_len(2L * k)) {
          dphi[j] <- sum(Winv[j, ] * gi)
        }
        for (half in 0:1) {
          ph_ <- if (half == 0L) p else p1
          xin <- if (half == 0L) S[[t]] else S1[[t]]
          for (j in seq_len(k)) {
            g <- dphi[half * k + j] * (1 - ph_[j] * ph_[j])
            if (g == 0) next
            step <- lr * (1 - beta) * g
            for (dd in seq_len(d)) {
              if (xin[dd] != 0)
                Wphi[dd, j] <- Wphi[dd, j] - step * xin[dd]
            }
          }
        }
      }
      for (j in seq_len(2L * k)) {
        xj <- inp_i[j]
        if (xj == 0) next
        for (o in seq_len(a_dim))
          Winv[j, o] <- Winv[j, o] - lr * (1 - beta) * gi[o] * xj
      }
      for (j in seq_len(k + a_dim)) {
        xj <- inp_f[j]
        if (xj == 0) next
        for (o in seq_len(k))
          Wfwd[j, o] <- Wfwd[j, o] - lr * beta * ef[o] * xj
      }
    }
    curve <- c(curve, ((1 - beta) * li_tot + beta * lf_tot) / T)
  }

  n <- length(rewards)
  tenth <- max(1L, n %/% 10L)
  payload <- list(
    estimate = rewards,
    intrinsic_reward = rewards,
    forward_loss = as.numeric(lf_tot / T),
    inverse_loss = as.numeric(li_tot / T),
    objective = as.numeric(curve[length(curve)]),
    loss_curve = curve,
    phi = lapply(S, phi),
    phi_next = lapply(S1, phi),
    mean_first = as.numeric(sum(rewards[seq_len(tenth)]) / tenth),
    mean_last = as.numeric(sum(rewards[seq(tenth + 1L, n)]) / (n - tenth)),
    eta = eta,
    beta = beta,
    n = n,
    features = features,
    method = "ICM (Pathak et al. 2017, eqs. 2-7)"
  )
  if (discrete)
    payload$inverse_accuracy <- as.numeric(n_correct) / T
  payload
}

.explor_cheatsheet <- function() {
  paste0("explor: ICM (Pathak 2017). phi learned via the INVERSE model ",
         "(eqs. 2-3) so it encodes only what the agent can affect; ",
         "forward model f(phi(s),a) (eq. 4); curiosity r^i = ",
         "(eta/2)||phihat(s') - phi(s')||^2 (eq. 6); joint loss ",
         "(1-beta)L_I + beta L_F (eq. 7). features='identity' is the ",
         "raw-observation baseline that the noisy TV fools.")
}

# compact aliases per ledger/NAMING.md
intrinsic_motivation <- explor
icm <- explor

# morie entry point
#' Morie entry point
#'
#' Part of the explor_native implementation; see the file header for the
#' source it follows.
#'
#' @param states See Usage.
#' @param actions See Usage.
#' @param next_states See Usage.
#' @param n_actions Defaults to \code{NULL}.
#' @param n_features Defaults to \code{8}.
#' @param eta Defaults to \code{1}.
#' @param beta Defaults to \code{0.2}.
#' @param lr Defaults to \code{0.05}.
#' @param epochs Defaults to \code{1}.
#' @param features Defaults to \code{"inverse"}.
#' @param discrete Defaults to \code{TRUE}.
#' @param seed Defaults to \code{0}.
#' @return The value of \code{explor}.
#' @export
morie_explor <- function(states, actions, next_states, n_actions = NULL,
                         n_features = 8, eta = 1, beta = 0.2,
                         lr = 0.05, epochs = 1, features = "inverse",
                         discrete = TRUE, seed = 0) {
  explor(states, actions, next_states, n_actions, n_features, eta,
         beta, lr, epochs, features, discrete, seed)
}
