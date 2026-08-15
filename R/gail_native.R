# Generative Adversarial Imitation Learning: the discriminator and the
# cost it hands to the policy.
#
# Ho, J., & Ermon, S. (2016) "Generative Adversarial Imitation Learning",
# NeurIPS, arXiv:1606.03476.
#
# Native implementation mirroring Python morie.fn.gail exactly: the same
# saddle-point of E_pi[log D] + E_piE[log(1 - D)] - lambda H(pi)
# (eq. 16), the same full-batch gradient ascent on eq. 17, the same
# optional ridge penalty, the same clipped sigmoid for D, the same
# per-pair Q aggregation of eq. 18, and the same occupancy-measure
# counts. The orientation is the GAN-imitation convention -- D -> 1 on
# LEARNER data, D -> 0 on EXPERT data, and the policy minimises
# log D -- so the per-policy-sample cost the optimiser consumes is
# log D(s, a) with that sign. No random numbers are drawn inside these
# routines, so the shared generator is not touched.

# Internal: stable scalar sigmoid, identical to the Python helper.
#' @keywords internal
#' @noRd
.gail_sigmoid <- function(z) {
  if (z >= 0.0) {
    return(1.0 / (1.0 + exp(-z)))
  }
  e <- exp(z)
  e / (1.0 + e)
}

# Internal: zip states and actions into (state-tuple, action) pairs,
# mirroring Python _pairs. Numeric states become the tuple of their
# as.numeric() values; scalar ints/strings pass through as-is.
#' @keywords internal
#' @noRd
.gail_pairs <- function(states, actions, name) {
  S <- lapply(states, function(s) {
    if (is.numeric(s) && length(s) == 1L) return(s)
    if (is.character(s) && length(s) == 1L) return(s)
    as.numeric(s)
  })
  A <- as.list(actions)
  if (length(S) != length(A))
    stop(sprintf("gail: %s states and actions must have the same length (%d vs %d)",
                 name, length(S), length(A)))
  if (length(S) == 0L)
    stop(sprintf("gail: %s must be non-empty", name))
  lapply(seq_along(S), function(i) list(S[[i]], A[[i]]))
}

#' Empirical occupancy measure
#'
#' GAIL's whole framing is that imitation is occupancy-measure
#' matching, so this is worth being able to look at directly. Returns
#' a named numeric vector whose names are stringified \code{(state,
#' action)} pairs and whose values are the empirical frequencies over
#' the supplied samples.
#'
#' @param states,actions Lists of equal length; \code{states} may
#'   contain numeric vectors or scalars, \code{actions} anything.
#' @return Named numeric vector of pair frequencies.
#' @references Ho & Ermon (2016) arXiv:1606.03476.
#' @export
gail_occupancy_measure <- function(states, actions) {
  pr <- .gail_pairs(states, actions, "occupancy")
  counts <- list()
  for (p in pr) {
    key <- paste0("(", paste(p[[1]], collapse = ","), ")",
                  "x", "(", paste(p[[2]], collapse = ","), ")")
    counts[[key]] <- if (is.null(counts[[key]])) 1L
                     else counts[[key]] + 1L
  }
  n <- length(pr)
  v <- vapply(counts, function(v) v / n, numeric(1))
  v
}

#' Fit the GAIL discriminator and emit the per-step cost for the
#' policy
#'
#' Full-batch gradient ascent on eq. 17 (the gradient of
#' E_pi[log D] + E_piE[log(1 - D)]) with optional ridge. D is a
#' logistic model on features of (s, a); by default the features are
#' a one-hot indicator over the observed (s, a) pairs, which makes
#' D fully non-parametric and is the setting in which eq. 16's
#' optimum is exactly rho_pi / (rho_pi + rho_piE).
#'
#' @param expert_states,expert_actions Lists of equal length: the
#'   expert demonstrations tau_E.
#' @param policy_states,policy_actions Lists of equal length:
#'   trajectories tau_i sampled from the current policy.
#' @param features Optional callable \code{features(s, a) -> numeric};
#'   defaults to the one-hot indicator over the observed (s, a) pairs
#'   plus a bias column.
#' @param lr Learning rate for the full-batch ascent on eq. 17.
#' @param epochs Number of ascent steps (at least one).
#' @param l2 Optional ridge penalty on the weights.
#' @param lam lambda, the causal-entropy weight of eq. 16. Only enters
#'   the reported objective; the entropy of your policy is yours to
#'   supply.
#' @param policy_entropy H(pi), if you have it, for the eq. 16 value.
#' @param clip Floor on D and 1-D inside the logarithms.
#' @return A named list whose names match the Python RichResult
#'   payload keys: \code{estimate} and \code{cost} are log D(s, a) for
#'   each policy sample (the per-step cost a TRPO-style optimiser
#'   consumes); also \code{Q} (eq. 18's per-pair average over the
#'   policy samples), \code{D_policy}, \code{D_expert}, \code{objective}
#'   (eq. 16), \code{accuracy}, \code{weights}, \code{occupancy_policy},
#'   \code{occupancy_expert}, \code{n_policy}, \code{n_expert},
#'   \code{method}.
#' @references Ho & Ermon (2016) arXiv:1606.03476, eqs. 16-18 and
#'   Algorithm 1.
#' @export
gail <- function(expert_states, expert_actions,
                 policy_states, policy_actions,
                 features = NULL, lr = 0.1, epochs = 200,
                 l2 = 0.0, lam = 0.0, policy_entropy = 0.0,
                 clip = 1e-9) {
  E <- .gail_pairs(expert_states, expert_actions, "expert")
  P <- .gail_pairs(policy_states, policy_actions, "policy")

  nf <- NA_integer_
  if (is.null(features)) {
    # sort by repr() of each pair, just like the Python arm
    keys <- unique(c(E, P))
    ord <- order(vapply(keys, function(k) paste0("(", paste(k[[1]], collapse=","), ")",
                                                  "x", "(", paste(k[[2]], collapse=","), ")"),
                        character(1)))
    keys <- keys[ord]
    index <- list()
    for (i in seq_along(keys)) index[[paste0("(", paste(keys[[i]][[1]], collapse=","), ")",
                                             "x", "(", paste(keys[[i]][[2]], collapse=","), ")")]] <- i
    nf <- length(keys) + 1L

    feat <- function(p) {
      v <- rep(0.0, nf)
      key <- paste0("(", paste(p[[1]], collapse=","), ")",
                    "x", "(", paste(p[[2]], collapse=","), ")")
      v[index[[key]]] <- 1.0
      v[nf] <- 1.0
      v
    }
  } else {
    if (!is.function(features))
      stop("gail: features must be callable")
    feat <- function(p) {
      as.numeric(features(p[[1]], p[[2]]))
    }
    # probe dimensionality from the first policy sample, then append a
    # bias column to mirror the Python arm
    nf <- length(feat(P[[1]])) + 1L
    feat <- function(p) {
      v <- as.numeric(features(p[[1]], p[[2]]))
      c(v, 1.0)
    }
  }

  XP <- lapply(P, feat)
  XE <- lapply(E, feat)
  w <- rep(0.0, nf)
  lr <- as.numeric(lr)
  l2 <- as.numeric(l2)
  ep <- max(1L, as.integer(epochs))

  for (epoch in seq_len(ep)) {
    g <- rep(0.0, nf)
    # eq. 17 ascent: E_pi[log D] + E_piE[log(1 - D)]
    # d/dw log D      = (1 - D) x   on policy samples
    # d/dw log(1 - D) =    - D  x   on expert  samples
    for (x in XP) {
      z <- 0.0
      for (j in seq_len(nf)) z <- z + w[j] * x[j]
      d <- .gail_sigmoid(z)
      c <- (1.0 - d) / length(XP)
      for (j in seq_len(nf)) g[j] <- g[j] + c * x[j]
    }
    for (x in XE) {
      z <- 0.0
      for (j in seq_len(nf)) z <- z + w[j] * x[j]
      d <- .gail_sigmoid(z)
      c <- -d / length(XE)
      for (j in seq_len(nf)) g[j] <- g[j] + c * x[j]
    }
    for (j in seq_len(nf)) {
      w[j] <- w[j] + lr * (g[j] - l2 * w[j])
    }
  }

  Df <- function(x) {
    z <- 0.0
    for (j in seq_len(nf)) z <- z + w[j] * x[j]
    v <- .gail_sigmoid(z)
    pmax(clip, pmin(1.0 - clip, v))
  }

  dp <- vapply(XP, Df, numeric(1))
  de <- vapply(XE, Df, numeric(1))

  obj <- (sum(log(dp)) / length(dp)
          + sum(log(1.0 - de)) / length(de)
          - as.numeric(lam) * as.numeric(policy_entropy))
  cost <- log(dp)

  acc <- (sum(dp > 0.5) + sum(de <= 0.5)) /
         (length(dp) + length(de))

  # eq. 18's Q(s, a) = E_tau[log D | s_0 = s, a_0 = a], estimated by
  # averaging the cost over every occurrence of the pair in the
  # policy rollout
  q <- list()
  for (i in seq_along(P)) {
    key <- paste0("(", paste(P[[i]][[1]], collapse=","), ")",
                  "x", "(", paste(P[[i]][[2]], collapse=","), ")")
    if (is.null(q[[key]])) q[[key]] <- numeric(0)
    q[[key]] <- c(q[[key]], cost[i])
  }
  q <- lapply(q, mean)
  qv <- vapply(q, identity, numeric(1))

  list(
    estimate = cost,
    cost = cost,
    Q = qv,
    D_policy = dp,
    D_expert = de,
    objective = as.numeric(obj),
    accuracy = as.numeric(acc),
    weights = w,
    occupancy_policy = gail_occupancy_measure(policy_states, policy_actions),
    occupancy_expert = gail_occupancy_measure(expert_states, expert_actions),
    n_policy = length(P),
    n_expert = length(E),
    method = "GAIL discriminator (Ho & Ermon 2016, eqs. 16-18)"
  )
}
