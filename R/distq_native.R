# C51: the categorical distributional Bellman update (Bellemare, Dabney
# & Munos 2017). Native R implementation mirroring Python
# morie.fn.distq exactly: same atom grid, same projection with the
# exact-hit fix (when b lands on an atom, l == u and the split factors
# (u - b) and (b - l) are both zero; the literal Algorithm 1 would
# silently discard the entire mass p_j, so we add it to m[lo] in full),
# same T_hat clipping, same cross-entropy loss, same greedy action,
# same Bernoulli alternative, same value-distribution iteration.
#
# References
# ----------
# Bellemare, M. G., Dabney, W. & Munos, R. (2017) "A Distributional
# Perspective on Reinforcement Learning", ICML 2017 (arXiv:1707.06887)
# eq. (7) and Algorithm 1. Rowland, M., Bellemare, M. G., Dabney, W.,
# Munos, R. & Teh, Y. W. (2018) "An Analysis of Categorical
# Distributional Reinforcement Learning", AISTATS 2018
# (arXiv:1802.08163) contraction in Cramer distance.

# ---- private helpers --------------------------------------------------

# Build the atom support z_i = v_min + i * dz, with dz = (v_max - v_min) /
# (n_atoms - 1), and the spacing. n must be at least 2 and v_max > v_min.
.distq_atoms <- function(v_min, v_max, n_atoms) {
  n <- as.integer(n_atoms)
  if (n < 2L)
    stop(sprintf("distq: need at least 2 atoms, got %d", n))
  lo <- as.numeric(v_min)
  hi <- as.numeric(v_max)
  if (!(hi > lo))
    stop(sprintf("distq: need v_max > v_min, got %r and %r",
                 v_min, v_max))
  dz <- (hi - lo) / (n - 1L)
  z <- lo + (seq_len(n) - 1L) * dz
  list(z = z, dz = dz)
}

# Coerce next_probs to numeric, validate length / sign / sum-to-one.
.distq_check_prob <- function(next_probs, n_atoms, name) {
  p <- as.numeric(next_probs)
  if (length(p) != n_atoms)
    stop(sprintf("distq: %d %s for %d atoms",
                 length(p), name, n_atoms))
  if (any(p < -1e-9))
    stop(sprintf("distq: %s has a negative entry", name))
  tot <- sum(p)
  if (abs(tot - 1.0) > 1e-6)
    stop(sprintf("distq: %s sums to %.9f, not 1", name, tot))
  p
}

# ---- public API -------------------------------------------------------

#' The fixed support {z_i} and its spacing
#'
#' Atoms on \code{[v_min, v_max]} at equal intervals:
#' \code{z_i = v_min + (i-1) * dz}, \code{dz = (v_max - v_min) / (N - 1)}.
#'
#' @param v_min Lower bound of the support.
#' @param v_max Upper bound of the support.
#' @param n_atoms Number of atoms N. Must be at least 2.
#' @return A list with \code{z} (the atom positions) and \code{dz}
#'   (the spacing).
#' @export
atoms <- function(v_min, v_max, n_atoms) {
  a <- .distq_atoms(v_min, v_max, n_atoms)
  a$z
}

# (Internal companion returning both z and dz -- used by the other
# routines so we don't rebuild the spacing each time.)
.distq_atoms_full <- function(v_min, v_max, n_atoms) {
  .distq_atoms(v_min, v_max, n_atoms)
}

#' E[Z] = sum_i z_i p_i
#'
#' @param probs Numeric vector of probabilities.
#' @param z Numeric vector of atom positions.
#' @return Scalar mean of the categorical distribution.
#' @export
distribution_mean <- function(probs, z) {
  p <- as.numeric(probs)
  zz <- as.numeric(z)
  if (length(p) != length(zz))
    stop(sprintf("distq: %d probabilities for %d atoms",
                 length(p), length(zz)))
  sum(zz * p)
}

#' Eq. (7) / Algorithm 1: project r + gamma * z onto {z_i}
#'
#' For each atom j compute \code{tz = clip(r + gamma * z_j, v_min, v_max)},
#' its fractional position \code{b = (tz - v_min) / dz}, and split
#' \code{p_j} between the two integer neighbours \code{l = floor(b)} and
#' \code{u = ceil(b)}. The exact-hit case \code{l == u} is fixed: instead
#' of writing \code{p * (u - b) + p * (b - l) = 0}, the full mass is
#' added to \code{m[l]}.
#'
#' @param reward Sampled reward r.
#' @param gamma Discount in [0, 1]. Ignored when \code{done = TRUE}.
#' @param next_probs Numeric vector of length n_atoms: the next-state
#'   distribution under the greedy action. Must be a probability vector.
#' @param v_min Lower bound of the support.
#' @param v_max Upper bound of the support.
#' @param n_atoms Number of atoms. If \code{NULL} (the default), inferred
#'   from \code{next_probs}.
#' @param done Terminal flag; equivalent to \code{gamma = 0}.
#' @return Numeric vector of length n_atoms: the projected target
#'   probabilities, which sum to 1.
#' @export
categorical_projection <- function(reward, gamma, next_probs, v_min, v_max,
                                   n_atoms = NULL, done = FALSE) {
  p <- as.numeric(next_probs)
  n <- if (is.null(n_atoms)) length(p) else as.integer(n_atoms)
  if (length(p) != n)
    stop(sprintf("distq: %d next probabilities for %d atoms",
                 length(p), n))
  if (any(p < -1e-9))
    stop("distq: next_probs has a negative entry")
  tot <- sum(p)
  if (abs(tot - 1.0) > 1e-6)
    stop(sprintf("distq: next_probs sums to %.9f, not 1", tot))
  g <- if (isTRUE(done)) 0.0 else as.numeric(gamma)
  if (!(g >= 0 && g <= 1))
    stop(sprintf("distq: gamma must be in [0, 1], got %r", gamma))
  a <- .distq_atoms_full(v_min, v_max, n)
  z <- a$z
  dz <- a$dz
  m <- rep(0.0, n)
  r <- as.numeric(reward)
  for (j in seq_len(n)) {
    # T_hat z_j, clipped to the representable range
    tz <- max(min(r + g * z[j], z[n]), z[1L])
    b <- (tz - z[1L]) / dz
    lo <- as.integer(floor(b))
    hi <- as.integer(ceiling(b))
    if (lo < 0L) lo <- 0L
    if (lo > n - 1L) lo <- n - 1L
    if (hi < 0L) hi <- 0L
    if (hi > n - 1L) hi <- n - 1L
    if (lo == hi) {
      # exact hit: both split factors are zero, add the full mass
      m[lo + 1L] <- m[lo + 1L] + p[j]
    } else {
      m[lo + 1L] <- m[lo + 1L] + p[j] * (hi - b)
      m[hi + 1L] <- m[hi + 1L] + p[j] * (b - lo)
    }
  }
  m
}

#' Cross-entropy of the target against the current distribution
#'
#' \code{-sum_i m_i * log(max(p_i, eps))}, the cross-entropy term of
#' \code{D_KL(Phi T_hat Z || Z)}.
#'
#' @param m Numeric vector: the projected target.
#' @param probs Numeric vector: the current distribution.
#' @param eps Floor on the log to keep \code{p = 0} finite.
#' @return Scalar cross-entropy.
#' @export
categorical_loss <- function(m, probs, eps = 1e-12) {
  mm <- as.numeric(m)
  pp <- as.numeric(probs)
  if (length(mm) != length(pp))
    stop(sprintf("distq: %d targets for %d probabilities",
                 length(mm), length(pp)))
  e <- as.numeric(eps)
  tot <- 0.0
  for (i in seq_along(mm)) {
    tot <- tot - mm[i] * log(max(pp[i], e))
  }
  tot
}

#' Greedy action from per-action next-state distributions
#'
#' \code{a* = argmax_a sum_i z_i p_i(x', a)}.
#'
#' @param next_probs_by_action List of numeric vectors, one per action.
#' @param z Numeric vector of atom positions.
#' @return A list with \code{action} (1-based R index) and \code{q_values}
#'   (the per-action means).
#' @export
greedy_action <- function(next_probs_by_action, z) {
  rows <- as.list(next_probs_by_action)
  if (length(rows) == 0L)
    stop("distq: no actions given")
  zz <- as.numeric(z)
  qs <- vapply(rows, function(row) {
    distribution_mean(row, zz)
  }, numeric(1))
  best <- which.max(qs)
  list(action = as.integer(best), q_values = as.numeric(qs))
}

#' Algorithm 1 end to end: greedy action, projection, loss
#'
#' Returns a RichResult-style named list with the projected target
#' \code{m}, the cross-entropy loss, the chosen action, the Q values
#' it was chosen by, and the Q values for the current and target
#' distributions.
#'
#' @param reward Sampled reward r.
#' @param gamma Discount in [0, 1]. Ignored when \code{done = TRUE}.
#' @param next_probs_by_action List of per-action next-state
#'   distributions.
#' @param current_probs Numeric vector: the current Q-network
#'   distribution for the action under training.
#' @param v_min Lower bound of the support.
#' @param v_max Upper bound of the support.
#' @param done Terminal flag.
#' @return Named list with \code{estimate}, \code{loss}, \code{target},
#'   \code{action}, \code{q_values}, \code{q_target}, \code{q_current},
#'   \code{atoms}, \code{n_atoms}, \code{method}.
#' @export
c51_update <- function(reward, gamma, next_probs_by_action, current_probs,
                       v_min, v_max, done = FALSE) {
  cur <- as.numeric(current_probs)
  n <- length(cur)
  a <- .distq_atoms_full(v_min, v_max, n)
  z <- a$z
  g_act <- greedy_action(next_probs_by_action, z)
  a_star <- g_act$action
  qs <- g_act$q_values
  rows <- as.list(next_probs_by_action)
  m <- categorical_projection(reward, gamma, rows[[a_star]], v_min, v_max,
                              n_atoms = n, done = done)
  loss <- categorical_loss(m, cur)
  list(
    estimate = loss,
    loss = loss,
    target = m,
    action = a_star,
    q_values = qs,
    q_target = distribution_mean(m, z),
    q_current = distribution_mean(cur, z),
    atoms = z,
    n_atoms = n,
    method = paste("categorical algorithm (C51), Bellemare, Dabney &",
                   "Munos (2017) Algorithm 1")
  )
}

#' Bernoulli (N = 2) alternative
#'
#' The single-parameter form the C51 paper names in its discussion of
#' the projected operator: \code{Phi T_hat Z = clip((E[T_hat Z] -
#' v_min) / dz, 0, 1)}.
#'
#' @param reward Sampled reward r.
#' @param gamma Discount in [0, 1]. Ignored when \code{done = TRUE}.
#' @param next_probs Two-element probability vector.
#' @param v_min Lower bound of the support.
#' @param v_max Upper bound of the support.
#' @param done Terminal flag.
#' @return Scalar in [0, 1].
#' @export
bernoulli_algorithm <- function(reward, gamma, next_probs, v_min, v_max,
                                done = FALSE) {
  p <- as.numeric(next_probs)
  n <- length(p)
  a <- .distq_atoms_full(v_min, v_max, n)
  z <- a$z
  dz <- a$dz
  g <- if (isTRUE(done)) 0.0 else as.numeric(gamma)
  if (!(g >= 0 && g <= 1))
    stop(sprintf("distq: gamma must be in [0, 1], got %r", gamma))
  ex <- as.numeric(reward) + g * distribution_mean(p, z)
  min(max((ex - as.numeric(v_min)) / dz, 0.0), 1.0)
}

#' Value-distribution iteration
#'
#' Iterate the projected operator on a single self-looping state. The
#' reward is resampled from \code{(reward_atoms, reward_probs)} each
#' step. The projected operator is a Cramer-contraction (Rowland et al.
#' 2018), so this converges to the stationary value distribution.
#'
#' @param reward_atoms Numeric vector of possible reward values.
#' @param reward_probs Numeric vector of reward probabilities (sums
#'   to 1).
#' @param gamma Discount in [0, 1).
#' @param v_min Lower bound of the support.
#' @param v_max Upper bound of the support.
#' @param n_atoms Number of atoms.
#' @param iters Maximum number of iterations.
#' @param tol Convergence tolerance on the max abs shift.
#' @return A list with \code{distribution} (the final categorical
#'   distribution) and \code{info} (a named list with
#'   \code{iterations}, \code{converged}, \code{shift}).
#' @export
value_distribution_iteration <- function(reward_atoms, reward_probs, gamma,
                                         v_min, v_max, n_atoms,
                                         iters = 400L, tol = 1e-13) {
  ra <- as.numeric(reward_atoms)
  rp <- as.numeric(reward_probs)
  if (length(ra) != length(rp))
    stop(sprintf("distq: %d reward atoms but %d probabilities",
                 length(ra), length(rp)))
  if (abs(sum(rp) - 1.0) > 1e-9)
    stop(sprintf("distq: reward_probs sums to %.9f, not 1", sum(rp)))
  a <- .distq_atoms_full(v_min, v_max, as.integer(n_atoms))
  z <- a$z
  n <- length(z)
  cur <- rep(1.0 / n, n)
  shift <- NA_real_
  iters <- as.integer(iters)
  converged <- FALSE
  step_done <- 0L
  for (step in seq_len(iters)) {
    nxt <- rep(0.0, n)
    for (t in seq_along(ra)) {
      proj <- categorical_projection(ra[t], gamma, cur, v_min, v_max,
                                     n_atoms = n)
      nxt <- nxt + rp[t] * proj
    }
    shift <- max(abs(nxt - cur))
    cur <- nxt
    step_done <- step
    if (is.finite(shift) && shift < tol) {
      converged <- TRUE
      break
    }
  }
  list(distribution = cur,
       info = list(iterations = step_done, converged = converged,
                   shift = shift))
}

# compact alias per ledger/NAMING.md
categoricalprojection <- categorical_projection

# public names resolved by fn/_lazy_map.json
distributional_rl <- categorical_projection

#' @rdname atoms
#' @export
morie_distq <- atoms

#' @rdname atoms
#' @export
morie_distq <- atoms

#' @rdname atoms
#' @export
morie_distq <- atoms

#' @rdname atoms
#' @export
morie_distq <- atoms

#' @rdname atoms
#' @export
morie_distq <- atoms
