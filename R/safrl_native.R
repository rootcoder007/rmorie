# Constrained Policy Optimization for CMDPs.
#
# Achiam, J., Held, D., Tamar, A., & Abbeel, P. (2017) "Constrained
# Policy Optimization", ICML, arXiv:1705.10528. The CMDP framework
# itself is Altman, E. (1999) Constrained Markov Decision Processes.
#
# A CMDP is an MDP augmented with auxiliary cost functions
# C_1, ..., C_m and limits d_1, ..., d_m. Writing
# J_{C_i}(pi) = E_{tau ~ pi}[sum_t gamma^t C_i(s_t, a_t, s_{t+1})]
# for the C_i-return, the feasible set is
#
#   Pi_C = {pi in Pi : for all i, J_{C_i}(pi) <= d_i},
#   pi* = argmax_{pi in Pi_C} J(pi).
#
# The point of CPO is that the constraint holds throughout training,
# not just at the optimum -- it is the first policy-search algorithm for
# CMDPs to guarantee that for arbitrary policy classes.
#
# The theoretically justified update (eq. 10) maximises the surrogate
# advantage inside a KL trust region while constraining an upper bound
# on each cost return. For small steps that is well approximated by
# linearising objective and constraints and taking a second-order
# expansion of the KL, giving the convex program (eq. 11):
#
#   theta_{k+1} = argmax_theta g^T(theta - theta_k)
#       s.t. c_i + b_i^T(theta - theta_k) <= 0,
#            1/2 (theta - theta_k)^T H (theta - theta_k) <= delta,
#
# with g the objective gradient, b_i the gradient of constraint i,
# H the Fisher information matrix, and c_i = J_{C_i}(pi_k) - d_i --
# the current violation, positive when the constraint is already
# breached. Its dual (eq. 12) is a convex program in m+1 variables,
#
#   max_{lambda >= 0, nu >= 0}
#       -1/(2*lambda) * (g^T H^-1 g - 2 r^T nu + nu^T S nu)
#       + nu^T c - lambda*delta/2,
#       r = g^T H^-1 B,  S = B^T H^-1 B,
#
# and the primal solution follows from eq. 13:
#
#   theta* = theta_k + (1/lambda*) H^-1 (g - B nu*).
#
# With no constraints this collapses to the natural-gradient / TRPO
# step theta_k + sqrt(2*delta / (g^T H^-1 g)) H^-1 g, which is the
# cleanest available check on the algebra and is anchored as such.
#
# Infeasibility is a real case, not an edge case: approximation error
# can leave pi_k outside Pi_C with no feasible step inside the trust
# region. Section 6.2 then replaces the update with a recovery step
# that purely decreases the constraint value,
#
#   theta* = theta_k - sqrt(2*delta / (b^T H^-1 b)) H^-1 b,
#
# which is what `recovery` in the returned result flags.
#
# Proposition 2 bounds the damage when the linearisation is imperfect:
#
#   J_{C_i}(pi_{k+1}) <= d_i +
#       sqrt(2*delta) * gamma * epsilon^{pi_{k+1}}_{C_i} / (1-gamma)^2,
#
# so the constraint can be exceeded, but only by a bounded amount that
# shrinks with the trust region. worst_case_violation computes it.
#
# This module provides the CMDP machinery and the CPO step itself, given
# g, B, c and H -- the quantities an outer loop estimates from rollouts.
# cmdp_returns computes J and J_{C_i} for a tabular CMDP so the whole
# thing can be exercised, and lagrangian_cmdp solves a small tabular
# CMDP exactly by the linear program the paper cites as the known-model
# solution -- the reference the approximate step should agree with.


.safrl_mat <- function(M, name) {
  rows <- NULL
  if (is.matrix(M)) {
    nr <- nrow(M)
    nc <- ncol(M)
    rows <- vector("list", nr)
    for (i in seq_len(nr)) {
      rows[[i]] <- as.numeric(M[i, seq_len(nc)])
    }
  } else if (is.list(M)) {
    rows <- lapply(M, function(r) {
      if (is.list(r)) as.numeric(unlist(r)) else as.numeric(r)
    })
  } else {
    stop(sprintf("safrl: %s must be a matrix or list of vectors", name))
  }
  if (length(rows) == 0 || length(rows[[1]]) == 0) {
    stop(sprintf("safrl: %s must be non-empty", name))
  }
  w <- length(rows[[1]])
  for (r in rows) {
    if (length(r) != w) {
      stop(sprintf("safrl: %s must be rectangular", name))
    }
  }
  return(rows)
}


.safrl_vec <- function(v, name) {
  out <- NULL
  if (is.list(v)) {
    out <- as.numeric(unlist(v))
  } else {
    out <- as.numeric(v)
  }
  if (length(out) == 0) {
    stop(sprintf("safrl: %s must be non-empty", name))
  }
  return(out)
}


.safrl_solve <- function(A, b) {
  n <- length(b)
  M <- matrix(0, nrow = n, ncol = n + 1)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      M[i, j] <- A[[i]][j]
    }
    M[i, n + 1] <- b[i]
  }
  for (cc in seq_len(n)) {
    p <- cc
    rng <- (cc + 1):n
    if (length(rng) > 0) {
      for (r in rng) {
        if (abs(M[r, cc]) > abs(M[p, cc])) {
          p <- r
        }
      }
    }
    if (abs(M[p, cc]) < 1e-14) {
      stop("safrl: H is singular; CPO assumes the Fisher information matrix is positive definite")
    }
    if (p != cc) {
      tmp <- M[cc, ]
      M[cc, ] <- M[p, ]
      M[p, ] <- tmp
    }
    pv <- M[cc, cc]
    for (j in cc:(n + 1)) {
      M[cc, j] <- M[cc, j] / pv
    }
    for (r in seq_len(n)) {
      if (r == cc) next
      f <- M[r, cc]
      if (f == 0) next
      for (j in cc:(n + 1)) {
        M[r, j] <- M[r, j] - f * M[cc, j]
      }
    }
  }
  return(M[, n + 1])
}


.safrl_dual <- function(q, r, S, c, delta, m, tol, max_iter) {
  nu <- rep(0.0, m)

  A_of <- function(v) {
    a <- q
    for (j in seq_len(m)) {
      a <- a - 2.0 * r[j] * v[j]
    }
    for (j in seq_len(m)) {
      if (v[j] == 0.0) next
      for (k in seq_len(m)) {
        a <- a + v[j] * S[j, k] * v[k]
      }
    }
    return(max(a, 0.0))
  }

  obj <- function(v) {
    s <- 0.0
    for (j in seq_len(m)) {
      s <- s + v[j] * c[j]
    }
    return(s - sqrt(2.0 * delta * A_of(v)))
  }

  cur <- obj(nu)
  lr <- 1.0
  for (iter in seq_len(as.integer(max_iter))) {
    A <- A_of(nu)
    lam <- if (A > 0) sqrt(A / (2.0 * delta)) else 0.0
    if (lam <= 1e-14) break

    grad <- numeric(m)
    for (j in seq_len(m)) {
      dA <- -2.0 * r[j] + 2.0 * sum(S[j, ] * nu)
      grad[j] <- c[j] - dA / (2.0 * lam)
    }
    if (max(abs(grad)) < tol) break

    step_size <- lr
    improved <- FALSE
    for (bt in seq_len(60)) {
      cand <- pmax(0.0, nu + step_size * grad)
      val <- obj(cand)
      if (val > cur + 1e-18) {
        nu <- cand
        cur <- val
        improved <- TRUE
        lr <- min(lr * 1.5, 1e6)
        break
      }
      step_size <- step_size * 0.5
    }
    if (!improved) break
  }
  A <- A_of(nu)
  lam <- if (A > 0) sqrt(A / (2.0 * delta)) else 0.0
  return(list(lam = lam, nu = nu))
}


.safrl_finish <- function(step, g, cols, cv, H, delta, lam, nu,
                           feasible, recovery) {
  n <- length(g)
  kl <- 0.0
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      kl <- kl + 0.5 * step[i] * H[[i]][j] * step[j]
    }
  }
  m <- length(cols)
  viol <- numeric(m)
  for (j in seq_len(m)) {
    s <- cv[j]
    for (i in seq_len(n)) {
      s <- s + cols[[j]][i] * step[i]
    }
    viol[j] <- s
  }
  gain <- 0.0
  for (i in seq_len(n)) {
    gain <- gain + g[i] * step[i]
  }
  return(list(
    estimate = as.numeric(step),
    step = as.numeric(step),
    lambda_ = if (is.null(lam)) NULL else as.numeric(lam),
    nu = as.numeric(nu),
    feasible = as.logical(feasible),
    recovery = as.logical(recovery),
    predicted_gain = as.numeric(gain),
    predicted_violation = as.numeric(viol),
    kl = as.numeric(kl),
    delta = as.numeric(delta),
    method = "CPO step (Achiam et al. 2017, eqs. 11-14)"
  ))
}


#' morie_safrl
#'
#' Part of the safrl_native implementation; see the file header for the
#' source it follows.
#'
#' @param g See Usage.
#' @param H See Usage.
#' @param B Defaults to \code{NULL}.
#' @param c Defaults to \code{NULL}.
#' @param delta Defaults to \code{0.01}.
#' @param tol Defaults to \code{1e-12}.
#' @param max_iter Defaults to \code{5000}.
#' @return The value of \code{.safrl_finish}.
#' @export
morie_safrl <- function(g, H, B = NULL, c = NULL, delta = 0.01,
                         tol = 1e-12, max_iter = 5000) {
  gv <- .safrl_vec(g, "g")
  Hm <- .safrl_mat(H, "H")
  n <- length(gv)
  if (length(Hm) != n || length(Hm[[1]]) != n) {
    stop("safrl: H must be (n, n) matching g")
  }
  delta <- as.numeric(delta)
  if (delta <= 0.0) {
    stop("safrl: delta must be > 0")
  }

  Hinv_g <- .safrl_solve(Hm, gv)
  q <- sum(gv * Hinv_g)
  if (q < 0.0) {
    stop("safrl: g^T H^-1 g < 0; H is not positive definite")
  }

  if (is.null(B) || is.null(c)) {
    # Unconstrained: the natural gradient step of TRPO.
    scale <- if (q > 0) sqrt(2.0 * delta / q) else 0.0
    step <- scale * Hinv_g
    return(.safrl_finish(step, gv, list(), numeric(0), Hm, delta,
                          NULL, numeric(0), TRUE, FALSE))
  }

  Bm <- .safrl_mat(B, "B")
  if (length(Bm) != n) {
    stop("safrl: B must have one row per parameter")
  }
  m <- length(Bm[[1]])
  cv <- .safrl_vec(c, "c")
  if (length(cv) != m) {
    stop("safrl: c must have one entry per constraint")
  }

  # cols: list of m columns, each of length n.
  cols <- vector("list", m)
  for (j in seq_len(m)) {
    col <- numeric(n)
    for (i in seq_len(n)) {
      col[i] <- Bm[[i]][j]
    }
    cols[[j]] <- col
  }

  Hinv_b <- lapply(cols, function(col) .safrl_solve(Hm, col))

  r <- numeric(m)
  for (j in seq_len(m)) {
    r[j] <- sum(gv * Hinv_b[[j]])
  }

  S <- matrix(0, nrow = m, ncol = m)
  for (a in seq_len(m)) {
    for (b in seq_len(m)) {
      S[a, b] <- sum(cols[[a]] * Hinv_b[[b]])
    }
  }

  # Feasibility of eq. 11: for a single constraint the trust region
  # can satisfy c + b^T dtheta <= 0 iff c <= sqrt(2 delta b^T H^-1 b),
  # since the most negative attainable b^T dtheta is exactly that.
  feasible <- TRUE
  for (j in seq_len(m)) {
    reach <- sqrt(max(0.0, 2.0 * delta * S[j, j]))
    if (cv[j] > reach + 1e-12) {
      feasible <- FALSE
      break
    }
  }

  if (!feasible) {
    # Section 6.2 recovery: move purely to reduce the violated
    # constraint, as far as the trust region allows.
    j <- which.max(cv)
    denom <- S[j, j]
    if (denom <= 0.0) {
      stop(sprintf("safrl: constraint %d has zero curvature; cannot recover", j))
    }
    scale <- sqrt(2.0 * delta / denom)
    step <- -scale * Hinv_b[[j]]
    return(.safrl_finish(step, gv, cols, cv, Hm, delta,
                          NULL, numeric(0), FALSE, TRUE))
  }

  dual_result <- .safrl_dual(q, r, S, cv, delta, m, tol, max_iter)
  lam <- dual_result$lam
  nu <- dual_result$nu

  # eq. 13. Written as sqrt(2 delta / A) H^-1 (g - B nu) rather than
  # dividing by lambda, which is the same number but stays finite
  # when the constraints pin the step to zero and A -> 0 with it.
  Bnu <- numeric(n)
  for (i in seq_len(n)) {
    s <- 0.0
    for (j in seq_len(m)) {
      s <- s + cols[[j]][i] * nu[j]
    }
    Bnu[i] <- s
  }
  rhs <- gv - Bnu
  Hinv_rhs <- .safrl_solve(Hm, rhs)
  A <- sum(rhs * Hinv_rhs)
  if (A <= 1e-12 * max(1.0, q)) {
    # lambda* = 0: the objective gradient is entirely absorbed by
    # the constraint multipliers (g = B nu), so the KL constraint is
    # INACTIVE and eq. 13's division by lambda does not apply. The
    # primal optimum is then the zero step -- every direction the
    # objective wants is blocked by an active constraint. Scaling
    # the (numerically arbitrary) residual up to the full trust
    # region here would spend the whole KL budget going nowhere
    # useful, and would violate the constraints it just satisfied.
    step <- rep(0.0, n)
    lam <- 0.0
  } else {
    scale <- sqrt(2.0 * delta / A)
    step <- scale * Hinv_rhs
  }
  return(.safrl_finish(step, gv, cols, cv, Hm, delta, lam, nu,
                        TRUE, FALSE))
}


#' morie_safrl_cmdp_returns
#'
#' Part of the safrl_native implementation; see the file header for the
#' source it follows.
#'
#' @param policy See Usage.
#' @param states See Usage.
#' @param actions See Usage.
#' @param step See Usage.
#' @param reward See Usage.
#' @param costs See Usage.
#' @param gamma Defaults to \code{0.9}.
#' @param start Defaults to \code{NULL}.
#' @param iters Defaults to \code{5000}.
#' @param tol Defaults to \code{1e-14}.
#' @return A list with \code{estimate}, \code{J}, \code{J_C}, \code{gamma}, \code{method}.
#' @export
morie_safrl_cmdp_returns <- function(policy, states, actions, step,
                                      reward, costs, gamma = 0.9,
                                      start = NULL, iters = 5000,
                                      tol = 1e-14) {
  S <- as.list(states)
  A <- as.list(actions)
  dists <- c(list(reward), as.list(costs))
  out <- list()

  nS <- length(S)
  s_keys <- as.character(S)

  for (fn in dists) {
    V <- setNames(rep(0.0, nS), s_keys)
    for (iter in seq_len(as.integer(iters))) {
      newV <- setNames(rep(0.0, nS), s_keys)
      for (si in seq_len(nS)) {
        s <- S[[si]]
        tot <- 0.0
        for (a in A) {
          p <- policy(s, a)
          if (p == 0.0) next
          s1 <- step(s, a)
          k1 <- as.character(s1)
          tot <- tot + p * (fn(s, a, s1) + gamma * V[[k1]])
        }
        newV[si] <- tot
      }
      if (max(abs(newV - V)) < tol) {
        V <- newV
        break
      }
      V <- newV
    }

    if (is.null(start)) {
      out[[length(out) + 1]] <- mean(V)
    } else if (is.function(start)) {
      acc <- 0.0
      for (si in seq_len(nS)) {
        acc <- acc + start(S[[si]]) * V[si]
      }
      out[[length(out) + 1]] <- acc
    } else {
      out[[length(out) + 1]] <- V[[as.character(start)]]
    }
  }

  return(list(
    estimate = out[[1]],
    J = out[[1]],
    J_C = out[-1],
    gamma = as.numeric(gamma),
    method = "CMDP returns (Altman 1999; Achiam et al. 2017 sec. 4)"
  ))
}


#' morie_safrl_worst_case_violation
#'
#' Part of the safrl_native implementation; see the file header for the
#' source it follows.
#'
#' @param delta See Usage.
#' @param gamma See Usage.
#' @param epsilon See Usage.
#' @return A numeric value.
#' @export
morie_safrl_worst_case_violation <- function(delta, gamma, epsilon) {
  delta <- as.numeric(delta)
  gamma <- as.numeric(gamma)
  if (delta < 0.0) {
    stop("worst_case_violation: delta must be >= 0")
  }
  if (!(0.0 <= gamma && gamma < 1.0)) {
    stop("worst_case_violation: gamma must lie in [0, 1)")
  }
  return(sqrt(2.0 * delta) * gamma * as.numeric(epsilon) / (1.0 - gamma)^2)
}


# compact aliases per ledger/NAMING.md
morie_safe_rl <- morie_safrl
morie_saferl <- morie_safrl
morie_cpo_step <- morie_safrl
