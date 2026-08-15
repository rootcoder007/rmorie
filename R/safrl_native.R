```r
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
        a <-
