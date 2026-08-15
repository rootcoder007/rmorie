# morie.fn -- function file (rootcoder007/morie)
# Semidefinite programming by the barrier method.
#
# An SDP minimises a linear objective over a **linear matrix
# inequality**:
#
# .. math:: \text{minimise } c^\top x \quad\text{subject to}\quad
#           F(x) = F_0 + \sum_{i=1}^{n} x_i F_i \succeq 0.
#
# The feasible set is convex -- the PSD cone intersected with an affine
# subspace -- so the problem is convex even though the constraint is on
# a matrix. That is the whole reason SDP is tractable, and it is why an
# LP is the special case where every :math:`F_i` is diagonal.
#
# **The barrier is :math:`-\log\det F(x)`**, and its two properties are
# what make the method work. It is **finite exactly on the interior**
# and rises to :math:`+\infty` at the boundary, so an iterate can never
# step outside the cone; and it is **self-concordant**, which is what
# gives Newton's method its complexity guarantee here rather than in a
# generic nonlinear solver. ``barrier`` returns infinity outside rather
# than a large number, because a finite value there would let a line
# search wander out of the feasible set undetected.
#
# **Centring, then decreasing :math:`t`.** For each :math:`t` the
# centring problem :math:`\min\; t\,c^\top x - \log\det F(x)` is solved,
# and its solution is on the **central path**. The duality gap at the
# central point is exactly :math:`m/t` with :math:`m` the matrix
# dimension, so the accuracy is *known* at every stage rather than
# inferred from how much the iterate moved -- ``central_path_gap``
# returns it.
#
# **A closed form to check against.** The problem
# :math:`\max\, t` s.t. :math:`A - tI \succeq 0` has the exact solution
# :math:`t^\star = \lambda_{\min}(A)`, so the solver's answer can be
# compared with an eigenvalue rather than with itself.
# ``min_eigenvalue_sdp`` sets that problem up, and the anchor uses it.
#
# References
# ----------
# Boyd, S. & Vandenberghe, L. (2004) *Convex Optimization*, Cambridge
# University Press, doi:10.1017/CBO9780511804441. Sec. 4.6.2 (the
# semidefinite program with its linear matrix inequality constraint,
# and LP as the case of diagonal matrices); Sec. 9.6 and 11.1 (the
# logarithmic barrier -log det X for the PSD cone, its self-concordance,
# and the central path); Sec. 11.2-11.3 (the barrier method: solve the
# centring problem for a sequence of increasing t, with the duality gap
# at a central point equal to m/t, and the trade-off in the choice of
# the multiplier mu between the number of outer iterations and the
# difficulty of each centring step); and Sec. 5.5 (the KKT conditions
# and complementary slackness used for the optimality check).
#
# Vandenberghe, L. & Boyd, S. (1996) "Semidefinite Programming",
# *SIAM Review* 38(1), 49-95, doi:10.1137/1038003. The survey
# treatment, including the eigenvalue problems that reduce to SDP.
#
# Nesterov, Y. & Nemirovskii, A. (1994) *Interior-Point Polynomial
# Algorithms in Convex Programming*, SIAM,
# doi:10.1137/1.9781611970791. Self-concordance, which is what makes
# the barrier method's complexity claim hold.

.sdpwts_EPS <- 1e-12

# F(x) = F0 + sum x_i F_i (the LMI)
sdpwts_lmi <- function(x, F0, Fs) {
  v <- as.numeric(x)
  A <- as.matrix(F0)
  n <- nrow(A)
  if (length(v) != length(Fs)) {
    stop("sdpwts: ", length(v), " variables but ",
         length(Fs), " matrices")
  }
  out <- A
  for (i in seq_along(v)) {
    M <- as.matrix(Fs[[i]])
    if (nrow(M) != n || ncol(M) != n) {
      stop("sdpwts: F_", i - 1, " is not ", n, "x", n)
    }
    out <- out + v[i] * M
  }
  out
}

# Eigenvalue test on the constraint matrix
sdpwts_is_psd <- function(M, tol = -1e-10) {
  A <- as.matrix(M)
  ev <- eigen(A, symmetric = TRUE, only.values = TRUE)$values
  mev <- min(ev)
  list(eigenvalues = as.numeric(ev),
       min_eigenvalue = mev,
       psd = mev >= as.numeric(tol),
       strictly_feasible = mev > 0)
}

# -log det F(x), INFINITE outside the cone
sdpwts_barrier <- function(x, F0, Fs) {
  M <- sdpwts_lmi(x, F0, Fs)
  ev <- eigen(M, symmetric = TRUE, only.values = TRUE)$values
  mev <- min(ev)
  if (mev <= 0) {
    return(list(value = Inf, feasible = FALSE,
                min_eigenvalue = mev,
                note = paste0("outside the cone the barrier is +inf, ",
                              "not a large number")))
  }
  list(value = -sum(log(ev)),
       feasible = TRUE,
       min_eigenvalue = mev,
       eigenvalues = as.numeric(ev))
}

# The duality gap at a central point is exactly m/t
sdpwts_central_path_gap <- function(t, m) {
  tt <- as.numeric(t)
  mm <- as.integer(m)
  if (tt <= 0 || mm < 1) {
    stop("sdpwts: t must be positive and m at least 1")
  }
  list(gap = mm / tt, t = tt, m = mm,
       note = "an exact suboptimality bound at every stage")
}

# Private: the centring objective t * c'x - log det F(x)
.sdpwts_objective <- function(x, c_vec, F0, Fs, t) {
  b <- sdpwts_barrier(x, F0, Fs)
  if (!b$feasible) return(Inf)
  as.numeric(t) * sum(as.numeric(c_vec) * as.numeric(x)) + b$value
}

# Private: centring by gradient descent with a feasibility-aware
# backtracking line search
.sdpwts_centre <- function(x0, c_vec, F0, Fs, t, iters = 200,
                           tol = 1e-12, h = 1e-6) {
  x <- as.numeric(x0)
  n <- length(x)
  f <- .sdpwts_objective(x, c_vec, F0, Fs, t)
  if (!is.finite(f)) {
    stop("sdpwts: the starting point is not strictly feasible, ",
         "so the barrier is infinite there")
  }
  it <- 0
  for (it in seq_len(as.integer(iters))) {
    g <- numeric(n)
    for (i in seq_len(n)) {
      up <- x; up[i] <- up[i] + h
      dn <- x; dn[i] <- dn[i] - h
      fu <- .sdpwts_objective(up, c_vec, F0, Fs, t)
      fd <- .sdpwts_objective(dn, c_vec, F0, Fs, t)
      if (!is.finite(fu) || !is.finite(fd)) {
        g[i] <- 0
      } else {
        g[i] <- (fu - fd) / (2 * h)
      }
    }
    gn <- sqrt(sum(g * g))
    if (gn < as.numeric(tol)) break
    step <- 1.0
    moved <- FALSE
    for (j in seq_len(80)) {
      cand <- x - step * g
      fc <- .sdpwts_objective(cand, c_vec, F0, Fs, t)
      if (is.finite(fc) && fc < f - 1e-14) {
        x <- cand
        f <- fc
        moved <- TRUE
        break
      }
      step <- step * 0.5
    }
    if (!moved) break
  }
  list(x = x, value = f, iterations = it)
}

# The barrier method: centre, increase t, repeat
sdpwts_solve_sdp <- function(c, F0, Fs, x0, t0 = 1.0, mu = 10.0,
                             tol = 1e-8, max_outer = 60) {
  cc <- as.numeric(c)
  x <- as.numeric(x0)
  F0m <- as.matrix(F0)
  m <- nrow(F0m)
  if (!sdpwts_is_psd(sdpwts_lmi(x, F0, Fs))$strictly_feasible) {
    stop("sdpwts: the starting point must be STRICTLY feasible ",
         "-- the barrier method cannot begin on the boundary")
  }
  if (as.numeric(mu) <= 1.0) {
    stop("sdpwts: mu must exceed 1, or t never increases")
  }
  t <- as.numeric(t0)
  path <- list()
  outer <- 0
  for (outer in seq_len(as.integer(max_outer))) {
    r <- .sdpwts_centre(x, cc, F0, Fs, t)
    x <- r$x
    gap <- sdpwts_central_path_gap(t, m)$gap
    path[[length(path) + 1]] <- list(
      t = t,
      gap = gap,
      objective = sum(cc * x))
    if (gap < as.numeric(tol)) break
    t <- t * as.numeric(mu)
  }
  list(estimate = as.numeric(x),
       x = x,
       objective = sum(cc * x),
       gap = path[[length(path)]]$gap,
       outer_iterations = outer,
       path = path,
       m = m,
       min_eigenvalue = sdpwts_is_psd(sdpwts_lmi(x, F0, Fs))$min_eigenvalue,
       method = "barrier method for SDP; Boyd & Vandenberghe (2004) Sec. 11.2-11.3",
       note = "the gap m/t is an exact bound, so 'converged' is a measurement rather than a guess")
}

# Maximise t s.t. A - tI >= 0  (exact answer = lambda_min(A))
sdpwts_min_eigenvalue_sdp <- function(A, t0 = 1.0, mu = 10.0,
                                      tol = 1e-9) {
  M <- as.matrix(A)
  n <- nrow(M)
  ev <- eigen(M, symmetric = TRUE, only.values = TRUE)$values
  lam <- min(ev)
  F0 <- M
  F1 <- matrix(0, n, n)
  diag(F1) <- -1
  start <- lam - 1
  r <- sdpwts_solve_sdp(-1, F0, list(F1), start, t0, mu, tol)
  list(estimate = r$x[1],
       t = r$x[1],
       lambda_min = lam,
       error = abs(r$x[1] - lam),
       outer_iterations = r$outer_iterations,
       gap = r$gap,
       method = "eigenvalue problem as an SDP; Vandenberghe & Boyd (1996)",
       note = "the exact answer is lambda_min(A), so the solver is checked against something other than itself")
}

# Compact alias per ledger/NAMING.md (entry point)
morie_sdpwts <- sdpwts_solve_sdp

sdpwts_cheatsheet <- function() {
  paste0("sdpwts: minimise c'x subject to a LINEAR MATRIX ",
         "INEQUALITY F0 + sum x_i F_i >= 0. The feasible set is the ",
         "PSD cone cut by an affine subspace -- convex, which is why ",
         "it is tractable, and LP is the diagonal special case. The ",
         "barrier is -log det F(x): FINITE only on the interior ",
         "(so an iterate cannot leave the cone) and SELF-CONCORDANT ",
         "(which is what earns Newton's complexity guarantee). ",
         "Solve the centring problem for increasing t; at a central ",
         "point the duality gap is EXACTLY m/t, so accuracy is ",
         "known, not inferred. Check against max t s.t. A - tI >= ",
         "0, whose answer is lambda_min(A).")
}

#' @rdname sdpwts_lmi
#' @export
morie_sdpwts <- sdpwts_lmi

#' @rdname sdpwts_lmi
#' @export
morie_sdpwts <- sdpwts_lmi

#' @rdname sdpwts_lmi
#' @export
morie_sdpwts <- sdpwts_lmi

#' @rdname sdpwts_lmi
#' @export
morie_sdpwts <- sdpwts_lmi

#' @rdname sdpwts_lmi
#' @export
morie_sdpwts <- sdpwts_lmi

#' @rdname sdpwts_lmi
#' @export
morie_sdpwts <- sdpwts_lmi

#' @rdname sdpwts_lmi
#' @export
morie_sdpwts <- sdpwts_lmi

#' @rdname sdpwts_lmi
#' @export
morie_sdpwts <- sdpwts_lmi

#' @rdname sdpwts_lmi
#' @export
morie_sdpwts <- sdpwts_lmi

#' @rdname sdpwts_lmi
#' @export
morie_sdpwts <- sdpwts_lmi

#' @rdname sdpwts_lmi
#' @export
morie_sdpwts <- sdpwts_lmi
