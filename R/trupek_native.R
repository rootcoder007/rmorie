# R arm of trupek -- trust-region minimisation (Conn, Gould & Toint 2000).
#
# A trust region is the ball around the current point inside which a
# quadratic model of the objective is believed. Each iteration solves the
# model on that ball, compares the reduction the model PREDICTED against
# the reduction actually achieved, and lets that ratio decide both whether
# to take the step and what the radius should be next time. The
# bookkeeping is Algorithm BTR (6.1.1); the interesting part is the
# subproblem, and the book gives several ways to solve it, so all of them
# are here.
#
#   cauchy    The Cauchy point: minimise the model along steepest descent
#             within the ball. The cheapest step that still guarantees
#             convergence (Theorem 6.3.1).
#   dogleg    Powell's dogleg: the path from the Cauchy point to the
#             Newton point, cut where it leaves the ball. Needs a
#             positive definite Hessian.
#   steihaug  Steihaug-Toint truncated conjugate gradients (Algorithm
#             7.5.1): CG on the model, stopping at the first iterate that
#             leaves the ball or meets non-positive curvature, taking the
#             boundary point in both cases. Handles indefinite Hessians
#             and never factorises.
#   exact     The More-Sorensen characterisation (Section 7.3), solved by
#             bisection on lambda -- safe, and unlike Newton on the
#             secular equation it has no hard case to special-case.
#
# Nothing here is stochastic, so a run reproduces exactly.
#
# References
#   Conn, A.R., Gould, N.I.M. & Toint, P.L. (2000) "Trust-Region
#     Methods." MPS-SIAM Series on Optimization, SIAM, Philadelphia.
#     Algorithm 6.1.1 (BTR), Section 6.3 (Cauchy point), Section 7.3
#     (exact subproblem), Algorithm 7.5.1 (Steihaug-Toint).
#   Powell, M.J.D. (1970) "A new algorithm for unconstrained
#     optimization", in Nonlinear Programming, Academic Press, 31-65.
#   Steihaug, T. (1983) "The conjugate gradient method and trust regions
#     in large scale optimization", SIAM Journal on Numerical Analysis
#     20(3), 626-637, doi:10.1137/0720042

.TRUPEK_SUBS <- c("steihaug", "cauchy", "dogleg", "exact")

# Arithmetic that both arms can agree on, to the last bit.
#
# Two R habits break bit-for-bit agreement with a plain Python loop, and
# neither is obvious:
#
#   R's sum() accumulates in LONG DOUBLE, 80 bits on x86, and only rounds
#   back to double at the end. Python's sum() accumulates in double. On a
#   sum with cancellation -- which is every gradient near a minimum --
#   the two answers differ in the last bit. sum() is not the same
#   function in the two languages, however identical the source looks.
#
#   The matrix product operator goes through BLAS, which may reassociate
#   and use fused multiply-add.
#
# So dot products and matrix-vector products are written out as loops.
# Slower, and exactly reproducible.
#' So dot products and matrix-vector products are written out as loops
#'
#' Slower, and exactly reproducible.
#'
#' @param a A vector; its length is taken and its elements indexed.
#' @param b A vector; indexed elementwise.
#' @return A numeric value.
#' @export
.trupek_dot <- function(a, b) {
  s <- 0
  cc <- 0
  for (i in seq_along(a)) {
    t <- a[i] * b[i]
    u <- s + t
    if (abs(s) >= abs(t)) cc <- cc + ((s - u) + t)
    else cc <- cc + ((t - u) + s)
    s <- u
  }
  s + cc
}

#' .trupek_csum
#'
#' A step of the trupek_native implementation. Called by \code{.trupek_exact}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken and its elements indexed.
#' @return A numeric value.
#' @export
.trupek_csum <- function(v) {
  s <- 0
  cc <- 0
  for (i in seq_along(v)) {
    t <- v[i]
    u <- s + t
    if (abs(s) >= abs(t)) cc <- cc + ((s - u) + t)
    else cc <- cc + ((t - u) + s)
    s <- u
  }
  s + cc
}

#' .trupek_norm
#'
#' A step of the trupek_native implementation. Called by \code{.trupek_cauchy}, \code{.trupek_dogleg}, \code{.trupek_exact} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Passed to \code{.trupek_dot}.
#' @return A numeric value.
#' @export
.trupek_norm <- function(a) sqrt(.trupek_dot(a, a))

#' .trupek_matvec
#'
#' A step of the trupek_native implementation. Called by \code{.trupek_cauchy}, \code{.trupek_dogleg}, \code{.trupek_model} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param H A matrix; indexed by row and column.
#' @param v Passed to \code{.trupek_dot}.
#' @return The value of \code{out}, as built in the body.
#' @export
.trupek_matvec <- function(H, v) {
  n <- nrow(H)
  out <- numeric(n)
  for (i in seq_len(n)) out[i] <- .trupek_dot(H[i, ], v)
  out
}

#' .trupek_model
#'
#' A step of the trupek_native implementation. Called by \code{morie_trupek_trust_region}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g Passed to \code{.trupek_dot}.
#' @param H Passed to \code{.trupek_matvec}.
#' @param s Passed to \code{.trupek_dot}.
#' @return A numeric value.
#' @export
.trupek_model <- function(g, H, s)
  .trupek_dot(g, s) + 0.5 * .trupek_dot(s, .trupek_matvec(H, s))

# The positive tau with ||z + tau d|| = delta. Taking the stable root and
# recovering the other from the product keeps precision when the two roots
# differ wildly.
#' The positive tau with ||z + tau d|| = delta. Taking the stable root
#' and
#'
#' recovering the other from the product keeps precision when the two
#' roots differ wildly.
#'
#' @param z Passed to \code{.trupek_dot}.
#' @param d Passed to \code{.trupek_dot}.
#' @param delta Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
.trupek_boundary <- function(z, d, delta) {
  dd <- .trupek_dot(d, d)
  zd <- .trupek_dot(z, d)
  zz <- .trupek_dot(z, z)
  disc <- zd * zd - dd * (zz - delta * delta)
  if (disc < 0) disc <- 0
  sq <- sqrt(disc)
  if (zd >= 0) {
    tau <- if (dd > 0) (-zd - sq) / dd else 0
    tau2 <- if ((zd + sq) != 0) (zz - delta * delta) / (-zd - sq) else tau
    max(tau, tau2)
  } else {
    if (dd > 0) (-zd + sq) / dd else 0
  }
}

#' .trupek_cauchy
#'
#' A step of the trupek_native implementation. Called by \code{.trupek_dogleg}, \code{.trupek_sub}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A vector; its length is taken.
#' @param H Passed to \code{.trupek_matvec}.
#' @param delta Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.trupek_cauchy <- function(g, H, delta) {
  gn <- .trupek_norm(g)
  if (gn == 0) return(rep(0, length(g)))
  curv <- .trupek_dot(g, .trupek_matvec(H, g))
  t <- if (curv <= 0) delta / gn else min(gn * gn / curv, delta / gn)
  -t * g
}

# Cholesky, or NULL when H is not positive definite. Used both to test
# definiteness and to solve, so there is one code path.
#' Cholesky, or NULL when H is not positive definite. Used both to test
#'
#' definiteness and to solve, so there is one code path.
#'
#' @param H A matrix; indexed by row and column.
#' @return The value of \code{L}, as built in the body.
#' @export
.trupek_chol <- function(H) {
  n <- nrow(H)
  L <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(i)) {
      s <- H[i, j] - if (j > 1) .trupek_dot(L[i, seq_len(j - 1)], L[j, seq_len(j - 1)]) else 0
      if (i == j) {
        if (s <= 0) return(NULL)
        L[i, i] <- sqrt(s)
      } else L[i, j] <- s / L[j, j]
    }
  }
  L
}

#' .trupek_chol_solve
#'
#' A step of the trupek_native implementation. Called by \code{.trupek_dogleg}, \code{.trupek_exact}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param L A matrix; indexed by row and column.
#' @param b A vector; indexed elementwise.
#' @return The value of \code{x}, as built in the body.
#' @export
.trupek_chol_solve <- function(L, b) {
  n <- nrow(L)
  y <- numeric(n)
  for (i in seq_len(n))
    y[i] <- (b[i] - if (i > 1) .trupek_dot(L[i, seq_len(i - 1)], y[seq_len(i - 1)]) else 0) / L[i, i]
  x <- numeric(n)
  for (i in rev(seq_len(n)))
    x[i] <- (y[i] - if (i < n) .trupek_dot(L[seq(i + 1, n), i], x[seq(i + 1, n)]) else 0) / L[i, i]
  x
}

#' .trupek_dogleg
#'
#' A step of the trupek_native implementation. Called by \code{.trupek_sub}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g Numeric; combined arithmetically in the body.
#' @param H Passed to \code{.trupek_chol}.
#' @param delta Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.trupek_dogleg <- function(g, H, delta) {
  L <- .trupek_chol(H)
  if (is.null(L)) return(.trupek_cauchy(g, H, delta))
  pb <- -.trupek_chol_solve(L, g)
  if (.trupek_norm(pb) <= delta) return(pb)
  curv <- .trupek_dot(g, .trupek_matvec(H, g))
  if (curv <= 0) return(.trupek_cauchy(g, H, delta))
  pu <- -(.trupek_dot(g, g) / curv) * g
  if (.trupek_norm(pu) >= delta) return(-(delta / .trupek_norm(g)) * g)
  d <- pb - pu
  pu + .trupek_boundary(pu, d, delta) * d
}

#' .trupek_steihaug
#'
#' A step of the trupek_native implementation. Called by \code{.trupek_sub}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A vector; its length is taken.
#' @param H Passed to \code{.trupek_matvec}.
#' @param delta Passed to \code{.trupek_boundary}.
#' @param tol Numeric; passed to \code{max}.
#' @param maxit Passed to \code{>}.
#' @return A list with \code{s}, \code{why}.
#' @export
.trupek_steihaug <- function(g, H, delta, tol, maxit) {
  n <- length(g)
  z <- rep(0, n)
  r <- g
  d <- -g
  gn <- .trupek_norm(g)
  if (gn == 0) return(list(s = z, why = "zero gradient"))
  stop_at <- max(min(0.5, sqrt(gn)) * gn, tol)
  lim <- if (maxit > 0) maxit else 2L * n + 1L
  for (it in seq_len(lim)) {
    Hd <- .trupek_matvec(H, d)
    dHd <- .trupek_dot(d, Hd)
    if (dHd <= 0) {
      tau <- .trupek_boundary(z, d, delta)
      return(list(s = z + tau * d,
                  why = "negative curvature, stopped on the boundary"))
    }
    alpha <- .trupek_dot(r, r) / dHd
    z_next <- z + alpha * d
    if (.trupek_norm(z_next) >= delta) {
      tau <- .trupek_boundary(z, d, delta)
      return(list(s = z + tau * d,
                  why = "left the region, stopped on the boundary"))
    }
    r_next <- r + alpha * Hd
    if (.trupek_norm(r_next) < stop_at)
      return(list(s = z_next, why = "interior, residual below tolerance"))
    beta <- .trupek_dot(r_next, r_next) / .trupek_dot(r, r)
    d <- -r_next + beta * d
    z <- z_next
    r <- r_next
  }
  list(s = z, why = "iteration limit")
}

#' .trupek_exact
#'
#' A step of the trupek_native implementation. Called by \code{.trupek_sub}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g A vector; its length is taken.
#' @param H A matrix; indexed by row and column.
#' @param delta Numeric; combined arithmetically in the body.
#' @param tol Numeric; combined arithmetically in the body.
#' @param maxit Passed to \code{>}.
#' @return A list with \code{s}, \code{lambda}, \code{why}.
#' @export
.trupek_exact <- function(g, H, delta, tol, maxit) {
  n <- length(g)
  L <- .trupek_chol(H)
  if (!is.null(L)) {
    s <- -.trupek_chol_solve(L, g)
    if (.trupek_norm(s) <= delta)
      return(list(s = s, lambda = 0,
                  why = "interior, Hessian positive definite"))
  }
  lo <- 0
  hi <- 1
  for (i in seq_len(n)) {
    row <- -H[i, i] + .trupek_csum(abs(H[i, -i]))
    if (row > hi) hi <- row
  }
  hi <- max(hi, .trupek_norm(g) / delta + 1)
  repeat {
    Ls <- .trupek_chol(H + diag(hi, n))
    if (!is.null(Ls) &&
        .trupek_norm(.trupek_chol_solve(Ls, g)) <= delta) break
    hi <- hi * 2
    if (hi > 1e300) break
  }
  s <- rep(0, n)
  lam <- hi
  lim <- if (maxit > 0) maxit else 200L
  for (it in seq_len(lim)) {
    lam <- 0.5 * (lo + hi)
    Ls <- .trupek_chol(H + diag(lam, n))
    if (is.null(Ls)) { lo <- lam
    next }
    s <- -.trupek_chol_solve(Ls, g)
    ns <- .trupek_norm(s)
    if (abs(ns - delta) <= tol * delta) break
    if (ns > delta) lo <- lam else hi <- lam
  }
  list(s = s, lambda = lam, why = "on the boundary, shifted by lambda")
}

#' .trupek_sub
#'
#' A step of the trupek_native implementation. Called by \code{morie_trupek_trust_region}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param g Passed to \code{.trupek_cauchy}.
#' @param H Passed to \code{.trupek_cauchy}.
#' @param delta Passed to \code{.trupek_cauchy}.
#' @param sub One of \code{"cauchy"}, \code{"dogleg"}, \code{"exact"}.
#' @param tol Passed to \code{.trupek_exact}.
#' @param maxit Passed to \code{.trupek_exact}.
#' @return The value of \code{.trupek_steihaug}.
#' @export
.trupek_sub <- function(g, H, delta, sub, tol, maxit) {
  if (sub == "cauchy")
    return(list(s = .trupek_cauchy(g, H, delta), why = "Cauchy point"))
  if (sub == "dogleg")
    return(list(s = .trupek_dogleg(g, H, delta), why = "dogleg path"))
  if (sub == "exact") {
    r <- .trupek_exact(g, H, delta, tol, maxit)
    return(list(s = r$s, why = r$why))
  }
  .trupek_steihaug(g, H, delta, tol, maxit)
}

#' Trust-region minimisation
#'
#' @param f objective, taking a numeric vector.
#' @param grad_f gradient, taking a numeric vector.
#' @param hess_f Hessian, taking a numeric vector and returning a matrix.
#' @param x0 starting point.
#' @param delta initial radius.
#' @param delta_max cap on the radius; defaults to 1e3 times delta.
#' @param subproblem steihaug, cauchy, dogleg or exact.
#' @param eta1 accept the step when the ratio reaches this.
#' @param eta2 enlarge the radius only when the ratio reaches this.
#' @param gamma1 shrink factor.
#' @param gamma3 enlarge factor.
#' @param max_iter iteration cap.
#' @param gtol stop when the gradient norm falls below this.
#' @param dtol stop when the radius collapses below this. Near the
#'   solution the predicted reduction drops under the rounding noise in
#'   f, the ratio stops meaning anything and every step gets rejected;
#'   without this the loop shrinks to zero and burns max_iter.
#' @param sub_tol subproblem tolerance.
#' @param sub_maxit subproblem iteration cap, 0 for the default.
#' @return a list with x, fval, gnorm, delta, iterations, accepted,
#'   rejected, converged, exit_reason, history, subproblem,
#'   subproblem_exit and method.
#' @export
morie_trupek_trust_region <- function(f, grad_f, hess_f, x0, delta = 1,
                                      delta_max = NULL,
                                      subproblem = "steihaug",
                                      eta1 = 0.01, eta2 = 0.9,
                                      gamma1 = 0.5, gamma3 = 2,
                                      max_iter = 200L, gtol = 1e-10,
                                      dtol = 1e-14, sub_tol = 1e-12,
                                      sub_maxit = 0L) {
  if (!(length(subproblem) == 1L && subproblem %in% .TRUPEK_SUBS))
    stop(sprintf("trupek: subproblem = %s; expected one of %s", subproblem,
                 paste(.TRUPEK_SUBS, collapse = ", ")), call. = FALSE)
  x <- as.numeric(x0)
  if (is.null(delta_max)) delta_max <- 1e3 * delta
  fx <- as.numeric(f(x))
  acc <- 0L
  rej <- 0L
  hist <- list()
  conv <- FALSE
  last_why <- "not started"
  why <- "iteration limit"
  k <- 0L
  for (k in seq_len(as.integer(max_iter))) {
    g <- as.numeric(grad_f(x))
    gn <- .trupek_norm(g)
    if (gn <= gtol) {
      conv <- TRUE
      why <- "gradient below gtol"
      hist[[length(hist) + 1L]] <- c(fx, gn, delta, 0)
      break
    }
    if (delta <= dtol) {
      why <- "radius collapsed below dtol"
      hist[[length(hist) + 1L]] <- c(fx, gn, delta, 0)
      break
    }
    H <- as.matrix(hess_f(x))
    sr <- .trupek_sub(g, H, delta, subproblem, sub_tol, sub_maxit)
    s <- sr$s
    last_why <- sr$why
    pred <- -.trupek_model(g, H, s)
    if (pred <= 0) {
      delta <- delta * gamma1
      rej <- rej + 1L
      hist[[length(hist) + 1L]] <- c(fx, gn, delta, 0)
      next
    }
    xt <- x + s
    ft <- as.numeric(f(xt))
    rho <- (fx - ft) / pred
    hist[[length(hist) + 1L]] <- c(fx, gn, delta, rho)
    if (rho >= eta1) {
      x <- xt
      fx <- ft
      acc <- acc + 1L
    } else rej <- rej + 1L
    sn <- .trupek_norm(s)
    if (rho < eta1) delta <- gamma1 * delta
    else if (rho >= eta2 && sn >= (1 - 1e-12) * delta)
      delta <- min(gamma3 * delta, delta_max)
  }
  g <- as.numeric(grad_f(x))
  list(exit_reason = why,
       x = x, fval = fx, gnorm = .trupek_norm(g), delta = delta,
       iterations = as.integer(k), accepted = acc, rejected = rej,
       converged = conv, history = hist, subproblem = subproblem,
       subproblem_exit = last_why,
       method = sprintf(paste("basic trust region (Conn, Gould & Toint",
                              "2000, Algorithm 6.1.1) with the %s",
                              "subproblem"), subproblem))
}

#' Trust-region minimisation
#'
#' @param f objective.
#' @param grad_f gradient.
#' @param hess_f Hessian.
#' @param x0 starting point.
#' @param ... passed to morie_trupek_trust_region.
#' @return see morie_trupek_trust_region.
#' @export
morie_trupek <- function(f, grad_f, hess_f, x0, ...)
  morie_trupek_trust_region(f, grad_f, hess_f, x0, ...)
