# The logarithmic barrier method for inequality-constrained convex
# problems. Native implementation mirroring Python morie.fn.barerp
# exactly: the same barrier, the same gradient and Hessian (Boyd p. 564),
# the same central-path dual, the same KKT solve for the Newton step,
# the same backtracking line search, the same Algorithm 11.1 outer
# loop, the same phase I feasibility finder, and the same linear-
# program entry point. RNG draws (when any are used) come from the
# shared SplitMix64 generator so the two arms produce the SAME stream.
#
# Sources: Frisch, R. (1956) "La resolution des problemes de programme
# lineaire par la methode du potentiel logarithmique", Cahiers du
# Seminaire d'Econometrie No. 4, 7-23 (JSTOR 20075373); Frisch, R.
# (1957) "The Multiplex Method for Linear Programming", Sankhya 18(3/4),
# 329-362 (JSTOR 25048355); Boyd, S. and Vandenberghe, L. (2004)
# Convex Optimization, Cambridge University Press, Chapter 11.

#' Log barrier
#'
#' Boyd equation 11.5: \code{phi(x) = -sum log(-f_i(x))}.
#' Returns \code{+Inf} outside the strict interior.
#'
#' @param fvals Numeric vector of \eqn{f_i(x)} values.
#' @return Scalar barrier value, or \code{Inf} if any entry is >= 0.
#' @references Boyd, S. and Vandenberghe, L. (2004). Convex
#'   Optimization. Cambridge University Press, ch. 11.
#' @export
log_barrier <- function(fvals) {
  fvals <- as.numeric(fvals)
  out <- 0.0
  for (v in fvals) {
    if (v >= 0.0) return(Inf)
    out <- out - log(-v)
  }
  out
}

#' Frisch potential
#'
#' Frisch equation 5.1: \code{V = sum log(x_k)} over the slacks.
#' Returns \code{-Inf} outside the strict interior.
#'
#' @param slacks Numeric vector of slack values (must be > 0).
#' @return Scalar potential, or \code{-Inf} if any entry is <= 0.
#' @references Frisch, R. (1956).
#' @export
frisch_potential <- function(slacks) {
  slacks <- as.numeric(slacks)
  out <- 0.0
  for (v in slacks) {
    if (v <= 0.0) return(-Inf)
    out <- out + log(v)
  }
  out
}

#' Log barrier gradient
#'
#' \code{grad phi = sum grad f_i / (-f_i)} (Boyd p. 564).
#'
#' @param fvals Numeric vector of \eqn{f_i(x)} values.
#' @param jac Numeric matrix of constraint gradients (one row per
#'   constraint).
#' @return Numeric vector of length \code{ncol(jac)}.
#' @export
log_barrier_gradient <- function(fvals, jac) {
  fvals <- as.numeric(fvals)
  jac <- as.matrix(jac)
  n <- ncol(jac)
  out <- rep(0.0, n)
  if (length(fvals) == 0L) return(out)
  for (i in seq_along(fvals)) {
    w <- 1.0 / (-fvals[i])
    gi <- jac[i, ]
    out <- out + w * gi
  }
  out
}

#' Log barrier Hessian
#'
#' \code{H phi = sum grad f_i grad f_i^T / f_i^2 + sum H f_i / (-f_i)}
#' (Boyd p. 564). The second term is omitted when \code{hess} is
#' \code{NULL}, which is the affine case.
#'
#' @param fvals Numeric vector of \eqn{f_i(x)} values.
#' @param jac Numeric matrix of constraint gradients.
#' @param hess Optional list of constraint Hessians, one per
#'   constraint; \code{NULL} entries are skipped (use for affine).
#' @return Square numeric matrix.
#' @export
log_barrier_hessian <- function(fvals, jac, hess = NULL) {
  fvals <- as.numeric(fvals)
  jac <- as.matrix(jac)
  n <- ncol(jac)
  out <- matrix(0.0, n, n)
  if (length(fvals) == 0L) return(out)
  for (i in seq_along(fvals)) {
    v <- fvals[i]
    gi <- jac[i, ]
    w <- 1.0 / (v * v)
    out <- out + w * tcrossprod(gi)
    if (!is.null(hess) && !is.null(hess[[i]])) {
      w2 <- 1.0 / (-v)
      hi <- hess[[i]]
      out <- out + w2 * hi
    }
  }
  out
}

#' Central-path dual
#'
#' Boyd equation 11.10: \code{lambda_i*(t) = -1 / (t f_i(x))}.
#'
#' @param fvals Numeric vector of \eqn{f_i(x)} values.
#' @param t Positive scalar.
#' @return Numeric vector of dual multipliers.
#' @export
central_path_dual <- function(fvals, t) {
  t <- as.numeric(t)
  if (t <= 0.0) stop("barerp: t must be positive")
  fvals <- as.numeric(fvals)
  -1.0 / (t * fvals)
}

#' Predicted centering steps
#'
#' Boyd equation 11.13: \code{ceiling(log(m/(eps t0)) / log(mu))},
#' clamped to be non-negative, with a tiny subtraction \code{1e-12}
#' before the ceiling so a perfectly integer value is not bumped up.
#'
#' @param m Number of inequality constraints.
#' @param eps Duality-gap tolerance, positive.
#' @param t0 Initial \eqn{t}, positive.
#' @param mu Growth factor, > 1.
#' @return Non-negative integer.
#' @export
centering_steps <- function(m, eps, t0, mu) {
  m <- as.numeric(m); eps <- as.numeric(eps)
  t0 <- as.numeric(t0); mu <- as.numeric(mu)
  if (mu <= 1.0) stop("barerp: mu must exceed 1")
  if (eps <= 0.0 || t0 <= 0.0) stop("barerp: eps and t0 must be positive")
  val <- log(m / (eps * t0)) / log(mu)
  max(0L, as.integer(ceiling(val - 1e-12)))
}

# ---------------------------------------------------------------------------
# numerical derivatives, supplied or differenced
# ---------------------------------------------------------------------------

.num_grad <- function(f, x, h = 1e-6) {
  x <- as.numeric(x)
  n <- length(x)
  out <- numeric(n)
  for (j in seq_len(n)) {
    step <- h * max(1.0, abs(x[j]))
    up <- x; up[j] <- up[j] + step
    dn <- x; dn[j] <- dn[j] - step
    out[j] <- (f(up) - f(dn)) / (2.0 * step)
  }
  out
}

.num_hess <- function(f, x, h = 1e-4) {
  x <- as.numeric(x)
  n <- length(x)
  out <- matrix(0.0, n, n)
  f0 <- f(x)
  for (a in seq_len(n)) {
    sa <- h * max(1.0, abs(x[a]))
    for (b in seq_len(a, n)) {
      sb <- h * max(1.0, abs(x[b]))
      xpp <- x; xpm <- x; xmp <- x; xmm <- x
      xpp[a] <- xpp[a] + sa; xpp[b] <- xpp[b] + sb
      xpm[a] <- xpm[a] + sa; xpm[b] <- xpm[b] - sb
      xmp[a] <- xmp[a] - sa; xmp[b] <- xmp[b] + sb
      xmm[a] <- xmm[a] - sa; xmm[b] <- xmm[b] - sb
      if (a == b) {
        val <- (f(xpp) - 2.0 * f0 + f(xmm)) / (4.0 * sa * sa)
      } else {
        val <- (f(xpp) - f(xpm) - f(xmp) + f(xmm)) / (4.0 * sa * sb)
      }
      out[a, b] <- val
      out[b, a] <- val
    }
  }
  out
}

# An R closure carrying whatever derivatives were supplied. Mirrors
# the Python _Fun class field-for-field.
.Fun <- function(f, grad = NULL, hess = NULL, affine = FALSE) {
  self <- list(f = f, .g = grad, .h = hess, affine = isTRUE(affine))
  class(self) <- "Fun"
  self
}

val.Fun <- function(self, x) as.numeric(self$f(x))
grad.Fun <- function(self, x) {
  if (!is.null(self$.g)) {
    v <- self$.g(x)
    return(as.numeric(v))
  }
  .num_grad(self$f, x)
}
hess.Fun <- function(self, x) {
  if (!is.null(self$.h)) {
    M <- self$.h(x)
    return(matrix(as.numeric(M), length(x), length(x)))
  }
  if (self$affine) return(matrix(0.0, length(x), length(x)))
  .num_hess(self$f, x)
}

.as_fun <- function(spec) {
  if (inherits(spec, "Fun")) return(spec)
  if (is.function(spec)) return(.Fun(spec))
  if (is.list(spec)) {
    return(.Fun(spec$f, spec$grad, spec$hess, isTRUE(spec$affine)))
  }
  stop(paste0("barerp: a constraint must be a callable or a dict ",
              "with 'f' and optionally 'grad', 'hess', 'affine'"))
}

# ---------------------------------------------------------------------------
# the KKT solve for one Newton step (Boyd eq. 11.14)
# ---------------------------------------------------------------------------

.solve_kkt <- function(hmat, grad, aeq) {
  n <- length(grad)
  if (is.null(aeq) || length(aeq) == 0L) {
    sol <- solve(hmat, -grad)
    return(as.numeric(sol))
  }
  aeq <- as.matrix(aeq)
  p <- nrow(aeq)
  big <- matrix(0.0, n + p, n + p)
  big[seq_len(n), seq_len(n)] <- hmat
  big[seq_len(n) + n, seq_len(n)] <- aeq
  big[seq_len(n), seq_len(n) + n] <- aeq
  rhs <- c(-grad, rep(0.0, p))
  sol <- solve(big, rhs)
  sol[seq_len(n)]
}

# ---------------------------------------------------------------------------
# centering
# ---------------------------------------------------------------------------

.project_null <- function(v, aeq) {
  aeq <- as.matrix(aeq)
  v <- as.numeric(v)
  gram <- aeq %*% t(aeq)
  rhs <- aeq %*% v
  lam <- solve(gram, rhs)
  corr <- as.numeric(t(aeq) %*% lam)
  v - corr
}

#' Central point
#'
#' Compute \eqn{x*(t) = argmin t f0 + phi}. \code{centering="newton"}
#' uses Newton's method with backtracking line search and stops on the
#' Newton decrement; \code{"gradient"} takes Frisch's compromise as a
#' plain descent step on the same function.
#'
#' @param f0 Objective as a \code{Fun}, a function, or a list.
#' @param cons List of constraints in the same form.
#' @param x Starting point.
#' @param t Centering parameter.
#' @param aeq Optional equality-constraint matrix.
#' @param centering \code{"newton"} or \code{"gradient"}.
#' @param tol Tolerance on the decrement.
#' @param max_iter Maximum Newton (or gradient) iterations.
#' @param alpha Backtracking line-search parameter.
#' @param beta Backtracking line-search shrink factor.
#' @param step0 Initial line-search step.
#' @return List with \code{x}, \code{iters}, \code{decrement}.
#' @export
central_point <- function(f0, cons, x, t,
                          aeq = NULL, centering = "newton",
                          tol = 1e-10, max_iter = 200L,
                          alpha = 0.01, beta = 0.5, step0 = 1.0) {
  f0 <- .as_fun(f0)
  cons <- lapply(cons, .as_fun)
  x <- as.numeric(x)
  n <- length(x)
  t <- as.numeric(t)

  objective <- function(z) {
    fv <- vapply(cons, function(c) c$f(z), numeric(1))
    if (any(fv >= 0.0)) return(Inf)
    t * f0$f(z) + log_barrier(fv)
  }

  cur <- objective(x)
  if (is.infinite(cur) && cur > 0)
    stop(paste0("barerp: the starting point is not strictly feasible"))

  decrement <- Inf
  iters <- 0L
  for (iters in seq_len(as.integer(max_iter))) {
    fv <- vapply(cons, function(c) c$f(x), numeric(1))
    jac <- do.call(rbind, lapply(cons, function(c) c$g(x)))
    g0 <- f0$g(x)
    gphi <- log_barrier_gradient(fv, jac)
    grad <- t * g0 + gphi

    if (centering == "newton") {
      if (any(!vapply(cons, function(c) c$affine, logical(1)))) {
        hs <- lapply(cons, function(c) {
          if (c$affine) return(NULL)
          c$H(x)
        })
      } else {
        hs <- NULL
      }
      hmat <- log_barrier_hessian(fv, jac, hs)
      h0 <- f0$H(x)
      hmat <- hmat + t * h0
      step <- tryCatch(.solve_kkt(hmat, grad, aeq),
                       error = function(e) NULL)
      if (is.null(step)) break
      decrement <- -sum(grad * step)
      if (decrement / 2.0 <= tol) break
    } else {
      step <- -grad
      if (!is.null(aeq) && length(aeq) > 0L) {
        step <- .project_null(step, aeq)
      }
      decrement <- sum(grad * grad)
      if (decrement <= tol) break
    }

    s <- as.numeric(step0)
    gts <- sum(grad * step)
    accepted <- FALSE
    for (k in seq_len(80L)) {
      trial <- x + s * step
      val <- objective(trial)
      if (val <= cur + alpha * s * gts) { accepted <- TRUE; break }
      s <- s * beta
    }
    if (!accepted) break
    x <- x + s * step
    cur <- val
  }

  list(x = x, iters = iters, decrement = decrement)
}

#' Phase I
#'
#' Boyd section 11.4: minimise \eqn{s} subject to
#' \eqn{f_i(x) <= s}. The original problem is strictly feasible
#' exactly when the optimal \eqn{s} is negative.
#'
#' @param cons List of constraints.
#' @param x0 Starting point.
#' @param aeq Optional equality-constraint matrix.
#' @param beq Optional equality right-hand side.
#' @param max_outer Maximum outer iterations for the inner barrier
#'   method.
#' @param ... Forwarded to \code{barrier_method}.
#' @return List with \code{x}, \code{s}, \code{feasible}, \code{outer},
#'   \code{newton}.
#' @export
phase1 <- function(cons, x0, aeq = NULL, beq = NULL,
                    max_outer = 60L, ...) {
  cons <- lapply(cons, .as_fun)
  x0 <- as.numeric(x0)
  n <- length(x0)
  fv <- vapply(cons, function(c) c$f(x0), numeric(1))
  s0 <- max(fv) + 1.0

  lift <- function(c) {
    .Fun(function(z, cc = c) cc$f(z[seq_len(n)]) - z[n + 1L],
         function(z, cc = c) c(cc$g(z[seq_len(n)]), -1.0),
         NULL, affine = c$affine)
  }
  lifted <- lapply(cons, lift)
  obj <- .Fun(function(z) z[n + 1L],
              function(z) c(rep(0.0, n), 1.0),
              NULL, affine = TRUE)
  aeq2 <- NULL
  if (!is.null(aeq) && length(aeq) > 0L) {
    aeq <- as.matrix(aeq)
    aeq2 <- cbind(aeq, rep(0.0, nrow(aeq)))
  }

  res <- barrier_method(obj, lifted, c(x0, s0),
                        aeq = aeq2, beq = beq, max_outer = max_outer, ...)
  z <- res$x
  s <- z[n + 1L]
  list(x = z[seq_len(n)], s = s, feasible = (s < 0.0),
       outer = res$outer, newton = res$newton)
}

#' Barrier method
#'
#' Boyd Algorithm 11.1, the logarithmic barrier method of Frisch
#' (1956) equation 5.1. The central point carries its own certificate
#' (Boyd eq. 11.10-11.12), so the returned \code{gap = m/t} is an
#' exact duality gap, not an estimate.
#'
#' @param f0 Objective: a function, a \code{Fun}, or a list with
#'   \code{f}, \code{grad}, \code{hess}, \code{affine}.
#' @param constraints List of inequality constraints in the same
#'   form.
#' @param x0 Strictly feasible starting point; use \code{phase1} to
#'   find one.
#' @param t0 Initial centering parameter, positive.
#' @param mu Growth factor, > 1.
#' @param eps Duality-gap tolerance, positive.
#' @param aeq Optional equality-constraint matrix.
#' @param beq Optional equality right-hand side.
#' @param centering \code{"newton"} (default), \code{"gradient"}, or
#'   \code{"none"}.
#' @param tol Newton (or gradient) decrement tolerance.
#' @param max_inner Maximum centering iterations.
#' @param max_outer Maximum outer iterations.
#' @param grad Optional gradient override (forwarded to \code{_as_fun}
#'   when \code{f0} is a function).
#' @param hess Optional Hessian override.
#' @param affine Optional affine flag override.
#' @return A named list mirroring the Python RichResult payload:
#'   \code{x}, \code{fun}, \code{gap}, \code{t}, \code{lambda_},
#'   \code{slack}, \code{outer}, \code{newton}, \code{decrement},
#'   \code{history}, \code{centering}, \code{converged}, plus
#'   \code{steps_predicted}, \code{method}, \code{note} on the
#'   multi-step path.
#' @references Frisch, R. (1956). La resolution des problemes de
#'   programme lineaire par la methode du potentiel logarithmique.
#'   Cahiers du Seminaire d'Econometrie, 4, 7-23. Boyd, S. and
#'   Vandenberghe, L. (2004). Convex Optimization, ch. 11.
#' @export
barrier_method <- function(f0, constraints, x0,
                           t0 = 1.0, mu = 10.0, eps = 1e-8,
                           aeq = NULL, beq = NULL,
                           centering = "newton", tol = 1e-10,
                           max_inner = 200L, max_outer = 200L,
                           grad = NULL, hess = NULL, affine = FALSE) {
  .CENTERING <- c("newton", "gradient", "none")
  if (!(centering %in% .CENTERING))
    stop(paste0("barerp: centering must be one of ",
                paste(.CENTERING, collapse = ", ")))
  if (mu <= 1.0) stop("barerp: mu must exceed 1")
  if (t0 <= 0.0 || eps <= 0.0) stop("barerp: t0 and eps must be positive")

  if (is.function(f0) && !is.null(grad))
    f0 <- list(f = f0, grad = grad, hess = hess, affine = affine)
  f0 <- .as_fun(f0)
  cons <- lapply(constraints, .as_fun)
  m <- length(cons)
  if (m == 0L) stop(paste0("barerp: no inequality constraints; this is ",
                           "an unconstrained problem"))
  x <- as.numeric(x0)
  if (any(vapply(cons, function(c) c$f(x) >= 0.0, logical(1))))
    stop(paste0("barerp: x0 is not strictly feasible; use ",
                "phase1() to find a starting point"))
  if (!is.null(aeq) && length(aeq) > 0L) {
    aeq <- apply(as.matrix(aeq), 2, as.numeric)
    if (!is.null(beq)) {
      for (r in seq_len(nrow(aeq))) {
        lhs <- sum(aeq[r, ] * x)
        if (abs(lhs - as.numeric(beq[r])) > 1e-8)
          stop(sprintf("barerp: x0 violates equality row %d by %g",
                       r - 1L, lhs - as.numeric(beq[r])))
      }
    }
  } else {
    aeq <- NULL
  }

  if (centering == "none") {
    t <- m / eps
    cp <- central_point(f0, cons, x, t, aeq, "newton", tol, max_inner)
    x <- cp$x; it <- cp$iters; dec <- cp$decrement
    fv <- vapply(cons, function(c) c$f(x), numeric(1))
    return(list(
      x = x, fun = f0$f(x), gap = m / t, t = t,
      lambda_ = central_path_dual(fv, t), slack = -fv,
      outer = 1L, newton = it, decrement = dec,
      history = list(c(t, m / t, f0$f(x), it)),
      centering = "none", converged = TRUE,
      method = paste0("single centering at t = m/eps, Boyd sec. 11.3 ",
                      "opening -- 'rarely, if ever, used'")))
  }

  t <- as.numeric(t0)
  total <- 0L
  history <- list()
  outer <- 0L
  converged <- FALSE
  for (outer in seq_len(as.integer(max_outer))) {
    cp <- central_point(f0, cons, x, t, aeq, centering, tol, max_inner)
    x <- cp$x; it <- cp$iters; dec <- cp$decrement
    total <- total + it
    history[[length(history) + 1L]] <- c(t, m / t, f0$f(x), it)
    if (m / t < eps) { converged <- TRUE; break }
    t <- t * mu
  }

  fv <- vapply(cons, function(c) c$f(x), numeric(1))
  list(
    x = x,
    fun = f0$f(x),
    gap = m / t,
    t = t,
    lambda_ = central_path_dual(fv, t),
    slack = -fv,
    outer = outer,
    newton = total,
    decrement = dec,
    history = history,
    centering = centering,
    converged = converged,
    steps_predicted = centering_steps(m, eps, as.numeric(t0), as.numeric(mu)),
    method = paste0("Boyd Algorithm 11.1, the logarithmic barrier ",
                    "method of Frisch (1956) eq. 5.1"),
    note = paste0("gap is m/t, the exact duality gap certified by the ",
                  "central point's dual pair (Boyd eq. 11.10-11.12), ",
                  "not an estimate"))
}

#' Barrier method on a linear program
#'
#' Minimise \eqn{c^T x} subject to \eqn{A_ub x <= b_ub} and optionally
#' \eqn{A_eq x = b_eq}. With no \code{x0} a strictly feasible point
#' is found by \code{phase1} first.
#'
#' @param c Cost vector.
#' @param A_ub Inequality matrix.
#' @param b_ub Inequality right-hand side.
#' @param A_eq Optional equality matrix.
#' @param b_eq Optional equality right-hand side.
#' @param x0 Optional strictly feasible starting point.
#' @param ... Forwarded to \code{barrier_method}.
#' @return A list, see \code{barrier_method}.
#' @export
barrier_lp <- function(c, A_ub, b_ub, A_eq = NULL, b_eq = NULL,
                       x0 = NULL, ...) {
  c <- as.numeric(c)
  n <- length(c)
  rows <- apply(as.matrix(A_ub), 2, as.numeric)
  b <- as.numeric(b_ub)
  if (nrow(rows) != length(b))
    stop(sprintf("barerp: A_ub has %d rows but b_ub has %d",
                 nrow(rows), length(b)))
  for (i in seq_len(nrow(rows))) {
    if (length(rows[i, ]) != n)
      stop("barerp: A_ub row width does not match c")
  }

  obj <- list(f = function(z) sum(c * z),
              grad = function(z) c,
              affine = TRUE)
  make_con <- function(row, bi) {
    list(f = function(z, r = row, bb = bi) sum(r * z) - bb,
         grad = function(z, r = row) r,
         affine = TRUE)
  }
  cons <- vector("list", nrow(rows))
  for (i in seq_len(nrow(rows))) cons[[i]] <- make_con(rows[i, ], b[i])

  if (is.null(x0)) {
    ph <- phase1(cons, rep(0.0, n), aeq = A_eq, beq = b_eq)
    if (!isTRUE(ph$feasible))
      stop(sprintf(paste0("barerp: no strictly feasible point found; ",
                          "phase1 stopped at s = %g"), ph$s))
    x0 <- ph$x
  }
  barrier_method(obj, cons, x0, aeq = A_eq, beq = b_eq, ...)
}

# Aliases, mirroring the Python module's __all__.
barerp <- barrier_method
barriermethod <- barrier_method

#' One-line cheatsheet
#'
#' @return Character string summarising the method.
#' @export
.barerp_cheatsheet <- function() {
  paste0("barerp: the logarithmic barrier method. Frisch (1956) ",
         "eq. 5.1 defines the potential as the sum of the logs of ",
         "all the variables -- slacks included -- and moves along a ",
         "compromise between the preference gradient and the ",
         "potential gradient, staying inside the admissible region. ",
         "Boyd ch. 11 is the same path: minimise t f0 + phi with ",
         "phi = -sum log(-f_i), for t growing by mu each outer ",
         "iteration (Algorithm 11.1). The central point carries its ",
         "own certificate -- lambda_i = -1/(t f_i) is dual feasible ",
         "and the duality gap is exactly m/t -- so m/t < eps is a ",
         "guarantee. centering='newton' (default), 'gradient' ",
         "(Frisch's own), or 'none' (single shot at t = m/eps). ",
         "phase1() finds a strictly feasible start.")
}

# Dispatcher: the TASK.md main entry point. Same signature and return
# shape as the Python reference's barerp/barrier_method.
#' @export
morie_barerp <- function(f0, constraints, x0,
                         t0 = 1.0, mu = 10.0, eps = 1e-8,
                         aeq = NULL, beq = NULL,
                         centering = "newton", tol = 1e-10,
                         max_inner = 200L, max_outer = 200L,
                         grad = NULL, hess = NULL, affine = FALSE) {
  barrier_method(f0, constraints, x0,
                 t0 = t0, mu = mu, eps = eps,
                 aeq = aeq, beq = beq,
                 centering = centering, tol = tol,
                 max_inner = max_inner, max_outer = max_outer,
                 grad = grad, hess = hess, affine = affine)
}
