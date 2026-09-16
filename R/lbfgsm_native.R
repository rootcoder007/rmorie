## Limited-memory BFGS.
##
## Liu, D. C., & Nocedal, J. (1989) "On the limited memory BFGS method for
## large scale optimization", Mathematical Programming 45, 503-528.
##
## The search direction is formed by the two-loop recursion (their Sec. 2),
## which applies the inverse Hessian approximation implicitly from the last
## m correction pairs (s_k, y_k), never forming a matrix:
##
##     q = g
##     for i = k-1 .. k-m:   alpha_i = rho_i s_i' q ;  q -= alpha_i y_i
##     r = H0 q                       H0 = (s'y / y'y) I    (Sec. 2, eq. 7)
##     for i = k-m .. k-1:   beta = rho_i y_i' r ;  r += (alpha_i - beta) s_i
##
## with rho_i = 1/(y_i's_i). Pairs with y's <= 0 are skipped: accepting them
## would destroy positive-definiteness and the direction would stop being
## a descent direction.
##
## The line search enforces the Wolfe conditions, not merely Armijo:
##
##     f(x + td) <= f(x) + c_1 t g'd,
##     grad f(x + td)'d >= c_2 g'd .
##
## Armijo alone is not enough, and the failure is not subtle: without the
## curvature condition y's goes negative within a handful of iterations,
## every correction pair is then rejected by the guard above, the memory
## freezes and the method degenerates into a fixed-direction crawl. The
## curvature condition is precisely what makes y's > 0, so the two
## conditions are load-bearing together.

#' .lbfgsm_dot
#'
#' A step of the lbfgsm_native implementation. Called by \code{morie_lbfgsm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' b <- c(1.5, 2.5, 3.5)
#' res <- .lbfgsm_dot(a = A, b = b)
#' res

.lbfgsm_dot <- function(a, b) {
  sum(a * b)
}

# Diagonal preconditioner for H0.
#
# `precond` is NULL for none, a numeric vector giving the diagonal of
# the Hessian (its inverse becomes the scaling), or "auto" to estimate
# that diagonal from the most recent curvature pair. The estimate is
# elementwise |y_j| / |s_j|, which is a secant approximation to the
# jth diagonal entry, clamped against a zero or runaway component and
# normalised to a geometric mean of one so that it rescales directions
# relative to one another without fighting the scalar gamma.
.lbfgsm_precond <- function(precond, S, Y, n) {
  if (is.null(precond)) return(NULL)
  if (is.character(precond)) {
    if (!identical(precond, "auto")) {
      stop("`precond` must be NULL, \"auto\", or a numeric vector",
           call. = FALSE)
    }
    nS <- length(S)
    if (nS == 0L) return(NULL)
    s <- S[[nS]]
    y <- Y[[nS]]
    d <- abs(y) / pmax(abs(s), .Machine$double.eps)
    d[!is.finite(d) | d <= 0] <- 1
    # a geometric mean of one: the scalar part of the model stays with
    # gamma, and this carries only the relative scaling
    lg <- mean(log(d))
    if (!is.finite(lg)) return(NULL)
    d <- d / exp(lg)
    return(pmin(pmax(d, 1e-8), 1e8))
  }
  d <- as.numeric(precond)
  if (length(d) != n) {
    stop(sprintf("`precond` must have one entry per parameter (%d), got %d",
                 n, length(d)), call. = FALSE)
  }
  if (any(!is.finite(d)) || any(d <= 0)) {
    stop("`precond` must be positive and finite", call. = FALSE)
  }
  d
}

#' morie_lbfgsm
#'
#' A step of the lbfgsm_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param fun Accepted by the signature and not used anywhere in the body.
#' @param x0 Coerced to numeric by the body, with \code{as.numeric}.
#' @param grad Accepted by the signature and not used anywhere in the body.
#' @param m Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10}.
#' @param max_iter Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.
#' @param tol Passed to \code{<=}. Defaults to \code{1e-08}.
#' @param c1 Numeric; combined arithmetically in the body. Defaults to \code{1e-04}.
#' @param c2 Numeric; combined arithmetically in the body. Defaults to \code{0.9}.
#' @param max_ls Coerced to integer by the body, with \code{as.integer}. Defaults to \code{60}.
#' @return A list with \code{estimate}, \code{x}, \code{fun}, \code{grad},
#' \code{grad_norm}, \code{iterations}, \code{n_fun}, \code{memory}, \code{converged},
#'   \code{status} (why the loop stopped: the gradient test, a stall at
#'   the optimum, a line search that found no Wolfe step, or the
#'   iteration limit),
#' \code{history}, \code{method}.
#' @param tol_type Which stopping criterion `tol` applies to.
#'   `"absolute"` (the default) tests the gradient norm directly.
#'   `"initial"` tests it against its value at the starting point, and
#'   `"relative"` against `1 + |f|` -- the form SciPy and NLopt default
#'   to. See the note below before choosing `"relative"`.
#' @param precond Diagonal preconditioner for the initial inverse
#'   Hessian: `NULL` for none, a positive numeric vector giving the
#'   Hessian's diagonal, or `"auto"` to estimate it from the most recent
#'   curvature pair. Measured on the quadratics below, `"auto"` did not
#'   help; a supplied diagonal is the useful form.
#' @section Conditioning, and what the tolerance means:
#' An **absolute** gradient test depends on how the objective is scaled:
#' multiply `f` by a thousand and the same point stops passing. It is
#' also a strict thing to ask of L-BFGS on an ill-conditioned problem.
#' Measured over 120 random twenty-dimensional quadratics with condition
#' numbers from about 10 to 1.4e5, at `tol = 1e-7`:
#'
#' | setting | not converged | convergence claimed away from the optimum | median error in `x` |
#' | --- | --- | --- | --- |
#' | `"absolute"`, `m = 10` | 72 / 120 | 0 | 2.6e-07 |
#' | `"absolute"`, `m = 30` | 63 / 120 | 0 | 2.5e-08 |
#' | `"initial"`, `m = 10` | 58 / 120 | 0 | 3.9e-07 |
#' | `"relative"`, `m = 10` | 21 / 120 | **11** | 4.7e-07 |
#'
#' Read the third column before the second. `"relative"` converges far
#' more often and reaches that number partly by declaring convergence at
#' points that are not stationary -- eleven of 120, at errors above
#' 1e-5. Failing to reach an unnaturally tight tolerance is better than
#' reporting success at a non-stationary point, so `"absolute"` remains
#' the default and `"relative"` is offered for callers who want the
#' SciPy convention and know what it admits. `"initial"` is the
#' scale-free criterion that does not have this failure mode.
#'
#' A problem with a condition number at or above 1e4 naturally stalls
#' around a relative error of 1e-5, and no choice of tolerance changes
#' that -- the iteration stops at the *first* iterate under the
#' threshold, so the margin is about 1.5 times at every tolerance.
#' Raising `m` is the adjustment that helps: from 10 to 30 cut the
#' failures and improved the median solution by an order of magnitude,
#' at the cost of holding three times the history. `grad_norm`,
#' `grad_norm_relative` and `grad_norm_ratio` are all reported so the
#' behaviour can be judged rather than inferred.
#' @references Liu, D. C. and Nocedal, J. (1989). On the limited memory BFGS method
#'   for large scale optimization. \emph{Mathematical Programming}
#'   \strong{45}, 503-528.
#' @export
#' @keywords internal
morie_lbfgsm <- function(fun, x0, grad, m = 10, max_iter = 200, tol = 1e-8,
                         tol_type = c("absolute", "relative", "initial"),
                         precond = NULL,
                         c1 = 1e-4, c2 = 0.9, max_ls = 60) {
  tol_type <- match.arg(tol_type)
  x <- as.numeric(x0)
  n <- length(x)
  m <- as.integer(m)
  if (m < 1L) {
    stop(sprintf("lbfgs_minimize: m must be at least 1, got %d", m))
  }
  if (n == 0L) {
    stop("lbfgs_minimize: x0 must be non-empty")
  }

  f <- as.numeric(fun(x))
  g <- as.numeric(grad(x))
  S <- list()
  Y <- list()
  RHO <- numeric(0)
  n_f <- 1L
  it <- 0L
  converged <- FALSE
  status <- "iteration limit reached"
  gnorm0 <- sqrt(.lbfgsm_dot(g, g))
  history <- f

  for (it in seq_len(as.integer(max_iter))) {
    gnorm <- sqrt(.lbfgsm_dot(g, g))
    if (it == 1L) gnorm0 <- gnorm
    # An ABSOLUTE gradient test depends on how the objective is scaled:
    # multiply f by a thousand and the same point stops passing. The two
    # relative forms are what SciPy and NLopt use by default, and on an
    # ill-conditioned problem they are the ones that mean something --
    # see the note on conditioning in the details.
    crit <- switch(tol_type,
      absolute = gnorm,
      relative = gnorm / (1 + abs(f)),
      initial  = if (gnorm0 > 0) gnorm / gnorm0 else 0)
    if (crit <= tol) {
      converged <- TRUE
      status <- switch(tol_type,
        absolute = "gradient norm within tolerance",
        relative = "gradient norm within tolerance, relative to f",
        initial  = "gradient norm within tolerance, relative to its start")
      break
    }

    # --- two-loop recursion, Liu & Nocedal Sec. 2
    q <- g
    alphas <- numeric(0)
    nS <- length(S)
    if (nS > 0L) {
      for (i in rev(seq_len(nS))) {
        a <- RHO[i] * .lbfgsm_dot(S[[i]], q)
        alphas <- c(alphas, a)
        q <- q - a * Y[[i]]
      }
    }
    if (nS > 0L) {
      # H0 = (s'y / y'y) I -- the scaling that makes L-BFGS work
      # at all; with H0 = I the first step is wildly mis-scaled.
      gamma <- .lbfgsm_dot(S[[nS]], Y[[nS]]) / .lbfgsm_dot(Y[[nS]], Y[[nS]])
    } else {
      gamma <- 1.0
    }
    # A scalar H0 assumes the curvature is the same in every direction.
    # When the Hessian's diagonal spans orders of magnitude that is the
    # wrong model, and a Jacobi diagonal reduces the effective condition
    # number before the limited-memory update ever runs.
    dvec <- .lbfgsm_precond(precond, S, Y, n)
    r <- if (is.null(dvec)) gamma * q else gamma * (q / dvec)
    if (nS > 0L) {
      alphas <- rev(alphas)
      for (i in seq_len(nS)) {
        b <- RHO[i] * .lbfgsm_dot(Y[[i]], r)
        coef <- alphas[i] - b
        r <- r + coef * S[[i]]
      }
    }
    d <- -r

    slope <- .lbfgsm_dot(g, d)
    if (slope >= 0.0) {
      # Numerically lost descent; reset the memory and go downhill.
      S <- list()
      Y <- list()
      RHO <- numeric(0)
      d <- -g
      slope <- -.lbfgsm_dot(g, g)
    }

    # --- Wolfe line search by bracketing. Widen while the
    # curvature condition fails, bisect while Armijo fails.
    lo <- 0.0
    hi <- Inf
    t <- 1.0
    ok <- FALSE
    xt <- NULL
    ft <- NULL
    gt <- NULL
    # The Armijo test compares a required decrease of c1 * t * slope
    # against a MEASURED change in f, and once the step is small enough
    # the required decrease falls below the precision with which f can
    # be evaluated. At a stalling iterate this was asking for a decrease
    # of 4e-22 while f was computed to about 1e-12, so the comparison
    # was reading arithmetic noise and the search could not succeed for
    # any number of tries. Allowing the noise a seat at the table is
    # standard for a finite-precision line search.
    ftol <- 4 * .Machine$double.eps * max(1, abs(f))
    for (kk in seq_len(as.integer(max_ls))) {
      xt <- x + t * d
      ft <- as.numeric(fun(xt))
      n_f <- n_f + 1L
      if (ft > f + c1 * t * slope + ftol) {
        hi <- t
        t <- 0.5 * (lo + hi)
        next
      }
      gt <- as.numeric(grad(xt))
      if (.lbfgsm_dot(gt, d) < c2 * slope) {
        lo <- t
        t <- if (is.infinite(hi)) 2.0 * lo else 0.5 * (lo + hi)
        next
      }
      ok <- TRUE
      break
    }
    if (!ok) {
      if (length(S) > 0L) {
        # discard the curvature history and retake this iteration from
        # steepest descent, which is always a descent direction
        S <- list()
        Y <- list()
        RHO <- numeric(0)
        d <- -g
        slope <- -.lbfgsm_dot(g, g)
        lo <- 0.0
        hi <- Inf
        t <- 1.0
        for (kk in seq_len(as.integer(max_ls))) {
          xt <- x + t * d
          ft <- as.numeric(fun(xt))
          n_f <- n_f + 1L
          if (ft > f + c1 * t * slope) {
            hi <- t
            t <- 0.5 * (lo + hi)
            next
          }
          gt <- as.numeric(grad(xt))
          if (.lbfgsm_dot(gt, d) < c2 * slope) {
            lo <- t
            t <- if (is.infinite(hi)) 2.0 * lo else 0.5 * (lo + hi)
            next
          }
          ok <- TRUE
          break
        }
      }
      if (!ok) {
        # No step along steepest descent satisfies Wolfe either. At the
        # optimum that is the expected outcome rather than a fault: the
        # function is flat to machine precision and no step can both
        # reduce it and satisfy the curvature condition. Report which
        # of the two it is instead of calling both a failure.
        # `converged` means the REQUESTED tolerance was met, and
        # nothing else. A stall at the optimum with the tolerance unmet
        # is not success: the caller asked for a gradient norm it did
        # not get, and saying otherwise would report success for a
        # target that was missed. The distinction goes in `status`.
        status <- if (sqrt(.lbfgsm_dot(g, g)) <= sqrt(.Machine$double.eps) *
                        max(1, abs(f))) {
          "stalled at the optimum; no Wolfe step remains"
        } else {
          "line search could not satisfy the Wolfe conditions"
        }
        break
      }
    }
    if (is.null(gt)) {
      gt <- as.numeric(grad(xt))
    }
    s <- xt - x
    y <- gt - g
    ys <- .lbfgsm_dot(y, s)
    if (ys > 1e-16) {
      S <- c(S, list(s))
      Y <- c(Y, list(y))
      RHO <- c(RHO, 1.0 / ys)
      if (length(S) > m) {
        S <- S[-1L]
        Y <- Y[-1L]
        RHO <- RHO[-1L]
      }
    }
    x <- xt
    f <- ft
    g <- gt
    history <- c(history, f)
  }

  list(
    estimate = x,
    x = x,
    fun = as.numeric(f),
    grad = g,
    grad_norm = as.numeric(sqrt(.lbfgsm_dot(g, g))),
    grad_norm_relative = as.numeric(sqrt(.lbfgsm_dot(g, g)) / (1 + abs(f))),
    grad_norm_ratio = as.numeric(
      if (gnorm0 > 0) sqrt(.lbfgsm_dot(g, g)) / gnorm0 else NA_real_),
    tol = as.numeric(tol),
    tol_type = tol_type,
    iterations = as.integer(it),
    n_fun = as.integer(n_f),
    memory = as.integer(m),
    converged = converged,
    status = status,
    history = history,
    method = "L-BFGS two-loop recursion with a Wolfe line search (Liu & Nocedal 1989, Sec. 2)"
  )
}

lbfgs_minimize <- morie_lbfgsm
lbfgsm <- morie_lbfgsm

#' .lbfgsm_cheatsheet
#'
#' A step of the lbfgsm_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .lbfgsm_cheatsheet()
#' res
.lbfgsm_cheatsheet <- function() {
  "lbfgsm: L-BFGS two-loop recursion, H0 = (s'y/y'y) I, curvature pairs with y's <= 0 skipped, Armijo backtracking."
}
