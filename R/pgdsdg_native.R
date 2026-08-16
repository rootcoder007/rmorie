# morie.fn -- function file (rootcoder007/morie)
# Projected gradient descent, and the projections that make it work.
#
# **The method.** To minimise f over a closed convex set C, take a
# gradient step and put the result back:
#   x_{k+1} = P_C(x_k - t_k * grad f(x_k))
# Goldstein's observation is that this is not a heuristic repair. The
# projection P_C onto a closed convex set is non-expansive,
# ||P_C u - P_C v|| <= ||u - v||, so it cannot undo the contraction the
# gradient step achieves. For f convex with L-Lipschitz gradient and
# t <= 1/L, the iterates converge at rate O(1/k).
#
# **The fixed point is the optimality condition.** x* solves the
# constrained problem exactly when x* = P_C(x* - t * grad f(x*)) for
# any t > 0, which is the projected form of the KKT conditions, and
# is what the anchor checks rather than merely watching the objective
# stop moving.
#
# **Two accelerations, and one honest limitation.**
# backtracking searches for a step satisfying the descent inequality,
# which removes the need to know L. fista adds Nesterov momentum with
# the Beck-Teboulle t_{k+1} = (1 + sqrt(1 + 4 t_k^2))/2 sequence,
# improving the rate to O(1/k^2). It is not monotone: the objective
# can rise on individual iterations, and an implementation that stops
# on "objective increased" will stop early. The objective history is
# returned so this is visible rather than surprising.
#
# The limitation: all of this assumes f convex. On a non-convex f the
# method still converges to a stationary point of the constrained
# problem, and nothing here says it is a minimum.
#
# **Projections included.** Box, non-negative orthant, Euclidean ball,
# and the probability simplex. The simplex projection is the
# interesting one: it is not clipping-and-renormalising, which is a
# different and wrong operation. The correct projection solves for a
# threshold theta with sum_i max(x_i - theta, 0) = 1, and the anchor
# checks it against a brute-force minimisation rather than against the
# formula it was implemented from.
#
# References
# Goldstein, A. A. (1964) "Convex programming in Hilbert space",
#   Bulletin of the American Mathematical Society 70(5), 709-710,
#   doi:10.1090/S0002-9904-1964-11178-2.
# Levitin, E. S. & Polyak, B. T. (1966) "Constrained minimization
#   methods", USSR Computational Mathematics and Mathematical Physics
#   6(5), 1-50, doi:10.1016/0041-5553(66)90114-5.
# Beck, A. & Teboulle, M. (2009) "A fast iterative shrinkage-
#   thresholding algorithm for linear inverse problems", SIAM Journal
#   on Imaging Sciences 2(1), 183-202, doi:10.1137/080716542.

STEP_RULES <- c("fixed", "backtracking", "fista")

.pgdsdg_norm <- function(v) {
  sqrt(sum(v * v))
}

#' project_box
#'
#' Part of the pgdsdg_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param lower Defaults to \code{NULL}.
#' @param upper Defaults to \code{NULL}.
#' @return The value of \code{pmin}.
#' @export
project_box <- function(x, lower = NULL, upper = NULL) {
  v <- as.numeric(x)
  n <- length(v)
  lo <- if (is.null(lower)) {
    rep(-Inf, n)
  } else {
    sapply(seq_along(lower), function(i) {
      t <- lower[[i]]
      if (is.null(t)) -Inf else as.numeric(t)
    })
  }
  up <- if (is.null(upper)) {
    rep(Inf, n)
  } else {
    sapply(seq_along(upper), function(i) {
      t <- upper[[i]]
      if (is.null(t)) Inf else as.numeric(t)
    })
  }
  if (length(lo) != n || length(up) != n) {
    stop(sprintf("pgdsdg: the bounds must have one entry per coordinate (%d)", n))
  }
  if (any(lo > up)) {
    stop("pgdsdg: a lower bound exceeds its upper bound, so the box is empty")
  }
  pmin(pmax(v, lo), up)
}

#' project_nonneg
#'
#' Part of the pgdsdg_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @return The value of \code{pmax}.
#' @export
project_nonneg <- function(x) {
  pmax(0.0, as.numeric(x))
}

#' project_ball
#'
#' Part of the pgdsdg_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param radius Defaults to \code{1}.
#' @param centre Defaults to \code{NULL}.
#' @return A numeric value.
#' @export
project_ball <- function(x, radius = 1.0, centre = NULL) {
  r <- as.numeric(radius)
  if (r <= 0) {
    stop("pgdsdg: the ball radius must be positive")
  }
  v <- as.numeric(x)
  c <- if (is.null(centre)) rep(0.0, length(v)) else as.numeric(centre)
  if (length(c) != length(v)) {
    stop(sprintf("pgdsdg: the centre has %d coordinates but the point has %d",
                 length(c), length(v)))
  }
  d <- v - c
  nrm <- sqrt(sum(d * d))
  if (nrm <= r) {
    return(v)
  }
  c + d * r / nrm
}

.pgdsdg_project_simplex <- function(x, total = 1.0) {
  s <- as.numeric(total)
  if (s <= 0) {
    stop("pgdsdg: the simplex total must be positive")
  }
  v <- as.numeric(x)
  n <- length(v)
  if (n == 0) {
    stop("pgdsdg: cannot project an empty vector")
  }
  u <- sort(v, decreasing = TRUE)
  css <- 0.0
  rho <- 0
  theta <- u[1] - s
  for (i in seq_len(n)) {
    css <- css + u[i]
    t_val <- (css - s) / i
    if (u[i] - t_val > 0) {
      rho <- i
      theta <- t_val
    }
  }
  pmax(v - theta, 0.0)
}

#' projected_gradient
#'
#' Part of the pgdsdg_native implementation; see the file header for the
#' source it follows.
#'
#' @param f See Usage.
#' @param grad See Usage.
#' @param x0 See Usage.
#' @param project See Usage.
#' @param step Defaults to \code{NULL}.
#' @param rule Defaults to \code{"backtracking"}.
#' @param max_iter Defaults to \code{2000}.
#' @param tol Defaults to \code{1e-10}.
#' @return A list with \code{estimate}, \code{x}, \code{fun}, \code{iterations}, \code{history}, \code{step}, \code{rule}, \code{n_backtracks}, \code{fixed_point_residual}, \code{converged}, \code{monotone}, \code{method}.
#' @export
projected_gradient <- function(f, grad, x0, project, step = NULL,
                                rule = "backtracking", max_iter = 2000,
                                tol = 1e-10) {
  if (!(rule %in% STEP_RULES)) {
    stop(sprintf("pgdsdg: rule must be one of %s, got %s",
                 paste(STEP_RULES, collapse = ", "),
                 rule))
  }
  x <- project(as.numeric(x0))
  n <- length(x)
  t <- if (is.null(step)) 1.0 else as.numeric(step)
  if (t <= 0) {
    stop("pgdsdg: the step size must be positive")
  }
  if (rule == "fixed" && is.null(step)) {
    stop("pgdsdg: a fixed step rule needs an explicit step size")
  }
  hist <- as.numeric(f(x))
  y <- as.numeric(x)
  tk <- 1.0
  n_back <- 0
  it <- 0
  for (it in seq_len(max_iter)) {
    base <- if (rule == "fista") y else x
    g <- as.numeric(grad(base))
    if (length(g) != n) {
      stop(sprintf("pgdsdg: the gradient has %d components but the point has %d",
                   length(g), n))
    }
    if (rule == "fixed") {
      new <- project(base - t * g)
    } else {
      fb <- as.numeric(f(base))
      backtrack_failed <- TRUE
      cand <- NULL
      for (j in seq_len(60)) {
        cand <- project(base - t * g)
        d <- cand - base
        q <- fb + sum(g * d) + sum(d * d) / (2.0 * t)
        if (as.numeric(f(cand)) <= q + 1e-12) {
          backtrack_failed <- FALSE
          break
        }
        t <- t * 0.5
        n_back <- n_back + 1
      }
      if (backtrack_failed) {
        stop("pgdsdg: backtracking failed to find a step satisfying the descent inequality; is the gradient correct?")
      }
      new <- cand
    }
    if (rule == "fista") {
      tn <- 0.5 * (1.0 + sqrt(1.0 + 4.0 * tk * tk))
      y <- new + ((tk - 1.0) / tn) * (new - x)
      tk <- tn
    }
    move <- .pgdsdg_norm(new - x)
    x <- new
    hist <- c(hist, as.numeric(f(x)))
    if (move < tol) {
      break
    }
  }
  g <- as.numeric(grad(x))
  fixed <- project(x - 1.0 * g)
  resid <- .pgdsdg_norm(fixed - x)
  monotone <- all(hist[-length(hist)] >= hist[-1] - 1e-12)
  list(
    estimate = x,
    x = x,
    fun = as.numeric(f(x)),
    iterations = it,
    history = hist,
    step = t,
    rule = rule,
    n_backtracks = n_back,
    fixed_point_residual = resid,
    converged = resid < 1e-6,
    monotone = monotone,
    method = paste0("projected gradient (Goldstein 1964; Levitin & Polyak 1966)",
                    if (rule == "fista") " with Beck-Teboulle momentum" else "")
  )
}

morie_pgdsdg <- projected_gradient_descent <- function(f, grad, x0, project, ...) {
  projected_gradient(f, grad, x0, project, ...)
}

#' @rdname project_box
#' @export
morie_pgdsdg <- project_box

















