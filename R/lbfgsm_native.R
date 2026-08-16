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
.lbfgsm_dot <- function(a, b) {
  sum(a * b)
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
#' @param tol Defaults to \code{1e-08}.
#' @param c1 Numeric; combined arithmetically in the body. Defaults to \code{1e-04}.
#' @param c2 Numeric; combined arithmetically in the body. Defaults to \code{0.9}.
#' @param max_ls Coerced to integer by the body, with \code{as.integer}. Defaults to \code{60}.
#' @return A list with \code{estimate}, \code{x}, \code{fun}, \code{grad}, \code{grad_norm}, \code{iterations}, \code{n_fun}, \code{memory}, \code{converged}, \code{history}, \code{method}.
#' @export
morie_lbfgsm <- function(fun, x0, grad, m = 10, max_iter = 200, tol = 1e-8,
                         c1 = 1e-4, c2 = 0.9, max_ls = 60) {
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
  history <- f

  for (it in seq_len(as.integer(max_iter))) {
    gnorm <- sqrt(.lbfgsm_dot(g, g))
    if (gnorm <= tol) {
      converged <- TRUE
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
    r <- gamma * q
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
    if (!ok) {
      break
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
    iterations = as.integer(it),
    n_fun = as.integer(n_f),
    memory = as.integer(m),
    converged = converged,
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
.lbfgsm_cheatsheet <- function() {
  "lbfgsm: L-BFGS two-loop recursion, H0 = (s'y/y'y) I, curvature pairs with y's <= 0 skipped, Armijo backtracking."
}
