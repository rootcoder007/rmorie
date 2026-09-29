.soc_barrier <- function(x, cones) {
  n <- length(x)
  val <- 0
  g <- numeric(n)
  H <- matrix(0, n, n)
  for (cn in cones) {
    u <- sum(cn$e * x) + cn$d
    w <- as.vector(cn$A %*% x) + cn$b
    q <- u^2 - sum(w^2)
    if (u <= 0 || q <= 0) return(NULL)
    val <- val - log(q)
    dq <- 2 * u * cn$e - 2 * as.vector(crossprod(cn$A, w))
    g <- g - dq / q
    H <- H + outer(dq, dq) / q^2 - (2 * outer(cn$e, cn$e) - 2 * crossprod(cn$A)) / q
  }
  list(val = val, g = g, H = H)
}

.soc_center <- function(f, x, cones, t, max_newton = 100) {
  for (it in seq_len(max_newton)) {
    bv <- .soc_barrier(x, cones)
    val <- t * sum(f * x) + bv$val
    g <- t * f + bv$g
    H <- bv$H
    diag(H) <- diag(H) + 1e-14 * pmax(1, abs(diag(H)))
    dx <- -solve(H, g)
    lam2 <- -sum(g * dx)
    if (lam2 / 2 <= 1e-12) break
    s <- 1
    repeat {
      xn <- x + s * dx
      bn <- .soc_barrier(xn, cones)
      if (!is.null(bn) && t * sum(f * xn) + bn$val <= val - 0.25 * s * lam2) break
      s <- s / 2
      if (s < 1e-20) return(x)
    }
    x <- xn
  }
  x
}

#' Second-order cone programming by the barrier method
#'
#' R arm of \code{morie.fn.socpts}: minimise \eqn{c'x} subject to
#' \eqn{||A_i x + b_i||_2 \le e_i'x + d_i} for each cone, the affine bound
#' \code{(e_i, d_i)} given in \code{domains}. Generalised-logarithm barriers,
#' damped Newton centering with backtracking, \code{t} multiplied by 10 until
#' \code{m / t < eps}; phase I \eqn{\min s} with the bounds relaxed by
#' \eqn{s} finds a strictly feasible start.
#'
#' @param c Objective.
#' @param A List of cone matrices.
#' @param b List of cone offsets.
#' @param domains List of \code{list(e, d)} affine right-hand sides.
#' @param x0 Optional phase I start.
#' @param eps Duality-gap tolerance.
#' @return List with \code{x}, \code{objective}, \code{lhs}, \code{rhs},
#'   \code{slack}, \code{feasible}.
#' @references Boyd, S. and Vandenberghe, L. (2004). Convex Optimization.
#'   Cambridge University Press.
#'
#'   Lobo, M. S., Vandenberghe, L., Boyd, S. and Lebret, H. (1998).
#'   Applications of second-order cone programming. Linear Algebra and its
#'   Applications 284, 193-228.
#' @examples
#' Socpts(c(1, 0), list(diag(2)), list(c(0, 0)), list(list(c(0, 0), 1)))$x
#' @export
Socpts <- function(c, A, b, domains, x0 = NULL, eps = 1e-10) {
  f <- as.numeric(c)
  n <- length(f)
  if (!(length(A) == length(b) && length(b) == length(domains))) stop("A, b and domains must have the same length")
  cones <- lapply(seq_along(A), function(i) {
    Am <- matrix(as.numeric(as.matrix(A[[i]])), ncol = n)
    list(A = Am, b = as.numeric(b[[i]]), e = as.numeric(domains[[i]][[1]]), d = as.numeric(domains[[i]][[2]]))
  })
  m <- length(cones)
  x <- if (is.null(x0)) numeric(n) else as.numeric(x0)
  if (is.null(.soc_barrier(x, cones))) {
    viol <- max(vapply(cones, function(cn) sqrt(sum((as.vector(cn$A %*% x) + cn$b)^2)) - (sum(cn$e * x) + cn$d), 0))
    ph <- lapply(cones, function(cn) list(A = cbind(cn$A, 0), b = cn$b, e = c(cn$e, 1), d = cn$d))
    z <- c(x, max(viol, 0) + 1)
    fz <- c(numeric(n), 1)
    t <- 1
    repeat {
      z <- .soc_center(fz, z, ph, t)
      if (z[n + 1] < -1e-8 || m / t < 1e-12) break
      t <- t * 10
    }
    if (z[n + 1] >= 0) return(list(x = NULL, objective = NULL, feasible = FALSE))
    x <- z[seq_len(n)]
  }
  t <- 1
  repeat {
    x <- .soc_center(f, x, cones, t)
    if (m / t < eps) break
    t <- t * 10
  }
  lhs <- vapply(cones, function(cn) sqrt(sum((as.vector(cn$A %*% x) + cn$b)^2)), 0)
  rhs <- vapply(cones, function(cn) sum(cn$e * x) + cn$d, 0)
  list(x = x, objective = sum(f * x), lhs = lhs, rhs = rhs, slack = rhs - lhs, feasible = TRUE, estimate = sum(f * x))
}
