# SPDX-License-Identifier: AGPL-3.0-or-later
#' Frank-Wolfe conditional gradient
#'
#' Each step solves a linear problem over the feasible polytope and
#' moves toward its solution, so the iterate stays feasible by
#' construction and no projection is ever needed.  The duality gap
#' <grad, x - s> upper bounds the optimality gap and is reported.
#'
#' Formula: s = argmin_v <grad f(x), v>;
#'   x <- (1 - gamma) x + gamma s with gamma = 2/(t + 2).
#'
#' @param f Objective function of a numeric vector.
#' @param grad_f Gradient function.
#' @param domain Matrix whose rows are the polytope vertices.
#' @param x0 Feasible starting point.
#' @param steps Number of iterations.
#' @return List with \code{estimate}, \code{x}, \code{f_path},
#'   \code{gap}, \code{steps}, \code{n}, \code{method}.
#' @references Frank and Wolfe (1956), An algorithm for quadratic
#'   programming, Naval Research Logistics Quarterly 3(1-2):95-110.
#'   \doi{10.1002/nav.3800030109}
#' @export
#' @examples
#' V <- rbind(c(0, 0), c(1, 0), c(0, 1))
#' Frwol2(function(x) sum((x - 0.3)^2), function(x) 2 * (x - 0.3),
#'        domain = V, x0 = c(0.3, 0.3))
Frwol2 <- function(f, grad_f, domain, x0, steps = 50) {
  V <- .s03mat(domain)
  if (nrow(V) == 0L) stop("frank_wolfe: domain has no vertices")
  d <- ncol(V)
  x <- .s03vec(x0)
  if (length(x) != d) stop("frank_wolfe: x0 and the vertices have different dimensions")
  if (!is.function(f) || !is.function(grad_f)) stop("frank_wolfe: f and grad_f must be callable")
  ns <- as.integer(steps)
  if (ns < 1L) stop("frank_wolfe: steps must be at least 1")
  path <- as.numeric(f(x)); gap <- Inf
  for (t in seq_len(ns) - 1L) {
    g <- .s03vec(grad_f(x))
    if (length(g) != d) stop("frank_wolfe: gradient has the wrong length")
    sc <- as.numeric(V %*% g)
    i <- which.min(sc)
    gap <- sum(g * x) - sc[i]
    gamma <- 2 / (t + 2)
    x <- (1 - gamma) * x + gamma * V[i, ]
    path <- c(path, as.numeric(f(x)))
  }
  .t1_result(estimate = as.numeric(f(x)), x = x, f_path = path, gap = gap,
             steps = ns, n = d,
             method = "s = argmin_v <grad, v>, gamma_t = 2/(t+2), Frank & Wolfe (1956)")
}
