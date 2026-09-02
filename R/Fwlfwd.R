# SPDX-License-Identifier: AGPL-3.0-or-later
#' Fully corrective Frank-Wolfe
#'
#' Holloway replaces the fixed step-size rule with a full
#' re-optimisation over the convex hull of every vertex found so far.
#' The inner minimisation is a cycle of golden-section line searches
#' between the current point and each active vertex, repeated a fixed
#' number of rounds so both language arms take identical steps.
#'
#' Formula: S <- S + argmin_v <grad f(x), v>;
#'   x <- argmin\{ f(x) : x in conv(S) \}.
#'
#' @param f Objective function of a numeric vector.
#' @param grad_f Gradient function.
#' @param domain Matrix whose rows are the polytope vertices.
#' @param x0 Feasible starting point.
#' @param steps Number of vertex additions.
#' @param rounds Line-search sweeps over the active set per step.
#' @return List with \code{estimate}, \code{x}, \code{f_path},
#'   \code{gap}, \code{n_active}, \code{n}, \code{method}.
#' @references Holloway (1974), An extension of the Frank and Wolfe
#'   method of feasible directions, Mathematical Programming 6(1):14-27.
#'   \doi{10.1007/BF01580219}
#' @export
#' @examples
#' V <- rbind(c(0, 0), c(1, 0), c(0, 1))
#' Fwlfwd(function(x) sum((x - 0.3)^2), function(x) 2 * (x - 0.3),
#'        domain = V, x0 = c(0.3, 0.3))
Fwlfwd <- function(f, grad_f, domain, x0, steps = 10, rounds = 3) {
  V <- .s03mat(domain)
  if (nrow(V) == 0L) stop("fully_corrective_fw: domain has no vertices")
  d <- ncol(V)
  x <- .s03vec(x0)
  if (length(x) != d) stop("fully_corrective_fw: x0 and the vertices have different dimensions")
  if (!is.function(f) || !is.function(grad_f)) stop("fully_corrective_fw: f and grad_f must be callable")
  ns <- as.integer(steps)
  if (ns < 1L) stop("fully_corrective_fw: steps must be at least 1")
  invphi <- (sqrt(5) - 1) / 2
  lsearch <- function(x, v) {
    lo <- 0
    hi <- 1
    pt <- function(g) (1 - g) * x + g * v
    cc <- hi - invphi * (hi - lo)
    dd <- lo + invphi * (hi - lo)
    fc <- f(pt(cc))
    fd <- f(pt(dd))
    for (i in seq_len(60)) {
      if (fc < fd) {
        hi <- dd
        dd <- cc
        fd <- fc
        cc <- hi - invphi * (hi - lo)
        fc <- f(pt(cc))
      } else {
        lo <- cc
        cc <- dd
        fc <- fd
        dd <- lo + invphi * (hi - lo)
        fd <- f(pt(dd))
      }
    }
    pt((lo + hi) / 2)
  }
  active <- integer(0)
  path <- as.numeric(f(x))
  gap <- Inf
  for (s in seq_len(ns)) {
    g <- .s03vec(grad_f(x))
    sc <- as.numeric(V %*% g)
    i <- which.min(sc)
    gap <- sum(g * x) - sc[i]
    if (!(i %in% active)) active <- c(active, i)
    for (rr in seq_len(as.integer(rounds))) for (a in active) x <- lsearch(x, V[a, ])
    path <- c(path, as.numeric(f(x)))
  }
  .t1_result(estimate = as.numeric(f(x)), x = x, f_path = path, gap = gap,
             n_active = length(active), n = d,
             method = "vertex addition plus re-optimisation over conv(S), Holloway (1974)")
}
