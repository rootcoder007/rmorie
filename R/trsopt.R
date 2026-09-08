# SPDX-License-Identifier: AGPL-3.0-or-later
#' Trust-region subproblem
#'
#' Nocedal and Wright (2006), Numerical Optimization, 2nd ed., chapter 4.
#' The subproblem min_s g's + s'Bs/2 subject to ||s|| <= delta is solved,
#' by their theorem 4.1, iff there is lambda >= 0 with (B + lambda I)s* =
#' -g, lambda(delta - ||s*||) = 0, and B + lambda I positive
#' semidefinite.  The book is not open access; the theorem is quoted in
#' its standard published form.  The root-finding uses the secular
#' equation phi(lambda) = 1/delta - 1/||s(lambda)||, the form recommended
#' for its near-linearity.  The eigendecomposition is cyclic Jacobi with
#' sign-fixed vectors, so the hard case is detected rather than stumbled
#' into, and is reported.
#'
#' @param g the gradient.
#' @param H the model Hessian B.
#' @param delta the trust-region radius.
#' @param tol,max_iter root-finding controls.
#' @return list: estimate, s, lam, norm, boundary, hard_case, eigenvalues,
#'   method.
#' @keywords internal
#' @examples
#' Trsub(c(1, 0), matrix(c(2, 0, 0, 1), 2, 2), 0.1)$norm
#' @export
Trsub <- function(g, H, delta = 1, tol = 1e-13, max_iter = 200) {
  gv <- .s03vec(g)
  B <- .s03mat(H)
  n <- length(gv)
  eg <- .s03jacobi(B)
  vals <- eg$values
  vecs <- eg$vectors
  gt <- numeric(n)
  for (t in seq_len(n)) {
    s <- 0
    for (i in seq_len(n)) s <- s + vecs[i, t] * gv[i]
    gt[t] <- s
  }
  lam1 <- vals[1]
  D <- as.numeric(delta)
  snorm <- function(lm) {
    s <- 0
    for (t in seq_len(n)) {
      d <- vals[t] + lm
      if (abs(d) < 1e-300) next
      s <- s + (gt[t] / d)^2
    }
    sqrt(s)
  }
  hard <- FALSE
  if (lam1 > 0 && snorm(0) <= D) {
    lam <- 0
  } else {
    lo <- max(0, -lam1) + 1e-14
    hi <- lo + 1
    while (snorm(hi) > D && hi < 1e14) hi <- hi * 2
    if (snorm(lo) < D) hard <- TRUE
    for (it in seq_len(as.integer(max_iter))) {
      mid <- 0.5 * (lo + hi)
      if (snorm(mid) > D) lo <- mid else hi <- mid
      if (hi - lo < tol * max(1, hi)) break
    }
    lam <- 0.5 * (lo + hi)
  }
  s <- numeric(n)
  for (t in seq_len(n)) {
    d <- vals[t] + lam
    if (abs(d) < 1e-300) next
    cc <- -gt[t] / d
    for (i in seq_len(n)) s[i] <- s[i] + cc * vecs[i, t]
  }
  gs <- 0
  for (i in seq_len(n)) gs <- gs + gv[i] * s[i]
  Bs <- .s03matvec(B, s)
  q <- 0
  for (i in seq_len(n)) q <- q + s[i] * Bs[i]
  nrm <- 0
  for (i in seq_len(n)) nrm <- nrm + s[i] * s[i]
  nrm <- sqrt(nrm)
  list(estimate = -(gs + 0.5 * q), s = s, lam = lam, norm = nrm,
       boundary = abs(nrm - D) < 1e-6 * max(1, D), hard_case = hard,
       eigenvalues = vals,
       method = "Exact trust-region subproblem via the secular equation (Nocedal and Wright 2006, thm. 4.1)")
}
