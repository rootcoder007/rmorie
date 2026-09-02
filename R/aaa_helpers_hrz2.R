# SPDX-License-Identifier: AGPL-3.0-or-later
# Internal helpers for the Horowitz (2009) semiparametric shelf.
# Kept in their own file so both R trees hold a byte-identical copy.

# Internal helper: deterministic coordinate search
# @noRd
# A FIXED schedule: niter sweeps, each trying offsets
# (-steps ... +steps) * delta on every coordinate and keeping the best,
# with delta multiplied by shrink after each sweep.  There is NO
# tolerance-based early exit and no random restart, so the same inputs
# give the same answer in every language this is mirrored into -- which
# is the whole point.
#' Internal helper: deterministic coordinate search
#' @param fun Accepted by the signature and not used anywhere in the body.
#' @param x0 Coerced to numeric by the body, with \code{as.numeric}.
#' @param niter See Usage. Defaults to \code{12L}.
#' @param delta See Usage. Defaults to \code{1}.
#' @param shrink See Usage. Defaults to \code{0.5}.
#' @param steps See Usage. Defaults to \code{3L}.
#' @noRd
.hrz_coord_min <- function(fun, x0, niter = 12L, delta = 1, shrink = 0.5,
                           steps = 3L) {
  x <- as.numeric(x0)
  best <- as.numeric(fun(x))
  d <- as.numeric(delta)
  for (it in seq_len(niter)) {
    for (j in seq_along(x)) {
      base <- x[j]
      for (k in setdiff(seq.int(-steps, steps), 0L)) {
        x[j] <- base + k * d
        val <- as.numeric(fun(x))
        if (val < best) {
          best <- val
          base <- x[j]
        }
      }
      x[j] <- base
    }
    d <- d * shrink
  }
  list(par = x, value = best)
}

#' Internal helper: standard Gaussian kernel
#' @noRd
.hrz2_gk <- function(u) exp(-0.5 * u * u) / sqrt(2 * pi)

# Internal helper: fixed-iteration IRLS for the check loss
# @noRd
# Fixed iteration count and NO tolerance early exit, so the Python and
# R arms take exactly the same path.
#' Internal helper: fixed-iteration IRLS for the check loss
#' @param X A matrix; passed to \code{ncol}.
#' @param y Numeric; combined arithmetically in the body.
#' @param w Numeric; combined arithmetically in the body.
#' @param tau Numeric; combined arithmetically in the body.
#' @param niter See Usage. Defaults to \code{40L}.
#' @param eps See Usage. Defaults to \code{0.001}.
#' @noRd
.hrz2_qirls <- function(X, y, w, tau, niter = 40L, eps = 1e-3) {
  X <- as.matrix(X)
  p <- ncol(X)
  beta <- rep(0, p)
  for (k in seq_len(niter)) {
    r <- as.numeric(y - X %*% beta)
    # The check-loss weight is DISCONTINUOUS at r = 0.  Where the
    # residual is already inside the eps floor the sign test is decided
    # by machine noise, and 40 iterations amplify that into a visible
    # cross-language difference, so a residual within eps of zero is
    # treated as a tie and given the average weight.
    num <- ifelse(abs(r) < eps, 0.5, ifelse(r > 0, tau, 1 - tau))
    wk <- w * num / pmax(abs(r), eps)
    A <- crossprod(X, X * wk) + diag(1e-10, p)
    b <- crossprod(X, wk * y)
    # Jacobi equilibration.  A series design of powers is badly
    # conditioned, and solving it raw lets two LAPACK paths differ in
    # the last bits -- which 40 IRLS steps then amplify into a visible
    # cross-language gap.  Scaling by the square roots of the diagonal
    # is exact arithmetic and fixes it.
    dg <- sqrt(pmax(diag(A), 1e-300))
    beta <- as.numeric(solve(A / outer(dg, dg), as.numeric(b) / dg) / dg)
  }
  beta
}
