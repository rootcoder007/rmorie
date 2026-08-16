# SPDX-License-Identifier: AGPL-3.0-or-later
# tail3 batch shared helpers -- internal, not exported.
# Mirrors src/morie/fn/t3util.py arithmetic exactly.

.t3invphi <- 0.6180339887498949

#' t3golden
#'
#' Part of the t3util implementation; see the file header for the source
#' it follows.
#'
#' @param f See Usage.
#' @param lo See Usage.
#' @param hi See Usage.
#' @param iters Defaults to \code{80L}.
#' @return A numeric value.
#' @export
t3golden <- function(f, lo, hi, iters = 80L) {
  a <- as.numeric(lo); b <- as.numeric(hi)
  cc <- b - .t3invphi * (b - a)
  dd <- a + .t3invphi * (b - a)
  fc <- f(cc); fd <- f(dd)
  for (i in seq_len(as.integer(iters))) {
    if (fc < fd) {
      b <- dd; dd <- cc; fd <- fc
      cc <- b - .t3invphi * (b - a); fc <- f(cc)
    } else {
      a <- cc; cc <- dd; fc <- fd
      dd <- a + .t3invphi * (b - a); fd <- f(dd)
    }
  }
  0.5 * (a + b)
}

#' t3nodes
#'
#' Part of the t3util implementation; see the file header for the source
#' it follows.
#'
#' @param m Defaults to \code{401L}.
#' @param lim Defaults to \code{8}.
#' @return A list with \code{u}, \code{w}.
#' @export
t3nodes <- function(m = 401L, lim = 8.0) {
  m <- as.integer(m)
  u <- seq(-lim, lim, length.out = m)
  w <- exp(-0.5 * u * u)
  w <- w / sum(w)
  list(u = u, w = w)
}

#' t3bfs
#'
#' Part of the t3util implementation; see the file header for the source
#' it follows.
#'
#' @param A See Usage.
#' @param s See Usage.
#' @return The value of \code{d}, as built in the body.
#' @export
t3bfs <- function(A, s) {
  A <- as.matrix(A); n <- nrow(A)
  d <- rep(Inf, n); d[s] <- 0
  frontier <- s
  while (length(frontier) > 0) {
    nxt <- integer(0)
    for (i in frontier) for (j in seq_len(n)) {
      if (A[i, j] != 0 && is.infinite(d[j])) { d[j] <- d[i] + 1; nxt <- c(nxt, j) }
    }
    frontier <- nxt
  }
  d
}

#' t3relu
#'
#' Part of the t3util implementation; see the file header for the source
#' it follows.
#'
#' @param x See Usage.
#' @return The value of \code{ifelse}.
#' @export
t3relu <- function(x) ifelse(x > 0, x, 0)

#' t3expit
#'
#' Part of the t3util implementation; see the file header for the source
#' it follows.
#'
#' @param x See Usage.
#' @return A numeric value.
#' @export
t3expit <- function(x) 1 / (1 + exp(-x))

#' t3ols
#'
#' Part of the t3util implementation; see the file header for the source
#' it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
t3ols <- function(X, y) {
  X <- as.matrix(X); y <- as.numeric(y)
  xtx <- t(X) %*% X; xty <- t(X) %*% y
  b <- try(solve(xtx, xty), silent = TRUE)
  if (inherits(b, "try-error")) b <- .morie_pinv(xtx) %*% xty
  as.numeric(b)
}

