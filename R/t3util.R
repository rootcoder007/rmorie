# SPDX-License-Identifier: AGPL-3.0-or-later
# tail3 batch shared helpers -- internal, not exported.
# Mirrors src/morie/fn/t3util.py arithmetic exactly.

.t3invphi <- 0.6180339887498949

#' t3golden
#'
#' A step of the t3util implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param f Accepted by the signature and not used anywhere in the body.
#' @param lo Coerced to numeric by the body, with \code{as.numeric}.
#' @param hi Coerced to numeric by the body, with \code{as.numeric}.
#' @param iters Coerced to integer by the body, with \code{as.integer}. Defaults to \code{80L}.
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
#' A step of the t3util implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param m Coerced to integer by the body, with \code{as.integer}. Defaults to \code{401L}.
#' @param lim Numeric; combined arithmetically in the body. Defaults to \code{8}.
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
#' A step of the t3util implementation. Called by \code{ecccen}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param A A matrix; indexed by row and column.
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
#' A step of the t3util implementation. Called by \code{sage}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param x See Usage.
#' @return The value of \code{ifelse}.
#' @export
t3relu <- function(x) ifelse(x > 0, x, 0)

#' t3expit
#'
#' A step of the t3util implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
t3expit <- function(x) 1 / (1 + exp(-x))

#' t3ols
#'
#' A step of the t3util implementation. Called by \code{giss}, \code{qsarh}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param X A matrix; passed to \code{t}.
#' @param y A matrix; passed to \code{\%*\%}.
#' @return A vector, from \code{as.numeric}.
#' @export
t3ols <- function(X, y) {
  X <- as.matrix(X); y <- as.numeric(y)
  xtx <- t(X) %*% X; xty <- t(X) %*% y
  b <- try(solve(xtx, xty), silent = TRUE)
  if (inherits(b, "try-error")) b <- .morie_pinv(xtx) %*% xty
  as.numeric(b)
}

