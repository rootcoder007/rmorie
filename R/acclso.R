# SPDX-License-Identifier: AGPL-3.0-or-later
#' FISTA for the LASSO.
#'
#' Minimises F(b) = 0.5 ||X b - y||^2 + lam ||b||_1 by the accelerated
#' proximal-gradient scheme of Beck and Teboulle (2009, Sect. 4):
#' x_k = soft(y_k - grad f(y_k)/L, lam/L),
#' t_{k+1} = (1 + sqrt(1 + 4 t_k^2))/2,
#' y_{k+1} = x_k + ((t_k - 1)/t_{k+1})(x_k - x_{k-1}).
#'
#' @param X Design matrix, one record per row.
#' @param y Response vector of length n.
#' @param lam L1 penalty, non-negative.
#' @param steps Fixed number of FISTA iterations.
#' @param lipschitz Step constant L; NULL uses a fixed 50-step power
#'   iteration on X'X.
#'
#' @return List with beta, objective, rss, l1, lipschitz, steps, nonzero,
#'   n, p.
#' @references Beck, A. and Teboulle, M. (2009), SIAM Journal on Imaging
#'   Sciences 2(1), 183-202, Section 4.  Standard published form of FISTA;
#'   the SIAM article itself is paywalled and was not read.
#' @export
Fistalasso <- function(X, y, lam, steps = 100, lipschitz = NULL) {
  Xm <- .t1_mat(X); y <- .t1_vec(y)
  lam <- as.numeric(lam); steps <- as.integer(steps)
  n <- nrow(Xm); p <- ncol(Xm)
  if (n != length(y)) stop("X must have one row per entry of y")
  if (lam < 0) stop("lam must be non-negative")
  if (steps < 0) stop("steps must be non-negative")
  L <- if (is.null(lipschitz)) .k01_speclip(Xm, p) else as.numeric(lipschitz)
  if (L <= 0) L <- 1
  x <- rep(0, p); yv <- rep(0, p); t <- 1
  for (k in seq_len(steps)) {
    r <- as.numeric(Xm %*% yv) - y
    g <- as.numeric(t(Xm) %*% r)
    xn <- .k01_soft(yv - g / L, lam / L)
    tn <- (1 + sqrt(1 + 4 * t * t)) / 2
    w <- (t - 1) / tn
    yv <- xn + w * (xn - x)
    x <- xn
    t <- tn
  }
  res <- as.numeric(Xm %*% x) - y
  rss <- 0.5 * sum(res^2)
  l1 <- sum(abs(x))
  .t1_result(beta = x, objective = rss + lam * l1, rss = rss, l1 = l1,
             lipschitz = L, steps = steps, nonzero = sum(x != 0),
             n = n, p = p,
             method = "FISTA for the LASSO (Beck-Teboulle 2009 Sect. 4)")
}

#' .k01_soft
#'
#' A step of the acclso implementation. Called by \code{Admmlasso}, \code{Fistalasso}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param v Numeric; passed to \code{abs}.
#' @param t Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.k01_soft <- function(v, t) sign(v) * pmax(abs(v) - t, 0)

#' .k01_speclip
#'
#' A step of the acclso implementation. Called by \code{Agdproj}, \code{Fistalasso}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param Xm A matrix; passed to \code{t}.
#' @param p A count; the body uses it as \code{rep(...)}.
#' @param iters A count; the body uses it as \code{seq_len(...)}. Defaults to \code{50L}.
#' @return The value of \code{lam}, as built in the body.
#' @export
.k01_speclip <- function(Xm, p, iters = 50L) {
  v <- rep(1 / sqrt(p), p)
  lam <- 0
  for (i in seq_len(iters)) {
    w <- as.numeric(t(Xm) %*% (Xm %*% v))
    nw <- sqrt(sum(w^2))
    if (nw == 0) return(0)
    v <- w / nw
    lam <- nw
  }
  lam
}
