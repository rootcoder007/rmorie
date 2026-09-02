# SPDX-License-Identifier: AGPL-3.0-or-later
#' Transformation model with parametric T and nonparametric F, by GMM
#'
#' Horowitz, J. L. (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Springer, Section 6.1, pages 190-193 (volume
#' \[Pages 189-232\], read as rendered page images).  The model is
#' T(Y, alpha) = X beta + U with U independent of X (6.2), T known up to the
#' finite-dimensional alpha and F_U left unrestricted.  Page 192 explains why
#' nonlinear least squares is inconsistent here -- the derivative of T with
#' respect to alpha is correlated with U, so it is not a valid instrument --
#' and replaces it with a vector of valid instruments W satisfying E(WU) = 0
#' and dim(W) >= dim(beta) + 1.  The estimator minimises
#' G_n(a, b) Omega_n G_n(a, b) (6.8) with
#' G_n(a, b) = n^-1 sum_i W_i \[T(Y_i, a) - X_i b\] (6.7), and the book names
#' Omega_n = (W W)^-1 as one possible choice, which makes (6.8) the nonlinear
#' two-stage least-squares estimator.  That choice is used here, and the
#' instruments are X augmented with the squares of its non-intercept columns,
#' which are the "powers, cross-products, and other nonlinear functions of
#' components of X" the book proposes on p. 192.
#'
#' Two transformation families are offered, both taken from the book:
#' "boxcox" is (6.3), T(y, a) = (y^a - 1)/a for a nonzero and log y at a = 0,
#' and "bickel-doksum" is (6.4), T(y, a) = (|y|^a sgn(y) - 1)/a.  For any
#' fixed a the criterion is a quadratic in b with an explicit solution, so
#' only a one-dimensional search over a remains; it is done on a fixed grid
#' followed by golden-section refinement, so nothing is random.
#'
#' @param x n-by-p design matrix; the first column should be the intercept.
#' @param y Outcomes, positive for the Box-Cox family.
#' @param T_family "boxcox" (6.3) or "bickel-doksum" (6.4).
#' @param a_lo,a_hi,ngrid,refine The deterministic one-dimensional search over
#'   alpha.
#' @return list: estimate, theta_hat, alpha_hat, beta_hat, criterion, resid,
#'   T_family, n, method.
#' @keywords internal
#' @examples
#' set.seed(NULL)
#' xx <- cbind(1, seq(-1, 1, length.out = 30))
#' yy <- (1 + 2 * (xx[, 2] + 3))^(1 / 0.5)
#' Hrztpar(xx, yy)$theta_hat
#' @export
Hrztpar <- function(x, y, T_family = "boxcox", a_lo = -2, a_hi = 2,
                    ngrid = 81L, refine = 80L) {
  GR <- 0.6180339887498949
  XX <- .s03mat(x)
  yv <- .s03vec(y)
  n <- length(yv)
  if (n == 0L) stop("horowitz_parametric_T: y is empty")
  if (nrow(XX) != n) stop("horowitz_parametric_T: x has a different number of rows than y")
  p <- ncol(XX)
  if (p < 2L) stop("horowitz_parametric_T: need an intercept and at least one covariate")
  trans <- function(a) {
    if (identical(T_family, "boxcox")) {
      if (any(yv <= 0)) stop("horowitz_parametric_T: the Box-Cox family of (6.3) needs positive Y")
      if (a == 0) log(yv) else (yv^a - 1) / a
    } else if (identical(T_family, "bickel-doksum")) {
      if (a <= 0) stop("horowitz_parametric_T: the Bickel-Doksum family of (6.4) needs a > 0")
      ((abs(yv)^a) * sign(yv) - 1) / a
    } else {
      stop("horowitz_parametric_T: T_family must be boxcox or bickel-doksum")
    }
  }
  W <- XX
  for (k in seq_len(p - 1L) + 1L) W <- cbind(W, XX[, k] * XX[, k])
  q <- ncol(W)
  WtW <- .s03crossprod(W)
  XtW <- matrix(0, p, q)
  for (i in seq_len(n)) {
    for (a in seq_len(p)) {
      for (b in seq_len(q)) XtW[a, b] <- XtW[a, b] + XX[i, a] * W[i, b]
    }
  }
  solve_b <- function(Ty) {
    WtT <- numeric(q)
    for (i in seq_len(n)) {
      for (b in seq_len(q)) WtT[b] <- WtT[b] + W[i, b] * Ty[i]
    }
    OWX <- matrix(0, p, q)
    for (a in seq_len(p)) OWX[a, ] <- .s03ridgesolve(WtW, XtW[a, ], 1e-12)
    OWT <- .s03ridgesolve(WtW, WtT, 1e-12)
    A <- matrix(0, p, p)
    rhs <- numeric(p)
    for (a in seq_len(p)) {
      for (cc in seq_len(p)) {
        s <- 0
        for (b in seq_len(q)) s <- s + XtW[a, b] * OWX[cc, b]
        A[a, cc] <- s
      }
      s <- 0
      for (b in seq_len(q)) s <- s + XtW[a, b] * OWT[b]
      rhs[a] <- s
    }
    .s03ridgesolve(A, rhs, 1e-12)
  }
  crit <- function(a) {
    Ty <- trans(a)
    b <- solve_b(Ty)
    g <- numeric(q)
    for (i in seq_len(n)) {
      r <- Ty[i]
      for (k in seq_len(p)) r <- r - XX[i, k] * b[k]
      for (cc in seq_len(q)) g[cc] <- g[cc] + W[i, cc] * r
    }
    g <- g / n
    og <- .s03ridgesolve(WtW, g, 1e-12)
    s <- 0
    for (cc in seq_len(q)) s <- s + g[cc] * og[cc]
    list(v = s, b = b)
  }
  lo <- as.numeric(a_lo)
  hi <- as.numeric(a_hi)
  m <- as.integer(ngrid)
  if (m < 3L || hi <= lo) stop("horowitz_parametric_T: need a_lo < a_hi and ngrid >= 3")
  best <- NA_real_
  bi <- 0L
  for (i in seq_len(m) - 1L) {
    a <- lo + (hi - lo) * i / (m - 1L)
    v <- crit(a)$v
    if (is.na(best) || v < best) {
      best <- v
      bi <- i
    }
  }
  step <- (hi - lo) / (m - 1L)
  left <- lo + (hi - lo) * max(bi - 1L, 0L) / (m - 1L)
  right <- lo + (hi - lo) * min(bi + 1L, m - 1L) / (m - 1L)
  if (right - left < step) {
    left <- max(left - step, lo)
    right <- min(right + step, hi)
  }
  c1 <- right - GR * (right - left)
  c2 <- left + GR * (right - left)
  f1 <- crit(c1)$v
  f2 <- crit(c2)$v
  for (it in seq_len(as.integer(refine))) {
    if (f1 < f2) {
      right <- c2
      c2 <- c1
      f2 <- f1
      c1 <- right - GR * (right - left)
      f1 <- crit(c1)$v
    } else {
      left <- c1
      c1 <- c2
      f1 <- f2
      c2 <- left + GR * (right - left)
      f2 <- crit(c2)$v
    }
  }
  a_hat <- 0.5 * (left + right)
  fin <- crit(a_hat)
  b_hat <- fin$b
  Ty <- trans(a_hat)
  resid <- numeric(n)
  for (i in seq_len(n)) {
    r <- Ty[i]
    for (k in seq_len(p)) r <- r - XX[i, k] * b_hat[k]
    resid[i] <- r
  }
  list(estimate = a_hat, theta_hat = a_hat, alpha_hat = a_hat,
       beta_hat = b_hat, criterion = fin$v, resid = resid,
       T_family = T_family, n = n,
       method = "Horowitz (2009) eq. (6.7)-(6.8) NL2SLS with Omega = (W'W)^-1")
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hrztpar
#' @keywords internal
#' @export
morie_horowitz_parametric_T <- Hrztpar
