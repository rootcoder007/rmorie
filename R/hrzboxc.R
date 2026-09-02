# SPDX-License-Identifier: AGPL-3.0-or-later
#' Box-Cox regression by the minimum-distance estimator of Foster et al.
#'
#' Horowitz, J. L. (2009), Semiparametric and Nonparametric Methods in
#' Econometrics, Springer, Section 6.1.2, page 195 (volume \[Pages 189-232\],
#' read as a rendered page image).  The model is (6.2) with T the Box-Cox
#' transformation (6.3), page 190, T(y, a) = (y^a - 1)/a for a nonzero and
#' log y at a = 0, and F_U left unrestricted.  For any candidate a the slope
#' solves the ordinary least-squares problem printed at the top of p. 195,
#' b_n(a) = (sum_i X_i X_i')^-1 sum_i X_i T(Y_i, a); the residuals
#' U_hat_i = T(Y_i, a) - X_i b_n(a) give the empirical CDF
#' F_n\[u; a, b_n(a)\] = n^-1 sum_i I(U_hat_i < u); and since
#' P(Y < y) = E F_U\[T(y, a) - X beta\], alpha is estimated by minimising
#' R_n\[a, b_n(a)\] = n^-1 sum_i integral over u of
#' {I(Y_i < u) - F_n\[T(u, a) - X_i b_n(a)\]}^2 w(u) du.
#'
#' BOOK NOTE.  The displayed criterion on p. 195 prints the inner argument as
#' T(y, a); y is not bound anywhere in the expression while u is the variable
#' of integration, and the identity P(Y < y) = E F_U\[T(y, a) - X beta\] two
#' lines above fixes the reading: it is T(u, a).  That is what is implemented.
#'
#' The weight w is, as the book requires, positive, deterministic and bounded:
#' the uniform density on (0, max Y].  The outer integral is a trapezoid rule
#' on a fixed grid and the search over a is a fixed grid followed by
#' golden-section refinement, so nothing here is random.  Foster, Tian and Wei
#' (2001) is the source the book credits for the estimator and Theorem 6.1.
#'
#' @param x n-by-p design matrix; the first column should be the intercept.
#' @param y Strictly positive outcomes.
#' @param a_lo,a_hi,ngrid,refine The deterministic search over alpha.
#' @param nu Points in the trapezoid rule for the integral over u.
#' @return list: estimate, lambda_hat, beta_hat, criterion, resid, n, method.
#' @keywords internal
#' @examples
#' xx <- cbind(1, seq(-1, 1, length.out = 25))
#' Hrzboxc(xx, (0.5 * (3 + 1.5 * xx[, 2]) + 1)^2)$lambda_hat
#' @export
Hrzboxc <- function(x, y, a_lo = -2, a_hi = 2, ngrid = 81L, refine = 60L,
                    nu = 201L) {
  GR <- 0.6180339887498949
  bc <- function(v, a) if (a == 0) log(v) else (v^a - 1) / a
  XX <- .s03mat(x)
  yv <- .s03vec(y)
  n <- length(yv)
  if (n == 0L) stop("horowitz_box_cox: y is empty")
  if (nrow(XX) != n) stop("horowitz_box_cox: x has a different number of rows than y")
  if (any(yv <= 0)) {
    stop("horowitz_box_cox: the Box-Cox transformation of (6.3) needs positive Y")
  }
  p <- ncol(XX)
  m <- as.integer(nu)
  if (m < 3L) stop("horowitz_box_cox: nu must be at least 3")
  umax <- max(yv)
  ug <- umax * (seq_len(m)) / m
  du <- umax / m
  wt <- 1 / umax
  fit <- function(a) {
    Ty <- bc(yv, a)
    b <- .s03lstsq(XX, Ty, 1e-12)
    uh <- numeric(n)
    for (i in seq_len(n)) {
      r <- Ty[i]
      for (k in seq_len(p)) r <- r - XX[i, k] * b[k]
      uh[i] <- r
    }
    list(Ty = Ty, b = b, uh = uh)
  }
  crit <- function(a) {
    fi <- fit(a)
    b <- fi$b
    us <- sort(fi$uh)
    tot <- 0
    for (k in seq_len(m)) {
      u <- ug[k]
      tu <- bc(u, a)
      for (i in seq_len(n)) {
        xb <- 0
        for (j in seq_len(p)) xb <- xb + XX[i, j] * b[j]
        z <- tu - xb
        lo <- 0L
        hi <- n
        while (lo < hi) {
          mid <- (lo + hi) %/% 2L
          if (us[mid + 1L] < z) lo <- mid + 1L else hi <- mid
        }
        fn <- lo / n
        d <- (if (yv[i] < u) 1 else 0) - fn
        tot <- tot + d * d * wt * du
      }
    }
    list(v = tot / n, b = b)
  }
  lo <- as.numeric(a_lo)
  hi <- as.numeric(a_hi)
  g <- as.integer(ngrid)
  if (g < 3L || hi <= lo) stop("horowitz_box_cox: need a_lo < a_hi and ngrid >= 3")
  best <- NA_real_
  bi <- 0L
  for (i in seq_len(g) - 1L) {
    a <- lo + (hi - lo) * i / (g - 1L)
    v <- crit(a)$v
    if (is.na(best) || v < best) {
      best <- v
      bi <- i
    }
  }
  step <- (hi - lo) / (g - 1L)
  left <- lo + (hi - lo) * max(bi - 1L, 0L) / (g - 1L)
  right <- lo + (hi - lo) * min(bi + 1L, g - 1L) / (g - 1L)
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
  ff <- fit(a_hat)
  list(estimate = a_hat, lambda_hat = a_hat, beta_hat = ff$b,
       criterion = fin$v, resid = ff$uh, n = n,
       method = paste0("Horowitz (2009) Sec. 6.1.2 p.195 minimum distance ",
                       "(Foster, Tian and Wei 2001)"))
}

# canonical full-name alias (Py<->R API parity)
#' @rdname Hrzboxc
#' @keywords internal
#' @export
morie_horowitz_box_cox <- Hrzboxc
