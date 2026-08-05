# SPDX-License-Identifier: AGPL-3.0-or-later

#' Empirical Pickands dependence function
#'
#' Formula: A(t) = -log(P(F_X(X) <= u^(1-t), F_Y(Y) <= u^t)) / (-log u)
#'
#' A(t) is read off the empirical copula at the diagonal-shifted point
#' (u^(1-t), u^t).  It satisfies A(0) = A(1) = 1 exactly by
#' construction, lies between max(t, 1-t) and 1, equals 1 under
#' independence and max(t, 1-t) under perfect dependence.
#'
#' @param x First variable.
#' @param y Second variable.
#' @param t_grid Points in [0, 1], or NULL for eleven equally spaced.
#' @param u Copula level in (0, 1), or NULL for exp(-1).
#' @return List with \code{A}, \code{t}, \code{estimate} (A at 1/2),
#'   \code{chi}, \code{convex_ok}, \code{n}, \code{method}.
#' @references Pickands (1981), Bull. Int. Statist. Inst. 49:859-878.
#' @export
Evpdfn <- function(x, y, t_grid = NULL, u = NULL) {
  xs <- .s03vec(x); ys <- .s03vec(y)
  n <- length(xs)
  if (n == 0L) stop("empty input: x has no observations")
  if (length(ys) != n) stop("x and y must have the same length")
  if (is.null(t_grid)) t_grid <- (0:10) / 10 else t_grid <- .s03vec(t_grid)
  if (any(t_grid < 0 | t_grid > 1)) stop("t_grid must lie in [0, 1]")
  if (is.null(u)) u <- exp(-1)
  u <- as.numeric(u)
  if (!(u > 0 && u < 1)) stop("u must lie strictly in (0, 1)")
  ux <- .s03rank(xs) / (n + 1)
  uy <- .s03rank(ys) / (n + 1)
  lu <- -log(u)
  A <- numeric(length(t_grid))
  for (q in seq_along(t_grid)) {
    tt <- t_grid[q]
    a <- u^(1 - tt); b <- u^tt
    cnt <- 0L
    for (i in seq_len(n)) if (ux[i] <= a && uy[i] <= b) cnt <- cnt + 1L
    p <- cnt / n
    if (tt <= 0 || tt >= 1 || p <= 0) {
      A[q] <- 1
    } else {
      v <- -log(p) / lu
      lower <- max(tt, 1 - tt)
      A[q] <- min(max(v, lower), 1)
    }
  }
  half <- NA_real_
  for (q in seq_along(t_grid)) if (abs(t_grid[q] - 0.5) < 1e-12) half <- A[q]
  if (is.na(half)) half <- sum(A) / length(A)
  convex <- 1L
  if (length(A) > 2L) for (q in 2:(length(A) - 1L))
    if (A[q] > 0.5 * (A[q - 1] + A[q + 1]) + 1e-9) convex <- 0L
  .t1_result(A = A, t = t_grid, estimate = half, chi = 2 - 2 * half,
             convex_ok = convex, n = n,
             method = "empirical Pickands dependence function")
}
