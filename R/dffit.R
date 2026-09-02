# SPDX-License-Identifier: AGPL-3.0-or-later

# Hat values, residuals and (X'X)^-1 for the OLS fit of y on X.  Returns
# a list with n, p, e, h, sse and inv (inv[[k]] is COLUMN k of the
# inverse), shared by the DFFITS, DFBETAS and COVRATIO diagnostics.
#' Hat values, residuals and (X\'X)^-1 for the OLS fit of y on X.
#' Returns
#'
#' a list with n, p, e, h, sse and inv (inv[\[k\]] is COLUMN k of the
#' inverse), shared by the DFFITS, DFBETAS and COVRATIO diagnostics.
#'
#' @param y A vector; its length is taken.
#' @param X Passed to \code{.s03mat}.
#' @param intercept A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{n}, \code{p}, \code{e}, \code{h}, \code{sse}, \code{inv}, \code{D}.
#' @export
.bkw_influence <- function(y, X, intercept = TRUE) {
  y <- .s03vec(y)
  Xm <- .s03mat(X)
  n <- length(y)
  if (n == 0L) stop("empty input: y has no observations")
  if (nrow(Xm) != n) stop("y and X must have the same number of rows")
  D <- if (intercept) cbind(rep(1, n), Xm) else Xm
  D <- matrix(as.numeric(D), n)
  p <- ncol(D)
  if (n <= p + 1L) stop("need n > p + 1 observations for deletion diagnostics")
  XtX <- .s03crossprod(D)
  inv <- lapply(seq_len(p), function(k) {
    e <- numeric(p)
    e[k] <- 1
    .s03cholsolve(XtX, e)
  })
  beta <- .s03cholsolve(XtX, .s03matvec(t(D), y))
  fit <- .s03matvec(D, beta)
  e <- y - fit
  h <- numeric(n)
  for (i in seq_len(n)) {
    s <- 0
    for (a in seq_len(p)) for (b in seq_len(p))
      s <- s + D[i, a] * inv[[b]][a] * D[i, b]
    h[i] <- s
  }
  list(n = n, p = p, e = e, h = h, sse = sum(e * e), inv = inv, D = D)
}

# s_(i): the residual sd with observation i removed.
#' S_(i): the residual sd with observation i removed
#'
#' A step of the dffit implementation. Called by \code{Covrat}, \code{Dfbetb}, \code{Dffit}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param sse Numeric; combined arithmetically in the body.
#' @param e_i Numeric; combined arithmetically in the body.
#' @param h_i Numeric; combined arithmetically in the body.
#' @param n Numeric; combined arithmetically in the body.
#' @param p Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.bkw_sdel <- function(sse, e_i, h_i, n, p) {
  num <- sse - e_i * e_i / (1 - h_i)
  sqrt(max(num, 0) / (n - p - 1))
}

#' DFFITS scaled change in fitted value when obs i deleted
#'
#' Formula: DFFITS_i = (e_i / (s_(i) sqrt(1 - h_ii))) sqrt(h_ii / (1 - h_ii))
#'
#' The cut-off is the Belsley-Kuh-Welsch size-adjusted 2 sqrt(p/n).
#'
#' @param y Response vector, length n.
#' @param X Design matrix with n rows.  An intercept column is prepended
#'   unless \code{intercept} is FALSE.
#' @param intercept Whether to prepend a column of ones.
#' @return List with \code{estimate} (max |DFFITS|), \code{dffits},
#'   \code{threshold}, \code{flagged}, \code{n_influential}, \code{n},
#'   \code{p}, \code{method}.
#' @references Belsley, Kuh & Welsch (1980), Regression Diagnostics,
#'   Wiley, ch. 2.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Dffit(V, V)
Dffit <- function(y, X, intercept = TRUE) {
  f <- .bkw_influence(y, X, intercept)
  n <- f$n
  p <- f$p
  out <- numeric(n)
  for (i in seq_len(n)) {
    si <- .bkw_sdel(f$sse, f$e[i], f$h[i], n, p)
    out[i] <- if (si <= 0 || f$h[i] >= 1) NaN else
      f$e[i] * sqrt(f$h[i]) / (si * (1 - f$h[i]))
  }
  thr <- 2 * sqrt(p / n)
  flagged <- as.integer(!is.nan(out) & abs(out) > thr)
  fin <- abs(out[!is.nan(out)])
  .t1_result(estimate = if (length(fin)) max(fin) else NaN,
             dffits = out, threshold = thr, flagged = flagged,
             n_influential = sum(flagged), n = n, p = p,
             method = "DFFITS scaled change in fitted value when obs i deleted")
}
