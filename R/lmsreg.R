# SPDX-License-Identifier: AGPL-3.0-or-later
#' Least median of squares regression
#'
#' Rousseeuw, P. J. (1984), "Least Median of Squares Regression", Journal of
#' the American Statistical Association 79(388), 871-880.  The PDF is a scan
#' with no text layer, so every page cited was read as a rendered image.
#'
#' Equation (1.8), p. 872, defines the estimator: minimize_theta med_i r_i^2.
#'
#' Theorem 1, p. 872: if p > 1 and the observations are in general position,
#' the breakdown point of the LMS method is (\[n/2\] - p + 2)/n, where [r] is the
#' largest integer <= r.  That formula is reported in the payload.
#'
#' Theorem 2, p. 873: for p = 1 with all x_i = 1, so the sample reduces to
#' (y_i), the LMS location is the midpoint of the shortest half, the smallest
#' of y_{h:n} - y_{1:n}, ..., y_{n:n} - y_{n-h+1:n} with h = \[n/2\] + 1.
#' Corollary 1, p. 873: if at least n - \[n/2\] + p - 1 observations satisfy
#' y_i = x_i theta exactly and are in general position, the LMS solution equals
#' theta WHATEVER the other observations are.  Both are used as anchors.
#'
#' Equation (2.2), p. 874: S = 1.483 c(n, p) m_T with m_T^2 = min med r^2 and
#' 1/qnorm(.75) = 1.483.  The paper does NOT give a formula for the
#' finite-sample correction c(n, p): it says work is in progress to determine
#' it empirically, that it exceeds 1, that it converges to 1 as n grows, and it
#' quotes the single value c(20, 6) = 1.8.  So c defaults to 1 here, the
#' paper's own stated large-n limit, and is a parameter rather than invented.
#'
#' ALGORITHM.  The paper's own algorithm for simple regression, p. 874: for
#' each slope a the intercept subproblem m_a^2 = min_b med_i ((y_i - a x_i) - b)^2
#' is solved immediately by the location algorithm, that is by the shortest
#' half of the partial residuals.  This implementation does the same in
#' general: it enumerates the elemental subsets that determine the
#' non-intercept coefficients exactly and places the intercept at the midpoint
#' of the shortest half of the partial residuals.  Subsets are enumerated in
#' lexicographic order, not drawn at random.
#'
#' @param y n responses.
#' @param X n-by-p design matrix; include the column of ones yourself.
#' @param c_np the finite-sample correction of eq. (2.2); defaults to 1.
#' @param max_subsets refuse rather than enumerate more than this many.
#' @return list: estimate, coef, scale, residual, breakdown, intercept_col,
#'   c_np, n, p, n_subsets, method.
#' @keywords internal
#' @examples
#' Lmsreg(c(1, 2, 3, 4, 50), cbind(1, c(1, 2, 3, 4, 5)))$coef
#' @export
Lmsreg <- function(y, X, c_np = 1, max_subsets = 200000) {
  yy <- .s03vec(y)
  Xm <- .s03mat(X)
  n <- length(yy)
  if (n == 0L) stop("least_median_squares: y is empty")
  if (nrow(Xm) != n) stop("least_median_squares: X must have one row per response")
  p <- ncol(Xm)
  if (p == 0L) stop("least_median_squares: X has no columns")
  if (n < p) stop("least_median_squares: need at least p observations")
  total <- .rsnchoosek(n, p)
  if (total > max_subsets) stop(sprintf("least_median_squares: %d elemental subsets exceeds max_subsets", total))
  ic <- .rsintercept(Xm, n, p)
  hloc <- n %/% 2L + 1L
  bobj <- NULL; bth <- NULL; bres <- NULL
  for (J in .rscombos(n, p)) {
    A <- Xm[J, , drop = FALSE]
    b <- yy[J]
    th <- .rslusolve(A, b)
    if (is.null(th)) next
    if (ic >= 1L) {
      part <- numeric(n)
      for (i in seq_len(n)) {
        s <- yy[i]
        for (j in seq_len(p)) if (j != ic) s <- s - th[j] * Xm[i, j]
        part[i] <- s
      }
      sh <- .rsshortesthalf(part, hloc)
      th[ic] <- 0.5 * (sh$sorted[sh$start] + sh$sorted[sh$start + hloc - 1L])
    }
    res <- numeric(n)
    for (i in seq_len(n)) {
      s <- yy[i]
      for (j in seq_len(p)) s <- s - th[j] * Xm[i, j]
      res[i] <- s
    }
    obj <- .rsmedsq(res)
    if (is.null(bobj) || obj < bobj) { bobj <- obj; bth <- th; bres <- res }
  }
  if (is.null(bobj)) stop("least_median_squares: every elemental subset was singular")
  scale <- 1.483 * as.numeric(c_np) * sqrt(bobj)
  breakdown <- (n %/% 2L - p + 2L) / n
  list(estimate = bobj, coef = bth, scale = scale, residual = bres,
       breakdown = breakdown, intercept_col = as.numeric(ic - 1L),
       c_np = as.numeric(c_np), n = n, p = p, n_subsets = total,
       method = "Rousseeuw (1984) eq. (1.8) min med r^2, elemental fits with the p. 874 shortest-half intercept step; scale eq. (2.2)")
}
