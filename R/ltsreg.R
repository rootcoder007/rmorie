# SPDX-License-Identifier: AGPL-3.0-or-later
#' Least trimmed squares regression
#'
#' Rousseeuw, P. J. (1984), "Least Median of Squares Regression", Journal of
#' the American Statistical Association 79(388), 871-880, Section 4 "Related
#' approaches", p. 876, equation (4.1), read from a rendered page image because
#' the PDF is a scan with no text layer:
#'
#'   minimize_theta sum_\{i=1\}^\{h\} (r^2)_\{i:n\},
#'
#' "where (r^2)_\{1:n\} <= ... <= (r^2)_\{n:n\} are the ordered squared residuals.
#' If h = \[n/2\] + 1 is chosen, the breakdown point of Theorem 1 is obtained,
#' and for h = \[n/2\] + \[(p+1)/2\], the result of Remark 1 holds.  In general, h
#' may depend on some trimming proportion alpha, for instance by means of
#' h = \[n(1 - alpha)\] + 1."
#'
#' The default here is the maximal-breakdown choice of Remark 1,
#' h = \[n/2\] + \[(p+1)/2\], whose breakdown point (p. 873) is
#' (\[(n - p)/2\] + 1)/n.  Both breakdown formulas are reported.  The same page
#' states that the LTS converges like n^-1/2, unlike the LMS at n^-1/3, which
#' is why Section 4 introduces it.
#'
#' ALGORITHM.  Concentration, the direct analogue of the MCD C-step: from a
#' trial fit, refit by ordinary least squares on the h observations with the
#' smallest squared residuals.  The objective cannot increase, so the iteration
#' terminates; that monotonicity is asserted as an anchor rather than assumed.
#'
#' DETERMINISM.  Starts are elemental p-subsets enumerated in lexicographic
#' order, not drawn at random.
#'
#' At h = n nothing is trimmed and the estimator is exactly ordinary least
#' squares, which is the module's closed-form anchor.
#'
#' @param y n responses.
#' @param X n-by-p design matrix; include the intercept column yourself.
#' @param h number of retained residuals; defaults to \[n/2\] + \[(p+1)/2\].
#' @param max_starts cap on the elemental subsets enumerated.
#' @param max_iter cap on the concentration steps per start.
#' @return list: estimate, coef, subset, residual, objectives,
#'   breakdown_remark1, breakdown_theorem1, h, n, p, method.
#' @keywords internal
#' @examples
#' Ltsreg(c(1, 2, 3, 4, 50), cbind(1, c(1, 2, 3, 4, 5)))$coef
#' @export
Ltsreg <- function(y, X, h = NULL, max_starts = 200000, max_iter = 100L) {
  yy <- .s03vec(y)
  Xm <- .s03mat(X)
  n <- length(yy)
  if (n == 0L) stop("least_trimmed_squares: y is empty")
  if (nrow(Xm) != n) stop("least_trimmed_squares: X must have one row per response")
  p <- ncol(Xm)
  if (p == 0L) stop("least_trimmed_squares: X has no columns")
  hh <- if (is.null(h)) .rstrimmedh(n, p) else as.integer(h)
  if (hh < p) stop("least_trimmed_squares: h must be at least p")
  if (hh > n) stop("least_trimmed_squares: h cannot exceed the number of observations")
  total <- .rsnchoosek(n, p)
  if (total > max_starts) stop(sprintf("least_trimmed_squares: %d elemental subsets exceeds max_starts", total))
  bobj <- NULL
  bth <- NULL
  bidx <- NULL
  bchain <- numeric(0)
  for (J in .rscombos(n, p)) {
    A <- Xm[J, , drop = FALSE]
    b <- yy[J]
    th <- .rslusolve(A, b)
    if (is.null(th)) next
    chain <- numeric(0)
    idx <- NULL
    for (q in seq_len(as.integer(max_iter))) {
      o <- .rsltsobj(Xm, yy, th, n, p, hh)
      idx <- o$idx
      chain <- c(chain, o$tot)
      nth <- .rsltsfit(Xm, yy, idx, p)
      if (is.null(nth)) break
      no <- .rsltsobj(Xm, yy, nth, n, p, hh)
      if (identical(no$idx, idx)) {
        th <- nth
        chain <- c(chain, no$tot)
        idx <- no$idx
        break
      }
      th <- nth
      idx <- no$idx
    }
    o <- .rsltsobj(Xm, yy, th, n, p, hh)
    if (is.null(bobj) || o$tot < bobj) { bobj <- o$tot
    bth <- th
    bidx <- o$idx
    bchain <- chain }
  }
  if (is.null(bobj)) stop("least_trimmed_squares: every elemental subset was singular")
  res <- numeric(n)
  for (i in seq_len(n)) {
    s <- yy[i]
    for (j in seq_len(p)) s <- s - bth[j] * Xm[i, j]
    res[i] <- s
  }
  list(estimate = bobj, coef = bth, subset = as.numeric(bidx - 1L), residual = res,
       objectives = bchain,
       breakdown_remark1 = ((n - p) %/% 2L + 1L) / n,
       breakdown_theorem1 = (n %/% 2L - p + 2L) / n,
       h = hh, n = n, p = p,
       method = "Rousseeuw (1984) eq. (4.1) min sum of h smallest squared residuals, concentration from lexicographic elemental starts")
}
