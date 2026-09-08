# SPDX-License-Identifier: AGPL-3.0-or-later

#' Boundary-free kernel density estimator (Eq. 5.9)
#'
#' Eq. (5.9):
#' \deqn{\tilde f_X(x) = \frac{1}{nhg'(g^{-1}(x))}\sum_i
#' K\Big(\frac{g^{-1}(x)-g^{-1}(X_i)}{h}\Big),\quad x\in\Omega.}{ftilde(x) = 1/(n h
#' g'(g^-1(x))) sum_i K((g^-1(x) - g^-1(X_i))/h), x in Omega.}
#'
#' The `1/g'(g^-1(x))` factor is the Jacobian, and it is exactly what the
#' distribution-function estimator (5.5) does NOT need. A density is a
#' derivative, so it transforms with a Jacobian; a distribution function is a
#' probability, so it does not. That one difference runs through the chapter.
#'
#' Bias and variance are Theorem 5.5: `h^2 c2(x) mu2(K) / (2 g'(g^-1(x)))` and
#' `f(x) int K^2 / (n h g'(g^-1(x)))`. The variance is `O(1/(nh))`, not the
#' `O(h/n)` of the distribution estimators -- so THIS estimator takes the
#' density bandwidth rate `n^(-1/5)`, and the default is Silverman's rule on
#' the transformed scale, not the cube-root rule the rest of the suite uses.
#'
#' @param x Sample, inside the support `Omega`.
#' @param grid Evaluation points; defaults to the sorted sample.
#' @param ginv The inverse transformation; defaults to `log`.
#' @param dg `g'` as a function of `g^-1(x)`; defaults to `exp`, matching the
#'   default `ginv = log`.
#' @param h Bandwidth on the transformed scale; defaults to Silverman's
#'   `n^(-1/5)` rule, because this is a DENSITY estimator.
#' @return Named list with ``estimate``, ``grid``, ``h``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Eq. (5.9), Theorem 5.5.
#' @examples
#' Bfkde(c(0.5, 1, 1.5, 2, 3), grid = 1.5)
#' @export
Bfkde <- function(x, grid = NULL, ginv = NULL, dg = NULL, h = NULL) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L) stop("need at least two observations.")
  if (is.null(ginv)) {
    if (any(x <= 0)) stop("the default g = exp needs data on (0, Inf).")
    ginv <- log
    if (is.null(dg)) dg <- exp
  }
  if (is.null(dg)) stop("supply dg alongside a custom ginv.")
  y <- vapply(x, function(t) as.numeric(ginv(t)), numeric(1))
  if (is.null(h)) {
    sdy <- stats::sd(y)
    if (sdy <= 0) sdy <- 1
    h <- 1.06 * sdy * n^(-0.2)
  }
  if (h <= 0) stop("bandwidth must be positive.")
  g <- if (is.null(grid)) sort(x) else as.numeric(grid)
  est <- vapply(g, function(t) {
    z <- as.numeric(ginv(t))
    jac <- as.numeric(dg(z))
    if (jac <= 0) stop("g' must be positive; g is an increasing bijection (D4).")
    mean(stats::dnorm((z - y) / h)) / (h * jac)
  }, numeric(1))
  list(estimate = est, grid = g, h = h, n = n,
       method = "boundary-free kernel density estimator (Eq. 5.9)")
}

# CANONICAL TEST
# r <- Bfkde(c(0.5, 1, 1.5, 2, 3), grid = 1.5); stopifnot(r$estimate > 0)

#' @rdname Bfkde
#' @keywords internal
#' @export
morie_fauzi_bdfree_density_from_cdf <- Bfkde
