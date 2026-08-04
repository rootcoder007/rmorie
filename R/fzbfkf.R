# SPDX-License-Identifier: AGPL-3.0-or-later

#' Boundary-free kernel distribution function estimator (Eq. 5.5)
#'
#' Eq. (5.5):
#' \deqn{\tilde F_X(x) = \frac{1}{n}\sum_i W\Big(\frac{g^{-1}(x) - g^{-1}(X_i)}{h}\Big),\quad x\in\Omega.}{Ftilde(x) = (1/n) sum_i W((g^-1(x) - g^-1(X_i))/h), x in Omega.}
#'
#' It looks like nothing more than substituting `g^-1` into the naive
#' estimator, and that is what it is. The reason it WORKS is the
#' change-of-variable property of a distribution function: if `Y = g^-1(X)`
#' then `F_X(x) = F_Y(g^-1(x))` exactly, for increasing `g`. No Jacobian.
#'
#' That is why the trick is available here and not to a density estimator,
#' where the same substitution needs the `1/g'(g^-1(x))` factor of (5.9).
#' Sec. 5.2 puts it plainly: the property "cannot always be done to other
#' probability-related functions".
#'
#' Estimating on the transformed scale, where the support is the whole line,
#' means a symmetric kernel puts no mass outside `Omega` after mapping back --
#' the `O(h)` boundary bias never arises, and Theorem 5.2 gets `O(h^2)`
#' everywhere including at the edge.
#'
#' @param x Sample, inside the support `Omega`.
#' @param grid Evaluation points; defaults to the sorted sample.
#' @param ginv The inverse transformation; defaults to `log`, i.e. `g = exp`
#'   and `Omega = (0, Inf)`.
#' @param h Bandwidth on the TRANSFORMED scale; defaults to the
#'   distribution-function rule applied to `ginv(x)`.
#' @return Named list with ``estimate``, ``grid``, ``h``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Eq. (5.5).
#' @examples
#' Bfkdf(c(0.5, 1, 1.5, 2, 3), grid = 1.5)
#' @export
Bfkdf <- function(x, grid = NULL, ginv = NULL, h = NULL) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L) stop("need at least two observations.")
  if (is.null(ginv)) {
    if (any(x <= 0)) stop("the default g = exp needs data on (0, Inf).")
    ginv <- log
  }
  y <- vapply(x, function(t) as.numeric(ginv(t)), numeric(1))
  if (!all(is.finite(y))) stop("g^-1 left the real line on some observation.")
  if (is.null(h)) h <- .morie_kdfe_h(y)
  if (h <= 0) stop("bandwidth must be positive.")
  g <- if (is.null(grid)) sort(x) else as.numeric(grid)
  est <- vapply(g, function(t) mean(stats::pnorm((as.numeric(ginv(t)) - y) / h)),
                numeric(1))
  list(estimate = est, grid = g, h = h, n = n,
       method = "boundary-free kernel distribution function estimator (Eq. 5.5)")
}

# CANONICAL TEST
# r <- Bfkdf(c(0.5, 1, 1.5, 2, 3), grid = 1.5)
# stopifnot(r$estimate >= 0, r$estimate <= 1)

#' @rdname Bfkdf
#' @keywords internal
#' @export
morie_fauzi_bdfree_kdfe_test <- Bfkdf
