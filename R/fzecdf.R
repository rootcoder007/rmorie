# SPDX-License-Identifier: AGPL-3.0-or-later

#' Empirical distribution function
#'
#' \deqn{F_n(x) = n^{-1}\sum_i I(X_i \le x).}{F_n(x) = (1/n) sum_i I(X_i <= x).}
#'
#' The book's baseline, and the thing every kernel estimator in it is trying
#' to beat. Its bias is exactly zero and its variance exactly
#' `F(x)(1 - F(x))/n` -- no expansion, no remainder -- which is why Sec. 2.1
#' can say flatly that a kernel with `r1 > 0` beats it for ANY `F_X`: the
#' kernel estimator's variance is that same quantity minus
#' `2 h r1 f(x) / n`.
#'
#' Right-continuous, as the definition requires: ties at `x` count. `se` is
#' the exact binomial standard error, not an asymptotic one.
#'
#' @param x Sample.
#' @param grid Evaluation points; defaults to the sorted sample.
#' @return Named list with ``estimate``, ``se``, ``variance``, ``grid``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Sec. 2.1, and the bias/variance display preceding (2.3).
#' @examples
#' Edf(c(1, 2, 3, 4), grid = 2)
#' @export
Edf <- function(x, grid = NULL) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 1L) stop("need at least one observation.")
  g <- if (is.null(grid)) sort(x) else as.numeric(grid)
  est <- vapply(g, function(t) mean(x <= t), numeric(1))
  v <- est * (1 - est) / n
  list(estimate = est, se = sqrt(v), variance = v, grid = g, n = n,
       method = "empirical distribution function")
}

# CANONICAL TEST
# stopifnot(Edf(c(1, 2, 3, 4), grid = 2)$estimate == 0.5)

#' @rdname Edf
#' @keywords internal
#' @export
morie_fauzi_ecdf <- Edf
