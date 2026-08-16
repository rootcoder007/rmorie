# SPDX-License-Identifier: AGPL-3.0-or-later
#' Tail-dependence chi
#'
#' Empirical chi(u) = 2 - log P(F_X < u, F_Y < u) / log u, with the
#' probabilities replaced by observed proportions of the rank transforms.
#' As u tends to one chi(u) tends to the upper tail dependence
#' coefficient: chi = 0 for asymptotic independence, chi = 1 for perfect
#' tail dependence.  The joint proportion is bounded away from 0 and 1 by
#' half an observation so the logarithm stays finite, and chi is clamped
#' to [0, 1] (property 1, Coles p. 164).
#'
#' R arm of the existing Python \code{chiDep} module.
#'
#' @param x,y Equal-length numeric vectors, n >= 4.
#' @param u Threshold in (0, 1), default 0.95.
#' @return List with \code{estimate}, \code{u}, \code{n}, \code{method}.
#' @references Coles, S. (2001). An Introduction to Statistical Modeling
#'   of Extreme Values. Springer, section 8.4, pp. 163-165.
#' @examples
#' ChiDep(c(1, 2, 3, 4, 5, 6), c(1, 2, 3, 4, 5, 6), 0.5)
#' @export
ChiDep <- function(x, y, u = 0.95) {
  xs <- .s03vec(x); ys <- .s03vec(y); n <- length(xs)
  if (n != length(ys) || n < 4L) stop("x and y must be equal-length, n >= 4")
  rx <- .chidep_ranks01(xs); ry <- .chidep_ranks01(ys)
  joint <- sum(rx < u & ry < u) / n
  joint <- min(max(joint, 1 / (2 * n)), 1 - 1 / (2 * n))
  chi_u <- 2 - log(joint) / log(u)
  chi_u <- min(max(chi_u, 0), 1)
  list(estimate = as.numeric(chi_u), u = as.numeric(u), n = as.integer(n),
       method = "empirical chi(u) (Coles 2001 sec. 8.4)")
}

#' .chidep_ranks01
#'
#' Part of the ChiDep implementation; see the file header for the source
#' it follows.
#'
#' @param v See Usage.
#' @return The value of \code{r}, as built in the body.
#' @export
.chidep_ranks01 <- function(v) {
  ord <- order(v)
  r <- numeric(length(v))
  r[ord] <- seq_along(v) / (length(v) + 1)
  r
}

# CANONICAL TEST
# stopifnot(abs(ChiDep(1:20, 1:20, 0.5)$estimate - 1) < 1e-12)
