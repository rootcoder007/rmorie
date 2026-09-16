# SPDX-License-Identifier: AGPL-3.0-or-later

#' Manski-Pepper MTR-MTS bounds (the returns-to-schooling application)
#'
#' Joint monotone-treatment-response / monotone-treatment-selection
#' bounds on \eqn{E[y(t)]}. Under MTR (each unit response weakly
#' increasing in the ordered treatment) and MTS (units selecting higher
#' treatment have weakly higher mean response functions), Proposition 3
#' Corollary 2 of Manski and Pepper (2000), eq. (36), gives sharp bounds
#' that need no outcome-support assumption:
#' \deqn{\sum_{u<t} E[y|z=u] P(z=u) + E[y|z=t] P(z \ge t) \le E[y(t)]
#'   \le \sum_{u>t} E[y|z=u] P(z=u) + E[y|z=t] P(z \le t).}
#' For a contrast \eqn{t_1 > t_0} the upper bound is
#' \eqn{U(t_1) - L(t_0)}; the lower bound is 0 because MTR alone already
#' implies \eqn{y(t_1) \ge y(t_0)} unit by unit (Manski 1997,
#' Proposition M2).
#'
#' @param y Observed outcome.
#' @param z Realized treatment on an ordered scale; every distinct value
#'   is a level.
#' @param t1 Upper contrast level; default the largest realized level.
#' @param t0 Lower contrast level; default the smallest realized level.
#' @return List with \code{levels}, \code{lower}, \code{upper}
#'   (per-level, parallel vectors), \code{ate_lower} (0),
#'   \code{ate_upper}, \code{t1}, \code{t0}, \code{n}, \code{method}.
#' @references Manski, C. F. and Pepper, J. V. (2000), Monotone
#'   Instrumental Variables: With an Application to the Returns to
#'   Schooling, Econometrica 68(4):997-1010, eq. (36) (Proposition 3,
#'   Corollary 2) and eq. (34). Manski, C. F. (1997), Monotone Treatment
#'   Response, Econometrica 65(6):1311-1334, Proposition M2, as printed
#'   in Molinari, F. (2021), Handbook of Econometrics 7A, eq. (2.13)
#'   (arXiv:2004.11751).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Bndapp(V, V)
Bndapp <- function(y, z, t1 = NULL, t0 = NULL) {
  yv <- as.numeric(unlist(y))
  zv <- as.numeric(unlist(z))
  n <- length(yv)
  if (n == 0) stop("Bndapp: y is empty")
  if (length(zv) != n) stop("Bndapp: y and z must have the same length")
  lev <- sort(unique(zv))
  K <- length(lev)
  p <- numeric(K)
  m <- numeric(K)
  for (k in seq_len(K)) {
    sel <- zv == lev[k]
    p[k] <- sum(sel) / n
    m[k] <- mean(yv[sel])
  }
  lower <- numeric(K)
  upper <- numeric(K)
  for (k in seq_len(K)) {
    below <- seq_len(K) < k
    above <- seq_len(K) > k
    lower[k] <- sum(p[below] * m[below]) + m[k] * sum(p[!below])
    upper[k] <- sum(p[above] * m[above]) + m[k] * sum(p[!above])
  }
  tt1 <- if (is.null(t1)) lev[K] else as.numeric(t1)
  tt0 <- if (is.null(t0)) lev[1] else as.numeric(t0)
  if (!(tt1 %in% lev) || !(tt0 %in% lev)) {
    stop("Bndapp: t1 and t0 must be realized levels of z")
  }
  if (!(tt1 > tt0)) stop("Bndapp: need t1 > t0")
  i1 <- match(tt1, lev)
  i0 <- match(tt0, lev)
  .t1_result(levels = lev, lower = lower, upper = upper,
             ate_lower = 0, ate_upper = upper[i1] - lower[i0],
             t1 = tt1, t0 = tt0, n = n,
             method = "Manski-Pepper (2000) MTR-MTS bounds, eq. (36)")
}
