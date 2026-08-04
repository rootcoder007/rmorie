# SPDX-License-Identifier: AGPL-3.0-or-later

#' 1-Wasserstein distance on the line
#'
#' On the real line the Kantorovich--Rubinstein dual of the \eqn{p = 1}
#' transport problem,
#' \eqn{W_1(p, q) = \sup_{Lip(f) \le 1} (\int f dp - \int f dq)},
#' has the closed form \eqn{W_1(p, q) = \int |F_p(x) - F_q(x)| dx}: the
#' transport cost is the area between the two cumulative distribution
#' functions. No linear programme is needed, which is why this function
#' has no solver.
#'
#' Two input conventions are accepted. With \code{support} given,
#' \code{p} and \code{q} are probability masses on that common grid, both
#' normalised to sum to one; the CDFs are then step functions constant
#' between consecutive support points, so
#' \eqn{W_1 = \sum_i |P_i - Q_i| (s_{i+1} - s_i)} with \eqn{P}, \eqn{Q}
#' the cumulative masses. With \code{support} omitted, \code{p} and
#' \code{q} are raw samples and the empirical distributions are compared
#' by the same closed form on the pooled order statistics.
#'
#' Mirrors \code{morie.fn.wassdt} on the Python side.
#'
#' @param p,q Numeric vectors: probability masses on \code{support}, or
#'   raw samples when \code{support} is \code{NULL}.
#' @param support Numeric vector of common support points, strictly
#'   increasing and the same length as \code{p} and \code{q}, or
#'   \code{NULL}.
#' @return Named list with \code{distance}, \code{n_support},
#'   \code{method}.
#' @references Kantorovich L V & Rubinstein G S (1958). On a space of
#'   completely additive functions. \emph{Vestnik Leningrad. Univ.}
#'   13(7), 52--59. The one-dimensional closed form is Theorem 2.18 of
#'   Villani C (2009), \emph{Optimal Transport: Old and New}, Springer.
#' @examples
#' Wassdt(c(0.2, 0.5, 0.3), c(0.4, 0.4, 0.2), support = c(0, 1, 2))$distance
#' @export
Wassdt <- function(p, q, support = NULL) {
  if (is.null(support)) {
    pv <- as.numeric(p)
    qv <- as.numeric(q)
    if (length(pv) == 0L || length(qv) == 0L) {
      stop("both samples must be non-empty", call. = FALSE)
    }
    ## Same closed form on the pooled order statistics.
    xs <- sort(pv)
    ys <- sort(qv)
    grid <- sort(c(xs, ys))
    d <- 0
    if (length(grid) > 1L) {
      for (i in seq_len(length(grid) - 1L)) {
        cu <- sum(xs <= grid[i]) / length(xs)
        cv <- sum(ys <= grid[i]) / length(ys)
        d <- d + abs(cu - cv) * (grid[i + 1L] - grid[i])
      }
    }
    return(list(distance = d,
                n_support = length(pv) + length(qv),
                method = "1-Wasserstein distance (empirical, 1D)"))
  }

  s <- as.numeric(support)
  pv <- as.numeric(p)
  qv <- as.numeric(q)
  n <- length(s)
  if (length(pv) != n || length(qv) != n) {
    stop("p, q and support must have the same length", call. = FALSE)
  }
  if (n < 2L) stop("need at least two support points", call. = FALSE)
  if (any(diff(s) <= 0)) {
    stop("support must be strictly increasing", call. = FALSE)
  }
  if (any(pv < 0) || any(qv < 0)) {
    stop("probability masses must be non-negative", call. = FALSE)
  }
  sp <- 0
  sq <- 0
  for (i in seq_len(n)) {
    sp <- sp + pv[i]
    sq <- sq + qv[i]
  }
  if (sp <= 0 || sq <= 0) {
    stop("probability masses must have positive total mass", call. = FALSE)
  }

  cp <- 0
  cq <- 0
  total <- 0
  for (i in seq_len(n - 1L)) {
    cp <- cp + pv[i] / sp
    cq <- cq + qv[i] / sq
    gap <- cp - cq
    if (gap < 0) gap <- -gap
    total <- total + gap * (s[i + 1L] - s[i])
  }
  list(distance = total,
       n_support = n,
       method = "1-Wasserstein distance (1D, gridded)")
}
