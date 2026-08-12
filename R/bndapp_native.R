# Manski-Pepper bounds on mean potential outcomes under combined
# monotone treatment response (MTR) and monotone treatment selection
# (MTS).
#
# Source: Manski, C. F. and Pepper, J. V. (2000), Monotone
# instrumental variables: with an application to the returns to
# schooling, Econometrica 68(4), 997-1010.  The sharp bound used here
# is stated as eq. (9.18) of Manski, C. F. (2007), Identification for
# Prediction and Decision, Harvard University Press, Sec. 9.4, which
# reports the Manski-Pepper results:
#
#   sum_{s<t} E(y|z=s) P(z=s) + E(y|z=t) P(z>=t)  <=  E[y(t)]
#      <=  sum_{s>t} E(y|z=s) P(z=s) + E(y|z=t) P(z<=t)
#
# and the average-treatment-effect bound of his eq. (9.19),
#   0 <= E[y(t)] - E[y(s)] <= upper(t) - lower(s).
# The lower end is exactly 0 because MTR alone already signs the
# effect.  Unlike the one-assumption bounds, (9.18) is informative
# even when the outcome space is unbounded -- the point Manski makes
# immediately after stating it.
#
# CITATION NOTE: the Python arm previously labelled this "eq. (36)".
# That number could not be verified against either source (the
# Manski-Pepper scan carries no text layer), so both arms now cite the
# equations that were verified: Manski 2007 eqs. 9.18 and 9.19.
#
# Native implementation mirroring Python morie.fn.bndapp exactly:
# levels in ascending order, the same per-level means and shares.

#' Manski-Pepper MTR-MTS bounds
#'
#' Sharp bounds on each mean potential outcome \eqn{E[y(t)]} under the
#' combined assumptions of monotone treatment response and monotone
#' treatment selection, plus the implied bound on an average
#' treatment effect (Manski and Pepper 2000; Manski 2007, eqs.
#' 9.18-9.19).
#'
#' @param y Outcomes.
#' @param z Realised treatment level per observation; its sorted
#'   unique values are the levels.
#' @param t1 Upper treatment level, default the largest level.
#' @param t0 Lower treatment level, default the smallest level.
#' @return A list with \code{levels}, \code{lower}, \code{upper},
#'   \code{ate_lower} (always 0), \code{ate_upper}, \code{t1},
#'   \code{t0}, \code{n}, \code{method}.
#' @references Manski, C. F. and Pepper, J. V. (2000). Monotone
#'   instrumental variables. Econometrica, 68(4), 997-1010.
#' @export
morie_bndapp <- function(y, z, t1 = NULL, t0 = NULL) {
  yv <- as.numeric(y); zv <- as.numeric(z)
  n <- length(yv)
  if (n == 0L) stop("bndapp: y is empty")
  if (length(zv) != n) stop("bndapp: y and z must have the same length")
  lev <- sort(unique(zv))
  K <- length(lev)
  sh <- vapply(lev, function(g) sum(zv == g) / n, numeric(1))
  mu <- vapply(lev, function(g) mean(yv[zv == g]), numeric(1))
  lower <- numeric(K); upper <- numeric(K)
  for (k in seq_len(K)) {
    lo <- if (k > 1L) sum(sh[1:(k - 1L)] * mu[1:(k - 1L)]) else 0
    lo <- lo + mu[k] * sum(sh[k:K])
    hi <- if (k < K) sum(sh[(k + 1L):K] * mu[(k + 1L):K]) else 0
    hi <- hi + mu[k] * sum(sh[1:k])
    lower[k] <- lo; upper[k] <- hi
  }
  tt1 <- if (is.null(t1)) lev[K] else as.numeric(t1)
  tt0 <- if (is.null(t0)) lev[1L] else as.numeric(t0)
  if (!(tt1 %in% lev) || !(tt0 %in% lev))
    stop("bndapp: t1 and t0 must be realized levels of z")
  if (!(tt1 > tt0)) stop("bndapp: need t1 > t0")
  i1 <- match(tt1, lev); i0 <- match(tt0, lev)
  list(levels = lev, lower = lower, upper = upper,
       ate_lower = 0, ate_upper = upper[i1] - lower[i0],
       t1 = tt1, t0 = tt0, n = n,
       method = paste("Manski-Pepper (2000) MTR-MTS bounds",
                      "(Manski 2007 eqs. 9.18-9.19)"))
}
