# SPDX-License-Identifier: AGPL-3.0-or-later
#' Functional analysis of variance
#'
#' Ramsay and Silverman (2005), Functional Data Analysis, 2nd ed., Springer,
#' Chapter 13, the one-way functional ANOVA: each curve is written
#' y_gi(t) = mu(t) + alpha_g(t) + eps_gi(t), so at every argument value the
#' observations split into a grand mean function, a treatment effect function
#' and a residual.  The model holds pointwise, so the classical sums of
#' squares hold pointwise too and the integrated versions are their integrals
#' over the whole interval.  With a single argument value the whole thing
#' collapses to the textbook one-way ANOVA, which is the anchor for this
#' module.
#'
#' Group effects are the unweighted deviations of the group mean functions
#' from the grand mean function of all curves.
#'
#' @param functions N-by-T matrix, one curve per row.
#' @param groups N group labels.
#' @param t the argument grid of length T; defaults to equally spaced on
#'   [0, 1] and is unused when T is 1.
#' @return list: estimate, grand, effects, ssb, ssw, ssb_int, ssw_int, F,
#'   df1, df2, n, method.
#' @keywords internal
#' @examples
#' Fanva(matrix(c(1, 2, 3, 4, 5, 6), 6, 1), c(1, 1, 1, 2, 2, 2))$estimate
#' @export
Fanva <- function(functions, groups, t = NULL) {
  Y <- .s03mat(functions)
  g <- .s03vec(groups)
  N <- nrow(Y)
  if (N == 0L) stop("fanova: functions is empty")
  if (length(g) != N) stop("fanova: groups must have one label per curve")
  T_ <- ncol(Y)
  if (T_ == 0L) stop("fanova: functions has no argument values")
  levels <- sort(unique(g))
  G <- length(levels)
  if (G < 2L) stop("fanova: need at least two groups")
  if (N <= G) stop("fanova: need more curves than groups")
  grand <- .fdcolmeans(Y, N, T_)
  gmeans <- matrix(0, G, T_)
  counts <- integer(G)
  for (a in seq_len(G)) {
    idx <- which(g == levels[a])
    counts[a] <- length(idx)
    m <- numeric(T_)
    for (i in idx) for (j in seq_len(T_)) m[j] <- m[j] + Y[i, j]
    gmeans[a, ] <- m / length(idx)
  }
  effects <- matrix(0, G, T_)
  for (a in seq_len(G)) for (j in seq_len(T_)) effects[a, j] <- gmeans[a, j] - grand[j]
  ssb <- numeric(T_)
  for (a in seq_len(G)) for (j in seq_len(T_)) ssb[j] <- ssb[j] + counts[a] * effects[a, j] * effects[a, j]
  ssw <- numeric(T_)
  for (a in seq_len(G)) {
    for (i in seq_len(N)) {
      if (g[i] != levels[a]) next
      for (j in seq_len(T_)) {
        r <- Y[i, j] - gmeans[a, j]
        ssw[j] <- ssw[j] + r * r
      }
    }
  }
  df1 <- G - 1
  df2 <- N - G
  Fp <- numeric(T_)
  for (j in seq_len(T_)) Fp[j] <- if (ssw[j] > 0) (ssb[j] / df1) / (ssw[j] / df2) else Inf
  if (T_ == 1L) {
    ssb_int <- ssb[1]
    ssw_int <- ssw[1]
  } else {
    tt <- if (is.null(t)) .fdgrid(T_) else .s03vec(t)
    if (length(tt) != T_) stop("fanova: t must match the number of argument values")
    ssb_int <- .fdtrapz(tt, ssb)
    ssw_int <- .fdtrapz(tt, ssw)
  }
  Fint <- if (ssw_int > 0) (ssb_int / df1) / (ssw_int / df2) else Inf
  list(estimate = Fint, grand = grand, effects = effects, ssb = ssb, ssw = ssw,
       ssb_int = ssb_int, ssw_int = ssw_int, F = Fp, df1 = df1, df2 = df2, n = N,
       method = "Ramsay-Silverman (2005) Ch.13 one-way functional ANOVA, pointwise decomposition integrated over the whole interval")
}
