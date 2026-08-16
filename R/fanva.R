# SPDX-License-Identifier: AGPL-3.0-or-later
#' One-way functional analysis of variance
#'
#' Ramsay and Silverman (2005), Functional Data Analysis, 2nd ed., Springer,
#' Chapter 13 "Modelling functional responses with multivariate covariates":
#' each curve is written y_gi(t) = mu(t) + alpha_g(t) + eps_gi(t), so at every
#' argument value t the observations split into a grand mean function, a
#' treatment effect function, and a residual.  Because the model holds
#' pointwise the classical sums of squares hold pointwise too, and the
#' integrated versions are their integrals over the whole interval.  When the
#' curves are observed at a single argument value the whole thing collapses to
#' the textbook one-way ANOVA, which is the anchor used for this module.
#' Group effects are the unweighted deviations of the group mean functions
#' from the grand mean function of all curves.
#'
#' @param functions N-by-T matrix, one curve per row.
#' @param groups N group labels.
#' @param t the argument grid of length T; defaults to equally spaced on
#'   [0, 1] and is ignored when T == 1.
#' @return list: estimate, grand, effects, ssb, ssw, ssb_int, ssw_int, F,
#'   df1, df2, n, method.
#' @keywords internal
#' @examples
#' Fanva(matrix(c(1, 2, 5, 6), 4, 1), c(1, 1, 2, 2))$estimate
#' @export
Fanva <- function(functions, groups, t = NULL) {
  Y <- .s03mat(functions)
  g <- .s03vec(groups)
  N <- nrow(Y)
  if (N == 0L) stop("fanova: functions is empty")
  if (length(g) != N) stop("fanova: groups must have one label per curve")
  Tn <- ncol(Y)
  if (Tn == 0L) stop("fanova: functions has no argument values")
  levels <- sort(unique(g))
  G <- length(levels)
  if (G < 2L) stop("fanova: need at least two groups")
  if (N <= G) stop("fanova: need more curves than groups")
  grand <- numeric(Tn)
  for (i in seq_len(N)) for (j in seq_len(Tn)) grand[j] <- grand[j] + Y[i, j]
  grand <- grand / N
  gmeans <- vector("list", G)
  counts <- integer(G)
  for (a in seq_len(G)) {
    idx <- which(g == levels[a])
    counts[a] <- length(idx)
    m <- numeric(Tn)
    for (i in idx) for (j in seq_len(Tn)) m[j] <- m[j] + Y[i, j]
    gmeans[[a]] <- m / length(idx)
  }
  effects <- matrix(0, G, Tn)
  for (a in seq_len(G)) for (j in seq_len(Tn)) effects[a, j] <- gmeans[[a]][j] - grand[j]
  ssb <- numeric(Tn)
  for (a in seq_len(G)) for (j in seq_len(Tn)) ssb[j] <- ssb[j] + counts[a] * effects[a, j]^2
  ssw <- numeric(Tn)
  for (a in seq_len(G)) {
    for (i in seq_len(N)) {
      if (g[i] != levels[a]) next
      for (j in seq_len(Tn)) {
        r <- Y[i, j] - gmeans[[a]][j]
        ssw[j] <- ssw[j] + r * r
      }
    }
  }
  df1 <- G - 1
  df2 <- N - G
  Fp <- numeric(Tn)
  for (j in seq_len(Tn)) Fp[j] <- if (ssw[j] > 0) (ssb[j] / df1) / (ssw[j] / df2) else Inf
  if (Tn == 1L) {
    ssb_int <- ssb[1]
    ssw_int <- ssw[1]
  } else {
    tt <- if (is.null(t)) (seq_len(Tn) - 1) / (Tn - 1) else .s03vec(t)
    if (length(tt) != Tn) stop("fanova: t must match the number of argument values")
    ssb_int <- .fanva_trapz(tt, ssb)
    ssw_int <- .fanva_trapz(tt, ssw)
  }
  Fint <- if (ssw_int > 0) (ssb_int / df1) / (ssw_int / df2) else Inf
  list(estimate = Fint, grand = grand, effects = effects, ssb = ssb, ssw = ssw,
       ssb_int = ssb_int, ssw_int = ssw_int, F = Fp, df1 = df1, df2 = df2, n = N,
       method = "Ramsay-Silverman (2005) Ch.13 one-way functional ANOVA, pointwise decomposition integrated over the whole interval")
}

#' .fanva_trapz
#'
#' A step of the fanva implementation. Called by \code{Fanva}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param t A vector; its length is taken and its elements indexed.
#' @param v A vector; indexed elementwise.
#' @return The value of \code{s}, as built in the body.
#' @export
.fanva_trapz <- function(t, v) {
  s <- 0
  n <- length(t)
  if (n > 1L) for (i in seq_len(n - 1L)) s <- s + 0.5 * (v[i] + v[i + 1L]) * (t[i + 1L] - t[i])
  s
}
