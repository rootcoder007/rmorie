# SPDX-License-Identifier: AGPL-3.0-or-later
#' BART for survival outcomes
#'
#' Sparapani, Logan, McCulloch and Laud (2016), Nonparametric survival
#' analysis using Bayesian additive regression trees, Statistics in
#' Medicine 35(16), 2741-2753, and Chipman, George and McCulloch (2010),
#' BART, Annals of Applied Statistics 4(1), 266-298.  Sparapani et al.
#' recast survival as a sequence of binary events on a person-period grid,
#' p_ij = P(T = t_j | T >= t_j, x_i) = Phi(mu + f(t_j, x_i)) with f a sum
#' of trees, so BART applies to the discrete hazard rather than the time.
#' Both papers are paywalled; the recasting and the probit link are quoted
#' in their standard published form.
#'
#' Determinism: the backfitting MCMC is replaced by BOOSTED backfitting --
#' trees grown greedily on the working residual and shrunk.  That is a
#' deterministic sum-of-trees fit, not a posterior sample, and the method
#' string says so; no credible intervals are claimed.
#'
#' @param time,event follow-up times and event indicators.
#' @param X covariates.
#' @param n_trees number of stumps.
#' @param shrink the shrinkage factor.
#' @param grid the person-period grid.
#' @return list: estimate, hazard, surv, trees, grid, n, method.
#' @keywords internal
#' @examples
#' Bartsurv(c(1, 2, 3, 4), c(1, 0, 1, 1), matrix(c(0, 1, 0, 1), 4, 1), 3)$surv
#' @export
Bartsurv <- function(time, event, X = NULL, n_trees = 5, shrink = 0.3,
                     grid = NULL) {
  t <- .s03vec(time); e <- .s03vec(event); n <- length(t)
  Xr <- if (!is.null(X)) .s03mat(X) else matrix(0, n, 1)
  g <- if (!is.null(grid)) sort(unique(.s03vec(grid))) else sort(unique(t[e > 0.5]))
  rows <- list(); ys <- numeric(0); who <- integer(0)
  for (i in seq_len(n)) {
    for (j in seq_along(g)) {
      if (g[j] > t[i]) break
      rows[[length(rows) + 1L]] <- c(g[j], as.numeric(Xr[i, ]))
      ys <- c(ys, if (abs(g[j] - t[i]) < 1e-12 && e[i] > 0.5) 1 else 0)
      who <- c(who, i)
    }
  }
  m <- length(rows)
  Rm <- do.call(rbind, rows)
  pbar <- if (m) .s03mean(ys) else 0.5
  if (pbar <= 0) pbar <- 0.5 / m
  if (pbar >= 1) pbar <- 1 - 0.5 / m
  f <- rep(qnorm(pbar), m)
  trees <- list()
  stump <- function(X, r, w) {
    nn <- nrow(X); p <- ncol(X); best <- NULL
    for (a in seq_len(p)) {
      vals <- sort(unique(X[, a]))
      if (length(vals) < 2L) next
      for (tt in seq_len(length(vals) - 1L)) {
        thr <- 0.5 * (vals[tt] + vals[tt + 1L])
        sl <- 0; wl <- 0; sr <- 0; wr <- 0
        for (i in seq_len(nn)) {
          if (X[i, a] <= thr) { sl <- sl + w[i] * r[i]; wl <- wl + w[i] }
          else { sr <- sr + w[i] * r[i]; wr <- wr + w[i] }
        }
        if (wl <= 0 || wr <= 0) next
        ml <- sl / wl; mr <- sr / wr
        gain <- wl * ml * ml + wr * mr * mr
        if (is.null(best) || gain > best[[1]]) best <- list(gain, a, thr, ml, mr)
      }
    }
    best
  }
  for (it in seq_len(as.integer(n_trees))) {
    r <- numeric(m); w <- numeric(m)
    for (i in seq_len(m)) {
      p <- pnorm(f[i])
      p <- min(max(p, 1e-8), 1 - 1e-8)
      dens <- exp(-0.5 * f[i] * f[i]) / sqrt(2 * pi)
      grad <- (ys[i] - p) * dens / (p * (1 - p))
      hess <- dens * dens / (p * (1 - p))
      r[i] <- if (hess > 0) grad / hess else 0
      w[i] <- hess
    }
    st <- stump(Rm, r, w)
    if (is.null(st)) break
    a <- st[[2]]; thr <- st[[3]]; ml <- st[[4]]; mr <- st[[5]]
    trees[[length(trees) + 1L]] <- c(a - 1, thr, ml, mr)
    for (i in seq_len(m)) {
      f[i] <- f[i] + as.numeric(shrink) * (if (Rm[i, a] <= thr) ml else mr)
    }
  }
  haz <- pnorm(f)
  surv <- rep(1, n)
  for (i in seq_len(m)) surv[who[i]] <- surv[who[i]] * (1 - haz[i])
  list(estimate = if (m) haz[1] else NaN, hazard = haz, surv = surv,
       trees = trees, grid = g, n = n,
       method = paste0("Person-period probit hazard with a boosted sum of stumps ",
                       "(Sparapani et al. 2016 recasting; deterministic fit, not a posterior sample)"))
}
