# SPDX-License-Identifier: AGPL-3.0-or-later
#' DR-DiD with a stratified cluster-block bootstrap interval
#'
#' Sant'Anna and Zhao (2020), Journal of Econometrics 219(1), 101-122
#' (arXiv:1812.01723 -- FETCHED), equations (2.6)-(2.7) and section 3.2;
#' Cameron, Gelbach and Miller (2008), Bootstrap-based improvements for
#' inference with clustered errors, Review of Economics and Statistics
#' 90(3), 414-427, for the rule that the resampling unit must be the
#' cluster when errors are correlated within it.  The 2008 paper is
#' paywalled; the cluster-as-unit rule is stated identically wherever the
#' cluster bootstrap is defined.  One multiplier is therefore drawn per
#' cluster and applied to every member.
#'
#' Determinism: Mammen's two-point weights at van der Corput points,
#' indexed by cluster; no random resampling.
#'
#' @param y,D outcome change and treatment.
#' @param unit,time unit and period identifiers.
#' @param X covariates.
#' @param clusters cluster identifier; defaults to unit.
#' @param B bootstrap replicates.
#' @param alpha two-sided level.
#' @param y0 period-0 outcome.
#' @return list: estimate, se, ci_lo, ci_hi, boot, n_clusters, n, B, method.
#' @keywords internal
#' @examples
#' Drdidblock(c(1, 2, 0, 3), c(1, 0, 1, 0), c(1, 2, 3, 4), NULL, NULL,
#'            c("a", "a", "b", "b"), 20)$estimate
#' @export
Drdidblock <- function(y, D, unit = NULL, time = NULL, X = NULL,
                       clusters = NULL, B = 199, alpha = 0.05, y0 = NULL) {
  dy <- .s03vec(y)
  if (!is.null(y0)) dy <- dy - .s03vec(y0)
  fit <- .s03drdid(dy, D, X)
  inf <- fit$inf
  n <- length(inf)
  src <- if (!is.null(clusters)) clusters else if (!is.null(unit)) unit else seq_len(n)
  lab <- as.character(src)
  ids <- character(0)
  for (cc in lab) if (!(cc %in% ids)) ids <- c(ids, cc)
  gidx <- match(lab, ids)
  G <- length(ids)
  boot <- numeric(as.integer(B))
  for (b in seq_len(as.integer(B)) - 1L) {
    wts <- numeric(G)
    for (g in seq_len(G) - 1L) wts[g + 1L] <- .s03mammen(b * G + g)
    s <- 0
    for (i in seq_len(n)) s <- s + wts[gidx[i]] * inf[i]
    boot[b + 1L] <- fit$tau + s / n
  }
  a <- as.numeric(alpha)
  list(estimate = fit$tau,
       se = if (length(boot) > 1L) .s03sd(boot, 1L) else NaN,
       ci_lo = .s03quantile7(boot, a / 2), ci_hi = .s03quantile7(boot, 1 - a / 2),
       boot = boot, n_clusters = G, n = n, B = as.integer(B),
       method = "DR-DiD with a deterministic cluster-block multiplier bootstrap")
}
