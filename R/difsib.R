# SPDX-License-Identifier: AGPL-3.0-or-later
#' SIBTEST differential item functioning (Shealy and Stout 1993)
#'
#' Source FETCHED (reference implementation): \code{SIBTEST} in the CRAN
#' package \pkg{mirt} (mirt 1.46.1, \code{R/SIBTEST.R}), implementing
#' Shealy, R. and Stout, W. (1993), Psychometrika 58, 159-194.
#' (\code{difR::sibTest} is a thin wrapper that delegates to it.)  The
#' 1993 paper is paywalled here; the package source states the estimator
#' explicitly.  Grouping examinees on the matching score k:
#' \code{pstar_k = n_k / sum n_k},
#' \code{beta = sum_k pstar_k (Ystar_R,k - Ystar_F,k)},
#' \code{sigma = sqrt(sum_k pstar_k^2 (s2_F,k/n_F,k + s2_R,k/n_R,k))},
#' \code{X2 = (beta/sigma)^2} on one degree of freedom.
#'
#' A level contributes only when both groups are present there and both
#' within-cell variances are non-zero; \pkg{mirt} drops the rest and
#' renormalises \code{pstar}, and so does this, since a zero-variance
#' level contributes nothing to sigma and would otherwise deflate it.
#'
#' \code{Ystar} is \code{Ybar} unless \code{correction = TRUE}, which
#' applies the Shealy-Stout true-score regression correction as
#' \pkg{mirt} writes it,
#' \code{M_g = (Ybar_g,k+1 - Ybar_g,k-1)/(V_g,k+1 - V_g,k-1)} and
#' \code{Ystar_g,k = Ybar_g,k + M_g (V_k - V_g,k)}, skipped at the two
#' end levels where k-1 or k+1 does not exist.  The default is FALSE,
#' which is the estimator this row specifies.
#'
#' @param y Score on the suspect item, one per examinee.
#' @param group Group membership; exactly two distinct values, the first
#'   encountered being the reference group.
#' @param studied Optional alias for \code{y}, accepted for the original
#'   argument order; when given it overrides \code{y}.
#' @param matching Matching (valid subtest) score, one per examinee.
#'   Required.
#' @param correction Apply the Shealy-Stout regression correction.
#'   Default FALSE.
#' @return list: beta, sigma, statistic, p_value, df, n_levels, levels,
#'   pstar, correction, n, method.
#' @examples
#' set.seed(1)
#' m <- rep(0:2, each = 20)
#' g <- rep(c("r", "f"), 30)
#' Difsib(rbinom(60, 1, 0.5), g, matching = m)$beta
#' @export
Difsib <- function(y, group, studied = NULL, matching = NULL, correction = FALSE) {
  s <- as.numeric(if (is.null(studied)) y else studied)
  n <- length(s)
  g <- as.character(group)
  if (length(g) != n) stop("group must be the same length as the item score")
  if (is.null(matching)) stop("matching score is required")
  mt <- matching
  if (length(mt) != n) stop("matching must be the same length as the item score")
  levs <- unique(g)
  if (length(levs) != 2L) stop("group must have exactly 2 distinct values")
  ref <- levs[1]

  keys <- sort(unique(mt))
  nk <- length(keys)
  tab <- nr <- nf <- ybar_r <- ybar_f <- s2r <- s2f <- vr <- vf <- vk <- numeric(nk)
  for (j in seq_len(nk)) {
    idx <- which(mt == keys[j])
    ir <- idx[g[idx] == ref]
    iff <- idx[g[idx] != ref]
    tab[j] <- length(idx)
    nr[j] <- length(ir)
    nf[j] <- length(iff)
    ybar_r[j] <- if (nr[j] > 0) mean(s[ir]) else NaN
    ybar_f[j] <- if (nf[j] > 0) mean(s[iff]) else NaN
    s2r[j] <- if (nr[j] > 1) stats::var(s[ir]) else if (nr[j] == 1) 0 else NaN
    s2f[j] <- if (nf[j] > 1) stats::var(s[iff]) else if (nf[j] == 1) 0 else NaN
    vr[j] <- if (nr[j] > 0) mean(as.numeric(mt[ir])) else NaN
    vf[j] <- if (nf[j] > 0) mean(as.numeric(mt[iff])) else NaN
    vk[j] <- mean(as.numeric(mt[idx]))
  }
  keep <- which(nr > 0 & nf > 0 & !is.na(s2r) & !is.na(s2f) & s2r > 0 & s2f > 0)
  if (length(keep) == 0L) {
    stop("no matching level has both groups present with non-zero within-cell variance")
  }
  pstar <- numeric(nk)
  pstar[keep] <- tab[keep] / sum(tab[keep])

  ystar_r <- ybar_r
  ystar_f <- ybar_f
  if (correction) {
    for (j in keep) {
      if (j > 1L && j < nk) {
        dr <- vr[j + 1L] - vr[j - 1L]
        dff <- vf[j + 1L] - vf[j - 1L]
        mr <- if (!is.na(dr) && dr != 0) (ybar_r[j + 1L] - ybar_r[j - 1L]) / dr else 0
        mf <- if (!is.na(dff) && dff != 0) (ybar_f[j + 1L] - ybar_f[j - 1L]) / dff else 0
        if (is.na(mr)) mr <- 0
        if (is.na(mf)) mf <- 0
        ystar_r[j] <- ybar_r[j] + mr * (vk[j] - vr[j])
        ystar_f[j] <- ybar_f[j] + mf * (vk[j] - vf[j])
      }
    }
  }

  beta <- sum(pstar[keep] * (ystar_r[keep] - ystar_f[keep]))
  vv <- sum(pstar[keep]^2 * (s2f[keep] / nf[keep] + s2r[keep] / nr[keep]))
  sigma <- if (vv > 0) sqrt(vv) else NaN
  stat <- if (!is.nan(sigma) && sigma > 0) (beta / sigma)^2 else NaN
  p <- if (!is.nan(stat)) stats::pchisq(stat, 1, lower.tail = FALSE) else NaN
  list(
    beta = beta, sigma = sigma, statistic = stat, p_value = p, df = 1L,
    n_levels = length(keep), levels = as.numeric(keys[keep]),
    pstar = pstar[keep], correction = correction, n = n,
    method = "SIBTEST DIF (Shealy and Stout 1993; mirt::SIBTEST)"
  )
}
