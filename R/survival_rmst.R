# SPDX-License-Identifier: AGPL-3.0-or-later

#' Restricted mean survival time
#'
#' The area under the Kaplan-Meier step function up to \code{tau}, with the
#' Greenwood-type variance of Uno et al. (2014):
#' \deqn{\mathrm{RMST}(\tau) = \sum_j \hat S(t_{j-1})(t_j - t_{j-1}),\quad
#'   \widehat{\mathrm{Var}} = \sum_{t_j \le \tau} A_j^2 \frac{d_j}{n_j(n_j - d_j)},}{RMST = sum S(t_{j-1}) (t_j - t_{j-1}), Var = sum A_j^2 d_j / (n_j (n_j - d_j)),}
#' with \eqn{A_j = \int_{t_j}^{\tau} \hat S(u)\,du}.
#'
#' @param time Follow-up times.
#' @param event Event indicator (1 = event, 0 = censored).
#' @param tau Truncation time; default the largest observed time.
#' @param alpha Significance level for the confidence interval.
#' @return list with \code{rmst}, \code{se}, \code{ci_lower},
#'   \code{ci_upper}, \code{tau}.
#' @references Uno H, Claggett B, Tian L, et al. (2014). Moving beyond the
#'   hazard ratio in quantifying the between-group difference in survival
#'   analysis. \emph{Journal of Clinical Oncology}, 32(22), 2380-2385.
#'   Reference implementation: survRM2 (rmst1).
#' @examples
#' morie_rmst(c(2, 3, 3, 5, 8, 9), c(1, 1, 0, 1, 0, 1), tau = 8)$rmst
#' @export
morie_rmst <- function(time, event, tau = NULL, alpha = 0.05) {
  time <- as.numeric(time)
  event <- as.numeric(event)
  if (length(time) != length(event)) stop("time and event must have the same length", call. = FALSE)
  ut <- sort(unique(time))
  n_risk <- vapply(ut, function(u) sum(time >= u), numeric(1))
  n_event <- vapply(ut, function(u) sum(time == u & event == 1), numeric(1))
  surv <- cumprod(1 - n_event / n_risk)
  if (is.null(tau)) tau <- max(ut)
  idx <- which(ut <= tau)
  grid <- c(ut[idx], tau)
  areas <- diff(c(0, grid)) * c(1, surv[idx])
  rmst <- sum(areas)
  wk <- ifelse(n_risk[idx] - n_event[idx] == 0, 0,
               n_event[idx] / (n_risk[idx] * (n_risk[idx] - n_event[idx])))
  tail_area <- rev(cumsum(rev(areas[-1])))
  se <- sqrt(sum(tail_area^2 * wk))
  z <- stats::qnorm(1 - alpha / 2)
  list(rmst = rmst, se = se, ci_lower = rmst - z * se, ci_upper = rmst + z * se, tau = tau)
}

#' Difference in restricted mean survival time between two groups
#'
#' @param time Follow-up times.
#' @param event Event indicator.
#' @param group Two-level group indicator; the difference is level 2 minus
#'   level 1 (sorted), as survRM2 reports arm 1 minus arm 0.
#' @param tau Truncation time (must not exceed either group's last time).
#' @param alpha Significance level.
#' @return list with \code{rmst_diff}, \code{se}, \code{z}, \code{p_value},
#'   \code{ci_lower}, \code{ci_upper}, \code{rmst} (per group), \code{tau}.
#' @examples
#' morie_rmst_difference(c(2, 3, 3, 5, 8, 9, 4, 6), c(1, 1, 0, 1, 0, 1, 1, 0),
#'                       c(0, 0, 0, 0, 1, 1, 1, 1), tau = 5)$rmst_diff
#' @export
morie_rmst_difference <- function(time, event, group, tau = NULL, alpha = 0.05) {
  lv <- sort(unique(group))
  if (length(lv) != 2L) stop("group must have exactly two levels", call. = FALSE)
  if (is.null(tau)) tau <- min(vapply(lv, function(g) max(time[group == g]), numeric(1)))
  r0 <- morie_rmst(time[group == lv[1]], event[group == lv[1]], tau = tau, alpha = alpha)
  r1 <- morie_rmst(time[group == lv[2]], event[group == lv[2]], tau = tau, alpha = alpha)
  d <- r1$rmst - r0$rmst
  se <- sqrt(r0$se^2 + r1$se^2)
  z <- d / se
  zc <- stats::qnorm(1 - alpha / 2)
  list(rmst_diff = d, se = se, z = z, p_value = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
       ci_lower = d - zc * se, ci_upper = d + zc * se,
       rmst = stats::setNames(c(r0$rmst, r1$rmst), lv), tau = tau)
}

#' Weighted log-rank tests (Peto-Peto, Gehan-Wilcoxon, Tarone-Ware)
#'
#' The K-group weighted log-rank statistic
#' \eqn{U^\top V^{-1} U} on \eqn{K - 1} degrees of freedom, with weights
#' \eqn{w_j = 1} (log-rank), \eqn{\hat S(t_j-)} (Peto-Peto, as
#' \code{survival::survdiff(rho = 1)}), \eqn{n_j} (Gehan-Wilcoxon) or
#' \eqn{\sqrt{n_j}} (Tarone-Ware).
#'
#' @param time Follow-up times.
#' @param event Event indicator.
#' @param group Group labels (two or more).
#' @param weights One of \code{"logrank"}, \code{"peto"}, \code{"gehan"},
#'   \code{"tarone"}.
#' @return list with \code{statistic}, \code{df}, \code{p_value},
#'   \code{weights}.
#' @references Peto R, Peto J (1972). Asymptotically efficient rank
#'   invariant test procedures. \emph{JRSS A}, 135(2), 185-207.
#'   Tarone RE, Ware J (1977). On distribution-free tests for equality of
#'   survival distributions. \emph{Biometrika}, 64(1), 156-160.
#' @examples
#' morie_weighted_logrank(c(2, 3, 3, 5, 8, 9, 4, 6), c(1, 1, 0, 1, 0, 1, 1, 0),
#'                        c(0, 0, 0, 0, 1, 1, 1, 1), weights = "peto")$statistic
#' @export
morie_weighted_logrank <- function(time, event, group,
                                   weights = c("logrank", "peto", "gehan", "tarone")) {
  weights <- match.arg(weights)
  lv <- sort(unique(group))
  K <- length(lv)
  if (K < 2L) stop("need at least two groups", call. = FALSE)
  tt <- sort(unique(time[event == 1]))
  n_j <- vapply(tt, function(u) sum(time >= u), numeric(1))
  d_j <- vapply(tt, function(u) sum(time == u & event == 1), numeric(1))
  w <- switch(weights,
    logrank = rep(1, length(tt)),
    gehan = n_j,
    tarone = sqrt(n_j),
    peto = c(1, cumprod(1 - d_j / n_j))[seq_along(tt)]
  )
  U <- numeric(K - 1L)
  V <- matrix(0, K - 1L, K - 1L)
  for (k in seq_along(tt)) {
    u <- tt[k]
    nk <- vapply(lv[-K], function(g) sum(time >= u & group == g), numeric(1))
    dk <- vapply(lv[-K], function(g) sum(time == u & event == 1 & group == g), numeric(1))
    U <- U + w[k] * (dk - d_j[k] * nk / n_j[k])
    if (n_j[k] > 1) {
      f <- d_j[k] * (n_j[k] - d_j[k]) / (n_j[k]^2 * (n_j[k] - 1))
      V <- V + w[k]^2 * f * (diag(nk * n_j[k], K - 1L) - outer(nk, nk))
    }
  }
  stat <- as.numeric(t(U) %*% solve(V, U))
  list(statistic = stat, df = K - 1L,
       p_value = stats::pchisq(stat, K - 1L, lower.tail = FALSE), weights = weights)
}
