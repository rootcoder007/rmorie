# SPDX-License-Identifier: AGPL-3.0-or-later
#' Unobserved components (basic structural) model by the Kalman filter
#'
#' SOURCE. Harvey, A.C. (1989), Forecasting, Structural Time Series Models
#' and the Kalman Filter, Cambridge University Press;
#' doi:10.1017/CBO9781107049994. The basic structural model of Section 2.3
#' is y_t = mu_t + gamma_t + eps_t, mu_t = mu_{t-1} + beta_{t-1} + eta_t,
#' beta_t = beta_{t-1} + zeta_t, gamma_t = -sum_{j=1}^{s-1} gamma_{t-j} +
#' omega_t. It is put in state space form (Sec. 3.1) and run through the
#' Kalman filter (Sec. 3.2): a' = T a, P' = T P T' + Q, v = y - Z a',
#' F = Z P' Z' + 1, K = P' Z'/F, a = a' + K v, P = P' - K Z P'.
#'
#' Disturbance variances are ratios q to sigma^2, so sigma^2 concentrates
#' out exactly (Sec. 3.4): sigma2 = sum_{t>d} v_t^2/F_t / (T-d) and
#' logL = -(T-d)/2 (log 2pi + 1 + log sigma2) - 1/2 sum_{t>d} log F_t.
#'
#' DIFFUSE PRIOR. Sec. 3.3.4's limit of an infinite prior variance is
#' approximated by P_0 = kappa I with kappa large, and by dropping the
#' first d prediction errors, d = state dimension.
#'
#' SCOPE. Level, slope, seasonal and irregular only. The stochastic cycle
#' of Sec. 2.3.4 is NOT implemented -- it needs two further states plus a
#' damping and a frequency parameter. That is this implementation's scope
#' choice, stated rather than attributed. Likewise the variance ratios are
#' chosen by exhaustive search over a fixed lattice, not by numerical
#' optimisation, so both language arms land on identical numbers.
#'
#' NOT read from the book's own page images -- Harvey (1989) is not in the
#' local corpus. Anchored on the closed-form local-level case.
#'
#' @param y Univariate series.
#' @param components Any of "level", "trend", "seasonal", "irregular".
#' @param period Seasonal period s.
#' @param ratio_grid Lattice searched per stochastic variance ratio.
#' @param kappa Finite stand-in for the diffuse prior variance.
#' @return List with \code{level}, \code{slope}, \code{seasonal},
#'   \code{irregular}, \code{sigma2}, \code{loglik}, \code{aic},
#'   \code{ratios}, \code{resid}, \code{F}, \code{n}, \code{d}.
#' @references Harvey, A.C. (1989). Forecasting, Structural Time Series
#'   Models and the Kalman Filter. Cambridge University Press.
#'   doi:10.1017/CBO9781107049994.
#' @examples
#' Unobts(c(1, 2, 3, 4, 5, 6), "level")$sigma2
#' @export
Unobts <- function(y, components = "level", period = 4, ratio_grid = NULL,
                   kappa = 1e10) {
  yv <- .s03vec(y)
  n <- length(yv)
  if (n == 0L) stop("unobserved_components: y is empty")
  nm <- tolower(trimws(as.character(components)))
  for (c0 in nm) {
    if (!(c0 %in% c("level", "trend", "seasonal", "irregular"))) {
      stop(sprintf("unobserved_components: unknown component '%s'", c0))
    }
  }
  if (!("level" %in% nm)) nm <- c("level", nm)
  period <- as.integer(period)
  if ("seasonal" %in% nm && (is.na(period) || period < 2L)) {
    stop("unobserved_components: seasonal period must be at least 2")
  }
  has_trend <- "trend" %in% nm
  has_seas <- "seasonal" %in% nm
  ns <- if (has_seas) period - 1L else 0L
  d <- 1L + (if (has_trend) 1L else 0L) + ns
  Tm <- matrix(0, d, d)
  Z <- numeric(d)
  Tm[1, 1] <- 1
  Z[1] <- 1
  j <- 2L
  if (has_trend) {
    Tm[1, 2] <- 1
    Tm[2, 2] <- 1
    j <- 3L
  }
  slots <- c(1L, if (has_trend) 2L else NULL)
  if (has_seas) {
    for (q in seq_len(ns)) Tm[j, j + q - 1L] <- -1
    if (ns > 1L) for (q in seq_len(ns - 1L)) Tm[j + q, j + q - 1L] <- 1
    Z[j] <- 1
    slots <- c(slots, j)
  }
  if (n <= d) stop("unobserved_components: series shorter than the state dimension")
  grid <- if (is.null(ratio_grid)) c(0, 0.01, 0.1, 0.5, 1) else as.numeric(ratio_grid)
  if (length(grid) == 0L) stop("unobserved_components: ratio_grid is empty")
  ng <- length(grid)
  ns_slot <- length(slots)
  best_ll <- NA_real_
  best <- NULL
  for (cc in seq_len(ng^ns_slot) - 1L) {
    r <- cc
    ix <- integer(ns_slot)
    for (jj in seq_len(ns_slot)) {
      ix[jj] <- r %% ng
      r <- r %/% ng
    }
    qv <- numeric(d)
    for (jj in seq_len(ns_slot)) qv[slots[jj]] <- grid[ix[jj] + 1L]
    a <- numeric(d)
    P <- diag(kappa, d)
    lv <- numeric(n); lf <- numeric(n); st <- matrix(0, n, d)
    for (t in seq_len(n)) {
      ap <- .s03matvec(Tm, a)
      Pp <- .s03matmul(.s03matmul(Tm, P), t(Tm))
      for (i in seq_len(d)) Pp[i, i] <- Pp[i, i] + qv[i]
      Pz <- numeric(d)
      for (i in seq_len(d)) {
        s <- 0
        for (jj in seq_len(d)) s <- s + Pp[i, jj] * Z[jj]
        Pz[i] <- s
      }
      Fv <- 1
      for (i in seq_len(d)) Fv <- Fv + Z[i] * Pz[i]
      za <- 0
      for (i in seq_len(d)) za <- za + Z[i] * ap[i]
      v <- yv[t] - za
      K <- Pz / Fv
      a <- ap + K * v
      ZP <- numeric(d)
      for (jj in seq_len(d)) {
        s <- 0
        for (i in seq_len(d)) s <- s + Z[i] * Pp[i, jj]
        ZP[jj] <- s
      }
      Pn <- P
      for (i in seq_len(d)) for (jj in seq_len(d)) Pn[i, jj] <- Pp[i, jj] - K[i] * ZP[jj]
      P <- Pn
      lv[t] <- v; lf[t] <- Fv; st[t, ] <- a
    }
    m <- n - d
    ss <- 0; lsum <- 0
    for (t in seq(d + 1L, n)) {
      ss <- ss + lv[t] * lv[t] / lf[t]
      lsum <- lsum + log(lf[t])
    }
    s2 <- ss / m
    ll <- -0.5 * m * (log(2 * pi) + 1 + log(s2)) - 0.5 * lsum
    if (is.na(s2) || is.na(ll)) next
    if (is.na(best_ll) || ll > best_ll) {
      best_ll <- ll
      best <- list(ll = ll, s2 = s2, ratios = grid[ix + 1L], lv = lv,
                   lf = lf, st = st, qv = qv)
    }
  }
  if (is.null(best)) stop("unobserved_components: no admissible variance ratios")
  js <- if (has_trend) 3L else 2L
  level <- best$st[, 1L]
  slope <- if (has_trend) best$st[, 2L] else numeric(n)
  seas <- if (has_seas) best$st[, js] else numeric(n)
  npar <- ns_slot + 1L
  .t1_result(estimate = best$ll, level = level, slope = slope,
             seasonal = seas, irregular = yv - level - seas,
             sigma2 = best$s2, loglik = best$ll,
             aic = -2 * best$ll + 2 * npar, ratios = best$ratios,
             q = best$qv, resid = best$lv, F = best$lf, n = n, d = d,
             components = nm,
             method = paste("Basic structural model, Kalman filter,",
                            "concentrated diffuse likelihood",
                            "(Harvey 1989 Secs. 2.3, 3.2, 3.4)"))
}
