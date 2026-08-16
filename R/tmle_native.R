# SPDX-License-Identifier: AGPL-3.0-or-later
#
# R mirror of the morie.fn TMLE tier (tmlpoy, tmltrt, tmlsen, tmlqct,
# tmlmed, tmlivc, tmltvc, tmllng, npstm) and its shared targeting core
# (_tmle.py).

#' .morie_tmle_logit
#'
#' Part of the tmle_native implementation; see the file header for the
#' source it follows.
#'
#' @param p See Usage.
#' @return A numeric value.
#' @export
.morie_tmle_logit <- function(p) log(p / (1 - p))
#' .morie_tmle_expit
#'
#' Part of the tmle_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @return A numeric value.
#' @export
.morie_tmle_expit <- function(x) 1 / (1 + exp(-pmin(pmax(x, -35), 35)))

# OLS fitted on `fit_rows` only, predicted for everyone.
#' OLS fitted on `fit_rows` only, predicted for everyone
#'
#' Part of the tmle_native implementation; see the file header for the
#' source it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param fit_rows See Usage.
#' @return The value of \code{as.vector}.
#' @export
.morie_tmle_ols_predict <- function(X, y, fit_rows) {
  D <- cbind(1, X)
  b <- qr.coef(qr(D[fit_rows, , drop = FALSE]), y[fit_rows])
  b[is.na(b)] <- 0
  as.vector(D %*% b)
}

#' Targeted maximum likelihood estimate of the average treatment effect
#'
#' Three steps: fit the outcome regressions and the propensity; fluctuate
#' the outcome fit along the clever covariate
#' `H = A/g - (1 - A)/(1 - g)` on the outcome rescaled to \\[0, 1\\], so the
#' logistic fluctuation is valid for bounded continuous outcomes as well
#' as binary ones; then substitute. The estimate solves the efficient
#' influence-function equation, so the EIF supplies the standard error.
#' TMLE is doubly robust *and* a substitution estimator, so unlike AIPW
#' it can never leave the parameter space.
#'
#' Mirrors the shared `morie.fn._tmle.tmle_ate` core.
#'
#' @param y Outcome (binary or bounded continuous).
#' @param a Binary 0/1 treatment.
#' @param w Covariate matrix (or vector).
#' @param trunc Propensity truncation bound.
#' @param g Pre-computed propensity scores; skips the internal fit.
#' @param scale_outcome Rescale `y` to \\[0, 1\\] and map back.
#' @param max_iter,tol Fluctuation Newton controls.
#' @return List with `ate`, `se`, `ci`, `eif`, `epsilon`, `q1`, `q0`,
#'   `g`, `ey1`, `ey0`, `n`.
#' @references van der Laan MJ, Rubin D (2006). Targeted maximum
#'   likelihood learning. \emph{The International Journal of
#'   Biostatistics} 2(1), Article 11.
#'
#'   Gruber S, van der Laan MJ (2010). A targeted maximum likelihood
#'   estimator of a causal effect on a bounded continuous outcome.
#'   \emph{The International Journal of Biostatistics} 6(1), Article 26.
#' @export
morie_tmle_ate <- function(y, a, w, trunc = 0.01, g = NULL,
                           scale_outcome = TRUE, max_iter = 50L,
                           tol = 1e-10) {
  y <- as.numeric(y); a <- as.numeric(a)
  W <- as.matrix(w); storage.mode(W) <- "double"
  n <- length(y)
  if (length(a) != n || nrow(W) != n) {
    stop("y, a and w must share their first dimension.", call. = FALSE)
  }
  if (!all(a %in% c(0, 1))) stop("a must be binary 0/1.", call. = FALSE)
  if (sum(a) == 0 || sum(a) == n) {
    stop("need both treatment arms.", call. = FALSE)
  }
  if (trunc < 0 || trunc >= 0.5) {
    stop("trunc must lie in [0, 0.5).", call. = FALSE)
  }

  lo <- if (scale_outcome) min(y) else 0
  hi <- if (scale_outcome) max(y) else 1
  span <- hi - lo
  if (span <= 0) stop("outcome has zero range.", call. = FALSE)
  ys <- if (scale_outcome) (y - lo) / span else y
  ys <- pmin(pmax(ys, 1e-6), 1 - 1e-6)

  gw <- if (is.null(g)) .morie_logit_fit(W, a) else as.numeric(g)
  if (length(gw) != n) {
    stop("g must have one entry per observation.", call. = FALSE)
  }
  gw <- pmin(pmax(gw, max(trunc, 1e-6)), 1 - max(trunc, 1e-6))

  q1 <- pmin(pmax(.morie_tmle_ols_predict(W, ys, a == 1), 1e-6), 1 - 1e-6)
  q0 <- pmin(pmax(.morie_tmle_ols_predict(W, ys, a == 0), 1e-6), 1 - 1e-6)
  qa <- ifelse(a == 1, q1, q0)

  H <- a / gw - (1 - a) / (1 - gw)
  H1 <- 1 / gw
  H0 <- -1 / (1 - gw)

  eps <- 0
  off <- .morie_tmle_logit(qa)
  for (i in seq_len(max_iter)) {
    p <- .morie_tmle_expit(off + eps * H)
    grad <- sum(H * (ys - p))
    hess <- sum(H^2 * p * (1 - p))
    if (hess <= 1e-14) break
    step <- grad / hess
    eps <- eps + step
    if (abs(step) < tol) break
  }

  q1s <- .morie_tmle_expit(.morie_tmle_logit(q1) + eps * H1)
  q0s <- .morie_tmle_expit(.morie_tmle_logit(q0) + eps * H0)
  qas <- .morie_tmle_expit(off + eps * H)

  if (scale_outcome) {
    q1o <- q1s * span + lo; q0o <- q0s * span + lo; qao <- qas * span + lo
    yo <- y
  } else {
    q1o <- q1s; q0o <- q0s; qao <- qas; yo <- ys
  }

  psi <- mean(q1o - q0o)
  eif <- H * (yo - qao) + (q1o - q0o) - psi
  se <- sqrt(sum(eif^2)) / n

  list(ate = psi, se = se, ci = c(psi - 1.96 * se, psi + 1.96 * se),
       eif = eif, epsilon = eps, q1 = q1o, q0 = q0o, g = gw,
       ey1 = mean(q1o), ey0 = mean(q0o), n = n)
}

#' Propensity-only TMLE: double robustness by construction
#'
#' Sets the initial outcome fit to a constant so the targeting step
#' carries all the information. The outcome model is as wrong as it can
#' be, yet the estimate stays consistent provided the propensity model is
#' right. The fully specified TMLE is returned alongside for comparison.
#' Mirrors `morie.fn.tmlpoy`.
#'
#' @param y Outcome.
#' @param d Binary 0/1 treatment.
#' @param x Covariates for the propensity model.
#' @param trunc Propensity truncation.
#' @return List with `ate`, `se`, `ci`, `ate_full`, `epsilon`, `n`.
#' @references van der Laan MJ, Rose S (2011). \emph{Targeted Learning}.
#'   Springer, Ch. 4-5.
#' @export
morie_tmle_propensity_only <- function(y, d, x, trunc = 0.01) {
  X <- as.matrix(x); storage.mode(X) <- "double"
  n <- length(y)
  g <- .morie_logit_fit(X, as.numeric(d))
  null_w <- matrix(0, nrow = n, ncol = 1)
  out <- morie_tmle_ate(y, d, null_w, trunc = trunc, g = g)
  full <- morie_tmle_ate(y, d, X, trunc = trunc)
  list(ate = out$ate, se = out$se, ci = out$ci, ate_full = full$ate,
       epsilon = out$epsilon, n = out$n)
}

#' TMLE across a grid of propensity truncation levels
#'
#' Truncation bounds the clever covariate and hence the variance, at the
#' cost of bias. A stable plateau means positivity is fine; an estimate
#' that moves sharply with the truncation level is being driven by a few
#' extreme weights and should not be reported as a point estimate.
#' Mirrors `morie.fn.tmltrt`.
#'
#' @param y,d,x As in [morie_tmle_propensity_only()].
#' @param eps_grid Truncation levels, all strictly inside (0, 0.5).
#' @return List with `eps`, `ate`, `se`, `n_truncated`, `range`,
#'   `stable`, `n`.
#' @references Petersen ML, Porter KE, Gruber S, Wang Y, van der Laan MJ
#'   (2012). Diagnosing and responding to violations in the positivity
#'   assumption. \emph{Statistical Methods in Medical Research} 21(1),
#'   31-54.
#' @export
morie_tmle_truncation_sweep <- function(y, d, x,
                                        eps_grid = c(0.001, 0.01, 0.025,
                                                     0.05, 0.1)) {
  grid <- as.numeric(eps_grid)
  if (length(grid) < 2L || any(grid <= 0 | grid >= 0.5)) {
    stop("eps_grid needs at least 2 values, all strictly inside (0, 0.5).",
         call. = FALSE)
  }
  base <- morie_tmle_ate(y, d, x, trunc = 1e-6)
  g0 <- base$g
  res <- lapply(grid, function(e) morie_tmle_ate(y, d, x, trunc = e))
  ates <- vapply(res, function(r) r$ate, numeric(1))
  ses <- vapply(res, function(r) r$se, numeric(1))
  ntr <- vapply(grid, function(e) sum(g0 < e | g0 > 1 - e), numeric(1))
  rng <- max(ates) - min(ates)
  list(eps = grid, ate = ates, se = ses, n_truncated = ntr, range = rng,
       stable = rng < mean(ses), n = base$n)
}

#' TMLE bounds under the marginal sensitivity model
#'
#' An unmeasured confounder can shift the true propensity odds by at most
#' a factor Gamma. Re-running TMLE at the two Gamma-tilted propensities
#' gives an interval containing the estimate for every admissible
#' confounder of that strength; `gamma_critical` is the smallest Gamma at
#' which the interval first covers zero. Mirrors `morie.fn.tmlsen`.
#'
#' @param y,d,x As in [morie_tmle_propensity_only()].
#' @param gamma_grid Odds-ratio bounds (all >= 1).
#' @param trunc Propensity truncation.
#' @return List with `gamma`, `lower`, `upper`, `ate`, `gamma_critical`,
#'   `n`.
#' @references Tan Z (2006). A distributional approach for causal
#'   inference using propensity scores. \emph{JASA} 101(476), 1619-1637.
#'
#'   Zhao Q, Small DS, Bhattacharya BB (2019). Sensitivity analysis for
#'   inverse probability weighting estimators via the percentile
#'   bootstrap. \emph{JRSS-B} 81(4), 735-761.
#' @export
morie_tmle_sensitivity <- function(y, d, x, gamma_grid = NULL, trunc = 0.01) {
  grid <- if (is.null(gamma_grid)) seq(1, 3, length.out = 9) else {
    as.numeric(gamma_grid)
  }
  if (any(grid < 1)) stop("gamma values must be at least 1.", call. = FALSE)
  base <- morie_tmle_ate(y, d, x, trunc = trunc)
  odds <- base$g / (1 - base$g)
  lows <- highs <- numeric(length(grid))
  for (i in seq_along(grid)) {
    gam <- grid[i]
    g_lo <- pmin(pmax((odds / gam) / (1 + odds / gam), trunc), 1 - trunc)
    g_hi <- pmin(pmax((odds * gam) / (1 + odds * gam), trunc), 1 - trunc)
    a <- morie_tmle_ate(y, d, x, trunc = trunc, g = g_lo)$ate
    b <- morie_tmle_ate(y, d, x, trunc = trunc, g = g_hi)$ate
    lows[i] <- min(a, b); highs[i] <- max(a, b)
  }
  crosses <- lows <= 0 & highs >= 0
  list(gamma = grid, lower = lows, upper = highs, ate = base$ate,
       gamma_critical = if (any(crosses)) grid[which(crosses)[1]] else NULL,
       n = base$n)
}

#' TMLE for quantile treatment effects
#'
#' Each counterfactual CDF value is a mean of an indicator, so the TMLE
#' machinery runs on `1{Y <= t}` over a grid of `t`. The two curves are
#' monotonised (pointwise TMLE curves need not be monotone) and inverted
#' at the requested level. Mirrors `morie.fn.tmlqct`.
#'
#' @param y,d,x As in [morie_tmle_propensity_only()].
#' @param quantile Quantile level in (0, 1).
#' @param n_grid Grid points spanning the outcome range.
#' @param trunc Propensity truncation.
#' @return List with `qte`, `q1`, `q0`, `quantile`, `grid`, `f1`, `f0`,
#'   `n`.
#' @references Diaz I (2017). Efficient estimation of quantiles in
#'   missing data models. \emph{Journal of Statistical Planning and
#'   Inference} 190, 39-51.
#' @export
morie_tmle_quantile <- function(y, d, x, quantile = 0.5, n_grid = 60L,
                                trunc = 0.01) {
  y <- as.numeric(y)
  q <- as.numeric(quantile)
  if (q <= 0 || q >= 1) {
    stop("quantile must lie strictly in (0, 1).", call. = FALSE)
  }
  k <- as.integer(n_grid)
  if (k < 5L) stop("n_grid must be at least 5.", call. = FALSE)
  grid <- stats::quantile(y, seq(0.02, 0.98, length.out = k), names = FALSE)
  f1 <- f0 <- numeric(k)
  for (i in seq_len(k)) {
    ind <- as.numeric(y <= grid[i])
    if (min(ind) == max(ind)) {
      f1[i] <- f0[i] <- ind[1]
      next
    }
    out <- morie_tmle_ate(ind, d, x, trunc = trunc, scale_outcome = FALSE)
    f1[i] <- out$ey1; f0[i] <- out$ey0
  }
  f1 <- pmin(pmax(cummax(f1), 0), 1)
  f0 <- pmin(pmax(cummax(f0), 0), 1)
  invert <- function(F) grid[min(which(F >= q)[1], k, na.rm = TRUE)]
  q1 <- invert(f1); q0 <- invert(f0)
  list(qte = q1 - q0, q1 = q1, q0 = q0, quantile = q, grid = grid,
       f1 = f1, f0 = f0, n = length(y))
}

#' TMLE for natural direct and indirect mediation effects
#'
#' The total effect comes from TMLE; the direct effect fixes the mediator
#' distribution at its control-arm law by weighting the outcome
#' regression with the mediator density ratio. The indirect effect is
#' taken as the residual, which makes the decomposition add up exactly at
#' the price of putting all the modelling error in that piece. Mirrors
#' `morie.fn.tmlmed`.
#'
#' @param y Outcome.
#' @param treatment Binary 0/1 exposure.
#' @param mediator Continuous mediator (Gaussian density model).
#' @param covariates Optional baseline covariates.
#' @param trunc Propensity truncation.
#' @return List with `nde`, `nie`, `total`, `se_total`, `prop_mediated`,
#'   `n`.
#' @references Zheng W, van der Laan MJ (2012). Targeted maximum
#'   likelihood estimation of natural direct effects. \emph{The
#'   International Journal of Biostatistics} 8(1), Article 3.
#' @export
morie_tmle_mediation <- function(y, treatment, mediator, covariates = NULL,
                                 trunc = 0.01) {
  y <- as.numeric(y); A <- as.numeric(treatment); M <- as.numeric(mediator)
  n <- length(y)
  if (length(A) != n || length(M) != n) {
    stop("y, treatment and mediator must have equal length.", call. = FALSE)
  }
  if (!all(A %in% c(0, 1))) {
    stop("treatment must be binary 0/1.", call. = FALSE)
  }
  W <- if (is.null(covariates)) matrix(0, nrow = n, ncol = 1) else {
    as.matrix(covariates)
  }
  if (nrow(W) != n) {
    stop("covariates must have one row per observation.", call. = FALSE)
  }

  total <- morie_tmle_ate(y, A, W, trunc = trunc)

  Dm <- cbind(1, A, W)
  bm <- qr.coef(qr(Dm), M); bm[is.na(bm)] <- 0
  res <- M - as.vector(Dm %*% bm)
  s2 <- mean(res^2)
  if (s2 <= 0) {
    stop("mediator perfectly predicted; the density ratio is undefined.",
         call. = FALSE)
  }
  mu_a <- as.vector(Dm %*% bm)
  mu_0 <- as.vector(cbind(1, 0, W) %*% bm)
  ratio <- pmin(pmax(exp((-(M - mu_0)^2 + (M - mu_a)^2) / (2 * s2)), 0.05), 20)

  g <- pmin(pmax(.morie_logit_fit(W, A), trunc), 1 - trunc)
  w1 <- A / g * ratio
  w0 <- (1 - A) / (1 - g)
  ey1m0 <- sum(w1 * y) / sum(w1)
  ey0m0 <- sum(w0 * y) / sum(w0)
  nde <- ey1m0 - ey0m0
  nie <- total$ate - nde
  list(nde = nde, nie = nie, total = total$ate, se_total = total$se,
       prop_mediated = if (total$ate != 0) nie / total$ate else NA_real_,
       n = n)
}

#' TMLE for the instrumental-variable LATE
#'
#' TMLE is applied twice -- once with the outcome and once with the
#' treatment, both treating the instrument as the exposure -- and the
#' LATE is their ratio. The delta-method influence function combines the
#' two EIFs, which is why dividing the point estimates alone is not
#' enough: the denominator's uncertainty has to enter. Mirrors
#' `morie.fn.tmlivc`.
#'
#' @param y Outcome.
#' @param d Binary 0/1 treatment received.
#' @param z Binary 0/1 instrument assigned.
#' @param covariates Optional baseline covariates.
#' @param trunc Instrument-propensity truncation.
#' @return List with `late`, `se`, `ci`, `itt`, `compliance`, `n`.
#' @references Imbens GW, Angrist JD (1994). Identification and
#'   estimation of local average treatment effects. \emph{Econometrica}
#'   62(2), 467-475.
#' @export
morie_tmle_late <- function(y, d, z, covariates = NULL, trunc = 0.01) {
  y <- as.numeric(y); d <- as.numeric(d); z <- as.numeric(z)
  n <- length(y)
  if (length(d) != n || length(z) != n) {
    stop("y, d and z must have equal length.", call. = FALSE)
  }
  if (!all(d %in% c(0, 1)) || !all(z %in% c(0, 1))) {
    stop("d and z must be binary 0/1.", call. = FALSE)
  }
  W <- if (is.null(covariates)) matrix(0, nrow = n, ncol = 1) else {
    as.matrix(covariates)
  }
  num <- morie_tmle_ate(y, z, W, trunc = trunc)
  den <- morie_tmle_ate(d, z, W, trunc = trunc, scale_outcome = FALSE)
  if (abs(den$ate) < 1e-8) {
    stop("estimated compliance is zero; the LATE is not identified.",
         call. = FALSE)
  }
  late <- num$ate / den$ate
  infl <- (num$eif - late * den$eif) / den$ate
  se <- sqrt(sum(infl^2)) / n
  list(late = late, se = se, ci = c(late - 1.96 * se, late + 1.96 * se),
       itt = num$ate, compliance = den$ate, n = n)
}

#' Sequentially targeted g-computation under time-varying confounding
#'
#' Runs the iterated conditional expectation backwards, fluctuating at
#' *every* time point along the cumulative clever covariate rather than
#' only at the last. That is what makes the longitudinal estimator solve
#' the full efficient influence-function equation. Mirrors
#' `morie.fn.tmltvc`.
#'
#' @param y End-of-follow-up outcome.
#' @param a Binary 0/1 treatment matrix, one column per period.
#' @param l Time-varying confounder matrix of the same shape.
#' @param regime Static regime (scalar or one value per period).
#' @param trunc Treatment-probability truncation.
#' @return List with `estimate`, `epsilons`, `weights`, `regime`,
#'   `n_periods`, `n`.
#' @references van der Laan MJ, Gruber S (2012). Targeted minimum
#'   loss-based estimation of causal effects of multiple time point
#'   interventions. \emph{The International Journal of Biostatistics}
#'   8(1), Article 9.
#' @export
morie_tmle_time_varying <- function(y, a, l, regime = 1, trunc = 0.01) {
  y <- as.numeric(y)
  A <- as.matrix(a); storage.mode(A) <- "double"
  L <- as.matrix(l); storage.mode(L) <- "double"
  n <- length(y); T_ <- ncol(A)
  if (nrow(A) != n || !identical(dim(L), dim(A))) {
    stop("y, a and l shapes disagree.", call. = FALSE)
  }
  if (!all(A %in% c(0, 1))) stop("a must be binary 0/1.", call. = FALSE)
  reg <- rep_len(as.numeric(regime), T_)
  if (trunc <= 0 || trunc >= 0.5) {
    stop("trunc must lie in (0, 0.5).", call. = FALSE)
  }

  gs <- matrix(NA_real_, nrow = n, ncol = T_)
  for (t in seq_len(T_)) {
    past <- cbind(A[, seq_len(t - 1L), drop = FALSE],
                  L[, seq_len(t), drop = FALSE])
    at <- A[, t]
    gs[, t] <- if (min(at) == max(at)) {
      pmin(pmax(if (mean(at) > 0) mean(at) else 1, trunc), 1 - trunc)
    } else {
      pmin(pmax(.morie_logit_fit(past, at), trunc), 1 - trunc)
    }
  }
  regmat <- matrix(reg, nrow = n, ncol = T_, byrow = TRUE)
  p_follow <- ifelse(A == regmat, gs, 1 - gs)
  cum_g <- t(apply(p_follow, 1, cumprod))
  follows <- t(apply((A == regmat) * 1, 1, cumprod))
  if (T_ == 1L) {
    cum_g <- matrix(cum_g, ncol = 1L)
    follows <- matrix(follows, ncol = 1L)
  }

  lo <- min(y); hi <- max(y); span <- hi - lo
  if (span <= 0) stop("outcome has zero range.", call. = FALSE)
  Q <- pmin(pmax((y - lo) / span, 1e-6), 1 - 1e-6)

  eps_all <- numeric(T_)
  for (t in seq(T_, 1L)) {
    X <- cbind(1, A[, seq_len(t), drop = FALSE], L[, seq_len(t), drop = FALSE])
    b <- qr.coef(qr(X), Q); b[is.na(b)] <- 0
    q_obs <- pmin(pmax(as.vector(X %*% b), 1e-6), 1 - 1e-6)
    H <- follows[, t] / cum_g[, t]
    num <- sum(H * (Q - q_obs))
    den <- sum(H^2 * q_obs * (1 - q_obs))
    eps <- if (den > 1e-14) num / den else 0
    eps_all[t] <- eps
    Xa <- cbind(1, A[, seq_len(t - 1L), drop = FALSE], reg[t],
                L[, seq_len(t), drop = FALSE])
    q_reg <- pmin(pmax(as.vector(Xa %*% b), 1e-6), 1 - 1e-6)
    Q <- .morie_tmle_expit(.morie_tmle_logit(q_reg) + eps * (1 / cum_g[, t]))
  }

  list(estimate = mean(Q) * span + lo, epsilons = eps_all,
       weights = follows[, T_] / cum_g[, T_], regime = reg,
       n_periods = T_, n = n)
}

#' Longitudinal TMLE: always-treat minus never-treat
#'
#' Runs [morie_tmle_time_varying()] under the two static regimes and
#' differences them -- the longitudinal analogue of the g-formula
#' contrast, but with a targeting step at every time point rather than
#' plain substitution. Mirrors `morie.fn.tmllng`.
#'
#' @param y,a,l As in [morie_tmle_time_varying()].
#' @param trunc Treatment-probability truncation.
#' @return List with `estimate`, `ey_always`, `ey_never`, `n_periods`,
#'   `n`.
#' @references van der Laan MJ, Gruber S (2012). \emph{The International
#'   Journal of Biostatistics} 8(1), Article 9.
#' @export
morie_tmle_longitudinal <- function(y, a, l, trunc = 0.01) {
  A <- as.matrix(a)
  T_ <- ncol(A)
  hi <- morie_tmle_time_varying(y, a, l, regime = rep(1, T_), trunc = trunc)
  lo <- morie_tmle_time_varying(y, a, l, regime = rep(0, T_), trunc = trunc)
  list(estimate = hi$estimate - lo$estimate, ey_always = hi$estimate,
       ey_never = lo$estimate, n_periods = T_, n = hi$n)
}

#' TMLE for the difference in restricted mean survival time
#'
#' Targets the treatment contrast on the inverse-probability-of-censoring
#' weighted RMST pseudo outcome. The result is a difference in expected
#' months-alive-within-horizon, which unlike a hazard ratio is
#' collapsible and stays interpretable when proportional hazards fails.
#' Mirrors `morie.fn.npstm`.
#'
#' @param time Follow-up times (positive).
#' @param event 1 = event, 0 = censored.
#' @param a Binary 0/1 treatment.
#' @param w Covariates.
#' @param horizon Restriction time; default the 90th percentile of `time`.
#' @param trunc Propensity truncation.
#' @return List with `rmst_difference`, `se`, `ci`, `rmst1`, `rmst0`,
#'   `horizon`, `n_events`, `n`.
#' @references Moore KL, van der Laan MJ (2009). Increasing power in
#'   randomized trials with right censored outcomes through covariate
#'   adjustment. \emph{Journal of Biopharmaceutical Statistics} 19(6),
#'   1099-1131.
#' @export
morie_tmle_rmst <- function(time, event, a, w, horizon = NULL, trunc = 0.01) {
  time <- as.numeric(time); event <- as.numeric(event)
  if (any(time <= 0)) stop("time must be positive.", call. = FALSE)
  if (!all(event %in% c(0, 1))) {
    stop("event must be binary 0/1.", call. = FALSE)
  }
  tau <- if (is.null(horizon)) {
    stats::quantile(time, 0.9, names = FALSE)
  } else as.numeric(horizon)
  pseudo <- .morie_cf_rmst_pseudo(time, event, tau)
  out <- morie_tmle_ate(pseudo, a, w, trunc = trunc)
  list(rmst_difference = out$ate, se = out$se, ci = out$ci,
       rmst1 = out$ey1, rmst0 = out$ey0, horizon = tau,
       n_events = sum(event), n = out$n)
}
