# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native difference-in-differences engines (feat/native-specializations,
# module 14). Replaces fixest (TWFE + event study), did (Callaway-
# Sant'Anna 2021 group-time ATTs), DRDID (Sant'Anna-Zhao 2020 doubly
# robust DiD), bacondecomp (Goodman-Bacon 2021 decomposition),
# DIDmultiplegt (de Chaisemartin-D'Haultfoeuille 2020 DID-M) and
# TwoWayFEWeights (feTR weight diagnostic) with base-R implementations.
#
# The TWFE core is alternating-projection two-way demeaning followed by
# OLS on the demeaned system with CR1 cluster-robust variance using
# fixest's default small-sample correction (adj * cluster.adj with
# fixef.K = "nested"). The Callaway-Sant'Anna engine estimates each
# ATT(g,t) with the Sant'Anna-Zhao panel estimators (dr / reg / ipw)
# and does inference from the analytic influence functions, with an
# optional Mammen multiplier bootstrap identical in structure to
# did::mboot.

#' Internal helper: alternating-projection two-way demeaning
#'
#' Removes unit and time group means from every column of `M` by
#' alternating projections (Guimaraes-Portugal). Exact in one pass for
#' balanced panels; iterates to `tol` otherwise.
#'
#' @srrstats {G3.0} Convergence is assessed against a fixed numeric
#'   tolerance on the maximum absolute change, never by equality
#'   comparison of floating-point values.
#' @noRd
.morie_twfe_demean <- function(M, f1, f2, tol = 1e-11, max_iter = 500L) {
  M <- as.matrix(M)
  # Integer group codes are loop-invariant: coerce once, not per sweep.
  i1 <- as.integer(as.factor(f1)); i2 <- as.integer(as.factor(f2))
  n1 <- tabulate(i1); n2 <- tabulate(i2)
  for (it in seq_len(max_iter)) {
    g1 <- rowsum(M, i1, reorder = TRUE) / n1
    M1 <- M - g1[i1, , drop = FALSE]
    g2 <- rowsum(M1, i2, reorder = TRUE) / n2
    M2 <- M1 - g2[i2, , drop = FALSE]
    delta <- max(abs(M2 - M))
    M <- M2
    if (delta < tol) break
  }
  M
}

#' Internal helper: TWFE OLS with fixest-style CR1 cluster vcov
#'
#' Fits y on X with unit + time fixed effects absorbed, returning the
#' slope coefficients and cluster-robust standard errors under fixest's
#' default small-sample correction: `(n-1)/(n-K) * G/(G-1)` where K
#' counts the slopes plus the fixed-effect coefficients not nested in
#' the cluster variable (fixef.K = "nested").
#'
#' @srrstats {G1.0} Implements the two-way fixed-effects estimator via
#'   the Frisch-Waugh-Lovell theorem; cluster variance per Cameron &
#'   Miller (2015).
#' @noRd
.morie_did_twfe_native <- function(y, X, unit, time, cluster_ids) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  n <- length(y)
  Md <- .morie_twfe_demean(cbind(y, X), unit, time)
  yd <- Md[, 1L]
  Xd <- Md[, -1L, drop = FALSE]
  qrX <- qr(Xd)
  keep <- qrX$pivot[seq_len(qrX$rank)]
  Xk <- Xd[, keep, drop = FALSE]
  XtX_inv <- chol2inv(chol(crossprod(Xk)))
  beta_k <- as.numeric(XtX_inv %*% crossprod(Xk, yd))
  resid <- as.numeric(yd - Xk %*% beta_k)
  # Cluster meat
  cf <- as.factor(cluster_ids)
  G <- nlevels(cf)
  scores <- rowsum(Xk * resid, cf, reorder = FALSE)
  meat <- crossprod(scores)
  # K: slopes + FE coefficients not nested in the cluster. Unit FEs are
  # nested when clustering on unit (the default); time FEs never are.
  n_unit <- length(unique(unit)); n_time <- length(unique(time))
  # A cluster is nested in unit iff every unit maps to a single cluster.
  # The default clusters on unit, where nesting is identically true --
  # short-circuit it; otherwise test by (unit, cluster) pair counts,
  # which is O(n) instead of a per-group character split.
  unit_nested <- identical(cluster_ids, unit) ||
    length(unique(paste(unit, cluster_ids, sep = "\r"))) == n_unit
  fe_K <- if (unit_nested) n_time else n_unit + n_time - 1L
  K <- length(keep) + fe_K
  adj <- (n - 1) / (n - K) * G / (G - 1)
  V <- adj * (XtX_inv %*% meat %*% XtX_inv)
  beta <- rep(NA_real_, ncol(Xd)); beta[keep] <- beta_k
  se <- rep(NA_real_, ncol(Xd)); se[keep] <- sqrt(pmax(diag(V), 0))
  names(beta) <- names(se) <- colnames(Xd)
  list(beta = beta, se = se, vcov = V, keep = keep, residuals = resid,
       n = n, n_clusters = G, df_t = G - 1L,
       n_units = n_unit, n_periods = n_time)
}

# ---------------------------------------------------------------------------
# Sant'Anna-Zhao (2020) doubly robust engines
# ---------------------------------------------------------------------------

#' Internal helper: influence-function standard error, two conventions
#'
#' `"reference"` reproduces the canonical packages (DRDID / did):
#' population-sd scaling, `sd(IF) * sqrt(n-1) / n`. `"bessel"` keeps
#' Bessel's correction: `sd(IF) / sqrt(n)`. The two agree
#' asymptotically; the default is `"reference"` so results match the
#' reference implementations to machine precision.
#' @noRd
.morie_did_if_se <- function(IF, se_convention = c("reference", "bessel")) {
  se_convention <- match.arg(se_convention)
  n <- length(IF)
  if (identical(se_convention, "bessel")) {
    stats::sd(IF) / sqrt(n)
  } else {
    stats::sd(IF) * sqrt(n - 1) / n
  }
}

#' Internal helper: logistic propensity fit + asymptotic linear rep
#' @noRd
.morie_did_ps_fit <- function(D, X) {
  fit <- suppressWarnings(stats::glm.fit(X, D,
                                         family = stats::binomial()))
  ps <- as.numeric(fit$fitted.values)
  ps <- pmin(ps, 1 - 1e-16)
  score <- (D - ps) * X
  hess <- crossprod(X * (ps * (1 - ps)), X) / length(D)
  hinv <- tryCatch(solve(hess), error = function(e) .morie_ginv(hess))
  list(ps = ps, lin_rep = score %*% hinv)
}

#' Internal helper: OLS fit + asymptotic linear rep on a subsample
#' @noRd
.morie_did_or_fit <- function(y, X, subset_w) {
  # Weighted least squares of y on X with 0/1 (or general) weights.
  wX <- X * subset_w
  XpX <- crossprod(wX, X) / length(y)
  XpX_inv <- tryCatch(solve(XpX), error = function(e) .morie_ginv(XpX))
  beta <- as.numeric(XpX_inv %*% (crossprod(wX, y) / length(y)))
  fitted <- as.numeric(X %*% beta)
  lin_rep <- (subset_w * (y - fitted) * X) %*% XpX_inv
  list(fitted = fitted, beta = beta, lin_rep = lin_rep)
}

#' Internal helper: Sant'Anna-Zhao doubly robust DiD, panel version
#'
#' `dy` is the outcome change, `D` the treatment dummy, `X` the
#' intercept-prepended covariate matrix. Returns the ATT and its
#' influence function (the same estimand and IF as
#' `DRDID::drdid_panel`).
#'
#' @srrstats {G1.0} Sant'Anna & Zhao (2020), Journal of Econometrics
#'   219(1) — improved doubly robust DiD, panel estimand.
#' @noRd
.morie_drdid_panel_native <- function(dy, D, X) {
  n <- length(dy)
  psf <- .morie_did_ps_fit(D, X)
  ps <- psf$ps
  orf <- .morie_did_or_fit(dy, X, subset_w = as.numeric(D == 0))
  m <- orf$fitted
  w_treat <- D
  w_cont <- ps * (1 - D) / (1 - ps)
  att_treat <- mean(w_treat * (dy - m)) / mean(w_treat)
  att_cont <- mean(w_cont * (dy - m)) / mean(w_cont)
  att <- att_treat - att_cont
  # Influence function (DRDID::drdid_panel structure)
  inf_treat_1 <- (w_treat * (dy - m) - w_treat * att_treat) / mean(w_treat)
  M1 <- colMeans(w_treat * X) / mean(w_treat)
  inf_treat_or <- -as.numeric(orf$lin_rep %*% M1)
  inf_treat <- inf_treat_1 + inf_treat_or
  inf_cont_1 <- (w_cont * (dy - m) - w_cont * att_cont) / mean(w_cont)
  M2 <- colMeans(w_cont * (dy - m - att_cont) * X) / mean(w_cont)
  inf_cont_ps <- as.numeric(psf$lin_rep %*% M2)
  M3 <- colMeans(w_cont * X) / mean(w_cont)
  inf_cont_or <- -as.numeric(orf$lin_rep %*% M3)
  inf_cont <- inf_cont_1 + inf_cont_ps + inf_cont_or
  IF <- inf_treat - inf_cont
  list(att = att, IF = IF, se = .morie_did_if_se(IF))
}

#' Internal helper: outcome-regression DiD, panel version
#'
#' The `est_method = "reg"` estimand of `did::att_gt` /
#' `DRDID::reg_did_panel`.
#' @noRd
.morie_reg_did_panel_native <- function(dy, D, X) {
  n <- length(dy)
  orf <- .morie_did_or_fit(dy, X, subset_w = as.numeric(D == 0))
  m <- orf$fitted
  w_treat <- D
  w_cont <- D
  att_treat <- mean(w_treat * dy) / mean(w_treat)
  att_cont <- mean(w_cont * m) / mean(w_cont)
  att <- att_treat - att_cont
  inf_treat <- (w_treat * dy - w_treat * att_treat) / mean(w_treat)
  M1 <- colMeans(w_cont * X) / mean(w_cont)
  inf_cont_or <- as.numeric(orf$lin_rep %*% M1)
  inf_cont <- (w_cont * m - w_cont * att_cont) / mean(w_cont) + inf_cont_or
  IF <- inf_treat - inf_cont
  list(att = att, IF = IF, se = .morie_did_if_se(IF))
}

#' Internal helper: standardized IPW DiD, panel version
#'
#' The `est_method = "ipw"` estimand of `did::att_gt` (Abadie 2005 with
#' standardized weights; `DRDID::std_ipw_did_panel`).
#' @noRd
.morie_ipw_did_panel_native <- function(dy, D, X) {
  n <- length(dy)
  psf <- .morie_did_ps_fit(D, X)
  ps <- psf$ps
  w_treat <- D
  w_cont <- ps * (1 - D) / (1 - ps)
  att_treat <- mean(w_treat * dy) / mean(w_treat)
  att_cont <- mean(w_cont * dy) / mean(w_cont)
  att <- att_treat - att_cont
  inf_treat <- (w_treat * dy - w_treat * att_treat) / mean(w_treat)
  inf_cont_1 <- (w_cont * dy - w_cont * att_cont) / mean(w_cont)
  M2 <- colMeans(w_cont * (dy - att_cont) * X) / mean(w_cont)
  inf_cont_ps <- as.numeric(psf$lin_rep %*% M2)
  IF <- inf_treat - inf_cont_1 - inf_cont_ps
  list(att = att, IF = IF, se = .morie_did_if_se(IF))
}

#' Internal helper: Sant'Anna-Zhao locally efficient DR DiD,
#' repeated-cross-section version
#'
#' The estimand and influence function of `DRDID::drdid_rc` (the
#' locally efficient variant): a logistic propensity model plus four
#' outcome regressions (control/treated x pre/post).
#' @noRd
.morie_drdid_rc_native <- function(y, post, D, X) {
  n <- length(y)
  psf <- .morie_did_ps_fit(D, X)
  ps <- psf$ps
  or_c0 <- .morie_did_or_fit(y, X, as.numeric(D == 0 & post == 0))
  or_c1 <- .morie_did_or_fit(y, X, as.numeric(D == 0 & post == 1))
  or_t0 <- .morie_did_or_fit(y, X, as.numeric(D == 1 & post == 0))
  or_t1 <- .morie_did_or_fit(y, X, as.numeric(D == 1 & post == 1))
  mu_c0 <- or_c0$fitted; mu_c1 <- or_c1$fitted
  mu_t0 <- or_t0$fitted; mu_t1 <- or_t1$fitted
  mu_c <- post * mu_c1 + (1 - post) * mu_c0
  lam <- mean(post)
  # Weights (DRDID::drdid_rc)
  w_treat_pre  <- D * (1 - post)
  w_treat_post <- D * post
  w_cont_pre  <- ps * (1 - D) * (1 - post) / (1 - ps)
  w_cont_post <- ps * (1 - D) * post / (1 - ps)
  w_d <- D
  w_dt1 <- D * post
  w_dt0 <- D * (1 - post)
  eta_treat_pre  <- w_treat_pre  * (y - mu_c) / mean(w_treat_pre)
  eta_treat_post <- w_treat_post * (y - mu_c) / mean(w_treat_post)
  eta_cont_pre   <- w_cont_pre   * (y - mu_c) / mean(w_cont_pre)
  eta_cont_post  <- w_cont_post  * (y - mu_c) / mean(w_cont_post)
  # Local-efficiency adjustment terms
  eta_d_post  <- w_d   * (mu_t1 - mu_c1) / mean(w_d)
  eta_dt1_post <- w_dt1 * (mu_t1 - mu_c1) / mean(w_dt1)
  eta_d_pre  <- w_d   * (mu_t0 - mu_c0) / mean(w_d)
  eta_dt0_pre <- w_dt0 * (mu_t0 - mu_c0) / mean(w_dt0)
  att_treat_pre  <- mean(eta_treat_pre)
  att_treat_post <- mean(eta_treat_post)
  att_cont_pre   <- mean(eta_cont_pre)
  att_cont_post  <- mean(eta_cont_post)
  att_d_post  <- mean(eta_d_post)
  att_dt1_post <- mean(eta_dt1_post)
  att_d_pre  <- mean(eta_d_pre)
  att_dt0_pre <- mean(eta_dt0_pre)
  att <- (att_treat_post - att_treat_pre) -
    (att_cont_post - att_cont_pre) +
    (att_d_post - att_dt1_post) - (att_d_pre - att_dt0_pre)
  # --- Influence function (verbatim structure of DRDID::drdid_rc) ---
  inf_treat_pre  <- eta_treat_pre - w_treat_pre * att_treat_pre /
    mean(w_treat_pre)
  inf_treat_post <- eta_treat_post - w_treat_post * att_treat_post /
    mean(w_treat_post)
  M1_post <- -colMeans(w_treat_post * X) / mean(w_treat_post)
  M1_pre  <- -colMeans(w_treat_pre * X) / mean(w_treat_pre)
  inf_treat_or_post <- as.numeric(or_c1$lin_rep %*% M1_post)
  inf_treat_or_pre  <- as.numeric(or_c0$lin_rep %*% M1_pre)
  inf_treat_or <- inf_treat_or_post + inf_treat_or_pre
  inf_treat <- inf_treat_post - inf_treat_pre + inf_treat_or
  # Control components (estimation effects of ps and mu_c)
  inf_cont_pre  <- eta_cont_pre - w_cont_pre * att_cont_pre /
    mean(w_cont_pre)
  inf_cont_post <- eta_cont_post - w_cont_post * att_cont_post /
    mean(w_cont_post)
  M2_pre <- colMeans(w_cont_pre * (y - mu_c - att_cont_pre) * X) /
    mean(w_cont_pre)
  M2_post <- colMeans(w_cont_post * (y - mu_c - att_cont_post) * X) /
    mean(w_cont_post)
  inf_cont_ps <- as.numeric(psf$lin_rep %*% (M2_post - M2_pre))
  M3_post <- -colMeans(w_cont_post * X) / mean(w_cont_post)
  M3_pre  <- -colMeans(w_cont_pre * X) / mean(w_cont_pre)
  inf_cont_or <- as.numeric(or_c1$lin_rep %*% M3_post) +
    as.numeric(or_c0$lin_rep %*% M3_pre)
  inf_cont <- inf_cont_post - inf_cont_pre + inf_cont_ps + inf_cont_or
  # Efficiency-adjustment components
  inf_eff1 <- eta_d_post - w_d * att_d_post / mean(w_d)
  inf_eff2 <- eta_dt1_post - w_dt1 * att_dt1_post / mean(w_dt1)
  inf_eff3 <- eta_d_pre - w_d * att_d_pre / mean(w_d)
  inf_eff4 <- eta_dt0_pre - w_dt0 * att_dt0_pre / mean(w_dt0)
  inf_eff <- (inf_eff1 - inf_eff2) - (inf_eff3 - inf_eff4)
  mom_post <- colMeans((w_d / mean(w_d) - w_dt1 / mean(w_dt1)) * X)
  mom_pre  <- colMeans((w_d / mean(w_d) - w_dt0 / mean(w_dt0)) * X)
  inf_or <- as.numeric((or_t1$lin_rep - or_c1$lin_rep) %*% mom_post) -
    as.numeric((or_t0$lin_rep - or_c0$lin_rep) %*% mom_pre)
  IF <- inf_treat - inf_cont + inf_eff + inf_or
  list(att = att, IF = IF, se = .morie_did_if_se(IF))
}

# ---------------------------------------------------------------------------
# Callaway-Sant'Anna (2021) group-time ATTs
# ---------------------------------------------------------------------------

#' Internal helper: Mammen multiplier bootstrap over an IF matrix
#'
#' Structure of `did::mboot`: draw Mammen(1993) two-point weights per
#' unit, perturb the influence functions, and report the robust
#' (IQR-based) standard deviation of the bootstrap draws.
#' @noRd
.morie_did_mboot <- function(IF_mat, biters = 999L, seed = NULL,
                             cluster = NULL) {
  IF_mat <- as.matrix(IF_mat)
  n <- nrow(IF_mat)
  if (!is.null(cluster)) {
    IF_mat <- rowsum(IF_mat, cluster, reorder = FALSE) /
      as.numeric(table(cluster)[unique(as.character(cluster))])
    n <- nrow(IF_mat)
  }
  if (!is.null(seed)) set.seed(seed)
  sq5 <- sqrt(5)
  k1 <- 0.5 * (1 - sq5); k2 <- 0.5 * (1 + sq5)
  p_k1 <- 0.5 * (1 + sq5) / sq5
  boot <- matrix(NA_real_, biters, ncol(IF_mat))
  for (b in seq_len(biters)) {
    v <- ifelse(stats::runif(n) < p_k1, k1, k2)
    boot[b, ] <- sqrt(n) * colMeans(v * IF_mat)
  }
  se <- apply(boot, 2L, function(z) {
    (stats::quantile(z, 0.75, na.rm = TRUE, names = FALSE) -
       stats::quantile(z, 0.25, na.rm = TRUE, names = FALSE)) /
      (stats::qnorm(0.75) - stats::qnorm(0.25))
  }) / sqrt(n)
  list(se = as.numeric(se), boot = boot)
}

#' Internal helper: native Callaway-Sant'Anna ATT(g,t)
#'
#' Computes group-time average treatment effects on a (possibly
#' unbalanced) panel using the Sant'Anna-Zhao panel estimators on each
#' two-period comparison. Base period is "varying" (t-1 for
#' pre-treatment periods, g-1 for post-treatment ones), matching
#' `did::att_gt`'s default.
#'
#' @srrstats {G1.0} Callaway & Sant'Anna (2021), Journal of
#'   Econometrics 225(2) 200-230.
#' @noRd
.morie_attgt_native <- function(data, outcome, unit, time, gname,
                                covariates = NULL,
                                est_method = "dr",
                                control_group = "nevertreated",
                                biters = 999L, seed = NULL,
                                alpha = 0.05,
                                se_convention = "reference") {
  df <- as.data.frame(data)
  g_all <- df[[gname]]
  tlist <- sort(unique(df[[time]]))
  glist <- sort(unique(g_all[g_all > 0]))
  # Groups treated before the second period have no pre-period.
  glist <- glist[glist > tlist[1L]]
  ids <- sort(unique(df[[unit]]))
  n_ids <- length(ids)
  engine <- switch(est_method,
                   dr = .morie_drdid_panel_native,
                   reg = .morie_reg_did_panel_native,
                   ipw = .morie_ipw_did_panel_native,
                   stop("Unknown est_method: ", est_method))
  rows <- list()
  IF_cols <- list()
  for (g in glist) {
    for (tt in tlist[tlist != tlist[1L]]) {
      pret <- if (tt >= g) {
        # post-treatment: base period is the last pre-treatment period
        max(tlist[tlist < g])
      } else {
        # pre-treatment: varying base period t-1
        max(tlist[tlist < tt])
      }
      if (pret >= g) next
      # Control units: never-treated, or additionally not-yet-treated
      # by max(t, pret).
      is_control <- if (identical(control_group, "nevertreated")) {
        g_all == 0
      } else {
        (g_all == 0) | (g_all > max(tt, pret) & g_all != g)
      }
      keep_unit <- (g_all == g) | is_control
      sub <- df[keep_unit & df[[time]] %in% c(pret, tt), , drop = FALSE]
      # Units observed in both periods
      cnt <- table(sub[[unit]])
      both <- names(cnt)[cnt == 2L]
      sub <- sub[as.character(sub[[unit]]) %in% both, , drop = FALSE]
      sub <- sub[order(sub[[unit]], sub[[time]]), , drop = FALSE]
      pre_rows <- sub[sub[[time]] == pret, , drop = FALSE]
      post_rows <- sub[sub[[time]] == tt, , drop = FALSE]
      if (nrow(pre_rows) == 0L) next
      dy <- as.numeric(post_rows[[outcome]]) -
        as.numeric(pre_rows[[outcome]])
      D <- as.numeric(pre_rows[[gname]] == g)
      if (sum(D) == 0L || sum(1 - D) == 0L) next
      X <- if (length(covariates)) {
        cbind(1, as.matrix(pre_rows[, covariates, drop = FALSE]))
      } else {
        matrix(1, nrow = length(dy), ncol = 1L)
      }
      storage.mode(X) <- "double"
      fit <- engine(dy, D, X)
      # Map the subsample IF back to the full unit list, scaled by
      # n/n_sub (did's convention so that Var = mean(IF^2)/n).
      IF_full <- numeric(n_ids)
      pos <- match(as.character(pre_rows[[unit]]), as.character(ids))
      IF_full[pos] <- fit$IF * (n_ids / length(dy))
      rows[[length(rows) + 1L]] <- data.frame(
        group = g, t = tt, att = fit$att,
        se_analytic = if (identical(se_convention, "bessel"))
          stats::sd(IF_full) / sqrt(n_ids)
        else sqrt(mean(IF_full^2) / n_ids))
      IF_cols[[length(IF_cols) + 1L]] <- IF_full
    }
  }
  if (!length(rows)) {
    return(list(results = data.frame(group = numeric(), t = numeric(),
                                     att = numeric(), se = numeric()),
                IF = NULL))
  }
  res <- do.call(rbind, rows)
  IF_mat <- do.call(cbind, IF_cols)
  if (biters > 0L) {
    mb <- .morie_did_mboot(IF_mat, biters = biters, seed = seed)
    res$se <- mb$se
    bad <- !is.finite(res$se) | res$se <= 0
    res$se[bad] <- res$se_analytic[bad]
  } else {
    res$se <- res$se_analytic
  }
  list(results = res, IF = IF_mat, n = n_ids)
}

# ---------------------------------------------------------------------------
# de Chaisemartin & D'Haultfoeuille (2020) DID-M
# ---------------------------------------------------------------------------

#' Internal helper: the DID-M point estimator on group-level panel data
#'
#' Instantaneous treatment effect for switchers: for every pair of
#' consecutive periods, compares outcome changes of joiners (0 to 1)
#' with stable-at-0 groups and leavers (1 to 0) with stable-at-1
#' groups, weighting by switcher counts.
#'
#' @srrstats {G1.0} de Chaisemartin & D'Haultfoeuille (2020), American
#'   Economic Review 110(9) 2964-2996 (the DID_M estimand).
#' @noRd
.morie_didm_point <- function(df, outcome, treatment, unit, time) {
  # Collapse to group x time cells (mean outcome, mean treatment,
  # cell size), as did_multiplegt does internally.
  cell <- stats::aggregate(df[, c(outcome, treatment)],
                           by = list(.g = df[[unit]], .t = df[[time]]),
                           FUN = mean)
  cnt <- stats::aggregate(list(.n = rep(1L, nrow(df))),
                          by = list(.g = df[[unit]], .t = df[[time]]),
                          FUN = sum)
  cell <- merge(cell, cnt, by = c(".g", ".t"))
  tlist <- sort(unique(cell$.t))
  num <- 0; den <- 0
  for (k in seq_along(tlist)[-1L]) {
    t0 <- tlist[k - 1L]; t1 <- tlist[k]
    a <- cell[cell$.t == t0, , drop = FALSE]
    b <- cell[cell$.t == t1, , drop = FALSE]
    m <- merge(a, b, by = ".g", suffixes = c("_0", "_1"))
    if (!nrow(m)) next
    d0 <- m[[paste0(treatment, "_0")]]
    d1 <- m[[paste0(treatment, "_1")]]
    dy <- m[[paste0(outcome, "_1")]] - m[[paste0(outcome, "_0")]]
    w  <- m$.n_1
    join   <- d0 == 0 & d1 == 1
    stay0  <- d0 == 0 & d1 == 0
    leave  <- d0 == 1 & d1 == 0
    stay1  <- d0 == 1 & d1 == 1
    if (any(join) && any(stay0)) {
      did_plus <- stats::weighted.mean(dy[join], w[join]) -
        stats::weighted.mean(dy[stay0], w[stay0])
      n_plus <- sum(w[join])
      num <- num + n_plus * did_plus
      den <- den + n_plus
    }
    if (any(leave) && any(stay1)) {
      did_minus <- stats::weighted.mean(dy[stay1], w[stay1]) -
        stats::weighted.mean(dy[leave], w[leave])
      n_minus <- sum(w[leave])
      num <- num + n_minus * did_minus
      den <- den + n_minus
    }
  }
  if (den == 0) return(NA_real_)
  num / den
}

#' Internal helper: DID-M with cluster (group) bootstrap SE
#' @noRd
.morie_didm_native <- function(df, outcome, treatment, unit, time,
                               n_bootstrap = 200L, seed = 42L) {
  est <- .morie_didm_point(df, outcome, treatment, unit, time)
  se <- NA_real_
  if (n_bootstrap > 0L) {
    set.seed(seed)
    groups <- unique(df[[unit]])
    boot <- numeric(n_bootstrap)
    for (b in seq_len(n_bootstrap)) {
      gs <- sample(groups, length(groups), replace = TRUE)
      pieces <- lapply(seq_along(gs), function(i) {
        piece <- df[df[[unit]] == gs[i], , drop = FALSE]
        piece[[unit]] <- paste0("b", i)
        piece
      })
      bdf <- do.call(rbind, pieces)
      boot[b] <- .morie_didm_point(bdf, outcome, treatment, unit, time)
    }
    boot <- boot[is.finite(boot)]
    if (length(boot) > 1L) se <- stats::sd(boot)
  }
  list(effect = est, se_effect = se)
}

# ---------------------------------------------------------------------------
# Goodman-Bacon (2021) decomposition
# ---------------------------------------------------------------------------

#' Internal helper: Goodman-Bacon decomposition on a balanced panel
#'
#' Enumerates every 2x2 timing comparison; each pair's estimate is the
#' 2x2 DiD on the relevant subsample/window and its weight follows from
#' the Frisch-Waugh-Lovell identity: proportional to the subsample size
#' squared times the variance of the within-demeaned treatment.
#'
#' @srrstats {G1.0} Goodman-Bacon (2021), Journal of Econometrics
#'   225(2) 254-277.
#' @noRd
.morie_bacon_native <- function(data, outcome, treatment, unit, time) {
  df <- as.data.frame(data)[, c(outcome, treatment, unit, time)]
  names(df) <- c(".y", ".d", ".g", ".t")
  df <- df[stats::complete.cases(df), , drop = FALSE]
  # Treatment-onset time per unit (Inf = never treated)
  onset <- vapply(split(df, df$.g), function(u) {
    tr <- u$.t[u$.d == 1]
    if (length(tr)) min(tr) else Inf
  }, numeric(1))
  df$.onset <- onset[as.character(df$.g)]
  timing <- sort(unique(df$.onset[is.finite(df$.onset)]))
  cohorts <- c(timing, Inf)
  two_by_two <- function(sub) {
    # 2x2 DiD via FWL on the demeaned system; also return the weight
    # ingredient n^2 * var(demeaned D).
    Dd <- .morie_twfe_demean(cbind(sub$.d, sub$.y), sub$.g, sub$.t)
    dtil <- Dd[, 1L]; ytil <- Dd[, 2L]
    vd <- sum(dtil^2)
    if (vd < 1e-12) return(NULL)
    list(est = sum(dtil * ytil) / vd,
         wt_raw = vd / nrow(sub) * nrow(sub)^2)
  }
  rows <- list()
  for (i in seq_along(cohorts)) {
    for (j in seq_along(cohorts)) {
      if (i == j) next
      k <- cohorts[i]; l <- cohorts[j]
      if (!is.finite(k)) next   # "treated" side must be a real cohort
      if (is.finite(l) && k >= l) {
        # k = later-treated vs l = earlier-treated: usable window is
        # l's post-period only (earlier group's treatment is constant
        # there), i.e. periods >= l... handled from the (l, k) loop
        # iteration with roles swapped; skip the duplicate here.
        next
      }
      sub <- df[df$.onset %in% c(k, l), , drop = FALSE]
      if (is.finite(l)) {
        # Earlier (k) vs later (l): two windows.
        # (a) k treated vs l acting as control, before l treats.
        sub_a <- sub[sub$.t < l, , drop = FALSE]
        tt <- two_by_two(sub_a)
        if (!is.null(tt)) {
          rows[[length(rows) + 1L]] <- data.frame(
            treated = k, untreated = l,
            type = "Earlier vs Later Treated",
            estimate = tt$est, wt_raw = tt$wt_raw)
        }
        # (b) l treated vs k acting as (already-treated) control,
        # after k treats.
        sub_b <- sub[sub$.t >= k, , drop = FALSE]
        # Recode: treatment variation now comes from l's onset.
        tt <- two_by_two(sub_b)
        if (!is.null(tt)) {
          rows[[length(rows) + 1L]] <- data.frame(
            treated = l, untreated = k,
            type = "Later vs Earlier Treated",
            estimate = tt$est, wt_raw = tt$wt_raw)
        }
      } else {
        # Treated cohort vs never-treated
        tt <- two_by_two(sub)
        if (!is.null(tt)) {
          rows[[length(rows) + 1L]] <- data.frame(
            treated = k, untreated = Inf,
            type = "Treated vs Untreated",
            estimate = tt$est, wt_raw = tt$wt_raw)
        }
      }
    }
  }
  if (!length(rows)) {
    return(data.frame(treated = numeric(), untreated = numeric(),
                      type = character(), estimate = numeric(),
                      weight = numeric()))
  }
  out <- do.call(rbind, rows)
  out$weight <- out$wt_raw / sum(out$wt_raw)
  out$wt_raw <- NULL
  rownames(out) <- NULL
  out
}

# ---------------------------------------------------------------------------
# TwoWayFEWeights (feTR) diagnostic
# ---------------------------------------------------------------------------

#' Internal helper: feTR weights on the TWFE estimand
#'
#' Weights attached by the TWFE regression to each treated (g,t) cell:
#' proportional to the cell's residual from regressing the treatment on
#' unit and time fixed effects, normalized to sum to one over treated
#' cells (de Chaisemartin & D'Haultfoeuille 2020, feTR).
#' @noRd
.morie_twfe_weights_native <- function(df, group, time, treatment) {
  cell <- stats::aggregate(df[, treatment, drop = FALSE],
                           by = list(.g = df[[group]], .t = df[[time]]),
                           FUN = mean)
  cnt <- stats::aggregate(list(.n = rep(1L, nrow(df))),
                          by = list(.g = df[[group]], .t = df[[time]]),
                          FUN = sum)
  cell <- merge(cell, cnt, by = c(".g", ".t"))
  d <- cell[[treatment]]
  # N-weighted two-way demeaning of D on group + time FE
  w <- cell$.n / sum(cell$.n)
  dm <- d
  for (it in seq_len(500L)) {
    old <- dm
    gm <- tapply(dm * cell$.n, cell$.g, sum) /
      tapply(cell$.n, cell$.g, sum)
    dm <- dm - as.numeric(gm[as.character(cell$.g)])
    tm <- tapply(dm * cell$.n, cell$.t, sum) /
      tapply(cell$.n, cell$.t, sum)
    dm <- dm - as.numeric(tm[as.character(cell$.t)])
    if (max(abs(dm - old)) < 1e-12) break
  }
  treated <- d != 0
  denom <- sum(w[treated] * d[treated] * dm[treated])
  weights <- rep(NA_real_, nrow(cell))
  weights[treated] <- (w[treated] * d[treated] * dm[treated]) / denom
  data.frame(group = cell$.g, time = cell$.t,
             treatment = d, n = cell$.n, weight = weights)
}
