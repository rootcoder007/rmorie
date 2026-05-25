# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) morie contributors
#
# This file is part of morie. morie is free software: you can
# redistribute it and/or modify it under the terms of the GNU Affero
# General Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later
# version. See LICENSE for the full text.

#' Comprehensive effect-size calculations
#'
#' Effect-size estimators used in biomedical and social-science
#' research, each with analytic or bootstrap confidence intervals.
#'
#' Families: standardised mean differences (Cohen's d, Hedges' g,
#' Glass's delta); common-language ES (CLES); correlation-based (r,
#' R^2, eta^2, partial eta^2, omega^2, epsilon^2); contingency
#' (OR, RR, RD, NNT, NNH, rate ratio, IRD); association (Cohen's w,
#' Cramer's V, phi); non-parametric (rank-biserial, Cliff's delta,
#' Vargha-Delaney A); regression (standardised beta, CV); and meta-
#' analysis (fixed-/random-effects pooling, I^2, prediction interval).
#'
#' @references
#' Cohen (1988); Hedges & Olkin (1985); Borenstein et al. (2009);
#' Vargha & Delaney (2000); DerSimonian & Laird (1986).
#' @name effect_sizes
NULL


# -- Result container -------------------------------------------------

#' Build an effect-size result
#'
#' Returns a named-list with class `c("morie_effect_size", "list")`.
#'
#' @param measure   Name of the effect-size statistic.
#' @param estimate  Point estimate (numeric).
#' @param ci_lower  Lower confidence bound (or NA).
#' @param ci_upper  Upper confidence bound (or NA).
#' @param se        Standard error (or NA).
#' @param n         Sample size (or NA).
#' @param extra     Named list of additional outputs.
#' @return A `morie_effect_size` named-list.
#' @export
effect_size_result <- function(measure, estimate,
                                ci_lower = NA_real_, ci_upper = NA_real_,
                                se = NA_real_, n = NA_integer_,
                                extra = list()) {
  structure(
    list(measure = measure, estimate = as.numeric(estimate),
         ci_lower = as.numeric(ci_lower), ci_upper = as.numeric(ci_upper),
         se = as.numeric(se), n = n, extra = extra),
    class = c("morie_effect_size", "list")
  )
}


# -- Helpers ----------------------------------------------------------

.arr <- function(x) {
  v <- as.numeric(x)
  v[is.finite(v)]
}

.bootstrap_ci <- function(func, args, n_boot = 2000L,
                            confidence = 0.95, seed = 42L) {
  set.seed(seed)
  boot_vals <- rep(NA_real_, n_boot)
  for (b in seq_len(n_boot)) {
    resampled <- lapply(args, function(a) a[sample.int(length(a),
                                                          length(a),
                                                          replace = TRUE)])
    boot_vals[b] <- tryCatch(do.call(func, resampled),
                              error = function(e) NA_real_)
  }
  boot_vals <- boot_vals[is.finite(boot_vals)]
  if (length(boot_vals) == 0) {
    return(list(se = 0, ci_lo = NA_real_, ci_hi = NA_real_))
  }
  alpha <- (1 - confidence) / 2
  list(
    se    = sd(boot_vals),
    ci_lo = as.numeric(quantile(boot_vals, alpha)),
    ci_hi = as.numeric(quantile(boot_vals, 1 - alpha))
  )
}


# =====================================================================
# STANDARDISED MEAN DIFFERENCES
# =====================================================================

#' Cohen's d for independent samples
#'
#' @param x,y Numeric vectors (NA dropped).
#' @param confidence Confidence level for CI. Default 0.95.
#' @return A `morie_effect_size`.
#' @export
cohens_d <- function(x, y, confidence = 0.95) {
  x <- .arr(x)
  y <- .arr(y)
  nx <- length(x)
  ny <- length(y)
  if (nx < 2L || ny < 2L) {
    stop("cohens_d: need at least 2 finite observations per group; ",
         "got nx=", nx, ", ny=", ny, call. = FALSE)
  }
  sp <- sqrt(((nx - 1) * var(x) + (ny - 1) * var(y)) / (nx + ny - 2))
  d  <- if (sp > 0) (mean(x) - mean(y)) / sp else 0
  se <- sqrt((nx + ny) / (nx * ny) + d^2 / (2 * (nx + ny - 2)))
  z  <- qnorm((1 + confidence) / 2)
  effect_size_result("Cohen's d", d, d - z * se, d + z * se, se, nx + ny)
}


#' Hedges' g — bias-corrected Cohen's d
#'
#' Applies J = 1 - 3 / (4 * df - 1).
#'
#' @inheritParams cohens_d
#' @return A `morie_effect_size`.
#' @export
hedges_g <- function(x, y, confidence = 0.95) {
  x <- .arr(x)
  y <- .arr(y)
  d_res <- cohens_d(x, y, confidence)
  df_val <- length(x) + length(y) - 2
  J <- if (df_val > 1) 1 - 3 / (4 * df_val - 1) else 1
  g  <- d_res$estimate * J
  se <- if (!is.na(d_res$se)) d_res$se * J else 0
  z  <- qnorm((1 + confidence) / 2)
  effect_size_result("Hedges' g", g, g - z * se, g + z * se, se,
                      d_res$n, extra = list(correction_factor = J))
}


#' Glass's delta — control-group SD denominator
#'
#' @inheritParams cohens_d
#' @param control Which group is the control: `"x"` or `"y"` (default).
#' @return A `morie_effect_size`.
#' @export
glass_delta <- function(x, y, control = "y", confidence = 0.95) {
  x <- .arr(x)
  y <- .arr(y)
  ctrl <- if (control == "y") y else x
  sd_ctrl <- sd(ctrl)
  delta <- if (sd_ctrl > 0) (mean(x) - mean(y)) / sd_ctrl else 0
  n_ctrl <- length(ctrl)
  se <- sqrt(1 / length(x) + 1 / length(y) + delta^2 / (2 * (n_ctrl - 1)))
  z  <- qnorm((1 + confidence) / 2)
  effect_size_result("Glass's delta", delta, delta - z * se, delta + z * se,
                      se, length(x) + length(y))
}


# =====================================================================
# COMMON LANGUAGE EFFECT SIZE
# =====================================================================

#' Common Language Effect Size (probability of superiority)
#'
#' Estimates P(X > Y) for randomly drawn observations from each group.
#'
#' @inheritParams cohens_d
#' @return A `morie_effect_size`.
#' @export
cles <- function(x, y, confidence = 0.95) {
  x <- .arr(x)
  y <- .arr(y)
  nx <- length(x)
  ny <- length(y)
  diff_mat <- outer(x, y, "-")
  count <- sum(diff_mat > 0)
  ties  <- sum(diff_mat == 0)
  p_sup <- if (nx * ny > 0) (count + 0.5 * ties) / (nx * ny) else 0.5
  boot <- .bootstrap_ci(
    function(a, b) sum(outer(a, b, "-") > 0) / (length(a) * length(b)),
    list(x, y), confidence = confidence
  )
  effect_size_result("CLES (Prob. of superiority)", p_sup,
                      boot$ci_lo, boot$ci_hi, boot$se, nx + ny)
}


# =====================================================================
# CORRELATION-BASED
# =====================================================================

#' Pearson r as an effect size with Fisher-z CI
#'
#' @inheritParams cohens_d
#' @return A `morie_effect_size`.
#' @export
r_effect_size <- function(x, y, confidence = 0.95) {
  x <- .arr(x)
  y <- .arr(y)
  n <- min(length(x), length(y))
  x <- x[seq_len(n)]
  y <- y[seq_len(n)]
  r <- cor(x, y)
  z_r <- atanh(r)
  se_z <- if (n > 3) 1 / sqrt(n - 3) else Inf
  z_crit <- qnorm((1 + confidence) / 2)
  effect_size_result("Pearson r", r,
                      tanh(z_r - z_crit * se_z),
                      tanh(z_r + z_crit * se_z), se_z, n)
}


#' Coefficient of determination R^2
#'
#' @inheritParams cohens_d
#' @return A `morie_effect_size`.
#' @export
r_squared <- function(x, y) {
  r_res <- r_effect_size(x, y)
  r2 <- r_res$estimate^2
  effect_size_result(
    "R-squared", r2,
    if (!is.na(r_res$ci_lower)) r_res$ci_lower^2 else NA_real_,
    if (!is.na(r_res$ci_upper)) r_res$ci_upper^2 else NA_real_,
    n = r_res$n)
}


#' Eta-squared from ANOVA sums of squares
#'
#' @param ss_effect Sum of squares for the effect.
#' @param ss_total  Total sum of squares.
#' @return A `morie_effect_size`.
#' @export
eta_squared <- function(ss_effect, ss_total) {
  eta2 <- if (ss_total > 0) ss_effect / ss_total else 0
  effect_size_result("Eta-squared", eta2)
}


#' Partial eta-squared
#'
#' @param ss_effect Sum of squares for the effect.
#' @param ss_error  Error sum of squares.
#' @return A `morie_effect_size`.
#' @export
partial_eta_squared <- function(ss_effect, ss_error) {
  denom <- ss_effect + ss_error
  pe2 <- if (denom > 0) ss_effect / denom else 0
  effect_size_result("Partial eta-squared", pe2)
}


#' Omega-squared — less biased than eta-squared
#'
#' @param ss_effect,ss_total Sums of squares.
#' @param df_effect Numerator d.f. of the effect.
#' @param ms_error  Error mean square.
#' @return A `morie_effect_size`.
#' @export
omega_squared <- function(ss_effect, ss_total, df_effect, ms_error) {
  num   <- ss_effect - df_effect * ms_error
  denom <- ss_total + ms_error
  w2 <- if (denom > 0) max(num / denom, 0) else 0
  effect_size_result("Omega-squared", w2)
}


#' Epsilon-squared (Kelley, 1935)
#'
#' @inheritParams omega_squared
#' @return A `morie_effect_size`.
#' @export
epsilon_squared <- function(ss_effect, ss_total, df_effect, ms_error) {
  num <- ss_effect - df_effect * ms_error
  eps2 <- if (ss_total > 0) max(num / ss_total, 0) else 0
  effect_size_result("Epsilon-squared", eps2)
}


# =====================================================================
# CONTINGENCY TABLE EFFECT SIZES
# =====================================================================

#' Odds ratio for a 2x2 table `[[a, b], [c, d]]`
#'
#' @param a,b,c,d Cell counts.
#' @param confidence Confidence level. Default 0.95.
#' @return A `morie_effect_size`.
#' @export
odds_ratio <- function(a, b, c, d, confidence = 0.95) {
  or_val  <- if (b * c > 0) (a * d) / (b * c) else Inf
  log_or  <- if (or_val > 0 && is.finite(or_val)) log(or_val) else 0
  se_log  <- sqrt(1 / max(a, 1) + 1 / max(b, 1) +
                   1 / max(c, 1) + 1 / max(d, 1))
  z <- qnorm((1 + confidence) / 2)
  effect_size_result(
    "Odds ratio", or_val,
    exp(log_or - z * se_log), exp(log_or + z * se_log),
    se_log, a + b + c + d, extra = list(log_or = log_or)
  )
}


#' Risk ratio (relative risk) for a 2x2 table
#'
#' @inheritParams odds_ratio
#' @return A `morie_effect_size`.
#' @export
risk_ratio <- function(a, b, c, d, confidence = 0.95) {
  p1 <- if ((a + b) > 0) a / (a + b) else 0
  p2 <- if ((c + d) > 0) c / (c + d) else 0
  rr <- if (p2 > 0) p1 / p2 else Inf
  log_rr <- if (rr > 0 && is.finite(rr)) log(rr) else 0
  se_log <- if (a > 0 && c > 0)
    sqrt(b / (a * (a + b)) + d / (c * (c + d))) else Inf
  z <- qnorm((1 + confidence) / 2)
  effect_size_result("Risk ratio", rr,
                      exp(log_rr - z * se_log),
                      exp(log_rr + z * se_log),
                      se_log, a + b + c + d)
}


#' Risk difference for a 2x2 table
#'
#' @inheritParams odds_ratio
#' @return A `morie_effect_size`.
#' @export
risk_difference <- function(a, b, c, d, confidence = 0.95) {
  n1 <- a + b
  n2 <- c + d
  p1 <- if (n1 > 0) a / n1 else 0
  p2 <- if (n2 > 0) c / n2 else 0
  rd <- p1 - p2
  se <- if (n1 > 0 && n2 > 0)
    sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2) else 0
  z  <- qnorm((1 + confidence) / 2)
  effect_size_result("Risk difference", rd, rd - z * se, rd + z * se,
                      se, n1 + n2)
}


#' Number needed to treat (NNT) = 1 / |RD|
#'
#' @inheritParams odds_ratio
#' @return A `morie_effect_size`.
#' @export
number_needed_to_treat <- function(a, b, c, d, confidence = 0.95) {
  rd_res <- risk_difference(a, b, c, d, confidence)
  rd <- rd_res$estimate
  nnt   <- if (abs(rd) > 0) 1 / abs(rd) else Inf
  ci_lo <- if (!is.na(rd_res$ci_upper) && abs(rd_res$ci_upper) > 0)
    1 / abs(rd_res$ci_upper) else Inf
  ci_hi <- if (!is.na(rd_res$ci_lower) && abs(rd_res$ci_lower) > 0)
    1 / abs(rd_res$ci_lower) else Inf
  effect_size_result("NNT", nnt, min(ci_lo, ci_hi), max(ci_lo, ci_hi),
                      n = rd_res$n)
}


#' Number needed to harm (NNH) — sign-reversed NNT
#'
#' @inheritParams odds_ratio
#' @return A `morie_effect_size`.
#' @export
number_needed_to_harm <- function(a, b, c, d, confidence = 0.95) {
  result <- number_needed_to_treat(a, b, c, d, confidence)
  effect_size_result("NNH", result$estimate, result$ci_lower,
                      result$ci_upper, n = result$n)
}


#' Incidence rate ratio (IRR)
#'
#' @param events1,person_time1 Events and person-time in group 1.
#' @param events2,person_time2 Events and person-time in group 2.
#' @param confidence Confidence level. Default 0.95.
#' @return A `morie_effect_size`.
#' @export
rate_ratio <- function(events1, person_time1, events2, person_time2,
                         confidence = 0.95) {
  r1 <- if (person_time1 > 0) events1 / person_time1 else 0
  r2 <- if (person_time2 > 0) events2 / person_time2 else 0
  irr <- if (r2 > 0) r1 / r2 else Inf
  log_irr <- if (irr > 0 && is.finite(irr)) log(irr) else 0
  se <- sqrt(1 / max(events1, 1) + 1 / max(events2, 1))
  z  <- qnorm((1 + confidence) / 2)
  effect_size_result("Rate ratio", irr,
                      exp(log_irr - z * se), exp(log_irr + z * se),
                      se, events1 + events2)
}


#' Incidence rate difference (IRD)
#'
#' @inheritParams rate_ratio
#' @return A `morie_effect_size`.
#' @export
incidence_rate_difference <- function(events1, person_time1,
                                         events2, person_time2,
                                         confidence = 0.95) {
  r1 <- if (person_time1 > 0) events1 / person_time1 else 0
  r2 <- if (person_time2 > 0) events2 / person_time2 else 0
  ird <- r1 - r2
  se <- if (person_time1 > 0 && person_time2 > 0)
    sqrt(events1 / person_time1^2 + events2 / person_time2^2) else 0
  z <- qnorm((1 + confidence) / 2)
  effect_size_result("Incidence rate difference", ird,
                      ird - z * se, ird + z * se, se)
}


# =====================================================================
# ASSOCIATION MEASURES
# =====================================================================

#' Cohen's w for chi-squared
#'
#' @param observed Observed frequencies (numeric vector).
#' @param expected Expected frequencies (or NULL for uniform).
#' @return A `morie_effect_size`.
#' @export
cohens_w <- function(observed, expected = NULL) {
  obs <- as.numeric(observed)
  exp <- if (is.null(expected)) {
    rep(sum(obs) / length(obs), length(obs))
  } else as.numeric(expected)
  n    <- sum(obs)
  chi2 <- sum((obs - exp)^2 / (exp + 1e-15))
  w    <- if (n > 0) sqrt(chi2 / n) else 0
  effect_size_result("Cohen's w", w, n = as.integer(n))
}


#' Cohen's f from eta-squared
#'
#' @param eta2 Eta-squared value.
#' @return A `morie_effect_size`.
#' @export
cohens_f <- function(eta2) {
  f_val <- if (eta2 < 1) sqrt(eta2 / (1 - eta2)) else Inf
  effect_size_result("Cohen's f", f_val)
}


#' Cramer's V for a contingency table
#'
#' @param contingency_table Numeric matrix or table.
#' @param confidence Confidence level. Default 0.95.
#' @return A `morie_effect_size`.
#' @export
cramers_v <- function(contingency_table, confidence = 0.95) {
  tbl <- as.matrix(contingency_table)
  storage.mode(tbl) <- "double"
  cs   <- suppressWarnings(chisq.test(tbl, correct = FALSE))
  chi2 <- as.numeric(cs$statistic)
  n    <- sum(tbl)
  k    <- min(dim(tbl)) - 1
  v    <- if (n * k > 0) sqrt(chi2 / (n * k)) else 0
  # Bias-corrected V (Bergsma 2013).
  v_bc <- max(0, v^2 - k * (nrow(tbl) - 1) / (n - 1))
  v_bc <- if (v_bc > 0) sqrt(v_bc) else 0
  effect_size_result("Cramer's V", v, n = as.integer(n),
                      extra = list(bias_corrected_v = v_bc))
}


#' Phi coefficient for a 2x2 contingency table
#'
#' @param contingency_table 2x2 numeric matrix.
#' @return A `morie_effect_size`.
#' @export
phi_coefficient <- function(contingency_table) {
  tbl <- as.matrix(contingency_table)
  storage.mode(tbl) <- "double"
  if (!all(dim(tbl) == c(2, 2)))
    stop("Phi requires a 2x2 table.")
  cs   <- suppressWarnings(chisq.test(tbl, correct = FALSE))
  chi2 <- as.numeric(cs$statistic)
  n    <- sum(tbl)
  phi  <- if (n > 0) sqrt(chi2 / n) else 0
  if (tbl[1, 1] * tbl[2, 2] < tbl[1, 2] * tbl[2, 1]) phi <- -phi
  effect_size_result("Phi coefficient", phi, n = as.integer(n))
}


# =====================================================================
# NON-PARAMETRIC EFFECT SIZES
# =====================================================================

#' Rank-biserial correlation (matched rank version)
#'
#' @inheritParams cohens_d
#' @return A `morie_effect_size`.
#' @export
rank_biserial_correlation <- function(x, y, confidence = 0.95) {
  x <- .arr(x)
  y <- .arr(y)
  u <- suppressWarnings(
    wilcox.test(x, y, alternative = "two.sided", exact = FALSE)$statistic
  )
  nx <- length(x)
  ny <- length(y)
  r <- if (nx * ny > 0) 1 - 2 * u / (nx * ny) else 0
  boot <- .bootstrap_ci(
    function(a, b) {
      1 - 2 * as.numeric(suppressWarnings(
        wilcox.test(a, b, alternative = "two.sided",
                     exact = FALSE)$statistic)) /
        (length(a) * length(b))
    },
    list(x, y), confidence = confidence)
  effect_size_result("Rank-biserial correlation", as.numeric(r),
                      boot$ci_lo, boot$ci_hi, boot$se, nx + ny)
}


#' Cliff's delta
#'
#' @inheritParams cohens_d
#' @return A `morie_effect_size`.
#' @export
cliffs_delta <- function(x, y, confidence = 0.95) {
  x <- .arr(x)
  y <- .arr(y)
  nx <- length(x)
  ny <- length(y)
  diff_mat <- outer(x, y, "-")
  greater <- sum(diff_mat > 0)
  less    <- sum(diff_mat < 0)
  delta   <- if (nx * ny > 0) (greater - less) / (nx * ny) else 0
  .delta <- function(a, b) {
    dm <- outer(a, b, "-")
    (sum(dm > 0) - sum(dm < 0)) / (length(a) * length(b))
  }
  boot <- .bootstrap_ci(.delta, list(x, y), confidence = confidence)
  effect_size_result("Cliff's delta", delta,
                      boot$ci_lo, boot$ci_hi, boot$se, nx + ny)
}


#' Vargha-Delaney A statistic
#'
#' @inheritParams cohens_d
#' @return A `morie_effect_size`.
#' @export
vargha_delaney_a <- function(x, y, confidence = 0.95) {
  x <- .arr(x)
  y <- .arr(y)
  u <- as.numeric(suppressWarnings(
    wilcox.test(x, y, alternative = "two.sided", exact = FALSE)$statistic))
  nx <- length(x)
  ny <- length(y)
  a_val <- if (nx * ny > 0) u / (nx * ny) else 0.5
  boot <- .bootstrap_ci(
    function(a, b) {
      as.numeric(suppressWarnings(
        wilcox.test(a, b, alternative = "two.sided",
                     exact = FALSE)$statistic)) / (length(a) * length(b))
    },
    list(x, y), confidence = confidence)
  effect_size_result("Vargha-Delaney A", a_val,
                      boot$ci_lo, boot$ci_hi, boot$se, nx + ny)
}


# =====================================================================
# REGRESSION
# =====================================================================

#' Standardised regression coefficients (beta weights)
#'
#' Standardises X and y to zero mean and unit variance before OLS via
#' `stats::lm`.
#'
#' @param X Predictor matrix or data.frame (n x p).
#' @param y Outcome vector.
#' @return A data.frame with columns `variable, beta, se, t, p_value`.
#' @export
standardized_coefficients <- function(X, y) {
  if (is.data.frame(X)) {
    names_x <- colnames(X)
    X_arr   <- as.matrix(sapply(X, as.numeric))
  } else {
    X_arr   <- as.matrix(X)
    storage.mode(X_arr) <- "double"
    names_x <- paste0("x", seq_len(ncol(X_arr)))
  }
  y_arr <- as.numeric(y)
  X_std <- scale(X_arr, center = TRUE, scale = TRUE)
  y_std <- as.numeric(scale(y_arr, center = TRUE, scale = TRUE))
  df_fit <- as.data.frame(X_std)
  colnames(df_fit) <- names_x
  df_fit$.y <- y_std
  fit <- stats::lm(.y ~ ., data = df_fit)
  cf  <- summary(fit)$coefficients
  # Skip intercept (row 1).
  rows <- seq_len(nrow(cf))[-1]
  data.frame(
    variable = names_x,
    beta     = cf[rows, "Estimate"],
    se       = cf[rows, "Std. Error"],
    t        = cf[rows, "t value"],
    p_value  = cf[rows, "Pr(>|t|)"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


#' Coefficient of variation
#'
#' @param x Numeric vector.
#' @return A `morie_effect_size`.
#' @export
coefficient_of_variation <- function(x) {
  x <- .arr(x)
  m <- mean(x)
  s <- sd(x)
  cv <- if (abs(m) > 0) s / abs(m) else Inf
  effect_size_result("Coefficient of variation", cv, n = length(x))
}


#' Variance ratio (F-test for equality of variances)
#'
#' @inheritParams cohens_d
#' @return A `morie_effect_size`.
#' @export
variance_ratio <- function(x, y, confidence = 0.95) {
  x <- .arr(x)
  y <- .arr(y)
  v1 <- var(x)
  v2 <- var(y)
  f_val <- if (v2 > 0) v1 / v2 else Inf
  df1 <- length(x) - 1
  df2 <- length(y) - 1
  alpha <- (1 - confidence) / 2
  ci_lo <- f_val / qf(1 - alpha, df1, df2)
  ci_hi <- f_val / qf(alpha, df1, df2)
  p_val <- 2 * min(pf(f_val, df1, df2),
                    pf(f_val, df1, df2, lower.tail = FALSE))
  effect_size_result("Variance ratio (F)", f_val, ci_lo, ci_hi,
                      n = length(x) + length(y),
                      extra = list(p_value = p_val, df1 = df1, df2 = df2))
}


# =====================================================================
# CONVERSION FUNCTIONS
# =====================================================================

#' Convert Cohen's d to Pearson r
#'
#' @param d Cohen's d.
#' @param n1,n2 Sample sizes (or NULL).
#' @return Numeric r.
#' @export
d_to_r <- function(d, n1 = NULL, n2 = NULL) {
  a <- if (!is.null(n1) && !is.null(n2)) (n1 + n2)^2 / (n1 * n2) else 4
  d / sqrt(d^2 + a)
}

#' Convert Pearson r to Cohen's d
#' @param r Pearson r.
#' @return Numeric d.
#' @export
r_to_d <- function(r) {
  if (abs(r) < 1) 2 * r / sqrt(1 - r^2) else sign(r) * Inf
}

#' Convert odds ratio to Cohen's d (Hasselblad & Hedges, 1995)
#' @param or_val Odds ratio.
#' @return Numeric d.
#' @export
or_to_d <- function(or_val) {
  if (or_val > 0) log(or_val) * sqrt(3) / pi else 0
}

#' Convert Cohen's d to odds ratio
#' @param d Cohen's d.
#' @return Numeric OR.
#' @export
d_to_or <- function(d) exp(d * pi / sqrt(3))

#' Convert OR to Pearson r via d
#' @param or_val Odds ratio.
#' @return Numeric r.
#' @export
or_to_r <- function(or_val) d_to_r(or_to_d(or_val))

#' Convert Pearson r to OR via d
#' @param r Pearson r.
#' @return Numeric OR.
#' @export
r_to_or <- function(r) d_to_or(r_to_d(r))

#' Convert Cohen's d to NNT given a control event rate
#'
#' Uses the Kraemer & Kupfer (2006) approximation.
#'
#' @param d Cohen's d.
#' @param base_rate Control event rate (default 0.5).
#' @return Numeric NNT.
#' @export
d_to_nnt <- function(d, base_rate = 0.5) {
  z_cer  <- qnorm(base_rate)
  p_treat <- pnorm(d + z_cer)
  rd <- p_treat - base_rate
  if (abs(rd) > 0) 1 / abs(rd) else Inf
}


# =====================================================================
# META-ANALYSIS
# =====================================================================

#' Fixed-effects (inverse-variance) meta-analytic pooling
#'
#' @param estimates Numeric vector of effect-size estimates.
#' @param standard_errors Numeric vector of SEs.
#' @param confidence Confidence level. Default 0.95.
#' @return A `morie_effect_size` with Q + Q p-value in `extra`.
#' @export
fixed_effects_meta <- function(estimates, standard_errors,
                                confidence = 0.95) {
  theta <- as.numeric(estimates)
  se    <- as.numeric(standard_errors)
  w     <- 1 / se^2
  pooled    <- sum(w * theta) / sum(w)
  pooled_se <- sqrt(1 / sum(w))
  z <- qnorm((1 + confidence) / 2)
  q <- sum(w * (theta - pooled)^2)
  k <- length(theta)
  p_q <- if (k > 1) 1 - pchisq(q, k - 1) else 1
  effect_size_result("Fixed-effects meta-analysis", pooled,
                      pooled - z * pooled_se, pooled + z * pooled_se,
                      pooled_se, k,
                      extra = list(Q = q, Q_p_value = p_q))
}


#' Random-effects (DerSimonian-Laird) meta-analytic pooling
#'
#' @param estimates Numeric vector of effect-size estimates.
#' @param standard_errors Numeric vector of SEs.
#' @param confidence Confidence level. Default 0.95.
#' @param method Tau^2 estimator. Only `"DL"` implemented.
#' @return A `morie_effect_size` with tau^2, I^2, Q, prediction
#'   interval in `extra`.
#' @export
random_effects_meta <- function(estimates, standard_errors,
                                  confidence = 0.95, method = "DL") {
  theta <- as.numeric(estimates)
  se    <- as.numeric(standard_errors)
  k     <- length(theta)
  w     <- 1 / se^2
  theta_fe <- sum(w * theta) / sum(w)
  Q <- sum(w * (theta - theta_fe)^2)
  c_val <- sum(w) - sum(w^2) / sum(w)
  tau2  <- if (c_val > 0) max((Q - (k - 1)) / c_val, 0) else 0
  w_re  <- 1 / (se^2 + tau2)
  pooled    <- sum(w_re * theta) / sum(w_re)
  pooled_se <- sqrt(1 / sum(w_re))
  z <- qnorm((1 + confidence) / 2)
  i2 <- if (Q > 0) max((Q - (k - 1)) / Q, 0) * 100 else 0
  pred_se <- sqrt(pooled_se^2 + tau2)
  t_crit  <- qt((1 + confidence) / 2, max(k - 2, 1))
  effect_size_result(
    "Random-effects meta-analysis (DL)", pooled,
    pooled - z * pooled_se, pooled + z * pooled_se, pooled_se, k,
    extra = list(
      tau_squared = tau2, tau = sqrt(tau2),
      I_squared = i2, Q = Q,
      Q_p_value = if (k > 1) 1 - pchisq(Q, k - 1) else 1,
      prediction_interval_lower = pooled - t_crit * pred_se,
      prediction_interval_upper = pooled + t_crit * pred_se
    )
  )
}


#' I^2 heterogeneity statistic (Higgins)
#'
#' @param estimates Effect-size estimates.
#' @param standard_errors Standard errors.
#' @return Numeric I^2 percentage.
#' @export
i_squared <- function(estimates, standard_errors) {
  random_effects_meta(estimates, standard_errors)$extra$I_squared
}


#' Prediction interval for a new study (random-effects meta)
#'
#' @inheritParams random_effects_meta
#' @return Numeric c(lower, upper).
#' @export
prediction_interval <- function(estimates, standard_errors,
                                  confidence = 0.95) {
  r <- random_effects_meta(estimates, standard_errors,
                             confidence = confidence)
  c(r$extra$prediction_interval_lower,
    r$extra$prediction_interval_upper)
}


# =====================================================================
# BOOTSTRAP CI FOR ARBITRARY EFFECT SIZE
# =====================================================================

#' Generic bootstrap CI wrapper for any effect-size function
#'
#' @param func A function taking one or more numeric vectors, returning
#'   a scalar.
#' @param ... Numeric vectors to bootstrap, in `func`'s argument order.
#' @param n_boot Bootstrap replicates (default 2000).
#' @param confidence Confidence level. Default 0.95.
#' @param seed RNG seed. Default 42.
#' @return A `morie_effect_size`.
#' @export
bootstrap_effect_size_ci <- function(func, ..., n_boot = 2000L,
                                        confidence = 0.95, seed = 42L) {
  arrs <- lapply(list(...), .arr)
  point <- do.call(func, arrs)
  boot  <- .bootstrap_ci(func, arrs, n_boot = n_boot,
                          confidence = confidence, seed = seed)
  effect_size_result(
    paste0("Bootstrap (", deparse(substitute(func)), ")"),
    point, boot$ci_lo, boot$ci_hi, boot$se,
    sum(vapply(arrs, length, integer(1)))
  )
}
