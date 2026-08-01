# Advanced Statistics in Criminology surface (Weisburd, Wilson, Wooditch &
# Britt 2022, 5th ed, Springer, doi:10.1007/978-3-030-67738-1).
# Mirrors morie.fn._ca_crim; every function cites its equation numbers.

#' Simple OLS regression by the textbook equations
#'
#' Slope, intercept, correlation and slope t-test computed exactly as
#' Weisburd et al. (2022) equations 2.2 to 2.6. Mirrors morie.fn ca2e1
#' to ca2e6.
#'
#' @param x Numeric predictor vector.
#' @param y Numeric response vector, same length as x.
#' @return List with b1, b0, r, se_b1, t, t_from_r, df, n.
#' @export
morie_ols_simple <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  stopifnot(length(x) == length(y), length(x) >= 3)
  n <- length(x)
  xd <- x - mean(x)
  yd <- y - mean(y)
  sxx <- sum(xd^2)
  b1 <- sum(xd * yd) / sxx
  b0 <- mean(y) - b1 * mean(x)
  r <- sum(yd * xd) / sqrt(sum(yd^2) * sxx)
  resid <- y - (b0 + b1 * x)
  se_b1 <- sqrt(sum(resid^2) / (n - 2) / sxx)
  list(b1 = b1, b0 = b0, r = r, se_b1 = se_b1, t = b1 / se_b1,
       t_from_r = r * sqrt((n - 2) / (1 - r^2)), df = n - 2, n = n)
}

#' Two-predictor OLS slopes from correlations
#'
#' Equations 2.7 and 2.8 of Weisburd et al. (2022). Mirrors ca2e7 / ca2e8.
#'
#' @param r_y1,r_y2,r_12 Pairwise correlations.
#' @param s_y,s_1,s_2 Standard deviations of y, x1, x2.
#' @return List with b1 and b2.
#' @export
morie_ols_two_iv <- function(r_y1, r_y2, r_12, s_y, s_1, s_2) {
  stopifnot(abs(r_12) < 1)
  den <- 1 - r_12^2
  list(b1 = (r_y1 - r_y2 * r_12) / den * (s_y / s_1),
       b2 = (r_y2 - r_y1 * r_12) / den * (s_y / s_2))
}

#' OLS fit indices: variance partition, R squared, adjusted R squared, F
#'
#' Equations 2.11 to 2.17 of Weisburd et al. (2022). Mirrors ca2e11 to
#' ca2e17.
#'
#' @param y Observed response.
#' @param yhat Fitted values.
#' @param k Number of independent variables.
#' @return List with variances, sums of squares, r2, adj_r2, f_from_r2.
#' @export
morie_fit_indices <- function(y, yhat, k = 1) {
  y <- as.numeric(y)
  yhat <- as.numeric(yhat)
  stopifnot(length(y) == length(yhat))
  n <- length(y)
  ybar <- mean(y)
  ss_total <- sum((y - ybar)^2)
  ss_model <- sum((yhat - ybar)^2)
  ss_resid <- sum((y - yhat)^2)
  r2 <- ss_model / ss_total
  list(var_total = ss_total / n, var_model = ss_model / n,
       var_resid = ss_resid / n, ss_total = ss_total, ss_model = ss_model,
       ss_resid = ss_resid, r2 = r2,
       adj_r2 = 1 - (1 - r2) * (n - 1) / (n - k - 1),
       f_from_r2 = r2 * (n - k - 1) / ((1 - r2) * k), n = n)
}

#' Nested-model F change tests
#'
#' Equations 2.18 (sums of squares form) and 2.19 (R squared form) of
#' Weisburd et al. (2022). Mirrors ca2e18 / ca2e19.
#'
#' @param ss_resid_restricted,ss_resid_full Residual sums of squares.
#' @param r2_full,r2_restricted The two R squared values.
#' @param k_full,k_restricted Predictor counts.
#' @param n Sample size.
#' @return List with f_ss (df n - k_full) and f_r2 (df n - k_full - 1);
#'   either pair of inputs may be NA to skip that form.
#' @export
morie_f_change <- function(ss_resid_restricted = NA, ss_resid_full = NA,
                           r2_full = NA, r2_restricted = NA,
                           k_full, k_restricted, n) {
  df_num <- k_full - k_restricted
  stopifnot(df_num > 0)
  out <- list(df1 = df_num)
  if (!is.na(ss_resid_restricted) && !is.na(ss_resid_full)) {
    df_den <- n - k_full
    out$f_ss <- ((ss_resid_restricted - ss_resid_full) / df_num) /
      (ss_resid_full / df_den)
    out$df2_ss <- df_den
  }
  if (!is.na(r2_full) && !is.na(r2_restricted)) {
    df_den <- n - k_full - 1
    out$f_r2 <- ((r2_full - r2_restricted) / df_num) /
      ((1 - r2_full) / df_den)
    out$df2_r2 <- df_den
  }
  out
}

#' Standardized coefficients for OLS and logistic models
#'
#' Beta equals b times s_x over s_y for OLS (equation 2.20), b times s for
#' logistic (equation 4.10), and b times 2 s under the Gelman-Hill
#' convention (equation 4.11). Mirrors ca2e20, ca4e10, ca4e11.
#'
#' @param b Unstandardized coefficient.
#' @param s_x Standard deviation of the predictor.
#' @param s_y Standard deviation of the response; NULL for logistic Beta.
#' @param gelman If TRUE use two times s_x (dummy-variable convention).
#' @return Numeric Beta.
#' @export
morie_std_coef <- function(b, s_x, s_y = NULL, gelman = FALSE) {
  stopifnot(s_x > 0)
  if (is.null(s_y)) return(b * (if (gelman) 2 * s_x else s_x))
  stopifnot(s_y > 0)
  b * s_x / s_y
}

#' Tolerance and variance inflation factor
#'
#' Equations 3.1 and 3.2 of Weisburd et al. (2022). Mirrors ca3e1 / ca3e2.
#'
#' @param r2_x R squared from regressing the predictor on the others.
#' @return List with tolerance and vif.
#' @export
morie_vif_tolerance <- function(r2_x) {
  stopifnot(r2_x >= 0, r2_x < 1)
  tol <- 1 - r2_x
  list(tolerance = tol, vif = 1 / tol)
}

#' Logit link utilities
#'
#' logit, inverse logit, odds and the one-unit odds ratio exp(b):
#' equations 1.3, 4.1 to 4.8 of Weisburd et al. (2022). Mirrors ca1e3,
#' ca4e1 to ca4e8.
#'
#' @param p Probability strictly between 0 and 1.
#' @param xb Linear predictor value.
#' @param b Logistic regression coefficient.
#' @return List with logit(p), odds(p), p_from_xb, odds_ratio.
#' @export
morie_logit_link <- function(p = 0.5, xb = 0, b = 0) {
  stopifnot(p > 0, p < 1)
  list(logit = log(p / (1 - p)), odds = p / (1 - p),
       p_from_xb = 1 / (1 + exp(-xb)), odds_ratio = exp(b))
}

#' Logistic regression effect and fit statistics
#'
#' Derivative at mean (equation 4.9), percent correct (4.12), Cox and
#' Snell R squared (4.13), model chi square (4.14), Wald (4.15), z (4.16)
#' and likelihood-ratio chi square (4.18). Mirrors ca4e9 to ca4e18.
#'
#' @param ybar Mean of the binary response.
#' @param b Coefficient. @param se Its standard error.
#' @param n_correct,n_total Prediction counts.
#' @param neg2ll_null,neg2ll_full,neg2ll_reduced Reported -2LL values.
#' @param n Sample size for Cox and Snell.
#' @return List of the named statistics.
#' @export
morie_logistic_effects <- function(ybar, b, se, n_correct, n_total,
                                   neg2ll_null, neg2ll_full,
                                   neg2ll_reduced = NA, n = NA) {
  stopifnot(ybar > 0, ybar < 1, se > 0)
  chi2 <- neg2ll_null - neg2ll_full
  out <- list(dm = ybar * (1 - ybar) * b,
              pct_correct = 100 * n_correct / n_total,
              model_chi2 = chi2, wald = (b / se)^2, z = b / se)
  if (!is.na(n)) out$cox_snell_r2 <- 1 - exp(-chi2 / n)
  if (!is.na(neg2ll_reduced)) out$lr_chi2 <- neg2ll_reduced - neg2ll_full
  out
}

#' Multinomial probabilities and conditional odds ratios
#'
#' Softmax probabilities (equation 5.3) and the conditional odds ratio
#' exp(xb_m - xb_n) (equation 5.4). Mirrors ca5e3 / ca5e4.
#'
#' @param xbs Linear predictor per category (reference category 0).
#' @return List with probs and a function-free or_matrix of pairwise
#'   conditional odds ratios.
#' @export
morie_mlogit_probs <- function(xbs) {
  xbs <- as.numeric(xbs)
  z <- exp(xbs - max(xbs))
  probs <- z / sum(z)
  list(probs = probs, or_matrix = outer(exp(xbs), exp(xbs), "/"))
}

#' Ordinal (cumulative) logit utilities
#'
#' Cumulative probability (equation 5.6), cumulative logit (5.7) and the
#' two threshold parameterizations tau plus or minus Xb (5.8, 5.9).
#' Mirrors ca5e6 to ca5e9.
#'
#' @param probs Category probabilities summing to one.
#' @param m Category index (1-based, at most length(probs) - 1).
#' @param tau_m Threshold. @param xb Linear predictor value.
#' @return List with cum_prob, cum_logit, logit_plus, logit_minus.
#' @export
morie_ordinal_logit_ca <- function(probs, m, tau_m = 0, xb = 0) {
  probs <- as.numeric(probs)
  stopifnot(all(probs >= 0), abs(sum(probs) - 1) < 1e-8,
            m >= 1, m <= length(probs) - 1)
  cp <- sum(probs[seq_len(m)])
  list(cum_prob = cp, cum_logit = log(cp / (1 - cp)),
       logit_plus = tau_m + xb, logit_minus = tau_m - xb)
}

#' Count-model (Poisson family) utilities
#'
#' Log-link prediction exp(b0 + b1 x1) (equations 6.1 to 6.4), incidence
#' rate ratio exp(b), offset prediction (6.7), quasi-Poisson theta and
#' adjusted standard error, and the negative binomial variance mu plus
#' mu squared alpha (6.8). Mirrors ca6e1 to ca6e8.
#'
#' @param b0,b1,x1 Model terms. @param exposure Offset exposure.
#' @param y,yhat Observed and fitted counts for theta. @param k Predictors.
#' @param se Poisson standard error. @param mu,alpha Negative binomial terms.
#' @return List of the named quantities.
#' @export
morie_count_glm <- function(b0 = 0, b1 = 0, x1 = 0, exposure = 1,
                            y = NULL, yhat = NULL, k = 1, se = NA,
                            mu = NA, alpha = NA) {
  stopifnot(exposure > 0)
  out <- list(predict = exp(b0 + b1 * x1), irr = exp(b1),
              predict_offset = exp(b0 + b1 * x1 + log(exposure)))
  if (!is.null(y) && !is.null(yhat)) {
    y <- as.numeric(y); yhat <- as.numeric(yhat)
    stopifnot(length(y) == length(yhat), all(yhat > 0),
              length(y) > k + 1)
    out$theta <- sum((y - yhat)^2 / yhat) / (length(y) - k - 1)
    if (!is.na(se)) out$se_quasi <- se * sqrt(out$theta)
  }
  if (!is.na(mu) && !is.na(alpha)) out$negbin_var <- mu + mu^2 * alpha
  out
}

#' Multilevel variance components, ICC and likelihood-ratio test
#'
#' sigma2_u equals (MSbetween - MSwithin) / n (equation 7.6), the
#' intraclass correlation (7.7) and the LR chi square -2 (LL1 - LL2)
#' (7.8). Mirrors ca7e6 to ca7e8.
#'
#' @param ms_between,ms_within Mean squares. @param n_per_cluster Cluster n
#'   (harmonic mean under unequal sizes).
#' @param ll_null,ll_full Log-likelihoods.
#' @return List with sigma2_u, icc, lr_chi2.
#' @export
morie_hlm_components <- function(ms_between, ms_within, n_per_cluster,
                                 ll_null = NA, ll_full = NA) {
  stopifnot(n_per_cluster > 0)
  s2u <- (ms_between - ms_within) / n_per_cluster
  out <- list(sigma2_u = s2u,
              icc = if (s2u >= 0 && ms_within > 0)
                s2u / (s2u + ms_within) else NA_real_)
  if (!is.na(ll_null) && !is.na(ll_full))
    out$lr_chi2 <- -2 * (ll_null - ll_full)
  out
}

#' Statistical power for t-based tests (crim shelf)
#'
#' Cohen's d (equation 8.2), the noncentrality parameters for d and r
#' (chapter 8 and equation 8.6), t_beta equals delta minus t_CV (8.3)
#' with power from the Johnson-Kramer noncentral-t approximation,
#' lambda equals n f squared (8.5) and R squared from f squared (8.7).
#' Mirrors ca8e1 to ca8e7.
#'
#' @param d Effect size. @param n1,n2 Group sizes. @param t_cv Critical t.
#' @param df Degrees of freedom. @param f Cohen's f. @param n_total Total n.
#' @param r Correlation. @param n Sample size for the r delta.
#' @return List with delta_d, t_beta, beta, power, lambda, delta_r, r2_f2.
#' @export
morie_power_ttest_crim <- function(d = NA, n1 = NA, n2 = NA, t_cv = NA,
                                   df = NA, f = NA, n_total = NA,
                                   r = NA, n = NA) {
  out <- list()
  if (!is.na(d) && !is.na(n1) && !is.na(n2)) {
    out$delta_d <- d * sqrt(n1 * n2 / (n1 + n2))
    if (!is.na(t_cv) && !is.na(df)) {
      num <- t_cv * (1 - 1 / (4 * df)) - out$delta_d
      den <- sqrt(1 + t_cv^2 / (2 * df))
      out$t_beta <- out$delta_d - t_cv
      out$beta <- stats::pnorm(num / den)
      out$power <- 1 - out$beta
    }
  }
  if (!is.na(f) && !is.na(n_total)) {
    out$lambda <- n_total * f^2
    out$r2_f2 <- f^2 / (1 + f^2)
  }
  if (!is.na(r) && !is.na(n))
    out$delta_r <- r * sqrt(n - 2) / sqrt(1 - r^2)
  out
}

#' Randomized-experiment test statistics
#'
#' Confounded and randomized treatment coefficients (equations 9.1, 9.2),
#' pooled-variance independent t (9.3, 9.11), the 2 by 2 chi square (9.4)
#' and paired t (9.10). Mirrors ca9e1 to ca9e4, ca9e10, ca9e11.
#'
#' @param r_yt,r_yx,r_tx Correlations. @param s_y,s_t Standard deviations.
#' @param m1,m2,s1,s2,n1,n2 Group summaries. @param a,b,c,d Cell counts.
#' @param differences Paired differences.
#' @return List with b_t, b_t_random, t, df, chi2, t_paired, df_paired
#'   (only the pieces whose inputs were supplied).
#' @export
morie_rct_tests <- function(r_yt = NA, r_yx = NA, r_tx = NA, s_y = NA,
                            s_t = NA, m1 = NA, m2 = NA, s1 = NA, s2 = NA,
                            n1 = NA, n2 = NA, a = NA, b = NA, c = NA,
                            d = NA, differences = NULL) {
  out <- list()
  if (!is.na(r_yt) && !is.na(s_y) && !is.na(s_t)) {
    out$b_t_random <- r_yt * s_y / s_t
    if (!is.na(r_yx) && !is.na(r_tx))
      out$b_t <- (r_yt - r_yx * r_tx) / (1 - r_tx^2) * (s_y / s_t)
  }
  if (!is.na(m1) && !is.na(n1) && !is.na(n2)) {
    df <- n1 + n2 - 2
    pooled_var <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / df
    se <- sqrt(pooled_var * (n1 + n2) / (n1 * n2))
    out$t <- (m1 - m2) / se
    out$df <- df
    out$s_pooled <- sqrt(pooled_var)
  }
  if (!is.na(a)) {
    den <- (a + b) * (c + d) * (a + c) * (b + d)
    stopifnot(den > 0)
    out$chi2 <- (a * d - b * c)^2 * (a + b + c + d) / den
  }
  if (!is.null(differences)) {
    dd <- as.numeric(differences)
    stopifnot(length(dd) >= 2)
    out$t_paired <- mean(dd) / sqrt(stats::var(dd) / length(dd))
    out$df_paired <- length(dd) - 1
  }
  out
}

#' One-way and block-randomized ANOVA (crim shelf)
#'
#' MSbetween, MSwithin and F (equations 9.5 to 9.7, 9.12) and the block
#' design with treatment and block effects (9.13). Mirrors ca9e5 to
#' ca9e7, ca9e12, ca9e13.
#'
#' @param groups List of numeric vectors (one-way form).
#' @param y,treatment,block Long-format vectors (block form).
#' @return List with the one-way and, when supplied, block results.
#' @export
morie_experiment_anova <- function(groups = NULL, y = NULL,
                                   treatment = NULL, block = NULL) {
  out <- list()
  if (!is.null(groups)) {
    groups <- lapply(groups, as.numeric)
    a <- length(groups)
    stopifnot(a >= 2)
    allv <- unlist(groups)
    n_total <- length(allv)
    grand <- mean(allv)
    ss_b <- sum(vapply(groups, function(g)
      length(g) * (mean(g) - grand)^2, numeric(1)))
    ss_w <- sum(vapply(groups, function(g)
      sum((g - mean(g))^2), numeric(1)))
    out$ms_between <- ss_b / (a - 1)
    out$ms_within <- ss_w / (n_total - a)
    out$f <- out$ms_between / out$ms_within
    out$df1 <- a - 1
    out$df2 <- n_total - a
  }
  if (!is.null(y)) {
    y <- as.numeric(y)
    tl <- unique(treatment)
    bl <- unique(block)
    grand <- mean(y)
    ss_t <- sum(vapply(tl, function(t)
      sum(treatment == t) * (mean(y[treatment == t]) - grand)^2, numeric(1)))
    ss_bk <- sum(vapply(bl, function(k)
      sum(block == k) * (mean(y[block == k]) - grand)^2, numeric(1)))
    ss_tot <- sum((y - grand)^2)
    df_res <- length(y) - length(tl) - length(bl) + 1
    stopifnot(df_res > 0)
    out$f_treatment <- (ss_t / (length(tl) - 1)) /
      ((ss_tot - ss_t - ss_bk) / df_res)
    out$ss_block <- ss_bk
  }
  out
}

#' Propensity-score matching standardized absolute bias
#'
#' Equation 10.1 of Weisburd et al. (2022): 100 (xbar_t - xbar_c) over
#' the square root of the average of the two variances. Mirrors ca10e1.
#'
#' @param mean_t,mean_c Group means. @param s_t,s_c Group SDs.
#' @return Numeric bias in percent.
#' @export
morie_psm_balance <- function(mean_t, mean_c, s_t, s_c) {
  stopifnot(s_t >= 0, s_c >= 0, s_t + s_c > 0)
  100 * (mean_t - mean_c) / sqrt((s_t^2 + s_c^2) / 2)
}

#' Meta-analysis effect sizes: d, Hedges g, RR, OR, Fisher Z
#'
#' Equations 11.1 to 11.13 of Weisburd et al. (2022): Cohen's d with the
#' pooled SD, Hedges' correction and its standard error, d from t, risk
#' and odds ratios with logged standard errors, and Fisher's Z with its
#' standard error. Mirrors ca11e1 to ca11e14.
#'
#' @param m1,m2,s1,s2,n1,n2 Group summaries. @param t_value Optional t.
#' @param a,b,c,d 2 by 2 cell counts. @param r Correlation.
#' @return List of the named effect sizes and standard errors.
#' @export
morie_meta_effect_sizes <- function(m1 = NA, m2 = NA, s1 = NA, s2 = NA,
                                    n1 = NA, n2 = NA, t_value = NA,
                                    a = NA, b = NA, c = NA, d = NA,
                                    r = NA) {
  out <- list()
  if (!is.na(n1) && !is.na(n2)) {
    if (!is.na(s1) && !is.na(m1)) {
      sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
      out$s_pooled <- sp
      out$d <- (m1 - m2) / sp
    }
    out$j <- 1 - 3 / (4 * (n1 + n2) - 9)
    if (!is.null(out$d) && !is.na(out$d)) {
      out$g <- out$j * out$d
      out$se_g <- sqrt((n1 + n2) / (n1 * n2) +
                         out$g^2 / (2 * (n1 + n2)))
    }
    if (!is.na(t_value)) out$d_from_t <- t_value * sqrt((n1 + n2) / (n1 * n2))
  }
  if (!is.na(a)) {
    p1 <- a / (a + b)
    p2 <- c / (c + d)
    out$rr <- p1 / p2
    out$or <- a * d / (b * c)
    out$se_ln_rr <- sqrt((1 - p1) / ((a + b) * p1) +
                           (1 - p2) / ((c + d) * p2))
    out$se_ln_or <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
  }
  if (!is.na(r)) {
    out$fisher_z <- 0.5 * log((1 + r) / (1 - r))
    if (!is.na(n1)) out$se_fisher_z <- 1 / sqrt(n1 - 3)
  }
  out
}

#' Meta-analysis effect-size conversions
#'
#' Equations 11.15 to 11.33 of Weisburd et al. (2022): d from ln(OR) by
#' the logit (divide by sqrt(pi^2 / 3)), Cox (divide by 1.65) and probit
#' methods with standard errors; the inverse ln(OR) from d (0.551 and
#' 0.606 divisors); RR to OR and back; and point-biserial r to d and
#' back. Mirrors ca11e15 to ca11e33.
#'
#' @param ln_or Logged odds ratio. @param se_ln_or Its standard error.
#' @param p1,p2 Group probabilities. @param n1,n2 Group sizes.
#' @param d Cohen's d. @param se_d Its standard error. @param rr Risk
#'   ratio. @param or_value Odds ratio. @param r Point-biserial r.
#'   @param se_r Its standard error.
#' @return List of every conversion whose inputs were supplied.
#' @export
morie_meta_convert <- function(ln_or = NA, se_ln_or = NA, p1 = NA,
                               p2 = NA, n1 = NA, n2 = NA, d = NA,
                               se_d = NA, rr = NA, or_value = NA,
                               r = NA, se_r = NA) {
  sd_logistic <- sqrt(pi^2 / 3)
  out <- list(sd_logistic = sd_logistic)
  if (!is.na(ln_or)) {
    out$d_logit <- ln_or / sd_logistic
    out$d_cox <- ln_or / 1.65
  }
  if (!is.na(se_ln_or)) {
    out$se_d_logit <- sqrt(se_ln_or^2 / sd_logistic^2)
    out$se_d_cox <- sqrt(se_ln_or^2 / 1.65^2)
  }
  if (!is.na(p1) && !is.na(p2)) {
    z1 <- stats::qnorm(p1)
    z2 <- stats::qnorm(p2)
    out$d_probit <- z1 - z2
    if (!is.na(n1) && !is.na(n2))
      out$se_d_probit <- sqrt(2 * pi * p1 * (1 - p1) * exp(z1^2) / n1 +
                                2 * pi * p2 * (1 - p2) * exp(z2^2) / n2)
  }
  if (!is.na(d)) {
    out$ln_or_logit <- d / 0.551
    out$ln_or_cox <- d / 0.606
    h <- if (!is.na(n1) && !is.na(n2)) (n1 + n2)^2 / (n1 * n2) else 4
    out$r_from_d <- d / sqrt(d^2 + h)
    if (!is.na(se_d))
      out$se_r_from_d <- sqrt(h * se_d^2 / (d^2 + h)^3)
  }
  if (!is.na(se_d)) {
    out$se_ln_or_logit <- sqrt(se_d^2 / 0.551^2)
    out$se_ln_or_cox <- sqrt(se_d^2 / 0.606^2)
  }
  if (!is.na(rr) && !is.na(p2))
    out$or_from_rr <- rr * p2 * (1 - p2) / (p2 * (1 - rr * p2))
  if (!is.na(or_value) && !is.na(p2))
    out$rr_from_or <- or_value / (1 - p2 + p2 * or_value)
  if (!is.na(r)) {
    out$d_from_r <- 2 * r / sqrt(1 - r^2)
    if (!is.na(se_r))
      out$se_d_from_r <- sqrt(4 * se_r^2 / (1 - r^2)^3)
  }
  out
}

#' Meta-analysis pooling: weights, mean effect, Q, I squared, tau squared
#'
#' Equations 11.34 to 11.46 of Weisburd et al. (2022): inverse-variance
#' weights, the weighted mean effect with standard error and z, the
#' Q homogeneity statistic (definitional and computational forms agree),
#' Higgins' I squared, the DerSimonian-Laird tau squared, random-effects
#' weights, and the Qwithin / Qbetween partition. Mirrors ca11e34 to
#' ca11e46.
#'
#' @param ys Effect sizes. @param ses Their standard errors.
#' @param z_cv Critical z for the confidence interval.
#' @param groups Optional integer group labels for the moderator
#'   partition.
#' @return List with weights, mean, se, z, ci, q, df, i2, tau2,
#'   weights_random, and q_within / q_between when groups are given.
#' @export
morie_meta_pool <- function(ys, ses, z_cv = 1.96, groups = NULL) {
  ys <- as.numeric(ys)
  ses <- as.numeric(ses)
  stopifnot(length(ys) == length(ses), all(ses > 0))
  ws <- 1 / ses^2
  m <- sum(ws * ys) / sum(ws)
  se_m <- sqrt(1 / sum(ws))
  q <- sum(ws * (ys - m)^2)
  df <- length(ys) - 1
  cc <- sum(ws) - sum(ws^2) / sum(ws)
  tau2 <- max(0, (q - df) / cc)
  out <- list(weights = ws, mean = m, se = se_m, z = m / se_m,
              ci = c(m - z_cv * se_m, m + z_cv * se_m),
              q = q, df = df,
              i2 = max(0, (q - df) / q * 100),
              tau2 = tau2, weights_random = 1 / (ses^2 + tau2))
  if (!is.null(groups)) {
    qw <- 0
    for (g in unique(groups)) {
      idx <- groups == g
      wg <- ws[idx]
      mg <- sum(wg * ys[idx]) / sum(wg)
      qw <- qw + sum(wg * (ys[idx] - mg)^2)
    }
    out$q_within <- qw
    out$q_between <- q - qw
  }
  out
}

#' Moran's I spatial autocorrelation and its expectation
#'
#' Equations 12.1 and 12.2 of Weisburd et al. (2022). Mirrors ca12e1 /
#' ca12e2.
#'
#' @param x Numeric values at n spatial units.
#' @param w n by n spatial weights matrix.
#' @return List with i and expected (equal to -1 / (n - 1)).
#' @export
morie_morans_i <- function(x, w) {
  x <- as.numeric(x)
  w <- as.matrix(w)
  n <- length(x)
  stopifnot(n >= 2, all(dim(w) == n))
  xd <- x - mean(x)
  denom <- sum(w) * sum(xd^2)
  stopifnot(denom != 0)
  list(i = n * as.numeric(t(xd) %*% w %*% xd) / denom,
       expected = -1 / (n - 1))
}

#' Spatial lag (SAR) model reduced form
#'
#' Solves y equals rho W y plus xb plus e as the reduced form
#' (I - rho W)^-1 (xb + e); rho of zero recovers the OLS structural
#' model of equation 12.3. Equations 12.3 and 12.4. Mirrors ca12e3 /
#' ca12e4.
#'
#' @param rho Spatial autoregressive coefficient.
#' @param w Spatial weights matrix.
#' @param xb Structural component vector.
#' @param e Error vector.
#' @return Numeric y vector satisfying the SAR equation.
#' @export
morie_sar_lag <- function(rho, w, xb, e) {
  w <- as.matrix(w)
  xb <- as.numeric(xb)
  e <- as.numeric(e)
  n <- length(xb)
  stopifnot(all(dim(w) == n), length(e) == n)
  a <- diag(n) - rho * w
  stopifnot(is.finite(rcond(a)), rcond(a) > 1e-12)
  as.numeric(solve(a, xb + e))
}
