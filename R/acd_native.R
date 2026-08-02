# Analysis of Categorical Data with R surface (Bilder & Loughin 2025,
# 2nd ed, Chapman & Hall/CRC). Mirrors morie.fn._acd; every function
# cites its equation numbers.

#' Binomial inference: PMF, MLE variance, Wilson interval, true confidence
#'
#' Equations 1.1, 1.3, 1.4, 1.6 of Bilder & Loughin (2025).
#'
#' @param w Successes. @param n Trials. @param p Probability.
#' @param z Normal critical value.
#' @param interval_fn Function(w, n) returning c(lower, upper) for the
#'   true-confidence-level sum; NULL skips it.
#' @return List with pmf, var_mle, wilson (estimate/lower/upper), and
#'   true_level when interval_fn is given.
#' @export
morie_binomial_inference <- function(w, n, p = NA, z = 1.96,
                                     interval_fn = NULL) {
  stopifnot(w >= 0, w <= n, n > 0)
  out <- list()
  if (!is.na(p)) {
    out$pmf <- stats::dbinom(w, n, p)
    if (!is.null(interval_fn)) {
      cl <- 0
      for (ww in 0:n) {
        ci <- interval_fn(ww, n)
        if (ci[1] <= p && p <= ci[2]) cl <- cl + stats::dbinom(ww, n, p)
      }
      out$true_level <- cl
    }
  }
  p_hat <- w / n
  out$var_mle <- p_hat * (1 - p_hat) / n
  p_t <- (w + z^2 / 2) / (n + z^2)
  half <- z * sqrt(n) / (n + z^2) * sqrt(p_hat * (1 - p_hat) + z^2 / (4 * n))
  out$wilson <- c(estimate = p_t, lower = p_t - half, upper = p_t + half)
  out
}

#' Two-group binomial tests and the odds ratio Wald interval
#'
#' Equations 1.7, 1.8, 1.10 of Bilder & Loughin (2025).
#'
#' @param w1,n1,w2,n2 Successes and trials per group.
#' @param z Normal critical value.
#' @return List with x2 (Pearson), lrt, or_hat, or_lower, or_upper.
#' @export
morie_two_group_binomial <- function(w1, n1, w2, n2, z = 1.96) {
  p_bar <- (w1 + w2) / (n1 + n2)
  stopifnot(p_bar > 0, p_bar < 1)
  x2 <- 0
  for (g in list(c(w1, n1), c(w2, n2))) {
    x2 <- x2 + (g[1] - g[2] * p_bar)^2 / (g[2] * p_bar) +
      (g[2] - g[1] - g[2] * (1 - p_bar))^2 / (g[2] * (1 - p_bar))
  }
  term <- function(w, n, ph) {
    o <- 0
    if (w > 0) o <- o + w * log(p_bar / ph)
    if (n - w > 0) o <- o + (n - w) * log((1 - p_bar) / (1 - ph))
    o
  }
  lrt <- -2 * (term(w1, n1, w1 / n1) + term(w2, n2, w2 / n2))
  out <- list(x2 = x2, lrt = lrt)
  if (w1 > 0 && w2 > 0 && w1 < n1 && w2 < n2) {
    or_hat <- (w1 / (n1 - w1)) / (w2 / (n2 - w2))
    se <- sqrt(1 / w1 + 1 / (n1 - w1) + 1 / w2 + 1 / (n2 - w2))
    out$or_hat <- or_hat
    out$or_lower <- or_hat * exp(-z * se)
    out$or_upper <- or_hat * exp(z * se)
  }
  out
}

#' Logistic regression model, likelihood, and MLE
#'
#' Equations 2.1 to 2.9 of Bilder & Loughin (2025): pi = expit(Xb), the
#' logit form, the log-likelihood, the LRT, and residual deviance.
#'
#' @param x Design matrix (with intercept column). @param y 0/1 response.
#' @param b Coefficients (fit by Newton-Raphson when NULL).
#' @return List with beta, cov, loglik, deviance, pi.
#' @export
morie_logistic_fit <- function(x, y, b = NULL) {
  x <- as.matrix(x)
  y <- as.numeric(y)
  if (is.null(b)) {
    b <- rep(0, ncol(x))
    for (i in 1:100) {
      pi <- 1 / (1 + exp(-(x %*% b)))
      wdiag <- as.numeric(pi * (1 - pi))
      step <- solve(t(x * wdiag) %*% x, t(x) %*% (y - pi))
      b <- b + as.numeric(step)
      if (max(abs(step)) < 1e-10) break
    }
  }
  pi <- as.numeric(1 / (1 + exp(-(x %*% b))))
  ll <- sum(y * log(pi) + (1 - y) * log(1 - pi))
  dev <- 0
  for (i in seq_along(y)) {
    if (y[i] > 0) dev <- dev + y[i] * log(pi[i] / y[i])
    if (1 - y[i] > 0) dev <- dev + (1 - y[i]) * log((1 - pi[i]) / (1 - y[i]))
  }
  wdiag <- pi * (1 - pi)
  list(beta = as.numeric(b),
       cov = solve(t(x * wdiag) %*% x),
       loglik = ll, deviance = -2 * dev, pi = pi)
}

#' Logistic-scale Wald intervals for odds ratios and probabilities
#'
#' Equations 2.11, 2.14 to 2.16 of Bilder & Loughin (2025); also the
#' ordinal odds-ratio interval behind stub 3e50.
#'
#' @param b1 Coefficient. @param var_b1 Its variance. @param c Unit change.
#' @param z Critical value. @param xs Covariate vector (leading 1).
#' @param cov Coefficient covariance. @param xb Linear predictor value.
#' @return List with or, or_lower, or_upper, var_xb, pi, pi_lower, pi_upper.
#' @export
morie_logistic_wald <- function(b1 = NA, var_b1 = NA, c = 1, z = 1.96,
                                xs = NULL, cov = NULL, xb = NA) {
  out <- list()
  if (!is.na(b1)) {
    half <- abs(c) * z * sqrt(var_b1)
    out$or <- exp(c * b1)
    out$or_lower <- exp(c * b1 - half)
    out$or_upper <- exp(c * b1 + half)
  }
  if (!is.null(xs)) {
    xs <- as.numeric(xs)
    out$var_xb <- as.numeric(t(xs) %*% as.matrix(cov) %*% xs)
  }
  if (!is.na(xb)) {
    v <- if (!is.null(out$var_xb)) out$var_xb else var_b1
    half <- z * sqrt(v)
    expit <- function(u) 1 / (1 + exp(-u))
    out$pi <- expit(xb)
    out$pi_lower <- expit(xb - half)
    out$pi_upper <- expit(xb + half)
  }
  out
}

#' Multinomial and contingency-table PMFs
#'
#' Equations 3.1 to 3.3 of Bilder & Loughin (2025).
#'
#' @param counts Category counts (vector, or matrix for tables).
#' @param probs Probabilities (same shape; conditional rows for the
#'   product-multinomial form).
#' @param product If TRUE use the product-multinomial row-wise form.
#' @return Numeric probability.
#' @export
morie_multinomial_pmf <- function(counts, probs, product = FALSE) {
  if (is.matrix(counts) && product) {
    out <- 1
    for (i in seq_len(nrow(counts)))
      out <- out * stats::dmultinom(counts[i, ], prob = probs[i, ])
    return(out)
  }
  stats::dmultinom(as.numeric(counts), prob = as.numeric(probs))
}

#' Multicategory logit models
#'
#' Equations 3.4, 3.8, 3.10 to 3.13, 3.16 of Bilder & Loughin (2025):
#' baseline-category logits and probabilities, one-at-a-time Wald
#' intervals, proportional and non-proportional odds forms, the polr()
#' parameterization, and category probabilities from cumulatives.
#'
#' @param bj0 Intercept. @param bjs,xs Slope and covariate vectors.
#' @param logits_2_to_j Baseline logits for categories 2..J.
#' @param cum_probs Cumulative probabilities excluding P(Y <= J) = 1.
#' @param j Category index. @param polr If TRUE negate the slopes.
#' @param pi_hat,var_pi,z Wald pieces.
#' @return List with logit, probs, pi_j, wald (as supplied).
#' @export
morie_multicategory_logit <- function(bj0 = NA, bjs = NULL, xs = NULL,
                                      logits_2_to_j = NULL,
                                      cum_probs = NULL, j = NA,
                                      polr = FALSE, pi_hat = NA,
                                      var_pi = NA, z = 1.96) {
  out <- list()
  if (!is.na(bj0) && !is.null(bjs)) {
    s <- sum(as.numeric(bjs) * as.numeric(xs))
    out$logit <- if (polr) bj0 - s else bj0 + s
  }
  if (!is.null(logits_2_to_j)) {
    zv <- c(0, as.numeric(logits_2_to_j))
    e <- exp(zv - max(zv))
    out$probs <- e / sum(e)
  }
  if (!is.null(cum_probs) && !is.na(j)) {
    cp <- c(0, as.numeric(cum_probs), 1)
    stopifnot(all(diff(cp) >= -1e-12), j >= 1, j <= length(cp) - 1)
    out$pi_j <- cp[j + 1] - cp[j]
  }
  if (!is.na(pi_hat) && !is.na(var_pi))
    out$wald <- c(lower = pi_hat - z * sqrt(var_pi),
                  upper = pi_hat + z * sqrt(var_pi))
  out
}

#' Poisson regression and loglinear models
#'
#' Equations 4.1 to 4.7, 4.12, 4.15 of Bilder & Loughin (2025).
#'
#' @param mu_hat,n,z Score interval pieces. @param b0,bs,xs Log link.
#' @param exposure Rate-model exposure t. @param beta_x_i,beta_z_j,
#'   beta_xz_ij Loglinear cell parameters.
#' @param bxz Four interaction parameters c(ij, i'j', i'j, ij') for the
#'   odds ratio of eq 4.7.
#' @param beta_z_jp,beta_xz_i,s_j,s_jp Ordinal-score ratio pieces.
#' @return List with score_ci, mu, mu_rate, mu_cell, or_loglinear,
#'   mean_ratio (as supplied).
#' @export
morie_poisson_loglinear <- function(mu_hat = NA, n = NA, z = 1.96,
                                    b0 = NA, bs = NULL, xs = NULL,
                                    exposure = NA, beta_x_i = NA,
                                    beta_z_j = NA, beta_xz_ij = NA,
                                    bxz = NULL, beta_z_jp = NA,
                                    beta_xz_i = NA, s_j = NA, s_jp = NA) {
  out <- list()
  if (!is.na(mu_hat) && !is.na(n)) {
    centre <- mu_hat + z^2 / (2 * n)
    half <- z * sqrt((mu_hat + z^2 / (4 * n)) / n)
    out$score_ci <- c(lower = centre - half, upper = centre + half)
  }
  if (!is.na(b0) && !is.null(bs)) {
    out$mu <- exp(b0 + sum(as.numeric(bs) * as.numeric(xs)))
    if (!is.na(exposure)) out$mu_rate <- exposure * out$mu
  }
  if (!is.na(beta_x_i) && !is.na(beta_z_j)) {
    lin <- (if (is.na(b0)) 0 else b0) + beta_x_i + beta_z_j
    if (!is.na(beta_xz_ij)) lin <- lin + beta_xz_ij
    out$mu_cell <- exp(lin)
  }
  if (!is.null(bxz)) {
    b <- as.numeric(bxz)
    stopifnot(length(b) == 4)
    out$or_loglinear <- exp(b[1] + b[2] - b[3] - b[4])
  }
  if (!is.na(beta_z_jp) && !is.na(beta_xz_i))
    out$mean_ratio <- exp((beta_z_j - beta_z_jp) +
                            beta_xz_i * (s_j - s_jp))
  out
}

#' BIC model averaging
#'
#' Equations 5.2 to 5.4 of Bilder & Loughin (2025).
#'
#' @param bics BIC values. @param thetas Per-model estimates.
#' @param variances Per-model variances.
#' @return List with taus, theta_ma, var_ma.
#' @export
morie_bic_model_average <- function(bics, thetas = NULL,
                                    variances = NULL) {
  b <- as.numeric(bics)
  d <- b - min(b)
  e <- exp(-d / 2)
  taus <- e / sum(e)
  out <- list(taus = taus)
  if (!is.null(thetas)) {
    th <- as.numeric(thetas)
    out$theta_ma <- sum(taus * th)
    if (!is.null(variances))
      out$var_ma <- sum(taus * ((th - out$theta_ma)^2 +
                                  as.numeric(variances)))
  }
  out
}

#' Diagnostic-test prevalence and Dorfman group testing
#'
#' Equations 6.1 to 6.3, 6.26, 6.32 of Bilder & Loughin (2025).
#'
#' @param pi Apparent positive probability. @param se,sp Sensitivity and
#'   specificity. @param i_size Group size I. @param pi_tilde True
#'   prevalence. @param b0,bs,xs Group-testing logit pieces.
#' @return List with prevalence, expected_tests, pi_group (as supplied).
#' @export
morie_diagnostic_prevalence <- function(pi = NA, se = NA, sp = NA,
                                        i_size = NA, pi_tilde = NA,
                                        b0 = NA, bs = NULL, xs = NULL) {
  out <- list()
  if (!is.na(pi)) {
    stopifnot(se + sp > 1)
    out$prevalence <- (pi + sp - 1) / (se + sp - 1)
  }
  if (!is.na(i_size) && !is.na(pi_tilde))
    out$expected_tests <- 1 + i_size *
      (se + (1 - se - sp) * (1 - pi_tilde)^i_size)
  if (!is.na(b0) && !is.null(bs))
    out$pi_group <- 1 / (1 + exp(-(b0 + sum(as.numeric(bs) *
                                              as.numeric(xs)))))
  out
}

#' Exact conditional logistic inference
#'
#' Equations 6.4 to 6.6 of Bilder & Loughin (2025): the sufficiency form
#' of the logistic joint probability and the exact conditional PMF.
#'
#' @param b,x,y Joint-probability pieces (log-likelihood returned).
#' @param t_values,counts,beta,t_obs Exact-PMF pieces.
#' @return List with loglik and/or probs, p_at_t.
#' @export
morie_exact_conditional <- function(b = NULL, x = NULL, y = NULL,
                                    t_values = NULL, counts = NULL,
                                    beta = NA, t_obs = NA) {
  out <- list()
  if (!is.null(b)) {
    xm <- as.matrix(x)
    xb <- as.numeric(xm %*% as.numeric(b))
    out$loglik <- sum(as.numeric(y) * xb - log1p(exp(xb)))
  }
  if (!is.null(t_values)) {
    ts <- as.numeric(t_values)
    cs <- as.numeric(counts)
    ln <- beta * ts + log(cs)
    ln <- ln - max(ln)
    p <- exp(ln) / sum(exp(ln))
    out$probs <- p
    out$p_at_t <- p[which.min(abs(ts - t_obs))]
  }
  out
}

#' Survey-weighted categorical estimates
#'
#' Equations 6.7 to 6.11 of Bilder & Loughin (2025): weighted category
#' totals, jackknife variances, the delta-method proportion variance, and
#' the Kott-Carr effective-sample-size interval.
#'
#' @param weights,ys,category Weighted-total pieces.
#' @param replicate_estimates,full_estimate Jackknife pieces.
#' @param var_ni,var_n,cov_ni_n,pi_hat,n_hat Delta-method pieces.
#' @param var_pi,t_crit Kott-Carr pieces.
#' @return List with n_hat_i, jackknife_var, var_pi_delta, kott_carr.
#' @export
morie_survey_categorical <- function(weights = NULL, ys = NULL,
                                     category = NULL,
                                     replicate_estimates = NULL,
                                     full_estimate = NA, var_ni = NA,
                                     var_n = NA, cov_ni_n = NA,
                                     pi_hat = NA, n_hat = NA,
                                     var_pi = NA, t_crit = NA) {
  out <- list()
  if (!is.null(weights))
    out$n_hat_i <- sum(as.numeric(weights)[ys == category])
  if (!is.null(replicate_estimates)) {
    r <- as.numeric(replicate_estimates)
    out$jackknife_var <- (length(r) - 1) / length(r) *
      sum((r - full_estimate)^2)
  }
  if (!is.na(var_ni))
    out$var_pi_delta <- (var_ni + pi_hat^2 * var_n -
                           2 * pi_hat * cov_ni_n) / n_hat^2
  if (!is.na(var_pi) && !is.na(t_crit)) {
    n_eff <- pi_hat * (1 - pi_hat) / var_pi
    t2 <- t_crit^2
    centre <- 2 * n_eff * pi_hat + t2
    half <- t_crit * sqrt(t2 + 4 * n_eff * pi_hat * (1 - pi_hat))
    out$kott_carr <- c(n_effective = n_eff,
                       lower = (centre - half) / (2 * (n_eff + t2)),
                       upper = (centre + half) / (2 * (n_eff + t2)))
  }
  out
}

#' MRCV loglinear means and GLMM linear predictors
#'
#' Equations 6.14 to 6.18, 6.20 of Bilder & Loughin (2025).
#'
#' @param b0 Intercept. @param beta_w_a,beta_y_b,beta_z_c MRCV effects.
#' @param b1,x,random_intercept GLMM pieces.
#' @return List with mu_spmi, mu_three, eta_glmm (as supplied).
#' @export
morie_mrcv_glmm <- function(b0, beta_w_a = NA, beta_y_b = NA,
                            beta_z_c = NA, b1 = NA, x = NA,
                            random_intercept = NA) {
  out <- list()
  if (!is.na(beta_w_a) && !is.na(beta_y_b)) {
    lin <- b0 + beta_w_a + beta_y_b
    if (!is.na(beta_z_c)) lin <- lin + beta_z_c
    out$mu <- exp(lin)
  }
  if (!is.na(random_intercept)) {
    slope <- if (is.na(b1) || is.na(x)) 0 else b1 * x
    out$eta_glmm <- b0 + slope + random_intercept
  }
  out
}

#' Bayesian inference for a binomial probability
#'
#' Equations 6.21 to 6.25 of Bilder & Loughin (2025): Bayes rule, the
#' Beta posterior, the Bayes estimate decomposition, and grid-normalized
#' regression posteriors.
#'
#' @param p_a_given_b,p_b,p_a_given_notb Bayes-rule pieces.
#' @param pi,w,n,a,b Beta posterior pieces.
#' @param logliks,log_priors Grid posterior pieces.
#' @return List with posterior_prob, posterior_density, bayes_estimate,
#'   grid_weights (as supplied).
#' @export
morie_bayes_binomial <- function(p_a_given_b = NA, p_b = NA,
                                 p_a_given_notb = NA, pi = NA, w = NA,
                                 n = NA, a = NA, b = NA, logliks = NULL,
                                 log_priors = NULL) {
  out <- list()
  if (!is.na(p_a_given_b)) {
    num <- p_a_given_b * p_b
    out$posterior_prob <- num / (num + p_a_given_notb * (1 - p_b))
  }
  if (!is.na(w) && !is.na(a)) {
    if (!is.na(pi))
      out$posterior_density <- stats::dbeta(pi, w + a, n - w + b)
    out$bayes_estimate <- (w + a) / (n + a + b)
  }
  if (!is.null(logliks)) {
    ln <- as.numeric(logliks) + as.numeric(log_priors)
    ln <- ln - max(ln)
    out$grid_weights <- exp(ln) / sum(exp(ln))
  }
  out
}

#' Cubic splines: piecewise, truncated power, basis and odds-ratio forms
#'
#' Equations 6.34 to 6.37 of Bilder & Loughin (2025).
#'
#' @param x Evaluation point. @param knot Single-knot piecewise pieces.
#' @param coef_left,coef_right Cubic coefficients per side.
#' @param betas,knots Truncated-power pieces (4 + D coefficients).
#' @param a,b_pt Odds-ratio evaluation points.
#' @return List with piecewise, spline, spline_or (as supplied).
#' @export
morie_spline_logit <- function(x = NA, knot = NA, coef_left = NULL,
                               coef_right = NULL, betas = NULL,
                               knots = NULL, a = NA, b_pt = NA) {
  tps <- function(xx, bb, kk) {
    v <- bb[1] + bb[2] * xx + bb[3] * xx^2 + bb[4] * xx^3
    for (d in seq_along(kk))
      if (xx > kk[d]) v <- v + bb[4 + d] * (xx - kk[d])^3
    v
  }
  out <- list()
  if (!is.na(knot) && !is.null(coef_left)) {
    cc <- if (x <= knot) as.numeric(coef_left) else as.numeric(coef_right)
    out$piecewise <- cc[1] + cc[2] * x + cc[3] * x^2 + cc[4] * x^3
  }
  if (!is.null(betas)) {
    bb <- as.numeric(betas)
    kk <- as.numeric(knots)
    if (!is.na(x)) out$spline <- tps(x, bb, kk)
    if (!is.na(a) && !is.na(b_pt))
      out$spline_or <- exp(tps(a, bb, kk) - tps(b_pt, bb, kk))
  }
  out
}
