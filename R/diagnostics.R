# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie - Multi-domain Open Research and Inferential Estimation
# Copyright (C) 2026 Vansh Singh Ruhela and morie contributors
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with this program.  If not, see
# <https://www.gnu.org/licenses/>.

# ---------------------------------------------------------------------------
# Model diagnostics  (R port of src/morie/diagnostics.py)
# ---------------------------------------------------------------------------
# Residual analysis, leverage / Cook's distance / DFFITS / DFBETAS,
# multicollinearity (VIF, condition number, variance decomposition),
# specification tests (RESET, link, Hosmer-Lemeshow, PH-assumption),
# goodness-of-fit, and a one-shot full report.

# ---- Result containers ----------------------------------------------------

.new_residual_diag <- function(raw, std, student, deviance, pearson,
                               fitted, normality, hetero, autoc,
                               outlier_indices) {
  structure(
    list(raw_residuals = raw,
         standardized_residuals = std,
         studentized_residuals = student,
         deviance_residuals = deviance,
         pearson_residuals = pearson,
         fitted_values = fitted,
         normality_test = normality,
         heteroskedasticity_test = hetero,
         autocorrelation_test = autoc,
         n_outliers = length(outlier_indices),
         outlier_indices = outlier_indices),
    class = c("morie_residual_diagnostics", "list")
  )
}

.new_influence_diag <- function(h, cooks, dffits, dfbetas, covratio,
                                influential, high_lev, high_cook) {
  structure(
    list(hat_values = h, cooks_distance = cooks,
         dffits = dffits, dfbetas = dfbetas, covratio = covratio,
         n_influential = length(influential),
         influential_indices = influential,
         high_leverage_indices = high_lev,
         high_cooksd_indices = high_cook),
    class = c("morie_influence_diagnostics", "list")
  )
}

.new_collin_diag <- function(vif, cond_num, cond_idx, var_decomp,
                             eigvals, n_collin, pairs) {
  structure(
    list(vif = vif, condition_number = cond_num,
         condition_indices = cond_idx,
         variance_decomposition = var_decomp,
         eigenvalues = eigvals,
         n_collinear = n_collin, collinear_pairs = pairs),
    class = c("morie_collinearity_diagnostics", "list")
  )
}

.new_spec_test <- function(name, statistic, p_value, df, conclusion) {
  structure(
    list(name = name, statistic = statistic, p_value = p_value,
         df = df, conclusion = conclusion),
    class = c("morie_specification_test", "list")
  )
}

.new_gof <- function(r_squared, adj_r_squared, pseudo_r_squared,
                    aic, bic, log_likelihood, deviance, pearson_chi2,
                    df_model, df_residual, f_statistic, f_pvalue, n_obs) {
  structure(
    list(r_squared = r_squared, adj_r_squared = adj_r_squared,
         pseudo_r_squared = pseudo_r_squared,
         aic = aic, bic = bic, log_likelihood = log_likelihood,
         deviance = deviance, pearson_chi2 = pearson_chi2,
         df_model = df_model, df_residual = df_residual,
         f_statistic = f_statistic, f_pvalue = f_pvalue,
         n_obs = n_obs),
    class = c("morie_goodness_of_fit", "list")
  )
}

.new_diag_report <- function(residuals, influence, collinearity,
                             gof, spec_tests, assessment) {
  structure(
    list(residuals = residuals, influence = influence,
         collinearity = collinearity, goodness_of_fit = gof,
         specification_tests = spec_tests,
         overall_assessment = assessment),
    class = c("morie_diagnostic_report", "list")
  )
}

# Solve / pseudo-inverse helper.
.safe_solve <- function(A) {
  res <- try(solve(A), silent = TRUE)
  if (inherits(res, "try-error")) MASS::ginv(A) else res
}

#' Compute residual diagnostics
#'
#' Returns raw / standardised / externally studentised residuals along
#' with normality, heteroskedasticity (Breusch-Pagan), and
#' autocorrelation (Durbin-Watson) tests.  Optionally also returns
#' deviance and Pearson residuals for logistic / Poisson GLMs.
#'
#' @param y Observed response.
#' @param y_hat Fitted values.
#' @param X Design matrix.
#' @param model_type \code{"linear"}, \code{"logistic"}, or \code{"poisson"}.
#' @return A \code{morie_residual_diagnostics} list.
#' @export
compute_residuals <- function(y, y_hat, X, model_type = "linear") {
  y <- as.numeric(y)
  y_hat <- as.numeric(y_hat)
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)

  raw <- y - y_hat
  XtX <- crossprod(X)
  XtX_inv <- .safe_solve(XtX)
  H <- X %*% XtX_inv %*% t(X)
  h <- pmin(pmax(diag(H), 0), 1 - 1e-10)
  mse <- sum(raw ^ 2) / max(n - p, 1)
  std_res <- raw / sqrt(max(mse, 1e-10) * (1 - h))

  student_res <- numeric(n)
  for (i in seq_len(n)) {
    mse_i <- sum(raw[-i] ^ 2) / max(n - p - 1, 1)
    student_res[i] <- raw[i] / sqrt(max(mse_i * (1 - h[i]), 1e-10))
  }

  deviance_res <- NULL
  pearson_res <- NULL
  if (identical(model_type, "logistic")) {
    yh <- pmin(pmax(y_hat, 1e-10), 1 - 1e-10)
    deviance_res <- ifelse(y == 1,
                           sqrt(-2 * log(yh)),
                           -sqrt(-2 * log(1 - yh)))
    pearson_res <- (y - yh) / sqrt(yh * (1 - yh))
  } else if (identical(model_type, "poisson")) {
    yh <- pmax(y_hat, 1e-10)
    deviance_res <- sign(y - yh) *
      sqrt(2 * (y * log(pmax(y / yh, 1e-10)) - (y - yh)))
    pearson_res <- (y - yh) / sqrt(yh)
  }

  # Normality.
  if (n >= 3 && n <= 5000) {
    sw <- try(stats::shapiro.test(raw), silent = TRUE)
    if (inherits(sw, "try-error")) {
      normality <- list(statistic = NA_real_, p_value = NA_real_)
    } else {
      normality <- list(statistic = unname(sw$statistic),
                        p_value = unname(sw$p.value))
    }
  } else {
    # Fall back to a chi-squared moment test (D'Agostino-Pearson style).
    z <- (raw - mean(raw)) / stats::sd(raw)
    skew <- mean(z ^ 3)
    kurt <- mean(z ^ 4) - 3
    stat <- n * (skew ^ 2 / 6 + kurt ^ 2 / 24)
    normality <- list(statistic = as.numeric(stat),
                      p_value = 1 - stats::pchisq(stat, df = 2))
  }

  # Breusch-Pagan-style heteroskedasticity test.
  bp <- try({
    r_sq <- raw ^ 2
    r_sq_c <- r_sq - mean(r_sq)
    X_c <- sweep(X, 2, colMeans(X))
    beta_bp <- .safe_solve(crossprod(X_c)) %*% crossprod(X_c, r_sq_c)
    ss_reg <- sum((X_c %*% beta_bp) ^ 2)
    ss_tot <- sum(r_sq_c ^ 2)
    stat <- n * ss_reg / max(ss_tot, 1e-10)
    list(statistic = as.numeric(stat),
         p_value = 1 - stats::pchisq(stat, df = max(p - 1, 1)))
  }, silent = TRUE)
  if (inherits(bp, "try-error")) {
    bp <- list(statistic = NA_real_, p_value = NA_real_)
  }

  dw <- sum(diff(raw) ^ 2) / max(sum(raw ^ 2), 1e-10)
  autocorrelation <- list(durbin_watson = as.numeric(dw))

  outlier_indices <- which(abs(student_res) > 3)

  .new_residual_diag(
    raw = raw, std = std_res, student = student_res,
    deviance = deviance_res, pearson = pearson_res,
    fitted = y_hat, normality = normality,
    hetero = bp, autoc = autocorrelation,
    outlier_indices = outlier_indices
  )
}

#' Compute leverage and influence diagnostics
#'
#' Hat-matrix diagonal, Cook's distance (\code{stats::cooks.distance}
#' is preferred for fitted \code{lm}s; this function works straight
#' from \code{X}, \code{y}, and a fitted \code{y_hat}), DFFITS,
#' DFBETAS, and COVRATIO.
#'
#' @param y Response vector.
#' @param X Design matrix.
#' @param y_hat Optional fitted values (OLS is used if NULL).
#' @return A \code{morie_influence_diagnostics} list.
#' @export
compute_influence <- function(y, X, y_hat = NULL) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)

  beta <- drop(.safe_solve(crossprod(X)) %*% crossprod(X, y))
  if (is.null(y_hat)) y_hat <- drop(X %*% beta)
  residuals <- y - y_hat

  XtX_inv <- .safe_solve(crossprod(X))
  H <- X %*% XtX_inv %*% t(X)
  h <- pmin(pmax(diag(H), 0), 1 - 1e-10)
  mse <- sum(residuals ^ 2) / max(n - p, 1)

  cooks_d <- (residuals ^ 2 * h) / (p * mse * (1 - h) ^ 2 + 1e-10)

  mse_i <- numeric(n)
  for (i in seq_len(n))
    mse_i[i] <- sum(residuals[-i] ^ 2) / max(n - p - 1, 1)

  dffits <- residuals * sqrt(h / ((1 - h) * mse_i + 1e-10)) /
    sqrt(max(mse, 1e-10))

  dfbetas <- matrix(NA_real_, nrow = n, ncol = p)
  for (i in seq_len(n)) {
    fit_i <- try(.safe_solve(crossprod(X[-i, , drop = FALSE])) %*%
                   crossprod(X[-i, , drop = FALSE], y[-i]),
                 silent = TRUE)
    beta_i <- if (inherits(fit_i, "try-error")) beta else drop(fit_i)
    se_beta <- sqrt(pmax(diag(mse_i[i] * XtX_inv), 0))
    dfbetas[i, ] <- (beta - beta_i) / pmax(se_beta, 1e-10)
  }

  covratio <- numeric(n)
  for (i in seq_len(n)) {
    s2_i <- mse_i[i]
    t_i <- residuals[i] / sqrt(max(s2_i * (1 - h[i]), 1e-10))
    covratio[i] <- 1 /
      ((((n - p - 1 + t_i ^ 2) / (n - p)) ^ p) * (1 - h[i]) + 1e-10)
  }

  leverage_threshold <- 2 * p / n
  cooksd_threshold <- 4 / n
  high_leverage <- which(h > leverage_threshold)
  high_cooksd <- which(cooks_d > cooksd_threshold)
  influential <- sort(union(high_leverage, high_cooksd))

  .new_influence_diag(
    h = h, cooks = cooks_d, dffits = dffits,
    dfbetas = dfbetas, covratio = covratio,
    influential = influential,
    high_lev = high_leverage, high_cook = high_cooksd
  )
}

#' Variance Inflation Factors
#'
#' For each column j of X, regresses column j on the remaining
#' columns (plus an intercept) and returns 1/(1 - R^2).
#'
#' @param X Design matrix (without intercept).
#' @param column_names Optional character vector of names.
#' @return Named numeric vector of VIFs.
#' @export
compute_vif <- function(X, column_names = NULL) {
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  if (is.null(column_names))
    column_names <- if (!is.null(colnames(X))) colnames(X)
                    else paste0("X", seq_len(p) - 1L)

  vifs <- numeric(p)
  for (j in seq_len(p)) {
    y_j <- X[, j]
    X_j <- cbind(1, X[, -j, drop = FALSE])
    vif <- tryCatch({
      beta <- drop(.safe_solve(crossprod(X_j)) %*% crossprod(X_j, y_j))
      y_pred <- drop(X_j %*% beta)
      ss_res <- sum((y_j - y_pred) ^ 2)
      ss_tot <- sum((y_j - mean(y_j)) ^ 2)
      r2 <- 1 - ss_res / max(ss_tot, 1e-10)
      1 / max(1 - r2, 1e-10)
    }, error = function(e) Inf)
    vifs[j] <- vif
  }
  stats::setNames(vifs, column_names)
}

#' Comprehensive multicollinearity diagnostics
#'
#' @param X Design matrix.
#' @param column_names Optional column names.
#' @return A \code{morie_collinearity_diagnostics} list.
#' @export
collinearity_diagnostics <- function(X, column_names = NULL) {
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  if (is.null(column_names))
    column_names <- if (!is.null(colnames(X))) colnames(X)
                    else paste0("X", seq_len(p) - 1L)

  vifs <- compute_vif(X, column_names)

  X_scaled <- X / sqrt(matrix(colSums(X ^ 2), nrow = n, ncol = p,
                              byrow = TRUE) + 1e-10)

  svdres <- tryCatch(svd(X_scaled),
                     error = function(e) NULL)
  if (is.null(svdres)) {
    eigenvalues <- rep(1, p)
    var_decomp_df <- data.frame()
  } else {
    eigenvalues <- sort(svdres$d ^ 2, decreasing = TRUE)
    S2 <- svdres$d ^ 2
    phi <- svdres$v ^ 2
    denom <- rowSums(phi / matrix(S2 + 1e-10, nrow = p,
                                  ncol = p, byrow = TRUE))
    var_decomp <- (phi / matrix(S2 + 1e-10, nrow = p, ncol = p,
                                byrow = TRUE)) /
      pmax(denom, 1e-10)
    var_decomp_df <- as.data.frame(var_decomp,
                                   row.names = column_names)
    colnames(var_decomp_df) <- paste0("comp_", seq_len(p) - 1L)
  }

  condition_number <- sqrt(eigenvalues[1] / max(eigenvalues[p], 1e-10))
  condition_indices <- sqrt(eigenvalues[1] / pmax(eigenvalues, 1e-10))

  corr <- suppressWarnings(stats::cor(X))
  collinear_pairs <- list()
  if (p >= 2L) {
    for (i in seq_len(p - 1L)) {
      for (j in (i + 1L):p) {
        cij <- corr[i, j]
        if (!is.na(cij) && abs(cij) > 0.8) {
          collinear_pairs[[length(collinear_pairs) + 1L]] <- list(
            var1 = column_names[i], var2 = column_names[j],
            correlation = as.numeric(cij)
          )
        }
      }
    }
  }
  n_collinear <- sum(vifs > 10, na.rm = TRUE)

  .new_collin_diag(
    vif = vifs, cond_num = as.numeric(condition_number),
    cond_idx = condition_indices, var_decomp = var_decomp_df,
    eigvals = eigenvalues, n_collin = n_collinear,
    pairs = collinear_pairs
  )
}

#' Ramsey RESET test for functional-form misspecification
#'
#' @param y Response vector.
#' @param X Design matrix (with intercept).
#' @param powers Integer vector of powers of fitted values to add
#'   to the auxiliary regression (default \code{c(2, 3)}).
#' @return A \code{morie_specification_test}.
#' @export
ramsey_reset_test <- function(y, X, powers = c(2, 3)) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)

  beta_r <- drop(.safe_solve(crossprod(X)) %*% crossprod(X, y))
  y_hat <- drop(X %*% beta_r)
  ssr_r <- sum((y - y_hat) ^ 2)

  X_u <- X
  for (pw in powers) X_u <- cbind(X_u, y_hat ^ pw)
  p_u <- ncol(X_u)
  beta_u <- drop(.safe_solve(crossprod(X_u)) %*% crossprod(X_u, y))
  y_hat_u <- drop(X_u %*% beta_u)
  ssr_u <- sum((y - y_hat_u) ^ 2)

  df1 <- p_u - p
  df2 <- n - p_u
  f_stat <- ((ssr_r - ssr_u) / max(df1, 1)) / (ssr_u / max(df2, 1))
  f_p <- 1 - stats::pf(f_stat, df1, df2)

  conclusion <- if (!is.na(f_p) && f_p < 0.05)
    "Reject functional form (p < 0.05): consider nonlinear terms."
  else "No evidence of misspecification."

  .new_spec_test(name = "RESET",
                 statistic = as.numeric(f_stat),
                 p_value = as.numeric(f_p),
                 df = df1, conclusion = conclusion)
}

#' Pregibon link test
#'
#' @param y Response.
#' @param X Design matrix.
#' @param model_type \code{"linear"} or \code{"logistic"}.
#' @return A \code{morie_specification_test}.
#' @export
link_test <- function(y, X, model_type = "linear") {
  y <- as.numeric(y)
  X <- as.matrix(X)
  n <- length(y)

  beta <- drop(.safe_solve(crossprod(X)) %*% crossprod(X, y))
  y_hat <- drop(X %*% beta)

  X_link <- cbind(1, y_hat, y_hat ^ 2)
  beta_link <- drop(.safe_solve(crossprod(X_link)) %*%
                      crossprod(X_link, y))
  y_link <- drop(X_link %*% beta_link)
  residuals <- y - y_link
  mse <- sum(residuals ^ 2) / max(n - 3, 1)

  ts <- tryCatch({
    XtX_inv <- .safe_solve(crossprod(X_link))
    se <- sqrt(pmax(diag(mse * XtX_inv), 0))
    t_stat <- beta_link[3] / max(se[3], 1e-10)
    p_val <- 2 * (1 - stats::pt(abs(t_stat), df = n - 3))
    list(stat = as.numeric(t_stat), p = as.numeric(p_val))
  }, error = function(e) list(stat = NA_real_, p = NA_real_))

  conclusion <- if (!is.na(ts$p) && ts$p < 0.05)
    "Reject link specification (p < 0.05): consider alternative link function."
  else "No evidence of link misspecification."

  .new_spec_test(name = "link_test",
                 statistic = ts$stat, p_value = ts$p,
                 df = n - 3, conclusion = conclusion)
}

#' Hosmer-Lemeshow goodness-of-fit test for logistic regression
#'
#' @param y Binary response vector.
#' @param y_prob Predicted probabilities.
#' @param n_groups Number of decile groups (default 10).
#' @return A \code{morie_specification_test}.
#' @export
hosmer_lemeshow_test <- function(y, y_prob, n_groups = 10L) {
  y <- as.numeric(y)
  y_prob <- as.numeric(y_prob)
  n <- length(y)
  order_idx <- order(y_prob)
  groups <- split(order_idx, cut(seq_along(order_idx),
                                 n_groups, labels = FALSE))

  chi2_stat <- 0
  for (g in groups) {
    n_g <- length(g)
    if (n_g == 0L) next
    obs_events <- sum(y[g])
    exp_events <- sum(y_prob[g])
    obs_non <- n_g - obs_events
    exp_non <- n_g - exp_events
    if (exp_events > 0) chi2_stat <- chi2_stat +
      (obs_events - exp_events) ^ 2 / exp_events
    if (exp_non    > 0) chi2_stat <- chi2_stat +
      (obs_non - exp_non) ^ 2 / exp_non
  }

  df <- n_groups - 2L
  p_value <- 1 - stats::pchisq(chi2_stat, df)

  conclusion <- if (p_value < 0.05)
    "Poor fit (p < 0.05): model does not adequately fit the data."
  else "Adequate fit: no evidence of poor calibration."

  .new_spec_test(name = "hosmer_lemeshow",
                 statistic = as.numeric(chi2_stat),
                 p_value = as.numeric(p_value),
                 df = df, conclusion = conclusion)
}

#' Comprehensive goodness-of-fit statistics
#'
#' R^2 and adjusted R^2 for linear models; McFadden pseudo-R^2,
#' deviance and Pearson chi-squared for logistic / Poisson; AIC,
#' BIC, log-likelihood, and the omnibus F-test for linear models.
#'
#' @param y Response vector.
#' @param y_hat Fitted values.
#' @param X Design matrix.
#' @param model_type \code{"linear"}, \code{"logistic"}, \code{"poisson"}.
#' @param log_likelihood Optional precomputed log-likelihood.
#' @return A \code{morie_goodness_of_fit} list.
#' @export
compute_goodness_of_fit <- function(y, y_hat, X,
                                    model_type = "linear",
                                    log_likelihood = NULL) {
  y <- as.numeric(y)
  y_hat <- as.numeric(y_hat)
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)

  residuals <- y - y_hat
  ss_res <- sum(residuals ^ 2)
  ss_tot <- sum((y - mean(y)) ^ 2)
  df_model <- p - 1L
  df_residual <- n - p

  r_squared <- NULL
  adj_r_squared <- NULL
  pseudo_r_squared <- NULL
  f_stat <- NULL
  f_p <- NULL
  deviance <- NULL
  pearson_chi2 <- NULL

  if (identical(model_type, "linear")) {
    r_squared <- 1 - ss_res / max(ss_tot, 1e-10)
    adj_r_squared <- 1 - (1 - r_squared) * (n - 1) / max(df_residual, 1)
    mse_model <- (ss_tot - ss_res) / max(df_model, 1)
    mse_res   <- ss_res / max(df_residual, 1)
    f_stat <- mse_model / max(mse_res, 1e-10)
    f_p <- 1 - stats::pf(f_stat, df_model, df_residual)
    if (is.null(log_likelihood))
      log_likelihood <- -n / 2 *
        (log(2 * pi) + log(ss_res / n) + 1)
  } else if (identical(model_type, "logistic")) {
    yh <- pmin(pmax(y_hat, 1e-10), 1 - 1e-10)
    if (is.null(log_likelihood))
      log_likelihood <- sum(y * log(yh) + (1 - y) * log(1 - yh))
    p_bar <- mean(y)
    ll_null <- sum(y * log(p_bar) + (1 - y) * log(1 - p_bar))
    pseudo_r_squared <- 1 - log_likelihood / min(ll_null, -1e-10)
    deviance <- -2 * log_likelihood
    pearson_chi2 <- sum((y - yh) ^ 2 / (yh * (1 - yh) + 1e-10))
  } else if (identical(model_type, "poisson")) {
    yh <- pmax(y_hat, 1e-10)
    if (is.null(log_likelihood))
      log_likelihood <- sum(y * log(yh) - yh)
    ll_null <- sum(y * log(mean(y)) - mean(y))
    pseudo_r_squared <- 1 - log_likelihood / min(ll_null, -1e-10)
    deviance <- 2 * sum(y * log(pmax(y / yh, 1e-10)) - (y - yh))
    pearson_chi2 <- sum((y - yh) ^ 2 / yh)
  }

  aic <- -2 * log_likelihood + 2 * p
  bic <- -2 * log_likelihood + log(n) * p

  .new_gof(
    r_squared = r_squared, adj_r_squared = adj_r_squared,
    pseudo_r_squared = pseudo_r_squared,
    aic = as.numeric(aic), bic = as.numeric(bic),
    log_likelihood = as.numeric(log_likelihood),
    deviance = deviance, pearson_chi2 = pearson_chi2,
    df_model = df_model, df_residual = df_residual,
    f_statistic = if (!is.null(f_stat)) as.numeric(f_stat) else NULL,
    f_pvalue   = if (!is.null(f_p))    as.numeric(f_p)    else NULL,
    n_obs = n
  )
}

#' Proportional-hazards assumption test (Schoenfeld-style)
#'
#' Correlates each covariate with rank-transformed event times.
#'
#' @param survival_times Event/censoring times.
#' @param event_indicator 1 = event, 0 = censored.
#' @param covariates Covariate matrix.
#' @param covariate_names Optional names.
#' @return A list of \code{morie_specification_test} objects, one per
#'   covariate.
#' @export
ph_assumption_test <- function(survival_times, event_indicator,
                               covariates, covariate_names = NULL) {
  times <- as.numeric(survival_times)
  events <- as.integer(event_indicator)
  X <- as.matrix(covariates)
  p <- ncol(X)

  if (is.null(covariate_names))
    covariate_names <- if (!is.null(colnames(X))) colnames(X)
                       else paste0("X", seq_len(p) - 1L)

  event_mask <- events == 1L
  event_times <- times[event_mask]
  event_X <- X[event_mask, , drop = FALSE]
  ranked_times <- rank(event_times)

  out <- vector("list", p)
  for (j in seq_len(p)) {
    cor_res <- suppressWarnings(stats::cor.test(ranked_times,
                                                event_X[, j],
                                                method = "spearman"))
    rho <- unname(cor_res$estimate)
    p_val <- unname(cor_res$p.value)

    conclusion <- if (!is.na(p_val) && p_val < 0.05)
      sprintf("PH violation for %s (p < 0.05).", covariate_names[j])
    else
      sprintf("PH assumption holds for %s.", covariate_names[j])

    out[[j]] <- .new_spec_test(
      name = paste0("ph_test:", covariate_names[j]),
      statistic = as.numeric(rho),
      p_value = as.numeric(p_val),
      df = length(event_times) - 2L,
      conclusion = conclusion
    )
  }
  out
}

#' Likelihood ratio test for nested models
#'
#' @param ll_restricted Log-likelihood of the restricted model.
#' @param ll_full Log-likelihood of the full model.
#' @param df_diff Difference in degrees of freedom.
#' @return A \code{morie_specification_test}.
#' @export
likelihood_ratio_test <- function(ll_restricted, ll_full, df_diff) {
  lr_stat <- -2 * (ll_restricted - ll_full)
  p_value <- 1 - stats::pchisq(lr_stat, df_diff)
  conclusion <- if (p_value < 0.05)
    "Full model significantly improves fit (p < 0.05)."
  else "No significant improvement from full model."

  .new_spec_test(name = "likelihood_ratio",
                 statistic = as.numeric(lr_stat),
                 p_value = as.numeric(p_value),
                 df = df_diff, conclusion = conclusion)
}

#' Wald test for linear restrictions on parameters
#'
#' Tests H0: \code{R \%*\% beta = r}.
#'
#' @param estimates Parameter estimates.
#' @param vcov Variance-covariance matrix.
#' @param R Optional restriction matrix (default identity).
#' @param r Optional restriction vector (default zeros).
#' @return A \code{morie_specification_test}.
#' @export
wald_test <- function(estimates, vcov, R = NULL, r = NULL) {
  beta <- as.numeric(estimates)
  V <- as.matrix(vcov)
  p <- length(beta)
  if (is.null(R)) R <- diag(p) else R <- as.matrix(R)
  if (is.null(r)) r <- rep(0, nrow(R)) else r <- as.numeric(r)
  q <- nrow(R)
  diffv <- drop(R %*% beta - r)
  meat <- R %*% V %*% t(R)
  w_stat <- tryCatch(as.numeric(crossprod(diffv, solve(meat, diffv))),
                     error = function(e) {
                       as.numeric(crossprod(diffv, MASS::ginv(meat) %*% diffv))
                     })
  p_value <- 1 - stats::pchisq(w_stat, q)
  conclusion <- if (p_value < 0.05)
    "Reject null hypothesis (p < 0.05)."
  else
    "Cannot reject null hypothesis."

  .new_spec_test(name = "wald",
                 statistic = w_stat, p_value = as.numeric(p_value),
                 df = q, conclusion = conclusion)
}

#' Score (Lagrange multiplier) test
#'
#' @param score_vector Score vector evaluated under H0.
#' @param information_matrix Information matrix under H0.
#' @return A \code{morie_specification_test}.
#' @export
score_test <- function(score_vector, information_matrix) {
  U <- as.numeric(score_vector)
  I_mat <- as.matrix(information_matrix)
  q <- length(U)
  s_stat <- tryCatch(as.numeric(crossprod(U, solve(I_mat, U))),
                     error = function(e)
                       as.numeric(crossprod(U, MASS::ginv(I_mat) %*% U)))
  p_value <- 1 - stats::pchisq(s_stat, q)
  conclusion <- if (p_value < 0.05) "Reject H0." else "Cannot reject H0."
  .new_spec_test(name = "score", statistic = s_stat,
                 p_value = as.numeric(p_value), df = q,
                 conclusion = conclusion)
}

#' Full diagnostic report
#'
#' Runs residual, influence, collinearity, goodness-of-fit, and
#' specification tests, then summarises the overall assessment.
#'
#' @param y Response.
#' @param X Design matrix.
#' @param y_hat Optional fitted values (OLS used if NULL).
#' @param model_type \code{"linear"}, \code{"logistic"}, \code{"poisson"}.
#' @param column_names Optional column names for X.
#' @return A \code{morie_diagnostic_report}.
#' @export
full_diagnostics <- function(y, X, y_hat = NULL,
                             model_type = "linear",
                             column_names = NULL) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  if (is.null(y_hat)) {
    beta <- drop(.safe_solve(crossprod(X)) %*% crossprod(X, y))
    y_hat <- drop(X %*% beta)
  }

  residuals <- compute_residuals(y, y_hat, X, model_type)
  influence <- compute_influence(y, X, y_hat)
  collinearity <- collinearity_diagnostics(X, column_names)
  gof <- compute_goodness_of_fit(y, y_hat, X, model_type)

  spec_tests <- list()
  if (identical(model_type, "linear"))
    spec_tests <- c(spec_tests, list(ramsey_reset_test(y, X)))
  spec_tests <- c(spec_tests, list(link_test(y, X, model_type)))
  if (identical(model_type, "logistic"))
    spec_tests <- c(spec_tests, list(hosmer_lemeshow_test(y, y_hat)))

  issues <- character(0)
  if (!is.na(residuals$normality_test$p_value) &&
      residuals$normality_test$p_value < 0.05)
    issues <- c(issues, "non-normal residuals")
  if (!is.na(residuals$heteroskedasticity_test$p_value) &&
      residuals$heteroskedasticity_test$p_value < 0.05)
    issues <- c(issues, "heteroskedasticity")
  if (residuals$n_outliers > 0)
    issues <- c(issues, sprintf("%d outlier(s)", residuals$n_outliers))
  if (influence$n_influential > 0)
    issues <- c(issues,
                sprintf("%d influential point(s)", influence$n_influential))
  if (collinearity$n_collinear > 0)
    issues <- c(issues,
                sprintf("%d collinear variable(s)",
                        collinearity$n_collinear))

  assessment <- if (length(issues))
    paste0("Issues detected: ", paste(issues, collapse = "; "))
  else
    "No major diagnostic issues detected."

  .new_diag_report(residuals, influence, collinearity,
                   gof, spec_tests, assessment)
}
