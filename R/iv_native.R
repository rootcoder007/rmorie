# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native instrumental-variables and interrupted-time-series engines
# (feat/native-specializations, module 17). Replaces ivreg / AER
# (2SLS, LIML), gmm (two-step / CUE GMM, Hansen J) and plm (panel IV)
# with base-R implementations, and adds morie_its(): segmented
# regression with native Newey-West HAC inference.

#' Internal helper: k-class IV estimator with robust variance
#'
#' beta(k) = (X'(I - k M_Z) X)^\{-1\} X'(I - k M_Z) y where M_Z is the
#' annihilator of the instrument set. k = 1 gives 2SLS, k = 0 OLS, and
#' the LIML eigenvalue gives LIML. Robust (HC1) variance uses the
#' projected regressors as scores, matching
#' sandwich::vcovHC(ivreg-fit, "HC1").
#'
#' @srrstats {G1.0} Theil (1961) k-class; Fuller (1977); standard
#'   errors per White (1980) with the HC1 small-sample factor.
#' @noRd
.morie_iv_kclass_native <- function(y, X, Z, kappa = 1,
                                    robust = TRUE, cluster = NULL) {
  n <- length(y)
  k <- ncol(X)
  ZtZ_inv <- tryCatch(solve(crossprod(Z)),
                      error = function(e) .morie_ginv(crossprod(Z)))
  PzX <- Z %*% (ZtZ_inv %*% crossprod(Z, X))
  Pzy <- Z %*% (ZtZ_inv %*% crossprod(Z, y))
  # X'(I - kappa M_Z) X = (1-kappa) X'X + kappa X'PzX
  A <- (1 - kappa) * crossprod(X) + kappa * crossprod(X, PzX)
  b <- (1 - kappa) * crossprod(X, y) + kappa * crossprod(X, Pzy)
  A_inv <- tryCatch(solve(A), error = function(e) .morie_ginv(A))
  beta <- as.numeric(A_inv %*% b)
  names(beta) <- colnames(X)
  resid <- as.numeric(y - X %*% beta)
  if (!is.null(cluster)) {
    # Cluster-robust sandwich. Same bread as HC1, but the meat sums the
    # score over each cluster before squaring it, so within-cluster
    # correlation is carried instead of assumed away. Scores use the
    # projected regressors, as the HC1 branch below does.
    if (length(cluster) != n) {
      stop("`cluster` has length ", length(cluster), "; expected ", n,
           ", one per observation.", call. = FALSE)
    }
    g <- unique(cluster)
    G <- length(g)
    if (G < 2L) {
      stop("cluster-robust errors need at least two clusters; got ", G,
           ".", call. = FALSE)
    }
    meat <- matrix(0, k, k)
    for (cl in g) {
      idx <- which(cluster == cl)
      s <- crossprod(PzX[idx, , drop = FALSE], resid[idx])
      meat <- meat + tcrossprod(as.numeric(s))
    }
    correction <- (G / (G - 1)) * ((n - 1) / (n - k))
    V <- correction * (A_inv %*% meat %*% A_inv)
  } else if (robust) {
    # HC1 with projected scores (the 2SLS sandwich).
    meat <- crossprod(PzX, resid^2 * PzX) * n / (n - k)
    V <- A_inv %*% meat %*% A_inv
  } else {
    s2 <- sum(resid^2) / (n - k)
    V <- s2 * A_inv
  }
  se <- sqrt(pmax(diag(V), 0))
  names(se) <- colnames(X)
  list(beta = beta, se = se, vcov = V, residuals = resid,
       n = n, df = n - k, kappa = kappa, PzX = PzX)
}

#' Internal helper: design matrices for the IV estimators
#' @noRd
.morie_iv_design <- function(data, outcome, endogenous, instruments,
                             exogenous = NULL) {
  vars <- unique(c(outcome, endogenous, instruments, exogenous))
  df <- data[stats::complete.cases(data[, vars, drop = FALSE]), ,
             drop = FALSE]
  y <- as.numeric(df[[outcome]])
  X <- cbind(`(Intercept)` = 1,
             as.matrix(df[, c(endogenous, exogenous), drop = FALSE]))
  Z <- cbind(`(Intercept)` = 1,
             as.matrix(df[, c(instruments, exogenous), drop = FALSE]))
  storage.mode(X) <- "double"
  storage.mode(Z) <- "double"
  list(y = y, X = X, Z = Z, df = df)
}

#' Internal helper: LIML kappa via the eigenvalue problem
#'
#' kappa = smallest eigenvalue of (W' M_1 W)(W' M_Z W)^\{-1\} where
#' W = \[y, endogenous\], M_1 annihilates the exogenous block (incl.
#' intercept) and M_Z annihilates the full instrument set.
#' @noRd
.morie_iv_liml_kappa <- function(y, X_endo, Z_full, X_exo) {
  W <- cbind(y, X_endo)
  M <- function(A, B) {
    # residual maker of B applied to A
    A - B %*% (tryCatch(solve(crossprod(B)),
                        error = function(e) .morie_ginv(crossprod(B))) %*%
                 crossprod(B, A))
  }
  W1 <- M(W, X_exo)
  WZ <- M(W, Z_full)
  G <- crossprod(W, W1)
  H <- crossprod(W, WZ)
  ev <- eigen(solve(H, G), only.values = TRUE)$values
  min(Re(ev[abs(Im(ev)) < 1e-8]))
}

#' Internal helper: two-step efficient GMM for linear IV
#'
#' Step 1 is 2SLS; step 2 re-weights with the inverse of the
#' heteroskedasticity-robust moment covariance. Returns the Hansen J
#' statistic as a by-product.
#'
#' @srrstats {G1.0} Hansen (1982); the two-step efficient linear GMM
#'   estimator with an HC0 weight matrix.
#' @noRd
.morie_iv_gmm2_native <- function(y, X, Z) {
  n <- length(y)
  first <- .morie_iv_kclass_native(y, X, Z, kappa = 1)
  e1 <- first$residuals
  S <- crossprod(Z, e1^2 * Z) / n
  S_inv <- tryCatch(solve(S), error = function(e) .morie_ginv(S))
  XtZ <- crossprod(X, Z)
  Zty <- crossprod(Z, y)
  A <- XtZ %*% S_inv %*% t(XtZ)
  A_inv <- tryCatch(solve(A), error = function(e) .morie_ginv(A))
  beta <- as.numeric(A_inv %*% (XtZ %*% S_inv %*% Zty))
  names(beta) <- colnames(X)
  resid <- as.numeric(y - X %*% beta)
  # Efficient-GMM variance: n * (X'Z S2^{-1} Z'X)^{-1} with S2 at beta2
  S2 <- crossprod(Z, resid^2 * Z) / n
  S2_inv <- tryCatch(solve(S2), error = function(e) .morie_ginv(S2))
  A2 <- XtZ %*% S2_inv %*% t(XtZ)
  V <- n * tryCatch(solve(A2), error = function(e) .morie_ginv(A2))
  se <- sqrt(pmax(diag(V), 0)) / sqrt(n) * sqrt(n)  # V already scaled
  se <- sqrt(pmax(diag(V), 0)) / sqrt(n)
  names(se) <- colnames(X)
  gbar <- as.numeric(crossprod(Z, resid)) / n
  J <- n * as.numeric(t(gbar) %*% S2_inv %*% gbar)
  df_J <- ncol(Z) - ncol(X)
  list(beta = beta, se = se, vcov = V / n, residuals = resid,
       J = J, J_df = df_J,
       J_p = if (df_J > 0) stats::pchisq(J, df_J, lower.tail = FALSE)
             else NA_real_,
       n = n)
}

#' Internal helper: continuously-updated GMM (CUE)
#'
#' Minimizes n * gbar(beta)' S(beta)^\{-1\} gbar(beta) over beta with
#' stats::optim (BFGS), started at the two-step estimate.
#' @noRd
.morie_iv_cue_native <- function(y, X, Z, max_iter = 200L,
                                 tol = 1e-10) {
  n <- length(y)
  two <- .morie_iv_gmm2_native(y, X, Z)
  obj <- function(beta) {
    e <- as.numeric(y - X %*% beta)
    g <- as.numeric(crossprod(Z, e)) / n
    S <- crossprod(Z, e^2 * Z) / n
    S_inv <- tryCatch(solve(S), error = function(err) .morie_ginv(S))
    n * as.numeric(t(g) %*% S_inv %*% g)
  }
  opt <- stats::optim(two$beta, obj, method = "BFGS",
                      control = list(maxit = max_iter, reltol = tol))
  beta <- opt$par
  names(beta) <- colnames(X)
  e <- as.numeric(y - X %*% beta)
  S <- crossprod(Z, e^2 * Z) / n
  S_inv <- tryCatch(solve(S), error = function(err) .morie_ginv(S))
  XtZ <- crossprod(X, Z)
  A <- XtZ %*% S_inv %*% t(XtZ)
  V <- tryCatch(solve(A), error = function(err) .morie_ginv(A))
  se <- sqrt(pmax(diag(V), 0))
  names(se) <- colnames(X)
  list(beta = beta, se = se, vcov = V, residuals = e,
       J = opt$value, converged = opt$convergence == 0, n = n)
}

# ---------------------------------------------------------------------------
# Interrupted time series (module 17, new construction)
# ---------------------------------------------------------------------------

#' Internal helper: Newey-West (Bartlett) HAC covariance for OLS
#'
#' @srrstats {G1.0} Newey & West (1987); automatic lag
#'   4 * (n/100)^(2/9) per the common rule of thumb.
#' @noRd
.morie_nw_vcov <- function(X, resid, lag = NULL) {
  n <- nrow(X)
  k <- ncol(X)
  if (is.null(lag)) lag <- floor(4 * (n / 100)^(2 / 9))
  XtX_inv <- tryCatch(solve(crossprod(X)),
                      error = function(e) .morie_ginv(crossprod(X)))
  U <- X * resid
  omega <- crossprod(U)
  if (lag > 0) {
    for (l in seq_len(lag)) {
      w <- 1 - l / (lag + 1)
      G <- crossprod(U[seq_len(n - l), , drop = FALSE],
                     U[(l + 1):n, , drop = FALSE])
      omega <- omega + w * (G + t(G))
    }
  }
  # HC1-style small-sample scaling
  adj <- n / (n - k)
  XtX_inv %*% (adj * omega) %*% XtX_inv
}

#' Interrupted time-series analysis (segmented regression)
#'
#' Estimates the canonical single-interruption segmented regression
#' \deqn{Y_t = \beta_0 + \beta_1 t + \beta_2 D_t + \beta_3 (t - t_0) D_t
#'   + \varepsilon_t}{Y_t = b0 + b1 t + b2 D + b3 (t - t0) D + e}
#' where \eqn{D_t}{D} switches on at the interruption. \eqn{\beta_2}
#' is the immediate level change and \eqn{\beta_3} the slope change.
#' Inference uses native Newey-West (Bartlett-kernel) HAC standard
#' errors to respect serial correlation.
#'
#' @param data Data frame ordered by (or containing) the time index.
#' @param outcome Outcome column name.
#' @param time Time column name (numeric).
#' @param interruption_time First period of the intervention.
#' @param covariates Optional covariate column names.
#' @param lag HAC lag; default is the Newey-West rule of thumb.
#' @param alpha Significance level.
#' @return A list with \code{level_change}, \code{slope_change} (each a
#'   list with estimate/std_error/ci/p_value), \code{coefficients},
#'   \code{counterfactual} (data frame with observed, fitted, and
#'   no-intervention paths), \code{n_pre}, \code{n_post},
#'   \code{method}.
#' @references Bernal, J. L., Cummins, S., & Gasparrini, A. (2017).
#'   Interrupted time series regression for the evaluation of public
#'   health interventions. \emph{IJE}, 46(1), 348--355.
#' @examples
#' df <- data.frame(t = 1:60,
#'                  y = 10 + 0.2 * (1:60) + ifelse(1:60 >= 40, 5, 0) +
#'                    rnorm(60))
#' fit <- morie_its(df, "y", "t", interruption_time = 40)
#' fit$level_change$estimate
#' @export
morie_its <- function(data, outcome, time, interruption_time,
                      covariates = NULL, lag = NULL, alpha = 0.05) {
  df <- as.data.frame(data)
  df <- df[order(df[[time]]), , drop = FALSE]
  t_vec <- as.numeric(df[[time]])
  y <- as.numeric(df[[outcome]])
  D <- as.numeric(t_vec >= interruption_time)
  if (sum(D) < 2L || sum(1 - D) < 3L) {
    stop("Need >= 3 pre-interruption and >= 2 post-interruption ",
         "points.", call. = FALSE)
  }
  post_t <- (t_vec - interruption_time) * D
  X <- cbind(`(Intercept)` = 1, t = t_vec, level = D, trend = post_t)
  if (length(covariates)) {
    Xc <- as.matrix(df[, covariates, drop = FALSE])
    storage.mode(Xc) <- "double"
    X <- cbind(X, Xc)
  }
  qrX <- qr(X)
  beta <- qr.coef(qrX, y)
  beta[is.na(beta)] <- 0
  resid <- as.numeric(y - X %*% beta)
  V <- .morie_nw_vcov(X, resid, lag = lag)
  se <- sqrt(pmax(diag(V), 0))
  z <- stats::qnorm(1 - alpha / 2)
  piece <- function(nm) {
    i <- match(nm, colnames(X))
    list(estimate = unname(beta[i]), std_error = unname(se[i]),
         ci_lower = unname(beta[i] - z * se[i]),
         ci_upper = unname(beta[i] + z * se[i]),
         p_value = unname(2 * stats::pnorm(-abs(beta[i] / se[i]))))
  }
  cf_beta <- beta
  cf_beta[c("level", "trend")] <- 0
  counterfactual <- data.frame(
    time = t_vec, observed = y,
    fitted = as.numeric(X %*% beta),
    no_intervention = as.numeric(X %*% cf_beta),
    post = D == 1)
  list(
    level_change = piece("level"),
    slope_change = piece("trend"),
    coefficients = stats::setNames(as.numeric(beta), colnames(X)),
    std_errors = stats::setNames(as.numeric(se), colnames(X)),
    counterfactual = counterfactual,
    n_pre = sum(D == 0), n_post = sum(D == 1),
    hac_lag = if (is.null(lag)) floor(4 * (length(y) / 100)^(2 / 9))
              else lag,
    method = "interrupted time series (rmorie native, Newey-West HAC)"
  )
}
