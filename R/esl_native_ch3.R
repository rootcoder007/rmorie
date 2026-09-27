# SPDX-License-Identifier: AGPL-3.0-or-later
.esl3_design <- function(X, y) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  y <- as.numeric(y)
  if (nrow(X) != length(y)) {
    stop(sprintf("X has %d rows but y has %d entries.", nrow(X), length(y)), call. = FALSE)
  }
  list(X = X, y = y, n = nrow(X), p = ncol(X))
}

#' Least squares by QR (ESL eq 3.6)
#'
#' beta = (X'X)^-1 X'y solved by QR; a rank-deficient design is refused.
#' Standard errors use sigma^2 = RSS / (n - p) (eq 3.8).
#'
#' @param X Design matrix including any intercept column, n > p.
#' @param y Response.
#' @return Named list: estimate, beta, se, sigma2, rss, df_residual, n, p.
#' @references Hastie, T., Tibshirani, R. & Friedman, J. (2009). The
#'   Elements of Statistical Learning (2nd ed.), sec. 3.2.
#' @examples
#' morie_esl_ols_normal_equations(cbind(1, 0:3), c(1, 3, 5, 7))$beta
#' @export
morie_esl_ols_normal_equations <- function(X, y) {
  d <- .esl3_design(X, y)
  if (d$n <= d$p) stop(sprintf("OLS needs n > p; got n=%d, p=%d.", d$n, d$p), call. = FALSE)
  q <- qr(d$X)
  if (q$rank < d$p) {
    stop(sprintf("the design matrix is rank deficient (rank %d < p = %d); beta is not unique.", q$rank, d$p),
         call. = FALSE)
  }
  b <- unname(qr.coef(q, d$y))
  rss <- sum(qr.resid(q, d$y)^2)
  s2 <- rss / (d$n - d$p)
  list(estimate = b[1], beta = b, se = sqrt(s2 * diag(chol2inv(qr.R(q)))[order(q$pivot)]), sigma2 = s2,
       rss = rss, df_residual = d$n - d$p, n = d$n, p = d$p)
}

#' Residual sum of squares at given coefficients
#'
#' RSS(beta) = sum (y_i - x_i' beta)^2 at the supplied beta (ESL eq 3.2).
#'
#' @param X Design matrix.
#' @param y Response.
#' @param beta Coefficients.
#' @return Named list: estimate, residuals, mean_squared_error, n, p.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 3.2.
#' @examples
#' morie_esl_residual_sum_squares(cbind(1, 0:3), c(1, 3, 5, 8), c(1, 2))$estimate
#' @export
morie_esl_residual_sum_squares <- function(X, y, beta) {
  d <- .esl3_design(X, y)
  beta <- as.numeric(beta)
  if (length(beta) != d$p) stop(sprintf("X has %d columns but beta has %d entries.", d$p, length(beta)), call. = FALSE)
  r <- d$y - drop(d$X %*% beta)
  list(estimate = sum(r^2), residuals = r, mean_squared_error = sum(r^2) / d$n, n = d$n, p = d$p)
}

#' Covariance of the least-squares coefficients
#'
#' Var(beta) = sigma^2 (X'X)^-1 (ESL eq 3.8), depending on the design only.
#'
#' @param X Design matrix of full column rank.
#' @param sigma2 Noise variance.
#' @return Named list: estimate, covariance (p x p), variances, se, n, p.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 3.2.
#' @examples
#' morie_esl_var_beta_hat(cbind(1, c(1, -1, 1, -1)), 1)$variances
#' @export
morie_esl_var_beta_hat <- function(X, sigma2) {
  X <- as.matrix(X)
  if (sigma2 <= 0) stop(sprintf("the noise variance must be positive; got %s.", format(sigma2)), call. = FALSE)
  q <- qr(X)
  if (q$rank < ncol(X)) stop(sprintf("X'X is singular (rank < %d); the design is collinear.", ncol(X)), call. = FALSE)
  v <- sigma2 * solve(crossprod(X))
  list(estimate = sqrt(v[1, 1]), covariance = v, variances = diag(v), se = sqrt(diag(v)),
       n = nrow(X), p = ncol(X))
}

#' Standard errors of given coefficients
#'
#' se(beta_j) = sqrt(sigma^2 v_jj) with sigma^2 = RSS(beta) / (n - p) at the
#' supplied beta (ESL eq 3.8).
#'
#' @param X Design matrix, n > p.
#' @param y Response.
#' @param beta Coefficients.
#' @return Named list: estimate, se, sigma2_hat, rss, df_residual, n, p.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 3.2.
#' @examples
#' morie_esl_se_beta(cbind(1, c(1, -1, 1, -1)), c(3, -1, 3, -1), c(0, 2))$sigma2_hat
#' @export
morie_esl_se_beta <- function(X, y, beta) {
  d <- .esl3_design(X, y)
  if (d$n <= d$p) stop(sprintf("estimating sigma^2 needs n > p; got n=%d, p=%d.", d$n, d$p), call. = FALSE)
  rss <- morie_esl_residual_sum_squares(d$X, d$y, beta)$estimate
  s2 <- rss / (d$n - d$p)
  se <- if (s2 == 0) rep(0, d$p) else morie_esl_var_beta_hat(d$X, s2)$se
  list(estimate = se[1], se = se, sigma2_hat = s2, rss = rss, df_residual = d$n - d$p, n = d$n, p = d$p)
}

#' Z-scores of regression coefficients
#'
#' z_j = beta_j / se(beta_j) (ESL eq 3.12) with two-sided normal and t (n - p
#' df) p-values; a zero standard error gives an infinite z.
#'
#' @param X Design matrix.
#' @param y Response.
#' @param beta Coefficients.
#' @return Named list: estimate, z, se, p_normal, p_t, df, n, p.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 3.2.
#' @examples
#' X <- cbind(1, c(1, -1, 1, -1))
#' y <- c(3.2, -0.8, 2.8, -1.2)
#' morie_esl_z_score(X, y, qr.coef(qr(X), y))$z
#' @export
morie_esl_z_score <- function(X, y, beta) {
  f <- morie_esl_se_beta(X, y, beta)
  beta <- as.numeric(beta)
  z <- ifelse(f$se > 0, beta / f$se, ifelse(beta == 0, 0, Inf * sign(beta)))
  list(estimate = z[1], z = z, se = f$se, p_normal = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
       p_t = 2 * stats::pt(abs(z), f$df_residual, lower.tail = FALSE), df = f$df_residual, n = f$n, p = f$p)
}

#' F test of nested linear models
#'
#' F = ((RSS0 - RSS1) / (p1 - p0)) / (RSS1 / (N - p1 - 1)) (ESL eq 3.13);
#' an intercept is always included.
#'
#' @param model0,model1 Column indices (1-based) of X in each model; model0
#'   inside model1.
#' @param X Predictors without an intercept column.
#' @param y Response.
#' @return Named list: statistic, p_value, df1, df2, rss0, rss1, p0, p1, n.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 3.2.
#' @examples
#' i <- 1:20
#' X <- cbind(sin(i), cos(i), log(i))
#' morie_esl_f_test(1, 1:3, X, 1 + X[, 1] + 0.5 * X[, 3] + 0.1 * cos(7 * i))$statistic
#' @export
morie_esl_f_test <- function(model0, model1, X, y) {
  d <- .esl3_design(X, y)
  m0 <- sort(unique(as.integer(model0)))
  m1 <- sort(unique(as.integer(model1)))
  if (!all(m0 %in% m1)) stop("model0 must be nested inside model1", call. = FALSE)
  rss <- function(cols) sum(qr.resid(qr(cbind(1, d$X[, cols, drop = FALSE])), d$y)^2)
  r0 <- rss(m0)
  r1 <- rss(m1)
  df1 <- length(m1) - length(m0)
  df2 <- d$n - length(m1) - 1
  if (df1 <= 0 || df2 <= 0) stop("need p1 > p0 and N > p1 + 1", call. = FALSE)
  s <- ((r0 - r1) / df1) / (r1 / df2)
  list(statistic = s, p_value = stats::pf(s, df1, df2, lower.tail = FALSE), df1 = df1, df2 = df2,
       rss0 = r0, rss1 = r1, p0 = length(m0), p1 = length(m1), n = d$n)
}

#' Ridge regression with an unpenalised intercept
#'
#' beta = (X'X + lambda P)^-1 X'y where P is the identity with zeros for
#' constant columns (ESL eqs 3.44, 3.50); effective_df = trace of the hat
#' matrix. Columns are not standardised here.
#'
#' @param X Design matrix (a constant column is the unpenalised intercept).
#' @param y Response.
#' @param lambda Penalty, >= 0.
#' @return Named list: estimate, beta, effective_df, rss, lambda,
#'   intercept_penalised, columns_standardised, n, p.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 3.4.1.
#' @examples
#' morie_esl_ridge(cbind(1, 0:3), c(1, 3, 5, 7), 1)$beta
#' @export
morie_esl_ridge <- function(X, y, lambda) {
  d <- .esl3_design(X, y)
  if (lambda < 0) stop(sprintf("the ridge penalty must be non-negative; got %s.", format(lambda)), call. = FALSE)
  const <- apply(d$X, 2, function(v) diff(range(v)) == 0)
  P <- diag(as.numeric(!const), d$p)
  G <- crossprod(d$X) + lambda * P
  b <- drop(solve(G, crossprod(d$X, d$y)))
  H <- d$X %*% solve(G, t(d$X))
  free <- !const
  std <- if (any(free)) isTRUE(all(abs(apply(d$X[, free, drop = FALSE], 2, function(v) sqrt(mean((v - mean(v))^2))) - 1) < 1e-8)) else TRUE
  list(estimate = b[1], beta = unname(b), effective_df = sum(diag(H)), rss = sum((d$y - d$X %*% b)^2),
       lambda = lambda, intercept_penalised = FALSE, columns_standardised = std, n = d$n, p = d$p)
}

#' Lasso by cyclic coordinate descent
#'
#' Minimises (1/2) RSS + lambda sum |beta_j| over the non-constant columns
#' (ESL eqs 3.51-3.52) with the exact soft-threshold update; this lambda is n
#' times glmnet's.
#'
#' @param X Design matrix without all-zero columns.
#' @param y Response.
#' @param lambda Penalty, >= 0.
#' @param max_iter,tol Coordinate-descent controls.
#' @return Named list: estimate, beta, n_nonzero, active_set (1-based),
#'   objective, iterations, converged, lambda, n, p.
#' @references Tibshirani, R. (1996). JRSS B 58, 267-288.
#' @examples
#' morie_esl_lasso(diag(2), c(3, -1), 0.5)$beta
#' @export
morie_esl_lasso <- function(X, y, lambda, max_iter = 10000, tol = 1e-12) {
  d <- .esl3_design(X, y)
  if (lambda < 0) stop(sprintf("the lasso penalty must be non-negative; got %s.", format(lambda)), call. = FALSE)
  colsq <- colSums(d$X^2)
  if (any(colsq == 0)) stop("an all-zero column cannot be penalised meaningfully.", call. = FALSE)
  const <- apply(d$X, 2, function(v) diff(range(v)) == 0)
  beta <- numeric(d$p)
  r <- d$y
  converged <- FALSE
  it <- 0L
  for (it in seq_len(max_iter)) {
    delta <- 0
    for (j in seq_len(d$p)) {
      old <- beta[j]
      rho <- sum(d$X[, j] * r) + colsq[j] * old
      pen <- if (const[j]) 0 else lambda
      new <- sign(rho) * max(abs(rho) - pen, 0) / colsq[j]
      if (new != old) {
        r <- r + d$X[, j] * (old - new)
        beta[j] <- new
        delta <- max(delta, abs(new - old))
      }
    }
    if (delta < tol) {
      converged <- TRUE
      break
    }
  }
  active <- which(beta != 0)
  list(estimate = beta[1], beta = beta, n_nonzero = length(active), active_set = active,
       objective = 0.5 * sum(r^2) + lambda * sum(abs(beta[!const])), iterations = it,
       converged = converged, lambda = lambda, n = d$n, p = d$p)
}

#' Principal components regression
#'
#' beta = sum over the first M components of (z_m'y / z_m'z_m) v_m on the
#' centred design (ESL eq 3.61), returned on the original scale with the
#' intercept separately.
#'
#' @param X Predictors without an intercept column.
#' @param y Response.
#' @param M Number of components, 1 <= M <= min(n - 1, p).
#' @return Named list: estimate, beta, intercept, variance_explained,
#'   singular_values, M, n, p.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 3.5.1.
#' @examples
#' morie_esl_pcr(cbind(c(0, 1, 2, 3), c(1, 0, 2, 1)), c(1, 2, 5, 5), 1)$beta
#' @export
morie_esl_pcr <- function(X, y, M) {
  d <- .esl3_design(X, y)
  kmax <- min(d$n - 1, d$p)
  if (!(M >= 1 && M <= kmax)) stop(sprintf("M must lie in [1, %d]; got %d.", kmax, as.integer(M)), call. = FALSE)
  xbar <- colMeans(d$X)
  ybar <- mean(d$y)
  Xc <- sweep(d$X, 2, xbar)
  s <- svd(Xc)
  beta <- numeric(d$p)
  for (m in seq_len(M)) {
    z <- drop(Xc %*% s$v[, m])
    zz <- sum(z^2)
    if (zz > 0) beta <- beta + (sum(z * (d$y - ybar)) / zz) * s$v[, m]
  }
  tot <- sum(s$d^2)
  list(estimate = beta[1], beta = beta, intercept = ybar - sum(xbar * beta),
       variance_explained = if (tot > 0) sum(s$d[seq_len(M)]^2) / tot else NaN,
       singular_values = s$d, M = M, n = d$n, p = d$p)
}

#' Effective degrees of freedom of a linear smoother
#'
#' df(S) = trace(S) (ESL eq 5.16, 7.34), with trace(S S') and trace(2 S - S S')
#' alongside; a symmetric idempotent S is a projection.
#'
#' @param S Square smoother matrix.
#' @return Named list: estimate, trace_ssT, df_variance, is_projection, n.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 5.4.1.
#' @examples
#' X <- cbind(1, 0:3)
#' morie_esl_effective_dof(X %*% solve(crossprod(X), t(X)))$estimate
#' @export
morie_esl_effective_dof <- function(S) {
  S <- as.matrix(S)
  if (nrow(S) != ncol(S)) stop(sprintf("the smoother matrix must be square; got shape (%d, %d).", nrow(S), ncol(S)), call. = FALSE)
  list(estimate = sum(diag(S)), trace_ssT = sum(S * S), df_variance = sum(diag(2 * S - S %*% t(S))),
       is_projection = isTRUE(all.equal(S, t(S))) && isTRUE(all.equal(S %*% S, S)), n = nrow(S))
}
