# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cross-entropy (multinomial deviance) loss
#'
#' -sum_k y_k log p_k per observation, averaged (ESL eqs 7.7-7.8 give -2 times
#' the mean log-likelihood).
#'
#' @param y Indicator matrix (N by K) or one-hot rows.
#' @param p Predicted probabilities, same shape.
#' @return Named list: estimate (mean loss), per_observation, n, K.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 7.2.
#' @examples
#' morie_esl_cross_entropy(diag(2), rbind(c(0.8, 0.2), c(0.3, 0.7)))$estimate
#' @export
morie_esl_cross_entropy <- function(y, p) {
  y <- rbind(y)
  p <- rbind(p)
  if (any(dim(y) != dim(p))) stop("y and p must have the same shape", call. = FALSE)
  po <- -rowSums(ifelse(y > 0, y * log(p), 0))
  list(estimate = mean(po), per_observation = po, n = nrow(y), K = ncol(y))
}

#' Akaike information criterion
#'
#' AIC = -2 loglik + 2 d (ESL eq 7.29 in the -2 log-likelihood scale).
#'
#' @param loglik Maximised log-likelihood.
#' @param d Number of parameters.
#' @return Named list: estimate, loglik, d.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 7.5.
#' @examples
#' morie_esl_aic_score(-16.17, 3)$estimate
#' @export
morie_esl_aic_score <- function(loglik, d) {
  list(estimate = -2 * loglik + 2 * d, loglik = loglik, d = d)
}

#' Bayesian information criterion
#'
#' BIC = -2 loglik + d log N (ESL eq 7.35).
#'
#' @param loglik Maximised log-likelihood.
#' @param d Number of parameters.
#' @param N Sample size.
#' @return Named list: estimate, penalty, aic_penalty, penalises_more_than_aic,
#'   loglik, d, N.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 7.7.
#' @examples
#' morie_esl_bic_score(-16.17, 3, 40)$estimate
#' @export
morie_esl_bic_score <- function(loglik, d, N) {
  list(estimate = -2 * loglik + d * log(N), penalty = d * log(N), aic_penalty = 2 * d,
       penalises_more_than_aic = log(N) > 2, loglik = loglik, d = d, N = N)
}

#' Posterior model probabilities from BIC
#'
#' exp(-BIC_m / 2) / sum exp(-BIC_l / 2), computed on BIC differences (ESL eq
#' 7.41).
#'
#' @param bics BIC values.
#' @return Named list: value (first probability), probs.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 7.7.
#' @examples
#' bic_posterior_probs(c(40.1, 41.3, 45))$probs
#' @export
bic_posterior_probs <- function(bics) {
  e <- exp(-(bics - min(bics)) / 2)
  pr <- e / sum(e)
  list(value = pr[1], probs = pr)
}

#' One-standard-error rule
#'
#' The simplest model (lowest index) whose cross-validation error is within one
#' standard error of the minimum (ESL sec 7.10).
#'
#' @param cv_err Cross-validation errors, ordered by complexity.
#' @param cv_se Their standard errors.
#' @return Named list: estimate (chosen index, 1-based), index_min, threshold,
#'   chosen_error, min_error, n_within.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 7.10.
#' @examples
#' morie_esl_one_se_rule(c(0.30, 0.25, 0.22, 0.23, 0.26), rep(0.02, 5))$estimate
#' @export
morie_esl_one_se_rule <- function(cv_err, cv_se) {
  if (length(cv_err) != length(cv_se) || !length(cv_err)) stop("cv_err and cv_se must have the same positive length", call. = FALSE)
  im <- which.min(cv_err)
  th <- cv_err[im] + cv_se[im]
  ch <- which(cv_err <= th)[1]
  list(estimate = ch, index_min = im, threshold = th, chosen_error = cv_err[ch], min_error = cv_err[im],
       n_within = sum(cv_err <= th))
}

#' Vapnik-Chervonenkis bounds on the test error
#'
#' With eps = a1 (h (log(a2 N / h) + 1) - log(eta / 4)) / N, the classification
#' bound err + eps / 2 (1 + sqrt(1 + 4 err / eps)) and the regression bound err /
#' (1 - c sqrt(eps))_+ (ESL eq 7.46), plus the practical regression bound of eq
#' 7.47. Defaults a1 = 4, a2 = 2 (classification) or a1 = a2 = 1 (regression).
#'
#' @param err Training error.
#' @param h VC dimension.
#' @param N Sample size.
#' @param eta One minus the confidence.
#' @param task "classification" or "regression".
#' @param a1,a2,c Constants.
#' @return Named list: epsilon, bound, and for regression practical_bound.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 7.9.
#' @examples
#' morie_esl_vc_bound(0.1, 5, 100)$bound
#' @export
morie_esl_vc_bound <- function(err, h, N, eta = 0.05, task = c("classification", "regression"), a1 = NULL,
                               a2 = NULL, c = 1) {
  task <- match.arg(task)
  if (h <= 0 || N <= 0 || !(eta > 0 && eta < 1) || err < 0) stop("need h > 0, N > 0, 0 < eta < 1 and err >= 0", call. = FALSE)
  if (is.null(a1)) a1 <- if (task == "classification") 4 else 1
  if (is.null(a2)) a2 <- if (task == "classification") 2 else 1
  eps <- a1 * (h * (log(a2 * N / h) + 1) - log(eta / 4)) / N
  if (task == "classification") return(list(epsilon = eps, bound = err + eps / 2 * (1 + sqrt(1 + 4 * err / eps))))
  den <- 1 - c * sqrt(eps)
  rho <- h / N
  den2 <- 1 - sqrt(rho - rho * log(rho) + log(N) / (2 * N))
  list(epsilon = eps, bound = if (den > 0) err / den else Inf, practical_bound = if (den2 > 0) err / den2 else Inf)
}

#' Generalized cross-validation
#'
#' (RSS / N) / (1 - trace(S) / N)^2 (ESL eq 7.52), smooth.spline's GCV
#' criterion.
#'
#' @param y,fitted Response and fitted values.
#' @param trace_S Effective degrees of freedom.
#' @return Named list: gcv, rss, n.
#' @references Craven, P. & Wahba, G. (1979). Numerische Mathematik 31.
#' @examples
#' morie_esl_gcv(c(1, 2, 3, 5), c(1.1, 1.9, 3.2, 4.8), 2)$gcv
#' @export
morie_esl_gcv <- function(y, fitted, trace_S) {
  n <- length(y)
  if (length(fitted) != n || !(trace_S >= 0 && trace_S < n)) stop("need y and fitted of equal length and 0 <= trace(S) < N", call. = FALSE)
  rss <- sum((y - fitted)^2)
  list(gcv = rss / n / (1 - trace_S / n)^2, rss = rss, n = n)
}

#' Prediction error of a least-squares fit
#'
#' Err(x0) = sigma^2 + bias^2 + |h(x0)|^2 sigma^2 with h(x0) = X (X'X)^-1 x0
#' (ESL eq 7.11); the in-sample average variance term is p sigma^2 / N (eq
#' 7.12).
#'
#' @param X Design, N by p.
#' @param x0 Point.
#' @param sigma2 Noise variance.
#' @param bias2 Squared bias at x0.
#' @return Named list: variance, err_x0, in_sample_variance, h_norm2.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 7.3.
#' @examples
#' morie_esl_linear_prediction_error(cbind(1, sin(1:20)), c(1, 0.3), 0.5)$err_x0
#' @export
morie_esl_linear_prediction_error <- function(X, x0, sigma2, bias2 = 0) {
  X <- as.matrix(X)
  if (length(x0) != ncol(X) || sigma2 < 0 || nrow(X) <= ncol(X)) stop("need x0 with p entries, sigma2 >= 0 and N > p", call. = FALSE)
  hn2 <- sum((X %*% solve(crossprod(X), x0))^2)
  list(variance = hn2 * sigma2, err_x0 = sigma2 + bias2 + hn2 * sigma2, in_sample_variance = ncol(X) * sigma2 / nrow(X),
       h_norm2 = hn2)
}

#' Basis-expansion fit with pointwise standard errors
#'
#' beta by least squares, sigma^2 = RSS / N (or N - p) and se(mu(x)) = sqrt(h'
#' (H'H)^-1 h) sigma with a normal pointwise band (ESL eqs 8.2-8.4).
#'
#' @param H Basis at the training inputs, N by p.
#' @param y Response.
#' @param Hnew Basis at evaluation points (default H).
#' @param level Band level.
#' @param divisor "N" or "N-p".
#' @return Named list: beta, sigma2, fit, se, lower, upper.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 8.2.1.
#' @examples
#' x <- (1:30) / 10
#' morie_esl_basis_fit_se(cbind(1, x, x^2), sin(x), rbind(c(1, 1, 1)))$se
#' @export
morie_esl_basis_fit_se <- function(H, y, Hnew = NULL, level = 0.95, divisor = c("N", "N-p")) {
  divisor <- match.arg(divisor)
  H <- as.matrix(H)
  if (length(y) != nrow(H) || nrow(H) <= ncol(H)) stop("need matching H and y with N > p", call. = FALSE)
  q <- qr(H)
  b <- unname(qr.coef(q, y))
  s2 <- sum(qr.resid(q, y)^2) / (if (divisor == "N") nrow(H) else nrow(H) - ncol(H))
  Q <- if (is.null(Hnew)) H else rbind(Hnew)
  fit <- drop(Q %*% b)
  se <- sqrt(s2 * rowSums((Q %*% solve(crossprod(H))) * Q))
  z <- stats::qnorm((1 + level) / 2)
  list(beta = b, sigma2 = s2, fit = fit, se = se, lower = fit - z * se, upper = fit + z * se)
}

#' Parametric bootstrap bands for a basis-expansion fit
#'
#' Refits y* = mu_hat + N(0, RSS / N) noise B times and forms type-7 percentile
#' bands (ESL eq 8.6); uses the session RNG unless seed is given.
#'
#' @inheritParams morie_esl_basis_fit_se
#' @param B Number of bootstrap draws.
#' @param seed Optional seed; the caller's RNG state is restored.
#' @return Named list: fit, boot_se, lower, upper, B.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 8.2.1.
#' @examples
#' x <- (1:30) / 10
#' morie_esl_parametric_bootstrap(cbind(1, x, x^2), sin(x), rbind(c(1, 1, 1)), B = 100,
#'   seed = 1)$boot_se
#' @export
morie_esl_parametric_bootstrap <- function(H, y, Hnew = NULL, B = 200, level = 0.95, seed = NULL) {
  base <- morie_esl_basis_fit_se(H, y, Hnew)
  H <- as.matrix(H)
  Q <- if (is.null(Hnew)) H else rbind(Hnew)
  if (!is.null(seed)) {
    genv <- globalenv()
    old <- if (exists(".Random.seed", envir = genv, inherits = FALSE)) get(".Random.seed", envir = genv) else NULL
    on.exit(if (is.null(old)) rm(".Random.seed", envir = genv) else assign(".Random.seed", old, envir = genv))
    set.seed(seed)
  }
  mu <- drop(H %*% base$beta)
  q <- qr(H)
  D <- vapply(seq_len(B), function(b) drop(Q %*% qr.coef(q, mu + stats::rnorm(length(mu), 0, sqrt(base$sigma2)))),
              numeric(nrow(Q)))
  D <- matrix(D, nrow(Q))
  a <- (1 - level) / 2
  list(fit = base$fit, boot_se = apply(D, 1, stats::sd), lower = apply(D, 1, stats::quantile, a, names = FALSE),
       upper = apply(D, 1, stats::quantile, 1 - a, names = FALSE), B = B)
}

#' Posterior of basis coefficients under a Gaussian prior
#'
#' E(beta | Z) = (H'H + sigma^2 / tau Sigma^-1)^-1 H'y with covariance (...)^-1
#' sigma^2, and the posterior mean and sd of mu(x) = h(x)' beta (ESL eqs
#' 8.25-8.28).
#'
#' @param H Basis, N by p.
#' @param y Response.
#' @param sigma2 Noise variance.
#' @param tau Prior scale.
#' @param Sigma Prior correlation (default identity).
#' @param Hnew Basis at evaluation points (default H).
#' @return Named list: mean, cov, mu_mean, mu_sd.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 8.3.
#' @examples
#' x <- (1:30) / 10
#' morie_esl_bayes_basis_posterior(cbind(1, x, x^2), sin(x), 0.09, 2)$mean
#' @export
morie_esl_bayes_basis_posterior <- function(H, y, sigma2, tau, Sigma = NULL, Hnew = NULL) {
  H <- as.matrix(H)
  if (length(y) != nrow(H) || sigma2 <= 0 || tau <= 0) stop("need matching H and y, sigma2 > 0 and tau > 0", call. = FALSE)
  S <- if (is.null(Sigma)) diag(ncol(H)) else as.matrix(Sigma)
  Ai <- solve(crossprod(H) + sigma2 / tau * solve(S))
  m <- unname(drop(Ai %*% crossprod(H, y)))
  Q <- if (is.null(Hnew)) H else rbind(Hnew)
  list(mean = m, cov = Ai * sigma2, mu_mean = drop(Q %*% m), mu_sd = sqrt(sigma2 * rowSums((Q %*% Ai) * Q)))
}

#' Dirichlet posterior for category probabilities
#'
#' w ~ Di(a + counts) under a symmetric Di(a) prior (ESL eqs 8.32-8.34), with
#' mean, variance and the multinomial bootstrap variance w (1 - w) / N.
#'
#' @param counts Category counts.
#' @param a Prior concentration (0 is the noninformative limit).
#' @return Named list: alpha, mean, var, bootstrap_var.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 8.4.
#' @examples
#' morie_esl_dirichlet_posterior(c(12, 5, 3))$mean
#' @export
morie_esl_dirichlet_posterior <- function(counts, a = 0) {
  if (length(counts) < 2 || min(counts) < 0 || a < 0) stop("need at least two non-negative counts and a >= 0", call. = FALSE)
  al <- a + counts
  a0 <- sum(al)
  if (min(al) <= 0) stop("every posterior parameter must be positive (use a > 0 for empty categories)", call. = FALSE)
  w <- counts / sum(counts)
  list(alpha = al, mean = al / a0, var = al * (a0 - al) / (a0^2 * (a0 + 1)), bootstrap_var = w * (1 - w) / sum(counts))
}
