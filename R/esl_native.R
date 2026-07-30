# SPDX-License-Identifier: AGPL-3.0-or-later

## R parity for the morie.fn ESL shelf (Hastie, Tibshirani & Friedman 2009).
## Ported at full precision from the Python modules of the same name so the
## two agree to machine precision on the shared anchors.

.morie_esl_logmvn <- function(X, mu, S) {
  p <- ncol(X)
  L <- chol(S)                       # R's chol is upper-triangular
  z <- backsolve(L, t(sweep(X, 2L, mu, "-")), transpose = TRUE)
  -0.5 * (p * log(2 * pi) + colSums(z^2)) - sum(log(diag(L)))
}

#' EM for a Gaussian mixture
#'
#' Fits a k-component Gaussian mixture by expectation-maximisation. The
#' observed-data log-likelihood is checked to be non-decreasing at run time --
#' a decrease is a bug, not slow convergence. `reg` ridges each covariance
#' diagonal, which is what stops a component collapsing onto a single point
#' and driving the likelihood to infinity.
#'
#' @param X Data matrix, n by p (a vector is treated as one column).
#' @param k Number of components.
#' @param max_iter Maximum EM iterations.
#' @param tol Stop when the log-likelihood improves by less than this.
#' @param reg Ridge added to each covariance diagonal.
#' @param seed Seed for the k-means++ style initialisation.
#' @return List with `pi`, `mu`, `sigma`, `resp`, `labels`, `loglik`,
#'   `loglik_path`, `n_iter`, `converged`, `aic`, `bic`.
#' @references Hastie, T., Tibshirani, R., & Friedman, J. (2009). The Elements
#'   of Statistical Learning (2nd ed.), Sec 8.5. Springer.
#' @examples
#' set.seed(1)
#' x <- c(rnorm(200, -4, 0.5), rnorm(200, 4, 0.5))
#' fit <- morie_esl_em_gmm(x, k = 2)
#' sort(round(as.numeric(fit$mu), 1))
#' @export
morie_esl_em_gmm <- function(X, k = 2, max_iter = 200L, tol = 1e-6,
                             reg = 1e-6, seed = 0L) {
  X <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1L)
  n <- nrow(X); p <- ncol(X); k <- as.integer(k)
  if (k < 1L) stop("k must be at least 1", call. = FALSE)
  if (k > n) stop(sprintf("k=%d exceeds the number of observations (%d)", k, n),
                  call. = FALSE)
  set.seed(seed)
  centres <- X[sample.int(n, 1L), , drop = FALSE]
  if (k > 1L) for (i in seq_len(k - 1L)) {
    d2 <- apply(centres, 1L, function(c0) rowSums(sweep(X, 2L, c0, "-")^2))
    d2 <- if (is.matrix(d2)) apply(d2, 1L, min) else d2
    pr <- if (sum(d2) > 0) d2 / sum(d2) else rep(1 / n, n)
    centres <- rbind(centres, X[sample.int(n, 1L, prob = pr), , drop = FALSE])
  }
  mu <- centres
  S0 <- stats::cov(X); S0 <- matrix(S0, p, p) + reg * diag(p)
  sigma <- array(rep(S0, k), dim = c(p, p, k))
  pik <- rep(1 / k, k)

  path <- numeric(0); prev <- -Inf; converged <- FALSE; it <- 0L
  for (it in seq_len(max_iter)) {
    logp <- matrix(0, n, k)
    for (j in seq_len(k)) {
      logp[, j] <- log(pik[j] + 1e-300) +
        .morie_esl_logmvn(X, mu[j, ], sigma[, , j])
    }
    mx <- apply(logp, 1L, max)
    lse <- mx + log(rowSums(exp(logp - mx)))
    ll <- sum(lse)
    resp <- exp(logp - lse)
    path <- c(path, ll)
    if (ll + 1e-9 < prev) {
      stop(sprintf("EM log-likelihood decreased (%.10g -> %.10g); this is a bug",
                   prev, ll), call. = FALSE)
    }
    if (abs(ll - prev) < tol) { converged <- TRUE; prev <- ll; break }
    prev <- ll
    Nk <- colSums(resp) + 1e-300
    pik <- Nk / n
    mu <- (t(resp) %*% X) / Nk
    for (j in seq_len(k)) {
      d <- sweep(X, 2L, mu[j, ], "-")
      sigma[, , j] <- crossprod(d * resp[, j], d) / Nk[j] + reg * diag(p)
    }
  }
  npar <- k - 1 + k * p + k * p * (p + 1) / 2
  list(pi = pik, mu = mu, sigma = sigma, resp = resp,
       labels = max.col(resp) - 1L, loglik = prev, loglik_path = path,
       n_iter = it, converged = converged,
       aic = 2 * npar - 2 * prev, bic = npar * log(n) - 2 * prev,
       n = n, k = k, method = "esl_em_gmm")
}

#' Gaussian mixture density
#'
#' Fits a mixture with [morie_esl_em_gmm()] and evaluates the resulting
#' density. ESL Sec 6.8 presents the mixture as a density estimate -- a
#' smoother with a data-driven bandwidth -- rather than as a clustering
#' device, and the density is the object that view needs.
#'
#' @param X Training data, n by p.
#' @param k Number of components.
#' @param newdata Points at which to evaluate; defaults to `X`.
#' @param ... Passed to [morie_esl_em_gmm()].
#' @return List with `density`, `log_density`, `pi`, `mu`, `sigma`, `loglik`,
#'   `aic`, `bic`.
#' @references Hastie, T., et al. (2009). ESL (2nd ed.), Sec 6.8. Springer.
#' @examples
#' set.seed(1)
#' x <- c(rnorm(300, -3), rnorm(300, 3))
#' d <- morie_esl_gaussian_mixture(x, k = 2, newdata = c(-3, 0))
#' d$density[1] > d$density[2]
#' @export
morie_esl_gaussian_mixture <- function(X, k = 2, newdata = NULL, ...) {
  X <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1L)
  fit <- morie_esl_em_gmm(X, k = k, ...)
  Z <- if (is.null(newdata)) X else
    if (is.matrix(newdata)) newdata else matrix(as.numeric(newdata), ncol = 1L)
  if (ncol(Z) != ncol(X)) {
    stop(sprintf("newdata has %d columns but X has %d", ncol(Z), ncol(X)),
         call. = FALSE)
  }
  comp <- matrix(0, nrow(Z), k)
  for (j in seq_len(k)) {
    comp[, j] <- log(fit$pi[j] + 1e-300) +
      .morie_esl_logmvn(Z, fit$mu[j, ], fit$sigma[, , j])
  }
  mx <- apply(comp, 1L, max)
  logd <- mx + log(rowSums(exp(comp - mx)))
  list(density = exp(logd), log_density = logd, pi = fit$pi, mu = fit$mu,
       sigma = fit$sigma, loglik = fit$loglik, aic = fit$aic, bic = fit$bic,
       resp = fit$resp, labels = fit$labels,
       method = "esl_gaussian_mixture")
}

#' Iteratively reweighted least squares for a GLM
#'
#' Fisher scoring for the binomial and Poisson families with their canonical
#' links. Separation is detected from the fitted probabilities rather than
#' from a coefficient threshold: under separation the probabilities are pinned
#' at 0/1 long before any coefficient grows large enough to trip a cut-off.
#'
#' @param X Design matrix, n by p, without an intercept column.
#' @param y Response: 0/1 for binomial, non-negative counts for Poisson.
#' @param beta0 Starting coefficients; defaults to zeros.
#' @param family "binomial" or "poisson".
#' @param max_iter Maximum IRLS iterations.
#' @param tol Convergence tolerance on the coefficient change.
#' @param add_intercept Prepend a column of ones.
#' @return List with `beta`, `se`, `z`, `p_value`, `fitted`, `loglik`,
#'   `deviance`, `n_iter`, `converged`, `separated`.
#' @references Hastie, T., et al. (2009). ESL (2nd ed.), Sec 4.4.1. Springer.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(2000), ncol = 2)
#' y <- rbinom(1000, 1, plogis(-0.5 + 1.5 * X[, 1] - X[, 2]))
#' round(morie_esl_iwls(X, y)$beta, 1)
#' @export
morie_esl_iwls <- function(X, y, beta0 = NULL, family = "binomial",
                           max_iter = 50L, tol = 1e-8, add_intercept = TRUE) {
  X <- as.matrix(X); y <- as.numeric(y)
  if (nrow(X) != length(y)) {
    stop(sprintf("X has %d rows but y has %d", nrow(X), length(y)), call. = FALSE)
  }
  if (!family %in% c("binomial", "poisson")) {
    stop('family must be "binomial" or "poisson"', call. = FALSE)
  }
  if (family == "binomial" && !all(y %in% c(0, 1))) {
    stop("binomial y must be 0/1", call. = FALSE)
  }
  if (family == "poisson" && any(y < 0)) {
    stop("poisson y must be non-negative", call. = FALSE)
  }
  if (add_intercept) X <- cbind(1, X)
  n <- nrow(X); p <- ncol(X)
  beta <- if (is.null(beta0)) numeric(p) else as.numeric(beta0)
  if (length(beta) != p) stop(sprintf("beta0 must have %d entries", p), call. = FALSE)

  converged <- FALSE; it <- 0L
  for (it in seq_len(max_iter)) {
    eta <- as.numeric(X %*% beta)
    if (family == "binomial") {
      mu <- 1 / (1 + exp(-pmin(pmax(eta, -500), 500)))
      w <- pmax(mu * (1 - mu), 1e-10)
    } else {
      mu <- exp(pmin(pmax(eta, -500), 500)); w <- pmax(mu, 1e-10)
    }
    z <- eta + (y - mu) / w
    WX <- X * w
    new <- tryCatch(solve(crossprod(X, WX), crossprod(WX, z)),
                    error = function(e) qr.solve(crossprod(X, WX), crossprod(WX, z)))
    new <- as.numeric(new)
    delta <- max(abs(new - beta)); beta <- new
    if (delta < tol) { converged <- TRUE; break }
  }
  eta <- as.numeric(X %*% beta)
  if (family == "binomial") {
    mu <- 1 / (1 + exp(-pmin(pmax(eta, -500), 500)))
    w <- pmax(mu * (1 - mu), 1e-10)
    ll <- sum(y * log(mu + 1e-300) + (1 - y) * log(1 - mu + 1e-300))
  } else {
    mu <- exp(pmin(pmax(eta, -500), 500)); w <- pmax(mu, 1e-10)
    ll <- sum(y * log(mu + 1e-300) - mu - lgamma(y + 1))
  }
  separated <- family == "binomial" &&
    (max(abs(beta)) > 25 || all(abs(mu - y) < 1e-6))
  se <- tryCatch(sqrt(pmax(diag(solve(crossprod(X, X * w))), 0)),
                 error = function(e) rep(NA_real_, p))
  zst <- beta / se
  list(beta = beta, se = se, z = zst,
       p_value = 2 * stats::pnorm(-abs(zst)),
       fitted = mu, loglik = ll, deviance = -2 * ll,
       n_iter = it, converged = converged, separated = separated,
       family = family, method = "esl_iwls")
}

#' Logistic regression
#'
#' IRLS fit with the prediction path attached. The 0.5 threshold minimises
#' error rate only under equal misclassification costs and roughly balanced
#' classes; on imbalanced data it will predict the majority class everywhere.
#'
#' @param X Design matrix, n by p.
#' @param y Binary 0/1 response.
#' @param newdata Points to predict at; defaults to `X`.
#' @param threshold Probability cut-off in (0, 1).
#' @param ... Passed to [morie_esl_iwls()].
#' @return List with `beta`, `se`, `p_value`, `odds_ratio`, `prob`, `class`,
#'   `accuracy`, `confusion`, `loglik`, `deviance`.
#' @references Hastie, T., et al. (2009). ESL (2nd ed.), Sec 4.4. Springer.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(1000), ncol = 1)
#' y <- rbinom(1000, 1, plogis(0.3 + 2 * X[, 1]))
#' round(morie_esl_logistic_reg(X, y)$beta[2], 1)
#' @export
morie_esl_logistic_reg <- function(X, y, newdata = NULL, threshold = 0.5, ...) {
  if (threshold <= 0 || threshold >= 1) {
    stop("threshold must be in (0, 1)", call. = FALSE)
  }
  fit <- morie_esl_iwls(X, y, family = "binomial", ...)
  beta <- fit$beta
  Z <- as.matrix(if (is.null(newdata)) X else newdata)
  Z <- cbind(1, Z)
  if (ncol(Z) != length(beta)) {
    stop(sprintf("newdata gives %d columns but beta has %d", ncol(Z), length(beta)),
         call. = FALSE)
  }
  prob <- 1 / (1 + exp(-pmin(pmax(as.numeric(Z %*% beta), -500), 500)))
  cls <- as.integer(prob >= threshold)
  yv <- as.numeric(y)
  acc <- if (is.null(newdata)) mean(cls == yv) else NA_real_
  conf <- if (is.null(newdata))
    matrix(c(sum(yv == 0 & cls == 0), sum(yv == 0 & cls == 1),
             sum(yv == 1 & cls == 0), sum(yv == 1 & cls == 1)),
           2L, 2L, byrow = TRUE) else NULL
  list(beta = beta, se = fit$se, z = fit$z, p_value = fit$p_value,
       odds_ratio = exp(beta), prob = prob, class = cls,
       threshold = threshold, accuracy = acc, confusion = conf,
       loglik = fit$loglik, deviance = fit$deviance,
       converged = fit$converged, separated = fit$separated,
       method = "esl_logistic_reg")
}

#' K-fold cross-validation
#'
#' `model` is any function `model(X_tr, y_tr, X_te)` returning predictions, so
#' this is independent of any particular estimator; the default is OLS with an
#' intercept. The reported `se` is across folds, which is what the
#' one-standard-error rule uses -- the folds share training data and so are
#' positively correlated, biasing it downward.
#'
#' @param X Predictors, n by p.
#' @param y Response.
#' @param model Function `(X_tr, y_tr, X_te) -> predictions`; defaults to OLS.
#' @param k Number of folds, 2 to n.
#' @param loss "mse", "mae" or "01".
#' @param stratify Preserve class proportions per fold.
#' @param seed Seed for the fold shuffle.
#' @return List with `cv`, `se`, `fold_scores`, `predictions`, `fold_id`.
#' @references Hastie, T., et al. (2009). ESL (2nd ed.), Sec 7.10. Springer.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(300), ncol = 3)
#' y <- X %*% c(1, -2, 0.5) + rnorm(100, sd = 0.3)
#' morie_esl_cv_score(X, y, k = 5)$cv < stats::var(y)
#' @export
morie_esl_cv_score <- function(X, y, model = NULL, k = 5L, loss = "mse",
                               stratify = FALSE, seed = 0L) {
  X <- as.matrix(X); y <- as.numeric(y); n <- length(y)
  if (nrow(X) != n) stop(sprintf("X has %d rows but y has %d", nrow(X), n),
                         call. = FALSE)
  k <- as.integer(k)
  if (k < 2L || k > n) stop("k must be between 2 and n", call. = FALSE)
  if (is.null(model)) {
    model <- function(Xtr, ytr, Xte) {
      A <- cbind(1, Xtr)
      cbind(1, Xte) %*% qr.solve(A, ytr)
    }
  }
  set.seed(seed)
  fold <- integer(n)
  if (stratify) {
    for (cl in unique(y)) {
      idx <- sample(which(y == cl))
      fold[idx] <- (seq_along(idx) - 1L) %% k
    }
  } else {
    idx <- sample.int(n)
    fold[idx] <- (seq_len(n) - 1L) %% k
  }
  lf <- switch(loss,
    mse = function(a, b) (a - b)^2,
    mae = function(a, b) abs(a - b),
    `01` = function(a, b) as.numeric(a != b),
    stop(sprintf("loss must be mse, mae or 01, got '%s'", loss), call. = FALSE))

  pred <- rep(NA_real_, n); scores <- numeric(k)
  for (j in seq_len(k)) {
    te <- fold == (j - 1L); tr <- !te
    if (!any(tr)) stop(sprintf("fold %d leaves no training data", j), call. = FALSE)
    pred[te] <- as.numeric(model(X[tr, , drop = FALSE], y[tr], X[te, , drop = FALSE]))
    scores[j] <- mean(lf(y[te], pred[te]))
  }
  list(cv = mean(lf(y, pred)),
       se = if (k > 1L) stats::sd(scores) / sqrt(k) else NA_real_,
       fold_scores = scores, predictions = pred, fold_id = fold,
       k = k, loss = loss, n = n, method = "esl_cv_score")
}

#' Sure independence screening
#'
#' Ranks predictors by absolute marginal correlation and keeps the top `d`.
#' This is a pre-filter, not a selector: a predictor that matters only jointly
#' -- an interaction term, say -- is marginally uncorrelated and screens out,
#' which is what iterated SIS was introduced to patch. Constant columns rank
#' last rather than producing NA.
#'
#' @param X Predictors, n by p.
#' @param y Response.
#' @param d Number to keep; defaults to `min(p, n - 1)`.
#' @return List with `selected` (1-based column indices, strongest first),
#'   `omega`, `rank`, `d`, `dropped`.
#' @references Fan, J., & Lv, J. (2008). Sure independence screening for
#'   ultrahigh dimensional feature space. JRSS-B 70(5), 849-911.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(5000), ncol = 50)
#' y <- 3 * X[, 8] - 2 * X[, 22] + rnorm(100, sd = 0.1)
#' sort(morie_esl_sis_screening(X, y, d = 5)$selected)[1:2]
#' @export
morie_esl_sis_screening <- function(X, y, d = NULL) {
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  if (n != length(y)) stop(sprintf("X has %d rows but y has %d", n, length(y)),
                           call. = FALSE)
  if (n < 3L) stop("need at least 3 observations to screen", call. = FALSE)
  d <- if (is.null(d)) min(p, n - 1L) else as.integer(d)
  if (d < 1L || d > p) stop(sprintf("d must be between 1 and p=%d", p), call. = FALSE)

  yc <- y - mean(y); sy <- sqrt(sum(yc^2))
  Xc <- sweep(X, 2L, colMeans(X), "-")
  sx <- sqrt(colSums(Xc^2))
  omega <- abs(colSums(Xc * yc) / (sx * sy))
  key <- ifelse(is.finite(omega), omega, -Inf)
  ord <- order(-key, seq_len(p))
  rk <- integer(p); rk[ord] <- seq_len(p) - 1L
  list(selected = ord[seq_len(d)],
       omega = ifelse(is.finite(omega), omega, NA_real_),
       rank = rk, d = d, dropped = if (d < p) ord[(d + 1L):p] else integer(0),
       n = n, p = p, method = "esl_sis_screening")
}

#' Weight-decay penalty and its gradient
#'
#' The penalty is written `lambda * sum(w^2)`, so the gradient is
#' `2 * lambda * w` -- twice what frameworks using the `lambda/2` convention
#' apply for the same nominal lambda. The gradient is returned explicitly so
#' the convention in use is visible rather than assumed.
#'
#' @param weights Weight vector, excluding bias terms.
#' @param lambda_ Penalty strength, non-negative.
#' @param loss Unpenalised loss, added to give `objective`.
#' @param norm "l2" (ridge) or "l1" (lasso; subgradient reported as 0 at zero).
#' @return List with `penalty`, `gradient`, `objective`, `effective_lambda`.
#' @references Hastie, T., et al. (2009). ESL (2nd ed.), Sec 11.5.2. Springer.
#' @examples
#' morie_esl_weight_decay(c(3, -4), lambda_ = 0.5)$gradient
#' @export
morie_esl_weight_decay <- function(weights, lambda_ = 0.01, loss = 0,
                                   norm = "l2") {
  if (lambda_ < 0) stop("lambda_ must be non-negative", call. = FALSE)
  w <- as.numeric(weights)
  if (norm == "l2") {
    pen <- lambda_ * sum(w^2); grad <- 2 * lambda_ * w
  } else if (norm == "l1") {
    pen <- lambda_ * sum(abs(w)); grad <- lambda_ * sign(w)
  } else {
    stop('norm must be "l2" or "l1"', call. = FALSE)
  }
  list(penalty = pen, gradient = grad, objective = loss + pen,
       effective_lambda = if (norm == "l2") 2 * lambda_ else lambda_,
       norm = norm, n_weights = length(w), method = "esl_weight_decay")
}

#' Minimum description length
#'
#' With the standard `(d/2) log n` parameter cost, MDL equals BIC/2 exactly --
#' the ESL Sec 7.8 identity, which is the point of the section. `bits` divides
#' the nat-valued result by log 2.
#'
#' @param loglik Maximised log-likelihood.
#' @param theta Fitted parameters, or just their count.
#' @param n Sample size; needed for the BIC-equivalent parameter cost.
#' @param prior_sd If given (with `theta` as values), use a Gaussian prior cost
#'   instead of `(d/2) log n`.
#' @return List with `mdl`, `bits`, `data_cost`, `model_cost`, `bic`, `aic`, `d`.
#' @references Rissanen, J. (1978). Modeling by shortest data description.
#'   Automatica 14(5), 465-471.
#' @examples
#' r <- morie_esl_mdl(-120, 4, n = 100)
#' abs(r$mdl - r$bic / 2) < 1e-12
#' @export
morie_esl_mdl <- function(loglik, theta, n = NULL, prior_sd = NULL) {
  d <- if (length(theta) == 1L && is.numeric(theta) && theta == round(theta) &&
           is.null(prior_sd)) as.integer(theta) else length(theta)
  if (d < 0L) stop("the parameter count must be non-negative", call. = FALSE)
  ll <- as.numeric(loglik)
  if (!is.null(prior_sd)) {
    if (prior_sd <= 0) stop("prior_sd must be positive", call. = FALSE)
    th <- as.numeric(theta)
    model_cost <- 0.5 * sum(th^2) / prior_sd^2 + d * log(prior_sd * sqrt(2 * pi))
  } else if (!is.null(n)) {
    if (n < 1) stop("n must be at least 1", call. = FALSE)
    model_cost <- 0.5 * d * log(n)
  } else {
    model_cost <- 0.5 * d * log(2 * pi)
  }
  mdl <- -ll + model_cost
  bic <- if (!is.null(n) && is.null(prior_sd)) d * log(n) - 2 * ll else NA_real_
  list(mdl = mdl, bits = mdl / log(2), data_cost = -ll,
       model_cost = model_cost, bic = bic, aic = 2 * d - 2 * ll,
       d = d, loglik = ll, method = "esl_mdl")
}

#' Inverted dropout
#'
#' Keeps each unit with probability `p` and rescales survivors by `1/p`, so
#' the expected activation is unchanged and inference needs no adjustment --
#' `training = FALSE` is exactly the identity. Note `p` is the KEEP
#' probability, following Srivastava et al.; libraries that take the drop
#' probability instead will scale every activation by `p^2` if the two
#' conventions are mixed.
#'
#' @param X Activations, n by d.
#' @param p Keep probability in (0, 1].
#' @param training When FALSE the input is returned unchanged.
#' @param seed Seed for the mask.
#' @return List with `output`, `mask`, `kept_fraction`, `scale`.
#' @references Srivastava, N., et al. (2014). Dropout: A simple way to prevent
#'   neural networks from overfitting. JMLR 15, 1929-1958.
#' @examples
#' sort(unique(round(as.numeric(morie_esl_dropout(matrix(1, 20, 5), p = 0.5)$output), 6)))
#' @export
morie_esl_dropout <- function(X, p = 0.5, training = TRUE, seed = 0L) {
  if (p <= 0 || p > 1) {
    stop("p is the KEEP probability and must be in (0, 1]", call. = FALSE)
  }
  A <- as.matrix(X)
  if (!training || p == 1) {
    return(list(output = A, mask = matrix(1, nrow(A), ncol(A)),
                kept_fraction = 1, scale = 1, training = training,
                method = "esl_dropout"))
  }
  set.seed(seed)
  mask <- matrix(as.numeric(stats::runif(length(A)) < p), nrow(A), ncol(A))
  list(output = A * mask / p, mask = mask, kept_fraction = mean(mask),
       scale = 1 / p, training = TRUE, p = p, method = "esl_dropout")
}

#' Haar wavelet smoothing
#'
#' Thresholds the detail coefficients of a Haar DWT using the Donoho-Johnstone
#' universal threshold `sigma * sqrt(2 log n)`, with sigma estimated by the MAD
#' of the finest-scale details. The threshold is chosen so that with high
#' probability no pure-noise coefficient survives, so the shrinkage is
#' deliberately conservative -- a smoother-than-expected fit is the method
#' working as designed.
#'
#' @param y Signal; padded to a power of two by reflection.
#' @param mode "soft" (biased toward zero, continuous) or "hard".
#' @param threshold Explicit lambda; defaults to the universal threshold.
#' @param levels Decomposition depth; defaults to the maximum.
#' @return List with `signal`, `threshold`, `sigma`, `coefficients`,
#'   `n_zeroed`, `levels`.
#' @references Donoho, D. L., & Johnstone, I. M. (1994). Ideal spatial
#'   adaptation by wavelet shrinkage. Biometrika 81(3), 425-455.
#' @examples
#' set.seed(1)
#' clean <- rep(c(0, 4, 1, -2), each = 64)
#' r <- morie_esl_wavelet_smooth(clean + rnorm(256, sd = 0.5))
#' mean((r$signal - clean)^2) < 0.25
#' @export
morie_esl_wavelet_smooth <- function(y, mode = "soft", threshold = NULL,
                                     levels = NULL) {
  if (!mode %in% c("soft", "hard")) stop('mode must be "soft" or "hard"',
                                         call. = FALSE)
  y <- as.numeric(y); n0 <- length(y)
  if (n0 < 2L) stop("need at least 2 observations", call. = FALSE)
  n <- 2^ceiling(log2(n0))
  pad <- if (n > n0) c(y, rev(y))[seq_len(n)] else y
  max_lev <- as.integer(log2(n))
  levels <- if (is.null(levels)) max_lev else min(as.integer(levels), max_lev)

  approx <- pad; details <- list()
  for (i in seq_len(levels)) {
    even <- approx[seq(1L, length(approx), by = 2L)]
    odd  <- approx[seq(2L, length(approx), by = 2L)]
    approx <- (even + odd) / sqrt(2)
    details[[i]] <- (even - odd) / sqrt(2)
  }
  d1 <- details[[1L]]
  sigma <- stats::median(abs(d1 - stats::median(d1))) / 0.6745
  lam <- if (is.null(threshold)) sigma * sqrt(2 * log(n)) else as.numeric(threshold)

  zeroed <- 0L; shrunk <- vector("list", levels)
  for (i in seq_len(levels)) {
    d <- details[[i]]
    t <- if (mode == "soft") sign(d) * pmax(abs(d) - lam, 0) else
      ifelse(abs(d) > lam, d, 0)
    zeroed <- zeroed + sum(t == 0)
    shrunk[[i]] <- t
  }
  rec <- approx
  for (i in rev(seq_len(levels))) {
    d <- shrunk[[i]]
    even <- (rec + d) / sqrt(2); odd <- (rec - d) / sqrt(2)
    out <- numeric(2L * length(rec))
    out[seq(1L, length(out), by = 2L)] <- even
    out[seq(2L, length(out), by = 2L)] <- odd
    rec <- out
  }
  list(signal = rec[seq_len(n0)], threshold = lam, sigma = sigma,
       coefficients = shrunk, approx = approx, n_zeroed = zeroed,
       levels = levels, mode = mode, method = "esl_wavelet_smooth")
}
