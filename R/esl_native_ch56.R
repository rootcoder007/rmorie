# SPDX-License-Identifier: AGPL-3.0-or-later
.esl5_kw <- function(t, kernel) {
  switch(kernel,
    gaussian = exp(-0.5 * t^2),
    epanechnikov = ifelse(t <= 1, 0.75 * (1 - t^2), 0),
    "tri-cube" = ifelse(t <= 1, (1 - t^3)^3, 0),
    stop("kernel must be one of epanechnikov, tri-cube, gaussian", call. = FALSE)
  )
}

#' Natural cubic spline basis
#'
#' N_1 = 1, N_2 = x, N_(k+2) = d_k - d_(K-1) with d_k = ((x - xi_k)_+^3 - (x -
#' xi_K)_+^3) / (xi_K - xi_k) (ESL eqs 5.4-5.5); linear beyond the boundary
#' knots.
#'
#' @param x Evaluation points.
#' @param knots At least three distinct knots.
#' @return Named list: estimate (number of basis functions), basis (n by K),
#'   n_basis, knots, n.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 5.2.1.
#' @examples
#' morie_esl_natural_spline(c(0.1, 0.35, 0.6, 0.9), c(0.2, 0.5, 0.8))$basis
#' @export
morie_esl_natural_spline <- function(x, knots) {
  x <- as.numeric(x)
  kn <- sort(unique(as.numeric(knots)))
  K <- length(kn)
  if (K < 3) stop(sprintf("a natural spline needs at least 3 distinct knots; got %d.", K), call. = FALSE)
  pc <- function(v) ifelse(v > 0, v^3, 0)
  d <- function(k) (pc(x - kn[k]) - pc(x - kn[K])) / (kn[K] - kn[k])
  B <- cbind(1, x, vapply(seq_len(K - 2), function(k) d(k) - d(K - 1), numeric(length(x))))
  dimnames(B) <- NULL
  list(estimate = ncol(B), basis = B, n_basis = ncol(B), knots = kn, n = length(x))
}

#' Local linear regression
#'
#' At each target minimise sum K((x_i - x0) / lambda) (y_i - a - b (x_i -
#' x0))^2; the fit is a (ESL eqs 6.8-6.9).
#'
#' @param x0 Targets.
#' @param x,y Data.
#' @param lambda Bandwidth.
#' @param kernel "epanechnikov", "tri-cube" or "gaussian".
#' @return Named list: estimate, values, slopes, n_in_window, lambda, kernel, n.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 6.1.1.
#' @examples
#' x <- (1:30) / 30
#' morie_esl_local_linear(c(0.3, 0.6), x, sin(6 * x), 0.2)$values
#' @export
morie_esl_local_linear <- function(x0, x, y, lambda, kernel = "epanechnikov") {
  if (length(x) != length(y)) stop(sprintf("x (%d) and y (%d) lengths differ.", length(x), length(y)), call. = FALSE)
  if (lambda <= 0) stop(sprintf("the bandwidth must be positive; got %s.", format(lambda)), call. = FALSE)
  res <- vapply(x0, function(q) {
    w <- .esl5_kw(abs(q - x) / lambda, kernel)
    if (sum(w > 0) < 2) return(c(NaN, NaN, sum(w > 0)))
    s <- sqrt(w)
    b <- qr.coef(qr(cbind(s, s * (x - q))), s * y)
    c(b, sum(w > 0))
  }, numeric(3))
  list(estimate = res[1, 1], values = res[1, ], slopes = res[2, ], n_in_window = res[3, ], lambda = lambda,
       kernel = kernel, n = length(x))
}

#' Nadaraya-Watson kernel smoother
#'
#' f(x0) = sum K(|x0 - x_i| / lambda) y_i / sum K(|x0 - x_i| / lambda) (ESL eq 6.2).
#'
#' @inheritParams morie_esl_local_linear
#' @return Named list: estimate, values, effective_n, n_in_window, lambda,
#'   kernel, n.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 6.1.
#' @examples
#' x <- (1:30) / 30
#' morie_esl_nadaraya_watson(c(0.3, 0.6), x, sin(6 * x), 0.2)$values
#' @export
morie_esl_nadaraya_watson <- function(x0, x, y, lambda, kernel = "epanechnikov") {
  if (length(x) != length(y)) stop(sprintf("x_data (%d) and y_data (%d) lengths differ.", length(x), length(y)), call. = FALSE)
  if (!length(x)) stop("the smoother needs data.", call. = FALSE)
  if (lambda <= 0) stop(sprintf("the bandwidth must be positive; got %s.", format(lambda)), call. = FALSE)
  res <- vapply(x0, function(q) {
    w <- .esl5_kw(abs(q - x) / lambda, kernel)
    c(if (sum(w) > 0) sum(w * y) / sum(w) else NaN, sum(w), sum(w > 0))
  }, numeric(3))
  list(estimate = res[1, 1], values = res[1, ], effective_n = res[2, ], n_in_window = res[3, ], lambda = lambda,
       kernel = kernel, n = length(x))
}

#' Mallows' Cp in ESL's scaling
#'
#' Cp = (RSS + 2 d sigma^2) / n (ESL eq 7.26; for a linear smoother d =
#' trace(S), eq 6.35), with the classical RSS / sigma^2 - n + 2 d alongside.
#'
#' @param RSS Residual sum of squares.
#' @param d Number of parameters or effective degrees of freedom.
#' @param n Sample size.
#' @param sigma2 Noise variance estimate.
#' @return Named list: estimate, cp_classical, RSS, d, n, sigma2.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 7.5.
#' @examples
#' morie_esl_mallows_cp(10, 3, 50, 0.5)$estimate
#' @export
morie_esl_mallows_cp <- function(RSS, d, n, sigma2) {
  if (RSS < 0 || d < 0 || n < 1 || sigma2 <= 0) {
    stop("need RSS >= 0, d >= 0, n >= 1 and sigma2 > 0", call. = FALSE)
  }
  list(estimate = (RSS + 2 * d * sigma2) / n, cp_classical = RSS / sigma2 - n + 2 * d, RSS = RSS, d = d, n = n,
       sigma2 = sigma2)
}

#' Gaussian naive Bayes
#'
#' log pi_k + sum_j log N(x_j; mu_kj, v_kj) with maximum-likelihood class
#' variances plus var_smoothing (ESL eq 6.26); classify to the largest.
#'
#' @param X Training predictors.
#' @param y Class labels.
#' @param query Points to score (default X).
#' @param var_smoothing Added to every variance.
#' @return Named list: estimate, prediction, log_posterior (query by K),
#'   classes, priors, n, p, K.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 6.6.3.
#' @examples
#' i <- 1:30
#' morie_esl_naive_bayes(cbind(sin(i), cos(3 * i) + (i %% 3)), i %% 3, rbind(c(0.2, 1)))$prediction
#' @export
morie_esl_naive_bayes <- function(X, y, query = NULL, var_smoothing = 1e-9) {
  X <- as.matrix(X)
  if (length(y) != nrow(X)) stop(sprintf("X has %d rows but y has %d labels.", nrow(X), length(y)), call. = FALSE)
  cl <- sort(unique(y))
  if (length(cl) < 2) stop("naive Bayes needs at least two classes.", call. = FALSE)
  Q <- if (is.null(query)) X else rbind(query)
  L <- vapply(cl, function(k) {
    Xk <- X[y == k, , drop = FALSE]
    mu <- colMeans(Xk)
    v <- colMeans(sweep(Xk, 2, mu)^2) + var_smoothing
    -0.5 * rowSums(sweep(sweep(Q, 2, mu)^2, 2, v, "/") + matrix(log(2 * pi * v), nrow(Q), ncol(Q), byrow = TRUE)) +
      log(nrow(Xk) / nrow(X))
  }, numeric(nrow(Q)))
  L <- matrix(L, nrow(Q))
  pred <- cl[max.col(L, ties.method = "first")]
  list(estimate = pred[1], prediction = pred, log_posterior = L, classes = cl,
       priors = vapply(cl, function(k) mean(y == k), numeric(1)), n = nrow(X), p = ncol(X), K = length(cl))
}

#' Kernel ridge regression
#'
#' alpha = (K + lambda I)^-1 y with K_ij = k((x_i - x_j) / h) for the smoothing
#' kernels (gaussian is the normal density) and f(x0) = sum_i alpha_i k((x0 -
#' x_i) / h) (ESL eq 5.55).
#'
#' @param X Predictor.
#' @param y Response.
#' @param kernel "gaussian", "epanechnikov" or "uniform".
#' @param lam Ridge penalty, > 0.
#' @param x_eval Evaluation points (default X).
#' @param bandwidth Bandwidth; Silverman's rule when NULL.
#' @return Named list: x_eval, y_hat, alpha, bandwidth, penalty, n_obs.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 5.8.
#' @examples
#' x <- (1:25) / 25
#' kernel_ridge_regression(x, sin(6 * x), lam = 0.1, bandwidth = 0.2, x_eval = c(0.3, 0.6))$y_hat
#' @export
kernel_ridge_regression <- function(X, y, kernel = "gaussian", lam = 1, x_eval = NULL, bandwidth = NULL) {
  x <- as.numeric(X)
  y <- as.numeric(y)
  n <- length(x)
  if (n != length(y)) stop("x and y must have same length.", call. = FALSE)
  if (n < 3) stop("Need at least 3 observations.", call. = FALSE)
  if (lam <= 0) stop("penalty must be > 0.", call. = FALSE)
  kf <- switch(kernel,
    gaussian = stats::dnorm,
    epanechnikov = function(u) ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0),
    uniform = function(u) ifelse(abs(u) <= 1, 0.5, 0),
    stop("kernel must be gaussian, epanechnikov or uniform", call. = FALSE)
  )
  if (is.null(bandwidth)) bandwidth <- 1.06 * min(stats::sd(x), stats::IQR(x) / 1.34) * n^(-1 / 5)
  a <- solve(kf(outer(x, x, "-") / bandwidth) + lam * diag(n), y)
  xe <- if (is.null(x_eval)) x else as.numeric(x_eval)
  list(x_eval = xe, y_hat = drop(kf(outer(xe, x, "-") / bandwidth) %*% a), alpha = a, bandwidth = bandwidth,
       penalty = lam, n_obs = n)
}

#' Penalised logistic regression on a basis
#'
#' Maximises the log-likelihood minus (lambda / 2) theta' Omega theta by
#' penalised IRLS, theta = (N'WN + lambda Omega)^-1 N'Wz (ESL eqs 5.28-5.33); df
#' is the trace of N (N'WN + lambda Omega)^-1 N'W.
#'
#' @param N Basis matrix (include a constant column for an intercept).
#' @param y 0/1 response.
#' @param Omega Penalty matrix.
#' @param lambda Penalty, >= 0.
#' @param max_iter,tol Newton controls (convergence on the linear predictor N
#'   theta, identified even when theta is not, or on a relative change below
#'   1e-14 in the penalised log-likelihood for an ill-conditioned basis).
#' @return Named list: theta, prob, loglik, penalized_loglik, df, iterations,
#'   converged.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 5.6.
#' @examples
#' x <- sin(1:40)
#' N <- cbind(1, x, x^2)
#' morie_esl_penalized_logistic(N, as.integer(sin(3 * (1:40)) + x > 0.3), diag(c(0, 0, 1)), 0.5)$theta
#' @export
morie_esl_penalized_logistic <- function(N, y, Omega, lambda, max_iter = 100, tol = 1e-10) {
  N <- as.matrix(N)
  y <- as.numeric(y)
  Omega <- as.matrix(Omega)
  m <- ncol(N)
  if (length(y) != nrow(N) || any(dim(Omega) != m) || lambda < 0) {
    stop("need N (n x m), y of length n, Omega m x m and lambda >= 0", call. = FALSE)
  }
  if (any(!y %in% c(0, 1))) stop("y must be 0/1", call. = FALSE)
  th <- numeric(m)
  converged <- FALSE
  it <- 0L
  pll <- function(t) {
    e <- drop(N %*% t)
    sum(y * e - log1p(exp(e))) - 0.5 * lambda * drop(t(t) %*% Omega %*% t)
  }
  obj <- pll(th)
  for (it in seq_len(max_iter)) {
    eta <- drop(N %*% th)
    p <- 1 / (1 + exp(-eta))
    w <- p * (1 - p)
    z <- eta + (y - p) / w
    new <- unname(drop(solve(crossprod(N, w * N) + lambda * Omega, crossprod(N, w * z))))
    step <- max(abs(N %*% (new - th)))
    th <- new
    nobj <- pll(th)
    if (step < tol || (it >= 3 && abs(nobj - obj) <= 1e-14 * (1 + abs(nobj)))) {
      converged <- TRUE
      break
    }
    obj <- nobj
  }
  eta <- drop(N %*% th)
  p <- 1 / (1 + exp(-eta))
  w <- p * (1 - p)
  btwb <- crossprod(N, w * N)
  ll <- sum(y * eta - log1p(exp(eta)))
  list(theta = th, prob = p, loglik = ll, penalized_loglik = ll - 0.5 * lambda * drop(t(th) %*% Omega %*% th),
       df = sum(diag(solve(btwb + lambda * Omega, btwb))), iterations = it, converged = converged)
}

#' Varying-coefficient model
#'
#' At each z0 the weighted least-squares fit of y on (1, X) with weights
#' K(|z - z0| / lambda) gives alpha(z0), beta(z0) (ESL eqs 6.16-6.17).
#'
#' @param X Predictors, n by q.
#' @param z Variable the coefficients vary with.
#' @param y Response.
#' @param z0 Targets.
#' @param lambda Bandwidth.
#' @param kernel "epanechnikov", "tri-cube" or "gaussian".
#' @return Named list: coefficients (one row per target), n_in_window.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 6.4.2.
#' @examples
#' i <- 1:40
#' X <- cbind(cos(2 * i))
#' z <- ((7 * i) %% 40) / 40
#' morie_esl_varying_coef(X, z, 1 + z * X[, 1], c(0.3, 0.7), 0.35)$coefficients
#' @export
morie_esl_varying_coef <- function(X, z, y, z0, lambda, kernel = "epanechnikov") {
  X <- as.matrix(X)
  q <- ncol(X)
  if (length(z) != nrow(X) || length(y) != nrow(X) || lambda <= 0) {
    stop("need X, z and y of equal length, lambda > 0 and a known kernel", call. = FALSE)
  }
  out <- lapply(z0, function(t) {
    w <- .esl5_kw(abs(t - z) / lambda, kernel)
    k <- w > 0
    if (sum(k) <= q) return(list(b = rep(NaN, q + 1), n = sum(k)))
    s <- sqrt(w[k])
    list(b = unname(qr.coef(qr(s * cbind(1, X[k, , drop = FALSE])), s * y[k])), n = sum(k))
  })
  list(coefficients = do.call(rbind, lapply(out, `[[`, "b")), n_in_window = vapply(out, `[[`, numeric(1), "n"))
}

#' Local multinomial logistic regression
#'
#' Kernel-weighted multinomial logit on (x - x0), last class as baseline; the
#' class probabilities at x0 are the softmax of the local intercepts (ESL eqs
#' 6.18-6.20).
#'
#' @param X Predictors, n by p.
#' @param g Class labels.
#' @param x0 Targets, one per row.
#' @param lambda Bandwidth.
#' @param kernel "gaussian", "epanechnikov" or "tri-cube".
#' @return Named list: prob (targets by classes), intercepts, classes.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 6.5.
#' @examples
#' i <- 1:40
#' morie_esl_local_logistic(cbind(sin(i), cos(3 * i)), (i * 7) %% 3, rbind(c(0.2, 0.3)), 1)$prob
#' @export
morie_esl_local_logistic <- function(X, g, x0, lambda, kernel = "gaussian") {
  X <- as.matrix(X)
  x0 <- rbind(x0)
  if (length(g) != nrow(X) || lambda <= 0) stop("need X and g of equal length, lambda > 0 and a known kernel", call. = FALSE)
  fits <- lapply(seq_len(nrow(x0)), function(r) {
    D <- sweep(X, 2, x0[r, ])
    w <- .esl5_kw(sqrt(rowSums(D^2)) / lambda, kernel)
    morie_esl_multinomial_logit(D, g, query = rbind(rep(0, ncol(X))), weights = w)
  })
  list(prob = do.call(rbind, lapply(fits, function(f) f$prob[1, ])),
       intercepts = do.call(rbind, lapply(fits, function(f) f$coefficients[, 1])), classes = fits[[1]]$classes)
}

#' Radial basis function network with fixed prototypes
#'
#' Least squares on D_j(x) = exp(-|x - xi_j|^2 / lambda_j^2) with an intercept,
#' or on the renormalised h_j = D_j / sum D_k without one (ESL eqs 6.28-6.30).
#'
#' @param X Predictors, n by p.
#' @param y Response.
#' @param centers Prototypes, J by p.
#' @param scales J positive scales.
#' @param normalized Use the renormalised bases.
#' @param query Points to predict (default X).
#' @return Named list: coefficients, fitted, rss.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 6.7.
#' @examples
#' i <- 1:40
#' X <- cbind(sin(i), cos(3 * i))
#' morie_esl_rbf_network(X, sin(2 * i), rbind(c(0, 0), c(1, 1)), c(0.8, 1))$coefficients
#' @export
morie_esl_rbf_network <- function(X, y, centers, scales, normalized = FALSE, query = NULL) {
  X <- as.matrix(X)
  centers <- rbind(centers)
  scales <- as.numeric(scales)
  if (length(y) != nrow(X) || length(scales) != nrow(centers) || min(scales) <= 0 || nrow(X) <= nrow(centers) + 1) {
    stop("need matching X and y, one positive scale per centre and n > J + 1", call. = FALSE)
  }
  basis <- function(M) {
    D <- vapply(seq_len(nrow(centers)), function(j) exp(-rowSums(sweep(M, 2, centers[j, ])^2) / scales[j]^2),
                numeric(nrow(M)))
    D <- matrix(D, nrow(M))
    if (normalized) D / rowSums(D) else cbind(1, D)
  }
  q <- qr(basis(X))
  if (q$rank < ncol(q$qr)) stop("design matrix is rank deficient", call. = FALSE)
  b <- unname(qr.coef(q, y))
  Q <- if (is.null(query)) X else rbind(query)
  list(coefficients = b, fitted = drop(basis(Q) %*% b), rss = sum(qr.resid(q, y)^2))
}

#' Family-wise error rate of M tests
#'
#' 1 - (1 - alpha)^M for M independent level-alpha tests, the Bonferroni bound
#' min(1, M alpha), and the per-test levels alpha / M and 1 - (1 - alpha)^(1/M)
#' that hold the FWER at alpha (ESL sec 18.7.1).
#'
#' @param alpha Level in (0, 1).
#' @param M Number of tests.
#' @return Named list: fwer_independent, bonferroni_bound, per_test_bonferroni,
#'   per_test_sidak.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 18.7.
#' @examples
#' morie_esl_fwer(0.05, 10)
#' @export
morie_esl_fwer <- function(alpha, M) {
  if (!(alpha > 0 && alpha < 1) || M < 1) stop("need 0 < alpha < 1 and M >= 1", call. = FALSE)
  list(fwer_independent = 1 - (1 - alpha)^M, bonferroni_bound = min(1, M * alpha), per_test_bonferroni = alpha / M,
       per_test_sidak = 1 - (1 - alpha)^(1 / M))
}
