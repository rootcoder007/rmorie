# SPDX-License-Identifier: AGPL-3.0-or-later
.esl4_classes <- function(g) {
  cl <- sort(unique(g))
  if (length(cl) < 2) stop("need at least two classes", call. = FALSE)
  cl
}

#' Linear discriminant analysis discriminants
#'
#' delta_k(x) = x' S^-1 mu_k - mu_k' S^-1 mu_k / 2 + log pi_k with the pooled
#' covariance S (divisor N - K) and priors n_k / N (ESL eq 4.10); classify to
#' the largest.
#'
#' @param X Training predictors, N by p.
#' @param y Class labels.
#' @param query Points to score (default X).
#' @return Named list: estimate, prediction, discriminants (query rows by K),
#'   classes, priors, means (K by p), pooled_covariance, n, p, K.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 4.3.
#' @examples
#' i <- 1:30
#' X <- cbind(sin(i), cos(3 * i) + (i %% 3))
#' morie_esl_lda_disc(X, i %% 3, rbind(c(0, 1)))$prediction
#' @export
morie_esl_lda_disc <- function(X, y, query = NULL) {
  X <- as.matrix(X)
  n <- nrow(X)
  if (length(y) != n) stop(sprintf("X has %d rows but y has %d labels.", n, length(y)), call. = FALSE)
  cl <- .esl4_classes(y)
  K <- length(cl)
  if (n <= K) stop(sprintf("pooled covariance needs n > K; got n=%d, K=%d.", n, K), call. = FALSE)
  mu <- t(vapply(cl, function(k) colMeans(X[y == k, , drop = FALSE]), numeric(ncol(X))))
  pri <- vapply(cl, function(k) mean(y == k), numeric(1))
  S <- Reduce(`+`, lapply(seq_len(K), function(k) crossprod(sweep(X[y == cl[k], , drop = FALSE], 2, mu[k, ])))) / (n - K)
  Si <- solve(S)
  Q <- if (is.null(query)) X else rbind(query)
  D <- vapply(seq_len(K), function(k) drop(Q %*% Si %*% mu[k, ]) - 0.5 * drop(mu[k, ] %*% Si %*% mu[k, ]) + log(pri[k]),
              numeric(nrow(Q)))
  D <- matrix(D, nrow(Q))
  pred <- cl[max.col(D, ties.method = "first")]
  list(estimate = pred[1], prediction = pred, discriminants = D, classes = cl, priors = unname(pri),
       means = unname(mu), pooled_covariance = S, n = n, p = ncol(X), K = K)
}

#' Quadratic discriminant analysis discriminants
#'
#' delta_k(x) = -log|S_k| / 2 - (x - mu_k)' S_k^-1 (x - mu_k) / 2 + log pi_k with
#' per-class covariances (divisor n_k - 1) (ESL eq 4.12).
#'
#' @inheritParams morie_esl_lda_disc
#' @return Named list: estimate, prediction, discriminants, classes, priors,
#'   log_dets, n, p, K.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 4.3.
#' @examples
#' i <- 1:30
#' X <- cbind(sin(i), cos(3 * i) + (i %% 3))
#' morie_esl_qda(X, i %% 3, rbind(c(0, 1)))$prediction
#' @export
morie_esl_qda <- function(X, y, query = NULL) {
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  if (length(y) != n) stop(sprintf("X has %d rows but y has %d labels.", n, length(y)), call. = FALSE)
  cl <- .esl4_classes(y)
  K <- length(cl)
  Q <- if (is.null(query)) X else rbind(query)
  pri <- numeric(K)
  ld <- numeric(K)
  D <- matrix(0, nrow(Q), K)
  for (k in seq_len(K)) {
    Xk <- X[y == cl[k], , drop = FALSE]
    if (nrow(Xk) <= p) {
      stop(sprintf("class %s has %d observations but %d features; QDA needs more observations than features per class.",
                   format(cl[k]), nrow(Xk), p), call. = FALSE)
    }
    mu <- colMeans(Xk)
    Sk <- stats::cov(Xk)
    dt <- determinant(Sk, logarithm = TRUE)
    if (dt$sign <= 0) stop(sprintf("class %s has a singular covariance; QDA cannot proceed.", format(cl[k])), call. = FALSE)
    ld[k] <- as.numeric(dt$modulus)
    pri[k] <- nrow(Xk) / n
    dq <- sweep(Q, 2, mu)
    D[, k] <- -0.5 * ld[k] - 0.5 * rowSums((dq %*% solve(Sk)) * dq) + log(pri[k])
  }
  pred <- cl[max.col(D, ties.method = "first")]
  list(estimate = pred[1], prediction = pred, discriminants = D, classes = cl, priors = pri, log_dets = ld,
       n = n, p = p, K = K)
}

#' Multiple-output least squares
#'
#' B = (X'X)^-1 X'Y column by column by QR; residual covariance E'E / (N - q)
#' (ESL eqs 3.34-3.39).
#'
#' @param X Predictors, N by p.
#' @param Y Responses, N by K.
#' @param add_intercept Prepend a column of ones.
#' @return Named list: coefficients (q by K), fitted, rss, residual_covariance,
#'   n, q.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 3.2.4.
#' @examples
#' i <- 1:20
#' X <- cbind(sin(i), cos(i))
#' morie_esl_multi_output_ls(X, cbind(X[, 1] + 0.1 * cos(5 * i), X[, 2] - X[, 1]))$coefficients
#' @export
morie_esl_multi_output_ls <- function(X, Y, add_intercept = TRUE) {
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  D <- if (add_intercept) cbind(1, X) else X
  if (nrow(Y) != nrow(D) || nrow(D) <= ncol(D)) {
    stop("need X and Y with the same N rows and N greater than the design columns", call. = FALSE)
  }
  q <- qr(D)
  B <- qr.coef(q, Y)
  E <- qr.resid(q, Y)
  list(coefficients = unname(as.matrix(B)), fitted = unname(D %*% B), rss = unname(colSums(E^2)),
       residual_covariance = unname(crossprod(E) / (nrow(D) - ncol(D))), n = nrow(D), q = ncol(D))
}

#' Classification by indicator-matrix regression
#'
#' Regress the class indicator columns on (1, x) and classify to the largest
#' fitted value (ESL eqs 4.3-4.6).
#'
#' @param X Predictors, N by p.
#' @param g Class labels.
#' @param query Points to classify (default X).
#' @return Named list: prediction, fitted, coefficients ((p + 1) by K), classes.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 4.2.
#' @examples
#' i <- 1:30
#' morie_esl_indicator_regression(cbind(sin(i), cos(i)), i %% 3, rbind(c(0, 1)))$prediction
#' @export
morie_esl_indicator_regression <- function(X, g, query = NULL) {
  X <- as.matrix(X)
  cl <- .esl4_classes(g)
  Y <- vapply(cl, function(k) as.numeric(g == k), numeric(length(g)))
  B <- morie_esl_multi_output_ls(X, Y)$coefficients
  Q <- if (is.null(query)) X else rbind(query)
  f <- cbind(1, Q) %*% B
  list(prediction = cl[max.col(f, ties.method = "first")], fitted = unname(f), coefficients = B, classes = cl)
}

.esl4_mnl_probs <- function(Z, beta, K) {
  eta <- cbind(Z %*% matrix(beta, ncol(Z), K - 1), 0)
  eta <- eta - apply(eta, 1, max)
  e <- exp(eta)
  e / rowSums(e)
}

.esl4_mnl_info <- function(Z, P, K, wt) {
  q <- ncol(Z)
  npar <- (K - 1) * q
  H <- matrix(0, npar, npar)
  for (k in seq_len(K - 1)) {
    for (l in seq_len(K - 1)) {
      w <- wt * P[, k] * ((k == l) - P[, l])
      H[(k - 1) * q + seq_len(q), (l - 1) * q + seq_len(q)] <- crossprod(Z, Z * w)
    }
  }
  H
}

#' Multinomial logistic regression
#'
#' log(Pr(G = k | x) / Pr(G = K | x)) = beta_k0 + beta_k' x with the last sorted
#' class as baseline (ESL eqs 4.17-4.18), fitted by Newton-Raphson with step
#' halving; standard errors from the inverse information.
#'
#' @param X Predictors, N by p.
#' @param g Class labels.
#' @param query Points for prob (default X).
#' @param max_iter,tol Newton controls.
#' @param weights Optional non-negative case weights (the local fit of ESL
#'   eq 6.19).
#' @return Named list: coefficients and se ((K - 1) by (p + 1)), loglik,
#'   classes, baseline, prob, iterations, converged.
#' @references Hastie, Tibshirani & Friedman (2009), sec. 4.4.
#' @examples
#' i <- 1:40
#' X <- cbind(sin(i), cos(3 * i))
#' morie_esl_multinomial_logit(X, (i * 7) %% 3)$coefficients
#' @export
morie_esl_multinomial_logit <- function(X, g, query = NULL, max_iter = 100, tol = 1e-10, weights = NULL) {
  X <- as.matrix(X)
  cl <- .esl4_classes(g)
  K <- length(cl)
  Z <- cbind(1, X)
  q <- ncol(Z)
  yi <- match(g, cl)
  Yk <- outer(yi, seq_len(K - 1), "==") * 1
  wt <- if (is.null(weights)) rep(1, nrow(Z)) else as.numeric(weights)
  if (length(wt) != nrow(Z) || any(wt < 0)) stop("weights must be N non-negative values", call. = FALSE)
  beta <- numeric((K - 1) * q)
  llf <- function(b) {
    P <- .esl4_mnl_probs(Z, b, K)
    pos <- wt > 0
    list(ll = sum(wt[pos] * log(P[cbind(seq_along(yi), yi)][pos])), P = P)
  }
  cur <- llf(beta)
  converged <- FALSE
  it <- 0L
  for (it in seq_len(max_iter)) {
    grad <- as.numeric(crossprod(Z, wt * (Yk - cur$P[, seq_len(K - 1), drop = FALSE])))
    step <- solve(.esl4_mnl_info(Z, cur$P, K, wt), grad)
    fac <- 1
    while (fac >= 1 / 1024) {
      cand <- llf(beta + fac * step)
      if (cand$ll >= cur$ll - 1e-12) {
        beta <- beta + fac * step
        cur <- cand
        break
      }
      fac <- fac / 2
    }
    if (max(abs(fac * step)) < tol) {
      converged <- TRUE
      break
    }
  }
  V <- solve(.esl4_mnl_info(Z, cur$P, K, wt))
  Qz <- if (is.null(query)) Z else cbind(1, rbind(query))
  list(coefficients = t(matrix(beta, q, K - 1)), se = t(matrix(sqrt(diag(V)), q, K - 1)), loglik = cur$ll,
       classes = cl, baseline = cl[K], prob = .esl4_mnl_probs(Qz, beta, K), iterations = it, converged = converged)
}

#' L1-penalised logistic regression
#'
#' Maximises the log-likelihood minus lambda sum |beta_j| with an unpenalised
#' intercept (ESL eq 4.31) by proximal Newton: IRLS quadratic approximations
#' minimised by coordinate descent with soft thresholding. This lambda is N
#' times glmnet's.
#'
#' @param X Predictors, N by p.
#' @param y 0/1 response.
#' @param lambda Penalty, >= 0.
#' @param max_outer,max_inner,tol Iteration controls.
#' @return Named list: intercept, beta, active_set (1-based), loglik,
#'   objective, score, converged.
#' @references Friedman, J., Hastie, T. & Tibshirani, R. (2010). JSS 33(1).
#' @examples
#' i <- 1:40
#' X <- cbind(sin(i), cos(3 * i))
#' morie_esl_l1_logistic(X, as.integer(sin(3 * i) + X[, 1] > 0), 2)$beta
#' @export
morie_esl_l1_logistic <- function(X, y, lambda, max_outer = 200, max_inner = 10000, tol = 1e-12) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  n <- nrow(X)
  p <- ncol(X)
  if (length(y) != n || any(!y %in% c(0, 1)) || lambda < 0) {
    stop("need 0/1 responses matching X and lambda >= 0", call. = FALSE)
  }
  b0 <- 0
  b <- numeric(p)
  converged <- FALSE
  for (o in seq_len(max_outer)) {
    eta <- b0 + drop(X %*% b)
    pr <- 1 / (1 + exp(-eta))
    w <- pmax(pr * (1 - pr), 1e-10)
    r <- (y - pr) / w
    old <- c(b0, b)
    xw2 <- colSums(w * X^2)
    for (it in seq_len(max_inner)) {
      d0 <- sum(w * r) / sum(w)
      b0 <- b0 + d0
      r <- r - d0
      delta <- abs(d0)
      for (j in seq_len(p)) {
        rho <- sum(w * X[, j] * r) + xw2[j] * b[j]
        new <- if (xw2[j] > 0) sign(rho) * max(abs(rho) - lambda, 0) / xw2[j] else 0
        if (new != b[j]) {
          r <- r - X[, j] * (new - b[j])
          delta <- max(delta, abs(new - b[j]))
          b[j] <- new
        }
      }
      if (delta < tol) break
    }
    if (max(abs(c(b0, b) - old)) < tol) {
      converged <- TRUE
      break
    }
  }
  eta <- b0 + drop(X %*% b)
  pr <- 1 / (1 + exp(-eta))
  ll <- sum(y * eta - log1p(exp(eta)))
  list(intercept = b0, beta = b, active_set = which(b != 0), loglik = ll, objective = -ll + lambda * sum(abs(b)),
       score = drop(crossprod(X, y - pr)), converged = converged)
}
