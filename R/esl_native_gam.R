# SPDX-License-Identifier: AGPL-3.0-or-later
.esl9_spline_smooth <- function(x, r, lambda, w) {
  u <- sort(unique(x))
  gi <- match(x, u)
  m <- length(u)
  W <- as.numeric(tapply(w, gi, sum))
  ybar <- as.numeric(tapply(w * r, gi, sum)) / W
  if (m < 3 || lambda == 0) return(ybar[gi])
  h <- diff(u)
  Q <- matrix(0, m, m - 2)
  R <- matrix(0, m - 2, m - 2)
  for (j in 2:(m - 1)) {
    Q[j - 1, j - 1] <- 1 / h[j - 1]
    Q[j, j - 1] <- -1 / h[j - 1] - 1 / h[j]
    Q[j + 1, j - 1] <- 1 / h[j]
    R[j - 1, j - 1] <- (h[j - 1] + h[j]) / 3
    if (j < m - 1) R[j - 1, j] <- R[j, j - 1] <- h[j] / 6
  }
  gam <- solve(R + lambda * crossprod(Q, Q / W), crossprod(Q, ybar))
  (ybar - lambda * drop(Q %*% gam) / W)[gi]
}

.esl9_linear_smooth <- function(x, r, w) {
  xm <- sum(w * x) / sum(w)
  rm <- sum(w * r) / sum(w)
  sxx <- sum(w * (x - xm)^2)
  b <- if (sxx > 0) sum(w * (x - xm) * (r - rm)) / sxx else 0
  rm + b * (x - xm)
}

.esl9_backfit <- function(X, z, w, smooth, Fm, max_iter, tol) {
  p <- ncol(X)
  it <- 0L
  converged <- FALSE
  alpha <- 0
  for (it in seq_len(max_iter)) {
    delta <- 0
    alpha <- sum(w * (z - rowSums(Fm))) / sum(w)
    for (j in seq_len(p)) {
      new <- smooth(j, z - alpha - rowSums(Fm[, -j, drop = FALSE]), w)
      new <- new - sum(w * new) / sum(w)
      delta <- max(delta, abs(new - Fm[, j]))
      Fm[, j] <- new
    }
    if (delta < tol) {
      converged <- TRUE
      break
    }
  }
  list(alpha = alpha, F = Fm, it = it, converged = converged)
}

#' Generalized additive model by backfitting and local scoring
#'
#' g(mu) = alpha + sum_j f_j(X_j) with each f_j a cubic smoothing spline of
#' penalty lambda_j, fitted by backfitting with re-centred components (ESL
#' Alg. 9.1, eq 9.7); g = "logit" uses local scoring, weighted backfitting on
#' the working response (ESL Alg. 9.2). smoother = "linear" fits straight
#' lines.
#'
#' @param X Predictors without an intercept column, n by p.
#' @param y Response (0/1 for the logit link).
#' @param g NULL, "identity" or "logit".
#' @param lambdas One penalty or one per predictor.
#' @param smoother "spline" or "linear".
#' @param max_iter,tol,max_outer Iteration controls.
#' @return Named list: estimate, alpha, partial_fits (n by p), fitted, eta,
#'   iterations, converged, and rss or loglik.
#' @references Hastie, T. & Tibshirani, R. (1990). Generalized Additive Models.
#'   Chapman & Hall.
#' @examples
#' i <- 1:40
#' X <- cbind(((7 * i) %% 41) / 41 * 3, ((11 * i) %% 43) / 43 * 2)
#' morie_esl_gam(X, sin(2 * X[, 1]) + X[, 2]^2, lambdas = c(0.05, 0.2))$alpha
#' @export
morie_esl_gam <- function(X, y, g = NULL, lambdas = 1, smoother = c("spline", "linear"), max_iter = 500,
                          tol = 1e-10, max_outer = 100) {
  smoother <- match.arg(smoother)
  if (!is.null(g) && !g %in% c("identity", "logit")) stop(sprintf("link '%s' is not implemented; use 'identity' or 'logit'.", g), call. = FALSE)
  X <- as.matrix(X)
  y <- as.numeric(y)
  n <- nrow(X)
  p <- ncol(X)
  if (length(y) != n) stop(sprintf("X has %d rows but y has %d entries.", n, length(y)), call. = FALSE)
  lam <- if (length(lambdas) == 1) rep(lambdas, p) else as.numeric(lambdas)
  if (length(lam) != p || any(lam < 0)) stop("lambdas must be one non-negative value or one per predictor.", call. = FALSE)
  smooth <- function(j, r, w) {
    if (smoother == "linear") .esl9_linear_smooth(X[, j], r, w) else .esl9_spline_smooth(X[, j], r, lam[j], w)
  }
  Fm <- matrix(0, n, p)
  if (is.null(g) || g == "identity") {
    b <- .esl9_backfit(X, y, rep(1, n), smooth, Fm, max_iter, tol)
    eta <- b$alpha + rowSums(b$F)
    return(list(estimate = b$alpha, alpha = b$alpha, partial_fits = b$F, fitted = eta, eta = eta, iterations = b$it,
                converged = b$converged, rss = sum((y - eta)^2), n = n, p = p))
  }
  if (any(!y %in% c(0, 1))) stop("the logit link needs 0/1 responses.", call. = FALSE)
  alpha <- log(mean(y) / (1 - mean(y)))
  it <- 0L
  converged <- FALSE
  for (it in seq_len(max_outer)) {
    eta <- alpha + rowSums(Fm)
    pr <- 1 / (1 + exp(-eta))
    w <- pr * (1 - pr)
    old <- c(alpha, Fm)
    b <- .esl9_backfit(X, eta + (y - pr) / w, w, smooth, Fm, max_iter, tol)
    alpha <- b$alpha
    Fm <- b$F
    if (max(abs(c(alpha, Fm) - old)) < tol) {
      converged <- TRUE
      break
    }
  }
  eta <- alpha + rowSums(Fm)
  list(estimate = alpha, alpha = alpha, partial_fits = Fm, fitted = 1 / (1 + exp(-eta)), eta = eta, iterations = it,
       converged = converged, loglik = sum(y * eta - log1p(exp(eta))), n = n, p = p)
}
