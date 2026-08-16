# SPDX-License-Identifier: AGPL-3.0-or-later
# Internal helpers mirroring morie.fn._gp_core (Montesinos Lopez, Montesinos
# Lopez & Crossa 2022, Multivariate Statistical Machine Learning Methods for
# Genomic Prediction, Springer, DOI 10.1007/978-3-030-89010-0).
# Base R only. Not exported.

#' .gpflat
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.gpflat <- function(x) as.numeric(unlist(x))

#' .gpmat
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param A See Usage.
#' @return A matrix, from \code{matrix}.
#' @export
.gpmat <- function(A) {
  if (is.matrix(A)) return(matrix(as.numeric(A), nrow(A), ncol(A)))
  if (is.list(A)) return(do.call(rbind, lapply(A, as.numeric)))
  matrix(as.numeric(A), nrow = 1L)
}

# Solve A x = b; rank-deficient systems fall back on the minimum-norm
# pseudo-inverse solution, as the Python arm does.
#' Solve A x = b; rank-deficient systems fall back on the minimum-norm
#'
#' pseudo-inverse solution, as the Python arm does.
#'
#' @param A See Usage.
#' @param b See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.gpsolve <- function(A, b) {
  A <- .gpmat(A); b <- .gpflat(b)
  out <- tryCatch(as.numeric(solve(A, b)), error = function(e) NULL)
  if (!is.null(out) && all(is.finite(out))) return(out)
  as.numeric(.gppinv(A) %*% b)
}

#' .gppinv
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param A See Usage.
#' @return The value of \code{%*%}.
#' @export
.gppinv <- function(A) {
  s <- svd(.gpmat(A))
  tol <- max(dim(A)) * .Machine$double.eps * max(s$d, 0)
  di <- ifelse(s$d > tol, 1 / s$d, 0)
  s$v %*% (di * t(s$u))
}

#' .gpinv
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param A See Usage.
#' @return The value of \code{.gppinv}.
#' @export
.gpinv <- function(A) {
  A <- .gpmat(A)
  out <- tryCatch(solve(A), error = function(e) NULL)
  if (!is.null(out) && all(is.finite(out))) return(out)
  .gppinv(A)
}

# log|det A| via the modulus, matching _gp_core._logdet (which sums
# log|pivot| and so discards the sign).
#' Log|det A| via the modulus, matching _gp_core._logdet (which sums
#'
#' log|pivot| and so discards the sign).
#'
#' @param A See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.gplogdet <- function(A) {
  A <- .gpmat(A)
  d <- determinant(A, logarithm = TRUE)
  v <- as.numeric(d$modulus)
  if (!is.finite(v)) -Inf else v
}

# --- chapter 1: balanced one-way layout, eqs (1.2)-(1.5) -------------------
#' Chapter 1: balanced one-way layout, eqs (1.2)-(1.5)
#' -------------------
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param groups See Usage.
#' @return A list with \code{grand_mean}, \code{sd_single_mean}, \code{group_means}, \code{sd_residual}, \code{deviations}, \code{sigma2_b}, \code{icc}, \code{ms_between}, \code{ms_within}.
#' @export
.gponeway <- function(groups) {
  gs <- lapply(groups, as.numeric)
  if (length(gs) == 0L || any(vapply(gs, length, 1L) != length(gs[[1L]]))) {
    stop("need a balanced layout (equal group sizes)")
  }
  a <- length(gs); r <- length(gs[[1L]]); n <- a * r
  means <- vapply(gs, mean, 0)
  grand <- sum(vapply(gs, sum, 0)) / n
  ss_between <- r * sum((means - grand)^2)
  ss_within <- sum(vapply(seq_len(a), function(i) sum((gs[[i]] - means[i])^2), 0))
  ms_between <- ss_between / (a - 1)
  ms_within <- ss_within / (n - a)
  sigma2_b <- max((ms_between - ms_within) / r, 0)
  denom <- sigma2_b + ms_within
  list(grand_mean = grand,
       sd_single_mean = sqrt((ss_between + ss_within) / (n - 1)),
       group_means = means,
       sd_residual = sqrt(ms_within),
       deviations = means - grand,
       sigma2_b = sigma2_b,
       icc = if (denom > 0) sigma2_b / denom else 0,
       ms_between = ms_between, ms_within = ms_within)
}

# --- chapter 4: confusion matrix and metrics, eqs (4.5)-(4.14) -------------
#' Chapter 4: confusion matrix and metrics, eqs (4.5)-(4.14)
#' -------------
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param y_true See Usage.
#' @param y_pred See Usage.
#' @param n_classes Defaults to \code{NULL}.
#' @return The value of \code{M}, as built in the body.
#' @export
.gpconf <- function(y_true, y_pred, n_classes = NULL) {
  yt <- as.integer(.gpflat(y_true)); yp <- as.integer(.gpflat(y_pred))
  C <- if (is.null(n_classes)) max(c(yt, yp)) + 1L else as.integer(n_classes)
  M <- matrix(0, C, C)
  for (k in seq_along(yt)) M[yt[k] + 1L, yp[k] + 1L] <- M[yt[k] + 1L, yp[k] + 1L] + 1
  M
}

#' .gpclassmetrics
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param conf See Usage.
#' @param i See Usage.
#' @return A list with \code{TFN}, \code{TFP}, \code{TTN}, \code{TTP_all}, \code{precision}, \code{sensitivity}, \code{specificity}, \code{pCCC}.
#' @export
.gpclassmetrics <- function(conf, i) {
  conf <- .gpmat(conf); C <- nrow(conf); ii <- as.integer(i) + 1L
  tfn <- sum(conf[ii, -ii]); tfp <- sum(conf[-ii, ii])
  ttn <- sum(conf[-ii, -ii]); ttp <- sum(diag(conf)); total <- sum(conf)
  list(TFN = tfn, TFP = tfp, TTN = ttn, TTP_all = ttp,
       precision = if (ttp + tfp) ttp / (ttp + tfp) else 0,
       sensitivity = if (ttp + tfn) ttp / (ttp + tfn) else 0,
       specificity = if (ttn + tfp) ttn / (ttn + tfp) else 0,
       pCCC = if (total) ttp / total else 0)
}

#' .gpbrier
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param probs See Usage.
#' @param y_true See Usage.
#' @param n_classes Defaults to \code{NULL}.
#' @param halved Defaults to \code{FALSE}.
#' @return One of two values, depending on the branch taken.
#' @export
.gpbrier <- function(probs, y_true, n_classes = NULL, halved = FALSE) {
  P <- .gpmat(probs); yt <- as.integer(.gpflat(y_true)); Tn <- length(yt)
  C <- if (is.null(n_classes)) ncol(P) else as.integer(n_classes)
  tot <- 0
  for (k in seq_len(Tn)) {
    d <- as.numeric(seq_len(C) - 1L == yt[k])
    tot <- tot + sum((P[k, seq_len(C)] - d)^2)
  }
  bs <- tot / Tn
  if (halved) bs / 2 else bs
}

#' .gpmll
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param probs See Usage.
#' @param y_true See Usage.
#' @param n_classes Defaults to \code{NULL}.
#' @return A numeric value.
#' @export
.gpmll <- function(probs, y_true, n_classes = NULL) {
  P <- .gpmat(probs); yt <- as.integer(.gpflat(y_true))
  -sum(log(pmax(P[cbind(seq_along(yt), yt + 1L)], 1e-300))) / length(yt)
}

# --- chapter 5: linear mixed model, eqs (5.1)-(5.2) and REML ---------------
#' Chapter 5: linear mixed model, eqs (5.1)-(5.2) and REML
#' ---------------
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param Z See Usage.
#' @param D See Usage.
#' @param R Defaults to \code{NULL}.
#' @return A numeric value.
#' @export
.gplmmV <- function(Z, D, R = NULL) {
  Z <- .gpmat(Z); n <- nrow(Z)
  V <- Z %*% .gpmat(D) %*% t(Z)
  V + (if (is.null(R)) diag(n) else .gpmat(R))
}

#' .gpblueblup
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param Z See Usage.
#' @param y See Usage.
#' @param Sigma See Usage.
#' @param R Defaults to \code{NULL}.
#' @return A list with \code{beta}, \code{u}.
#' @export
.gpblueblup <- function(X, Z, y, Sigma, R = NULL) {
  X <- .gpmat(X); Z <- .gpmat(Z); y <- .gpflat(y)
  V <- .gplmmV(Z, Sigma, R)
  Vi <- .gpinv(V)
  XtVi <- t(X) %*% Vi
  beta <- .gpsolve(XtVi %*% X, as.numeric(XtVi %*% y))
  resid <- y - as.numeric(X %*% beta)
  u <- as.numeric(.gpmat(Sigma) %*% t(Z) %*% (Vi %*% resid))
  list(beta = beta, u = u)
}

#' .gplmmloglik
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param Z See Usage.
#' @param y See Usage.
#' @param D See Usage.
#' @param beta Defaults to \code{NULL}.
#' @param R Defaults to \code{NULL}.
#' @return A list with \code{value}, \code{beta}.
#' @export
.gplmmloglik <- function(X, Z, y, D, beta = NULL, R = NULL) {
  Xm <- .gpmat(X); y <- .gpflat(y); n <- length(y)
  V <- .gplmmV(Z, D, R); Vi <- .gpinv(V)
  if (is.null(beta)) {
    XtVi <- t(Xm) %*% Vi
    beta <- .gpsolve(XtVi %*% Xm, as.numeric(XtVi %*% y))
  }
  r <- y - as.numeric(Xm %*% beta)
  quad <- sum(r * as.numeric(Vi %*% r))
  list(value = -0.5 * n * log(2 * pi) - 0.5 * .gplogdet(V) - 0.5 * quad,
       beta = beta)
}

#' .gpremlloglik
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param Z See Usage.
#' @param y See Usage.
#' @param D See Usage.
#' @param R Defaults to \code{NULL}.
#' @return A list with \code{value}, \code{beta}.
#' @export
.gpremlloglik <- function(X, Z, y, D, R = NULL) {
  Xm <- .gpmat(X); y <- .gpflat(y)
  V <- .gplmmV(Z, D, R); Vi <- .gpinv(V)
  XtVi <- t(Xm) %*% Vi
  A <- XtVi %*% Xm
  beta <- .gpsolve(A, as.numeric(XtVi %*% y))
  r <- y - as.numeric(Xm %*% beta)
  quad <- sum(r * as.numeric(Vi %*% r))
  list(value = -0.5 * .gplogdet(A) - 0.5 * .gplogdet(V) - 0.5 * quad,
       beta = beta)
}

# --- chapter 3: least squares, eq (3.1) -----------------------------------
#' Chapter 3: least squares, eq (3.1)
#' -----------------------------------
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param add_intercept Defaults to \code{FALSE}.
#' @return A list with \code{beta}, \code{fitted}, \code{residuals}, \code{rss}, \code{sigma2}, \code{sigma2_ml}, \code{var_beta}, \code{se_beta}.
#' @export
.gpolsfit <- function(X, y, add_intercept = FALSE) {
  Xm <- .gpmat(X)
  if (add_intercept) Xm <- cbind(1, Xm)
  y <- .gpflat(y); n <- length(y); p1 <- ncol(Xm)
  XtX <- t(Xm) %*% Xm
  beta <- .gpsolve(XtX, as.numeric(t(Xm) %*% y))
  fitted <- as.numeric(Xm %*% beta)
  resid <- y - fitted
  rss <- sum(resid^2)
  dof <- n - p1
  sigma2 <- if (dof > 0) rss / dof else NaN
  XtXi <- .gpinv(XtX)
  list(beta = beta, fitted = fitted, residuals = resid, rss = rss,
       sigma2 = sigma2, sigma2_ml = rss / n,
       var_beta = sigma2 * XtXi,
       se_beta = sqrt(sigma2 * diag(XtXi)))
}

# --- chapter 5: Kronecker products and the multi-trait model, eq (5.5) -----
#' Chapter 5: Kronecker products and the multi-trait model, eq (5.5)
#' -----
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param A See Usage.
#' @param B See Usage.
#' @return The value of \code{kronecker}.
#' @export
.gpkron <- function(A, B) kronecker(.gpmat(A), .gpmat(B))

#' .gpmultitrait
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param Y See Usage.
#' @param Z See Usage.
#' @param G See Usage.
#' @param Sigma_T See Usage.
#' @param R_T See Usage.
#' @param X Defaults to \code{NULL}.
#' @return A list with \code{mu}, \code{beta}, \code{b}, \code{b_by_line}.
#' @export
.gpmultitrait <- function(Y, Z, G, Sigma_T, R_T, X = NULL) {
  Ym <- .gpmat(Y); J <- nrow(Ym); nT <- ncol(Ym)
  y <- as.numeric(t(Ym))                      # stacked line-by-line
  I_nT <- diag(nT)
  Xm <- .gpkron(matrix(1, J, 1), I_nT)
  if (!is.null(X)) Xm <- cbind(Xm, .gpmat(X))
  Zm <- .gpkron(Z, I_nT)
  Sigma <- .gpkron(G, Sigma_T)
  R <- .gpkron(diag(J), R_T)
  bb <- .gpblueblup(Xm, Zm, y, Sigma, R)
  b <- bb$u
  list(mu = bb$beta[seq_len(nT)], beta = bb$beta, b = b,
       b_by_line = lapply(seq_len(length(b) %/% nT),
                          function(i) b[((i - 1L) * nT + 1L):(i * nT)]))
}

# --- chapter 7: multinomial logistic model, eqs (7.6)-(7.10) ---------------
#' Chapter 7: multinomial logistic model, eqs (7.6)-(7.10)
#' ---------------
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param beta0 See Usage.
#' @param beta See Usage.
#' @param baseline_last Defaults to \code{TRUE}.
#' @return A matrix, from \code{t}.
#' @export
.gpmnprobs <- function(X, beta0, beta, baseline_last = TRUE) {
  Xm <- .gpmat(X); b0 <- .gpflat(beta0); B <- .gpmat(beta)
  if (baseline_last) { b0 <- c(b0, 0); B <- rbind(B, rep(0, ncol(Xm))) }
  t(apply(Xm, 1L, function(row) {
    eta <- b0 + as.numeric(B %*% row)
    ex <- exp(eta - max(eta))
    ex / sum(ex)
  }))
}

#' .gpmnloglik
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param beta0 See Usage.
#' @param beta See Usage.
#' @param baseline_last Defaults to \code{TRUE}.
#' @return A numeric value.
#' @export
.gpmnloglik <- function(X, y, beta0, beta, baseline_last = TRUE) {
  P <- .gpmnprobs(X, beta0, beta, baseline_last)
  ys <- as.integer(.gpflat(y))
  sum(log(pmax(P[cbind(seq_along(ys), ys + 1L)], 1e-300)))
}

#' .gppenmnloglik
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param beta0 See Usage.
#' @param beta See Usage.
#' @param lam See Usage.
#' @param penalty Defaults to \code{"ridge"}.
#' @param baseline_last Defaults to \code{TRUE}.
#' @return A list with \code{loglik}, \code{penalty}, \code{penalized_loglik}.
#' @export
.gppenmnloglik <- function(X, y, beta0, beta, lam, penalty = "ridge",
                           baseline_last = TRUE) {
  ll <- .gpmnloglik(X, y, beta0, beta, baseline_last)
  B <- .gpmat(beta)
  pen <- if (identical(penalty, "lasso")) sum(abs(B)) else sum(B^2)
  list(loglik = ll, penalty = as.numeric(lam) * pen,
       penalized_loglik = ll - as.numeric(lam) * pen)
}

# --- chapter 10: ANN loss, eq (10.5) --------------------------------------
#' Chapter 10: ANN loss, eq (10.5)
#' --------------------------------------
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param y_hat See Usage.
#' @param y See Usage.
#' @return A numeric value.
#' @export
.gpannsse <- function(y_hat, y) 0.5 * sum((.gpmat(y_hat) - .gpmat(y))^2)

# --- chapter 3: expected prediction error, p.80 ---------------------------
#' Chapter 3: expected prediction error, p.80
#' ---------------------------
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param sigma2 See Usage.
#' @param x_star See Usage.
#' @param eigenvalues See Usage.
#' @return A numeric value.
#' @export
.gpepe <- function(sigma2, x_star, eigenvalues) {
  xs <- .gpflat(x_star); lam <- .gpflat(eigenvalues)
  if (any(lam <= 0)) stop("eigenvalues must be positive")
  as.numeric(sigma2) * (1 + sum(xs^2 / lam))
}

# --- chapter 15: zero-altered Poisson forest, eqs (15.1)-(15.4) -----------
#' Chapter 15: zero-altered Poisson forest, eqs (15.1)-(15.4)
#' -----------
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param mu_pred See Usage.
#' @param theta_pred See Usage.
#' @return A list with \code{mu}, \code{theta}.
#' @export
.gpzaplink <- function(mu_pred, theta_pred)
  list(mu = exp(min(as.numeric(mu_pred), 700)),
       theta = 1 / (1 + exp(-as.numeric(theta_pred))))

# Book erratum: eq. (15.3) as printed drops the mu from the numerator. The
# ZAP pmf printed above it, the Var(Y) line below it, and the p.652
# estimating equation for mu all carry the mu, so the internally consistent
# mean (1 - theta) mu / (1 - exp(-mu)) is what is implemented.
#' Book erratum: eq. (15.3) as printed drops the mu from the numerator.
#' The
#'
#' ZAP pmf printed above it, the Var(Y) line below it, and the p.652
#' estimating equation for mu all carry the mu, so the internally
#' consistent mean (1 - theta) mu / (1 - exp(-mu)) is what is
#' implemented.
#'
#' @param theta_hat See Usage.
#' @param mu_hat See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.gpzappredict <- function(theta_hat, mu_hat) {
  th <- as.numeric(theta_hat); mu <- as.numeric(mu_hat)
  denom <- 1 - exp(-mu)
  if (denom <= 0) 0 else (1 - th) * mu / denom
}

#' .gpzapcpredict
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param theta_hat See Usage.
#' @param mu_hat See Usage.
#' @param threshold Defaults to \code{0.5}.
#' @return One of two values, depending on the branch taken.
#' @export
.gpzapcpredict <- function(theta_hat, mu_hat, threshold = 0.5)
  if (as.numeric(theta_hat) > as.numeric(threshold)) 0 else as.numeric(mu_hat)

#' .gpztploglik
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param y_positive See Usage.
#' @param mu See Usage.
#' @return A numeric value.
#' @export
.gpztploglik <- function(y_positive, mu) {
  ys <- .gpflat(y_positive); n <- length(ys); mu <- as.numeric(mu)
  if (mu <= 0 || n == 0L) return(-Inf)
  -n * log(1 - exp(-mu)) + log(mu) * sum(ys) - n * mu - sum(lgamma(ys + 1))
}

#' .gpztpmle
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param y_positive See Usage.
#' @param tol Defaults to \code{1e-12}.
#' @param max_iter Defaults to \code{200}.
#' @return A numeric value.
#' @export
.gpztpmle <- function(y_positive, tol = 1e-12, max_iter = 200) {
  ys <- .gpflat(y_positive); n <- length(ys)
  if (n == 0L) stop("need at least one positive observation")
  target <- sum(ys) / n
  if (target <= 1) return(0)
  lo <- 1e-9; hi <- 1
  while (hi / (1 - exp(-hi)) < target) { hi <- hi * 2; if (hi > 1e6) break }
  for (k in seq_len(as.integer(max_iter))) {
    mid <- 0.5 * (lo + hi)
    v <- mid / (1 - exp(-mid))
    if (abs(v - target) < tol) return(mid)
    if (v < target) lo <- mid else hi <- mid
  }
  0.5 * (lo + hi)
}

#' .gpzapbestsplit
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param y See Usage.
#' @param x See Usage.
#' @param candidates Defaults to \code{NULL}.
#' @return A list with \code{threshold}, \code{loglik}.
#' @export
.gpzapbestsplit <- function(y, x, candidates = NULL) {
  ys <- .gpflat(y); xs <- .gpflat(x)
  vals <- if (is.null(candidates)) sort(unique(xs)) else .gpflat(candidates)
  bt <- NULL; bl <- -Inf
  for (v in vals) {
    L <- ys[xs <= v & ys > 0]; R <- ys[xs > v & ys > 0]
    if (!length(L) || !length(R)) next
    ll <- .gpztploglik(L, .gpztpmle(L)) + .gpztploglik(R, .gpztpmle(R))
    if (is.null(bt) || ll > bl) { bt <- v; bl <- ll }
  }
  list(threshold = bt, loglik = if (is.null(bt)) -Inf else bl)
}

# --- chapter 7: ordinal latent-scale predictors, eqs (7.3)-(7.5) ----------
#' Chapter 7: ordinal latent-scale predictors, eqs (7.3)-(7.5)
#' ----------
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param n See Usage.
#' @param X_E Defaults to \code{NULL}.
#' @param X Defaults to \code{NULL}.
#' @param X_EM Defaults to \code{NULL}.
#' @param Z_L Defaults to \code{NULL}.
#' @param L_g Defaults to \code{NULL}.
#' @return A list with \code{design}, \code{widths}, \code{n_columns}.
#' @export
.gpordlatent <- function(n, X_E = NULL, X = NULL, X_EM = NULL,
                         Z_L = NULL, L_g = NULL) {
  blocks <- list(); nm <- character(0)
  add <- function(name, M) { blocks[[length(blocks) + 1L]] <<- .gpmat(M); nm <<- c(nm, name) }
  if (!is.null(X_E)) add("environments", X_E)
  if (!is.null(X)) add("markers", X)
  if (!is.null(X_EM)) add("env_x_marker", X_EM)
  if (!is.null(Z_L)) add("genetic",
    if (!is.null(L_g)) .gpmat(Z_L) %*% .gpmat(L_g) else .gpmat(Z_L))
  if (!length(blocks)) stop("the predictor needs at least one block")
  design <- do.call(cbind, lapply(blocks, function(M) M[seq_len(n), , drop = FALSE]))
  w <- as.list(vapply(blocks, ncol, 1L)); names(w) <- nm
  list(design = design, widths = w, n_columns = ncol(design))
}

# --- chapter 8: reproducing kernel Hilbert space, eqs (8.1)-(8.3) ---------
#' Chapter 8: reproducing kernel Hilbert space, eqs (8.1)-(8.3)
#' ---------
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param beta See Usage.
#' @param K See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.gprkhsnorm <- function(beta, K) {
  b <- .gpflat(beta); as.numeric(t(b) %*% .gpmat(K) %*% b)
}

#' .gprkhspredict
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param K_new See Usage.
#' @param beta See Usage.
#' @param eta0 Defaults to \code{0}.
#' @return A numeric value.
#' @export
.gprkhspredict <- function(K_new, beta, eta0 = 0)
  as.numeric(eta0) + as.numeric(.gpmat(K_new) %*% .gpflat(beta))

#' .gprkhsfitsq
#'
#' Part of the helpers_gp_core implementation; see the file header for
#' the source it follows.
#'
#' @param K See Usage.
#' @param y See Usage.
#' @param lam Defaults to \code{1}.
#' @return A list with \code{eta0}, \code{beta}, \code{fitted}, \code{residuals}, \code{loss}, \code{penalty}, \code{objective}.
#' @export
.gprkhsfitsq <- function(K, y, lam = 1) {
  Km <- .gpmat(K); ys <- .gpflat(y); n <- length(ys)
  A <- matrix(0, n + 1L, n + 1L); rhs <- numeric(n + 1L)
  colsum <- colSums(Km)
  A[1L, 1L] <- 1
  A[1L, 2L:(n + 1L)] <- colsum / n
  rhs[1L] <- sum(ys) / n
  A[2L:(n + 1L), 1L] <- colsum * 2 / n
  A[2L:(n + 1L), 2L:(n + 1L)] <- 2 * (t(Km) %*% Km) / n + as.numeric(lam) * Km
  rhs[2L:(n + 1L)] <- 2 * as.numeric(t(Km) %*% ys) / n
  sol <- .gpsolve(A, rhs)
  eta0 <- sol[1L]; beta <- sol[-1L]
  fitted <- .gprkhspredict(Km, beta, eta0)
  resid <- ys - fitted
  loss <- sum(resid^2) / n
  pen <- 0.5 * as.numeric(lam) * .gprkhsnorm(beta, Km)
  list(eta0 = eta0, beta = beta, fitted = fitted, residuals = resid,
       loss = loss, penalty = pen, objective = loss + pen)
}
