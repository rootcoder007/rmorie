# SPDX-License-Identifier: AGPL-3.0-or-later
# Internal helpers mirroring morie.fn._gp_core (Montesinos Lopez, Montesinos
# Lopez & Crossa 2022, Multivariate Statistical Machine Learning Methods for
# Genomic Prediction, Springer, DOI 10.1007/978-3-030-89010-0).
# Base R only. Not exported.

.gpflat <- function(x) as.numeric(unlist(x))

.gpmat <- function(A) {
  if (is.matrix(A)) return(matrix(as.numeric(A), nrow(A), ncol(A)))
  if (is.list(A)) return(do.call(rbind, lapply(A, as.numeric)))
  matrix(as.numeric(A), nrow = 1L)
}

# Solve A x = b; rank-deficient systems fall back on the minimum-norm
# pseudo-inverse solution, as the Python arm does.
.gpsolve <- function(A, b) {
  A <- .gpmat(A); b <- .gpflat(b)
  out <- tryCatch(as.numeric(solve(A, b)), error = function(e) NULL)
  if (!is.null(out) && all(is.finite(out))) return(out)
  as.numeric(.gppinv(A) %*% b)
}

.gppinv <- function(A) {
  s <- svd(.gpmat(A))
  tol <- max(dim(A)) * .Machine$double.eps * max(s$d, 0)
  di <- ifelse(s$d > tol, 1 / s$d, 0)
  s$v %*% (di * t(s$u))
}

.gpinv <- function(A) {
  A <- .gpmat(A)
  out <- tryCatch(solve(A), error = function(e) NULL)
  if (!is.null(out) && all(is.finite(out))) return(out)
  .gppinv(A)
}

# log|det A| via the modulus, matching _gp_core._logdet (which sums
# log|pivot| and so discards the sign).
.gplogdet <- function(A) {
  A <- .gpmat(A)
  d <- determinant(A, logarithm = TRUE)
  v <- as.numeric(d$modulus)
  if (!is.finite(v)) -Inf else v
}

# --- chapter 1: balanced one-way layout, eqs (1.2)-(1.5) -------------------
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
.gpconf <- function(y_true, y_pred, n_classes = NULL) {
  yt <- as.integer(.gpflat(y_true)); yp <- as.integer(.gpflat(y_pred))
  C <- if (is.null(n_classes)) max(c(yt, yp)) + 1L else as.integer(n_classes)
  M <- matrix(0, C, C)
  for (k in seq_along(yt)) M[yt[k] + 1L, yp[k] + 1L] <- M[yt[k] + 1L, yp[k] + 1L] + 1
  M
}

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

.gpmll <- function(probs, y_true, n_classes = NULL) {
  P <- .gpmat(probs); yt <- as.integer(.gpflat(y_true))
  -sum(log(pmax(P[cbind(seq_along(yt), yt + 1L)], 1e-300))) / length(yt)
}

# --- chapter 5: linear mixed model, eqs (5.1)-(5.2) and REML ---------------
.gplmmV <- function(Z, D, R = NULL) {
  Z <- .gpmat(Z); n <- nrow(Z)
  V <- Z %*% .gpmat(D) %*% t(Z)
  V + (if (is.null(R)) diag(n) else .gpmat(R))
}

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
