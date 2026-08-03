# MVSML shelf: R mirror of the morie Python genomic-prediction core
# (src/morie/fn/_gp_core.py).  Montesinos López, Montesinos López &
# Crossa (2022), Springer, DOI 10.1007/978-3-030-89010-0.
# Certified equations: (1.2)-(1.5) pp.15-16; (2.1)-(2.2) p.36;
# (2.3)-(2.4) p.53; GRM method 3 p.52; PCA sec. 2.8 pp.63-64;
# (3.1) p.71 + OLS pp.72-73; EPE p.80; ridge p.81; (4.5)-(4.14)
# pp.131-136.

#' @noRd
morie_mvsml_one_way <- function(groups) {
  G <- lapply(groups, as.numeric)
  r <- length(G[[1]])
  a <- length(G)
  if (any(vapply(G, length, 1L) != r)) {
    stop("need a balanced layout (equal group sizes)")
  }
  n <- a * r
  means <- vapply(G, mean, 0)
  grand <- mean(unlist(G))
  ss_b <- r * sum((means - grand)^2)
  ss_w <- sum(unlist(Map(function(g, m) sum((g - m)^2), G, means)))
  ms_b <- ss_b / (a - 1)
  ms_w <- ss_w / (n - a)
  s2b <- max((ms_b - ms_w) / r, 0)
  list(grand_mean = grand,
       sd_single_mean = sqrt((ss_b + ss_w) / (n - 1)),
       group_means = means,
       sd_residual = sqrt(ms_w),
       deviations = means - grand,
       sigma2_b = s2b,
       icc = if (s2b + ms_w > 0) s2b / (s2b + ms_w) else 0,
       ms_between = ms_b, ms_within = ms_w)
}

#' @noRd
morie_mvsml_mme <- function(X, Z, y, Sigma_inv, R_inv = NULL) {
  X <- as.matrix(X); Z <- as.matrix(Z); y <- as.numeric(y)
  n <- length(y); p <- ncol(X); q <- ncol(Z)
  if (is.null(R_inv)) R_inv <- diag(n)
  XtRi <- t(X) %*% R_inv
  ZtRi <- t(Z) %*% R_inv
  LHS <- rbind(cbind(XtRi %*% X, XtRi %*% Z),
               cbind(ZtRi %*% X, ZtRi %*% Z + as.matrix(Sigma_inv)))
  RHS <- rbind(XtRi %*% y, ZtRi %*% y)
  sol <- solve(LHS, RHS)
  list(blue = as.numeric(sol[seq_len(p)]),
       blup = as.numeric(sol[p + seq_len(q)]))
}

#' @noRd
morie_mvsml_blue_blup_v <- function(X, Z, y, Sigma, R = NULL) {
  X <- as.matrix(X); Z <- as.matrix(Z); y <- as.numeric(y)
  n <- length(y)
  if (is.null(R)) R <- diag(n)
  V <- Z %*% as.matrix(Sigma) %*% t(Z) + R
  Vi <- solve(V)
  beta <- solve(t(X) %*% Vi %*% X, t(X) %*% Vi %*% y)
  u <- as.matrix(Sigma) %*% t(Z) %*% Vi %*% (y - X %*% beta)
  list(blue = as.numeric(beta), blup = as.numeric(u))
}

#' @noRd
morie_mvsml_grm <- function(M) {
  Xs <- scale(as.matrix(M))
  tcrossprod(Xs) / ncol(Xs)
}

#' @noRd
morie_mvsml_gblup <- function(X, y, G, sigma2_g, sigma2_e = 1) {
  q <- nrow(G); n <- length(y)
  Z <- diag(n)[, seq_len(q), drop = FALSE]
  morie_mvsml_blue_blup_v(X, Z, y, sigma2_g * G,
                          diag(sigma2_e, n))$blup
}

#' @noRd
morie_mvsml_snp_blup <- function(X, y, M, sigma2_m, sigma2_e = 1) {
  M <- as.matrix(M); p <- ncol(M); n <- length(y)
  fit <- morie_mvsml_blue_blup_v(X, M, y, diag(sigma2_m, p),
                                 diag(sigma2_e, n))
  list(marker_effects = fit$blup,
       gebv = as.numeric(M %*% fit$blup))
}

#' @noRd
morie_mvsml_pca <- function(X, k = NULL) {
  Xs <- scale(as.matrix(X))
  n <- nrow(Xs)
  Q <- (t(Xs) %*% Xs) / (n - 1)
  e <- eigen(Q, symmetric = TRUE)
  lam <- e$values
  PC <- Xs %*% e$vectors
  k <- if (is.null(k)) ncol(Xs) else k
  list(eigenvalues = lam, sd_pc = sqrt(pmax(lam, 0)),
       loadings = e$vectors, scores = PC,
       compressed = PC[, seq_len(k), drop = FALSE],
       prop_variance = lam / sum(lam),
       cum_variance = cumsum(lam) / sum(lam))
}

#' @noRd
morie_mvsml_ols <- function(X, y, add_intercept = TRUE) {
  X <- as.matrix(X)
  if (add_intercept) X <- cbind(1, X)
  y <- as.numeric(y)
  XtXi <- solve(t(X) %*% X)
  beta <- as.numeric(XtXi %*% t(X) %*% y)
  fitted <- as.numeric(X %*% beta)
  resid <- y - fitted
  rss <- sum(resid^2)
  dof <- length(y) - ncol(X)
  s2 <- if (dof > 0) rss / dof else NA_real_
  list(beta = beta, fitted = fitted, residuals = resid, rss = rss,
       sigma2 = s2, sigma2_ml = rss / length(y),
       se_beta = sqrt(s2 * diag(XtXi)))
}

#' @noRd
morie_mvsml_ridge <- function(X, y, lambda, add_intercept = TRUE) {
  X <- as.matrix(X)
  if (add_intercept) X <- cbind(1, X)
  y <- as.numeric(y)
  p <- ncol(X)
  D <- diag(p)
  if (add_intercept) D[1, 1] <- 0
  beta <- as.numeric(solve(t(X) %*% X + lambda * D, t(X) %*% y))
  fitted <- as.numeric(X %*% beta)
  rss <- sum((y - fitted)^2)
  pen <- lambda * sum((beta^2)[diag(D) == 1])
  list(beta = beta, fitted = fitted, rss = rss, penalty = pen,
       prss = rss + pen)
}

#' @noRd
morie_mvsml_epe <- function(sigma2, x_star, eigenvalues) {
  if (any(eigenvalues <= 0)) stop("eigenvalues must be positive")
  sigma2 * (1 + sum(x_star^2 / eigenvalues))
}

#' @noRd
morie_mvsml_binary_metrics <- function(y_true, y_pred, positive = 1) {
  yt <- as.integer(y_true); yp <- as.integer(y_pred)
  tp <- sum(yt == positive & yp == positive)
  tn <- sum(yt != positive & yp != positive)
  fp <- sum(yt != positive & yp == positive)
  fn <- sum(yt == positive & yp != positive)
  n <- length(yt)
  p0 <- (tp + tn) / n
  pe <- (tp + fn) / n * (tp + fp) / n + (fp + tn) / n * (fn + tn) / n
  se <- if (tp + fn > 0) tp / (tp + fn) else 0
  sp <- if (tn + fp > 0) tn / (tn + fp) else 0
  list(tp = tp, fp = fp, tn = tn, fn = fn, pccc = p0,
       sensitivity = se, specificity = sp,
       precision = if (tp + fp > 0) tp / (tp + fp) else 0,
       neg_pred_value = if (tn + fn > 0) tn / (tn + fn) else 0,
       prevalence = (tp + fn) / n, detection_rate = tp / n,
       balanced_accuracy = (se + sp) / 2,
       kappa = if (pe < 1) (p0 - pe) / (1 - pe) else 0)
}

#' @noRd
morie_mvsml_mcc <- function(y_true, y_pred, positive = 1) {
  m <- morie_mvsml_binary_metrics(y_true, y_pred, positive)
  den <- (m$tp + m$fp) * (m$tp + m$fn) * (m$tn + m$fp) *
    (m$tn + m$fn)
  if (den == 0) return(0)
  (m$tp * m$tn - m$fp * m$fn) / sqrt(den)
}

#' @noRd
morie_mvsml_class_metrics <- function(conf, i) {
  C <- nrow(conf)
  tfn <- sum(conf[i, -i])
  tfp <- sum(conf[-i, i])
  ttn <- sum(conf[-i, -i])
  ttp <- sum(diag(conf))
  total <- sum(conf)
  list(TFN = tfn, TFP = tfp, TTN = ttn, TTP_all = ttp,
       precision = if (ttp + tfp > 0) ttp / (ttp + tfp) else 0,
       sensitivity = if (ttp + tfn > 0) ttp / (ttp + tfn) else 0,
       specificity = if (ttn + tfp > 0) ttn / (ttn + tfp) else 0,
       pCCC = if (total > 0) ttp / total else 0)
}

#' @noRd
morie_mvsml_brier <- function(probs, y_true, halved = FALSE) {
  P <- as.matrix(probs)
  yt <- as.integer(y_true)
  D <- matrix(0, nrow = nrow(P), ncol = ncol(P))
  D[cbind(seq_along(yt), yt + 1L)] <- 1
  bs <- sum((P - D)^2) / length(yt)
  if (halved) bs / 2 else bs
}

#' @noRd
morie_mvsml_mll <- function(probs, y_true) {
  P <- as.matrix(probs)
  yt <- as.integer(y_true)
  -mean(log(pmax(P[cbind(seq_along(yt), yt + 1L)], 1e-300)))
}
