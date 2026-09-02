# MVSML shelf: R mirror of the morie Python genomic-prediction core
# (src/morie/fn/_gp_core.py).  Montesinos López, Montesinos López &
# Crossa (2022), Springer, DOI 10.1007/978-3-030-89010-0.
# Certified equations: (1.2)-(1.5) pp.15-16; (2.1)-(2.2) p.36;
# (2.3)-(2.4) p.53; GRM method 3 p.52; PCA sec. 2.8 pp.63-64;
# (3.1) p.71 + OLS pp.72-73; EPE p.80; ridge p.81; (4.5)-(4.14)
# pp.131-136.

#' @noRd
morie_pinv <- function(A, rcond = 1e-15) {
  A <- as.matrix(A)
  s <- svd(A)
  tol <- rcond * max(s$d)
  di <- ifelse(s$d > tol, 1 / s$d, 0)
  s$v %*% diag(di, nrow = length(di)) %*% t(s$u)
}

#' @noRd
morie_solve <- function(A, b = NULL) {
  # Rank-deficient systems are legitimate here (intercept-only design
  # blocks, zero covariate columns, a genomic relationship matrix with
  # fewer markers than lines), so fall back on the rank-gated
  # pseudo-inverse rather than failing.
  if (is.null(b)) {
    out <- try(solve(A), silent = TRUE)
    if (inherits(out, "try-error")) {
      return(morie_pinv(A))
    }
    return(out)
  }
  out <- try(solve(A, b), silent = TRUE)
  if (inherits(out, "try-error")) {
    return(morie_pinv(A) %*% b)
  }
  out
}

#' @noRd
morie_one_way <- function(groups) {
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
  list(
    grand_mean = grand,
    sd_single_mean = sqrt((ss_b + ss_w) / (n - 1)),
    group_means = means,
    sd_residual = sqrt(ms_w),
    deviations = means - grand,
    sigma2_b = s2b,
    icc = if (s2b + ms_w > 0) s2b / (s2b + ms_w) else 0,
    ms_between = ms_b, ms_within = ms_w
  )
}

#' @noRd
morie_mme <- function(X, Z, y, Sigma_inv, R_inv = NULL) {
  X <- as.matrix(X)
  Z <- as.matrix(Z)
  y <- as.numeric(y)
  n <- length(y)
  p <- ncol(X)
  q <- ncol(Z)
  if (is.null(R_inv)) R_inv <- diag(n)
  XtRi <- t(X) %*% R_inv
  ZtRi <- t(Z) %*% R_inv
  LHS <- rbind(
    cbind(XtRi %*% X, XtRi %*% Z),
    cbind(ZtRi %*% X, ZtRi %*% Z + as.matrix(Sigma_inv))
  )
  RHS <- rbind(XtRi %*% y, ZtRi %*% y)
  sol <- morie_solve(LHS, RHS)
  list(
    blue = as.numeric(sol[seq_len(p)]),
    blup = as.numeric(sol[p + seq_len(q)])
  )
}

#' @noRd
morie_blue_blup_v <- function(X, Z, y, Sigma, R = NULL) {
  X <- as.matrix(X)
  Z <- as.matrix(Z)
  y <- as.numeric(y)
  n <- length(y)
  if (is.null(R)) R <- diag(n)
  V <- Z %*% as.matrix(Sigma) %*% t(Z) + R
  Vi <- morie_solve(V)
  beta <- morie_solve(t(X) %*% Vi %*% X, t(X) %*% Vi %*% y)
  u <- as.matrix(Sigma) %*% t(Z) %*% Vi %*% (y - X %*% beta)
  list(blue = as.numeric(beta), blup = as.numeric(u))
}

#' @noRd
morie_grm <- function(M) {
  Xs <- scale(as.matrix(M))
  tcrossprod(Xs) / ncol(Xs)
}

#' @noRd
morie_gblup <- function(X, y, G, sigma2_g, sigma2_e = 1) {
  q <- nrow(G)
  n <- length(y)
  Z <- diag(n)[, seq_len(q), drop = FALSE]
  morie_blue_blup_v(
    X, Z, y, sigma2_g * G,
    diag(sigma2_e, n)
  )$blup
}

#' @noRd
morie_snp_blup <- function(X, y, M, sigma2_m, sigma2_e = 1) {
  M <- as.matrix(M)
  p <- ncol(M)
  n <- length(y)
  fit <- morie_blue_blup_v(
    X, M, y, diag(sigma2_m, p),
    diag(sigma2_e, n)
  )
  list(
    marker_effects = fit$blup,
    gebv = as.numeric(M %*% fit$blup)
  )
}

#' @noRd
morie_pca <- function(X, k = NULL) {
  Xs <- scale(as.matrix(X))
  n <- nrow(Xs)
  Q <- (t(Xs) %*% Xs) / (n - 1)
  e <- eigen(Q, symmetric = TRUE)
  lam <- e$values
  # An eigenvector is defined only up to sign, and eigen() and Python's
  # eigh need not choose the same one. Pin it: each column's
  # largest-magnitude entry is made positive, matching the convention in
  # _tail1core.eigsym. Without this the eigenvalues agree across the two
  # arms while the loadings differ by -1.
  V <- e$vectors
  for (j in seq_len(ncol(V))) {
    piv <- which.max(abs(V[, j]))
    if (V[piv, j] < 0) V[, j] <- -V[, j]
  }
  e$vectors <- V
  PC <- Xs %*% V
  k <- if (is.null(k)) ncol(Xs) else k
  list(
    eigenvalues = lam, sd_pc = sqrt(pmax(lam, 0)),
    loadings = e$vectors, scores = PC,
    compressed = PC[, seq_len(k), drop = FALSE],
    prop_variance = lam / sum(lam),
    cum_variance = cumsum(lam) / sum(lam)
  )
}


#' @noRd
morie_ridge <- function(X, y, lambda, add_intercept = TRUE) {
  X <- as.matrix(X)
  if (add_intercept) X <- cbind(1, X)
  y <- as.numeric(y)
  p <- ncol(X)
  D <- diag(p)
  if (add_intercept) D[1, 1] <- 0
  beta <- as.numeric(morie_solve(
    t(X) %*% X + lambda * D,
    t(X) %*% y
  ))
  fitted <- as.numeric(X %*% beta)
  rss <- sum((y - fitted)^2)
  pen <- lambda * sum((beta^2)[diag(D) == 1])
  list(
    beta = beta, fitted = fitted, rss = rss, penalty = pen,
    prss = rss + pen
  )
}

#' @noRd
morie_epe <- function(sigma2, x_star, eigenvalues) {
  if (any(eigenvalues <= 0)) stop("eigenvalues must be positive")
  sigma2 * (1 + sum(x_star^2 / eigenvalues))
}

#' @noRd
morie_binary_metrics <- function(y_true, y_pred, positive = 1) {
  yt <- as.integer(y_true)
  yp <- as.integer(y_pred)
  tp <- sum(yt == positive & yp == positive)
  tn <- sum(yt != positive & yp != positive)
  fp <- sum(yt != positive & yp == positive)
  fn <- sum(yt == positive & yp != positive)
  n <- length(yt)
  p0 <- (tp + tn) / n
  pe <- (tp + fn) / n * (tp + fp) / n + (fp + tn) / n * (fn + tn) / n
  se <- if (tp + fn > 0) tp / (tp + fn) else 0
  sp <- if (tn + fp > 0) tn / (tn + fp) else 0
  list(
    tp = tp, fp = fp, tn = tn, fn = fn, pccc = p0,
    sensitivity = se, specificity = sp,
    precision = if (tp + fp > 0) tp / (tp + fp) else 0,
    neg_pred_value = if (tn + fn > 0) tn / (tn + fn) else 0,
    prevalence = (tp + fn) / n, detection_rate = tp / n,
    balanced_accuracy = (se + sp) / 2,
    kappa = if (pe < 1) (p0 - pe) / (1 - pe) else 0
  )
}

#' @noRd
morie_mcc <- function(y_true, y_pred, positive = 1) {
  m <- morie_binary_metrics(y_true, y_pred, positive)
  den <- (m$tp + m$fp) * (m$tp + m$fn) * (m$tn + m$fp) *
    (m$tn + m$fn)
  if (den == 0) {
    return(0)
  }
  (m$tp * m$tn - m$fp * m$fn) / sqrt(den)
}

#' @noRd
morie_class_metrics <- function(conf, i) {
  C <- nrow(conf)
  tfn <- sum(conf[i, -i])
  tfp <- sum(conf[-i, i])
  ttn <- sum(conf[-i, -i])
  ttp <- sum(diag(conf))
  total <- sum(conf)
  list(
    TFN = tfn, TFP = tfp, TTN = ttn, TTP_all = ttp,
    precision = if (ttp + tfp > 0) ttp / (ttp + tfp) else 0,
    sensitivity = if (ttp + tfn > 0) ttp / (ttp + tfn) else 0,
    specificity = if (ttn + tfp > 0) ttn / (ttn + tfp) else 0,
    pCCC = if (total > 0) ttp / total else 0
  )
}

#' @noRd
morie_brier <- function(probs, y_true, halved = FALSE) {
  P <- as.matrix(probs)
  yt <- as.integer(y_true)
  D <- matrix(0, nrow = nrow(P), ncol = ncol(P))
  D[cbind(seq_along(yt), yt + 1L)] <- 1
  bs <- sum((P - D)^2) / length(yt)
  if (halved) bs / 2 else bs
}

#' @noRd
morie_mll <- function(probs, y_true) {
  P <- as.matrix(probs)
  yt <- as.integer(y_true)
  -mean(log(pmax(P[cbind(seq_along(yt), yt + 1L)], 1e-300)))
}

# ---- chapter 5: linear mixed models (MVSML 2022 pp.142-155) ----

#' @noRd
morie_kron <- function(A, B) kronecker(as.matrix(A), as.matrix(B))

#' @noRd
morie_lmm_v <- function(Z, D, R = NULL) {
  Z <- as.matrix(Z)
  if (is.null(R)) R <- diag(nrow(Z))
  Z %*% as.matrix(D) %*% t(Z) + R
}

#' @noRd
#' REML log-likelihood of a linear mixed model (MVSML eq. 5.2)
#'
#' Restricted log-likelihood for y = X b + Z u + e with u ~ N(0, D) and
#' e ~ N(0, R): the profile over b is taken at its GLS estimate and the
#' log-determinant of X' V^-1 X is subtracted (Searle, Casella &
#' McCulloch 1992, ch. 6; MVSML ch. 5). Companion of [morie_lmm_loglik()].
#' The one-way random-effects REML fit lives in [morie_remlfn()].
#'
#' @param X Fixed-effects design matrix (n x p).
#' @param Z Random-effects design matrix (n x q).
#' @param y Numeric response of length n.
#' @param D Covariance of the random effects (q x q).
#' @param R Residual covariance (n x n); identity when `NULL`.
#' @return A list with `loglik` (the REML log-likelihood) and `beta`
#'   (the GLS fixed-effect estimate).
#' @examples
#' X <- matrix(1, 6, 1)
#' Z <- matrix(c(1,0, 1,0, 1,0, 0,1, 0,1, 0,1), 6, byrow = TRUE)
#' y <- c(5.0, 5.2, 4.8, 6.4, 6.6, 6.2)
#' morie_reml_loglik(X, Z, y, diag(0.5, 2))$loglik
#' @export
morie_reml_loglik <- function(X, Z, y, D, R = NULL) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  V <- morie_lmm_v(Z, D, R)
  Vi <- morie_solve(V)
  A <- t(X) %*% Vi %*% X
  beta <- morie_solve(A, t(X) %*% Vi %*% y)
  r <- y - X %*% beta
  ll <- -0.5 * determinant(A, logarithm = TRUE)$modulus[1] -
    0.5 * determinant(V, logarithm = TRUE)$modulus[1] -
    0.5 * as.numeric(t(r) %*% Vi %*% r)
  list(loglik = ll, beta = as.numeric(beta))
}

morie_lmm_loglik <- function(X, Z, y, D, R = NULL, beta = NULL) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  n <- length(y)
  V <- morie_lmm_v(Z, D, R)
  Vi <- morie_solve(V)
  if (is.null(beta)) {
    beta <- morie_solve(t(X) %*% Vi %*% X, t(X) %*% Vi %*% y)
  }
  r <- y - X %*% beta
  ll <- -0.5 * n * log(2 * pi) -
    0.5 * determinant(V, logarithm = TRUE)$modulus[1] -
    0.5 * as.numeric(t(r) %*% Vi %*% r)
  list(loglik = ll, beta = as.numeric(beta))
}

#' @noRd
morie_em_lmm <- function(X, Z, y, D0 = NULL, sigma2_0 = 1,
                         n_iter = 200L, tol = 1e-10) {
  X <- as.matrix(X)
  Z <- as.matrix(Z)
  y <- as.numeric(y)
  n <- length(y)
  q <- ncol(Z)
  D <- if (is.null(D0)) diag(q) else as.matrix(D0)
  s2 <- sigma2_0
  XtX <- t(X) %*% X
  ZtZ <- t(Z) %*% Z
  beta <- morie_solve(XtX, t(X) %*% y)
  bt <- rep(0, q)
  it <- 0L
  for (i in seq_len(n_iter)) {
    it <- i
    Dt <- morie_solve(morie_solve(D) + ZtZ / s2)
    bt <- as.numeric(Dt %*% t(Z) %*% (y - X %*% beta) / s2)
    beta_new <- morie_solve(XtX, t(X) %*% (y - Z %*% bt))
    e <- as.numeric(y - X %*% beta_new - Z %*% bt)
    ZDZ <- Z %*% Dt %*% t(Z)
    s2_new <- (sum(diag(ZDZ)) + sum(e^2)) / n
    D_new <- Dt + outer(bt, bt)
    gap <- max(abs(s2_new - s2), max(abs(beta_new - beta)))
    beta <- beta_new
    s2 <- s2_new
    D <- D_new
    if (gap < tol) break
  }
  list(
    beta = as.numeric(beta), sigma2 = s2, D = D, b = bt,
    iterations = it
  )
}

#' @noRd
morie_gblup_model <- function(y, Z_L, G, sigma2_g,
                              sigma2_e = 1) {
  y <- as.numeric(y)
  n <- length(y)
  X <- matrix(1, nrow = n, ncol = 1)
  fit <- morie_blue_blup_v(
    X, Z_L, y, sigma2_g * as.matrix(G),
    diag(sigma2_e, n)
  )
  list(mu = fit$blue[1], b = fit$blup)
}

#' @noRd
morie_gxe_blup <- function(y, X_E, Z_L, Z_EL, G, sigma2_g,
                           Sigma_E, sigma2_e = 1) {
  y <- as.numeric(y)
  n <- length(y)
  X <- if (is.null(X_E) || length(X_E) == 0) {
    matrix(1, nrow = n, ncol = 1)
  } else {
    cbind(1, as.matrix(X_E))
  }
  ZL <- as.matrix(Z_L)
  ZEL <- as.matrix(Z_EL)
  q1 <- ncol(ZL)
  S2 <- morie_kron(Sigma_E, G)
  q2 <- nrow(S2)
  Z <- cbind(ZL, ZEL)
  Sigma <- matrix(0, q1 + q2, q1 + q2)
  Sigma[seq_len(q1), seq_len(q1)] <- sigma2_g * as.matrix(G)
  Sigma[q1 + seq_len(q2), q1 + seq_len(q2)] <- S2
  fit <- morie_blue_blup_v(X, Z, y, Sigma, diag(sigma2_e, n))
  list(
    beta = fit$blue, b_lines = fit$blup[seq_len(q1)],
    b_gxe = fit$blup[q1 + seq_len(q2)]
  )
}

#' @noRd
morie_multitrait <- function(Y, Z, G, Sigma_T, R_T, X = NULL) {
  Ym <- as.matrix(Y)
  J <- nrow(Ym)
  nT <- ncol(Ym)
  y <- as.numeric(t(Ym))
  Xm <- morie_kron(matrix(1, J, 1), diag(nT))
  if (!is.null(X)) Xm <- cbind(Xm, as.matrix(X))
  Zm <- morie_kron(Z, diag(nT))
  Sigma <- morie_kron(G, Sigma_T)
  R <- morie_kron(diag(J), R_T)
  fit <- morie_blue_blup_v(Xm, Zm, y, Sigma, R)
  list(
    mu = fit$blue[seq_len(nT)], beta = fit$blue, b = fit$blup,
    b_by_line = split(
      fit$blup,
      rep(seq_len(length(fit$blup) / nT),
        each = nT
      )
    )
  )
}

#' @noRd
morie_gxe_multitrait <- function(Y, Z_L, Z_EL, G, Sigma_T,
                                 Sigma_E, Sigma_2T, R_T,
                                 X = NULL) {
  Ym <- as.matrix(Y)
  rows <- nrow(Ym)
  nT <- ncol(Ym)
  y <- as.numeric(t(Ym))
  Xm <- morie_kron(matrix(1, rows, 1), diag(nT))
  if (!is.null(X)) Xm <- cbind(Xm, as.matrix(X))
  Z1 <- morie_kron(Z_L, diag(nT))
  Z2 <- morie_kron(Z_EL, diag(nT))
  S1 <- morie_kron(G, Sigma_T)
  S2 <- morie_kron(morie_kron(Sigma_E, G), Sigma_2T)
  q1 <- nrow(S1)
  q2 <- nrow(S2)
  Z <- cbind(Z1, Z2)
  Sigma <- matrix(0, q1 + q2, q1 + q2)
  Sigma[seq_len(q1), seq_len(q1)] <- S1
  Sigma[q1 + seq_len(q2), q1 + seq_len(q2)] <- S2
  R <- morie_kron(diag(rows), R_T)
  fit <- morie_blue_blup_v(Xm, Z, y, Sigma, R)
  list(
    mu = fit$blue[seq_len(nT)], beta = fit$blue,
    b_lines = fit$blup[seq_len(q1)],
    b_gxe = fit$blup[q1 + seq_len(q2)]
  )
}

# ---- chapter 6: Bayesian genomic linear regression (pp.171-186) ----

#' @noRd
morie_scaled_inv_chisq <- function(nu, S, n = 1L) {
  S / rchisq(n, df = nu)
}

#' @noRd
morie_brr_hyper <- function(y, R2 = 0.5, nu = 5, nu_beta = 5,
                            sum_var_x = NULL) {
  var_y <- var(as.numeric(y))
  S_beta <- var_y * R2 * (nu_beta + 2)
  if (!is.null(sum_var_x)) S_beta <- S_beta / sum_var_x
  list(
    S = var_y * (1 - R2) * (nu + 2), S_beta = S_beta,
    nu = nu, nu_beta = nu_beta, var_y = var_y
  )
}

#' @noRd
morie_chol_lower <- function(G) t(chol(as.matrix(G)))

#' @noRd
morie_brr_gibbs <- function(y, X, n_iter = 2000L,
                            burn_in = 500L, nu = 5,
                            nu_beta = 5, R2 = 0.5,
                            seed = 42L) {
  set.seed(seed)
  y <- as.numeric(y)
  X <- as.matrix(X)
  n <- length(y)
  p <- ncol(X)
  hp <- morie_brr_hyper(y, R2, nu, nu_beta)
  mu <- mean(y)
  beta <- rep(0, p)
  s2 <- hp$S / (nu + 2)
  s2b <- hp$S_beta / (nu_beta + 2)
  XtX <- t(X) %*% X
  acc <- list(mu = 0, beta = rep(0, p), s2 = 0, s2b = 0, k = 0L)
  for (it in seq_len(n_iter)) {
    s2b <- morie_scaled_inv_chisq(
      nu_beta + p,
      hp$S_beta + sum(beta^2)
    )
    A <- XtX / s2 + diag(1 / s2b, p)
    rhs <- t(X) %*% (y - mu) / s2
    Ai <- morie_pinv(A)
    mean_b <- Ai %*% rhs
    L <- t(chol(Ai))
    beta <- as.numeric(mean_b + L %*% rnorm(p))
    r <- y - X %*% beta
    mu <- mean(r) + sqrt(s2 / n) * rnorm(1)
    e <- r - mu
    s2 <- morie_scaled_inv_chisq(nu + n, hp$S + sum(e^2))
    if (it > burn_in) {
      acc$k <- acc$k + 1L
      acc$mu <- acc$mu + mu
      acc$beta <- acc$beta + beta
      acc$s2 <- acc$s2 + s2
      acc$s2b <- acc$s2b + s2b
    }
  }
  list(
    mu = acc$mu / acc$k, beta = acc$beta / acc$k,
    sigma2 = acc$s2 / acc$k, sigma2_beta = acc$s2b / acc$k,
    n_kept = acc$k, hyper = hp
  )
}

#' @noRd
morie_bayes_gblup <- function(y, G, n_iter = 2000L,
                              burn_in = 500L, seed = 42L,
                              ...) {
  L <- morie_chol_lower(G)
  fit <- morie_brr_gibbs(y, L,
    n_iter = n_iter,
    burn_in = burn_in, seed = seed, ...
  )
  fit$g <- as.numeric(L %*% fit$beta)
  fit$sigma2_g <- fit$sigma2_beta
  fit
}

#' @noRd
morie_rkhs_cov <- function(Z_L, G, Z_LE = NULL, I_env = NULL,
                           sigma2_g = 1, sigma2_ge = 1) {
  ZL <- as.matrix(Z_L)
  out <- list(K_L = sigma2_g * (ZL %*% as.matrix(G) %*% t(ZL)))
  if (!is.null(Z_LE) && !is.null(I_env)) {
    ZLE <- as.matrix(Z_LE)
    out$K_LE <- sigma2_ge *
      (ZLE %*% morie_kron(I_env, G) %*% t(ZLE))
  }
  out
}

#' @noRd
morie_extended_predictor <- function(n, X_E = NULL, X = NULL,
                                     X_EM = NULL) {
  design <- matrix(1, nrow = n, ncol = 1)
  widths <- c(intercept = 1L)
  for (nm in c("environments", "markers", "env_x_marker")) {
    B <- switch(nm,
      environments = X_E,
      markers = X,
      env_x_marker = X_EM
    )
    if (!is.null(B)) {
      B <- as.matrix(B)
      design <- cbind(design, B)
      widths[nm] <- ncol(B)
    }
  }
  list(design = design, widths = widths, n_columns = ncol(design))
}

# ---- chapter 6b: multi-trait Bayesian / BMTME (pp.190-196) ----

#' @noRd
morie_inv_wishart <- function(nu, S) {
  S <- as.matrix(S)
  p <- nrow(S)
  L <- t(chol(morie_pinv(S)))
  A <- matrix(0, p, p)
  for (i in seq_len(p)) {
    A[i, i] <- sqrt(rchisq(1, nu - i + 1))
    if (i > 1) A[i, seq_len(i - 1)] <- rnorm(i - 1)
  }
  LA <- L %*% A
  morie_pinv(LA %*% t(LA))
}

#' @noRd
morie_multitrait_ridge <- function(Z1, G) {
  L <- morie_chol_lower(G)
  list(X1 = as.matrix(Z1) %*% L, L_G = L)
}

#' @noRd
morie_bmtme_conditionals <- function(Y, Z1, Z2, G, Sigma_T,
                                     Sigma_E, R, b1 = NULL,
                                     b2 = NULL, nu_T = NULL,
                                     S_T = NULL, nu_E = NULL,
                                     S_E = NULL) {
  Y <- as.matrix(Y)
  nT <- ncol(Y)
  G <- as.matrix(G)
  J <- nrow(G)
  Ginv <- morie_pinv(G)
  q2 <- ncol(as.matrix(Z2))
  if (is.null(b1)) b1 <- matrix(0, J, nT)
  if (is.null(b2)) b2 <- matrix(0, q2, nT)
  b1 <- as.matrix(b1)
  b2 <- as.matrix(b2)
  SEinv <- morie_pinv(Sigma_E)
  I <- nrow(SEinv)
  STinv <- morie_pinv(Sigma_T)
  if (is.null(nu_T)) nu_T <- nT + 2
  if (is.null(nu_E)) nu_E <- I + 2
  if (is.null(S_T)) S_T <- diag(nT)
  if (is.null(S_E)) S_E <- diag(I)
  term1 <- t(b1) %*% Ginv %*% b1
  term2 <- t(b2) %*% morie_kron(SEinv, Ginv) %*% b2
  scale_T <- term1 + term2 + S_T
  b2s <- matrix(0, I, J * nT)
  for (e in seq_len(I)) {
    idx <- 0L
    for (t in seq_len(nT)) {
      for (a in seq_len(J)) {
        idx <- idx + 1L
        row <- (e - 1L) * J + a
        b2s[e, idx] <- if (row <= nrow(b2)) b2[row, t] else 0
      }
    }
  }
  inner <- b2s %*% morie_kron(Ginv, STinv) %*% t(b2s)
  list(
    nu_T_post = nu_T + J + nrow(b2), scale_T = scale_T,
    nu_E_post = nu_E + J * I, scale_E = inner + S_E
  )
}

# ---- chapter 7: ordinal / categorical models (pp.209-215) ----

#' @noRd
morie_ordinal_probs <- function(eta, thresholds,
                                link = "probit") {
  F <- if (link == "probit") pnorm else plogis
  t(vapply(as.numeric(eta), function(e) {
    cuts <- c(0, F(thresholds + e), 1)
    diff(cuts)
  }, numeric(length(thresholds) + 1L)))
}

#' @noRd
morie_rtruncnorm <- function(mean, sd, lo, hi) {
  a <- if (is.finite(lo)) pnorm((lo - mean) / sd) else 0
  b <- if (is.finite(hi)) pnorm((hi - mean) / sd) else 1
  if (b <= a) {
    return(mean)
  }
  u <- min(max(a + (b - a) * runif(1), 1e-12), 1 - 1e-12)
  mean + sd * qnorm(u)
}

#' @noRd
morie_ordinal_probit_gibbs <- function(y, X, n_iter = 1500L,
                                       burn_in = 400L,
                                       nu_beta = 5,
                                       S_beta = 1,
                                       seed = 42L) {
  set.seed(seed)
  y <- as.integer(y)
  X <- as.matrix(X)
  n <- length(y)
  p <- ncol(X)
  C <- max(y)
  beta <- rep(0, p)
  s2b <- 1
  gamma <- seq_len(C - 1L) - C / 2
  l <- rep(0, n)
  col_ss <- colSums(X^2)
  acc_beta <- rep(0, p)
  acc_gamma <- rep(0, C - 1L)
  acc_s2b <- 0
  kept <- 0L
  for (it in seq_len(n_iter)) {
    eta <- as.numeric(X %*% beta)
    for (i in seq_len(n)) {
      c_ <- y[i]
      lo <- if (c_ >= 2) gamma[c_ - 1L] else -Inf
      hi <- if (c_ <= C - 1L) gamma[c_] else Inf
      l[i] <- morie_rtruncnorm(-eta[i], 1, lo, hi)
    }
    for (j in seq_len(p)) {
      others <- if (p > 1) X[, -j, drop = FALSE] %*% beta[-j] else 0
      e_j <- l + others
      v <- 1 / (1 / s2b + col_ss[j])
      m <- -v * sum(X[, j] * e_j)
      beta[j] <- m + sqrt(v) * rnorm(1)
    }
    for (c_ in seq_len(C - 1L)) {
      a_c <- suppressWarnings(max(l[y == c_]))
      b_c <- suppressWarnings(min(l[y == c_ + 1L]))
      lo <- max(a_c, if (c_ >= 2) gamma[c_ - 1L] else -Inf)
      hi <- min(b_c, if (c_ <= C - 2L) gamma[c_ + 1L] else Inf)
      if (is.finite(lo) && is.finite(hi) && hi > lo) {
        gamma[c_] <- lo + (hi - lo) * runif(1)
      }
    }
    s2b <- morie_scaled_inv_chisq(
      nu_beta + p,
      S_beta + sum(beta^2)
    )
    if (it > burn_in) {
      kept <- kept + 1L
      acc_beta <- acc_beta + beta
      acc_gamma <- acc_gamma + gamma
      acc_s2b <- acc_s2b + s2b
    }
  }
  list(
    beta = acc_beta / kept, gamma = acc_gamma / kept,
    sigma2_beta = acc_s2b / kept, n_kept = kept,
    n_categories = C
  )
}

# ---- chapter 7b: multinomial and Poisson (pp.225-233) ----

#' @noRd
morie_multinomial_probs <- function(X, beta0, beta,
                                    baseline_last = TRUE) {
  X <- as.matrix(X)
  B <- as.matrix(beta)
  b0 <- as.numeric(beta0)
  if (baseline_last) {
    b0 <- c(b0, 0)
    B <- rbind(B, rep(0, ncol(X)))
  }
  eta <- sweep(X %*% t(B), 2, b0, "+")
  m <- apply(eta, 1, max)
  ex <- exp(eta - m)
  ex / rowSums(ex)
}

#' @noRd
morie_multinomial_loglik <- function(X, y, beta0, beta,
                                     baseline_last = TRUE) {
  P <- morie_multinomial_probs(X, beta0, beta, baseline_last)
  sum(log(pmax(P[cbind(seq_along(y), as.integer(y) + 1L)], 1e-300)))
}

#' @noRd
morie_penalized_multinomial <- function(X, y, beta0, beta,
                                        lambda,
                                        penalty = "ridge",
                                        baseline_last = TRUE) {
  ll <- morie_multinomial_loglik(
    X, y, beta0, beta,
    baseline_last
  )
  pen <- if (penalty == "lasso") {
    sum(abs(as.matrix(beta)))
  } else {
    sum(as.matrix(beta)^2)
  }
  list(
    loglik = ll, penalty = lambda * pen,
    penalized_loglik = ll - lambda * pen
  )
}

#' @noRd
morie_multinomial_block <- function(X, y, beta0, beta, lambda,
                                    cls,
                                    baseline_last = TRUE) {
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  P <- morie_multinomial_probs(X, beta0, beta, baseline_last)
  cc <- cls + 1L
  Xs <- cbind(1, X)
  b_cur <- c(as.numeric(beta0)[cc], as.matrix(beta)[cc, ])
  pc <- pmin(pmax(P[, cc], 1e-6), 1 - 1e-6)
  w <- pc * (1 - pc)
  eta <- as.numeric(Xs %*% b_cur)
  ystar <- eta + ((as.integer(y) == cls) - pc) / w
  D <- diag(p + 1)
  D[1, 1] <- 0
  A <- t(Xs) %*% (Xs * w) + lambda * D
  sol <- as.numeric(morie_solve(A, t(Xs) %*% (w * ystar)))
  list(
    beta0 = sol[1], beta = sol[-1], weights = w,
    working_response = ystar
  )
}

#' @noRd
morie_poisson_pmf <- function(y, lambda) {
  exp(y * log(lambda) - lambda - lgamma(y + 1))
}

#' @noRd
morie_penalized_poisson <- function(X, y, lambda = 1,
                                    penalty = "ridge",
                                    n_iter = 100L,
                                    tol = 1e-10,
                                    add_intercept = TRUE) {
  X <- as.matrix(X)
  if (add_intercept) X <- cbind(1, X)
  y <- as.numeric(y)
  n <- length(y)
  p <- ncol(X)
  beta <- rep(0, p)
  if (add_intercept) beta[1] <- log(max(mean(y), 1e-6))
  D <- diag(p)
  if (add_intercept) D[1, 1] <- 0
  it <- 0L
  for (i in seq_len(n_iter)) {
    it <- i
    eta <- as.numeric(X %*% beta)
    mu <- exp(pmin(eta, 700))
    w <- pmax(mu, 1e-9)
    z <- eta + (y - mu) / w
    A <- t(X) %*% (X * w) + lambda * D
    new <- as.numeric(morie_solve(A, t(X) %*% (w * z)))
    if (penalty == "lasso") {
      idx <- if (add_intercept) -1L else seq_along(new)
      new[idx] <- sign(new[idx]) * pmax(abs(new[idx]) - lambda, 0)
    }
    gap <- max(abs(new - beta))
    beta <- new
    if (gap < tol) break
  }
  eta <- as.numeric(X %*% beta)
  mu <- exp(pmin(eta, 700))
  ll <- sum(y * eta - mu - lgamma(y + 1))
  pen <- lambda * sum((beta^2)[diag(D) == 1]) / 2
  list(
    beta = beta, fitted = mu, loglik = ll,
    penalized_loglik = ll - pen, iterations = it
  )
}

# ---- chapter 8: RKHS regression and kernels (pp.252-266) ----

#' @noRd
morie_kernel_matrix <- function(X, kernel = "linear",
                                gamma = NULL, degree = 2,
                                coef0 = 1, Z = NULL) {
  A <- as.matrix(X)
  B <- if (is.null(Z)) A else as.matrix(Z)
  if (is.null(gamma)) gamma <- 1 / ncol(A)
  G <- A %*% t(B)
  if (kernel == "linear") {
    return(G)
  }
  if (kernel == "polynomial") {
    return((gamma * G + coef0)^degree)
  }
  if (kernel == "sigmoid") {
    return(tanh(gamma * G + coef0))
  }
  d2 <- outer(rowSums(A^2), rowSums(B^2), "+") - 2 * G
  d2[d2 < 0] <- 0
  if (kernel == "gaussian") {
    return(exp(-gamma * d2))
  }
  if (kernel == "exponential") {
    return(exp(-gamma * sqrt(d2)))
  }
  stop("unknown kernel: ", kernel)
}

#' @noRd
morie_is_psd <- function(K, tol = 1e-9) {
  S <- (as.matrix(K) + t(as.matrix(K))) / 2
  lam <- eigen(S, symmetric = TRUE, only.values = TRUE)$values
  list(psd = min(lam) >= -tol, eigenvalues = lam)
}

#' @noRd
morie_rkhs_norm <- function(beta, K) {
  b <- as.numeric(beta)
  as.numeric(t(b) %*% as.matrix(K) %*% b)
}

#' @noRd
morie_rkhs_predict <- function(K_new, beta, eta0 = 0) {
  as.numeric(eta0 + as.matrix(K_new) %*% as.numeric(beta))
}

#' @noRd
morie_rkhs_fit <- function(K, y, lambda = 1) {
  K <- as.matrix(K)
  y <- as.numeric(y)
  n <- length(y)
  A <- matrix(0, n + 1, n + 1)
  rhs <- numeric(n + 1)
  A[1, 1] <- 1
  A[1, -1] <- colSums(K) / n
  rhs[1] <- mean(y)
  KtK <- t(K) %*% K
  A[-1, 1] <- colSums(K) * 2 / n
  A[-1, -1] <- 2 * KtK / n + lambda * K
  rhs[-1] <- 2 * as.numeric(t(K) %*% y) / n
  sol <- as.numeric(morie_solve(A, rhs))
  eta0 <- sol[1]
  beta <- sol[-1]
  fitted <- morie_rkhs_predict(K, beta, eta0)
  resid <- y - fitted
  loss <- mean(resid^2)
  pen <- 0.5 * lambda * morie_rkhs_norm(beta, K)
  list(
    eta0 = eta0, beta = beta, fitted = fitted,
    residuals = resid, loss = loss, penalty = pen,
    objective = loss + pen
  )
}

#' @noRd
morie_arccos_kernel <- function(X, Z = NULL, depth = 1L,
                                normalize_median = FALSE) {
  A <- as.matrix(X)
  same <- is.null(Z)
  B <- if (same) A else as.matrix(Z)
  na <- sqrt(rowSums(A^2))
  nb <- sqrt(rowSums(B^2))
  Jf <- function(th) sin(th) + (pi - th) * cos(th)
  cosang <- (A %*% t(B)) / outer(na, nb)
  cosang[cosang > 1] <- 1
  cosang[cosang < -1] <- -1
  K <- outer(na, nb) * Jf(acos(cosang)) / pi
  dA <- if (same) diag(K) else na^2
  dB <- if (same) dA else nb^2
  if (depth > 1L) {
    for (l in seq_len(depth - 1L)) {
      den <- sqrt(outer(dA, dB))
      cs <- K / den
      cs[cs > 1] <- 1
      cs[cs < -1] <- -1
      K <- den * Jf(acos(cs)) / pi
      dA <- dA * Jf(0) / pi
      dB <- dB * Jf(0) / pi
    }
  }
  if (normalize_median) K <- K / median(K)
  K
}

# ---- chapter 8c: Bayesian kernel BLUP (pp.281-285) ----

#' @noRd
morie_hadamard <- function(A, B) as.matrix(A) * as.matrix(B)

#' @noRd
morie_bayesian_kernel_blup <- function(y, K, sigma2_u = 1,
                                       sigma2_e = 1,
                                       mu = NULL) {
  y <- as.numeric(y)
  K <- as.matrix(K)
  n <- length(y)
  if (is.null(mu)) mu <- mean(y)
  Kinv <- morie_pinv(K)
  A <- Kinv / sigma2_u + diag(1 / sigma2_e, n)
  Kt <- morie_pinv(A)
  u <- as.numeric(Kt %*% (y - mu)) / sigma2_e
  list(
    mu = mu, u = u, K_tilde = Kt,
    sigma2_u = sigma2_u, sigma2_e = sigma2_e
  )
}

#' @noRd
morie_kernel_blup_replicated <- function(Z, K,
                                         sigma2_u = 1) {
  Z <- as.matrix(Z)
  sigma2_u * (Z %*% as.matrix(K) %*% t(Z))
}

#' @noRd
morie_kernel_blup_gxe <- function(Z_u1, K, Z_E,
                                  sigma2_u1 = 1,
                                  sigma2_u2 = 1) {
  Zu <- as.matrix(Z_u1)
  ZE <- as.matrix(Z_E)
  K1 <- Zu %*% as.matrix(K) %*% t(Zu)
  KE <- ZE %*% t(ZE)
  list(
    K1 = sigma2_u1 * K1,
    K2 = sigma2_u2 * morie_hadamard(K1, KE),
    K_env = KE
  )
}

# ---- chapter 8d/8e: kernel compression + RKHS equations ----

#' @noRd
morie_kernel_eigen_design <- function(K, tol = 1e-10) {
  S <- (as.matrix(K) + t(as.matrix(K))) / 2
  e <- eigen(S, symmetric = TRUE)
  keep <- which(e$values > tol)
  list(
    P = e$vectors[, keep, drop = FALSE] %*%
      diag(sqrt(e$values[keep]), length(keep)),
    rank = length(keep), eigenvalues = e$values[keep]
  )
}

#' @noRd
morie_nystrom <- function(X, m_index, kernel = "linear",
                          gamma = NULL) {
  A <- as.matrix(X)
  p <- ncol(A)
  Xm <- A[m_index, , drop = FALSE]
  if (kernel == "linear") {
    Kmm <- Xm %*% t(Xm) / p
    Knm <- A %*% t(Xm) / p
  } else {
    Kmm <- morie_kernel_matrix(Xm, kernel, gamma)
    Knm <- morie_kernel_matrix(A, kernel, gamma, Z = Xm)
  }
  list(
    Q = Knm %*% morie_pinv(Kmm) %*% t(Knm),
    K_mm = Kmm, K_nm = Knm, rank = length(m_index)
  )
}

#' @noRd
morie_sparse_kernel_design <- function(X, m_index,
                                       kernel = "linear",
                                       gamma = NULL,
                                       tol = 1e-10) {
  ny <- morie_nystrom(X, m_index, kernel, gamma)
  e <- eigen((ny$K_mm + t(ny$K_mm)) / 2, symmetric = TRUE)
  keep <- which(e$values > tol)
  US <- e$vectors[, keep, drop = FALSE] %*%
    diag(1 / sqrt(e$values[keep]), length(keep))
  list(
    P = ny$K_nm %*% US, Q = ny$Q, rank = length(keep),
    K_mm = ny$K_mm, K_nm = ny$K_nm
  )
}

#' @noRd
morie_rkhs_mixed_equations <- function(C, K, y, lambda = 1,
                                       sigma2_e = 1,
                                       form = "direct") {
  C <- as.matrix(C)
  K <- as.matrix(K)
  y <- as.numeric(y)
  n <- length(y)
  q <- ncol(C)
  if (form == "direct") {
    A <- rbind(
      cbind(t(C) %*% C, t(C) %*% K),
      cbind(
        t(K) %*% C,
        t(K) %*% K + lambda * K * sigma2_e
      )
    )
    rhs <- c(t(C) %*% y, t(K) %*% y)
  } else {
    A <- rbind(
      cbind(t(C) %*% C, t(C) %*% K),
      cbind(C, K + diag(lambda * sigma2_e, n))
    )
    rhs <- c(t(C) %*% y, y)
  }
  sol <- as.numeric(morie_solve(A, rhs))
  theta <- sol[seq_len(q)]
  beta <- sol[-seq_len(q)]
  u <- as.numeric(K %*% beta)
  list(
    theta = theta, beta = beta, u = u,
    fitted = as.numeric(C %*% theta) + u,
    sigma2_beta = 1 / lambda
  )
}

# ---- chapter 9: support vector machines (pp.339-350) ----

#' @noRd
morie_svm_label_matrix <- function(X, y) as.matrix(X) * as.numeric(y)

#' @noRd
morie_svm_decision <- function(X, beta0, beta) {
  as.numeric(beta0 + as.matrix(X) %*% as.numeric(beta))
}

#' @noRd
morie_svm_dual_objective <- function(alpha, X, y, K = NULL) {
  a <- as.numeric(alpha)
  ys <- as.numeric(y)
  G <- if (is.null(K)) as.matrix(X) %*% t(as.matrix(X)) else as.matrix(K)
  sum(a) - 0.5 * sum(outer(a * ys, a * ys) * G)
}

#' @noRd
morie_svm_beta <- function(alpha, X, y) {
  as.numeric(t(as.matrix(X)) %*% (as.numeric(alpha) * as.numeric(y)))
}

#' @noRd
morie_svm_intercept <- function(alpha, X, y, K = NULL,
                                tol = 1e-8) {
  a <- as.numeric(alpha)
  ys <- as.numeric(y)
  G <- if (is.null(K)) as.matrix(X) %*% t(as.matrix(X)) else as.matrix(K)
  S <- which(a > tol)
  if (!length(S)) {
    return(0)
  }
  mean(vapply(S, function(i) ys[i] - sum(a[S] * ys[S] * G[i, S]), 0))
}

#' @noRd
morie_svm_fit_dual <- function(X, y, C = NULL, n_iter = 4000L,
                               tol = 1e-9, K = NULL) {
  X <- as.matrix(X)
  ys <- as.numeric(y)
  n <- length(ys)
  G <- if (is.null(K)) X %*% t(X) else as.matrix(K)
  H <- outer(ys, ys) * G
  step <- 1 / (n * max(abs(diag(H))))
  a <- rep(0, n)
  yy <- sum(ys^2)
  for (it in seq_len(n_iter)) {
    g <- 1 - as.numeric(H %*% a)
    g <- g - sum(g * ys) / yy * ys # project onto g . y = 0
    new <- a + step * g
    new <- pmax(0, if (is.null(C)) new else pmin(new, C))
    if (max(abs(new - a)) < tol) {
      a <- new
      break
    }
    a <- new
  }
  list(
    alpha = a, beta = morie_svm_beta(a, X, ys),
    beta0 = morie_svm_intercept(a, X, ys, K),
    support_vectors = which(a > 1e-6),
    objective = morie_svm_dual_objective(a, X, ys, K)
  )
}

# ---- chapter 10: ANN and backpropagation (pp.385, 409-412) ----

#' @noRd
morie_act <- function(name, z, deriv = FALSE) {
  if (name == "identity") {
    return(if (deriv) rep(1, length(z)) else z)
  }
  if (name == "logistic") {
    s <- 1 / (1 + exp(-pmax(pmin(z, 700), -700)))
    return(if (deriv) s * (1 - s) else s)
  }
  if (name == "tanh") {
    t <- tanh(z)
    return(if (deriv) 1 - t^2 else t)
  }
  if (name == "relu") {
    return(if (deriv) as.numeric(z > 0) else pmax(0, z))
  }
  stop("unknown activation: ", name)
}

#' @noRd
morie_ann_forward <- function(X, W, activations = NULL) {
  A <- as.matrix(X)
  acts <- if (is.null(activations)) {
    c(rep("logistic", length(W) - 1), "identity")
  } else {
    activations
  }
  layers <- list(A)
  nets <- list()
  for (li in seq_along(W)) {
    z <- layers[[li]] %*% t(as.matrix(W[[li]]))
    nets[[li]] <- z
    layers[[li + 1]] <- matrix(morie_act(acts[li], as.numeric(z)),
      nrow = nrow(z)
    )
  }
  list(
    output = layers[[length(layers)]], layers = layers,
    nets = nets, activations = acts
  )
}

#' @noRd
morie_ann_sse <- function(y_hat, y) {
  0.5 * sum((as.matrix(y_hat) - as.matrix(y))^2)
}

#' @noRd
morie_ann_gradients <- function(X, y, W, activations = NULL) {
  f <- morie_ann_forward(X, W, activations)
  Y <- as.matrix(y)
  L <- length(W)
  acts <- f$activations
  d <- (f$layers[[L + 1]] - Y) *
    matrix(morie_act(acts[L], as.numeric(f$nets[[L]]), TRUE),
      nrow = nrow(Y)
    )
  grads <- vector("list", L)
  for (li in seq(L, 1)) {
    grads[[li]] <- t(d) %*% f$layers[[li]]
    if (li > 1) {
      d <- (d %*% as.matrix(W[[li]])) *
        matrix(
          morie_act(
            acts[li - 1],
            as.numeric(f$nets[[li - 1]]), TRUE
          ),
          nrow = nrow(Y)
        )
    }
  }
  list(gradients = grads, loss = morie_ann_sse(f$layers[[L + 1]], Y))
}

#' @noRd
morie_ann_train <- function(X, y, W, eta = 0.1, n_iter = 500L,
                            activations = NULL, tol = 1e-12) {
  Wc <- lapply(W, as.matrix)
  hist <- numeric(0)
  for (it in seq_len(n_iter)) {
    g <- morie_ann_gradients(X, y, Wc, activations)
    hist <- c(hist, g$loss)
    for (li in seq_along(Wc)) Wc[[li]] <- Wc[[li]] - eta * g$gradients[[li]]
    if (length(hist) > 1 && abs(diff(tail(hist, 2))) < tol) break
  }
  f <- morie_ann_gradients(X, y, Wc, activations)
  list(W = Wc, loss = f$loss, history = hist, iterations = length(hist))
}

#' @noRd
morie_ann_numeric_gradient <- function(X, y, W,
                                       activations = NULL,
                                       eps = 1e-6) {
  lapply(seq_along(W), function(li) {
    Wm <- as.matrix(W[[li]])
    G <- matrix(0, nrow(Wm), ncol(Wm))
    for (u in seq_len(nrow(Wm))) {
      for (v in seq_len(ncol(Wm))) {
        acc <- 0
        for (s in c(1, -1)) {
          Wp <- lapply(W, as.matrix)
          Wp[[li]][u, v] <- Wp[[li]][u, v] + s * eps
          acc <- acc + s * morie_ann_sse(
            morie_ann_forward(X, Wp, activations)$output, y
          )
        }
        G[u, v] <- acc / (2 * eps)
      }
    }
    G
  })
}
