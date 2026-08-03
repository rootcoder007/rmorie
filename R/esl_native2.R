# SPDX-License-Identifier: AGPL-3.0-or-later

## R parity for the morie.fn ESL shelf, part 2: kernel methods, regularisation
## paths, manifold learning, prototype methods and probabilistic models.
## Ported at full precision from the Python modules of the same name.

## ---------------------------------------------------------------------------
## Shared SMO solver -- written once so a sign error cannot live in one
## kernel's copy and not another's.
## ---------------------------------------------------------------------------

.morie_kernel_matrix <- function(X, Z = NULL, kernel = "rbf", gamma = NULL,
                                 degree = 3, coef0 = 1) {
  X <- as.matrix(X)
  Z <- if (is.null(Z)) X else as.matrix(Z)
  if (ncol(X) != ncol(Z)) {
    stop(sprintf("X has %d columns but Z has %d", ncol(X), ncol(Z)), call. = FALSE)
  }
  if (kernel == "linear") return(tcrossprod(X, Z))
  if (kernel == "poly")   return((tcrossprod(X, Z) + coef0)^degree)
  if (kernel == "rbf") {
    if (is.null(gamma)) gamma <- 1 / ncol(X)
    d2 <- outer(rowSums(X^2), rowSums(Z^2), "+") - 2 * tcrossprod(X, Z)
    return(exp(-gamma * pmax(d2, 0)))
  }
  if (kernel == "sigmoid") {
    if (is.null(gamma)) gamma <- 1 / ncol(X)
    return(tanh(gamma * tcrossprod(X, Z) + coef0))
  }
  stop(sprintf("unknown kernel '%s'", kernel), call. = FALSE)
}

.morie_smo <- function(K, y, C = 1, tol = 1e-3, max_passes = 50L,
                       max_iter = 10000L, seed = 0L) {
  y <- as.numeric(y); n <- length(y)
  alpha <- numeric(n); b <- 0
  set.seed(seed)
  passes <- 0L; it <- 0L
  while (passes < max_passes && it < max_iter) {
    changed <- 0L
    for (i in seq_len(n)) {
      it <- it + 1L
      Ei <- sum(K[i, ] * alpha * y) + b - y[i]
      if ((y[i] * Ei < -tol && alpha[i] < C) || (y[i] * Ei > tol && alpha[i] > 0)) {
        j <- sample.int(n - 1L, 1L)
        if (j >= i) j <- j + 1L
        Ej <- sum(K[j, ] * alpha * y) + b - y[j]
        ai_old <- alpha[i]; aj_old <- alpha[j]
        if (y[i] != y[j]) {
          L <- max(0, aj_old - ai_old); Hi <- min(C, C + aj_old - ai_old)
        } else {
          L <- max(0, ai_old + aj_old - C); Hi <- min(C, ai_old + aj_old)
        }
        if (L >= Hi) next
        eta <- 2 * K[i, j] - K[i, i] - K[j, j]
        if (eta >= 0) next
        alpha[j] <- min(max(aj_old - y[j] * (Ei - Ej) / eta, L), Hi)
        if (abs(alpha[j] - aj_old) < 1e-12) { alpha[j] <- aj_old; next }
        alpha[i] <- ai_old + y[i] * y[j] * (aj_old - alpha[j])
        b1 <- b - Ei - y[i] * (alpha[i] - ai_old) * K[i, i] -
          y[j] * (alpha[j] - aj_old) * K[i, j]
        b2 <- b - Ej - y[i] * (alpha[i] - ai_old) * K[i, j] -
          y[j] * (alpha[j] - aj_old) * K[j, j]
        b <- if (alpha[i] > 0 && alpha[i] < C) b1 else
          if (alpha[j] > 0 && alpha[j] < C) b2 else (b1 + b2) / 2
        changed <- changed + 1L
      }
    }
    passes <- if (changed == 0L) passes + 1L else 0L
  }
  list(alpha = alpha, b = b, n_iter = it, converged = passes >= max_passes)
}

#' Kernel support vector machine
#'
#' Fits the dual soft-margin SVM by sequential minimal optimisation. Only the
#' kernel enters, never the feature map. Observations with `alpha = 0` do not
#' appear in the decision function at all, which is what makes the solution
#' sparse in the training set; small `C` gives a wide margin and many support
#' vectors, large `C` a narrow one and few.
#'
#' @param X Training predictors, n by p.
#' @param y Labels; any two distinct values, mapped internally to -1/+1.
#' @param C Box constraint, positive.
#' @param kernel "rbf", "linear", "poly" or "sigmoid".
#' @param gamma Kernel width for rbf/sigmoid; defaults to `1/p`.
#' @param degree,coef0 Polynomial and sigmoid kernel parameters.
#' @param newdata Points to classify; defaults to `X`.
#' @param tol,max_passes,seed SMO controls.
#' @return List with `alpha`, `b`, `support` (1-based indices), `decision`,
#'   `class`, `accuracy`, and `dual_gap_check` (which the equality constraint
#'   forces to zero).
#' @references Platt, J. (1998). Sequential minimal optimization. MSR-TR-98-14.
#'   Hastie, T., et al. (2009). ESL (2nd ed.), Sec 12.3. Springer.
#' @examples
#' set.seed(1)
#' X <- rbind(matrix(rnorm(120, -2), ncol = 2), matrix(rnorm(120, 2), ncol = 2))
#' y <- rep(c(-1, 1), each = 60)
#' fit <- morie_esl_svm_kernel(X, y, C = 1, kernel = "rbf", seed = 1L)
#' abs(fit$dual_gap_check) < 1e-9
#' @export
morie_esl_svm_kernel <- function(X, y, C = 1, kernel = "rbf", gamma = NULL,
                                 degree = 3, coef0 = 1, newdata = NULL,
                                 tol = 1e-3, max_passes = 50L, seed = 0L) {
  if (C <= 0) stop("C must be positive", call. = FALSE)
  X <- as.matrix(X); yr <- as.vector(y)
  if (nrow(X) != length(yr)) {
    stop(sprintf("X has %d rows but y has %d", nrow(X), length(yr)), call. = FALSE)
  }
  classes <- sort(unique(yr))
  if (length(classes) != 2L) {
    stop(sprintf("y must have exactly 2 classes, found %d", length(classes)),
         call. = FALSE)
  }
  ypm <- ifelse(yr == classes[2L], 1, -1)
  K <- .morie_kernel_matrix(X, kernel = kernel, gamma = gamma,
                            degree = degree, coef0 = coef0)
  fit <- .morie_smo(K, ypm, C = C, tol = tol, max_passes = max_passes, seed = seed)
  alpha <- fit$alpha; b <- fit$b
  Z <- if (is.null(newdata)) X else as.matrix(newdata)
  dec <- as.numeric(.morie_kernel_matrix(Z, X, kernel = kernel, gamma = gamma,
                                         degree = degree, coef0 = coef0) %*%
                      (alpha * ypm)) + b
  train_dec <- as.numeric(K %*% (alpha * ypm)) + b
  list(alpha = alpha, b = b, support = which(alpha > 1e-8),
       n_support = sum(alpha > 1e-8), decision = dec,
       class = ifelse(dec >= 0, classes[2L], classes[1L]),
       accuracy = mean(sign(train_dec) == ypm),
       dual_gap_check = sum(alpha * ypm), kernel = kernel, C = C,
       classes = classes, n_iter = fit$n_iter, converged = fit$converged,
       method = "esl_svm_kernel")
}

#' Linear support vector classifier
#'
#' Soft-margin linear SVC solved through its dual, with the primal weights
#' recovered as `w = sum(alpha * y * x)`. Unlike the kernel form the weight
#' vector is explicit, so the rule is interpretable coefficient by
#' coefficient. The margin width `2 / ||w||` is reported, and widens as `C`
#' falls.
#'
#' @param X Predictors, n by p.
#' @param y Labels; any two distinct values.
#' @param C Cost of margin violations, positive.
#' @param newdata Points to classify; defaults to `X`.
#' @param tol,max_passes,seed SMO controls.
#' @return List with `w`, `b`, `margin`, `alpha`, `support`, `decision`,
#'   `class`, `accuracy`, `slack`, `n_violations`.
#' @references Hastie, T., et al. (2009). ESL (2nd ed.), Sec 12.2. Springer.
#' @examples
#' set.seed(1)
#' X <- rbind(matrix(rnorm(100, -3, 0.5), ncol = 2),
#'            matrix(rnorm(100, 3, 0.5), ncol = 2))
#' y <- rep(c(-1, 1), each = 50)
#' morie_esl_svc(X, y, C = 1, seed = 1L)$accuracy
#' @export
morie_esl_svc <- function(X, y, C = 1, newdata = NULL, tol = 1e-3,
                          max_passes = 50L, seed = 0L) {
  if (C <= 0) stop("C must be positive", call. = FALSE)
  X <- as.matrix(X); yr <- as.vector(y)
  if (nrow(X) != length(yr)) {
    stop(sprintf("X has %d rows but y has %d", nrow(X), length(yr)), call. = FALSE)
  }
  classes <- sort(unique(yr))
  if (length(classes) != 2L) {
    stop(sprintf("y must have exactly 2 classes, found %d", length(classes)),
         call. = FALSE)
  }
  ypm <- ifelse(yr == classes[2L], 1, -1)
  K <- .morie_kernel_matrix(X, kernel = "linear")
  fit <- .morie_smo(K, ypm, C = C, tol = tol, max_passes = max_passes, seed = seed)
  alpha <- fit$alpha; b <- fit$b
  w <- as.numeric(crossprod(X, alpha * ypm))
  wn <- sqrt(sum(w^2))
  Z <- if (is.null(newdata)) X else as.matrix(newdata)
  if (ncol(Z) != ncol(X)) {
    stop(sprintf("newdata has %d columns but X has %d", ncol(Z), ncol(X)),
         call. = FALSE)
  }
  dec <- as.numeric(Z %*% w) + b
  train_dec <- as.numeric(X %*% w) + b
  slack <- pmax(0, 1 - ypm * train_dec)
  list(w = w, b = b, margin = if (wn > 0) 2 / wn else Inf, w_norm = wn,
       alpha = alpha, support = which(alpha > 1e-8), decision = dec,
       class = ifelse(dec >= 0, classes[2L], classes[1L]),
       accuracy = mean(sign(train_dec) == ypm), slack = slack,
       n_violations = sum(slack > 1e-8), classes = classes, C = C,
       n_iter = fit$n_iter, converged = fit$converged, method = "esl_svc")
}

#' Least angle regression
#'
#' Computes the LAR coefficient path. Every active predictor keeps exactly the
#' same absolute correlation with the residual throughout -- the equiangularity
#' that distinguishes LAR from forward stepwise, which takes the full
#' least-squares step and destroys it. This is LAR proper, without the lasso
#' modification, so coefficients never leave the active set.
#'
#' @param X Predictors, n by p.
#' @param y Response.
#' @param max_steps Number of LAR steps; defaults to `min(p, n - 1)`.
#' @param standardize Scale columns to unit norm first. Correlations are not
#'   comparable across differently-scaled predictors, so turning this off makes
#'   the entry order depend on the units.
#' @return List with `coef_path` (in original units), `coef`, `intercept`,
#'   `active` (1-based entry order), `correlations`, `r_squared`.
#' @references Efron, B., Hastie, T., Johnstone, I., & Tibshirani, R. (2004).
#'   Least angle regression. Annals of Statistics 32(2), 407-499.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(600), ncol = 5)
#' y <- 5 * X[, 3] - 2 * X[, 1] + rnorm(120, sd = 0.1)
#' morie_esl_least_angle_reg(X, y)$active[1:2]
#' @export
morie_esl_least_angle_reg <- function(X, y, max_steps = NULL,
                                      standardize = TRUE) {
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); p <- ncol(X)
  if (n != length(y)) stop(sprintf("X has %d rows but y has %d", n, length(y)),
                           call. = FALSE)
  cap <- min(p, n - 1L)
  max_steps <- if (is.null(max_steps)) cap else as.integer(max_steps)
  if (max_steps < 1L || max_steps > cap) {
    stop(sprintf("max_steps must be between 1 and %d", cap), call. = FALSE)
  }
  xbar <- colMeans(X); Xc <- sweep(X, 2L, xbar, "-")
  scale <- if (standardize) sqrt(colSums(Xc^2)) else rep(1, p)
  scale[scale <= 0] <- 1
  Xs <- sweep(Xc, 2L, scale, "/")
  ybar <- mean(y); yc <- y - ybar

  beta <- numeric(p); mu <- numeric(n)
  active <- integer(0)
  path <- list(beta); cors <- list(as.numeric(crossprod(Xs, yc)))

  for (step in seq_len(max_steps)) {
    cc <- as.numeric(crossprod(Xs, yc - mu))
    Cmax <- max(abs(cc))
    if (Cmax < 1e-12) break
    for (j in which(abs(abs(cc) - Cmax) < 1e-10)) {
      if (!(j %in% active)) active <- c(active, j)
    }
    A <- active
    s <- sign(cc[A])
    XA <- sweep(Xs[, A, drop = FALSE], 2L, s, "*")
    G <- crossprod(XA)
    Ginv1 <- tryCatch(solve(G, rep(1, length(A))),
                      error = function(e) qr.solve(G, rep(1, length(A))))
    AA <- 1 / sqrt(sum(Ginv1))
    w <- AA * Ginv1
    u <- as.numeric(XA %*% w)
    a <- as.numeric(crossprod(Xs, u))
    inactive <- setdiff(seq_len(p), A)
    if (length(inactive) == 0L) {
      gamma <- Cmax / AA
    } else {
      cand <- c((Cmax - cc[inactive]) / (AA - a[inactive]),
                (Cmax + cc[inactive]) / (AA + a[inactive]))
      cand <- cand[is.finite(cand) & cand > 1e-12]
      gamma <- if (length(cand)) min(cand) else Cmax / AA
    }
    mu <- mu + gamma * u
    beta[A] <- beta[A] + gamma * w * s
    path[[length(path) + 1L]] <- beta
    cors[[length(cors) + 1L]] <- as.numeric(crossprod(Xs, yc - mu))
  }
  coef <- sweep(do.call(rbind, path), 2L, scale, "/")
  last <- coef[nrow(coef), ]
  fitted <- as.numeric(X %*% last) + (ybar - sum(xbar * last))
  ss_tot <- sum((y - ybar)^2)
  list(coef_path = coef, coef = last,
       intercept = ybar - sum(xbar * last), active = active,
       correlations = do.call(rbind, cors), fitted = fitted,
       r_squared = if (ss_tot > 0) 1 - sum((y - fitted)^2) / ss_tot else NA_real_,
       n_steps = nrow(coef) - 1L, method = "esl_least_angle_reg")
}

#' Sparse principal components
#'
#' L1-penalised alternating maximisation: soft-threshold `Sigma %*% v`, then
#' renormalise, with later components on the deflated covariance. Ordinary PCA
#' loadings are almost never zero, so every component mixes all p variables;
#' the L1 penalty zeroes loadings outright. The cost, which must be stated:
#' sparse components are NOT orthogonal, so their variances do not sum to the
#' total and "percent variance explained" is not additive. `adjusted_variance`
#' applies the Zou-Hastie-Tibshirani correction.
#'
#' @param X Data, n by p.
#' @param k Number of components.
#' @param lambda_ Soft-threshold level; zero recovers ordinary PCA.
#' @param max_iter,tol Power-iteration controls.
#' @param center,scale Centre columns, optionally scale to unit variance.
#' @return List with `loadings`, `scores`, `sparsity`, `adjusted_variance`,
#'   `explained`, `n_iter`.
#' @references Zou, H., Hastie, T., & Tibshirani, R. (2006). Sparse principal
#'   component analysis. JCGS 15(2), 265-286.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(1200), ncol = 6) %*% diag(c(5, 3, 1, 1, 1, 1))
#' morie_esl_sparse_pca(X, k = 1, lambda_ = 2)$sparsity > 0
#' @export
morie_esl_sparse_pca <- function(X, k = 2, lambda_ = 0.1, max_iter = 500L,
                                 tol = 1e-8, center = TRUE, scale = FALSE) {
  X <- as.matrix(X); n <- nrow(X); p <- ncol(X); k <- as.integer(k)
  if (k < 1L || k > min(n, p)) stop("k must be between 1 and min(n, p)",
                                    call. = FALSE)
  if (lambda_ < 0) stop("lambda_ must be non-negative", call. = FALSE)
  Z <- if (center) sweep(X, 2L, colMeans(X), "-") else X
  if (scale) {
    sd <- apply(Z, 2L, stats::sd); sd[sd <= 0] <- 1
    Z <- sweep(Z, 2L, sd, "/")
  }
  S <- matrix(stats::cov(Z), p, p)
  loadings <- matrix(0, p, k); iters <- integer(k); Sd <- S
  for (j in seq_len(k)) {
    ev <- eigen(Sd, symmetric = TRUE)
    v <- ev$vectors[, 1L]
    it <- 0L
    for (it in seq_len(max_iter)) {
      t <- as.numeric(Sd %*% v)
      t <- sign(t) * pmax(abs(t) - lambda_, 0)
      nrm <- sqrt(sum(t^2))
      if (nrm < 1e-12) break
      new <- t / nrm
      if (sqrt(sum((new - v)^2)) < tol || sqrt(sum((new + v)^2)) < tol) {
        v <- new; break
      }
      v <- new
    }
    if (sum(abs(v)) > 0 && v[which.max(abs(v))] < 0) v <- -v
    loadings[, j] <- v; iters[j] <- it
    Sd <- Sd - as.numeric(t(v) %*% Sd %*% v) * tcrossprod(v)
  }
  scores <- Z %*% loadings
  R <- qr.R(qr(scores))
  adj <- diag(R)^2 / max(n - 1L, 1L)
  total <- sum(diag(S))
  list(loadings = loadings, scores = scores, sparsity = mean(loadings == 0),
       adjusted_variance = adj,
       explained = if (total > 0) adj / total else rep(NA_real_, k),
       total_variance = total, lambda_ = lambda_, n_iter = iters,
       method = "esl_sparse_pca")
}

#' Two-dimensional thin-plate smoothing spline
#'
#' The infinite-dimensional penalised problem has a finite-dimensional
#' minimiser: a radial basis expansion in `r^2 log r` at the data points plus a
#' linear null-space term, so it is solved exactly by linear algebra. The
#' penalty annihilates linear functions, so as lambda grows the fit tends to
#' the least-squares PLANE, not to a constant.
#'
#' Solved on an orthonormal basis of `null(A')` rather than through the
#' bordered saddle-point system: that matrix has condition number of order
#' lambda, and a relative-cutoff least-squares solve truncates the O(1) linear
#' block outright, so the fit drifts instead of converging.
#'
#' @param X Locations, n by 2.
#' @param y Observed values.
#' @param lambda_ Smoothing parameter, non-negative.
#' @param newdata Locations to predict at; defaults to `X`.
#' @return List with `fitted`, `delta`, `beta`, `residuals`, `edf`, `rss`, `gcv`.
#' @references Duchon, J. (1977). Splines minimizing rotation-invariant
#'   semi-norms in Sobolev spaces. Springer.
#'   Hastie, T., et al. (2009). ESL (2nd ed.), Sec 5.7. Springer.
#' @examples
#' set.seed(1)
#' P <- matrix(runif(50, -1, 1), ncol = 2)
#' z <- sin(2 * P[, 1]) + P[, 2]^2
#' max(abs(morie_esl_thin_plate_spline(P, z, lambda_ = 0)$residuals)) < 1e-6
#' @export
morie_esl_thin_plate_spline <- function(X, y, lambda_ = 1, newdata = NULL) {
  if (lambda_ < 0) stop("lambda_ must be non-negative", call. = FALSE)
  X <- as.matrix(X); y <- as.numeric(y); n <- length(y)
  if (nrow(X) != n) stop(sprintf("X has %d rows but y has %d", nrow(X), n),
                         call. = FALSE)
  if (ncol(X) != 2L) {
    stop(sprintf("thin-plate splines here are 2-D; X has %d columns", ncol(X)),
         call. = FALSE)
  }
  if (n < 3L) stop("need at least 3 points to fit the linear null space",
                   call. = FALSE)
  E <- .morie_tps_kernel(X, X)
  A <- cbind(1, X)
  Q <- qr.Q(qr(A), complete = TRUE)
  Q2 <- Q[, -seq_len(3L), drop = FALSE]
  Elam <- E + lambda_ * diag(n)
  gamma <- solve(crossprod(Q2, Elam %*% Q2), crossprod(Q2, y))
  delta <- as.numeric(Q2 %*% gamma)
  beta <- as.numeric(qr.solve(A, y - Elam %*% delta))
  Z <- if (is.null(newdata)) X else as.matrix(newdata)
  if (ncol(Z) != 2L) stop("newdata must have 2 columns", call. = FALSE)
  fitted <- as.numeric(.morie_tps_kernel(Z, X) %*% delta) +
    as.numeric(cbind(1, Z) %*% beta)
  train <- as.numeric(E %*% delta) + as.numeric(A %*% beta)
  resid <- y - train
  S <- qr.solve(Elam, E)
  edf <- sum(diag(S)) + 3
  rss <- sum(resid^2)
  list(fitted = fitted, delta = delta, beta = beta, residuals = resid,
       edf = edf, rss = rss, gcv = n * rss / max((n - edf)^2, 1e-12),
       lambda_ = lambda_, method = "esl_thin_plate_spline")
}

.morie_tps_kernel <- function(A, B) {
  d2 <- outer(rowSums(A^2), rowSums(B^2), "+") - 2 * tcrossprod(A, B)
  d2 <- pmax(d2, 0)
  out <- 0.5 * d2 * log(d2)
  out[d2 <= 0] <- 0
  out
}

## ---------------------------------------------------------------------------
## Manifold learning and prototype methods
## ---------------------------------------------------------------------------


#' Independent component analysis (FastICA)
#'
#' Recovers independent sources by finding maximally non-Gaussian projections:
#' by the central limit theorem a mixture of independent sources is closer to
#' Gaussian than any source is. Two indeterminacies are intrinsic, not defects
#' -- the SCALE of each source (absorbed into the mixing matrix, so components
#' are returned with unit variance) and their ORDER. Any comparison to known
#' sources must be up to permutation and sign. Gaussian sources cannot be
#' separated at all, since a rotation of independent Gaussians is again
#' independent Gaussian.
#'
#' @param X Observed mixtures, n by p.
#' @param k Number of components; defaults to `p`.
#' @param fun Contrast: "logcosh", "exp" or "cube".
#' @param max_iter,tol Fixed-point controls.
#' @param seed Seed for the random initialisation.
#' @return List with `sources` (unit variance), `unmixing`, `mixing`,
#'   `whitening`, `mean`, `n_iter`, `converged`.
#' @references Hyvarinen, A., & Oja, E. (2000). Independent component analysis:
#'   Algorithms and applications. Neural Networks 13(4-5), 411-430.
#' @examples
#' tt <- seq(0, 8 * pi, length.out = 500)
#' S <- cbind(sin(tt), sign(cos(2.7 * tt)))
#' A <- matrix(c(1, -0.6, 0.7, 1.2), 2)
#' r <- morie_esl_ica(S %*% t(A), k = 2, seed = 1L)
#' round(apply(r$sources, 2, function(z) sqrt(mean((z - mean(z))^2))), 6)
#' @export
morie_esl_ica <- function(X, k = NULL, fun = "logcosh", max_iter = 500L,
                          tol = 1e-8, seed = 0L) {
  X <- as.matrix(X); n <- nrow(X); p <- ncol(X)
  k <- if (is.null(k)) p else as.integer(k)
  if (k < 1L || k > p) stop(sprintf("k must be between 1 and p=%d", p),
                            call. = FALSE)
  if (n < 2L) stop("need at least 2 observations", call. = FALSE)
  mu <- colMeans(X); Xc <- sweep(X, 2L, mu, "-")
  ev <- eigen(matrix(stats::cov(Xc), p, p), symmetric = TRUE)
  idx <- seq_len(k)
  d <- pmax(ev$values[idx], 1e-12); E <- ev$vectors[, idx, drop = FALSE]
  K <- t(sweep(E, 2L, sqrt(d), "/"))
  Z <- Xc %*% t(K)

  g  <- switch(fun, logcosh = tanh,
               exp = function(u) u * exp(-u^2 / 2),
               cube = function(u) u^3,
               stop('fun must be "logcosh", "exp" or "cube"', call. = FALSE))
  gp <- switch(fun, logcosh = function(u) 1 - tanh(u)^2,
               exp = function(u) (1 - u^2) * exp(-u^2 / 2),
               cube = function(u) 3 * u^2)

  set.seed(seed)
  W <- matrix(0, k, k); iters <- integer(k); converged <- TRUE
  for (j in seq_len(k)) {
    w <- stats::rnorm(k); w <- w / sqrt(sum(w^2))
    it <- 0L; ok <- FALSE
    for (it in seq_len(max_iter)) {
      wx <- as.numeric(Z %*% w)
      new <- colMeans(Z * g(wx)) - mean(gp(wx)) * w
      if (j > 1L) {
        Wp <- W[seq_len(j - 1L), , drop = FALSE]
        new <- new - as.numeric(crossprod(Wp, Wp %*% new))
      }
      nrm <- sqrt(sum(new^2))
      if (nrm < 1e-12) break
      new <- new / nrm
      if (abs(abs(sum(new * w)) - 1) < tol) { w <- new; ok <- TRUE; break }
      w <- new
    }
    if (!ok) converged <- FALSE
    W[j, ] <- w; iters[j] <- it
  }
  S <- Z %*% t(W)
  sdv <- apply(S, 2L, function(z) sqrt(mean((z - mean(z))^2)))
  sdv[sdv <= 0] <- 1
  S <- sweep(S, 2L, sdv, "/")
  unmix <- sweep(W, 1L, sdv, "/") %*% K
  list(sources = S, unmixing = unmix, mixing = .morie_ginv(unmix),
       whitening = K, mean = mu, n_iter = iters, converged = converged,
       fun = fun, method = "esl_ica")
}

#' Isomap
#'
#' Classical MDS on shortest-path distances through a nearest-neighbour graph,
#' approximating geodesics on the manifold. On a Swiss roll two points on
#' facing sheets are close in space but far along the surface, and only the
#' graph distance knows the difference.
#'
#' The neighbourhood size is the weak spot: too small and the graph
#' disconnects (the embedding is then undefined, and that is raised rather than
#' filled in); too large and it short-circuits across a fold, silently
#' reintroducing the Euclidean distance the method exists to avoid.
#'
#' @param X Data, n by p.
#' @param k Embedding dimension.
#' @param neighbors Neighbourhood size.
#' @return List with `embedding`, `eigenvalues`, `geodesic`,
#'   `residual_variance`.
#' @references Tenenbaum, J. B., de Silva, V., & Langford, J. C. (2000).
#'   Science 290(5500), 2319-2323.
#' @examples
#' set.seed(1)
#' tt <- runif(200, 1.5 * pi, 4.5 * pi)
#' X <- cbind(tt * cos(tt), runif(200, 0, 10), tt * sin(tt))
#' abs(stats::cor(morie_esl_isomap(X, k = 2, neighbors = 8)$embedding[, 1], tt)) > 0.9
#' @export
morie_esl_isomap <- function(X, k = 2, neighbors = 5) {
  X <- as.matrix(X); n <- nrow(X)
  k <- as.integer(k); neighbors <- as.integer(neighbors)
  if (k < 1L || k >= n) stop(sprintf("k must be between 1 and %d", n - 1L),
                             call. = FALSE)
  if (neighbors < 1L || neighbors >= n) {
    stop(sprintf("neighbors must be between 1 and %d", n - 1L), call. = FALSE)
  }
  D <- sqrt(pmax(outer(rowSums(X^2), rowSums(X^2), "+") - 2 * tcrossprod(X), 0))
  G <- matrix(Inf, n, n); diag(G) <- 0
  for (i in seq_len(n)) {
    nb <- order(D[i, ])[2L:(neighbors + 1L)]
    G[i, nb] <- D[i, nb]
  }
  G <- pmin(G, t(G))
  for (m in seq_len(n)) G <- pmin(G, outer(G[, m], G[m, ], "+"))
  if (any(!is.finite(G))) {
    stop(sprintf(paste("the %d-nearest-neighbour graph has %d disconnected",
                       "components; raise `neighbors`"),
                 neighbors, .morie_n_components(is.finite(G))), call. = FALSE)
  }
  Hc <- diag(n) - 1 / n
  B <- -0.5 * Hc %*% (G^2) %*% Hc
  ev <- eigen((B + t(B)) / 2, symmetric = TRUE)
  pos <- pmax(ev$values[seq_len(k)], 0)
  emb <- sweep(ev$vectors[, seq_len(k), drop = FALSE], 2L, sqrt(pos), "*")
  Dg <- sqrt(pmax(outer(rowSums(emb^2), rowSums(emb^2), "+") -
                    2 * tcrossprod(emb), 0))
  iu <- upper.tri(G)
  list(embedding = emb, eigenvalues = ev$values, geodesic = G,
       residual_variance = 1 - stats::cor(G[iu], Dg[iu])^2,
       n_components = 1L, neighbors = neighbors, method = "esl_isomap")
}

.morie_n_components <- function(adj) {
  n <- nrow(adj); seen <- logical(n); comps <- 0L
  for (s in seq_len(n)) {
    if (seen[s]) next
    comps <- comps + 1L; stack <- s; seen[s] <- TRUE
    while (length(stack)) {
      u <- stack[length(stack)]; stack <- stack[-length(stack)]
      nb <- which(adj[u, ] & !seen)
      seen[nb] <- TRUE; stack <- c(stack, nb)
    }
  }
  comps
}

#' Locally linear embedding
#'
#' Reconstructs each point from its neighbours under a sum-to-one constraint --
#' which makes the weights invariant to rotation, rescaling and translation of
#' each neighbourhood, and so a description of local geometry rather than local
#' position -- then finds low-dimensional coordinates the same weights
#' reconstruct.
#'
#' The bottom eigenvector of `M = (I - W)'(I - W)` is the constant vector with
#' eigenvalue ~0 and is discarded; keeping it is the classic LLE bug and costs
#' a dimension.
#'
#' @param X Data, n by p.
#' @param k Embedding dimension.
#' @param neighbors Neighbourhood size.
#' @param reg Relative ridge on the local Gram matrix -- required, not
#'   cosmetic, when a neighbourhood is larger than the ambient dimension.
#' @return List with `embedding`, `weights`, `eigenvalues`,
#'   `reconstruction_error`.
#' @references Roweis, S. T., & Saul, L. K. (2000). Science 290(5500),
#'   2323-2326.
#' @examples
#' set.seed(1)
#' tt <- runif(150, 0, 4 * pi)
#' X <- cbind(tt * cos(tt), tt * sin(tt), rnorm(150, sd = 0.05))
#' all(abs(rowSums(morie_esl_lle(X, k = 2, neighbors = 10)$weights) - 1) < 1e-8)
#' @export
morie_esl_lle <- function(X, k = 2, neighbors = 5, reg = 1e-3) {
  X <- as.matrix(X); n <- nrow(X)
  k <- as.integer(k); m <- as.integer(neighbors)
  if (k < 1L || k >= n) stop(sprintf("k must be between 1 and %d", n - 1L),
                             call. = FALSE)
  if (m < 1L || m >= n) stop(sprintf("neighbors must be between 1 and %d", n - 1L),
                             call. = FALSE)
  D <- outer(rowSums(X^2), rowSums(X^2), "+") - 2 * tcrossprod(X)
  W <- matrix(0, n, n); err <- 0
  for (i in seq_len(n)) {
    nb <- order(D[i, ])[2L:(m + 1L)]
    Zn <- sweep(X[nb, , drop = FALSE], 2L, X[i, ], "-")
    Cm <- tcrossprod(Zn)
    tr <- sum(diag(Cm))
    Cm <- if (tr > 0) Cm + reg * tr * diag(m) else Cm + reg * diag(m)
    w <- solve(Cm, rep(1, m)); w <- w / sum(w)
    W[i, nb] <- w
    err <- err + sum((X[i, ] - as.numeric(crossprod(X[nb, , drop = FALSE], w)))^2)
  }
  I <- diag(n)
  M <- crossprod(I - W)
  ev <- eigen((M + t(M)) / 2, symmetric = TRUE)
  ord <- order(ev$values)
  emb <- ev$vectors[, ord[2L:(k + 1L)], drop = FALSE]
  list(embedding = emb * sqrt(n), weights = W,
       eigenvalues = ev$values[ord[seq_len(k + 1L)]],
       reconstruction_error = err / n, neighbors = m, method = "esl_lle")
}

#' Self-organizing map
#'
#' Prototypes live on a fixed lattice; the winner and its LATTICE neighbours
#' move toward each observation. Dragging the neighbours along is what
#' separates a SOM from k-means -- it forces prototypes adjacent on the grid to
#' be adjacent in data space, which is what makes the map usable as a display.
#' If the neighbourhood width decays too fast the SOM degenerates into online
#' k-means, which is the usual cause of a map showing no topological ordering.
#'
#' @param X Data, n by p.
#' @param grid Lattice shape `c(rows, cols)`.
#' @param eta Initial learning rate in (0, 1].
#' @param n_epochs Passes over the data.
#' @param sigma0 Initial neighbourhood width; defaults to `max(grid) / 2`.
#' @param seed Seed.
#' @return List with `prototypes`, `lattice`, `assignment`,
#'   `quantization_error`, `topographic_error`, `counts`.
#' @references Kohonen, T. (1990). The self-organizing map. Proc. IEEE 78(9),
#'   1464-1480.
#' @examples
#' set.seed(1)
#' X <- matrix(runif(600), ncol = 2)
#' morie_esl_self_organize(X, grid = c(5, 5), seed = 1L)$topographic_error < 0.25
#' @export
morie_esl_self_organize <- function(X, grid = c(5L, 5L), eta = 0.5,
                                    n_epochs = 50L, sigma0 = NULL, seed = 0L) {
  if (eta <= 0 || eta > 1) stop("eta must be in (0, 1]", call. = FALSE)
  X <- as.matrix(X); n <- nrow(X)
  rows <- as.integer(grid[1L]); cols <- as.integer(grid[2L])
  if (rows < 1L || cols < 1L) stop("grid dimensions must be positive",
                                   call. = FALSE)
  K <- rows * cols
  if (K > n) stop(sprintf("grid has %d nodes but there are only %d observations",
                          K, n), call. = FALSE)
  lattice <- cbind(rep(seq_len(rows) - 1L, each = cols),
                   rep(seq_len(cols) - 1L, times = rows))
  storage.mode(lattice) <- "double"
  Dlat <- outer(rowSums(lattice^2), rowSums(lattice^2), "+") -
    2 * tcrossprod(lattice)
  sigma0 <- if (is.null(sigma0)) max(rows, cols) / 2 else as.numeric(sigma0)
  set.seed(seed)
  M <- X[sample.int(n, K), , drop = FALSE]
  for (ep in seq_len(n_epochs)) {
    frac <- (ep - 1L) / max(n_epochs - 1L, 1L)
    lr <- eta * (0.01 / eta)^frac
    sig <- max(sigma0 * (0.5 / sigma0)^frac, 1e-3)
    for (i in sample.int(n)) {
      win <- which.min(rowSums(sweep(M, 2L, X[i, ], "-")^2))
      h <- exp(-Dlat[win, ] / (2 * sig^2))
      M <- M + lr * h * sweep(-M, 2L, X[i, ], "+")
    }
  }
  d2 <- outer(rowSums(X^2), rowSums(M^2), "+") - 2 * tcrossprod(X, M)
  ordm <- t(apply(d2, 1L, order))
  assign <- ordm[, 1L]
  qe <- mean(sqrt(pmax(d2[cbind(seq_len(n), assign)], 0)))
  te <- mean(Dlat[cbind(ordm[, 1L], ordm[, 2L])] > 2)
  list(prototypes = M, lattice = lattice, assignment = assign,
       quantization_error = qe, topographic_error = te,
       counts = tabulate(assign, nbins = K), grid = c(rows, cols),
       n_epochs = n_epochs, method = "esl_self_organize")
}

#' Learning vector quantization (LVQ1)
#'
#' The nearest prototype is pulled toward a training point when their labels
#' agree and pushed away when they do not. That repulsion is the whole
#' difference from k-means-per-class: it drives prototypes OUT of the contested
#' region, so where classes overlap the fitted prototypes end up further apart
#' than the class centroids. They are placed to win the nearest-prototype vote,
#' not to summarise their class.
#'
#' LVQ is defined by an algorithm rather than an objective, so nothing is
#' guaranteed to decrease; the learning rate is decayed to zero to force the
#' iteration to settle.
#'
#' @param X Predictors, n by p.
#' @param y Class labels.
#' @param n_prototypes Prototypes per class.
#' @param eta Initial learning rate in (0, 1].
#' @param n_epochs Passes over the data.
#' @param newdata Points to classify; defaults to `X`.
#' @param seed Seed.
#' @return List with `prototypes`, `prototype_class`, `class`, `accuracy`.
#' @references Kohonen, T. (1989). Self-Organization and Associative Memory
#'   (3rd ed.). Springer.
#' @examples
#' set.seed(1)
#' X <- rbind(matrix(rnorm(200, -2), ncol = 2), matrix(rnorm(200, 2), ncol = 2))
#' y <- rep(0:1, each = 100)
#' morie_esl_prototype_lvq(X, y, n_prototypes = 2, seed = 1L)$accuracy > 0.9
#' @export
morie_esl_prototype_lvq <- function(X, y, n_prototypes = 2, eta = 0.1,
                                    n_epochs = 50L, newdata = NULL, seed = 0L) {
  if (n_prototypes < 1) stop("n_prototypes must be at least 1", call. = FALSE)
  if (eta <= 0 || eta > 1) stop("eta must be in (0, 1]", call. = FALSE)
  X <- as.matrix(X); yr <- as.vector(y); n <- nrow(X)
  if (length(yr) != n) stop(sprintf("X has %d rows but y has %d", n, length(yr)),
                            call. = FALSE)
  classes <- sort(unique(yr))
  set.seed(seed)
  protos <- NULL; mc <- NULL
  for (cl in classes) {
    idx <- which(yr == cl)
    if (length(idx) < n_prototypes) {
      stop(sprintf("class %s has %d observations, fewer than n_prototypes=%d",
                   as.character(cl), length(idx), n_prototypes), call. = FALSE)
    }
    protos <- rbind(protos, X[sample(idx, n_prototypes), , drop = FALSE])
    mc <- c(mc, rep(cl, n_prototypes))
  }
  M <- protos
  for (ep in seq_len(n_epochs)) {
    lr <- eta * (1 - (ep - 1L) / n_epochs)
    for (i in sample.int(n)) {
      j <- which.min(rowSums(sweep(M, 2L, X[i, ], "-")^2))
      sgn <- if (mc[j] == yr[i]) 1 else -1
      M[j, ] <- M[j, ] + sgn * lr * (X[i, ] - M[j, ])
    }
  }
  Z <- if (is.null(newdata)) X else as.matrix(newdata)
  if (ncol(Z) != ncol(X)) {
    stop(sprintf("newdata has %d columns but X has %d", ncol(Z), ncol(X)),
         call. = FALSE)
  }
  nearest <- function(A) mc[apply(
    outer(rowSums(A^2), rowSums(M^2), "+") - 2 * tcrossprod(A, M), 1L, which.min)]
  list(prototypes = M, prototype_class = mc, class = nearest(Z),
       accuracy = mean(nearest(X) == yr), classes = classes,
       n_prototypes = n_prototypes, method = "esl_prototype_lvq")
}

#' Partial dependence
#'
#' The prediction averaged over the observed joint distribution of the
#' complement variables, with the variables in `S` held fixed. This is a
#' marginal of the FITTED surface, not a conditional expectation: it answers
#' "what does the model do if I move x_S", not "what is E of y given x_S".
#'
#' The average runs over the marginal of the complement, so when x_S and the
#' complement are correlated the model is evaluated at combinations that never
#' occur, and the curve there is extrapolation. `extrapolation_warning` flags
#' grid points whose synthetic rows sit far from the observed cloud --
#' measured in the JOINT, since every quantile grid point is by construction
#' close to observed x_S values.
#'
#' @param model Function taking an `(m, p)` matrix and returning predictions.
#' @param X Data supplying the complement distribution, n by p.
#' @param S Column indices (1-based) to vary.
#' @param grid Values of x_S to evaluate; defaults to a quantile grid.
#' @param n_grid Grid size when `grid` is not given.
#' @return List with `grid`, `pd`, `centered`, `extrapolation_warning`.
#' @references Friedman, J. H. (2001). Greedy function approximation.
#'   Annals of Statistics 29(5), 1189-1232.
#' @examples
#' set.seed(1)
#' X <- matrix(runif(800, -2, 2), ncol = 2)
#' f <- function(Z) 3 * Z[, 1] + Z[, 2]^2
#' r <- morie_esl_partial_dependence(f, X, S = 1, n_grid = 9)
#' abs(stats::coef(stats::lm(r$pd ~ r$grid[, 1]))[[2]] - 3) < 1e-8
#' @export
morie_esl_partial_dependence <- function(model, X, S, grid = NULL,
                                         n_grid = 20L) {
  X <- as.matrix(X); n <- nrow(X); p <- ncol(X)
  S <- as.integer(S)
  if (any(S < 1L | S > p)) {
    stop(sprintf("S contains a column index outside 1..%d", p), call. = FALSE)
  }
  if (anyDuplicated(S)) stop("S must not repeat a column", call. = FALSE)
  if (is.null(grid)) {
    qs <- seq(0.05, 0.95, length.out = as.integer(n_grid))
    axes <- lapply(S, function(j) as.numeric(stats::quantile(X[, j], qs,
                                                             type = 7L)))
    G <- as.matrix(expand.grid(rev(axes)))
    G <- G[, rev(seq_along(S)), drop = FALSE]
    dimnames(G) <- NULL
  } else {
    G <- matrix(as.numeric(grid), ncol = length(S))
  }
  Dxx <- sqrt(pmax(outer(rowSums(X^2), rowSums(X^2), "+") -
                     2 * tcrossprod(X), 0))
  diag(Dxx) <- Inf
  ref_nn <- stats::median(apply(Dxx, 1L, min))

  pd <- numeric(nrow(G)); warn <- logical(nrow(G))
  for (t in seq_len(nrow(G))) {
    Z <- X
    Z[, S] <- matrix(G[t, ], n, length(S), byrow = TRUE)
    pd[t] <- mean(as.numeric(model(Z)))
    dz <- sqrt(pmax(outer(rowSums(Z^2), rowSums(X^2), "+") -
                      2 * tcrossprod(Z, X), 0))
    warn[t] <- stats::median(apply(dz, 1L, min)) > 2 * ref_nn
  }
  list(grid = G, pd = pd, centered = pd - mean(pd),
       extrapolation_warning = warn, S = S, n = n,
       method = "esl_partial_dependence")
}

## ---------------------------------------------------------------------------
## Neural networks and probabilistic models
## ---------------------------------------------------------------------------

#' Single-hidden-layer neural network
#'
#' The ESL Sec 11.3 architecture, fitted by full-batch gradient descent.
#' Weights are initialised small but NON-ZERO: exactly zero leaves the model
#' perfectly symmetric, so every hidden unit computes the same thing and stays
#' that way, while large starting weights saturate the sigmoid and kill the
#' gradient. Inputs are standardised by default, because weight decay is
#' otherwise a statement about the predictors' measurement units. The penalty
#' spares the biases, since shrinking an intercept toward zero is a claim about
#' the origin.
#'
#' @param X Predictors, n by p.
#' @param y Response; numeric for regression, class labels otherwise.
#' @param M Hidden units.
#' @param lambda_ Weight decay, non-negative.
#' @param lr Learning rate.
#' @param n_epochs Full-batch gradient steps.
#' @param task "regression" or "classification".
#' @param newdata Points to predict at; defaults to `X`.
#' @param seed Seed for weight initialisation.
#' @param standardize Standardise the inputs.
#' @return List with `fitted` (or `prob`/`class`), `alpha`, `beta`,
#'   `loss_path`, `hidden`, and `r_squared` or `accuracy`.
#' @references Hastie, T., et al. (2009). ESL (2nd ed.), Sec 11.3-11.4.
#'   Springer.
#' @examples
#' set.seed(1)
#' X <- matrix(runif(400, -2, 2), ncol = 2)
#' y <- sin(X[, 1]) + X[, 2]^2
#' morie_esl_neural_net(X, y, M = 8, lr = 0.3, n_epochs = 2000L,
#'                      seed = 1L)$r_squared > 0.8
#' @export
morie_esl_neural_net <- function(X, y, M = 5L, lambda_ = 0, lr = 0.1,
                                 n_epochs = 400L, task = "regression",
                                 newdata = NULL, seed = 0L,
                                 standardize = TRUE) {
  if (M < 1) stop("M must be at least 1", call. = FALSE)
  if (lambda_ < 0) stop("lambda_ must be non-negative", call. = FALSE)
  X <- as.matrix(X); yr <- as.vector(y); n <- nrow(X); p <- ncol(X)
  M <- as.integer(M)
  if (length(yr) != n) stop(sprintf("X has %d rows but y has %d", n, length(yr)),
                            call. = FALSE)
  mu <- if (standardize) colMeans(X) else numeric(p)
  sd <- if (standardize) apply(X, 2L, function(z) sqrt(mean((z - mean(z))^2)))
        else rep(1, p)
  sd[sd <= 0] <- 1
  Xs <- sweep(sweep(X, 2L, mu, "-"), 2L, sd, "/")

  if (task == "classification") {
    classes <- sort(unique(yr)); K <- length(classes)
    Y <- matrix(0, n, K)
    Y[cbind(seq_len(n), match(yr, classes))] <- 1
  } else if (task == "regression") {
    classes <- NULL; K <- 1L
    Y <- matrix(as.numeric(yr), n, 1L)
  } else {
    stop('task must be "regression" or "classification"', call. = FALSE)
  }

  set.seed(seed)
  a  <- matrix(stats::runif(p * M, -0.7, 0.7), p, M)
  a0 <- numeric(M)
  b  <- matrix(stats::runif(M * K, -0.7, 0.7), M, K)
  b0 <- numeric(K)
  sigm <- function(u) 1 / (1 + exp(-pmin(pmax(u, -500), 500)))

  losses <- numeric(n_epochs)
  for (ep in seq_len(n_epochs)) {
    Zh <- sigm(sweep(Xs %*% a, 2L, a0, "+"))
    T  <- sweep(Zh %*% b, 2L, b0, "+")
    if (task == "regression") {
      P <- T; err <- P - Y; loss <- mean(err^2)
    } else {
      e <- exp(T - apply(T, 1L, max))
      P <- e / rowSums(e); err <- P - Y
      loss <- -mean(rowSums(Y * log(P + 1e-300)))
    }
    losses[ep] <- loss + lambda_ * (sum(a^2) + sum(b^2))
    gT  <- if (task == "regression") 2 * err / n else err / n
    gb  <- crossprod(Zh, gT) + 2 * lambda_ * b
    gb0 <- colSums(gT)
    gZ  <- (gT %*% t(b)) * Zh * (1 - Zh)
    ga  <- crossprod(Xs, gZ) + 2 * lambda_ * a
    ga0 <- colSums(gZ)
    a <- a - lr * ga; a0 <- a0 - lr * ga0
    b <- b - lr * gb; b0 <- b0 - lr * gb0
  }

  Zt <- if (is.null(newdata)) X else as.matrix(newdata)
  if (ncol(Zt) != p) stop(sprintf("newdata has %d columns but X has %d",
                                  ncol(Zt), p), call. = FALSE)
  Hh <- sigm(sweep(sweep(sweep(Zt, 2L, mu, "-"), 2L, sd, "/") %*% a, 2L, a0, "+"))
  T  <- sweep(Hh %*% b, 2L, b0, "+")
  Htr <- sigm(sweep(Xs %*% a, 2L, a0, "+"))
  Ttr <- sweep(Htr %*% b, 2L, b0, "+")

  out <- list(alpha = a, alpha0 = a0, beta = b, beta0 = b0, hidden = Hh,
              loss_path = losses, M = M, lambda_ = lambda_, task = task,
              mean = mu, sd = sd, method = "esl_neural_net")
  if (task == "regression") {
    ss <- sum((yr - mean(yr))^2)
    out$fitted <- as.numeric(T)
    out$r_squared <- if (ss > 0) 1 - sum((yr - as.numeric(Ttr))^2) / ss else NA_real_
  } else {
    e <- exp(T - apply(T, 1L, max)); prob <- e / rowSums(e)
    etr <- exp(Ttr - apply(Ttr, 1L, max)); ptr <- etr / rowSums(etr)
    out$prob <- prob
    out$class <- classes[max.col(prob)]
    out$classes <- classes
    out$accuracy <- mean(classes[max.col(ptr)] == yr)
  }
  out
}

#' Backpropagation sweep
#'
#' Forward pass, then the gradients by the ESL eq. (11.5) backward recurrence.
#' The content of backprop is the chain rule REUSING the output error: it is
#' computed once and propagated, rather than recomputed per weight, which turns
#' an O(number of weights) finite-difference cost into a single extra sweep.
#'
#' Returns the gradients rather than applying them, so the choice of optimiser
#' stays separate from the derivative.
#'
#' @param X Inputs, n by p, already scaled as the network expects.
#' @param y Targets; numeric for regression, 1-based class indices otherwise.
#' @param weights List with `alpha` (p by M), `alpha0`, `beta` (M by K), `beta0`.
#' @param task "regression" or "classification".
#' @return List with `grad_alpha`, `grad_alpha0`, `grad_beta`, `grad_beta0`,
#'   `delta`, `hidden`, `output`, `loss`.
#' @references Hastie, T., et al. (2009). ESL (2nd ed.), Sec 11.4. Springer.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(120), ncol = 3)
#' y <- rnorm(40)
#' W <- list(alpha = matrix(rnorm(12) * 0.5, 3, 4), alpha0 = numeric(4),
#'           beta = matrix(rnorm(4) * 0.5, 4, 1), beta0 = 0)
#' dim(morie_esl_backprop(X, y, W)$grad_alpha)
#' @export
morie_esl_backprop <- function(X, y, weights, task = "regression") {
  X <- as.matrix(X); n <- nrow(X); p <- ncol(X)
  for (key in c("alpha", "alpha0", "beta", "beta0")) {
    if (is.null(weights[[key]])) {
      stop(sprintf("weights is missing '%s'", key), call. = FALSE)
    }
  }
  a  <- as.matrix(weights$alpha)
  a0 <- as.numeric(weights$alpha0)
  b  <- as.matrix(weights$beta)
  b0 <- as.numeric(weights$beta0)
  if (nrow(a) != p) {
    stop(sprintf("alpha has %d rows but X has %d columns", nrow(a), p),
         call. = FALSE)
  }
  M <- nrow(b); K <- ncol(b)
  if (ncol(a) != M) {
    stop(sprintf("alpha has %d hidden units but beta has %d", ncol(a), M),
         call. = FALSE)
  }
  A <- sweep(X %*% a, 2L, a0, "+")
  Z <- 1 / (1 + exp(-pmin(pmax(A, -500), 500)))
  T <- sweep(Z %*% b, 2L, b0, "+")
  yr <- as.vector(y)
  if (task == "regression") {
    Y <- matrix(as.numeric(yr), n, K)
    delta <- 2 * (T - Y) / n
    loss <- mean((T - Y)^2)
  } else if (task == "classification") {
    Y <- matrix(0, n, K)
    Y[cbind(seq_len(n), as.integer(yr))] <- 1
    e <- exp(T - apply(T, 1L, max)); P <- e / rowSums(e)
    delta <- (P - Y) / n
    loss <- -mean(rowSums(Y * log(P + 1e-300)))
  } else {
    stop('task must be "regression" or "classification"', call. = FALSE)
  }
  s <- (delta %*% t(b)) * Z * (1 - Z)          # ESL eq. (11.5)
  list(grad_alpha = crossprod(X, s), grad_alpha0 = colSums(s),
       grad_beta = crossprod(Z, delta), grad_beta0 = colSums(delta),
       delta = delta, s = s, hidden = Z, output = T, loss = loss,
       method = "esl_backprop")
}

#' Restricted Boltzmann machine (contrastive divergence)
#'
#' "Restricted" means no within-layer edges, so the conditionals factorise and
#' block Gibbs sampling is cheap. The partition function is a sum over all
#' `2^(|v| + |h|)` configurations and is intractable, so the likelihood
#' gradient cannot be computed; CD-k substitutes a k-step chain started at the
#' data.
#'
#' That makes this a biased gradient of an approximate objective, not the
#' likelihood -- so NO log-likelihood is reported. `reconstruction_error` is
#' reported instead, with the caveat that it can fall while the model gets
#' worse, which is why RBM training is judged by samples.
#'
#' @param v Binary visible data, n by d, entries in 0/1.
#' @param h Number of hidden units.
#' @param lr Learning rate.
#' @param n_epochs Training epochs.
#' @param k_cd Gibbs steps per update.
#' @param seed Seed.
#' @param batch_size Mini-batch size; defaults to the full data.
#' @return List with `W`, `a`, `b`, `hidden_prob`, `reconstruction`,
#'   `reconstruction_error`, `error_path`, `free_energy`.
#' @references Hinton, G. E. (2002). Training products of experts by minimizing
#'   contrastive divergence. Neural Computation 14(8), 1771-1800.
#' @examples
#' base <- rbind(c(1, 1, 1, 0, 0, 0), c(0, 0, 0, 1, 1, 1))
#' V <- base[rep(1:2, each = 50), ]
#' r <- morie_esl_boltzmann(V, h = 3, lr = 0.5, n_epochs = 200L, seed = 1L)
#' r$error_path[length(r$error_path)] < r$error_path[1]
#' @export
morie_esl_boltzmann <- function(v, h = 4L, lr = 0.1, n_epochs = 200L,
                                k_cd = 1L, seed = 0L, batch_size = NULL) {
  V <- as.matrix(v)
  if (!all(V %in% c(0, 1))) stop("v must be binary (0/1)", call. = FALSE)
  n <- nrow(V); d <- ncol(V); h <- as.integer(h)
  if (h < 1L) stop("h must be at least 1", call. = FALSE)
  if (k_cd < 1L) stop("k_cd must be at least 1", call. = FALSE)
  bs <- if (is.null(batch_size)) n else min(as.integer(batch_size), n)
  set.seed(seed)
  W <- matrix(stats::rnorm(d * h, 0, 0.01), d, h)
  a <- numeric(d); b <- numeric(h)
  sigm <- function(u) 1 / (1 + exp(-pmin(pmax(u, -500), 500)))
  path <- numeric(n_epochs)
  for (ep in seq_len(n_epochs)) {
    idx <- sample.int(n)
    for (s in seq(1L, n, by = bs)) {
      B <- V[idx[s:min(s + bs - 1L, n)], , drop = FALSE]
      m <- nrow(B)
      ph0 <- sigm(sweep(B %*% W, 2L, b, "+"))
      hs <- matrix(as.numeric(stats::runif(length(ph0)) < ph0), m, h)
      vk <- B; hk <- hs
      for (t in seq_len(k_cd)) {
        pv <- sigm(sweep(hk %*% t(W), 2L, a, "+"))
        vk <- matrix(as.numeric(stats::runif(length(pv)) < pv), m, d)
        phk <- sigm(sweep(vk %*% W, 2L, b, "+"))
        hk <- matrix(as.numeric(stats::runif(length(phk)) < phk), m, h)
      }
      phk <- sigm(sweep(vk %*% W, 2L, b, "+"))
      W <- W + lr * (crossprod(B, ph0) - crossprod(vk, phk)) / m
      a <- a + lr * colMeans(B - vk)
      b <- b + lr * colMeans(ph0 - phk)
    }
    ph <- sigm(sweep(V %*% W, 2L, b, "+"))
    recon <- sigm(sweep(ph %*% t(W), 2L, a, "+"))
    path[ep] <- mean((V - recon)^2)
  }
  ph <- sigm(sweep(V %*% W, 2L, b, "+"))
  recon <- sigm(sweep(ph %*% t(W), 2L, a, "+"))
  free <- -as.numeric(V %*% a) -
    rowSums(log1p(exp(pmin(pmax(sweep(V %*% W, 2L, b, "+"), -500), 500))))
  list(W = W, a = a, b = b, hidden_prob = ph, reconstruction = recon,
       reconstruction_error = path[length(path)], error_path = path,
       free_energy = free, k_cd = k_cd, n_hidden = h,
       method = "esl_boltzmann")
}

#' Dirichlet process (stick-breaking)
#'
#' Sethuraman's construction makes the draw explicit. The result is DISCRETE
#' with probability one however continuous the base measure is -- which is what
#' makes the DP a clustering prior: repeated draws collide, and each distinct
#' value is a cluster. Alpha controls fragmentation; the expected number of
#' distinct values among n draws grows like `alpha * log n`, so it grows with
#' the data rather than being fixed in advance.
#'
#' Truncating at `n_atoms` leaves part of the stick unbroken; that residual is
#' reported as `truncation_mass` and warned about when it is not negligible.
#'
#' @param alpha Concentration, positive.
#' @param G0 Base-measure sampler `G0(n)`; defaults to standard normal.
#' @param n_atoms Truncation level.
#' @param size If given, also draw this many observations from G.
#' @param seed Seed.
#' @return List with `weights`, `atoms`, `truncation_mass`, and when `size` is
#'   given `samples`, `labels`, `n_clusters`, `expected_clusters`.
#' @references Sethuraman, J. (1994). A constructive definition of Dirichlet
#'   priors. Statistica Sinica 4, 639-650.
#' @examples
#' r <- morie_esl_dirichlet_proc(alpha = 2, n_atoms = 200L, seed = 1L)
#' abs(sum(r$weights) + r$truncation_mass - 1) < 1e-12
#' @export
morie_esl_dirichlet_proc <- function(alpha = 1, G0 = NULL, n_atoms = 50L,
                                     size = NULL, seed = 0L) {
  if (alpha <= 0) stop("alpha must be positive", call. = FALSE)
  n_atoms <- as.integer(n_atoms)
  if (n_atoms < 1L) stop("n_atoms must be at least 1", call. = FALSE)
  set.seed(seed)
  betas <- stats::rbeta(n_atoms, 1, alpha)
  remain <- c(1, cumprod(1 - betas)[-n_atoms])
  weights <- betas * remain
  trunc <- prod(1 - betas)
  atoms <- if (is.null(G0)) stats::rnorm(n_atoms) else as.numeric(G0(n_atoms))
  if (length(atoms) != n_atoms) {
    stop(sprintf("G0 returned %d atoms, expected %d", length(atoms), n_atoms),
         call. = FALSE)
  }
  out <- list(weights = weights, atoms = atoms, truncation_mass = trunc,
              alpha = alpha, n_atoms = n_atoms,
              method = "esl_dirichlet_proc")
  if (trunc > 1e-3) {
    warning(sprintf(paste("%.3g of the stick is unbroken at n_atoms=%d; the",
                          "truncation is distorting the draw -- raise n_atoms"),
                    trunc, n_atoms), call. = FALSE)
  }
  if (!is.null(size)) {
    size <- as.integer(size)
    pick <- sample.int(n_atoms, size, replace = TRUE,
                       prob = weights / sum(weights))
    out$samples <- atoms[pick]
    out$labels <- pick
    out$n_clusters <- length(unique(pick))
    out$expected_clusters <- alpha * log1p(size / alpha)
  }
  out
}

#' Pairwise Markov random field
#'
#' By Hammersley-Clifford a strictly positive distribution factorises over the
#' cliques of its graph exactly when it satisfies the graph's Markov
#' properties, so the graph IS the conditional-independence statement. The
#' partition function and exact marginals are computed by enumeration, which
#' costs `states^nodes` and is therefore capped -- past that the exact answer is
#' unavailable and this says so rather than running for hours.
#'
#' Potentials are not probabilities: they need not be normalised, need not be
#' below one, and an individual potential has no marginal interpretation. Only
#' the product, after dividing by Z, does.
#'
#' @param edges Two-column matrix or list of `(i, j)` node pairs (1-based), or
#'   a symmetric 0/1 adjacency matrix.
#' @param psi Named list mapping `"i-j"` to a `states` by `states` potential;
#'   missing edges get an attractive Ising potential.
#' @param states Number of states per node.
#' @return List with `log_Z`, `marginals`, `configurations`, `probabilities`,
#'   `mode`, `n_edges`.
#' @references Hammersley, J. M., & Clifford, P. (1971). Markov fields on
#'   finite graphs and lattices. Unpublished manuscript.
#' @examples
#' r <- morie_esl_markov_rf(rbind(c(1, 2), c(2, 3)))
#' all(abs(r$marginals - 0.5) < 1e-12)
#' @export
morie_esl_markov_rf <- function(edges, psi = NULL, states = 2L) {
  E <- as.matrix(edges)
  if (nrow(E) == ncol(E) && nrow(E) > 2L && all(E %in% c(0, 1)) &&
      isTRUE(all.equal(E, t(E), check.attributes = FALSE))) {
    idx <- which(upper.tri(E) & E == 1, arr.ind = TRUE)
    E <- idx[order(idx[, 1L], idx[, 2L]), , drop = FALSE]
  }
  storage.mode(E) <- "integer"
  V <- max(E)
  s <- as.integer(states)
  if (s < 2L) stop("states must be at least 2", call. = FALSE)
  if (s^V > 2^22) {
    stop(sprintf("exact enumeration needs %d^%d configurations; the cap is 2^22",
                 s, V), call. = FALSE)
  }
  default <- matrix(exp(-1), s, s); diag(default) <- exp(1)
  key <- function(i, j) paste0(i, "-", j)
  pot <- list()
  for (r in seq_len(nrow(E))) {
    i <- E[r, 1L]; j <- E[r, 2L]
    P <- if (!is.null(psi[[key(i, j)]])) psi[[key(i, j)]] else
      if (!is.null(psi[[key(j, i)]])) psi[[key(j, i)]] else default
    if (!all(dim(as.matrix(P)) == c(s, s))) {
      stop(sprintf("potential for edge %d-%d is not %d by %d", i, j, s, s),
           call. = FALSE)
    }
    pot[[r]] <- as.matrix(P)
  }
  cfgs <- as.matrix(expand.grid(rep(list(seq_len(s) - 1L), V)))
  dimnames(cfgs) <- NULL
  logw <- numeric(nrow(cfgs))
  for (r in seq_len(nrow(E))) {
    i <- E[r, 1L]; j <- E[r, 2L]
    logw <- logw + log(pot[[r]][cbind(cfgs[, i] + 1L, cfgs[, j] + 1L)] + 1e-300)
  }
  mx <- max(logw)
  logZ <- mx + log(sum(exp(logw - mx)))
  prob <- exp(logw - logZ)
  marg <- matrix(0, V, s)
  for (aa in seq_len(s)) marg[, aa] <- colSums((cfgs == (aa - 1L)) * prob)
  list(log_Z = logZ, marginals = marg, configurations = cfgs,
       probabilities = prob, mode = cfgs[which.max(prob), ],
       edges = E, n_edges = nrow(E), n_nodes = V,
       method = "esl_markov_rf")
}

#' Score-matching objective
#'
#' Fitting an energy model needs `log Z`, which is usually intractable. Score
#' matching sidesteps it by matching the gradient of the log density with
#' respect to x, in which `Z` -- constant in x -- has already cancelled.
#' Hyvarinen's result is that the objective then depends only on the model, so
#' the DATA's own score is never needed. That is the entire trick, and it is
#' what lets an unnormalised model be fitted at all.
#'
#' The Jacobian trace costs `d` extra evaluations per point when `grad_score`
#' is not supplied, which is what makes plain score matching expensive in high
#' dimension and motivated the sliced and denoising variants.
#'
#' @param score Function returning the model score, `(n, d)` to `(n, d)`.
#' @param X Samples, n by d.
#' @param grad_score Optional function returning the diagonal of the score
#'   Jacobian; a central difference is used when absent.
#' @param eps Finite-difference step.
#' @return List with `objective`, `trace_term`, `norm_term`, `per_point`.
#' @references Hyvarinen, A. (2005). Estimation of non-normalized statistical
#'   models by score matching. JMLR 6, 695-709.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(400, 2), ncol = 1)
#' J <- sapply(c(0, 1, 2, 3), function(m)
#'   morie_esl_score_match(function(Z) -(Z - m), X)$objective)
#' which.min(J)
#' @export
morie_esl_score_match <- function(score, X, grad_score = NULL, eps = 1e-5) {
  X <- as.matrix(X); n <- nrow(X); d <- ncol(X)
  psi <- as.matrix(score(X))
  if (!all(dim(psi) == dim(X))) {
    stop(sprintf("score returned %d by %d, expected %d by %d",
                 nrow(psi), ncol(psi), n, d), call. = FALSE)
  }
  if (is.null(grad_score)) {
    diagJ <- matrix(0, n, d)
    for (j in seq_len(d)) {
      Xp <- X; Xm <- X
      Xp[, j] <- Xp[, j] + eps
      Xm[, j] <- Xm[, j] - eps
      diagJ[, j] <- (as.matrix(score(Xp))[, j] - as.matrix(score(Xm))[, j]) /
        (2 * eps)
    }
  } else {
    diagJ <- as.matrix(grad_score(X))
    if (!all(dim(diagJ) == dim(X))) {
      stop(sprintf("grad_score returned %d by %d, expected %d by %d",
                   nrow(diagJ), ncol(diagJ), n, d), call. = FALSE)
    }
  }
  per <- rowSums(diagJ) + 0.5 * rowSums(psi^2)
  list(objective = mean(per), trace_term = mean(rowSums(diagJ)),
       norm_term = mean(0.5 * rowSums(psi^2)), per_point = per,
       n = n, d = d, method = "esl_score_match")
}
