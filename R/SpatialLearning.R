.sml_ssum <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

.sml_features <- function(X, coords, trend = 1) {
  C <- as.matrix(coords)
  s1 <- C[, 1]
  s2 <- if (ncol(C) > 1) C[, 2] else rep(0, nrow(C))
  Z <- if (is.null(X)) cbind(s1, s2) else cbind(as.matrix(X), s1, s2)
  if (trend == 2) Z <- cbind(Z, s1^2, s1 * s2, s2^2)
  unname(Z)
}

.sml_ols <- function(Z, y) {
  D <- cbind(1, Z)
  as.vector(solve(crossprod(D), crossprod(D, y)))
}

.sml_pred <- function(b, Z) as.vector(cbind(1, Z) %*% b)

.sml_rmse <- function(a, b) sqrt(mean((a - b)^2))

.sml_stream <- function(seed) {
  env <- new.env()
  env$seed <- seed
  env$block <- 0
  env$buf <- numeric(0)
  env$pos <- 0
  env
}

.sml_next <- function(st) {
  if (st$pos >= length(st$buf)) {
    st$buf <- .morie_random_uniform(1024, seed = st$seed, stream = st$block)
    st$block <- st$block + 1
    st$pos <- 0
  }
  st$pos <- st$pos + 1
  st$buf[st$pos]
}

.sml_split <- function(Z, y, idx, feats, min_leaf) {
  best <- NULL
  n <- length(idx)
  for (f in feats) {
    ord <- idx[order(Z[idx, f], idx)]
    vals <- Z[ord, f]
    ys <- y[ord]
    tot <- .sml_ssum(ys)
    tot2 <- .sml_ssum(ys * ys)
    sl <- 0
    sl2 <- 0
    for (k in seq_len(n - 1)) {
      sl <- sl + ys[k]
      sl2 <- sl2 + ys[k] * ys[k]
      if (k < min_leaf || n - k < min_leaf || !(vals[k] < vals[k + 1])) next
      sr <- tot - sl
      sr2 <- tot2 - sl2
      sse <- (sl2 - sl * sl / k) + (sr2 - sr * sr / (n - k))
      if (is.null(best) || sse < best$sse) {
        best <- list(sse = sse, f = f, thr = 0.5 * (vals[k] + vals[k + 1]), left = ord[seq_len(k)],
                     right = ord[(k + 1):n])
      }
    }
  }
  best
}

.sml_tree <- function(Z, y, idx, max_depth, min_leaf, mtry, st, depth = 0) {
  ys <- y[idx]
  mn <- .sml_ssum(ys) / length(ys)
  if ((!is.null(max_depth) && depth >= max_depth) || length(idx) < 2 * min_leaf || max(ys) == min(ys)) {
    return(list(leaf = TRUE, value = mn))
  }
  p <- ncol(Z)
  if (is.null(mtry) || mtry >= p) {
    feats <- seq_len(p)
  } else {
    u <- vapply(seq_len(p), function(j) .sml_next(st), 0)
    feats <- sort(order(u, seq_len(p))[seq_len(mtry)])
  }
  b <- .sml_split(Z, y, idx, feats, min_leaf)
  if (is.null(b)) return(list(leaf = TRUE, value = mn))
  list(leaf = FALSE, f = b$f, thr = b$thr,
       left = .sml_tree(Z, y, b$left, max_depth, min_leaf, mtry, st, depth + 1),
       right = .sml_tree(Z, y, b$right, max_depth, min_leaf, mtry, st, depth + 1))
}

.sml_tpred <- function(t, z) {
  while (!t$leaf) t <- if (z[t$f] <= t$thr) t$left else t$right
  t$value
}

.sml_forest <- function(Z, y, n_trees, mtry, min_leaf, max_depth, seed) {
  n <- length(y)
  st <- .sml_stream(seed)
  trees <- vector("list", n_trees)
  osum <- numeric(n)
  ocnt <- numeric(n)
  for (b in seq_len(n_trees)) {
    boot <- vapply(seq_len(n), function(i) min(floor(.sml_next(st) * n), n - 1) + 1, 0)
    t <- .sml_tree(Z, y, boot, max_depth, min_leaf, mtry, st)
    trees[[b]] <- t
    for (i in setdiff(seq_len(n), boot)) {
      osum[i] <- osum[i] + .sml_tpred(t, Z[i, ])
      ocnt[i] <- ocnt[i] + 1
    }
  }
  list(trees = trees, oob = ifelse(ocnt > 0, osum / ocnt, NaN))
}

.sml_fpred <- function(trees, Z) {
  vapply(seq_len(nrow(Z)), function(i) .sml_ssum(vapply(trees, .sml_tpred, 0, z = Z[i, ])) / length(trees), 0)
}

.sml_boost <- function(Z, y, n_trees, rate, max_depth, min_leaf) {
  n <- length(y)
  f0 <- .sml_ssum(y) / n
  Fv <- rep(f0, n)
  trees <- vector("list", n_trees)
  for (m in seq_len(n_trees)) {
    t <- .sml_tree(Z, y - Fv, seq_len(n), max_depth, min_leaf, NULL, NULL)
    trees[[m]] <- t
    Fv <- Fv + rate * vapply(seq_len(n), function(i) .sml_tpred(t, Z[i, ]), 0)
  }
  list(f0 = f0, trees = trees, F = Fv)
}

.sml_bpred <- function(f0, trees, rate, Z) {
  vapply(seq_len(nrow(Z)), function(i) f0 + rate * .sml_ssum(vapply(trees, .sml_tpred, 0, z = Z[i, ])), 0)
}

.sml_std <- function(Z) {
  mu <- colMeans(Z)
  sd <- sqrt(colMeans(sweep(Z, 2, mu)^2))
  sd[sd == 0] <- 1
  list(mu = mu, sd = sd, S = sweep(sweep(Z, 2, mu), 2, sd, "/"))
}

.sml_cd <- function(Z, y, lam, alpha, tol = 1e-12, max_iter = 100000) {
  s <- .sml_std(Z)
  S <- s$S
  n <- length(y)
  ybar <- mean(y)
  r <- y - ybar
  b <- numeric(ncol(S))
  for (it in seq_len(max_iter)) {
    dmax <- 0
    for (j in seq_len(ncol(S))) {
      rho <- sum(S[, j] * (r + S[, j] * b[j])) / n
      z <- sign(rho) * max(abs(rho) - lam * alpha, 0) / (1 + lam * (1 - alpha))
      d <- z - b[j]
      if (d != 0) {
        r <- r - d * S[, j]
        b[j] <- z
        dmax <- max(dmax, abs(d))
      }
    }
    if (dmax < tol) break
  }
  beta <- b / s$sd
  c(ybar - sum(beta * s$mu), beta)
}

.sml_blocks <- function(coords, n_blocks, k, seed) {
  C <- as.matrix(coords)
  xs <- C[, 1]
  ys <- if (ncol(C) > 1) C[, 2] else rep(0, nrow(C))
  wx <- (max(xs) - min(xs)) / n_blocks
  wy <- (max(ys) - min(ys)) / n_blocks
  if (wx == 0) wx <- 1
  if (wy == 0) wy <- 1
  cell <- pmin(floor((xs - min(xs)) / wx), n_blocks - 1) * n_blocks + pmin(floor((ys - min(ys)) / wy), n_blocks - 1)
  used <- sort(unique(cell))
  u <- .morie_random_uniform(length(used), seed = seed)
  ord <- order(u, seq_along(used))
  fb <- integer(length(used))
  fb[ord] <- (seq_along(used) - 1L) %% k
  fb[match(cell, used)]
}

.sml_nnls <- function(A, b, max_iter = 500) {
  n <- ncol(A)
  x <- numeric(n)
  P <- integer(0)
  for (it in seq_len(max_iter)) {
    w <- as.vector(crossprod(A, b - A %*% x))
    cand <- setdiff(which(w > 1e-12), P)
    if (!length(cand)) break
    P <- c(P, cand[order(-w[cand], cand)[1]])
    repeat {
      Ap <- A[, P, drop = FALSE]
      z <- as.vector(solve(crossprod(Ap), crossprod(Ap, b)))
      if (min(z) > 0) {
        x[] <- 0
        x[P] <- z
        break
      }
      neg <- which(z <= 0)
      t <- min(x[P[neg]] / (x[P[neg]] - z[neg]))
      x[P] <- x[P] + t * (z - x[P])
      P <- P[x[P] > 1e-12]
      x[setdiff(seq_len(n), P)] <- 0
      if (!length(P)) break
    }
  }
  x
}

#' Spatial machine learning with coordinate features
#'
#' R arm of the Python modules \code{morie.fn.zx*}: learners on the spatial
#' features (covariates plus the coordinates, or a quadratic trend surface)
#' and spatial cross-validation. \code{Zxscv}: exact leave-one-out of the
#' trend-surface regression from the hat matrix. \code{Zxsbu}: buffered LOO.
#' \code{Zxsbv}: spatial block cross-validation (blocks ordered by Philox
#' uniforms, dealt to k folds). \code{Zxsls}, \code{Zxsle}: lasso and
#' elastic net by coordinate descent (glmnet objective). \code{Zxsqr}:
#' quantile regression (\code{\link{QuantileRegressionLp}}). \code{Zxsrf},
#' \code{Zxsbg}: random forest and bagging of CART trees on Philox bootstrap
#' draws. \code{Zxsgb}: least-squares gradient boosting. \code{Zxsvm}:
#' least-squares SVM with a Gaussian kernel. \code{Zxsen}: stacking of the
#' trend model, forest and boosting with non-negative weights from block
#' cross-validation. \code{Zsglm}: spatial GLMM simulation
#' (\code{\link{SpatialGlmmSimulate}}).
#'
#' @param y Response.
#' @param X Covariates (without intercept) or NULL.
#' @param coords Locations (n x 2).
#' @param trend Trend-surface degree (1 or 2).
#' @param radius Buffer radius.
#' @param n_blocks Blocks per axis.
#' @param k Folds.
#' @param seed Philox seed.
#' @param lam Penalty.
#' @param alpha Lasso share of the elastic-net penalty.
#' @param tau Quantile level.
#' @param n_trees Trees or boosting rounds.
#' @param mtry Features per split.
#' @param min_leaf Minimum leaf size.
#' @param max_depth Depth cap (NULL for none).
#' @param rate Shrinkage.
#' @param gamma Kernel width (default 1/p).
#' @param C SVM regularisation.
#' @param beta,latent,family,trials,size,shape,sigma GLMM simulation inputs.
#' @return A list whose \code{value} is the headline number, with the
#'   components of the Python result.
#' @references Roberts, D. R. et al. (2017). Cross-validation strategies for
#'   data with temporal, spatial, hierarchical, or phylogenetic structure.
#'   Ecography 40, 913-929.
#'
#'   Breiman, L. (2001). Random forests. Machine Learning 45, 5-32.
#'
#'   Hengl, T. et al. (2018). Random forest as a generic framework for
#'   predictive modeling of spatial and spatio-temporal variables. PeerJ 6,
#'   e5518.
#'
#'   Friedman, J. H. (2001). Greedy function approximation: a gradient
#'   boosting machine. Annals of Statistics 29, 1189-1232.
#'
#'   Friedman, J., Hastie, T. and Tibshirani, R. (2010). Regularization paths
#'   for generalized linear models via coordinate descent. Journal of
#'   Statistical Software 33, 1-22.
#'
#'   Suykens, J. A. K. and Vandewalle, J. (1999). Least squares support vector
#'   machine classifiers. Neural Processing Letters 9, 293-300.
#'
#'   Wolpert, D. H. (1992). Stacked generalization. Neural Networks 5,
#'   241-259.
#' @examples
#' S <- cbind((0:19 %% 5) / 4, (0:19 %/% 5) / 3)
#' X <- matrix(((0:19 * 7) %% 11) / 10)
#' y <- 1 + 2 * X[, 1] + S[, 1] - S[, 2] + ((0:19 * 3) %% 5 - 2) / 10
#' Zxscv(y, X, S)$value
#' Zxsrf(y, X, S, n_trees = 10, min_leaf = 3, seed = 1)$value
#' @export
Zxscv <- function(y, X, coords, trend = 1) {
  Z <- .sml_features(X, coords, trend)
  D <- cbind(1, Z)
  h <- rowSums((D %*% solve(crossprod(D))) * D)
  fit <- .sml_pred(.sml_ols(Z, y), Z)
  pred <- y - (y - fit) / (1 - h)
  list(value = .sml_rmse(y, pred), predictions = pred, leverage = h, n = length(y))
}

#' @rdname Zxscv
#' @export
Zxsbu <- function(y, X, coords, radius, trend = 1) {
  Z <- .sml_features(X, coords, trend)
  C <- as.matrix(coords)
  D <- as.matrix(stats::dist(C[, 1:2]))
  n <- length(y)
  pred <- numeric(n)
  ntr <- integer(n)
  for (i in seq_len(n)) {
    tr <- which(D[i, ] > radius)
    if (length(tr) <= ncol(Z) + 1) stop("buffer leaves too few training points")
    pred[i] <- .sml_pred(.sml_ols(Z[tr, , drop = FALSE], y[tr]), Z[i, , drop = FALSE])
    ntr[i] <- length(tr)
  }
  list(value = .sml_rmse(y, pred), predictions = pred, n_train = ntr, radius = radius)
}

#' @rdname Zxscv
#' @export
Zxsbv <- function(y, X, coords, n_blocks = 3, k = 5, seed = 0, trend = 1) {
  Z <- .sml_features(X, coords, trend)
  folds <- .sml_blocks(coords, n_blocks, k, seed)
  pred <- numeric(length(y))
  frm <- c()
  for (f in sort(unique(folds))) {
    te <- which(folds == f)
    tr <- which(folds != f)
    p <- .sml_pred(.sml_ols(Z[tr, , drop = FALSE], y[tr]), Z[te, , drop = FALSE])
    pred[te] <- p
    frm <- c(frm, .sml_rmse(y[te], p))
  }
  list(value = .sml_rmse(y, pred), folds = folds, fold_rmse = frm, predictions = pred)
}

#' @rdname Zxscv
#' @export
Zxsls <- function(y, X, coords, lam, trend = 2) {
  Z <- .sml_features(X, coords, trend)
  b <- .sml_cd(Z, y, lam, 1)
  fit <- .sml_pred(b, Z)
  list(value = sum(b[-1] != 0), coefficients = b, fitted = fit, rmse = .sml_rmse(y, fit))
}

#' @rdname Zxscv
#' @export
Zxsle <- function(y, X, coords, lam, alpha = 0.5, trend = 2) {
  Z <- .sml_features(X, coords, trend)
  b <- .sml_cd(Z, y, lam, alpha)
  fit <- .sml_pred(b, Z)
  list(value = sum(b[-1] != 0), coefficients = b, fitted = fit, rmse = .sml_rmse(y, fit))
}

#' @rdname Zxscv
#' @export
Zxsqr <- function(y, X, coords, tau = 0.5, trend = 1) {
  Z <- .sml_features(X, coords, trend)
  r <- QuantileRegressionLp(y, cbind(1, Z), tau)
  list(value = r$objective, coefficients = r$coefficients, fitted = .sml_pred(r$coefficients, Z), tau = tau)
}

#' @rdname Zxscv
#' @export
Zxsrf <- function(y, X, coords, n_trees = 100, mtry = NULL, min_leaf = 5, max_depth = NULL, seed = 0) {
  Z <- .sml_features(X, coords, 1)
  m <- if (is.null(mtry)) max(1, ncol(Z) %/% 3) else mtry
  f <- .sml_forest(Z, y, n_trees, m, min_leaf, max_depth, seed)
  ok <- !is.nan(f$oob)
  list(value = .sml_rmse(y[ok], f$oob[ok]), oob = f$oob, fitted = .sml_fpred(f$trees, Z), trees = f$trees, mtry = m)
}

#' @rdname Zxscv
#' @export
Zxsbg <- function(y, X, coords, n_trees = 100, min_leaf = 5, max_depth = NULL, seed = 0) {
  Z <- .sml_features(X, coords, 1)
  f <- .sml_forest(Z, y, n_trees, NULL, min_leaf, max_depth, seed)
  ok <- !is.nan(f$oob)
  list(value = .sml_rmse(y[ok], f$oob[ok]), oob = f$oob, fitted = .sml_fpred(f$trees, Z))
}

#' @rdname Zxscv
#' @export
Zxsgb <- function(y, X, coords, n_trees = 100, rate = 0.1, max_depth = 2, min_leaf = 5) {
  Z <- .sml_features(X, coords, 1)
  b <- .sml_boost(Z, y, n_trees, rate, max_depth, min_leaf)
  list(value = .sml_rmse(y, b$F), fitted = b$F, init = b$f0, trees = b$trees, rate = rate)
}

#' @rdname Zxscv
#' @export
Zxsvm <- function(y, X, coords, gamma = NULL, C = 10) {
  Z <- .sml_features(X, coords, 1)
  g <- if (is.null(gamma)) 1 / ncol(Z) else gamma
  s <- .sml_std(Z)
  n <- length(y)
  K <- exp(-g * as.matrix(stats::dist(s$S))^2)
  A <- rbind(c(0, rep(1, n)), cbind(1, K + diag(1 / C, n)))
  sol <- unname(solve(A, c(0, y)))
  fit <- as.vector(sol[1] + K %*% sol[-1])
  list(value = .sml_rmse(y, fit), fitted = fit, bias = sol[1], alpha = sol[-1], gamma = g)
}

#' @rdname Zxscv
#' @export
Zxsen <- function(y, X, coords, n_blocks = 3, k = 5, seed = 0, n_trees = 50) {
  Z <- .sml_features(X, coords, 1)
  n <- length(y)
  m <- max(1, ncol(Z) %/% 3)
  folds <- .sml_blocks(coords, n_blocks, k, seed)
  learners <- function(tr) {
    Zt <- Z[tr, , drop = FALSE]
    yt <- y[tr]
    b <- .sml_ols(Zt, yt)
    fr <- .sml_forest(Zt, yt, n_trees, m, 3, NULL, seed)
    gb <- .sml_boost(Zt, yt, n_trees, 0.1, 2, 3)
    function(Q) cbind(.sml_pred(b, Q), .sml_fpred(fr$trees, Q), .sml_bpred(gb$f0, gb$trees, 0.1, Q))
  }
  oof <- matrix(0, n, 3)
  for (f in sort(unique(folds))) {
    te <- which(folds == f)
    oof[te, ] <- learners(which(folds != f))(Z[te, , drop = FALSE])
  }
  w <- .sml_nnls(oof, y)
  fit <- as.vector(learners(seq_len(n))(Z) %*% w)
  list(value = .sml_rmse(y, as.vector(oof %*% w)), weights = w, fitted = fit, folds = folds, oof = oof)
}

#' @rdname Zxscv
#' @export
Zsglm <- function(X, beta, latent, family = "poisson", trials = 1, size = NULL, shape = NULL, sigma = 1, seed = 1) {
  r <- SpatialGlmmSimulate(X, beta, latent, family = family, trials = trials, size = size, shape = shape,
                           sigma = sigma, seed = seed)
  list(value = mean(r$y), y = r$y, eta = r$eta, mu = r$mu, family = family)
}
