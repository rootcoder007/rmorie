# The highly adaptive lasso.
# Sources: van der Laan, M. J. & Rose, S. (2018) *Targeted Learning
# in Data Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 6
# (the assumption that the target is cadlag with finite variation
# norm; the representation of a d-variate cadlag function as a sum
# over subsets of integrals against products of indicator basis
# functions; the definition of the variation norm as the sum of
# the variation norms of the sections; the estimator as the
# minimiser of empirical risk over linear combinations of indicator
# basis functions subject to the sum of absolute coefficients being
# bounded by lambda, itself selected by cross-validation; the
# identity between that L1 bound and the variation norm of the
# discrete approximation; and the guarantee of a rate faster than
# n^{-1/4} even for complete nonparametric models and
# high-dimensional data structures). van der Laan, M. J. (2017)
# "A generally efficient targeted minimum loss based estimator
# based on the highly adaptive lasso", International Journal of
# Biostatistics 13(2), 20150097, doi:10.1515/ijb-2015-0097.
# Benkeser, D. & van der Laan, M. J. (2016) "The Highly Adaptive
# Lasso Estimator", Proceedings of the 2016 IEEE International
# Conference on Data Science and Advanced Analytics (DSAA),
# 689-696, doi:10.1109/DSAA.2016.93.
#
# Native implementation mirroring Python morie.fn.tlhal exactly:
# the same indicator basis (main, pairwise, and triple terms), the
# same projected-gradient descent onto the L1 ball with the
# same Lipschitz step from a power iteration, the same predict
# routine, and the same V-fold CV for lambda.

morie_tlhal <- function(X, y, lambdas = NULL, V = 5L, seed = 0L,
                        lam = 1.0, iters = 2000L, step = 0.05,
                        max_order = 2L, knots = NULL,
                        intercept = TRUE,
                        mode = c("fit", "predict", "cv", "norm")) {
  mode <- match.arg(mode)
  if (mode == "cv")
    return(cv_select_lambda(X, y, lambdas, V = V, seed = seed,
                            iters = iters, step = step,
                            max_order = max_order,
                            intercept = intercept))
  if (mode == "predict")
    return(hal_predict(X, y))
  if (mode == "norm")
    return(variation_norm(X))
  hal_fit(X, y, lam = lam, iters = iters, step = step,
          max_order = max_order, knots = knots,
          intercept = intercept)
}

indicator_basis <- function(X, knots = NULL, max_order = 2L) {
  rows <- as.matrix(X)
  if (is.null(dim(rows))) rows <- matrix(as.numeric(X), ncol = 1)
  n <- nrow(rows); d <- ncol(rows)
  K <- rows
  if (!is.null(knots)) {
    K <- as.matrix(knots)
    if (is.null(dim(K))) K <- matrix(as.numeric(knots), ncol = 1)
  }
  subsets <- lapply(seq_len(d), function(j) j)
  if (max_order >= 2L)
    subsets <- c(subsets, lapply(seq_len(d), function(a)
      lapply((a + 1L):d, function(b) c(a, b))))
  if (max_order >= 3L)
    subsets <- c(subsets, lapply(seq_len(d), function(a)
      lapply((a + 1L):d, function(b)
        lapply((b + 1L):d, function(c) c(a, b, c)))))
  subsets <- unlist(subsets, recursive = FALSE)
  cols <- list()
  for (S in subsets) {
    for (u in seq_len(nrow(K))) {
      cols[[length(cols) + 1L]] <- list(
        S = as.integer(S),
        v = as.numeric(K[u, S, drop = FALSE]))
    }
  }
  design <- matrix(0, n, length(cols))
  for (j in seq_along(cols)) {
    cj <- cols[[j]]
    S <- cj$S
    v <- cj$v
    if (length(S) == 1L) {
      design[, j] <- as.integer(rows[, S] >= v)
    } else {
      design[, j] <- as.integer(apply(
        rows[, S, drop = FALSE], 1,
        function(r) all(r >= v)))
    }
  }
  list(design = design, columns = cols, n_basis = length(cols),
       max_order = as.integer(max_order))
}

variation_norm <- function(beta) {
  b <- as.numeric(beta)
  sum(abs(b))
}

hal_fit <- function(X, y, lam = 1.0, iters = 2000L, step = 0.05,
                    max_order = 2L, knots = NULL, intercept = TRUE) {
  B <- indicator_basis(X, knots, max_order)
  D <- B$design
  t <- as.numeric(y)
  n <- nrow(D); p <- ncol(D)
  if (length(t) != n)
    stop(sprintf("tlhal: %d rows but %d outcomes", n, length(t)))
  if (as.numeric(lam) <= 0)
    stop("tlhal: lambda must be positive")
  b <- rep(0, p)
  b0 <- if (intercept) mean(t) else 0
  v <- rep(1, p)
  lmax <- 1
  for (it in seq_len(30L)) {
    Dv <- as.numeric(D %*% v)
    w <- as.numeric(crossprod(D, Dv))
    nw <- sqrt(sum(w * w))
    if (nw <= 1e-12) break
    v <- w / nw
    lmax <- nw
  }
  step <- min(as.numeric(step), 0.9 * n / max(2 * lmax, 1e-12))
  hist <- numeric(as.integer(iters))
  for (it in seq_len(as.integer(iters))) {
    pred <- as.numeric(b0 + D %*% b)
    res <- pred - t
    hist[it] <- sum(res * res) / n
    gr <- 2 * as.numeric(crossprod(D, res)) / n
    b <- b - as.numeric(step) * gr
    if (intercept) b0 <- b0 - as.numeric(step) * 2 * mean(res)
    b <- .tlhal_project_l1(b, as.numeric(lam))
  }
  pred <- as.numeric(b0 + D %*% b)
  list(estimate = b, beta = b, intercept = b0,
       columns = B$columns, n_basis = B$n_basis,
       variation_norm = variation_norm(b), lambda = as.numeric(lam),
       mse = sum((pred - t)^2) / n, mse_history = hist,
       max_order = as.integer(max_order),
       method = "highly adaptive lasso; van der Laan & Rose (2018) Chap. 6",
       note = "the L1 bound IS the variation norm of the fit, and the rate beats n^{-1/4} without any smoothness assumption")
}

.tlhal_project_l1 <- function(v, lam) {
  if (sum(abs(v)) <= lam) return(v)
  u <- sort(abs(v), decreasing = TRUE)
  css <- 0; rho <- 0; theta <- 0
  for (j in seq_along(u)) {
    css <- css + u[j]
    if (u[j] - (css - lam) / (j) > 0) {
      rho <- j
      theta <- (css - lam) / j
    }
  }
  sign(v) * pmax(abs(v) - theta, 0)
}

hal_predict <- function(model, X) {
  rows <- as.matrix(X)
  if (is.null(dim(rows))) rows <- matrix(as.numeric(X), ncol = 1)
  cols <- model$columns; b <- model$beta
  out <- rep(model$intercept, nrow(rows))
  for (j in seq_along(cols)) {
    if (b[j] == 0) next
    cj <- cols[[j]]
    S <- cj$S; v <- cj$v
    if (length(S) == 1L) {
      out <- out + b[j] * as.numeric(rows[, S] >= v)
    } else {
      out <- out + b[j] * as.numeric(apply(
        rows[, S, drop = FALSE], 1,
        function(r) all(r >= v)))
    }
  }
  out
}

cv_select_lambda <- function(X, y, lambdas, V = 5L, seed = 0L,
                             ...) {
  rows <- as.matrix(X)
  if (is.null(dim(rows))) rows <- matrix(as.numeric(X), ncol = 1)
  t <- as.numeric(y)
  n <- length(t)
  e_rng <- .ghc_rng(as.numeric(seed))
  idx <- seq_len(n)
  for (i in n:2) {
    j <- as.integer(.ghc_unif(e_rng, 1L) * (i + 1)) %% (i + 1)
    if (j == 0L) j <- 1L
    if (j == i) j <- i - 1L
    tmp <- idx[i]; idx[i] <- idx[j]; idx[j] <- tmp
  }
  folds <- lapply(seq_len(as.integer(V)), function(v)
    idx[seq(v, length(idx), by = as.integer(V))])
  risks <- list()
  for (lam in lambdas) {
    tot <- 0; m <- 0
    for (f in folds) {
      tr <- setdiff(seq_len(n), f)
      fit <- hal_fit(rows[tr, , drop = FALSE], t[tr],
                     lam = as.numeric(lam), knots = rows[tr, , drop = FALSE], ...)
      pr <- hal_predict(fit, rows[f, , drop = FALSE])
      for (a in seq_along(f))
        tot <- tot + (pr[a] - t[f[a]])^2
      m <- m + length(f)
    }
    risks[[as.character(lam)]] <- tot / m
  }
  best <- as.numeric(names(which.min(risks)))
  list(lambda = best, cv_risks = risks,
       note = "lambda bounds the variation norm, so the tuning parameter is interpretable")
}

.tlhal_cheatsheet <- function() {
  paste("tlhal: replace SMOOTHNESS with a VARIATION NORM bound. ",
        "Any cadlag function of finite variation is a sum over ",
        "subsets of integrals against products of indicators, so ",
        "fit a linear combination of INDICATOR BASIS functions ",
        "under sum|beta| <= lambda -- and for the discrete ",
        "approximation that L1 bound IS the variation norm. ",
        "Lambda is chosen by cross-validation. The rate beats ",
        "n^{-1/4} even in a fully nonparametric model, which is ",
        "exactly the threshold double-robust efficiency arguments ",
        "require of nuisance estimators.", sep = "")
}
