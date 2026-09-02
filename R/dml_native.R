# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native double machine learning engines (feat/native-specializations,
# module 10). Chernozhukov et al. (2018): Neyman-orthogonal scores +
# K-fold cross-fitting. Nuisance learners are GCV-tuned ridge
# regressions (outcome) and logistic regression (propensity) -- fully
# deterministic, no DoubleML/mlr3/ranger at runtime. Repetitions
# aggregate by DoubleML's median rule.

# GCV-tuned ridge on standardized X; returns out-of-fold predictions.
#' Internal helper: cross-fit ridge with per-fold GCV lambda
#' @noRd
.morie_dml_xfit_ridge_gcv <- function(X, y, n_folds = 5L,
                                      random_state = 42L) {
  n <- nrow(X)
  p <- ncol(X)
  set.seed(random_state)
  folds <- sample(rep(seq_len(n_folds), length.out = n))
  pred <- numeric(n)
  grid <- 10^seq(-3, 3, length.out = 13)
  Xs <- scale(X)
  scl <- attr(Xs, "scaled:scale")
  scl[scl == 0] <- 1
  Xs <- sweep(sweep(X, 2, attr(Xs, "scaled:center"), "-"), 2, scl, "/")
  for (k in seq_len(n_folds)) {
    te <- which(folds == k)
    tr <- setdiff(seq_len(n), te)
    Xt <- Xs[tr, , drop = FALSE]
    yt <- y[tr]
    yc <- mean(yt)
    sv <- svd(Xt)
    d2 <- sv$d^2
    uty <- crossprod(sv$u, yt - yc)
    best <- Inf
    beta <- NULL
    for (lam in grid) {
      shrink <- sv$d / (d2 + lam)
      # GCV = n * RSS / (n - edf)^2
      fitted <- sv$u %*% (uty * d2 / (d2 + lam))
      rss <- sum(((yt - yc) - fitted)^2)
      edf <- sum(d2 / (d2 + lam))
      gcv <- length(tr) * rss / (length(tr) - edf)^2
      if (gcv < best) {
        best <- gcv
        beta <- sv$v %*% (shrink * uty)
      }
    }
    pred[te] <- as.numeric(Xs[te, , drop = FALSE] %*% beta) + yc
  }
  pred
}

# Cross-fit logistic propensity, clipped.
#' Internal helper: cross-fit logistic propensity
#' @noRd
.morie_dml_xfit_logit <- function(X, d, n_folds = 5L, random_state = 42L) {
  n <- nrow(X)
  set.seed(random_state)
  folds <- sample(rep(seq_len(n_folds), length.out = n))
  ps <- numeric(n)
  for (k in seq_len(n_folds)) {
    te <- which(folds == k)
    tr <- setdiff(seq_len(n), te)
    dat_tr <- data.frame(d = d[tr], X[tr, , drop = FALSE])
    fit <- suppressWarnings(stats::glm(d ~ .,
      data = dat_tr,
      family = stats::binomial()
    ))
    ps[te] <- stats::predict(fit,
      newdata = data.frame(X[te, , drop = FALSE]),
      type = "response"
    )
  }
  pmin(pmax(ps, 0.01), 0.99)
}

# One PLR cross-fit repetition: theta + IF-based se.
#' Internal helper: PLR single repetition
#' @noRd
.morie_dml_plr_once <- function(X, y, d, n_folds, seed) {
  ml_y <- .morie_dml_xfit_ridge_gcv(X, y, n_folds, seed)
  ml_d <- .morie_dml_xfit_ridge_gcv(X, d, n_folds, seed + 1L)
  u <- y - ml_y
  v <- d - ml_d
  denom <- sum(v * v)
  if (denom <= 0) {
    stop("morie_estimate_double_ml: treatment residual variance is zero")
  }
  theta <- sum(v * u) / denom
  psi <- v * (u - theta * v)
  se <- sqrt(sum(psi^2)) / denom
  c(theta = theta, se = se)
}

#' Internal helper: native PLR DML engine (median-aggregated reps)
#' @srrstats {G1.0} Primary reference: Chernozhukov et al. (2018,
#'   Econometrics J. 21(1)) -- double/debiased ML, partially linear
#'   model, Neyman-orthogonal score, cross-fitting; DoubleML (Bach et
#'   al. 2024, JSS) is the reference implementation cross-validated
#'   against in tests/cross/.
#' @srrstats {G3.1} Nuisance learners and their tuning (GCV ridge,
#'   logistic propensity, clipping bounds) are documented here.
#' @noRd
.morie_dml_plr_native <- function(X, y, d, n_folds = 5L, n_rep = 1L,
                                  random_state = 42L) {
  reps <- vapply(
    seq_len(n_rep), function(r) {
      .morie_dml_plr_once(
        X, y, d, n_folds,
        random_state + (r - 1L) * 1000L
      )
    },
    numeric(2)
  )
  theta_med <- stats::median(reps["theta", ])
  # DoubleML median aggregation: se^2 = median(se_r^2 + (theta_r - theta_med)^2)
  se <- sqrt(stats::median(reps["se", ]^2 +
    (reps["theta", ] - theta_med)^2))
  list(theta = theta_med, se = se)
}

#' Internal helper: native IRM (AIPW) DML engine
#' @srrstats {G1.0} Chernozhukov et al. (2018) interactive regression
#'   model with the AIPW orthogonal score.
#' @noRd
.morie_dml_irm_native <- function(X, y, d, n_folds = 5L,
                                  random_state = 42L) {
  if (length(unique(d)) < 2L) {
    stop("morie_estimate_irm: treatment must have both arms present",
      call. = FALSE
    )
  }
  n <- nrow(X)
  set.seed(random_state)
  folds <- sample(rep(seq_len(n_folds), length.out = n))
  mu1 <- numeric(n)
  mu0 <- numeric(n)
  ps <- .morie_dml_xfit_logit(X, d, n_folds, random_state)
  for (k in seq_len(n_folds)) {
    te <- which(folds == k)
    tr <- setdiff(seq_len(n), te)
    tr1 <- tr[d[tr] == 1]
    tr0 <- tr[d[tr] == 0]
    mu1[te] <- if (length(tr1) >= ncol(X) + 2L) {
      .morie_dml_ridge_predict(
        X[tr1, , drop = FALSE], y[tr1],
        X[te, , drop = FALSE]
      )
    } else {
      mean(y[tr1])
    }
    mu0[te] <- if (length(tr0) >= ncol(X) + 2L) {
      .morie_dml_ridge_predict(
        X[tr0, , drop = FALSE], y[tr0],
        X[te, , drop = FALSE]
      )
    } else {
      mean(y[tr0])
    }
  }
  psi <- (mu1 - mu0) + d * (y - mu1) / ps - (1 - d) * (y - mu0) / (1 - ps)
  theta <- mean(psi)
  se <- stats::sd(psi) / sqrt(n)
  list(theta = theta, se = se)
}

# Train-on-A predict-on-B ridge with GCV lambda.
#' Internal helper: ridge train/predict
#' @noRd
.morie_dml_ridge_predict <- function(Xtr, ytr, Xte) {
  ctr <- colMeans(Xtr)
  scl <- apply(Xtr, 2, stats::sd)
  scl[scl == 0] <- 1
  Xs <- sweep(sweep(Xtr, 2, ctr, "-"), 2, scl, "/")
  Zs <- sweep(sweep(Xte, 2, ctr, "-"), 2, scl, "/")
  yc <- mean(ytr)
  sv <- svd(Xs)
  d2 <- sv$d^2
  uty <- crossprod(sv$u, ytr - yc)
  grid <- 10^seq(-3, 3, length.out = 13)
  best <- Inf
  beta <- NULL
  for (lam in grid) {
    fitted <- sv$u %*% (uty * d2 / (d2 + lam))
    rss <- sum(((ytr - yc) - fitted)^2)
    edf <- sum(d2 / (d2 + lam))
    gcv <- nrow(Xs) * rss / (nrow(Xs) - edf)^2
    if (gcv < best) {
      best <- gcv
      beta <- sv$v %*% ((sv$d / (d2 + lam)) * uty)
    }
  }
  as.numeric(Zs %*% beta) + yc
}
