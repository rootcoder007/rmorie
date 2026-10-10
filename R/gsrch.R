# SPDX-License-Identifier: AGPL-3.0-or-later

#' Grid search with cross-validation (R parity)
#'
#' Native k-fold cross-validated grid search. Every candidate in
#' \code{tune_grid} is fitted on each training fold with one of rmorie's
#' own learners and scored on the held-out fold; the best candidate is
#' the one with the lowest mean RMSE (regression) or highest mean
#' accuracy (classification). The resampling protocol reproduces
#' \code{caret::train(..., trControl = trainControl(method = "cv"))}:
#' after \code{set.seed(seed)} one integer is drawn to seed the fold
#' assignment, which is made with caret's stratified \code{createFolds}
#' algorithm (outcome classes, or up to five quantile bins of a numeric
#' outcome, are spread evenly over the folds) in an isolated RNG
#' stream; per-fold RMSE / R-squared / MAE or accuracy / Cohen's kappa
#' are averaged over the folds (\code{NA} folds dropped), and ties are
#' broken as caret does. With the same seed the folds, and therefore
#' the scores, agree with caret's.
#'
#' Supported \code{method} values and their native learners:
#' \describe{
#'   \item{\code{"lm"}}{least squares (pivoted QR), tuning parameter
#'     \code{intercept} (logical); regression only.}
#'   \item{\code{"glm"}}{logistic (classification, two classes) or
#'     Gaussian (regression) GLM by IRLS (\code{stats::glm.fit});
#'     tuning parameter \code{parameter = "none"}.}
#'   \item{\code{"glmnet"}}{elastic net by pathwise coordinate descent on
#'     standardised predictors (Friedman, Hastie and Tibshirani 2010):
#'     Gaussian, binomial or multinomial by task. As in glmnet, each
#'     training fold is fitted along glmnet's default 100-point lambda
#'     path (same lambda_max, lambda.min.ratio and early-stopping rules)
#'     and predictions at a grid lambda are linearly interpolated
#'     between path points. Tuning parameters \code{alpha},
#'     \code{lambda}.}
#'   \item{\code{"ridge"}}{ridge regression in the Zou-Hastie elastic net
#'     parametrisation of \code{elasticnet::enet} at fraction 1
#'     (unit-norm predictors, coefficients rescaled by
#'     \code{1 + lambda}); tuning parameter \code{lambda}; regression
#'     only.}
#' }
#' Any other \code{method} is an error naming the supported set.
#'
#' @param x Numeric predictor matrix.
#' @param y Response.
#' @param method Learner id, one of \code{"lm"}, \code{"glm"},
#'   \code{"glmnet"}, \code{"ridge"} (default \code{"glmnet"} for
#'   classification, \code{"ridge"} for regression).
#' @param tune_grid data.frame of hyperparameter combos to evaluate
#'   (column names are the method's tuning parameters). When
#'   \code{NULL} and \code{method} is supplied, a three-level default
#'   grid is built as caret does.
#' @param cv CV folds.
#' @param task "auto", "classification", or "regression".
#' @param seed RNG seed (drives the fold assignment; the caller's RNG
#'   stream is restored on exit).
#' @return Named list: estimate (CV score of the best candidate), best_params,
#'   best_score (same as estimate), cv_results_params (the candidate
#'   parameters, sorted, together with the fold standard deviations of
#'   the metrics), cv_results_mean_score (mean CV RMSE for regression,
#'   accuracy for classification, aligned with cv_results_params), task,
#'   n, method.
#' @references Friedman, J., Hastie, T. and Tibshirani, R. (2010).
#'   Regularization paths for generalized linear models via coordinate
#'   descent. Journal of Statistical Software 33(1).
#'   Kuhn, M. (2008). Building predictive models in R using the caret
#'   package. Journal of Statistical Software 28(5).
#' @examples
#' set.seed(1)
#' morie_grid_search_cv(
#'   x = matrix(rnorm(150), 50, 3), y = rnorm(50),
#'   method = "lm", tune_grid = data.frame(intercept = c(TRUE, FALSE)),
#'   cv = 3L, task = "regression", seed = 1L
#' )
#' @export
morie_grid_search_cv <- function(x, y, method = NULL, tune_grid = NULL,
                                 cv = 5L, task = "auto", seed = 0L) {
  x <- .morie_ensure_design_matrix(x)
  if (is.null(dim(x))) x <- matrix(x, ncol = 1)
  x <- as.matrix(x)
  colnames(x) <- colnames(x) %||% paste0("x", seq_len(ncol(x)) - 1L)
  task <- .gs_resolve_task(task, y)
  .rmorie_local_seed(seed)
  if (is.null(method)) {
    if (task == "classification") {
      method <- "glmnet"
      if (is.null(tune_grid)) {
        tune_grid <- expand.grid(alpha = 1, lambda = c(0.01, 0.1, 1.0, 10.0))
      }
    } else {
      method <- "ridge"
      if (is.null(tune_grid)) {
        tune_grid <- expand.grid(lambda = c(0.01, 0.1, 1.0, 10.0))
      }
    }
  }
  res <- .gs_cv_search(x, y, method = method, task = task,
                       tune_grid = tune_grid, tune_length = 3L,
                       search = "grid", cv = cv)
  list(
    estimate = res$best_score,
    best_params = as.list(res$best),
    best_score = res$best_score,
    cv_results_params = res$results[
      , setdiff(
        colnames(res$results),
        c("Accuracy", "Kappa", "RMSE", "Rsquared", "MAE")
      ),
      drop = FALSE
    ],
    cv_results_mean_score = res$scores,
    task = task,
    n = nrow(x),
    method = sprintf("Grid search CV (%s)", method)
  )
}

# ---------------------------------------------------------------------------
# Native CV search engine shared by morie_grid_search_cv() and
# morie_random_search_cv() (R/rndsr.R).
# ---------------------------------------------------------------------------

.gs_supported_methods <- c("lm", "glm", "glmnet", "ridge")

.gs_resolve_task <- function(task, y) {
  if (identical(task, "auto")) {
    task <- if (is.factor(y) || all(y %in% c(0L, 1L)) || is.integer(y)) {
      "classification"
    } else {
      "regression"
    }
  }
  task
}

# caret::createFolds(y, k) fold assignment (stratified), returned as the
# list of held-out index vectors.
.gs_create_folds <- function(y, k) {
  n <- length(y)
  if (is.numeric(y)) {
    cuts <- floor(n / k)
    if (cuts < 2) cuts <- 2
    if (cuts > 5) cuts <- 5
    breaks <- unique(stats::quantile(y, probs = seq(0, 1, length = cuts)))
    y <- cut(y, breaks, include.lowest = TRUE)
  }
  if (k < n) {
    y <- factor(as.character(y))
    num <- table(y)
    fv <- integer(n)
    for (i in seq_along(num)) {
      min_reps <- num[i] %/% k
      if (min_reps > 0) {
        spares <- num[i] %% k
        sv <- rep(seq_len(k), min_reps)
        if (spares > 0) sv <- c(sv, sample(seq_len(k), spares))
        fv[which(y == names(num)[i])] <- sample(sv)
      } else {
        fv[which(y == names(num)[i])] <- sample(seq_len(k), size = num[i])
      }
    }
  } else {
    fv <- seq_len(n)
  }
  unname(split(seq_len(n), fv))
}

# One draw from the main stream seeds the fold assignment, which runs in
# an isolated stream (the main stream is put back afterwards) -- caret's
# train.default protocol.
.gs_seeded_folds <- function(y, k) {
  rs <- sample.int(.Machine$integer.max, 1L)
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }
  on.exit(.rmorie_restore_seed(old), add = TRUE)
  set.seed(rs)
  .gs_create_folds(y, k)
}

.gs_method_info <- function(method) {
  if (!is.character(method) || length(method) != 1L ||
      !(method %in% .gs_supported_methods)) {
    stop(sprintf(
      "method '%s' has no native learner; supported methods are: %s.",
      paste(format(method), collapse = ", "),
      paste(sprintf("\"%s\"", .gs_supported_methods), collapse = ", ")
    ), call. = FALSE)
  }
  switch(method,
    lm = list(params = "intercept", types = "regression"),
    glm = list(params = "parameter", types = c("regression", "classification")),
    glmnet = list(params = c("alpha", "lambda"),
                  types = c("regression", "classification")),
    ridge = list(params = "lambda", types = "regression")
  )
}

# Default / random candidate grids (caret's model-info grid functions).
.gs_default_grid <- function(method, x, y, len, search) {
  switch(method,
    lm = data.frame(intercept = TRUE),
    glm = data.frame(parameter = "none"),
    ridge = if (search == "grid") {
      expand.grid(lambda = c(0, 10^seq(-1, -4, length = len - 1)))
    } else {
      data.frame(lambda = 10^stats::runif(len, min = -5, 1))
    },
    glmnet = if (search == "grid") {
      fam <- if (is.factor(y)) {
        if (nlevels(y) > 2) "multinomial" else "binomial"
      } else {
        "gaussian"
      }
      lam <- .gs_glmnet_lambda_seq(x, y, fam, alpha = 0.5, nlambda = len + 2)
      lam <- unique(lam)
      lam <- lam[-c(1, length(lam))]
      lam <- lam[seq_len(min(length(lam), len))]
      expand.grid(alpha = seq(0.1, 1, length = len), lambda = lam)
    } else {
      data.frame(alpha = stats::runif(len, min = 0, 1),
                 lambda = 2^stats::runif(len, min = -10, 3))
    }
  )
}

.gs_cv_search <- function(x, y, method, task, tune_grid = NULL,
                          tune_length = 3L, search = "grid", cv = 5L) {
  info <- .gs_method_info(method)
  if (!(task %in% info$types)) {
    stop(sprintf("method '%s' does not support %s.", method, task),
         call. = FALSE)
  }
  y_use <- if (task == "classification") {
    factor(make.names(as.character(y)))
  } else {
    as.numeric(y)
  }
  if (task == "classification" && method == "glm" && nlevels(y_use) > 2) {
    stop("method 'glm' handles two-class outcomes only.", call. = FALSE)
  }
  n <- nrow(x)
  if (length(y_use) != n) stop("x and y have different numbers of rows.",
                               call. = FALSE)
  cv <- as.integer(cv)
  folds <- .gs_seeded_folds(y_use, cv)
  if (is.null(tune_grid)) {
    tune_grid <- .gs_default_grid(method, x, y_use, tune_length, search)
    if (search != "grid" && tune_length < nrow(tune_grid)) {
      tune_grid <- tune_grid[seq_len(tune_length), , drop = FALSE]
    }
  }
  tune_grid <- as.data.frame(tune_grid, stringsAsFactors = FALSE)
  names(tune_grid) <- sub("^\\.", "", names(tune_grid))
  if (!setequal(names(tune_grid), info$params)) {
    stop(sprintf("The tuning parameter grid should have columns %s",
                 paste(info$params, collapse = ", ")), call. = FALSE)
  }
  tune_grid <- tune_grid[!duplicated(tune_grid), info$params, drop = FALSE]
  rownames(tune_grid) <- NULL
  G <- nrow(tune_grid)
  metric_names <- if (task == "classification") {
    c("Accuracy", "Kappa")
  } else {
    c("RMSE", "Rsquared", "MAE")
  }
  per_fold <- array(NA_real_, c(G, length(metric_names), length(folds)))
  for (f in seq_along(folds)) {
    te <- folds[[f]]
    tr <- setdiff(seq_len(n), te)
    preds <- tryCatch(
      .gs_fit_predict(method, x[tr, , drop = FALSE], y_use[tr],
                      x[te, , drop = FALSE], tune_grid, task),
      error = function(e) {
        warning(sprintf("fold %d failed: %s", f, conditionMessage(e)),
                call. = FALSE)
        NULL
      }
    )
    for (g in seq_len(G)) {
      pr <- if (is.null(preds)) rep(NA, length(te)) else preds[[g]]
      per_fold[g, , f] <- .gs_post_resample(pr, y_use[te])
    }
  }
  means <- apply(per_fold, c(1, 2), function(v) mean(v, na.rm = TRUE))
  sds <- apply(per_fold, c(1, 2), function(v) stats::sd(v, na.rm = TRUE))
  means <- matrix(means, G)
  sds <- matrix(sds, G)
  means[is.nan(means)] <- NA
  perf <- data.frame(tune_grid,
                     stats::setNames(as.data.frame(means), metric_names),
                     stats::setNames(as.data.frame(sds),
                                     paste0(metric_names, "SD")),
                     check.names = FALSE, stringsAsFactors = FALSE)
  # caret: candidates in parameter order, then the model's own sort
  # (simplest first) decides ties, best = first optimum.
  perf <- perf[do.call(order, unname(as.list(perf[info$params]))), ,
               drop = FALSE]
  metric <- metric_names[1]
  sorted <- switch(method,
    glmnet = perf[order(-perf$lambda, perf$alpha), , drop = FALSE],
    ridge = perf[order(-perf$lambda), , drop = FALSE],
    perf
  )
  if (all(is.na(sorted[[metric]]))) {
    stop(sprintf("all the %s metric values are missing.", metric),
         call. = FALSE)
  }
  bi <- if (task == "classification") {
    which.max(sorted[[metric]])
  } else {
    which.min(sorted[[metric]])
  }
  best <- sorted[bi, info$params, drop = FALSE]
  rownames(best) <- NULL
  rownames(perf) <- NULL
  list(results = perf, scores = perf[[metric]], best = best,
       best_score = as.numeric(sorted[[metric]][bi]))
}

# caret::postResample() on one held-out fold.
.gs_post_resample <- function(pred, obs) {
  keep <- !is.na(pred)
  pred <- pred[keep]
  obs <- obs[keep]
  if (is.factor(obs)) {
    if (length(obs) == 0L) return(c(NA_real_, NA_real_))
    pred <- factor(pred, levels = levels(obs))
    tab <- table(obs, pred)
    m <- sum(tab)
    p0 <- sum(diag(tab)) / m
    pc <- sum((rowSums(tab) / m) * (colSums(tab) / m))
    out <- c(p0, (p0 - pc) / (1 - pc))
  } else {
    if (length(obs) == 0L) return(rep(NA_real_, 3))
    rc <- if (length(unique(pred)) < 2 || length(unique(obs)) < 2) {
      NA_real_
    } else {
      stats::cor(pred, obs)
    }
    out <- c(sqrt(mean((pred - obs)^2)), rc^2, mean(abs(pred - obs)))
  }
  out[is.nan(out)] <- NA_real_
  out
}

# Fit the learner on (xtr, ytr) for every row of grid and predict xte.
# Returns a list (one prediction vector per grid row).
.gs_fit_predict <- function(method, xtr, ytr, xte, grid, task) {
  G <- nrow(grid)
  switch(method,
    lm = lapply(seq_len(G), function(g) {
      .gs_ls_predict(xtr, ytr, xte, intercept = isTRUE(as.logical(grid$intercept[g])))
    }),
    glm = {
      pr <- .gs_glm_predict(xtr, ytr, xte, task)
      rep(list(pr), G)
    },
    ridge = lapply(seq_len(G), function(g) {
      .gs_enet_ridge_predict(xtr, ytr, xte, grid$lambda[g])
    }),
    glmnet = {
      fam <- if (task == "classification") {
        if (nlevels(ytr) > 2) "multinomial" else "binomial"
      } else {
        "gaussian"
      }
      out <- vector("list", G)
      for (a in unique(grid$alpha)) {
        idx <- which(grid$alpha == a)
        path <- .gs_glmnet_path(xtr, ytr, fam, alpha = a)
        for (g in idx) {
          out[[g]] <- .gs_glmnet_predict(path, xte, grid$lambda[g])
        }
      }
      out
    }
  )
}

# Least squares by pivoted QR; aliased columns get coefficient 0, which is
# what predict.lm does with a rank-deficient fit.
.gs_ls_predict <- function(xtr, ytr, xte, intercept = TRUE) {
  X <- if (intercept) cbind(1, xtr) else xtr
  Z <- if (intercept) cbind(1, xte) else xte
  qx <- qr(X)
  b <- qr.coef(qx, ytr)
  b[is.na(b)] <- 0
  drop(Z %*% b)
}

.gs_glm_predict <- function(xtr, ytr, xte, task) {
  X <- cbind(1, xtr)
  Z <- cbind(1, xte)
  if (task == "classification") {
    lev <- levels(ytr)
    yy <- as.numeric(ytr == lev[2])
    fit <- stats::glm.fit(X, yy, family = stats::binomial())
    b <- fit$coefficients
    b[is.na(b)] <- 0
    p <- stats::plogis(drop(Z %*% b))
    ifelse(p < 0.5, lev[1], lev[2])
  } else {
    fit <- stats::glm.fit(X, ytr, family = stats::gaussian())
    b <- fit$coefficients
    b[is.na(b)] <- 0
    drop(Z %*% b)
  }
}

# Ridge in the elasticnet::enet parametrisation at fraction s = 1:
# beta = (1 + lambda) (Xn'Xn + lambda I)^{-1} Xn'yc with centred,
# unit-L2-norm columns Xn (constant columns dropped).
.gs_enet_ridge_predict <- function(xtr, ytr, xte, lambda) {
  n <- nrow(xtr)
  xm <- colMeans(xtr)
  xc <- sweep(xtr, 2, xm)
  nx <- sqrt(colSums(xc^2))
  keep <- nx / sqrt(n) >= .Machine$double.eps
  ym <- mean(ytr)
  if (!any(keep)) return(rep(ym, nrow(xte)))
  xn <- sweep(xc[, keep, drop = FALSE], 2, nx[keep], "/")
  yc <- ytr - ym
  b <- if (lambda > 0) {
    (1 + lambda) * solve(crossprod(xn) + diag(lambda, ncol(xn)),
                         crossprod(xn, yc))
  } else {
    bb <- qr.coef(qr(xn), yc)
    bb[is.na(bb)] <- 0
    bb
  }
  zn <- sweep(sweep(xte[, keep, drop = FALSE], 2, xm[keep]), 2, nx[keep], "/")
  ym + drop(zn %*% b)
}

# ---------------------------------------------------------------------------
# Elastic-net path (glmnet's default path, standardize = TRUE,
# intercept = TRUE, unit weights). Objective at each lambda:
#   gaussian:   (1/2n) RSS + lambda P_alpha(beta), solved on y / sd(y) with
#               lambda / sd(y) and rescaled (glmnet's convention);
#   binomial / multinomial: -(1/n) loglik + lambda P_alpha(beta);
# P_alpha(b) = (1 - alpha)/2 ||b||^2 + alpha ||b||_1 on standardized x.
# Each penalised weighted least-squares step runs on rmorie's compiled
# coordinate-descent kernel (.morie_coord_descent_cpp).
# ---------------------------------------------------------------------------

.gs_glmnet_std <- function(x) {
  n <- nrow(x)
  xm <- colMeans(x)
  xs <- sqrt(colMeans(sweep(x, 2, xm)^2))
  ju <- apply(x, 2, function(v) any(v != v[1]))
  xs[!ju] <- 1
  list(n = n, xm = xm, xs = xs, ju = ju,
       Xs = sweep(sweep(x[, ju, drop = FALSE], 2, xm[ju]), 2, xs[ju], "/"))
}

.gs_glmnet_ymat <- function(y, family) {
  if (family == "binomial") {
    as.numeric(y == levels(y)[2])
  } else if (family == "multinomial") {
    outer(as.integer(y), seq_len(nlevels(y)), "==") * 1
  } else {
    as.numeric(y)
  }
}

# lambda_max (in the scale glmnet reports) for the given family.
.gs_glmnet_lambda_max <- function(st, yy, family, alpha) {
  n <- st$n
  if (family == "gaussian") {
    ym <- mean(yy)
    ys <- sqrt(mean((yy - ym)^2))
    g <- abs(drop(crossprod(st$Xs, (yy - ym) / ys))) / n
    max(g) / max(alpha, 1e-3) * ys
  } else if (family == "binomial") {
    g <- abs(drop(crossprod(st$Xs, yy - mean(yy)))) / n
    max(g) / max(alpha, 1e-3)
  } else {
    G <- abs(crossprod(st$Xs, sweep(yy, 2, colMeans(yy)))) / n
    max(G) / max(alpha, 1e-3)
  }
}

.gs_glmnet_lambda_seq <- function(x, y, family, alpha, nlambda = 100L) {
  st <- .gs_glmnet_std(x)
  yy <- .gs_glmnet_ymat(y, family)
  lmax <- .gs_glmnet_lambda_max(st, yy, family, alpha)
  flmin <- if (st$n < ncol(x)) 1e-2 else 1e-4
  alf <- max(1e-6, flmin)^(1 / (nlambda - 1))
  lam <- numeric(nlambda)
  alm <- lmax
  lam[1] <- Inf
  for (m in seq_len(nlambda - 1L)) {
    alm <- alm * alf
    lam[m + 1L] <- alm
  }
  .gs_fix_lam(lam)
}

.gs_fix_lam <- function(lam) {
  if (length(lam) > 2) {
    llam <- log(lam)
    lam[1] <- exp(2 * llam[2] - llam[3])
  }
  lam
}

# Penalised weighted least squares with unpenalised intercept:
#   min_b0,b (1/2) sum v (z - b0 - X b)^2 + lam P_alpha(b).
.gs_pwls <- function(X, z, v, alpha, lam, warm, tol = 1e-13) {
  sv <- sum(v)
  xw <- colSums(v * X) / sv
  zw <- sum(v * z) / sv
  s <- sqrt(nrow(X) * v)
  Xc <- X - rep(xw, each = nrow(X))
  fit <- .morie_coord_descent_cpp(Xc * s, (z - zw) * s,
                                  alpha, lam, 1000000L, tol, warm)
  b <- as.numeric(fit$beta_std)
  list(b = b, b0 = zw - sum(xw * b))
}

.gs_glmnet_path <- function(x, y, family, alpha, nlambda = 100L,
                            fdev = 1e-5, devmax = 0.999, mnlam = 5L,
                            pmin = 1e-9) {
  st <- .gs_glmnet_std(x)
  n <- st$n
  Xs <- st$Xs
  p <- ncol(x)
  pj <- ncol(Xs)
  yy <- .gs_glmnet_ymat(y, family)
  flmin <- if (n < p) 1e-2 else 1e-4
  alf <- max(1e-6, flmin)^(1 / (nlambda - 1))
  mnl <- min(mnlam, nlambda)
  if (family == "gaussian") {
    ym <- mean(yy)
    ys <- sqrt(mean((yy - ym)^2))
    if (!(ys > 0)) stop("y is constant; gaussian elastic net fails.", call. = FALSE)
    yt <- (yy - ym) / ys
    lmax_s <- max(abs(drop(crossprod(Xs, yt))) / n) / max(alpha, 1e-3)
    B <- matrix(0, pj, nlambda)
    lam <- numeric(nlambda)
    rsq <- numeric(nlambda)
    b <- numeric(pj)
    alm <- lmax_s
    L <- nlambda
    for (m in seq_len(nlambda)) {
      if (m == 1L) {
        b <- numeric(pj)
        lam[m] <- Inf
      } else {
        alm <- alm * alf
        lam[m] <- alm * ys
        if (pj > 0) {
          b <- as.numeric(.morie_coord_descent_cpp(Xs, yt, alpha, alm,
                                                   1000000L, 1e-13, b)$beta_std)
        }
      }
      B[, m] <- b
      rsq[m] <- 1 - mean((yt - drop(Xs %*% b))^2)
      if (m >= mnl) {
        prop <- if (rsq[m] == 0) Inf else (rsq[m] - rsq[m - 1L]) / rsq[m]
        if (prop < fdev || rsq[m] > devmax) {
          L <- m
          break
        }
      }
    }
    beta <- matrix(0, p, L)
    beta[st$ju, ] <- B[, seq_len(L), drop = FALSE] * ys / st$xs[st$ju]
    a0 <- ym - drop(crossprod(st$xm, beta))
    return(list(family = family, lambda = .gs_fix_lam(lam[seq_len(L)]),
                a0 = a0, beta = beta, levels = NULL))
  }
  if (family == "binomial") {
    q0 <- mean(yy)
    lmax <- max(abs(drop(crossprod(Xs, yy - q0))) / n) / max(alpha, 1e-3)
    dev_fun <- function(q) {
      q <- pmin(pmax(q, pmin), 1 - pmin)
      -2 * sum(yy * log(q) + (1 - yy) * log(1 - q)) / n
    }
    dev0 <- dev_fun(rep(q0, n))
    b0 <- stats::qlogis(q0)
    b <- numeric(pj)
    A0 <- numeric(nlambda)
    B <- matrix(0, pj, nlambda)
    lam <- numeric(nlambda)
    dev <- numeric(nlambda)
    alm <- lmax
    L <- nlambda
    for (m in seq_len(nlambda)) {
      if (m == 1L) {
        lam[m] <- Inf
      } else {
        alm <- alm * alf
        lam[m] <- alm
        for (it in seq_len(1000L)) {
          eta <- b0 + drop(Xs %*% b)
          q <- stats::plogis(eta)
          v <- pmax(q * (1 - q), 1e-12)
          z <- eta + (yy - q) / v
          old <- c(b0, b)
          fit <- .gs_pwls(Xs, z, v / n, alpha, alm, b)
          b0 <- fit$b0
          b <- fit$b
          if (max(abs(c(b0, b) - old)) < 1e-11) break
        }
      }
      A0[m] <- b0
      B[, m] <- b
      q <- stats::plogis(b0 + drop(Xs %*% b))
      dev[m] <- 1 - dev_fun(q) / dev0
      if (m >= mnl) {
        if (dev[m] - dev[m - 1L] < fdev || dev[m] > devmax ||
            sum(q * (1 - q)) / n <= (1 + pmin) * pmin * (1 - pmin)) {
          L <- m
          break
        }
      }
    }
    beta <- matrix(0, p, L)
    beta[st$ju, ] <- B[, seq_len(L), drop = FALSE] / st$xs[st$ju]
    a0 <- A0[seq_len(L)] - drop(crossprod(st$xm, beta))
    return(list(family = family, lambda = .gs_fix_lam(lam[seq_len(L)]),
                a0 = a0, beta = beta, levels = levels(y)))
  }
  # multinomial (ungrouped): block coordinate descent over classes, each
  # block a penalised IRLS step on that class's linear predictor.
  K <- ncol(yy)
  ybar <- colMeans(yy)
  lmax <- max(abs(crossprod(Xs, sweep(yy, 2, ybar))) / n) / max(alpha, 1e-3)
  softmax <- function(E) {
    E <- E - E[cbind(seq_len(nrow(E)), max.col(E, ties.method = "first"))]
    P <- exp(E)
    P / rowSums(P)
  }
  lin <- function(B, b0) Xs %*% B + rep(b0, each = n)
  dev_fun <- function(P) {
    -2 * sum(yy * log(pmax(P, pmin))) / n
  }
  b0 <- log(ybar)
  b0 <- b0 - mean(b0)
  Bc <- matrix(0, pj, K)
  dev0 <- dev_fun(matrix(ybar, n, K, byrow = TRUE))
  A0 <- matrix(0, K, nlambda)
  BB <- array(0, c(pj, K, nlambda))
  lam <- numeric(nlambda)
  dev <- numeric(nlambda)
  alm <- lmax
  L <- nlambda
  for (m in seq_len(nlambda)) {
    if (m == 1L) {
      lam[m] <- Inf
    } else {
      alm <- alm * alf
      lam[m] <- alm
      for (it in seq_len(2000L)) {
        old <- c(b0, Bc)
        for (k in seq_len(K)) {
          E <- lin(Bc, b0)
          P <- softmax(E)
          q <- P[, k]
          v <- pmax(q * (1 - q), 1e-12)
          z <- E[, k] + (yy[, k] - q) / v
          fit <- .gs_pwls(Xs, z, v / n, alpha, alm, Bc[, k])
          b0[k] <- fit$b0
          Bc[, k] <- fit$b
        }
        if (max(abs(c(b0, Bc) - old)) < 1e-11) break
      }
    }
    A0[, m] <- b0
    BB[, , m] <- Bc
    P <- softmax(lin(Bc, b0))
    dev[m] <- 1 - dev_fun(P) / dev0
    if (m >= mnl) {
      if (dev[m] - dev[m - 1L] < fdev || dev[m] > devmax) {
        L <- m
        break
      }
    }
  }
  beta <- array(0, c(p, K, L))
  a0 <- matrix(0, K, L)
  for (m in seq_len(L)) {
    bm <- matrix(0, p, K)
    bm[st$ju, ] <- BB[, , m] / st$xs[st$ju]
    beta[, , m] <- bm
    a0[, m] <- A0[, m] - drop(crossprod(bm, st$xm))
  }
  list(family = family, lambda = .gs_fix_lam(lam[seq_len(L)]),
       a0 = a0, beta = beta, levels = levels(y))
}

# glmnet's lambda.interp(): linear interpolation in lambda between the
# bracketing path points, clamped to the path's ends.
.gs_lambda_interp <- function(lambda, s) {
  if (length(lambda) == 1L) {
    return(list(left = rep(1, length(s)), right = rep(1, length(s)),
                frac = rep(1, length(s))))
  }
  k <- length(lambda)
  sfrac <- (lambda[1] - s) / (lambda[1] - lambda[k])
  lambda <- (lambda[1] - lambda) / (lambda[1] - lambda[k])
  sfrac[sfrac < min(lambda)] <- min(lambda)
  sfrac[sfrac > max(lambda)] <- max(lambda)
  coord <- stats::approx(lambda, seq_along(lambda), sfrac)$y
  left <- floor(coord)
  right <- ceiling(coord)
  sfrac <- (sfrac - lambda[right]) / (lambda[left] - lambda[right])
  sfrac[left == right] <- 1
  sfrac[abs(lambda[left] - lambda[right]) < .Machine$double.eps] <- 1
  list(left = left, right = right, frac = sfrac)
}

.gs_glmnet_predict <- function(path, newx, s) {
  li <- .gs_lambda_interp(path$lambda, s)
  fr <- li$frac
  if (path$family == "multinomial") {
    K <- length(path$levels)
    a0 <- path$a0[, li$left] * fr + path$a0[, li$right] * (1 - fr)
    B <- path$beta[, , li$left] * fr + path$beta[, , li$right] * (1 - fr)
    B <- matrix(B, ncol = K)
    E <- newx %*% B + rep(a0, each = nrow(newx))
    return(path$levels[max.col(E, ties.method = "first")])
  }
  a0 <- path$a0[li$left] * fr + path$a0[li$right] * (1 - fr)
  b <- path$beta[, li$left] * fr + path$beta[, li$right] * (1 - fr)
  eta <- a0 + drop(newx %*% b)
  if (path$family == "binomial") {
    path$levels[ifelse(eta > 0, 2L, 1L)]
  } else {
    eta
  }
}
