# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Native k-fold CV grid / random search (morie_grid_search_cv,
# morie_random_search_cv) cross-validated against caret::train (same seed
# -> same fold assignment and, for random search, the same candidate
# draws) and the native elastic-net path against glmnet.
#
# Tolerances:
#  * lm / glm / ridge learners are closed-form or the same IRLS as caret's
#    learners, so CV metrics agree to rounding (1e-10).
#  * glmnet stops coordinate descent at thresh = 1e-7 (relative to the
#    null deviance) while the native solver converges to ~1e-13, so
#    glmnet-based CV RMSE / MAE / R-squared agree to ~1e-6 relative; we
#    allow 1e-5. Classification accuracy / kappa are discrete and must
#    agree exactly.
#  * Against glmnet run with thresh = 1e-20 the path coefficients agree
#    to 1e-8.

.cv_data <- function() {
  set.seed(42)
  n <- 80
  x <- matrix(stats::rnorm(n * 5), n, 5)
  colnames(x) <- paste0("x", 0:4)
  list(
    x = x,
    yr = drop(x %*% c(1, -0.5, 0.25, 0, 0)) + stats::rnorm(n),
    yb = as.integer(x[, 1] - x[, 2] + stats::rnorm(n) > 0),
    y3 = cut(x[, 1] + stats::rnorm(n, sd = 0.7), c(-Inf, -0.5, 0.5, Inf),
             labels = c("a", "b", "c"))
  )
}

.caret_ref <- function(x, y, method, task, grid = NULL, n_iter = NULL,
                       cv = 5L, seed = 1L) {
  y_use <- if (task == "classification") {
    factor(make.names(as.character(y)))
  } else {
    as.numeric(y)
  }
  set.seed(seed)
  if (is.null(n_iter)) {
    ctrl <- caret::trainControl(method = "cv", number = cv, classProbs = FALSE)
    fit <- suppressWarnings(caret::train(x = x, y = y_use, method = method,
                                         tuneGrid = grid, trControl = ctrl))
  } else {
    ctrl <- caret::trainControl(method = "cv", number = cv, search = "random",
                                classProbs = FALSE)
    fit <- suppressWarnings(caret::train(x = x, y = y_use, method = method,
                                         tuneLength = n_iter, trControl = ctrl))
  }
  res <- fit$results
  rownames(res) <- NULL
  list(results = res, best = fit$bestTune)
}

.expect_cv_match <- function(nat_params, nat_scores, nat_best, ref, metric,
                             tol) {
  res <- ref$results
  pcols <- names(nat_params)
  expect_identical(pcols, setdiff(names(res), c("Accuracy", "Kappa", "RMSE",
                                                "Rsquared", "MAE")))
  for (k in pcols) {
    if (is.numeric(res[[k]])) {
      # caret's *SD columns are the spread of the per-fold scores: a small difference of
      # numbers that agree to tol, so their relative error is about 100 times larger
      expect_equal(nat_params[[k]], res[[k]],
                   tolerance = if (endsWith(k, "SD")) 100 * tol else tol)
    } else {
      expect_identical(as.character(nat_params[[k]]), as.character(res[[k]]))
    }
  }
  expect_equal(nat_scores, res[[metric]], tolerance = tol)
  expect_equal(unlist(nat_best), unlist(as.list(ref$best)), tolerance = 1e-12,
               ignore_attr = TRUE)
}

test_that("grid search matches caret::train for every native learner", {
  skip_heavy()
  skip_if_not_installed("caret")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("elasticnet")
  d <- .cv_data()
  cases <- list(
    list("ridge", "regression", d$yr, expand.grid(lambda = c(0, 0.01, 0.1, 1, 10)), 1e-10),
    list("lm", "regression", d$yr, data.frame(intercept = c(TRUE, FALSE)), 1e-10),
    list("glm", "regression", d$yr, data.frame(parameter = "none"), 1e-10),
    list("glm", "classification", d$yb, data.frame(parameter = "none"), 1e-12),
    list("glmnet", "regression", d$yr,
         expand.grid(alpha = c(0, 0.3, 1), lambda = c(0.001, 0.01, 0.1, 0.5, 5)), 1e-5),
    list("glmnet", "classification", d$yb,
         expand.grid(alpha = c(0, 0.5, 1), lambda = c(0.001, 0.01, 0.1, 0.3, 10)), 1e-12),
    list("glmnet", "classification", d$y3,
         expand.grid(alpha = c(0.2, 1), lambda = c(0.001, 0.01, 0.05, 0.2)), 1e-12)
  )
  for (cs in cases) {
    method <- cs[[1]]
    task <- cs[[2]]
    y <- cs[[3]]
    grid <- cs[[4]]
    tol <- cs[[5]]
    ref <- .caret_ref(d$x, y, method, task, grid = grid, seed = 3L)
    nat <- morie_grid_search_cv(d$x, y, method = method, tune_grid = grid,
                                cv = 5L, task = task, seed = 3L)
    metric <- if (task == "classification") "Accuracy" else "RMSE"
    .expect_cv_match(nat$cv_results_params, nat$cv_results_mean_score,
                     nat$best_params, ref, metric, tol)
    best_row <- merge(ref$best, ref$results)
    expect_equal(nat$best_score, best_row[[metric]], tolerance = tol)
  }
})

test_that("default grids (no tune_grid) reproduce caret's", {
  skip_heavy()
  skip_if_not_installed("caret")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("elasticnet")
  d <- .cv_data()
  for (cs in list(list("glmnet", "regression", d$yr, 1e-5),
                  list("glmnet", "classification", d$yb, 1e-12),
                  list("ridge", "regression", d$yr, 1e-10))) {
    ref <- .caret_ref(d$x, cs[[3]], cs[[1]], cs[[2]], seed = 5L)
    nat <- morie_grid_search_cv(d$x, cs[[3]], method = cs[[1]], cv = 5L,
                                task = cs[[2]], seed = 5L)
    metric <- if (cs[[2]] == "classification") "Accuracy" else "RMSE"
    .expect_cv_match(nat$cv_results_params, nat$cv_results_mean_score,
                     nat$best_params, ref, metric, cs[[4]])
  }
})

test_that("random search reproduces caret's draws, folds and scores", {
  skip_heavy()
  skip_if_not_installed("caret")
  skip_if_not_installed("glmnet")
  skip_if_not_installed("elasticnet")
  d <- .cv_data()
  for (cs in list(list("ridge", "regression", d$yr, 1e-10),
                  list("glmnet", "regression", d$yr, 1e-5),
                  list("glmnet", "classification", d$yb, 1e-12))) {
    ref <- .caret_ref(d$x, cs[[3]], cs[[1]], cs[[2]], n_iter = 6L, cv = 4L,
                      seed = 7L)
    nat <- morie_random_search_cv(d$x, cs[[3]], method = cs[[1]], n_iter = 6L,
                                  cv = 4L, task = cs[[2]], seed = 7L)
    metric <- if (cs[[2]] == "classification") "Accuracy" else "RMSE"
    .expect_cv_match(nat$sampled_params, nat$sampled_scores,
                     nat$best_params, ref, metric, cs[[4]])
  }
})

test_that("native elastic-net path matches glmnet's path and coefficients", {
  skip_heavy()
  skip_if_not_installed("glmnet")
  d <- .cv_data()
  for (a in c(0, 0.5, 1)) {
    pg <- rmorie:::.gs_glmnet_path(d$x, d$yr, "gaussian", a)
    pb <- rmorie:::.gs_glmnet_path(d$x, factor(d$yb), "binomial", a)
    pm <- rmorie:::.gs_glmnet_path(d$x, d$y3, "multinomial", a)
    # same default lambda sequence and early-stopping point
    g1 <- glmnet::glmnet(d$x, d$yr, alpha = a)
    g2 <- glmnet::glmnet(d$x, factor(d$yb), family = "binomial", alpha = a)
    g3 <- glmnet::glmnet(d$x, d$y3, family = "multinomial", alpha = a)
    expect_equal(pg$lambda, g1$lambda, tolerance = 1e-12)
    expect_equal(pb$lambda, g2$lambda, tolerance = 1e-12)
    expect_equal(pm$lambda, g3$lambda, tolerance = 1e-12)
    # coefficients against a fully converged glmnet on the same lambdas
    t1 <- glmnet::glmnet(d$x, d$yr, alpha = a, lambda = pg$lambda[-1],
                         thresh = 1e-20, maxit = 1e7)
    t2 <- glmnet::glmnet(d$x, factor(d$yb), family = "binomial", alpha = a,
                         lambda = pb$lambda[-1], thresh = 1e-20, maxit = 1e7)
    t3 <- glmnet::glmnet(d$x, d$y3, family = "multinomial", alpha = a,
                         lambda = pm$lambda[-1], thresh = 1e-20, maxit = 1e7)
    expect_equal(pg$beta[, -1], unname(as.matrix(t1$beta)), tolerance = 1e-8)
    expect_equal(pg$a0[-1], unname(t1$a0), tolerance = 1e-8)
    expect_equal(pb$beta[, -1], unname(as.matrix(t2$beta)), tolerance = 1e-8)
    expect_equal(pb$a0[-1], unname(t2$a0), tolerance = 1e-8)
    for (k in 1:3) {
      expect_equal(pm$beta[, k, -1], unname(as.matrix(t3$beta[[k]])),
                   tolerance = 1e-8)
    }
    # interpolated predictions between path points (glmnet exact = FALSE)
    s <- c(pg$lambda[3] * 0.97, 0.05, 1e3)
    expect_equal(
      vapply(s, function(l) rmorie:::.gs_glmnet_predict(pg, d$x[1:5, ], l), numeric(5)),
      unname(stats::predict(g1, d$x[1:5, ], s = s)), tolerance = 1e-5
    )
  }
})

test_that("search runs natively and rejects learners without a native port", {
  set.seed(1)
  x <- matrix(stats::rnorm(150), 50, 3)
  y <- drop(x %*% c(1, 0, -1)) + stats::rnorm(50)
  r <- morie_grid_search_cv(x, y, method = "lm",
                            tune_grid = data.frame(intercept = c(TRUE, FALSE)),
                            cv = 3L, task = "regression", seed = 1L)
  expect_named(r, c("estimate", "best_params", "best_score",
                    "cv_results_params", "cv_results_mean_score", "task", "n",
                    "method"))
  expect_true(isTRUE(r$best_params$intercept) || isFALSE(r$best_params$intercept))
  expect_equal(r$best_score, min(r$cv_results_mean_score))
  expect_identical(r$estimate, r$best_score)
  rr <- morie_random_search_cv(x, y, n_iter = 4L, cv = 3L, task = "regression")
  expect_named(rr, c("estimate", "best_params", "best_score", "sampled_params",
                     "sampled_scores", "n_iter", "task", "n", "method"))
  expect_identical(nrow(rr$sampled_params), 4L)
  rc <- morie_grid_search_cv(x, as.integer(y > 0), cv = 3L)
  expect_identical(rc$task, "classification")
  expect_equal(rc$best_score, max(rc$cv_results_mean_score))
  expect_error(
    morie_grid_search_cv(x, y, method = "rpart",
                         tune_grid = data.frame(cp = 0.01)),
    "supported methods are: \"lm\", \"glm\", \"glmnet\", \"ridge\""
  )
  expect_error(morie_grid_search_cv(x, as.integer(y > 0), method = "ridge"),
               "does not support classification")
  expect_error(morie_grid_search_cv(x, y, method = "ridge",
                                    tune_grid = data.frame(alpha = 1)),
               "should have columns lambda")
  # the caller's RNG stream is left untouched
  set.seed(99)
  before <- .Random.seed
  morie_grid_search_cv(x, y, method = "ridge", cv = 3L, seed = 2L)
  expect_identical(.Random.seed, before)
})
