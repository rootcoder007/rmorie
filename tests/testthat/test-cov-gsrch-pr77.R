# R/gsrch.R, the lines PR #77 added that no other test reached: caret's fold assignment for small and
# unbalanced samples, the lm / glm / glmnet default grids, the glm learner, ridge with no usable
# column, the glmnet lambda sequence and multinomial predictions, and every refusal -- against caret
# and glmnet where they exist.

.gs <- function(name) get(name, envir = environment(morie_grid_search_cv))

test_that("folds are caret's createFolds for numeric, unbalanced and leave-one-out samples", {
  skip_if_not_installed("caret")
  cf <- .gs(".gs_create_folds")
  for (case in list(list(as.numeric(1:8), 5L),                                 # cuts below 2
                    list(factor(c(rep("a", 9), "b", "b")), 5L),               # a class rarer than k
                    list(as.numeric(1:4), 4L),                                # k = n
                    list(stats::rnorm(40), 5L))) {
    set.seed(11)
    a <- cf(case[[1]], case[[2]])
    set.seed(11)
    b <- unname(caret::createFolds(case[[1]], case[[2]], list = TRUE, returnTrain = FALSE))
    expect_identical(lapply(a, sort), lapply(b, sort))
  }
  # no seed yet in the session: the fold draw leaves none behind
  sf <- .gs(".gs_seeded_folds")
  genv <- globalenv()
  if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
    keep <- get(".Random.seed", envir = genv)
    on.exit(assign(".Random.seed", keep, envir = genv), add = TRUE)
    rm(".Random.seed", envir = genv)
  }
  expect_length(sf(as.numeric(1:20), 4L), 4L)
})

test_that("default grids are caret's for lm, glm and glmnet", {
  skip_if_not_installed("caret")
  skip_if_not_installed("glmnet")
  dg <- .gs(".gs_default_grid")
  set.seed(2)
  x <- matrix(stats::rnorm(300), 100, 3, dimnames = list(NULL, c("a", "b", "c")))
  y <- x[, 1] - x[, 2] + stats::rnorm(100)
  expect_identical(dg("lm", x, y, 3L, "grid"), data.frame(intercept = TRUE))
  expect_identical(dg("glm", x, y, 3L, "grid"), data.frame(parameter = "none"))
  for (yy in list(y, factor(y > 0), factor(cut(y, 3, labels = c("lo", "mid", "hi"))))) {
    ours <- dg("glmnet", x, yy, 3L, "grid")
    ref <- caret::getModelInfo("glmnet", regex = FALSE)$glmnet$grid(x, yy, len = 3, search = "grid")
    expect_equal(ours$alpha, ref$alpha)
    expect_equal(ours$lambda, ref$lambda, tolerance = 1e-8)
  }
})

test_that("the search refuses what it cannot run, says a failed fold, and stops when every fold failed", {
  cs <- .gs(".gs_cv_search")
  set.seed(4)
  x <- matrix(stats::rnorm(120), 40, 3, dimnames = list(NULL, c("a", "b", "c")))
  y3 <- factor(sample(c("p", "q", "r"), 40, TRUE))
  expect_error(cs(x, y3, "glm", "classification"), "method 'glm' handles two-class outcomes only.", fixed = TRUE)
  expect_error(cs(x, stats::rnorm(39), "lm", "regression"), "x and y have different numbers of rows.", fixed = TRUE)
  w <- character()
  expect_error(withCallingHandlers(cs(x, rep(1, 40), "glmnet", "regression", tune_grid = data.frame(alpha = 0.5, lambda = 0.1)),
                                   warning = function(e) { w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning") }),
               "all the RMSE metric values are missing.", fixed = TRUE)
  expect_true(any(grepl("y is constant; gaussian elastic net fails.", w, fixed = TRUE)))
  expect_true(any(grepl("^fold 1 failed: ", w)))
  # a random search keeps tune_length candidates
  r <- cs(x, x[, 1] + stats::rnorm(40), "glmnet", "regression", tune_length = 2L, search = "random")
  expect_equal(nrow(r$results), 2L)
})

test_that("the glm learner predicts as glm.fit does, for both tasks, and through the public entry", {
  gp <- .gs(".gs_glm_predict")
  set.seed(6)
  xtr <- matrix(stats::rnorm(80), 40, 2)
  xte <- matrix(stats::rnorm(20), 10, 2)
  yr <- drop(xtr %*% c(1, -1)) + stats::rnorm(40)
  expect_equal(gp(xtr, yr, xte, "regression"), drop(cbind(1, xte) %*% stats::lm.fit(cbind(1, xtr), yr)$coefficients))
  yc <- factor(ifelse(yr > 0, "hi", "lo"), levels = c("lo", "hi"))
  b <- stats::glm.fit(cbind(1, xtr), as.numeric(yc == "hi"), family = stats::binomial())$coefficients
  expect_identical(gp(xtr, yc, xte, "classification"), ifelse(stats::plogis(drop(cbind(1, xte) %*% b)) < 0.5, "lo", "hi"))
  res <- morie_grid_search_cv(cbind(a = xtr[, 1], b = xtr[, 2]), yc, method = "glm", cv = 4L)
  expect_true(is.finite(res$best_score))
  # a single covariate given as a vector
  expect_true(is.finite(morie_grid_search_cv(xtr[, 1], yr, method = "lm", cv = 4L)$best_score))
})

test_that("ridge with no usable column predicts the mean; empty resamples are NA", {
  rp <- .gs(".gs_enet_ridge_predict")
  expect_equal(rp(matrix(1, 5, 2), c(1, 2, 3, 4, 5), matrix(0, 3, 2), 0.1), rep(3, 3))
  pr <- .gs(".gs_post_resample")
  expect_identical(pr(factor(character(0)), factor(character(0), levels = "a")), c(NA_real_, NA_real_))
  expect_identical(pr(numeric(0), numeric(0)), rep(NA_real_, 3))
})

test_that("the glmnet lambda sequence and multinomial predictions match glmnet", {
  skip_if_not_installed("glmnet")
  set.seed(8)
  x <- matrix(stats::rnorm(400), 100, 4)
  y <- x[, 1] + 0.5 * x[, 2] + stats::rnorm(100)
  yb <- factor(y > 0)
  ym <- factor(cut(y, 3, labels = c("lo", "mid", "hi")))
  ls <- .gs(".gs_glmnet_lambda_seq")
  for (cfg in list(list(y, "gaussian"), list(yb, "binomial"), list(ym, "multinomial"))) {
    ours <- ls(x, cfg[[1]], cfg[[2]], alpha = 0.5, nlambda = 7L)
    ref <- glmnet::glmnet(x, cfg[[1]], family = cfg[[2]], alpha = 0.5, nlambda = 7)$lambda
    expect_equal(ours[seq_along(ref)], ref, tolerance = 1e-8)
  }
  path <- .gs(".gs_glmnet_path")(x, ym, "multinomial", alpha = 1)
  ref <- glmnet::glmnet(x, ym, family = "multinomial", alpha = 1)
  s <- c(0.05, 0.01)
  ours <- .gs(".gs_glmnet_predict")(path, x[1:30, ], s)
  expect_identical(as.vector(ours), as.vector(stats::predict(ref, x[1:30, ], s = s, type = "class")))
  li <- .gs(".gs_lambda_interp")(0.3, c(0.1, 0.5))
  expect_identical(li, list(left = c(1, 1), right = c(1, 1), frac = c(1, 1)))
  res <- morie_grid_search_cv(x, ym, method = "glmnet", cv = 3L)
  expect_true(res$best_score > 0.3)
  rr <- morie_grid_search_cv(x, y, method = "glmnet", cv = 3L)        # a numeric response: gaussian
  expect_true(is.finite(rr$best_score) && rr$best_score < stats::sd(y))
})
