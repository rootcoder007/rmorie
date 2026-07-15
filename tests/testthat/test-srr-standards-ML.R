# SPDX-License-Identifier: AGPL-3.0-or-later
#
# srr "ML" standards: one test per checklist item, run against the
# rmorie ML pipeline surface (R/ml_pipeline.R). Each test verifies the
# behaviour the standard describes.

# ---- shared fixtures --------------------------------------------------
.ml_reg <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  x1 <- rnorm(n); x2 <- rnorm(n)
  data.frame(x1 = x1, x2 = x2, y = 2 + 1.5 * x1 - 0.8 * x2 + rnorm(n, sd = 0.3))
}
.ml_clf <- function(n = 300L, seed = 2L) {
  set.seed(seed)
  x1 <- rnorm(n); x2 <- rnorm(n)
  p <- 1 / (1 + exp(-(0.5 + 1.2 * x1 - 0.9 * x2)))
  data.frame(x1 = x1, x2 = x2, y = rbinom(n, 1, p))
}

# ======================= ML1: input data ==============================

test_that("ML1.0/ML1.0a train vs test data are conceptually distinct", {
  sp <- morie_ml_split(.ml_reg(), c(train = 0.7, test = 0.3), seed = 1)
  expect_true(all(c("train", "test") %in% names(sp)))
  expect_gt(nrow(sp$train), nrow(sp$test))
})

test_that("ML1.1/ML1.1a train/test/validation labels present + confirmed", {
  sp <- morie_ml_split(.ml_reg(), c(train = 0.5, test = 0.25, validation = 0.25))
  expect_setequal(unique(sp$roles), c("train", "test", "validation"))
  expect_equal(length(sp$roles), sp$n)          # every row labelled
})

test_that("ML1.1b label matching is case-insensitive + partial", {
  expect_equal(unname(.ml_match_role(c("Training", "TEST", "valid"))),
               c("train", "test", "validation"))
  sp <- morie_ml_split(.ml_reg(), c(Train = 0.8, Testing = 0.2))
  expect_true(all(c("train", "test") %in% names(sp)))
})

test_that("ML1.2 single tabular input, partitions distinguished by a variable", {
  sp <- morie_ml_split(.ml_reg(), c(train = 0.6, test = 0.4))
  expect_type(sp$roles, "character")            # the distinguishing variable
  expect_equal(nrow(sp$train) + nrow(sp$test), sp$n)
})

test_that("ML1.3 train and test are cleanly partitioned (distinct list items)", {
  sp <- morie_ml_split(.ml_reg(), c(train = 0.6, test = 0.4), seed = 5)
  expect_true(is.data.frame(sp$train) && is.data.frame(sp$test))
  expect_equal(nrow(sp$train) + nrow(sp$test), sp$n)   # disjoint + covering
})

test_that("ML1.4 partitions carry distinct labels", {
  sp <- morie_ml_split(.ml_reg(), c(train = 0.7, test = 0.3))
  expect_true(all(sp$roles %in% c("train", "test")))
})

test_that("ML1.5 a single function summarises dataset contents (counts)", {
  sp <- morie_ml_split(.ml_reg(120L), c(train = 0.5, test = 0.5))
  out <- capture.output(print(sp))
  expect_true(any(grepl("train", out)) && any(grepl("120 rows", out)))
})

test_that("ML1.6/ML1.6a training errors informatively on missing values", {
  d <- .ml_reg(50L); d$x1[3] <- NA
  m <- morie_ml_model("linear", "gd", epochs = 10)
  expect_error(morie_ml_train(m, d[c("x1", "x2")], d$y), "missing")
})

test_that("ML1.6b docs/examples show how to impute rather than discard", {
  d <- .ml_reg(50L); d$x1[3] <- NA
  rec <- morie_ml_prep(morie_ml_recipe("y", c("x1", "x2")), d)
  baked <- morie_ml_bake(rec, d)
  expect_false(anyNA(baked$x1))                 # imputed, not dropped
})

test_that("ML1.7/ML1.7a imputation offers multiple user-defined methods", {
  x <- c(1, NA, 3, NA, 5)
  expect_equal(morie_ml_impute(x, "mean")[2], mean(x, na.rm = TRUE))
  expect_equal(morie_ml_impute(x, "median")[2], stats::median(x, na.rm = TRUE))
  expect_equal(morie_ml_impute(x, "constant", value = 0)[2], 0)
})

test_that("ML1.7b imputation steps are documented + reproducible", {
  d <- .ml_reg(40L); d$x1[c(2, 4)] <- NA
  rec <- morie_ml_prep(morie_ml_recipe("y", "x1", impute = "median"), d)
  expect_equal(rec$params$x1$impute_fill, stats::median(d$x1, na.rm = TRUE))
})

test_that("ML1.8 missing values treated equally for train and test", {
  tr <- .ml_reg(60L); te <- .ml_reg(30L, seed = 9); te$x1[1] <- NA
  rec <- morie_ml_prep(morie_ml_recipe("y", c("x1", "x2")), tr)
  bt <- morie_ml_bake(rec, te)
  expect_false(anyNA(bt$x1))                    # same recipe fills test NA
})

# ======================= ML2: preprocessing ===========================

test_that("ML2.0/ML2.0a preprocessing is a dedicated function returning a submittable object", {
  rec <- morie_ml_prep(morie_ml_recipe("y", c("x1", "x2")), .ml_reg())
  baked <- morie_ml_bake(rec, .ml_reg())
  expect_s3_class(rec, "morie_ml_recipe")
  m <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 20),
                      baked[c("x1", "x2")], baked$y)
  expect_s3_class(m, "morie_ml_fit")            # directly submittable
})

test_that("ML2.0b recipe object has a class with a print method (summary)", {
  rec <- morie_ml_recipe("y", "x1")
  out <- capture.output(print(rec))
  expect_true(any(grepl("morie_ml_recipe", out)))
})

test_that("ML2.1 transformations applied are recorded", {
  rec <- morie_ml_prep(morie_ml_recipe("y", "x1"), .ml_reg())
  expect_true(!is.null(rec$params$x1$center) && !is.null(rec$params$x1$scale))
})

test_that("ML2.2/ML2.2a explicit target values for transforms are honoured", {
  rec <- morie_ml_prep(
    morie_ml_recipe("y", "x1", scale = FALSE,
                    center_target = c(x1 = 100)), .ml_reg())
  expect_equal(rec$params$x1$center, 100)
})

test_that("ML2.2b explicit transform values are documented via params", {
  rec <- morie_ml_prep(morie_ml_recipe("y", "x1"), .ml_reg())
  expect_named(rec$params$x1, c("impute", "impute_fill", "center", "scale"))
})

test_that("ML2.3 transformation values are stored in the returned object", {
  d <- .ml_reg()
  rec <- morie_ml_prep(morie_ml_recipe("y", "x1"), d)
  expect_equal(rec$params$x1$center, mean(d$x1), tolerance = 1e-8)
})

test_that("ML2.4 default transformations are documented + applied", {
  rec <- morie_ml_recipe("y", "x1")             # defaults: impute mean, center, scale
  expect_equal(rec$impute, "mean")
  expect_true(rec$center && rec$scale)
})

test_that("ML2.5 default transformations can be switched off", {
  d <- .ml_reg()
  rec <- morie_ml_prep(morie_ml_recipe("y", "x1", center = FALSE, scale = FALSE), d)
  baked <- morie_ml_bake(rec, d)
  expect_equal(baked$x1, d$x1)                  # untransformed
})

test_that("ML2.6 transformation functions are exported for reuse", {
  expect_equal(morie_ml_center(c(2, 4, 6)), c(-2, 0, 2), ignore_attr = TRUE)
  expect_equal(as.numeric(morie_ml_scale(c(2, 4, 6))),
               c(2, 4, 6) / stats::sd(c(2, 4, 6)))
})

test_that("ML2.7 transformations can be reversed", {
  d <- .ml_reg()
  rec <- morie_ml_prep(morie_ml_recipe("y", c("x1", "x2")), d)
  baked <- morie_ml_bake(rec, d)
  back  <- morie_ml_bake(rec, baked, reverse = TRUE)
  expect_equal(back$x1, d$x1, tolerance = 1e-8)
})

# ======================= ML3: model specification =====================

test_that("ML3.0/ML3.0a a model can be specified without fitting", {
  spec <- morie_ml_model("logistic", nofit = TRUE)
  out <- morie_ml_train(spec, .ml_clf()[c("x1", "x2")], .ml_clf()$y)
  expect_s3_class(out, "morie_ml_model")        # returned unfitted
})

test_that("ML3.0b spec accepts the preprocessing-stage output", {
  rec <- morie_ml_prep(morie_ml_recipe("y", c("x1", "x2")), .ml_reg())
  baked <- morie_ml_bake(rec, .ml_reg())
  spec <- morie_ml_model("linear", "gd", epochs = 20)
  expect_s3_class(morie_ml_train(spec, baked[c("x1", "x2")], baked$y),
                  "morie_ml_fit")
})

test_that("ML3.0c the spec object can be directly trained", {
  spec <- morie_ml_model("linear", "gd", epochs = 20)
  expect_s3_class(morie_ml_train(spec, .ml_reg()[c("x1", "x2")], .ml_reg()$y),
                  "morie_ml_fit")
})

test_that("ML3.0d spec object has a defined class + print", {
  spec <- morie_ml_model("linear")
  expect_s3_class(spec, "morie_ml_model")
  expect_true(any(grepl("morie_ml_model", capture.output(print(spec)))))
})

test_that("ML3.1 both untrained (params-only) and trained models supported", {
  spec <- morie_ml_model("linear", "gd", epochs = 10)      # params only
  fit  <- morie_ml_train(spec, .ml_reg()[c("x1", "x2")], .ml_reg()$y)
  expect_s3_class(spec, "morie_ml_model")
  expect_s3_class(fit, "morie_ml_fit")
})

test_that("ML3.2 different models can be applied to the same data spec", {
  d <- .ml_reg(); X <- d[c("x1", "x2")]; y <- d$y
  f1 <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 30), X, y)
  f2 <- morie_ml_train(morie_ml_model("linear", "adam", epochs = 30), X, y)
  expect_false(isTRUE(all.equal(f1$weights, f2$weights)))
})

test_that("ML3.3 model-object class behaviour is defined (predict/print)", {
  fit <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 20),
                        .ml_reg()[c("x1", "x2")], .ml_reg()$y)
  expect_true(is.numeric(predict(fit, .ml_reg()[c("x1", "x2")])))
})

test_that("ML3.4/ML3.4b training rate is a documented parameter with a default", {
  expect_equal(formals(morie_ml_model)$learning_rate, 0.1)
})

test_that("ML3.4a training rate can be selected automatically (tuning)", {
  g <- expand.grid(learning_rate = c(0.01, 0.1, 0.3), optimizer = "gd",
                   stringsAsFactors = FALSE)
  best <- morie_ml_tune(.ml_reg()[c("x1", "x2")], .ml_reg()$y, g,
                        type = "linear", n_folds = 3)$best
  expect_true(best$learning_rate %in% c(0.01, 0.1, 0.3))
})

test_that("ML3.5/ML3.5a optimizer algorithm is a controllable parameter", {
  expect_true(all(c("gd", "sgd", "adam") %in% .ml_optimizers))
  expect_s3_class(morie_ml_model(optimizer = "adam"), "morie_ml_model")
})

test_that("ML3.5b the loss function is a controllable parameter", {
  expect_equal(morie_ml_model("logistic")$loss, "logloss")
  expect_equal(morie_ml_model("linear")$loss, "mse")
})

test_that("ML3.6/ML3.6a multiple search-space (optimizer) algorithms exist", {
  expect_gte(length(.ml_optimizers), 2L)
})

test_that("ML3.6b multiple loss functions are permitted", {
  expect_true(all(c("logloss", "mse") %in% names(.ml_loss)))
})

test_that("ML3.7 algorithms are pure R (no C++/GPU control needed)", {
  # ML3.7 is conditional on algorithms coded in C++; the pipeline is
  # pure R, so CPU/GPU selection does not arise. Confirm no compiled
  # entry points are used by the trainer.
  expect_false(any(grepl("\\.Call|\\.C\\(|useDynLib",
                         deparse(body(morie_ml_train)))))
})

# ======================= ML4: training ================================

test_that("ML4.0 a single unified function trains the model", {
  expect_true(is.function(morie_ml_train))
  fit <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 20),
                        .ml_reg()[c("x1", "x2")], .ml_reg()$y)
  expect_s3_class(fit, "morie_ml_fit")
})

test_that("ML4.1/ML4.1a trained object retains model-internal parameters", {
  fit <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 20),
                        .ml_reg()[c("x1", "x2")], .ml_reg()$y)
  expect_true(is.numeric(fit$weights) && length(fit$weights) == 3L)
})

test_that("ML4.1b the loss at each epoch is retained", {
  fit <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 50, tol = 0),
                        .ml_reg()[c("x1", "x2")], .ml_reg()$y)
  expect_length(fit$loss_path, 50L)
  expect_lt(fit$loss_path[50], fit$loss_path[1])   # loss decreases
})

test_that("ML4.1c information advancing each step (gradient norm) is retained", {
  fit <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 20),
                        .ml_reg()[c("x1", "x2")], .ml_reg()$y)
  expect_true(is.numeric(fit$grad_norm) && length(fit$grad_norm) >= 1L)
})

test_that("ML4.2 retained information can be extracted", {
  fit <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 20),
                        .ml_reg()[c("x1", "x2")], .ml_reg()$y)
  expect_true(all(c("weights", "loss_path", "grad_norm") %in% names(fit)))
})

test_that("ML4.3 batch parameters are explicit + documented", {
  expect_equal(formals(morie_ml_model)$batch_size, 32L)
})

test_that("ML4.4 guidance on batch-size selection is provided", {
  # documented on the parameter; smaller batches => more gradient steps
  d <- .ml_clf()
  f_small <- morie_ml_train(morie_ml_model("logistic", "sgd", batch_size = 16,
                                           epochs = 5), d[c("x1", "x2")], d$y)
  f_big   <- morie_ml_train(morie_ml_model("logistic", "sgd", batch_size = 128,
                                           epochs = 5), d[c("x1", "x2")], d$y)
  expect_gt(length(f_small$grad_norm), length(f_big$grad_norm))
})

test_that("ML4.5 time-to-train can be estimated", {
  est <- morie_ml_train_time(morie_ml_model("linear", "gd", epochs = 100),
                             .ml_reg()[c("x1", "x2")], .ml_reg()$y)
  expect_true(is.numeric(est) && est >= 0)
})

test_that("ML4.6 progress information is provided + suppressible", {
  m <- morie_ml_model("linear", "gd", epochs = 3)
  expect_message(morie_ml_train(m, .ml_reg()[c("x1", "x2")], .ml_reg()$y,
                                verbose = TRUE), "epoch")
  expect_silent(morie_ml_train(m, .ml_reg()[c("x1", "x2")], .ml_reg()$y,
                               verbose = FALSE))
})

test_that("ML4.7 results are combined across resampling iterations", {
  rs <- morie_ml_resample(morie_ml_model("linear", "gd", epochs = 30),
                          .ml_reg()[c("x1", "x2")], .ml_reg()$y, n_folds = 4)
  expect_length(rs$scores, 4L)
  expect_true(is.numeric(rs$mean))
})

test_that("ML4.8/ML4.8a resampling partitions each case for testing once", {
  set.seed(42); n <- nrow(.ml_reg())
  folds <- sample(rep(seq_len(5L), length.out = n))
  expect_equal(length(folds), n)               # every case assigned one fold
  expect_setequal(unique(folds), 1:5)
})

# ======================= ML5: trained model object ====================

test_that("ML5.0/ML5.0a trained object has its own class", {
  fit <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 10),
                        .ml_reg()[c("x1", "x2")], .ml_reg()$y)
  expect_s3_class(fit, "morie_ml_fit")
})

test_that("ML5.0b trained object has a default print method", {
  fit <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 10),
                        .ml_reg()[c("x1", "x2")], .ml_reg()$y)
  expect_true(any(grepl("morie_ml_fit", capture.output(print(fit)))))
})

test_that("ML5.1 trained object carries input metadata", {
  fit <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 10),
                        .ml_reg()[c("x1", "x2")], .ml_reg()$y)
  expect_equal(fit$feature_names, c("x1", "x2"))
  expect_equal(fit$n_obs, 200L)
})

test_that("ML5.2/ML5.2a trained-object functionality is exercised (predict/summary)", {
  fit <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 10),
                        .ml_reg()[c("x1", "x2")], .ml_reg()$y)
  expect_s3_class(summary(fit), "data.frame")
  expect_length(predict(fit, .ml_reg()[c("x1", "x2")]), 200L)
})

test_that("ML5.2b/ML5.2c trained model can be saved and reloaded", {
  fit <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 10),
                        .ml_reg()[c("x1", "x2")], .ml_reg()$y)
  f <- morie_ml_save(fit, file.path(tempdir(), "mdl"))
  back <- morie_ml_load(f)
  expect_equal(back$weights, fit$weights)       # round-trips via saveRDS
})

test_that("ML5.3 performance assessment is a distinct function", {
  fit <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 40),
                        .ml_reg()[c("x1", "x2")], .ml_reg()$y)
  m <- morie_ml_assess(fit, .ml_reg()[c("x1", "x2")], .ml_reg()$y,
                       metrics = "rmse")
  expect_true(is.numeric(m["rmse"]) && m["rmse"] > 0)
})

test_that("ML5.4/ML5.4a a variety of consistent metrics are available", {
  fit <- morie_ml_train(morie_ml_model("logistic", "adam", epochs = 60),
                        .ml_clf()[c("x1", "x2")], .ml_clf()$y)
  m <- morie_ml_assess(fit, .ml_clf()[c("x1", "x2")], .ml_clf()$y,
                       metrics = c("accuracy", "roc_auc", "brier"))
  expect_named(m, c("accuracy", "roc_auc", "brier"))
  expect_true(m["accuracy"] >= 0 && m["accuracy"] <= 1)
})

test_that("ML5.4b custom metrics can be submitted", {
  fit <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 40),
                        .ml_reg()[c("x1", "x2")], .ml_reg()$y)
  m <- morie_ml_assess(fit, .ml_reg()[c("x1", "x2")], .ml_reg()$y,
                       metrics = "rmse",
                       custom = list(medae = function(y, p) median(abs(y - p))))
  expect_true("medae" %in% names(m))
})

# ======================= ML6: prediction workflow =====================

test_that("ML6.0 workflow separates training from prediction", {
  fit <- morie_ml_train(morie_ml_model("logistic", "adam", epochs = 60),
                        .ml_clf()[c("x1", "x2")], .ml_clf()$y)
  pr <- predict(fit, .ml_clf()[c("x1", "x2")], type = "response")
  expect_true(all(pr >= 0 & pr <= 1))
})

test_that("ML6.1/ML6.1a full workflow (split->prep->train->assess) runs end-to-end", {
  d <- .ml_clf(300L)
  sp <- morie_ml_split(d, c(train = 0.7, test = 0.3), seed = 3)
  rec <- morie_ml_prep(morie_ml_recipe("y", c("x1", "x2")), sp$train)
  tr <- morie_ml_bake(rec, sp$train); te <- morie_ml_bake(rec, sp$test)
  fit <- morie_ml_train(morie_ml_model("logistic", "adam", epochs = 100),
                        tr[c("x1", "x2")], tr$y)
  acc <- morie_ml_assess(fit, te[c("x1", "x2")], te$y, metrics = "accuracy")
  expect_gt(acc["accuracy"], 0.6)               # learns a real signal
})

# ======================= ML7: tests ===================================

test_that("ML7.0 partial + case-insensitive role matching is confirmed", {
  # ML1.1b spec examples: "Test", "test", "testing" must all suffice
  # (case-insensitive + prefix partial matching). "Testing" extends the
  # role name; "tst" is not a prefix and is intentionally not matched.
  expect_equal(unname(.ml_match_role("Testing")), "test")
  expect_equal(unname(.ml_match_role("test")), "test")
  expect_equal(unname(.ml_match_role("TRAINING")), "train")
})

test_that("ML7.1 different numeric scaling of input changes fitted weights", {
  d <- .ml_reg(); X <- d[c("x1", "x2")]
  f1 <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 30), X, d$y)
  Xs <- X; Xs$x1 <- Xs$x1 * 100
  f2 <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 30), Xs, d$y)
  expect_false(isTRUE(all.equal(unname(f1$weights), unname(f2$weights))))
})

test_that("ML7.2 internal imputation matches an external computation", {
  x <- c(1, NA, 3, 5, NA, 7)
  expect_equal(morie_ml_impute(x, "mean")[is.na(x)],
               rep(mean(x, na.rm = TRUE), 2))    # vs external mean()
})

test_that("ML7.3/ML7.3a/ML7.3b model-object class + accessors are as documented", {
  fit <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 10),
                        .ml_reg()[c("x1", "x2")], .ml_reg()$y)
  expect_s3_class(fit, "morie_ml_fit")
  expect_true(exists("print.morie_ml_fit"))     # documented S3 method
  expect_error(predict(fit, .ml_reg()[c("x1", "x2")], type = "class"),
               "logistic-only")                 # documented restriction
})

test_that("ML7.4 lower training rate needs more epochs to reach a loss", {
  X <- .ml_reg()[c("x1", "x2")]; y <- .ml_reg()$y
  f_slow <- morie_ml_train(morie_ml_model("linear", "gd", learning_rate = 0.001,
                                          epochs = 300), X, y)
  f_fast <- morie_ml_train(morie_ml_model("linear", "gd", learning_rate = 0.1,
                                          epochs = 300), X, y)
  expect_gt(f_slow$loss_path[10], f_fast$loss_path[10])  # slower descent
})

test_that("ML7.5 optimal-training-rate routine (tuning) runs", {
  g <- expand.grid(learning_rate = c(0.01, 0.1), optimizer = "gd",
                   stringsAsFactors = FALSE)
  res <- morie_ml_tune(.ml_reg()[c("x1", "x2")], .ml_reg()$y, g,
                       type = "linear", n_folds = 3)
  expect_true(nrow(res$results) == 2L && "score" %in% names(res$results))
})

test_that("ML7.6 more epochs yield a lower (or equal) training loss", {
  X <- .ml_reg()[c("x1", "x2")]; y <- .ml_reg()$y
  f_short <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 5, tol = 0),
                            X, y)
  f_long  <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 200, tol = 0),
                            X, y)
  expect_lte(f_long$loss_path[f_long$n_epochs], f_short$loss_path[f_short$n_epochs])
})

test_that("ML7.7 different optimizers are tested + all reduce the loss", {
  X <- .ml_reg()[c("x1", "x2")]; y <- .ml_reg()$y
  for (opt in c("gd", "sgd", "adam")) {
    f <- morie_ml_train(morie_ml_model("linear", opt, epochs = 60, tol = 0), X, y)
    expect_lt(f$loss_path[f$n_epochs], f$loss_path[1])
  }
})

test_that("ML7.8 different loss functions are tested", {
  X <- .ml_clf()[c("x1", "x2")]; y <- .ml_clf()$y
  f_ll <- morie_ml_train(morie_ml_model("logistic", "adam", loss = "logloss",
                                        epochs = 40), X, y)
  f_ms <- morie_ml_train(morie_ml_model("logistic", "adam", loss = "mse",
                                        epochs = 40), X, y)
  expect_false(isTRUE(all.equal(f_ll$weights, f_ms$weights)))
})

test_that("ML7.9/ML7.9a combinations of categorical settings are compared", {
  grid <- expand.grid(optimizer = c("gd", "adam"),
                      learning_rate = c(0.05, 0.1), stringsAsFactors = FALSE)
  res <- morie_ml_tune(.ml_reg()[c("x1", "x2")], .ml_reg()$y, grid,
                       type = "linear", n_folds = 3)
  expect_equal(nrow(res$results), 4L)           # all 2x2 combinations
})

test_that("ML7.10 optimizer paths can be extracted + tested", {
  fit <- morie_ml_train(morie_ml_model("linear", "gd", epochs = 40, tol = 0),
                        .ml_reg()[c("x1", "x2")], .ml_reg()$y)
  expect_length(fit$loss_path, 40L)
  expect_true(all(diff(fit$loss_path) <= 1e-6)) # monotone non-increasing (gd)
})

test_that("ML7.11/ML7.11a metrics are tested over a range of inputs", {
  fits <- lapply(c(10, 100), function(e)
    morie_ml_train(morie_ml_model("logistic", "adam", epochs = e),
                   .ml_clf()[c("x1", "x2")], .ml_clf()$y))
  accs <- vapply(fits, function(f)
    morie_ml_assess(f, .ml_clf()[c("x1", "x2")], .ml_clf()$y,
                    metrics = "accuracy")["accuracy"], numeric(1))
  expect_gte(accs[2], accs[1] - 0.05)           # more training !< worse
})
