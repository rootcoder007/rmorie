# SPDX-License-Identifier: AGPL-3.0-or-later
#
# ml_pipeline.R -- a coherent supervised machine-learning pipeline for
# rmorie: data splitting, a preprocessing recipe stage, a model
# specification stage, an iterative (gradient-descent) training stage
# that exposes optimizers / learning rates / epochs / loss paths, a
# trained-model object with predict / save / load, and performance
# assessment + hyperparameter tuning. Built on base R + stats only.
#
# API surface (every srr ML standard maps onto one of these):
#   morie_ml_split()        train / test / validation partitioning
#   morie_ml_recipe()       preprocessing spec  (+ prep / bake / reverse)
#   morie_ml_center/scale/impute()   exported, reusable transforms
#   morie_ml_model()        model spec (optimizer, lr, epochs, loss; nofit)
#   morie_ml_train()        unified training entry point
#   predict / print / summary .morie_ml_fit
#   morie_ml_save/load()    serialize a trained model
#   morie_ml_assess()       performance metrics (built-in + custom)
#   morie_ml_resample()     combine results across resampling folds
#   morie_ml_tune()         hyperparameter search / model selection
#   morie_ml_train_time()   estimate time to train


#' srr machine-learning (ML) standards
#'
#' rmorie implements a full supervised ML pipeline in this file; the ML
#' standards are addressed against that surface and its tests
#' (test-srr-standards-ML.R). Each pointer names the responsible
#' function or behaviour.
#'
#' @srrstats {ML1.0} morie_ml_split() and its docs distinguish training from test data.
#' @srrstats {ML1.0a} the train/test terminology is used throughout the split documentation.
#' @srrstats {ML1.1} morie_ml_split() labels rows train/test/validation.
#' @srrstats {ML1.1a} the assigned roles are confirmed via the returned `roles` vector.
#' @srrstats {ML1.1b} .ml_match_role() matches role names case-insensitively and by prefix.
#' @srrstats {ML1.2} morie_ml_split() takes one tabular object; the `roles` variable distinguishes partitions.
#' @srrstats {ML1.3} morie_ml_split() returns train/test as distinct list items.
#' @srrstats {ML1.4} partitions are distinctly labelled by role.
#' @srrstats {ML1.5} print.morie_ml_split() summarises per-partition row counts.
#' @srrstats {ML1.6} morie_ml_train() errors informatively on missing input values.
#' @srrstats {ML1.6a} the error documents that missing values must be imputed first.
#' @srrstats {ML1.6b} morie_ml_recipe()/morie_ml_impute() show how to impute rather than discard.
#' @srrstats {ML1.7} morie_ml_impute() documents how missing values are processed.
#' @srrstats {ML1.7a} morie_ml_impute() offers mean/median/constant imputation.
#' @srrstats {ML1.7b} imputation fills are recorded in the prepped recipe params.
#' @srrstats {ML1.8} a prepped recipe applies the same imputation to train and test.
#' @srrstats {ML2.0} morie_ml_recipe() is the dedicated pre-processing specification function.
#' @srrstats {ML2.0a} a baked recipe returns data directly submittable to morie_ml_train().
#' @srrstats {ML2.0b} morie_ml_recipe has a class with a print method summarising steps.
#' @srrstats {ML2.1} applied transformations are recorded in recipe$params.
#' @srrstats {ML2.2} center_target enables explicit numeric transform targets.
#' @srrstats {ML2.2a} default transform values (data mean/sd) are documented on the params.
#' @srrstats {ML2.2b} transform parameters are recorded and inspectable per predictor.
#' @srrstats {ML2.3} transformation values are stored in the returned recipe object.
#' @srrstats {ML2.4} default transformations are documented on morie_ml_recipe parameters.
#' @srrstats {ML2.5} center=FALSE / scale=FALSE switch off default transformations.
#' @srrstats {ML2.6} morie_ml_center/scale/impute are exported for reuse.
#' @srrstats {ML2.7} morie_ml_bake(reverse=TRUE) inverts the transformation.
#' @srrstats {ML3.0} morie_ml_model() specifies the model as a stage distinct from training.
#' @srrstats {ML3.0a} nofit=TRUE returns the spec without fitting.
#' @srrstats {ML3.0b} morie_ml_train() accepts the preprocessing (baked) output.
#' @srrstats {ML3.0c} the morie_ml_model spec is directly trainable by morie_ml_train().
#' @srrstats {ML3.0d} morie_ml_model has a class with a default print method.
#' @srrstats {ML3.1} both untrained specs and trained fits are first-class objects.
#' @srrstats {ML3.2} different model specs apply to the same data (optimizer/loss varied).
#' @srrstats {ML3.3} the morie_ml_fit class behaviour (predict/print) is defined and tested.
#' @srrstats {ML3.4} learning_rate is a documented parameter with a default.
#' @srrstats {ML3.4a} morie_ml_tune() automatically selects the training rate by CV.
#' @srrstats {ML3.4b} learning_rate default and its restrictions are documented.
#' @srrstats {ML3.5} morie_ml_model() exposes optimizer and loss parameters.
#' @srrstats {ML3.5a} optimizer selects the search-space algorithm (gd/sgd/adam).
#' @srrstats {ML3.5b} loss selects the loss function (logloss/mse).
#' @srrstats {ML3.6} multiple search algorithms and losses are permitted.
#' @srrstats {ML3.6a} gd/sgd/adam are all available (.ml_optimizers).
#' @srrstats {ML3.6b} logloss and mse are both available (.ml_loss).
#' @srrstats {ML3.7} the pipeline is pure R with no C++/GPU code, so CPU/GPU control does not arise (condition not triggered).
#' @srrstats {ML4.0} morie_ml_train() is the single unified training interface.
#' @srrstats {ML4.1} the fit retains model-internal parameters and optimisation paths.
#' @srrstats {ML4.1a} fit$weights holds the model-internal parameters.
#' @srrstats {ML4.1b} fit$loss_path holds the loss at each epoch.
#' @srrstats {ML4.1c} fit$grad_norm holds the gradient norm advancing each step.
#' @srrstats {ML4.2} the retained loss_path/grad_norm/weights are directly extractable.
#' @srrstats {ML4.3} batch_size is an explicit documented parameter.
#' @srrstats {ML4.4} the effect of batch size on gradient steps is documented and tested.
#' @srrstats {ML4.5} morie_ml_train_time() estimates the time to train.
#' @srrstats {ML4.6} verbose= provides per-epoch progress, suppressible by default.
#' @srrstats {ML4.7} morie_ml_resample() combines results across resampling folds.
#' @srrstats {ML4.8} morie_ml_resample() partitions data so each case is tested once.
#' @srrstats {ML4.8a} fold assignment is explicit and reproducible via seed.
#' @srrstats {ML5.0} morie_ml_train() returns a trained-model object.
#' @srrstats {ML5.0a} the object has its own class (morie_ml_fit).
#' @srrstats {ML5.0b} print.morie_ml_fit provides a default on-screen summary.
#' @srrstats {ML5.1} the fit carries input metadata (feature_names, n_obs).
#' @srrstats {ML5.2} predict/summary/print methods exercise the fit object's functionality.
#' @srrstats {ML5.2a} the class functionality (predict/summary) is explicitly documented.
#' @srrstats {ML5.2b} morie_ml_save()/morie_ml_load() save and reload the trained model.
#' @srrstats {ML5.2c} serialization uses base saveRDS/readRDS.
#' @srrstats {ML5.3} morie_ml_assess() implements performance assessment as a distinct function.
#' @srrstats {ML5.4} morie_ml_assess() supports several metrics (accuracy/roc_auc/brier/rmse/mae/r2).
#' @srrstats {ML5.4a} metrics are computed consistently through .ml_metric.
#' @srrstats {ML5.4b} custom metrics can be submitted via the `custom` argument.
#' @srrstats {ML6.0} the workflow separates training (morie_ml_train) from prediction (predict).
#' @srrstats {ML6.1} the full split->prep->train->assess workflow is documented and demonstrated.
#' @srrstats {ML6.1a} the end-to-end workflow is demonstrated in tests and examples.
#' @srrstats {ML7.0} tests confirm case-insensitive + partial role matching.
#' @srrstats {ML7.1} tests demonstrate the effect of different input scaling on fitted weights.
#' @srrstats {ML7.2} tests compare internal imputation with an external computation.
#' @srrstats {ML7.3} tests exercise the morie_ml_fit class accessors.
#' @srrstats {ML7.3a} tests identify the documented class restriction (class prediction is logistic-only).
#' @srrstats {ML7.3b} tests exercise the class's predict/summary abilities.
#' @srrstats {ML7.4} tests demonstrate that lower training rates give slower descent.
#' @srrstats {ML7.5} tests exercise the tuning routine that selects training rates.
#' @srrstats {ML7.6} tests demonstrate more epochs yield lower training loss.
#' @srrstats {ML7.7} tests exercise gd/sgd/adam optimizers, each reducing the loss.
#' @srrstats {ML7.8} tests exercise different loss functions.
#' @srrstats {ML7.9} tests compare all combinations of categorical settings via a grid.
#' @srrstats {ML7.9a} the grid is formed with expand.grid over categorical factors.
#' @srrstats {ML7.10} tests extract and check optimizer paths (loss_path monotonicity).
#' @srrstats {ML7.11} tests assess performance metrics over a range of trained models.
#' @srrstats {ML7.11a} metric comparison spans differently-trained models.
#' @noRd
NULL

# ============================================================
# Role labels (ML1.1): case-insensitive partial matching
# ============================================================

#' Internal helper: Ml Match Role
#' @noRd
.ml_roles <- c("train", "test", "validation")

#' Internal helper: Ml Match Role
#' @noRd
.ml_match_role <- function(x) {
  # partial + case-insensitive: "Training"/"tst"/"valid" all resolve
  x <- tolower(trimws(as.character(x)))
  out <- vapply(x, function(v) {
    hit <- .ml_roles[startsWith(.ml_roles, v) | startsWith(v, .ml_roles)]
    if (length(hit) == 1L) hit else NA_character_
  }, character(1))
  out
}

# ============================================================
# ML1: data splitting into train / test / validation
# ============================================================

#' Partition data into train / test / validation sets
#'
#' Splits a single tabular data object into distinctly-labelled
#' training, test, and (optionally) validation partitions, with optional
#' stratification on a column.
#'
#' @param data A data.frame.
#' @param prop Named numeric of partition proportions; names are matched
#'   case-insensitively and by partial match to "train"/"test"/
#'   "validation" (ML1.1b). Must sum to 1.
#' @param strata Optional column name to stratify the split on.
#' @param seed RNG seed for reproducibility.
#' @return An object of class `morie_ml_split`: a list with `train`,
#'   `test`, and (if requested) `validation` data.frames plus a `roles`
#'   vector giving each row's assignment.
#' @examples
#' sp <- morie_ml_split(mtcars, c(train = 0.6, test = 0.4), seed = 1)
#' nrow(sp$train) + nrow(sp$test)
#' @export
morie_ml_split <- function(data,
                           prop = c(train = 0.6, test = 0.2,
                                    validation = 0.2),
                           strata = NULL, seed = 42L) {
  data <- .morie_check_data(data, arg = "data")
  if (is.null(names(prop))) stop("`prop` must be named", call. = FALSE)
  names(prop) <- .ml_match_role(names(prop))
  if (anyNA(names(prop))) stop("`prop` names must match train/test/validation",
                               call. = FALSE)
  if (abs(sum(prop) - 1) > 1e-8) stop("`prop` must sum to 1", call. = FALSE)
  if (!is.null(strata) && !strata %in% names(data)) {
    stop(sprintf("strata column '%s' not found", strata), call. = FALSE)
  }
  set.seed(seed)
  n <- nrow(data)
  if (is.null(strata)) {
    # deterministic proportional counts, then shuffle the assignment
    counts <- floor(prop * n); counts[1] <- n - sum(counts[-1])
    roles <- sample(rep(names(prop), counts))
  } else {
    roles <- character(n)
    for (lev in unique(data[[strata]])) {
      idx <- which(data[[strata]] == lev)
      counts <- floor(prop * length(idx))
      counts[1] <- length(idx) - sum(counts[-1])
      roles[idx] <- sample(rep(names(prop), counts))
    }
  }
  out <- list(roles = roles)
  for (r in names(prop)) out[[r]] <- data[roles == r, , drop = FALSE]
  out$n <- n
  out$prop <- prop
  class(out) <- c("morie_ml_split", "morie_rich_result", "list")
  out
}

#' @param x A `morie_ml_split`.
#' @param ... Unused.
#' @return `x`, invisibly (ML1.5 dataset summary).
#' @export
print.morie_ml_split <- function(x, ...) {
  cat("<morie_ml_split>\n")
  for (r in intersect(.ml_roles, names(x))) {
    cat(sprintf("  %-11s %d rows (%.0f%%)\n", r, nrow(x[[r]]),
                100 * nrow(x[[r]]) / x$n))
  }
  cat(sprintf("  total       %d rows\n", x$n))
  invisible(x)
}

# ============================================================
# ML2: preprocessing recipe stage
# ============================================================

#' Mean/median/constant imputation of a numeric vector
#' @param x Numeric vector.
#' @param method One of "mean", "median", "constant".
#' @param value Fill value when `method = "constant"`.
#' @return Imputed numeric vector.
#' @examples
#' morie_ml_impute(c(1, NA, 3), "mean")
#' @export
morie_ml_impute <- function(x, method = c("mean", "median", "constant"),
                            value = 0) {
  method <- match.arg(method)
  x <- as.numeric(x)
  fill <- switch(method,
    mean = mean(x, na.rm = TRUE),
    median = stats::median(x, na.rm = TRUE),
    constant = value)
  x[is.na(x)] <- fill
  x
}

#' Center a numeric vector to a target mean
#' @param x Numeric vector.
#' @param center Target value subtracted (default the sample mean).
#' @return Centered numeric vector, carrying a `center` attribute so the
#'   transform can be reversed.
#' @examples
#' morie_ml_center(1:5)
#' @export
morie_ml_center <- function(x, center = mean(x, na.rm = TRUE)) {
  out <- as.numeric(x) - center
  attr(out, "center") <- center
  out
}

#' Scale a numeric vector by a target standard deviation
#' @param x Numeric vector.
#' @param scale Divisor (default the sample SD).
#' @return Scaled numeric vector, carrying a `scale` attribute.
#' @examples
#' morie_ml_scale(1:5)
#' @export
morie_ml_scale <- function(x, scale = stats::sd(x, na.rm = TRUE)) {
  if (isTRUE(scale == 0) || is.na(scale)) scale <- 1
  out <- as.numeric(x) / scale
  attr(out, "scale") <- scale
  out
}

#' Specify a preprocessing recipe
#'
#' A recipe records which transformations to apply to the predictors;
#' `morie_ml_prep()` estimates their parameters from training data and
#' `morie_ml_bake()` applies (or reverses) them. This is the dedicated
#' pre-processing-specification stage: it is defined and parametrised
#' separately from model fitting, records the values of all
#' transformations, and can be switched off.
#'
#' @param outcome Name of the response column.
#' @param predictors Character vector of predictor columns (default: all
#'   other numeric columns).
#' @param impute One of "none", "mean", "median" (ML1.7a; default "mean").
#' @param center Logical; center predictors (ML2.2). Default TRUE.
#' @param scale Logical; scale predictors to unit SD. Default TRUE.
#' @param center_target Optional named numeric of explicit target means
#'   (ML2.2); overrides the data mean where supplied.
#' @return A `morie_ml_recipe` specification object.
#' @examples
#' rec <- morie_ml_recipe("mpg", c("hp", "wt"))
#' rec <- morie_ml_prep(rec, mtcars)
#' head(morie_ml_bake(rec, mtcars))
#' @export
morie_ml_recipe <- function(outcome, predictors = NULL,
                            impute = c("mean", "median", "none"),
                            center = TRUE, scale = TRUE,
                            center_target = NULL) {
  impute <- match.arg(impute)
  rec <- list(outcome = outcome, predictors = predictors,
              impute = impute, center = center, scale = scale,
              center_target = center_target, params = NULL, prepped = FALSE)
  class(rec) <- c("morie_ml_recipe", "morie_rich_result", "list")
  rec
}

#' Estimate recipe parameters from training data
#' @param recipe A `morie_ml_recipe`.
#' @param data Training data.frame.
#' @return The recipe with estimated `params` and `prepped = TRUE`.
#' @examples
#' morie_ml_prep(morie_ml_recipe("mpg", "hp"), mtcars)
#' @export
morie_ml_prep <- function(recipe, data) {
  stopifnot(inherits(recipe, "morie_ml_recipe"))
  data <- .morie_check_data(data, required = recipe$outcome, arg = "data")
  preds <- recipe$predictors
  if (is.null(preds)) {
    preds <- setdiff(names(data)[vapply(data, is.numeric, logical(1))],
                     recipe$outcome)
  }
  p <- list()
  for (v in preds) {
    col <- morie_ml_impute(data[[v]],
                           if (recipe$impute == "none") "constant"
                           else recipe$impute)
    ctr <- if (isTRUE(recipe$center)) {
      if (!is.null(recipe$center_target) && v %in% names(recipe$center_target))
        recipe$center_target[[v]] else mean(col)
    } else 0
    scl <- if (isTRUE(recipe$scale)) {
      s <- stats::sd(col); if (is.na(s) || s == 0) 1 else s
    } else 1
    p[[v]] <- list(impute = if (recipe$impute == "none") NA_real_
                            else morie_ml_impute(data[[v]], recipe$impute)[
                              which(is.na(data[[v]]))[1]],
                   impute_fill = switch(recipe$impute,
                     mean = mean(data[[v]], na.rm = TRUE),
                     median = stats::median(data[[v]], na.rm = TRUE),
                     none = NA_real_),
                   center = ctr, scale = scl)
  }
  recipe$predictors <- preds
  recipe$params <- p
  recipe$prepped <- TRUE
  recipe
}

#' Apply (or reverse) a prepped recipe to data
#' @param recipe A prepped `morie_ml_recipe`.
#' @param newdata Data to transform.
#' @param reverse If TRUE, invert the transformation (ML2.7).
#' @return A data.frame of transformed predictors (plus the outcome when
#'   present).
#' @examples
#' rec <- morie_ml_prep(morie_ml_recipe("mpg", "hp"), mtcars)
#' baked <- morie_ml_bake(rec, mtcars)
#' back  <- morie_ml_bake(rec, baked, reverse = TRUE)
#' @export
morie_ml_bake <- function(recipe, newdata, reverse = FALSE) {
  stopifnot(inherits(recipe, "morie_ml_recipe"))
  if (!isTRUE(recipe$prepped)) stop("recipe is not prepped; call morie_ml_prep()",
                                    call. = FALSE)
  newdata <- as.data.frame(newdata)
  out <- newdata
  for (v in recipe$predictors) {
    pv <- recipe$params[[v]]
    if (!isTRUE(reverse)) {
      col <- newdata[[v]]
      if (recipe$impute != "none") col[is.na(col)] <- pv$impute_fill
      out[[v]] <- (col - pv$center) / pv$scale
    } else {
      out[[v]] <- newdata[[v]] * pv$scale + pv$center
    }
  }
  out
}

#' @param x A `morie_ml_recipe`.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.morie_ml_recipe <- function(x, ...) {
  cat("<morie_ml_recipe>\n")
  cat(sprintf("  outcome:    %s\n", x$outcome))
  cat(sprintf("  predictors: %s\n",
              if (is.null(x$predictors)) "<all numeric>"
              else paste(x$predictors, collapse = ", ")))
  cat(sprintf("  steps:      impute=%s center=%s scale=%s\n",
              x$impute, x$center, x$scale))
  cat(sprintf("  prepped:    %s\n", x$prepped))
  invisible(x)
}

# ============================================================
# ML3: model specification + ML4: iterative training core
# ============================================================

#' Internal helper: Ml Loss
#' @noRd
.ml_loss <- list(
  logloss = list(
    link = function(z) 1 / (1 + exp(-z)),
    loss = function(p, y) -mean(y * log(p + 1e-12) +
                                (1 - y) * log(1 - p + 1e-12)),
    grad = function(X, p, y) crossprod(X, p - y) / length(y)
  ),
  mse = list(
    link = function(z) z,
    loss = function(p, y) mean((p - y)^2),
    grad = function(X, p, y) 2 * crossprod(X, p - y) / length(y)
  )
)

#' Internal helper: Ml Optimizers
#' @noRd
.ml_optimizers <- c("gd", "sgd", "adam")

#' Specify a supervised model without fitting it
#'
#' Defines the learner, its optimizer, learning rate, number of epochs,
#' loss function, and regularisation, as a distinct stage prior to
#' training. With `nofit = TRUE` (the default here is FALSE) the object is
#' returned without being fitted, enabling batch control of expensive fits.
#'
#' @param type One of "logistic" (classification) or "linear"
#'   (regression). Distinguishes the default loss.
#' @param optimizer One of "gd" (full-batch gradient descent), "sgd"
#'   (stochastic minibatch), or "adam".
#' @param learning_rate Step size (default 0.1). Lower values give longer
#'   but more stable descent (ML7.4).
#' @param epochs Number of passes over the data (default 200).
#' @param loss Loss name; defaults to "logloss" for logistic and "mse"
#'   for linear. Custom losses may be supplied by name in `.ml_loss`.
#' @param batch_size Minibatch size for "sgd" (default 32).
#' @param l2 Ridge (L2) penalty (default 0).
#' @param tol Convergence tolerance on the loss (default 1e-8).
#' @param seed RNG seed.
#' @param nofit If TRUE, `morie_ml_train` returns the untrained spec.
#' @return A `morie_ml_model` specification.
#' @examples
#' morie_ml_model("logistic", optimizer = "adam", epochs = 50)
#' @export
morie_ml_model <- function(type = c("logistic", "linear"),
                           optimizer = c("adam", "gd", "sgd"),
                           learning_rate = 0.1, epochs = 200L,
                           loss = NULL, batch_size = 32L, l2 = 0,
                           tol = 1e-8, seed = 42L, nofit = FALSE) {
  type <- match.arg(type)
  optimizer <- match.arg(optimizer)
  if (is.null(loss)) loss <- if (type == "logistic") "logloss" else "mse"
  if (!loss %in% names(.ml_loss)) {
    stop(sprintf("unknown loss '%s'; known: %s", loss,
                 paste(names(.ml_loss), collapse = ", ")), call. = FALSE)
  }
  .morie_check_scalar(learning_rate, "numeric", "learning_rate")
  if (learning_rate <= 0) stop("`learning_rate` must be > 0", call. = FALSE)
  if (epochs <= 0) stop("`epochs` must be > 0", call. = FALSE)
  spec <- list(type = type, optimizer = optimizer,
               learning_rate = learning_rate, epochs = as.integer(epochs),
               loss = loss, batch_size = as.integer(batch_size), l2 = l2,
               tol = tol, seed = seed, nofit = nofit)
  class(spec) <- c("morie_ml_model", "morie_rich_result", "list")
  spec
}

#' @param x A `morie_ml_model`.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.morie_ml_model <- function(x, ...) {
  cat("<morie_ml_model>\n")
  cat(sprintf("  type=%s loss=%s optimizer=%s lr=%g epochs=%d l2=%g\n",
              x$type, x$loss, x$optimizer, x$learning_rate, x$epochs, x$l2))
  invisible(x)
}

#' Internal helper: Ml Design
#' @noRd
.ml_design <- function(x) {
  X <- if (is.data.frame(x)) as.matrix(x[vapply(x, is.numeric, logical(1))])
       else as.matrix(x)
  storage.mode(X) <- "double"
  cbind(`(Intercept)` = 1, X)
}

#' Train a specified model
#'
#' The single unified training entry point. Runs the iterative optimiser
#' declared in the model spec, recording the loss at each epoch and the
#' gradient norm at each step, and returns a trained-model object.
#'
#' @param model A `morie_ml_model` specification.
#' @param x Predictor matrix or data.frame (numeric columns used).
#' @param y Response vector (0/1 for logistic).
#' @param verbose Print per-epoch progress (default FALSE). Progress can
#'   thus be suppressed while warnings/errors are retained.
#' @return A `morie_ml_fit` object (or the untrained spec if
#'   `model$nofit`), carrying `weights`, `loss_path`, `grad_norm`,
#'   `converged`, and the input metadata.
#' @examples
#' m <- morie_ml_model("linear", optimizer = "gd", epochs = 100)
#' fit <- morie_ml_train(m, mtcars[c("hp", "wt")], mtcars$mpg)
#' @export
morie_ml_train <- function(model, x, y, verbose = FALSE) {
  stopifnot(inherits(model, "morie_ml_model"))
  if (isTRUE(model$nofit)) return(model)
  X <- .ml_design(x)
  y <- as.numeric(y)
  if (nrow(X) != length(y)) stop("`x` and `y` lengths differ", call. = FALSE)
  if (anyNA(X) || anyNA(y)) stop("`x`/`y` contain missing values; impute first",
                                 call. = FALSE)
  L <- .ml_loss[[model$loss]]
  set.seed(model$seed)
  w <- rep(0, ncol(X))
  loss_path <- numeric(model$epochs)
  grad_norm <- numeric(0)
  m_adam <- v_adam <- rep(0, ncol(X)); b1 <- 0.9; b2 <- 0.999; t_adam <- 0
  converged <- FALSE
  for (ep in seq_len(model$epochs)) {
    if (model$optimizer == "sgd") {
      idx_batches <- split(sample(nrow(X)),
                           ceiling(seq_len(nrow(X)) / model$batch_size))
    } else {
      idx_batches <- list(seq_len(nrow(X)))
    }
    for (bi in idx_batches) {
      Xb <- X[bi, , drop = FALSE]; yb <- y[bi]
      p <- L$link(as.numeric(Xb %*% w))
      g <- as.numeric(L$grad(Xb, p, yb)) + model$l2 * w
      grad_norm <- c(grad_norm, sqrt(sum(g^2)))
      if (model$optimizer == "adam") {
        t_adam <- t_adam + 1
        m_adam <- b1 * m_adam + (1 - b1) * g
        v_adam <- b2 * v_adam + (1 - b2) * g^2
        mhat <- m_adam / (1 - b1^t_adam); vhat <- v_adam / (1 - b2^t_adam)
        w <- w - model$learning_rate * mhat / (sqrt(vhat) + 1e-8)
      } else {
        w <- w - model$learning_rate * g
      }
    }
    p_full <- L$link(as.numeric(X %*% w))
    loss_path[ep] <- L$loss(p_full, y) + model$l2 * sum(w^2) / 2
    if (verbose) message(sprintf("epoch %d loss %.6f", ep, loss_path[ep]))
    if (ep > 1 && abs(loss_path[ep - 1] - loss_path[ep]) < model$tol) {
      loss_path <- loss_path[seq_len(ep)]; converged <- TRUE; break
    }
  }
  fit <- list(weights = stats::setNames(w, colnames(X)),
              loss_path = loss_path, grad_norm = grad_norm,
              n_epochs = length(loss_path), converged = converged,
              type = model$type, loss = model$loss,
              optimizer = model$optimizer, spec = model,
              feature_names = colnames(X)[-1], n_obs = nrow(X))
  class(fit) <- c("morie_ml_fit", "morie_rich_result", "list")
  fit
}

#' Predict from a trained model
#' @param object A `morie_ml_fit`.
#' @param newdata Predictor matrix/data.frame with the training columns.
#' @param type "response" (probabilities / fitted values) or "class"
#'   (0/1, logistic only).
#' @param ... Unused.
#' @return Numeric vector of predictions.
#' @examples
#' m <- morie_ml_train(morie_ml_model("linear", "gd"),
#'                     mtcars[c("hp", "wt")], mtcars$mpg)
#' predict(m, mtcars[c("hp", "wt")])[1:3]
#' @export
predict.morie_ml_fit <- function(object, newdata, type = c("response", "class"),
                                 ...) {
  type <- match.arg(type)
  X <- .ml_design(newdata)
  p <- .ml_loss[[object$loss]]$link(as.numeric(X %*% object$weights))
  if (type == "class") {
    if (object$type != "logistic") stop("class prediction is logistic-only",
                                         call. = FALSE)
    return(as.integer(p >= 0.5))
  }
  p
}

#' @param x A `morie_ml_fit`.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.morie_ml_fit <- function(x, ...) {
  cat("<morie_ml_fit>\n")
  cat(sprintf("  type=%s loss=%s optimizer=%s\n", x$type, x$loss, x$optimizer))
  cat(sprintf("  epochs run: %d   converged: %s\n", x$n_epochs, x$converged))
  cat(sprintf("  final loss: %.6f\n", x$loss_path[x$n_epochs]))
  cat(sprintf("  features:   %s\n", paste(x$feature_names, collapse = ", ")))
  invisible(x)
}

#' @param object A `morie_ml_fit`.
#' @param ... Unused.
#' @return A one-row data.frame summarising the fit.
#' @export
summary.morie_ml_fit <- function(object, ...) {
  data.frame(type = object$type, loss = object$loss,
             optimizer = object$optimizer, n_obs = object$n_obs,
             n_epochs = object$n_epochs, converged = object$converged,
             final_loss = object$loss_path[object$n_epochs],
             stringsAsFactors = FALSE)
}

# ============================================================
# ML5: serialization + assessment
# ============================================================

#' Save a trained model to disk
#' @param fit A `morie_ml_fit`.
#' @param path File path (`.rds`). A `.rds` suffix is appended if absent.
#' @return The path written, invisibly.
#' @examples
#' m <- morie_ml_train(morie_ml_model("linear", "gd"),
#'                     mtcars[c("hp", "wt")], mtcars$mpg)
#' f <- file.path(tempdir(), "model")
#' morie_ml_save(m, f)
#' @export
morie_ml_save <- function(fit, path) {
  stopifnot(inherits(fit, "morie_ml_fit"))
  if (!grepl("\\.rds$", path, ignore.case = TRUE)) path <- paste0(path, ".rds")
  saveRDS(fit, path)
  invisible(path)
}

#' Load a trained model from disk
#' @param path File path written by [morie_ml_save()].
#' @return The restored `morie_ml_fit`.
#' @examples
#' m <- morie_ml_train(morie_ml_model("linear", "gd"),
#'                     mtcars[c("hp", "wt")], mtcars$mpg)
#' f <- morie_ml_save(m, file.path(tempdir(), "model"))
#' identical(class(morie_ml_load(f)), class(m))
#' @export
morie_ml_load <- function(path) {
  if (!grepl("\\.rds$", path, ignore.case = TRUE)) path <- paste0(path, ".rds")
  obj <- readRDS(path)
  if (!inherits(obj, "morie_ml_fit")) stop("file is not a morie_ml_fit",
                                           call. = FALSE)
  obj
}

#' Internal helper: Ml Metric
#' @noRd
.ml_metric <- function(name, y, pred) {
  switch(name,
    accuracy = mean((pred >= 0.5) == (y == 1)),
    roc_auc  = .val_auc(y, pred),
    brier    = mean((pred - y)^2),
    rmse     = sqrt(mean((pred - y)^2)),
    mae      = mean(abs(pred - y)),
    r2       = 1 - sum((y - pred)^2) / sum((y - mean(y))^2),
    stop(sprintf("unknown metric '%s'", name), call. = FALSE))
}

#' Assess model performance
#'
#' Model-performance assessment implemented as a function distinct from
#' training. Supports several built-in metrics and user-supplied custom
#' metrics.
#'
#' @param fit A `morie_ml_fit`.
#' @param x Predictors.
#' @param y True responses.
#' @param metrics Character vector of built-in metric names
#'   ("accuracy", "roc_auc", "brier", "rmse", "mae", "r2").
#' @param custom Optional named list of `function(y, pred)` custom metrics
#'   (ML5.4b).
#' @return A named numeric vector of metric values.
#' @examples
#' m <- morie_ml_train(morie_ml_model("linear", "gd"),
#'                     mtcars[c("hp", "wt")], mtcars$mpg)
#' morie_ml_assess(m, mtcars[c("hp", "wt")], mtcars$mpg,
#'                 metrics = c("rmse", "r2"))
#' @export
morie_ml_assess <- function(fit, x, y,
                            metrics = if (fit$type == "logistic")
                              c("accuracy", "roc_auc", "brier")
                            else c("rmse", "mae", "r2"),
                            custom = NULL) {
  y <- as.numeric(y)
  pred <- predict(fit, x, type = "response")
  vals <- vapply(metrics, function(m) .ml_metric(m, y, pred), numeric(1))
  if (!is.null(custom)) {
    cv <- vapply(names(custom), function(nm) custom[[nm]](y, pred), numeric(1))
    vals <- c(vals, cv)
  }
  vals
}

# ============================================================
# ML4.7/4.8: resampling  +  ML3.4: tuning / model selection
# ============================================================

#' Train + assess across resampling folds and combine the results
#' @param model A `morie_ml_model`.
#' @param x Predictors.
#' @param y Response.
#' @param n_folds Number of folds (default 5). Data is partitioned so
#'   each observation is used for testing exactly once (ML4.8).
#' @param metric Metric to report per fold.
#' @param seed RNG seed.
#' @return A list with per-fold `scores` and their `mean`/`sd`.
#' @examples
#' morie_ml_resample(morie_ml_model("linear", "gd", epochs = 50),
#'                   mtcars[c("hp", "wt")], mtcars$mpg, n_folds = 3)$mean
#' @export
morie_ml_resample <- function(model, x, y, n_folds = 5L,
                              metric = if (model$type == "logistic") "roc_auc"
                                       else "rmse", seed = 42L) {
  x <- as.data.frame(x); y <- as.numeric(y)
  set.seed(seed)
  folds <- sample(rep(seq_len(n_folds), length.out = nrow(x)))
  scores <- vapply(seq_len(n_folds), function(k) {
    tr <- folds != k; te <- folds == k
    fit <- morie_ml_train(model, x[tr, , drop = FALSE], y[tr])
    unname(morie_ml_assess(fit, x[te, , drop = FALSE], y[te], metrics = metric))
  }, numeric(1))
  list(scores = scores, mean = mean(scores), sd = stats::sd(scores),
       metric = metric, n_folds = n_folds)
}

#' Tune hyperparameters by cross-validated grid search
#'
#' Selects the model configuration (learning rate, optimizer, epochs,
#' L2) minimising / maximising a resampled metric.
#'
#' @param x Predictors.
#' @param y Response.
#' @param grid A data.frame whose columns are `morie_ml_model` arguments;
#'   each row is a candidate configuration.
#' @param type "logistic" or "linear".
#' @param n_folds Resampling folds.
#' @param metric Metric to optimise.
#' @param maximize Whether larger metric is better (default: TRUE for
#'   roc_auc/accuracy/r2, FALSE otherwise).
#' @param seed RNG seed.
#' @return A list with the full `results` table and the `best` row.
#' @examples
#' g <- expand.grid(learning_rate = c(0.01, 0.1), optimizer = "gd",
#'                  stringsAsFactors = FALSE)
#' morie_ml_tune(mtcars[c("hp", "wt")], mtcars$mpg, g,
#'               type = "linear", n_folds = 3)$best
#' @export
morie_ml_tune <- function(x, y, grid, type = "linear", n_folds = 5L,
                          metric = if (type == "logistic") "roc_auc" else "rmse",
                          maximize = metric %in% c("roc_auc", "accuracy", "r2"),
                          seed = 42L) {
  grid <- as.data.frame(grid, stringsAsFactors = FALSE)
  score <- vapply(seq_len(nrow(grid)), function(i) {
    args <- c(list(type = type), as.list(grid[i, , drop = FALSE]))
    spec <- do.call(morie_ml_model, args)
    morie_ml_resample(spec, x, y, n_folds = n_folds, metric = metric,
                      seed = seed)$mean
  }, numeric(1))
  results <- cbind(grid, score = score)
  best_i <- if (maximize) which.max(score) else which.min(score)
  list(results = results, best = results[best_i, , drop = FALSE],
       metric = metric, maximize = maximize)
}

#' Estimate time to train a model
#' @param model A `morie_ml_model`.
#' @param x Predictors.
#' @param y Response.
#' @param probe_epochs Epochs to time for the estimate (default 5).
#' @return Estimated seconds to run the full `model$epochs`.
#' @examples
#' morie_ml_train_time(morie_ml_model("linear", "gd", epochs = 100),
#'                     mtcars[c("hp", "wt")], mtcars$mpg)
#' @export
morie_ml_train_time <- function(model, x, y, probe_epochs = 5L) {
  probe <- model; probe$epochs <- as.integer(probe_epochs); probe$tol <- 0
  t0 <- proc.time()[["elapsed"]]
  morie_ml_train(probe, x, y)
  per_epoch <- (proc.time()[["elapsed"]] - t0) / probe_epochs
  per_epoch * model$epochs
}
