# SPDX-License-Identifier: AGPL-3.0-or-later
#' Data and model validation framework
#'
#' R port of the Python module \code{morie.validation}: schema validation,
#' data quality scoring, cross-validation, calibration / discrimination /
#' decision-curve analysis, overfitting detection, temporal / external
#' validation, and reproducibility manifests.
#'
#' Most callables return a named list (class
#' \code{"morie_validation_result"}) so the R caller does not need
#' \pkg{S4} or \pkg{R6}. Model-fitting routines accept a user-supplied
#' \code{fit_fn} of signature \code{function(X, y) -> model} and a
#' \code{predict_fn} of signature \code{function(model, X) -> prob}; this
#' keeps the port framework-agnostic (works with \pkg{glm}, \pkg{glmnet},
#' \pkg{randomForest}, \pkg{xgboost}, etc.).
#'
#' @name validation
NULL


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.val_result <- function(class_name = NULL, ...) {
  out <- list(...)
  class(out) <- c(class_name,
                  "morie_validation_result", "morie_rich_result", "list")
  out
}

.val_auc <- function(y_true, y_pred) {
  # Mann-Whitney-based AUC; ties contribute 0.5
  y_true <- as.integer(y_true)
  pos <- y_pred[y_true == 1L]
  neg <- y_pred[y_true == 0L]
  if (length(pos) == 0L || length(neg) == 0L) return(NA_real_)
  r <- rank(c(pos, neg))
  (sum(r[seq_along(pos)]) - length(pos) * (length(pos) + 1) / 2) /
    (length(pos) * length(neg))
}

.val_brier <- function(y_true, y_pred) {
  mean((y_pred - as.numeric(y_true))^2)
}

.val_score <- function(scoring, y_true, y_pred) {
  switch(scoring,
    "roc_auc"  = .val_auc(y_true, y_pred),
    "auc"      = .val_auc(y_true, y_pred),
    "accuracy" = mean((y_pred >= 0.5) == as.logical(y_true)),
    "brier"    = -.val_brier(y_true, y_pred),  # negate so higher is better
    stop(sprintf("Unknown scoring: %s", scoring)))
}


# ===========================================================================
# Schema validation
# ===========================================================================

#' Construct a column rule
#'
#' @param name Column name.
#' @param dtype One of "numeric", "character"/"object", "datetime", or NULL.
#' @param required Whether the column must be present.
#' @param nullable Whether NA values are allowed.
#' @param null_threshold Maximum allowed fraction of NA (0--1).
#' @param min_val,max_val Numeric bounds (or NULL).
#' @param allowed_values Vector of permitted values (or NULL).
#' @param unique Logical; whether values must be unique.
#' @param regex Regex pattern for string columns (or NULL).
#' @param custom Optional function \code{(column) -> logical(1)}.
#' @return A \code{column_rule} list.
#' @export
column_rule <- function(name, dtype = NULL, required = TRUE,
                         nullable = TRUE, null_threshold = 1.0,
                         min_val = NULL, max_val = NULL,
                         allowed_values = NULL, unique = FALSE,
                         regex = NULL, custom = NULL) {
  structure(
    list(name = name, dtype = dtype, required = required,
         nullable = nullable, null_threshold = null_threshold,
         min_val = min_val, max_val = max_val,
         allowed_values = allowed_values, unique = unique,
         regex = regex, custom = custom),
    class = c("morie_column_rule", "list")
  )
}

#' Validate a data frame against a list of column rules
#' @param data A data frame.
#' @param rules List of \code{column_rule} objects.
#' @param raise_on_error If TRUE, throw on first error.
#' @export
validate_schema <- function(data, rules, raise_on_error = FALSE) {
  errors <- character(0)
  warnings_ <- character(0)
  passed <- TRUE

  for (rule in rules) {
    if (!(rule$name %in% names(data))) {
      if (isTRUE(rule$required)) {
        msg <- sprintf("Missing required column: '%s'", rule$name)
        errors <- c(errors, msg)
        passed <- FALSE
        if (raise_on_error) stop(msg)
      }
      next
    }
    col <- data[[rule$name]]

    # dtype
    if (!is.null(rule$dtype)) {
      if (rule$dtype == "numeric" && !is.numeric(col)) {
        errors <- c(errors, sprintf("Column '%s' expected numeric, got %s",
                                    rule$name, class(col)[1]))
                                    passed <- FALSE
      } else if (rule$dtype %in% c("character", "object") &&
                 !is.character(col) && !is.factor(col)) {
        warnings_ <- c(warnings_,
          sprintf("Column '%s' expected character, got %s",
                  rule$name, class(col)[1]))
      } else if (rule$dtype == "datetime" &&
                 !inherits(col, c("POSIXct", "POSIXlt", "Date"))) {
        errors <- c(errors, sprintf("Column '%s' expected datetime, got %s",
                                    rule$name, class(col)[1]))
                                    passed <- FALSE
      }
    }
    # null checks
    nfrac <- mean(is.na(col))
    if (!isTRUE(rule$nullable) && nfrac > 0) {
      errors <- c(errors,
        sprintf("Column '%s' has %d NA but nullable=FALSE",
                rule$name, sum(is.na(col))))
                passed <- FALSE
    }
    if (nfrac > rule$null_threshold) {
      errors <- c(errors,
        sprintf("Column '%s' null fraction %.3f exceeds threshold %.3f",
                rule$name, nfrac, rule$null_threshold))
                passed <- FALSE
    }
    # range
    if (!is.null(rule$min_val) && is.numeric(col)) {
      v <- sum(col[!is.na(col)] < rule$min_val)
      if (v > 0) {
        errors <- c(errors, sprintf(
          "Column '%s': %d values below min=%s", rule$name, v, rule$min_val))
        passed <- FALSE
      }
    }
    if (!is.null(rule$max_val) && is.numeric(col)) {
      v <- sum(col[!is.na(col)] > rule$max_val)
      if (v > 0) {
        errors <- c(errors, sprintf(
          "Column '%s': %d values above max=%s", rule$name, v, rule$max_val))
        passed <- FALSE
      }
    }
    # allowed values
    if (!is.null(rule$allowed_values)) {
      bad <- col[!is.na(col) & !(col %in% rule$allowed_values)]
      if (length(bad) > 0) {
        errors <- c(errors, sprintf(
          "Column '%s': %d values not in allowed set. Examples: %s",
          rule$name, length(bad),
          paste(utils::head(unique(bad), 5), collapse = ", ")))
        passed <- FALSE
      }
    }
    # uniqueness
    if (isTRUE(rule$unique)) {
      nd <- sum(duplicated(col[!is.na(col)]))
      if (nd > 0) {
        errors <- c(errors, sprintf(
          "Column '%s' has %d duplicate values (unique=TRUE)",
          rule$name, nd))
          passed <- FALSE
      }
    }
    # regex
    if (!is.null(rule$regex) && (is.character(col) || is.factor(col))) {
      m <- grepl(rule$regex, as.character(col[!is.na(col)]))
      if (any(!m)) {
        errors <- c(errors, sprintf(
          "Column '%s': %d values don't match regex '%s'",
          rule$name, sum(!m), rule$regex))
          passed <- FALSE
      }
    }
    # custom
    if (!is.null(rule$custom)) {
      ok <- tryCatch(isTRUE(rule$custom(col)),
                     error = function(e) {
                       errors <<- c(errors, sprintf(
                         "Column '%s': custom validation raised %s",
                         rule$name, conditionMessage(e)))
                       passed <<- FALSE
                       FALSE
                     })
      if (!ok && length(errors) == 0L) {
        errors <- c(errors,
                    sprintf("Column '%s': custom validation failed", rule$name))
        passed <- FALSE
      }
    }
  }
  if (raise_on_error && !passed)
    stop(sprintf("Schema validation failed: %s", paste(errors, collapse = "; ")))
  .val_result("morie_schema_result",
              passed = passed, errors = errors, warnings = warnings_)
}

#' Check referential integrity (child FK -> parent PK)
#' @param child Data frame with foreign key.
#' @param parent Data frame with primary key.
#' @param child_key,parent_key Column names.
#' @export
check_referential_integrity <- function(child, parent, child_key, parent_key) {
  pv <- unique(parent[[parent_key]])
  pv <- pv[!is.na(pv)]
  cv <- child[[child_key]]
  cv <- cv[!is.na(cv)]
  orphans <- cv[!(cv %in% pv)]
  if (length(orphans) > 0) {
    return(.val_result("morie_schema_result", passed = FALSE,
      errors = sprintf(
        "%d orphan rows in '%s' not in '%s'. Examples: %s",
        length(orphans), child_key, parent_key,
        paste(utils::head(unique(orphans), 5), collapse = ", ")),
      warnings = character(0)))
  }
  .val_result("morie_schema_result", passed = TRUE,
              errors = character(0), warnings = character(0))
}


# ===========================================================================
# Data quality scoring
# ===========================================================================

#' Multi-dimensional data quality scores
#' @param data Data frame.
#' @param date_cols Datetime column names for timeliness.
#' @param freshness_days Days for full timeliness score.
#' @param key_cols Columns that should be unique together.
#' @param consistency_rules List of functions \code{(df) -> logical(1)}.
#' @export
score_data_quality <- function(data, date_cols = NULL, freshness_days = 365L,
                                key_cols = NULL, consistency_rules = NULL) {
  details <- list()
  total <- nrow(data) * ncol(data)
  non_null <- sum(!is.na(data))
  completeness <- if (total > 0) non_null / total else 0
  details$null_counts <- vapply(data, function(c) sum(is.na(c)), 0L)

  if (!is.null(consistency_rules) && length(consistency_rules) > 0L) {
    n_pass <- sum(vapply(consistency_rules,
                         function(fn) isTRUE(fn(data)), logical(1)))
    consistency <- n_pass / length(consistency_rules)
    details$consistency_checks <- length(consistency_rules)
    details$consistency_passed <- n_pass
  } else {
    consistency <- 1.0
    details$consistency_checks <- 0L
  }

  if (!is.null(date_cols) && length(date_cols) > 0L) {
    fs <- c()
    now <- Sys.time()
    for (dc in date_cols) {
      if (dc %in% names(data) &&
          inherits(data[[dc]], c("POSIXct", "POSIXlt", "Date"))) {
        md <- max(data[[dc]], na.rm = TRUE)
        if (!is.na(md)) {
          age <- as.numeric(difftime(now, md, units = "days"))
          fs <- c(fs, max(0, 1 - age / freshness_days))
          details[[sprintf("timeliness_%s", dc)]] <-
            list(max_date = as.character(md), age_days = age)
        }
      }
    }
    timeliness <- if (length(fs) > 0L) mean(fs) else 1.0
  } else { timeliness <- 1.0 }

  if (!is.null(key_cols) && length(key_cols) > 0L) {
    sub <- stats::na.omit(data[, key_cols, drop = FALSE])
    nd <- sum(duplicated(sub))
    uniqueness <- if (nrow(sub) > 0L) 1 - nd / nrow(sub) else 1
    details$key_duplicates <- nd
  } else {
    nd <- sum(duplicated(data))
    uniqueness <- if (nrow(data) > 0L) 1 - nd / nrow(data) else 1
    details$row_duplicates <- nd
  }
  overall <- mean(c(completeness, consistency, timeliness, uniqueness))
  .val_result("morie_data_quality_report",
              completeness = completeness, consistency = consistency,
              timeliness = timeliness, uniqueness = uniqueness,
              overall = overall, details = details)
}


# ===========================================================================
# Cross-validation
# ===========================================================================

.val_cv_indices <- function(n, n_folds, method, y = NULL,
                             groups = NULL, n_repeats = 10L,
                             random_state = 42L) {
  set.seed(random_state)
  switch(method,
    "kfold" = {
      ord <- sample.int(n)
      split(ord, cut(seq_along(ord), n_folds, labels = FALSE))
    },
    "stratified_kfold" = {
      out <- vector("list", n_folds)
      for (lvl in unique(y)) {
        idx <- which(y == lvl)
        idx <- sample(idx)
        f <- cut(seq_along(idx), n_folds, labels = FALSE)
        for (k in seq_len(n_folds)) out[[k]] <- c(out[[k]], idx[f == k])
      }
      out
    },
    "grouped_kfold" = {
      if (is.null(groups)) stop("groups required for grouped_kfold")
      g <- unique(groups)
      g <- sample(g)
      f <- cut(seq_along(g), n_folds, labels = FALSE)
      lapply(seq_len(n_folds), function(k) which(groups %in% g[f == k]))
    },
    "loo" = as.list(seq_len(n)),
    "monte_carlo" = {
      test_size <- max(1L, as.integer(n / n_folds))
      lapply(seq_len(n_repeats),
             function(.x) sample.int(n, test_size))
    },
    "time_series" = {
      step <- floor(n / (n_folds + 1L))
      lapply(seq_len(n_folds), function(k) {
        train_end <- step * k
        test_end <- min(n, train_end + step)
        seq.int(train_end + 1L, test_end)
      })
    },
    stop(sprintf("Unknown CV method: %s", method)))
}

#' Cross-validate a model using a user-supplied fit/predict pair
#'
#' @param fit_fn A function \code{(X, y) -> model}.
#' @param predict_fn A function \code{(model, X) -> probability vector}.
#' @param X Matrix or data frame of features.
#' @param y Vector of targets.
#' @param method Resampling strategy: "kfold", "stratified_kfold",
#'   "grouped_kfold", "loo", "monte_carlo", "time_series".
#' @param n_folds Number of folds.
#' @param n_repeats Repeats for monte_carlo.
#' @param scoring "roc_auc", "accuracy", "brier".
#' @param groups Group labels for grouped_kfold.
#' @param confidence Confidence level for the score CI.
#' @param random_state Seed.
#' @export
cross_validate <- function(fit_fn, predict_fn, X, y,
                            method = "stratified_kfold",
                            n_folds = 5L, n_repeats = 10L,
                            scoring = "roc_auc", groups = NULL,
                            confidence = 0.95, random_state = 42L) {
  X <- as.matrix(X)
  y <- as.vector(y)
  n <- length(y)
  test_idx <- .val_cv_indices(n, n_folds, method, y = y, groups = groups,
                              n_repeats = n_repeats, random_state = random_state)
  scores <- vapply(test_idx, function(ti) {
    tr <- setdiff(seq_len(n), ti)
    mdl <- fit_fn(X[tr, , drop = FALSE], y[tr])
    pr  <- predict_fn(mdl, X[ti, , drop = FALSE])
    .val_score(scoring, y[ti], pr)
  }, 0)
  z <- stats::qnorm(1 - (1 - confidence) / 2)
  se <- stats::sd(scores) / sqrt(length(scores))
  .val_result("morie_cv_result",
              scores = scores, mean = mean(scores), sd = stats::sd(scores),
              ci_lower = mean(scores) - z * se,
              ci_upper = mean(scores) + z * se,
              fold_details = list())
}
#' Nested cross-validation with inner-loop grid search
#'
#' Performs nested K-fold CV: the outer loop estimates generalisation
#' performance while an inner CV grid search picks the best hyperparameter
#' configuration on each outer training fold. Two calling conventions are
#' supported for backward compatibility:
#'
#' \itemize{
#'   \item \strong{Legacy stub form:} \code{nested_cross_validate(tune_fn,
#'         predict_fn, X, y, outer_folds, scoring, random_state)} where
#'         \code{tune_fn(X, y)} returns a fitted model (no grid argument).
#'         In this mode no inner search is run.
#'   \item \strong{Full form:} pass \code{fit_fn}, \code{predict_fn},
#'         \code{score_fn}, and \code{hyperparam_grid} (a named list of
#'         candidate vectors). The function enumerates the Cartesian
#'         product, runs inner K-fold CV on each outer training fold,
#'         picks the best configuration, refits on the full outer-train
#'         fold, and scores on the held-out outer fold.
#' }
#'
#' @param fit_fn Function with signature \code{(X, y, hyperparams) -> model}
#'   accepting a single hyperparameter list (full form only).
#' @param predict_fn Function with signature \code{(model, X) -> y_pred}.
#' @param score_fn Optional custom scoring function
#'   \code{(y_true, y_pred) -> numeric(1)}. Higher is better. If \code{NULL},
#'   the named scoring rule via \code{scoring} is used.
#' @param X Numeric predictor matrix (or coercible).
#' @param y Response vector.
#' @param hyperparam_grid Named list of candidate vectors (one per
#'   hyperparameter). The Cartesian product defines the search grid.
#' @param outer_k Number of outer folds (default 5).
#' @param inner_k Number of inner folds (default 3).
#' @param scoring Named scoring rule passed to the internal scorer
#'   (\code{"roc_auc"}, \code{"accuracy"}, \code{"brier"}). Used only if
#'   \code{score_fn} is \code{NULL}.
#' @param random_state Integer seed for fold construction (default 42).
#' @param tune_fn Deprecated legacy positional argument; see Description.
#' @param outer_folds Deprecated alias for \code{outer_k} (legacy stub form).
#' @return Named list with \code{outer_scores} (numeric vector, length
#'   \code{outer_k}), \code{best_hyperparams_per_fold} (list of named lists),
#'   \code{mean_score}, \code{se_score}, and \code{n_configs}.
#' @examples
#' set.seed(1)
#' n <- 120
#' X <- matrix(rnorm(n * 3), n, 3)
#' y <- as.integer(plogis(X[, 1]) > runif(n))
#' fit_fn <- function(X, y, hp) {
#'   df <- data.frame(y = y, X)
#'   suppressWarnings(stats::glm(y ~ ., data = df, family = stats::binomial()))
#' }
#' predict_fn <- function(model, X) {
#'   stats::predict(model, newdata = data.frame(X), type = "response")
#' }
#' nested_cross_validate(fit_fn = fit_fn, predict_fn = predict_fn,
#'                       X = X, y = y,
#'                       hyperparam_grid = list(dummy = c(1)),
#'                       outer_k = 3L, inner_k = 2L)
#' @export
nested_cross_validate <- function(fit_fn = NULL, predict_fn = NULL,
                                  X = NULL, y = NULL,
                                  score_fn = NULL,
                                  hyperparam_grid = NULL,
                                  outer_k = 5L, inner_k = 3L,
                                  scoring = "roc_auc",
                                  random_state = 42L,
                                  tune_fn = NULL,
                                  outer_folds = NULL) {
  # ---- Legacy stub form: (tune_fn, predict_fn, X, y, outer_folds, ...) ----
  if (!is.null(tune_fn) && is.null(hyperparam_grid)) {
    if (is.null(outer_folds)) outer_folds <- outer_k
    return(cross_validate(tune_fn, predict_fn, X, y,
                          method = "stratified_kfold",
                          n_folds = outer_folds,
                          scoring = scoring,
                          random_state = random_state))
  }

  if (is.null(fit_fn) || is.null(predict_fn) ||
      is.null(X) || is.null(y) || is.null(hyperparam_grid)) {
    stop("nested_cross_validate: fit_fn, predict_fn, X, y, ",
         "and hyperparam_grid are all required in the full form.")
  }

  X <- as.matrix(X)
  y <- as.vector(y)
  n <- length(y)
  if (n < outer_k || outer_k < 2L) {
    stop("nested_cross_validate: outer_k must be >= 2 and <= n.")
  }

  scorer <- if (!is.null(score_fn)) score_fn
            else function(yt, yp) .val_score(scoring, yt, yp)

  # Expand grid: a list of named-list configurations
  grid_df <- do.call(expand.grid, c(hyperparam_grid,
                                    list(stringsAsFactors = FALSE,
                                         KEEP.OUT.ATTRS = FALSE)))
  configs <- lapply(seq_len(nrow(grid_df)),
                    function(i) as.list(grid_df[i, , drop = FALSE]))
  n_configs <- length(configs)

  # Fold assignment (outer)
  set.seed(random_state)
  outer_folds_vec <- sample(rep(seq_len(outer_k), length.out = n))

  outer_scores <- numeric(outer_k)
  best_per_fold <- vector("list", outer_k)

  for (k in seq_len(outer_k)) {
    test_idx <- which(outer_folds_vec == k)
    train_idx <- setdiff(seq_len(n), test_idx)
    n_tr <- length(train_idx)
    if (n_tr < inner_k || inner_k < 2L) {
      stop("nested_cross_validate: inner_k must be >= 2 and ",
           "<= outer-fold training size.")
    }

    # Inner folds
    set.seed(random_state + k)
    inner_folds_vec <- sample(rep(seq_len(inner_k), length.out = n_tr))

    inner_scores <- numeric(n_configs)
    for (c_idx in seq_len(n_configs)) {
      hp <- configs[[c_idx]]
      fold_scores <- numeric(inner_k)
      for (j in seq_len(inner_k)) {
        inner_test_rel <- which(inner_folds_vec == j)
        inner_test <- train_idx[inner_test_rel]
        inner_train <- train_idx[-inner_test_rel]
        mdl <- fit_fn(X[inner_train, , drop = FALSE], y[inner_train], hp)
        yp <- predict_fn(mdl, X[inner_test, , drop = FALSE])
        fold_scores[j] <- scorer(y[inner_test], yp)
      }
      inner_scores[c_idx] <- mean(fold_scores, na.rm = TRUE)
    }
    best_idx <- which.max(inner_scores)
    best_hp <- configs[[best_idx]]
    best_per_fold[[k]] <- best_hp

    # Refit on full outer-train, score on outer-test
    mdl <- fit_fn(X[train_idx, , drop = FALSE], y[train_idx], best_hp)
    yp <- predict_fn(mdl, X[test_idx, , drop = FALSE])
    outer_scores[k] <- scorer(y[test_idx], yp)
  }

  mean_score <- mean(outer_scores, na.rm = TRUE)
  se_score <- stats::sd(outer_scores, na.rm = TRUE) / sqrt(outer_k)

  list(
    outer_scores = outer_scores,
    best_hyperparams_per_fold = best_per_fold,
    mean_score = mean_score,
    se_score = se_score,
    n_configs = n_configs
  )
}

#' Bootstrap .632 / .632+ validation
#' @inheritParams cross_validate
#' @param n_bootstraps Number of bootstrap replicates.
#' @param method "632" or "632plus".
#' @export
bootstrap_validate <- function(fit_fn, predict_fn, X, y,
                                n_bootstraps = 200L,
                                scoring = "roc_auc",
                                method = "632plus",
                                random_state = 42L) {
  X <- as.matrix(X)
  y <- as.vector(y)
  n <- length(y)
  set.seed(random_state)
  apparent <- .val_score(scoring, y, predict_fn(fit_fn(X, y), X))
  oob <- numeric(0)
  for (.i in seq_len(n_bootstraps)) {
    idx <- sample.int(n, n, replace = TRUE)
    oob_idx <- setdiff(seq_len(n), idx)
    if (length(oob_idx) == 0L) next
    if (length(unique(y[idx])) < 2L) next
    mdl <- fit_fn(X[idx, , drop = FALSE], y[idx])
    oob <- c(oob, .val_score(scoring, y[oob_idx],
                              predict_fn(mdl, X[oob_idx, , drop = FALSE])))
  }
  if (method == "632") {
    corrected <- 0.368 * apparent + 0.632 * mean(oob)
  } else if (method == "632plus") {
    y_perm <- sample(y)
    gamma <- .val_score(scoring, y_perm, predict_fn(fit_fn(X, y), X))
    r_bar <- if (abs(gamma - apparent) > 1e-12)
      (mean(oob) - apparent) / (gamma - apparent) else 0
    r_bar <- max(0, min(1, r_bar))
    w <- 0.632 / (1 - 0.368 * r_bar)
    corrected <- (1 - w) * apparent + w * mean(oob)
  } else {
    stop(sprintf("Unknown bootstrap method: %s", method))
  }
  z <- stats::qnorm(0.975)
  se <- stats::sd(oob) / sqrt(length(oob))
  .val_result("morie_cv_result",
              scores = oob, mean = corrected, sd = stats::sd(oob),
              ci_lower = corrected - z * se,
              ci_upper = corrected + z * se,
              fold_details = list())
}


# ===========================================================================
# Calibration
# ===========================================================================

#' Comprehensive calibration assessment for binary outcomes
#' @param y_true Integer 0/1 vector.
#' @param y_pred Predicted probabilities.
#' @param n_groups Hosmer-Lemeshow groups.
#' @export
assess_calibration <- function(y_true, y_pred, n_groups = 10L) {
  y_true <- as.integer(y_true)
  y_pred <- as.numeric(y_pred)
  ord <- order(y_pred)
  groups <- split(ord, cut(seq_along(ord), n_groups, labels = FALSE))
  hl <- 0
  for (g in groups) {
    obs <- sum(y_true[g])
    exp_ <- sum(y_pred[g])
    n_g <- length(g)
    avg_p <- mean(y_pred[g])
    if (avg_p > 0 && avg_p < 1)
      hl <- hl + (obs - exp_)^2 / (n_g * avg_p * (1 - avg_p))
  }
  hl_p <- stats::pchisq(hl, df = n_groups - 2, lower.tail = FALSE)
  logit_p <- log(pmax(pmin(y_pred, 1 - 1e-10), 1e-10) /
                  (1 - pmax(pmin(y_pred, 1 - 1e-10), 1e-10)))
  fit <- stats::glm(y_true ~ logit_p, family = stats::binomial())
  cal_slope <- unname(stats::coef(fit)[2])
  cal_intercept <- unname(stats::coef(fit)[1])
  brier <- .val_brier(y_true, y_pred)
  prev <- mean(y_true)
  brier_max <- prev * (1 - prev)
  scaled <- if (brier_max > 0) 1 - brier / brier_max else 0
  citl <- mean(y_pred) - mean(y_true)
  .val_result("morie_calibration_result",
              hosmer_lemeshow_stat = hl, hosmer_lemeshow_p = hl_p,
              calibration_slope = cal_slope,
              calibration_intercept = cal_intercept,
              brier_score = brier, scaled_brier = scaled,
              calibration_in_the_large = citl)
}


# ===========================================================================
# Discrimination
# ===========================================================================

#' Discrimination assessment for binary classifier
#' @inheritParams assess_calibration
#' @param y_pred_ref Optional reference-model probabilities for NRI/IDI.
#' @param n_bootstrap Bootstrap reps for AUC CI.
#' @param confidence Confidence level.
#' @param random_state Seed.
#' @export
assess_discrimination <- function(y_true, y_pred, y_pred_ref = NULL,
                                   n_bootstrap = 1000L,
                                   confidence = 0.95,
                                   random_state = 42L) {
  y_true <- as.integer(y_true)
  y_pred <- as.numeric(y_pred)
  auroc <- .val_auc(y_true, y_pred)
  set.seed(random_state)
  boots <- numeric(0)
  for (.i in seq_len(n_bootstrap)) {
    idx <- sample.int(length(y_true), length(y_true), replace = TRUE)
    if (length(unique(y_true[idx])) < 2L) next
    boots <- c(boots, .val_auc(y_true[idx], y_pred[idx]))
  }
  alpha <- (1 - confidence) / 2
  ci_lo <- stats::quantile(boots, alpha, names = FALSE, na.rm = TRUE)
  ci_hi <- stats::quantile(boots, 1 - alpha, names = FALSE, na.rm = TRUE)
  somers <- 2 * (auroc - 0.5)
  ev <- y_true == 1L
  disc_slope <- mean(y_pred[ev]) - mean(y_pred[!ev])

  nri_val <- NA_real_
  idi_val <- NA_real_
  if (!is.null(y_pred_ref)) {
    y_pred_ref <- as.numeric(y_pred_ref)
    up_e <- sum(y_pred[ev] > y_pred_ref[ev]) -
             sum(y_pred[ev] < y_pred_ref[ev])
    nri_e <- if (sum(ev) > 0L) up_e / sum(ev) else 0
    up_ne <- sum(y_pred[!ev] < y_pred_ref[!ev]) -
              sum(y_pred[!ev] > y_pred_ref[!ev])
    nri_ne <- if (sum(!ev) > 0L) up_ne / sum(!ev) else 0
    nri_val <- nri_e + nri_ne
    ref_disc <- mean(y_pred_ref[ev]) - mean(y_pred_ref[!ev])
    idi_val <- disc_slope - ref_disc
  }
  .val_result("morie_discrimination_result",
              auroc = auroc, auroc_ci_lower = ci_lo, auroc_ci_upper = ci_hi,
              c_statistic = auroc, somers_d = somers,
              discrimination_slope = disc_slope,
              nri = nri_val, idi = idi_val)
}


# ===========================================================================
# Decision curve analysis
# ===========================================================================

#' Decision curve analysis
#' @inheritParams assess_calibration
#' @param thresholds Numeric vector of thresholds (defaults to
#'   \code{seq(0.01, 0.99, 0.01)}).
#' @export
decision_curve_analysis <- function(y_true, y_pred, thresholds = NULL) {
  y_true <- as.integer(y_true)
  y_pred <- as.numeric(y_pred)
  n <- length(y_true)
  if (is.null(thresholds)) thresholds <- seq(0.01, 0.98, by = 0.01)
  prev <- mean(y_true)
  nb <- numeric(length(thresholds))
  nb_all <- numeric(length(thresholds))
  for (i in seq_along(thresholds)) {
    t <- thresholds[i]
    pp <- y_pred >= t
    tp <- sum(pp & y_true == 1L)
    fp <- sum(pp & y_true == 0L)
    nb[i] <- tp / n - fp / n * (t / (1 - t))
    nb_all[i] <- prev - (1 - prev) * (t / (1 - t))
  }
  .val_result("morie_decision_curve_result",
              thresholds = thresholds, net_benefit = nb,
              net_benefit_all = nb_all,
              net_benefit_none = rep(0, length(thresholds)))
}


# ===========================================================================
# Overfitting detection
# ===========================================================================

#' Bootstrap optimism-corrected performance
#' @inheritParams bootstrap_validate
#' @param n_bootstrap Integer; number of bootstrap resamples used to
#'   estimate the optimism correction (default 200).
#' @export
detect_overfitting <- function(fit_fn, predict_fn, X, y,
                                scoring = "roc_auc",
                                n_bootstrap = 200L,
                                random_state = 42L) {
  X <- as.matrix(X)
  y <- as.vector(y)
  n <- length(y)
  set.seed(random_state)
  apparent <- .val_score(scoring, y, predict_fn(fit_fn(X, y), X))
  opt <- numeric(0)
  for (.i in seq_len(n_bootstrap)) {
    idx <- sample.int(n, n, replace = TRUE)
    if (length(unique(y[idx])) < 2L) next
    mdl <- fit_fn(X[idx, , drop = FALSE], y[idx])
    perf_b <- .val_score(scoring, y[idx],
                         predict_fn(mdl, X[idx, , drop = FALSE]))
    perf_o <- .val_score(scoring, y, predict_fn(mdl, X))
    opt <- c(opt, perf_b - perf_o)
  }
  optimism <- mean(opt)
  corrected <- apparent - optimism
  shrinkage <- if (apparent > 0) corrected / apparent else 1
  rec <- if (optimism > 0.05)
    "Substantial overfitting detected. Consider regularisation or simpler model."
  else if (optimism > 0.02)
    "Moderate optimism. Bootstrap-corrected estimates recommended."
  else
    "Minimal overfitting. Model performance appears stable."
  .val_result("morie_overfit_result",
              apparent_performance = apparent,
              optimism = optimism,
              corrected_performance = corrected,
              shrinkage_factor = shrinkage,
              recommendation = rec)
}


# ===========================================================================
# Temporal validation
# ===========================================================================

#' Train on earlier data, test on later data
#' @param fit_fn,predict_fn As in \code{cross_validate}.
#' @param X Data frame including \code{date_col}.
#' @param y Target vector.
#' @param date_col Name of date column in \code{X}.
#' @param split_date Date to split on, or NULL.
#' @param split_quantile Quantile of dates (if \code{split_date} is NULL).
#' @param scoring Scoring metric.
#' @export
temporal_validate <- function(fit_fn, predict_fn, X, y, date_col,
                               split_date = NULL,
                               split_quantile = 0.7,
                               scoring = "roc_auc") {
  dates <- as.POSIXct(X[[date_col]])
  if (is.null(split_date)) {
    split_date <- stats::quantile(as.numeric(dates),
                                  split_quantile, names = FALSE)
    split_date <- as.POSIXct(split_date, origin = "1970-01-01")
  } else split_date <- as.POSIXct(split_date)
  tr <- dates <= split_date
  te <- dates > split_date
  feat_cols <- setdiff(names(X), date_col)
  X_tr <- as.matrix(X[tr, feat_cols, drop = FALSE])
  X_te <- as.matrix(X[te, feat_cols, drop = FALSE])
  y_tr <- y[tr]
  y_te <- y[te]
  if (length(unique(y_tr)) < 2L || length(unique(y_te)) < 2L)
    stop("Both train and test must have at least 2 classes.")
  mdl <- fit_fn(X_tr, y_tr)
  ts <- .val_score(scoring, y_tr, predict_fn(mdl, X_tr))
  vs <- .val_score(scoring, y_te, predict_fn(mdl, X_te))
  .val_result("morie_temporal_result",
              train_score = ts, test_score = vs,
              degradation = ts - vs,
              split_date = as.character(as.Date(split_date)),
              train_n = sum(tr), test_n = sum(te))
}


# ===========================================================================
# External validation
# ===========================================================================

#' External validation on new data
#' @param predict_fn A function \code{(X) -> probability vector}
#'   (model already bound by closure).
#' @param X_external Matrix or data frame of features.
#' @param y_external Outcome vector.
#' @param X_development Optional development-data features for KS-based
#'   domain-shift diagnostics.
#' @export
external_validate <- function(predict_fn, X_external, y_external,
                               X_development = NULL) {
  X_ext <- as.matrix(X_external)
  y_ext <- as.integer(y_external)
  y_pred <- predict_fn(X_ext)
  disc <- assess_discrimination(y_ext, y_pred)
  cal <- assess_calibration(y_ext, y_pred)
  domain <- list()
  if (!is.null(X_development)) {
    X_dev <- as.matrix(X_development)
    for (j in seq_len(ncol(X_ext))) {
      kt <- suppressWarnings(stats::ks.test(X_dev[, j], X_ext[, j]))
      domain[[sprintf("feature_%d", j)]] <- kt$p.value
    }
  }
  .val_result("morie_external_result",
              discrimination = disc, calibration = cal,
              n_external = length(y_ext), domain_shift = domain)
}


# ===========================================================================
# Reproducibility manifest
# ===========================================================================

#' Create a manifest of the environment for reproducibility
#' @param data Data frame (used for a SHA-256 checksum).
#' @param parameters Optional list of analysis parameters.
#' @param seeds Optional named list of random seeds.
#' @export
create_reproducibility_manifest <- function(data, parameters = NULL,
                                             seeds = NULL) {
  buf <- paste(utils::capture.output(utils::write.csv(data, row.names = FALSE)),
               collapse = "\
")
  checksum <- if (requireNamespace("digest", quietly = TRUE))
    digest::digest(buf, algo = "sha256") else NA_character_
  pkgs <- c("base", "stats", "knitr", "Matrix", "MASS")
  versions <- vapply(pkgs, function(p) {
    tryCatch(as.character(utils::packageVersion(p)),
             error = function(e) "not installed")
  }, character(1))
  .val_result("morie_reproducibility_manifest",
              r_version = R.version.string,
              package_versions = setNames(as.list(versions), pkgs),
              random_seeds = if (is.null(seeds)) list() else seeds,
              data_checksum = checksum,
              parameters = if (is.null(parameters)) list() else parameters,
              timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
}
