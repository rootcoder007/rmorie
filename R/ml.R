# SPDX-License-Identifier: AGPL-3.0-or-later
#
# ml.R -- Machine Learning sensitivity diagnostics.
#
# R port of src/morie/ml.py. Wraps a small Random Forest robustness
# evaluator and a SMOTE (or random-oversampling fallback) routine for
# binary-outcome class balancing. Pure base R + `randomForest` /
# `smotefamily` from Suggests; no Python interop required.

#' Evaluate Random Forest robustness
#'
#' Fits a 100-tree Random Forest on training data and reports a
#' classification report (precision / recall / F1 / support per class)
#' on the held-out test set. Mirrors `morie.ml.eval_robustness`.
#'
#' @param X Training features (data.frame or matrix).
#' @param y Training labels (factor or coercible to factor).
#' @param test_X Test features.
#' @param test_y Test labels.
#' @param n_estimators Number of trees. Default 100.
#' @param random_state Integer seed. Default 42.
#' @return Named list keyed by class label and `accuracy` with
#'   precision / recall / f1-score / support per class, mirroring
#'   sklearn's `classification_report(output_dict=True)`.
#' @export
morie_ml_eval_robustness <- function(X, y, test_X, test_y,
                                     n_estimators = 100L,
                                     random_state = 42L) {
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("morie_ml_eval_robustness requires the 'randomForest' package.")
  }
  y <- as.factor(y)
  test_y <- factor(as.character(test_y), levels = levels(y))
  set.seed(random_state)
  fit <- randomForest::randomForest(
    x = as.data.frame(X), y = y, ntree = as.integer(n_estimators)
  )
  preds <- stats::predict(fit, newdata = as.data.frame(test_X))
  preds <- factor(as.character(preds), levels = levels(y))

  classes <- levels(y)
  out <- list()
  for (cls in classes) {
    tp <- sum(preds == cls & test_y == cls)
    fp <- sum(preds == cls & test_y != cls)
    fn <- sum(preds != cls & test_y == cls)
    support <- sum(test_y == cls)
    precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
    recall    <- if ((tp + fn) > 0) tp / (tp + fn) else 0
    f1 <- if ((precision + recall) > 0) {
      2 * precision * recall / (precision + recall)
    } else 0
    out[[as.character(cls)]] <- list(
      precision = precision, recall = recall,
      `f1-score` = f1, support = support
    )
  }
  out[["accuracy"]] <- mean(preds == test_y)
  out[["method"]]   <- "Random Forest classification report"
  out
}


#' Apply SMOTE oversampling to balance a binary outcome
#'
#' R port of `morie.ml.apply_smote`. Uses `smotefamily::SMOTE` when
#' installed and feasible; falls back to random oversampling (duplicate
#' minority rows) otherwise. Returns the resampled `(X, y)` together
#' with a status list of before/after counts and the method used.
#'
#' @param X Feature data.frame.
#' @param y Binary outcome vector (numeric or factor).
#' @param random_state Integer seed for the random fallback. Default 42.
#' @param k_neighbors Integer or NULL. SMOTE neighbour count; auto-picked
#'   to `min(5, minority - 1)` when NULL.
#' @return list(X, y, status) where `status` mirrors the Python dict
#'   keys (`method`, `minority_before`, `majority_before`,
#'   `imbalance_ratio_before`, `total_before`, `total_after`, plus
#'   `class_<label>_before` / `class_<label>_after`).
#' @export
morie_ml_apply_smote <- function(X, y, random_state = 42L,
                                 k_neighbors = NULL) {
  y_chr <- as.character(y)
  counts_before <- table(y_chr)
  minority_count <- as.integer(min(counts_before))
  majority_count <- as.integer(max(counts_before))
  if (is.null(k_neighbors)) {
    k_neighbors <- if (minority_count > 1) {
      as.integer(min(5L, minority_count - 1L))
    } else 1L
  }

  method <- "smote"
  X_res <- X
  y_res <- y
  smote_ok <- requireNamespace("smotefamily", quietly = TRUE) &&
    minority_count > k_neighbors
  if (smote_ok) {
    res <- try(
      smotefamily::SMOTE(X = as.data.frame(X), target = y_chr,
                         K = as.integer(k_neighbors)),
      silent = TRUE
    )
    if (!inherits(res, "try-error") && !is.null(res$data)) {
      tgt <- res$data$class
      X_res <- res$data[, setdiff(names(res$data), "class"), drop = FALSE]
      y_res <- if (is.factor(y)) factor(tgt, levels = levels(y)) else tgt
    } else {
      smote_ok <- FALSE
    }
  }
  if (!smote_ok) {
    method <- "random_oversample"
    minority_label <- names(counts_before)[which.min(counts_before)]
    minority_idx <- which(y_chr == minority_label)
    n_needed <- majority_count - minority_count
    if (n_needed > 0 && minority_count > 0) {
      set.seed(random_state)
      sample_idx <- sample(minority_idx, size = n_needed, replace = TRUE)
      X_res <- rbind(as.data.frame(X),
                     as.data.frame(X)[sample_idx, , drop = FALSE])
      y_res <- c(y, y[sample_idx])
    }
  }

  counts_after <- table(as.character(y_res))
  status <- list(
    method = method,
    minority_before = minority_count,
    majority_before = majority_count,
    imbalance_ratio_before = if (majority_count > 0) {
      round(minority_count / majority_count, 4)
    } else 0,
    total_before = length(y),
    total_after  = length(y_res)
  )
  for (k in names(counts_before)) {
    status[[paste0("class_", k, "_before")]] <- as.integer(counts_before[[k]])
  }
  for (k in names(counts_after)) {
    status[[paste0("class_", k, "_after")]] <- as.integer(counts_after[[k]])
  }
  list(X = as.data.frame(X_res), y = y_res, status = status)
}
