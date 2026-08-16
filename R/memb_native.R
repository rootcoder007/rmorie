# Membership inference against machine learning models (shadow training).
# Sources: Shokri, R., Stronati, M., Song, C., & Shmatikov, V. (2017)
# "Membership Inference Attacks Against Machine Learning Models", *IEEE
# Symposium on Security and Privacy*, 3-18.
#
# Given black-box access to a target model -- a record goes in, a vector
# of class probabilities comes out -- decide whether that record was in
# the model's training set. The attack "exploits the observation that
# machine learning models often behave differently on the data that
# they were trained on versus the data that they 'see' for the first
# time", and the paper's contribution is a way to *learn* that
# difference without ever seeing the target's training data.
#
# Shadow training. The attacker trains k shadow models on data drawn
# like the target's, and for each one knows the ground truth. Querying
# shadow model i with its own training records gives outputs labelled
# "in"; querying it with a disjoint test set gives outputs labelled
# "out". Those labelled prediction vectors are the training set for the
# attack model (figure 3). Because "the attack model is a collection of
# models, one for each output class of the target model", a separate
# binary classifier is fitted per true class, which is what makes the
# attack sensitive to the class-conditional shape of the output vector
# rather than to overall confidence alone.
#
# Where shadow data comes from. All three of the paper's routes are
# here (synthesis):
#   "model" -- synthesis from the target model itself, Algorithm 1;
#   "marginals" -- independently sampling the value of each feature;
#   "noisy" -- real data with a fraction of features perturbed.
#
# The classifier used for the target, the shadows and the attack models
# is supplied by the caller as train_fn(X, y) -> predict_fn; a native
# multinomial logistic regression (logistic_trainer) is the default.
# The attack is only as strong as the gap it feeds on.

#' .memb_sigmoid
#'
#' A step of the memb_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.memb_sigmoid <- function(x) 1 / (1 + exp(-x))
#' .memb_softmax
#'
#' A step of the memb_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Numeric; passed to \code{max}.
#' @return A numeric value.
#' @export
.memb_softmax <- function(z) {
  mx <- max(z)
  ez <- exp(z - mx)
  ez / sum(ez)
}

#' logistic_trainer
#'
#' A step of the memb_native implementation. Called by \code{memb}, \code{morie_memb}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param l2 Numeric; combined arithmetically in the body. Defaults to \code{0.001}.
#' @param epochs Coerced to integer by the body, with \code{as.integer}. Defaults to \code{300L}.
#' @param lr Numeric; combined arithmetically in the body. Defaults to \code{0.5}.
#' @param seed Accepted by the signature and not used anywhere in the body. Defaults to \code{0}.
#' @return The value of \code{train}, as built in the body.
#' @export
logistic_trainer <- function(l2 = 1e-3, epochs = 300L, lr = 0.5, seed = 0) {
  train <- function(X, y) {
    n <- length(X)
    if (n == 0L)
      stop("memb: cannot train on an empty dataset")
    d <- length(X[[1L]])
    classes <- sort(unique(unlist(y)))
    idx <- setNames(seq_along(classes) - 1L, as.character(classes))
    C <- length(classes)
    W <- matrix(0, C, d)
    b <- rep(0.0, C)
    Xmat <- do.call(rbind, lapply(X, as.numeric))
    Yint <- vapply(y, function(v) idx[[as.character(v)]], integer(1))
    for (ep in seq_len(as.integer(epochs))) {
      Z <- Xmat %*% t(W) + matrix(b, nrow = n, ncol = C, byrow = TRUE)
      mx <- apply(Z, 1L, max)
      EZ <- exp(Z - mx)
      SSUM <- rowSums(EZ)
      P <- EZ / SSUM
      Ind <- matrix(0, n, C)
      for (i in seq_len(n)) Ind[i, Yint[i] + 1L] <- 1
      Err <- Ind - P
      gW <- t(Err) %*% Xmat / n
      gb <- colSums(Err) / n
      W <- W + lr * (gW - l2 * W)
      b <- b + lr * gb
    }
    predict <- function(rows) {
      Xq <- do.call(rbind, lapply(rows, as.numeric))
      Z <- Xq %*% t(W) + matrix(b, nrow = nrow(Xq), ncol = C, byrow = TRUE)
      mx <- apply(Z, 1L, max)
      EZ <- exp(Z - mx)
      SSUM <- rowSums(EZ)
      sweep(EZ, 1L, SSUM, "/")
    }
    attr(predict, "classes") <- classes
    predict
  }
  train
}

#' knn_trainer
#'
#' A step of the memb_native implementation. Called by \code{morie_memb}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param k A count; the body uses it as \code{seq_len(...)}. Defaults to \code{1L}.
#' @param smoothing A count; the body uses it as \code{rep(...)}. Defaults to \code{0.001}.
#' @return The value of \code{train}, as built in the body.
#' @export
knn_trainer <- function(k = 1L, smoothing = 1e-3) {
  k <- as.integer(k)
  if (k < 1L) stop("memb: k must be >= 1")
  train <- function(X, y) {
    if (length(X) == 0L)
      stop("memb: cannot train on an empty dataset")
    classes <- sort(unique(unlist(y)))
    idx <- setNames(seq_along(classes) - 1L, as.character(classes))
    rows <- do.call(rbind, lapply(X, as.numeric))
    labs <- unlist(y)
    predict <- function(query) {
      Q <- do.call(rbind, lapply(query, as.numeric))
      out <- matrix(0, nrow(Q), length(classes))
      for (r in seq_len(nrow(Q))) {
        d <- rowSums((t(rows) - Q[r, ])^2)
        ord <- order(d)
        votes <- rep(smoothing, length(classes))
        for (i in ord[seq_len(k)]) {
          votes[idx[[as.character(labs[i])]] + 1L] <-
            votes[idx[[as.character(labs[i])]] + 1L] + 1
        }
        out[r, ] <- votes / sum(votes)
      }
      out
    }
    attr(predict, "classes") <- classes
    predict
  }
  train
}

#' attack_dataset
#'
#' A step of the memb_native implementation. Called by \code{memb}, \code{morie_memb}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param model_predict Accepted by the signature and not used anywhere in the body.
#' @param in_X A vector; its length is taken.
#' @param in_y A vector; indexed elementwise.
#' @param out_X A vector; its length is taken.
#' @param out_y A vector; indexed elementwise.
#' @return A list with \code{rows}, \code{labels}, \code{classes}.
#' @export
attack_dataset <- function(model_predict, in_X, in_y, out_X, out_y) {
  rows <- list(); lab <- c(); cls <- c()
  if (length(in_X) > 0L) {
    pr <- model_predict(in_X)
    for (i in seq_along(in_X)) {
      rows[[length(rows) + 1L]] <- as.numeric(pr[[i]])
      lab <- c(lab, 1)
      cls <- c(cls, as.character(in_y[[i]]))
    }
  }
  if (length(out_X) > 0L) {
    pr <- model_predict(out_X)
    for (i in seq_along(out_X)) {
      rows[[length(rows) + 1L]] <- as.numeric(pr[[i]])
      lab <- c(lab, 0)
      cls <- c(cls, as.character(out_y[[i]]))
    }
  }
  list(rows = rows, labels = lab, classes = cls)
}

#' synthesize
#'
#' A step of the memb_native implementation. Called by \code{morie_memb}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param target_predict Accepted by the signature and not used anywhere in the body.
#' @param c Numeric; combined arithmetically in the body.
#' @param n_features A count; the body uses it as \code{seq_len(...)}.
#' @param feature_values The body requires: memb: feature_values must have one entry per feature.
#' @param k_max Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param k_min Numeric; passed to \code{max}. Defaults to \code{1L}.
#' @param conf_min The body requires: memb: conf_min must lie in (0, 1). Defaults to \code{0.8}.
#' @param iter_max A count; the body uses it as \code{seq_len(...)}. Defaults to \code{1000L}.
#' @param rej_max Defaults to \code{10L}.
#' @param seed Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @return Nothing; the function is called for its effect.
#' @export
synthesize <- function(target_predict, c, n_features, feature_values = NULL,
                       k_max = NULL, k_min = 1L, conf_min = 0.8,
                       iter_max = 1000L, rej_max = 10L, seed = 0) {
  n_features <- as.integer(n_features)
  if (n_features < 1L) stop("memb: n_features must be >= 1")
  if (!(conf_min > 0 && conf_min < 1))
    stop("memb: conf_min must lie in (0, 1)")
  if (k_min < 1L) stop("memb: k_min must be >= 1")
  k_max <- if (is.null(k_max)) n_features else as.integer(k_max)
  if (k_max < k_min) stop("memb: k_max must be at least k_min")
  e <- .ghc_rng(as.numeric(seed))
  vals <- if (is.null(feature_values)) lapply(seq_len(n_features), function(j) c(0.0, 1.0)) else feature_values
  if (length(vals) != n_features)
    stop("memb: feature_values must have one entry per feature")
  rand_record <- function(base = NULL, k = NULL) {
    if (is.null(base)) {
      x <- numeric(n_features)
      for (j in seq_len(n_features)) {
        u <- .ghc_unif(e, 1L)
        x[j] <- vals[[j]][1L + as.integer(u * length(vals[[j]]))]
      }
      return(x)
    }
    x <- as.numeric(base)
    picks <- integer(0)
    while (length(picks) < min(k, n_features)) {
      u <- .ghc_unif(e, 1L)
      j <- 1L + as.integer(u * n_features)
      if (!(j %in% picks)) picks <- c(picks, j)
    }
    for (j in picks) {
      choices <- setdiff(vals[[j]], x[j])
      if (length(choices) == 0L) choices <- vals[[j]]
      u <- .ghc_unif(e, 1L)
      x[j] <- choices[1L + as.integer(u * length(choices))]
    }
    x
  }
  x <- rand_record()
  y_best <- 0.0
  x_best <- x
  j <- 0L
  k <- k_max
  for (it in seq_len(iter_max)) {
    y <- as.numeric(target_predict(list(x))[[1L]])
    if (c >= length(y))
      stop("memb: class ", c, " is outside the target's output vector")
    yc <- y[c + 1L]
    if (yc >= y_best) {
      if (yc > conf_min && c == which.max(y) - 1L) {
        u <- .ghc_unif(e, 1L)
        if (u < yc) return(x)
      }
      x_best <- x
      y_best <- yc
      j <- 0L
    } else {
      j <- j + 1L
      if (j > rej_max) {
        k <- max(k_min, -(-k %/% 2L))
        j <- 0L
      }
    }
    x <- rand_record(x_best, k)
  }
  NULL
}

#' synthesize_marginals
#'
#' A step of the memb_native implementation. Called by \code{morie_memb}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A vector; its length is taken and its elements indexed.
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param seed Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @return The value of \code{out}, as built in the body.
#' @export
synthesize_marginals <- function(X, n, seed = 0) {
  if (length(X) == 0L)
    stop("memb: no data to take marginals from")
  e <- .ghc_rng(as.numeric(seed))
  d <- length(X[[1L]])
  Xmat <- do.call(rbind, lapply(X, as.numeric))
  cols <- lapply(seq_len(d), function(j) Xmat[, j])
  out <- vector("list", n)
  for (i in seq_len(n)) {
    u <- .ghc_unif(e, d)
    out[[i]] <- vapply(seq_len(d), function(j) {
      cols[[j]][1L + as.integer(u[j] * length(cols[[j]]))]
    }, numeric(1))
  }
  out
}

#' synthesize_noisy
#'
#' A step of the memb_native implementation. Called by \code{morie_memb}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A vector; indexed elementwise.
#' @param fraction The body requires: memb: fraction must lie in [0, 1]. Defaults to \code{0.1}.
#' @param feature_values Defaults to \code{NULL}.
#' @param seed Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @return The value of \code{out}, as built in the body.
#' @export
synthesize_noisy <- function(X, fraction = 0.1, feature_values = NULL,
                              seed = 0) {
  if (!(fraction >= 0 && fraction <= 1))
    stop("memb: fraction must lie in [0, 1]")
  e <- .ghc_rng(as.numeric(seed))
  d <- length(X[[1L]])
  Xmat <- do.call(rbind, lapply(X, as.numeric))
  vals <- if (is.null(feature_values)) lapply(seq_len(d), function(j) sort(unique(Xmat[, j]))) else feature_values
  out <- vector("list", nrow(Xmat))
  for (r in seq_len(nrow(Xmat))) {
    x <- as.numeric(Xmat[r, ])
    for (j in seq_len(d)) {
      u <- .ghc_unif(e, 1L)
      if (u < fraction) {
        choices <- setdiff(vals[[j]], x[j])
        if (length(choices) == 0L) choices <- vals[[j]]
        u2 <- .ghc_unif(e, 1L)
        x[j] <- choices[1L + as.integer(u2 * length(choices))]
      }
    }
    out[[r]] <- x
  }
  out
}

#' precision_recall
#'
#' A step of the memb_native implementation. Called by \code{memb}, \code{morie_memb}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param pred Coerced to integer by the body, with \code{as.integer}.
#' @param truth Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{precision}, \code{recall}, \code{accuracy}, \code{tp}, \code{fp}, \code{fn}, \code{tn}.
#' @export
precision_recall <- function(pred, truth) {
  pred <- as.integer(pred); truth <- as.integer(truth)
  tp <- sum(pred == 1 & truth == 1)
  fp <- sum(pred == 1 & truth == 0)
  fn <- sum(pred == 0 & truth == 1)
  tn <- sum(pred == 0 & truth == 0)
  n <- tp + fp + fn + tn
  list(precision = if (tp + fp > 0L) tp / (tp + fp) else NaN,
       recall = if (tp + fn > 0L) tp / (tp + fn) else NaN,
       accuracy = if (n > 0L) (tp + tn) / n else NaN,
       tp = tp, fp = fp, fn = fn, tn = tn)
}

#' .memb_sorted_features
#'
#' A step of the memb_native implementation. Called by \code{memb}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param vec Coerced to numeric by the body, with \code{as.numeric}.
#' @param top Optional; may be \code{NULL}. Numeric; passed to \code{min}.
#' @return One of two values, depending on the branch taken.
#' @export
.memb_sorted_features <- function(vec, top = NULL) {
  s <- sort(as.numeric(vec), decreasing = TRUE)
  if (is.null(top)) s else s[seq_len(min(top, length(s)))]
}

#' memb
#'
#' A step of the memb_native implementation. Called by \code{morie_memb}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param target_predict Accepted by the signature and not used anywhere in the body.
#' @param shadow_data See Usage.
#' @param eval_in A vector; indexed elementwise.
#' @param eval_out A vector; indexed elementwise.
#' @param train_fn Defaults to \code{NULL}.
#' @param attack_train_fn Defaults to \code{NULL}.
#' @param n_shadow Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param sort_features A flag; the body branches on it. Defaults to \code{FALSE}.
#' @param threshold Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.5}.
#' @return A list with \code{estimate}, \code{metrics}, \code{per_class}, \code{predictions}, \code{scores}, \code{truth}, \code{n_shadow}, \code{attack_train_size}, \code{attack_classes}, \code{threshold}, \code{note}, \code{method}.
#' @export
memb <- function(target_predict, shadow_data, eval_in, eval_out,
                 train_fn = NULL, attack_train_fn = NULL, n_shadow = NULL,
                 sort_features = FALSE, threshold = 0.5) {
  if (is.null(train_fn)) train_fn <- logistic_trainer()
  if (is.null(attack_train_fn)) attack_train_fn <- logistic_trainer()
  specs <- shadow_data
  if (!is.null(n_shadow)) specs <- specs[seq_len(min(as.integer(n_shadow), length(specs)))]
  if (length(specs) == 0L)
    stop("memb: at least one shadow model is needed")
  rows <- list(); labels <- integer(0); classes <- character(0)
  for (spec in specs) {
    tr_X <- spec[[1L]]; tr_y <- spec[[2L]]
    te_X <- spec[[3L]]; te_y <- spec[[4L]]
    if (length(tr_X) == 0L)
      stop("memb: a shadow model has no training data")
    shadow <- train_fn(tr_X, tr_y)
    ad <- attack_dataset(shadow, tr_X, tr_y, te_X, te_y)
    rows <- c(rows, ad$rows)
    labels <- c(labels, ad$labels)
    classes <- c(classes, ad$classes)
  }
  if (length(rows) == 0L)
    stop("memb: the shadow models produced no attack data")
  per_class <- list()
  for (c in sort(unique(classes))) {
    idx <- which(classes == c)
    if (length(unique(labels[idx])) < 2L) next
    feats <- lapply(idx, function(t) {
      if (isTRUE(sort_features)) .memb_sorted_features(rows[[t]]) else rows[[t]]
    })
    per_class[[as.character(c)]] <- attack_train_fn(feats, labels[idx])
  }
  if (length(per_class) == 0L)
    stop("memb: no class had both in and out examples, so no attack model could be trained")
  eval_X <- c(eval_in[[1L]], eval_out[[1L]])
  eval_y <- c(eval_in[[2L]], eval_out[[2L]])
  truth <- c(rep(1L, length(eval_in[[1L]])),
             rep(0L, length(eval_out[[1L]])))
  outputs <- if (length(eval_X) > 0L) target_predict(eval_X) else list()
  scores <- numeric(length(outputs)); preds <- integer(length(outputs))
  for (i in seq_along(outputs)) {
    vec <- as.numeric(outputs[[i]])
    c <- as.character(eval_y[[i]])
    model <- per_class[[c]]
    if (is.null(model)) {
      scores[i] <- NaN; preds[i] <- 0L
      next
    }
    feat <- if (isTRUE(sort_features)) .memb_sorted_features(vec) else vec
    pr <- as.numeric(model(list(feat))[[1L]])
    member <- if (length(pr) > 1L) pr[2L] else pr[1L]
    scores[i] <- member
    preds[i] <- as.integer(member >= threshold)
  }
  metrics <- precision_recall(preds, truth)
  by_class <- list()
  for (c in sort(unique(unlist(eval_y)))) {
    sel <- which(vapply(eval_y, function(v) identical(v, c), logical(1)))
    if (length(sel) > 0L)
      by_class[[as.character(c)]] <- precision_recall(preds[sel], truth[sel])
  }
  list(estimate = metrics, metrics = metrics, per_class = by_class,
       predictions = preds, scores = scores, truth = truth,
       n_shadow = length(specs),
       attack_train_size = length(rows),
       attack_classes = names(per_class),
       threshold = as.numeric(threshold),
       note = "the attack can only find a gap that exists: against a target that does not overfit, precision falls to the base rate (Shokri et al. 2017, section VII)",
       method = "shadow-trained membership inference (Shokri et al. 2017)")
}

membership_inference <- memb

#' .memb_cheatsheet
#'
#' A step of the memb_native implementation. Called by \code{morie_memb}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.memb_cheatsheet <- function() {
  paste("memb: membership inference (Shokri et al. 2017). Black-box ",
        "output vector in, member/non-member out. Train k SHADOW ",
        "models on data distributed like the target's, where you DO ",
        "know membership; their outputs on their own training data are ",
        "labelled 'in' and on a disjoint test set 'out'; that labelled ",
        "set trains the attack model -- one per output class, since ",
        "the tell is class-conditional. Shadow data from Algorithm 1 ",
        "synthesis against the target, from feature marginals, or from ",
        "noisy real data. Metrics are precision and recall over ",
        "members. The attack lives on the train/test gap: no ",
        "overfitting, no attack.", sep = "")
}

#' morie_memb
#'
#' A step of the memb_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param op A vector; its length is taken.
#' @param ... Passed through.
#' @return The value of \code{switch}.
#' @export
morie_memb <- function(op, ...) {
  if (missing(op) || length(op) != 1L)
    stop("memb: op must be one of memb, attack_dataset, synthesize, synthesize_marginals, synthesize_noisy, precision_recall, logistic_trainer, knn_trainer, cheatsheet")
  op <- as.character(op)
  switch(op,
    "memb" = memb(...),
    "membership_inference" = memb(...),
    "attack_dataset" = attack_dataset(...),
    "synthesize" = synthesize(...),
    "synthesize_marginals" = synthesize_marginals(...),
    "synthesize_noisy" = synthesize_noisy(...),
    "precision_recall" = precision_recall(...),
    "logistic_trainer" = list(train_fn = logistic_trainer(...)),
    "knn_trainer" = list(train_fn = knn_trainer(...)),
    "cheatsheet" = list(cheatsheet = .memb_cheatsheet()),
    stop("memb: unknown op ", shQuote(op))
  )
}
