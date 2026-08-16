# SPDX-License-Identifier: AGPL-3.0-or-later
# R port of morie.fn hm* modules (rw4_b shard). Delegates to cores already
# exported by geron_ml_native.R / geron_ml2_native.R where they exist;
# ports the rest natively. RNG-dependent Python paths (np.random.default_rng,
# PCG64) have no R equivalent, so they are substituted with the package's
# shared LCG stream (.morie_gr_lcg_u, s <- (1664525*s + 1013904223) mod 2^32,
# u <- (s+0.5)/2^32) from geron_ml_native.R -- documented non-portable,
# matching the precedent at geron_ml_native.R:3101. Anchors for those
# functions are chosen on deterministic branches/statistics, not draws.
# Indices below are 0-based only where explicitly noted (matching Python);
# everything else follows R's native 1-based indexing.

# ---------------------------------------------------------------- internal helpers (not exported)

#' .morie_gr_w4b_sigmoid
#'
#' A step of the geron_w4b_native implementation. Called by \code{morie_geron_gru}, \code{morie_geron_lstm}, \code{morie_geron_mish}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z A vector; its length is taken and its elements indexed.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_gr_w4b_sigmoid <- function(z) {
  out <- numeric(length(z))
  pos <- z >= 0
  out[pos] <- 1 / (1 + exp(-z[pos]))
  e <- exp(z[!pos])
  out[!pos] <- e / (1 + e)
  out
}

#' .morie_gr_w4b_softmax_rows
#'
#' A step of the geron_w4b_native implementation. Called by \code{.morie_gr_w4b_sdpa}, \code{morie_geron_hf_pipelines}, \code{morie_geron_knowledge_distillation} and 1 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Z A matrix; passed to \code{nrow}.
#' @return A numeric value.
#' @export
.morie_gr_w4b_softmax_rows <- function(Z) {
  Z <- as.matrix(Z)
  mx <- apply(Z, 1, max)
  E <- exp(Z - matrix(mx, nrow = nrow(Z), ncol = ncol(Z)))
  E / rowSums(E)
}

#' .morie_gr_w4b_popsd
#'
#' A step of the geron_w4b_native implementation. Called by \code{morie_geron_he_init_hmhei}, \code{morie_geron_learning_curves_hmlcv}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Numeric; passed to \code{mean}.
#' @return A numeric value.
#' @export
.morie_gr_w4b_popsd <- function(x) sqrt(mean((x - mean(x))^2))

#' .morie_gr_w4b_softplus
#'
#' A step of the geron_w4b_native implementation. Called by \code{morie_geron_mish}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z Numeric; passed to \code{abs}.
#' @return A numeric value.
#' @export
.morie_gr_w4b_softplus <- function(z) pmax(z, 0) + log1p(exp(-abs(z)))

#' .morie_gr_w4b_pairwise_distances
#'
#' A step of the geron_w4b_native implementation. Called by \code{morie_geron_isomap}, \code{morie_geron_kernel_pca_rbf_hmkprbf}, \code{morie_geron_kmeans_plus_plus} and 7 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @return A numeric value.
#' @export
.morie_gr_w4b_pairwise_distances <- function(X) {
  X <- as.matrix(X)
  n <- nrow(X)
  sq <- matrix(0, n, n)
  for (i in seq_len(n)) sq[i, ] <- rowSums((X - matrix(X[i, ], n, ncol(X), byrow = TRUE))^2)
  sqrt(pmax(sq, 0))
}

#' .morie_gr_w4b_double_center
#'
#' A step of the geron_w4b_native implementation. Called by \code{morie_geron_double_center}, \code{morie_geron_mds}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param D A matrix; passed to \code{nrow}.
#' @return A numeric value.
#' @export
.morie_gr_w4b_double_center <- function(D) {
  D <- as.matrix(D)
  n <- nrow(D)
  J <- diag(n) - matrix(1 / n, n, n)
  -0.5 * (J %*% (D^2) %*% J)
}

# ---------------------------------------------------------------- 1. hmgru

#' GRU cell forward step (Geron Ch 13, morie.fn hmgru)
#' @param x_t Input vector.
#' @param h_prev Previous hidden state.
#' @param weights Named list with W_z,U_z,b_z,W_r,U_r,b_r,W_h,U_h,b_h.
#' @return List with h_t, z_t, r_t, h_tilde, estimate, n, method.
#' @export
morie_geron_gru <- function(x_t, h_prev, weights) {
  keys <- c("W_z", "U_z", "b_z", "W_r", "U_r", "b_r", "W_h", "U_h", "b_h")
  .morie_gr_need(
    all(keys %in% names(weights)),
    paste0("geron_gru: weights must supply ", paste(keys, collapse = ", "))
  )
  x <- as.numeric(x_t)
  h <- as.numeric(h_prev)
  .morie_gr_need(length(x) > 0L && length(h) > 0L, "geron_gru: x_t and h_prev must be non-empty")
  .morie_gr_need(all(is.finite(x)) && all(is.finite(h)), "geron_gru: x_t and h_prev must be finite")
  n_in <- length(x)
  n_units <- length(h)
  W <- list()
  for (k in keys) {
    arr <- weights[[k]]
    if (startsWith(k, "b")) {
      arr <- as.numeric(arr)
      .morie_gr_need(length(arr) == n_units, paste0("geron_gru: weights[['", k, "']] has the wrong length"))
    } else {
      arr <- as.matrix(arr)
      want <- if (startsWith(k, "W")) c(n_units, n_in) else c(n_units, n_units)
      .morie_gr_need(all(dim(arr) == want), paste0("geron_gru: weights[['", k, "']] has the wrong shape"))
    }
    .morie_gr_need(all(is.finite(arr)), paste0("geron_gru: weights[['", k, "']] contains non-finite values"))
    W[[k]] <- arr
  }
  z <- .morie_gr_w4b_sigmoid(as.numeric(W$W_z %*% x + W$U_z %*% h + W$b_z))
  r <- .morie_gr_w4b_sigmoid(as.numeric(W$W_r %*% x + W$U_r %*% h + W$b_r))
  h_tilde <- tanh(as.numeric(W$W_h %*% x + W$U_h %*% (r * h) + W$b_h))
  h_t <- (1 - z) * h + z * h_tilde
  list(
    h_t = h_t, z_t = z, r_t = r, h_tilde = h_tilde,
    estimate = sqrt(sum(h_t^2)), n = n_units, method = "GRU cell forward step"
  )
}

# ---------------------------------------------------------------- 2. hmhei

#' He (Kaiming) initialization (Geron Ch 11, morie.fn hmhei)
#'
#' RNG substitute: LCG uniform stream mapped through qnorm (normal) or an
#' affine map (uniform); not bit-portable to numpy's PCG64. Exact anchors
#' use the deterministic targets (var_target/std_target/limit/shape); the
#' draw itself is checked only statistically.
#' @param fan_in,fan_out Layer widths.
#' @param seed Seed.
#' @param distribution "normal" or "uniform".
#' @return List with W, std_target, var_target, limit, empirical_std, estimate, n, method.
#' @export
morie_geron_he_init_hmhei <- function(fan_in, seed = 0, fan_out = NULL, distribution = "normal") {
  n_in <- as.integer(fan_in)
  .morie_gr_need(n_in >= 1L, "geron_he_init: fan_in must be a positive integer")
  n_out <- if (is.null(fan_out)) n_in else as.integer(fan_out)
  .morie_gr_need(n_out >= 1L, "geron_he_init: fan_out must be a positive integer")
  .morie_gr_need(
    distribution %in% c("normal", "uniform"),
    "geron_he_init: distribution must be 'normal' or 'uniform'"
  )
  var <- 2 / n_in
  std <- sqrt(var)
  limit <- sqrt(6 / n_in)
  u <- .morie_gr_lcg_u(n_in * n_out, seed)
  W <- if (distribution == "normal") {
    matrix(stats::qnorm(u, 0, std), nrow = n_in, ncol = n_out, byrow = TRUE)
  } else {
    matrix(-limit + 2 * limit * u, nrow = n_in, ncol = n_out, byrow = TRUE)
  }
  list(
    W = W, std_target = std, var_target = var, limit = limit,
    empirical_std = .morie_gr_w4b_popsd(as.vector(W)), fan_in = n_in, fan_out = n_out,
    distribution = distribution, estimate = std, n = n_in * n_out,
    method = "He (Kaiming) initialization"
  )
}

# ---------------------------------------------------------------- 3. hmhfpi

#' Task pipeline: preprocess -> model -> postprocess (Geron Ch 14, morie.fn hmhfpi)
#' @param task One of text-classification, sentiment-analysis, feature-extraction, text-generation.
#' @param inputs List of inputs.
#' @param model function(inputs) -> raw output.
#' @param labels Optional label names.
#' @param top_k Ranked labels per input.
#' @return List with predictions, scores, raw, task, estimate, n, method.
#' @export
morie_geron_hf_pipelines <- function(task, inputs, model, labels = NULL, top_k = 1) {
  tasks <- c("text-classification", "sentiment-analysis", "feature-extraction", "text-generation")
  .morie_gr_need(task %in% tasks, "geron_hf_pipelines: unsupported task")
  .morie_gr_need(is.function(model), "geron_hf_pipelines: model must be callable")
  items <- as.list(inputs)
  .morie_gr_need(length(items) > 0L, "geron_hf_pipelines: inputs is empty")
  raw <- model(items)

  if (task %in% c("text-classification", "sentiment-analysis")) {
    logits <- if (is.null(dim(raw))) matrix(as.numeric(raw), nrow = 1) else as.matrix(raw)
    storage.mode(logits) <- "double"
    .morie_gr_need(nrow(logits) == length(items), "geron_hf_pipelines: row count mismatch")
    .morie_gr_need(all(is.finite(logits)), "geron_hf_pipelines: non-finite logits")
    n_lab <- ncol(logits)
    names_lab <- if (is.null(labels)) paste0("LABEL_", seq_len(n_lab) - 1L) else as.character(labels)
    .morie_gr_need(length(names_lab) == n_lab, "geron_hf_pipelines: labels length mismatch")
    k <- as.integer(top_k)
    .morie_gr_need(k >= 1L && k <= n_lab, "geron_hf_pipelines: top_k out of range")
    probs <- .morie_gr_w4b_softmax_rows(logits)
    preds <- vector("list", length(items))
    for (i in seq_along(items)) {
      ord <- order(probs[i, ], decreasing = TRUE)[seq_len(k)]
      preds[[i]] <- if (k == 1L) {
        list(label = names_lab[ord[1]], score = probs[i, ord[1]])
      } else {
        lapply(ord, function(j) list(label = names_lab[j], score = probs[i, j]))
      }
    }
    out <- preds
    scores <- probs
    headline <- mean(apply(probs, 1, max))
  } else if (task == "feature-extraction") {
    vecs <- lapply(raw, as.numeric)
    .morie_gr_need(length(vecs) == length(items), "geron_hf_pipelines: vector count mismatch")
    widths <- unique(vapply(vecs, length, integer(1)))
    .morie_gr_need(length(widths) == 1L, "geron_hf_pipelines: inconsistent widths")
    out <- do.call(rbind, vecs)
    .morie_gr_need(all(is.finite(out)), "geron_hf_pipelines: non-finite features")
    scores <- NULL
    headline <- mean(sqrt(rowSums(out^2)))
  } else {
    gens <- as.list(raw)
    .morie_gr_need(length(gens) == length(items), "geron_hf_pipelines: generation count mismatch")
    out <- lapply(gens, function(g) list(generated_text = g))
    scores <- NULL
    headline <- mean(vapply(gens, function(g) nchar(as.character(g)), numeric(1)))
  }
  list(
    predictions = out, scores = scores, raw = raw, task = task, estimate = headline,
    n = length(items), method = "Task pipeline (preprocess -> model -> postprocess)"
  )
}

# ---------------------------------------------------------------- 4. hmhftn

#' Trainer loop: mini-batch SGD with per-epoch evaluation (Geron Ch 14, morie.fn hmhftn)
#'
#' Shuffle order comes from the shared LCG stream, not numpy's PCG64
#' permutation; with batch_size >= nrow(X) there is one batch per epoch so
#' the result is exact and shuffle-independent.
#' @param model List with params, loss_and_grad = function(params,X,y) -> list(loss, grad).
#' @param args List with epochs, batch_size, learning_rate, seed.
#' @param train_ds,eval_ds List(X, y).
#' @return List with params, best_params, train_loss, eval_loss, history, best_epoch, estimate, n, method.
#' @export
morie_geron_hf_trainer <- function(model, args = list(), train_ds, eval_ds = NULL) {
  .morie_gr_need(
    !is.null(model$params) && !is.null(model$loss_and_grad),
    "geron_hf_trainer: model needs params and loss_and_grad"
  )
  params <- as.numeric(model$params)
  lg <- model$loss_and_grad
  .morie_gr_need(is.function(lg), "geron_hf_trainer: model$loss_and_grad must be callable")
  .morie_gr_need(length(params) > 0L, "geron_hf_trainer: model$params is empty")
  epochs <- as.integer(if (is.null(args$epochs)) 1L else args$epochs)
  batch_size <- as.integer(if (is.null(args$batch_size)) 8L else args$batch_size)
  lr <- as.numeric(if (is.null(args$learning_rate)) 0.01 else args$learning_rate)
  seed <- as.integer(if (is.null(args$seed)) 0L else args$seed)
  .morie_gr_need(epochs >= 1L, "geron_hf_trainer: epochs must be at least 1")
  .morie_gr_need(batch_size >= 1L, "geron_hf_trainer: batch_size must be at least 1")
  .morie_gr_need(is.finite(lr) && lr > 0, "geron_hf_trainer: learning_rate must be positive and finite")

  mkds <- function(d) list(X = as.matrix(d[[1]]), y = as.numeric(d[[2]]))
  tr <- mkds(train_ds)
  .morie_gr_need(nrow(tr$X) == length(tr$y), "geron_hf_trainer: train_ds row mismatch")
  ev <- if (is.null(eval_ds)) tr else mkds(eval_ds)
  m <- nrow(tr$X)
  history <- vector("list", epochs)
  best_params <- params
  best_loss <- Inf
  best_epoch <- 0L
  for (ep in seq_len(epochs)) {
    u <- .morie_gr_lcg_u(m, seed + 7919L * (ep - 1L))
    order_idx <- order(u)
    losses <- numeric(0)
    starts <- seq(1L, m, by = batch_size)
    step <- 0L
    for (st in starts) {
      step <- step + 1L
      idx <- order_idx[st:min(st + batch_size - 1L, m)]
      res <- lg(params, tr$X[idx, , drop = FALSE], tr$y[idx])
      loss <- as.numeric(res[[1]])
      grad <- as.numeric(res[[2]])
      .morie_gr_need(
        length(grad) == length(params),
        sprintf("geron_hf_trainer: gradient shape mismatch at epoch %d step %d", ep, step)
      )
      .morie_gr_need(
        is.finite(loss) && all(is.finite(grad)),
        sprintf("geron_hf_trainer: non-finite loss/grad at epoch %d step %d", ep, step)
      )
      losses <- c(losses, loss)
      params <- params - lr * grad
    }
    train_loss <- mean(losses)
    ev_res <- lg(params, ev$X, ev$y)
    eval_loss <- as.numeric(ev_res[[1]])
    .morie_gr_need(is.finite(eval_loss), sprintf("geron_hf_trainer: eval loss non-finite at epoch %d", ep))
    history[[ep]] <- list(epoch = ep, train_loss = train_loss, eval_loss = eval_loss)
    if (eval_loss < best_loss) {
      best_loss <- eval_loss
      best_params <- params
      best_epoch <- ep
    }
  }
  list(
    params = params, best_params = best_params, train_loss = history[[epochs]]$train_loss,
    eval_loss = best_loss, history = history, best_epoch = best_epoch, estimate = best_loss,
    n = m, method = "Trainer loop (mini-batch SGD with per-epoch evaluation)"
  )
}

# ---------------------------------------------------------------- 5. hmhgb

#' .morie_gr_w4b_bincount
#'
#' A step of the geron_w4b_native implementation. Called by \code{.morie_gr_w4b_hgb_best_split}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A vector; its length is taken.
#' @param w See Usage.
#' @param minlength A count; the body uses it as \code{numeric(...)}.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_gr_w4b_bincount <- function(x, w, minlength) {
  out <- numeric(minlength)
  if (length(x)) {
    agg <- tapply(w, factor(x, levels = 0:(minlength - 1)), sum)
    agg[is.na(agg)] <- 0
    out <- as.numeric(agg)
  }
  out
}

#' .morie_gr_w4b_searchsorted_left
#'
#' A step of the geron_w4b_native implementation. Called by \code{morie_geron_histogram_gradient_boosting}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param e A vector; its length is taken.
#' @param x A vector; its length is taken.
#' @return A vector, from \code{vapply}.
#' @export
.morie_gr_w4b_searchsorted_left <- function(e, x) {
  if (length(e) == 0L) {
    return(rep(0L, length(x)))
  }
  vapply(x, function(xi) sum(e < xi), integer(1))
}

#' .morie_gr_w4b_hgb_best_split
#'
#' A step of the geron_w4b_native implementation. Called by \code{.morie_gr_w4b_hgb_grow}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param binned A matrix; indexed by row and column.
#' @param grad Numeric; passed to \code{sum}.
#' @param hess Numeric; passed to \code{sum}.
#' @param n_bins Numeric; combined arithmetically in the body.
#' @param min_leaf See Usage.
#' @return A list with \code{gain}, \code{feature}, \code{bin0}.
#' @export
.morie_gr_w4b_hgb_best_split <- function(binned, grad, hess, n_bins, min_leaf) {
  n_feat <- ncol(binned)
  G <- sum(grad)
  H <- sum(hess)
  parent <- if (H > 0) G * G / H else 0
  best_gain <- 0
  best_j <- -1L
  best_bin0 <- -1L
  for (j in seq_len(n_feat)) {
    gh <- .morie_gr_w4b_bincount(binned[, j], grad, n_bins)
    hh <- .morie_gr_w4b_bincount(binned[, j], hess, n_bins)
    gl <- cumsum(gh)[seq_len(n_bins - 1L)]
    hl <- cumsum(hh)[seq_len(n_bins - 1L)]
    gr <- G - gl
    hr <- H - hl
    ok <- (hl >= min_leaf) & (hr >= min_leaf)
    if (!any(ok)) next
    gain <- ifelse(ok, gl^2 / ifelse(hl > 0, hl, 1) + gr^2 / ifelse(hr > 0, hr, 1) - parent, -Inf)
    b <- which.max(gain)
    if (gain[b] > best_gain) {
      best_gain <- gain[b]
      best_j <- j
      best_bin0 <- b - 1L
    }
  }
  list(gain = best_gain, feature = best_j, bin0 = best_bin0)
}

#' .morie_gr_w4b_hgb_grow
#'
#' A step of the geron_w4b_native implementation. Called by \code{morie_geron_histogram_gradient_boosting}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param binned A matrix; indexed by row and column.
#' @param grad A vector; indexed elementwise.
#' @param hess A vector; indexed elementwise.
#' @param rows A vector; its length is taken and its elements indexed.
#' @param depth Numeric; combined arithmetically in the body.
#' @param max_depth Passed to \code{.morie_gr_w4b_hgb_grow}.
#' @param n_bins Passed to \code{.morie_gr_w4b_hgb_best_split}.
#' @param min_leaf Numeric; combined arithmetically in the body.
#' @return A list with \code{feature}, \code{bin0}, \code{gain}, \code{left}, \code{right}.
#' @export
.morie_gr_w4b_hgb_grow <- function(binned, grad, hess, rows, depth, max_depth, n_bins, min_leaf) {
  g <- grad[rows]
  h <- hess[rows]
  leafval <- function() if (sum(h) > 0) sum(g) / sum(h) else 0
  if (depth >= max_depth || length(rows) < 2L * min_leaf) {
    return(list(leaf = leafval()))
  }
  bs <- .morie_gr_w4b_hgb_best_split(binned[rows, , drop = FALSE], g, h, n_bins, min_leaf)
  if (bs$feature < 0L || bs$gain <= 1e-12) {
    return(list(leaf = leafval()))
  }
  left_mask <- binned[rows, bs$feature] <= bs$bin0
  left <- rows[left_mask]
  right <- rows[!left_mask]
  if (length(left) == 0L || length(right) == 0L) {
    return(list(leaf = leafval()))
  }
  list(
    feature = bs$feature, bin0 = bs$bin0, gain = bs$gain,
    left = .morie_gr_w4b_hgb_grow(binned, grad, hess, left, depth + 1L, max_depth, n_bins, min_leaf),
    right = .morie_gr_w4b_hgb_grow(binned, grad, hess, right, depth + 1L, max_depth, n_bins, min_leaf)
  )
}

#' .morie_gr_w4b_hgb_predict
#'
#' A step of the geron_w4b_native implementation. Called by \code{morie_geron_histogram_gradient_boosting}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param node See Usage.
#' @param binned A matrix; indexed by row and column.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_gr_w4b_hgb_predict <- function(node, binned) {
  out <- numeric(nrow(binned))
  stack <- list(list(node = node, rows = seq_len(nrow(binned))))
  while (length(stack)) {
    top <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    nd <- top$node
    rows <- top$rows
    if (!is.null(nd$leaf)) {
      out[rows] <- nd$leaf
      next
    }
    mask <- binned[rows, nd$feature] <= nd$bin0
    stack[[length(stack) + 1L]] <- list(node = nd$left, rows = rows[mask])
    stack[[length(stack) + 1L]] <- list(node = nd$right, rows = rows[!mask])
  }
  out
}

#' Histogram-based gradient boosting, squared loss (Geron Ch 6, morie.fn hmhgb)
#' @param X,y Design matrix and targets.
#' @param max_iter Rounds.
#' @param learning_rate Shrinkage.
#' @param max_bins Bins per feature.
#' @param max_depth Tree depth.
#' @param min_samples_leaf Min rows/leaf.
#' @return List with prediction, train_mse, mse_history, baseline, trees, bins_used, bin_edges, estimate, n, method.
#' @export
morie_geron_histogram_gradient_boosting <- function(X, y, max_iter = 100, learning_rate = 0.1,
                                                    max_bins = 255, max_depth = 3, min_samples_leaf = 1) {
  A <- if (is.null(dim(X))) matrix(as.numeric(X), ncol = 1) else as.matrix(X)
  storage.mode(A) <- "double"
  .morie_gr_need(nrow(A) > 0L && ncol(A) > 0L, "geron_histogram_gradient_boosting: X must be non-empty")
  yy <- as.numeric(y)
  .morie_gr_need(length(yy) == nrow(A), "geron_histogram_gradient_boosting: row mismatch")
  .morie_gr_need(all(is.finite(A)) && all(is.finite(yy)), "geron_histogram_gradient_boosting: must be finite")
  rounds <- as.integer(max_iter)
  .morie_gr_need(rounds >= 1L, "geron_histogram_gradient_boosting: max_iter must be >= 1")
  lr <- as.numeric(learning_rate)
  .morie_gr_need(lr > 0 && lr <= 1, "geron_histogram_gradient_boosting: bad learning_rate")
  bins <- as.integer(max_bins)
  .morie_gr_need(bins >= 2L, "geron_histogram_gradient_boosting: max_bins must be >= 2")
  depth <- as.integer(max_depth)
  .morie_gr_need(depth >= 1L, "geron_histogram_gradient_boosting: max_depth must be >= 1")
  msl <- as.integer(min_samples_leaf)
  .morie_gr_need(msl >= 1L, "geron_histogram_gradient_boosting: min_samples_leaf must be >= 1")

  m <- nrow(A)
  n_feat <- ncol(A)
  binned <- matrix(0L, nrow = m, ncol = n_feat)
  edges <- vector("list", n_feat)
  used <- integer(n_feat)
  for (j in seq_len(n_feat)) {
    col <- A[, j]
    distinct <- sort(unique(col))
    e <- if (length(distinct) <= bins) {
      if (length(distinct) > 1L) distinct[-length(distinct)] else numeric(0)
    } else {
      qs <- seq(0, 1, length.out = bins + 1L)[2:bins]
      sort(unique(stats::quantile(col, qs, type = 7, names = FALSE)))
    }
    edges[[j]] <- e
    binned[, j] <- .morie_gr_w4b_searchsorted_left(e, col)
    used[j] <- max(binned[, j]) + 1L
  }
  n_bin_slots <- max(max(used), 2L)

  baseline <- mean(yy)
  pred <- rep(baseline, m)
  hess <- rep(1, m)
  trees <- vector("list", rounds)
  history <- numeric(rounds + 1L)
  history[1] <- mean((pred - yy)^2)
  rows_all <- seq_len(m)
  for (t in seq_len(rounds)) {
    grad <- yy - pred
    tree <- .morie_gr_w4b_hgb_grow(binned, grad, hess, rows_all, 0L, depth, n_bin_slots, msl)
    step <- .morie_gr_w4b_hgb_predict(tree, binned)
    pred <- pred + lr * step
    trees[[t]] <- tree
    history[t + 1L] <- mean((pred - yy)^2)
  }
  mse <- history[rounds + 1L]
  list(
    prediction = pred, train_mse = mse, mse_history = history, baseline = baseline, trees = trees,
    bins_used = used, bin_edges = edges, learning_rate = lr, estimate = mse, n = m,
    method = "Histogram-based gradient boosting (squared loss)"
  )
}

# ---------------------------------------------------------------- 6. hmhplm

#' Depth selection by validation-error plateau (Geron Ch 9, morie.fn hmhplm)
#'
#' Validation split order comes from the shared LCG stream, not numpy's
#' PCG64 permutation.
#' @param model function(n_layers, X_train, y_train, X_val, y_val) -> val_error.
#' @param X,y Data.
#' @param max_layers,min_layers Depth range.
#' @param patience Non-improving depths tolerated.
#' @param tol Minimum improvement.
#' @param val_fraction Held-out fraction.
#' @param seed Split seed.
#' @return List with best_n_layers, best_error, errors, depths_tried, stopped_early, estimate, n, method.
#' @export
morie_geron_hidden_layers_heuristic <- function(model, X, y, max_layers = 10, min_layers = 1,
                                                patience = 2, tol = 1e-4, val_fraction = 0.2, seed = 0) {
  .morie_gr_need(is.function(model), "geron_hidden_layers_heuristic: model must be callable")
  A <- as.matrix(X)
  yy <- as.numeric(y)
  .morie_gr_need(nrow(A) > 0L && nrow(A) == length(yy), "geron_hidden_layers_heuristic: X/y mismatch")
  lo <- as.integer(min_layers)
  hi <- as.integer(max_layers)
  .morie_gr_need(lo >= 1L && hi >= lo, "geron_hidden_layers_heuristic: bad layer range")
  pat <- as.integer(patience)
  .morie_gr_need(pat >= 1L, "geron_hidden_layers_heuristic: patience must be >= 1")
  t <- as.numeric(tol)
  .morie_gr_need(is.finite(t) && t >= 0, "geron_hidden_layers_heuristic: bad tol")
  vf <- as.numeric(val_fraction)
  .morie_gr_need(vf > 0 && vf < 1, "geron_hidden_layers_heuristic: bad val_fraction")

  m <- nrow(A)
  n_val <- as.integer(round(m * vf))
  .morie_gr_need(n_val >= 1L && (m - n_val) >= 1L, "geron_hidden_layers_heuristic: unusable split")
  u <- .morie_gr_lcg_u(m, seed)
  perm <- order(u)
  vi <- perm[seq_len(n_val)]
  ti <- perm[(n_val + 1L):m]
  Xt <- A[ti, , drop = FALSE]
  yt <- yy[ti]
  Xv <- A[vi, , drop = FALSE]
  yv <- yy[vi]

  errors <- list()
  best_L <- NA_integer_
  best_err <- Inf
  stale <- 0L
  stopped <- FALSE
  for (L in lo:hi) {
    err <- as.numeric(model(L, Xt, yt, Xv, yv))
    .morie_gr_need(
      is.finite(err),
      sprintf("geron_hidden_layers_heuristic: model returned a non-finite error at depth %d", L)
    )
    errors[[as.character(L)]] <- err
    if (err < best_err - t) {
      best_err <- err
      best_L <- L
      stale <- 0L
    } else {
      stale <- stale + 1L
      if (stale >= pat) {
        stopped <- TRUE
        break
      }
    }
  }
  if (is.na(best_L)) {
    best_L <- lo
    best_err <- errors[[as.character(lo)]]
  }
  list(
    best_n_layers = best_L, best_error = best_err, errors = errors, depths_tried = length(errors),
    stopped_early = stopped, estimate = best_err, n = m,
    method = "Depth selection by validation-error plateau"
  )
}

# ---------------------------------------------------------------- 7. hmhpt (uses internal ridge/grid search + existing CV core)

#' .morie_gr_w4b_ridge_estimator
#'
#' A step of the geron_w4b_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Xtr A matrix; passed to \code{as.matrix}.
#' @param ytr See Usage.
#' @param alpha Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @return The value of \code{function}.
#' @export
.morie_gr_w4b_ridge_estimator <- function(Xtr, ytr, alpha = 0.0) {
  X <- as.matrix(Xtr)
  y <- as.numeric(ytr)
  .morie_gr_need(alpha >= 0, "ridge_estimator: alpha must be non-negative")
  n <- ncol(X)
  theta <- solve(t(X) %*% X + alpha * diag(n), t(X) %*% y)
  function(Xnew) as.numeric(as.matrix(Xnew) %*% theta)
}

#' .morie_gr_w4b_grid_search
#'
#' A step of the geron_w4b_native implementation. Called by \code{morie_geron_hyperparameter_tuning}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param param_grid See Usage.
#' @param X Passed to \code{.morie_gr_mat}.
#' @param y See Usage.
#' @param estimator Defaults to \code{NULL}.
#' @param K Passed to \code{morie_geron_cross_validation_score}. Defaults to \code{3}.
#' @param score Passed to \code{morie_geron_cross_validation_score}.
#' @return A list with \code{best_params}, \code{best_score}, \code{results}, \code{n_candidates}, \code{n_fits}, \code{estimate}, \code{n}, \code{method}.
#' @export
.morie_gr_w4b_grid_search <- function(param_grid, X, y, estimator = NULL, K = 3, score = NULL) {
  names_p <- names(param_grid)
  .morie_gr_need(length(names_p) > 0L, "geron_grid_search: param_grid is empty")
  # param_grid values are already atomic vectors; expand.grid on them (not on
  # as.list()-wrapped pools) keeps columns atomic instead of list-columns,
  # so grid[i, ] scalars pass straight through to the estimator.
  grid <- expand.grid(param_grid, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  est <- if (is.null(estimator)) .morie_gr_w4b_ridge_estimator else estimator
  A <- .morie_gr_mat(X, "X")
  yy <- as.numeric(y)
  results <- vector("list", nrow(grid))
  best <- NULL
  for (i in seq_len(nrow(grid))) {
    params <- as.list(grid[i, , drop = FALSE])
    fit_fn <- function(Xtr, ytr) do.call(est, c(list(Xtr, ytr), params))
    pred_fn <- function(mdl, Xte) mdl(Xte)
    cv <- morie_geron_cross_validation_score(A, yy, K = K, fit = fit_fn, predict = pred_fn, score = score)
    s <- cv$cv_score
    results[[i]] <- list(params = params, cv_score = s, fold_scores = cv$fold_scores)
    if (is.null(best) || s > best$cv_score) best <- results[[i]]
  }
  list(
    best_params = best$params, best_score = best$cv_score, results = results,
    n_candidates = nrow(grid), n_fits = nrow(grid) * as.integer(K), estimate = best$cv_score,
    n = nrow(A), method = "Exhaustive grid search with K-fold cross-validation"
  )
}

#' Hyperparameter tuning by grid or random search (Geron Ch 1, morie.fn hmhpt)
#'
#' The grid branch is exact and deterministic (delegates to the existing
#' morie_geron_cross_validation_score core). The random branch's candidate
#' draws use the shared LCG stream, not numpy's PCG64 -- non-portable.
#' @param param_grid Named list of candidate value vectors.
#' @param X,y Data.
#' @param estimator Optional function(Xtr,ytr,...) -> function(Xnew) predict.
#' @param search "grid" or "random".
#' @param n_iter Random draws.
#' @param K Folds.
#' @param seed Seed.
#' @param score Optional scorer.
#' @return List with best_params, best_score, results, n_candidates, n_fits, distinct_values_per_param, estimate, n, method.
#' @export
morie_geron_hyperparameter_tuning <- function(param_grid, X, y, estimator = NULL, search = "grid",
                                              n_iter = 10, K = 3, seed = 0, score = NULL) {
  .morie_gr_need(search %in% c("grid", "random"), "geron_hyperparameter_tuning: bad search")
  names_p <- names(param_grid)
  .morie_gr_need(length(names_p) > 0L, "geron_hyperparameter_tuning: param_grid is empty")
  pools <- lapply(param_grid, as.list)
  distinct <- lapply(pools, function(v) length(unique(vapply(v, function(x) paste(x, collapse = ","), ""))))

  if (search == "grid") {
    inner <- .morie_gr_w4b_grid_search(param_grid, X, y, estimator = estimator, K = K, score = score)
    inner$distinct_values_per_param <- distinct
    inner$search <- "grid"
    inner$method <- "Hyperparameter tuning by grid or random search"
    return(inner)
  }

  iters <- as.integer(n_iter)
  .morie_gr_need(iters >= 1L, "geron_hyperparameter_tuning: n_iter must be >= 1")
  est <- if (is.null(estimator)) .morie_gr_w4b_ridge_estimator else estimator
  A <- .morie_gr_mat(X, "X")
  yy <- as.numeric(y)
  results <- vector("list", iters)
  best <- NULL
  for (i in seq_len(iters)) {
    u <- .morie_gr_lcg_u(length(names_p), seed + 104729L * i)
    params <- setNames(vector("list", length(names_p)), names_p)
    for (j in seq_along(names_p)) {
      pool <- pools[[names_p[j]]]
      idx <- min(length(pool), 1L + floor(u[j] * length(pool)))
      params[[names_p[j]]] <- pool[[idx]]
    }
    fit_fn <- function(Xtr, ytr) do.call(est, c(list(Xtr, ytr), params))
    pred_fn <- function(mdl, Xte) mdl(Xte)
    cv <- morie_geron_cross_validation_score(A, yy, K = K, fit = fit_fn, predict = pred_fn, score = score)
    s <- cv$cv_score
    results[[i]] <- list(params = params, cv_score = s, fold_scores = cv$fold_scores)
    if (is.null(best) || s > best$cv_score) best <- results[[i]]
  }
  list(
    best_params = best$params, best_score = best$cv_score, results = results,
    n_candidates = iters, n_fits = iters * as.integer(K), distinct_values_per_param = distinct,
    search = "random", estimate = best$cv_score, n = nrow(A),
    method = "Hyperparameter tuning by grid or random search"
  )
}

# ---------------------------------------------------------------- 8. hmicl

#' In-context learning by prompt-conditioned scoring (Geron Ch 15, morie.fn hmicl)
#' @param model function(prompt, candidate) -> log-probability.
#' @param examples List of (x,y) pairs.
#' @param query Query input.
#' @param candidates Optional label set.
#' @param template,separator Prompt assembly.
#' @return List with prediction, prompt, log_probs, posterior, n_shot, candidates, estimate, n, method.
#' @export
morie_geron_in_context_learning_hmicl <- function(model, examples, query, candidates = NULL,
                                                  template = "{x} -> {y}", separator = "\n") {
  .morie_gr_need(is.function(model), "geron_in_context_learning: model must be callable")
  pairs <- examples
  cands <- if (is.null(candidates)) {
    seen <- c()
    for (ex in pairs) {
      y <- ex[[2]]
      if (!(y %in% seen)) seen <- c(seen, y)
    }
    seen
  } else {
    as.list(candidates)
  }
  .morie_gr_need(length(cands) > 0L, "geron_in_context_learning: no candidate labels")
  .morie_gr_need(grepl("\\{x\\}", template, fixed = FALSE), "geron_in_context_learning: template needs {x}")
  fmt <- function(x, y) {
    s <- template
    s <- sub("{x}", as.character(x), s, fixed = TRUE)
    sub("{y}", as.character(y), s, fixed = TRUE)
  }
  lines <- vapply(pairs, function(ex) fmt(ex[[1]], ex[[2]]), character(1))
  query_line <- trimws(fmt(query, ""), which = "right")
  prompt <- paste(c(lines, query_line), collapse = separator)

  scores <- numeric(length(cands))
  for (i in seq_along(cands)) {
    s <- as.numeric(model(prompt, cands[[i]]))
    .morie_gr_need(is.finite(s), "geron_in_context_learning: model returned a non-finite log-probability")
    scores[i] <- s
  }
  shifted <- scores - max(scores)
  e <- exp(shifted)
  post <- e / sum(e)
  best <- which.max(scores)
  list(
    prediction = cands[[best]], prompt = prompt, log_probs = scores, posterior = post,
    n_shot = length(pairs), candidates = cands, estimate = post[best], n = length(pairs),
    method = "In-context learning by prompt-conditioned scoring"
  )
}

# ---------------------------------------------------------------- 9. hmigr (delegates n_groups==2 case to morie_geron_cart_split_cost)

#' Information gain from a split, entropy criterion (Geron Ch 5, morie.fn hmigr)
#' @param y Class labels.
#' @param split Group assignment (same length as y).
#' @return List with information_gain, parent_entropy, child_entropy, weighted_child_entropy,
#'   intrinsic_information, gain_ratio, estimate, n, method.
#' @export
morie_geron_information_gain_hmigr <- function(y, split) {
  yy <- as.vector(y)
  ss <- as.vector(split)
  .morie_gr_need(length(yy) > 0L, "geron_information_gain: y is empty")
  .morie_gr_need(length(yy) == length(ss), "geron_information_gain: y/split length mismatch")
  groups <- unique(ss)
  n_groups <- length(groups)
  m <- length(yy)
  inverse <- match(ss, groups)
  counts <- as.numeric(table(factor(inverse, levels = seq_len(n_groups))))

  entropy_fn <- function(labels) {
    if (length(labels) == 0L) {
      return(0)
    }
    p <- as.numeric(table(labels)) / length(labels)
    -sum(p * log2(p))
  }

  if (n_groups == 2L) {
    indicator <- matrix(as.numeric(inverse) - 1, ncol = 1)
    cart <- morie_geron_cart_split_cost(indicator, yy, feature = 0, threshold = 0.5, criterion = "entropy")
    weighted <- cart$cost
    parent <- cart$impurity_parent
    child <- c(cart$impurity_left, cart$impurity_right)
  } else {
    parent <- entropy_fn(yy)
    child <- vapply(seq_len(n_groups), function(g) entropy_fn(yy[inverse == g]), numeric(1))
    weighted <- sum((counts / m) * child)
  }
  gain <- parent - weighted
  p_groups <- counts / m
  intrinsic <- if (n_groups > 1L) -sum(p_groups * log2(p_groups)) else 0
  ratio <- if (intrinsic > 0) gain / intrinsic else 0

  list(
    information_gain = gain, parent_entropy = parent, child_entropy = child,
    weighted_child_entropy = weighted, group_sizes = counts, n_groups = n_groups,
    intrinsic_information = intrinsic, gain_ratio = ratio, estimate = gain, n = m,
    method = "Information gain (entropy impurity decrease)"
  )
}

# ---------------------------------------------------------------- 10. hmins

#' k-nearest-neighbour (instance-based) prediction (Geron Ch 1, morie.fn hmins)
#' @param X_train,y_train Stored instances/targets.
#' @param x_query Query point(s).
#' @param k Neighbours.
#' @param task auto/classification/regression.
#' @param weights uniform/distance.
#' @return List with prediction, neighbors, distances, task, feature_ranges, estimate, n, method.
#' @export
morie_geron_instance_based <- function(X_train, y_train, x_query, k = 1, task = "auto", weights = "uniform") {
  A <- as.matrix(X_train)
  storage.mode(A) <- "double"
  yy <- as.vector(y_train)
  .morie_gr_need(nrow(A) == length(yy), "geron_instance_based: X_train/y_train mismatch")
  Q <- if (is.null(dim(x_query))) matrix(as.numeric(x_query), nrow = 1) else as.matrix(x_query)
  storage.mode(Q) <- "double"
  .morie_gr_need(ncol(Q) == ncol(A), "geron_instance_based: x_query width mismatch")
  kk <- as.integer(k)
  .morie_gr_need(kk >= 1L && kk <= nrow(A), "geron_instance_based: k out of range")
  .morie_gr_need(weights %in% c("uniform", "distance"), "geron_instance_based: bad weights")
  .morie_gr_need(task %in% c("auto", "classification", "regression"), "geron_instance_based: bad task")

  if (task == "auto") {
    yf <- suppressWarnings(as.numeric(yy))
    numeric_y <- !any(is.na(yf))
    integral <- numeric_y && all(yf == floor(yf))
    resolved <- if (!numeric_y || (integral && length(unique(yy)) <= max(2, length(yy) %/% 2))) "classification" else "regression"
  } else {
    resolved <- task
  }

  nq <- nrow(Q)
  ntr <- nrow(A)
  D <- matrix(0, nq, ntr)
  for (i in seq_len(nq)) D[i, ] <- sqrt(pmax(colSums((t(A) - Q[i, ])^2), 0))
  nn <- matrix(0L, nq, kk)
  dist <- matrix(0, nq, kk)
  for (i in seq_len(nq)) {
    ord <- order(D[i, ])[seq_len(kk)]
    nn[i, ] <- ord
    dist[i, ] <- D[i, ord]
  }

  preds <- vector("list", nq)
  for (q in seq_len(nq)) {
    idx <- nn[q, ]
    d <- dist[q, ]
    w <- if (weights == "distance") {
      if (any(d == 0)) as.numeric(d == 0) else 1 / d
    } else {
      rep(1, kk)
    }
    if (resolved == "regression") {
      preds[[q]] <- sum(w * as.numeric(yy[idx])) / sum(w)
    } else {
      classes <- unique(yy[idx])
      totals <- vapply(classes, function(c) sum(w[yy[idx] == c]), numeric(1))
      preds[[q]] <- classes[which.max(totals)]
    }
  }
  pred <- if (resolved == "regression") as.numeric(preds) else unlist(preds)
  ranges <- apply(A, 2, max) - apply(A, 2, min)
  spread <- if (any(ranges > 0)) max(ranges) / min(ranges[ranges > 0]) else 1

  list(
    prediction = pred, neighbors = nn, distances = dist, task = resolved, feature_ranges = ranges,
    estimate = if (is.numeric(pred)) pred[1] else 0, n = nrow(A),
    method = "k-nearest-neighbour (instance-based) prediction"
  )
}

# ---------------------------------------------------------------- 11. hmint8

#' Post-training integer quantization (Geron Ch 17, morie.fn hmint8)
#' @param x Tensor.
#' @param n_bits Bit width (2..16).
#' @param symmetric Symmetric or asymmetric scheme.
#' @return List with q, dequantized, scale, zero_point, max_error, rel_error, compression, estimate, n, method.
#' @export
morie_geron_int8_quant <- function(x, n_bits = 8, symmetric = TRUE) {
  a <- as.numeric(x)
  .morie_gr_need(length(a) > 0L, "geron_int8_quant: x is empty")
  .morie_gr_need(all(is.finite(a)), "geron_int8_quant: x contains non-finite values")
  b <- as.integer(n_bits)
  .morie_gr_need(b >= 2L && b <= 16L, "geron_int8_quant: n_bits must lie in 2..16")
  lo <- min(a)
  hi <- max(a)
  if (isTRUE(symmetric)) {
    amax <- max(abs(a))
    .morie_gr_need(amax != 0, "geron_int8_quant: the tensor is all zeros, so the scale would be zero")
    qmax <- 2^(b - 1) - 1
    scale <- amax / qmax
    zero <- 0
    q <- pmin(pmax(round(a / scale), -qmax), qmax)
    deq <- q * scale
  } else {
    .morie_gr_need(hi != lo, "geron_int8_quant: the tensor is constant, so the scale would be zero")
    qmax <- 2^b - 1
    scale <- (hi - lo) / qmax
    zero <- lo
    q <- pmin(pmax(round((a - zero) / scale), 0), qmax)
    deq <- q * scale + zero
  }
  err <- abs(deq - a)
  denom <- pmax(abs(a), .Machine$double.xmin)
  rel <- if (any(a != 0)) max(err / denom) else 0
  compression <- 64 / b
  list(
    q = q, dequantized = deq, scale = scale, zero_point = zero, max_error = max(err),
    rel_error = rel, compression = compression, n_levels = 2^b, estimate = max(err),
    n = length(a), method = "Post-training integer quantization"
  )
}

# ---------------------------------------------------------------- 12. hmipca

#' Incremental PCA via streaming co-moment updates (Geron Ch 7, morie.fn hmipca)
#' @param X_iter List of batches (matrices), or a single matrix.
#' @param n_components Components to keep.
#' @param batch_size Rows per batch when X_iter is a single matrix.
#' @return List with components, explained_variance, explained_variance_ratio, mean, n_samples_seen,
#'   n_batches, estimate, n, method.
#' @export
morie_geron_incremental_pca <- function(X_iter, n_components, batch_size = NULL) {
  if (is.list(X_iter) && !is.data.frame(X_iter) && is.null(dim(X_iter))) {
    batches <- lapply(X_iter, function(b) {
      m <- as.matrix(b)
      storage.mode(m) <- "double"
      if (is.null(dim(b)) || length(dim(b)) < 2) matrix(as.numeric(b), nrow = 1) else m
    })
  } else {
    arr <- as.matrix(X_iter)
    storage.mode(arr) <- "double"
    bs <- if (is.null(batch_size)) nrow(arr) else as.integer(batch_size)
    .morie_gr_need(bs >= 1L, "geron_incremental_pca: batch_size must be >= 1")
    starts <- seq(1L, nrow(arr), by = bs)
    batches <- lapply(starts, function(s) arr[s:min(s + bs - 1L, nrow(arr)), , drop = FALSE])
  }
  .morie_gr_need(length(batches) > 0L, "geron_incremental_pca: no batches were supplied")
  n_feat <- ncol(batches[[1]])
  mean_v <- numeric(n_feat)
  S <- matrix(0, n_feat, n_feat)
  seen <- 0L
  for (B in batches) {
    .morie_gr_need(ncol(B) == n_feat, "geron_incremental_pca: batch feature-count mismatch")
    .morie_gr_need(all(is.finite(B)), "geron_incremental_pca: batch contains non-finite values")
    nb <- nrow(B)
    mb <- colMeans(B)
    Cb <- B - matrix(mb, nb, n_feat, byrow = TRUE)
    Sb <- t(Cb) %*% Cb
    if (seen == 0L) {
      mean_v <- mb
      S <- Sb
      seen <- nb
    } else {
      delta <- mb - mean_v
      total <- seen + nb
      S <- S + Sb + outer(delta, delta) * (seen * nb / total)
      mean_v <- mean_v + delta * (nb / total)
      seen <- total
    }
  }
  d <- as.integer(n_components)
  .morie_gr_need(d >= 1L && d <= n_feat, "geron_incremental_pca: n_components out of range")
  .morie_gr_need(seen >= 2L, "geron_incremental_pca: needs at least 2 samples")
  cov <- S / (seen - 1)
  cov <- 0.5 * (cov + t(cov))
  eg <- eigen(cov, symmetric = TRUE)
  vals <- pmax(eg$values, 0)
  vecs <- eg$vectors
  total_var <- sum(vals)
  .morie_gr_need(total_var != 0, "geron_incremental_pca: zero variance in every direction")
  components <- t(vecs[, seq_len(d), drop = FALSE])
  explained <- vals[seq_len(d)]
  ratio <- explained / total_var
  list(
    components = components, explained_variance = explained, explained_variance_ratio = ratio,
    mean = mean_v, covariance = cov, n_samples_seen = seen, n_batches = length(batches),
    estimate = sum(ratio), n = seen, method = "Incremental PCA (streaming co-moment accumulation)"
  )
}

# ---------------------------------------------------------------- 18/19. hmkmpp, hmkmn (kmeans needed by hmiseg/hmkmlim/hmmbkm)

#' k-means++ seeding, D^2 sampling (Geron Ch 8, morie.fn hmkmpp)
#'
#' RNG substitute via the shared LCG stream (not numpy PCG64).
#' @param X Data.
#' @param n_clusters Centres.
#' @param seed Seed.
#' @return List with centers, indices, d2, min_pair_distance, estimate, n, method.
#' @export
morie_geron_kmeans_plus_plus <- function(X, n_clusters, seed = 0) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  .morie_gr_need(nrow(A) > 0L, "geron_kmeans_plus_plus: X must be non-empty")
  .morie_gr_need(all(is.finite(A)), "geron_kmeans_plus_plus: X contains non-finite values")
  m <- nrow(A)
  k <- as.integer(n_clusters)
  .morie_gr_need(k >= 1L && k <= m, "geron_kmeans_plus_plus: n_clusters out of range")

  u0 <- .morie_gr_lcg_u(1L, seed)
  first <- 1L + floor(u0[1] * m)
  chosen <- c(first)
  d2 <- rowSums((A - matrix(A[first, ], m, ncol(A), byrow = TRUE))^2)
  draw_i <- 1L
  while (length(chosen) < k) {
    total <- sum(d2)
    if (total <= 0) {
      remaining <- setdiff(seq_len(m), chosen)
      .morie_gr_need(length(remaining) > 0L, "geron_kmeans_plus_plus: cannot pick distinct centres")
      nxt <- remaining[1]
    } else {
      uu <- .morie_gr_lcg_u(1L, seed + 97L * draw_i)
      draw_i <- draw_i + 1L
      target <- uu[1] * total
      csum <- cumsum(d2)
      nxt <- min(which(csum >= target), m)
    }
    chosen <- c(chosen, nxt)
    d2 <- pmin(d2, rowSums((A - matrix(A[nxt, ], m, ncol(A), byrow = TRUE))^2))
  }
  centers <- A[chosen, , drop = FALSE]
  if (k > 1L) {
    Dc <- .morie_gr_w4b_pairwise_distances(centers)
    min_pair <- min(Dc[upper.tri(Dc)])
  } else {
    min_pair <- Inf
  }
  list(
    centers = centers, indices = chosen - 1L, d2 = d2, min_pair_distance = min_pair,
    estimate = sum(d2), n = m, method = "k-means++ seeding (D^2 sampling)"
  )
}

#' k-means clustering via Lloyd's algorithm, k-means++ seeded (Geron Ch 8, morie.fn hmkmn)
#' @param X Data.
#' @param n_clusters Clusters.
#' @param seed Base seed.
#' @param max_iter Iteration cap.
#' @param tol Centre-shift tolerance.
#' @param n_init Restarts.
#' @return List with labels, centers, inertia, n_iter, distances, counts, estimate, n, method.
#' @export
morie_geron_kmeans <- function(X, n_clusters, seed = 0, max_iter = 300, tol = 1e-10, n_init = 10) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  .morie_gr_need(nrow(A) > 0L, "geron_kmeans: X must be non-empty")
  .morie_gr_need(all(is.finite(A)), "geron_kmeans: X contains non-finite values")
  m <- nrow(A)
  k <- as.integer(n_clusters)
  .morie_gr_need(k >= 1L && k <= m, "geron_kmeans: n_clusters out of range")
  max_iter <- as.integer(max_iter)
  n_init <- as.integer(n_init)

  best <- NULL
  for (run in seq_len(n_init) - 1L) {
    centers <- morie_geron_kmeans_plus_plus(A, k, seed = as.integer(seed) + run)$centers
    labels <- rep(0L, m)
    it <- 0L
    for (it in seq_len(max_iter)) {
      d2 <- matrix(0, m, k)
      for (j in seq_len(k)) d2[, j] <- rowSums((A - matrix(centers[j, ], m, ncol(A), byrow = TRUE))^2)
      labels <- apply(d2, 1, which.min)
      new <- centers
      for (j in seq_len(k)) {
        mask <- labels == j
        if (any(mask)) {
          new[j, ] <- colMeans(A[mask, , drop = FALSE])
        } else {
          worst <- which.max(apply(d2, 1, min))
          new[j, ] <- A[worst, ]
        }
      }
      shift <- sum(abs(new - centers))
      centers <- new
      if (shift <= tol) break
    }
    d2 <- matrix(0, m, k)
    for (j in seq_len(k)) d2[, j] <- rowSums((A - matrix(centers[j, ], m, ncol(A), byrow = TRUE))^2)
    labels <- apply(d2, 1, which.min)
    inertia <- sum(apply(d2, 1, min))
    if (is.null(best) || inertia < best$inertia) {
      best <- list(inertia = inertia, centers = centers, labels = labels, n_iter = it, dist = sqrt(d2))
    }
  }
  counts <- as.numeric(table(factor(best$labels, levels = seq_len(k))))
  list(
    labels = best$labels - 1L, centers = best$centers, inertia = best$inertia, n_iter = best$n_iter,
    distances = best$dist, counts = counts, estimate = best$inertia, n = m,
    method = "k-means (Lloyd's algorithm, k-means++ seeded)"
  )
}

# ---------------------------------------------------------------- 13. hmiseg (delegates to morie_geron_kmeans)

#' Colour segmentation by k-means on pixels (Geron Ch 8, morie.fn hmiseg)
#' @param image Array (h,w,c) or (h,w).
#' @param n_clusters Segments.
#' @param seed Seed for k-means.
#' @return List with segmented, labels, palette, inertia, compression_ratio, estimate, n, method.
#' @export
morie_geron_image_segmentation <- function(image, n_clusters, seed = 0) {
  img <- image
  d <- dim(img)
  .morie_gr_need(!is.null(d) && length(d) %in% c(2L, 3L), "geron_image_segmentation: bad image shape")
  if (length(d) == 2L) {
    img <- array(img, dim = c(d, 1L))
    d <- dim(img)
  }
  h <- d[1]
  w <- d[2]
  c_ <- d[3]
  flat <- matrix(as.numeric(img), nrow = h * w, ncol = c_)
  k <- as.integer(n_clusters)
  .morie_gr_need(k >= 1L && k <= h * w, "geron_image_segmentation: n_clusters out of range")
  km <- morie_geron_kmeans(flat, n_clusters = k, seed = as.integer(seed))
  labels1 <- km$labels + 1L
  palette <- km$centers
  seg <- palette[labels1, , drop = FALSE]
  seg_arr <- array(seg, dim = c(h, w, c_))
  distinct <- nrow(unique(flat))
  ratio <- distinct / k
  list(
    segmented = seg_arr, labels = matrix(km$labels, h, w), palette = palette, inertia = km$inertia,
    compression_ratio = ratio, estimate = km$inertia, n = h * w,
    method = "Colour segmentation by k-means on pixels"
  )
}

# ---------------------------------------------------------------- 15. hmkd

#' Knowledge distillation loss (Geron Ch 17, morie.fn hmkd)
#' @param teacher,student Logit matrices (m,C).
#' @param X Unused placeholder (callables not supported here).
#' @param y Optional integer hard labels (0-based).
#' @param T Temperature.
#' @param alpha Hard-label weight.
#' @return List with loss, ce_loss, kl_loss, teacher_probs, student_probs, agreement, estimate, n, method.
#' @export
morie_geron_knowledge_distillation <- function(teacher, student, X = NULL, y = NULL, T = 2.0, alpha = 0.5) {
  temp <- as.numeric(T)
  .morie_gr_need(is.finite(temp) && temp > 0, "geron_knowledge_distillation: T must be positive and finite")
  a <- as.numeric(alpha)
  .morie_gr_need(a >= 0 && a <= 1, "geron_knowledge_distillation: alpha must lie in [0, 1]")
  tl <- as.matrix(teacher)
  storage.mode(tl) <- "double"
  sl <- as.matrix(student)
  storage.mode(sl) <- "double"
  .morie_gr_need(all(dim(tl) == dim(sl)), "geron_knowledge_distillation: teacher/student shape mismatch")
  m <- nrow(tl)
  C <- ncol(tl)

  softmaxT <- function(Z, Tt) .morie_gr_w4b_softmax_rows(Z / Tt)
  pt <- softmaxT(tl, temp)
  ps <- softmaxT(sl, temp)
  log_ratio <- log(pmax(pt, 1e-300)) - log(pmax(ps, 1e-300))
  kl <- mean(rowSums(pt * log_ratio))
  kl_term <- temp * temp * kl

  if (is.null(y)) {
    .morie_gr_need(a == 0, "geron_knowledge_distillation: alpha weights the hard-label term but y is NULL")
    ce <- 0
  } else {
    yy <- as.integer(y)
    .morie_gr_need(length(yy) == m, "geron_knowledge_distillation: y length mismatch")
    p_hard <- .morie_gr_w4b_softmax_rows(sl)
    picked <- p_hard[cbind(seq_len(m), yy + 1L)]
    ce <- mean(-log(pmax(picked, 1e-300)))
  }
  loss <- a * ce + (1 - a) * kl_term
  agreement <- mean(apply(tl, 1, which.max) == apply(sl, 1, which.max))
  list(
    loss = loss, ce_loss = ce, kl_loss = kl_term, kl_raw = kl, teacher_probs = pt, student_probs = ps,
    agreement = agreement, T = temp, alpha = a, estimate = loss, n = m,
    method = "Knowledge distillation loss"
  )
}

# ---------------------------------------------------------------- 16. hmkfd (delegates to existing CV core)

#' K-fold partition plus CV score (Geron Ch 2, morie.fn hmkfd)
#' @param X,y Data.
#' @param k Folds.
#' @param seed Optional shuffle seed.
#' @param fit,predict,score Optional callables.
#' @return List with cv_score, fold_scores, train_indices, test_indices, fold_sizes, estimate, n, method.
#' @export
morie_geron_kfold <- function(X, y, k, seed = NULL, fit = NULL, predict = NULL, score = NULL) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  yy <- as.numeric(y)
  .morie_gr_need(nrow(A) == length(yy), "geron_kfold: X/y mismatch")
  m <- nrow(A)
  K <- as.integer(k)
  .morie_gr_need(K >= 2L && K <= m, "geron_kfold: k out of range")

  idx <- seq_len(m)
  if (!is.null(seed)) {
    u <- .morie_gr_lcg_u(m, as.integer(seed))
    idx <- order(u)
  }
  folds <- .morie_gr_array_split(idx, K)
  test_idx <- folds
  train_idx <- lapply(folds, function(f) setdiff(idx, f))

  inner <- morie_geron_cross_validation_score(A, yy,
    K = K, fit = fit, predict = predict, score = score,
    shuffle = FALSE, random_state = NULL
  )
  list(
    cv_score = inner$cv_score, fold_scores = inner$fold_scores, fold_sizes = inner$fold_sizes,
    se = inner$se, train_indices = train_idx, test_indices = test_idx, K = K,
    estimate = inner$cv_score, n = m, method = "K-fold partition and cross-validated score"
  )
}

# ---------------------------------------------------------------- 17. hmkmlim (delegates to morie_geron_kmeans)

#' k-means assumption diagnostics (Geron Ch 8, morie.fn hmkmlim)
#' @param X Data.
#' @param n_clusters Clusters.
#' @param seed Seed for k-means.
#' @return List with anisotropy, max_anisotropy, reassigned_fraction, size_ratio, labels,
#'   mahalanobis_labels, estimate, n, method.
#' @export
morie_geron_kmeans_limits <- function(X, n_clusters = 2, seed = 0) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  m <- nrow(A)
  d <- ncol(A)
  k <- as.integer(n_clusters)
  .morie_gr_need(k >= 2L && k <= m, "geron_kmeans_limits: n_clusters out of range")
  km <- morie_geron_kmeans(A, n_clusters = k, seed = as.integer(seed))
  labels1 <- km$labels + 1L
  centers <- km$centers
  counts <- as.numeric(table(factor(labels1, levels = seq_len(k))))
  .morie_gr_need(all(counts > 0), "geron_kmeans_limits: an empty cluster was returned")

  aniso <- numeric(k)
  covs <- vector("list", k)
  for (j in seq_len(k)) {
    pts <- A[labels1 == j, , drop = FALSE]
    if (nrow(pts) < 2L) {
      cov <- diag(d)
      aniso[j] <- 1
    } else {
      cov <- stats::cov(pts)
      ev <- pmax(eigen(cov, symmetric = TRUE, only.values = TRUE)$values, 0)
      lo <- min(ev)
      hi <- max(ev)
      aniso[j] <- if (lo == 0 && hi > 0) Inf else if (lo > 0) hi / lo else 1
    }
    covs[[j]] <- cov
  }
  ridge <- 1e-9 * sum(diag(stats::cov(A))) + 1e-12
  md <- matrix(0, m, k)
  for (j in seq_len(k)) {
    covj <- covs[[j]] + ridge * diag(d)
    inv <- solve(covj)
    diff <- A - matrix(centers[j, ], m, d, byrow = TRUE)
    md[, j] <- rowSums((diff %*% inv) * diff)
  }
  maha_labels <- apply(md, 1, which.min)
  reassigned <- mean(maha_labels != labels1)
  finite_aniso <- aniso[is.finite(aniso)]
  max_aniso <- if (length(aniso)) max(aniso) else 1
  list(
    anisotropy = aniso, max_anisotropy = max_aniso,
    mean_anisotropy = if (length(finite_aniso)) mean(finite_aniso) else Inf,
    reassigned_fraction = reassigned, size_ratio = max(counts) / min(counts),
    labels = labels1 - 1L, mahalanobis_labels = maha_labels - 1L, counts = counts,
    estimate = reassigned, n = m, method = "k-means assumption diagnostics"
  )
}

# ---------------------------------------------------------------- 20/21/22. hmkprbf (core), hmkppl, hmkpsg

#' Centre a Gram matrix in feature space (Geron Ch 7, morie.fn hmkprbf helper)
#' @param K Gram matrix. @return Centred Gram matrix.
#' @export
morie_geron_center_gram <- function(K) {
  K <- as.matrix(K)
  n <- nrow(K)
  ones <- matrix(1 / n, n, n)
  K - ones %*% K - K %*% ones + ones %*% K %*% ones
}

#' Eigen-decomposition step shared by every kernel-PCA variant (Geron Ch 7, morie.fn hmkprbf helper)
#' @param K Gram matrix.
#' @param n_components Components to keep.
#' @return List(projection, eigenvalues, alphas, K_centered).
#' @export
morie_geron_kernel_pca_from_gram <- function(K, n_components) {
  Kc <- morie_geron_center_gram(K)
  n <- nrow(Kc)
  d <- as.integer(n_components)
  .morie_gr_need(d >= 1L && d <= n, "kernel_pca_from_gram: n_components out of range")
  Kc <- 0.5 * (Kc + t(Kc))
  eg <- eigen(Kc, symmetric = TRUE)
  ord <- order(eg$values, decreasing = TRUE)[seq_len(d)]
  vals <- eg$values[ord]
  vecs <- eg$vectors[, ord, drop = FALSE]
  positive <- vals > 1e-12
  .morie_gr_need(any(positive), "kernel_pca_from_gram: no positive eigenvalue")
  alphas <- matrix(0, n, d)
  proj <- matrix(0, n, d)
  alphas[, positive] <- vecs[, positive, drop = FALSE] %*% diag(1 / sqrt(vals[positive]), sum(positive))
  proj[, positive] <- vecs[, positive, drop = FALSE] %*% diag(sqrt(vals[positive]), sum(positive))
  list(projection = proj, eigenvalues = vals, alphas = alphas, K_centered = Kc)
}

#' Kernel PCA with RBF kernel (Geron Ch 7, morie.fn hmkprbf)
#' @param X Data.
#' @param n_components Components.
#' @param gamma Kernel width (default 1/n_features).
#' @return List with X_projected, eigenvalues, alphas, K, explained_variance_ratio, gamma, estimate, n, method.
#' @export
morie_geron_kernel_pca_rbf_hmkprbf <- function(X, n_components, gamma = NULL) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  m <- nrow(A)
  n_feat <- ncol(A)
  g <- if (is.null(gamma)) 1 / n_feat else as.numeric(gamma)
  .morie_gr_need(is.finite(g) && g > 0, "geron_kernel_pca_rbf: gamma must be positive and finite")
  D2 <- .morie_gr_w4b_pairwise_distances(A)^2
  K <- exp(-g * D2)
  core <- morie_geron_kernel_pca_from_gram(K, n_components)
  Kc <- core$K_centered
  total <- sum(pmax(eigen(0.5 * (Kc + t(Kc)), symmetric = TRUE, only.values = TRUE)$values, 0))
  ratio <- if (total > 0) pmax(core$eigenvalues, 0) / total else rep(0, length(core$eigenvalues))
  list(
    X_projected = core$projection, eigenvalues = core$eigenvalues, alphas = core$alphas, K = K,
    K_centered = Kc, explained_variance_ratio = ratio, gamma = g, estimate = core$eigenvalues[1],
    n = m, method = "Kernel PCA (RBF kernel)"
  )
}

#' Kernel PCA with polynomial kernel (Geron Ch 7, morie.fn hmkppl)
#' @param X Data.
#' @param n_components Components.
#' @param degree Degree.
#' @param gamma Scale.
#' @param coef0 Offset.
#' @return List with X_projected, eigenvalues, alphas, K, feature_space_dim, estimate, n, method.
#' @export
morie_geron_kernel_pca_poly <- function(X, n_components, degree = 3, gamma = NULL, coef0 = 1.0) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  m <- nrow(A)
  n_feat <- ncol(A)
  deg <- as.integer(degree)
  .morie_gr_need(deg >= 1L, "geron_kernel_pca_poly: degree must be >= 1")
  g <- if (is.null(gamma)) 1 / n_feat else as.numeric(gamma)
  .morie_gr_need(is.finite(g) && g > 0, "geron_kernel_pca_poly: gamma must be positive and finite")
  c0 <- as.numeric(coef0)
  K <- (g * (A %*% t(A)) + c0)^deg
  .morie_gr_need(all(is.finite(K)), "geron_kernel_pca_poly: the kernel overflowed")
  core <- morie_geron_kernel_pca_from_gram(K, n_components)
  comb <- function(a, b) {
    out <- 1
    for (i in seq_len(b) - 1L) out <- (out * (a - i)) %/% (i + 1)
    as.integer(out)
  }
  fdim <- if (c0 != 0) comb(n_feat + deg, deg) else comb(n_feat + deg - 1L, deg)
  list(
    X_projected = core$projection, eigenvalues = core$eigenvalues, alphas = core$alphas, K = K,
    K_centered = core$K_centered, feature_space_dim = fdim, degree = deg, gamma = g, coef0 = c0,
    estimate = core$eigenvalues[1], n = m, method = "Kernel PCA (polynomial kernel)"
  )
}

#' Kernel PCA with sigmoid kernel (Geron Ch 7, morie.fn hmkpsg)
#' @param X Data.
#' @param n_components Components.
#' @param gamma Scale.
#' @param coef0 Offset.
#' @return List with X_projected, eigenvalues, alphas, K, n_negative_eigenvalues, is_psd, estimate, n, method.
#' @export
morie_geron_kernel_pca_sigmoid <- function(X, n_components, gamma = NULL, coef0 = 1.0) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  m <- nrow(A)
  n_feat <- ncol(A)
  g <- if (is.null(gamma)) 1 / n_feat else as.numeric(gamma)
  .morie_gr_need(is.finite(g) && g > 0, "geron_kernel_pca_sigmoid: gamma must be positive and finite")
  c0 <- as.numeric(coef0)
  K <- tanh(g * (A %*% t(A)) + c0)
  core <- morie_geron_kernel_pca_from_gram(K, n_components)
  Kc <- core$K_centered
  spectrum <- eigen(0.5 * (Kc + t(Kc)), symmetric = TRUE, only.values = TRUE)$values
  tol <- 1e-8 * max(1, max(abs(spectrum)))
  n_neg <- sum(spectrum < -tol)
  list(
    X_projected = core$projection, eigenvalues = core$eigenvalues, alphas = core$alphas, K = K,
    K_centered = Kc, spectrum = spectrum, n_negative_eigenvalues = n_neg, is_psd = n_neg == 0,
    gamma = g, coef0 = c0, estimate = core$eigenvalues[1], n = m, method = "Kernel PCA (sigmoid kernel)"
  )
}

# ---------------------------------------------------------------- 23. hmkrn (delegates to morie_geron_he_init_hmhei)

#' Convolutional filter tensor, He-initialised (Geron Ch 12, morie.fn hmkrn)
#' @param kh,kw Kernel height/width.
#' @param c_in,c_out Channels.
#' @param seed Seed.
#' @param init "he" or "zeros".
#' @return List with kernel, bias, shape, n_parameters, fan_in, std, estimate, n, method.
#' @export
morie_geron_filter_kernel <- function(kh, kw, c_in, c_out, seed = 0, init = "he") {
  h <- as.integer(kh)
  w <- as.integer(kw)
  ci <- as.integer(c_in)
  co <- as.integer(c_out)
  .morie_gr_need(h >= 1L && w >= 1L && ci >= 1L && co >= 1L, "geron_filter_kernel: sizes must be positive")
  .morie_gr_need(init %in% c("he", "zeros"), "geron_filter_kernel: init must be 'he' or 'zeros'")
  fan_in <- h * w * ci
  if (init == "zeros") {
    Kt <- array(0, dim = c(h, w, ci, co))
    std <- 0
  } else {
    flat <- morie_geron_he_init_hmhei(fan_in, seed = as.integer(seed), fan_out = co)
    std <- flat$std_target
    # row-major (numpy C-order) reshape of the (fan_in, co) weight stream into (h, w, ci, co):
    # fill the reversed-dim array column-major (co fastest), then permute axes back.
    v <- as.numeric(t(flat$W))
    Kt <- aperm(array(v, dim = c(co, ci, w, h)), perm = c(4, 3, 2, 1))
  }
  bias <- numeric(co)
  n_params <- length(Kt) + length(bias)
  list(
    kernel = Kt, bias = bias, shape = c(h, w, ci, co), n_parameters = n_params, fan_in = fan_in,
    std = std, estimate = n_params, n = length(Kt), method = "Convolutional filter tensor"
  )
}

# ---------------------------------------------------------------- 24. hmkvc (delegates to morie_geron_int8_quant)

#' KV-cache quantization (Geron Ch 17, morie.fn hmkvc)
#' @param K,V Arrays (n_heads, seq_len, d_head).
#' @param n_bits Bit width.
#' @param per_head Per-head scales.
#' @param dtype_bytes Bytes per element before compression.
#' @return List with K_dequantized, V_dequantized, bytes_before, bytes_after, compression_ratio, max_error, estimate, n, method.
#' @export
morie_geron_kv_cache_compress <- function(K, V, n_bits = 8, per_head = TRUE, dtype_bytes = 2) {
  Ka <- K
  Va <- V
  if (length(dim(Ka)) == 2L) Ka <- array(Ka, dim = c(1, dim(Ka)))
  if (length(dim(Va)) == 2L) Va <- array(Va, dim = c(1, dim(Va)))
  .morie_gr_need(all(dim(Ka) == dim(Va)), "geron_kv_cache_compress: K/V shape mismatch")
  db <- as.integer(dtype_bytes)
  b <- as.integer(n_bits)
  n_heads <- dim(Ka)[1]
  outs <- list()
  scales <- list(K = c(), V = c())
  max_err <- 0
  for (nm in c("K", "V")) {
    Tt <- if (nm == "K") Ka else Va
    deq <- array(0, dim = dim(Tt))
    if (isTRUE(per_head)) {
      for (h in seq_len(n_heads)) {
        slab <- Tt[h, , ]
        if (all(slab == 0)) {
          deq[h, , ] <- slab
          scales[[nm]] <- c(scales[[nm]], 0)
          next
        }
        q <- morie_geron_int8_quant(as.vector(slab), n_bits = b, symmetric = TRUE)
        deq[h, , ] <- array(q$dequantized, dim = dim(slab))
        scales[[nm]] <- c(scales[[nm]], q$scale)
      }
    } else {
      q <- morie_geron_int8_quant(as.vector(Tt), n_bits = b, symmetric = TRUE)
      deq <- array(q$dequantized, dim = dim(Tt))
      scales[[nm]] <- c(scales[[nm]], q$scale)
    }
    outs[[nm]] <- deq
    max_err <- max(max_err, max(abs(deq - Tt)))
  }
  n_elem <- length(Ka) + length(Va)
  bytes_before <- n_elem * db
  n_scales <- length(scales$K) + length(scales$V)
  bytes_after <- ceiling(n_elem * b / 8) + n_scales * 4
  ratio <- bytes_before / bytes_after
  list(
    K_dequantized = outs$K, V_dequantized = outs$V, scales = scales, bytes_before = bytes_before,
    bytes_after = bytes_after, compression_ratio = ratio, max_error = max_err, n_bits = b,
    estimate = ratio, n = n_elem, method = "KV-cache quantization"
  )
}

# ---------------------------------------------------------------- 25. hml1c (delegates to EXISTING morie_geron_1cycle_schedule)

#' 1cycle learning-rate policy (Geron Ch 11, morie.fn hml1c)
#' @param t Step (0-based).
#' @param T Cycle length.
#' @param lr_max,lr_min Rate bounds.
#' @param mom_max,mom_min Momentum bounds.
#' @return List with lr, momentum, lr_schedule, momentum_schedule, peak_step, phase, estimate, n, method.
#' @export
morie_geron_one_cycle <- function(t, T, lr_max, lr_min, mom_max = 0.95, mom_min = 0.85) {
  T_int <- as.integer(T)
  t_int <- as.integer(t)
  .morie_gr_need(T_int >= 2L, "geron_one_cycle: T must be at least 2")
  .morie_gr_need(t_int >= 0L && t_int < T_int, "geron_one_cycle: t out of range")
  hi <- as.numeric(lr_max)
  lo <- as.numeric(lr_min)
  .morie_gr_need(is.finite(hi) && is.finite(lo) && lo > 0 && hi > lo, "geron_one_cycle: bad lr_max/lr_min")
  inner <- morie_geron_1cycle_schedule(lo, hi, t = t_int, T = T_int, mom_max = as.numeric(mom_max), mom_min = as.numeric(mom_min))
  peak <- as.integer(inner$peak_step)
  phase <- if (t_int == peak) "peak" else if (t_int < peak) "warmup" else "anneal"
  list(
    lr = inner$lr_schedule[t_int + 1L], momentum = inner$momentum_schedule[t_int + 1L],
    lr_schedule = inner$lr_schedule, momentum_schedule = inner$momentum_schedule, peak_step = peak,
    phase = phase, estimate = inner$lr_schedule[t_int + 1L], n = T_int, method = "1cycle learning-rate policy"
  )
}

# ---------------------------------------------------------------- 26. hml2r

#' L2 (ridge) regularization penalty (Geron Ch 11, morie.fn hml2r)
#' @param theta Parameters.
#' @param alpha Strength.
#' @param skip_bias Exclude theta\[1\].
#' @param eta Optional learning rate.
#' @return List with penalty, gradient, l2_norm, shrink_factor, estimate, n, method.
#' @export
morie_geron_l2_regularization <- function(theta, alpha, skip_bias = FALSE, eta = NULL) {
  t <- as.numeric(theta)
  .morie_gr_need(length(t) > 0L, "geron_l2_regularization: theta is empty")
  .morie_gr_need(all(is.finite(t)), "geron_l2_regularization: theta contains non-finite values")
  a <- as.numeric(alpha)
  .morie_gr_need(is.finite(a) && a >= 0, "geron_l2_regularization: alpha must be finite and non-negative")
  mask <- rep(1, length(t))
  if (isTRUE(skip_bias)) {
    .morie_gr_need(length(t) >= 2L, "geron_l2_regularization: skip_bias needs a non-bias parameter")
    mask[1] <- 0
  }
  penalty <- 0.5 * a * sum((t * mask)^2)
  grad <- a * t * mask
  l2 <- sqrt(sum((t * mask)^2))
  shrink <- NULL
  if (!is.null(eta)) {
    lr <- as.numeric(eta)
    .morie_gr_need(is.finite(lr) && lr > 0, "geron_l2_regularization: eta must be positive and finite")
    shrink <- 1 - lr * a
  }
  list(
    penalty = penalty, gradient = grad, l2_norm = l2, shrink_factor = shrink, estimate = penalty,
    n = length(t), method = "L2 (ridge) regularization penalty"
  )
}

# ---------------------------------------------------------------- 27. hmlaso (delegates to EXISTING morie_geron_l1_regularization)

#' Lasso cost = MSE + alpha * L1 penalty (Geron Ch 4, morie.fn hmlaso)
#' @param X,y Data.
#' @param theta Coefficients.
#' @param alpha L1 strength.
#' @param skip_bias Exclude theta\[1\].
#' @return List with cost, mse, penalty, gradient, n_zero, estimate, n, method.
#' @export
morie_geron_lasso_cost_hmlaso <- function(X, y, theta, alpha, skip_bias = FALSE) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  yy <- as.numeric(y)
  t <- as.numeric(theta)
  .morie_gr_need(length(yy) == nrow(A), "geron_lasso_cost: X/y mismatch")
  .morie_gr_need(length(t) == ncol(A), "geron_lasso_cost: theta/X mismatch")
  pen <- morie_geron_l1_regularization(t, alpha, skip_bias = skip_bias)
  m <- length(yy)
  resid <- as.numeric(A %*% t) - yy
  mse <- mean(resid^2)
  cost <- mse + pen$penalty
  grad <- (2 / m) * as.numeric(t(A) %*% resid) + pen$gradient
  list(
    cost = cost, mse = mse, penalty = pen$penalty, gradient = grad, residuals = resid,
    n_zero = sum(t == 0), alpha = as.numeric(alpha), estimate = cost, n = m,
    method = "Lasso cost = MSE + alpha * L1 penalty"
  )
}

# ---------------------------------------------------------------- 28. hmlcv

#' Learning curves: train/validation RMSE vs training-set size (Geron Ch 4, morie.fn hmlcv)
#'
#' Split order via the shared LCG stream, not numpy PCG64 permutation.
#' @param X,y Data.
#' @param n_splits Sizes to evaluate.
#' @param val_fraction Held-out fraction.
#' @param fit,predict Optional callables (default OLS via lstsq).
#' @param seed Split seed.
#' @return List with train_sizes, rmse_train, rmse_val, final_gap, verdict, estimate, n, method.
#' @export
morie_geron_learning_curves_hmlcv <- function(X, y, n_splits = 10, val_fraction = 0.2, fit = NULL, predict = NULL, seed = 0) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  yy <- as.numeric(y)
  .morie_gr_need(nrow(A) == length(yy), "geron_learning_curves: X/y mismatch")
  vf <- as.numeric(val_fraction)
  .morie_gr_need(vf > 0 && vf < 1, "geron_learning_curves: bad val_fraction")
  splits <- as.integer(n_splits)
  .morie_gr_need(splits >= 2L, "geron_learning_curves: n_splits must be >= 2")
  fit_fn <- if (is.null(fit)) function(Xtr, ytr) .morie_gr_lstsq(Xtr, ytr) else fit
  pred_fn <- if (is.null(predict)) function(theta, Xte) as.numeric(as.matrix(Xte) %*% theta) else predict

  m <- nrow(A)
  n_val <- as.integer(round(m * vf))
  .morie_gr_need(n_val >= 1L && (m - n_val) >= 2L, "geron_learning_curves: unusable split")
  u <- .morie_gr_lcg_u(m, as.integer(seed))
  perm <- order(u)
  val_i <- perm[seq_len(n_val)]
  tr_i <- perm[(n_val + 1L):m]
  Xv <- A[val_i, , drop = FALSE]
  yv <- yy[val_i]
  Xt <- A[tr_i, , drop = FALSE]
  yt <- yy[tr_i]

  n_train <- nrow(Xt)
  sizes <- sort(unique(as.integer(round(seq(2, n_train, length.out = splits)))))
  rmse_tr <- numeric(length(sizes))
  rmse_va <- numeric(length(sizes))
  for (i in seq_along(sizes)) {
    s <- sizes[i]
    model <- fit_fn(Xt[seq_len(s), , drop = FALSE], yt[seq_len(s)])
    p_tr <- as.numeric(pred_fn(model, Xt[seq_len(s), , drop = FALSE]))
    p_va <- as.numeric(pred_fn(model, Xv))
    rmse_tr[i] <- sqrt(mean((p_tr - yt[seq_len(s)])^2))
    rmse_va[i] <- sqrt(mean((p_va - yv)^2))
  }
  gap <- rmse_va[length(sizes)] - rmse_tr[length(sizes)]
  scale <- .morie_gr_w4b_popsd(yy)
  verdict <- if (rmse_tr[length(sizes)] > 0.1 * max(scale, 1e-12) && gap < 0.5 * max(rmse_tr[length(sizes)], 1e-12)) {
    "underfitting"
  } else if (gap > max(0.5 * max(rmse_tr[length(sizes)], 1e-12), 0.1 * max(scale, 1e-12))) "overfitting" else "fits well"

  list(
    train_sizes = sizes, rmse_train = rmse_tr, rmse_val = rmse_va, final_gap = gap, verdict = verdict,
    estimate = rmse_va[length(sizes)], n = m, method = "Learning curves (train/validation RMSE vs training-set size)"
  )
}

# ---------------------------------------------------------------- 44 (early). hmmds core (needed by hmlle, hmlof, hmiso, hmmdc)

#' Euclidean pairwise distance matrix (Geron Ch 7, morie.fn hmmds helper)
#' @param X Data matrix. @return Distance matrix.
#' @export
morie_geron_pairwise_distances <- function(X) .morie_gr_w4b_pairwise_distances(X)

#' Double-centre a squared-distance matrix (Geron Ch 7, morie.fn hmmds helper)
#' @param D Distance matrix. @return Gram matrix B.
#' @export
morie_geron_double_center <- function(D) .morie_gr_w4b_double_center(D)

#' Classical (Torgerson) multidimensional scaling (Geron Ch 7, morie.fn hmmds)
#' @param X Data, or a distance matrix if precomputed=TRUE.
#' @param n_components Embedding dimension.
#' @param precomputed Treat X as a symmetric distance matrix.
#' @return List with embedding, eigenvalues, stress, distance_matrix, n_negative_eigenvalues, estimate, n, method.
#' @export
morie_geron_mds <- function(X, n_components, precomputed = FALSE) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  .morie_gr_need(all(is.finite(A)), "geron_mds: X contains non-finite values")
  if (isTRUE(precomputed)) {
    .morie_gr_need(nrow(A) == ncol(A), "geron_mds: precomputed distance matrix must be square")
    .morie_gr_need(isTRUE(all.equal(A, t(A))), "geron_mds: precomputed distance matrix is not symmetric")
    .morie_gr_need(all(diag(A) == 0), "geron_mds: precomputed distance matrix must have zero diagonal")
    .morie_gr_need(all(A >= 0), "geron_mds: distances must be non-negative")
    D <- A
  } else {
    D <- .morie_gr_w4b_pairwise_distances(A)
  }

  m <- nrow(D)
  d <- as.integer(n_components)
  .morie_gr_need(d >= 1L && d <= m, "geron_mds: n_components out of range")
  B <- .morie_gr_w4b_double_center(D)
  B <- 0.5 * (B + t(B))
  eg <- eigen(B, symmetric = TRUE)
  ord <- order(eg$values, decreasing = TRUE)
  vals <- eg$values[ord]
  vecs <- eg$vectors[, ord, drop = FALSE]
  tol <- 1e-8 * max(1, max(abs(vals)))
  n_neg <- sum(vals < -tol)
  keep <- vals[seq_len(d)]
  pos <- keep > tol
  emb <- matrix(0, m, d)
  if (any(pos)) emb[, pos] <- vecs[, seq_len(d), drop = FALSE][, pos, drop = FALSE] %*% diag(sqrt(keep[pos]), sum(pos))
  D_emb <- .morie_gr_w4b_pairwise_distances(emb)
  iu <- upper.tri(D)
  stress <- sum((D[iu] - D_emb[iu])^2)
  list(
    embedding = emb, eigenvalues = vals, stress = stress, distance_matrix = D, embedded_distances = D_emb,
    n_negative_eigenvalues = n_neg, estimate = stress, n = m,
    method = "Classical (Torgerson) multidimensional scaling"
  )
}

# ---------------------------------------------------------------- 29. hmlle (delegates to morie_geron_pairwise_distances)

#' Locally linear embedding (Geron Ch 7, morie.fn hmlle)
#' @param X Data.
#' @param n_components Embedding dimension.
#' @param n_neighbors Neighbours.
#' @param reg Ridge fraction.
#' @return List with embedding, weights, reconstruction_error, eigenvalues, estimate, n, method.
#' @export
morie_geron_locally_linear_embedding <- function(X, n_components, n_neighbors = 5, reg = 1e-3) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  m <- nrow(A)
  k <- as.integer(n_neighbors)
  .morie_gr_need(k >= 1L && k < m, "geron_locally_linear_embedding: n_neighbors out of range")
  d <- as.integer(n_components)
  .morie_gr_need(d >= 1L && d <= m - 2L, "geron_locally_linear_embedding: n_components out of range")
  r <- as.numeric(reg)
  .morie_gr_need(is.finite(r) && r >= 0, "geron_locally_linear_embedding: reg must be non-negative")

  D <- .morie_gr_w4b_pairwise_distances(A)
  W <- matrix(0, m, m)
  err <- numeric(m)
  for (i in seq_len(m)) {
    nb <- setdiff(order(D[i, ]), i)[seq_len(k)]
    Z <- A[nb, , drop = FALSE] - matrix(A[i, ], k, ncol(A), byrow = TRUE)
    G <- Z %*% t(Z)
    tr <- sum(diag(G))
    ridge <- r * (if (tr > 0) tr else 1)
    G <- G + ridge * diag(k) / k
    w <- tryCatch(solve(G, rep(1, k)), error = function(e) NULL)
    .morie_gr_need(!is.null(w), "geron_locally_linear_embedding: local Gram matrix is singular")
    s <- sum(w)
    .morie_gr_need(s != 0, "geron_locally_linear_embedding: weights sum to zero")
    w <- w / s
    W[i, nb] <- w
    err[i] <- sum((A[i, ] - as.numeric(w %*% A[nb, , drop = FALSE]))^2)
  }
  I <- diag(m)
  M <- t(I - W) %*% (I - W)
  M <- 0.5 * (M + t(M))
  eg <- eigen(M, symmetric = TRUE)
  ord <- order(eg$values)
  vals <- eg$values[ord]
  vecs <- eg$vectors[, ord, drop = FALSE]
  emb <- vecs[, 2:(d + 1), drop = FALSE]
  list(
    embedding = emb, weights = W, reconstruction_error = err, eigenvalues = vals[1:(d + 1)], M = M,
    n_neighbors = k, estimate = sum(err), n = m, method = "Locally linear embedding"
  )
}

# ---------------------------------------------------------------- 30. hmlnet (delegates to EXISTING morie_geron_conv_output_size)

#' LeNet-5 architecture resolution (Geron Ch 12, morie.fn hmlnet)
#' @param n_classes Outputs.
#' @param input_size Square input size.
#' @param in_channels Input channels.
#' @return List with layers, total_parameters, output_shape, receptive_field, estimate, n, method.
#' @export
morie_geron_lenet5 <- function(n_classes = 10, input_size = 32, in_channels = 1) {
  k <- as.integer(n_classes)
  .morie_gr_need(k >= 2L, "geron_lenet5: n_classes must be >= 2")
  s <- as.integer(input_size)
  .morie_gr_need(s >= 8L, "geron_lenet5: input_size must be >= 8")
  c_in <- as.integer(in_channels)
  .morie_gr_need(c_in >= 1L, "geron_lenet5: in_channels must be >= 1")

  layers <- list()
  total <- 0L
  conv <- function(name, size, cin, cout, kernel) {
    out <- as.integer(morie_geron_conv_output_size(size, kernel, padding = 0, stride = 1)$out_size[1])
    .morie_gr_need(out >= 1L, "geron_lenet5: kernel does not fit the map")
    p <- kernel * kernel * cin * cout + cout
    layers[[length(layers) + 1]] <<- list(name = name, type = "conv", output_shape = c(out, out, cout), parameters = p)
    total <<- total + p
    c(out, cout)
  }
  pool <- function(name, size, chan) {
    out <- as.integer(morie_geron_conv_output_size(size, 2, padding = 0, stride = 2)$out_size[1])
    layers[[length(layers) + 1]] <<- list(name = name, type = "pool", output_shape = c(out, out, chan), parameters = 0L)
    c(out, chan)
  }
  r1 <- conv("C1", s, c_in, 6L, 5L)
  sz <- r1[1]
  ch <- r1[2]
  r2 <- pool("S2", sz, ch)
  sz <- r2[1]
  ch <- r2[2]
  r3 <- conv("C3", sz, ch, 16L, 5L)
  sz <- r3[1]
  ch <- r3[2]
  r4 <- pool("S4", sz, ch)
  sz <- r4[1]
  ch <- r4[2]
  .morie_gr_need(sz >= 5L, "geron_lenet5: input too small before C5")
  r5 <- conv("C5", sz, ch, 120L, 5L)
  sz <- r5[1]
  ch <- r5[2]
  .morie_gr_need(sz == 1L, "geron_lenet5: C5 did not land on 1x1")

  p_f6 <- 120L * 84L + 84L
  total <- total + p_f6
  layers[[length(layers) + 1]] <- list(name = "F6", type = "dense", output_shape = 84L, parameters = p_f6)
  p_out <- 84L * k + k
  total <- total + p_out
  layers[[length(layers) + 1]] <- list(name = "output", type = "dense", output_shape = k, parameters = p_out)

  chain <- list(c(5, 1), c(2, 2), c(5, 1), c(2, 2), c(5, 1))
  rf <- 1
  for (kv in rev(chain)) rf <- (rf - 1) * kv[2] + kv[1]
  rf <- if (rf > 32) 32 else rf

  list(
    layers = layers, total_parameters = total, output_shape = k, receptive_field = as.integer(rf),
    convention = "parameter-free pooling, fully-connected C3", estimate = total, n = length(layers),
    method = "LeNet-5 architecture resolution"
  )
}

# ---------------------------------------------------------------- 32. hmlntr

#' Layer normalization: per-sample, across features (Geron Ch 11, morie.fn hmlntr)
#' @param x Matrix (m,d) or vector.
#' @param gamma,beta Per-feature scale/shift.
#' @param eps Variance floor.
#' @return List with y, x_hat, mu, var, estimate, n, method.
#' @export
morie_geron_layer_normalization_hmlntr <- function(x, gamma = 1.0, beta = 0.0, eps = 1e-5) {
  X <- if (is.null(dim(x))) matrix(as.numeric(x), nrow = 1) else as.matrix(x)
  storage.mode(X) <- "double"
  .morie_gr_need(all(is.finite(X)), "geron_layer_normalization: x contains non-finite values")
  m <- nrow(X)
  d <- ncol(X)
  .morie_gr_need(d >= 2L, "geron_layer_normalization: needs at least 2 features")
  e <- as.numeric(eps)
  .morie_gr_need(e >= 0, "geron_layer_normalization: eps must be non-negative")
  g <- rep_len(as.numeric(gamma), d)
  b <- rep_len(as.numeric(beta), d)
  mu <- rowMeans(X)
  var <- rowMeans((X - mu)^2)
  .morie_gr_need(!(e == 0 && any(var == 0)), "geron_layer_normalization: constant row with eps=0")
  x_hat <- (X - mu) / sqrt(var + e)
  y <- sweep(sweep(x_hat, 2, g, "*"), 2, b, "+")
  list(
    y = y, x_hat = x_hat, mu = mu, var = var, gamma = g, beta = b, estimate = mean(y), n = m,
    method = "Layer normalization (per-sample, across features)"
  )
}

# ---------------------------------------------------------------- 31. hmlnr (delegates to morie_geron_layer_normalization_hmlntr)

#' Layer normalization inside an RNN cell (Geron Ch 13, morie.fn hmlnr)
#' @param x Pre-activations (T, n_units).
#' @param gamma,beta Scale/shift.
#' @param eps Variance floor.
#' @param activation tanh/relu/none.
#' @return List with h, normalized, mu, var, estimate, n, method.
#' @export
morie_geron_layer_norm_rnn <- function(x, gamma = 1.0, beta = 0.0, eps = 1e-5, activation = "tanh") {
  .morie_gr_need(activation %in% c("tanh", "relu", "none"), "geron_layer_norm_rnn: bad activation")
  inner <- morie_geron_layer_normalization_hmlntr(x, gamma = gamma, beta = beta, eps = eps)
  z <- inner$y
  h <- if (activation == "tanh") tanh(z) else if (activation == "relu") pmax(z, 0) else z
  list(
    h = h, normalized = inner$x_hat, pre_activation = z, mu = inner$mu, var = inner$var,
    estimate = mean(h), n = nrow(z), method = "Layer normalization inside an RNN cell"
  )
}

# ---------------------------------------------------------------- 33. hmlof (delegates to morie_geron_pairwise_distances)

#' Local outlier factor (Geron Ch 8, morie.fn hmlof)
#' @param X Data.
#' @param n_neighbors Neighbourhood size k.
#' @param contamination Optional outlier fraction.
#' @return List with lof, lrd, k_distance, neighbors, is_outlier, estimate, n, method.
#' @export
morie_geron_local_outlier_factor_hmlof <- function(X, n_neighbors = 20, contamination = NULL) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  m <- nrow(A)
  k <- as.integer(n_neighbors)
  .morie_gr_need(k >= 1L && k < m, "geron_local_outlier_factor: n_neighbors out of range")
  D <- .morie_gr_w4b_pairwise_distances(A)
  diag(D) <- Inf
  nbrs <- matrix(0L, m, k)
  kdist <- numeric(m)
  for (i in seq_len(m)) {
    ord <- order(D[i, ])[seq_len(k)]
    nbrs[i, ] <- ord
    kdist[i] <- D[i, ord[k]]
  }
  lrd <- numeric(m)
  for (p in seq_len(m)) {
    reach <- pmax(kdist[nbrs[p, ]], D[p, nbrs[p, ]])
    mean_reach <- mean(reach)
    .morie_gr_need(mean_reach != 0, "geron_local_outlier_factor: point coincides with all neighbours")
    lrd[p] <- 1 / mean_reach
  }
  lof <- vapply(seq_len(m), function(p) mean(lrd[nbrs[p, ]]) / lrd[p], numeric(1))
  is_outlier <- rep(FALSE, m)
  if (!is.null(contamination)) {
    c_ <- as.numeric(contamination)
    .morie_gr_need(c_ > 0 && c_ <= 0.5, "geron_local_outlier_factor: bad contamination")
    n_out <- max(1L, as.integer(round(c_ * m)))
    is_outlier[order(lof, decreasing = TRUE)[seq_len(n_out)]] <- TRUE
  }
  list(
    lof = lof, lrd = lrd, k_distance = kdist, neighbors = nbrs, is_outlier = is_outlier, distances = D,
    estimate = max(lof), n = m, method = "Local outlier factor"
  )
}

# ---------------------------------------------------------------- 34. hmlrh

#' Learning-rate finder heuristic: divergence point / safety (Geron Ch 9, morie.fn hmlrh)
#' @param lr_curve Matrix/list of (lr, loss) pairs.
#' @param divergence_factor Multiple of running min.
#' @param safety Divisor.
#' @return List with lr, lr_diverge, lr_min_loss, min_loss, diverged, estimate, n, method.
#' @export
morie_geron_learning_rate_heuristic <- function(lr_curve, divergence_factor = 4.0, safety = 10.0) {
  arr <- as.matrix(lr_curve)
  storage.mode(arr) <- "double"
  if (ncol(arr) == 2L) {
    lrs <- arr[, 1]
    losses <- arr[, 2]
  } else {
    lrs <- arr[1, ]
    losses <- arr[2, ]
  }
  .morie_gr_need(length(lrs) >= 2L, "geron_learning_rate_heuristic: sweep needs >= 2 points")
  .morie_gr_need(all(is.finite(lrs)) && all(is.finite(losses)), "geron_learning_rate_heuristic: non-finite values")
  .morie_gr_need(all(lrs > 0), "geron_learning_rate_heuristic: rates must be positive")
  .morie_gr_need(all(diff(lrs) > 0), "geron_learning_rate_heuristic: rates must be strictly increasing")
  df <- as.numeric(divergence_factor)
  .morie_gr_need(df > 1, "geron_learning_rate_heuristic: divergence_factor must exceed 1")
  sf <- as.numeric(safety)
  .morie_gr_need(sf >= 1, "geron_learning_rate_heuristic: safety must be >= 1")

  running_min <- cummin(losses)
  i_min <- which.min(losses)
  diverge_i <- NA_integer_
  for (i in 2:length(losses)) {
    if (losses[i] > df * running_min[i - 1]) {
      diverge_i <- i
      break
    }
  }
  if (is.na(diverge_i)) {
    lr_div <- NaN
    lr <- lrs[i_min]
    diverged <- FALSE
  } else {
    lr_div <- lrs[diverge_i]
    lr <- lr_div / sf
    diverged <- TRUE
  }
  list(
    lr = lr, lr_diverge = lr_div, lr_min_loss = lrs[i_min], min_loss = losses[i_min], diverged = diverged,
    lrs = lrs, losses = losses, estimate = lr, n = length(lrs),
    method = "LR-finder heuristic (divergence point / 10)"
  )
}

# ---------------------------------------------------------------- 35. hmlrl

#' Life satisfaction = theta0 + theta1 * GDP per capita (Geron Ch 1, morie.fn hmlrl)
#' @param gdp GDP per capita.
#' @param theta0,theta1 Intercept/slope.
#' @param life_sat Optional observed values.
#' @return List with prediction, residuals, rmse, r2, theta0, theta1, estimate, n, method.
#' @export
morie_geron_linear_regression_life <- function(gdp, theta0, theta1, life_sat = NULL) {
  x <- as.numeric(gdp)
  .morie_gr_need(length(x) > 0L, "geron_linear_regression_life: gdp is empty")
  .morie_gr_need(all(is.finite(x)), "geron_linear_regression_life: gdp non-finite")
  t0 <- as.numeric(theta0)
  t1 <- as.numeric(theta1)
  pred <- t0 + t1 * x
  resid <- rmse <- r2 <- NULL
  if (!is.null(life_sat)) {
    y <- as.numeric(life_sat)
    .morie_gr_need(length(y) == length(x), "geron_linear_regression_life: life_sat length mismatch")
    resid <- y - pred
    rmse <- sqrt(mean(resid^2))
    ss_tot <- sum((y - mean(y))^2)
    .morie_gr_need(ss_tot != 0, "geron_linear_regression_life: life_sat has zero variance")
    r2 <- 1 - sum(resid^2) / ss_tot
  }
  list(
    prediction = pred, residuals = resid, rmse = rmse, r2 = r2, theta0 = t0, theta1 = t1,
    estimate = pred[1], n = length(x), method = "Univariate linear model (life satisfaction vs GDP per capita)"
  )
}

# ---------------------------------------------------------------- 36. hmlrpt

#' Linear regression by SGD, cross-checked against normal equations (Geron Ch 10, morie.fn hmlrpt)
#'
#' Shuffle order via the shared LCG stream, not numpy PCG64. With
#' batch_size == nrow(X) (the default) descent is full-batch and
#' shuffle-independent, so the anchor is exact.
#' @param X,y Data.
#' @param epochs Passes.
#' @param lr Learning rate.
#' @param batch_size Mini-batch size.
#' @param seed Shuffle seed.
#' @return List with w, b, loss_history, final_loss, w_closed_form, b_closed_form, gap, lr_limit, estimate, n, method.
#' @export
morie_geron_linreg_pytorch <- function(X, y, epochs = 100, lr = 0.01, batch_size = NULL, seed = 0) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  yy <- as.numeric(y)
  .morie_gr_need(nrow(A) == length(yy), "geron_linreg_pytorch: X/y mismatch")
  n_epochs <- as.integer(epochs)
  .morie_gr_need(n_epochs >= 1L, "geron_linreg_pytorch: epochs must be >= 1")
  eta <- as.numeric(lr)
  .morie_gr_need(is.finite(eta) && eta > 0, "geron_linreg_pytorch: bad lr")
  m <- nrow(A)
  n <- ncol(A)
  bs <- if (is.null(batch_size)) m else as.integer(batch_size)
  .morie_gr_need(bs >= 1L && bs <= m, "geron_linreg_pytorch: bad batch_size")

  Ab <- cbind(1, A)
  theta_cf <- .morie_gr_lstsq(Ab, yy)
  b_cf <- theta_cf[1]
  w_cf <- theta_cf[-1]
  H <- (2 / m) * (t(Ab) %*% Ab)
  lam <- max(eigen(0.5 * (H + t(H)), symmetric = TRUE, only.values = TRUE)$values)
  limit <- if (lam > 0) 2 / lam else Inf

  w <- rep(0, n)
  b <- 0
  history <- numeric(n_epochs)
  for (ep in seq_len(n_epochs)) {
    order_idx <- if (bs < m) order(.morie_gr_lcg_u(m, as.integer(seed) + 7919L * (ep - 1L))) else seq_len(m)
    for (st in seq(1L, m, by = bs)) {
      idx <- order_idx[st:min(st + bs - 1L, m)]
      Xb <- A[idx, , drop = FALSE]
      yb <- yy[idx]
      resid <- as.numeric(Xb %*% w) + b - yb
      gw <- (2 / length(idx)) * as.numeric(t(Xb) %*% resid)
      gb <- 2 * mean(resid)
      w <- w - eta * gw
      b <- b - eta * gb
    }
    loss <- mean((as.numeric(A %*% w) + b - yy)^2)
    .morie_gr_need(
      is.finite(loss) && loss <= 1e12 * (1 + mean(yy^2)),
      sprintf("geron_linreg_pytorch: the loss diverged at lr=%g", eta)
    )
    history[ep] <- loss
  }
  gap <- sqrt(sum((c(b, w) - theta_cf)^2))
  list(
    w = w, b = b, loss_history = history, final_loss = history[n_epochs], w_closed_form = w_cf,
    b_closed_form = b_cf, gap = gap, lr_limit = limit, estimate = history[n_epochs], n = m,
    method = "Linear regression by SGD (PyTorch listing, computed in numpy)"
  )
}

# ---------------------------------------------------------------- 37. hmlstm

#' LSTM cell forward step (Geron Ch 13, morie.fn hmlstm)
#' @param x_t Input.
#' @param h_prev,c_prev Previous short/long-term states.
#' @param weights Named list with W_i,U_i,b_i,W_f,U_f,b_f,W_o,U_o,b_o,W_g,U_g,b_g.
#' @return List with h_t, c_t, i_t, f_t, o_t, g_t, estimate, n, method.
#' @export
morie_geron_lstm <- function(x_t, h_prev, c_prev, weights) {
  keys <- c("W_i", "U_i", "b_i", "W_f", "U_f", "b_f", "W_o", "U_o", "b_o", "W_g", "U_g", "b_g")
  .morie_gr_need(all(keys %in% names(weights)), "geron_lstm: weights missing required keys")
  x <- as.numeric(x_t)
  h <- as.numeric(h_prev)
  c <- as.numeric(c_prev)
  .morie_gr_need(length(x) > 0L && length(h) > 0L, "geron_lstm: x_t/h_prev must be non-empty")
  .morie_gr_need(length(c) == length(h), "geron_lstm: c_prev/h_prev length mismatch")
  n_in <- length(x)
  n_units <- length(h)
  W <- list()
  for (k in keys) {
    arr <- weights[[k]]
    if (startsWith(k, "b")) {
      arr <- as.numeric(arr)
      want <- n_units
    } else {
      arr <- as.matrix(arr)
      want <- if (startsWith(k, "W")) c(n_units, n_in) else c(n_units, n_units)
    }
    .morie_gr_need(all(is.finite(arr)), paste0("geron_lstm: weights[['", k, "']] non-finite"))
    W[[k]] <- arr
  }
  i_g <- .morie_gr_w4b_sigmoid(as.numeric(W$W_i %*% x + W$U_i %*% h + W$b_i))
  f_g <- .morie_gr_w4b_sigmoid(as.numeric(W$W_f %*% x + W$U_f %*% h + W$b_f))
  o_g <- .morie_gr_w4b_sigmoid(as.numeric(W$W_o %*% x + W$U_o %*% h + W$b_o))
  g_g <- tanh(as.numeric(W$W_g %*% x + W$U_g %*% h + W$b_g))
  c_t <- f_g * c + i_g * g_g
  h_t <- o_g * tanh(c_t)
  list(
    h_t = h_t, c_t = c_t, i_t = i_g, f_t = f_g, o_t = o_g, g_t = g_g,
    estimate = sqrt(sum(h_t^2)), n = n_units, method = "LSTM cell forward step"
  )
}

# ---------------------------------------------------------------- 38. hmmbkm (delegates to morie_geron_kmeans_plus_plus)

#' Mini-batch k-means, Sculley's variant (Geron Ch 8, morie.fn hmmbkm)
#'
#' Batch draws via the shared LCG stream, not numpy PCG64 choice.
#' @param X Data.
#' @param n_clusters Clusters.
#' @param batch_size Points sampled per iteration.
#' @param seed Seed.
#' @param n_iter Iterations.
#' @return List with labels, centers, inertia, counts, n_iter, estimate, n, method.
#' @export
morie_geron_minibatch_kmeans <- function(X, n_clusters, batch_size, seed = 0, n_iter = 100) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  m <- nrow(A)
  k <- as.integer(n_clusters)
  .morie_gr_need(k >= 1L && k <= m, "geron_minibatch_kmeans: n_clusters out of range")
  bs <- as.integer(batch_size)
  .morie_gr_need(bs >= 1L && bs <= m, "geron_minibatch_kmeans: bad batch_size")
  iters <- as.integer(n_iter)
  .morie_gr_need(iters >= 1L, "geron_minibatch_kmeans: n_iter must be >= 1")

  centers <- morie_geron_kmeans_plus_plus(A, k, seed = as.integer(seed))$centers
  counts <- integer(k)
  for (it in seq_len(iters)) {
    u <- .morie_gr_lcg_u(m, as.integer(seed) + 131L * it)
    batch <- order(u)[seq_len(bs)]
    Xb <- A[batch, , drop = FALSE]
    d2 <- matrix(0, bs, k)
    for (j in seq_len(k)) d2[, j] <- rowSums((Xb - matrix(centers[j, ], bs, ncol(A), byrow = TRUE))^2)
    assign <- apply(d2, 1, which.min)
    for (i in seq_len(bs)) {
      cj <- assign[i]
      counts[cj] <- counts[cj] + 1L
      lrate <- 1 / counts[cj]
      centers[cj, ] <- (1 - lrate) * centers[cj, ] + lrate * Xb[i, ]
    }
  }
  d2_all <- matrix(0, m, k)
  for (j in seq_len(k)) d2_all[, j] <- rowSums((A - matrix(centers[j, ], m, ncol(A), byrow = TRUE))^2)
  labels <- apply(d2_all, 1, which.min)
  inertia <- sum(apply(d2_all, 1, min))
  list(
    labels = labels - 1L, centers = centers, inertia = inertia, counts = counts, n_iter = iters,
    estimate = inertia, n = m, method = "Mini-batch k-means"
  )
}

# ---------------------------------------------------------------- 39. hmmcd

#' Monte Carlo dropout uncertainty (Geron Ch 11, morie.fn hmmcd)
#'
#' Masks via the shared LCG stream, not numpy PCG64.
#' @param model function(x_masked) -> prediction.
#' @param x Input.
#' @param K Passes.
#' @param p Drop probability.
#' @param seed Seed.
#' @return List with mean, var, std, sem, samples, predictive_entropy, estimate, n, method.
#' @export
morie_geron_mc_dropout <- function(model, x, K = 100, p = 0.5, seed = 0) {
  .morie_gr_need(is.function(model), "geron_mc_dropout: model must be callable")
  a <- as.numeric(x)
  .morie_gr_need(length(a) > 0L, "geron_mc_dropout: x is empty")
  passes <- as.integer(K)
  .morie_gr_need(passes >= 2L, "geron_mc_dropout: K must be at least 2")
  rate <- as.numeric(p)
  .morie_gr_need(rate >= 0 && rate < 1, "geron_mc_dropout: p must lie in [0, 1)")

  samples <- NULL
  shape_len <- NULL
  for (i in seq_len(passes)) {
    u <- .morie_gr_lcg_u(length(a), as.integer(seed) + 65537L * i)
    mask <- as.numeric(u >= rate) / (1 - rate)
    out <- as.numeric(model(a * mask))
    .morie_gr_need(all(is.finite(out)), "geron_mc_dropout: model returned non-finite output")
    if (is.null(shape_len)) {
      shape_len <- length(out)
    } else {
      .morie_gr_need(length(out) == shape_len, "geron_mc_dropout: model output shape changed between passes")
    }
    samples <- rbind(samples, out)
  }
  mean_v <- colMeans(samples)
  var_v <- apply(samples, 2, stats::var)
  std_v <- sqrt(var_v)
  sem_v <- std_v / sqrt(passes)
  ent <- NULL
  if (shape_len > 1L && all(mean_v >= 0) && abs(sum(mean_v) - 1) < 1e-6) {
    ent <- -sum(mean_v * log(pmax(mean_v, 1e-300)))
  }
  list(
    mean = mean_v, var = var_v, std = std_v, sem = sem_v, samples = samples, predictive_entropy = ent,
    K = passes, p = rate, estimate = mean(mean_v), n = length(a), method = "Monte Carlo dropout uncertainty"
  )
}

# ---------------------------------------------------------------- 40. hmmcel

#' Memory-cell recurrence c_t = f(c_prev, x_t) (Geron Ch 13, morie.fn hmmcel)
#' @param c_prev Initial state.
#' @param x_t Input (n_in,) or (T,n_in).
#' @param f function(c_prev,x_t) -> c_t.
#' @return List with c_t, states, deltas, n_steps, estimate, n, method.
#' @export
morie_geron_memory_cell <- function(c_prev, x_t, f) {
  .morie_gr_need(is.function(f), "geron_memory_cell: f must be callable")
  c <- as.numeric(c_prev)
  .morie_gr_need(length(c) > 0L, "geron_memory_cell: c_prev is empty")
  X <- if (is.null(dim(x_t))) matrix(as.numeric(x_t), nrow = 1) else as.matrix(x_t)
  storage.mode(X) <- "double"
  states <- vector("list", nrow(X))
  deltas <- numeric(nrow(X))
  cur <- c
  for (step in seq_len(nrow(X))) {
    nxt <- as.numeric(f(cur, X[step, ]))
    .morie_gr_need(length(nxt) == length(cur), "geron_memory_cell: f changed the state shape")
    .morie_gr_need(all(is.finite(nxt)), "geron_memory_cell: f returned a non-finite state")
    deltas[step] <- sqrt(sum((nxt - cur)^2))
    cur <- nxt
    states[[step]] <- cur
  }
  S <- do.call(rbind, states)
  list(
    c_t = cur, states = S, deltas = deltas, n_steps = nrow(S), estimate = sqrt(sum(cur^2)),
    n = length(cur), method = "Memory-cell recurrence c_t = f(c_{t-1}, x_t)"
  )
}

# ---------------------------------------------------------------- 41. hmmcp

#' MCP/JSON-RPC 2.0 exchange with envelope validation (Geron Ch 15, morie.fn hmmcp)
#' @param server function(request) -> response.
#' @param client Callable or list of requests.
#' @param requests Optional override.
#' @return List with exchanges, n_ok, n_errors, methods, estimate, n, method.
#' @export
morie_geron_model_context_protocol <- function(server, client, requests = NULL) {
  .morie_gr_need(is.function(server), "geron_model_context_protocol: server must be callable")
  reqs <- if (!is.null(requests)) requests else if (is.function(client)) client() else client
  .morie_gr_need(length(reqs) > 0L, "geron_model_context_protocol: no requests")

  validate_req <- function(req, i) {
    .morie_gr_need(is.list(req), sprintf("geron_model_context_protocol: request %d is not an object", i))
    .morie_gr_need(identical(req$jsonrpc, "2.0"), sprintf("geron_model_context_protocol: request %d bad jsonrpc", i))
    .morie_gr_need(
      is.character(req$method) && length(req$method) == 1L,
      sprintf("geron_model_context_protocol: request %d has no string method", i)
    )
    req
  }
  validate_resp <- function(resp, req, i) {
    .morie_gr_need(is.list(resp), sprintf("geron_model_context_protocol: response %d is not an object", i))
    .morie_gr_need(identical(resp$jsonrpc, "2.0"), sprintf("geron_model_context_protocol: response %d bad jsonrpc", i))
    has_result <- !is.null(resp$result)
    has_error <- !is.null(resp$error)
    .morie_gr_need(
      has_result != has_error,
      sprintf("geron_model_context_protocol: response %d must carry exactly one of result/error", i)
    )
    .morie_gr_need(
      identical(resp$id, req$id),
      sprintf("geron_model_context_protocol: response %d id mismatch", i)
    )
    resp
  }

  exchanges <- vector("list", length(reqs))
  n_ok <- 0L
  n_err <- 0L
  methods <- character(length(reqs))
  for (i in seq_along(reqs)) {
    req <- validate_req(reqs[[i]], i)
    resp <- server(req)
    resp <- validate_resp(resp, req, i)
    is_err <- !is.null(resp$error)
    if (is_err) n_err <- n_err + 1L else n_ok <- n_ok + 1L
    methods[i] <- req$method
    exchanges[[i]] <- list(
      request = req, response = resp, method = req$method, ok = !is_err,
      error_name = if (is_err) resp$error$message else NULL
    )
  }
  list(
    exchanges = exchanges, n_ok = n_ok, n_errors = n_err, methods = methods, wire_bytes = NA_integer_,
    estimate = n_ok, n = length(exchanges), method = "MCP / JSON-RPC 2.0 exchange with envelope validation"
  )
}

# ---------------------------------------------------------------- 42. hmmdc (delegates to morie_geron_pairwise_distances)

#' Mode-collapse diagnostics (Geron Ch 18, morie.fn hmmdc)
#' @param samples Generated samples.
#' @param reference Optional real data.
#' @param tol Optional mode radius.
#' @return List with n_modes, collapse_score, mean_pairwise_distance, coverage, mode_labels, mode_sizes, estimate, n, method.
#' @export
morie_geron_mode_collapse <- function(samples, reference = NULL, tol = NULL) {
  S <- as.matrix(samples)
  storage.mode(S) <- "double"
  m <- nrow(S)
  .morie_gr_need(m >= 2L, "geron_mode_collapse: needs at least 2 samples")
  D <- .morie_gr_w4b_pairwise_distances(S)
  iu <- upper.tri(D)
  mean_pd <- mean(D[iu])
  max_pd <- max(D[iu])

  R <- NULL
  if (!is.null(reference)) {
    R <- as.matrix(reference)
    storage.mode(R) <- "double"
  }

  t <- if (is.null(tol)) {
    scale <- if (!is.null(R) && nrow(R) > 1L) {
      Dr <- .morie_gr_w4b_pairwise_distances(R)
      max(Dr[upper.tri(Dr)])
    } else {
      max_pd
    }
    0.05 * scale
  } else {
    as.numeric(tol)
  }

  labels <- rep(-1L, m)
  n_modes <- 0L
  for (i in seq_len(m)) {
    if (labels[i] != -1L) next
    n_modes <- n_modes + 1L
    stack <- c(i)
    labels[i] <- n_modes
    while (length(stack)) {
      u <- stack[length(stack)]
      stack <- stack[-length(stack)]
      cand <- which(D[u, ] <= t & labels == -1L)
      if (length(cand)) {
        labels[cand] <- n_modes
        stack <- c(stack, cand)
      }
    }
  }
  sizes <- as.numeric(table(factor(labels, levels = seq_len(n_modes))))

  coverage <- NULL
  if (!is.null(R)) {
    Dg <- matrix(0, nrow(R), m)
    for (i in seq_len(nrow(R))) Dg[i, ] <- sqrt(pmax(colSums((t(S) - R[i, ])^2), 0))
    covered <- if (t > 0) apply(Dg <= max(t, 1e-12) * 5, 1, any) else apply(Dg == 0, 1, any)
    coverage <- mean(covered)
  }
  collapse <- 1 - n_modes / m
  list(
    n_modes = n_modes, collapse_score = collapse, mean_pairwise_distance = mean_pd,
    max_pairwise_distance = max_pd, coverage = coverage, mode_labels = labels - 1L, mode_sizes = sizes,
    tol = t, estimate = collapse, n = m, method = "Mode-collapse diagnostics"
  )
}

# ---------------------------------------------------------------- 43. hmmdp

#' MDP validation plus value iteration (Geron Ch 19, morie.fn hmmdp)
#' @param states,actions Label vectors.
#' @param P Array (n_s,n_a,n_s).
#' @param R Array (n_s,n_a,n_s) or (n_s,n_a).
#' @param gamma Discount in \[0,1).
#' @param max_iter Sweep cap.
#' @param tol Convergence threshold.
#' @return List with V, Q, policy, policy_labels, n_iter, effective_horizon, estimate, n, method.
#' @export
morie_geron_mdp <- function(states, actions, P, R, gamma = 0.95, max_iter = 1000, tol = 1e-10) {
  S <- states
  A <- actions
  n_s <- length(S)
  n_a <- length(A)
  .morie_gr_need(n_s >= 1L && n_a >= 1L, "geron_mdp: need at least one state and one action")
  Pa <- array(as.numeric(P), dim = c(n_s, n_a, n_s))
  .morie_gr_need(all(is.finite(Pa)) && all(Pa >= 0), "geron_mdp: P must be finite and non-negative")
  for (s in seq_len(n_s)) {
    for (a in seq_len(n_a)) {
      tot <- sum(Pa[s, a, ])
      .morie_gr_need(abs(tot - 1) <= 1e-9, sprintf("geron_mdp: P[%d, %d] does not sum to 1", s - 1L, a - 1L))
    }
  }
  Rraw <- R
  Ra <- if (length(dim(Rraw)) == 2L || is.null(dim(Rraw))) {
    Rm <- matrix(as.numeric(Rraw), n_s, n_a)
    array(rep(Rm, n_s), dim = c(n_s, n_a, n_s))
  } else {
    array(as.numeric(Rraw), dim = c(n_s, n_a, n_s))
  }
  .morie_gr_need(all(is.finite(Ra)), "geron_mdp: R contains non-finite values")
  g <- as.numeric(gamma)
  .morie_gr_need(g >= 0 && g < 1, "geron_mdp: gamma must lie in [0, 1)")
  iters <- as.integer(max_iter)

  expected_r <- matrix(0, n_s, n_a)
  for (s in seq_len(n_s)) for (a in seq_len(n_a)) expected_r[s, a] <- sum(Pa[s, a, ] * Ra[s, a, ])
  V <- rep(0, n_s)
  n_iter <- 0L
  delta <- Inf
  for (n_iter in seq_len(iters)) {
    Q <- matrix(0, n_s, n_a)
    for (s in seq_len(n_s)) for (a in seq_len(n_a)) Q[s, a] <- expected_r[s, a] + g * sum(Pa[s, a, ] * V)
    V_new <- apply(Q, 1, max)
    delta <- max(abs(V_new - V))
    V <- V_new
    if (delta <= tol) break
  }
  Q <- matrix(0, n_s, n_a)
  for (s in seq_len(n_s)) for (a in seq_len(n_a)) Q[s, a] <- expected_r[s, a] + g * sum(Pa[s, a, ] * V)
  policy <- apply(Q, 1, which.max)
  horizon <- if (g < 1) 1 / (1 - g) else Inf
  list(
    V = V, Q = Q, policy = policy - 1L, policy_labels = A[policy], expected_reward = expected_r,
    n_iter = n_iter, gamma = g, effective_horizon = horizon, estimate = max(V), n = n_s,
    method = "MDP validation and value iteration"
  )
}

# ---------------------------------------------------------------- 14/44. hmiso (delegates to morie_geron_mds, morie_geron_pairwise_distances)

#' .morie_gr_w4b_dijkstra
#'
#' A step of the geron_w4b_native implementation. Called by \code{morie_geron_isomap}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param adj A vector; indexed elementwise.
#' @param source See Usage.
#' @param m A count; the body uses it as \code{rep(...)}.
#' @return The value of \code{dist}, as built in the body.
#' @export
.morie_gr_w4b_dijkstra <- function(adj, source, m) {
  dist <- rep(Inf, m)
  dist[source] <- 0
  seen <- rep(FALSE, m)
  repeat {
    cand <- which(!seen)
    if (!length(cand)) break
    u <- cand[which.min(dist[cand])]
    if (!is.finite(dist[u])) break
    seen[u] <- TRUE
    for (e in adj[[u]]) {
      v <- e[1]
      w <- e[2]
      nd <- dist[u] + w
      if (nd < dist[v]) dist[v] <- nd
    }
  }
  dist
}

#' Isomap: geodesic MDS via kNN graph + Dijkstra (Geron Ch 7, morie.fn hmiso)
#' @param X Data.
#' @param n_components Embedding dim.
#' @param n_neighbors Neighbours.
#' @return List with embedding, geodesic_distances, eigenvalues, stress, n_neighbors, estimate, n, method.
#' @export
morie_geron_isomap <- function(X, n_components, n_neighbors = 5) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  m <- nrow(A)
  k <- as.integer(n_neighbors)
  .morie_gr_need(k >= 1L && k < m, "geron_isomap: n_neighbors out of range")
  D <- .morie_gr_w4b_pairwise_distances(A)
  edge <- matrix(FALSE, m, m)
  for (i in seq_len(m)) {
    picked <- setdiff(order(D[i, ]), i)[seq_len(k)]
    edge[i, picked] <- TRUE
    edge[picked, i] <- TRUE
  }
  adj <- vector("list", m)
  for (i in seq_len(m)) {
    js <- which(edge[i, ])
    adj[[i]] <- lapply(js, function(j) c(j, D[i, j]))
  }
  G <- matrix(0, m, m)
  for (i in seq_len(m)) G[i, ] <- .morie_gr_w4b_dijkstra(adj, i, m)
  .morie_gr_need(all(is.finite(G)), "geron_isomap: the neighbour graph is disconnected; raise n_neighbors")
  G <- 0.5 * (G + t(G))
  diag(G) <- 0

  mds <- morie_geron_mds(G, n_components = n_components, precomputed = TRUE)
  iu <- upper.tri(D)
  Dsafe <- D
  Dsafe[iu][Dsafe[iu] == 0] <- 1
  ratio <- mean((G[iu]) / (Dsafe[iu]))
  list(
    embedding = mds$embedding, geodesic_distances = G, euclidean_distances = D,
    eigenvalues = mds$eigenvalues, stress = mds$stress, geodesic_ratio = ratio, n_neighbors = k,
    estimate = mds$stress, n = m, method = "Isomap (geodesic distances + classical MDS)"
  )
}

# ---------------------------------------------------------------- 45. hmmha (internal scaled-dot-product-attention helper)

#' .morie_gr_w4b_sdpa
#'
#' A step of the geron_w4b_native implementation. Called by \code{morie_geron_multihead_attention}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Q A matrix; passed to \code{ncol}.
#' @param K A matrix; passed to \code{t}.
#' @param V A matrix; passed to \code{\%*\%}.
#' @param mask Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @return A list with \code{output}, \code{attention}.
#' @export
.morie_gr_w4b_sdpa <- function(Q, K, V, mask = NULL) {
  Q <- as.matrix(Q)
  K <- as.matrix(K)
  V <- as.matrix(V)
  d_k <- ncol(Q)
  scores <- (Q %*% t(K)) / sqrt(d_k)
  if (!is.null(mask)) {
    Mm <- as.matrix(mask)
    scores[Mm == 0] <- -Inf
  }
  attn <- .morie_gr_w4b_softmax_rows(scores)
  list(output = attn %*% V, attention = attn)
}

#' Multi-head attention: split, attend, concat, project (Geron Ch 15, morie.fn hmmha)
#' @param Q,K,V Query/key/value matrices.
#' @param n_heads Head count (must divide Q/K width).
#' @param W_O Optional output projection.
#' @param mask Optional mask.
#' @return List with output, head_outputs, attention_weights, d_head, estimate, n, method.
#' @export
morie_geron_multihead_attention <- function(Q, K, V, n_heads, W_O = NULL, mask = NULL) {
  Qa <- as.matrix(Q)
  Ka <- as.matrix(K)
  Va <- as.matrix(V)
  h <- as.integer(n_heads)
  .morie_gr_need(h >= 1L, "geron_multihead_attention: n_heads must be >= 1")
  .morie_gr_need(ncol(Qa) == ncol(Ka), "geron_multihead_attention: Q/K width mismatch")
  .morie_gr_need(nrow(Ka) == nrow(Va), "geron_multihead_attention: K/V row mismatch")
  .morie_gr_need(ncol(Qa) %% h == 0L, "geron_multihead_attention: n_heads does not divide Q/K width")
  .morie_gr_need(ncol(Va) %% h == 0L, "geron_multihead_attention: n_heads does not divide V width")
  d_head <- ncol(Qa) %/% h
  d_vhead <- ncol(Va) %/% h
  outs <- vector("list", h)
  attns <- vector("list", h)
  for (i in seq_len(h)) {
    qs <- ((i - 1L) * d_head + 1L):(i * d_head)
    vs <- ((i - 1L) * d_vhead + 1L):(i * d_vhead)
    head <- .morie_gr_w4b_sdpa(Qa[, qs, drop = FALSE], Ka[, qs, drop = FALSE], Va[, vs, drop = FALSE], mask = mask)
    outs[[i]] <- head$output
    attns[[i]] <- head$attention
  }
  concat <- do.call(cbind, outs)
  Wo <- if (is.null(W_O)) diag(ncol(concat)) else as.matrix(W_O)
  .morie_gr_need(nrow(Wo) == ncol(concat), "geron_multihead_attention: W_O row mismatch")
  out <- concat %*% Wo
  list(
    output = out, concat = concat, head_outputs = outs, attention_weights = attns, d_head = d_head,
    n_heads = h, estimate = sqrt(sum(out^2)), n = nrow(Qa), method = "Multi-head attention (split, attend, concat, project)"
  )
}

# ---------------------------------------------------------------- 46. hmmis7

#' Mistral-7B architecture accounting (Geron Ch 15, morie.fn hmmis7)
#' @param prompt Integer token ids (0-based).
#' @param n_tokens Tokens to generate.
#' @param n_layers,d_model,n_heads,n_kv_heads,d_ff,vocab_size,window Architecture.
#' @param dtype_bytes Cache bytes/elem.
#' @return List with total_parameters, parameters_per_layer, breakdown, d_head, kv_cache_bytes,
#'   kv_cache_saving, attention_mask, effective_context, estimate, n, method.
#' @export
morie_geron_mistral7b <- function(prompt, n_tokens, n_layers = 32, d_model = 4096, n_heads = 32,
                                  n_kv_heads = 8, d_ff = 14336, vocab_size = 32000, window = 4096,
                                  dtype_bytes = 2) {
  ids <- as.integer(prompt)
  .morie_gr_need(length(ids) > 0L, "geron_mistral7b: prompt is empty")
  n_layers <- as.integer(n_layers)
  d_model <- as.integer(d_model)
  n_heads <- as.integer(n_heads)
  n_kv_heads <- as.integer(n_kv_heads)
  d_ff <- as.integer(d_ff)
  vocab_size <- as.integer(vocab_size)
  win <- as.integer(window)
  db <- as.integer(dtype_bytes)
  n_new <- as.integer(n_tokens)
  .morie_gr_need(n_new >= 0L, "geron_mistral7b: n_tokens must be non-negative")
  .morie_gr_need(d_model %% n_heads == 0L, "geron_mistral7b: d_model not divisible by n_heads")
  .morie_gr_need(n_heads %% n_kv_heads == 0L, "geron_mistral7b: n_heads not divisible by n_kv_heads")
  .morie_gr_need(min(ids) >= 0L && max(ids) < vocab_size, "geron_mistral7b: token ids out of range")

  # double, not integer: parameter counts run into the billions and overflow
  # R's 32-bit integer (.Machine$integer.max ~= 2.1e9).
  d_model_d <- as.double(d_model)
  d_ff_d <- as.double(d_ff)
  vocab_d <- as.double(vocab_size)
  n_layers_d <- as.double(n_layers)
  d_head <- d_model %/% n_heads
  d_kv <- n_kv_heads * d_head
  attn_per_layer <- 2 * d_model_d * d_model_d + 2 * d_model_d * d_kv
  ffn_per_layer <- 3 * d_model_d * d_ff_d
  norm_per_layer <- 2 * d_model_d
  per_layer <- attn_per_layer + ffn_per_layer + norm_per_layer

  breakdown <- list(
    token_embedding = vocab_d * d_model_d, output_head = vocab_d * d_model_d,
    attention = n_layers_d * attn_per_layer, feedforward = n_layers_d * ffn_per_layer,
    norms = n_layers_d * norm_per_layer + d_model_d
  )
  total <- Reduce(`+`, breakdown)
  n_prompt <- length(ids)
  total_len <- n_prompt + n_new
  kv_bytes <- 2 * n_layers_d * total_len * d_kv * db
  kv_bytes_mha <- 2 * n_layers_d * total_len * d_model_d * db

  i <- matrix(seq_len(n_prompt) - 1L, n_prompt, n_prompt)
  j <- t(i)
  mask <- (j <= i) & (j > i - win)

  list(
    total_parameters = total, parameters_per_layer = per_layer, breakdown = breakdown, d_head = d_head,
    d_kv = d_kv, kv_cache_bytes = kv_bytes, kv_cache_bytes_mha = kv_bytes_mha,
    kv_cache_saving = kv_bytes_mha / kv_bytes, attention_mask = mask, window = win,
    effective_context = win * n_layers, n_prompt_tokens = n_prompt, n_generated = n_new,
    estimate = total, n = n_prompt, method = "Mistral-7B architecture accounting"
  )
}

# ---------------------------------------------------------------- 47. hmmish

#' Mish activation z*tanh(softplus(z)) (Geron Ch 11, morie.fn hmmish)
#' @param z Pre-activations. @return List with activation, derivative, softplus, minimum, estimate, n, method.
#' @export
morie_geron_mish <- function(z) {
  a <- as.numeric(z)
  .morie_gr_need(length(a) > 0L, "geron_mish: z is empty")
  .morie_gr_need(all(is.finite(a)), "geron_mish: z contains non-finite values")
  sp <- .morie_gr_w4b_softplus(a)
  th <- tanh(sp)
  out <- a * th
  deriv <- th + a * .morie_gr_w4b_sigmoid(a) * (1 - th * th)
  list(
    activation = out, derivative = deriv, softplus = sp, minimum = min(out), estimate = mean(out),
    n = length(a), method = "Mish activation"
  )
}

# ---------------------------------------------------------------- 48. hmmlb (delegates to EXISTING morie_geron_confusion_matrix)

#' Multilabel classification metrics, leave-one-out kNN default (Geron Ch 3, morie.fn hmmlb)
#' @param X Features.
#' @param Y Binary label matrix (m,K).
#' @param k Neighbours.
#' @param Y_pred Optional predictions.
#' @return List with Y_pred, subset_accuracy, hamming_loss, jaccard, per_label_f1, macro_f1,
#'   zero_baseline_hamming, estimate, n, method.
#' @export
morie_geron_multilabel <- function(X, Y, k = 3, Y_pred = NULL) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  Yb <- as.matrix(Y)
  storage.mode(Yb) <- "integer"
  .morie_gr_need(nrow(Yb) == nrow(A), "geron_multilabel: X/Y row mismatch")
  m <- nrow(Yb)
  K <- ncol(Yb)

  if (!is.null(Y_pred)) {
    P <- as.matrix(Y_pred)
    storage.mode(P) <- "integer"
    source <- "supplied"
  } else {
    kk <- as.integer(k)
    .morie_gr_need(kk >= 1L && kk <= m - 1L, "geron_multilabel: bad k for leave-one-out")
    D <- .morie_gr_w4b_pairwise_distances(A)
    diag(D) <- Inf
    P <- matrix(0L, m, K)
    for (i in seq_len(m)) {
      nn <- order(D[i, ])[seq_len(kk)]
      P[i, ] <- as.integer(colMeans(Yb[nn, , drop = FALSE]) >= 0.5)
    }
    source <- sprintf("leave-one-out %d-NN binary relevance", kk)
  }
  correct <- P == Yb
  hamming <- 1 - mean(correct)
  subset <- mean(apply(correct, 1, all))
  inter <- rowSums((P == 1) & (Yb == 1))
  union <- rowSums((P == 1) | (Yb == 1))
  jac <- ifelse(union > 0, inter / pmax(union, 1), 1)
  jaccard <- mean(jac)
  f1s <- numeric(K)
  for (j in seq_len(K)) {
    cm <- morie_geron_confusion_matrix(Yb[, j], P[, j], n_classes = 2)
    f1s[j] <- cm$f1[2]
  }
  macro <- mean(f1s)
  zero_baseline <- mean(Yb)
  list(
    Y_pred = P, subset_accuracy = subset, hamming_loss = hamming, jaccard = jaccard, per_label_f1 = f1s,
    macro_f1 = macro, zero_baseline_hamming = zero_baseline, source = source, estimate = subset, n = m,
    method = "Multilabel classification (binary relevance kNN, leave-one-out)"
  )
}

# ---------------------------------------------------------------- 49. hmmlm

#' Masked language modelling objective (Geron Ch 15, morie.fn hmmlm)
#'
#' Mask positions via the shared LCG stream, not numpy PCG64 choice.
#' @param X Integer token ids (0-based), vector or matrix.
#' @param mask_frac Fraction masked.
#' @param model Optional function(masked_X, positions) -> probability matrix.
#' @param vocab_size Optional vocab size.
#' @param seed Seed.
#' @param mask_token Substituted value.
#' @return List with loss, baseline_loss, perplexity, masked_positions, targets, probabilities, n_masked, estimate, n, method.
#' @export
morie_geron_masked_lm <- function(X, mask_frac = 0.15, model = NULL, vocab_size = NULL, seed = 0, mask_token = -1) {
  A <- if (is.null(dim(X))) matrix(as.integer(X), nrow = 1) else matrix(as.integer(X), nrow = nrow(as.matrix(X)))
  .morie_gr_need(all(A >= 0), "geron_masked_lm: token ids must be non-negative")
  V <- if (is.null(vocab_size)) max(A) + 1L else as.integer(vocab_size)
  .morie_gr_need(max(A) < V, "geron_masked_lm: token ids out of range")
  frac <- as.numeric(mask_frac)
  .morie_gr_need(frac > 0 && frac < 1, "geron_masked_lm: mask_frac must lie in (0, 1)")

  flat <- as.integer(A)
  n_tok <- length(flat)
  n_mask <- max(1L, as.integer(round(frac * n_tok)))
  .morie_gr_need(n_mask < n_tok, "geron_masked_lm: masking leaves no visible context")
  u <- .morie_gr_lcg_u(n_tok, as.integer(seed))
  positions <- sort(order(u)[seq_len(n_mask)])
  targets <- flat[positions]

  masked <- flat
  masked[positions] <- as.integer(mask_token)
  visible <- flat[-positions]
  counts <- as.numeric(table(factor(visible, levels = 0:(V - 1)))) + 1
  unigram <- counts / sum(counts)
  baseline_probs <- matrix(unigram, nrow = n_mask, ncol = V, byrow = TRUE)
  baseline_loss <- mean(-log(pmax(baseline_probs[cbind(seq_len(n_mask), targets + 1L)], 1e-300)))

  if (is.null(model)) {
    probs <- baseline_probs
  } else {
    .morie_gr_need(is.function(model), "geron_masked_lm: model must be callable")
    probs <- as.matrix(model(matrix(masked, nrow(A), ncol(A)), positions - 1L))
    .morie_gr_need(all(dim(probs) == c(n_mask, V)), "geron_masked_lm: model returned the wrong shape")
    sums <- rowSums(probs)
    .morie_gr_need(all(abs(sums - 1) <= 1e-6), "geron_masked_lm: model rows do not sum to 1")
  }
  picked <- probs[cbind(seq_len(n_mask), targets + 1L)]
  loss <- mean(-log(pmax(picked, 1e-300)))
  ppl <- exp(loss)
  list(
    loss = loss, baseline_loss = baseline_loss, perplexity = ppl, masked_positions = positions - 1L,
    masked_input = matrix(masked, nrow(A), ncol(A)), targets = targets, probabilities = probs,
    n_masked = n_mask, vocab_size = V, estimate = loss, n = n_tok, method = "Masked language modelling objective"
  )
}

# ---------------------------------------------------------------- 50. hmmlpf

#' MLP forward pass (Geron Ch 9, morie.fn hmmlpf)
#' @param X Input batch.
#' @param weights,biases Per-layer parameters.
#' @param activations Per-layer activation names or functions.
#' @return List with output, activations, pre_activations, n_parameters, estimate, n, method.
#' @export
morie_geron_mlp <- function(X, weights, biases, activations) {
  A <- if (is.null(dim(X))) matrix(as.numeric(X), nrow = 1) else as.matrix(X)
  storage.mode(A) <- "double"
  L <- length(weights)
  .morie_gr_need(L > 0L, "geron_mlp: weights is empty")
  .morie_gr_need(length(biases) == L && length(activations) == L, "geron_mlp: length mismatch")
  fnmap <- list(
    relu = function(z) pmax(z, 0), tanh = tanh, sigmoid = .morie_gr_w4b_sigmoid,
    softmax = .morie_gr_w4b_softmax_rows, identity = function(z) z, linear = function(z) z
  )
  a <- A
  acts <- list()
  pres <- list()
  n_params <- 0L
  fns <- character(L)
  for (l in seq_len(L)) {
    W <- as.matrix(weights[[l]])
    storage.mode(W) <- "double"
    b <- as.numeric(biases[[l]])
    .morie_gr_need(nrow(W) == ncol(a), "geron_mlp: layer width mismatch")
    .morie_gr_need(length(b) == ncol(W), "geron_mlp: bias width mismatch")
    spec <- activations[[l]]
    fn <- if (is.function(spec)) spec else fnmap[[tolower(spec)]]
    .morie_gr_need(!is.null(fn), "geron_mlp: unknown activation")
    fns[l] <- if (is.function(spec)) "callable" else tolower(spec)
    z <- sweep(a %*% W, 2, b, "+")
    a <- fn(z)
    .morie_gr_need(all(dim(a) == dim(z)), "geron_mlp: activation changed shape")
    pres[[l]] <- z
    acts[[l]] <- a
    n_params <- n_params + length(W) + length(b)
  }
  list(
    output = a, activations = acts, pre_activations = pres, n_parameters = n_params,
    layer_activations = fns, estimate = mean(a), n = nrow(A), method = "MLP forward pass"
  )
}

# ---------------------------------------------------------------- 51. hmmms

#' Min-max scaling to a target range (Geron Ch 2, morie.fn hmmms)
#' @param X Data.
#' @param feature_range Target (low, high).
#' @return List with X_scaled, data_min, data_max, data_range, scale, estimate, n, method.
#' @export
morie_geron_min_max_scaling <- function(X, feature_range = c(0.0, 1.0)) {
  A <- if (is.null(dim(X))) matrix(as.numeric(X), ncol = 1) else as.matrix(X)
  storage.mode(A) <- "double"
  low <- as.numeric(feature_range[1])
  high <- as.numeric(feature_range[2])
  .morie_gr_need(low < high, "geron_min_max_scaling: feature_range must satisfy low < high")
  mn <- apply(A, 2, min)
  mx <- apply(A, 2, max)
  rng <- mx - mn
  .morie_gr_need(all(rng != 0), "geron_min_max_scaling: a constant column makes scaling undefined")
  unit <- sweep(sweep(A, 2, mn, "-"), 2, rng, "/")
  scaled <- unit * (high - low) + low
  list(
    X_scaled = scaled, data_min = mn, data_max = mx, data_range = rng, scale = (high - low) / rng,
    feature_range = c(low, high), estimate = mean(scaled), n = nrow(A), method = "Min-max scaling"
  )
}

# ---------------------------------------------------------------- 52. hmmnl

#' Softmax regression fitted by batch gradient descent (Geron Ch 4, morie.fn hmmnl)
#' @param X Features.
#' @param Y Class indices (0-based) or one-hot matrix.
#' @param lr Learning rate.
#' @param n_iter Iterations.
#' @param add_bias Prepend ones column.
#' @param alpha L2 strength on non-bias rows.
#' @return List with Theta, probabilities, prediction, loss_history, loss, accuracy, estimate, n, method.
#' @export
morie_geron_multinomial_logistic <- function(X, Y, lr = 0.1, n_iter = 1000, add_bias = TRUE, alpha = 0.0) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  m <- nrow(A)
  Ya <- Y
  if (is.null(dim(Ya)) || ncol(as.matrix(Ya)) == 1L) {
    idx <- as.integer(as.vector(Ya))
    K <- max(idx) + 1L
    .morie_gr_need(K >= 2L, "geron_multinomial_logistic: needs at least 2 classes")
    Yh <- diag(K)[idx + 1L, , drop = FALSE]
  } else {
    Yh <- as.matrix(Ya)
    storage.mode(Yh) <- "double"
    K <- ncol(Yh)
    idx <- apply(Yh, 1, which.max) - 1L
  }
  if (isTRUE(add_bias)) A <- cbind(1, A)
  n <- ncol(A)
  eta <- as.numeric(lr)
  .morie_gr_need(is.finite(eta) && eta > 0, "geron_multinomial_logistic: bad lr")
  iters <- as.integer(n_iter)
  .morie_gr_need(iters >= 1L, "geron_multinomial_logistic: n_iter must be >= 1")
  a_ <- as.numeric(alpha)
  .morie_gr_need(is.finite(a_) && a_ >= 0, "geron_multinomial_logistic: bad alpha")

  Theta <- matrix(0, n, K)
  penalty_mask <- matrix(1, n, K)
  if (isTRUE(add_bias)) penalty_mask[1, ] <- 0
  history <- numeric(iters)
  for (it in seq_len(iters)) {
    P <- .morie_gr_w4b_softmax_rows(A %*% Theta)
    loss <- mean(-log(pmax(P[cbind(seq_len(m), idx + 1L)], 1e-300)))
    if (a_ > 0) loss <- loss + 0.5 * a_ * sum((Theta * penalty_mask)^2)
    .morie_gr_need(is.finite(loss), "geron_multinomial_logistic: the loss diverged")
    history[it] <- loss
    grad <- (t(A) %*% (P - Yh)) / m + a_ * Theta * penalty_mask
    Theta <- Theta - eta * grad
  }
  P <- .morie_gr_w4b_softmax_rows(A %*% Theta)
  final_loss <- mean(-log(pmax(P[cbind(seq_len(m), idx + 1L)], 1e-300)))
  pred <- apply(P, 1, which.max) - 1L
  acc <- mean(pred == idx)
  list(
    Theta = Theta, probabilities = P, prediction = pred, loss_history = history, loss = final_loss,
    accuracy = acc, n_classes = K, estimate = final_loss, n = m,
    method = "Softmax regression fitted by batch gradient descent"
  )
}

# ---------------------------------------------------------------- 53. hmmnsh

#' Mean shift: mode-seeking via kernel density gradient ascent (Geron Ch 8, morie.fn hmmnsh)
#' @param X Data.
#' @param bandwidth Kernel bandwidth.
#' @param kernel gaussian/flat.
#' @param max_iter Iteration cap.
#' @param tol Shift tolerance.
#' @param merge_tol Optional mode-merge distance.
#' @return List with labels, modes, n_clusters, trajectories_iters, estimate, n, method.
#' @export
morie_geron_mean_shift <- function(X, bandwidth, kernel = "gaussian", max_iter = 300, tol = 1e-6, merge_tol = NULL) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  m <- nrow(A)
  h <- as.numeric(bandwidth)
  .morie_gr_need(is.finite(h) && h > 0, "geron_mean_shift: bandwidth must be positive")
  .morie_gr_need(kernel %in% c("gaussian", "flat"), "geron_mean_shift: bad kernel")
  mt <- if (is.null(merge_tol)) h / 2 else as.numeric(merge_tol)
  .morie_gr_need(mt > 0, "geron_mean_shift: merge_tol must be positive")

  peaks <- matrix(0, m, ncol(A))
  iters_v <- integer(m)
  for (i in seq_len(m)) {
    x <- A[i, ]
    for (step in seq_len(as.integer(max_iter))) {
      d2 <- rowSums((A - matrix(x, m, ncol(A), byrow = TRUE))^2)
      w <- if (kernel == "gaussian") exp(-d2 / (2 * h * h)) else as.numeric(d2 <= h * h)
      tot <- sum(w)
      .morie_gr_need(tot != 0, "geron_mean_shift: a point has no neighbours within the flat bandwidth")
      new <- as.numeric((w %*% A) / tot)
      shift <- sqrt(sum((new - x)^2))
      x <- new
      iters_v[i] <- step
      if (shift <= tol) break
    }
    peaks[i, ] <- x
  }
  modes <- NULL
  labels <- integer(m)
  for (i in seq_len(m)) {
    assigned <- FALSE
    if (!is.null(modes)) {
      for (j in seq_len(nrow(modes))) {
        if (sqrt(sum((peaks[i, ] - modes[j, ])^2)) <= mt) {
          labels[i] <- j
          assigned <- TRUE
          break
        }
      }
    }
    if (!assigned) {
      modes <- rbind(modes, peaks[i, ])
      labels[i] <- nrow(modes)
    }
  }
  for (j in seq_len(nrow(modes))) if (any(labels == j)) modes[j, ] <- colMeans(A[labels == j, , drop = FALSE])
  list(
    labels = labels - 1L, modes = modes, n_clusters = nrow(modes), peaks = peaks,
    trajectories_iters = iters_v, bandwidth = h, estimate = nrow(modes), n = m,
    method = "Mean shift (mode seeking)"
  )
}

# ---------------------------------------------------------------- 54. hmmod

#' Model-based least squares, closed form cross-checked against descent (Geron Ch 1, morie.fn hmmod)
#' @param X,y Data.
#' @param add_bias Prepend ones column.
#' @param eta Optional learning rate.
#' @param n_iter Descent iterations.
#' @return List with theta, theta_gd, mse, r2, gap, eta, estimate, n, method.
#' @export
morie_geron_model_based <- function(X, y, add_bias = TRUE, eta = NULL, n_iter = 1000) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  yy <- as.numeric(y)
  .morie_gr_need(nrow(A) == length(yy), "geron_model_based: X/y mismatch")
  if (isTRUE(add_bias)) A <- cbind(1, A)
  m <- nrow(A)
  n <- ncol(A)
  .morie_gr_need(m >= n, "geron_model_based: underdetermined fit")
  iters <- as.integer(n_iter)
  .morie_gr_need(iters >= 1L, "geron_model_based: n_iter must be >= 1")

  theta <- .morie_gr_lstsq(A, yy)
  H <- (2 / m) * (t(A) %*% A)
  lam <- max(eigen(0.5 * (H + t(H)), symmetric = TRUE, only.values = TRUE)$values)
  lr <- if (is.null(eta)) (if (lam > 0) 1 / lam else 0.1) else as.numeric(eta)
  .morie_gr_need(is.finite(lr) && lr > 0, "geron_model_based: eta must be positive and finite")

  t_gd <- rep(0, n)
  for (i in seq_len(iters)) {
    t_gd <- t_gd - lr * ((2 / m) * as.numeric(t(A) %*% (as.numeric(A %*% t_gd) - yy)))
    .morie_gr_need(all(is.finite(t_gd)), "geron_model_based: gradient descent diverged")
  }
  resid <- as.numeric(A %*% theta) - yy
  mse <- mean(resid^2)
  ss_tot <- sum((yy - mean(yy))^2)
  r2 <- if (ss_tot == 0) (if (mse == 0) 1 else -Inf) else 1 - sum(resid^2) / ss_tot
  gap <- sqrt(sum((t_gd - theta)^2))
  list(
    theta = theta, theta_gd = t_gd, mse = mse, r2 = r2, gap = gap, eta = lr, residuals = resid,
    n_parameters = n, estimate = mse, n = m, method = "Model-based fit (least squares, closed form and by descent)"
  )
}

# ---------------------------------------------------------------- 55. hmmpp

#' .morie_gr_w4b_mpp_partition
#'
#' A step of the geron_w4b_native implementation. Called by \code{morie_geron_model_parallelism}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param w A vector; its length is taken and its elements indexed.
#' @param k Numeric; combined arithmetically in the body.
#' @return The value of \code{assign}, as built in the body.
#' @export
.morie_gr_w4b_mpp_partition <- function(w, k) {
  fits <- function(cap) {
    used <- 1L
    load <- 0
    for (v in w) {
      if (v > cap) {
        return(FALSE)
      }
      if (load + v > cap) {
        used <- used + 1L
        load <- v
        if (used > k) {
          return(FALSE)
        }
      } else {
        load <- load + v
      }
    }
    TRUE
  }
  lo <- max(w)
  hi <- sum(w)
  for (i in seq_len(200)) {
    if (hi - lo <= 1e-9 * max(1, hi)) break
    mid <- 0.5 * (lo + hi)
    if (fits(mid)) hi <- mid else lo <- mid
  }
  cap <- hi
  assign <- integer(length(w))
  dev <- 1L
  load <- 0
  n <- length(w)
  for (i in seq_along(w)) {
    v <- w[i]
    if (dev < k && (load + v > cap || (n - i + 1L) <= (k - dev))) {
      dev <- dev + 1L
      load <- 0
    }
    assign[i] <- dev
    load <- load + v
  }
  assign
}

#' Optimal contiguous model-parallel layer partition (Geron Ch 17, morie.fn hmmpp)
#' @param model Sequence/list of per-layer parameter counts, arrays, or mappings with a "params" entry.
#' @param n_devices Devices.
#' @return List with assignment, device_loads, max_load, imbalance, naive_utilisation, cut_points, estimate, n, method.
#' @export
morie_geron_model_parallelism <- function(model, n_devices) {
  w <- vapply(model, function(item) {
    if (is.list(item) && !is.null(item$params)) as.numeric(item$params) else as.numeric(length(unlist(item)))
  }, numeric(1))
  w[vapply(model, function(item) is.numeric(item) && length(item) == 1L, logical(1))] <-
    as.numeric(model[vapply(model, function(item) is.numeric(item) && length(item) == 1L, logical(1))])
  .morie_gr_need(length(w) > 0L, "geron_model_parallelism: model has no layers")
  .morie_gr_need(all(is.finite(w)) && all(w >= 0), "geron_model_parallelism: bad layer sizes")
  k <- as.integer(n_devices)
  .morie_gr_need(k >= 1L && k <= length(w), "geron_model_parallelism: n_devices out of range")
  assign <- .morie_gr_w4b_mpp_partition(w, k)
  loads <- vapply(seq_len(k), function(d) sum(w[assign == d]), numeric(1))
  mean_l <- mean(loads)
  imbalance <- if (mean_l > 0) (max(loads) - mean_l) / mean_l else 0
  cuts <- which(diff(assign) != 0)
  list(
    assignment = assign - 1L, layer_sizes = w, device_loads = loads, max_load = max(loads),
    imbalance = imbalance, naive_utilisation = 1 / k, cut_points = cuts - 1L, n_transfers = length(cuts),
    estimate = assign - 1L, n = length(w),
    method = "Optimal contiguous layer partition minimising the maximum device load"
  )
}

# ---------------------------------------------------------------- 56. hmmto

#' Multioutput classification by k-NN vote (Geron Ch 3, morie.fn hmmto)
#' @param X Features.
#' @param Y Categorical targets (m,t).
#' @param k Neighbours voting.
#' @param X_new Optional rows to predict.
#' @return List with predictions, predict, accuracy_per_output, accuracy, n_outputs, estimate, n, method.
#' @export
morie_geron_multioutput <- function(X, Y, k = 1, X_new = NULL) {
  A <- as.matrix(X)
  storage.mode(A) <- "double"
  Yv <- if (is.null(dim(Y))) matrix(Y, ncol = 1) else as.matrix(Y)
  m <- nrow(Yv)
  t_ <- ncol(Yv)
  .morie_gr_need(m == nrow(A), "geron_multioutput: X/Y row mismatch")
  kk <- as.integer(k)
  .morie_gr_need(kk >= 1L, "geron_multioutput: k must be >= 1")

  vote <- function(D, exclude_self) {
    out <- matrix(vector(mode = storage.mode(Yv), length = nrow(D) * t_), nrow(D), t_)
    for (i in seq_len(nrow(D))) {
      ord <- order(D[i, ])
      if (exclude_self) ord <- ord[ord != i]
      nb <- ord[seq_len(kk)]
      for (j in seq_len(t_)) {
        tab <- table(Yv[nb, j])
        out[i, j] <- names(tab)[which.max(tab)]
      }
    }
    out
  }
  predict_fn <- function(Xnew) {
    B <- if (is.null(dim(Xnew))) matrix(as.numeric(Xnew), nrow = 1) else as.matrix(Xnew)
    D <- matrix(0, nrow(B), nrow(A))
    for (i in seq_len(nrow(B))) D[i, ] <- sqrt(rowSums((A - matrix(B[i, ], nrow(A), ncol(A), byrow = TRUE))^2))
    vote(D, exclude_self = FALSE)
  }
  if (is.null(X_new)) {
    D <- .morie_gr_w4b_pairwise_distances(A)
    pred <- vote(D, exclude_self = TRUE)
    storage.mode(pred) <- storage.mode(Yv)
    per <- vapply(seq_len(t_), function(j) mean(pred[, j] == Yv[, j]), numeric(1))
    acc <- mean(pred == Yv)
    exact <- mean(apply(pred == Yv, 1, all))
  } else {
    pred <- predict_fn(X_new)
    storage.mode(pred) <- storage.mode(Yv)
    per <- rep(NaN, t_)
    acc <- NaN
    exact <- NaN
  }
  list(
    predictions = pred, predict = predict_fn, accuracy_per_output = per, accuracy = acc,
    exact_match = exact, n_outputs = t_, estimate = pred, n = m,
    method = sprintf("%d-NN multioutput vote (leave-one-out when no new rows are given)", kk)
  )
}

# ---------------------------------------------------------------- 57. hmmxp (internal maxpool helper)

#' .morie_gr_w4b_maxpool
#'
#' A step of the geron_w4b_native implementation. Called by \code{morie_geron_max_pool}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A matrix; indexed by row and column.
#' @param k Numeric; combined arithmetically in the body.
#' @param s Numeric; combined arithmetically in the body.
#' @return A list with \code{y}, \code{argmax}, \code{output_shape}.
#' @export
.morie_gr_w4b_maxpool <- function(x, k, s) {
  x <- as.matrix(x)
  H <- nrow(x)
  W <- ncol(x)
  oh <- (H - k) %/% s + 1L
  ow <- (W - k) %/% s + 1L
  y <- matrix(0, oh, ow)
  arg <- matrix(0L, oh, ow)
  for (i in seq_len(oh)) {
    for (j in seq_len(ow)) {
      r0 <- (i - 1L) * s + 1L
      c0 <- (j - 1L) * s + 1L
      block <- x[r0:(r0 + k - 1L), c0:(c0 + k - 1L), drop = FALSE]
      y[i, j] <- max(block)
      arg[i, j] <- which.max(block)
    }
  }
  list(y = y, argmax = arg, output_shape = c(oh, ow))
}

#' Max pooling over a sliding window, per channel (Geron Ch 12, morie.fn hmmxp)
#' @param x Feature map (h,w) or (h,w,c).
#' @param window Square pooling window.
#' @param stride Defaults to window.
#' @return List with pooled, argmax, output_shape, parameters, estimate, n, method.
#' @export
morie_geron_max_pool <- function(x, window = 2, stride = NULL) {
  a <- x
  d <- dim(a)
  .morie_gr_need(!is.null(d) && length(d) %in% c(2L, 3L), "geron_max_pool: x must be (h,w) or (h,w,c)")
  k <- as.integer(window)
  .morie_gr_need(k >= 1L, "geron_max_pool: window must be >= 1")
  s <- if (is.null(stride)) k else as.integer(stride)
  .morie_gr_need(s >= 1L, "geron_max_pool: stride must be >= 1")
  H <- d[1]
  W <- d[2]
  .morie_gr_need(k <= H && k <= W, "geron_max_pool: window does not fit the map")

  if (length(d) == 2L) {
    base <- .morie_gr_w4b_maxpool(a, k, s)
    pooled <- base$y
    arg <- base$argmax
    shape <- base$output_shape
  } else {
    outs <- vector("list", d[3])
    args <- vector("list", d[3])
    for (c_ in seq_len(d[3])) {
      b <- .morie_gr_w4b_maxpool(a[, , c_], k, s)
      outs[[c_]] <- b$y
      args[[c_]] <- b$argmax
    }
    pooled <- array(unlist(outs), dim = c(nrow(outs[[1]]), ncol(outs[[1]]), d[3]))
    arg <- array(unlist(args), dim = dim(pooled))
    shape <- dim(pooled)
  }
  list(
    pooled = pooled, y = pooled, argmax = arg, output_shape = shape, parameters = 0L,
    estimate = pooled, n = length(a), method = "Max pooling over a sliding window, per channel"
  )
}
