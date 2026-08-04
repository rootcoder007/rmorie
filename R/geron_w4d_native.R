# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Geron shelf, wave 4d. Mirrors 54 morie.fn modules (hm* wave 4) from
# Geron A, Hands-On Machine Learning with Scikit-Learn, Keras and
# TensorFlow (3rd ed., O'Reilly 2022).
#
# Shares conventions and helpers with geron_ml_native.R / geron_ml2_native.R:
#   * 0-based indices from Python (argmax positions, token ids, ...) stay
#     0-based here; any index used to SUBSET an R object gets +1 locally.
#   * numpy default var/std is population (ddof=0): use .morie_gr_pvar /
#     .morie_gr_psd (already defined in geron_ml_native.R).
#   * numpy ravel/reshape is row-major; matrices from a flat stream use
#     byrow = TRUE.
#   * %/% binds tighter than +/- in R: every floor-division of a sum is
#     parenthesised.
#   * LCG draw-for-draw: s <- (1664525*s + 1013904223) %% 2^32,
#     u <- (s + 0.5) / 2^32 -- see .morie_w4d_lcg_u below (own stream
#     helper so the wave-4 modules that inline their own LCG loop match
#     Python's per-call `rng` variable exactly; existing .morie_gr_lcg_u
#     / .morie_al_lcg use a slightly different call contract).
#
# Reused, not redefined: .morie_gr_softmax (1-D softmax, geron_ml_native.R)
# for the hmsftm-delegating callers (hmsac); .morie_gr_pvar/.morie_gr_psd
# for population variance/sd.

.morie_w4d_lcg_u <- function(n, seed) {
  s <- as.numeric(seed) %% 2^32
  out <- numeric(n)
  for (i in seq_len(n)) {
    s <- (1664525 * s + 1013904223) %% 2^32
    out[i] <- (s + 0.5) / 2^32
  }
  out
}

# ---------------------------------------------------------------------
# hmsdp: scaled dot-product attention (kernel used by hmsatt / hmself*)
# ---------------------------------------------------------------------

#' Scaled dot-product attention
#'
#' Att(Q,K,V) = softmax(Q K^T / sqrt(d_k)) V. Masked entries are set to
#' -Inf before the row-wise softmax; a query row with no visible key is
#' an error.
#'
#' @param Q Queries (T_q, d_k); a 1-D vector is read as one row.
#' @param K Keys (T_k, d_k).
#' @param V Values (T_k, d_v).
#' @param d_k Scaling dimension; defaults to ncol(K).
#' @param mask Optional (T_q, T_k) or length-T_k logical/0-1 mask; TRUE/1 = visible.
#' @return list with Y, attention, scores, d_k, estimate, n, method.
#' @export
morie_geron_scaled_dot_product <- function(Q, K, V, d_k = NULL, mask = NULL) {
  Qa <- as.matrix(Q)
  Ka <- as.matrix(K)
  Va <- as.matrix(V)
  if (is.null(dim(Q)) || length(dim(as.array(Q))) < 2) Qa <- matrix(as.numeric(Q), nrow = 1)
  if (is.null(dim(K)) || length(dim(as.array(K))) < 2) Ka <- matrix(as.numeric(K), nrow = 1)
  if (is.null(dim(V)) || length(dim(as.array(V))) < 2) Va <- matrix(as.numeric(V), ncol = 1)
  dk <- if (is.null(d_k)) ncol(Ka) else as.integer(d_k)

  scores <- (Qa %*% t(Ka)) / sqrt(as.numeric(dk))
  if (!is.null(mask)) {
    m <- as.matrix(mask)
    if (is.null(dim(mask)) || length(mask) == ncol(scores)) {
      m <- matrix(as.logical(mask), nrow = nrow(scores), ncol = ncol(scores), byrow = TRUE)
    } else {
      m <- matrix(as.logical(mask), nrow = nrow(scores), ncol = ncol(scores))
    }
    scores[!m] <- -Inf
  }

  attn <- matrix(0.0, nrow(scores), ncol(scores))
  for (i in seq_len(nrow(scores))) {
    row <- scores[i, ]
    finite <- is.finite(row)
    a <- numeric(length(row))
    if (sum(finite) == 1) {
      a[finite] <- 1.0
    } else {
      a[finite] <- .morie_gr_softmax(row[finite])
    }
    attn[i, ] <- a
  }
  Y <- attn %*% Va

  list(
    Y = Y, output = Y, attention = attn, scores = scores, d_k = dk,
    estimate = max(attn), n = nrow(Qa),
    method = "Scaled dot-product attention with pre-softmax masking"
  )
}

# ---------------------------------------------------------------------
# hmsatt: self-attention (Q=K=V from the same input)
# ---------------------------------------------------------------------

#' Self-attention: Q=K=V come from same input
#'
#' Projects X into Q, K, V with W_Q/W_K/W_V and delegates the kernel to
#' \code{morie_geron_scaled_dot_product}.
#'
#' @param X Sequence (T, d_model).
#' @param W_Q,W_K Projections (d_model, d_k); same width required.
#' @param W_V Projection (d_model, d_v).
#' @param mask Optional mask passed through to the kernel.
#' @return list with Y, attention, Q, K, V, estimate, n, method.
#' @export
morie_geron_self_attention_modules <- function(X, W_Q, W_K, W_V, mask = NULL) {
  Xa <- as.matrix(X)
  Q <- Xa %*% as.matrix(W_Q)
  K <- Xa %*% as.matrix(W_K)
  V <- Xa %*% as.matrix(W_V)
  inner <- morie_geron_scaled_dot_product(Q, K, V, d_k = ncol(Q), mask = mask)
  list(
    Y = inner$Y, attention = inner$attention, scores = inner$scores,
    Q = Q, K = K, V = V,
    estimate = max(inner$attention), n = nrow(Xa),
    method = "Self-attention: shared-source projections into hmsdp"
  )
}

# ---------------------------------------------------------------------
# hmsac: soft actor-critic (tabular)
# ---------------------------------------------------------------------

#' Soft actor-critic (SAC): entropy-regularized max reward
#'
#' Tabular SAC: soft state value V(s) = sum_a pi(a|s)(Q(s,a) - alpha log pi(a|s)),
#' soft critic update Q += lr*(target - Q), closed-form policy improvement
#' pi(.|s) = softmax(Q(s,.)/alpha).
#'
#' @param env List/environment with functions \code{reset()}, \code{step(a)}
#'   returning list(s2, r, done), and integers n_states, n_actions.
#' @param policy Optional initial (n_states, n_actions) probabilities; default uniform.
#' @param critic Optional initial (n_states, n_actions) Q table; default zeros.
#' @param epochs,lr,alpha,gamma,steps,seed As in the Python original.
#' @return list with policy, Q, V, entropy, returns, estimate, n, method.
#' @export
morie_geron_sac <- function(env, policy = NULL, critic = NULL, epochs = 20, lr = 0.5,
                            alpha = 0.2, gamma = 0.9, steps = 20, seed = 0) {
  n_s <- as.integer(env$n_states)
  n_a <- as.integer(env$n_actions)
  E <- as.integer(epochs)
  Tt <- as.integer(steps)
  step_size <- as.numeric(lr)
  temp <- as.numeric(alpha)
  g <- as.numeric(gamma)

  Pi <- if (is.null(policy)) matrix(1.0 / n_a, n_s, n_a) else as.matrix(policy)
  Q <- if (is.null(critic)) matrix(0.0, n_s, n_a) else as.matrix(critic)

  rng <- as.numeric(seed) %% 2^32
  u_draw <- function() {
    rng <<- (1664525 * rng + 1013904223) %% 2^32
    (rng + 0.5) / 2^32
  }
  soft_V <- function(P, Qt) {
    logp <- log(pmax(P, .Machine$double.xmin))
    rowSums(P * (Qt - temp * logp))
  }

  entropies <- numeric(E)
  returns <- numeric(E)
  s <- as.integer(env$reset())
  for (ep in seq_len(E)) {
    batch <- vector("list", Tt)
    total <- 0.0
    for (t in seq_len(Tt)) {
      u <- u_draw()
      cs <- cumsum(Pi[s + 1, ])
      a <- sum(cs < u) # 0-based action index: count of cumulative bins strictly below u
      a <- min(a, n_a - 1L)
      step_out <- env$step(a)
      s2 <- as.integer(step_out[[1]])
      rew <- as.numeric(step_out[[2]])
      done <- isTRUE(step_out[[3]])
      batch[[t]] <- list(s = s, a = a, r = rew, s2 = s2, done = done)
      total <- total + rew
      s <- if (done) as.integer(env$reset()) else s2
    }
    V <- soft_V(Pi, Q)
    for (t in seq_len(Tt)) {
      b <- batch[[t]]
      target <- b$r + (if (b$done) 0.0 else g * V[b$s2 + 1])
      Q[b$s + 1, b$a + 1] <- Q[b$s + 1, b$a + 1] + step_size * (target - Q[b$s + 1, b$a + 1])
    }
    for (i in seq_len(n_s)) Pi[i, ] <- .morie_gr_softmax(Q[i, ] / temp)
    logp <- log(pmax(Pi, .Machine$double.xmin))
    entropies[ep] <- mean(-rowSums(Pi * logp))
    returns[ep] <- total
  }
  V <- soft_V(Pi, Q)

  list(
    policy = Pi, Q = Q, V = V, entropy = entropies, returns = returns,
    alpha = temp, gamma = g, estimate = returns[E], n = as.integer(E * Tt),
    method = "Tabular soft actor-critic: soft value backup + closed-form Boltzmann policy improvement"
  )
}

# ---------------------------------------------------------------------
# hmsae: stacked (deep) tied-weight autoencoder
# ---------------------------------------------------------------------

.morie_w4d_lcg_mat <- function(nr, nc, seed, scale = 0.1) {
  n <- nr * nc
  s <- as.numeric(seed) %% 2^32
  out <- numeric(n)
  for (i in seq_len(n)) {
    s <- (1664525 * s + 1013904223) %% 2^32
    out[i] <- (2.0 * ((s + 0.5) / 2^32) - 1.0) * scale
  }
  matrix(out, nrow = nr, ncol = nc, byrow = TRUE)
}

.morie_w4d_sae_train_layer <- function(H, k, epochs, lr, seed) {
  n <- nrow(H)
  d <- ncol(H)
  W <- .morie_w4d_lcg_mat(d, k, seed)
  b <- numeric(k)
  cc <- numeric(d)
  losses <- numeric(epochs)
  for (e in seq_len(epochs)) {
    h <- tanh(sweep(H %*% W, 2, b, "+"))
    rec <- sweep(h %*% t(W), 2, cc, "+")
    diff <- rec - H
    losses[e] <- mean(diff * diff)
    gm <- 2.0 * diff / (n * d)
    dW <- t(gm) %*% h
    dc <- colSums(gm)
    dz <- (gm %*% W) * (1.0 - h * h)
    dW <- dW + t(H) %*% dz
    db <- colSums(dz)
    W <- W - lr * dW
    b <- b - lr * db
    cc <- cc - lr * dc
  }
  list(W = W, b = b, c = cc, losses = losses)
}

.morie_w4d_sae_forward <- function(A, Ws, bs, cs) {
  L <- length(Ws)
  hs <- vector("list", L + 1)
  hs[[1]] <- A
  H <- A
  for (i in seq_len(L)) {
    H <- tanh(sweep(H %*% Ws[[i]], 2, bs[[i]], "+"))
    hs[[i + 1]] <- H
  }
  rs <- vector("list", L + 1)
  rs[[L + 1]] <- H
  for (i in L:1) {
    u <- sweep(rs[[i + 1]] %*% t(Ws[[i]]), 2, cs[[i]], "+")
    rs[[i]] <- if (i == 1) u else tanh(u)
  }
  list(hs = hs, rs = rs)
}

.morie_w4d_sae_backward <- function(A, Ws, bs, cs, hs, rs) {
  n <- nrow(A)
  d <- ncol(A)
  L <- length(Ws)
  dW <- lapply(Ws, function(w) matrix(0.0, nrow(w), ncol(w)))
  db <- lapply(bs, function(v) numeric(length(v)))
  dc <- lapply(cs, function(v) numeric(length(v)))
  gm <- 2.0 * (rs[[1]] - A) / (n * d)
  for (i in seq_len(L)) {
    du <- if (i == 1) gm else gm * (1.0 - rs[[i]] * rs[[i]])
    dc[[i]] <- dc[[i]] + colSums(du)
    dW[[i]] <- dW[[i]] + t(du) %*% rs[[i + 1]]
    gm <- du %*% Ws[[i]]
  }
  for (i in L:1) {
    dz <- gm * (1.0 - hs[[i + 1]] * hs[[i + 1]])
    dW[[i]] <- dW[[i]] + t(hs[[i]]) %*% dz
    db[[i]] <- db[[i]] + colSums(dz)
    gm <- dz %*% t(Ws[[i]])
  }
  list(dW = dW, db = db, dc = dc)
}

#' Stacked (deep) autoencoder with multiple encoding/decoding layers
#'
#' Greedy layer-wise pretraining (each layer its own tied-weight
#' autoencoder), optionally followed by end-to-end fine-tuning of the
#' unrolled symmetric encoder/decoder stack.
#'
#' @param X Training data (n, d).
#' @param hidden_sizes Encoder widths, outermost first.
#' @param epochs Gradient steps per stage.
#' @param lr Learning rate.
#' @param seed LCG seed.
#' @param finetune Run end-to-end fine-tuning after pretraining.
#' @return list with codes, reconstruction, recon_error, layer_losses,
#'   finetune_losses, weights, biases, decoder_biases, widths, estimate, n, method.
#' @export
morie_geron_stacked_autoencoder_modules <- function(X, hidden_sizes = c(2), epochs = 200, lr = 0.5,
                                                    seed = 0, finetune = TRUE) {
  A <- as.matrix(X)
  sizes <- as.integer(hidden_sizes)
  E <- as.integer(epochs)
  step <- as.numeric(lr)
  widths <- c(ncol(A), sizes)

  Ws <- list()
  bs <- list()
  cs <- list()
  layer_losses <- list()
  H <- A
  for (i in seq_along(sizes)) {
    k <- sizes[i]
    tl <- .morie_w4d_sae_train_layer(H, k, E, step, as.numeric(seed) + 17 * i)
    Ws[[i]] <- tl$W
    bs[[i]] <- tl$b
    cs[[i]] <- tl$c
    layer_losses[[i]] <- tl$losses
    H <- tanh(sweep(H %*% tl$W, 2, tl$b, "+"))
  }

  fr <- .morie_w4d_sae_forward(A, Ws, bs, cs)
  ft <- mean((fr$rs[[1]] - A)^2)
  if (isTRUE(finetune)) {
    for (e in seq_len(E)) {
      fr <- .morie_w4d_sae_forward(A, Ws, bs, cs)
      bk <- .morie_w4d_sae_backward(A, Ws, bs, cs, fr$hs, fr$rs)
      for (i in seq_along(Ws)) {
        Ws[[i]] <- Ws[[i]] - step * bk$dW[[i]]
        bs[[i]] <- bs[[i]] - step * bk$db[[i]]
        cs[[i]] <- cs[[i]] - step * bk$dc[[i]]
      }
      fr <- .morie_w4d_sae_forward(A, Ws, bs, cs)
      ft <- c(ft, mean((fr$rs[[1]] - A)^2))
    }
  }

  fr <- .morie_w4d_sae_forward(A, Ws, bs, cs)
  err <- mean((fr$rs[[1]] - A)^2)

  list(
    codes = fr$hs[[length(fr$hs)]], reconstruction = fr$rs[[1]], recon_error = err,
    layer_losses = layer_losses, finetune_losses = ft, weights = Ws, biases = bs,
    decoder_biases = cs, widths = widths, estimate = err, n = nrow(A),
    method = "Tied-weight stacked autoencoder: greedy layer-wise pretraining then end-to-end fine-tuning"
  )
}

# ---------------------------------------------------------------------
# hmself: self-supervised pretext task (mask / denoise / callable)
# ---------------------------------------------------------------------

.morie_w4d_fit_predict <- function(A, t) {
  D <- cbind(1.0, A)
  theta <- MASS::ginv(t(D) %*% D) %*% (t(D) %*% t)
  list(theta = theta, fitted = as.numeric(D %*% theta))
}

#' Self-supervised learning: generate labels from the data itself via pretext task
#'
#' \code{"mask"}: hold out one feature, predict it from the rest, per feature.
#' \code{"denoise"}: corrupt inputs with deterministic LCG noise, reconstruct.
#' A function may be passed instead: \code{pretext(X)} returning list(Xp, yp).
#'
#' @param X Unlabeled data (n, d).
#' @param pretext "mask", "denoise", or a function.
#' @param noise Corruption scale for "denoise".
#' @param seed LCG seed.
#' @return list with loss, task_losses, predictions, targets, r2, pretext, estimate, n, method.
#' @export
morie_geron_self_supervised <- function(X, pretext = "mask", noise = 0.1, seed = 0) {
  A <- as.matrix(X)
  n <- nrow(A)
  d <- ncol(A)

  if (is.function(pretext)) {
    out <- pretext(A)
    Xp <- as.matrix(out[[1]])
    yp <- as.matrix(out[[2]])
    preds <- matrix(0.0, nrow(yp), ncol(yp))
    losses <- numeric(ncol(yp))
    for (j in seq_len(ncol(yp))) {
      fp <- .morie_w4d_fit_predict(Xp, yp[, j])
      preds[, j] <- fp$fitted
      losses[j] <- mean((fp$fitted - yp[, j])^2)
    }
    targets <- yp
    name <- "callable"
  } else {
    task <- tolower(as.character(pretext))
    if (task == "mask") {
      preds <- matrix(0.0, n, d)
      losses <- numeric(d)
      for (j in seq_len(d)) {
        keep <- setdiff(seq_len(d), j)
        fp <- .morie_w4d_fit_predict(A[, keep, drop = FALSE], A[, j])
        preds[, j] <- fp$fitted
        losses[j] <- mean((fp$fitted - A[, j])^2)
      }
      targets <- A
      name <- "mask"
    } else if (task == "denoise") {
      sc <- as.numeric(noise)
      s <- as.numeric(seed) %% 2^32
      E <- numeric(n * d)
      for (i in seq_len(n * d)) {
        s <- (1664525 * s + 1013904223) %% 2^32
        E[i] <- (2.0 * ((s + 0.5) / 2^32) - 1.0) * sc
      }
      Xn <- A + matrix(E, nrow = n, ncol = d, byrow = TRUE)
      preds <- matrix(0.0, n, d)
      losses <- numeric(d)
      for (j in seq_len(d)) {
        fp <- .morie_w4d_fit_predict(Xn, A[, j])
        preds[, j] <- fp$fitted
        losses[j] <- mean((fp$fitted - A[, j])^2)
      }
      targets <- A
      name <- "denoise"
    } else {
      stop("morie_geron_self_supervised: pretext must be 'mask', 'denoise' or a function")
    }
  }

  loss <- mean(losses)
  vv <- apply(targets, 2, .morie_gr_pvar)
  r2 <- ifelse(vv > 0, 1.0 - losses / ifelse(vv > 0, vv, 1.0), NA_real_)

  list(
    loss = loss, task_losses = losses, predictions = preds, targets = targets, r2 = r2,
    pretext = name, estimate = loss, n = n,
    method = paste0("Self-supervised '", name, "' pretext solved by least squares; no external labels used")
  )
}

# ---------------------------------------------------------------------
# hmsem: semi-supervised (Laplacian-regularised least squares)
# ---------------------------------------------------------------------

#' Semi-supervised learning: small labeled set plus large unlabeled pool
#'
#' theta = (X_l'X_l + alpha X_u'LX_u)^-1 X_l'y_l, with L = D - W the
#' graph Laplacian of the RBF affinity on the unlabeled pool.
#'
#' @param X_l Labeled inputs (n_l, d).
#' @param y_l Labels, length n_l.
#' @param X_u Unlabeled pool (n_u, d).
#' @param alpha Smoothness weight (>=0).
#' @param gamma RBF width (>0).
#' @param fit_intercept Prepend intercept column.
#' @return list with theta, fitted, unlabeled_pred, sup_loss, roughness,
#'   objective, laplacian, affinity, alpha, estimate, n, method.
#' @export
morie_geron_semisupervised <- function(X_l, y_l, X_u, alpha = 1.0, gamma = 1.0,
                                       fit_intercept = TRUE) {
  L1 <- as.matrix(X_l)
  U <- as.matrix(X_u)
  t <- as.numeric(y_l)
  a <- as.numeric(alpha)
  g <- as.numeric(gamma)

  Dl <- if (isTRUE(fit_intercept)) cbind(1.0, L1) else L1
  Du <- if (isTRUE(fit_intercept)) cbind(1.0, U) else U

  nu <- nrow(U)
  diffsq <- matrix(0.0, nu, nu)
  for (i in seq_len(nu)) {
    d2 <- rowSums(sweep(U, 2, U[i, ], "-")^2)
    diffsq[i, ] <- d2
  }
  W <- exp(-g * diffsq)
  diag(W) <- 0.0
  Lap <- diag(rowSums(W)) - W

  M <- t(Dl) %*% Dl + a * (t(Du) %*% Lap %*% Du)
  theta <- as.numeric(MASS::ginv(M) %*% (t(Dl) %*% t))
  fitted <- as.numeric(Dl %*% theta)
  f_u <- as.numeric(Du %*% theta)
  sup <- mean((fitted - t)^2)
  rough <- as.numeric(f_u %*% Lap %*% f_u)

  list(
    theta = theta, fitted = fitted, unlabeled_pred = f_u, sup_loss = sup, roughness = rough,
    objective = sum((fitted - t)^2) + a * rough, laplacian = Lap, affinity = W, alpha = a,
    estimate = sup, n = nrow(L1) + nrow(U),
    method = "Laplacian-regularised least squares: supervised loss + alpha * graph smoothness on the unlabeled pool"
  )
}

# ---------------------------------------------------------------------
# hmsenet: Squeeze-and-Excitation block
# ---------------------------------------------------------------------

.morie_w4d_lcg_vec <- function(n, seed, scale = 0.5) {
  s <- as.numeric(seed) %% 2^32
  out <- numeric(n)
  for (i in seq_len(n)) {
    s <- (1664525 * s + 1013904223) %% 2^32
    out[i] <- (2.0 * ((s + 0.5) / 2^32) - 1.0) * scale
  }
  out
}

#' Squeeze-and-Excitation (SENet) block for channel recalibration
#'
#' s = sigmoid(W2 ReLU(W1 GAP(x))); y = s * x. Squeeze is global average
#' pooling over the spatial axes (input dims 1,2), excitation is a
#' bottleneck MLP C -> C/r -> C.
#'
#' @param x Feature map array with dim (H, W, C), or a plain vector (C,).
#' @param r Reduction ratio; must divide C.
#' @param W1,W2 Optional excitation weights (C, C/r) and (C/r, C); default LCG.
#' @param seed LCG seed when W1/W2 not supplied.
#' @return list with y, s, gate, z, hidden, n_params, estimate, n, method.
#' @export
morie_geron_senet <- function(x, r = 16, W1 = NULL, W2 = NULL, seed = 0) {
  X <- x
  d <- dim(as.array(X))
  if (is.null(d) || length(d) == 1) {
    z <- as.numeric(X)
    Xarr <- array(z, dim = length(z))
  } else if (length(d) == 3) {
    Xarr <- array(as.numeric(X), dim = d)
    z <- apply(Xarr, 3, mean)
  } else {
    stop("morie_geron_senet: x must be (H, W, C) or (C,)")
  }
  C <- length(z)
  red <- as.integer(r)
  hidden <- C %/% red

  A <- if (is.null(W1)) matrix(.morie_w4d_lcg_vec(C * hidden, as.numeric(seed) + 1), nrow = C, ncol = hidden, byrow = TRUE) else as.matrix(W1)
  B <- if (is.null(W2)) matrix(.morie_w4d_lcg_vec(hidden * C, as.numeric(seed) + 2), nrow = hidden, ncol = C, byrow = TRUE) else as.matrix(W2)

  h <- pmax(as.numeric(z %*% A), 0.0)
  gate_z <- as.numeric(h %*% B)
  s <- 1.0 / (1.0 + exp(-gate_z))
  y <- if (length(d) == 3) sweep(Xarr, 3, s, "*") else Xarr * s

  list(
    y = y, s = s, gate = s, z = z, hidden = hidden, n_params = 2L * C * hidden,
    estimate = mean(s), n = C,
    method = "Squeeze (GAP) -> excite (bottleneck MLP + sigmoid) -> scale"
  )
}

# ---------------------------------------------------------------------
# hmsent: sentiment analysis (tokenize -> model -> softmax -> counts)
# ---------------------------------------------------------------------

#' Sentiment analysis with RNN or transformer on tokens
#'
#' Orchestrates tokenise -> caller model -> softmax -> (optional) counted
#' accuracy/confusion/precision/recall/F1.
#'
#' @param texts Character vector of documents.
#' @param model Function(tokens) -> numeric scores, length K >= 2, constant across texts.
#' @param tokenizer Optional function(text) -> character vector of tokens; default lowercase+split.
#' @param y_true Optional integer gold labels in 0..K-1.
#' @param labels Optional character vector of class names, length K.
#' @return list with probabilities, predicted, tokens, labels, accuracy, confusion,
#'   precision, recall, f1, macro_f1, n_classes, estimate, n, method.
#' @export
morie_geron_sentiment_analysis <- function(texts, model, tokenizer = NULL, y_true = NULL, labels = NULL) {
  docs <- as.list(texts)
  tok <- if (is.null(tokenizer)) function(t) strsplit(tolower(as.character(t)), "\\s+")[[1]] else tokenizer

  token_lists <- vector("list", length(docs))
  K <- NULL
  rows <- vector("list", length(docs))
  for (i in seq_along(docs)) {
    toks <- tok(docs[[i]])
    token_lists[[i]] <- toks
    sc <- as.numeric(model(toks))
    if (is.null(K)) K <- length(sc)
    rows[[i]] <- .morie_gr_softmax(sc)
  }
  P <- do.call(rbind, rows)
  pred <- apply(P, 1, which.max) - 1L # 0-based

  names_ <- if (!is.null(labels)) as.character(labels) else NULL

  acc <- conf <- prec <- rec <- f1 <- macro <- NULL
  if (!is.null(y_true)) {
    g <- as.integer(y_true)
    conf <- matrix(0L, K, K)
    for (i in seq_along(g)) conf[g[i] + 1, pred[i] + 1] <- conf[g[i] + 1, pred[i] + 1] + 1L
    tp <- diag(conf)
    colsum <- colSums(conf)
    rowsum <- rowSums(conf)
    prec <- ifelse(colsum > 0, tp / pmax(colsum, 1), 0.0)
    rec <- ifelse(rowsum > 0, tp / pmax(rowsum, 1), 0.0)
    denom <- prec + rec
    f1 <- ifelse(denom > 0, 2 * prec * rec / ifelse(denom > 0, denom, 1.0), 0.0)
    macro <- mean(f1)
    acc <- sum(tp) / sum(conf)
  }

  list(
    probabilities = P, predicted = pred, tokens = token_lists, labels = names_,
    accuracy = acc, confusion = conf, precision = prec, recall = rec, f1 = f1, macro_f1 = macro,
    n_classes = K, estimate = if (!is.null(acc)) acc else mean(apply(P, 1, max)), n = length(docs),
    method = "Tokenise -> model scores -> softmax, with counted accuracy / confusion / macro-F1"
  )
}

# ---------------------------------------------------------------------
# hmseq2: sequence-to-sequence encoder-decoder
# ---------------------------------------------------------------------

#' Sequence-to-sequence encoder-decoder architecture
#'
#' enc(src) -> z (context vector); dec(z, prefix) -> next-token scores.
#' Runs teacher-forced cross-entropy against tgt and a free-running
#' greedy decode.
#'
#' @param src Source sequence, passed to encoder unchanged.
#' @param tgt Integer target token ids (0-based), used for teacher forcing.
#' @param encoder Function(src) -> numeric context vector z.
#' @param decoder Function(z, prefix) -> numeric scores, length V >= 2 (prefix is 0-based ids).
#' @param max_len Greedy decode length; default length(tgt).
#' @param eos Optional token id (0-based) that stops greedy decoding.
#' @return list with z, loss, perplexity, token_logprobs, greedy, greedy_logprob,
#'   greedy_matches, exposure_bias, vocab_size, estimate, n, method.
#' @export
morie_geron_seq2seq <- function(src, tgt, encoder, decoder, max_len = NULL, eos = NULL) {
  y <- as.integer(tgt)
  z <- as.numeric(encoder(src))

  scores_at <- function(prefix) as.numeric(decoder(z, prefix))

  first <- scores_at(integer(0))
  V <- length(first)

  logps <- numeric(length(y))
  for (s in seq_along(y)) {
    sc <- if (s == 1) first else scores_at(y[seq_len(s - 1)])
    p <- .morie_gr_softmax(sc)
    logps[s] <- log(max(p[y[s] + 1], .Machine$double.xmin))
  }
  loss <- -mean(logps)

  L <- if (is.null(max_len)) length(y) else as.integer(max_len)
  greedy <- integer(0)
  greedy_lp <- 0.0
  for (s in seq_len(L)) {
    sc <- if (s == 1) first else scores_at(greedy)
    p <- .morie_gr_softmax(sc)
    k <- which.max(sc) - 1L
    greedy_lp <- greedy_lp + log(max(p[k + 1], .Machine$double.xmin))
    greedy <- c(greedy, k)
    if (!is.null(eos) && k == as.integer(eos)) break
  }

  matched <- sum(greedy[seq_len(min(length(greedy), length(y)))] == y[seq_len(min(length(greedy), length(y)))])

  list(
    z = z, loss = loss, perplexity = exp(loss), token_logprobs = logps,
    greedy = greedy, greedy_logprob = greedy_lp, greedy_matches = matched,
    exposure_bias = 1.0 - matched / max(1, min(length(greedy), length(y))),
    vocab_size = V, estimate = loss, n = length(y),
    method = "Encoder-decoder: teacher-forced cross-entropy plus greedy free-running decode"
  )
}

# ---------------------------------------------------------------------
# hmsft: supervised fine-tuning on instruction-response pairs
# ---------------------------------------------------------------------

#' Supervised fine-tuning (SFT) on instruction-response pairs
#'
#' L = -sum_i log P(y_i | x_i). Softmax head trained by exact-gradient
#' descent on bag-of-words (string prompts) or raw numeric features.
#'
#' @param model Optional initial weight matrix (n_features, n_responses); default zeros.
#' @param instruction_data List of list(instruction, response) pairs (strings or numeric/int).
#' @param epochs,lr,l2 As in the Python original.
#' @return list with W, loss, sum_loss, loss_curve, accuracy, predicted, probabilities,
#'   vocab, labels, estimate, n, method.
#' @export
morie_geron_sft <- function(model = NULL, instruction_data, epochs = 200, lr = 0.5, l2 = 0.0) {
  prompts <- lapply(instruction_data, `[[`, 1)
  responses <- lapply(instruction_data, `[[`, 2)

  if (is.character(prompts[[1]])) {
    toks_list <- lapply(prompts, function(p) strsplit(tolower(p), "\\s+")[[1]])
    vocab <- sort(unique(unlist(toks_list)))
    idx <- setNames(seq_along(vocab) - 1L, vocab)
    X <- matrix(0.0, length(prompts), length(vocab))
    for (i in seq_along(prompts)) {
      for (w in toks_list[[i]]) X[i, idx[[w]] + 1] <- X[i, idx[[w]] + 1] + 1.0
    }
  } else {
    X <- do.call(rbind, lapply(prompts, as.numeric))
    vocab <- NULL
  }
  if (is.character(responses[[1]])) {
    labels <- sort(unique(unlist(responses)))
    lidx <- setNames(seq_along(labels) - 1L, labels)
    y <- as.integer(sapply(responses, function(r) lidx[[r]]))
  } else {
    yv <- as.integer(unlist(responses))
    labels <- sort(unique(yv))
    lidx <- setNames(seq_along(labels) - 1L, as.character(labels))
    y <- as.integer(sapply(yv, function(r) lidx[[as.character(r)]]))
  }

  n <- nrow(X)
  d <- ncol(X)
  K <- length(labels)
  E <- as.integer(epochs)
  step <- as.numeric(lr)
  decay <- as.numeric(l2)

  W <- if (is.null(model)) matrix(0.0, d, K) else as.matrix(model)
  Y <- matrix(0.0, n, K)
  for (i in seq_len(n)) Y[i, y[i] + 1] <- 1.0

  fwd <- function(W) {
    z <- X %*% W
    z <- z - apply(z, 1, max)
    e <- exp(z)
    P <- e / rowSums(e)
    ll <- -mean(log(pmax(P[cbind(seq_len(n), y + 1L)], .Machine$double.xmin)))
    list(P = P, ll = ll)
  }

  losses <- numeric(0)
  for (e in seq_len(E)) {
    fp <- fwd(W)
    losses <- c(losses, fp$ll)
    W <- W - step * (t(X) %*% (fp$P - Y) / n + decay * W)
  }
  fp <- fwd(W)
  losses <- c(losses, fp$ll)
  pred <- apply(fp$P, 1, which.max) - 1L

  list(
    W = W, loss = fp$ll, sum_loss = fp$ll * n, loss_curve = losses,
    accuracy = mean(pred == y), predicted = labels[pred + 1], probabilities = fp$P,
    vocab = vocab, labels = labels, estimate = fp$ll, n = n,
    method = "Softmax maximum-likelihood fine-tuning on instruction-response demonstrations"
  )
}

# ---------------------------------------------------------------------
# hmsgdc: SGD hinge-loss (linear SVM) classifier
# ---------------------------------------------------------------------

#' SGD classifier with hinge loss (linear SVM) trained by stochastic gradient descent
#'
#' Subgradient update: violated margin (y_i f(x_i) < 1) -> w -= lr*(alpha*w - y_i x_i),
#' b += lr*y_i; satisfied -> w -= lr*alpha*w. Sample order drawn from a
#' deterministic LCG Fisher-Yates shuffle.
#'
#' @param X Design matrix (n, d).
#' @param y Binary labels (2 distinct values).
#' @param lr,n_iter,alpha,seed,shuffle As in the Python original.
#' @return list with w, b, loss_curve, decision, predicted, accuracy, n_support,
#'   classes, estimate, n, method.
#' @export
morie_geron_sgd_classifier <- function(X, y, lr = 0.1, n_iter = 10, alpha = 0.0001,
                                       seed = 0, shuffle = TRUE) {
  Xa <- as.matrix(X)
  ya <- as.numeric(y)
  classes <- sort(unique(ya))
  if (all(unique(ya) %in% c(-1, 1))) {
    t <- ya
  } else {
    pos <- classes[length(classes)]
    t <- ifelse(ya == pos, 1.0, -1.0)
  }
  step <- as.numeric(lr)
  E <- as.integer(n_iter)
  reg <- as.numeric(alpha)
  n <- nrow(Xa)
  d <- ncol(Xa)
  w <- numeric(d)
  b <- 0.0
  rng <- as.numeric(seed) %% 2^32
  u_draw <- function() {
    rng <<- (1664525 * rng + 1013904223) %% 2^32
    (rng + 0.5) / 2^32
  }

  losses <- numeric(E)
  for (ep in seq_len(E)) {
    order <- seq_len(n) # 1-based
    if (isTRUE(shuffle)) {
      for (i in n:2) {
        j <- as.integer(u_draw() * i) + 1L # 0-based j in [0,i-1] Python -> R index j+1 in [1,i]
        tmp <- order[i]
        order[i] <- order[j]
        order[j] <- tmp
      }
    }
    for (i in order) {
      margin <- t[i] * (sum(Xa[i, ] * w) + b)
      if (margin < 1.0) {
        w <- w - step * (reg * w - t[i] * Xa[i, ])
        b <- b + step * t[i]
      } else {
        w <- w - step * reg * w
      }
    }
    f <- as.numeric(Xa %*% w) + b
    losses[ep] <- mean(pmax(0.0, 1.0 - t * f)) + 0.5 * reg * sum(w * w)
  }

  f <- as.numeric(Xa %*% w) + b
  pred_pm <- ifelse(f >= 0, 1.0, -1.0)
  pred <- if (length(classes) == 2) ifelse(pred_pm > 0, classes[length(classes)], classes[1]) else pred_pm
  acc <- mean(pred_pm == t)

  list(
    w = w, b = b, loss_curve = losses, decision = f, predicted = pred, accuracy = acc,
    n_support = sum(t * f < 1.0), classes = classes, estimate = losses[E], n = n,
    method = "Hinge-loss linear SVM trained by stochastic subgradient descent with L2 decay"
  )
}

# ---------------------------------------------------------------------
# hmsil: silhouette score for cluster quality
# ---------------------------------------------------------------------

#' Silhouette score for cluster quality
#'
#' s(i) = (b(i) - a(i)) / max(a(i), b(i)); a(i) mean distance to own
#' cluster (self excluded), b(i) smallest mean distance to any other
#' cluster. Singleton clusters score 0 by convention.
#'
#' @param X Data (n, d), n >= 2.
#' @param labels Cluster assignment per row.
#' @param metric "euclidean" or "manhattan".
#' @return list with silhouette, samples, a, b, cluster_means, n_clusters, estimate, n, method.
#' @export
morie_geron_silhouette <- function(X, labels, metric = "euclidean") {
  A <- as.matrix(X)
  lab <- as.vector(labels)
  uniq <- sort(unique(lab))
  m <- tolower(metric)
  n <- nrow(A)

  D <- matrix(0.0, n, n)
  for (i in seq_len(n)) {
    diff <- sweep(A, 2, A[i, ], "-")
    D[i, ] <- if (m == "euclidean") sqrt(rowSums(diff^2)) else rowSums(abs(diff))
  }

  a <- numeric(n)
  b <- numeric(n)
  s <- numeric(n)
  for (i in seq_len(n)) {
    own <- lab == lab[i]
    own_others <- own
    own_others[i] <- FALSE
    others_means <- sapply(uniq[uniq != lab[i]], function(c) mean(D[i, lab == c]))
    if (!any(own_others)) {
      a[i] <- 0.0
      b[i] <- min(others_means)
      s[i] <- 0.0
      next
    }
    a[i] <- mean(D[i, own_others])
    b[i] <- min(others_means)
    denom <- max(a[i], b[i])
    s[i] <- if (denom > 0) (b[i] - a[i]) / denom else 0.0
  }

  means <- setNames(sapply(uniq, function(c) mean(s[lab == c])), as.character(uniq))

  list(
    silhouette = mean(s), samples = s, a = a, b = b, cluster_means = means,
    n_clusters = length(uniq), estimate = mean(s), n = n,
    method = paste0("Silhouette coefficient with ", m, " distances (self excluded from cohesion)")
  )
}

# ---------------------------------------------------------------------
# hmsslc: semi-supervised learning via k-means representative labeling
# ---------------------------------------------------------------------

.morie_w4d_lloyd <- function(Z, k, seed = 0, iters = 100) {
  n <- nrow(Z)
  s <- as.numeric(seed) %% 2^32
  centers <- matrix(Z[1, ], nrow = 1)
  for (kk in 2:k) {
    if (k < 2) break
    d2 <- apply(centers, 1, function(c) rowSums(sweep(Z, 2, c, "-")^2))
    d2 <- if (is.matrix(d2)) apply(d2, 1, min) else d2
    tot <- sum(d2)
    s <- (1664525 * s + 1013904223) %% 2^32
    u <- (s + 0.5) / 2^32 * (if (tot > 0) tot else 1.0)
    idx <- if (tot > 0) sum(cumsum(d2) < u) else (nrow(centers) %% n) # 0-based
    idx <- min(idx, n - 1)
    centers <- rbind(centers, Z[idx + 1, ])
  }
  C <- centers
  lab <- rep(0L, n)
  for (it in seq_len(iters)) {
    d <- sapply(seq_len(nrow(C)), function(j) rowSums(sweep(Z, 2, C[j, ], "-")^2))
    new_lab <- apply(d, 1, which.min) - 1L
    if (it > 1 && all(new_lab == lab)) break
    lab <- new_lab
    for (j in 0:(k - 1)) {
      if (any(lab == j)) C[j + 1, ] <- colMeans(Z[lab == j, , drop = FALSE])
    }
  }
  list(labels = lab, centers = C)
}

#' Semi-supervised learning via k-means representative labeling
#'
#' Clusters the unlabeled pool (Lloyd/k-means++ on a deterministic LCG
#' stream), finds each cluster's real-point representative closest to
#' its centroid, labels that representative from the nearest labeled
#' point, and propagates the label to every cluster member.
#'
#' @param X Unlabeled pool (n, d).
#' @param X_labeled Labeled instances (m, d).
#' @param y_labeled Their labels, length m.
#' @param n_clusters Labeling budget.
#' @param seed LCG seed.
#' @param y_true Optional gold labels to score against.
#' @return list with labels, cluster, centers, representatives, representative_labels,
#'   accuracy, estimate, n, method.
#' @export
morie_geron_semisupervised_cluster <- function(X, X_labeled, y_labeled, n_clusters = 2,
                                               seed = 0, y_true = NULL) {
  A <- as.matrix(X)
  B <- as.matrix(X_labeled)
  yl <- y_labeled
  k <- as.integer(n_clusters)
  lloyd <- .morie_w4d_lloyd(A, k, seed = seed)
  cluster <- lloyd$labels
  C <- lloyd$centers

  reps <- integer(k)
  rep_lab <- vector("list", k)
  for (j in 0:(k - 1)) {
    members <- which(cluster == j) - 1L # 0-based
    d <- rowSums(sweep(A[members + 1, , drop = FALSE], 2, C[j + 1, ], "-")^2)
    reps[j + 1] <- members[which.min(d)]
    dl <- rowSums(sweep(B, 2, A[reps[j + 1] + 1, ], "-")^2)
    rep_lab[[j + 1]] <- yl[which.min(dl)]
  }
  rep_lab_v <- unlist(rep_lab)
  labels <- rep_lab_v[cluster + 1]

  acc <- NULL
  if (!is.null(y_true)) acc <- mean(labels == y_true)

  list(
    labels = labels, cluster = cluster, centers = C, representatives = reps,
    representative_labels = rep_lab_v, accuracy = acc,
    estimate = if (!is.null(acc)) acc else k / nrow(A), n = nrow(A),
    method = "k-means clustering, nearest-labeled representative labeling, propagation to members"
  )
}

# ---------------------------------------------------------------------
# hmstk: stacking (blending) ensemble with out-of-fold meta-features
# ---------------------------------------------------------------------

#' Stacking (blending): meta-learner combines outputs of base learners
#'
#' Meta-learner trained on out-of-fold base predictions (K-fold
#' cross-prediction) to avoid the leakage of in-sample base predictions.
#'
#' @param X Design matrix (n, d).
#' @param y Targets, length n.
#' @param base_models List of function(Xtr, ytr, Xte) -> predictions.
#' @param meta_model Optional blender function(Xtr, ytr, Xte); default OLS w/ intercept.
#' @param k_folds Folds for out-of-fold predictions.
#' @return list with predicted, meta_features, oof_mse, stacked_mse, best_base_mse,
#'   gain, n_base, estimate, n, method.
#' @export
morie_geron_stacking <- function(X, y, base_models, meta_model = NULL, k_folds = 3) {
  A <- as.matrix(X)
  t <- as.numeric(y)
  n <- nrow(A)
  K <- as.integer(k_folds)

  ols <- function(Xtr, ytr, Xte) {
    P <- cbind(1.0, as.matrix(Xtr))
    Qm <- cbind(1.0, as.matrix(Xte))
    theta <- .morie_gr_lstsq(P, as.numeric(ytr))
    as.numeric(Qm %*% theta)
  }
  meta <- if (is.null(meta_model)) ols else meta_model

  folds <- lapply(0:(K - 1), function(i) seq(i, n - 1, by = K)) # 0-based indices
  M <- length(base_models)
  Z <- matrix(0.0, n, M)
  for (f in folds) {
    mask <- rep(TRUE, n)
    mask[f + 1] <- FALSE
    for (j in seq_len(M)) {
      p <- as.numeric(base_models[[j]](A[mask, , drop = FALSE], t[mask], A[f + 1, , drop = FALSE]))
      Z[f + 1, j] <- p
    }
  }

  oof_mse <- sapply(seq_len(M), function(j) mean((Z[, j] - t)^2))
  stacked <- as.numeric(meta(Z, t, Z))
  stacked_mse <- mean((stacked - t)^2)
  best <- min(oof_mse)

  list(
    predicted = stacked, meta_features = Z, oof_mse = oof_mse, stacked_mse = stacked_mse,
    best_base_mse = best, gain = best - stacked_mse, n_base = M, estimate = stacked_mse, n = n,
    method = "Stacking with K-fold out-of-fold meta-features and a least-squares blender by default"
  )
}

# ---------------------------------------------------------------------
# hmstr: stratified sampling (largest-remainder allocation)
# ---------------------------------------------------------------------

#' Stratified sampling preserves class/strata proportions in each split
#'
#' Largest-remainder (Hamilton) allocation: floor(n_total * N_h / N) per
#' stratum, leftover units to largest fractional remainders (ties to
#' larger stratum); within-stratum draw via Fisher-Yates on an LCG stream.
#'
#' @param X Data rows (n, ...).
#' @param y Optional labels used as strata.
#' @param stratum Optional explicit stratum key per row.
#' @param n_total Sample size (required).
#' @param seed LCG seed.
#' @return list with indices (0-based), X_sample, y_sample, allocation, strata,
#'   population_share, sample_share, max_share_error, estimate, n, method.
#' @export
morie_geron_stratified_sampling <- function(X, y = NULL, stratum = NULL, n_total, seed = 0) {
  A <- as.matrix(X)
  keys <- if (!is.null(stratum)) stratum else y
  k <- as.vector(keys)
  m <- as.integer(n_total)
  n <- nrow(A)

  uniq <- sort(unique(k))
  counts <- sapply(uniq, function(h) sum(k == h))

  quota <- m * counts / n
  base <- pmax(floor(quota), 1)
  left <- m - sum(base)
  if (left < 0) {
    order_idx <- order(-counts)
    i <- 0
    while (left < 0) {
      j <- order_idx[(i %% length(order_idx)) + 1]
      if (base[j] > 1) {
        base[j] <- base[j] - 1
        left <- left + 1
      }
      i <- i + 1
    }
  } else if (left > 0) {
    rema <- quota - floor(quota)
    ord <- order(-rema, -counts)
    for (i in seq_len(left)) base[ord[((i - 1) %% length(ord)) + 1]] <- base[ord[((i - 1) %% length(ord)) + 1]] + 1
  }

  rng <- as.numeric(seed) %% 2^32
  u_draw <- function() {
    rng <<- (1664525 * rng + 1013904223) %% 2^32
    (rng + 0.5) / 2^32
  }

  picked <- integer(0)
  alloc <- list()
  for (hi in seq_along(uniq)) {
    h <- uniq[hi]
    want <- base[hi]
    idx <- which(k == h) - 1L # 0-based
    ln <- length(idx)
    if (ln > 1) {
      for (i in ln:2) {
        j <- as.integer(u_draw() * i) + 1L
        tmp <- idx[i]
        idx[i] <- idx[j]
        idx[j] <- tmp
      }
    }
    picked <- c(picked, sort(idx[seq_len(want)]))
    alloc[[as.character(h)]] <- as.integer(want)
  }
  picked <- sort(picked)

  pop_share <- counts / n
  samp_share <- base / m
  err <- max(abs(pop_share - samp_share))
  ys <- if (!is.null(y)) y[picked + 1] else NULL

  list(
    indices = picked, X_sample = A[picked + 1, , drop = FALSE], y_sample = ys, allocation = alloc,
    strata = uniq, population_share = pop_share, sample_share = samp_share, max_share_error = err,
    estimate = err, n = length(picked),
    method = "Proportional allocation with the largest-remainder rule, LCG draw within strata"
  )
}

# ---------------------------------------------------------------------
# hmstr2: stride / convolution output-size arithmetic
# ---------------------------------------------------------------------

#' Stride: step size of kernel sliding over input
#'
#' output_dim = floor((in_dim + 2p - k)/s) + 1.
#'
#' @param in_dim Input length.
#' @param k Kernel size.
#' @param p Padding each side.
#' @param s Stride.
#' @return list with output_dim, dropped, same_padding, padded_dim, in_dim, k, p, s, estimate, n, method.
#' @export
morie_geron_stride <- function(in_dim, k, p = 0, s = 1) {
  n_in <- as.integer(in_dim)
  kk <- as.integer(k)
  pp <- as.integer(p)
  ss <- as.integer(s)
  padded <- n_in + 2 * pp
  out <- ((padded - kk) %/% ss) + 1
  dropped <- padded - kk - ss * (out - 1)
  same_total <- max(0, ss * (-((-n_in) %/% ss) - 1) + kk - n_in)

  list(
    output_dim = out, dropped = dropped, same_padding = same_total, padded_dim = padded,
    in_dim = n_in, k = kk, p = pp, s = ss, estimate = out, n = n_in,
    method = "Convolution output size floor((in + 2p - k)/s) + 1"
  )
}

# ---------------------------------------------------------------------
# hmsup: supervised learning (empirical risk min + exact LOO risk)
# ---------------------------------------------------------------------

#' Supervised learning paradigm: learn mapping f(x)->y from labeled examples
#'
#' Delegates the fit to \code{morie_geron_batch_learning} (closed-form
#' ridge/least squares); adds exact leave-one-out risk from the hat
#' matrix, e_loo_i = e_i / (1 - h_ii).
#'
#' @param X Labeled inputs (n, d).
#' @param y Targets, length n.
#' @param ridge L2 penalty.
#' @param fit_intercept Include intercept column.
#' @return list with theta, predict, fitted, residuals, empirical_risk, loo_risk,
#'   optimism, leverage, r2, estimate, n, method.
#' @export
morie_geron_supervised_learning <- function(X, y, ridge = 0.0, fit_intercept = TRUE) {
  A <- as.matrix(X)
  t <- as.numeric(y)
  lam <- as.numeric(ridge)

  inner <- morie_geron_batch_learning(A, t, fit_intercept = isTRUE(fit_intercept), ridge = lam)
  theta <- inner$theta
  resid <- inner$residuals
  risk <- inner$train_mse

  D <- if (isTRUE(fit_intercept)) cbind(1.0, A) else A
  P <- t(D) %*% D + lam * diag(ncol(D))
  H <- D %*% MASS::ginv(P) %*% t(D)
  h <- pmin(pmax(diag(H), 0.0), 1.0)
  loo <- mean((resid / (1.0 - h))^2)

  list(
    theta = theta, predict = inner$predict, fitted = inner$fitted, residuals = resid,
    empirical_risk = risk, loo_risk = loo, optimism = loo - risk, leverage = h, r2 = inner$r2,
    estimate = risk, n = length(t),
    method = "Empirical risk minimisation over linear hypotheses (fit via hmbat) with exact leave-one-out risk"
  )
}

# ---------------------------------------------------------------------
# hmsvdp: OLS via SVD pseudoinverse
# ---------------------------------------------------------------------

#' OLS via SVD pseudoinverse (robust to singular X^T X)
#'
#' theta = X^+ y = V Sigma^+ U^T y; minimum-norm solution when rank deficient.
#'
#' @param X Design matrix (n, d).
#' @param y Targets, length n.
#' @param rcond Optional relative singular-value cutoff; default max(n,d)*eps.
#' @param fit_intercept Prepend a column of ones.
#' @return list with theta, singular_values, rank, condition_number, residuals,
#'   rss, deficient, estimate, n, method.
#' @export
morie_geron_svd_pseudoinverse <- function(X, y, rcond = NULL, fit_intercept = FALSE) {
  Xa <- as.matrix(X)
  ya <- as.numeric(y)
  if (isTRUE(fit_intercept)) Xa <- cbind(1.0, Xa)

  sv_out <- svd(Xa)
  U <- sv_out$u
  sv <- sv_out$d
  Vt <- t(sv_out$v)
  cut <- if (is.null(rcond)) max(dim(Xa)) * .Machine$double.eps else as.numeric(rcond)
  keep <- sv > cut * sv[1]
  rank <- sum(keep)
  s_inv <- rep(0.0, length(sv))
  s_inv[keep] <- 1.0 / sv[keep]
  theta <- as.numeric(t(Vt) %*% (s_inv * as.numeric(t(U) %*% ya)))
  resid <- as.numeric(Xa %*% theta) - ya
  rss <- sum(resid^2)
  cond <- sv[1] / sv[keep][sum(keep)]

  list(
    theta = theta, singular_values = sv, rank = rank, condition_number = cond, residuals = resid,
    rss = rss, deficient = rank < ncol(Xa), estimate = rss, n = nrow(Xa),
    method = "Minimum-norm least squares theta = V Sigma^+ U^T y"
  )
}

# ---------------------------------------------------------------------
# hmsvm2: save/load model state dict round trip (RDS stand-in for torch)
# ---------------------------------------------------------------------

#' Save and load PyTorch model state_dict (RDS-native stand-in)
#'
#' Round-trips a named list of numeric arrays through an RDS file and
#' checks keys/shapes/values match exactly on reload.
#'
#' @param model Named list of arrays (state dict), or an unnamed list (named param_0, param_1, ...).
#' @param path Destination file (\code{.rds}); its directory must already exist.
#' @param verify Reload and compare after writing.
#' @return list with path, keys, shapes, n_params, bytes, loaded, exact, max_diff, estimate, n, method.
#' @export
morie_geron_save_load_pytorch <- function(model, path, verify = TRUE) {
  if (!is.null(names(model)) && all(nzchar(names(model)))) {
    state <- model
  } else {
    state <- setNames(model, paste0("param_", seq_along(model) - 1L))
  }
  state <- lapply(state, function(v) if (is.array(v) || is.matrix(v)) v else as.numeric(v))

  p <- as.character(path)
  if (!grepl("\\.rds$", p)) p <- paste0(p, ".rds")
  saveRDS(state, p)

  loaded <- list()
  exact <- NULL
  max_diff <- NULL
  if (isTRUE(verify)) {
    loaded <- readRDS(p)
    exact <- TRUE
    max_diff <- 0.0
    for (k in names(state)) {
      v <- state[[k]]
      w <- loaded[[k]]
      exact <- exact && isTRUE(all.equal(as.numeric(w), as.numeric(v), tolerance = 0))
      max_diff <- max(max_diff, max(abs(as.numeric(w) - as.numeric(v))))
    }
  }

  n_params <- sum(sapply(state, length))
  nbytes <- as.integer(file.info(p)$size)

  list(
    path = p, keys = sort(names(state)),
    shapes = lapply(state, function(v) if (!is.null(dim(v))) dim(v) else length(v)),
    n_params = n_params, bytes = nbytes, loaded = loaded, exact = exact, max_diff = max_diff,
    estimate = as.numeric(n_params), n = length(state),
    method = "state_dict serialised to RDS and reloaded, with key/shape and bit-exact value checks"
  )
}

# ---------------------------------------------------------------------
# hmswin: Swin Transformer (shifted-window attention)
# ---------------------------------------------------------------------

#' Swin Transformer: shifted-window attention
#'
#' Linearly embeds pixels to d_model, partitions the token grid into
#' non-overlapping window_size x window_size windows, runs attention
#' inside each window via \code{morie_geron_scaled_dot_product}. Odd
#' layers (0-based) cyclically shift the grid by window_size %/% 2
#' before partitioning and undo it after.
#'
#' @param image (H, W) or (H, W, C) array.
#' @param window_size Window side length.
#' @param n_layers Blocks.
#' @param d_model Token embedding width.
#' @param seed LCG seed.
#' @return list with Y, pooled, n_windows, window_tokens, shifted_layers,
#'   attention_pairs, full_attention_pairs, estimate, n, method.
#' @export
morie_geron_swin <- function(image, window_size, n_layers = 2, d_model = 4, seed = 0) {
  img <- as.array(image)
  if (length(dim(img)) == 2) dim(img) <- c(dim(img), 1)
  Hh <- dim(img)[1]
  Ww <- dim(img)[2]
  Cc <- dim(img)[3]
  w <- as.integer(window_size)
  d <- as.integer(d_model)
  L <- as.integer(n_layers)

  Emat <- matrix(.morie_w4d_lcg_vec(Cc * d, as.numeric(seed) + 1, scale = 0.1), nrow = Cc, ncol = d, byrow = TRUE)
  flat <- matrix(aperm(img, c(3, 2, 1)), ncol = Cc, byrow = TRUE) # row-major flatten over (H,W), channel last
  # rebuild row-major flatten explicitly: rows ordered by (h, w) with C as columns
  flat <- matrix(0.0, Hh * Ww, Cc)
  r <- 1
  for (i in seq_len(Hh)) {
    for (j in seq_len(Ww)) {
      flat[r, ] <- img[i, j, ]
      r <- r + 1
    }
  }
  Xt <- flat %*% Emat
  X <- array(0.0, dim = c(Hh, Ww, d))
  r <- 1
  for (i in seq_len(Hh)) {
    for (j in seq_len(Ww)) {
      X[i, j, ] <- Xt[r, ]
      r <- r + 1
    }
  }

  shift <- w %/% 2
  shifted_layers <- 0L
  roll2 <- function(arr, sh1, sh2) {
    d1 <- dim(arr)[1]
    d2 <- dim(arr)[2]
    idx1 <- ((seq_len(d1) - 1 - sh1) %% d1) + 1
    idx2 <- ((seq_len(d2) - 1 - sh2) %% d2) + 1
    arr[idx1, idx2, , drop = FALSE]
  }

  for (layer in 0:(L - 1)) {
    do_shift <- (layer %% 2 == 1) && shift > 0
    shifted_layers <- shifted_layers + as.integer(do_shift)
    Z <- if (do_shift) roll2(X, shift, shift) else X
    base <- as.numeric(seed) + 100 * (layer + 1)
    Wq <- matrix(.morie_w4d_lcg_vec(d * d, base + 1), d, d, byrow = TRUE)
    Wk <- matrix(.morie_w4d_lcg_vec(d * d, base + 2), d, d, byrow = TRUE)
    Wv <- matrix(.morie_w4d_lcg_vec(d * d, base + 3), d, d, byrow = TRUE)
    out <- array(0.0, dim = dim(Z))
    for (i0 in seq(1, Hh, by = w)) {
      for (j0 in seq(1, Ww, by = w)) {
        blk_arr <- Z[i0:(i0 + w - 1), j0:(j0 + w - 1), , drop = FALSE]
        blk <- matrix(0.0, w * w, d)
        rr <- 1
        for (ii in seq_len(w)) {
          for (jj in seq_len(w)) {
            blk[rr, ] <- blk_arr[ii, jj, ]
            rr <- rr + 1
          }
        }
        a <- morie_geron_scaled_dot_product(blk %*% Wq, blk %*% Wk, blk %*% Wv, d_k = d)
        Yb <- a$Y
        rr <- 1
        for (ii in seq_len(w)) {
          for (jj in seq_len(w)) {
            out[i0 + ii - 1, j0 + jj - 1, ] <- Yb[rr, ]
            rr <- rr + 1
          }
        }
      }
    }
    Z <- Z + out
    X <- if (do_shift) roll2(Z, -shift, -shift) else Z
  }

  n_windows <- (Hh %/% w) * (Ww %/% w)
  tokens_per_window <- w * w
  pairs <- n_windows * tokens_per_window * tokens_per_window

  Xflat <- matrix(0.0, Hh * Ww, d)
  r <- 1
  for (i in seq_len(Hh)) {
    for (j in seq_len(Ww)) {
      Xflat[r, ] <- X[i, j, ]
      r <- r + 1
    }
  }

  list(
    Y = X, pooled = colMeans(Xflat), n_windows = n_windows, window_tokens = tokens_per_window,
    shifted_layers = shifted_layers, attention_pairs = pairs, full_attention_pairs = (Hh * Ww)^2,
    estimate = pairs, n = Hh * Ww,
    method = "Swin: window-local scaled dot-product attention with alternating cyclic shift"
  )
}

# ---------------------------------------------------------------------
# hmsymd: symbolic differentiation (small CAS: parse/diff/simplify/eval)
# ---------------------------------------------------------------------
# Tree nodes are R lists: list(kind, ...). Binary op: list(op, left, right).
# num: list("num", value). var: list("var", name). neg: list("neg", child).
# call: list("call", fname, arg).

.morie_w4d_symd_funcs <- c("sin", "cos", "exp", "log", "tanh", "sqrt")

.morie_w4d_symd_tokenize <- function(src) {
  s <- as.character(src)
  chars <- strsplit(s, "")[[1]]
  n <- length(chars)
  out <- list()
  i <- 1
  while (i <= n) {
    c <- chars[i]
    if (grepl("\\s", c)) {
      i <- i + 1
    } else if (grepl("[0-9]", c) || (c == "." && i < n && grepl("[0-9]", chars[i + 1]))) {
      j <- i
      while (j <= n && grepl("[0-9.]", chars[j])) j <- j + 1
      out[[length(out) + 1]] <- list("num", as.numeric(paste(chars[i:(j - 1)], collapse = "")))
      i <- j
    } else if (grepl("[A-Za-z_]", c)) {
      j <- i
      while (j <= n && grepl("[A-Za-z0-9_]", chars[j])) j <- j + 1
      out[[length(out) + 1]] <- list("name", paste(chars[i:(j - 1)], collapse = ""))
      i <- j
    } else if (c %in% c("+", "-", "*", "/", "^", "(", ")", ",")) {
      out[[length(out) + 1]] <- list("op", c)
      i <- i + 1
    } else {
      stop(sprintf("parse: unexpected character %s", c))
    }
  }
  out
}

#' Parse an arithmetic expression string into an R tree
#' @param src Expression source string.
#' @return Nested list tree (see module header for node shapes).
#' @export
morie_geron_symd_parse <- function(src) {
  toks <- .morie_w4d_symd_tokenize(src)
  pos <- 1L
  ntoks <- length(toks)
  peek <- function() if (pos <= ntoks) toks[[pos]] else NULL
  eat <- function(kind, val = NULL) {
    t <- peek()
    if (is.null(t) || t[[1]] != kind || (!is.null(val) && t[[2]] != val)) stop("parse: unexpected token")
    pos <<- pos + 1L
    t
  }
  expr <- NULL
  term <- NULL
  unary <- NULL
  power <- NULL
  atom <- NULL
  expr <- function() {
    node <- term()
    while (!is.null(peek()) && peek()[[1]] == "op" && peek()[[2]] %in% c("+", "-")) {
      op <- eat("op")[[2]]
      node <- list(op, node, term())
    }
    node
  }
  term <- function() {
    node <- unary()
    while (!is.null(peek()) && peek()[[1]] == "op" && peek()[[2]] %in% c("*", "/")) {
      op <- eat("op")[[2]]
      node <- list(op, node, unary())
    }
    node
  }
  unary <- function() {
    t <- peek()
    if (!is.null(t) && t[[1]] == "op" && t[[2]] == "-") {
      eat("op", "-")
      return(list("neg", unary()))
    }
    power()
  }
  power <- function() {
    base <- atom()
    t <- peek()
    if (!is.null(t) && t[[1]] == "op" && t[[2]] == "^") {
      eat("op", "^")
      return(list("^", base, unary()))
    }
    base
  }
  atom <- function() {
    t <- peek()
    if (is.null(t)) stop("parse: unexpected end of expression")
    if (t[[1]] == "num") {
      eat("num")
      return(list("num", t[[2]]))
    }
    if (t[[1]] == "name") {
      eat("name")
      nt <- peek()
      if (!is.null(nt) && nt[[1]] == "op" && nt[[2]] == "(") {
        if (!(t[[2]] %in% .morie_w4d_symd_funcs)) stop(sprintf("parse: unknown function %s", t[[2]]))
        eat("op", "(")
        arg <- expr()
        eat("op", ")")
        return(list("call", t[[2]], arg))
      }
      return(list("var", t[[2]]))
    }
    if (t[[1]] == "op" && t[[2]] == "(") {
      eat("op", "(")
      node <- expr()
      eat("op", ")")
      return(node)
    }
    stop("parse: unexpected token")
  }
  node <- expr()
  if (pos != ntoks + 1) stop("parse: trailing input")
  node
}

.morie_w4d_symd_num <- function(v) list("num", as.numeric(v))
.morie_w4d_symd_eqnum <- function(t, v) length(t) == 2 && t[[1]] == "num" && isTRUE(all.equal(t[[2]], v, tolerance = 0))

.morie_w4d_symd_simplify <- function(t) {
  k <- t[[1]]
  if (k %in% c("num", "var")) {
    return(t)
  }
  if (k == "neg") {
    a <- .morie_w4d_symd_simplify(t[[2]])
    if (a[[1]] == "num") {
      return(.morie_w4d_symd_num(-a[[2]]))
    }
    return(list("neg", a))
  }
  if (k == "call") {
    return(list("call", t[[2]], .morie_w4d_symd_simplify(t[[3]])))
  }
  a <- .morie_w4d_symd_simplify(t[[2]])
  b <- .morie_w4d_symd_simplify(t[[3]])
  op <- k
  if (a[[1]] == "num" && b[[1]] == "num") {
    if (op == "+") {
      return(.morie_w4d_symd_num(a[[2]] + b[[2]]))
    }
    if (op == "-") {
      return(.morie_w4d_symd_num(a[[2]] - b[[2]]))
    }
    if (op == "*") {
      return(.morie_w4d_symd_num(a[[2]] * b[[2]]))
    }
    if (op == "/" && b[[2]] != 0) {
      return(.morie_w4d_symd_num(a[[2]] / b[[2]]))
    }
    if (op == "^") {
      return(.morie_w4d_symd_num(a[[2]]^b[[2]]))
    }
  }
  if (op == "+") {
    if (.morie_w4d_symd_eqnum(a, 0)) {
      return(b)
    }
    if (.morie_w4d_symd_eqnum(b, 0)) {
      return(a)
    }
  }
  if (op == "-" && .morie_w4d_symd_eqnum(b, 0)) {
    return(a)
  }
  if (op == "*") {
    if (.morie_w4d_symd_eqnum(a, 0) || .morie_w4d_symd_eqnum(b, 0)) {
      return(.morie_w4d_symd_num(0))
    }
    if (.morie_w4d_symd_eqnum(a, 1)) {
      return(b)
    }
    if (.morie_w4d_symd_eqnum(b, 1)) {
      return(a)
    }
  }
  if (op == "/") {
    if (.morie_w4d_symd_eqnum(a, 0)) {
      return(.morie_w4d_symd_num(0))
    }
    if (.morie_w4d_symd_eqnum(b, 1)) {
      return(a)
    }
  }
  if (op == "^") {
    if (.morie_w4d_symd_eqnum(b, 1)) {
      return(a)
    }
    if (.morie_w4d_symd_eqnum(b, 0)) {
      return(.morie_w4d_symd_num(1))
    }
  }
  list(op, a, b)
}

.morie_w4d_symd_diff <- function(t, var) {
  kind <- t[[1]]
  if (kind == "num") {
    return(.morie_w4d_symd_num(0))
  }
  if (kind == "var") {
    return(if (t[[2]] == var) .morie_w4d_symd_num(1) else .morie_w4d_symd_num(0))
  }
  if (kind == "neg") {
    return(list("neg", .morie_w4d_symd_diff(t[[2]], var)))
  }
  if (kind == "+") {
    return(list("+", .morie_w4d_symd_diff(t[[2]], var), .morie_w4d_symd_diff(t[[3]], var)))
  }
  if (kind == "-") {
    return(list("-", .morie_w4d_symd_diff(t[[2]], var), .morie_w4d_symd_diff(t[[3]], var)))
  }
  if (kind == "*") {
    return(list(
      "+", list("*", .morie_w4d_symd_diff(t[[2]], var), t[[3]]),
      list("*", t[[2]], .morie_w4d_symd_diff(t[[3]], var))
    ))
  }
  if (kind == "/") {
    return(list(
      "/",
      list("-", list("*", .morie_w4d_symd_diff(t[[2]], var), t[[3]]), list("*", t[[2]], .morie_w4d_symd_diff(t[[3]], var))),
      list("^", t[[3]], .morie_w4d_symd_num(2))
    ))
  }
  if (kind == "^") {
    u <- t[[2]]
    v <- t[[3]]
    if (v[[1]] == "num") {
      return(list("*", list("*", v, list("^", u, .morie_w4d_symd_num(v[[2]] - 1))), .morie_w4d_symd_diff(u, var)))
    }
    if (u[[1]] == "num") {
      return(list("*", list("*", list("^", u, v), list("call", "log", u)), .morie_w4d_symd_diff(v, var)))
    }
    inner <- list("*", v, list("call", "log", u))
    return(list("*", list("^", u, v), .morie_w4d_symd_diff(inner, var)))
  }
  if (kind == "call") {
    f <- t[[2]]
    u <- t[[3]]
    du <- .morie_w4d_symd_diff(u, var)
    if (f == "sin") {
      return(list("*", list("call", "cos", u), du))
    }
    if (f == "cos") {
      return(list("neg", list("*", list("call", "sin", u), du)))
    }
    if (f == "exp") {
      return(list("*", list("call", "exp", u), du))
    }
    if (f == "log") {
      return(list("/", du, u))
    }
    if (f == "tanh") {
      return(list("*", list("-", .morie_w4d_symd_num(1), list("^", list("call", "tanh", u), .morie_w4d_symd_num(2))), du))
    }
    if (f == "sqrt") {
      return(list("/", du, list("*", .morie_w4d_symd_num(2), list("call", "sqrt", u))))
    }
    stop(sprintf("geron_symbolic_diff: no derivative rule for %s", f))
  }
  stop("geron_symbolic_diff: unknown node")
}

#' Render a parsed tree back to infix source
#' @param t Tree node (from morie_geron_symd_parse).
#' @return Character scalar.
#' @export
morie_geron_symd_to_string <- function(t) {
  k <- t[[1]]
  if (k == "num") {
    v <- t[[2]]
    return(if (v == floor(v)) as.character(as.integer(v)) else as.character(v))
  }
  if (k == "var") {
    return(t[[2]])
  }
  if (k == "neg") {
    return(paste0("-", morie_geron_symd_to_string(t[[2]])))
  }
  if (k == "call") {
    return(paste0(t[[2]], "(", morie_geron_symd_to_string(t[[3]]), ")"))
  }
  left <- morie_geron_symd_to_string(t[[2]])
  right <- morie_geron_symd_to_string(t[[3]])
  if (k %in% c("*", "/", "^")) {
    if (t[[2]][[1]] %in% c("+", "-")) left <- paste0("(", left, ")")
    if (t[[3]][[1]] %in% c("+", "-", "*", "/")) right <- paste0("(", right, ")")
  }
  paste(left, k, right)
}

#' Evaluate a parsed tree against an environment (no eval/parse of R code)
#' @param t Tree node.
#' @param env Named list/numeric vector of variable values.
#' @return Numeric scalar.
#' @export
morie_geron_symd_evaluate <- function(t, env) {
  k <- t[[1]]
  if (k == "num") {
    return(as.numeric(t[[2]]))
  }
  if (k == "var") {
    return(as.numeric(env[[t[[2]]]]))
  }
  if (k == "neg") {
    return(-morie_geron_symd_evaluate(t[[2]], env))
  }
  if (k == "call") {
    a <- morie_geron_symd_evaluate(t[[3]], env)
    fn <- switch(t[[2]],
      sin = sin,
      cos = cos,
      exp = exp,
      log = log,
      tanh = tanh,
      sqrt = sqrt
    )
    return(fn(a))
  }
  a <- morie_geron_symd_evaluate(t[[2]], env)
  b <- morie_geron_symd_evaluate(t[[3]], env)
  switch(k,
    "+" = a + b,
    "-" = a - b,
    "*" = a * b,
    "/" = a / b,
    "^" = a^b
  )
}

.morie_w4d_symd_count <- function(t) {
  if (t[[1]] %in% c("num", "var")) {
    return(1L)
  }
  if (t[[1]] == "neg") {
    return(1L + .morie_w4d_symd_count(t[[2]]))
  }
  if (t[[1]] == "call") {
    return(1L + .morie_w4d_symd_count(t[[3]]))
  }
  1L + .morie_w4d_symd_count(t[[2]]) + .morie_w4d_symd_count(t[[3]])
}

#' Symbolic differentiation: manipulate algebraic expressions analytically
#'
#' Parses source into a tree, applies derivative rules (sum, product,
#' quotient, power, chain, sin/cos/exp/log/tanh/sqrt), constant-folds.
#' When \code{at} is given, checks against a central finite difference.
#'
#' @param expr Expression source string or already-parsed tree.
#' @param var Variable to differentiate with respect to.
#' @param at Optional named list/vector giving a point to evaluate at.
#' @return list with derivative, tree, expression, value, numeric_check,
#'   error, nodes, var, estimate, n, method.
#' @export
morie_geron_symbolic_diff <- function(expr, var = "x", at = NULL) {
  tree <- if (is.character(expr)) morie_geron_symd_parse(expr) else expr
  v <- as.character(var)

  d <- .morie_w4d_symd_simplify(.morie_w4d_symd_diff(tree, v))
  text <- morie_geron_symd_to_string(d)

  value <- NULL
  numeric_ <- NULL
  err <- NULL
  if (!is.null(at)) {
    env <- as.list(at)
    value <- as.numeric(morie_geron_symd_evaluate(d, env))
    h <- 1e-5 * max(1.0, abs(as.numeric(env[[v]])))
    up <- env
    dn <- env
    up[[v]] <- env[[v]] + h
    dn[[v]] <- env[[v]] - h
    numeric_ <- (morie_geron_symd_evaluate(tree, up) - morie_geron_symd_evaluate(tree, dn)) / (2 * h)
    err <- abs(value - numeric_)
  }

  list(
    derivative = text, tree = d, expression = morie_geron_symd_to_string(tree), value = value,
    numeric_check = numeric_, error = err, nodes = .morie_w4d_symd_count(d), var = v,
    estimate = if (!is.null(value)) value else as.numeric(.morie_w4d_symd_count(d)),
    n = .morie_w4d_symd_count(tree),
    method = "Rule-based differentiation of the parsed expression tree with constant folding"
  )
}

# ---------------------------------------------------------------------
# hmt5: T5 span corruption + text-to-text framing
# ---------------------------------------------------------------------

.morie_w4d_tokens <- function(x) {
  if (is.character(x) && length(x) == 1) strsplit(x, "\\s+")[[1]] else as.character(x)
}

#' T5 span-corruption masking (encoder input / decoder target / spans)
#' @param tokens Character vector of tokens.
#' @param noise_density Fraction of tokens to mask, in (0,1).
#' @param mean_span Mean span length (>=1).
#' @param seed LCG seed for span placement.
#' @return list(inputs, target, spans) where spans is a list of c(start0, length) pairs (0-based start).
#' @export
morie_geron_span_corrupt <- function(tokens, noise_density = 0.15, mean_span = 3, seed = 0) {
  n <- length(tokens)
  dens <- as.numeric(noise_density)
  span <- as.integer(mean_span)
  n_noise <- max(1, round(n * dens))
  n_spans <- max(1, round(n_noise / span))

  s <- as.numeric(seed) %% 2^32
  chosen <- list()
  used <- integer(0)
  guard <- 0
  while (length(chosen) < n_spans && guard < 100 * n_spans) {
    guard <- guard + 1
    s <- (1664525 * s + 1013904223) %% 2^32
    start <- as.integer(((s + 0.5) / 2^32) * n) # 0-based
    len_so_far <- if (length(chosen) > 0) sum(sapply(chosen, `[`, 2)) else 0
    length_ <- max(1, min(span, n - start, n_noise - len_so_far))
    rng_idx <- start:(start + length_ - 1)
    if (length_ < 1 || any(rng_idx %in% used)) next
    if (start + length_ > n || (start == 0 && length_ == n)) next
    chosen[[length(chosen) + 1]] <- c(start, length_)
    used <- c(used, rng_idx)
    if (sum(sapply(chosen, `[`, 2)) >= n_noise) break
  }
  ord <- order(sapply(chosen, `[`, 1))
  chosen <- chosen[ord]

  inputs <- character(0)
  target <- character(0)
  i <- 0
  k <- 0
  for (sp in chosen) {
    start <- sp[1]
    length_ <- sp[2]
    if (start > i) inputs <- c(inputs, tokens[(i + 1):start])
    sentinel <- sprintf("<extra_id_%d>", k)
    inputs <- c(inputs, sentinel)
    target <- c(target, sentinel, tokens[(start + 1):(start + length_)])
    i <- start + length_
    k <- k + 1
  }
  if (i < n) inputs <- c(inputs, tokens[(i + 1):n])
  target <- c(target, sprintf("<extra_id_%d>", k))
  list(inputs = inputs, target = target, spans = chosen)
}

#' Rebuild the original sequence from a T5-corrupted input and its target
#' @param inputs Character vector (encoder input with sentinels).
#' @param target Character vector (decoder target with sentinels).
#' @return Character vector, the restored sequence.
#' @export
morie_geron_t5_restore <- function(inputs, target) {
  spans <- list()
  cur <- NULL
  for (t in target) {
    if (startsWith(t, "<extra_id_")) {
      cur <- t
      spans[[cur]] <- character(0)
    } else if (!is.null(cur)) spans[[cur]] <- c(spans[[cur]], t)
  }
  out <- character(0)
  for (t in inputs) {
    if (startsWith(t, "<extra_id_")) {
      out <- c(out, spans[[t]])
    } else {
      out <- c(out, t)
    }
  }
  out
}

#' T5: text-to-text transfer transformer (encoder-decoder)
#'
#' Span-corruption pretraining (drop contiguous spans, replace with
#' numbered sentinels; target is the dropped spans) plus text-to-text
#' framing of a supervised pair as \code{(prefix + source) -> target}.
#'
#' @param src Source sequence (string, whitespace-split, or char vector).
#' @param tgt Optional target sequence for text-to-text framing.
#' @param noise_density,mean_span,seed,prefix As in the Python original.
#' @return list with encoder_input, decoder_target, spans, restored, lossless,
#'   n_masked, sentinels, text_to_text, estimate, n, method.
#' @export
morie_geron_t5 <- function(src, tgt = NULL, noise_density = 0.15, mean_span = 3, seed = 0,
                           prefix = "translate:") {
  toks <- .morie_w4d_tokens(src)
  sc <- morie_geron_span_corrupt(toks, noise_density, mean_span, seed)
  enc <- sc$inputs
  dec <- sc$target
  spans <- sc$spans
  rebuilt <- morie_geron_t5_restore(enc, dec)
  lossless <- identical(rebuilt, toks)

  t2t <- NULL
  if (!is.null(tgt)) {
    tgt_toks <- .morie_w4d_tokens(tgt)
    t2t <- c(trimws(paste(prefix, paste(toks, collapse = " "))), paste(tgt_toks, collapse = " "))
  }

  n_masked <- sum(sapply(spans, `[`, 2))

  list(
    encoder_input = enc, decoder_target = dec, spans = spans, restored = rebuilt, lossless = lossless,
    n_masked = n_masked, sentinels = length(spans), text_to_text = t2t,
    estimate = n_masked / length(toks), n = length(toks),
    method = "T5 span corruption with sentinel tokens, verified by exact reconstruction; text-to-text framing"
  )
}

# ---------------------------------------------------------------------
# hmtd3: TD3 (twin delayed DDPG, tabular)
# ---------------------------------------------------------------------

#' Twin delayed DDPG (TD3): two critics + delayed policy updates
#'
#' target = r + gamma * min(Q1_target, Q2_target)(s', a_tilde). Target
#' policy smoothing perturbs the target action with probability noise;
#' policy/target refresh (argmax + Polyak averaging at rate tau) happens
#' only every policy_delay epochs.
#'
#' @param env List with reset(), step(a), n_states, n_actions.
#' @param policy Optional initial deterministic policy (one 0-based action per state).
#' @param Q1,Q2 Optional initial (n_states, n_actions) critics; default zeros.
#' @param epochs,lr,gamma,steps,policy_delay,tau,noise,seed As in Python original.
#' @return list with policy, Q1, Q2, Q1_target, Q2_target, returns,
#'   overestimation_gap, policy_updates, estimate, n, method.
#' @export
morie_geron_td3 <- function(env, policy = NULL, Q1 = NULL, Q2 = NULL, epochs = 30, lr = 0.5,
                            gamma = 0.9, steps = 20, policy_delay = 2, tau = 0.5, noise = 0.2, seed = 0) {
  n_s <- as.integer(env$n_states)
  n_a <- as.integer(env$n_actions)
  E <- as.integer(epochs)
  Tt <- as.integer(steps)
  step_size <- as.numeric(lr)
  g <- as.numeric(gamma)
  tau_f <- as.numeric(tau)
  eps <- as.numeric(noise)
  delay <- as.integer(policy_delay)

  mu <- if (is.null(policy)) rep(0L, n_s) else as.integer(policy)
  q1 <- if (is.null(Q1)) matrix(0.0, n_s, n_a) else as.matrix(Q1)
  q2 <- if (is.null(Q2)) matrix(0.0, n_s, n_a) else as.matrix(Q2)
  q1_t <- q1
  q2_t <- q2
  mu_t <- mu

  rng <- as.numeric(seed) %% 2^32
  u_draw <- function() {
    rng <<- (1664525 * rng + 1013904223) %% 2^32
    (rng + 0.5) / 2^32
  }

  returns <- numeric(E)
  gaps <- numeric(0)
  n_policy_updates <- 0L
  s <- as.integer(env$reset())
  for (ep in seq_len(E)) {
    batch <- vector("list", Tt)
    total <- 0.0
    for (t in seq_len(Tt)) {
      u1 <- u_draw()
      a <- if (u1 < 0.3) as.integer(u_draw() * n_a) else mu[s + 1]
      a <- min(max(a, 0L), n_a - 1L)
      step_out <- env$step(a)
      s2 <- as.integer(step_out[[1]])
      rew <- as.numeric(step_out[[2]])
      done <- isTRUE(step_out[[3]])
      batch[[t]] <- list(s = s, a = a, r = rew, s2 = s2, done = done)
      total <- total + rew
      s <- if (done) as.integer(env$reset()) else s2
    }
    for (t in seq_len(Tt)) {
      b <- batch[[t]]
      a_t <- mu_t[b$s2 + 1]
      if (u_draw() < eps) {
        a_t <- min(max(a_t + (if (u_draw() < 0.5) 1L else -1L), 0L), n_a - 1L)
      }
      twin <- min(q1_t[b$s2 + 1, a_t + 1], q2_t[b$s2 + 1, a_t + 1])
      single <- q1_t[b$s2 + 1, a_t + 1]
      gaps <- c(gaps, single - twin)
      target <- b$r + (if (b$done) 0.0 else g * twin)
      q1[b$s + 1, b$a + 1] <- q1[b$s + 1, b$a + 1] + step_size * (target - q1[b$s + 1, b$a + 1])
      q2[b$s + 1, b$a + 1] <- q2[b$s + 1, b$a + 1] + step_size * (target - q2[b$s + 1, b$a + 1])
    }
    if ((ep %% delay) == 0) {
      mu <- apply(q1, 1, which.max) - 1L
      q1_t <- (1 - tau_f) * q1_t + tau_f * q1
      q2_t <- (1 - tau_f) * q2_t + tau_f * q2
      mu_t <- mu
      n_policy_updates <- n_policy_updates + 1L
    }
    returns[ep] <- total
  }

  gap <- if (length(gaps) > 0) mean(gaps) else 0.0

  list(
    policy = mu, Q1 = q1, Q2 = q2, Q1_target = q1_t, Q2_target = q2_t, returns = returns,
    overestimation_gap = gap, policy_updates = n_policy_updates, estimate = returns[E], n = E * Tt,
    method = "Tabular TD3: twin critics, clipped target smoothing, delayed Polyak policy/target updates"
  )
}

# ---------------------------------------------------------------------
# hmtfl: transfer learning (partial freeze fine-tuning)
# ---------------------------------------------------------------------

#' Transfer learning: reuse pretrained model, fine-tune on new task
#'
#' The first n_frozen weight matrices are used forward-only and never
#' updated; the trainable tail is optimised by backprop through a tanh
#' stack (final layer linear).
#'
#' @param pretrained_model List of weight matrices W_0 (d_in,h1), W_1 (h1,h2), ...
#' @param X New-task inputs (n, d_in).
#' @param y New-task targets (n,) or (n, d_out).
#' @param n_frozen Leading layers to freeze.
#' @param epochs,lr As in Python original.
#' @return list with weights, frozen, loss_curve, initial_loss, final_loss,
#'   trainable_params, total_params, estimate, n, method.
#' @export
morie_geron_transfer_learning <- function(pretrained_model, X, y, n_frozen = 1, epochs = 200, lr = 0.05) {
  Ws <- lapply(pretrained_model, as.matrix)
  A <- as.matrix(X)
  Tm <- as.matrix(y)
  L <- length(Ws)
  nf <- as.integer(n_frozen)
  E <- as.integer(epochs)
  step <- as.numeric(lr)
  n <- nrow(Tm)
  d_out <- ncol(Tm)

  forward <- function(Ws) {
    acts <- list(A)
    H <- A
    for (i in seq_len(L)) {
      z <- H %*% Ws[[i]]
      H <- if (i < L) tanh(z) else z
      acts[[i + 1]] <- H
    }
    acts
  }
  loss_of <- function(acts) mean((acts[[L + 1]] - Tm)^2)

  acts <- forward(Ws)
  losses <- loss_of(acts)
  for (e in seq_len(E)) {
    acts <- forward(Ws)
    gm <- 2.0 * (acts[[L + 1]] - Tm) / (n * d_out)
    grads <- vector("list", L)
    for (i in L:1) {
      if (i < L) gm <- gm * (1.0 - acts[[i + 1]] * acts[[i + 1]])
      grads[[i]] <- t(acts[[i]]) %*% gm
      gm <- gm %*% t(Ws[[i]])
    }
    for (i in (nf + 1):L) Ws[[i]] <- Ws[[i]] - step * grads[[i]]
    losses <- c(losses, loss_of(forward(Ws)))
  }

  total <- sum(sapply(Ws, length))
  trainable <- sum(sapply(Ws[(nf + 1):L], length))

  list(
    weights = Ws, frozen = nf, loss_curve = losses, initial_loss = losses[1], final_loss = losses[length(losses)],
    trainable_params = trainable, total_params = total, estimate = losses[length(losses)], n = n,
    method = "Fine-tuning the unfrozen tail by backpropagation; frozen layers forward-only"
  )
}

# ---------------------------------------------------------------------
# hmtsc: TorchScript-style tracing (op-sequence graph capture/replay)
# ---------------------------------------------------------------------

.morie_w4d_apply_op <- function(op, x) {
  if (is.list(op) && !is.null(op$kind)) {
    kind <- op$kind
    if (kind == "linear") {
      return(x %*% as.matrix(op$param))
    }
    if (kind == "bias") {
      return(sweep(x, 2, as.numeric(op$param), "+"))
    }
    if (kind == "relu") {
      return(pmax(x, 0.0))
    }
    if (kind == "tanh") {
      return(tanh(x))
    }
    if (kind == "sigmoid") {
      xc <- pmin(pmax(x, -700), 700)
      return(1.0 / (1.0 + exp(-xc)))
    }
  }
  if (is.function(op)) {
    return(op(x))
  }
  stop("apply_op: unknown op")
}

#' Trace a typed graph by running model ops on example_inputs once
#'
#' Each op is a list(kind=..., param=...) for "linear"/"bias", list(kind=...)
#' for "relu"/"tanh"/"sigmoid", or a plain R function. Records observed
#' input/output shapes.
#'
#' @param model List of ops.
#' @param example_inputs Matrix/vector input.
#' @return list(graph, output) where graph is a list of node lists
#'   (index, kind, in_shape, out_shape, op).
#' @export
morie_geron_trace <- function(model, example_inputs) {
  x <- as.matrix(example_inputs)
  graph <- list()
  for (i in seq_along(model)) {
    op <- model[[i]]
    in_shape <- dim(x)
    y <- .morie_w4d_apply_op(op, x)
    kind <- if (is.list(op) && !is.null(op$kind)) op$kind else "callable"
    graph[[i]] <- list(index = i - 1L, kind = kind, in_shape = in_shape, out_shape = dim(y), op = op)
    x <- y
  }
  list(graph = graph, output = x)
}

#' Re-execute a traced graph, enforcing recorded shapes (ncol must match)
#' @param graph List of node lists from \code{morie_geron_trace}.
#' @param x Matrix/vector input.
#' @return Matrix output.
#' @export
morie_geron_run_graph <- function(graph, x) {
  a <- as.matrix(x)
  for (node in graph) {
    if (ncol(a) != node$in_shape[2]) stop("run_graph: traced graph is only valid for the shapes it was recorded with")
    a <- .morie_w4d_apply_op(node$op, a)
  }
  a
}

#' TorchScript: statically-typed graph representation of PyTorch models
#'
#' Traces an op sequence into a static graph (shapes frozen at each
#' node), then verifies by replay.
#'
#' @param model List of ops (see \code{morie_geron_trace}).
#' @param example_inputs Representative input.
#' @return list with graph, output, replay, max_diff, n_nodes, shapes, estimate, n, method.
#' @export
morie_geron_torchscript <- function(model, example_inputs) {
  tr <- morie_geron_trace(model, example_inputs)
  graph <- tr$graph
  out <- tr$output
  replay <- morie_geron_run_graph(graph, example_inputs)
  diff <- max(abs(replay - out))

  list(
    graph = graph, output = out, replay = replay, max_diff = diff, n_nodes = length(graph),
    shapes = lapply(graph, function(g) list(g$in_shape, g$out_shape)), estimate = diff,
    n = nrow(as.matrix(example_inputs)),
    method = "Shape-specialised tracing of an op sequence into a static graph, verified by replay"
  )
}

# ---------------------------------------------------------------------
# hmtfm: Transformer encoder stack (Vaswani et al. 2017)
# ---------------------------------------------------------------------

.morie_w4d_layernorm <- function(x, eps = 1e-5) {
  mu <- rowMeans(x)
  sd <- sqrt(apply(x, 1, .morie_gr_pvar) + eps)
  (x - mu) / sd
}

#' Parameter count of a transformer encoder stack
#' @param d_model,d_ff,n_layers Integers.
#' @return Integer parameter count.
#' @export
morie_geron_encoder_params <- function(d_model, d_ff, n_layers) {
  per <- 4 * d_model * d_model + d_model * d_ff + d_ff + d_ff * d_model + d_model + 4 * d_model
  as.integer(per * n_layers)
}

#' Transformer architecture (Vaswani et al. 2017)
#'
#' Multi-head attention (heads via \code{morie_geron_scaled_dot_product}),
#' residual + layer norm, position-wise feedforward, residual + layer norm.
#'
#' @param X Input sequence (T, d_model).
#' @param n_heads,d_model,n_layers,d_ff,seed,mask As in Python original.
#' @return list with Y, attention, total_params, d_model, d_ff, n_heads,
#'   n_layers, estimate, n, method.
#' @export
morie_geron_transformer <- function(X, n_heads = 2, d_model = NULL, n_layers = 2, d_ff = NULL,
                                    seed = 0, mask = NULL) {
  Xa <- as.matrix(X)
  d <- if (is.null(d_model)) ncol(Xa) else as.integer(d_model)
  h <- as.integer(n_heads)
  L <- as.integer(n_layers)
  ff <- if (is.null(d_ff)) 4 * d else as.integer(d_ff)
  dh <- d %/% h

  H <- Xa
  attn_all <- vector("list", L)
  for (layer in 0:(L - 1)) {
    base <- as.numeric(seed) + 1000 * layer
    Wq <- matrix(.morie_w4d_lcg_vec(d * d, base + 1, 0.1), d, d, byrow = TRUE)
    Wk <- matrix(.morie_w4d_lcg_vec(d * d, base + 2, 0.1), d, d, byrow = TRUE)
    Wv <- matrix(.morie_w4d_lcg_vec(d * d, base + 3, 0.1), d, d, byrow = TRUE)
    Wo <- matrix(.morie_w4d_lcg_vec(d * d, base + 4, 0.1), d, d, byrow = TRUE)
    W1 <- matrix(.morie_w4d_lcg_vec(d * ff, base + 5, 0.1), d, ff, byrow = TRUE)
    W2 <- matrix(.morie_w4d_lcg_vec(ff * d, base + 6, 0.1), ff, d, byrow = TRUE)
    Q <- H %*% Wq
    K <- H %*% Wk
    V <- H %*% Wv
    out <- matrix(0.0, nrow(Q), ncol(Q))
    heads <- array(0.0, dim = c(h, nrow(H), nrow(H)))
    for (j in 0:(h - 1)) {
      sl <- (j * dh + 1):((j + 1) * dh)
      a <- morie_geron_scaled_dot_product(Q[, sl, drop = FALSE], K[, sl, drop = FALSE], V[, sl, drop = FALSE],
        d_k = dh, mask = mask
      )
      out[, sl] <- a$Y
      heads[j + 1, , ] <- a$attention
    }
    attn_all[[layer + 1]] <- heads
    H <- .morie_w4d_layernorm(H + out %*% Wo)
    H <- .morie_w4d_layernorm(H + pmax(H %*% W1, 0.0) %*% W2)
  }

  total <- morie_geron_encoder_params(d, ff, L)

  list(
    Y = H, attention = attn_all, total_params = total, d_model = d, d_ff = ff, n_heads = h, n_layers = L,
    estimate = as.numeric(total), n = nrow(Xa),
    method = "Post-norm transformer encoder stack with exact parameter accounting"
  )
}

# ---------------------------------------------------------------------
# hmtpp: tensor parallelism (column / row sharding)
# ---------------------------------------------------------------------

#' Tensor parallelism: split individual tensors across devices
#'
#' Column-parallel: shard W by columns, y_i = x W_i, concatenate (no
#' communication). Row-parallel: shard W by rows and x by columns to
#' match, y = sum_i x_i W_i (all-reduce).
#'
#' @param model A weight matrix or list of weight matrices.
#' @param n_devices Number of devices.
#' @param x Optional input activations; default ones.
#' @param scheme "column" or "row".
#' @return list with output, reference, max_diff, shards, params_per_device,
#'   params_total, comm_elements, all_reduce, scheme, estimate, n, method.
#' @export
morie_geron_tensor_parallelism <- function(model, n_devices = 2, x = NULL, scheme = "column") {
  mats <- if (is.list(model) && !is.matrix(model)) lapply(model, as.matrix) else list(as.matrix(model))
  N <- as.integer(n_devices)
  sch <- tolower(scheme)

  a <- if (is.null(x)) matrix(1.0, 1, nrow(mats[[1]])) else as.matrix(x)

  ref <- a
  for (w in mats) ref <- ref %*% w

  split_cols <- function(w, n) {
    cw <- ncol(w) %/% n
    lapply(0:(n - 1), function(i) w[, (i * cw + 1):((i + 1) * cw), drop = FALSE])
  }
  split_rows <- function(w, n) {
    rw <- nrow(w) %/% n
    lapply(0:(n - 1), function(i) w[(i * rw + 1):((i + 1) * rw), , drop = FALSE])
  }

  shards_all <- list()
  comm <- 0
  hmat <- a
  for (w in mats) {
    if (sch == "column") {
      shards <- split_cols(w, N)
      shards_all[[length(shards_all) + 1]] <- shards
      hmat <- do.call(cbind, lapply(shards, function(s) hmat %*% s))
    } else {
      shards <- split_rows(w, N)
      shards_all[[length(shards_all) + 1]] <- shards
      x_sh <- split_cols(hmat, N)
      partials <- Map(function(xi, si) xi %*% si, x_sh, shards)
      hmat <- Reduce(`+`, partials)
      comm <- comm + N * length(partials[[1]])
    }
  }

  diff <- max(abs(hmat - ref))
  per_dev <- sum(sapply(shards_all, function(s) length(s[[1]])))

  list(
    output = hmat, reference = ref, max_diff = diff, shards = shards_all, params_per_device = per_dev,
    params_total = sum(sapply(mats, length)), comm_elements = comm, all_reduce = (sch == "row" && N > 1),
    scheme = sch, estimate = diff, n = nrow(a),
    method = paste0(sch, "-parallel sharding across ", N, " devices, verified against the unsharded computation")
  )
}

# ---------------------------------------------------------------------
# hmtrlf: TRL fine-tuning (SFT delegate / DPO / PPO)
# ---------------------------------------------------------------------

.morie_w4d_sigmoid_stable <- function(z) {
  out <- numeric(length(z))
  pos <- z >= 0
  out[pos] <- 1.0 / (1.0 + exp(-z[pos]))
  e <- exp(z[!pos])
  out[!pos] <- e / (1.0 + e)
  out
}

#' Fine-tuning via TRL (Transformer Reinforcement Learning) library
#'
#' "sft" delegates to \code{morie_geron_sft}. "dpo": Direct Preference
#' Optimisation, loss = -log sigmoid(beta*(Delta_policy - Delta_ref)).
#' "ppo": clipped surrogate min(rA, clip(r,1-eps,1+eps)A).
#'
#' @param model Optional initial parameters; zeros by default.
#' @param dataset Shape depends on method (see Python docstring).
#' @param method "sft", "dpo", or "ppo".
#' @param epochs,lr,beta,clip_eps,theta_ref As in Python original.
#' @return list (shape depends on method) with theta, loss, loss_curve, ..., estimate, n, method.
#' @export
morie_geron_trl_finetune <- function(model = NULL, dataset, method = "sft", epochs = 200, lr = 0.1,
                                     beta = 0.1, clip_eps = 0.2, theta_ref = NULL) {
  m <- tolower(method)
  data <- dataset
  E <- as.integer(epochs)
  step <- as.numeric(lr)

  if (m == "sft") {
    inner <- morie_geron_sft(model, data, epochs = E, lr = step)
    return(list(
      theta = inner$W, loss = inner$loss, loss_curve = inner$loss_curve, accuracy = inner$accuracy,
      trainer = "sft", estimate = inner$loss, n = inner$n, method = "TRL SFT trainer (delegated to hmsft)"
    ))
  }

  if (m == "dpo") {
    b <- as.numeric(beta)
    chosen <- lapply(data, function(it) as.numeric(it[[1]]))
    rejected <- lapply(data, function(it) as.numeric(it[[2]]))
    C <- do.call(rbind, chosen)
    R <- do.call(rbind, rejected)
    d <- ncol(C)
    theta <- if (is.null(model)) numeric(d) else as.numeric(model)
    ref <- if (is.null(theta_ref)) numeric(d) else as.numeric(theta_ref)
    D <- C - R
    ref_delta <- as.numeric(D %*% ref)
    losses <- numeric(0)
    for (e in seq_len(E)) {
      z <- b * (as.numeric(D %*% theta) - ref_delta)
      losses <- c(losses, mean(-log(pmax(.morie_w4d_sigmoid_stable(z), .Machine$double.xmin))))
      gm <- -(.morie_w4d_sigmoid_stable(-z) * b) * D
      theta <- theta - step * colMeans(gm)
    }
    z <- b * (as.numeric(D %*% theta) - ref_delta)
    losses <- c(losses, mean(-log(pmax(.morie_w4d_sigmoid_stable(z), .Machine$double.xmin))))
    margin <- mean(as.numeric(D %*% theta))
    return(list(
      theta = theta, loss = losses[length(losses)], loss_curve = losses, margin = margin, beta = b,
      trainer = "dpo", estimate = losses[length(losses)], n = nrow(C),
      method = "DPO: -log sigmoid(beta * (policy margin - reference margin)) by gradient descent"
    ))
  }

  eps <- as.numeric(clip_eps)
  feats <- lapply(data, function(it) as.numeric(it[[1]]))
  Xp <- do.call(rbind, feats)
  lo <- as.numeric(sapply(data, `[[`, 2))
  Av <- as.numeric(sapply(data, `[[`, 3))
  d <- ncol(Xp)
  theta <- if (is.null(model)) numeric(d) else as.numeric(model)

  losses <- numeric(0)
  clipped <- 0.0
  ratio <- NULL
  for (e in seq_len(E)) {
    ratio <- exp(pmin(pmax(as.numeric(Xp %*% theta) - lo, -30), 30))
    unclipped <- ratio * Av
    clip_r <- pmin(pmax(ratio, 1 - eps), 1 + eps)
    obj <- pmin(unclipped, clip_r * Av)
    losses <- c(losses, -mean(obj))
    active <- unclipped <= clip_r * Av
    clipped <- mean(!active)
    gm <- ifelse(active, ratio * Av, 0.0) * Xp
    theta <- theta + step * colMeans(gm)
  }
  ratio <- exp(pmin(pmax(as.numeric(Xp %*% theta) - lo, -30), 30))
  obj <- pmin(ratio * Av, pmin(pmax(ratio, 1 - eps), 1 + eps) * Av)
  losses <- c(losses, -mean(obj))

  list(
    theta = theta, loss = losses[length(losses)], loss_curve = losses, ratio = ratio,
    clipped_fraction = clipped, clip_eps = eps, trainer = "ppo", estimate = losses[length(losses)], n = nrow(Xp),
    method = "PPO clipped surrogate min(rA, clip(r, 1-eps, 1+eps)A) by gradient ascent"
  )
}

# ---------------------------------------------------------------------
# hmtcmp: torch.compile (linear-chain fusion + matmul-order DP)
# ---------------------------------------------------------------------

#' Optimal parenthesisation of a matrix chain (matrix-chain DP)
#' @param dims Chain dimensions; matrix i has shape (dims\[i\], dims\[i+1\]).
#' @return list(cost, split) with cost the min multiplication count.
#' @export
morie_geron_matmul_order <- function(dims) {
  n <- length(dims) - 1
  m <- matrix(0.0, n, n)
  split <- matrix(0L, n, n)
  if (n >= 2) {
    for (length_ in 2:n) {
      for (i in 1:(n - length_ + 1)) {
        j <- i + length_ - 1
        best <- NULL
        for (k in i:(j - 1)) {
          cost <- m[i, k] + m[k + 1, j] + dims[i] * dims[k + 1] * dims[j + 1]
          if (is.null(best) || cost < best) {
            best <- cost
            split[i, j] <- k
          }
        }
        m[i, j] <- best
      }
    }
  }
  list(cost = m[1, n], split = split)
}

#' torch.compile: graph-capturing JIT for forward and backward passes
#'
#' Traces the model (\code{morie_geron_trace}), fuses consecutive
#' "linear" ops into one matmul (exact, verified by replay), and under
#' mode = "max-autotune" costs the fused product via the matrix-chain DP.
#'
#' @param model List of ops (see \code{morie_geron_trace}).
#' @param mode "default", "reduce-overhead", or "max-autotune".
#' @param example_inputs Required input used for graph capture.
#' @return list with compiled, graph, output, eager_output, max_diff, n_ops,
#'   n_compiled, fused_runs, flops_eager, flops_compiled, speedup, mode, estimate, n, method.
#' @export
morie_geron_torch_compile <- function(model, mode = "default", example_inputs) {
  m <- tolower(mode)
  tr <- morie_geron_trace(model, example_inputs)
  graph <- tr$graph
  eager <- tr$output
  x <- as.matrix(example_inputs)

  compiled <- list()
  flops_eager <- 0.0
  flops_comp <- 0.0
  i <- 1L
  ngraph <- length(graph)
  fused_runs <- 0L

  while (i <= ngraph) {
    if (graph[[i]]$kind != "linear") {
      compiled[[length(compiled) + 1]] <- graph[[i]]
      i <- i + 1L
      next
    }
    j <- i
    mats <- list()
    while (j <= ngraph && graph[[j]]$kind == "linear") {
      mats[[length(mats) + 1]] <- as.matrix(graph[[j]]$op$param)
      j <- j + 1L
    }
    rows <- graph[[i]]$in_shape[1]
    dims <- c(rows, sapply(mats, ncol))
    for (k in seq_along(mats)) flops_eager <- flops_eager + dims[k] * nrow(mats[[k]]) * ncol(mats[[k]])
    if (length(mats) == 1) {
      compiled[[length(compiled) + 1]] <- graph[[i]]
      flops_comp <- flops_comp + dims[1] * nrow(mats[[1]]) * ncol(mats[[1]])
    } else {
      fused_runs <- fused_runs + 1L
      if (m == "max-autotune") {
        mo <- morie_geron_matmul_order(dims)
        flops_comp <- flops_comp + mo$cost
      } else {
        W <- mats[[1]]
        for (w in mats[-1]) W <- W %*% w
        flops_comp <- flops_comp + dims[1] * nrow(W) * ncol(W)
      }
      W <- mats[[1]]
      for (w in mats[-1]) W <- W %*% w
      compiled[[length(compiled) + 1]] <- list(
        index = length(compiled), kind = "linear", in_shape = graph[[i]]$in_shape,
        out_shape = graph[[j - 1]]$out_shape, op = list(kind = "linear", param = W),
        fused_from = sapply(graph[i:(j - 1)], function(g) g$index)
      )
    }
    i <- j
  }

  out <- morie_geron_run_graph(compiled, x)
  diff <- max(abs(out - eager))
  speedup <- if (flops_comp > 0) flops_eager / flops_comp else 1.0

  list(
    compiled = compiled, graph = graph, output = out, eager_output = eager, max_diff = diff,
    n_ops = length(graph), n_compiled = length(compiled), fused_runs = fused_runs,
    flops_eager = flops_eager, flops_compiled = flops_comp, speedup = speedup, mode = m,
    estimate = speedup, n = nrow(x),
    method = "Graph capture (hmtsc) + linear-chain fusion, with matrix-chain DP association under max-autotune"
  )
}

# ---------------------------------------------------------------------
# hmuns: unsupervised learning (agglomerative + silhouette + autoencoder)
# ---------------------------------------------------------------------

#' Unsupervised learning: discover structure from unlabeled data
#'
#' Delegates grouping to \code{morie_geron_agglomerative}, scores it
#' with \code{morie_geron_silhouette}, and delegates compression to
#' \code{morie_geron_autoencoder} (both from geron_ml_native.R / this
#' shard).
#'
#' @param X Unlabeled data (n, d), n >= 2.
#' @param n_clusters Groups to look for.
#' @param bottleneck Code width for compression.
#' @param linkage Linkage rule.
#' @return list with labels, silhouette, silhouette_samples, codes, reconstruction,
#'   recon_error, explained_variance_ratio, merge_heights, estimate, n, method.
#' @export
morie_geron_unsupervised_learning <- function(X, n_clusters = 2, bottleneck = 1, linkage = "average") {
  A <- as.matrix(X)
  k <- as.integer(n_clusters)
  bn <- as.integer(bottleneck)

  grp <- morie_geron_agglomerative(A, n_clusters = k, linkage = linkage)
  labels <- as.vector(grp$labels)
  sil <- morie_geron_silhouette(A, labels)
  comp <- morie_geron_autoencoder(A, bn)

  list(
    labels = labels, silhouette = sil$silhouette, silhouette_samples = sil$samples,
    codes = comp$codes, reconstruction = comp$reconstruction, recon_error = comp$recon_error,
    explained_variance_ratio = comp$explained_variance_ratio, merge_heights = grp$heights,
    estimate = sil$silhouette, n = nrow(A),
    method = "Unsupervised structure: agglomerative grouping (hmagc) + silhouette (hmsil) + linear autoencoder (hmaen)"
  )
}

# ---------------------------------------------------------------------
# hmunsp: unsupervised pretraining (autoencoder + LOO-scored linear head)
# ---------------------------------------------------------------------

.morie_w4d_loo_mse <- function(D, t) {
  P <- MASS::ginv(t(D) %*% D)
  theta <- as.numeric(P %*% (t(D) %*% t))
  resid <- as.numeric(D %*% theta) - t
  h <- pmin(pmax(rowSums((D %*% P) * D), 0.0), 1.0)
  train_mse <- mean(resid * resid)
  if (any(h >= 1.0 - 1e-12)) {
    return(list(theta = theta, loo = Inf, train = train_mse))
  }
  list(theta = theta, loo = mean((resid / (1.0 - h))^2), train = train_mse)
}

#' Unsupervised pretraining: learn representation via reconstruction before labels
#'
#' Fits an autoencoder on X_unlab (via \code{morie_geron_autoencoder}),
#' pushes X_lab through the frozen encoder, fits a linear head on codes,
#' compares exact leave-one-out MSE against a raw-feature control.
#'
#' @param X_unlab Unlabeled pool (m, d).
#' @param X_lab Labeled inputs (n, d).
#' @param y_lab Labels, length n.
#' @param bottleneck Code width.
#' @return list with encoder, encode, codes, theta, theta_control, pretrained_loo,
#'   control_loo, pretrained_train_mse, control_train_mse, gain, recon_error,
#'   explained_variance_ratio, estimate, n, method.
#' @export
morie_geron_unsupervised_pretraining <- function(X_unlab, X_lab, y_lab, bottleneck = 1) {
  U <- as.matrix(X_unlab)
  L <- as.matrix(X_lab)
  t <- as.numeric(y_lab)
  bn <- as.integer(bottleneck)

  ae <- morie_geron_autoencoder(U, bn)
  codes <- ae$encode(L)
  Dp <- cbind(1.0, codes)
  fp <- .morie_w4d_loo_mse(Dp, t)
  Dc <- cbind(1.0, L)
  fc <- .morie_w4d_loo_mse(Dc, t)

  list(
    encoder = ae$encoder, encode = ae$encode, codes = codes, theta = fp$theta, theta_control = fc$theta,
    pretrained_loo = fp$loo, control_loo = fc$loo, pretrained_train_mse = fp$train, control_train_mse = fc$train,
    gain = fc$loo - fp$loo, recon_error = ae$recon_error, explained_variance_ratio = ae$explained_variance_ratio,
    estimate = fp$loo, n = nrow(L),
    method = "Autoencoder pretraining on the unlabeled pool (hmaen), frozen encoder + linear head, LOO-scored against a raw-feature control"
  )
}

# ---------------------------------------------------------------------
# hmvgr: vanishing gradients diagnosis
# ---------------------------------------------------------------------

#' Per-layer gradient L2 norms, ordered input side first
#' @param grads List of numeric vectors/arrays, one per layer.
#' @return Numeric vector of norms.
#' @export
morie_geron_layer_norms <- function(grads) {
  sapply(grads, function(g) sqrt(sum(as.numeric(g)^2)))
}

#' Vanishing gradients: small gradients shrink through many layers
#'
#' Reports per-layer ratio ||`g_{l+1}`||/||g_l|| and the geometric mean
#' ratio (constant per-layer factor); flags "vanishing" if the geometric
#' mean falls below tol.
#'
#' @param grads List of >= 2 per-layer gradients, input side first.
#' @param tol Geometric-mean ratio threshold, in (0,1).
#' @return list with norms, ratios, geometric_ratio, attenuation, vanishing,
#'   tol, estimate, n, method.
#' @export
morie_geron_vanishing_gradients <- function(grads, tol = 0.5) {
  norms <- morie_geron_layer_norms(grads)
  t <- as.numeric(tol)

  ratios <- norms[-1] / norms[-length(norms)]
  geo <- exp(mean(log(norms[-length(norms)] / norms[-1])))
  atten <- norms[1] / norms[length(norms)]

  list(
    norms = norms, ratios = ratios, geometric_ratio = geo, attenuation = atten,
    vanishing = geo < t, tol = t, estimate = geo, n = length(norms),
    method = "Per-layer gradient norms with the geometric mean decay factor towards the input"
  )
}

# ---------------------------------------------------------------------
# hmvae: variational autoencoder (Gaussian prior, reparameterisation)
# ---------------------------------------------------------------------

.morie_w4d_lcg_normal <- function(n, seed) {
  m <- n + (n %% 2)
  s <- as.numeric(seed) %% 2^32
  u <- numeric(m)
  for (i in seq_len(m)) {
    s <- (1664525 * s + 1013904223) %% 2^32
    u[i] <- (s + 0.5) / 2^32
  }
  a <- u[seq(1, m, 2)]
  b <- u[seq(2, m, 2)]
  z <- c(sqrt(-2 * log(a)) * cos(2 * pi * b), sqrt(-2 * log(a)) * sin(2 * pi * b))
  z[seq_len(n)]
}

#' ELBO loss and exact gradients for fixed reparameterisation noise
#' @param X Data (n, d).
#' @param params list(Wmu, bmu, Wlv, blv, Wd, bd).
#' @param eps Matrix of standard-normal noise (n, latent_dim).
#' @param beta KL weight.
#' @return list(loss, recon, kl, grads) with grads a same-shaped list as params.
#' @export
morie_geron_vae_loss_and_grads <- function(X, params, eps, beta = 1.0) {
  Wmu <- params[[1]]
  bmu <- params[[2]]
  Wlv <- params[[3]]
  blv <- params[[4]]
  Wd <- params[[5]]
  bd <- params[[6]]
  n <- nrow(X)
  d <- ncol(X)
  mu <- sweep(X %*% Wmu, 2, bmu, "+")
  lv <- pmin(pmax(sweep(X %*% Wlv, 2, blv, "+"), -30.0), 30.0)
  sdv <- exp(0.5 * lv)
  z <- mu + eps * sdv
  xhat <- sweep(z %*% Wd, 2, bd, "+")
  diff <- xhat - X
  recon <- mean(diff * diff)
  kl <- sum(0.5 * (mu * mu + exp(lv) - 1.0 - lv)) / n
  loss <- recon + beta * kl

  dxhat <- 2.0 * diff / (n * d)
  dWd <- t(z) %*% dxhat
  dbd <- colSums(dxhat)
  dz <- dxhat %*% t(Wd)
  dmu <- dz + beta * mu / n
  dlv <- dz * eps * 0.5 * sdv + beta * 0.5 * (exp(lv) - 1.0) / n
  grads <- list(t(X) %*% dmu, colSums(dmu), t(X) %*% dlv, colSums(dlv), dWd, dbd)
  list(loss = loss, recon = recon, kl = kl, grads = grads)
}

#' Variational autoencoder with latent Gaussian prior
#'
#' z = mu + sigma*eps (reparameterisation trick), trained by gradient
#' descent on the negative ELBO = recon MSE + beta*KL(q||N(0,I)).
#'
#' @param X Training data (n, d).
#' @param latent_dim,epochs,lr,beta,seed As in Python original.
#' @return list with mu, log_var, z, reconstruction, recon_error, kl, elbo,
#'   loss_curve, params, beta, estimate, n, method.
#' @export
morie_geron_vae <- function(X, latent_dim = 2, epochs = 200, lr = 0.05, beta = 1.0, seed = 0) {
  A <- as.matrix(X)
  k <- as.integer(latent_dim)
  E <- as.integer(epochs)
  step <- as.numeric(lr)
  b <- as.numeric(beta)
  n <- nrow(A)
  d <- ncol(A)

  params <- list(
    matrix(.morie_w4d_lcg_vec(d * k, as.numeric(seed) + 1, 0.1), d, k, byrow = TRUE), numeric(k),
    matrix(.morie_w4d_lcg_vec(d * k, as.numeric(seed) + 2, 0.1), d, k, byrow = TRUE), numeric(k),
    matrix(.morie_w4d_lcg_vec(k * d, as.numeric(seed) + 3, 0.1), k, d, byrow = TRUE), numeric(d)
  )
  losses <- numeric(E)
  for (e in seq_len(E)) {
    eps <- matrix(.morie_w4d_lcg_normal(n * k, as.numeric(seed) + 1000 + (e - 1)), n, k, byrow = TRUE)
    r <- morie_geron_vae_loss_and_grads(A, params, eps, b)
    losses[e] <- r$loss
    for (i in 1:6) params[[i]] <- params[[i]] - step * r$grads[[i]]
  }

  Wmu <- params[[1]]
  bmu <- params[[2]]
  Wlv <- params[[3]]
  blv <- params[[4]]
  Wd <- params[[5]]
  bd <- params[[6]]
  mu <- sweep(A %*% Wmu, 2, bmu, "+")
  lv <- pmin(pmax(sweep(A %*% Wlv, 2, blv, "+"), -30.0), 30.0)
  eps <- matrix(.morie_w4d_lcg_normal(n * k, as.numeric(seed) + 999), n, k, byrow = TRUE)
  z <- mu + eps * exp(0.5 * lv)
  xhat <- sweep(z %*% Wd, 2, bd, "+")
  recon <- mean((xhat - A)^2)
  kl <- sum(0.5 * (mu * mu + exp(lv) - 1.0 - lv)) / n

  list(
    mu = mu, log_var = lv, z = z, reconstruction = xhat, recon_error = recon, kl = kl,
    elbo = -(recon + b * kl), loss_curve = losses, params = params, beta = b,
    estimate = recon + b * kl, n = n,
    method = "Gaussian VAE trained on the negative ELBO with the reparameterisation trick"
  )
}

# ---------------------------------------------------------------------
# hmvbgm: variational Bayesian Gaussian mixture (VBGMM)
# ---------------------------------------------------------------------

#' Digamma psi(x) for x > 0 (recurrence + asymptotic series; no scipy dep)
#' @param x Numeric vector, x > 0.
#' @return Numeric vector.
#' @export
morie_geron_digamma <- function(x) {
  a <- as.numeric(x)
  r <- numeric(length(a))
  while (any(a < 6.0)) {
    m <- a < 6.0
    r[m] <- r[m] - 1.0 / a[m]
    a[m] <- a[m] + 1.0
  }
  f <- 1.0 / (a * a)
  r + log(a) - 0.5 / a + f * (-1.0 / 12 + f * (1.0 / 120 + f * (-1.0 / 252 + f * (1.0 / 240))))
}

#' Bayesian Gaussian mixture with variational inference (VBGMM)
#'
#' Mean-field VB with Dirichlet(alpha0) prior on mixing weights;
#' responsibility uses E\[log pi_k\] = psi(alpha_k) - psi(sum alpha) via
#' \code{morie_geron_digamma}, which prunes unneeded components as
#' alpha0 -> 0.
#'
#' @param X Data (n, d).
#' @param n_components,max_iter,alpha0,tol,var_floor,seed As in Python original.
#' @return list with weights, means, variances, resp, labels, alpha,
#'   n_effective, n_iter, estimate, n, method.
#' @export
morie_geron_variational_bayes_gmm <- function(X, n_components = 3, max_iter = 100, alpha0 = 1e-2,
                                              tol = 1e-6, var_floor = 1e-6, seed = 0) {
  A <- as.matrix(X)
  n <- nrow(A)
  d <- ncol(A)
  K <- as.integer(n_components)
  it_max <- as.integer(max_iter)
  a0 <- as.numeric(alpha0)
  vf <- as.numeric(var_floor)

  order_ <- order(A[, 1]) # 1-based, ties by original order (stable, matches mergesort behaviour)
  means <- matrix(0.0, K, d)
  for (i in 0:(K - 1)) {
    pos <- if (K > 1) round(i * (n - 1) / (K - 1)) else 0
    means[i + 1, ] <- A[order_[pos + 1], ]
  }
  variances <- matrix(rep(pmax(apply(A, 2, .morie_gr_pvar), vf), each = K), K, d)
  alpha <- rep(a0 + n / K, K)
  resp <- matrix(1.0 / K, n, K)

  n_iter <- 0L
  for (it in seq_len(it_max)) {
    n_iter <- it
    elog_pi <- morie_geron_digamma(alpha) - morie_geron_digamma(sum(alpha))
    log_norm <- -0.5 * rowSums(log(2 * pi * variances))
    quad <- matrix(0.0, n, K)
    for (k in seq_len(K)) quad[, k] <- -0.5 * rowSums(sweep(A, 2, means[k, ], "-")^2 / matrix(variances[k, ], n, d, byrow = TRUE))
    log_r <- sweep(quad, 2, elog_pi + log_norm, "+")
    log_r <- log_r - apply(log_r, 1, max)
    new_resp <- exp(log_r)
    new_resp <- new_resp / rowSums(new_resp)
    delta <- max(abs(new_resp - resp))
    resp <- new_resp

    Nk <- colSums(resp)
    alpha <- a0 + Nk
    for (k in seq_len(K)) {
      if (Nk[k] > 1e-12) {
        means[k, ] <- as.numeric(resp[, k] %*% A) / Nk[k]
        variances[k, ] <- pmax(as.numeric(resp[, k] %*% (sweep(A, 2, means[k, ], "-")^2)) / Nk[k], vf)
      }
    }
    if (delta < tol) break
  }

  weights <- alpha / sum(alpha)
  labels <- apply(resp, 1, which.max) - 1L
  n_eff <- sum(weights > 1.0 / (10.0 * K))

  list(
    weights = weights, means = means, variances = variances, resp = resp, labels = labels, alpha = alpha,
    n_effective = n_eff, n_iter = n_iter, estimate = as.numeric(n_eff), n = n,
    method = "Mean-field VB with a Dirichlet prior on the weights (E[log pi] via digamma) and MAP diagonal Gaussians"
  )
}

# ---------------------------------------------------------------------
# hmvbrt: VideoBERT (joint MLM over video + text tokens)
# ---------------------------------------------------------------------

#' VideoBERT: transformer on discretized video tokens + text
#'
#' Joint MLM: video/text tokens embedded into one sequence with
#' modality-typed embeddings, one joint self-attention block
#' (\code{morie_geron_scaled_dot_product}), softmax head over the
#' joint vocabulary scores masked positions.
#'
#' @param video_tokens,text_tokens Integer (0-based) token id vectors.
#' @param d_model Embedding width.
#' @param mask_positions Optional 0-based positions to mask.
#' @param mask_prob Masking rate when mask_positions is NULL.
#' @param seed LCG seed.
#' @return list with loss, token_losses, attention, hidden, masked, predictions,
#'   targets, cross_modal_mass, n_video, n_text, vocab_size, estimate, n, method.
#' @export
morie_geron_videobert <- function(video_tokens, text_tokens, d_model = 8, mask_positions = NULL,
                                  mask_prob = 0.25, seed = 0) {
  v <- as.integer(video_tokens)
  t <- as.integer(text_tokens)
  d <- as.integer(d_model)
  n_v <- length(v)
  n_t <- length(t)
  Tn <- n_v + n_t
  v_vocab <- max(v) + 1L
  ids <- c(v, t + v_vocab) # 0-based joint ids
  V <- v_vocab + max(t) + 1L
  modality <- c(rep(0L, n_v), rep(1L, n_t))

  if (is.null(mask_positions)) {
    p <- as.numeric(mask_prob)
    stride <- max(1, round(1.0 / p))
    masked <- seq(0, Tn - 1, by = stride) # 0-based
  } else {
    masked <- as.integer(mask_positions)
  }

  Emat <- matrix(.morie_w4d_lcg_vec(V * d, as.numeric(seed) + 1, 0.5), V, d, byrow = TRUE)
  Mod <- matrix(.morie_w4d_lcg_vec(2 * d, as.numeric(seed) + 2, 0.2), 2, d, byrow = TRUE)
  H <- Emat[ids + 1, , drop = FALSE] + Mod[modality + 1, , drop = FALSE]
  mask_emb <- matrix(.morie_w4d_lcg_vec(d, as.numeric(seed) + 3, 0.1), 1, d, byrow = TRUE)
  H[masked + 1, ] <- matrix(mask_emb, length(masked), d, byrow = TRUE)

  Wq <- matrix(.morie_w4d_lcg_vec(d * d, as.numeric(seed) + 10, 0.5), d, d, byrow = TRUE)
  Wk <- matrix(.morie_w4d_lcg_vec(d * d, as.numeric(seed) + 11, 0.5), d, d, byrow = TRUE)
  Wv <- matrix(.morie_w4d_lcg_vec(d * d, as.numeric(seed) + 12, 0.5), d, d, byrow = TRUE)
  att <- morie_geron_scaled_dot_product(H %*% Wq, H %*% Wk, H %*% Wv, d_k = d)
  Y <- att$Y
  Amat <- att$attention

  Wout <- matrix(.morie_w4d_lcg_vec(d * V, as.numeric(seed) + 20, 0.5), d, V, byrow = TRUE)
  losses <- numeric(length(masked))
  preds <- integer(length(masked))
  for (mi in seq_along(masked)) {
    m <- masked[mi]
    p_dist <- .morie_gr_softmax(as.numeric(Y[m + 1, ] %*% Wout))
    losses[mi] <- -log(max(p_dist[ids[m + 1] + 1], .Machine$double.xmin))
    preds[mi] <- which.max(p_dist) - 1L
  }
  loss <- mean(losses)

  cross <- mean(sapply(seq_len(Tn), function(i) sum(Amat[i, modality != modality[i]])))

  list(
    loss = loss, token_losses = losses, attention = Amat, hidden = Y, masked = masked, predictions = preds,
    targets = ids[masked + 1], cross_modal_mass = cross, n_video = n_v, n_text = n_t, vocab_size = V,
    estimate = loss, n = Tn,
    method = "Joint MLM over concatenated video/text tokens with modality embeddings and shared self-attention"
  )
}

# ---------------------------------------------------------------------
# hmvilb: ViLBERT (dual-stream co-attention)
# ---------------------------------------------------------------------

#' ViLBERT: dual-stream vision-language transformer
#'
#' Two separate streams exchange info only through swapped queries:
#' image queries over text keys/values -> attention_v2t; text queries
#' over image keys/values -> attention_t2v (both via
#' \code{morie_geron_scaled_dot_product}).
#'
#' @param image Region features (n_regions, d_v), or (H,W) map flattened to single-feature regions.
#' @param text Token features (n_tokens, d_t), or 1-D vector of 0-based token ids.
#' @param d_model Shared co-attention width.
#' @param seed LCG seed.
#' @return list with image_out, text_out, attention_v2t, attention_t2v, pooled,
#'   image_hidden, text_hidden, n_regions, n_tokens, estimate, n, method.
#' @export
morie_geron_vilbert <- function(image, text, d_model = 8, seed = 0) {
  img <- as.matrix(image)
  d <- as.integer(d_model)

  if (is.null(dim(text)) && all(text == round(text))) {
    tid <- as.integer(text)
    Emat <- matrix(.morie_w4d_lcg_vec((max(tid) + 1) * d, as.numeric(seed) + 1), max(tid) + 1, d, byrow = TRUE)
    Tf <- Emat[tid + 1, , drop = FALSE]
  } else {
    Tf <- as.matrix(text)
  }

  Wv <- matrix(.morie_w4d_lcg_vec(ncol(img) * d, as.numeric(seed) + 2), ncol(img), d, byrow = TRUE)
  Wt <- if (ncol(Tf) != d) matrix(.morie_w4d_lcg_vec(ncol(Tf) * d, as.numeric(seed) + 3), ncol(Tf), d, byrow = TRUE) else diag(d)
  Hv <- img %*% Wv
  Ht <- Tf %*% Wt

  Wq_v <- matrix(.morie_w4d_lcg_vec(d * d, as.numeric(seed) + 10), d, d, byrow = TRUE)
  Wk_t <- matrix(.morie_w4d_lcg_vec(d * d, as.numeric(seed) + 11), d, d, byrow = TRUE)
  Wv_t <- matrix(.morie_w4d_lcg_vec(d * d, as.numeric(seed) + 12), d, d, byrow = TRUE)
  Wq_t <- matrix(.morie_w4d_lcg_vec(d * d, as.numeric(seed) + 20), d, d, byrow = TRUE)
  Wk_v <- matrix(.morie_w4d_lcg_vec(d * d, as.numeric(seed) + 21), d, d, byrow = TRUE)
  Wv_v <- matrix(.morie_w4d_lcg_vec(d * d, as.numeric(seed) + 22), d, d, byrow = TRUE)

  v2t <- morie_geron_scaled_dot_product(Hv %*% Wq_v, Ht %*% Wk_t, Ht %*% Wv_t, d_k = d)
  t2v <- morie_geron_scaled_dot_product(Ht %*% Wq_t, Hv %*% Wk_v, Hv %*% Wv_v, d_k = d)

  img_out <- Hv + v2t$Y
  txt_out <- Ht + t2v$Y
  pooled <- 0.5 * (colMeans(img_out) + colMeans(txt_out))

  list(
    image_out = img_out, text_out = txt_out, attention_v2t = v2t$attention, attention_t2v = t2v$attention,
    pooled = pooled, image_hidden = Hv, text_hidden = Ht, n_regions = nrow(img), n_tokens = nrow(Tf),
    estimate = max(v2t$attention), n = nrow(img) + nrow(Tf),
    method = "Dual-stream co-attention: image queries over text keys/values and vice versa (hmsdp)"
  )
}

# ---------------------------------------------------------------------
# hmvit: Vision Transformer (ViT)
# ---------------------------------------------------------------------

.morie_w4d_sinusoidal <- function(Tn, d) {
  pos <- matrix(0:(Tn - 1), Tn, 1)
  denom <- matrix(10000.0^((2 * ((0:(d - 1)) %/% 2)) / d), Tn, d, byrow = TRUE)
  angle <- (pos %*% matrix(1, 1, d)) / denom
  even <- matrix((0:(d - 1)) %% 2 == 0, Tn, d, byrow = TRUE)
  ifelse(even, sin(angle), cos(angle))
}

#' Vision Transformer (ViT): transformer on image patches
#'
#' Patchify image, linear-embed patches, prepend \[CLS\], add sinusoidal
#' position encodings, run \code{morie_geron_transformer}, linear head
#' on the \[CLS\] row.
#'
#' @param image (H,W) or (H,W,C) array.
#' @param patch_size Patch side length.
#' @param n_layers,d_model,n_heads,n_classes,seed As in Python original.
#' @return list with logits, cls, tokens, patches, n_patches, seq_len, patch_dim,
#'   grid, encoder_params, total_params, predicted, estimate, n, method.
#' @export
morie_geron_vision_transformer <- function(image, patch_size, n_layers = 2, d_model = 8, n_heads = 2,
                                           n_classes = 2, seed = 0) {
  img <- as.array(image)
  if (length(dim(img)) == 2) dim(img) <- c(dim(img), 1)
  Hh <- dim(img)[1]
  Ww <- dim(img)[2]
  Cc <- dim(img)[3]
  p <- as.integer(patch_size)
  d <- as.integer(d_model)
  h <- as.integer(n_heads)
  K <- as.integer(n_classes)
  L <- as.integer(n_layers)

  nh <- Hh %/% p
  nw <- Ww %/% p
  n_patches <- nh * nw
  patch_dim <- p * p * Cc
  patches <- matrix(0.0, n_patches, patch_dim)
  idx <- 1
  for (i in 0:(nh - 1)) {
    for (j in 0:(nw - 1)) {
      blk <- img[(i * p + 1):((i + 1) * p), (j * p + 1):((j + 1) * p), , drop = FALSE]
      # row-major flatten matching numpy .reshape(-1) of (p,p,C): index order (row, col, channel)
      v <- numeric(patch_dim)
      vi <- 1
      for (r in seq_len(p)) {
        for (c in seq_len(p)) {
          for (ch in seq_len(Cc)) {
            v[vi] <- blk[r, c, ch]
            vi <- vi + 1
          }
        }
      }
      patches[idx, ] <- v
      idx <- idx + 1
    }
  }

  Emat <- matrix(.morie_w4d_lcg_vec(patch_dim * d, as.numeric(seed) + 1, 0.1), patch_dim, d, byrow = TRUE)
  b_e <- .morie_w4d_lcg_vec(d, as.numeric(seed) + 2, 0.1)
  cls <- matrix(.morie_w4d_lcg_vec(d, as.numeric(seed) + 3, 0.1), 1, d, byrow = TRUE)
  tokens <- rbind(cls, sweep(patches %*% Emat, 2, b_e, "+"))
  seq_len_ <- nrow(tokens)
  tokens <- tokens + .morie_w4d_sinusoidal(seq_len_, d)

  enc <- morie_geron_transformer(tokens, n_heads = h, n_layers = L, seed = as.numeric(seed) + 10)
  Y <- enc$Y
  Wh <- matrix(.morie_w4d_lcg_vec(d * K, as.numeric(seed) + 4, 0.1), d, K, byrow = TRUE)
  b_h <- .morie_w4d_lcg_vec(K, as.numeric(seed) + 5, 0.1)
  logits <- as.numeric(Y[1, ] %*% Wh) + b_h

  enc_p <- morie_geron_encoder_params(d, 4 * d, L)
  total <- patch_dim * d + d + d + seq_len_ * d + enc_p + d * K + K

  list(
    logits = logits, cls = Y[1, ], tokens = Y, patches = patches, n_patches = n_patches, seq_len = seq_len_,
    patch_dim = patch_dim, grid = c(nh, nw), encoder_params = enc_p, total_params = total,
    predicted = which.max(logits) - 1L, estimate = as.numeric(total), n = n_patches,
    method = "ViT: patch embedding + [CLS] + sinusoidal positions + transformer encoder (hmtfm)"
  )
}

# ---------------------------------------------------------------------
# hmvqv: VQ-VAE (vector-quantized latents, straight-through estimator)
# ---------------------------------------------------------------------

#' Nearest-codebook-entry assignment
#' @param z_e Encodings (n, k).
#' @param codebook Codes (K, k).
#' @return list(indices, z_q) with indices 0-based.
#' @export
morie_geron_quantize <- function(z_e, codebook) {
  n <- nrow(z_e)
  K <- nrow(codebook)
  d2 <- matrix(0.0, n, K)
  for (j in seq_len(K)) d2[, j] <- rowSums(sweep(z_e, 2, codebook[j, ], "-")^2)
  idx <- apply(d2, 1, which.min) - 1L
  list(indices = idx, z_q = codebook[idx + 1, , drop = FALSE])
}

#' Discrete VAE (VQ-VAE): vector-quantized latents with codebook
#'
#' L = recon MSE + codebook loss + beta*commitment loss; straight-through
#' estimator copies the decoder gradient at z_q onto z_e.
#'
#' @param X Training data (n, d).
#' @param codebook_size,latent_dim,epochs,lr,beta,seed As in Python original.
#' @return list with codes, indices, z_e, z_q, codebook, counts, reconstruction,
#'   recon_error, codebook_loss, commitment_loss, perplexity, loss_curve, beta,
#'   estimate, n, method.
#' @export
morie_geron_vq_vae <- function(X, codebook_size = 4, latent_dim = 2, epochs = 200, lr = 0.05,
                               beta = 0.25, seed = 0) {
  A <- as.matrix(X)
  n <- nrow(A)
  d <- ncol(A)
  K <- as.integer(codebook_size)
  k <- as.integer(latent_dim)
  E <- as.integer(epochs)
  step <- as.numeric(lr)
  b <- as.numeric(beta)

  We <- matrix(.morie_w4d_lcg_vec(d * k, as.numeric(seed) + 1, 0.5), d, k, byrow = TRUE)
  be <- numeric(k)
  Wd <- matrix(.morie_w4d_lcg_vec(k * d, as.numeric(seed) + 2, 0.5), k, d, byrow = TRUE)
  bd <- numeric(d)
  z0 <- sweep(A %*% We, 2, be, "+")
  cb_init <- t(sapply(0:(K - 1), function(i) z0[round(i * (n - 1) / max(1, K - 1)) + 1, ]))
  if (k == 1) cb_init <- matrix(cb_init, K, 1)
  cb <- cb_init + matrix(.morie_w4d_lcg_vec(K * k, as.numeric(seed) + 3, 0.01), K, k, byrow = TRUE)

  losses <- numeric(E)
  idx <- NULL
  z_e <- NULL
  z_q <- NULL
  for (e in seq_len(E)) {
    z_e <- sweep(A %*% We, 2, be, "+")
    qz <- morie_geron_quantize(z_e, cb)
    idx <- qz$indices
    z_q <- qz$z_q
    xhat <- sweep(z_q %*% Wd, 2, bd, "+")
    diff <- xhat - A
    recon <- mean(diff * diff)
    cb_loss <- mean((z_q - z_e)^2)
    commit <- mean((z_e - z_q)^2)
    losses[e] <- recon + cb_loss + b * commit

    dxhat <- 2.0 * diff / (n * d)
    dWd <- t(z_q) %*% dxhat
    dbd <- colSums(dxhat)
    dz_q <- dxhat %*% t(Wd)
    dz_e <- dz_q + b * 2.0 * (z_e - z_q) / (n * k)
    dWe <- t(A) %*% dz_e
    dbe <- colSums(dz_e)
    dcb <- matrix(0.0, K, k)
    for (j in 0:(K - 1)) {
      m <- idx == j
      if (any(m)) dcb[j + 1, ] <- 2.0 * colSums(sweep(-z_e[m, , drop = FALSE], 2, cb[j + 1, ], "+")) / (n * k)
    }

    We <- We - step * dWe
    be <- be - step * dbe
    Wd <- Wd - step * dWd
    bd <- bd - step * dbd
    cb <- cb - step * dcb
  }

  z_e <- sweep(A %*% We, 2, be, "+")
  qz <- morie_geron_quantize(z_e, cb)
  idx <- qz$indices
  z_q <- qz$z_q
  xhat <- sweep(z_q %*% Wd, 2, bd, "+")
  recon <- mean((xhat - A)^2)
  cb_loss <- mean((z_q - z_e)^2)
  counts <- sapply(0:(K - 1), function(j) sum(idx == j))
  p <- counts / sum(counts)
  nz <- p > 0
  perplexity <- exp(-sum(p[nz] * log(p[nz])))

  list(
    codes = z_q, indices = idx, z_e = z_e, z_q = z_q, codebook = cb, counts = counts,
    reconstruction = xhat, recon_error = recon, codebook_loss = cb_loss, commitment_loss = cb_loss,
    perplexity = perplexity, loss_curve = losses, beta = b, estimate = recon, n = n,
    method = "VQ-VAE: nearest-code quantisation, straight-through encoder gradient, codebook + commitment losses"
  )
}

# ---------------------------------------------------------------------
# hmwemb: word embeddings (lookup table + cosine similarity)
# ---------------------------------------------------------------------

#' Word embeddings: dense vector representations learned per token
#'
#' Embedding table as a lookup (row i is the token's vector), LCG-init
#' or a supplied pretrained matrix, plus the cosine-similarity matrix.
#'
#' @param vocab Character vector of distinct tokens.
#' @param d Embedding width (ignored if E supplied).
#' @param E Optional pretrained (V,d) matrix.
#' @param seed LCG seed.
#' @return list with E, lookup, index, vocab, norms, similarity, n_params, d,
#'   estimate, n, method. lookup is a function(token_or_char_vector).
#' @export
morie_geron_word_embeddings <- function(vocab, d = 8, E = NULL, seed = 0) {
  toks <- as.character(vocab)
  V <- length(toks)

  if (is.null(E)) {
    k <- as.integer(d)
    s <- as.numeric(seed) %% 2^32
    scale <- 1.0 / sqrt(k)
    flat <- numeric(V * k)
    for (i in seq_len(V * k)) {
      s <- (1664525 * s + 1013904223) %% 2^32
      flat[i] <- (2.0 * ((s + 0.5) / 2^32) - 1.0) * scale
    }
    M <- matrix(flat, V, k, byrow = TRUE)
  } else {
    M <- as.matrix(E)
    k <- ncol(M)
  }

  index <- setNames(seq_len(V) - 1L, toks)

  lookup <- function(token) {
    if (length(token) > 1) {
      return(M[index[token] + 1, , drop = FALSE])
    }
    M[index[[token]] + 1, ]
  }

  norms <- sqrt(rowSums(M * M))
  safe <- ifelse(norms > 0, norms, 1.0)
  U <- M / safe
  sim <- U %*% t(U)

  list(
    E = M, lookup = lookup, index = as.list(index), vocab = toks, norms = norms, similarity = sim,
    n_params = V * k, d = k, estimate = as.numeric(V * k), n = V,
    method = "Embedding lookup table with cosine-similarity geometry"
  )
}

# ---------------------------------------------------------------------
# hmwpt: WordPiece tokenizer (likelihood-scored merges)
# ---------------------------------------------------------------------

.morie_w4d_wp_split <- function(word) {
  chars <- strsplit(word, "")[[1]]
  c(chars[1], paste0("##", chars[-1]))
}

#' WordPiece tokenizer: maximum likelihood subword segmentation
#'
#' Greedily merges the pair maximising freq(AB)/(freq(A)*freq(B)) (not
#' raw frequency, that is BPE); continuation pieces carry "##"; final
#' segmentation is greedy longest-match-first.
#'
#' @param corpus Character string or vector (whitespace-tokenised).
#' @param vocab_size Target vocabulary size (>= alphabet size).
#' @return list with vocab, merges, scores, tokenize (function), alphabet,
#'   word_counts, estimate, n, method.
#' @export
morie_geron_wordpiece_tokenizer <- function(corpus, vocab_size = 50) {
  words <- unlist(strsplit(corpus, "[[:space:]]+"))
  words <- words[nzchar(words)]
  counts <- table(words)
  V <- as.integer(vocab_size)

  uwords <- names(counts)
  splits <- setNames(lapply(uwords, .morie_w4d_wp_split), uwords)
  alphabet <- sort(unique(unlist(splits)))
  vocab <- alphabet
  merges <- list()
  scores <- numeric(0)

  while (length(vocab) < V) {
    pair_freq <- list()
    piece_freq <- list()
    for (w in uwords) {
      c_ <- as.numeric(counts[[w]])
      s <- splits[[w]]
      for (p in s) piece_freq[[p]] <- (if (is.null(piece_freq[[p]])) 0 else piece_freq[[p]]) + c_
      if (length(s) > 1) {
        for (i in seq_len(length(s) - 1)) {
          key <- paste(s[i], s[i + 1], sep = "")
          pair_freq[[key]] <- (if (is.null(pair_freq[[key]])) 0 else pair_freq[[key]]) + c_
        }
      }
    }
    if (length(pair_freq) == 0) break
    keys <- sort(names(pair_freq))
    best <- NULL
    best_score <- -1.0
    for (key in keys) {
      ab <- strsplit(key, "", fixed = TRUE)[[1]]
      a <- ab[1]
      b <- ab[2]
      sc <- pair_freq[[key]] / (piece_freq[[a]] * piece_freq[[b]])
      if (sc > best_score) {
        best <- c(a, b)
        best_score <- sc
      }
    }
    a <- best[1]
    b <- best[2]
    new <- if (startsWith(b, "##")) paste0(a, substring(b, 3)) else paste0(a, b)
    if (new %in% vocab) break
    vocab <- c(vocab, new)
    merges[[length(merges) + 1]] <- c(a, b)
    scores <- c(scores, best_score)
    for (w in uwords) {
      s <- splits[[w]]
      out <- character(0)
      i <- 1
      while (i <= length(s)) {
        if (i < length(s) && s[i] == a && s[i + 1] == b) {
          out <- c(out, new)
          i <- i + 2
        } else {
          out <- c(out, s[i])
          i <- i + 1
        }
      }
      splits[[w]] <- out
    }
  }

  vocab_set <- vocab

  tokenize <- function(word) {
    w <- as.character(word)
    chars <- strsplit(w, "")[[1]]
    n <- length(chars)
    out <- character(0)
    start <- 1
    while (start <= n) {
      end <- n
      piece <- NULL
      while (start <= end) {
        cand <- if (start == 1) paste(chars[start:end], collapse = "") else paste0("##", paste(chars[start:end], collapse = ""))
        if (cand %in% vocab_set) {
          piece <- cand
          break
        }
        end <- end - 1
      }
      if (is.null(piece)) {
        return("[UNK]")
      }
      out <- c(out, piece)
      start <- end + 1
    }
    out
  }

  list(
    vocab = vocab, merges = merges, scores = scores, tokenize = tokenize, alphabet = alphabet,
    word_counts = as.list(counts), estimate = as.numeric(length(vocab)), n = sum(counts),
    method = "WordPiece: greedy likelihood-scored merges freq(AB)/(freq(A)freq(B)), longest-match segmentation"
  )
}

# ---------------------------------------------------------------------
# hmwrst: warm restarts (SGDR cosine annealing)
# ---------------------------------------------------------------------

#' Warm restarts: cosine decay with periodic restarts (SGDR)
#'
#' eta = eta_min + 0.5*(eta_max-eta_min)*(1+cos(pi*T_cur/T_i)); cycle
#' lengths grow geometrically `T_{i+1}` = round(T_i * factor).
#'
#' @param t Integer step or vector of steps (>=0).
#' @param T0 First cycle length.
#' @param factor Geometric growth (>=1).
#' @param eta_max,eta_min Learning-rate bounds.
#' @return list with eta, cycle, cycle_length, step_in_cycle, restarts, T0,
#'   factor, estimate, n, method.
#' @export
morie_geron_warm_restarts <- function(t, T0 = 10, factor = 2.0, eta_max = 0.1, eta_min = 0.0) {
  steps <- as.integer(t)
  T_ <- as.integer(T0)
  f <- as.numeric(factor)
  hi <- as.numeric(eta_max)
  lo <- as.numeric(eta_min)

  n <- length(steps)
  etas <- numeric(n)
  cyc <- integer(n)
  clen <- integer(n)
  scur <- integer(n)
  for (k in seq_len(n)) {
    step <- steps[k]
    i <- 0L
    start <- 0L
    length_ <- T_
    while (step >= start + length_) {
      start <- start + length_
      length_ <- max(1L, round(length_ * f))
      i <- i + 1L
    }
    cur <- step - start
    etas[k] <- lo + 0.5 * (hi - lo) * (1.0 + cos(pi * cur / length_))
    cyc[k] <- i
    clen[k] <- length_
    scur[k] <- cur
  }

  list(
    eta = etas, cycle = cyc, cycle_length = clen, step_in_cycle = scur, restarts = max(cyc),
    T0 = T_, factor = f, estimate = etas[n], n = n,
    method = "SGDR: cosine annealing within geometrically growing cycles"
  )
}

# ---------------------------------------------------------------------
# hmxcpt: Xception architecture (parameter/layer resolution)
# ---------------------------------------------------------------------

#' Depthwise separable convolution weight count
#' @param k Kernel side.
#' @param c_in Input channels.
#' @param c_out Output channels.
#' @return Integer parameter count k*k*c_in + c_in*c_out.
#' @export
morie_geron_separable_params <- function(k, c_in, c_out) {
  as.integer(k * k * c_in + c_in * c_out)
}

#' Xception: extreme inception using depthwise separable convolutions
#'
#' Resolves the entry/middle(x8)/exit flow into concrete layers, shapes
#' and parameter counts; separable convs cost k*k*c_in + c_in*c_out,
#' batch norm 2*C trainable + 2*C non-trainable per layer.
#'
#' @param n_classes,in_channels,input_size As in Python original.
#' @return list with layers, total_params, trainable_params, non_trainable_params,
#'   weight_params, bn_channels, n_separable, separable_params_total,
#'   standard_conv_params, savings_ratio, output_shape, final_map, estimate, n, method.
#' @export
morie_geron_xception <- function(n_classes = 1000, in_channels = 3, input_size = 299) {
  K <- as.integer(n_classes)
  cin <- as.integer(in_channels)
  size <- as.integer(input_size)

  layers <- list()
  bn_channels <- 0L
  spatial <- size

  out_fn <- function(sp, k, s, p) ((sp + 2 * p - k) %/% s) + 1L

  add <- function(kind, params, c_out, bn = TRUE) {
    if (bn) bn_channels <<- bn_channels + c_out
    layers[[length(layers) + 1]] <<- list(kind = kind, params = as.integer(params), channels = as.integer(c_out), out = as.integer(spatial))
  }

  spatial <- out_fn(spatial, 3, 2, 0)
  add("conv3x3/s2", 3 * 3 * cin * 32, 32)
  c_ <- 32
  add("conv3x3", 3 * 3 * c_ * 64, 64)
  c_ <- 64
  for (width in c(128, 256, 728)) {
    add("separable3x3", morie_geron_separable_params(3, c_, width), width)
    add("separable3x3", morie_geron_separable_params(3, width, width), width)
    spatial <- ((spatial - 3 + 2 * 1) %/% 2) + 1L
    add("maxpool3x3/s2", 0, width, bn = FALSE)
    add("conv1x1 shortcut/s2", c_ * width, width)
    c_ <- width
  }

  for (rep_ in 1:8) for (j in 1:3) add("separable3x3", morie_geron_separable_params(3, 728, 728), 728)

  add("separable3x3", morie_geron_separable_params(3, 728, 728), 728)
  add("separable3x3", morie_geron_separable_params(3, 728, 1024), 1024)
  spatial <- ((spatial - 3 + 2 * 1) %/% 2) + 1L
  add("maxpool3x3/s2", 0, 1024, bn = FALSE)
  add("conv1x1 shortcut/s2", 728 * 1024, 1024)
  add("separable3x3", morie_geron_separable_params(3, 1024, 1536), 1536)
  add("separable3x3", morie_geron_separable_params(3, 1536, 2048), 2048)
  add("global_avg_pool", 0, 2048, bn = FALSE)
  layers[[length(layers) + 1]] <- list(kind = "fc", params = as.integer(2048 * K + K), channels = as.integer(K), out = 1L)

  weight_params <- sum(sapply(layers, `[[`, "params"))
  bn_trainable <- 2 * bn_channels
  trainable <- weight_params + bn_trainable
  non_trainable <- 2 * bn_channels
  n_sep <- sum(sapply(layers, function(l) l$kind == "separable3x3"))

  std_equiv <- 0
  c_prev <- cin
  for (l in layers) {
    if (l$kind == "separable3x3") std_equiv <- std_equiv + 3 * 3 * c_prev * l$channels
    if (l$channels > 0 && l$kind != "fc") c_prev <- l$channels
  }
  sep_total <- sum(sapply(layers, function(l) if (l$kind == "separable3x3") l$params else 0))

  list(
    layers = layers, total_params = as.integer(trainable + non_trainable), trainable_params = as.integer(trainable),
    non_trainable_params = as.integer(non_trainable), weight_params = as.integer(weight_params),
    bn_channels = as.integer(bn_channels), n_separable = as.integer(n_sep),
    separable_params_total = as.integer(sep_total), standard_conv_params = as.integer(std_equiv),
    savings_ratio = sep_total / std_equiv, output_shape = K, final_map = c(spatial, spatial, 2048),
    estimate = as.numeric(trainable), n = length(layers),
    method = "Xception resolved to concrete layers, shapes and parameter counts"
  )
}

# ---------------------------------------------------------------------
# hmxgb: XGBoost (second-order regularised gradient boosting)
# ---------------------------------------------------------------------

.morie_w4d_xgb_leaf_weight <- function(G, H, lam) -G / (H + lam)
.morie_w4d_xgb_gain <- function(GL, HL, GR, HR, lam, gamma) {
  G <- GL + GR
  H <- HL + HR
  0.5 * (GL * GL / (HL + lam) + GR * GR / (HR + lam) - G * G / (H + lam)) - gamma
}

.morie_w4d_xgb_build <- function(X, g, h, depth, max_depth, lam, gamma, min_child_weight) {
  G <- sum(g)
  H <- sum(h)
  node <- list(weight = .morie_w4d_xgb_leaf_weight(G, H, lam), G = G, H = H, leaf = TRUE)
  if (depth >= max_depth || nrow(X) < 2) {
    return(node)
  }
  best <- NULL
  for (j in seq_len(ncol(X))) {
    ord <- order(X[, j])
    xs <- X[ord, j]
    gs <- g[ord]
    hs <- h[ord]
    GL <- 0.0
    HL <- 0.0
    for (i in seq_len(length(xs) - 1)) {
      GL <- GL + gs[i]
      HL <- HL + hs[i]
      if (xs[i + 1] == xs[i]) next
      GR <- G - GL
      HR <- H - HL
      if (HL < min_child_weight || HR < min_child_weight) next
      gain <- .morie_w4d_xgb_gain(GL, HL, GR, HR, lam, gamma)
      if (is.null(best) || gain > best$gain) best <- list(gain = gain, j = j, thr = 0.5 * (xs[i] + xs[i + 1]))
    }
  }
  if (is.null(best) || best$gain <= 0) {
    return(node)
  }
  left <- X[, best$j] <= best$thr
  node$leaf <- FALSE
  node$feature <- best$j - 1L # 0-based feature index
  node$threshold <- best$thr
  node$gain <- best$gain
  node$left <- .morie_w4d_xgb_build(X[left, , drop = FALSE], g[left], h[left], depth + 1, max_depth, lam, gamma, min_child_weight)
  node$right <- .morie_w4d_xgb_build(X[!left, , drop = FALSE], g[!left], h[!left], depth + 1, max_depth, lam, gamma, min_child_weight)
  node
}

.morie_w4d_xgb_predict <- function(node, X) {
  out <- numeric(nrow(X))
  for (i in seq_len(nrow(X))) {
    nd <- node
    while (!nd$leaf) nd <- if (X[i, nd$feature + 1] <= nd$threshold) nd$left else nd$right
    out[i] <- nd$weight
  }
  out
}

#' XGBoost: regularized gradient boosting with second-order Taylor approximation
#'
#' w_j = -G_j/(H_j+lambda); gain = 0.5*(GL^2/(HL+lam) + GR^2/(HR+lam) -
#' G^2/(H+lam)) - gamma. Exact greedy split search over sorted feature
#' values.
#'
#' @param X Design matrix (n, d).
#' @param y Targets `({0,1}` for logistic).
#' @param n_estimators,learning_rate,max_depth,reg_lambda,gamma,min_child_weight,objective As in Python original.
#' @return list with predicted, raw_score, trees, base_score, loss_curve,
#'   feature_importance, objective, estimate, n, method.
#' @export
morie_geron_xgboost <- function(X, y, n_estimators = 10, learning_rate = 0.3, max_depth = 3,
                                reg_lambda = 1.0, gamma = 0.0, min_child_weight = 1.0, objective = "squared") {
  A <- as.matrix(X)
  t <- as.numeric(y)
  obj <- tolower(objective)
  M <- as.integer(n_estimators)
  eta <- as.numeric(learning_rate)
  depth <- as.integer(max_depth)
  lam <- as.numeric(reg_lambda)
  gam <- as.numeric(gamma)
  mcw <- as.numeric(min_child_weight)

  if (obj == "squared") {
    base <- mean(t)
    Fv <- rep(base, length(t))
  } else {
    p <- min(max(mean(t), 1e-6), 1 - 1e-6)
    base <- log(p / (1 - p))
    Fv <- rep(base, length(t))
  }

  loss_fn <- function(Fv) {
    if (obj == "squared") {
      return(mean(0.5 * (Fv - t)^2))
    }
    z <- pmin(pmax(Fv, -50), 50)
    mean(log1p(exp(z)) - t * z)
  }

  trees <- list()
  losses <- loss_fn(Fv)
  importance <- numeric(ncol(A))
  for (m in seq_len(M)) {
    if (obj == "squared") {
      g <- Fv - t
      h <- rep(1.0, length(t))
    } else {
      p <- 1.0 / (1.0 + exp(-pmin(pmax(Fv, -50), 50)))
      g <- p - t
      h <- pmax(p * (1.0 - p), 1e-12)
    }
    tree <- .morie_w4d_xgb_build(A, g, h, 0, depth, lam, gam, mcw)
    Fv <- Fv + eta * .morie_w4d_xgb_predict(tree, A)
    trees[[m]] <- tree
    losses <- c(losses, loss_fn(Fv))

    stack <- list(tree)
    while (length(stack) > 0) {
      nd <- stack[[length(stack)]]
      stack[[length(stack)]] <- NULL
      if (!nd$leaf) {
        importance[nd$feature + 1] <- importance[nd$feature + 1] + nd$gain
        stack <- c(stack, list(nd$left, nd$right))
      }
    }
  }

  pred <- if (obj == "squared") Fv else 1.0 / (1.0 + exp(-pmin(pmax(Fv, -50), 50)))

  list(
    predicted = pred, raw_score = Fv, trees = trees, base_score = base, loss_curve = losses,
    feature_importance = importance, objective = obj, estimate = losses[length(losses)], n = length(t),
    method = "XGBoost: exact greedy splits scored by the second-order gain, shrunk by eta"
  )
}

# ---------------------------------------------------------------------
# hmxgr: exploding gradients diagnosis (mirrors hmvgr, shares layer_norms)
# ---------------------------------------------------------------------

#' Exploding gradients: gradients grow through layers
#'
#' Mirror of \code{morie_geron_vanishing_gradients}: geometric-mean
#' amplification per layer towards the input; optional global-norm
#' clipping g <- g * clip_norm / ||g||_global.
#'
#' @param grads List of >= 2 per-layer gradients, input side first.
#' @param tol Amplification threshold (> 1).
#' @param clip_norm Optional clipping threshold (> 0).
#' @return list with norms, ratios, geometric_ratio, amplification, exploding,
#'   global_norm, clipped, scale, tol, estimate, n, method.
#' @export
morie_geron_exploding_gradients <- function(grads, tol = 2.0, clip_norm = NULL) {
  norms <- morie_geron_layer_norms(grads)
  t <- as.numeric(tol)

  ratios <- norms[-1] / norms[-length(norms)]
  geo <- exp(mean(log(norms[-length(norms)] / norms[-1])))
  amp <- norms[1] / norms[length(norms)]
  global_norm <- sqrt(sum(norms * norms))

  clipped <- NULL
  scale <- 1.0
  if (!is.null(clip_norm)) {
    c_ <- as.numeric(clip_norm)
    scale <- min(1.0, c_ / global_norm)
    clipped <- lapply(grads, function(g) as.numeric(g) * scale)
  }

  list(
    norms = norms, ratios = ratios, geometric_ratio = geo, amplification = amp, exploding = geo > t,
    global_norm = global_norm, clipped = clipped, scale = scale, tol = t, estimate = geo, n = length(norms),
    method = "Per-layer gradient norms with geometric amplification and optional global-norm clipping"
  )
}

# ---------------------------------------------------------------------
# hmxln: XLNet (permutation-based autoregressive pretraining)
# ---------------------------------------------------------------------

#' Two-stream attention masks for a factorisation order
#'
#' content\[t,j\]=1 when j precedes-or-equals t in perm (0-based perm);
#' query\[t,j\]=1 only for strictly earlier positions.
#'
#' @param perm 0-based permutation vector (length T).
#' @return list(content, query), each a T x T 0/1 matrix.
#' @export
morie_geron_permutation_masks <- function(perm) {
  p <- as.integer(perm)
  Tn <- length(p)
  rank_ <- integer(Tn)
  rank_[p + 1] <- 0:(Tn - 1)
  content <- matrix(0.0, Tn, Tn)
  query <- matrix(0.0, Tn, Tn)
  for (t in 0:(Tn - 1)) {
    for (j in 0:(Tn - 1)) {
      if (rank_[j + 1] <= rank_[t + 1]) content[t + 1, j + 1] <- 1.0
      if (rank_[j + 1] < rank_[t + 1]) query[t + 1, j + 1] <- 1.0
    }
  }
  list(content = content, query = query)
}

#' XLNet: permutation-based autoregressive pretraining
#'
#' Draws a factorisation order from an LCG Fisher-Yates shuffle, builds
#' two-stream masks (\code{morie_geron_permutation_masks}), scores each
#' conditional from the visible (query-stream) context under an
#' embedding-based softmax head.
#'
#' @param X Integer (0-based) token ids, length >= 2.
#' @param n_layers,vocab_size,d_model,seed As in Python original.
#' @return list with permutation, content_mask, query_mask, logprobs,
#'   total_logprob, perplexity, embeddings, n_layers, estimate, n, method.
#' @export
morie_geron_xlnet <- function(X, n_layers = 1, vocab_size = NULL, d_model = 8, seed = 0) {
  x <- as.integer(X)
  V <- if (is.null(vocab_size)) max(x) + 1L else as.integer(vocab_size)
  d <- as.integer(d_model)
  L <- as.integer(n_layers)
  Tn <- length(x)

  s <- as.numeric(seed) %% 2^32
  u_draw <- function() {
    s <<- (1664525 * s + 1013904223) %% 2^32
    (s + 0.5) / 2^32
  }

  perm <- 0:(Tn - 1)
  if (Tn > 1) {
    for (i in Tn:2) {
      j <- as.integer(u_draw() * i) + 1L
      tmp <- perm[i]
      perm[i] <- perm[j]
      perm[j] <- tmp
    }
  }
  pm <- morie_geron_permutation_masks(perm)
  content <- pm$content
  query <- pm$query

  Emat <- matrix(0.0, V, d)
  for (v in 0:(V - 1)) for (k in 1:d) Emat[v + 1, k] <- 2.0 * u_draw() - 1.0
  Wout <- matrix(0.0, d, V)
  for (k in 1:d) for (v in 0:(V - 1)) Wout[k, v + 1] <- 2.0 * u_draw() - 1.0

  logps <- numeric(Tn)
  for (t in 0:(Tn - 1)) {
    vis <- query[t + 1, ] > 0
    ctx <- if (any(vis)) colMeans(Emat[x[vis] + 1, , drop = FALSE]) else numeric(d)
    logits <- as.numeric((ctx * L) %*% Wout)
    p <- .morie_gr_softmax(logits)
    logps[t + 1] <- log(max(p[x[t + 1] + 1], .Machine$double.xmin))
  }
  total <- sum(logps)

  list(
    permutation = perm, content_mask = content, query_mask = query, logprobs = logps, total_logprob = total,
    perplexity = exp(-total / Tn), embeddings = Emat, n_layers = L, estimate = total, n = Tn,
    method = "Permutation LM: two-stream masks over a sampled factorisation order with softmax conditionals"
  )
}

# ---------------------------------------------------------------------
# hmyolo: YOLO single-shot detection (grid decode + greedy per-class NMS)
# ---------------------------------------------------------------------

#' Intersection over union of two (x1,y1,x2,y2) boxes
#' @param a,b Numeric length-4 vectors.
#' @return Scalar IoU.
#' @export
morie_geron_box_iou <- function(a, b) {
  iw <- max(0.0, min(a[3], b[3]) - max(a[1], b[1]))
  ih <- max(0.0, min(a[4], b[4]) - max(a[2], b[2]))
  inter <- iw * ih
  ua <- (a[3] - a[1]) * (a[4] - a[2]) + (b[3] - b[1]) * (b[4] - b[2]) - inter
  if (ua > 0) inter / ua else 0.0
}

#' YOLO: single-shot object detection via grid regression
#'
#' Decodes an (S,S,B*5+C) prediction tensor (tx,ty,tw,th,conf per box,
#' shared class probs per cell), score = conf*class_prob, greedy
#' per-class NMS at iou_threshold.
#'
#' @param image Passed to model unchanged.
#' @param model Function(image) -> array (S,S,B*5+C).
#' @param n_boxes,conf_threshold,iou_threshold As in Python original.
#' @return list with boxes, scores, classes (0-based), n_detections, n_candidates,
#'   suppressed, grid, n_classes, estimate, n, method.
#' @export
morie_geron_yolo <- function(image, model, n_boxes = 1, conf_threshold = 0.5, iou_threshold = 0.45) {
  B <- as.integer(n_boxes)
  ct <- as.numeric(conf_threshold)
  it <- as.numeric(iou_threshold)

  P <- as.array(model(image))
  S <- dim(P)[1]
  C <- dim(P)[3] - 5 * B

  cand <- list()
  for (i in 0:(S - 1)) {
    for (j in 0:(S - 1)) {
      cls <- P[i + 1, j + 1, (5 * B + 1):(5 * B + C)]
      k <- which.max(cls) - 1L
      for (b in 0:(B - 1)) {
        vals <- P[i + 1, j + 1, (5 * b + 1):(5 * b + 5)]
        tx <- vals[1]
        ty <- vals[2]
        tw <- vals[3]
        th <- vals[4]
        conf <- vals[5]
        score <- conf * cls[k + 1]
        if (score < ct || tw <= 0 || th <= 0) next
        cx <- (j + tx) / S
        cy <- (i + ty) / S
        box <- c(cx - tw / 2, cy - th / 2, cx + tw / 2, cy + th / 2)
        cand[[length(cand) + 1]] <- list(score = score, k = k, box = box)
      }
    }
  }

  ord <- order(-sapply(cand, `[[`, "score"))
  cand <- cand[ord]
  keep <- list()
  for (c_ in cand) {
    ok <- TRUE
    for (kk in keep) {
      if (kk$k == c_$k && morie_geron_box_iou(c_$box, kk$box) > it) {
        ok <- FALSE
        break
      }
    }
    if (ok) keep[[length(keep) + 1]] <- c_
  }

  n_keep <- length(keep)
  boxes <- if (n_keep > 0) t(sapply(keep, `[[`, "box")) else matrix(numeric(0), 0, 4)
  scores <- if (n_keep > 0) sapply(keep, `[[`, "score") else numeric(0)
  classes <- if (n_keep > 0) sapply(keep, `[[`, "k") else integer(0)
  if (n_keep > 0) {
    ord2 <- order(classes, -scores)
    boxes <- boxes[ord2, , drop = FALSE]
    scores <- scores[ord2]
    classes <- classes[ord2]
  }

  list(
    boxes = boxes, scores = scores, classes = classes, n_detections = n_keep, n_candidates = length(cand),
    suppressed = length(cand) - n_keep, grid = S, n_classes = C,
    estimate = if (n_keep > 0) max(scores) else 0.0, n = S * S * B,
    method = "Grid decode of (x, y, w, h, conf) + class scores, then greedy per-class NMS"
  )
}

# ---------------------------------------------------------------------
# hmzsl: zero-shot classification (softmax scoring + null-prompt calibration)
# ---------------------------------------------------------------------

#' Zero-shot learning: LLM generalizes to unseen tasks from prompt only
#'
#' Scores labels under model(prompt, labels) or model(prompt) with no
#' weight updates; softmax-normalises. If null_prompt given, subtracts
#' the null-prompt scores before normalising (contextual calibration).
#'
#' @param model Function(prompt) or function(prompt, labels) -> named list/numeric scores.
#' @param prompt Passed to model unchanged.
#' @param labels Optional candidate labels (>=2).
#' @param null_prompt Optional content-free prompt for calibration.
#' @return list with probabilities, scores, raw_scores, labels, predicted (0-based),
#'   predicted_label, margin, entropy, calibrated, estimate, n, method.
#' @export
morie_geron_zero_shot <- function(model, prompt, labels = NULL, null_prompt = NULL) {
  score_fn <- function(p) {
    out <- if (!is.null(labels)) tryCatch(model(p, labels), error = function(e) model(p)) else model(p)
    if (!is.null(names(out)) && all(nzchar(names(out)))) {
      list(s = as.numeric(out), keys = names(out))
    } else {
      list(s = as.numeric(out), keys = NULL)
    }
  }

  r0 <- score_fn(prompt)
  s <- r0$s
  names_ <- if (!is.null(labels)) as.character(labels) else r0$keys

  calibrated <- FALSE
  raw <- s
  if (!is.null(null_prompt)) {
    r1 <- score_fn(null_prompt)
    s <- s - r1$s
    calibrated <- TRUE
  }

  p <- .morie_gr_softmax(s)
  ord <- order(-p)
  k <- ord[1] - 1L # 0-based
  margin <- p[ord[1]] - p[ord[2]]
  ent <- -sum(p * log(pmax(p, .Machine$double.xmin)))

  list(
    probabilities = p, scores = s, raw_scores = raw, labels = names_, predicted = k,
    predicted_label = names_[k + 1], margin = margin, entropy = ent, calibrated = calibrated,
    estimate = p[k + 1], n = length(names_),
    method = paste0(
      "Zero-shot label scoring with softmax normalisation",
      if (calibrated) " and null-prompt contextual calibration" else ""
    )
  )
}

# ---------------------------------------------------------------------
# hmspcl: spectral clustering (Laplacian eigenvectors + Lloyd)
# ---------------------------------------------------------------------

#' Spectral clustering: eigenvectors of graph Laplacian
#'
#' Builds an RBF or symmetric k-NN affinity graph, forms the
#' unnormalised Laplacian L = D - W, embeds via its n_clusters smallest
#' eigenvectors, k-means (Lloyd/k-means++ on an LCG stream) in that
#' embedding.
#'
#' @param X Data (n, d).
#' @param n_clusters,affinity,gamma,n_neighbors,seed As in Python original.
#' @return list with labels, embedding, centers, eigenvalues, affinity_matrix,
#'   laplacian, n_components, estimate, n, method.
#' @export
morie_geron_spectral_clustering <- function(X, n_clusters = 2, affinity = "rbf", gamma = 1.0,
                                            n_neighbors = 3, seed = 0) {
  A <- as.matrix(X)
  n <- nrow(A)
  k <- as.integer(n_clusters)
  aff <- tolower(affinity)
  g <- as.numeric(gamma)

  D2 <- matrix(0.0, n, n)
  for (i in seq_len(n)) D2[i, ] <- rowSums(sweep(A, 2, A[i, ], "-")^2)

  if (aff == "rbf") {
    W <- exp(-g * D2)
  } else {
    nb <- as.integer(n_neighbors)
    W <- matrix(0.0, n, n)
    for (i in seq_len(n)) {
      ord <- order(D2[i, ])[2:(nb + 1)]
      W[i, ord] <- 1.0
    }
    W <- pmax(W, t(W))
  }
  diag(W) <- 0.0

  Lap <- diag(rowSums(W)) - W
  eg <- eigen(Lap, symmetric = TRUE)
  vals <- rev(eg$values)
  vecs <- eg$vectors[, rev(seq_len(ncol(eg$vectors)))] # ascending order to match np.linalg.eigh
  tol <- 1e-8 * max(1.0, max(abs(Lap)))
  n_comp <- sum(vals < tol)
  U <- vecs[, seq_len(k), drop = FALSE]
  lloyd <- .morie_w4d_lloyd(U, k, seed = seed)
  labels <- lloyd$labels
  centers <- lloyd$centers

  eigengap <- if (k < n) vals[k + 1] - vals[k] else NA_real_

  list(
    labels = labels, embedding = U, centers = centers, eigenvalues = vals, affinity_matrix = W,
    laplacian = Lap, n_components = n_comp, estimate = if (k < n) eigengap else 0.0, n = n,
    method = paste0("Unnormalised Laplacian spectral clustering (", aff, " affinity) + Lloyd k-means in the embedding")
  )
}

# ---------------------------------------------------------------------
# hmsrnn: simple (Elman) RNN forward pass
# ---------------------------------------------------------------------

#' Simple RNN forward pass over a sequence
#'
#' h_t = tanh(W_x x_t + W_h `h_{t-1}` + b), unrolled step by step.
#'
#' @param X Sequence (T, n_inputs).
#' @param Wx Input weights (n_inputs, n_units).
#' @param Wh Recurrent weights (n_units, n_units).
#' @param b Optional bias; default zeros.
#' @param h0 Optional initial hidden state; default zeros.
#' @return list with H, h_T, grads, jacobian_gain, n_units, estimate, n, method.
#' @export
morie_geron_simple_rnn <- function(X, Wx, Wh, b = NULL, h0 = NULL) {
  Xa <- as.matrix(X)
  Wxa <- as.matrix(Wx)
  Wha <- as.matrix(Wh)
  n_units <- ncol(Wxa)
  bias <- if (is.null(b)) numeric(n_units) else as.numeric(b)
  h <- if (is.null(h0)) numeric(n_units) else as.numeric(h0)

  Tn <- nrow(Xa)
  H <- matrix(0.0, Tn, n_units)
  G <- matrix(0.0, Tn, n_units)
  for (t in seq_len(Tn)) {
    z <- as.numeric(Xa[t, ] %*% Wxa) + as.numeric(h %*% Wha) + bias
    h <- tanh(z)
    H[t, ] <- h
    G[t, ] <- 1.0 - h * h
  }
  wh_spec <- svd(Wha)$d[1]
  gain <- if (Tn > 1) prod(apply(G, 1, max)) * wh_spec^(Tn - 1) else max(G)

  list(
    H = H, h_T = H[Tn, ], grads = G, jacobian_gain = gain, n_units = n_units,
    estimate = mean(H[Tn, ]), n = Tn,
    method = "Simple (Elman) RNN unrolled with tanh states from hmtanh"
  )
}

# ---------------------------------------------------------------------
# hmsrp: sparse random projection (Achlioptas/Li)
# ---------------------------------------------------------------------

#' Sparse random projection matrix with `{-1,0,+1}` entries
#'
#' R_ij = +-sqrt(s/d_out) with prob 1/(2s) each (s = 1/density), else 0.
#' Measures realised pairwise-distance distortion.
#'
#' @param X Data (n, d_in).
#' @param d_out Target dimension.
#' @param density Optional non-zero fraction in (0,1\]; default 1/sqrt(d_in).
#' @param seed LCG seed.
#' @return list with X_proj, R, density, s, scale, nnz, max_distortion,
#'   mean_distortion, estimate, n, method.
#' @export
morie_geron_sparse_rand_projection <- function(X, d_out, density = NULL, seed = 0) {
  A <- as.matrix(X)
  n <- nrow(A)
  d_in <- ncol(A)
  k <- as.integer(d_out)
  dens <- if (is.null(density)) 1.0 / sqrt(d_in) else as.numeric(density)

  s_ <- 1.0 / dens
  scale <- sqrt(s_ / k)
  rng <- as.numeric(seed) %% 2^32
  R <- matrix(0.0, d_in, k)
  for (i in seq_len(d_in)) {
    for (j in seq_len(k)) {
      rng <- (1664525 * rng + 1013904223) %% 2^32
      u <- (rng + 0.5) / 2^32
      if (u < 0.5 * dens) {
        R[i, j] <- scale
      } else if (u < dens) R[i, j] <- -scale
    }
  }
  Xp <- A %*% R

  max_dist <- NA_real_
  mean_dist <- NA_real_
  if (n >= 2) {
    d0 <- numeric(0)
    d1 <- numeric(0)
    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        a <- sqrt(sum((A[i, ] - A[j, ])^2))
        if (a > 0) {
          d0 <- c(d0, a)
          d1 <- c(d1, sqrt(sum((Xp[i, ] - Xp[j, ])^2)))
        }
      }
    }
    if (length(d0) > 0) {
      ratio <- d1 / d0
      max_dist <- max(abs(ratio - 1.0))
      mean_dist <- mean(abs(ratio - 1.0))
    }
  }

  list(
    X_proj = Xp, R = R, density = dens, s = s_, scale = scale, nnz = sum(R != 0),
    max_distortion = max_dist, mean_distortion = mean_dist, estimate = max_dist, n = n,
    method = "Achlioptas/Li sparse random projection with +/-sqrt(s/d_out) entries"
  )
}

# ---------------------------------------------------------------------
# hmssg: semantic segmentation (per-pixel argmax + IoU scoring)
# ---------------------------------------------------------------------

#' Semantic segmentation: per-pixel class labels
#'
#' Per-pixel argmax over model(image) -> (H,W,K) scores; when y_true is
#' given, per-class IoU = |P_k & G_k| / |P_k | G_k|, mean over classes
#' present, pixel accuracy, confusion matrix.
#'
#' @param image (H,W) or (H,W,C) array, passed to model unchanged.
#' @param model Function(image) -> array (H,W,K) scores.
#' @param y_true Optional (H,W) integer (0-based) ground-truth labels.
#' @return list with labels, scores, class_counts, iou, mean_iou, pixel_accuracy,
#'   confusion, n_classes, estimate, n, method.
#' @export
morie_geron_semantic_segmentation <- function(image, model, y_true = NULL) {
  img <- as.array(image)
  Hh <- dim(img)[1]
  Ww <- dim(img)[2]

  scores <- as.array(model(image))
  K <- dim(scores)[3]

  labels <- matrix(0L, Hh, Ww)
  for (i in seq_len(Hh)) for (j in seq_len(Ww)) labels[i, j] <- which.max(scores[i, j, ]) - 1L
  counts <- sapply(0:(K - 1), function(k) sum(labels == k))

  iou <- NULL
  mean_iou <- NULL
  acc <- NULL
  conf <- NULL
  if (!is.null(y_true)) {
    G <- matrix(as.integer(y_true), Hh, Ww)
    conf <- matrix(0L, K, K)
    for (i in seq_len(Hh)) for (j in seq_len(Ww)) conf[G[i, j] + 1, labels[i, j] + 1] <- conf[G[i, j] + 1, labels[i, j] + 1] + 1L
    inter <- diag(conf)
    union_ <- rowSums(conf) + colSums(conf) - diag(conf)
    iou <- ifelse(union_ > 0, inter / ifelse(union_ > 0, union_, 1), NA_real_)
    present <- union_ > 0
    mean_iou <- mean(iou[present])
    acc <- sum(diag(conf)) / sum(conf)
  }

  list(
    labels = labels, scores = scores, class_counts = counts, iou = iou, mean_iou = mean_iou,
    pixel_accuracy = acc, confusion = conf, n_classes = K,
    estimate = if (!is.null(mean_iou)) mean_iou else max(counts) / (Hh * Ww), n = Hh * Ww,
    method = "Per-pixel argmax with per-class IoU / pixel accuracy against ground truth"
  )
}

# ---------------------------------------------------------------------
# hmtsf: time series forecasting (lag-window ridge regression)
# ---------------------------------------------------------------------

#' Time series forecasting with a fixed-width lag window
#'
#' Supervised windowing (`y_{t-w+1..t}`) -> `y_{t+1}`, ridge-regularised
#' normal equations (min-norm solve), rolled forward recursively or
#' fit directly per horizon step.
#'
#' @param y Univariate series.
#' @param horizon Steps ahead.
#' @param window Lag width.
#' @param ridge L2 penalty (not on intercept).
#' @param recursive Iterate one-step model vs direct per-step fit.
#' @return list with forecast, coef, intercept, train_mse, naive_mse, skill,
#'   window, horizon, estimate, n, method.
#' @export
morie_geron_time_series_forecast <- function(y, horizon = 1, window = 3, ridge = 0.0, recursive = TRUE) {
  s <- as.numeric(y)
  w <- as.integer(window)
  h <- as.integer(horizon)
  lam <- as.numeric(ridge)

  fit_lead <- function(lead) {
    rows <- length(s) - w - lead + 1
    Amat <- matrix(0.0, rows, w + 1)
    t <- numeric(rows)
    for (i in seq_len(rows)) {
      Amat[i, 1:w] <- s[i:(i + w - 1)]
      Amat[i, w + 1] <- 1.0
      t[i] <- s[i + w + lead - 1]
    }
    P <- diag(lam, w + 1)
    P[w + 1, w + 1] <- 0.0
    M <- t(Amat) %*% Amat + P
    beta <- as.numeric(MASS::ginv(M) %*% (t(Amat) %*% t))
    list(beta = beta, A = Amat, t = t)
  }

  if (isTRUE(recursive)) {
    ft <- fit_lead(1)
    beta <- ft$beta
    Amat <- ft$A
    t <- ft$t
    hist <- s[(length(s) - w + 1):length(s)]
    fc <- numeric(h)
    for (i in seq_len(h)) {
      nxt <- sum(beta[1:w] * hist[(length(hist) - w + 1):length(hist)]) + beta[w + 1]
      fc[i] <- nxt
      hist <- c(hist, nxt)
    }
    coef <- beta[1:w]
    intercept <- beta[w + 1]
    resid <- as.numeric(Amat %*% beta) - t
    train_mse <- sum(resid * resid) / length(resid)
  } else {
    fc <- numeric(h)
    coef <- list()
    train_sse <- 0.0
    train_n <- 0
    for (lead in seq_len(h)) {
      ft <- fit_lead(lead)
      beta <- ft$beta
      Amat <- ft$A
      t <- ft$t
      fc[lead] <- sum(beta[1:w] * s[(length(s) - w + 1):length(s)]) + beta[w + 1]
      coef[[lead]] <- beta[1:w]
      r <- as.numeric(Amat %*% beta) - t
      train_sse <- train_sse + sum(r * r)
      train_n <- train_n + length(r)
    }
    coef <- do.call(rbind, coef)
    intercept <- NULL
    train_mse <- train_sse / train_n
  }

  naive <- if (length(s) > w) mean((s[(w + 1):length(s)] - s[w:(length(s) - 1)])^2) else NA_real_
  skill <- if (!is.na(naive) && naive > 0) 1.0 - train_mse / naive else NA_real_

  list(
    forecast = fc, coef = coef, intercept = intercept, train_mse = train_mse, naive_mse = naive, skill = skill,
    window = w, horizon = h, estimate = fc[length(fc)], n = length(s),
    method = paste0(if (recursive) "Recursive" else "Direct", " lag-window linear forecast (ridge normal equations)")
  )
}

# ---------------------------------------------------------------------
# hmtsne: t-SNE (perplexity-calibrated affinities, Student-t Q, KL descent)
# ---------------------------------------------------------------------

#' Row-wise Gaussian affinities with sigma solved to hit a target perplexity
#'
#' Binary search on beta = 1/(2 sigma^2) per row until row entropy equals log(perplexity).
#'
#' @param D2 Squared-distance matrix (n, n).
#' @param perplexity Target perplexity.
#' @param tol Entropy tolerance.
#' @param max_steps Binary-search iterations.
#' @return list(P, betas).
#' @export
morie_geron_conditional_p <- function(D2, perplexity, tol = 1e-5, max_steps = 100) {
  n <- nrow(D2)
  P <- matrix(0.0, n, n)
  target <- log(perplexity)
  betas <- rep(1.0, n)
  for (i in seq_len(n)) {
    lo <- -Inf
    hi <- Inf
    beta <- 1.0
    idx <- setdiff(seq_len(n), i)
    Di <- D2[i, idx]
    Pi <- NULL
    for (step in seq_len(max_steps)) {
      Pi <- exp(-Di * beta)
      s_ <- sum(Pi)
      if (s_ <= 0) {
        H <- 0.0
        Pi <- rep(1.0 / length(Pi), length(Pi))
      } else {
        Pi <- Pi / s_
        nz <- Pi > 0
        H <- -sum(Pi[nz] * log(Pi[nz]))
      }
      if (abs(H - target) < tol) break
      if (H > target) {
        lo <- beta
        beta <- if (hi == Inf) beta * 2 else (beta + hi) / 2
      } else {
        hi <- beta
        beta <- if (lo == -Inf) beta / 2 else (beta + lo) / 2
      }
    }
    P[i, idx] <- Pi
    betas[i] <- beta
  }
  list(P = P, betas = betas)
}

#' t-SNE: KL divergence between joint probabilities in high- and low-dim
#'
#' High-d affinities: perplexity-calibrated Gaussians, symmetrised
#' p_ij=(p_j|i+p_i|j)/2n. Low-d: Student-t kernel q_ij ~ (1+||yi-yj||^2)^-1.
#' KL(P||Q) minimised by momentum gradient descent.
#'
#' @param X Data (n, d), n >= 3.
#' @param n_components,perplexity,seed,n_iter,lr,momentum As in Python original.
#' @return list with embedding, P, Q, kl, kl_curve, betas, perplexity, estimate, n, method.
#' @export
morie_geron_tsne <- function(X, n_components = 2, perplexity = 5.0, seed = 0, n_iter = 300,
                             lr = NULL, momentum = 0.8) {
  A <- as.matrix(X)
  n <- nrow(A)
  k <- as.integer(n_components)
  perp <- as.numeric(perplexity)
  it <- as.integer(n_iter)
  eta <- if (is.null(lr)) max(4.0, n / 12.0) else as.numeric(lr)
  mom <- as.numeric(momentum)

  D2 <- matrix(0.0, n, n)
  for (i in seq_len(n)) D2[i, ] <- rowSums(sweep(A, 2, A[i, ], "-")^2)
  cp <- morie_geron_conditional_p(D2, perp)
  Pc <- cp$P
  betas <- cp$betas
  P <- (Pc + t(Pc)) / (2.0 * n)
  diag(P) <- 0.0

  s <- as.numeric(seed) %% 2^32
  flat <- numeric(n * k)
  for (i in seq_len(n * k)) {
    s <- (1664525 * s + 1013904223) %% 2^32
    flat[i] <- (2.0 * ((s + 0.5) / 2^32) - 1.0) * 1e-2
  }
  Y <- matrix(flat, n, k, byrow = TRUE)
  Vm <- matrix(0.0, n, k)

  kls <- numeric(it)
  for (iter in seq_len(it)) {
    num <- matrix(0.0, n, n)
    for (i in seq_len(n)) num[i, ] <- 1.0 / (1.0 + rowSums(sweep(Y, 2, Y[i, ], "-")^2))
    diag(num) <- 0.0
    Q <- pmax(num / sum(num), 1e-12)
    diag(Q) <- 0.0
    mask <- P > 0
    kls[iter] <- sum(P[mask] * log(P[mask] / Q[mask]))
    W <- (P - Q) * num
    grad <- 4.0 * ((diag(rowSums(W)) - W) %*% Y)
    Vm <- mom * Vm - eta * grad
    Y <- Y + Vm
  }

  num <- matrix(0.0, n, n)
  for (i in seq_len(n)) num[i, ] <- 1.0 / (1.0 + rowSums(sweep(Y, 2, Y[i, ], "-")^2))
  diag(num) <- 0.0
  Q <- pmax(num / sum(num), 1e-12)
  diag(Q) <- 0.0
  mask <- P > 0
  kl <- sum(P[mask] * log(P[mask] / Q[mask]))
  kls <- c(kls, kl)

  list(
    embedding = Y, P = P, Q = Q, kl = kl, kl_curve = kls, betas = betas, perplexity = perp,
    estimate = kl, n = n,
    method = "t-SNE: perplexity-calibrated Gaussian P, Student-t Q, KL minimised by gradient descent with momentum"
  )
}

# ---------------------------------------------------------------------
# hmumap: UMAP (fuzzy kNN graph + fitted (a,b) kernel + CE descent)
# ---------------------------------------------------------------------

#' Fit (a, b) so 1/(1 + a d^(2b)) matches UMAP's target curve
#'
#' Deterministic grid search on log(a) and b (no optimiser dependency).
#'
#' @param min_dist Minimum spacing.
#' @param spread Spread parameter.
#' @return list(a, b, sse).
#' @export
morie_geron_fit_ab <- function(min_dist, spread = 1.0) {
  d <- seq(0.0, 3.0 * spread, length.out = 300)
  target <- ifelse(d <= min_dist, 1.0, exp(-(d - min_dist) / spread))
  best <- NULL
  for (la in seq(-3.0, 3.0, length.out = 121)) {
    a <- exp(la)
    for (b in seq(0.25, 3.0, length.out = 56)) {
      v <- 1.0 / (1.0 + a * pmax(d, 1e-12)^(2 * b))
      sse <- sum((v - target)^2)
      if (is.null(best) || sse < best$sse) best <- list(sse = sse, a = a, b = b)
    }
  }
  list(a = best$a, b = best$b, sse = best$sse)
}

#' UMAP: uniform manifold approximation, preserves local and some global structure
#'
#' Smoothed-kNN fuzzy graph (rho = nearest-neighbour distance, sigma
#' solved by binary search to hit log2(k)), fuzzy union W = A+A'-A.A',
#' low-d kernel v = 1/(1+a*d^2b) fitted via \code{morie_geron_fit_ab},
#' exact fuzzy cross-entropy minimised by gradient descent (gradient
#' clipped at norm 4).
#'
#' @param X Data (n, d), n >= 3.
#' @param n_components,n_neighbors,min_dist,seed,n_iter,lr As in Python original.
#' @return list with embedding, graph, directed_graph, a, b, ab_sse, cross_entropy,
#'   ce_curve, rho, sigma, estimate, n, method.
#' @export
morie_geron_umap <- function(X, n_components = 2, n_neighbors = 3, min_dist = 0.1, seed = 0,
                             n_iter = 300, lr = 0.1) {
  A <- as.matrix(X)
  n <- nrow(A)
  k <- as.integer(n_neighbors)
  m <- as.integer(n_components)
  md <- as.numeric(min_dist)
  it <- as.integer(n_iter)
  eta <- as.numeric(lr)

  D <- matrix(0.0, n, n)
  for (i in seq_len(n)) D[i, ] <- sqrt(rowSums(sweep(A, 2, A[i, ], "-")^2))
  target <- log2(k)
  P <- matrix(0.0, n, n)
  rho <- numeric(n)
  sig <- numeric(n)
  for (i in seq_len(n)) {
    ord <- order(D[i, ])[2:(k + 1)]
    di <- D[i, ord]
    rho[i] <- di[1]
    lo <- 0.0
    hi <- Inf
    s_ <- 1.0
    w_ <- NULL
    for (step in 1:64) {
      w_ <- exp(-pmax(di - rho[i], 0.0) / s_)
      tot <- sum(w_)
      if (abs(tot - target) < 1e-5) break
      if (tot > target) {
        hi <- s_
        s_ <- (lo + hi) / 2
      } else {
        lo <- s_
        s_ <- if (hi == Inf) s_ * 2 else (lo + hi) / 2
      }
    }
    sig[i] <- s_
    P[i, ord] <- exp(-pmax(di - rho[i], 0.0) / s_)
  }
  W <- P + t(P) - P * t(P)
  diag(W) <- 0.0
  W <- pmin(pmax(W, 0.0), 1.0 - 1e-9)

  fab <- morie_geron_fit_ab(md)
  a <- fab$a
  b <- fab$b

  s0 <- as.numeric(seed) %% 2^32
  flat <- numeric(n * m)
  for (i in seq_len(n * m)) {
    s0 <- (1664525 * s0 + 1013904223) %% 2^32
    flat[i] <- (2.0 * ((s0 + 0.5) / 2^32) - 1.0)
  }
  Y <- matrix(flat, n, m, byrow = TRUE)

  eps <- 1e-9
  ces <- numeric(0)
  off <- !diag(TRUE, n)
  for (iter in seq_len(it + 1)) {
    s2 <- matrix(0.0, n, n)
    for (i in seq_len(n)) s2[i, ] <- rowSums(sweep(Y, 2, Y[i, ], "-")^2)
    s2 <- pmax(s2, eps)
    v <- 1.0 / (1.0 + a * s2^b)
    v <- pmin(pmax(v, eps), 1.0 - eps)
    ce <- sum(W[off] * log(W[off] / v[off] + eps) + (1 - W[off]) * log((1 - W[off]) / (1 - v[off])))
    ces <- c(ces, ce)
    if (length(ces) > it) break
    dce_dv <- -W / v + (1.0 - W) / (1.0 - v)
    dv_ds <- -a * b * s2^(b - 1.0) / (1.0 + a * s2^b)^2
    coef <- dce_dv * dv_ds
    diag(coef) <- 0.0
    grad <- 2.0 * ((diag(rowSums(coef)) - coef) %*% Y)
    gn <- max(abs(grad))
    if (gn > 4.0) grad <- grad * (4.0 / gn)
    Y <- Y - eta * grad
  }

  list(
    embedding = Y, graph = W, directed_graph = P, a = a, b = b, ab_sse = fab$sse,
    cross_entropy = ces[length(ces)], ce_curve = ces, rho = rho, sigma = sig,
    estimate = ces[length(ces)], n = n,
    method = "UMAP: smoothed kNN fuzzy graph, fitted (a, b) low-d kernel, exact fuzzy cross-entropy by gradient descent"
  )
}
