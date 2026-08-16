# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Geron training shelf, wave 5b. Mirrors 161 morie.fn gr* modules from
# Geron A, Hands-On Machine Learning with Scikit-Learn, Keras and
# TensorFlow (3rd ed., O'Reilly 2022).
#
# Conventions (identical to geron_ml_native.R, whose helpers this file
# reuses rather than duplicating -- .morie_gr_pvar, .morie_gr_psd,
# .morie_gr_softmax, .morie_gr_log_softmax, .morie_gr_log_softmax_rows,
# .morie_gr_lcg_u, .morie_gr_lcg_w, .morie_gr_init, .morie_gr_lstsq,
# .morie_gr_need, .morie_gr_array_split -- plus .morie_al_lcg and
# .morie_al_softmax_rows from alammar_llm_native.R):
#
#   * Indices Python returns 0-based (argmax, actions, token ids, batch
#     members, fold members, kept top-k positions) stay 0-based here.
#     Any index used to SUBSET an R object gets +1 internally.
#   * numpy's default var/std is the POPULATION form (ddof = 0). Where a
#     module asked for ddof = 1 (grfim spread, grgrp achieved_variance,
#     grhei achieved_variance, grgs std_scores) we use stats::var.
#   * numpy ravel/reshape are row-major: matrices built from a flat LCG
#     stream use byrow = TRUE, and flattening goes through t().
#   * %/% binds tighter than +/- in R, so every floor-division of a sum
#     is parenthesised.
#   * The LCG is drawn draw-for-draw in the Python order. Box-Muller
#     pairs consume two uniforms and emit cos then sin, so an odd count
#     still burns the whole pair.
#
# Shared cores, documented where they are reused:
#   * .morie_gr_mse_core  -- grmse; grlaso/grelas/grn011/grn013 stack the
#     penalties on top of it, grn007 differentiates it, greast/grmgd/
#     grlrnc iterate it.
#   * .morie_gr_attend    -- attsdp's kernel; grsdpa, grsa, grmha,
#     grflash (as its reference), grswin, grpvt, grpio, grteb, grtdb all
#     route through it.
#   * .morie_gr_softmax_vec (grn021) -- grsmxp, grxent, grxeng, grn024,
#     grtmp, grtop.
#   * .morie_gr_sigmoid_vec (grsig) -- grlogp, grlogc, grlogg, grovr,
#     grovo, grpels, grsen, grsnt, grstae.

# ------------------------------------------------------------ helpers

#' .morie_gr_fin
#'
#' A step of the geron_train_native implementation. Called by \code{.morie_gr_attend}, \code{.morie_gr_check_mdp}, \code{.morie_gr_mse_core} and 105 others in the module.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param x See Usage.
#' @param name See Usage.
#' @return Invisibly,a logical value.
#' @export
.morie_gr_fin <- function(x, name) {
  .morie_gr_need(all(is.finite(x)), paste0(name, " must be finite."))
  invisible(TRUE)
}

# np.atleast_2d: a vector becomes a ONE-ROW matrix.
#' Np.atleast_2d: a vector becomes a ONE-ROW matrix
#'
#' A step of the geron_train_native implementation. Called by \code{.morie_gr_attend}, \code{.morie_gr_check_mdp}, \code{.morie_gr_mse_core} and 71 others in the module.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param x A matrix; passed to \code{dim}.
#' @return The value of \code{m}, as built in the body.
#' @export
.morie_gr_a2d <- function(x) {
  if (is.matrix(x)) {
    m <- x
  } else if (is.array(x) && length(dim(x)) > 2L) {
    stop("expected a 1-D or 2-D object.", call. = FALSE)
  } else {
    m <- matrix(as.numeric(x), nrow = 1L)
  }
  storage.mode(m) <- "double"
  m
}

# Box-Muller normals off the reference LCG, cos then sin per pair.
#' Box-Muller normals off the reference LCG, cos then sin per pair
#'
#' A step of the geron_train_native implementation. Called by \code{morie_geron_ddpm_forward_process}, \code{morie_geron_ddpm_reverse_step}, \code{morie_geron_gaussian_random_projection} and 2 others in the module.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param count A count; the body uses it as \code{seq_len(...)}.
#' @param seed See Usage.
#' @param clamp A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return The value of \code{[}.
#' @export
.morie_gr_lcg_normals <- function(count, seed, clamp = FALSE) {
  count <- as.integer(count)
  if (count <= 0L) {
    return(numeric(0))
  }
  n_pairs <- (count + 1L) %/% 2L
  out <- numeric(2L * n_pairs)
  s <- as.numeric(seed) %% 2^32
  for (i in seq_len(n_pairs)) {
    s <- .morie_al_lcg(s)
    u1 <- (s + 0.5) / 2^32
    s <- .morie_al_lcg(s)
    u2 <- (s + 0.5) / 2^32
    if (clamp) u1 <- max(u1, 1e-12)
    rad <- sqrt(-2 * log(u1))
    out[2L * i - 1L] <- rad * cos(2 * pi * u2)
    out[2L * i] <- rad * sin(2 * pi * u2)
  }
  out[seq_len(count)]
}

# LCG Fisher-Yates over 0..n-1, exactly the Python loop (i from n-1 down).
#' LCG Fisher-Yates over 0..n-1, exactly the Python loop (i from n-1
#' down)
#'
#' A step of the geron_train_native implementation. Called by \code{morie_geron_dataloader_minibatch}, \code{morie_geron_kfold_cv}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param seed See Usage.
#' @return The value of \code{perm}, as built in the body.
#' @export
.morie_gr_lcg_perm <- function(n, seed) {
  n <- as.integer(n)
  perm <- seq_len(n) - 1L
  if (n < 2L) {
    return(perm)
  }
  s <- as.numeric(seed) %% 2^32
  for (i in seq.int(n - 1L, 1L)) {
    s <- .morie_al_lcg(s)
    u <- (s + 0.5) / 2^32
    j <- min(as.integer(u * (i + 1)), i)
    tmp <- perm[i + 1L]
    perm[i + 1L] <- perm[j + 1L]
    perm[j + 1L] <- tmp
  }
  perm
}

# grsig kernel: the two-branch overflow-safe logistic.
#' Grsig kernel: the two-branch overflow-safe logistic
#'
#' A step of the geron_train_native implementation. Called by \code{morie_geron_gru_cell}, \code{morie_geron_logistic_regression_probability}, \code{morie_geron_lstm_cell} and 6 others in the module.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param z A vector; its length is taken and its elements indexed.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_gr_sigmoid_vec <- function(z) {
  z <- as.numeric(z)
  out <- numeric(length(z))
  pos <- z >= 0
  out[pos] <- 1 / (1 + exp(-z[pos]))
  e <- exp(z[!pos])
  out[!pos] <- e / (1 + e)
  out
}

# grn021 kernel.
#' Grn021 kernel
#'
#' A step of the geron_train_native implementation. Called by \code{morie_geron_ch4_softmax_function}, \code{morie_geron_temperature_sampling}, \code{morie_geron_topk_sampling}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param s A vector; its length is taken.
#' @return A numeric value.
#' @export
.morie_gr_softmax_vec <- function(s) {
  s <- as.numeric(s)
  .morie_gr_need(length(s) > 0L, "score vector is empty.")
  .morie_gr_fin(s, "score vector")
  e <- exp(s - max(s))
  e / sum(e)
}

# attsdp / grsdpa kernel. mask: logical matrix, TRUE = attend.
#' Attsdp / grsdpa kernel. mask: logical matrix, TRUE = attend
#'
#' A step of the geron_train_native implementation. Called by \code{morie_geron_block_multi_head_attention}, \code{morie_geron_flash_attention_tile}, \code{morie_geron_multi_head_attention} and 5 others in the module.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param Q A matrix; passed to \code{ncol}.
#' @param K A matrix; passed to \code{nrow}.
#' @param V A matrix; passed to \code{nrow}.
#' @param mask Defaults to \code{NULL}.
#' @return A list with \code{output}, \code{weights}, \code{scores}.
#' @export
.morie_gr_attend <- function(Q, K, V, mask = NULL) {
  Q <- .morie_gr_a2d(Q)
  K <- .morie_gr_a2d(K)
  V <- .morie_gr_a2d(V)
  .morie_gr_need(ncol(Q) == ncol(K), "Q and K must share d_k.")
  .morie_gr_need(nrow(K) == nrow(V), "K and V must have the same rows.")
  .morie_gr_fin(Q, "Q")
  .morie_gr_fin(K, "K")
  .morie_gr_fin(V, "V")
  dk <- ncol(K)
  .morie_gr_need(dk > 0L, "d_k is zero.")
  scores <- Q %*% t(K) / sqrt(dk)
  if (!is.null(mask)) {
    keep <- matrix(as.logical(mask), nrow = nrow(scores))
    .morie_gr_need(all(dim(keep) == dim(scores)), "mask shape mismatch.")
    .morie_gr_need(all(rowSums(keep) > 0), "mask blocks every key.")
    scores[!keep] <- -Inf
  }
  W <- .morie_al_softmax_rows(scores)
  list(output = W %*% V, weights = W, scores = scores)
}

# grmse core, shared by every regularised cost and every GD driver here.
#' Grmse core, shared by every regularised cost and every GD driver here
#'
#' A step of the geron_train_native implementation. Called by \code{morie_geron_ch4_mse_gradient_vector}, \code{morie_geron_ch4_normal_equation}, \code{morie_geron_early_stopping} and 4 others in the module.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param y A vector; its length is taken.
#' @param theta A matrix; passed to \code{\%*\%}.
#' @return A list with \code{cost}, \code{rmse}, \code{residuals}, \code{predictions}, \code{n}.
#' @export
.morie_gr_mse_core <- function(X, y, theta) {
  X <- .morie_gr_a2d(X)
  y <- as.numeric(y)
  theta <- as.numeric(theta)
  m <- nrow(X)
  n <- ncol(X)
  .morie_gr_need(m > 0L, "X has no rows; MSE over zero instances is undefined.")
  .morie_gr_need(length(y) == m, "y length must equal nrow(X).")
  .morie_gr_need(length(theta) == n, "theta length must equal ncol(X).")
  .morie_gr_fin(X, "X")
  .morie_gr_fin(y, "y")
  .morie_gr_fin(theta, "theta")
  pred <- as.numeric(X %*% theta)
  resid <- pred - y
  cost <- mean(resid^2)
  list(
    cost = cost, rmse = sqrt(cost), residuals = resid,
    predictions = pred, n = m
  )
}

#' .morie_gr_logaddexp0
#'
#' A step of the geron_train_native implementation. Called by \code{morie_geron_dpo_loss}, \code{morie_geron_regression_mlp_output}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param z Numeric; passed to \code{abs}.
#' @return A numeric value.
#' @export
.morie_gr_logaddexp0 <- function(z) pmax(z, 0) + log1p(exp(-abs(z)))

# ------------------------------------------------------------- grdino

#' DINO self-distillation loss (Geron Ch 16, morie.fn grdino)
#'
#' L = -mean_i sum_k P_teacher(k) log P_student(k), teacher sharpened by
#' `tau_t` and optionally centred.
#'
#' @param student_logits,teacher_logits Numeric matrices (m, K); a vector
#'   is treated as one row.
#' @param tau_s,tau_t Positive temperatures.
#' @param center Optional length-K centring vector subtracted from the
#'   teacher logits.
#' @return List with `loss`, `teacher_probs`, `student_probs`,
#'   `teacher_entropy`, `student_entropy`, `max_teacher_prob`,
#'   `is_sharpened`, `per_sample_loss`.
#' @export
morie_geron_dino_self_distillation <- function(student_logits, teacher_logits,
                                               tau_s, tau_t, center = NULL) {
  S <- .morie_gr_a2d(student_logits)
  Tl <- .morie_gr_a2d(teacher_logits)
  .morie_gr_need(
    all(dim(S) == dim(Tl)),
    "student_logits and teacher_logits must match."
  )
  .morie_gr_fin(S, "logits")
  .morie_gr_fin(Tl, "logits")
  ts <- as.numeric(tau_s)
  tt <- as.numeric(tau_t)
  .morie_gr_need(is.finite(ts) && ts > 0, "tau_s must be a positive finite temperature.")
  .morie_gr_need(is.finite(tt) && tt > 0, "tau_t must be a positive finite temperature.")
  m <- nrow(S)
  K <- ncol(S)
  if (!is.null(center)) {
    cc <- as.numeric(center)
    .morie_gr_need(length(cc) == K, paste0("center must have ", K, " entries."))
    .morie_gr_fin(cc, "center")
    Tl <- sweep(Tl, 2L, cc, "-")
  }
  logp_t <- .morie_gr_log_softmax_rows(Tl / tt)
  logp_s <- .morie_gr_log_softmax_rows(S / ts)
  P <- exp(logp_t)
  Q <- exp(logp_s)
  per <- -rowSums(P * logp_s)
  loss <- mean(per)
  drop1 <- function(M) if (m == 1L) as.numeric(M) else M
  list(
    loss = loss, teacher_probs = drop1(P), student_probs = drop1(Q),
    teacher_entropy = mean(-rowSums(P * logp_t)),
    student_entropy = mean(-rowSums(Q * logp_s)),
    max_teacher_prob = mean(apply(P, 1L, max)),
    is_sharpened = tt < ts, per_sample_loss = per,
    estimate = loss, n = m
  )
}

# -------------------------------------------------------------- grdlm

#' Mini-batch DataLoader indices (Geron Ch 10, morie.fn grdlm)
#'
#' Shuffles 0..n-1 once with the reference LCG Fisher-Yates, then slices
#' into consecutive batches of `b`. Returned indices are 0-based.
#'
#' @param n Number of instances.
#' @param b Batch size in \[1, n\].
#' @param shuffle Logical; permute before slicing.
#' @param seed LCG seed.
#' @param drop_last Drop a short final batch.
#' @return List with `batches` (list of 0-based index vectors),
#'   `n_batches`, `batch_sizes`, `permutation`, `covers_all`.
#' @export
morie_geron_dataloader_minibatch <- function(n, b, shuffle = TRUE, seed = 0,
                                             drop_last = FALSE) {
  n <- as.integer(n)
  b <- as.integer(b)
  .morie_gr_need(n >= 1L, "n must be at least 1.")
  .morie_gr_need(b >= 1L && b <= n, paste0("b must lie in [1, ", n, "]."))
  perm <- if (isTRUE(shuffle)) .morie_gr_lcg_perm(n, seed) else seq_len(n) - 1L
  starts <- seq.int(0L, n - 1L, by = b)
  batches <- lapply(starts, function(i) perm[(i + 1L):min(i + b, n)])
  if (isTRUE(drop_last) && length(batches) &&
    length(batches[[length(batches)]]) < b) {
    batches <- batches[-length(batches)]
  }
  .morie_gr_need(length(batches) > 0L, "drop_last discarded every batch.")
  seen <- sort(unlist(batches))
  list(
    batches = batches, n_batches = length(batches),
    batch_sizes = vapply(batches, length, integer(1L)),
    permutation = perm,
    covers_all = identical(as.integer(seen), seq_len(n) - 1L),
    drop_last = isTRUE(drop_last), seed = as.integer(seed), n = n
  )
}

# ------------------------------------------------------------- grdpmf

#' DDPM forward diffusion q(x_t | x_0) (Geron Ch 18, morie.fn grdpmf)
#'
#' x_t = sqrt(alpha_bar_t) x_0 + sqrt(1 - alpha_bar_t) eps.
#'
#' @param x0 Numeric vector or matrix.
#' @param t 0-based index into `alpha_bar`.
#' @param alpha_bar Non-increasing cumulative products in \[0, 1\].
#' @param noise Optional noise of the same shape; otherwise LCG normals.
#' @param seed LCG seed.
#' @return List with `x_t`, `noise`, `signal_coef`, `noise_coef`, `snr`,
#'   `alpha_bar_t`.
#' @export
morie_geron_ddpm_forward_process <- function(x0, t, alpha_bar, noise = NULL,
                                             seed = 0) {
  X <- x0
  .morie_gr_need(length(X) > 0L, "x0 is empty.")
  .morie_gr_fin(X, "x0")
  ab <- as.numeric(alpha_bar)
  .morie_gr_need(length(ab) > 0L, "alpha_bar is empty.")
  .morie_gr_need(all(ab >= 0 & ab <= 1), "alpha_bar entries must lie in [0, 1].")
  .morie_gr_need(all(diff(ab) <= 1e-12), "alpha_bar must be non-increasing.")
  t <- as.integer(t)
  .morie_gr_need(t >= 0L && t < length(ab), "t must index alpha_bar.")
  a <- ab[t + 1L]
  if (is.null(noise)) {
    eps <- .morie_gr_lcg_normals(length(X), seed)
    if (is.matrix(X)) eps <- matrix(eps, nrow = nrow(X), byrow = TRUE)
  } else {
    eps <- noise
    .morie_gr_need(length(eps) == length(X), "noise shape != x0 shape.")
    .morie_gr_fin(eps, "noise")
  }
  sc <- sqrt(a)
  nc <- sqrt(1 - a)
  xt <- sc * X + nc * eps
  snr <- if (a == 1) Inf else a / (1 - a)
  list(
    x_t = xt, noise = eps, signal_coef = sc, noise_coef = nc, snr = snr,
    alpha_bar_t = a, t = t, estimate = xt, n = length(X)
  )
}

# ------------------------------------------------------------- grdpml

#' DDPM simplified noise-prediction loss (Geron Ch 18, morie.fn grdpml)
#'
#' @param eps,eps_pred Numeric matrices of equal shape (vectors = 1 row).
#' @param reduction "mean" or "sum"; selects `estimate` only.
#' @return List with `loss`, `sum_squared_error`, `per_sample`,
#'   `residual`, `rmse`.
#' @export
morie_geron_ddpm_simple_loss <- function(eps, eps_pred, reduction = "mean") {
  E <- .morie_gr_a2d(eps)
  P <- .morie_gr_a2d(eps_pred)
  .morie_gr_need(all(dim(E) == dim(P)), "eps and eps_pred must have the same shape.")
  .morie_gr_need(length(E) > 0L, "eps is empty.")
  .morie_gr_fin(E, "eps")
  .morie_gr_fin(P, "eps_pred")
  .morie_gr_need(reduction %in% c("mean", "sum"), "reduction must be 'mean' or 'sum'.")
  res <- E - P
  sq <- res^2
  loss <- mean(sq)
  total <- sum(sq)
  list(
    loss = loss, sum_squared_error = total, per_sample = rowSums(sq),
    residual = res, rmse = sqrt(loss),
    estimate = if (reduction == "mean") loss else total, n = nrow(E)
  )
}

# ------------------------------------------------------------- grdpmr

#' DDPM reverse denoising step (Geron Ch 18, morie.fn grdpmr)
#'
#' `x_{t-1}` = (x_t - ((1-a_t)/sqrt(1-ab_t)) eps) / sqrt(a_t) + sigma z.
#'
#' @param x_t,eps_pred Numeric objects of the same shape.
#' @param t 0-based timestep.
#' @param alpha,alpha_bar Schedules indexed by `t`.
#' @param sigma Non-negative noise scale; 0 gives the DDIM step.
#' @param z Optional noise; otherwise LCG normals.
#' @param seed LCG seed.
#' @return List with `x_prev`, `mean`, `noise_term`, `eps_coef`,
#'   `x0_estimate`, `alpha_t`, `alpha_bar_t`.
#' @export
morie_geron_ddpm_reverse_step <- function(x_t, t, eps_pred, alpha, alpha_bar,
                                          sigma, z = NULL, seed = 0) {
  X <- x_t
  E <- eps_pred
  .morie_gr_need(length(E) == length(X), "eps_pred shape != x_t shape.")
  .morie_gr_need(length(X) > 0L, "x_t is empty.")
  .morie_gr_fin(X, "x_t")
  .morie_gr_fin(E, "eps_pred")
  a <- as.numeric(alpha)
  ab <- as.numeric(alpha_bar)
  t <- as.integer(t)
  .morie_gr_need(
    t >= 0L && t < length(a) && t < length(ab),
    "t must index both alpha and alpha_bar."
  )
  at <- a[t + 1L]
  abt <- ab[t + 1L]
  .morie_gr_need(at > 0 && at <= 1, "alpha[t] must lie in (0, 1].")
  .morie_gr_need(abt >= 0 && abt < 1, "alpha_bar[t] must lie in [0, 1).")
  sigma <- as.numeric(sigma)
  .morie_gr_need(is.finite(sigma) && sigma >= 0, "sigma must be non-negative and finite.")
  coef <- (1 - at) / sqrt(1 - abt)
  mu <- (X - coef * E) / sqrt(at)
  if (sigma == 0) {
    zz <- X * 0
  } else if (is.null(z)) {
    zz <- .morie_gr_lcg_normals(length(X), seed)
    if (is.matrix(X)) zz <- matrix(zz, nrow = nrow(X), byrow = TRUE)
  } else {
    zz <- z
    .morie_gr_need(length(zz) == length(X), "z shape != x_t shape.")
    .morie_gr_fin(zz, "z")
  }
  noise_term <- sigma * zz
  x_prev <- mu + noise_term
  x0 <- if (abt > 0) (X - sqrt(1 - abt) * E) / sqrt(abt) else X * NA_real_
  list(
    x_prev = x_prev, mean = mu, noise_term = noise_term, eps_coef = coef,
    x0_estimate = x0, alpha_t = at, alpha_bar_t = abt, sigma = sigma,
    estimate = x_prev, n = length(X)
  )
}

# -------------------------------------------------------------- grdpo

#' Direct Preference Optimization loss (Geron Ch 15, morie.fn grdpo)
#'
#' L = mean log(1 + exp(-beta * \[(lp_w - ref_w) - (lp_l - ref_l)\])).
#'
#' @param logp_w,logp_l,logp_ref_w,logp_ref_l Non-positive log-probability
#'   vectors of equal length.
#' @param beta Positive inverse temperature.
#' @return List with `loss`, `margin`, `implicit_reward_chosen`,
#'   `implicit_reward_rejected`, `accuracy`, `per_pair_loss`.
#' @export
morie_geron_dpo_loss <- function(logp_w, logp_l, logp_ref_w, logp_ref_l,
                                 beta = 0.1) {
  a <- as.numeric(logp_w)
  b <- as.numeric(logp_l)
  ra <- as.numeric(logp_ref_w)
  rb <- as.numeric(logp_ref_l)
  .morie_gr_need(
    length(unique(c(length(a), length(b), length(ra), length(rb)))) == 1L,
    "all four log-probability arrays must have the same length."
  )
  for (nm in c("logp_w", "logp_l", "logp_ref_w", "logp_ref_l")) {
    arr <- switch(nm,
      logp_w = a,
      logp_l = b,
      logp_ref_w = ra,
      logp_ref_l = rb
    )
    .morie_gr_fin(arr, nm)
    .morie_gr_need(all(arr <= 0), paste0(nm, " must be <= 0."))
  }
  beta <- as.numeric(beta)
  .morie_gr_need(is.finite(beta) && beta > 0, "beta must be a positive finite float.")
  rw <- a - ra
  rl <- b - rb
  margin <- rw - rl
  per <- .morie_gr_logaddexp0(-beta * margin)
  loss <- mean(per)
  list(
    loss = loss, margin = margin, implicit_reward_chosen = beta * rw,
    implicit_reward_rejected = beta * rl, accuracy = mean(margin > 0),
    per_pair_loss = per, beta = beta, estimate = loss, n = length(a)
  )
}

# ------------------------------------------------------------- grdqnl

#' DQN bootstrap MSE loss (Geron Ch 19, morie.fn grdqnl)
#'
#' @param Q,Q_target (n_states, n_actions) matrices.
#' @param batch List of transitions c(s, a, r, s_next) or
#'   c(s, a, r, s_next, done); s / a / s_next are 0-based.
#' @param gamma Discount in \[0, 1\].
#' @return List with `loss`, `targets`, `predictions`, `td_errors`,
#'   `max_abs_td_error`, `n_terminal`.
#' @export
morie_geron_dqn_loss <- function(Q, Q_target, batch, gamma = 0.99) {
  Qa <- .morie_gr_a2d(Q)
  Qt <- .morie_gr_a2d(Q_target)
  .morie_gr_need(all(dim(Qa) == dim(Qt)), "Q and Q_target must have the same shape.")
  .morie_gr_need(length(Qa) > 0L, "Q must be a non-empty table.")
  .morie_gr_fin(Qa, "Q")
  .morie_gr_fin(Qt, "Q_target")
  nS <- nrow(Qa)
  nA <- ncol(Qa)
  gamma <- as.numeric(gamma)
  .morie_gr_need(gamma >= 0 && gamma <= 1, "gamma must lie in [0, 1].")
  rows <- batch
  .morie_gr_need(length(rows) > 0L, "batch is empty.")
  targets <- numeric(length(rows))
  preds <- numeric(length(rows))
  n_term <- 0L
  for (k in seq_along(rows)) {
    tr <- rows[[k]]
    .morie_gr_need(
      length(tr) %in% c(4L, 5L),
      "batch rows must be (s, a, r, s_next[, done])."
    )
    s <- as.integer(tr[[1]])
    a <- as.integer(tr[[2]])
    r <- as.numeric(tr[[3]])
    s2 <- as.integer(tr[[4]])
    done <- if (length(tr) == 5L) as.logical(tr[[5]]) else FALSE
    .morie_gr_need(s >= 0L && s < nS && s2 >= 0L && s2 < nS, "state out of range.")
    .morie_gr_need(a >= 0L && a < nA, "action out of range.")
    .morie_gr_need(is.finite(r), "reward must be finite.")
    boot <- if (done) 0 else gamma * max(Qt[s2 + 1L, ])
    if (done) n_term <- n_term + 1L
    targets[k] <- r + boot
    preds[k] <- Qa[s + 1L, a + 1L]
  }
  td <- targets - preds
  loss <- mean(td^2)
  list(
    loss = loss, targets = targets, predictions = preds, td_errors = td,
    max_abs_td_error = max(abs(td)), n_terminal = n_term, gamma = gamma,
    estimate = loss, n = length(rows)
  )
}

# -------------------------------------------------------------- grdro

#' Inverted dropout (Geron Ch 11, morie.fn grdro)
#'
#' Mask ~ `1{u < 1-p}` over the LCG uniform stream, output a * mask / (1-p).
#'
#' @param a Numeric vector or matrix of activations.
#' @param p Drop probability in \[0, 1).
#' @param seed LCG seed.
#' @return List with `output`, `mask`, `keep_prob`, `fraction_dropped`,
#'   `scale`, `expectation_ratio`.
#' @export
morie_geron_dropout <- function(a, p, seed = 0) {
  A <- a
  .morie_gr_need(length(A) > 0L, "a is empty.")
  .morie_gr_fin(A, "a")
  p <- as.numeric(p)
  .morie_gr_need(p >= 0 && p < 1, "p must lie in [0, 1).")
  keep <- 1 - p
  u <- .morie_gr_lcg_u(length(A), seed)
  if (is.matrix(A)) u <- matrix(u, nrow = nrow(A), byrow = TRUE)
  mask <- (u < keep) * 1
  out <- mask * A / keep
  mean_in <- mean(A)
  list(
    output = out, mask = mask, keep_prob = keep,
    fraction_dropped = 1 - mean(mask), scale = 1 / keep,
    expectation_ratio = if (mean_in != 0) mean(out) / mean_in else NULL,
    p = p, seed = as.integer(seed), estimate = out, n = length(A)
  )
}

# ------------------------------------------------------------- grduel

#' Dueling DQN decomposition (Geron Ch 19, morie.fn grduel)
#'
#' Q = V + (A - rowMeans(A)); `best_action` is 0-based.
#'
#' @param V State values (scalar or one per state).
#' @param A (n_states, n_actions) advantage matrix, at least 2 actions.
#' @return List with `Q`, `centered_advantage`, `mean_advantage`,
#'   `best_action`, `advantage_sums_to_zero`.
#' @export
morie_geron_dueling_dqn <- function(V, A) {
  Am <- .morie_gr_a2d(A)
  .morie_gr_need(length(Am) > 0L, "A must be non-empty.")
  .morie_gr_fin(Am, "A")
  Vv <- as.numeric(V)
  if (length(Vv) == 1L && nrow(Am) != 1L) Vv <- rep(Vv, nrow(Am))
  .morie_gr_need(length(Vv) == nrow(Am), "V length must match the states in A.")
  .morie_gr_fin(Vv, "V")
  .morie_gr_need(ncol(Am) >= 2L, "the decomposition needs at least 2 actions.")
  mean_a <- rowMeans(Am)
  cent <- Am - mean_a
  Q <- cent + Vv
  list(
    Q = Q, centered_advantage = cent, mean_advantage = mean_a,
    best_action = max.col(Q, ties.method = "first") - 1L,
    advantage_sums_to_zero = all(abs(rowSums(cent)) <= 1e-12),
    estimate = Q, n = nrow(Am)
  )
}

# -------------------------------------------------------------- grdyq

#' Dynamic INT8 quantization (Geron Ch 19, morie.fn grdyq)
#'
#' Per-tensor scales max|.|/127, INT32 accumulate, dequantize by sx*sw.
#' np.rint and R's round both break ties to even.
#'
#' @param x,w Conformable numeric matrices.
#' @return List with `output`, `reference`, `max_abs_error`,
#'   `relative_error`, `scale_x`, `scale_w`, `x_quantized`, `w_quantized`,
#'   `accumulator_max`.
#' @export
morie_geron_dynamic_quantization <- function(x, w) {
  X <- .morie_gr_a2d(x)
  W <- .morie_gr_a2d(w)
  .morie_gr_need(ncol(X) == nrow(W), "x columns must equal w rows.")
  .morie_gr_need(length(X) > 0L && length(W) > 0L, "x and w must be non-empty.")
  .morie_gr_fin(X, "x")
  .morie_gr_fin(W, "w")
  mx <- max(abs(X))
  mw <- max(abs(W))
  .morie_gr_need(mx != 0 && mw != 0, "x or w is all zeros.")
  sx <- mx / 127
  sw <- mw / 127
  Xq <- round(X / sx)
  Wq <- round(W / sw)
  acc <- Xq %*% Wq
  out <- acc * sx * sw
  ref <- X %*% W
  err <- max(abs(out - ref))
  denom <- max(abs(ref))
  list(
    output = out, reference = ref, max_abs_error = err,
    relative_error = if (denom > 0) err / denom else 0,
    scale_x = sx, scale_w = sw, x_quantized = Xq, w_quantized = Wq,
    accumulator_max = max(abs(acc)), estimate = out, n = nrow(X)
  )
}

# ------------------------------------------------------------- greast

#' Early stopping on validation RMSE (Geron Ch 4, morie.fn greast)
#'
#' Runs every one of `n_iter` batch-GD steps (gradient from the shared
#' MSE core) and rolls back to the argmin validation RMSE, not the first
#' uptick. `best_iteration` is 0-based.
#'
#' @param X_train,y_train,X_val,y_val Training and validation data.
#' @param n_iter Number of gradient steps.
#' @param eta Positive learning rate.
#' @param theta0 Optional starting parameters; default zeros.
#' @return List with `theta`, `best_iteration`, `best_val_rmse`,
#'   `final_val_rmse`, `val_rmse_history`, `train_rmse_history`,
#'   `overfitting_detected`.
#' @export
morie_geron_early_stopping <- function(X_train, y_train, X_val, y_val, n_iter,
                                       eta, theta0 = NULL) {
  A <- .morie_gr_a2d(X_train)
  V <- .morie_gr_a2d(X_val)
  ytr <- as.numeric(y_train)
  yva <- as.numeric(y_val)
  .morie_gr_need(ncol(A) == ncol(V), "X_train and X_val must have the same columns.")
  .morie_gr_need(length(ytr) == nrow(A), "y_train length must equal nrow(X_train).")
  .morie_gr_need(length(yva) == nrow(V), "y_val length must equal nrow(X_val).")
  .morie_gr_need(nrow(V) > 0L, "X_val is empty.")
  n_iter <- as.integer(n_iter)
  .morie_gr_need(n_iter >= 1L, "n_iter must be at least 1.")
  eta <- as.numeric(eta)
  .morie_gr_need(is.finite(eta) && eta > 0, "eta must be a positive finite float.")
  th <- if (is.null(theta0)) numeric(ncol(A)) else as.numeric(theta0)
  .morie_gr_need(length(th) == ncol(A), "theta0 length must equal ncol(X_train).")
  tr_hist <- .morie_gr_mse_core(A, ytr, th)$rmse
  va_hist <- .morie_gr_mse_core(V, yva, th)$rmse
  best_theta <- th
  best_it <- 0L
  for (it in seq_len(n_iter)) {
    fit <- .morie_gr_mse_core(A, ytr, th)
    g <- 2 / nrow(A) * as.numeric(t(A) %*% fit$residuals)
    th <- th - eta * g
    .morie_gr_need(all(is.finite(th)), "parameters diverged; eta is too large.")
    tr_hist <- c(tr_hist, .morie_gr_mse_core(A, ytr, th)$rmse)
    v <- .morie_gr_mse_core(V, yva, th)$rmse
    va_hist <- c(va_hist, v)
    if (v < va_hist[best_it + 1L]) {
      best_theta <- th
      best_it <- it
    }
  }
  list(
    theta = best_theta, best_iteration = best_it,
    best_val_rmse = va_hist[best_it + 1L],
    final_val_rmse = va_hist[length(va_hist)],
    val_rmse_history = va_hist, train_rmse_history = tr_hist,
    overfitting_detected = best_it < n_iter, estimate = best_theta,
    n = nrow(A)
  )
}

# ------------------------------------------------------------- gredsq

#' Encoder-decoder seq2seq decoding loop (Geron Ch 16, morie.fn gredsq)
#'
#' c = encoder(x) once, then greedy y_t = decoder(y_prev, c, t) with the
#' shape and finiteness contracts enforced at every step.
#'
#' @param encoder,decoder Functions; `decoder(y_prev, c, t)` with `t`
#'   1-based as in the Python loop.
#' @param x Encoder input.
#' @param max_out_len Maximum number of decoding steps.
#' @param start_token Optional initial `y_prev`; default zeros like `c`.
#' @param eos_token Optional scalar that stops decoding.
#' @return List with `outputs`, `context`, `context_dim`, `n_steps`,
#'   `stopped_early`.
#' @export
morie_geron_encoder_decoder_seq2seq <- function(encoder, decoder, x, max_out_len,
                                                start_token = NULL,
                                                eos_token = NULL) {
  .morie_gr_need(is.function(encoder), "encoder must be a function.")
  .morie_gr_need(is.function(decoder), "decoder must be a function.")
  max_out_len <- as.integer(max_out_len)
  .morie_gr_need(max_out_len >= 1L, "max_out_len must be at least 1.")
  cc <- as.numeric(encoder(x))
  .morie_gr_need(length(cc) > 0L, "encoder returned an empty context vector.")
  .morie_gr_fin(cc, "context")
  y_prev <- if (is.null(start_token)) rep(0, length(cc)) else as.numeric(start_token)
  outputs <- list()
  stopped <- FALSE
  first_len <- NULL
  for (t in seq_len(max_out_len)) {
    y <- decoder(y_prev, cc, t)
    .morie_gr_fin(y, paste0("decoder output at step ", t))
    if (is.null(first_len)) {
      first_len <- length(y)
    } else {
      .morie_gr_need(length(y) == first_len, "decoder changed output shape.")
    }
    outputs[[length(outputs) + 1L]] <- if (length(y) == 1L) as.numeric(y) else y
    y_prev <- y
    if (!is.null(eos_token) && length(y) == 1L &&
      as.numeric(y) == as.numeric(eos_token)) {
      stopped <- TRUE
      break
    }
  }
  list(
    outputs = outputs, context = cc, context_dim = length(cc),
    n_steps = length(outputs), stopped_early = stopped,
    estimate = outputs, n = length(outputs)
  )
}

# ---------------------------------------------------- grlaso / grelas /
# ---------------------------------------------------- grn011 / grn013

#' Lasso cost (Geron Eq 4-10, morie.fn grlaso)
#'
#' J = MSE + alpha * `sum_{i>=1}` |theta_i|, on the shared MSE core.
#'
#' @param X,y,theta Design, targets and parameters.
#' @param alpha Non-negative L1 weight.
#' @param penalize_intercept Include theta\[1\] in the penalty.
#' @return List with `cost`, `mse`, `l1_penalty`, `l1_norm`, `n_zero`.
#' @export
morie_geron_lasso_cost <- function(X, y, theta, alpha,
                                   penalize_intercept = FALSE) {
  fit <- .morie_gr_mse_core(X, y, theta)
  theta <- as.numeric(theta)
  alpha <- as.numeric(alpha)
  .morie_gr_need(is.finite(alpha) && alpha >= 0, "alpha must be non-negative and finite.")
  w <- if (isTRUE(penalize_intercept)) theta else theta[-1L]
  .morie_gr_need(
    length(w) > 0L || isTRUE(penalize_intercept),
    "theta has only the intercept, which is not penalised."
  )
  l1 <- sum(abs(w))
  penalty <- alpha * l1
  list(
    cost = fit$cost + penalty, mse = fit$cost, l1_penalty = penalty,
    l1_norm = l1, n_zero = sum(w == 0), alpha = alpha,
    estimate = fit$cost + penalty, n = fit$n
  )
}

#' Elastic net cost (Geron Eq 4-12, morie.fn grelas)
#'
#' J = MSE + r alpha L1 + ((1-r)/2) alpha L2, stacked on
#' [morie_geron_lasso_cost()] at \code{r * alpha}.
#'
#' @param X,y,theta Design, targets and parameters.
#' @param alpha Non-negative penalty weight.
#' @param r L1 mix ratio in \[0, 1\].
#' @param penalize_intercept Include theta\[1\] in both penalties.
#' @return List with `cost`, `mse`, `l1_penalty`, `l2_penalty`,
#'   `l1_norm`, `l2_norm_sq`.
#' @export
morie_geron_elastic_net_cost <- function(X, y, theta, alpha, r,
                                         penalize_intercept = FALSE) {
  alpha <- as.numeric(alpha)
  r <- as.numeric(r)
  .morie_gr_need(is.finite(alpha) && alpha >= 0, "alpha must be non-negative and finite.")
  .morie_gr_need(r >= 0 && r <= 1, "r must lie in [0, 1].")
  inner <- morie_geron_lasso_cost(X, y, theta, r * alpha,
    penalize_intercept = penalize_intercept
  )
  theta <- as.numeric(theta)
  w <- if (isTRUE(penalize_intercept)) theta else theta[-1L]
  l2sq <- sum(w^2)
  l2 <- (1 - r) / 2 * alpha * l2sq
  list(
    cost = inner$cost + l2, mse = inner$mse, l1_penalty = inner$l1_penalty,
    l2_penalty = l2, l1_norm = inner$l1_norm, l2_norm_sq = l2sq,
    alpha = alpha, r = r, estimate = inner$cost + l2, n = inner$n
  )
}

#' Lasso cost, 2*alpha convention (Geron Eq 4-11, morie.fn grn011)
#'
#' @param X,y,theta Design, targets and parameters.
#' @param alpha Non-negative weight; the penalty uses 2 * alpha.
#' @param penalize_intercept Include theta\[1\] in the penalty.
#' @return List with `cost`, `mse`, `l1_penalty`, `l1_norm`,
#'   `effective_alpha`.
#' @export
morie_geron_ch4_lasso_regression_cost_function <- function(X, y, theta, alpha,
                                                           penalize_intercept = FALSE) {
  alpha <- as.numeric(alpha)
  .morie_gr_need(is.finite(alpha) && alpha >= 0, "alpha must be non-negative and finite.")
  inner <- morie_geron_lasso_cost(X, y, theta, 2 * alpha,
    penalize_intercept = penalize_intercept
  )
  list(
    cost = inner$cost, mse = inner$mse, l1_penalty = inner$l1_penalty,
    l1_norm = inner$l1_norm, alpha = alpha, effective_alpha = 2 * alpha,
    estimate = inner$cost, n = inner$n
  )
}

#' Elastic net cost, Eq 4-13 convention (morie.fn grn013)
#'
#' J = MSE + 2 r alpha L1 + (1-r)(alpha/m) L2.
#'
#' @param X,y,theta Design, targets and parameters.
#' @param alpha Non-negative penalty weight.
#' @param r L1 mix ratio in \[0, 1\].
#' @param penalize_intercept Include theta\[1\] in both penalties.
#' @return List with `cost`, `mse`, `l1_penalty`, `l2_penalty`,
#'   `l1_norm`, `l2_norm_sq`.
#' @export
morie_geron_ch4_elastic_net_cost_function <- function(X, y, theta, alpha, r,
                                                      penalize_intercept = FALSE) {
  alpha <- as.numeric(alpha)
  r <- as.numeric(r)
  .morie_gr_need(is.finite(alpha) && alpha >= 0, "alpha must be non-negative and finite.")
  .morie_gr_need(r >= 0 && r <= 1, "r must lie in [0, 1].")
  inner <- morie_geron_lasso_cost(X, y, theta, 2 * r * alpha,
    penalize_intercept = penalize_intercept
  )
  theta <- as.numeric(theta)
  w <- if (isTRUE(penalize_intercept)) theta else theta[-1L]
  l2sq <- sum(w^2)
  l2 <- (1 - r) * (alpha / inner$n) * l2sq
  list(
    cost = inner$cost + l2, mse = inner$mse, l1_penalty = inner$l1_penalty,
    l2_penalty = l2, l1_norm = inner$l1_norm, l2_norm_sq = l2sq,
    alpha = alpha, r = r, estimate = inner$cost + l2, n = inner$n
  )
}

# -------------------------------------------------------------- gremb

#' Embedding table lookup (Geron Ch 16, morie.fn gremb)
#'
#' @param ids 0-based integer token ids.
#' @param E (V, d) embedding table.
#' @return List with `embeddings`, `vocab_size`, `dim`, `n_unique`,
#'   `n_parameters`, `ids`.
#' @export
morie_geron_embedding_lookup <- function(ids, E) {
  Tm <- .morie_gr_a2d(E)
  .morie_gr_need(length(Tm) > 0L, "E must be a non-empty (V, d) table.")
  .morie_gr_fin(Tm, "E")
  V <- nrow(Tm)
  d <- ncol(Tm)
  I <- as.numeric(ids)
  .morie_gr_need(length(I) > 0L, "ids is empty.")
  .morie_gr_need(all(I == floor(I)), "token ids must be integers.")
  I <- as.integer(I)
  .morie_gr_need(
    min(I) >= 0L && max(I) < V,
    paste0("token ids must lie in [0, ", V - 1L, "].")
  )
  out <- Tm[I + 1L, , drop = FALSE]
  list(
    embeddings = out, vocab_size = V, dim = d,
    n_unique = length(unique(I)), n_parameters = V * d, ids = I,
    estimate = out, n = length(I)
  )
}

# -------------------------------------------------------------- grent

#' Shannon entropy (Geron Eq 5-3, morie.fn grent)
#'
#' @param y Class label vector.
#' @param base Log base greater than 1.
#' @return List with `entropy`, `proportions`, `classes`, `counts`,
#'   `max_possible`.
#' @export
morie_geron_shannon_entropy <- function(y, base = 2) {
  y <- as.vector(y)
  .morie_gr_need(length(y) > 0L, "y is empty.")
  base <- as.numeric(base)
  .morie_gr_need(is.finite(base) && base > 1, "base must be finite and greater than 1.")
  tb <- table(y)
  classes <- sort(unique(y))
  counts <- as.integer(tb[as.character(classes)])
  p <- counts / sum(counts)
  nz <- p[p > 0]
  H <- -sum(nz * (log(nz) / log(base)))
  K <- length(classes)
  list(
    entropy = H, proportions = p, classes = classes, counts = counts,
    max_possible = log(K) / log(base), base = base, estimate = H,
    n = length(y)
  )
}

# -------------------------------------------------------------- grepl

#' Epsilon-greedy action selection (Geron Ch 18, morie.fn grepl)
#'
#' Greedy w.p. 1-eps, otherwise uniform over ALL actions, so
#' P(greedy) = 1 - eps + eps/|A|. Actions returned 0-based.
#'
#' @param Q_s Action values for one state.
#' @param eps Exploration rate in \[0, 1\].
#' @param seed LCG seed; two uniforms are drawn (explore test, then
#'   action) exactly as in Python.
#' @return List with `action`, `greedy_action`, `explored`,
#'   `action_probabilities`, `greedy_probability`.
#' @export
morie_geron_epsilon_greedy <- function(Q_s, eps, seed = 0) {
  Q <- as.numeric(Q_s)
  .morie_gr_need(length(Q) > 0L, "Q_s is empty.")
  .morie_gr_fin(Q, "Q_s")
  eps <- as.numeric(eps)
  .morie_gr_need(eps >= 0 && eps <= 1, "eps must lie in [0, 1].")
  A <- length(Q)
  greedy <- which.max(Q) - 1L
  probs <- rep(eps / A, A)
  probs[greedy + 1L] <- probs[greedy + 1L] + 1 - eps
  u <- .morie_gr_lcg_u(2L, seed)
  explored <- u[1L] < eps
  action <- if (explored) min(as.integer(u[2L] * A), A - 1L) else greedy
  list(
    action = action, greedy_action = greedy, explored = explored,
    action_probabilities = probs, greedy_probability = probs[greedy + 1L],
    eps = eps, seed = as.integer(seed), estimate = action, n = A
  )
}

# -------------------------------------------------------------- grevr

#' Explained variance ratio (Geron Ch 8, morie.fn grevr)
#'
#' @param singular_values Non-negative singular values.
#' @param threshold Cumulative target in (0, 1\].
#' @return List with `explained_variance_ratio`, `cumulative`,
#'   `n_components_for_threshold`, `total_variance`.
#' @export
morie_geron_explained_variance_ratio <- function(singular_values,
                                                 threshold = 0.95) {
  s <- as.numeric(singular_values)
  .morie_gr_need(length(s) > 0L, "singular_values is empty.")
  .morie_gr_fin(s, "singular_values")
  .morie_gr_need(all(s >= 0), "singular values are non-negative by definition.")
  total <- sum(s^2)
  .morie_gr_need(total != 0, "all singular values are zero.")
  threshold <- as.numeric(threshold)
  .morie_gr_need(threshold > 0 && threshold <= 1, "threshold must lie in (0, 1].")
  evr <- s^2 / total
  cum <- cumsum(evr)
  k <- sum(cum < threshold - 1e-12) + 1L
  list(
    explained_variance_ratio = evr, cumulative = cum,
    n_components_for_threshold = k, total_variance = total,
    threshold = threshold, estimate = evr, n = length(s)
  )
}

# --------------------------------------------------------------- grf1

#' F1 score (Geron Eq 3-3, morie.fn grf1)
#'
#' Counts delegated to [morie_geron_confusion_matrix()].
#'
#' @param y_true,y_pred Non-negative integer labels.
#' @param positive_class 0-based class to score.
#' @return List with `f1`, `precision`, `recall`, `macro_f1`,
#'   `per_class_f1`, `confusion_matrix`.
#' @export
morie_geron_f1_score <- function(y_true, y_pred, positive_class = 1) {
  yt <- as.vector(y_true)
  yp <- as.vector(y_pred)
  .morie_gr_need(length(yt) == length(yp), "y_true and y_pred must be equal length.")
  .morie_gr_need(length(yt) > 0L, "F1 over zero instances is undefined.")
  labels <- sort(unique(as.integer(c(yt, yp))))
  .morie_gr_need(all(labels >= 0L), "labels must be non-negative integers.")
  pc <- as.integer(positive_class)
  .morie_gr_need(pc %in% labels, "positive_class appears in neither y_true nor y_pred.")
  cm <- morie_geron_confusion_matrix(yt, yp)
  prec <- as.numeric(cm$precision)[pc + 1L]
  rec <- as.numeric(cm$recall)[pc + 1L]
  f1 <- if (!is.finite(prec) || !is.finite(rec) || prec + rec == 0) {
    0
  } else {
    2 * prec * rec / (prec + rec)
  }
  list(
    f1 = f1, precision = prec, recall = rec, macro_f1 = cm$macro_f1,
    per_class_f1 = cm$f1, confusion_matrix = cm$matrix,
    positive_class = pc, estimate = f1, n = length(yt)
  )
}

# -------------------------------------------------------------- grfad

#' Forward-mode autodiff dual number (morie.fn grfad `Dual`)
#'
#' @param value,deriv Value and derivative components.
#' @return Object of class `morie_gr_dual`.
#' @export
morie_gr_dual <- function(value, deriv = 0) {
  structure(list(value = as.numeric(value), deriv = as.numeric(deriv)),
    class = "morie_gr_dual"
  )
}

#' .morie_gr_lift
#'
#' A step of the geron_train_native implementation. Called by \code{Ops.morie_gr_dual}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param o Passed to \code{morie_gr_dual}.
#' @return One of two values, depending on the branch taken.
#' @export
.morie_gr_lift <- function(o) {
  if (inherits(o, "morie_gr_dual")) {
    o
  } else {
    morie_gr_dual(o, 0)
  }
}

#' Arithmetic on dual numbers
#' @param e1,e2 Operands.
#' @return A `morie_gr_dual`.
#' @export
Ops.morie_gr_dual <- function(e1, e2) {
  if (missing(e2)) {
    if (.Generic == "-") {
      return(morie_gr_dual(-e1$value, -e1$deriv))
    }
    if (.Generic == "+") {
      return(e1)
    }
    stop("unsupported unary operator ", .Generic, call. = FALSE)
  }
  a <- .morie_gr_lift(e1)
  b <- .morie_gr_lift(e2)
  switch(.Generic,
    "+" = morie_gr_dual(a$value + b$value, a$deriv + b$deriv),
    "-" = morie_gr_dual(a$value - b$value, a$deriv - b$deriv),
    "*" = morie_gr_dual(
      a$value * b$value,
      a$deriv * b$value + a$value * b$deriv
    ),
    "/" = {
      .morie_gr_need(b$value != 0, "division by a dual number with value 0.")
      morie_gr_dual(
        a$value / b$value,
        (a$deriv * b$value - a$value * b$deriv) / b$value^2
      )
    },
    "^" = {
      p <- b$value
      morie_gr_dual(a$value^p, p * a$value^(p - 1) * a$deriv)
    },
    stop("unsupported operator ", .Generic, call. = FALSE)
  )
}

#' Elementary functions on dual numbers
#' @param x A `morie_gr_dual`.
#' @param ... Unused.
#' @return A `morie_gr_dual`.
#' @export
Math.morie_gr_dual <- function(x, ...) {
  switch(.Generic,
    "exp" = {
      e <- exp(x$value)
      morie_gr_dual(e, e * x$deriv)
    },
    "log" = {
      .morie_gr_need(x$value > 0, "log is undefined at a non-positive value.")
      morie_gr_dual(log(x$value), x$deriv / x$value)
    },
    "sqrt" = {
      .morie_gr_need(x$value > 0, "sqrt derivative is undefined here.")
      s <- sqrt(x$value)
      morie_gr_dual(s, x$deriv / (2 * s))
    },
    "sin" = morie_gr_dual(sin(x$value), cos(x$value) * x$deriv),
    "cos" = morie_gr_dual(cos(x$value), -sin(x$value) * x$deriv),
    "tanh" = {
      t <- tanh(x$value)
      morie_gr_dual(t, (1 - t * t) * x$deriv)
    },
    stop("unsupported function ", .Generic, call. = FALSE)
  )
}

#' Forward-mode autodiff (Geron Ch 12, morie.fn grfad)
#'
#' Evaluates `f` at the dual number x + x' eps and reports the exact
#' derivative alongside a central-difference cross-check.
#'
#' @param x,x_prime Finite value and seed derivative.
#' @param f Function of one `morie_gr_dual`.
#' @return List with `value`, `derivative`, `finite_difference_check`,
#'   `check_abs_error`.
#' @export
morie_geron_forward_mode_autodiff <- function(x, x_prime, f) {
  .morie_gr_need(is.function(f), "f must be a function.")
  x <- as.numeric(x)
  x_prime <- as.numeric(x_prime)
  .morie_gr_need(is.finite(x) && is.finite(x_prime), "x and x_prime must be finite.")
  out <- f(morie_gr_dual(x, x_prime))
  .morie_gr_need(inherits(out, "morie_gr_dual"), "f must return a morie_gr_dual.")
  .morie_gr_need(
    is.finite(out$value) && is.finite(out$deriv),
    "f produced a non-finite value or derivative."
  )
  h <- 1e-06 * max(1, abs(x))
  fd <- tryCatch(
    {
      (f(morie_gr_dual(x + h, 0))$value - f(morie_gr_dual(x - h, 0))$value) /
        (2 * h) * x_prime
    },
    error = function(e) NULL
  )
  err <- if (is.null(fd)) NULL else abs(fd - out$deriv)
  list(
    value = out$value, derivative = out$deriv,
    finite_difference_check = fd, check_abs_error = err,
    x = x, x_prime = x_prime, estimate = out$deriv, n = 1L
  )
}

# -------------------------------------------------------------- grfcn

#' Transposed convolution / FCN upsampling (Geron Ch 14, morie.fn grfcn)
#'
#' @param X (H, W) input map.
#' @param W (kh, kw) kernel.
#' @param stride Positive integer.
#' @return List with `output`, `output_shape`, `contribution_counts`,
#'   `uniform_coverage`, `upsample_factor`.
#' @export
morie_geron_fcn_upsample <- function(X, W, stride = 2) {
  A <- .morie_gr_a2d(X)
  K <- .morie_gr_a2d(W)
  .morie_gr_need(length(A) > 0L && length(K) > 0L, "X and W must be non-empty.")
  .morie_gr_fin(A, "X")
  .morie_gr_fin(K, "W")
  s <- as.integer(stride)
  .morie_gr_need(s >= 1L, "stride must be a positive integer.")
  H <- nrow(A)
  Wi <- ncol(A)
  kh <- nrow(K)
  kw <- ncol(K)
  oh <- (H - 1L) * s + kh
  ow <- (Wi - 1L) * s + kw
  Y <- matrix(0, oh, ow)
  counts <- matrix(0L, oh, ow)
  for (i in seq_len(H)) {
    for (j in seq_len(Wi)) {
      ri <- ((i - 1L) * s + 1L):((i - 1L) * s + kh)
      rj <- ((j - 1L) * s + 1L):((j - 1L) * s + kw)
      Y[ri, rj] <- Y[ri, rj] + A[i, j] * K
      counts[ri, rj] <- counts[ri, rj] + 1L
    }
  }
  list(
    output = Y, output_shape = c(oh, ow), contribution_counts = counts,
    uniform_coverage = min(counts) == max(counts),
    upsample_factor = length(Y) / length(A), stride = s,
    estimate = Y, n = length(Y)
  )
}

# -------------------------------------------------------------- grlinf

#' Affine layer forward pass (Geron Ch 10, morie.fn grlinf)
#'
#' Y = X W^T + b with W stored (out, in) as in nn.Linear.
#'
#' @param X Instance vector or (batch, in) matrix.
#' @param W (out, in) weight matrix.
#' @param b Scalar or length-out bias.
#' @return List with `output`, `preactivation_norm`, `in_features`,
#'   `out_features`, `n_parameters`, `batch`.
#' @export
morie_geron_linear_layer_forward <- function(X, W, b) {
  W <- .morie_gr_a2d(W)
  out_f <- nrow(W)
  in_f <- ncol(W)
  batch <- is.matrix(X)
  Xm <- if (batch) X else matrix(as.numeric(X), nrow = 1L)
  storage.mode(Xm) <- "double"
  .morie_gr_need(ncol(Xm) == in_f, "X features must match W columns.")
  bv <- as.numeric(b)
  if (length(bv) == 1L) bv <- rep(bv, out_f)
  .morie_gr_need(length(bv) == out_f, "b length must equal the layer outputs.")
  .morie_gr_fin(Xm, "X")
  .morie_gr_fin(W, "W")
  .morie_gr_fin(bv, "b")
  Y <- Xm %*% t(W)
  Y <- sweep(Y, 2L, bv, "+")
  list(
    output = if (batch) Y else as.numeric(Y),
    preactivation_norm = sqrt(sum(Y^2)), in_features = in_f,
    out_features = out_f, n_parameters = length(W) + out_f,
    batch = batch, estimate = if (batch) Y else as.numeric(Y),
    n = nrow(Xm)
  )
}

# -------------------------------------------------------------- grffn

#' Transformer position-wise feed-forward (Geron Ch 16, morie.fn grffn)
#'
#' FFN(x) = relu(x W1 + b1) W2 + b2, routed through
#' [morie_geron_linear_layer_forward()] with the transposed weights.
#'
#' @param x Token vector or (T, d_model) matrix.
#' @param W1 (d_model, d_ff), b1 length d_ff.
#' @param b1 Hidden bias.
#' @param W2 (d_ff, d_model), b2 length d_model.
#' @param b2 Output bias.
#' @return List with `output`, `hidden`, `d_model`, `d_ff`,
#'   `expansion_ratio`, `sparsity`, `n_parameters`.
#' @export
morie_geron_transformer_feedforward <- function(x, W1, b1, W2, b2) {
  W1 <- .morie_gr_a2d(W1)
  W2 <- .morie_gr_a2d(W2)
  .morie_gr_need(ncol(W1) == nrow(W2), "W1 hidden width must match W2 rows.")
  .morie_gr_need(ncol(W2) == nrow(W1), "the sublayer must be shape-preserving.")
  first <- morie_geron_linear_layer_forward(x, t(W1), b1)
  H <- pmax(if (first$batch) {
    first$output
  } else {
    matrix(first$output, nrow = 1L)
  }, 0)
  second <- morie_geron_linear_layer_forward(
    if (first$batch) H else as.numeric(H), t(W2), b2
  )
  Y <- second$output
  d_model <- nrow(W1)
  d_ff <- ncol(W1)
  list(
    output = Y, hidden = if (first$batch) H else as.numeric(H),
    d_model = d_model, d_ff = d_ff, expansion_ratio = d_ff / d_model,
    sparsity = mean(H == 0),
    n_parameters = first$n_parameters + second$n_parameters,
    estimate = Y, n = if (is.matrix(x)) nrow(x) else 1L
  )
}

# -------------------------------------------------------------- grfim

#' Forest feature importance, MDI (Geron Ch 7, morie.fn grfim)
#'
#' Per-tree impurity decreases normalised to sum 1, then averaged.
#' `spread` is the ddof = 1 standard deviation; `ranking` is 0-based and
#' stable (order(method = "radix") matches numpy's stable argsort).
#'
#' @param tree_importances (B, F) non-negative matrix.
#' @return List with `importance`, `spread`, `ranking`,
#'   `per_tree_normalized`, `n_trees`, `n_features`.
#' @export
morie_geron_feature_importance_mdi <- function(tree_importances) {
  A <- .morie_gr_a2d(tree_importances)
  .morie_gr_need(length(A) > 0L, "tree_importances is empty.")
  .morie_gr_fin(A, "tree_importances")
  .morie_gr_need(all(A >= 0), "impurity decreases are non-negative.")
  rowsum <- rowSums(A)
  .morie_gr_need(all(rowsum != 0), "some tree has zero total impurity decrease.")
  N <- A / rowsum
  imp <- colMeans(N)
  spread <- if (nrow(N) > 1L) apply(N, 2L, stats::sd) else rep(0, ncol(N))
  ord <- order(-imp, method = "radix") - 1L
  list(
    importance = imp, spread = spread, ranking = ord,
    per_tree_normalized = N, n_trees = nrow(A), n_features = ncol(A),
    estimate = imp, n = nrow(A)
  )
}

# ------------------------------------------------------------- grflam

#' Flamingo tanh-gated cross-attention (Geron Ch 17, morie.fn grflam)
#'
#' h_new = h + tanh(alpha) * crossattn(h, visual), the cross-attention
#' coming from [morie_geron_cross_attention()].
#'
#' @param h (T, d_model) language hidden states.
#' @param visual_features (S, d_v) visual tokens.
#' @param alpha Finite gate parameter; 0 leaves the LM untouched.
#' @param weights List or named list with WQ, WK, WV.
#' @param mask Optional attention mask.
#' @return List with `h_new`, `gate`, `attention_output`,
#'   `attention_weights`, `delta_norm`, `is_identity`.
#' @export
morie_geron_flamingo_cross_modal_attn <- function(h, visual_features, alpha,
                                                  weights, mask = NULL) {
  H <- .morie_gr_a2d(h)
  Vf <- .morie_gr_a2d(visual_features)
  alpha <- as.numeric(alpha)
  .morie_gr_need(is.finite(alpha), "alpha must be finite.")
  if (!is.null(names(weights)) && all(c("WQ", "WK", "WV") %in% names(weights))) {
    WQ <- weights$WQ
    WK <- weights$WK
    WV <- weights$WV
  } else {
    .morie_gr_need(length(weights) == 3L, "weights must hold exactly WQ, WK, WV.")
    WQ <- weights[[1L]]
    WK <- weights[[2L]]
    WV <- weights[[3L]]
  }
  .morie_gr_need(
    ncol(.morie_gr_a2d(WV)) == ncol(H),
    "WV must map to d_model for the residual add."
  )
  ca <- morie_geron_cross_attention(H, Vf, WQ, WK, WV, mask = mask)
  attn <- as.matrix(ca$output)
  gate <- tanh(alpha)
  H_new <- H + gate * attn
  list(
    h_new = H_new, gate = gate, alpha = alpha, attention_output = ca$output,
    attention_weights = ca$attention_weights,
    delta_norm = sqrt(sum((gate * attn)^2)), is_identity = gate == 0,
    estimate = H_new, n = nrow(H)
  )
}

# ------------------------------------------------------------ grsdpa

#' Scaled dot-product attention (Geron Ch 16, morie.fn grsdpa)
#'
#' The shared kernel behind grsa, grmha, grflash, grswin, grpvt, grpio,
#' grteb and grtdb here.
#'
#' @param Q,K,V Query, key and value matrices.
#' @param mask Optional logical mask, TRUE = attend.
#' @return List with `output`, `weights`, `scores`, `d_k`.
#' @export
morie_geron_scaled_dot_product_attention <- function(Q, K, V, mask = NULL) {
  r <- .morie_gr_attend(Q, K, V, mask)
  list(
    output = r$output, weights = r$weights, scores = r$scores,
    d_k = ncol(.morie_gr_a2d(K)), estimate = r$output,
    n = nrow(r$output)
  )
}

# ------------------------------------------------------------ grflash

#' FlashAttention tiled online softmax (Geron Ch 16, morie.fn grflash)
#'
#' Streams K/V in blocks of `block_size`, rescaling the running softmax
#' denominator; exact against the plain attention kernel at every block
#' size.
#'
#' @param Q,K,V Attention inputs.
#' @param block_size Positive tile height over the key axis.
#' @return List with `output`, `reference_output`, `max_abs_error`,
#'   `n_blocks`, `row_max`, `row_denominator`, `peak_score_elements`,
#'   `full_score_elements`.
#' @export
morie_geron_flash_attention_tile <- function(Q, K, V, block_size = 2) {
  Qa <- .morie_gr_a2d(Q)
  Ka <- .morie_gr_a2d(K)
  Va <- .morie_gr_a2d(V)
  .morie_gr_need(ncol(Qa) == ncol(Ka), "Q and K must share d_k.")
  .morie_gr_need(nrow(Ka) == nrow(Va), "K and V must have the same rows.")
  .morie_gr_fin(Qa, "Q")
  .morie_gr_fin(Ka, "K")
  .morie_gr_fin(Va, "V")
  bs <- as.integer(block_size)
  .morie_gr_need(bs >= 1L, "block_size must be a positive integer.")
  Tq <- nrow(Qa)
  dk <- ncol(Qa)
  Tk <- nrow(Ka)
  dv <- ncol(Va)
  scale <- 1 / sqrt(dk)
  m <- rep(-Inf, Tq)
  l <- rep(0, Tq)
  O <- matrix(0, Tq, dv)
  n_blocks <- 0L
  for (start in seq.int(0L, Tk - 1L, by = bs)) {
    idx <- (start + 1L):min(start + bs, Tk)
    Kj <- Ka[idx, , drop = FALSE]
    Vj <- Va[idx, , drop = FALSE]
    S <- Qa %*% t(Kj) * scale
    m_tilde <- apply(S, 1L, max)
    P <- exp(S - m_tilde)
    l_tilde <- rowSums(P)
    m_new <- pmax(m, m_tilde)
    a <- exp(ifelse(is.finite(m), m, 0) - m_new) * is.finite(m)
    b <- exp(m_tilde - m_new)
    l_new <- a * l + b * l_tilde
    O <- (a * l * O + b * (P %*% Vj)) / l_new
    m <- m_new
    l <- l_new
    n_blocks <- n_blocks + 1L
  }
  R <- .morie_gr_attend(Qa, Ka, Va)$output
  list(
    output = O, reference_output = R, max_abs_error = max(abs(O - R)),
    n_blocks = n_blocks, row_max = m, row_denominator = l,
    peak_score_elements = Tq * min(bs, Tk), full_score_elements = Tq * Tk,
    block_size = bs, estimate = O, n = Tq
  )
}

# -------------------------------------------------------------- grfmp

#' Convolutional feature-map size (Geron Ch 14, morie.fn grfmp)
#'
#' @param H_out,W_out,C_out Positive output dimensions.
#' @param bytes_per_value Bytes per stored activation.
#' @param batch_size Positive batch size.
#' @return List with `dim`, `bytes`, `megabytes`, `batch_bytes`,
#'   `batch_megabytes`, `shape`.
#' @export
morie_geron_feature_map_dim <- function(H_out, W_out, C_out,
                                        bytes_per_value = 4, batch_size = 1) {
  H_out <- as.integer(H_out)
  W_out <- as.integer(W_out)
  C_out <- as.integer(C_out)
  .morie_gr_need(
    H_out >= 1L && W_out >= 1L && C_out >= 1L,
    "H_out, W_out and C_out must be positive integers."
  )
  bpv <- as.integer(bytes_per_value)
  bs <- as.integer(batch_size)
  .morie_gr_need(bpv >= 1L, "bytes_per_value must be a positive integer.")
  .morie_gr_need(bs >= 1L, "batch_size must be a positive integer.")
  dim <- as.numeric(H_out) * W_out * C_out
  nbytes <- dim * bpv
  list(
    dim = dim, bytes = nbytes, megabytes = nbytes / 2^20,
    batch_bytes = nbytes * bs, batch_megabytes = nbytes * bs / 2^20,
    shape = c(H_out, W_out, C_out), estimate = dim, n = dim
  )
}

# -------------------------------------------------------------- grfp6

#' FP16 mixed precision with loss scaling (Geron Ch 19, morie.fn grfp6)
#'
#' @param loss Non-negative finite loss.
#' @param S Positive loss scale.
#' @param gradients Optional gradients to scale and recover.
#' @return List with `loss_scaled`, `overflow`, `scaled_gradients`,
#'   `recovered_gradients`, `max_roundtrip_error`, `n_underflow_before`,
#'   `n_underflow_after`, `fp16_max`, `fp16_min_normal`.
#' @export
morie_geron_fp16_mixed_precision <- function(loss, S, gradients = NULL) {
  FP16_MAX <- 65504
  FP16_TINY <- 6.103515625e-05
  FP16_SUB <- 5.960464477539063e-08
  loss <- as.numeric(loss)
  .morie_gr_need(is.finite(loss), "loss must be finite.")
  .morie_gr_need(loss >= 0, "loss must be non-negative.")
  S <- as.numeric(S)
  .morie_gr_need(is.finite(S) && S > 0, "S must be a positive finite loss scale.")
  ls <- loss * S
  overflow <- ls > FP16_MAX
  sg <- NULL
  rg <- NULL
  err <- 0
  n_before <- 0L
  n_after <- 0L
  if (!is.null(gradients)) {
    g <- gradients
    .morie_gr_fin(g, "gradients")
    scaled <- g * S
    recovered <- scaled / S
    overflow <- overflow || any(abs(scaled) > FP16_MAX)
    n_before <- sum(g != 0 & abs(g) < FP16_TINY)
    n_after <- sum(scaled != 0 & abs(scaled) < FP16_SUB)
    err <- if (length(g)) max(abs(recovered - g)) else 0
    sg <- scaled
    rg <- recovered
  }
  list(
    loss_scaled = ls, overflow = overflow, scaled_gradients = sg,
    recovered_gradients = rg, max_roundtrip_error = err,
    n_underflow_before = n_before, n_underflow_after = n_after,
    fp16_max = FP16_MAX, fp16_min_normal = FP16_TINY, S = S,
    estimate = ls, n = if (is.null(gradients)) 1L else length(gradients)
  )
}

# -------------------------------------------------------------- grgan

#' GAN minimax objective (Geron Ch 17, morie.fn grgan)
#'
#' V = mean log D(x) + mean log(1 - D(G(z))) after clipping to
#' \[eps, 1-eps\]; the accuracy uses the UNCLIPPED probabilities.
#'
#' @param real,fake Real and fake batches (used only for their sizes).
#' @param D_real,D_fake Discriminator probabilities in \[0, 1\].
#' @param eps Clip guard in (0, 0.5).
#' @return List with `value`, `d_loss`, `g_loss_nonsaturating`,
#'   `g_loss_saturating`, `d_accuracy`, `at_equilibrium`.
#' @export
morie_geron_gan_minimax <- function(real, fake, D_real, D_fake, eps = 1e-12) {
  dr <- as.numeric(D_real)
  df <- as.numeric(D_fake)
  .morie_gr_need(
    length(dr) > 0L && length(df) > 0L,
    "D_real and D_fake must be non-empty."
  )
  .morie_gr_fin(dr, "D_real")
  .morie_gr_fin(df, "D_fake")
  .morie_gr_need(all(dr >= 0 & dr <= 1), "D_real must lie in [0, 1].")
  .morie_gr_need(all(df >= 0 & df <= 1), "D_fake must lie in [0, 1].")
  eps <- as.numeric(eps)
  .morie_gr_need(eps > 0 && eps < 0.5, "eps must lie in (0, 0.5).")
  drc <- pmin(pmax(dr, eps), 1 - eps)
  dfc <- pmin(pmax(df, eps), 1 - eps)
  value <- mean(log(drc)) + mean(log(1 - dfc))
  list(
    value = value, d_loss = -value,
    g_loss_nonsaturating = -mean(log(dfc)),
    g_loss_saturating = mean(log(1 - dfc)),
    d_accuracy = (mean(dr >= 0.5) + mean(df < 0.5)) / 2,
    at_equilibrium = abs(value + 2 * log(2)) < 1e-06,
    estimate = value, n = length(dr) + length(df)
  )
}

# -------------------------------------------------------------- grgbm

#' Gradient boosting residual stage (Geron Ch 7, morie.fn grgbm)
#'
#' r = y - F_prev; a best-SSE decision stump (midpoint cuts over the
#' sorted unique values of each feature, first minimum wins) is fitted to
#' r unless `learner` is supplied; F_new = F_prev + nu * h. The stump's
#' `feature` index is 0-based.
#'
#' @param X (m, n) design.
#' @param y Targets.
#' @param F_prev Current ensemble prediction (scalar or length m).
#' @param learner Optional function(X, r) returning m predictions.
#' @param learning_rate Shrinkage nu in (0, 1\].
#' @return List with `residuals`, `h_prediction`, `F_new`, `mse_before`,
#'   `mse_after`, `stump`.
#' @export
morie_geron_gradient_boosting_residual <- function(X, y, F_prev, learner = NULL,
                                                   learning_rate = 1) {
  A <- .morie_gr_a2d(X)
  y <- as.numeric(y)
  m <- nrow(A)
  .morie_gr_need(length(y) == m, "y length must equal nrow(X).")
  Fv <- as.numeric(F_prev)
  if (length(Fv) == 1L) Fv <- rep(Fv, m)
  .morie_gr_need(length(Fv) == m, "F_prev length must equal nrow(X).")
  .morie_gr_fin(A, "X")
  .morie_gr_fin(y, "y")
  .morie_gr_fin(Fv, "F_prev")
  nu <- as.numeric(learning_rate)
  .morie_gr_need(nu > 0 && nu <= 1, "learning_rate must lie in (0, 1].")
  resid <- y - Fv
  stump <- NULL
  if (is.null(learner)) {
    best <- NULL
    for (j in seq_len(ncol(A))) {
      vals <- sort(unique(A[, j]))
      if (length(vals) < 2L) next
      cuts <- (vals[-length(vals)] + vals[-1L]) / 2
      for (tt in cuts) {
        left <- A[, j] <= tt
        if (!any(left) || all(left)) next
        lv <- mean(resid[left])
        rv <- mean(resid[!left])
        sse <- sum((resid[left] - lv)^2) + sum((resid[!left] - rv)^2)
        if (is.null(best) || sse < best$sse) {
          best <- list(
            sse = sse, feature = j - 1L, threshold = tt,
            left_value = lv, right_value = rv
          )
        }
      }
    }
    .morie_gr_need(!is.null(best), "no split is possible.")
    h <- ifelse(A[, best$feature + 1L] <= best$threshold,
      best$left_value, best$right_value
    )
    stump <- best
  } else {
    .morie_gr_need(is.function(learner), "learner must be a function.")
    h <- as.numeric(learner(A, resid))
    .morie_gr_need(length(h) == m, "learner returned the wrong number of predictions.")
    .morie_gr_fin(h, "learner predictions")
  }
  F_new <- Fv + nu * h
  list(
    residuals = resid, h_prediction = h, F_new = F_new,
    mse_before = mean(resid^2), mse_after = mean((y - F_new)^2),
    stump = stump, learning_rate = nu, estimate = F_new, n = m
  )
}

# -------------------------------------------------------------- grgcl

#' Global-norm gradient clipping (Geron Ch 11, morie.fn grgcl)
#'
#' g *= min(1, c / (||g|| + 1e-12)) -- the norm is capped and the
#' direction preserved, so the cosine with the original is 1.
#'
#' @param gradients Numeric object, or a list of them.
#' @param c Positive threshold.
#' @return List with `clipped`, `total_norm`, `clipped_norm`,
#'   `clip_coef`, `was_clipped`, `cosine_with_original`.
#' @export
morie_geron_gradient_clipping_grgcl <- function(gradients, c) {
  c <- as.numeric(c)
  .morie_gr_need(is.finite(c) && c > 0, "c must be a positive finite threshold.")
  listed <- is.list(gradients)
  .morie_gr_need(!listed || length(gradients) > 0L, "gradients is empty.")
  flat <- if (listed) unlist(lapply(gradients, as.numeric)) else as.numeric(gradients)
  .morie_gr_need(length(flat) > 0L, "gradients is empty.")
  .morie_gr_fin(flat, "gradients")
  n0 <- sqrt(sum(flat^2))
  coef <- min(1, c / (n0 + 1e-12))
  clipped <- if (listed) lapply(gradients, function(g) g * coef) else gradients * coef
  flat_c <- if (listed) unlist(lapply(clipped, as.numeric)) else as.numeric(clipped)
  n1 <- sqrt(sum(flat_c^2))
  cosv <- if (n0 > 0 && n1 > 0) sum(flat * flat_c) / (n0 * n1) else 1
  .morie_gr_need(n1 <= c * (1 + 1e-09), "clipped norm exceeds the threshold.")
  list(
    clipped = clipped, total_norm = n0, clipped_norm = n1,
    clip_coef = coef, was_clipped = n0 > c, cosine_with_original = cosv,
    threshold = c, estimate = n1, n = length(flat)
  )
}

# -------------------------------------------------------------- grgin

#' Gini impurity (Geron Eq 5-1, morie.fn grgin)
#'
#' @param y Class label vector.
#' @return List with `gini`, `proportions`, `classes`, `counts`,
#'   `max_possible`, `majority_class`.
#' @export
morie_geron_gini_impurity_grgin <- function(y) {
  y <- as.vector(y)
  .morie_gr_need(length(y) > 0L, "y is empty.")
  classes <- sort(unique(y))
  tb <- table(y)
  counts <- as.integer(tb[as.character(classes)])
  p <- counts / sum(counts)
  K <- length(classes)
  list(
    gini = 1 - sum(p^2), proportions = p, classes = classes,
    counts = counts, max_possible = 1 - 1 / K,
    majority_class = classes[which.max(counts)],
    estimate = 1 - sum(p^2), n = length(y)
  )
}

# ------------------------------------------------------------- grgmll

#' Gaussian mixture log-likelihood (Geron Ch 9, morie.fn grgmll)
#'
#' log L = sum_i logsumexp_k \[log pi_k + log N(x_i | mu_k, Sigma_k)\],
#' the component densities computed through a Cholesky solve.
#'
#' @param X (m, d) data.
#' @param pi Mixing weights summing to 1.
#' @param means (K, d) component means.
#' @param covars K-list (or (K, d, d) array) of covariance matrices.
#' @return List with `log_likelihood`, `per_sample`,
#'   `mean_log_likelihood`, `component_log_densities`, `n_components`.
#' @export
morie_geron_gmm_log_likelihood <- function(X, pi, means, covars) {
  A <- .morie_gr_a2d(X)
  w <- as.numeric(pi)
  M <- .morie_gr_a2d(means)
  .morie_gr_need(length(A) > 0L, "X must be a non-empty 2-D array.")
  m <- nrow(A)
  d <- ncol(A)
  K <- length(w)
  .morie_gr_need(all(dim(M) == c(K, d)), "means must have shape (K, d).")
  S <- if (is.list(covars)) {
    covars
  } else {
    arr <- covars
    if (is.matrix(arr) && K == 1L) {
      list(arr)
    } else {
      lapply(seq_len(K), function(k) matrix(arr[k, , ], d, d))
    }
  }
  .morie_gr_need(length(S) == K, "covars must have K entries.")
  .morie_gr_need(all(w >= 0), "mixing weights must be non-negative.")
  .morie_gr_need(abs(sum(w) - 1) <= 1e-08, "mixing weights must sum to 1.")
  .morie_gr_fin(A, "X")
  .morie_gr_fin(M, "means")
  logp <- matrix(0, m, K)
  for (k in seq_len(K)) {
    if (w[k] == 0) {
      logp[, k] <- -Inf
      next
    }
    Sk <- matrix(as.numeric(S[[k]]), d, d)
    .morie_gr_fin(Sk, "covars")
    L <- tryCatch(t(chol(Sk)), error = function(e) {
      stop("a covariance matrix is not positive definite.", call. = FALSE)
    })
    diffm <- t(A - matrix(M[k, ], m, d, byrow = TRUE))
    sol <- forwardsolve(L, diffm)
    maha <- colSums(sol^2)
    log_det <- 2 * sum(log(diag(L)))
    # NOTE: the argument is named `pi` (the mixing weights), so the
    # circle constant must be qualified as base::pi here.
    logp[, k] <- log(w[k]) - 0.5 * (d * log(2 * base::pi) + log_det + maha)
  }
  mx <- apply(logp, 1L, max)
  per <- mx + log(rowSums(exp(logp - mx)))
  .morie_gr_need(all(is.finite(per)), "some instance has zero density under every component.")
  total <- sum(per)
  list(
    log_likelihood = total, per_sample = per,
    mean_log_likelihood = mean(per), component_log_densities = logp,
    n_components = K, estimate = total, n = m
  )
}

# ------------------------------------------------------------- grgmem

#' Gaussian mixture EM step (Geron Ch 9, morie.fn grgmem)
#'
#' E-step responsibilities from [morie_geron_gmm_log_likelihood()], then
#' the weighted pi / mu / Sigma updates, with the monotonicity of the
#' log-likelihood asserted.
#'
#' @param X (m, d) data.
#' @param pi,means,covars Current mixture parameters.
#' @param reg Non-negative ridge added to each new covariance diagonal.
#' @return List with `responsibilities`, `pi_new`, `means_new`,
#'   `covars_new`, `Nk`, `log_likelihood_before`,
#'   `log_likelihood_after`, `improvement`.
#' @export
morie_geron_gmm_em_step <- function(X, pi, means, covars, reg = 1e-06) {
  A <- .morie_gr_a2d(X)
  before <- morie_geron_gmm_log_likelihood(A, pi, means, covars)
  logp <- before$component_log_densities
  m <- nrow(A)
  d <- ncol(A)
  K <- ncol(logp)
  reg <- as.numeric(reg)
  .morie_gr_need(reg >= 0, "reg must be non-negative.")
  mx <- apply(logp, 1L, max)
  E <- exp(logp - mx)
  R <- E / rowSums(E)
  Nk <- colSums(R)
  .morie_gr_need(all(Nk > 0), "some component has zero total responsibility.")
  pi_new <- Nk / m
  mu_new <- (t(R) %*% A) / Nk
  S_new <- vector("list", K)
  for (k in seq_len(K)) {
    diffm <- A - matrix(mu_new[k, ], m, d, byrow = TRUE)
    S_new[[k]] <- t(diffm * R[, k]) %*% diffm / Nk[k] + reg * diag(d)
  }
  after <- morie_geron_gmm_log_likelihood(A, pi_new, mu_new, S_new)
  improvement <- after$log_likelihood - before$log_likelihood
  .morie_gr_need(
    improvement >= -1e-06 * max(1, abs(before$log_likelihood)),
    "the EM step decreased the log-likelihood."
  )
  list(
    responsibilities = R, pi_new = pi_new, means_new = mu_new,
    covars_new = S_new, Nk = Nk,
    log_likelihood_before = before$log_likelihood,
    log_likelihood_after = after$log_likelihood,
    improvement = improvement, estimate = mu_new, n = m
  )
}

# ------------------------------------------------------------- grgptl

#' Autoregressive next-token cross-entropy (Geron Ch 16, morie.fn grgptl)
#'
#' @param logits (T, V) matrix.
#' @param targets 0-based token ids, length T.
#' @param reduction "sum" or "mean"; selects `estimate` only.
#' @return List with `loss`, `mean_loss`, `perplexity`,
#'   `per_token_loss`, `target_logprob`.
#' @export
morie_geron_gpt_autoregressive_loss <- function(logits, targets,
                                                reduction = "sum") {
  Z <- .morie_gr_a2d(logits)
  .morie_gr_need(length(Z) > 0L, "logits must be a non-empty (T, V) array.")
  .morie_gr_fin(Z, "logits")
  Tn <- nrow(Z)
  V <- ncol(Z)
  t <- as.integer(as.numeric(targets))
  .morie_gr_need(length(t) == Tn, "targets length must equal the logit positions.")
  .morie_gr_need(
    min(t) >= 0L && max(t) < V,
    paste0("targets must lie in [0, ", V - 1L, "].")
  )
  .morie_gr_need(reduction %in% c("sum", "mean"), "reduction must be 'sum' or 'mean'.")
  logp <- .morie_gr_log_softmax_rows(Z)
  chosen <- logp[cbind(seq_len(Tn), t + 1L)]
  per <- -chosen
  total <- sum(per)
  mn <- mean(per)
  list(
    loss = total, mean_loss = mn, perplexity = exp(mn),
    per_token_loss = per, target_logprob = chosen,
    estimate = if (reduction == "sum") total else mn, n = Tn
  )
}

# -------------------------------------------------------------- grgrp

#' Gaussian random projection (Geron Ch 8, morie.fn grgrp)
#'
#' Z = X R with R_ij ~ N(0, 1/d) drawn from the reference LCG and
#' reshaped ROW-MAJOR into (n, d). `achieved_variance` is the ddof = 1
#' variance, matching the payload (not the ddof = 0 summary line).
#'
#' @param X (m, n) data.
#' @param d Target dimension.
#' @param seed LCG seed.
#' @return List with `projected`, `R`, `target_variance`,
#'   `achieved_variance`, `mean_distance_ratio`, `max_distance_ratio`.
#' @export
morie_geron_gaussian_random_projection <- function(X, d, seed = 0) {
  A <- .morie_gr_a2d(X)
  .morie_gr_need(length(A) > 0L, "X is empty.")
  .morie_gr_fin(A, "X")
  d <- as.integer(d)
  .morie_gr_need(d >= 1L, "d must be a positive integer.")
  m <- nrow(A)
  n <- ncol(A)
  R <- matrix(.morie_gr_lcg_normals(n * d, seed),
    nrow = n, ncol = d,
    byrow = TRUE
  ) * sqrt(1 / d)
  Z <- A %*% R
  ratio <- numeric(0)
  if (m > 1L) {
    ij <- utils::combn(m, 2L)
    d0 <- sqrt(colSums((t(A[ij[1L, ], , drop = FALSE]) -
      t(A[ij[2L, ], , drop = FALSE]))^2))
    d1 <- sqrt(colSums((t(Z[ij[1L, ], , drop = FALSE]) -
      t(Z[ij[2L, ], , drop = FALSE]))^2))
    keep <- d0 > 0
    ratio <- if (any(keep)) d1[keep] / d0[keep] else numeric(0)
  }
  list(
    projected = Z, R = R, target_variance = 1 / d,
    achieved_variance = if (length(R) > 1L) stats::var(as.numeric(R)) else 0,
    mean_distance_ratio = if (length(ratio)) mean(ratio) else NULL,
    max_distance_ratio = if (length(ratio)) max(ratio) else NULL,
    d = d, seed = as.integer(seed), estimate = Z, n = m
  )
}

# ------------------------------------------------------------- grgruc

#' GRU cell forward pass (Geron Ch 15, morie.fn grgruc)
#'
#' PORTED AS THE PYTHON HAS IT: this module uses the tied-gate
#' convention h = (1 - z) h_prev + z h_tilde (rather than the more common
#' z h_prev + (1 - z) h_tilde), the gates act on the concatenation
#' \[h, x\] and the candidate on \[r * h, x\], and there are no biases.
#'
#' @param x_t Input vector.
#' @param h_prev Previous hidden state.
#' @param Wz,Wr,W (H, H + n) weight matrices.
#' @return List with `h`, `z`, `r`, `h_tilde`, `update_fraction`.
#' @export
morie_geron_gru_cell <- function(x_t, h_prev, Wz, Wr, W) {
  x <- as.numeric(x_t)
  h <- as.numeric(h_prev)
  H <- length(h)
  n <- length(x)
  .morie_gr_need(H > 0L, "h_prev is empty.")
  .morie_gr_fin(x, "x_t")
  .morie_gr_fin(h, "h_prev")
  mats <- list(Wz = .morie_gr_a2d(Wz), Wr = .morie_gr_a2d(Wr), W = .morie_gr_a2d(W))
  for (nm in names(mats)) {
    .morie_gr_need(
      all(dim(mats[[nm]]) == c(H, H + n)),
      paste0(nm, " must have shape (H, H + n).")
    )
    .morie_gr_fin(mats[[nm]], nm)
  }
  hx <- c(h, x)
  z <- .morie_gr_sigmoid_vec(as.numeric(mats$Wz %*% hx))
  r <- .morie_gr_sigmoid_vec(as.numeric(mats$Wr %*% hx))
  h_tilde <- tanh(as.numeric(mats$W %*% c(r * h, x)))
  h_new <- (1 - z) * h + z * h_tilde
  list(
    h = h_new, z = z, r = r, h_tilde = h_tilde,
    update_fraction = mean(z), estimate = h_new, n = H
  )
}

# --------------------------------------------------------------- grgs

#' Exhaustive grid search with K-fold CV (Geron Ch 2, morie.fn grgs)
#'
#' Cartesian product of `param_grid` (first name varies slowest, matching
#' itertools.product) crossed with [morie_geron_kfold_cv()] splits.
#' `best_index` is 0-based; `std_scores` uses ddof = 1.
#'
#' @param X,y Data.
#' @param param_grid Named list of candidate value vectors.
#' @param K Number of folds.
#' @param fit_score function(X_tr, y_tr, X_va, y_va, params) -> numeric,
#'   higher is better.
#' @param shuffle,seed Passed to the fold generator.
#' @return List with `best_params`, `best_score`, `best_index`,
#'   `best_std`, `mean_scores`, `std_scores`, `all_scores`, `candidates`,
#'   `n_fits`.
#' @export
morie_geron_grid_search_cv <- function(X, y, param_grid, K, fit_score,
                                       shuffle = FALSE, seed = 0) {
  A <- X
  y_arr <- y
  m <- if (is.matrix(A)) nrow(A) else length(A)
  ylen <- if (is.matrix(y_arr)) nrow(y_arr) else length(y_arr)
  .morie_gr_need(ylen == m, "y rows must equal X rows.")
  .morie_gr_need(
    is.list(param_grid) && length(param_grid) > 0L,
    "param_grid must be a non-empty named list."
  )
  .morie_gr_need(is.function(fit_score), "fit_score must be a function.")
  nms <- names(param_grid)
  grid <- expand.grid(rev(lapply(param_grid, seq_along)),
    KEEP.OUT.ATTRS = FALSE
  )
  grid <- grid[, rev(seq_along(nms)), drop = FALSE]
  names(grid) <- nms
  combos <- lapply(seq_len(nrow(grid)), function(i) {
    stats::setNames(lapply(nms, function(nm) param_grid[[nm]][[grid[i, nm]]]), nms)
  })
  splits <- morie_geron_kfold_cv(m, K, shuffle = shuffle, seed = seed)$splits
  scores <- matrix(0, length(combos), length(splits))
  for (i in seq_along(combos)) {
    for (k in seq_along(splits)) {
      tr <- splits[[k]]$train + 1L
      va <- splits[[k]]$val + 1L
      Xtr <- if (is.matrix(A)) A[tr, , drop = FALSE] else A[tr]
      Xva <- if (is.matrix(A)) A[va, , drop = FALSE] else A[va]
      s <- as.numeric(fit_score(Xtr, y_arr[tr], Xva, y_arr[va], combos[[i]]))
      .morie_gr_need(
        length(s) == 1L && is.finite(s),
        "fit_score must return one finite number."
      )
      scores[i, k] <- s
    }
  }
  mn <- rowMeans(scores)
  sdv <- if (ncol(scores) > 1L) {
    apply(scores, 1L, stats::sd)
  } else {
    rep(0, length(combos))
  }
  best <- which.max(mn)
  list(
    best_params = combos[[best]], best_score = mn[best],
    best_index = best - 1L, best_std = sdv[best], mean_scores = mn,
    std_scores = sdv, all_scores = scores, candidates = combos,
    n_fits = length(combos) * length(splits), estimate = combos[[best]],
    n = m
  )
}

# -------------------------------------------------------------- grhbb

#' Perceptron (Hebb) learning rule (Geron Ch 10, morie.fn grhbb)
#'
#' @param x Input vector.
#' @param y_true,y_pred Target and predicted outputs.
#' @param w (n_in, n_out) weights.
#' @param eta Positive learning rate.
#' @return List with `w_new`, `delta_w`, `error`, `converged`,
#'   `update_norm`.
#' @export
morie_geron_hebb_rule_grhbb <- function(x, y_true, y_pred, w, eta) {
  x <- as.numeric(x)
  y_true <- as.numeric(y_true)
  y_pred <- as.numeric(y_pred)
  W <- .morie_gr_a2d(w)
  .morie_gr_need(length(y_true) == length(y_pred), "y_true and y_pred must match.")
  .morie_gr_need(
    all(dim(W) == c(length(x), length(y_true))),
    "w must have shape (n_in, n_out)."
  )
  eta <- as.numeric(eta)
  .morie_gr_need(is.finite(eta) && eta > 0, "eta must be a positive finite float.")
  .morie_gr_fin(c(x, y_true, y_pred, as.numeric(W)), "inputs")
  err <- y_true - y_pred
  dW <- eta * outer(x, err)
  list(
    w_new = W + dW, delta_w = dW, error = err, converged = all(err == 0),
    update_norm = sqrt(sum(dW^2)), eta = eta, estimate = W + dW,
    n = length(x)
  )
}

# -------------------------------------------------------------- grhei

#' He (Kaiming) normal initialization (Geron Ch 11, morie.fn grhei)
#'
#' W ~ N(0, 2/fan_in) from the reference LCG, reshaped ROW-MAJOR into
#' (fan_out, fan_in). `achieved_variance` uses ddof = 1.
#'
#' @param fan_in Positive input width.
#' @param fan_out Positive output width; defaults to `fan_in`.
#' @param seed LCG seed.
#' @return List with `W`, `target_variance`, `achieved_variance`,
#'   `achieved_mean`, `std`, `relative_error`.
#' @export
morie_geron_he_init <- function(fan_in, fan_out = NULL, seed = 0) {
  fan_in <- as.integer(fan_in)
  .morie_gr_need(fan_in >= 1L, "fan_in must be a positive integer.")
  fan_out <- if (is.null(fan_out)) fan_in else as.integer(fan_out)
  .morie_gr_need(fan_out >= 1L, "fan_out must be a positive integer.")
  var <- 2 / fan_in
  sdv <- sqrt(var)
  W <- matrix(.morie_gr_lcg_normals(fan_in * fan_out, seed),
    nrow = fan_out,
    ncol = fan_in, byrow = TRUE
  ) * sdv
  achieved <- if (length(W) > 1L) stats::var(as.numeric(W)) else 0
  list(
    W = W, target_variance = var, achieved_variance = achieved,
    achieved_mean = mean(W), std = sdv,
    relative_error = abs(achieved - var) / var, fan_in = fan_in,
    fan_out = fan_out, seed = as.integer(seed), estimate = W,
    n = length(W)
  )
}

# -------------------------------------------------------------- grhev

#' Heaviside step activation (Geron Ch 10, morie.fn grhev)
#'
#' @param z Numeric input.
#' @param threshold Finite threshold; the step is closed at it.
#' @return List with `output`, `fraction_active`, `threshold`.
#' @export
morie_geron_heaviside_step <- function(z, threshold = 0) {
  .morie_gr_fin(z, "z")
  threshold <- as.numeric(threshold)
  .morie_gr_need(is.finite(threshold), "threshold must be finite.")
  out <- (z >= threshold) * 1
  list(
    output = out, fraction_active = mean(out), threshold = threshold,
    estimate = out, n = length(out)
  )
}

# --------------------------------------------------------------- grig

#' Information gain of a split (Geron Ch 6, morie.fn grig)
#'
#' IG = H(parent) - (mL/m) H(L) - (mR/m) H(R), impurity via
#' [morie_geron_shannon_entropy()] or [morie_geron_gini_impurity_grgin()].
#'
#' @param y Class labels.
#' @param left_mask Logical (or 0/1) mask of the left child.
#' @param criterion "entropy" or "gini".
#' @return List with `information_gain`, `parent_impurity`,
#'   `left_impurity`, `right_impurity`, `weighted_child_impurity`,
#'   `m_left`, `m_right`.
#' @export
morie_geron_information_gain <- function(y, left_mask, criterion = "entropy") {
  .morie_gr_need(
    criterion %in% c("entropy", "gini"),
    "criterion must be 'entropy' or 'gini'."
  )
  y <- as.vector(y)
  mask <- as.vector(left_mask)
  if (!is.logical(mask)) {
    .morie_gr_need(all(mask %in% c(0, 1)), "left_mask must be boolean (or 0/1).")
    mask <- mask == 1
  }
  .morie_gr_need(length(mask) == length(y), "left_mask length must equal y.")
  .morie_gr_need(length(y) > 0L, "y is empty.")
  mL <- sum(mask)
  mR <- sum(!mask)
  .morie_gr_need(mL > 0L && mR > 0L, "the split sends every instance to one side.")
  imp <- function(arr) {
    if (criterion == "entropy") {
      morie_geron_shannon_entropy(arr)$entropy
    } else {
      morie_geron_gini_impurity_grgin(arr)$gini
    }
  }
  parent <- imp(y)
  left <- imp(y[mask])
  right <- imp(y[!mask])
  m <- length(y)
  child <- mL / m * left + mR / m * right
  ig <- parent - child
  .morie_gr_need(ig >= -1e-12, "information gain came out negative.")
  ig <- max(ig, 0)
  list(
    information_gain = ig, parent_impurity = parent, left_impurity = left,
    right_impurity = right, weighted_child_impurity = child,
    m_left = mL, m_right = mR, criterion = criterion, estimate = ig, n = m
  )
}

# -------------------------------------------------------------- grimp

#' Simple imputation (Geron Ch 2, morie.fn grimp)
#'
#' Fills NA per column with the mean / median / most frequent observed
#' value. numpy's mode takes the SMALLEST value among ties, which
#' `sort(unique(.))` reproduces here.
#'
#' @param X Numeric vector or matrix, NA = missing.
#' @param strategy "mean", "median", "mode" or "most_frequent".
#' @return List with `imputed`, `statistics`, `n_missing`,
#'   `missing_by_column`.
#' @export
morie_geron_simple_imputer <- function(X, strategy = "mean") {
  .morie_gr_need(
    strategy %in% c("mean", "median", "mode", "most_frequent"),
    "unknown strategy."
  )
  vector_in <- !is.matrix(X)
  A <- if (vector_in) matrix(as.numeric(X), ncol = 1L) else X
  storage.mode(A) <- "double"
  .morie_gr_need(nrow(A) > 0L, "X has no rows.")
  .morie_gr_need(!any(is.infinite(A)), "X contains +/-Inf; only NA marks missing.")
  miss <- is.na(A)
  .morie_gr_need(
    !any(colSums(miss) == nrow(A)),
    "some column is entirely missing."
  )
  stats_v <- numeric(ncol(A))
  for (j in seq_len(ncol(A))) {
    col <- A[!miss[, j], j]
    stats_v[j] <- if (strategy == "mean") {
      mean(col)
    } else if (strategy == "median") {
      stats::median(col)
    } else {
      vals <- sort(unique(col))
      cnt <- tabulate(match(col, vals))
      vals[which.max(cnt)]
    }
  }
  out <- A
  for (j in seq_len(ncol(A))) out[miss[, j], j] <- stats_v[j]
  list(
    imputed = if (vector_in) as.numeric(out) else out,
    statistics = stats_v, n_missing = sum(miss),
    missing_by_column = colSums(miss), strategy = strategy,
    estimate = if (vector_in) as.numeric(out) else out, n = nrow(A)
  )
}

# -------------------------------------------------------------- grinc

#' In-context / few-shot prompt assembly (Geron Ch 16, morie.fn grinc)
#'
#' @param examples List of length-2 (input, output) pairs.
#' @param query The held-out input.
#' @param predict Optional function(prompt) -> answer.
#' @param separator Joiner between blocks.
#' @param template Must contain both `{input}` and `{output}`.
#' @return List with `prompt`, `k_shot`, `answer`, `prompt_chars`,
#'   `prompt_words`, `example_order`.
#' @export
morie_geron_in_context_learning <- function(examples, query, predict = NULL,
                                            separator = "\n",
                                            template = "{input} -> {output}") {
  .morie_gr_need(
    grepl("{input}", template, fixed = TRUE) &&
      grepl("{output}", template, fixed = TRUE),
    "template must contain both {input} and {output} fields."
  )
  fmt <- function(a, b) {
    s <- gsub("{input}", as.character(a), template, fixed = TRUE)
    gsub("{output}", as.character(b), s, fixed = TRUE)
  }
  ex <- examples
  blocks <- character(length(ex))
  for (i in seq_along(ex)) {
    pair <- ex[[i]]
    .morie_gr_need(length(pair) == 2L, "each example must be an (input, output) pair.")
    blocks[i] <- fmt(pair[[1L]], pair[[2L]])
  }
  tail <- sub("[ \t\r\n]+$", "", fmt(query, ""))
  prompt <- paste(c(blocks, tail), collapse = separator)
  answer <- NULL
  if (!is.null(predict)) {
    .morie_gr_need(is.function(predict), "predict must be a function.")
    answer <- predict(prompt)
    .morie_gr_need(!is.null(answer), "predict returned NULL.")
  }
  list(
    prompt = prompt, k_shot = length(ex), answer = answer,
    prompt_chars = nchar(prompt),
    prompt_words = length(strsplit(trimws(prompt), "[ \t\r\n]+")[[1L]]),
    example_order = vapply(ex, function(p) as.character(p[[1L]]), character(1L)),
    estimate = if (!is.null(answer)) answer else prompt, n = length(ex)
  )
}

# -------------------------------------------------------------- grjll

#' Johnson-Lindenstrauss minimum dimension (Geron Ch 8, morie.fn grjll)
#'
#' d >= 4 log(m) / (eps^2/2 - eps^3/3), ceiling-rounded.
#'
#' @param n_samples At least 2.
#' @param eps Distortion in (0, 1); may be a vector.
#' @return List with `min_dimension`, `exact`, `denominator`, `eps`.
#' @export
morie_geron_johnson_lindenstrauss_bound <- function(n_samples, eps) {
  m <- as.integer(n_samples)
  .morie_gr_need(m >= 2L, "n_samples must be at least 2.")
  e <- as.numeric(eps)
  .morie_gr_fin(e, "eps")
  .morie_gr_need(all(e > 0 & e < 1), "eps must lie strictly in (0, 1).")
  denom <- e^2 / 2 - e^3 / 3
  d <- 4 * log(m) / denom
  list(
    min_dimension = as.integer(ceiling(d)), exact = d, denominator = denom,
    eps = e, n_samples = m, estimate = as.integer(ceiling(d)), n = m
  )
}

# -------------------------------------------------------------- grkdl

#' Knowledge distillation loss (Geron Ch 19, morie.fn grkdl)
#'
#' L = (1-a) CE(student, y) + a T^2 KL(soft student || soft teacher).
#'
#' PORTED AS THE PYTHON HAS IT: the soft term is the LITERAL reverse KL
#' sum q (log q - log t) with the STUDENT as the left argument, which is
#' the mirror image of Hinton's KL(teacher || student). Both directions
#' are returned (`kl_student_teacher`, `kl_teacher_student`) but only the
#' student-first one enters `loss`.
#'
#' @param student_logits,teacher_logits (m, K) logit matrices.
#' @param y 0-based hard labels, length m.
#' @param alpha Soft/hard mix in \[0, 1\].
#' @param T Positive temperature.
#' @return List with `loss`, `ce_hard`, `kl_soft`,
#'   `kl_student_teacher`, `kl_teacher_student`, `soft_targets`,
#'   `teacher_entropy`.
#' @export
morie_geron_knowledge_distillation_loss <- function(student_logits,
                                                    teacher_logits, y, alpha, T) {
  S <- .morie_gr_a2d(student_logits)
  Tl <- .morie_gr_a2d(teacher_logits)
  .morie_gr_need(all(dim(S) == dim(Tl)), "student and teacher logits must match.")
  .morie_gr_fin(S, "logits")
  .morie_gr_fin(Tl, "logits")
  m <- nrow(S)
  K <- ncol(S)
  lab <- as.integer(as.numeric(y))
  .morie_gr_need(length(lab) == m, "y length must equal the instances.")
  .morie_gr_need(
    min(lab) >= 0L && max(lab) < K,
    paste0("y must lie in [0, ", K - 1L, "].")
  )
  alpha <- as.numeric(alpha)
  .morie_gr_need(alpha >= 0 && alpha <= 1, "alpha must lie in [0, 1].")
  Temp <- as.numeric(T)
  .morie_gr_need(is.finite(Temp) && Temp > 0, "T must be a positive finite temperature.")
  logp_hard <- .morie_gr_log_softmax_rows(S)
  ce <- -mean(logp_hard[cbind(seq_len(m), lab + 1L)])
  logq <- .morie_gr_log_softmax_rows(S / Temp)
  logt <- .morie_gr_log_softmax_rows(Tl / Temp)
  q <- exp(logq)
  p <- exp(logt)
  kl_st <- mean(rowSums(q * (logq - logt)))
  kl_ts <- mean(rowSums(p * (logt - logq)))
  soft <- Temp^2 * kl_st
  loss <- (1 - alpha) * ce + alpha * soft
  list(
    loss = loss, ce_hard = ce, kl_soft = soft, kl_student_teacher = kl_st,
    kl_teacher_student = kl_ts,
    soft_targets = if (m == 1L) as.numeric(p) else p,
    teacher_entropy = mean(-rowSums(p * logt)), alpha = alpha, T = Temp,
    estimate = loss, n = m
  )
}

# -------------------------------------------------------------- grkfd

#' K-fold cross-validation splits (Geron Ch 2, morie.fn grkfd)
#'
#' Folds follow numpy's array_split: the first `n %% K` folds get one
#' extra member. All indices are 0-based.
#'
#' @param n Number of instances (at least 2).
#' @param K Folds in \[2, n\].
#' @param shuffle Permute with the LCG Fisher-Yates first.
#' @param seed LCG seed.
#' @return List with `splits` (each `train` / `val`), `val_folds`,
#'   `fold_sizes`, `each_used_once`, `train_size`.
#' @export
morie_geron_kfold_cv <- function(n, K, shuffle = FALSE, seed = 0) {
  n <- as.integer(n)
  K <- as.integer(K)
  .morie_gr_need(n >= 2L, "n must be at least 2 to split at all.")
  .morie_gr_need(K >= 2L && K <= n, paste0("K must lie in [2, ", n, "]."))
  order0 <- if (isTRUE(shuffle)) .morie_gr_lcg_perm(n, seed) else seq_len(n) - 1L
  folds <- .morie_gr_array_split(order0, K)
  splits <- lapply(seq_len(K), function(k) {
    list(train = unlist(folds[-k], use.names = FALSE), val = folds[[k]])
  })
  used <- sort(unlist(folds, use.names = FALSE))
  list(
    splits = splits, val_folds = folds,
    fold_sizes = vapply(folds, length, integer(1L)),
    each_used_once = identical(as.integer(used), seq_len(n) - 1L),
    train_size = vapply(splits, function(s) length(s$train), integer(1L)),
    K = K, seed = as.integer(seed), estimate = splits, n = n
  )
}

# ------------------------------------------------------------- grkldg

#' Gaussian KL to N(0, I) (Geron Ch 17, morie.fn grkldg)
#'
#' KL = -0.5 sum(1 + logvar - mu^2 - exp(logvar)); zero exactly at the
#' prior.
#'
#' @param mu,logvar Numeric objects of the same shape.
#' @return List with `kl`, `per_dimension`, `per_sample`, `variance`,
#'   `n_active_dims`.
#' @export
morie_geron_kl_divergence_gaussian <- function(mu, logvar) {
  m_arr <- mu
  lv <- logvar
  .morie_gr_need(length(m_arr) == length(lv), "mu and logvar must have the same shape.")
  .morie_gr_need(length(m_arr) > 0L, "mu is empty.")
  .morie_gr_fin(m_arr, "mu")
  .morie_gr_fin(lv, "logvar")
  .morie_gr_need(all(lv <= 80), "logvar would overflow exp().")
  var <- exp(lv)
  per_dim <- -0.5 * (1 + lv - m_arr^2 - var)
  A <- .morie_gr_a2d(per_dim)
  per_sample <- rowSums(A)
  total <- sum(per_sample)
  .morie_gr_need(total >= -1e-09, "KL came out negative.")
  total <- max(total, 0)
  list(
    kl = total, per_dimension = per_dim, per_sample = per_sample,
    variance = var, n_active_dims = sum(colMeans(A) > 0.01),
    estimate = total, n = nrow(A)
  )
}

# -------------------------------------------------------------- grkmo

#' k-means inertia (Geron Ch 9, morie.fn grkmo)
#'
#' @param X (m, n) data.
#' @param centroids (k, n) centroids.
#' @param labels 0-based assignments.
#' @return List with `inertia`, `per_cluster_inertia`, `cluster_sizes`,
#'   `distances`, `centroids_are_means`, `empty_clusters` (0-based).
#' @export
morie_geron_kmeans_objective <- function(X, centroids, labels) {
  A <- .morie_gr_a2d(X)
  C <- .morie_gr_a2d(centroids)
  lab <- as.integer(as.numeric(labels))
  .morie_gr_need(ncol(A) == ncol(C), "X and centroids must share the feature count.")
  m <- nrow(A)
  k <- nrow(C)
  .morie_gr_need(length(lab) == m, "labels length must equal nrow(X).")
  .morie_gr_need(
    min(lab) >= 0L && max(lab) < k,
    paste0("labels must lie in [0, ", k - 1L, "].")
  )
  .morie_gr_fin(A, "X")
  .morie_gr_fin(C, "centroids")
  d2 <- rowSums((A - C[lab + 1L, , drop = FALSE])^2)
  per <- vapply(seq_len(k) - 1L, function(j) sum(d2[lab == j]), numeric(1L))
  sizes <- vapply(seq_len(k) - 1L, function(j) sum(lab == j), integer(1L))
  is_mean <- TRUE
  for (j in seq_len(k) - 1L) {
    if (sizes[j + 1L] > 0L &&
      !isTRUE(all(abs(colMeans(A[lab == j, , drop = FALSE]) - C[j + 1L, ]) <= 1e-10))) {
      is_mean <- FALSE
      break
    }
  }
  list(
    inertia = sum(d2), per_cluster_inertia = per, cluster_sizes = sizes,
    distances = sqrt(d2), centroids_are_means = is_mean,
    empty_clusters = which(sizes == 0L) - 1L, estimate = sum(d2), n = m
  )
}

# ------------------------------------------------------------- grkmpp

#' k-means++ seeding (Geron Ch 9, morie.fn grkmpp)
#'
#' Centre t+1 sampled with P(x) proportional to min_c ||x - c||^2 using
#' the searchsorted-on-cumsum draw, one LCG uniform per centre. Indices
#' are 0-based.
#'
#' @param X (m, n) data.
#' @param k Centroids in \[1, m\].
#' @param seed LCG seed.
#' @return List with `centroids`, `indices`, `min_pairwise_distance`,
#'   `sampling_probabilities`.
#' @export
morie_geron_kmeans_pp_seeding <- function(X, k, seed = 0) {
  A <- .morie_gr_a2d(X)
  .morie_gr_need(length(A) > 0L, "X must be a non-empty 2-D array.")
  .morie_gr_fin(A, "X")
  m <- nrow(A)
  k <- as.integer(k)
  .morie_gr_need(k >= 1L && k <= m, paste0("k must lie in [1, ", m, "]."))
  u <- .morie_gr_lcg_u(k, seed)
  first <- min(as.integer(u[1L] * m), m - 1L)
  idx <- first
  d2 <- rowSums((A - matrix(A[first + 1L, ], m, ncol(A), byrow = TRUE))^2)
  probs <- list()
  if (k > 1L) {
    for (step in 2L:k) {
      total <- sum(d2)
      if (total <= 0) {
        remaining <- setdiff(seq_len(m) - 1L, idx)
        .morie_gr_need(length(remaining) > 0L, "not enough distinct points.")
        pick <- remaining[1L]
        probs[[length(probs) + 1L]] <- NULL
      } else {
        probs[[length(probs) + 1L]] <- d2 / total
        target <- u[step] * total
        pick <- min(sum(cumsum(d2) < target), m - 1L)
      }
      idx <- c(idx, pick)
      d2 <- pmin(d2, rowSums((A - matrix(A[pick + 1L, ], m, ncol(A), byrow = TRUE))^2))
    }
  }
  C <- A[idx + 1L, , drop = FALSE]
  mind <- if (k > 1L) {
    ij <- utils::combn(k, 2L)
    min(sqrt(colSums((t(C[ij[1L, ], , drop = FALSE]) -
      t(C[ij[2L, ], , drop = FALSE]))^2)))
  } else {
    Inf
  }
  list(
    centroids = C, indices = idx, min_pairwise_distance = mind,
    sampling_probabilities = probs, seed = as.integer(seed),
    estimate = C, n = m
  )
}

# -------------------------------------------------------------- grkpc

#' Kernel PCA with an RBF kernel (Geron Ch 8, morie.fn grkpc)
#'
#' Double-centred Gram matrix, top-d eigenpairs scaled by sqrt(lambda).
#' Eigenvector SIGNS are not pinned in the Python (numpy eigh's LAPACK
#' convention), so the parity tests compare eigenvalues, the kernel and
#' |projection|.
#'
#' @param X (m, n) data.
#' @param gamma Positive RBF width parameter.
#' @param d Components in \[1, m\].
#' @return List with `projected`, `eigenvalues`, `eigenvectors`,
#'   `explained_variance_ratio`, `kernel`, `kernel_centered`.
#' @export
morie_geron_kernel_pca_rbf <- function(X, gamma = 1, d = 2) {
  A <- .morie_gr_a2d(X)
  .morie_gr_need(nrow(A) > 0L, "X must be a non-empty 2-D array.")
  .morie_gr_fin(A, "X")
  gamma <- as.numeric(gamma)
  .morie_gr_need(is.finite(gamma) && gamma > 0, "gamma must be a positive finite float.")
  m <- nrow(A)
  d <- as.integer(d)
  .morie_gr_need(d >= 1L && d <= m, paste0("d must lie in [1, ", m, "]."))
  sq <- as.matrix(stats::dist(A))^2
  K <- exp(-gamma * sq)
  one <- matrix(1 / m, m, m)
  Kc <- K - one %*% K - K %*% one + one %*% K %*% one
  Kc <- (Kc + t(Kc)) / 2
  eg <- eigen(Kc, symmetric = TRUE)
  lam <- eg$values[seq_len(d)]
  V <- eg$vectors[, seq_len(d), drop = FALSE]
  lam_pos <- pmax(lam, 0)
  Z <- V * rep(sqrt(lam_pos), each = m)
  total <- sum(pmax(eg$values, 0))
  evr <- if (total > 0) lam_pos / total else rep(0, d)
  list(
    projected = Z, eigenvalues = lam, eigenvectors = V,
    explained_variance_ratio = evr, kernel = K, kernel_centered = Kc,
    gamma = gamma, estimate = Z, n = m
  )
}

# -------------------------------------------------------------- grkvc

#' KV-cache memory footprint (Geron Ch 16, morie.fn grkvc)
#'
#' bytes = ceil(seq * L * H * d * 2 * batch * bits / 8); note the
#' parenthesised floor-division plus remainder bump, exactly as Python.
#'
#' @param seq_len,num_layers,num_heads,d_head Positive integers.
#' @param bits Storage width per value.
#' @param batch_size Positive batch size.
#' @param baseline_bits Reference width for the compression ratio.
#' @return List with `cache_bytes`, `megabytes`, `gigabytes`,
#'   `baseline_bytes`, `compression_ratio`, `bytes_per_token`,
#'   `n_values`.
#' @export
morie_geron_kv_cache_compression <- function(seq_len, num_layers, num_heads,
                                             d_head, bits = 16, batch_size = 1,
                                             baseline_bits = 16) {
  iv <- vapply(
    list(seq_len, num_layers, num_heads, d_head),
    function(v) as.numeric(as.integer(v)), numeric(1L)
  )
  .morie_gr_need(all(iv >= 1), "seq_len, num_layers, num_heads and d_head must be positive.")
  bits <- as.integer(bits)
  baseline_bits <- as.integer(baseline_bits)
  bs <- as.integer(batch_size)
  .morie_gr_need(bits >= 1L, "bits must be a positive integer.")
  .morie_gr_need(baseline_bits >= 1L, "baseline_bits must be a positive integer.")
  .morie_gr_need(bs >= 1L, "batch_size must be a positive integer.")
  n_values <- iv[1L] * iv[2L] * iv[3L] * iv[4L] * 2 * bs
  nbytes <- (n_values * bits) %/% 8
  if ((n_values * bits) %% 8 != 0) nbytes <- nbytes + 1
  baseline <- (n_values * baseline_bits) %/% 8
  list(
    cache_bytes = nbytes, megabytes = nbytes / 2^20,
    gigabytes = nbytes / 2^30, baseline_bytes = baseline,
    compression_ratio = baseline_bits / bits,
    bytes_per_token = nbytes %/% iv[1L], n_values = n_values,
    bits = bits, estimate = nbytes, n = iv[1L]
  )
}

# --------------------------------------------------------------- grln

#' Layer normalization (Geron Ch 15, morie.fn grln)
#'
#' Per-row (x - mu)/sqrt(var + eps) with the POPULATION variance, then
#' gamma / beta. No batch axis, unlike batch norm.
#'
#' @param X Vector (one row) or (m, d) matrix, d >= 2.
#' @param gamma,beta Scalars or length-d vectors.
#' @param eps Non-negative variance floor.
#' @return List with `output`, `normalized`, `mean`, `variance`.
#' @export
morie_geron_layer_normalization <- function(X, gamma = 1, beta = 0, eps = 1e-05) {
  single <- !is.matrix(X)
  A <- if (single) matrix(as.numeric(X), nrow = 1L) else X
  storage.mode(A) <- "double"
  .morie_gr_need(ncol(A) >= 2L, "layer norm needs at least 2 features.")
  .morie_gr_fin(A, "X")
  eps <- as.numeric(eps)
  .morie_gr_need(eps >= 0, "eps must be non-negative.")
  d <- ncol(A)
  g <- as.numeric(gamma)
  b <- as.numeric(beta)
  if (length(g) == 1L) g <- rep(g, d)
  if (length(b) == 1L) b <- rep(b, d)
  .morie_gr_need(
    length(g) == d && length(b) == d,
    "gamma and beta must be scalars or length d."
  )
  mu <- rowMeans(A)
  var <- rowMeans((A - mu)^2)
  denom <- sqrt(var + eps)
  .morie_gr_need(all(denom != 0), "a constant row with eps = 0 divides by zero.")
  Xh <- (A - mu) / denom
  Y <- sweep(Xh, 2L, g, "*")
  Y <- sweep(Y, 2L, b, "+")
  list(
    output = if (single) as.numeric(Y) else Y,
    normalized = if (single) as.numeric(Xh) else Xh,
    mean = if (single) mu[1L] else mu,
    variance = if (single) var[1L] else var, eps = eps,
    estimate = if (single) as.numeric(Y) else Y, n = nrow(A)
  )
}

# -------------------------------------------------------------- grlof

#' Local outlier factor (Geron Ch 9, morie.fn grlof)
#'
#' LOF = mean(lrd(neighbour)) / lrd(x) with reachability smoothing.
#' Neighbour indices are 0-based and come from a STABLE sort of the
#' distance matrix (numpy argsort kind="stable" == order(method="radix")).
#'
#' @param X (m, n) data.
#' @param k Neighbours in \[1, m-1\].
#' @return List with `lof`, `lrd`, `k_distance`, `neighbors`,
#'   `most_outlying`.
#' @export
morie_geron_local_outlier_factor <- function(X, k = 5) {
  A <- .morie_gr_a2d(X)
  .morie_gr_need(nrow(A) > 0L, "X must be a non-empty 2-D array.")
  .morie_gr_fin(A, "X")
  m <- nrow(A)
  k <- as.integer(k)
  .morie_gr_need(
    k >= 1L && k <= m - 1L,
    paste0("k must lie in [1, ", m - 1L, "].")
  )
  D <- as.matrix(stats::dist(A))
  diag(D) <- Inf
  nbrs <- t(vapply(seq_len(m), function(i) {
    order(D[i, ], method = "radix")[seq_len(k)] - 1L
  }, integer(k)))
  if (k == 1L) nbrs <- matrix(nbrs, ncol = 1L)
  kdist <- D[cbind(seq_len(m), nbrs[, k] + 1L)]
  .morie_gr_need(all(kdist != 0), "some point has k or more exact duplicates.")
  reach <- matrix(0, m, k)
  for (i in seq_len(m)) reach[i, ] <- pmax(kdist[nbrs[i, ] + 1L], D[i, nbrs[i, ] + 1L])
  lrd <- 1 / rowMeans(reach)
  lof <- vapply(seq_len(m), function(i) mean(lrd[nbrs[i, ] + 1L]), numeric(1L)) / lrd
  list(
    lof = lof, lrd = lrd, k_distance = kdist, neighbors = nbrs,
    most_outlying = which.max(lof) - 1L, k = k, estimate = lof, n = m
  )
}

# ------------------------------------- grlogp / grlogc / grlogg / grsig

#' Logistic (sigmoid) activation (morie.fn grsig)
#'
#' The overflow-safe two-branch form; the shared kernel behind every
#' logistic routine in this file.
#'
#' @param t Numeric logits.
#' @return List with `sigma` and `derivative`.
#' @export
morie_geron_sigmoid_grsig <- function(t) {
  .morie_gr_need(length(t) > 0L, "t is empty.")
  .morie_gr_fin(t, "t")
  out <- .morie_gr_sigmoid_vec(t)
  if (is.matrix(t)) out <- matrix(out, nrow = nrow(t))
  list(
    sigma = out, derivative = out * (1 - out), estimate = out,
    n = length(t)
  )
}

#' Logistic regression probability (Geron Eq 4-15, morie.fn grlogp)
#'
#' @param X Instance vector or (m, n) design.
#' @param theta Length-n parameters.
#' @return List with `probability`, `logit`, `prediction`.
#' @export
morie_geron_logistic_regression_probability <- function(X, theta) {
  Xm <- if (is.matrix(X)) X else matrix(as.numeric(X), nrow = 1L)
  storage.mode(Xm) <- "double"
  theta <- as.numeric(theta)
  .morie_gr_need(nrow(Xm) > 0L, "X has no rows.")
  .morie_gr_need(ncol(Xm) == length(theta), "X columns must equal length(theta).")
  .morie_gr_fin(Xm, "X")
  .morie_gr_fin(theta, "theta")
  z <- as.numeric(Xm %*% theta)
  p <- .morie_gr_sigmoid_vec(z)
  list(
    probability = p, logit = z, prediction = as.integer(p >= 0.5),
    estimate = p, n = nrow(Xm)
  )
}

#' Binary logistic log-loss (Geron Eq 4-17, morie.fn grlogc)
#'
#' @param X,y,theta Design, 0/1 targets and parameters.
#' @param eps Clip guard in (0, 0.5).
#' @return List with `cost`, `probabilities`, `per_instance_loss`,
#'   `n_clipped`, `accuracy`.
#' @export
morie_geron_logistic_cross_entropy_cost <- function(X, y, theta, eps = 1e-15) {
  probs <- morie_geron_logistic_regression_probability(X, theta)
  p <- probs$probability
  y <- as.numeric(y)
  .morie_gr_need(length(y) == length(p), "y length must equal the rows of X.")
  .morie_gr_need(all(y %in% c(0, 1)), "y must contain only 0 and 1.")
  eps <- as.numeric(eps)
  .morie_gr_need(eps > 0 && eps < 0.5, "eps must lie in (0, 0.5).")
  n_clipped <- sum(p < eps | p > 1 - eps)
  pc <- pmin(pmax(p, eps), 1 - eps)
  per <- -(y * log(pc) + (1 - y) * log(1 - pc))
  list(
    cost = mean(per), probabilities = p, per_instance_loss = per,
    n_clipped = n_clipped, accuracy = mean((p >= 0.5) * 1 == y),
    estimate = mean(per), n = length(p)
  )
}

#' Logistic cost gradient (Geron Eq 4-18, morie.fn grlogg)
#'
#' @param X,y,theta Design, 0/1 targets and parameters.
#' @return List with `gradient`, `grad_norm`, `probabilities`, `errors`.
#' @export
morie_geron_logistic_cost_gradient <- function(X, y, theta) {
  probs <- morie_geron_logistic_regression_probability(X, theta)
  Xm <- if (is.matrix(X)) X else matrix(as.numeric(X), nrow = 1L)
  storage.mode(Xm) <- "double"
  p <- probs$probability
  y <- as.numeric(y)
  .morie_gr_need(length(y) == length(p), "y length must equal the rows of X.")
  .morie_gr_need(all(y %in% c(0, 1)), "y must contain only 0 and 1.")
  err <- p - y
  grad <- as.numeric(t(Xm) %*% err) / length(p)
  list(
    gradient = grad, grad_norm = sqrt(sum(grad^2)), probabilities = p,
    errors = err, estimate = grad, n = length(p)
  )
}

# ------------------------------------------------------------- grlrco

#' Cosine annealing LR schedule (Geron Ch 11, morie.fn grlrco)
#'
#' eta_t = eta_min + 0.5 (eta_max - eta_min)(1 + cos(pi t / T)); the full
#' 0..T curve is returned and its monotonicity checked.
#'
#' @param eta_min,eta_max Non-negative rates with eta_min <= eta_max.
#' @param t Step in \[0, T\].
#' @param T Positive horizon.
#' @return List with `eta`, `schedule`, `halfway_value`,
#'   `is_monotone_decreasing`.
#' @export
morie_geron_lr_cosine_annealing <- function(eta_min, eta_max, t, T) {
  eta_min <- as.numeric(eta_min)
  eta_max <- as.numeric(eta_max)
  .morie_gr_need(
    is.finite(eta_min) && is.finite(eta_max),
    "eta_min and eta_max must be finite."
  )
  .morie_gr_need(eta_min >= 0 && eta_max >= 0, "learning rates must be non-negative.")
  .morie_gr_need(eta_min <= eta_max, "eta_min must not exceed eta_max.")
  T <- as.integer(T)
  .morie_gr_need(T >= 1L, "T must be at least 1.")
  t <- as.integer(t)
  .morie_gr_need(t >= 0L && t <= T, paste0("t must lie in [0, ", T, "]."))
  steps <- 0:T
  curve <- eta_min + 0.5 * (eta_max - eta_min) * (1 + cos(pi * steps / T))
  list(
    eta = curve[t + 1L], schedule = curve, eta_min = eta_min,
    eta_max = eta_max, halfway_value = eta_min + 0.5 * (eta_max - eta_min),
    is_monotone_decreasing = all(diff(curve) <= 1e-15), t = t, T = T,
    estimate = curve[t + 1L], n = T + 1L
  )
}

# ------------------------------------------------------------- grlrex

#' Exponential LR decay (Geron Ch 11, morie.fn grlrex)
#'
#' eta_t = eta_0 gamma^t with half-life ln2 / ln(1/gamma).
#'
#' @param eta0 Positive initial rate.
#' @param gamma Decay in (0, 1\].
#' @param t Non-negative step.
#' @return List with `eta`, `schedule`, `half_life`,
#'   `is_monotone_decreasing`, `fraction_remaining`.
#' @export
morie_geron_lr_exponential_schedule <- function(eta0, gamma, t) {
  eta0 <- as.numeric(eta0)
  .morie_gr_need(is.finite(eta0) && eta0 > 0, "eta0 must be a positive finite float.")
  gamma <- as.numeric(gamma)
  .morie_gr_need(gamma > 0 && gamma <= 1, "gamma must lie in (0, 1].")
  t <- as.integer(t)
  .morie_gr_need(t >= 0L, "t must be non-negative.")
  steps <- 0:t
  curve <- eta0 * gamma^steps
  half <- if (gamma == 1) Inf else log(2) / log(1 / gamma)
  list(
    eta = curve[t + 1L], schedule = curve, half_life = half,
    is_monotone_decreasing = all(diff(curve) <= 1e-18),
    fraction_remaining = curve[t + 1L] / eta0, eta0 = eta0,
    gamma = gamma, t = t, estimate = curve[t + 1L], n = t + 1L
  )
}

# ------------------------------------------------------------- grlrnc

#' Learning curves (Geron Ch 4, morie.fn grlrnc)
#'
#' OLS refit (normal equation) on growing prefixes of the training half;
#' the split is the FIRST m - n_val rows, no shuffling.
#'
#' @param X,y Data.
#' @param n_splits Number of training sizes (unique after rounding).
#' @param val_fraction Validation share in (0, 1).
#' @return List with `train_sizes`, `train_rmse`, `val_rmse`,
#'   `final_gap`, `val_size`.
#' @export
morie_geron_learning_curves <- function(X, y, n_splits = 10,
                                        val_fraction = 0.2) {
  A <- .morie_gr_a2d(X)
  y_arr <- as.numeric(y)
  m <- nrow(A)
  n_feat <- ncol(A)
  .morie_gr_need(length(y_arr) == m, "y length must equal nrow(X).")
  val_fraction <- as.numeric(val_fraction)
  .morie_gr_need(
    val_fraction > 0 && val_fraction < 1,
    "val_fraction must lie in (0, 1)."
  )
  # numpy's round() is banker's rounding, same as R's round().
  n_val <- as.integer(round(m * val_fraction))
  .morie_gr_need(n_val >= 1L, "val_fraction leaves no validation instances.")
  n_train <- m - n_val
  .morie_gr_need(n_train >= n_feat, "too few training rows for the parameters.")
  n_splits <- as.integer(n_splits)
  .morie_gr_need(n_splits >= 1L, "n_splits must be at least 1.")
  Xtr <- A[seq_len(n_train), , drop = FALSE]
  ytr <- y_arr[seq_len(n_train)]
  Xva <- A[(n_train + 1L):m, , drop = FALSE]
  yva <- y_arr[(n_train + 1L):m]
  lin <- if (n_splits == 1L) {
    n_feat
  } else {
    seq(n_feat, n_train, length.out = n_splits)
  }
  sizes <- sort(unique(as.integer(lin)))
  tr_rmse <- numeric(0)
  va_rmse <- numeric(0)
  for (s in sizes) {
    th <- morie_geron_ch4_normal_equation(
      Xtr[seq_len(s), , drop = FALSE],
      ytr[seq_len(s)]
    )$theta
    tr_rmse <- c(tr_rmse, .morie_gr_mse_core(
      Xtr[seq_len(s), , drop = FALSE],
      ytr[seq_len(s)], th
    )$rmse)
    va_rmse <- c(va_rmse, .morie_gr_mse_core(Xva, yva, th)$rmse)
  }
  list(
    train_sizes = sizes, train_rmse = tr_rmse, val_rmse = va_rmse,
    final_gap = va_rmse[length(va_rmse)] - tr_rmse[length(tr_rmse)],
    val_size = n_val, estimate = va_rmse, n = m
  )
}

# ------------------------------------------------------------- grlstc

#' LSTM cell forward pass (Geron Ch 15, morie.fn grlstc)
#'
#' c = f c_prev + i g, h = o tanh(c); every gate acts on \[h_prev, x_t\].
#'
#' @param x_t,h_prev,c_prev State vectors.
#' @param Wf,Wi,Wg,Wo (H, H + n) gate weights.
#' @param bf,bi,bg,bo Scalars or length-H biases.
#' @return List with `h`, `c`, `f`, `i`, `g`, `o`, `forget_open`.
#' @export
morie_geron_lstm_cell <- function(x_t, h_prev, c_prev, Wf, Wi, Wg, Wo,
                                  bf, bi, bg, bo) {
  x <- as.numeric(x_t)
  h <- as.numeric(h_prev)
  cc <- as.numeric(c_prev)
  .morie_gr_need(length(h) == length(cc), "h_prev and c_prev must have equal length.")
  H <- length(h)
  n <- length(x)
  .morie_gr_need(H > 0L, "h_prev is empty.")
  z <- c(h, x)
  gates <- list()
  Ws <- list(f = Wf, i = Wi, g = Wg, o = Wo)
  bs <- list(f = bf, i = bi, g = bg, o = bo)
  for (nm in c("f", "i", "g", "o")) {
    M <- .morie_gr_a2d(Ws[[nm]])
    .morie_gr_need(
      all(dim(M) == c(H, H + n)),
      paste0("W", nm, " must have shape (H, H + n).")
    )
    bv <- as.numeric(bs[[nm]])
    if (length(bv) == 1L) bv <- rep(bv, H)
    .morie_gr_need(length(bv) == H, paste0("b", nm, " must be scalar or length H."))
    .morie_gr_fin(M, paste0("W", nm))
    .morie_gr_fin(bv, paste0("b", nm))
    gates[[nm]] <- as.numeric(M %*% z) + bv
  }
  .morie_gr_fin(c(x, h, cc), "x_t, h_prev and c_prev")
  f <- .morie_gr_sigmoid_vec(gates$f)
  i <- .morie_gr_sigmoid_vec(gates$i)
  g <- tanh(gates$g)
  o <- .morie_gr_sigmoid_vec(gates$o)
  c_new <- f * cc + i * g
  h_new <- o * tanh(c_new)
  list(
    h = h_new, c = c_new, f = f, i = i, g = g, o = o,
    forget_open = mean(f), estimate = h_new, n = H
  )
}

# -------------------------------------------------------------- grmae

#' Mean absolute error (Geron Eq 2-2, morie.fn grmae)
#'
#' @param y_true,y_pred Equal-length numeric vectors.
#' @return List with `mae`, `rmse`, `max_error`,
#'   `median_absolute_error`, `residuals`.
#' @export
morie_geron_mae_grmae <- function(y_true, y_pred) {
  yt <- as.numeric(y_true)
  yp <- as.numeric(y_pred)
  .morie_gr_need(length(yt) == length(yp), "y_true and y_pred must be equal length.")
  .morie_gr_need(length(yt) > 0L, "MAE over zero instances is undefined.")
  .morie_gr_fin(yt, "y_true")
  .morie_gr_fin(yp, "y_pred")
  resid <- yp - yt
  a <- abs(resid)
  list(
    mae = mean(a), rmse = sqrt(mean(resid^2)), max_error = max(a),
    median_absolute_error = stats::median(a), residuals = resid,
    estimate = mean(a), n = length(a)
  )
}

# ------------------------------------------------------------- grmcol

#' GAN mode coverage / collapse (Geron Ch 17, morie.fn grmcol)
#'
#' A sample counts for its nearest true mode when within `tol`; the
#' default `tol` is half the smallest inter-mode distance. Mode indices
#' are 0-based.
#'
#' @param samples (m, d) generated samples.
#' @param true_modes (K, d) mode centres.
#' @param tol Optional positive radius.
#' @return List with `coverage`, `mode_collapse_rate`, `modes_hit`,
#'   `modes_missed`, `samples_per_mode`, `n_off_distribution`, `tol`.
#' @export
morie_geron_gan_mode_collapse_metric <- function(samples, true_modes,
                                                 tol = NULL) {
  S <- .morie_gr_a2d(samples)
  M <- .morie_gr_a2d(true_modes)
  if (nrow(S) == 1L && ncol(M) != ncol(S) && ncol(S) == nrow(M)) S <- t(S)
  .morie_gr_need(ncol(S) == ncol(M), "samples and modes must share the dimension.")
  .morie_gr_need(length(S) > 0L && length(M) > 0L, "samples and true_modes must be non-empty.")
  .morie_gr_fin(S, "samples")
  .morie_gr_fin(M, "true_modes")
  K <- nrow(M)
  if (is.null(tol)) {
    .morie_gr_need(K >= 2L, "with a single true mode pass tol explicitly.")
    ij <- utils::combn(K, 2L)
    sep <- sqrt(colSums((t(M[ij[1L, ], , drop = FALSE]) -
      t(M[ij[2L, ], , drop = FALSE]))^2))
    .morie_gr_need(all(sep != 0), "true_modes contains duplicates.")
    tol <- min(sep) / 2
  }
  tol <- as.numeric(tol)
  .morie_gr_need(is.finite(tol) && tol > 0, "tol must be a positive finite radius.")
  D <- matrix(0, nrow(S), K)
  for (k in seq_len(K)) {
    D[, k] <- sqrt(rowSums((S - matrix(M[k, ], nrow(S), ncol(S), byrow = TRUE))^2))
  }
  nearest <- max.col(-D, ties.method = "first") - 1L
  within <- apply(D, 1L, min) <= tol
  counts <- vapply(
    seq_len(K) - 1L,
    function(k) sum(within & nearest == k), integer(1L)
  )
  hit <- which(counts > 0L) - 1L
  coverage <- length(hit) / K
  list(
    coverage = coverage, mode_collapse_rate = 1 - coverage,
    modes_hit = hit, modes_missed = which(counts == 0L) - 1L,
    samples_per_mode = counts, n_off_distribution = sum(!within),
    tol = tol, estimate = coverage, n = nrow(S)
  )
}

# -------------------------------------------------------------- grmgd

#' Mini-batch gradient descent (Geron Ch 4, morie.fn grmgd)
#'
#' Batches from [morie_geron_dataloader_minibatch()] with a per-epoch
#' seed of `seed + epoch`, gradient from the shared MSE core.
#'
#' @param X,y,theta Data and starting parameters.
#' @param eta Positive learning rate.
#' @param b Batch size in \[1, m\].
#' @param n_iter Number of steps.
#' @param seed Base LCG seed.
#' @return List with `theta`, `cost_history`, `theta_history`,
#'   `initial_cost`, `final_cost`.
#' @export
morie_geron_minibatch_gradient_descent <- function(X, y, theta, eta, b, n_iter,
                                                   seed = 0) {
  A <- .morie_gr_a2d(X)
  y_arr <- as.numeric(y)
  th <- as.numeric(theta)
  start <- .morie_gr_mse_core(A, y_arr, th)
  m <- nrow(A)
  b <- as.integer(b)
  .morie_gr_need(b >= 1L && b <= m, paste0("b must lie in [1, ", m, "]."))
  n_iter <- as.integer(n_iter)
  .morie_gr_need(n_iter >= 1L, "n_iter must be at least 1.")
  eta <- as.numeric(eta)
  .morie_gr_need(is.finite(eta) && eta > 0, "eta must be a positive finite float.")
  costs <- start$cost
  hist <- list(th)
  epoch <- 0L
  queue <- list()
  for (step in seq_len(n_iter)) {
    if (length(queue) == 0L) {
      queue <- morie_geron_dataloader_minibatch(m, b,
        shuffle = TRUE,
        seed = as.integer(seed) + epoch
      )$batches
      epoch <- epoch + 1L
    }
    idx <- queue[[1L]] + 1L
    queue <- queue[-1L]
    fit <- .morie_gr_mse_core(A[idx, , drop = FALSE], y_arr[idx], th)
    g <- 2 / length(idx) * as.numeric(t(A[idx, , drop = FALSE]) %*% fit$residuals)
    th <- th - eta * g
    .morie_gr_need(all(is.finite(th)), "parameters diverged; eta is too large.")
    costs <- c(costs, .morie_gr_mse_core(A, y_arr, th)$cost)
    hist[[length(hist) + 1L]] <- th
  }
  list(
    theta = th, cost_history = costs, theta_history = hist,
    initial_cost = costs[1L], final_cost = costs[length(costs)],
    eta = eta, batch_size = b, estimate = th, n = m
  )
}

# -------------------------------------------------------------- grmha

#' Multi-head attention (Geron Ch 16, morie.fn grmha)
#'
#' h heads of width d_model/h, each through the shared attention kernel,
#' concatenated then mixed by WO.
#'
#' @param Q,K,V (T, d_model) inputs.
#' @param WQ,WK,WV,WO Projection matrices with d_model rows.
#' @param h Number of heads dividing d_model.
#' @param mask Optional attention mask.
#' @return List with `output`, `head_outputs`, `attention_weights`,
#'   `concat`, `d_head`, `d_model`, `n_heads`.
#' @export
morie_geron_multi_head_attention <- function(Q, K, V, WQ, WK, WV, WO, h,
                                             mask = NULL) {
  Qa <- .morie_gr_a2d(Q)
  Ka <- .morie_gr_a2d(K)
  Va <- .morie_gr_a2d(V)
  .morie_gr_need(nrow(Ka) == nrow(Va), "K and V must have equal rows.")
  d_model <- ncol(Qa)
  .morie_gr_need(
    ncol(Ka) == d_model && ncol(Va) == d_model,
    "Q, K and V must share d_model."
  )
  h <- as.integer(h)
  .morie_gr_need(h >= 1L, "h must be a positive integer.")
  .morie_gr_need(d_model %% h == 0L, "h must divide d_model exactly.")
  d_head <- d_model %/% h
  mats <- lapply(list(WQ = WQ, WK = WK, WV = WV, WO = WO), .morie_gr_a2d)
  for (nm in names(mats)) {
    .morie_gr_need(
      nrow(mats[[nm]]) == d_model,
      paste0(nm, " must have d_model rows.")
    )
    if (nm != "WO") {
      .morie_gr_need(
        ncol(mats[[nm]]) == d_model,
        paste0(nm, " must have d_model columns.")
      )
    }
    .morie_gr_fin(mats[[nm]], nm)
  }
  .morie_gr_fin(Qa, "Q")
  .morie_gr_fin(Ka, "K")
  .morie_gr_fin(Va, "V")
  Qp <- Qa %*% mats$WQ
  Kp <- Ka %*% mats$WK
  Vp <- Va %*% mats$WV
  heads <- vector("list", h)
  weights <- vector("list", h)
  for (i in seq_len(h)) {
    sl <- ((i - 1L) * d_head + 1L):(i * d_head)
    r <- .morie_gr_attend(
      Qp[, sl, drop = FALSE], Kp[, sl, drop = FALSE],
      Vp[, sl, drop = FALSE], mask
    )
    heads[[i]] <- r$output
    weights[[i]] <- r$weights
  }
  concat <- do.call(cbind, heads)
  out <- concat %*% mats$WO
  list(
    output = out, head_outputs = heads, attention_weights = weights,
    concat = concat, d_head = d_head, d_model = d_model, n_heads = h,
    estimate = out, n = nrow(Qa)
  )
}

# -------------------------------------------------------------- grmlb

#' Multilabel thresholded prediction (Geron Ch 3, morie.fn grmlb)
#'
#' Strict `>` thresholding per label; per-label F1 via
#' [morie_geron_f1_score()], plus the micro F1 over pooled counts.
#'
#' @param X (m, K) score matrix.
#' @param Y (m, K) 0/1 targets.
#' @param thresholds Scalar or length-K thresholds.
#' @return List with `predictions`, `per_label_f1`,
#'   `per_label_precision`, `per_label_recall`, `macro_f1`, `micro_f1`,
#'   `exact_match_ratio`, `hamming_loss`.
#' @export
morie_geron_multilabel_classification <- function(X, Y, thresholds = 0.5) {
  S <- .morie_gr_a2d(X)
  Ti <- .morie_gr_a2d(Y)
  .morie_gr_need(all(dim(Ti) == dim(S)), "Y must have the same shape as X.")
  .morie_gr_fin(S, "X (scores)")
  storage.mode(Ti) <- "integer"
  .morie_gr_need(all(Ti %in% c(0L, 1L)), "Y must contain only 0 and 1.")
  m <- nrow(S)
  K <- ncol(S)
  t <- as.numeric(thresholds)
  if (length(t) == 1L) t <- rep(t, K)
  .morie_gr_need(length(t) == K, "thresholds must have one entry per label.")
  P <- matrix(0L, m, K)
  for (k in seq_len(K)) P[, k] <- as.integer(S[, k] > t[k])
  f1s <- numeric(K)
  precs <- numeric(K)
  recs <- numeric(K)
  for (k in seq_len(K)) {
    yk <- Ti[, k]
    pk <- P[, k]
    if (!any(yk == 1L) && !any(pk == 1L)) {
      f1s[k] <- 0
      precs[k] <- NA_real_
      recs[k] <- NA_real_
      next
    }
    rk <- morie_geron_f1_score(yk, pk, positive_class = 1)
    f1s[k] <- rk$f1
    precs[k] <- rk$precision
    recs[k] <- rk$recall
  }
  tp <- sum(P == 1L & Ti == 1L)
  fp <- sum(P == 1L & Ti == 0L)
  fn <- sum(P == 0L & Ti == 1L)
  micro <- if (2 * tp + fp + fn == 0) 0 else 2 * tp / (2 * tp + fp + fn)
  list(
    predictions = P, per_label_f1 = f1s, per_label_precision = precs,
    per_label_recall = recs, macro_f1 = mean(f1s), micro_f1 = micro,
    exact_match_ratio = mean(rowSums(P == Ti) == K),
    hamming_loss = mean(P != Ti), thresholds = t,
    estimate = mean(f1s), n = m
  )
}

# -------------------------------------------------------------- grmlc

#' Classification MLP softmax head (Geron Ch 10, morie.fn grmlc)
#'
#' @param a_last Last hidden activation (vector or batch).
#' @param W_out (K, hidden) output weights.
#' @param b_out Output bias.
#' @return List with `probabilities`, `logits`, `predicted_class`
#'   (0-based), `max_probability`, `n_classes`.
#' @export
morie_geron_classification_mlp_output <- function(a_last, W_out, b_out) {
  inner <- morie_geron_linear_layer_forward(a_last, W_out, b_out)
  Z <- .morie_gr_a2d(inner$output)
  .morie_gr_need(ncol(Z) >= 2L, "a softmax head needs at least 2 classes.")
  P <- .morie_al_softmax_rows(Z)
  single <- !inner$batch
  pred <- max.col(P, ties.method = "first") - 1L
  list(
    probabilities = if (single) as.numeric(P) else P,
    logits = if (single) as.numeric(Z) else Z,
    predicted_class = if (single) pred[1L] else pred,
    max_probability = if (single) max(P[1L, ]) else apply(P, 1L, max),
    n_classes = ncol(Z),
    estimate = if (single) as.numeric(P) else P, n = nrow(Z)
  )
}

# -------------------------------------------------------------- grmlm

#' BERT masked-language-model loss (Geron Ch 16, morie.fn grmlm)
#'
#' Cross-entropy on the masked positions only, the selected rows handed
#' to [morie_geron_gpt_autoregressive_loss()].
#'
#' @param logits (T, V) matrix.
#' @param targets 0-based token ids.
#' @param mask Logical (or 0/1) selector of masked positions.
#' @return List with `loss`, `mean_loss`, `perplexity`,
#'   `per_token_loss`, `masked_positions` (0-based), `n_masked`,
#'   `mask_rate`.
#' @export
morie_geron_bert_mlm_loss <- function(logits, targets, mask) {
  Z <- .morie_gr_a2d(logits)
  t <- as.integer(as.numeric(targets))
  m <- as.vector(mask)
  if (!is.logical(m)) {
    .morie_gr_need(all(m %in% c(0, 1)), "mask must be boolean (or 0/1).")
    m <- m == 1
  }
  Tn <- nrow(Z)
  .morie_gr_need(
    length(t) == Tn && length(m) == Tn,
    "targets and mask must match the logit positions."
  )
  n_masked <- sum(m)
  .morie_gr_need(n_masked > 0L, "no position is masked.")
  inner <- morie_geron_gpt_autoregressive_loss(Z[m, , drop = FALSE], t[m])
  list(
    loss = inner$loss, mean_loss = inner$mean_loss,
    perplexity = inner$perplexity, per_token_loss = inner$per_token_loss,
    masked_positions = which(m) - 1L, n_masked = n_masked,
    mask_rate = n_masked / Tn, estimate = inner$loss, n = Tn
  )
}

# ------------------------------------------------------------- grmlpf

#' MLP forward pass (Geron Ch 10, morie.fn grmlpf)
#'
#' a_l = phi(W_l `a_{l-1}` + b_l), stacked
#' [morie_geron_linear_layer_forward()] calls; the last layer uses
#' `output_activation`.
#'
#' @param x Input vector or batch.
#' @param weights,biases Lists of (out, in) matrices and biases.
#' @param activation One of "relu", "tanh", "sigmoid", "identity".
#' @param output_activation Optional override for the last layer.
#' @return List with `output`, `activations`, `layer_sizes`, `n_layers`,
#'   `n_parameters`, `dead_units`.
#' @export
morie_geron_mlp_forward <- function(x, weights, biases, activation = "relu",
                                    output_activation = NULL) {
  acts_ok <- c("relu", "tanh", "sigmoid", "identity")
  .morie_gr_need(activation %in% acts_ok, "unknown activation.")
  out_act <- if (is.null(output_activation)) activation else output_activation
  .morie_gr_need(out_act %in% acts_ok, "unknown output_activation.")
  .morie_gr_need(length(weights) > 0L, "weights is empty.")
  .morie_gr_need(
    length(weights) == length(biases),
    "weights and biases must have the same length."
  )
  apply_act <- function(nm, Z) {
    switch(nm,
      relu = pmax(Z, 0),
      tanh = tanh(Z),
      sigmoid = 1 / (1 + exp(-Z)),
      Z
    )
  }
  a <- x
  batch <- is.matrix(a)
  acts <- list(a)
  sizes <- if (batch) ncol(a) else length(a)
  n_par <- 0L
  L <- length(weights)
  for (i in seq_len(L)) {
    step <- morie_geron_linear_layer_forward(a, weights[[i]], biases[[i]])
    Z <- step$output
    a <- apply_act(if (i == L) out_act else activation, Z)
    acts[[length(acts) + 1L]] <- a
    sizes <- c(sizes, step$out_features)
    n_par <- n_par + step$n_parameters
  }
  last_hidden <- acts[[length(acts) - 1L]]
  dead <- if (activation == "relu" && L > 1L) sum(last_hidden == 0) else 0L
  list(
    output = a, activations = acts, layer_sizes = sizes, n_layers = L,
    n_parameters = n_par, dead_units = dead, estimate = a,
    n = if (batch) nrow(x) else 1L
  )
}

# -------------------------------------------------------------- grmlr

#' Regression MLP output head (Geron Ch 10, morie.fn grmlr)
#'
#' @param a_last Last hidden activation.
#' @param W_out,b_out Output weights and bias.
#' @param activation "identity", "softplus", "relu" or "sigmoid".
#' @return List with `prediction`, `preactivation`, `out_features`.
#' @export
morie_geron_regression_mlp_output <- function(a_last, W_out, b_out,
                                              activation = "identity") {
  .morie_gr_need(
    activation %in% c("identity", "softplus", "relu", "sigmoid"),
    "unknown activation."
  )
  inner <- morie_geron_linear_layer_forward(a_last, W_out, b_out)
  Z <- inner$output
  Y <- switch(activation,
    identity = Z,
    softplus = .morie_gr_logaddexp0(Z),
    relu = pmax(Z, 0),
    1 / (1 + exp(-Z))
  )
  list(
    prediction = Y, preactivation = Z, activation = activation,
    out_features = inner$out_features, estimate = Y, n = inner$n
  )
}

# -------------------------------------------------------------- grmms

#' Min-max feature scaling (Geron Ch 2, morie.fn grmms)
#'
#' @param X Numeric vector or matrix.
#' @param feature_range Increasing length-2 target range.
#' @return List with `scaled`, `data_min`, `data_max`, `data_range`,
#'   `scale`.
#' @export
morie_geron_minmax_scaler <- function(X, feature_range = c(0, 1)) {
  vector_in <- !is.matrix(X)
  A <- if (vector_in) matrix(as.numeric(X), ncol = 1L) else X
  storage.mode(A) <- "double"
  .morie_gr_need(nrow(A) > 0L, "X has no rows.")
  .morie_gr_fin(A, "X")
  lo <- as.numeric(feature_range[1L])
  hi <- as.numeric(feature_range[2L])
  .morie_gr_need(hi > lo, "feature_range must be increasing.")
  mn <- apply(A, 2L, min)
  mx <- apply(A, 2L, max)
  rng <- mx - mn
  .morie_gr_need(all(rng != 0), "a constant column divides by zero.")
  unit <- sweep(sweep(A, 2L, mn, "-"), 2L, rng, "/")
  S <- unit * (hi - lo) + lo
  list(
    scaled = if (vector_in) as.numeric(S) else S, data_min = mn,
    data_max = mx, data_range = rng, scale = (hi - lo) / rng,
    feature_range = c(lo, hi),
    estimate = if (vector_in) as.numeric(S) else S, n = nrow(A)
  )
}

# -------------------------------------------------------------- grmnr

#' Max-norm weight projection (Geron Ch 11, morie.fn grmnr)
#'
#' Per-row (axis = 1) or per-column (axis = 0) w *= min(1, r/||w||); a
#' constraint applied after the step, not a penalty. `rows_projected`
#' is 0-based.
#'
#' @param W (out, in) weights.
#' @param r Positive radius.
#' @param axis 1 for rows (default), 0 for columns.
#' @return List with `W_new`, `norms_before`, `norms_after`,
#'   `n_projected`, `rows_projected`.
#' @export
morie_geron_max_norm_regularization <- function(W, r, axis = 1) {
  A <- .morie_gr_a2d(W)
  .morie_gr_need(length(A) > 0L, "W is empty.")
  .morie_gr_fin(A, "W")
  r <- as.numeric(r)
  .morie_gr_need(is.finite(r) && r > 0, "r must be a positive finite radius.")
  axis <- as.integer(axis)
  .morie_gr_need(axis %in% c(0L, 1L), "axis must be 0 or 1.")
  # numpy axis = 1 reduces over columns -> one norm per ROW.
  norms <- if (axis == 1L) sqrt(rowSums(A^2)) else sqrt(colSums(A^2))
  scale <- ifelse(norms > r, r / ifelse(norms > 0, norms, 1), 1)
  B <- if (axis == 1L) A * scale else sweep(A, 2L, scale, "*")
  after <- if (axis == 1L) sqrt(rowSums(B^2)) else sqrt(colSums(B^2))
  hit <- which(norms > r) - 1L
  .morie_gr_need(all(after <= r * (1 + 1e-09)), "a projected row still exceeds r.")
  list(
    W_new = B, norms_before = norms, norms_after = after,
    n_projected = length(hit), rows_projected = hit, r = r,
    estimate = B, n = nrow(A)
  )
}

# -------------------------------------------------------------- grmom

#' Momentum optimizer step (Geron Ch 11, morie.fn grmom)
#'
#' v = beta v + g; theta -= eta v; terminal speedup 1/(1-beta).
#'
#' @param theta,grad,v Same-shaped numeric objects.
#' @param eta Positive learning rate.
#' @param beta Momentum in \[0, 1).
#' @return List with `theta_new`, `v_new`, `step`, `terminal_speedup`.
#' @export
morie_geron_momentum_update <- function(theta, grad, v, eta, beta = 0.9) {
  .morie_gr_need(
    length(grad) == length(theta) && length(v) == length(theta),
    "grad and v must match theta's shape."
  )
  .morie_gr_need(length(theta) > 0L, "theta is empty.")
  .morie_gr_fin(theta, "theta")
  .morie_gr_fin(grad, "grad")
  .morie_gr_fin(v, "v")
  eta <- as.numeric(eta)
  .morie_gr_need(is.finite(eta) && eta > 0, "eta must be a positive finite float.")
  beta <- as.numeric(beta)
  .morie_gr_need(beta >= 0 && beta < 1, "beta must lie in [0, 1).")
  v_new <- beta * v + grad
  step <- eta * v_new
  list(
    theta_new = theta - step, v_new = v_new, step = step,
    terminal_speedup = 1 / (1 - beta), beta = beta, eta = eta,
    estimate = sqrt(sum(step^2)), n = length(theta)
  )
}

# -------------------------------------------------------------- grmpl

#' 2D max pooling (Geron Ch 14, morie.fn grmpl)
#'
#' `argmax_indices` are 0-based positions inside the ROW-MAJOR flattened
#' window, matching numpy's argmax on a C-ordered slice.
#'
#' @param X (H, W) map.
#' @param k Window side.
#' @param stride Defaults to `k`.
#' @return List with `output`, `output_shape`, `argmax_indices`,
#'   `reduction_factor`.
#' @export
morie_geron_max_pooling <- function(X, k = 2, stride = NULL) {
  A <- .morie_gr_a2d(X)
  .morie_gr_fin(A, "X")
  k <- as.integer(k)
  .morie_gr_need(k >= 1L, "k must be a positive integer.")
  s <- if (is.null(stride)) k else as.integer(stride)
  .morie_gr_need(s >= 1L, "stride must be a positive integer.")
  H <- nrow(A)
  W <- ncol(A)
  .morie_gr_need(k <= H && k <= W, "the window does not fit in the input.")
  out_h <- (H - k) %/% s + 1L
  out_w <- (W - k) %/% s + 1L
  Y <- matrix(0, out_h, out_w)
  arg <- matrix(0L, out_h, out_w)
  for (i in seq_len(out_h)) {
    for (j in seq_len(out_w)) {
      win <- A[((i - 1L) * s + 1L):((i - 1L) * s + k),
        ((j - 1L) * s + 1L):((j - 1L) * s + k),
        drop = FALSE
      ]
      Y[i, j] <- max(win)
      arg[i, j] <- which.max(as.numeric(t(win))) - 1L
    }
  }
  list(
    output = Y, output_shape = c(out_h, out_w), argmax_indices = arg,
    reduction_factor = length(A) / length(Y), k = k, stride = s,
    estimate = Y, n = length(Y)
  )
}

# -------------------------------------------------------------- grmse

#' Linear-regression MSE cost (Geron Eq 4-3, morie.fn grmse)
#'
#' The shared core: grlaso, grelas, grn011, grn013 add penalties to it,
#' grn007 differentiates it, greast / grmgd / grlrnc iterate it.
#'
#' @param X,y,theta Design, targets and parameters.
#' @return List with `cost`, `rmse`, `residuals`, `predictions`.
#' @export
morie_geron_linreg_mse_cost_grmse <- function(X, y, theta) {
  r <- .morie_gr_mse_core(X, y, theta)
  list(
    cost = r$cost, rmse = r$rmse, residuals = r$residuals,
    predictions = r$predictions, estimate = r$cost, n = r$n
  )
}

# ------------------------------------------------------ grn002 / grn001

#' Linear regression prediction (Geron Eq 4-2, morie.fn grn002)
#'
#' y_hat = theta_0 + sum_j theta_j x_j.
#'
#' @param theta Length n+1 parameters, theta\[1\] the bias.
#' @param x Instance vector or (m, n) batch.
#' @return List with `prediction`, `contributions` (single instance
#'   only), `bias`.
#' @export
morie_geron_ch4_linear_regression_prediction <- function(theta, x) {
  theta <- as.numeric(theta)
  .morie_gr_need(length(theta) >= 1L, "theta must contain at least theta_0.")
  .morie_gr_fin(theta, "theta")
  .morie_gr_fin(x, "x")
  n <- length(theta) - 1L
  batch <- is.matrix(x)
  Xm <- if (batch) x else matrix(as.numeric(x), nrow = 1L)
  storage.mode(Xm) <- "double"
  .morie_gr_need(ncol(Xm) == n, "x features must equal length(theta) - 1.")
  pred <- as.numeric(theta[1L] + Xm %*% theta[-1L])
  list(
    prediction = if (batch) pred else pred[1L],
    contributions = if (batch) NULL else theta[-1L] * Xm[1L, ],
    bias = theta[1L], estimate = if (batch) pred else pred[1L],
    n = nrow(Xm)
  )
}

#' Life satisfaction ~ GDP per capita (Geron Eq 4-1, morie.fn grn001)
#'
#' Delegates to [morie_geron_ch4_linear_regression_prediction()].
#'
#' @param theta_0,theta_1 Finite intercept and slope.
#' @param GDP_per_capita Non-negative scalar or vector.
#' @return List with `life_satisfaction`, `theta_0`, `theta_1`.
#' @export
morie_geron_ch4_simple_linear_life_satisfaction <- function(theta_0, theta_1,
                                                            GDP_per_capita) {
  theta_0 <- as.numeric(theta_0)
  theta_1 <- as.numeric(theta_1)
  .morie_gr_need(
    is.finite(theta_0) && is.finite(theta_1),
    "theta_0 and theta_1 must be finite."
  )
  g <- as.numeric(GDP_per_capita)
  .morie_gr_fin(g, "GDP_per_capita")
  .morie_gr_need(all(g >= 0), "GDP per capita cannot be negative.")
  scalar <- length(g) == 1L && !is.matrix(GDP_per_capita)
  Xm <- matrix(g, ncol = 1L)
  inner <- morie_geron_ch4_linear_regression_prediction(c(theta_0, theta_1), Xm)
  value <- if (scalar) inner$prediction[1L] else inner$prediction
  list(
    life_satisfaction = value, theta_0 = theta_0, theta_1 = theta_1,
    estimate = value, n = nrow(Xm)
  )
}

# ------------------------------------------------------ grn005 / grn007

#' Normal equation (Geron Eq 4-5, morie.fn grn005)
#'
#' theta = (X^T X)^-1 X^T y, solved not inverted, with the rank and
#' condition number reported.
#'
#' @param X,y Design and targets, m >= n.
#' @return List with `theta`, `cost`, `residuals`, `rank`,
#'   `condition_number`.
#' @export
morie_geron_ch4_normal_equation <- function(X, y) {
  X <- .morie_gr_a2d(X)
  y <- as.numeric(y)
  m <- nrow(X)
  n <- ncol(X)
  .morie_gr_need(m > 0L && n > 0L, "X must be non-empty.")
  .morie_gr_need(length(y) == m, "y length must equal nrow(X).")
  .morie_gr_fin(X, "X")
  .morie_gr_fin(y, "y")
  .morie_gr_need(m >= n, "X^T X is singular whenever m < n.")
  G <- t(X) %*% X
  sv <- svd(G, nu = 0L, nv = 0L)$d
  rank <- sum(sv > max(dim(G)) * .Machine$double.eps * max(sv))
  .morie_gr_need(rank >= n, "X^T X is rank deficient: the features are collinear.")
  theta <- as.numeric(solve(G, t(X) %*% y))
  cond <- max(sv) / min(sv)
  fit <- .morie_gr_mse_core(X, y, theta)
  list(
    theta = theta, cost = fit$cost, residuals = fit$residuals,
    rank = rank, condition_number = cond, estimate = theta, n = m
  )
}

#' MSE gradient vector (Geron Eq 4-7, morie.fn grn007)
#'
#' grad = (2/m) X^T (X theta - y), differentiating the shared MSE core.
#'
#' @param X,y,theta Design, targets and parameters.
#' @return List with `gradient`, `cost`, `grad_norm`.
#' @export
morie_geron_ch4_mse_gradient_vector <- function(X, y, theta) {
  fit <- .morie_gr_mse_core(X, y, theta)
  X <- .morie_gr_a2d(X)
  grad <- 2 / nrow(X) * as.numeric(t(X) %*% fit$residuals)
  list(
    gradient = grad, cost = fit$cost, grad_norm = sqrt(sum(grad^2)),
    estimate = grad, n = nrow(X)
  )
}

# ------------------------------------------------------------- grn016

#' Logistic thresholded prediction (Geron Ch 4, morie.fn grn016)
#'
#' y_hat = 1 iff p_hat >= threshold; the boundary goes positive.
#'
#' @param p_hat Probabilities in \[0, 1\].
#' @param threshold Cut in \[0, 1\].
#' @return List with `y_hat`, `positive_rate`, `margin`.
#' @export
morie_geron_ch4_logistic_regression_prediction <- function(p_hat,
                                                           threshold = 0.5) {
  p <- p_hat
  .morie_gr_need(length(p) > 0L, "p_hat is empty.")
  .morie_gr_fin(p, "p_hat")
  .morie_gr_need(all(p >= 0 & p <= 1), "p_hat must be probabilities in [0, 1].")
  threshold <- as.numeric(threshold)
  .morie_gr_need(threshold >= 0 && threshold <= 1, "threshold must lie in [0, 1].")
  yhat <- (p >= threshold) * 1L
  list(
    y_hat = yhat, positive_rate = mean(yhat), margin = p - threshold,
    threshold = threshold, estimate = yhat, n = length(p)
  )
}

# --------------------------------- grn021 / grsmxs / grsmxp / grxent /
# --------------------------------- grxeng / grn024 / grtmp / grtop

#' Softmax function (morie.fn grn021)
#'
#' Max-shifted normalized exponential; `k` and `argmax` are 0-based.
#'
#' @param s Score vector.
#' @param k 0-based class of interest.
#' @param K Optional length check.
#' @return List with `probability`, `probabilities`, `argmax`.
#' @export
morie_geron_ch4_softmax_function <- function(s, k, K = NULL) {
  p <- .morie_gr_softmax_vec(s)
  if (!is.null(K)) {
    .morie_gr_need(
      as.integer(K) == length(p),
      "K must equal the score vector length."
    )
  }
  k <- as.integer(k)
  .morie_gr_need(
    k >= 0L && k < length(p),
    paste0("k must lie in [0, ", length(p) - 1L, "].")
  )
  list(
    probability = p[k + 1L], probabilities = p,
    argmax = which.max(p) - 1L, estimate = p[k + 1L], n = length(p)
  )
}

#' .morie_gr_score_matrix
#'
#' A step of the geron_train_native implementation. Called by \code{.morie_gr_probability_matrix}, \code{morie_geron_softmax_score_grsmxs}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param X A matrix; passed to \code{ncol}.
#' @param theta See Usage.
#' @return A list with \code{X}, \code{theta}, \code{scores}.
#' @export
.morie_gr_score_matrix <- function(X, theta) {
  X <- .morie_gr_a2d(X)
  Tm <- if (is.matrix(theta)) theta else matrix(as.numeric(theta), ncol = 1L)
  storage.mode(Tm) <- "double"
  .morie_gr_need(length(X) > 0L, "X must be a non-empty (m, n) matrix.")
  .morie_gr_need(nrow(Tm) == ncol(X), "theta must be (n_features, K).")
  .morie_gr_fin(X, "X")
  .morie_gr_fin(Tm, "theta")
  list(X = X, theta = Tm, scores = X %*% Tm)
}

#' Softmax class scores (morie.fn grsmxs)
#'
#' @param X (m, n) design.
#' @param theta (n, K) parameters.
#' @return List with `scores` and 0-based `argmax`.
#' @export
morie_geron_softmax_score_grsmxs <- function(X, theta) {
  r <- .morie_gr_score_matrix(X, theta)
  list(
    scores = r$scores, argmax = max.col(r$scores, ties.method = "first") - 1L,
    estimate = r$scores, n = nrow(r$X)
  )
}

#' .morie_gr_probability_matrix
#'
#' A step of the geron_train_native implementation. Called by \code{.morie_gr_gradient_matrix}, \code{morie_geron_softmax_cross_entropy_cost}, \code{morie_geron_softmax_probability}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param X Passed to \code{.morie_gr_score_matrix}.
#' @param theta Passed to \code{.morie_gr_score_matrix}.
#' @return A list with \code{X}, \code{theta}, \code{probabilities}, \code{scores}.
#' @export
.morie_gr_probability_matrix <- function(X, theta) {
  r <- .morie_gr_score_matrix(X, theta)
  P <- t(apply(r$scores, 1L, .morie_gr_softmax_vec))
  if (ncol(r$scores) == 1L) P <- matrix(P, ncol = 1L)
  list(X = r$X, theta = r$theta, probabilities = P, scores = r$scores)
}

#' Softmax regression probabilities (morie.fn grsmxp)
#'
#' Composes [morie_geron_softmax_score_grsmxs()] with the grn021 softmax.
#'
#' @param X,theta Design and (n, K) parameters.
#' @return List with `probabilities`, 0-based `predictions`, `scores`.
#' @export
morie_geron_softmax_probability <- function(X, theta) {
  r <- .morie_gr_probability_matrix(X, theta)
  list(
    probabilities = r$probabilities,
    predictions = max.col(r$probabilities, ties.method = "first") - 1L,
    scores = r$scores, estimate = r$probabilities, n = nrow(r$X)
  )
}

#' .morie_gr_one_hot
#'
#' A step of the geron_train_native implementation. Called by \code{.morie_gr_gradient_matrix}, \code{morie_geron_softmax_cross_entropy_cost}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param Y A matrix; passed to \code{nrow}.
#' @param K A count; the body uses it as \code{matrix(...)}.
#' @param m A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{Y}, as built in the body.
#' @export
.morie_gr_one_hot <- function(Y, K, m) {
  if (!is.matrix(Y) || (1L %in% dim(Y) && K != 1L)) {
    idx <- as.numeric(Y)
    .morie_gr_need(length(idx) == m, "Y label count must equal nrow(X).")
    .morie_gr_need(all(idx == round(idx)), "integer label vector Y contains non-integers.")
    idx <- as.integer(idx)
    .morie_gr_need(
      min(idx) >= 0L && max(idx) < K,
      paste0("labels must lie in [0, ", K - 1L, "].")
    )
    out <- matrix(0, m, K)
    out[cbind(seq_len(m), idx + 1L)] <- 1
    return(out)
  }
  Y <- matrix(as.numeric(Y), nrow(Y), ncol(Y))
  .morie_gr_need(all(dim(Y) == c(m, K)), "one-hot Y must have shape (m, K).")
  .morie_gr_need(
    all(Y >= 0) && all(abs(rowSums(Y) - 1) < 1e-08),
    "one-hot Y rows must be non-negative and sum to 1."
  )
  Y
}

#' Softmax cross-entropy cost (morie.fn grxent)
#'
#' @param X,theta Design and (n, K) parameters.
#' @param Y 0-based labels or a one-hot matrix.
#' @return List with `cost`, `per_instance`, `probabilities`, `accuracy`.
#' @export
morie_geron_softmax_cross_entropy_cost <- function(X, Y, theta) {
  r <- .morie_gr_probability_matrix(X, theta)
  P <- r$probabilities
  m <- nrow(P)
  K <- ncol(P)
  Yh <- .morie_gr_one_hot(Y, K, m)
  logp <- log(pmax(P, 1e-300))
  per <- -rowSums(Yh * logp)
  list(
    cost = mean(per), per_instance = per, probabilities = P,
    accuracy = mean(max.col(P, ties.method = "first") ==
      max.col(Yh, ties.method = "first")),
    estimate = mean(per), n = m
  )
}

#' .morie_gr_gradient_matrix
#'
#' A step of the geron_train_native implementation. Called by \code{morie_geron_ch4_cross_entropy_gradient_vector}, \code{morie_geron_softmax_cost_gradient}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param X Passed to \code{.morie_gr_probability_matrix}.
#' @param Y Passed to \code{.morie_gr_one_hot}.
#' @param theta Passed to \code{.morie_gr_probability_matrix}.
#' @return A list with \code{X}, \code{P}, \code{Yh}, \code{G}.
#' @export
.morie_gr_gradient_matrix <- function(X, Y, theta) {
  r <- .morie_gr_probability_matrix(X, theta)
  P <- r$probabilities
  m <- nrow(P)
  K <- ncol(P)
  Yh <- .morie_gr_one_hot(Y, K, m)
  list(X = r$X, P = P, Yh = Yh, G = t(r$X) %*% (P - Yh) / m)
}

#' Softmax cross-entropy gradient (morie.fn grxeng)
#'
#' grad = (1/m) X^T (P_hat - Y); its columns sum to zero.
#'
#' @param X,Y,theta Design, labels and (n, K) parameters.
#' @return List with `gradient`, `probabilities`, `gradient_norm`.
#' @export
morie_geron_softmax_cost_gradient <- function(X, Y, theta) {
  r <- .morie_gr_gradient_matrix(X, Y, theta)
  list(
    gradient = r$G, probabilities = r$P,
    gradient_norm = sqrt(sum(r$G^2)), estimate = r$G, n = nrow(r$X)
  )
}

#' Cross-entropy gradient for one class (morie.fn grn024)
#'
#' Selects column `k` (0-based) of the grxeng gradient rather than
#' recomputing it.
#'
#' @param X,Y,Theta Design, labels and (n, K) parameters.
#' @param k 0-based class.
#' @return List with `gradient`, `class`, `gradient_norm`, `mean_error`.
#' @export
morie_geron_ch4_cross_entropy_gradient_vector <- function(X, Y, Theta, k) {
  r <- .morie_gr_gradient_matrix(X, Y, Theta)
  k <- as.integer(k)
  .morie_gr_need(
    k >= 0L && k < ncol(r$G),
    paste0("k must lie in [0, ", ncol(r$G) - 1L, "].")
  )
  g <- r$G[, k + 1L]
  list(
    gradient = g, class = k, gradient_norm = sqrt(sum(g^2)),
    mean_error = mean(r$P[, k + 1L] - r$Yh[, k + 1L]),
    estimate = g, n = nrow(r$X)
  )
}

# -------------------------------------------------------------- grnag

#' Nesterov accelerated gradient (Geron Ch 11, morie.fn grnag)
#'
#' Gradient evaluated at the look-ahead theta - eta beta v, then
#' v = beta v + g and theta -= eta v.
#'
#' @param theta Starting parameters.
#' @param grad_fn function(theta) -> gradient.
#' @param v Velocity, same length as theta.
#' @param eta Positive learning rate.
#' @param beta Momentum in \[0, 1).
#' @param n_steps Number of steps.
#' @return List with `theta_new`, `v_new`, `lookahead`, `gradient`,
#'   `path`, `step`.
#' @export
morie_geron_nesterov_accelerated_gradient <- function(theta, grad_fn, v, eta,
                                                      beta = 0.9, n_steps = 1) {
  th <- as.numeric(theta)
  vv <- as.numeric(v)
  .morie_gr_need(length(th) > 0L, "theta is empty.")
  .morie_gr_need(length(vv) == length(th), "v length must equal theta.")
  .morie_gr_fin(th, "theta")
  .morie_gr_fin(vv, "v")
  .morie_gr_need(is.function(grad_fn), "grad_fn must be a function.")
  eta <- as.numeric(eta)
  .morie_gr_need(is.finite(eta) && eta > 0, "eta must be a positive finite float.")
  beta <- as.numeric(beta)
  .morie_gr_need(beta >= 0 && beta < 1, "beta must lie in [0, 1).")
  n_steps <- as.integer(n_steps)
  .morie_gr_need(n_steps >= 1L, "n_steps must be at least 1.")
  path <- list()
  look <- NULL
  grads <- NULL
  for (i in seq_len(n_steps)) {
    ahead <- th - eta * beta * vv
    g <- as.numeric(grad_fn(ahead))
    .morie_gr_need(length(g) == length(th), "grad_fn returned the wrong shape.")
    .morie_gr_fin(g, "grad_fn output")
    vv <- beta * vv + g
    th <- th - eta * vv
    path[[length(path) + 1L]] <- th
    look <- ahead
    grads <- g
  }
  step <- eta * vv
  list(
    theta_new = th, v_new = vv, lookahead = look, gradient = grads,
    path = path, step = step, estimate = th, n = length(th)
  )
}

# ------------------------------------------------------------- grnmfo

#' NMF Frobenius objective (Geron Ch 8, morie.fn grnmfo)
#'
#' @param X,W,H Non-negative matrices with X ~ W H.
#' @return List with `objective`, `reconstruction`, `residual`,
#'   `relative_error`, `rank`.
#' @export
morie_geron_nmf_objective <- function(X, W, H) {
  A <- .morie_gr_a2d(X)
  Wm <- .morie_gr_a2d(W)
  Hm <- .morie_gr_a2d(H)
  .morie_gr_need(length(A) > 0L, "X must be a non-empty (m, n) matrix.")
  .morie_gr_fin(A, "X")
  .morie_gr_fin(Wm, "W")
  .morie_gr_fin(Hm, "H")
  .morie_gr_need(all(A >= 0), "X has negative entries; NMF requires X >= 0.")
  .morie_gr_need(all(Wm >= 0), "W has negative entries.")
  .morie_gr_need(all(Hm >= 0), "H has negative entries.")
  .morie_gr_need(nrow(Wm) == nrow(A), "W rows must equal X rows.")
  .morie_gr_need(ncol(Hm) == ncol(A), "H columns must equal X columns.")
  .morie_gr_need(ncol(Wm) == nrow(Hm), "W and H inner dimensions must agree.")
  R <- Wm %*% Hm
  E <- A - R
  obj <- sum(E^2)
  denom <- sum(A^2)
  list(
    objective = obj, reconstruction = R, residual = E,
    relative_error = if (denom > 0) sqrt(obj / denom) else 0,
    rank = ncol(Wm), estimate = obj, n = nrow(A)
  )
}

# ------------------------------------------------------------- grnorm

#' Normal equation, closed-form OLS (morie.fn grnorm)
#'
#' Rejects a collinear design by reciprocal condition number rather than
#' by rank.
#'
#' @param X,y Design and targets.
#' @param add_intercept Prepend a column of ones.
#' @param rcond Minimum acceptable reciprocal condition number.
#' @return List with `theta`, `fitted`, `residuals`, `rss`,
#'   `condition_number`.
#' @export
morie_geron_normal_equation_grnorm <- function(X, y, add_intercept = FALSE,
                                               rcond = 1e-12) {
  A <- .morie_gr_a2d(X)
  yv <- as.numeric(y)
  .morie_gr_need(length(A) > 0L, "X must be a non-empty (m, n) matrix.")
  if (isTRUE(add_intercept)) A <- cbind(1, A)
  .morie_gr_need(nrow(A) == length(yv), "X rows must equal length(y).")
  .morie_gr_fin(A, "X")
  .morie_gr_fin(yv, "y")
  .morie_gr_need(nrow(A) >= ncol(A), "X^T X cannot have full rank.")
  G <- t(A) %*% A
  sv <- svd(G, nu = 0L, nv = 0L)$d
  rc <- if (max(sv) > 0) min(sv) / max(sv) else 0
  .morie_gr_need(rc >= rcond, "X^T X is singular; features are collinear.")
  theta <- as.numeric(solve(G, t(A) %*% yv))
  fitted <- as.numeric(A %*% theta)
  res <- yv - fitted
  list(
    theta = theta, fitted = fitted, residuals = res, rss = sum(res^2),
    condition_number = max(sv) / min(sv), estimate = theta, n = nrow(A)
  )
}

# -------------------------------------------------------------- grnsp

#' BERT next-sentence-prediction loss (morie.fn grnsp)
#'
#' A 1-D `logits` vector is promoted to (n, 2) with a zero first column,
#' so a single score is read as the IsNext logit.
#'
#' @param logits (n, 2) matrix or length-n vector.
#' @param labels 0/1 labels.
#' @return List with `loss`, `per_pair`, `probabilities`, `accuracy`,
#'   `baseline_loss`.
#' @export
morie_geron_bert_nsp_loss <- function(logits, labels) {
  Z <- if (is.matrix(logits)) {
    logits
  } else {
    cbind(rep(0, length(logits)), as.numeric(logits))
  }
  storage.mode(Z) <- "double"
  .morie_gr_need(ncol(Z) == 2L && length(Z) > 0L, "logits must be (n, 2) or (n,).")
  .morie_gr_fin(Z, "logits")
  y <- as.integer(as.numeric(labels))
  .morie_gr_need(length(y) == nrow(Z), "labels length must equal the pairs.")
  .morie_gr_need(all(y %in% c(0L, 1L)), "NSP labels must be 0 or 1.")
  logp <- .morie_gr_log_softmax_rows(Z)
  per <- -logp[cbind(seq_along(y), y + 1L)]
  p <- exp(logp)
  list(
    loss = mean(per), per_pair = per, probabilities = p,
    accuracy = mean((max.col(p, ties.method = "first") - 1L) == y),
    baseline_loss = log(2), estimate = mean(per), n = length(y)
  )
}

# -------------------------------------------------------------- grnud

#' Central-difference numerical derivative (morie.fn grnud)
#'
#' (f(x+h) - f(x-h)) / 2h with a Richardson extrapolation cross-check
#' from the 2h estimate.
#'
#' @param f Scalar-valued function.
#' @param x Scalar or numeric vector.
#' @param h Positive step.
#' @return List with `derivative`, `derivative_2h`, `richardson`,
#'   `step_error`.
#' @export
morie_geron_numerical_differentiation <- function(f, x, h = 1e-05) {
  .morie_gr_need(is.function(f), "f must be a function.")
  h <- as.numeric(h)
  .morie_gr_need(is.finite(h) && h > 0, "h must be a positive finite float.")
  xa <- as.numeric(x)
  .morie_gr_fin(xa, "x")
  scalar <- length(xa) == 1L
  .diff <- function(step) {
    out <- numeric(length(xa))
    for (i in seq_along(xa)) {
      xu <- xa
      xd <- xa
      xu[i] <- xu[i] + step
      xd[i] <- xd[i] - step
      up <- as.numeric(f(if (scalar) xu[1L] else xu))
      dn <- as.numeric(f(if (scalar) xd[1L] else xd))
      .morie_gr_need(is.finite(up) && is.finite(dn), "f returned a non-finite value.")
      out[i] <- (up - dn) / (2 * step)
    }
    out
  }
  d1 <- .diff(h)
  d2 <- .diff(2 * h)
  rich <- (4 * d1 - d2) / 3
  list(
    derivative = d1, derivative_2h = d2, richardson = rich,
    step_error = max(abs(d1 - d2)), estimate = d1,
    n = if (scalar) 1L else length(xa)
  )
}

# -------------------------------------------------------------- groft

#' Train-validation generalisation gap (morie.fn groft)
#'
#' gap_t = train_t - val_t; `best_val_epoch` and `max_gap_epoch` are
#' 0-based.
#'
#' @param train_scores,val_scores Equal-length per-epoch scores.
#' @return List with `gap`, `final_gap`, `max_gap`, `max_gap_epoch`,
#'   `best_val_epoch`, `overfitting_epochs`.
#' @export
morie_geron_overfitting_gap <- function(train_scores, val_scores) {
  tr <- as.numeric(train_scores)
  va <- as.numeric(val_scores)
  .morie_gr_need(length(tr) > 0L, "train_scores is empty.")
  .morie_gr_need(length(tr) == length(va), "score vectors must be equal length.")
  .morie_gr_fin(tr, "train_scores")
  .morie_gr_fin(va, "val_scores")
  gap <- tr - va
  list(
    gap = gap, final_gap = gap[length(gap)], max_gap = max(gap),
    max_gap_epoch = which.max(gap) - 1L,
    best_val_epoch = which.max(va) - 1L,
    overfitting_epochs = sum(gap > 0), estimate = gap[length(gap)],
    n = length(tr)
  )
}

# -------------------------------------------------------------- grohe

#' One-hot encoding (Geron Ch 2, morie.fn grohe)
#'
#' @param categories Vector of category values.
#' @param levels Optional level order; default `sort(unique(.))`.
#' @param drop_first Drop the first indicator column (dummy coding).
#' @return List with `encoded`, `levels`, `columns`, `n_columns`.
#' @export
morie_geron_one_hot_encoding_grohe <- function(categories, levels = NULL,
                                               drop_first = FALSE) {
  cats <- as.vector(categories)
  .morie_gr_need(length(cats) > 0L, "categories is empty.")
  lv <- if (is.null(levels)) sort(unique(cats)) else as.vector(levels)
  .morie_gr_need(length(unique(lv)) == length(lv), "levels contains duplicates.")
  .morie_gr_need(all(cats %in% lv), "categories contain values absent from levels.")
  M <- matrix(0, length(cats), length(lv))
  M[cbind(seq_along(cats), match(cats, lv))] <- 1
  kept <- lv
  if (isTRUE(drop_first)) {
    .morie_gr_need(length(lv) >= 2L, "drop_first needs at least two levels.")
    M <- M[, -1L, drop = FALSE]
    kept <- lv[-1L]
  }
  list(
    encoded = M, levels = lv, columns = kept, n_columns = ncol(M),
    estimate = M, n = length(cats)
  )
}

# -------------------------------------------------------------- grord

#' Ordinal encoding (Geron Ch 2, morie.fn grord)
#'
#' enc(level k) = k, 0-based over the declared level order.
#'
#' @param categories Vector of category values.
#' @param levels Optional level order; default `sort(unique(.))`.
#' @return List with `encoded`, `levels`, `mapping`.
#' @export
morie_geron_ordinal_encoding_grord <- function(categories, levels = NULL) {
  cats <- as.vector(categories)
  .morie_gr_need(length(cats) > 0L, "categories is empty.")
  lv <- if (is.null(levels)) sort(unique(cats)) else as.vector(levels)
  .morie_gr_need(length(unique(lv)) == length(lv), "levels contains duplicates.")
  .morie_gr_need(all(cats %in% lv), "categories contain values absent from levels.")
  enc <- match(cats, lv) - 1L
  list(
    encoded = enc, levels = lv,
    mapping = stats::setNames(seq_along(lv) - 1L, as.character(lv)),
    estimate = enc, n = length(cats)
  )
}

# ------------------------------------------------------- grovr / grovo

#' Batch-GD logistic fit used by the OvR / OvO reductions (morie.fn grovr)
#'
#' Prepends a bias column, starts at zero, and runs `n_iter` full-batch
#' gradient steps on the mean log-loss.
#'
#' @param X (m, n) design without a bias column.
#' @param y 0/1 targets.
#' @param eta Learning rate.
#' @param n_iter Number of steps.
#' @param l2 Optional ridge on the non-bias coefficients.
#' @return Numeric coefficient vector of length n + 1.
#' @export
morie_geron_train_logreg <- function(X, y, eta = 0.5, n_iter = 400, l2 = 0) {
  A <- cbind(1, .morie_gr_a2d(X))
  y <- as.numeric(y)
  w <- numeric(ncol(A))
  for (i in seq_len(as.integer(n_iter))) {
    p <- .morie_gr_sigmoid_vec(as.numeric(A %*% w))
    grad <- as.numeric(t(A) %*% (p - y)) / nrow(A)
    if (l2 != 0) grad[-1L] <- grad[-1L] + l2 * w[-1L]
    w <- w - eta * grad
  }
  w
}

#' One-vs-Rest multiclass reduction (Geron Ch 3, morie.fn grovr)
#'
#' K classifiers, each class against the rest, argmax of the scores.
#'
#' @param X (m, n) design.
#' @param y Integer class labels.
#' @param base_fit Optional function(X, y_binary) returning a scorer.
#' @param eta,n_iter Passed to [morie_geron_train_logreg()].
#' @return List with `predictions`, `scores`, `classes`,
#'   `n_classifiers`, `accuracy`, `coefficients`.
#' @export
morie_geron_one_vs_rest <- function(X, y, base_fit = NULL, eta = 0.5,
                                    n_iter = 400) {
  A <- .morie_gr_a2d(X)
  yv <- as.numeric(y)
  .morie_gr_need(length(A) > 0L, "X must be a non-empty (m, n) matrix.")
  .morie_gr_need(length(yv) == nrow(A), "y length must equal nrow(X).")
  .morie_gr_need(all(yv == round(yv)), "y must hold integer class labels.")
  yv <- as.integer(yv)
  classes <- sort(unique(yv))
  .morie_gr_need(length(classes) >= 2L, "OvR needs at least 2 classes.")
  .morie_gr_fin(A, "X")
  S <- matrix(0, nrow(A), length(classes))
  coefs <- vector("list", length(classes))
  for (j in seq_along(classes)) {
    yb <- as.numeric(yv == classes[j])
    if (is.null(base_fit)) {
      w <- morie_geron_train_logreg(A, yb, eta = eta, n_iter = n_iter)
      S[, j] <- as.numeric(cbind(1, A) %*% w)
      coefs[[j]] <- w
    } else {
      .morie_gr_need(is.function(base_fit), "base_fit must be a function.")
      model <- base_fit(A, yb)
      .morie_gr_need(is.function(model), "base_fit must return a callable scorer.")
      s <- as.numeric(model(A))
      .morie_gr_need(length(s) == nrow(A), "scorer returned the wrong length.")
      .morie_gr_fin(s, "scores")
      S[, j] <- s
      coefs[[j]] <- NULL
    }
  }
  pred <- classes[max.col(S, ties.method = "first")]
  list(
    predictions = pred, scores = S, classes = classes,
    n_classifiers = length(classes), accuracy = mean(pred == yv),
    coefficients = coefs, estimate = pred, n = nrow(A)
  )
}

#' One-vs-One multiclass reduction (Geron Ch 3, morie.fn grovo)
#'
#' K(K-1)/2 pairwise duels; a positive score votes for the SECOND class
#' of the pair, ties in the vote count go to the lowest class index.
#'
#' @param X (m, n) design.
#' @param y Integer class labels.
#' @param base_fit Optional function(X, y_binary) returning a scorer.
#' @param eta,n_iter Passed to [morie_geron_train_logreg()].
#' @return List with `predictions`, `votes`, `classes`, `pairs`,
#'   `n_classifiers`, `accuracy`, `ties`.
#' @export
morie_geron_one_vs_one <- function(X, y, base_fit = NULL, eta = 0.5,
                                   n_iter = 400) {
  A <- .morie_gr_a2d(X)
  yv <- as.numeric(y)
  .morie_gr_need(length(A) > 0L, "X must be a non-empty (m, n) matrix.")
  .morie_gr_need(length(yv) == nrow(A), "y length must equal nrow(X).")
  .morie_gr_need(all(yv == round(yv)), "y must hold integer class labels.")
  yv <- as.integer(yv)
  classes <- sort(unique(yv))
  K <- length(classes)
  .morie_gr_need(K >= 2L, "OvO needs at least 2 classes.")
  .morie_gr_fin(A, "X")
  votes <- matrix(0L, nrow(A), K)
  pairs <- list()
  for (i in seq_len(K - 1L)) {
    for (j in (i + 1L):K) {
      ci <- classes[i]
      cj <- classes[j]
      sel <- yv == ci | yv == cj
      .morie_gr_need(any(sel), "no instances for a pair.")
      Xp <- A[sel, , drop = FALSE]
      yp <- as.numeric(yv[sel] == cj)
      .morie_gr_need(min(yp) != max(yp), "a pair has only one class present.")
      if (is.null(base_fit)) {
        w <- morie_geron_train_logreg(Xp, yp, eta = eta, n_iter = n_iter)
        s <- as.numeric(cbind(1, A) %*% w)
      } else {
        .morie_gr_need(is.function(base_fit), "base_fit must be a function.")
        model <- base_fit(Xp, yp)
        .morie_gr_need(is.function(model), "base_fit must return a callable scorer.")
        s <- as.numeric(model(A))
        .morie_gr_need(length(s) == nrow(A), "scorer returned the wrong length.")
        .morie_gr_fin(s, "scores")
      }
      votes[s > 0, j] <- votes[s > 0, j] + 1L
      votes[s <= 0, i] <- votes[s <= 0, i] + 1L
      pairs[[length(pairs) + 1L]] <- c(ci, cj)
    }
  }
  pred <- classes[max.col(votes, ties.method = "first")]
  top <- apply(votes, 1L, max)
  ties <- sum(rowSums(votes == top) > 1L)
  list(
    predictions = pred, votes = votes, classes = classes, pairs = pairs,
    n_classifiers = length(pairs), accuracy = mean(pred == yv),
    ties = ties, estimate = pred, n = nrow(A)
  )
}

# ------------------------------------------------------------- grpcap

#' PCA projection (Geron Ch 8, morie.fn grpcap)
#'
#' Centre, SVD, project on the first d right singular vectors; each
#' component's sign is pinned so its largest-magnitude entry is positive,
#' which makes the projection deterministic across BLAS builds.
#'
#' @param X (m, n) data.
#' @param d Components in \[1, min(m, n)\].
#' @return List with `projection`, `components`, `explained_variance`,
#'   `explained_variance_ratio`, `cumulative_ratio`, `singular_values`,
#'   `mean`.
#' @export
morie_geron_pca_projection <- function(X, d) {
  A <- .morie_gr_a2d(X)
  .morie_gr_need(length(A) > 0L, "X must be a non-empty (m, n) matrix.")
  .morie_gr_fin(A, "X")
  m <- nrow(A)
  n <- ncol(A)
  d <- as.integer(d)
  .morie_gr_need(
    d >= 1L && d <= min(m, n),
    paste0("d must lie in [1, ", min(m, n), "].")
  )
  mu <- colMeans(A)
  Xc <- sweep(A, 2L, mu, "-")
  sv <- svd(Xc)
  V <- t(sv$v)[seq_len(d), , drop = FALSE]
  for (i in seq_len(d)) {
    j <- which.max(abs(V[i, ]))
    if (V[i, j] < 0) V[i, ] <- -V[i, ]
  }
  Z <- Xc %*% t(V)
  total <- sum(sv$d^2)
  var <- sv$d[seq_len(d)]^2 / max(m - 1L, 1L)
  ratio <- if (total > 0) sv$d[seq_len(d)]^2 / total else rep(0, d)
  list(
    projection = Z, components = V, explained_variance = var,
    explained_variance_ratio = ratio, cumulative_ratio = cumsum(ratio),
    singular_values = sv$d[seq_len(d)], mean = mu, estimate = Z, n = m
  )
}

# --------------------------------------------------------------- grpe

#' Sinusoidal positional encoding (Geron Ch 16, morie.fn grpe)
#'
#' PE(pos, 2i) = sin(pos / base^(2i/d)), PE(pos, 2i+1) = cos(...).
#'
#' @param seq_len Positions.
#' @param d_model Even model width, at least 2.
#' @param base Frequency base greater than 1.
#' @return List with `encoding` and `wavelengths`.
#' @export
morie_geron_sinusoidal_positional_encoding <- function(seq_len, d_model,
                                                       base = 10000) {
  seq_len <- as.integer(seq_len)
  d_model <- as.integer(d_model)
  .morie_gr_need(seq_len >= 1L, "seq_len must be at least 1.")
  .morie_gr_need(d_model >= 2L, "d_model must be at least 2.")
  .morie_gr_need(d_model %% 2L == 0L, "d_model must be even.")
  base <- as.numeric(base)
  .morie_gr_need(is.finite(base) && base > 1, "base must be finite and greater than 1.")
  pos <- seq_len(seq_len) - 1
  i <- seq_len(d_model %/% 2L) - 1
  div <- base^(2 * i / d_model)
  ang <- outer(pos, div, "/")
  PE <- matrix(0, seq_len, d_model)
  PE[, seq(1L, d_model, by = 2L)] <- sin(ang)
  PE[, seq(2L, d_model, by = 2L)] <- cos(ang)
  list(encoding = PE, wavelengths = 2 * pi * div, estimate = PE, n = seq_len)
}

# ------------------------------------------------------------- grpels

#' Peephole LSTM cell (Geron Ch 15, morie.fn grpels)
#'
#' f and i peep at `c_{t-1}`, o peeps at the NEW c_t, g has no peephole;
#' the U vectors are diagonal.
#'
#' @param x_t,h_prev,c_prev State vectors.
#' @param Wf,Wi,Wg,Wo (H, H + n) gate weights on \[h_prev, x_t\].
#' @param Uf,Ui,Uo Length-H diagonal peepholes.
#' @param bf,bi,bg,bo Length-H biases.
#' @return List with `h`, `c`, `f`, `i`, `g`, `o`.
#' @export
morie_geron_peephole_lstm_cell <- function(x_t, h_prev, c_prev, Wf, Wi, Wg, Wo,
                                           Uf, Ui, Uo, bf, bi, bg, bo) {
  x <- as.numeric(x_t)
  h <- as.numeric(h_prev)
  cc <- as.numeric(c_prev)
  n_h <- length(h)
  .morie_gr_need(n_h > 0L && length(x) > 0L, "x_t and h_prev must be non-empty.")
  .morie_gr_need(length(cc) == n_h, "c_prev must match h_prev.")
  n_cat <- n_h + length(x)
  W <- lapply(list(Wf = Wf, Wi = Wi, Wg = Wg, Wo = Wo), .morie_gr_a2d)
  for (nm in names(W)) {
    .morie_gr_need(
      all(dim(W[[nm]]) == c(n_h, n_cat)),
      paste0(nm, " must be (H, H + n).")
    )
  }
  U <- lapply(list(Uf = Uf, Ui = Ui, Uo = Uo), as.numeric)
  for (nm in names(U)) {
    .morie_gr_need(
      length(U[[nm]]) == n_h,
      paste0(nm, " must have H entries.")
    )
  }
  B <- lapply(list(bf = bf, bi = bi, bg = bg, bo = bo), as.numeric)
  for (nm in names(B)) {
    .morie_gr_need(
      length(B[[nm]]) == n_h,
      paste0(nm, " must have H entries.")
    )
  }
  for (v in c(W, U, B, list(x_t = x, h_prev = h, c_prev = cc))) {
    .morie_gr_fin(v, "inputs")
  }
  z <- c(h, x)
  f <- .morie_gr_sigmoid_vec(as.numeric(W$Wf %*% z) + U$Uf * cc + B$bf)
  i <- .morie_gr_sigmoid_vec(as.numeric(W$Wi %*% z) + U$Ui * cc + B$bi)
  g <- tanh(as.numeric(W$Wg %*% z) + B$bg)
  c_new <- f * cc + i * g
  o <- .morie_gr_sigmoid_vec(as.numeric(W$Wo %*% z) + U$Uo * c_new + B$bo)
  h_new <- o * tanh(c_new)
  list(
    h = h_new, c = c_new, f = f, i = i, g = g, o = o,
    estimate = h_new, n = n_h
  )
}

# -------------------------------------------------------------- grpex

#' Prioritized replay importance weights (Geron Ch 19, morie.fn grpex)
#'
#' P ~ (|delta| + eps)^alpha; w = (N P)^-beta normalised by its maximum,
#' so max(w) = 1. `max_weight_index` is 0-based.
#'
#' @param priorities TD errors (default) or explicit positive priorities.
#' @param N Replay buffer size; defaults to the number of priorities.
#' @param alpha,beta Prioritisation and correction exponents in \[0, 1\].
#' @param eps Non-negative floor added to |delta|.
#' @param are_td_errors Treat `priorities` as TD errors.
#' @return List with `weights`, `probabilities`, `priorities`,
#'   `max_weight_index`.
#' @export
morie_geron_prioritized_experience_weight <- function(priorities, N = NULL,
                                                      alpha = 0.6, beta = 0.4,
                                                      eps = 1e-06,
                                                      are_td_errors = TRUE) {
  d <- as.numeric(priorities)
  .morie_gr_need(length(d) > 0L, "priorities is empty.")
  .morie_gr_fin(d, "priorities")
  alpha <- as.numeric(alpha)
  beta <- as.numeric(beta)
  .morie_gr_need(alpha >= 0 && alpha <= 1, "alpha must lie in [0, 1].")
  .morie_gr_need(beta >= 0 && beta <= 1, "beta must lie in [0, 1].")
  eps <- as.numeric(eps)
  .morie_gr_need(eps >= 0, "eps must be non-negative.")
  if (isTRUE(are_td_errors)) {
    p <- abs(d) + eps
  } else {
    .morie_gr_need(all(d > 0), "explicit priorities must be positive.")
    p <- d
  }
  .morie_gr_need(!all(p == 0), "all priorities are zero.")
  pa <- p^alpha
  P <- pa / sum(pa)
  N <- if (is.null(N)) length(d) else as.integer(N)
  .morie_gr_need(N >= length(d), "N is smaller than the priorities supplied.")
  w <- (N * P)^(-beta)
  w <- w / max(w)
  list(
    weights = w, probabilities = P, priorities = p,
    max_weight_index = which.max(w) - 1L, estimate = w, n = length(d)
  )
}

# -------------------------------------------------------------- grpio

#' Perceiver IO cross-attention bottleneck (morie.fn grpio)
#'
#' Latents cross-attend to the input, then self-attend, `n_iter` times;
#' output queries decode the final latent. All attention through the
#' shared kernel, with no learned projections.
#'
#' @param X (M, d) input array.
#' @param Z_latent (N, d) latent array with N < M.
#' @param output_queries (O, d) decoding queries.
#' @param n_iter Cross+self rounds.
#' @return List with `output`, `latent`, `cross_weights`,
#'   `latent_self_weights`, `output_weights`, `complexity_ratio`.
#' @export
morie_geron_perceiver_io <- function(X, Z_latent, output_queries, n_iter = 1) {
  A <- .morie_gr_a2d(X)
  Z <- .morie_gr_a2d(Z_latent)
  O <- .morie_gr_a2d(output_queries)
  for (nm in c("X", "Z_latent", "output_queries")) {
    M <- switch(nm,
      X = A,
      Z_latent = Z,
      O
    )
    .morie_gr_need(length(M) > 0L, paste0(nm, " must be non-empty."))
    .morie_gr_fin(M, nm)
  }
  .morie_gr_need(ncol(Z) == ncol(A), "Z_latent width must equal the input width.")
  .morie_gr_need(ncol(O) == ncol(Z), "output_queries width must equal the latent width.")
  .morie_gr_need(nrow(Z) <= nrow(A), "the latent array must not exceed the input.")
  n_iter <- as.integer(n_iter)
  .morie_gr_need(n_iter >= 1L, "n_iter must be at least 1.")
  z <- Z
  cross_w <- list()
  self_w <- list()
  for (i in seq_len(n_iter)) {
    r <- .morie_gr_attend(z, A, A)
    z <- r$output
    cross_w[[i]] <- r$weights
    r2 <- .morie_gr_attend(z, z, z)
    z <- r2$output
    self_w[[i]] <- r2$weights
  }
  ro <- .morie_gr_attend(O, z, z)
  list(
    output = ro$output, latent = z, cross_weights = cross_w,
    latent_self_weights = self_w, output_weights = ro$weights,
    complexity_ratio = nrow(A) / nrow(Z), estimate = ro$output,
    n = nrow(A)
  )
}

# ------------------------------------------------------------- grpoly

#' Polynomial feature expansion (Geron Ch 4, morie.fn grpoly)
#'
#' \[1, X, X^2, ..., X^d\] per feature, powers only, no cross terms, so
#' the column order is (bias), then all features at power 1, then all at
#' power 2, and so on. `powers` entries are 0-based (feature, power)
#' pairs with (0, 0) marking the bias.
#'
#' @param X Vector or (m, n) matrix.
#' @param degree Highest power, at least 1.
#' @param include_bias Prepend a column of ones.
#' @return List with `features`, `powers`, `n_features`.
#' @export
morie_geron_polynomial_features <- function(X, degree, include_bias = TRUE) {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1L)
  storage.mode(A) <- "double"
  .morie_gr_need(length(A) > 0L, "X must be non-empty.")
  .morie_gr_fin(A, "X")
  degree <- as.integer(degree)
  .morie_gr_need(degree >= 1L, "degree must be at least 1.")
  cols <- list()
  powers <- list()
  if (isTRUE(include_bias)) {
    cols[[1L]] <- matrix(1, nrow(A), 1L)
    powers[[1L]] <- c(0L, 0L)
  }
  for (p in seq_len(degree)) {
    for (j in seq_len(ncol(A))) {
      cols[[length(cols) + 1L]] <- A[, j, drop = FALSE]^p
      powers[[length(powers) + 1L]] <- c(j - 1L, p)
    }
  }
  F <- do.call(cbind, cols)
  list(
    features = F, powers = powers, n_features = ncol(F),
    estimate = F, n = nrow(A)
  )
}

# -------------------------------------------------------------- grppo

#' PPO clipped surrogate objective (Geron Ch 19, morie.fn grppo)
#'
#' L = mean min(rA, clip(r, 1-e, 1+e) A). `clipped_fraction` uses an
#' isTRUE(all.equal)-style tolerance matching numpy's isclose defaults
#' (rtol 1e-5, atol 1e-8).
#'
#' @param ratios Positive probability ratios.
#' @param advantages Advantage estimates.
#' @param eps Clip half-width in (0, 1).
#' @return List with `objective`, `per_step`, `unclipped`, `clipped`,
#'   `clipped_fraction`.
#' @export
morie_geron_ppo_clipped_objective <- function(ratios, advantages, eps = 0.2) {
  r <- as.numeric(ratios)
  A <- as.numeric(advantages)
  .morie_gr_need(length(r) > 0L, "ratios is empty.")
  .morie_gr_need(length(r) == length(A), "ratios and advantages must match.")
  .morie_gr_fin(r, "ratios")
  .morie_gr_fin(A, "advantages")
  .morie_gr_need(all(r > 0), "probability ratios must be positive.")
  eps <- as.numeric(eps)
  .morie_gr_need(eps > 0 && eps < 1, "eps must lie in (0, 1).")
  unclipped <- r * A
  clipped <- pmin(pmax(r, 1 - eps), 1 + eps) * A
  per <- pmin(unclipped, clipped)
  close <- abs(unclipped - clipped) <= (1e-08 + 1e-05 * abs(clipped))
  list(
    objective = mean(per), per_step = per, unclipped = unclipped,
    clipped = clipped, clipped_fraction = mean(!close),
    estimate = mean(per), n = length(r)
  )
}

# --------------------------------------------------- grroc / grprc

#' .morie_gr_sorted_counts
#'
#' A step of the geron_train_native implementation. Called by \code{morie_geron_precision_recall_curve}, \code{morie_geron_roc_curve}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param y_true See Usage.
#' @param y_scores See Usage.
#' @return A list with \code{y}, \code{s}, \code{P}, \code{N}.
#' @export
.morie_gr_sorted_counts <- function(y_true, y_scores) {
  yt <- as.vector(y_true)
  s <- as.numeric(y_scores)
  .morie_gr_need(length(yt) > 0L, "y_true is empty.")
  .morie_gr_need(length(yt) == length(s), "y_true and y_scores must match.")
  .morie_gr_fin(s, "y_scores")
  .morie_gr_need(all(unique(yt) %in% c(0, 1)), "y_true must be binary 0/1.")
  P <- sum(yt == 1)
  N <- sum(yt == 0)
  .morie_gr_need(P > 0L && N > 0L, "need both classes present.")
  ord <- order(-s, method = "radix")
  list(y = as.integer(yt)[ord], s = s[ord], P = P, N = N)
}

#' ROC curve (Geron Ch 3, morie.fn grroc)
#'
#' TPR against FPR over the distinct scores (ties collapsed into one
#' point), AUC by the trapezoid rule.
#'
#' @param y_true Binary 0/1 labels.
#' @param y_scores Decision scores.
#' @return List with `fpr`, `tpr`, `thresholds`, `auc`.
#' @export
morie_geron_roc_curve <- function(y_true, y_scores) {
  r <- .morie_gr_sorted_counts(y_true, y_scores)
  ys <- r$y
  ss <- r$s
  P <- r$P
  N <- r$N
  tp <- 0L
  fp <- 0L
  fpr <- 0
  tpr <- 0
  thr <- Inf
  i <- 1L
  while (i <= length(ys)) {
    j <- i
    while (j + 1L <= length(ys) && ss[j + 1L] == ss[i]) j <- j + 1L
    tp <- tp + sum(ys[i:j] == 1L)
    fp <- fp + sum(ys[i:j] == 0L)
    fpr <- c(fpr, fp / N)
    tpr <- c(tpr, tp / P)
    thr <- c(thr, ss[i])
    i <- j + 1L
  }
  auc <- sum(diff(fpr) * (tpr[-1L] + tpr[-length(tpr)]) / 2)
  list(
    fpr = fpr, tpr = tpr, thresholds = thr, auc = auc,
    estimate = auc, n = length(ys)
  )
}

#' Precision-recall curve (Geron Ch 3, morie.fn grprc)
#'
#' Precision and recall at each distinct score; AP = sum (R_k - `R_{k-1}`)
#' P_k, with no interpolation. `best_f1` picks the first maximum.
#'
#' @param y_true Binary 0/1 labels.
#' @param y_scores Decision scores.
#' @return List with `precision`, `recall`, `thresholds`, `f1`,
#'   `average_precision`, `best_f1`, `best_threshold`.
#' @export
morie_geron_precision_recall_curve <- function(y_true, y_scores) {
  r <- .morie_gr_sorted_counts(y_true, y_scores)
  ys <- r$y
  ss <- r$s
  P <- r$P
  tp <- 0L
  fp <- 0L
  prec <- numeric(0)
  rec <- numeric(0)
  thr <- numeric(0)
  i <- 1L
  while (i <= length(ys)) {
    j <- i
    while (j + 1L <= length(ys) && ss[j + 1L] == ss[i]) j <- j + 1L
    tp <- tp + sum(ys[i:j] == 1L)
    fp <- fp + sum(ys[i:j] == 0L)
    prec <- c(prec, tp / (tp + fp))
    rec <- c(rec, tp / P)
    thr <- c(thr, ss[i])
    i <- j + 1L
  }
  ap <- sum(diff(c(0, rec)) * prec)
  f1 <- ifelse(prec + rec == 0, 0, 2 * prec * rec / (prec + rec))
  best <- which.max(f1)
  list(
    precision = prec, recall = rec, thresholds = thr, f1 = f1,
    average_precision = ap, best_f1 = f1[best],
    best_threshold = thr[best], estimate = ap, n = length(ys)
  )
}

# ------------------------------------------------------- grpre / grrec

#' Precision TP/(TP+FP) (Geron Ch 3, morie.fn grpre)
#'
#' Counts delegated to [morie_geron_confusion_matrix()]; macro averaging
#' skips classes that were never predicted.
#'
#' @param y_true,y_pred Integer labels.
#' @param positive 0-based positive class when `average` is NULL.
#' @param average NULL or "macro".
#' @return List with `precision`, `tp`, `fp`, `per_class`.
#' @export
morie_geron_precision <- function(y_true, y_pred, positive = 1,
                                  average = NULL) {
  yt <- as.integer(as.numeric(y_true))
  yp <- as.integer(as.numeric(y_pred))
  .morie_gr_need(length(yt) > 0L, "y_true is empty.")
  .morie_gr_need(length(yt) == length(yp), "y_true and y_pred must match.")
  n_classes <- max(yt, yp) + 1L
  cm <- morie_geron_confusion_matrix(yt, yp, n_classes = n_classes)
  per_class <- as.numeric(cm$precision)
  M <- matrix(as.numeric(unlist(cm$matrix)), n_classes, n_classes,
    byrow = !is.matrix(cm$matrix)
  )
  if (is.matrix(cm$matrix)) M <- matrix(as.numeric(cm$matrix), n_classes, n_classes)
  predicted <- colSums(M)
  if (is.null(average)) {
    pos <- as.integer(positive)
    .morie_gr_need(pos >= 0L && pos < n_classes, "positive class out of range.")
    .morie_gr_need(predicted[pos + 1L] != 0, "class was never predicted.")
    tp <- M[pos + 1L, pos + 1L]
    fp <- predicted[pos + 1L] - tp
    val <- tp / (tp + fp)
  } else {
    .morie_gr_need(identical(average, "macro"), "average must be NULL or 'macro'.")
    seen <- predicted > 0
    .morie_gr_need(any(seen), "no class was ever predicted.")
    val <- mean(per_class[seen])
    tp <- sum(diag(M))
    fp <- sum(M) - sum(diag(M))
  }
  list(
    precision = val, tp = as.integer(tp), fp = as.integer(fp),
    per_class = per_class, estimate = val, n = length(yt)
  )
}

#' Recall TP/(TP+FN) (Geron Ch 3, morie.fn grrec)
#'
#' @param y_true,y_pred Integer labels.
#' @param positive 0-based positive class when `average` is NULL.
#' @param average NULL or "macro".
#' @return List with `recall`, `tp`, `fn`, `per_class`, `f1`.
#' @export
morie_geron_recall <- function(y_true, y_pred, positive = 1, average = NULL) {
  yt <- as.integer(as.numeric(y_true))
  yp <- as.integer(as.numeric(y_pred))
  .morie_gr_need(length(yt) > 0L, "y_true is empty.")
  .morie_gr_need(length(yt) == length(yp), "y_true and y_pred must match.")
  n_classes <- max(yt, yp) + 1L
  cm <- morie_geron_confusion_matrix(yt, yp, n_classes = n_classes)
  M <- matrix(as.numeric(cm$matrix), n_classes, n_classes)
  support <- rowSums(M)
  per_class <- as.numeric(cm$recall)
  if (is.null(average)) {
    pos <- as.integer(positive)
    .morie_gr_need(pos >= 0L && pos < n_classes, "positive class out of range.")
    .morie_gr_need(support[pos + 1L] != 0, "class never occurs in y_true.")
    tp <- M[pos + 1L, pos + 1L]
    fn <- support[pos + 1L] - tp
    val <- tp / (tp + fn)
  } else {
    .morie_gr_need(identical(average, "macro"), "average must be NULL or 'macro'.")
    seen <- support > 0
    val <- mean(per_class[seen])
    tp <- sum(diag(M))
    fn <- sum(M) - sum(diag(M))
  }
  list(
    recall = val, tp = as.integer(tp), fn = as.integer(fn),
    per_class = per_class, f1 = cm$f1, estimate = val, n = length(yt)
  )
}

# -------------------------------------------------------------- grprn

#' Magnitude-based unstructured pruning (Geron Ch 19, morie.fn grprn)
#'
#' Zeroes the k = round(sparsity * size) smallest |W| using a STABLE
#' sort, so ties are broken by position exactly as numpy's mergesort.
#'
#' @param W Numeric object.
#' @param sparsity Requested fraction in \[0, 1).
#' @return List with `W_pruned`, `mask`, `threshold`,
#'   `achieved_sparsity`, `n_pruned`, `norm_retained`.
#' @export
morie_geron_weight_pruning <- function(W, sparsity) {
  A <- W
  .morie_gr_need(length(A) > 0L, "W is empty.")
  .morie_gr_fin(A, "W")
  sparsity <- as.numeric(sparsity)
  .morie_gr_need(sparsity >= 0 && sparsity < 1, "sparsity must lie in [0, 1).")
  # numpy ravel is row-major; flatten through t() when W is a matrix.
  mag <- if (is.matrix(A)) as.numeric(t(abs(A))) else abs(as.numeric(A))
  k <- as.integer(round(sparsity * length(mag)))
  if (k == 0L) {
    thr <- -Inf
    keep <- rep(1, length(mag))
  } else {
    ord <- order(mag, method = "radix")
    keep <- rep(1, length(mag))
    keep[ord[seq_len(k)]] <- 0
    thr <- mag[ord[k]]
  }
  mask <- if (is.matrix(A)) matrix(keep, nrow(A), ncol(A), byrow = TRUE) else keep
  P <- ifelse(mask > 0, A, 0)
  if (is.matrix(A)) P <- matrix(P, nrow(A), ncol(A))
  nA <- sqrt(sum(A^2))
  list(
    W_pruned = P, mask = mask, threshold = thr,
    achieved_sparsity = mean(mask == 0), n_pruned = k,
    norm_retained = if (nA > 0) sqrt(sum(P^2)) / nA else 0,
    estimate = P, n = length(A)
  )
}

# --------------------------------------------------------------- grq8

#' Symmetric quantization kernel (morie.fn grq8 `quantize_symmetric`)
#'
#' @param x Numeric object.
#' @param bits Width in \[2, 32\].
#' @return List with `q`, `scale`, `dequantized`.
#' @export
morie_geron_quantize_symmetric <- function(x, bits = 8) {
  a <- x
  .morie_gr_need(length(a) > 0L, "x is empty.")
  .morie_gr_fin(a, "x")
  bits <- as.integer(bits)
  .morie_gr_need(bits >= 2L && bits <= 32L, "bits must lie in [2, 32].")
  qmax <- 2^(bits - 1L) - 1
  amax <- max(abs(a))
  .morie_gr_need(amax != 0, "x is all zeros.")
  s <- amax / qmax
  q <- pmin(pmax(round(a / s), -qmax), qmax)
  list(q = q, scale = s, dequantized = q * s)
}

#' Symmetric INT8 quantization (Geron Ch 19, morie.fn grq8)
#'
#' s = max|x|/qmax, q = round(x/s) clipped; zero maps to zero exactly.
#'
#' @param x Numeric object.
#' @param bits Width in \[2, 32\].
#' @return List with `q`, `scale`, `dequantized`, `max_abs_error`,
#'   `snr_db`.
#' @export
morie_geron_int8_quantization <- function(x, bits = 8) {
  r <- morie_geron_quantize_symmetric(x, bits)
  a <- x
  err <- r$dequantized - a
  signal <- sum(a^2)
  noise <- sum(err^2)
  list(
    q = r$q, scale = r$scale, dequantized = r$dequantized,
    max_abs_error = max(abs(err)),
    snr_db = if (noise == 0) Inf else 10 * log10(signal / noise),
    bits = as.integer(bits), estimate = r$q, n = length(a)
  )
}

# -------------------------------------------------------------- grqat

#' Quantization-aware training (Geron Ch 19, morie.fn grqat)
#'
#' y = s clip(round(x/s)); the straight-through estimator passes the
#' gradient in range and kills it where clipped.
#'
#' @param x Numeric object.
#' @param s Positive step size.
#' @param bits Width in \[2, 32\].
#' @param upstream_grad Optional gradient of the same shape.
#' @return List with `y`, `q`, `ste_mask`, `grad_x`, `clipped_fraction`.
#' @export
morie_geron_quantization_aware_training <- function(x, s, bits = 8,
                                                    upstream_grad = NULL) {
  a <- x
  .morie_gr_need(length(a) > 0L, "x is empty.")
  .morie_gr_fin(a, "x")
  s <- as.numeric(s)
  .morie_gr_need(is.finite(s) && s > 0, "s must be a positive finite step.")
  bits <- as.integer(bits)
  .morie_gr_need(bits >= 2L && bits <= 32L, "bits must lie in [2, 32].")
  qmax <- 2^(bits - 1L) - 1
  q <- pmin(pmax(round(a / s), -qmax), qmax)
  y <- q * s
  mask <- (abs(a / s) <= qmax) * 1
  g <- if (is.null(upstream_grad)) {
    if (is.matrix(a)) matrix(1, nrow(a), ncol(a)) else rep(1, length(a))
  } else {
    .morie_gr_need(
      length(upstream_grad) == length(a),
      "upstream_grad shape != x shape."
    )
    .morie_gr_fin(upstream_grad, "upstream_grad")
    upstream_grad
  }
  list(
    y = y, q = q, ste_mask = mask, grad_x = g * mask,
    clipped_fraction = 1 - mean(mask), estimate = y, n = length(a)
  )
}

# --------------------------------------------------------------- grql

#' Q-learning update (Geron Ch 18, morie.fn grql)
#'
#' Q(s,a) += alpha \[r + gamma max_a' Q(s',a') - Q(s,a)\]; `done` drops the
#' bootstrap. State and action indices are 0-based.
#'
#' @param Q (n_states, n_actions) table.
#' @param s,a,s_next 0-based indices.
#' @param r Finite reward.
#' @param alpha Step size in (0, 1\].
#' @param gamma Discount in \[0, 1\].
#' @param done Terminal flag.
#' @return List with `Q`, `old_value`, `new_value`, `target`,
#'   `td_error`, `max_next`.
#' @export
morie_geron_q_learning_update <- function(Q, s, a, r, s_next, alpha, gamma,
                                          done = FALSE) {
  Qm <- .morie_gr_a2d(Q)
  .morie_gr_need(length(Qm) > 0L, "Q must be a non-empty table.")
  .morie_gr_fin(Qm, "Q")
  S <- nrow(Qm)
  A <- ncol(Qm)
  s <- as.integer(s)
  a <- as.integer(a)
  s_next <- as.integer(s_next)
  .morie_gr_need(
    s >= 0L && s < S && s_next >= 0L && s_next < S,
    "states out of range."
  )
  .morie_gr_need(a >= 0L && a < A, "action out of range.")
  r <- as.numeric(r)
  .morie_gr_need(is.finite(r), "reward must be finite.")
  alpha <- as.numeric(alpha)
  .morie_gr_need(alpha > 0 && alpha <= 1, "alpha must lie in (0, 1].")
  gamma <- as.numeric(gamma)
  .morie_gr_need(gamma >= 0 && gamma <= 1, "gamma must lie in [0, 1].")
  old <- Qm[s + 1L, a + 1L]
  best_next <- max(Qm[s_next + 1L, ])
  target <- if (isTRUE(done)) r else r + gamma * best_next
  td <- target - old
  Qm[s + 1L, a + 1L] <- old + alpha * td
  list(
    Q = Qm, old_value = old, new_value = Qm[s + 1L, a + 1L],
    target = target, td_error = td, max_next = best_next,
    estimate = Qm[s + 1L, a + 1L], n = S
  )
}

# ------------------------------------------------------------- grptq

#' Static post-training quantization (Geron Ch 19, morie.fn grptq)
#'
#' Runs a calibration batch through the layers and derives one
#' activation scale per tensor from the requested percentile of |a|.
#' numpy's default (linear) percentile interpolation is matched by
#' stats::quantile(type = 7).
#'
#' @param model List of functions applied in order.
#' @param calibration_data Representative input batch.
#' @param bits Quantization width.
#' @param percentile Calibration percentile in (0, 100\].
#' @return List with `scales`, `activation_ranges`, `quantized_output`,
#'   `dequantized_output`, `output`, `max_abs_error`.
#' @export
morie_geron_static_ptq <- function(model, calibration_data, bits = 8,
                                   percentile = 100) {
  layers <- model
  .morie_gr_need(length(layers) > 0L, "model has no layers.")
  for (f in layers) .morie_gr_need(is.function(f), "every model layer must be a function.")
  A <- calibration_data
  .morie_gr_need(length(A) > 0L, "calibration_data is empty.")
  .morie_gr_fin(A, "calibration_data")
  percentile <- as.numeric(percentile)
  .morie_gr_need(
    percentile > 0 && percentile <= 100,
    "percentile must lie in (0, 100]."
  )
  rng <- function(arr) {
    as.numeric(stats::quantile(abs(as.numeric(arr)),
      percentile / 100,
      names = FALSE, type = 7L
    ))
  }
  acts <- A
  ranges <- rng(acts)
  outputs <- list(acts)
  nrows <- function(z) if (is.matrix(z)) nrow(z) else length(z)
  for (i in seq_along(layers)) {
    nxt <- layers[[i]](acts)
    .morie_gr_fin(nxt, "layer activations")
    .morie_gr_need(nrows(nxt) == nrows(acts), "a layer changed the batch size.")
    acts <- nxt
    ranges <- c(ranges, rng(acts))
    outputs[[length(outputs) + 1L]] <- acts
  }
  qmax <- 2^(as.integer(bits) - 1L) - 1
  .morie_gr_need(all(ranges != 0), "an activation tensor is identically zero.")
  scales <- ranges / qmax
  last <- outputs[[length(outputs)]]
  qs <- morie_geron_quantize_symmetric(last, bits)
  list(
    scales = scales, activation_ranges = ranges, quantized_output = qs$q,
    dequantized_output = qs$dequantized, output = last,
    max_abs_error = max(abs(qs$dequantized - last)),
    percentile = percentile, estimate = scales, n = nrows(A)
  )
}

# -------------------------------------------------------------- grpvt

#' Pyramid ViT spatial-reduction attention (morie.fn grpvt)
#'
#' Queries at full resolution, keys and values from an R x R
#' average-pooled map. The (H, W, d) array is flattened ROW-MAJOR into
#' H*W tokens, which in R means indexing \[i, j, \] in that order.
#'
#' @param X (H, W, d_model) array.
#' @param WQ,WK,WV Projections with d_model rows.
#' @param reduction_ratio R dividing both H and W.
#' @return List with `output`, `weights`, `reduced_tokens`,
#'   `compression`, `reduced_map`.
#' @export
morie_geron_pyramid_vit_stage <- function(X, WQ, WK, WV, reduction_ratio = 2) {
  A <- X
  .morie_gr_need(
    length(dim(A)) == 3L && length(A) > 0L,
    "X must be a non-empty (H, W, d_model) array."
  )
  .morie_gr_fin(A, "X")
  H <- dim(A)[1L]
  W <- dim(A)[2L]
  d <- dim(A)[3L]
  R <- as.integer(reduction_ratio)
  .morie_gr_need(R >= 1L, "reduction_ratio must be at least 1.")
  .morie_gr_need(
    H %% R == 0L && W %% R == 0L,
    "reduction_ratio must divide the feature map."
  )
  mats <- lapply(list(WQ = WQ, WK = WK, WV = WV), .morie_gr_a2d)
  for (nm in names(mats)) {
    .morie_gr_need(
      nrow(mats[[nm]]) == d,
      paste0(nm, " must have d_model rows.")
    )
  }
  .morie_gr_need(ncol(mats$WQ) == ncol(mats$WK), "WQ and WK must share d_k.")
  tokens <- matrix(0, H * W, d)
  for (i in seq_len(H)) {
    for (j in seq_len(W)) {
      tokens[(i - 1L) * W + j, ] <- A[i, j, ]
    }
  }
  hh <- H %/% R
  ww <- W %/% R
  SR <- matrix(0, hh * ww, d)
  for (bi in seq_len(hh)) {
    for (bj in seq_len(ww)) {
      blk <- A[((bi - 1L) * R + 1L):(bi * R), ((bj - 1L) * R + 1L):(bj * R), ,
        drop = FALSE
      ]
      SR[(bi - 1L) * ww + bj, ] <- apply(blk, 3L, mean)
    }
  }
  r <- .morie_gr_attend(tokens %*% mats$WQ, SR %*% mats$WK, SR %*% mats$WV)
  list(
    output = r$output, weights = r$weights, reduced_tokens = nrow(SR),
    compression = H * W / nrow(SR), reduced_map = SR,
    estimate = r$output, n = H * W
  )
}

# ------------------------------------------------------ grvpi / grqpi

#' .morie_gr_check_mdp
#'
#' A step of the geron_train_native implementation. Called by \code{.morie_gr_policy_evaluation}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param policy See Usage.
#' @param transitions See Usage.
#' @param rewards See Usage.
#' @param gamma See Usage.
#' @return A list with \code{pi}, \code{P}, \code{R}, \code{gamma}, \code{S}, \code{A}.
#' @export
.morie_gr_check_mdp <- function(policy, transitions, rewards, gamma) {
  P <- transitions
  .morie_gr_need(length(dim(P)) == 3L, "transitions must be (S, A, S').")
  S <- dim(P)[1L]
  A <- dim(P)[2L]
  .morie_gr_need(dim(P)[3L] == S, "transitions last axis must equal the states.")
  R <- rewards
  if (length(dim(R)) != 3L) {
    Rm <- .morie_gr_a2d(R)
    .morie_gr_need(all(dim(Rm) == c(S, A)), "rewards must be (S, A, S') or (S, A).")
    R <- array(rep(as.numeric(Rm), S), dim = c(S, A, S))
  }
  pi <- policy
  if (!is.matrix(pi) && length(pi) == S) {
    det <- as.integer(pi)
    .morie_gr_need(all(det >= 0L & det < A), "deterministic policy actions out of range.")
    pim <- matrix(0, S, A)
    pim[cbind(seq_len(S), det + 1L)] <- 1
    pi <- pim
  } else {
    pi <- .morie_gr_a2d(pi)
    .morie_gr_need(all(dim(pi) == c(S, A)), "policy must be (S,) or (S, A).")
  }
  .morie_gr_need(
    all(pi >= 0) && all(abs(rowSums(pi) - 1) < 1e-08),
    "policy rows must be non-negative and sum to 1."
  )
  .morie_gr_need(
    all(P >= 0) && all(abs(apply(P, c(1L, 2L), sum) - 1) < 1e-08),
    "transition rows must be non-negative and sum to 1."
  )
  .morie_gr_fin(R, "rewards")
  gamma <- as.numeric(gamma)
  .morie_gr_need(gamma >= 0 && gamma < 1, "gamma must lie in [0, 1).")
  list(pi = pi, P = P, R = R, gamma = gamma, S = S, A = A)
}

#' .morie_gr_policy_evaluation
#'
#' A step of the geron_train_native implementation. Called by \code{morie_geron_action_value_function}, \code{morie_geron_state_value_function}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param policy Passed to \code{.morie_gr_check_mdp}.
#' @param transitions Passed to \code{.morie_gr_check_mdp}.
#' @param rewards Passed to \code{.morie_gr_check_mdp}.
#' @param gamma Passed to \code{.morie_gr_check_mdp}.
#' @return A list with \code{V}, \code{pi}, \code{P}, \code{R}, \code{r_sa}, \code{gamma}, \code{S}, \code{A}.
#' @export
.morie_gr_policy_evaluation <- function(policy, transitions, rewards, gamma) {
  m <- .morie_gr_check_mdp(policy, transitions, rewards, gamma)
  S <- m$S
  A <- m$A
  r_sa <- apply(m$P * m$R, c(1L, 2L), sum)
  r_pi <- rowSums(m$pi * r_sa)
  P_pi <- matrix(0, S, S)
  for (s in seq_len(S)) {
    for (a in seq_len(A)) {
      P_pi[s, ] <- P_pi[s, ] + m$pi[s, a] * m$P[s, a, ]
    }
  }
  V <- as.numeric(solve(diag(S) - m$gamma * P_pi, r_pi))
  list(
    V = V, pi = m$pi, P = m$P, R = m$R, r_sa = r_sa, gamma = m$gamma,
    S = S, A = A
  )
}

#' State value function V^pi (Geron Ch 18, morie.fn grvpi)
#'
#' Solves (I - gamma P_pi) V = r_pi exactly; no iteration, no
#' convergence error. `state` is 0-based.
#'
#' @param state 0-based state index.
#' @param policy Deterministic action vector or (S, A) stochastic matrix.
#' @param transitions (S, A, S') array.
#' @param rewards (S, A, S') or (S, A) array.
#' @param gamma Discount in \[0, 1).
#' @return List with `value`, `values`, `state`.
#' @export
morie_geron_state_value_function <- function(state, policy, transitions,
                                             rewards, gamma) {
  pe <- .morie_gr_policy_evaluation(policy, transitions, rewards, gamma)
  s <- as.integer(state)
  .morie_gr_need(s >= 0L && s < length(pe$V), "state out of range.")
  list(
    value = pe$V[s + 1L], values = pe$V, state = s,
    estimate = pe$V[s + 1L], n = length(pe$V)
  )
}

#' Action-value function Q^pi (Geron Ch 18, morie.fn grqpi)
#'
#' Q = r_sa + gamma sum_s' P V^pi(s'); V from
#' [morie_geron_state_value_function()]'s evaluation. Indices 0-based.
#'
#' @param state,action 0-based indices.
#' @param policy,transitions,rewards,gamma MDP specification.
#' @return List with `q_value`, `q_values`, `values`, `advantage`,
#'   `greedy_action`.
#' @export
morie_geron_action_value_function <- function(state, action, policy,
                                              transitions, rewards, gamma) {
  pe <- .morie_gr_policy_evaluation(policy, transitions, rewards, gamma)
  S <- pe$S
  A <- pe$A
  s <- as.integer(state)
  a <- as.integer(action)
  .morie_gr_need(s >= 0L && s < S, "state out of range.")
  .morie_gr_need(a >= 0L && a < A, "action out of range.")
  Q <- pe$r_sa
  for (ss in seq_len(S)) {
    for (aa in seq_len(A)) {
      Q[ss, aa] <- Q[ss, aa] + pe$gamma * sum(pe$P[ss, aa, ] * pe$V)
    }
  }
  list(
    q_value = Q[s + 1L, a + 1L], q_values = Q, values = pe$V,
    advantage = Q[s + 1L, a + 1L] - pe$V[s + 1L],
    greedy_action = which.max(Q[s + 1L, ]) - 1L,
    estimate = Q[s + 1L, a + 1L], n = S
  )
}

# -------------------------------------------------------------- grrad

#' Reverse-mode automatic differentiation (Geron Ch 12, morie.fn grrad)
#'
#' dL/dx = sum over children of (dL/dy)(dy/dx) in one reverse sweep;
#' fan-out ACCUMULATES. The graph is a named list node ->
#' named list parent -> local partial.
#'
#' @param graph Named list of named partial-derivative lists.
#' @param loss_grad Seed gradient at the output node.
#' @param output Optional output node name; inferred when unique.
#' @return List with `gradients`, `leaf_gradients`, `order`, `output`.
#' @export
morie_geron_reverse_mode_autodiff <- function(graph, loss_grad = 1,
                                              output = NULL) {
  .morie_gr_need(
    is.list(graph) && length(graph) > 0L,
    "graph must be a non-empty node -> parents mapping."
  )
  parents <- list()
  nodes <- character(0)
  for (node in names(graph)) {
    ps <- graph[[node]]
    .morie_gr_need(
      is.list(ps) || is.numeric(ps),
      "each graph entry must be a parent -> partial list."
    )
    vals <- vapply(ps, as.numeric, numeric(1L))
    .morie_gr_need(all(is.finite(vals)), "graph holds a non-finite partial.")
    parents[[node]] <- stats::setNames(as.list(vals), names(ps))
    nodes <- union(nodes, c(node, names(ps)))
  }
  for (n in nodes) if (is.null(parents[[n]])) parents[[n]] <- list()
  children_count <- stats::setNames(rep(0L, length(nodes)), nodes)
  for (node in names(parents)) {
    for (p in names(parents[[node]])) {
      children_count[p] <- children_count[p] + 1L
    }
  }
  sinks <- names(children_count)[children_count == 0L]
  if (is.null(output)) {
    .morie_gr_need(length(sinks) == 1L, "cannot infer the output node; pass output=.")
    output <- sinks[1L]
  } else {
    .morie_gr_need(output %in% nodes, "output node is not in the graph.")
  }
  order_v <- character(0)
  seen <- character(0)
  temp <- character(0)
  visit <- function(n) {
    if (n %in% seen) {
      return(invisible(NULL))
    }
    .morie_gr_need(!(n %in% temp), "graph has a cycle; this is not a DAG.")
    temp <<- c(temp, n)
    for (p in names(parents[[n]])) visit(p)
    temp <<- setdiff(temp, n)
    seen <<- c(seen, n)
    order_v <<- c(order_v, n)
  }
  visit(output)
  order_v <- rev(order_v)
  loss_grad <- as.numeric(loss_grad)
  .morie_gr_need(is.finite(loss_grad), "loss_grad must be finite.")
  grads <- stats::setNames(as.list(rep(0, length(order_v))), order_v)
  grads[[output]] <- loss_grad
  for (n in order_v) {
    for (p in names(parents[[n]])) {
      if (!is.null(grads[[p]])) {
        grads[[p]] <- grads[[p]] + grads[[n]] * parents[[n]][[p]]
      }
    }
  }
  leaves <- grads[vapply(
    names(grads),
    function(n) length(parents[[n]]) == 0L, logical(1L)
  )]
  list(
    gradients = grads, leaf_gradients = leaves, order = order_v,
    output = output, estimate = leaves, n = length(order_v)
  )
}

# ------------------------------------------------------------- grrein

#' REINFORCE Monte Carlo policy gradient (Geron Ch 18, morie.fn grrein)
#'
#' theta += alpha sum_t (G_t - b) grad log pi_t; `log_probs` must be
#' score VECTORS, one row per step.
#'
#' @param theta Parameters.
#' @param log_probs (T, p) score matrix.
#' @param returns_G Length-T returns.
#' @param alpha Positive step size.
#' @param baseline NULL, "mean", or a finite number.
#' @return List with `theta_new`, `gradient`, `advantages`, `baseline`,
#'   `step_norm`.
#' @export
morie_geron_reinforce_policy_gradient <- function(theta, log_probs, returns_G,
                                                  alpha, baseline = NULL) {
  th <- as.numeric(theta)
  S <- .morie_gr_a2d(log_probs)
  G <- as.numeric(returns_G)
  .morie_gr_need(length(th) > 0L, "theta is empty.")
  .morie_gr_need(ncol(S) == length(th), "log_probs rows must be score vectors.")
  .morie_gr_need(nrow(S) == length(G), "log_probs steps must equal returns_G.")
  .morie_gr_fin(S, "log_probs")
  .morie_gr_fin(G, "returns_G")
  .morie_gr_fin(th, "theta")
  alpha <- as.numeric(alpha)
  .morie_gr_need(is.finite(alpha) && alpha > 0, "alpha must be a positive finite float.")
  b <- if (is.null(baseline)) {
    0
  } else if (is.character(baseline)) {
    .morie_gr_need(baseline == "mean", "baseline must be NULL, 'mean' or a number.")
    mean(G)
  } else {
    bb <- as.numeric(baseline)
    .morie_gr_need(is.finite(bb), "baseline must be finite.")
    bb
  }
  adv <- G - b
  grad <- as.numeric(t(S) %*% adv)
  new <- th + alpha * grad
  list(
    theta_new = new, gradient = grad, advantages = adv, baseline = b,
    step_norm = sqrt(sum((alpha * grad)^2)), estimate = new, n = length(G)
  )
}

# -------------------------------------------------------------- grrep

#' Reparameterization trick (Geron Ch 17, morie.fn grrep)
#'
#' z = mu + exp(0.5 logvar) eps; the 0.5 is load-bearing. The LCG normal
#' stream here CLAMPS u1 at 1e-12 before the log, unlike the grhei /
#' grgrp / grdpmf streams, and always fills whole Box-Muller pairs.
#'
#' @param mu,logvar Same-shaped numeric objects.
#' @param eps Optional noise; otherwise LCG normals.
#' @param seed LCG seed.
#' @return List with `z`, `sigma`, `eps`, `dz_dmu`, `dz_dlogvar`,
#'   `sample_mean`, `sample_variance`.
#' @export
morie_geron_reparameterization_trick <- function(mu, logvar, eps = NULL,
                                                 seed = 42) {
  M <- mu
  LV <- logvar
  .morie_gr_need(length(M) > 0L, "mu is empty.")
  .morie_gr_need(length(M) == length(LV), "mu and logvar must have the same shape.")
  .morie_gr_fin(M, "mu")
  .morie_gr_fin(LV, "logvar")
  sigma <- exp(0.5 * LV)
  if (is.null(eps)) {
    E <- .morie_gr_lcg_normals(length(M), seed, clamp = TRUE)
    if (is.matrix(M)) E <- matrix(E, nrow = nrow(M), byrow = TRUE)
  } else {
    E <- eps
    .morie_gr_need(length(E) == length(M), "eps shape != mu shape.")
    .morie_gr_fin(E, "eps")
  }
  z <- M + sigma * E
  ones <- if (is.matrix(M)) matrix(1, nrow(M), ncol(M)) else rep(1, length(M))
  list(
    z = z, sigma = sigma, eps = E, dz_dmu = ones,
    dz_dlogvar = 0.5 * sigma * E, sample_mean = mean(E),
    sample_variance = .morie_gr_pvar(as.numeric(E)),
    estimate = z, n = length(M)
  )
}

# -------------------------------------------------------------- grret

#' Discounted return (Geron Ch 18, morie.fn grret)
#'
#' G_t = r_t + gamma `G_{t+1}` by a backward sweep; effective horizon
#' 1/(1-gamma).
#'
#' @param rewards Reward sequence.
#' @param gamma Discount in \[0, 1\].
#' @return List with `returns`, `G0`, `effective_horizon`.
#' @export
morie_geron_discounted_return <- function(rewards, gamma) {
  r <- as.numeric(rewards)
  .morie_gr_need(length(r) > 0L, "rewards is empty.")
  .morie_gr_fin(r, "rewards")
  gamma <- as.numeric(gamma)
  .morie_gr_need(gamma >= 0 && gamma <= 1, "gamma must lie in [0, 1].")
  G <- numeric(length(r))
  acc <- 0
  for (t in rev(seq_along(r))) {
    acc <- r[t] + gamma * acc
    G[t] <- acc
  }
  list(
    returns = G, G0 = G[1L], gamma = gamma,
    effective_horizon = if (gamma == 1) Inf else 1 / (1 - gamma),
    estimate = G[1L], n = length(G)
  )
}

# ------------------------------------------------------------- grridg

#' Ridge cost (Geron Ch 4, morie.fn grridg)
#'
#' J = MSE + (alpha/2) `sum_{i>=1}` theta_i^2 with the bias unpenalised;
#' the gradient is returned alongside.
#'
#' @param X,y,theta Design, targets and parameters.
#' @param alpha Non-negative penalty.
#' @param intercept Treat theta\[1\] as an unpenalised bias.
#' @return List with `cost`, `mse`, `penalty`, `gradient`.
#' @export
morie_geron_ridge_cost_grridg <- function(X, y, theta, alpha, intercept = TRUE) {
  A <- .morie_gr_a2d(X)
  yv <- as.numeric(y)
  th <- as.numeric(theta)
  .morie_gr_need(length(A) > 0L, "X must be a non-empty (m, n) matrix.")
  .morie_gr_need(nrow(A) == length(yv), "X rows must equal length(y).")
  .morie_gr_need(ncol(A) == length(th), "X features must equal length(theta).")
  .morie_gr_fin(A, "X")
  .morie_gr_fin(yv, "y")
  .morie_gr_fin(th, "theta")
  alpha <- as.numeric(alpha)
  .morie_gr_need(is.finite(alpha) && alpha >= 0, "alpha must be finite and non-negative.")
  res <- as.numeric(A %*% th) - yv
  mse <- mean(res^2)
  w <- th
  if (isTRUE(intercept)) w[1L] <- 0
  pen <- 0.5 * alpha * sum(w^2)
  grad <- 2 / nrow(A) * as.numeric(t(A) %*% res) + alpha * w
  list(
    cost = mse + pen, mse = mse, penalty = pen, gradient = grad,
    estimate = mse + pen, n = nrow(A)
  )
}

# ------------------------------------------------------------- grridn

#' Ridge closed form (Geron Ch 4, morie.fn grridn)
#'
#' theta = (X^T X + alpha diag(0, 1, ..., 1))^-1 X^T y; the bias column
#' is never penalised.
#'
#' @param X,y Design and targets.
#' @param alpha Non-negative penalty.
#' @param intercept Zero the first diagonal entry.
#' @return List with `theta`, `fitted`, `residuals`, `rss`, `penalty`.
#' @export
morie_geron_ridge_normal_equation <- function(X, y, alpha, intercept = TRUE) {
  A <- .morie_gr_a2d(X)
  yv <- as.numeric(y)
  .morie_gr_need(length(A) > 0L, "X must be a non-empty (m, n) matrix.")
  .morie_gr_need(nrow(A) == length(yv), "X rows must equal length(y).")
  .morie_gr_fin(A, "X")
  .morie_gr_fin(yv, "y")
  alpha <- as.numeric(alpha)
  .morie_gr_need(is.finite(alpha) && alpha >= 0, "alpha must be finite and non-negative.")
  n <- ncol(A)
  Amat <- diag(n)
  if (isTRUE(intercept)) {
    .morie_gr_need(n >= 2L, "intercept=TRUE needs at least one feature besides the bias.")
    Amat[1L, 1L] <- 0
  }
  G <- t(A) %*% A + alpha * Amat
  sv <- svd(G, nu = 0L, nv = 0L)$d
  .morie_gr_need(
    sum(sv > max(dim(G)) * .Machine$double.eps * max(sv)) >= n,
    "X^T X + alpha*A is singular."
  )
  theta <- as.numeric(solve(G, t(A) %*% yv))
  fitted <- as.numeric(A %*% theta)
  res <- yv - fitted
  list(
    theta = theta, fitted = fitted, residuals = res, rss = sum(res^2),
    penalty = alpha * sum(theta * as.numeric(Amat %*% theta)),
    estimate = theta, n = nrow(A)
  )
}

# ------------------------------------------------------------- grrlhf

#' RLHF reward minus KL objective (Geron Ch 15, morie.fn grrlhf)
#'
#' J = mean r - beta mean(log pi - log pi_ref).
#'
#' @param rewards Per-sample rewards.
#' @param policy_logprobs,ref_logprobs Non-positive log-probabilities.
#' @param beta Non-negative KL weight.
#' @return List with `objective`, `mean_reward`, `kl`, `kl_terms`,
#'   `per_sample`.
#' @export
morie_geron_rlhf_reward_kl_objective <- function(rewards, policy_logprobs,
                                                 ref_logprobs, beta = 0.1) {
  r <- as.numeric(rewards)
  lp <- as.numeric(policy_logprobs)
  lr <- as.numeric(ref_logprobs)
  .morie_gr_need(length(r) > 0L, "rewards is empty.")
  .morie_gr_need(
    length(r) == length(lp) && length(r) == length(lr),
    "all three vectors must have equal length."
  )
  .morie_gr_fin(r, "rewards")
  .morie_gr_fin(lp, "policy_logprobs")
  .morie_gr_fin(lr, "ref_logprobs")
  .morie_gr_need(
    all(lp <= 0) && all(lr <= 0),
    "log-probabilities must be non-positive."
  )
  beta <- as.numeric(beta)
  .morie_gr_need(is.finite(beta) && beta >= 0, "beta must be finite and non-negative.")
  kl_terms <- lp - lr
  kl <- mean(kl_terms)
  list(
    objective = mean(r) - beta * kl, mean_reward = mean(r), kl = kl,
    kl_terms = kl_terms, per_sample = r - beta * kl_terms, beta = beta,
    estimate = mean(r) - beta * kl, n = length(r)
  )
}

# ------------------------------------------------------------- grrmse

#' Root mean squared error (Geron Ch 2, morie.fn grrmse)
#'
#' @param y_true,y_pred Equal-length numeric vectors.
#' @return List with `rmse`, `mse`, `mae`, `max_error`, `residuals`.
#' @export
morie_geron_rmse_grrmse <- function(y_true, y_pred) {
  yt <- as.numeric(y_true)
  yp <- as.numeric(y_pred)
  .morie_gr_need(length(yt) > 0L, "y_true is empty.")
  .morie_gr_need(length(yt) == length(yp), "y_true and y_pred must match.")
  .morie_gr_fin(yt, "y_true")
  .morie_gr_fin(yp, "y_pred")
  res <- yp - yt
  mse <- mean(res^2)
  list(
    rmse = sqrt(mse), mse = mse, mae = mean(abs(res)),
    max_error = max(abs(res)), residuals = res, estimate = sqrt(mse),
    n = length(yt)
  )
}

# -------------------------------------------------------------- grrnd

#' Randomized search with K-fold CV (Geron Ch 2, morie.fn grrnd)
#'
#' The LCG stream is consumed first for the fold permutation (m draws,
#' argsorted stably) and then one draw PER PARAMETER per configuration,
#' in the order of `param_dist`. `std_score` is the ddof = 0 deviation.
#'
#' @param X,y Data.
#' @param param_dist Named list of specs: a function(u), a length-2
#'   numeric range, or a vector of discrete choices.
#' @param n_iter Configurations to draw.
#' @param K Folds in \[2, m\].
#' @param fit_score Required scoring function.
#' @param seed LCG seed.
#' @return List with `best_params`, `best_score`, `results`,
#'   `fold_sizes`.
#' @export
morie_geron_randomized_search_cv <- function(X, y, param_dist, n_iter, K,
                                             fit_score = NULL, seed = 42) {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1L)
  storage.mode(A) <- "double"
  yv <- as.numeric(y)
  .morie_gr_need(length(A) > 0L, "X is empty.")
  .morie_gr_need(nrow(A) == length(yv), "X rows must equal length(y).")
  .morie_gr_need(is.function(fit_score), "fit_score is required and must be a function.")
  n_iter <- as.integer(n_iter)
  K <- as.integer(K)
  .morie_gr_need(n_iter >= 1L, "n_iter must be at least 1.")
  .morie_gr_need(K >= 2L && K <= nrow(A), "K must lie in [2, n_samples].")
  .morie_gr_need(
    is.list(param_dist) && length(param_dist) > 0L,
    "param_dist must be a non-empty named list."
  )
  n_draws <- nrow(A) + n_iter * length(param_dist)
  stream <- .morie_gr_lcg_u(n_draws, seed)
  pos <- 0L
  take <- function() {
    pos <<- pos + 1L
    stream[pos]
  }
  perm <- order(vapply(seq_len(nrow(A)), function(i) take(), numeric(1L)),
    method = "radix"
  ) - 1L
  folds <- .morie_gr_array_split(perm, K)
  .morie_gr_need(
    all(vapply(folds, length, integer(1L)) > 0L),
    "K produces an empty fold."
  )
  sample_one <- function(spec) {
    u <- take()
    if (is.function(spec)) {
      return(spec(u))
    }
    if (is.numeric(spec) && length(spec) == 2L && !is.list(spec)) {
      lo <- as.numeric(spec[1L])
      hi <- as.numeric(spec[2L])
      .morie_gr_need(hi > lo, "range must have high > low.")
      return(lo + u * (hi - lo))
    }
    seq_v <- spec
    .morie_gr_need(length(seq_v) > 0L, "a discrete parameter list is empty.")
    seq_v[[min(as.integer(u * length(seq_v)), length(seq_v) - 1L) + 1L]]
  }
  results <- list()
  for (it in seq_len(n_iter)) {
    params <- stats::setNames(lapply(param_dist, sample_one), names(param_dist))
    scores <- numeric(K)
    for (f in seq_len(K)) {
      va <- folds[[f]] + 1L
      tr <- unlist(folds[-f], use.names = FALSE) + 1L
      s <- as.numeric(fit_score(
        A[tr, , drop = FALSE], yv[tr],
        A[va, , drop = FALSE], yv[va], params
      ))
      .morie_gr_need(is.finite(s), "fit_score returned a non-finite score.")
      scores[f] <- s
    }
    results[[it]] <- list(
      params = params, mean_score = mean(scores),
      std_score = .morie_gr_psd(scores),
      fold_scores = scores
    )
  }
  best <- results[[which.max(vapply(results, function(r) r$mean_score, numeric(1L)))]]
  list(
    best_params = best$params, best_score = best$mean_score,
    results = results,
    fold_sizes = vapply(folds, length, integer(1L)),
    estimate = best$mean_score, n = nrow(A)
  )
}

# ------------------------------------------------------------- grrnnc

#' Simple (Elman) RNN cell (Geron Ch 15, morie.fn grrnnc)
#'
#' h_t = tanh(Whh `h_{t-1}` + Wxh x_t + b); the spectral norm of Whh
#' decides vanishing versus exploding gradients.
#'
#' @param x_t,h_prev State vectors.
#' @param Whh (H, H) recurrent weights.
#' @param Wxh (H, n) input weights.
#' @param b Length-H bias.
#' @return List with `h`, `pre_activation`, `derivative`,
#'   `spectral_norm_Whh`, `saturated`.
#' @export
morie_geron_simple_rnn_cell <- function(x_t, h_prev, Whh, Wxh, b) {
  x <- as.numeric(x_t)
  h <- as.numeric(h_prev)
  A <- .morie_gr_a2d(Whh)
  B <- .morie_gr_a2d(Wxh)
  bv <- as.numeric(b)
  n_h <- length(h)
  .morie_gr_need(n_h > 0L && length(x) > 0L, "x_t and h_prev must be non-empty.")
  .morie_gr_need(all(dim(A) == c(n_h, n_h)), "Whh must be (H, H).")
  .morie_gr_need(all(dim(B) == c(n_h, length(x))), "Wxh must be (H, n).")
  .morie_gr_need(length(bv) == n_h, "b must have H entries.")
  .morie_gr_fin(c(x, h, as.numeric(A), as.numeric(B), bv), "inputs")
  z <- as.numeric(A %*% h) + as.numeric(B %*% x) + bv
  h_new <- tanh(z)
  sv <- max(svd(A, nu = 0L, nv = 0L)$d)
  list(
    h = h_new, pre_activation = z, derivative = 1 - h_new^2,
    spectral_norm_Whh = sv, saturated = mean(abs(h_new) > 0.99),
    estimate = h_new, n = n_h
  )
}

# -------------------------------------------------------------- grrsk

#' Residual (skip) connection (Geron Ch 14, morie.fn grrsk)
#'
#' y = F(x) + x; a shape-changing block needs `projection`.
#'
#' @param x Block input.
#' @param Fx Block output.
#' @param projection Optional (d_in, d_out) shortcut projection.
#' @return List with `output`, `shortcut`, `residual_norm`,
#'   `shortcut_norm`, `residual_fraction`.
#' @export
morie_geron_resnet_skip <- function(x, Fx, projection = NULL) {
  xa <- x
  fa <- Fx
  .morie_gr_need(length(xa) > 0L, "x is empty.")
  .morie_gr_fin(xa, "x")
  .morie_gr_fin(fa, "F(x)")
  short <- xa
  same <- identical(dim(fa), dim(xa)) && length(fa) == length(xa)
  if (!same) {
    .morie_gr_need(!is.null(projection), "supply projection= for a shape-changing block.")
    P <- .morie_gr_a2d(projection)
    xm <- if (is.matrix(xa)) xa else matrix(as.numeric(xa), nrow = 1L)
    .morie_gr_need(nrow(P) == ncol(xm), "projection must be (d_in, d_out).")
    short <- xm %*% P
    if (!is.matrix(xa)) short <- as.numeric(short)
    .morie_gr_need(
      length(short) == length(fa),
      "projected shortcut does not match F(x)."
    )
  }
  out <- fa + short
  fn <- sqrt(sum(fa^2))
  sn <- sqrt(sum(short^2))
  list(
    output = out, shortcut = short, residual_norm = fn,
    shortcut_norm = sn,
    residual_fraction = if (fn + sn > 0) fn / (fn + sn) else 0,
    estimate = out, n = length(out)
  )
}

# --------------------------------------------------------------- grsa

#' Self-attention, single head (Geron Ch 16, morie.fn grsa)
#'
#' SA(X) = softmax(XWq (XWk)^T / sqrt(d_k)) XWv through the shared
#' attention kernel.
#'
#' @param X (T, d_model) tokens.
#' @param WQ,WK,WV Projections with d_model rows.
#' @param mask Optional attention mask.
#' @return List with `output`, `weights`, `Q`, `K`, `V`.
#' @export
morie_geron_self_attention <- function(X, WQ, WK, WV, mask = NULL) {
  X <- .morie_gr_a2d(X)
  .morie_gr_need(length(X) > 0L, "X must be a non-empty (T, d_model) matrix.")
  mats <- lapply(list(WQ = WQ, WK = WK, WV = WV), .morie_gr_a2d)
  for (nm in names(mats)) {
    .morie_gr_need(
      nrow(mats[[nm]]) == ncol(X),
      paste0(nm, " must have d_model rows.")
    )
  }
  .morie_gr_need(ncol(mats$WQ) == ncol(mats$WK), "WQ and WK must share d_k.")
  Q <- X %*% mats$WQ
  K <- X %*% mats$WK
  V <- X %*% mats$WV
  r <- .morie_gr_attend(Q, K, V, mask)
  list(
    output = r$output, weights = r$weights, Q = Q, K = K, V = V,
    estimate = r$output, n = nrow(X)
  )
}

# -------------------------------------------------------------- grsae

#' Sparse autoencoder objective (Geron Ch 17, morie.fn grsae)
#'
#' L = ||x - dec||^2 + lam ||h||_1; the L1 zeroes units outright.
#'
#' @param x Input batch.
#' @param hidden Code activations.
#' @param decoded Reconstruction.
#' @param lam Non-negative L1 weight.
#' @return List with `loss`, `reconstruction_loss`, `l1_penalty`,
#'   `sparsity`, `mean_activation`, `code_size`.
#' @export
morie_geron_sparse_autoencoder <- function(x, hidden, decoded, lam = 0.001) {
  X <- .morie_gr_a2d(x)
  H <- .morie_gr_a2d(hidden)
  D <- .morie_gr_a2d(decoded)
  .morie_gr_need(length(X) > 0L, "x is empty.")
  .morie_gr_need(all(dim(D) == dim(X)), "decoded must match x.")
  .morie_gr_need(nrow(H) == nrow(X), "hidden rows must equal x rows.")
  .morie_gr_fin(X, "x")
  .morie_gr_fin(H, "hidden")
  .morie_gr_fin(D, "decoded")
  lam <- as.numeric(lam)
  .morie_gr_need(is.finite(lam) && lam >= 0, "lam must be finite and non-negative.")
  recon <- sum((X - D)^2)
  l1 <- lam * sum(abs(H))
  list(
    loss = recon + l1, reconstruction_loss = recon, l1_penalty = l1,
    sparsity = mean(abs(H) < 1e-08), mean_activation = mean(abs(H)),
    code_size = ncol(H), estimate = recon + l1, n = nrow(X)
  )
}

# -------------------------------------------------------------- grscm

#' Denoising score matching loss (Geron Ch 18, morie.fn grscm)
#'
#' L = mean w ||eps/sigma - s_theta(x0 + sigma eps)||^2.
#'
#' @param x0 Clean data (m, d).
#' @param sigma Scalar or length-m noise levels.
#' @param eps Noise of the same shape as x0.
#' @param score_pred Matrix or function(x_noisy, sigma).
#' @param weight NULL, "sigma2", or a length-m vector.
#' @return List with `loss`, `per_sample`, `target`, `x_noisy`,
#'   `residual`.
#' @export
morie_geron_score_matching_loss <- function(x0, sigma, eps, score_pred,
                                            weight = NULL) {
  X <- .morie_gr_a2d(x0)
  E <- .morie_gr_a2d(eps)
  .morie_gr_need(length(X) > 0L, "x0 is empty.")
  .morie_gr_need(all(dim(E) == dim(X)), "eps must match x0.")
  .morie_gr_fin(X, "x0")
  .morie_gr_fin(E, "eps")
  s <- as.numeric(sigma)
  if (length(s) == 1L) s <- rep(s, nrow(X))
  .morie_gr_need(length(s) == nrow(X), "sigma length must equal the rows of x0.")
  .morie_gr_need(all(s > 0) && all(is.finite(s)), "sigma must be strictly positive.")
  x_noisy <- X + s * E
  target <- E / s
  S <- if (is.function(score_pred)) {
    .morie_gr_a2d(score_pred(x_noisy, s))
  } else {
    .morie_gr_a2d(score_pred)
  }
  .morie_gr_need(all(dim(S) == dim(X)), "score_pred must match x0's shape.")
  .morie_gr_fin(S, "score_pred")
  resid <- target - S
  per <- rowSums(resid^2)
  w <- if (is.null(weight)) {
    rep(1, nrow(X))
  } else if (is.character(weight)) {
    .morie_gr_need(weight == "sigma2", "weight must be NULL, 'sigma2' or a vector.")
    s^2
  } else {
    ww <- as.numeric(weight)
    .morie_gr_need(length(ww) == nrow(X), "weight length must equal the rows of x0.")
    .morie_gr_need(all(ww >= 0), "weight must be non-negative.")
    ww
  }
  per_w <- w * per
  list(
    loss = mean(per_w), per_sample = per_w, target = target,
    x_noisy = x_noisy, residual = resid, estimate = mean(per_w),
    n = nrow(X)
  )
}

# -------------------------------------------------------------- grsen

#' Squeeze-and-Excitation channel attention (Geron Ch 14, morie.fn grsen)
#'
#' z = GAP(X); s = sigmoid(W2 relu(W1 z)); Y = s * X per channel --
#' sigmoid, not softmax, so channels are gated independently.
#'
#' @param X (H, W, C) feature map.
#' @param W1 (bottleneck, C) squeeze weights.
#' @param W2 (C, bottleneck) excite weights.
#' @return List with `output`, `scale`, `squeeze`, `hidden`,
#'   `reduction_ratio`.
#' @export
morie_geron_senet_squeeze_excite <- function(X, W1, W2) {
  A <- X
  .morie_gr_need(
    length(dim(A)) == 3L && length(A) > 0L,
    "X must be a non-empty (H, W, C) array."
  )
  .morie_gr_fin(A, "X")
  C <- dim(A)[3L]
  A1 <- .morie_gr_a2d(W1)
  A2 <- .morie_gr_a2d(W2)
  .morie_gr_need(ncol(A1) == C, "W1 must have C columns.")
  .morie_gr_need(ncol(A2) == nrow(A1), "W2 must match the bottleneck.")
  .morie_gr_need(nrow(A2) == C, "W2 must produce C channel gates.")
  .morie_gr_fin(A1, "W1")
  .morie_gr_fin(A2, "W2")
  z <- apply(A, 3L, mean)
  hidden <- pmax(0, as.numeric(A1 %*% z))
  s <- .morie_gr_sigmoid_vec(as.numeric(A2 %*% hidden))
  Y <- A
  for (k in seq_len(C)) Y[, , k] <- A[, , k] * s[k]
  list(
    output = Y, scale = s, squeeze = z, hidden = hidden,
    reduction_ratio = C / nrow(A1), estimate = Y, n = C
  )
}

# -------------------------------------------------------------- grsft

#' SFT masked cross-entropy objective (Geron Ch 15, morie.fn grsft)
#'
#' -mean log p over the MASKED response tokens only; prompt tokens are
#' excluded from the average.
#'
#' @param logits (T, V) matrix.
#' @param response_mask Logical selector of response positions.
#' @param targets 0-based token ids.
#' @return List with `loss`, `per_token`, `perplexity`,
#'   `n_response_tokens`, `token_logprobs`.
#' @export
morie_geron_sft_objective <- function(logits, response_mask, targets) {
  Z <- .morie_gr_a2d(logits)
  .morie_gr_need(length(Z) > 0L, "logits must be a non-empty (T, V) matrix.")
  .morie_gr_fin(Z, "logits")
  Tn <- nrow(Z)
  V <- ncol(Z)
  tgt <- as.numeric(targets)
  .morie_gr_need(length(tgt) == Tn, "targets length must equal the positions.")
  .morie_gr_need(all(tgt == round(tgt)), "targets must be integer token ids.")
  tgt <- as.integer(tgt)
  .morie_gr_need(
    min(tgt) >= 0L && max(tgt) < V,
    paste0("target ids must lie in [0, ", V - 1L, "].")
  )
  mask <- as.vector(response_mask)
  .morie_gr_need(length(mask) == Tn, "response_mask length must equal the positions.")
  mask <- as.logical(mask)
  .morie_gr_need(any(mask), "response_mask selects no tokens.")
  logp <- .morie_gr_log_softmax_rows(Z)
  tok <- logp[cbind(seq_len(Tn), tgt + 1L)]
  per <- -tok[mask]
  list(
    loss = mean(per), per_token = per, perplexity = exp(mean(per)),
    n_response_tokens = sum(mask), token_logprobs = tok,
    estimate = mean(per), n = Tn
  )
}

# -------------------------------------------------------------- grsgd

#' Stochastic gradient descent (Geron Ch 4, morie.fn grsgd)
#'
#' One LCG-drawn sample per step, gradient 2 x_i (x_i^T theta - y_i);
#' passing both `t0` and `t1` anneals the rate as t0/(t + t1). The
#' sampled row indices are 0-based.
#'
#' @param X,y,theta Data and starting parameters.
#' @param eta Fixed rate (ignored when scheduled).
#' @param n_iter Number of steps.
#' @param seed LCG seed.
#' @param t0,t1 Optional schedule constants, supplied together.
#' @return List with `theta`, `path`, `cost_path`, `learning_rates`,
#'   `sample_order`.
#' @export
morie_geron_stochastic_gradient_descent <- function(X, y, theta, eta, n_iter,
                                                    seed = 42, t0 = NULL,
                                                    t1 = NULL) {
  A <- .morie_gr_a2d(X)
  yv <- as.numeric(y)
  th <- as.numeric(theta)
  .morie_gr_need(length(A) > 0L, "X must be a non-empty (m, n) matrix.")
  .morie_gr_need(nrow(A) == length(yv), "X rows must equal length(y).")
  .morie_gr_need(ncol(A) == length(th), "X features must equal length(theta).")
  .morie_gr_fin(A, "X")
  .morie_gr_fin(yv, "y")
  .morie_gr_fin(th, "theta")
  n_iter <- as.integer(n_iter)
  .morie_gr_need(n_iter >= 1L, "n_iter must be at least 1.")
  scheduled <- !is.null(t0) && !is.null(t1)
  .morie_gr_need(
    is.null(t0) == is.null(t1),
    "t0 and t1 must be supplied together or not at all."
  )
  if (scheduled) {
    t0 <- as.numeric(t0)
    t1 <- as.numeric(t1)
    .morie_gr_need(t0 > 0 && t1 > 0, "t0 and t1 must be positive.")
  } else {
    eta <- as.numeric(eta)
    .morie_gr_need(is.finite(eta) && eta > 0, "eta must be a positive finite float.")
  }
  m <- nrow(A)
  u <- .morie_gr_lcg_u(n_iter, seed)
  path <- list()
  costs <- mean((as.numeric(A %*% th) - yv)^2)
  rates <- numeric(0)
  order_v <- integer(0)
  for (t in seq_len(n_iter)) {
    i <- as.integer(u[t] * m)
    if (i == m) i <- m - 1L
    lr <- if (scheduled) t0 / ((t - 1L) + t1) else eta
    grad <- 2 * A[i + 1L, ] * (sum(A[i + 1L, ] * th) - yv[i + 1L])
    th <- th - lr * grad
    .morie_gr_need(all(is.finite(th)), "theta diverged; eta is too large.")
    path[[length(path) + 1L]] <- th
    costs <- c(costs, mean((as.numeric(A %*% th) - yv)^2))
    rates <- c(rates, lr)
    order_v <- c(order_v, i)
  }
  list(
    theta = th, path = path, cost_path = costs, learning_rates = rates,
    sample_order = order_v, estimate = th, n = m
  )
}

# -------------------------------------------------------------- grsil

#' Silhouette score (Geron Ch 9, morie.fn grsil)
#'
#' s_i = (b - a) / max(a, b) with `a` excluding the point itself;
#' singleton clusters score 0.
#'
#' @param X (m, n) data.
#' @param labels Cluster labels, at least 2 distinct and fewer than m.
#' @return List with `silhouette`, `per_sample`, `per_cluster`, `a`, `b`.
#' @export
morie_geron_silhouette_score <- function(X, labels) {
  A <- .morie_gr_a2d(X)
  lab <- as.vector(labels)
  .morie_gr_need(length(A) > 0L, "X must be a non-empty (m, n) matrix.")
  .morie_gr_need(length(lab) == nrow(A), "labels length must equal nrow(X).")
  .morie_gr_fin(A, "X")
  uniq <- sort(unique(lab))
  .morie_gr_need(length(uniq) >= 2L, "silhouette needs at least 2 clusters.")
  .morie_gr_need(length(uniq) < nrow(A), "too many clusters for the points.")
  D <- as.matrix(stats::dist(A))
  m <- nrow(A)
  a <- numeric(m)
  b <- numeric(m)
  s <- numeric(m)
  for (i in seq_len(m)) {
    own <- lab == lab[i]
    n_own <- sum(own)
    b[i] <- min(vapply(
      uniq[uniq != lab[i]],
      function(cl) mean(D[i, lab == cl]), numeric(1L)
    ))
    if (n_own <= 1L) {
      a[i] <- 0
      s[i] <- 0
      next
    }
    a[i] <- sum(D[i, own]) / (n_own - 1L)
    denom <- max(a[i], b[i])
    s[i] <- if (denom == 0) 0 else (b[i] - a[i]) / denom
  }
  per_cluster <- stats::setNames(
    vapply(uniq, function(cl) mean(s[lab == cl]), numeric(1L)),
    as.character(uniq)
  )
  list(
    silhouette = mean(s), per_sample = s, per_cluster = per_cluster,
    a = a, b = b, estimate = mean(s), n = m
  )
}

# -------------------------------------------------------------- grsmd

#' .morie_gr_sym_is_const
#'
#' A step of the geron_train_native implementation. Called by \code{.morie_gr_sym_diff}, \code{.morie_gr_sym_eval}, \code{.morie_gr_sym_simplify} and 2 others in the module.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param e A vector; its length is taken.
#' @return A logical value.
#' @export
.morie_gr_sym_is_const <- function(e) is.numeric(e) && length(e) == 1L
.morie_gr_sym_unary <- c("sin", "cos", "exp", "log", "neg")
.morie_gr_sym_binary <- c("+", "-", "*", "/", "^")

#' .morie_gr_sym_eval
#'
#' A step of the geron_train_native implementation. Called by \code{.morie_gr_sym_simplify}, \code{morie_geron_symbolic_differentiation}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param e A vector; its length is taken and its elements indexed.
#' @param env A vector; indexed elementwise.
#' @return The value of \code{switch}.
#' @export
.morie_gr_sym_eval <- function(e, env) {
  if (.morie_gr_sym_is_const(e)) {
    return(as.numeric(e))
  }
  if (is.character(e) && length(e) == 1L) {
    .morie_gr_need(
      !is.null(env[[e]]),
      paste0("variable ", e, " has no value in the environment.")
    )
    return(as.numeric(env[[e]]))
  }
  op <- e[[1L]]
  if (op %in% .morie_gr_sym_unary) {
    v <- .morie_gr_sym_eval(e[[2L]], env)
    if (op == "sin") {
      return(sin(v))
    }
    if (op == "cos") {
      return(cos(v))
    }
    if (op == "exp") {
      return(exp(v))
    }
    if (op == "neg") {
      return(-v)
    }
    .morie_gr_need(v > 0, "log is undefined at a non-positive value.")
    return(log(v))
  }
  a <- .morie_gr_sym_eval(e[[2L]], env)
  b <- .morie_gr_sym_eval(e[[3L]], env)
  switch(op,
    "+" = a + b,
    "-" = a - b,
    "*" = a * b,
    "/" = {
      .morie_gr_need(b != 0, "division by zero.")
      a / b
    },
    a^b
  )
}

#' .morie_gr_sym_simplify
#'
#' A step of the geron_train_native implementation. Called by \code{morie_geron_symbolic_differentiation}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param e A vector; its length is taken and its elements indexed.
#' @return A vector, from \code{c}.
#' @export
.morie_gr_sym_simplify <- function(e) {
  if (.morie_gr_sym_is_const(e) || (is.character(e) && length(e) == 1L)) {
    return(e)
  }
  op <- e[[1L]]
  args <- lapply(e[-1L], .morie_gr_sym_simplify)
  if (all(vapply(args, .morie_gr_sym_is_const, logical(1L)))) {
    folded <- tryCatch(.morie_gr_sym_eval(c(list(op), args), list()),
      error = function(err) NULL
    )
    if (!is.null(folded)) {
      return(folded)
    }
    return(c(list(op), args))
  }
  if (op == "+") {
    if (identical(args[[1L]], 0)) {
      return(args[[2L]])
    }
    if (identical(args[[2L]], 0)) {
      return(args[[1L]])
    }
  } else if (op == "-") {
    if (identical(args[[2L]], 0)) {
      return(args[[1L]])
    }
  } else if (op == "*") {
    if (identical(args[[1L]], 0) || identical(args[[2L]], 0)) {
      return(0)
    }
    if (identical(args[[1L]], 1)) {
      return(args[[2L]])
    }
    if (identical(args[[2L]], 1)) {
      return(args[[1L]])
    }
  } else if (op == "/") {
    if (identical(args[[1L]], 0)) {
      return(0)
    }
    if (identical(args[[2L]], 1)) {
      return(args[[1L]])
    }
  } else if (op == "^") {
    if (identical(args[[2L]], 0)) {
      return(1)
    }
    if (identical(args[[2L]], 1)) {
      return(args[[1L]])
    }
  }
  c(list(op), args)
}

#' .morie_gr_sym_diff
#'
#' A step of the geron_train_native implementation. Called by \code{morie_geron_symbolic_differentiation}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param e A vector; its length is taken and its elements indexed.
#' @param var Passed to \code{.morie_gr_sym_diff}.
#' @return The value of \code{list}.
#' @export
.morie_gr_sym_diff <- function(e, var) {
  if (.morie_gr_sym_is_const(e)) {
    return(0)
  }
  if (is.character(e) && length(e) == 1L) {
    return(if (e == var) 1 else 0)
  }
  op <- e[[1L]]
  if (op == "+") {
    return(list(
      "+", .morie_gr_sym_diff(e[[2L]], var),
      .morie_gr_sym_diff(e[[3L]], var)
    ))
  }
  if (op == "-") {
    return(list(
      "-", .morie_gr_sym_diff(e[[2L]], var),
      .morie_gr_sym_diff(e[[3L]], var)
    ))
  }
  if (op == "*") {
    return(list(
      "+",
      list("*", .morie_gr_sym_diff(e[[2L]], var), e[[3L]]),
      list("*", e[[2L]], .morie_gr_sym_diff(e[[3L]], var))
    ))
  }
  if (op == "/") {
    num <- list(
      "-", list("*", .morie_gr_sym_diff(e[[2L]], var), e[[3L]]),
      list("*", e[[2L]], .morie_gr_sym_diff(e[[3L]], var))
    )
    return(list("/", num, list("^", e[[3L]], 2)))
  }
  if (op == "^") {
    .morie_gr_need(
      .morie_gr_sym_is_const(e[[3L]]),
      "only constant exponents are supported."
    )
    return(list(
      "*", list("*", e[[3L]], list("^", e[[2L]], e[[3L]] - 1)),
      .morie_gr_sym_diff(e[[2L]], var)
    ))
  }
  if (op == "neg") {
    return(list("neg", .morie_gr_sym_diff(e[[2L]], var)))
  }
  .morie_gr_need(op %in% .morie_gr_sym_unary, "unknown operator.")
  inner <- .morie_gr_sym_diff(e[[2L]], var)
  outer_e <- if (op == "sin") {
    list("cos", e[[2L]])
  } else if (op == "cos") {
    list("neg", list("sin", e[[2L]]))
  } else if (op == "exp") {
    list("exp", e[[2L]])
  } else {
    list("/", 1, e[[2L]])
  }
  list("*", outer_e, inner)
}

#' .morie_gr_sym_str
#'
#' A step of the geron_train_native implementation. Called by \code{morie_geron_symbolic_differentiation}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param e A vector; its length is taken and its elements indexed.
#' @return A character value.
#' @export
.morie_gr_sym_str <- function(e) {
  if (.morie_gr_sym_is_const(e)) {
    return(format(e))
  }
  if (is.character(e) && length(e) == 1L) {
    return(e)
  }
  op <- e[[1L]]
  if (op == "neg") {
    return(paste0("-(", .morie_gr_sym_str(e[[2L]]), ")"))
  }
  if (op %in% .morie_gr_sym_unary) {
    return(paste0(op, "(", .morie_gr_sym_str(e[[2L]]), ")"))
  }
  paste0(
    "(", .morie_gr_sym_str(e[[2L]]), " ", op, " ",
    .morie_gr_sym_str(e[[3L]]), ")"
  )
}

#' Symbolic differentiation (Geron Ch 12, morie.fn grsmd)
#'
#' Expression trees are lists like `list("*", "x", 3)`; sum, product,
#' quotient, power and chain rules, then constant folding.
#'
#' @param expression Expression tree.
#' @param var Variable name.
#' @param at Optional named list of values to evaluate the derivative.
#' @return List with `derivative`, `derivative_str`, `expression_str`,
#'   `variable` and, when `at` is given, `value`.
#' @export
morie_geron_symbolic_differentiation <- function(expression, var = "x",
                                                 at = NULL) {
  .morie_gr_need(is.character(var) && nzchar(var), "var must be a variable name.")
  check <- function(e) {
    if (.morie_gr_sym_is_const(e) || (is.character(e) && length(e) == 1L)) {
      return(invisible())
    }
    .morie_gr_need(is.list(e) && length(e) >= 2L, "malformed sub-expression.")
    op <- e[[1L]]
    if (op %in% .morie_gr_sym_unary) {
      .morie_gr_need(
        length(e) == 2L,
        "unary operator takes one argument."
      )
    } else if (op %in% .morie_gr_sym_binary) {
      .morie_gr_need(
        length(e) == 3L,
        "binary operator takes two arguments."
      )
    } else {
      stop("unknown operator ", op, call. = FALSE)
    }
    for (a in e[-1L]) check(a)
  }
  check(expression)
  d <- .morie_gr_sym_simplify(.morie_gr_sym_diff(expression, var))
  out <- list(
    derivative = d, derivative_str = .morie_gr_sym_str(d),
    expression_str = .morie_gr_sym_str(expression), variable = var,
    estimate = .morie_gr_sym_str(d), n = 1L
  )
  if (!is.null(at)) {
    .morie_gr_need(is.list(at), "at must be a named list.")
    out$value <- .morie_gr_sym_eval(d, at)
    out$estimate <- out$value
  }
  out
}

# -------------------------------------------------------------- grsnt

#' Binary sentiment head (Geron Ch 16, morie.fn grsnt)
#'
#' p = sigmoid(w . pool(E\[ids\]) + b); mean pooling loses order, max acts
#' as a keyword detector. Token ids are 0-based.
#'
#' @param token_ids 0-based ids.
#' @param E (V, d) embedding table.
#' @param w Length-d weights.
#' @param b Finite bias.
#' @param pooling "mean", "max" or "sum".
#' @param threshold Label cut in \[0, 1\].
#' @return List with `probability`, `label`, `logit`, `pooled`,
#'   `token_contributions`.
#' @export
morie_geron_sentiment_binary <- function(token_ids, E, w, b = 0,
                                         pooling = "mean", threshold = 0.5) {
  ids <- as.numeric(token_ids)
  .morie_gr_need(length(ids) > 0L, "token_ids is empty.")
  .morie_gr_need(all(ids == round(ids)), "token_ids must be integers.")
  ids <- as.integer(ids)
  Em <- .morie_gr_a2d(E)
  .morie_gr_need(length(Em) > 0L, "E must be a non-empty (V, d) matrix.")
  .morie_gr_need(min(ids) >= 0L && max(ids) < nrow(Em), "token ids out of range.")
  wv <- as.numeric(w)
  .morie_gr_need(length(wv) == ncol(Em), "w length must equal the embedding width.")
  .morie_gr_fin(Em, "E")
  .morie_gr_fin(wv, "w")
  b <- as.numeric(b)
  .morie_gr_need(is.finite(b), "b must be finite.")
  threshold <- as.numeric(threshold)
  .morie_gr_need(threshold >= 0 && threshold <= 1, "threshold must lie in [0, 1].")
  V <- Em[ids + 1L, , drop = FALSE]
  pooled <- switch(pooling,
    mean = colMeans(V),
    max = apply(V, 2L, max),
    sum = colSums(V),
    stop("pooling must be 'mean', 'max' or 'sum'.", call. = FALSE)
  )
  logit <- sum(pooled * wv) + b
  p <- .morie_gr_sigmoid_vec(logit)
  list(
    probability = p, label = as.integer(p >= threshold), logit = logit,
    pooled = pooled, token_contributions = as.numeric(V %*% wv),
    estimate = p, n = length(ids)
  )
}

# ------------------------------------------------------------- grstae

#' Stacked (deep) autoencoder forward pass (morie.fn grstae)
#'
#' Encoder matrices in, tied transposes back out; the bottleneck is
#' enforced and the tower's symmetry checked. No biases.
#'
#' @param x Input batch.
#' @param layer_weights List of weight matrices.
#' @param activation "relu", "sigmoid", "tanh" or "linear".
#' @param tied Reuse the transposed encoder weights as the decoder.
#' @param output_activation Activation of the final decoder layer.
#' @return List with `reconstruction`, `code`, `activations`,
#'   `reconstruction_error`, `compression`, `tied`.
#' @export
morie_geron_stacked_autoencoder <- function(x, layer_weights,
                                            activation = "relu", tied = TRUE,
                                            output_activation = "linear") {
  act <- function(z, kind) {
    switch(kind,
      relu = pmax(z, 0),
      sigmoid = matrix(.morie_gr_sigmoid_vec(z), nrow(z)),
      tanh = tanh(z),
      linear = z,
      stop("activation must be relu, sigmoid, tanh or linear.", call. = FALSE)
    )
  }
  X <- .morie_gr_a2d(x)
  .morie_gr_need(length(X) > 0L, "x is empty.")
  .morie_gr_fin(X, "x")
  mats <- lapply(layer_weights, .morie_gr_a2d)
  .morie_gr_need(length(mats) > 0L, "layer_weights is empty.")
  for (W in mats) .morie_gr_fin(W, "layer_weights")
  if (isTRUE(tied)) {
    enc <- mats
    dec <- lapply(rev(mats), t)
  } else {
    .morie_gr_need(
      length(mats) %% 2L == 0L,
      "untied weights need an encoder and decoder matrix per level."
    )
    half <- length(mats) %/% 2L
    enc <- mats[seq_len(half)]
    dec <- mats[(half + 1L):length(mats)]
  }
  width <- ncol(X)
  for (W in enc) {
    .morie_gr_need(nrow(W) == width, "an encoder layer has the wrong input width.")
    width <- ncol(W)
  }
  code_width <- width
  .morie_gr_need(code_width < ncol(X), "the code must be narrower than the input.")
  for (W in dec) {
    .morie_gr_need(nrow(W) == width, "a decoder layer has the wrong input width.")
    width <- ncol(W)
  }
  .morie_gr_need(width == ncol(X), "the tower is not symmetric.")
  acts <- list(X)
  h <- X
  for (W in enc) {
    h <- act(h %*% W, activation)
    acts[[length(acts) + 1L]] <- h
  }
  code <- h
  for (i in seq_along(dec)) {
    kind <- if (i == length(dec)) output_activation else activation
    h <- act(h %*% dec[[i]], kind)
    acts[[length(acts) + 1L]] <- h
  }
  list(
    reconstruction = h, code = code, activations = acts,
    reconstruction_error = mean((h - X)^2),
    compression = ncol(X) / ncol(code), tied = isTRUE(tied),
    estimate = h, n = nrow(X)
  )
}

# -------------------------------------------------------------- grstd

#' Standardization (Geron Ch 2, morie.fn grstd)
#'
#' z = (x - mean)/sd per column with ddof = 0 by default; a constant
#' column raises.
#'
#' @param X Vector or matrix.
#' @param ddof Denominator correction in \[0, m-1\].
#' @return List with `scaled`, `mean`, `scale`.
#' @export
morie_geron_standardization_grstd <- function(X, ddof = 0) {
  flat <- !is.matrix(X)
  A <- if (flat) matrix(as.numeric(X), ncol = 1L) else X
  storage.mode(A) <- "double"
  .morie_gr_need(length(A) > 0L, "X must be non-empty.")
  .morie_gr_fin(A, "X")
  ddof <- as.integer(ddof)
  .morie_gr_need(
    ddof >= 0L && ddof < nrow(A),
    paste0("ddof must lie in [0, ", nrow(A) - 1L, "].")
  )
  mu <- colMeans(A)
  sd_v <- apply(A, 2L, function(col) {
    sqrt(sum((col - mean(col))^2) / (length(col) - ddof))
  })
  .morie_gr_need(all(sd_v != 0), "a constant column makes standardization undefined.")
  Z <- sweep(sweep(A, 2L, mu, "-"), 2L, sd_v, "/")
  out <- if (flat) as.numeric(Z) else Z
  list(scaled = out, mean = mu, scale = sd_v, estimate = out, n = nrow(A))
}

# -------------------------------------------------------------- grstk

#' Stacking blender (Geron Ch 7, morie.fn grstk)
#'
#' Least-squares meta-learner (minimum-norm, matching numpy.lstsq) over
#' out-of-fold base predictions, with an intercept by default.
#'
#' @param base_preds (m, L) base model predictions.
#' @param y Targets.
#' @param blender Optional function(P, y) returning a predictor.
#' @param include_intercept Prepend a column of ones.
#' @return List with `predictions`, `weights`, `rmse`, `base_rmse`,
#'   `improvement`.
#' @export
morie_geron_stacking_predictor <- function(base_preds, y, blender = NULL,
                                           include_intercept = TRUE) {
  P <- .morie_gr_a2d(base_preds)
  yv <- as.numeric(y)
  .morie_gr_need(length(P) > 0L, "base_preds must be a non-empty (m, L) matrix.")
  .morie_gr_need(nrow(P) == length(yv), "base_preds rows must equal length(y).")
  .morie_gr_fin(P, "base_preds")
  .morie_gr_fin(yv, "y")
  base_rmse <- sqrt(colMeans((P - yv)^2))
  if (is.null(blender)) {
    D <- if (isTRUE(include_intercept)) cbind(1, P) else P
    coef <- .morie_gr_lstsq(D, yv)
    pred <- as.numeric(D %*% coef)
    weights <- coef
  } else {
    .morie_gr_need(is.function(blender), "blender must be a function.")
    fitted <- blender(P, yv)
    .morie_gr_need(is.function(fitted), "blender must return a callable predictor.")
    pred <- as.numeric(fitted(P))
    .morie_gr_need(length(pred) == length(yv), "blender returned the wrong length.")
    .morie_gr_fin(pred, "blender predictions")
    weights <- NULL
  }
  rmse <- sqrt(mean((pred - yv)^2))
  list(
    predictions = pred, weights = weights, rmse = rmse,
    base_rmse = base_rmse, improvement = min(base_rmse) - rmse,
    estimate = pred, n = nrow(P)
  )
}

# ------------------------------------------------------------- grswin

#' Swin windowed self-attention (morie.fn grswin)
#'
#' Attention inside M x M windows, O(HW M^2) rather than O((HW)^2);
#' a non-zero `shift` rolls the partition by -shift on both spatial axes
#' before windowing and rolls the output back.
#'
#' @param X (H, W, d_model) array.
#' @param window_size M dividing both H and W.
#' @param WQ,WK,WV Projections with d_model rows.
#' @param shift Cyclic shift in \[0, M-1\].
#' @return List with `output`, `window_weights`, `n_windows`,
#'   `tokens_per_window`, `shift`.
#' @export
morie_geron_swin_window_attention <- function(X, window_size, WQ, WK, WV,
                                              shift = 0) {
  A <- X
  .morie_gr_need(
    length(dim(A)) == 3L && length(A) > 0L,
    "X must be a non-empty (H, W, d_model) array."
  )
  .morie_gr_fin(A, "X")
  H <- dim(A)[1L]
  W <- dim(A)[2L]
  d <- dim(A)[3L]
  M <- as.integer(window_size)
  .morie_gr_need(M >= 1L, "window_size must be at least 1.")
  .morie_gr_need(H %% M == 0L && W %% M == 0L, "window_size must divide the map.")
  mats <- lapply(list(WQ = WQ, WK = WK, WV = WV), .morie_gr_a2d)
  for (nm in names(mats)) {
    .morie_gr_need(
      nrow(mats[[nm]]) == d,
      paste0(nm, " must have d_model rows.")
    )
  }
  .morie_gr_need(ncol(mats$WQ) == ncol(mats$WK), "WQ and WK must share d_k.")
  shift <- as.integer(shift)
  .morie_gr_need(
    M == 1L || (shift >= 0L && shift < M),
    paste0("shift must lie in [0, ", M - 1L, "].")
  )
  roll <- function(Z, s) {
    if (s == 0L) {
      return(Z)
    }
    hi <- ((seq_len(dim(Z)[1L]) - 1L + s) %% dim(Z)[1L]) + 1L
    wi <- ((seq_len(dim(Z)[2L]) - 1L + s) %% dim(Z)[2L]) + 1L
    Z[hi, wi, , drop = FALSE]
  }
  B <- if (shift) roll(A, shift) else A
  dv <- ncol(mats$WV)
  out <- array(0, dim = c(H, W, dv))
  weights <- list()
  for (i in seq.int(1L, H, by = M)) {
    for (j in seq.int(1L, W, by = M)) {
      win <- matrix(0, M * M, d)
      for (a in seq_len(M)) {
        for (b in seq_len(M)) {
          win[(a - 1L) * M + b, ] <- B[i + a - 1L, j + b - 1L, ]
        }
      }
      r <- .morie_gr_attend(win %*% mats$WQ, win %*% mats$WK, win %*% mats$WV)
      for (a in seq_len(M)) {
        for (b in seq_len(M)) {
          out[i + a - 1L, j + b - 1L, ] <- r$output[(a - 1L) * M + b, ]
        }
      }
      weights[[length(weights) + 1L]] <- r$weights
    }
  }
  if (shift) out <- roll(out, -shift %% H)
  list(
    output = out, window_weights = weights, n_windows = length(weights),
    tokens_per_window = M * M, shift = shift, estimate = out, n = H * W
  )
}

# -------------------------------------------------------------- grtd0

#' TD(0) value update (Geron Ch 18, morie.fn grtd0)
#'
#' V(s) += alpha \[r + gamma V(s') - V(s)\]; `done` drops the bootstrap.
#' States are 0-based.
#'
#' @param V State value vector.
#' @param state,next_state 0-based indices.
#' @param reward Finite reward.
#' @param alpha Step size in (0, 1\].
#' @param gamma Discount in \[0, 1\].
#' @param done Terminal flag.
#' @return List with `V`, `old_value`, `new_value`, `target`,
#'   `td_error`.
#' @export
morie_geron_td_zero_update <- function(V, state, next_state, reward, alpha,
                                       gamma, done = FALSE) {
  Vv <- as.numeric(V)
  .morie_gr_need(length(Vv) > 0L, "V is empty.")
  .morie_gr_fin(Vv, "V")
  s <- as.integer(state)
  sn <- as.integer(next_state)
  .morie_gr_need(
    s >= 0L && s < length(Vv) && sn >= 0L && sn < length(Vv),
    "states out of range."
  )
  reward <- as.numeric(reward)
  .morie_gr_need(is.finite(reward), "reward must be finite.")
  alpha <- as.numeric(alpha)
  .morie_gr_need(alpha > 0 && alpha <= 1, "alpha must lie in (0, 1].")
  gamma <- as.numeric(gamma)
  .morie_gr_need(gamma >= 0 && gamma <= 1, "gamma must lie in [0, 1].")
  old <- Vv[s + 1L]
  target <- if (isTRUE(done)) reward else reward + gamma * Vv[sn + 1L]
  td <- target - old
  Vv[s + 1L] <- old + alpha * td
  list(
    V = Vv, old_value = old, new_value = Vv[s + 1L], target = target,
    td_error = td, estimate = Vv[s + 1L], n = length(Vv)
  )
}

# ------------------------------------------------------ grteb / grtdb

#' Layer norm used by the transformer blocks (morie.fn grteb)
#'
#' @param X (T, d) matrix.
#' @param gamma,beta Optional length-d scale and shift.
#' @param eps Positive variance floor.
#' @return Normalized matrix.
#' @export
morie_geron_block_layer_norm <- function(X, gamma = NULL, beta = NULL,
                                         eps = 1e-05) {
  A <- .morie_gr_a2d(X)
  .morie_gr_need(length(A) > 0L, "layer_norm needs a non-empty 2-D array.")
  eps <- as.numeric(eps)
  .morie_gr_need(eps > 0, "eps must be positive.")
  mu <- rowMeans(A)
  var <- rowMeans((A - mu)^2)
  Z <- (A - mu) / sqrt(var + eps)
  d <- ncol(A)
  if (!is.null(gamma)) {
    g <- as.numeric(gamma)
    .morie_gr_need(length(g) == d, "gamma must have d entries.")
    Z <- sweep(Z, 2L, g, "*")
  }
  if (!is.null(beta)) {
    b <- as.numeric(beta)
    .morie_gr_need(length(b) == d, "beta must have d entries.")
    Z <- sweep(Z, 2L, b, "+")
  }
  Z
}

#' Per-head multi-head attention used by the transformer blocks
#'
#' @param Q_in Query-side input.
#' @param KV_in Key/value-side input.
#' @param weights Named list with `WQ`, `WK`, `WV` (lists of per-head
#'   matrices) and `WO`.
#' @param mask Optional attention mask.
#' @return List with `output` and `weights` (one per head).
#' @export
morie_geron_block_multi_head_attention <- function(Q_in, KV_in, weights,
                                                   mask = NULL) {
  .morie_gr_need(is.list(weights), "attention weights must be a named list.")
  .morie_gr_need(
    all(c("WQ", "WK", "WV", "WO") %in% names(weights)),
    "attention weights missing WQ/WK/WV/WO."
  )
  Qi <- .morie_gr_a2d(Q_in)
  Ki <- .morie_gr_a2d(KV_in)
  hq <- weights$WQ
  hk <- weights$WK
  hv <- weights$WV
  .morie_gr_need(
    length(hq) == length(hk) && length(hq) == length(hv),
    "WQ/WK/WV must list the same number of heads."
  )
  .morie_gr_need(length(hq) > 0L, "no attention heads supplied.")
  outs <- list()
  ws <- list()
  for (i in seq_along(hq)) {
    A <- .morie_gr_a2d(hq[[i]])
    B <- .morie_gr_a2d(hk[[i]])
    C <- .morie_gr_a2d(hv[[i]])
    .morie_gr_need(nrow(A) == ncol(Qi), "a WQ head has the wrong width.")
    .morie_gr_need(
      nrow(B) == ncol(Ki) && nrow(C) == ncol(Ki),
      "WK/WV heads must match the key input width."
    )
    r <- .morie_gr_attend(Qi %*% A, Ki %*% B, Ki %*% C, mask)
    outs[[i]] <- r$output
    ws[[i]] <- r$weights
  }
  concat <- do.call(cbind, outs)
  WO <- .morie_gr_a2d(weights$WO)
  .morie_gr_need(
    nrow(WO) == ncol(concat),
    "WO must have the concatenated head width as its rows."
  )
  list(output = concat %*% WO, weights = ws)
}

#' Position-wise feed-forward used by the transformer blocks
#'
#' @param X (T, d_model) matrix.
#' @param weights Named list with `W1`, `W2` and optional `b1`, `b2`.
#' @return Matrix of the same shape as `X`.
#' @export
morie_geron_block_feed_forward <- function(X, weights) {
  .morie_gr_need(is.list(weights), "ffn weights must be a named list.")
  .morie_gr_need(
    all(c("W1", "W2") %in% names(weights)),
    "ffn weights missing W1/W2."
  )
  A <- .morie_gr_a2d(X)
  W1 <- .morie_gr_a2d(weights$W1)
  W2 <- .morie_gr_a2d(weights$W2)
  .morie_gr_need(nrow(W1) == ncol(A), "W1 must have d_model rows.")
  .morie_gr_need(nrow(W2) == ncol(W1), "W2 must match the hidden width.")
  .morie_gr_need(ncol(W2) == ncol(A), "W2 must map back to d_model.")
  b1 <- if (is.null(weights$b1)) rep(0, ncol(W1)) else as.numeric(weights$b1)
  b2 <- if (is.null(weights$b2)) rep(0, ncol(W2)) else as.numeric(weights$b2)
  .morie_gr_need(
    length(b1) == ncol(W1) && length(b2) == ncol(W2),
    "ffn biases do not match their weight widths."
  )
  H <- pmax(sweep(A %*% W1, 2L, b1, "+"), 0)
  sweep(H %*% W2, 2L, b2, "+")
}

#' Transformer encoder block, post-norm (Geron Ch 16, morie.fn grteb)
#'
#' h = LN(x + MHA(x)); y = LN(h + FFN(h)).
#'
#' @param x (T, d_model) tokens.
#' @param mha_weights Attention weights, optionally with `gamma`/`beta`.
#' @param ffn_weights Feed-forward weights, optionally with
#'   `gamma`/`beta`.
#' @param mask Optional attention mask.
#' @param eps Layer-norm floor.
#' @return List with `output`, `attention_output`, `attention_weights`,
#'   `hidden`, `ffn_output`.
#' @export
morie_geron_transformer_encoder_block <- function(x, mha_weights, ffn_weights,
                                                  mask = NULL, eps = 1e-05) {
  X <- .morie_gr_a2d(x)
  .morie_gr_need(length(X) > 0L, "x must be a non-empty (T, d_model) matrix.")
  .morie_gr_fin(X, "x")
  att <- morie_geron_block_multi_head_attention(X, X, mha_weights, mask)
  .morie_gr_need(
    all(dim(att$output) == dim(X)),
    "attention output must match the residual shape; check WO."
  )
  h <- morie_geron_block_layer_norm(
    X + att$output, mha_weights$gamma,
    mha_weights$beta, eps
  )
  f <- morie_geron_block_feed_forward(h, ffn_weights)
  y <- morie_geron_block_layer_norm(
    h + f, ffn_weights$gamma,
    ffn_weights$beta, eps
  )
  list(
    output = y, attention_output = att$output,
    attention_weights = att$weights, hidden = h, ffn_output = f,
    estimate = y, n = nrow(X)
  )
}

#' Transformer decoder block, post-norm (Geron Ch 16, morie.fn grtdb)
#'
#' LN(x + masked MHA) -> LN(h1 + cross(h1, enc)) -> LN(h2 + FFN); the
#' causal mask is built in and the cross-attention is unmasked.
#'
#' @param x (T, d_model) target tokens.
#' @param encoder_output (S, d_model) source states.
#' @param weights Named list with `self`, `cross` and `ffn` blocks.
#' @param eps Layer-norm floor.
#' @return List with `output`, `self_attention_weights`,
#'   `cross_attention_weights`, `h1`, `h2`, `causal_mask`.
#' @export
morie_geron_transformer_decoder_block <- function(x, encoder_output, weights,
                                                  eps = 1e-05) {
  X <- .morie_gr_a2d(x)
  E <- .morie_gr_a2d(encoder_output)
  .morie_gr_need(length(X) > 0L, "x must be a non-empty (T, d_model) matrix.")
  .morie_gr_need(length(E) > 0L, "encoder_output must be non-empty.")
  .morie_gr_fin(X, "x")
  .morie_gr_fin(E, "encoder_output")
  .morie_gr_need(is.list(weights), "weights must be a named list.")
  .morie_gr_need(
    all(c("self", "cross", "ffn") %in% names(weights)),
    "weights missing self/cross/ffn."
  )
  Tn <- nrow(X)
  causal <- lower.tri(matrix(0, Tn, Tn), diag = TRUE)
  sa <- morie_geron_block_multi_head_attention(X, X, weights$self, causal)
  .morie_gr_need(all(dim(sa$output) == dim(X)), "masked self-attention shape mismatch.")
  h1 <- morie_geron_block_layer_norm(
    X + sa$output, weights$self$gamma,
    weights$self$beta, eps
  )
  ca <- morie_geron_block_multi_head_attention(h1, E, weights$cross, NULL)
  .morie_gr_need(all(dim(ca$output) == dim(X)), "cross-attention shape mismatch.")
  h2 <- morie_geron_block_layer_norm(
    h1 + ca$output, weights$cross$gamma,
    weights$cross$beta, eps
  )
  f <- morie_geron_block_feed_forward(h2, weights$ffn)
  y <- morie_geron_block_layer_norm(
    h2 + f, weights$ffn$gamma,
    weights$ffn$beta, eps
  )
  list(
    output = y, self_attention_weights = sa$weights,
    cross_attention_weights = ca$weights, h1 = h1, h2 = h2,
    causal_mask = causal, estimate = y, n = Tn
  )
}

# -------------------------------------------------------------- grtlu

#' Threshold logic unit (Geron Ch 10, morie.fn grtlu)
#'
#' h(x) = 1 iff w.x + b >= 0; one hyperplane only, so no XOR.
#'
#' @param x Instance vector or (m, n) batch.
#' @param w Length-n weights.
#' @param b Finite bias.
#' @return List with `output` and `margin`.
#' @export
morie_geron_threshold_logic_unit <- function(x, w, b = 0) {
  single <- !is.matrix(x)
  A <- if (single) matrix(as.numeric(x), nrow = 1L) else x
  storage.mode(A) <- "double"
  wv <- as.numeric(w)
  .morie_gr_need(length(A) > 0L, "x must be non-empty.")
  .morie_gr_need(length(wv) == ncol(A), "w length must equal the features of x.")
  .morie_gr_fin(A, "x")
  .morie_gr_fin(wv, "w")
  b <- as.numeric(b)
  .morie_gr_need(is.finite(b), "b must be finite.")
  z <- as.numeric(A %*% wv) + b
  out <- as.integer(z >= 0)
  list(
    output = if (single) out[1L] else out,
    margin = if (single) z[1L] else z,
    estimate = if (single) out[1L] else out, n = nrow(A)
  )
}

# ------------------------------------------------------- grtmp / grtop

#' Temperature-scaled softmax (Geron Ch 16, morie.fn grtmp)
#'
#' p = softmax(z/T); T -> 0 greedy, T -> inf uniform, ranking never
#' changes. `argmax` is 0-based.
#'
#' @param logits Score vector.
#' @param T Strictly positive temperature.
#' @return List with `probabilities`, `entropy`, `perplexity`, `argmax`.
#' @export
morie_geron_temperature_sampling <- function(logits, T = 1) {
  z <- as.numeric(logits)
  .morie_gr_need(length(z) > 0L, "logits is empty.")
  .morie_gr_fin(z, "logits")
  T <- as.numeric(T)
  .morie_gr_need(is.finite(T) && T > 0, "T must be strictly positive.")
  p <- .morie_gr_softmax_vec(z / T)
  ent <- -sum(p * log(pmax(p, 1e-300)))
  list(
    probabilities = p, entropy = ent, perplexity = exp(ent),
    argmax = which.max(p) - 1L, temperature = T, estimate = p,
    n = length(z)
  )
}

#' Top-k truncated sampling distribution (morie.fn grtop)
#'
#' Keeps the k highest probabilities (ties broken by the lower index,
#' matching numpy's lexsort on (-p, index)), renormalises by their mass
#' and zeroes the rest. `kept_indices` are 0-based and ascending.
#'
#' @param logits Score vector.
#' @param k Truncation size in \[1, length(logits)\].
#' @param T Positive temperature.
#' @return List with `probabilities`, `kept_indices`, `kept_mass`,
#'   `full_probabilities`, `entropy`.
#' @export
morie_geron_topk_sampling <- function(logits, k, T = 1) {
  z <- as.numeric(logits)
  .morie_gr_need(length(z) > 0L, "logits is empty.")
  .morie_gr_fin(z, "logits")
  k <- as.integer(k)
  .morie_gr_need(
    k >= 1L && k <= length(z),
    paste0("k must lie in [1, ", length(z), "].")
  )
  T <- as.numeric(T)
  .morie_gr_need(is.finite(T) && T > 0, "T must be strictly positive.")
  p <- .morie_gr_softmax_vec(z / T)
  ord <- order(-p, seq_along(p), method = "radix")
  keep <- sort(ord[seq_len(k)]) - 1L
  mass <- sum(p[keep + 1L])
  .morie_gr_need(mass > 0, "the top-k tokens carry no probability mass.")
  out <- rep(0, length(p))
  out[keep + 1L] <- p[keep + 1L] / mass
  list(
    probabilities = out, kept_indices = keep, kept_mass = mass,
    full_probabilities = p,
    entropy = -sum(out[keep + 1L] * log(out[keep + 1L])),
    estimate = out, n = length(z)
  )
}

# -------------------------------------------------------------- grtnh

#' Hyperbolic tangent activation (morie.fn grtnh)
#'
#' @param z Pre-activations.
#' @return List with `activation`, `derivative`, `saturated`.
#' @export
morie_geron_tanh_activation <- function(z) {
  .morie_gr_need(length(z) > 0L, "z is empty.")
  .morie_gr_fin(z, "z")
  a <- tanh(z)
  list(
    activation = a, derivative = 1 - a * a,
    saturated = mean(abs(a) > 0.99), estimate = a, n = length(z)
  )
}

# ------------------------------------------------------ grtrc / grtrv

#' Classification tree leaf (Geron Ch 6, morie.fn grtrc)
#'
#' The leaf predicts the majority class (0-based, first maximum);
#' proportions are the tree's probabilities.
#'
#' @param y Integer class labels over the whole node set.
#' @param leaf_mask Optional logical selector of the leaf's rows.
#' @return List with `prediction`, `proportions`, `counts`, `gini`,
#'   `entropy`, `n_leaf`.
#' @export
morie_geron_tree_classification_leaf <- function(y, leaf_mask = NULL) {
  yv <- as.numeric(y)
  .morie_gr_need(length(yv) > 0L, "y is empty.")
  .morie_gr_need(all(yv == round(yv)), "classification leaves need integer labels.")
  yv <- as.integer(yv)
  .morie_gr_need(min(yv) >= 0L, "class labels must be non-negative.")
  K <- max(yv) + 1L
  sel <- if (is.null(leaf_mask)) {
    yv
  } else {
    mask <- as.vector(leaf_mask)
    .morie_gr_need(length(mask) == length(yv), "leaf_mask must match y.")
    yv[as.logical(mask)]
  }
  .morie_gr_need(length(sel) > 0L, "leaf_mask selects no instances.")
  counts <- tabulate(sel + 1L, nbins = K)
  p <- counts / length(sel)
  nz <- p[p > 0]
  list(
    prediction = which.max(counts) - 1L, proportions = p, counts = counts,
    gini = 1 - sum(p^2), entropy = -sum(nz * log2(nz)),
    n_leaf = length(sel), estimate = which.max(counts) - 1L,
    n = length(yv)
  )
}

#' Regression tree leaf (Geron Ch 6, morie.fn grtrv)
#'
#' The leaf value is the mean of its targets, the squared-error
#' minimiser.
#'
#' @param y Numeric targets over the node set.
#' @param leaf_mask Optional logical selector.
#' @return List with `prediction`, `mse`, `std`, `n_leaf`.
#' @export
morie_geron_tree_regression_leaf <- function(y, leaf_mask = NULL) {
  yv <- as.numeric(y)
  .morie_gr_need(length(yv) > 0L, "y is empty.")
  .morie_gr_fin(yv, "y")
  sel <- if (is.null(leaf_mask)) {
    yv
  } else {
    mask <- as.vector(leaf_mask)
    .morie_gr_need(length(mask) == length(yv), "leaf_mask must match y.")
    yv[as.logical(mask)]
  }
  .morie_gr_need(length(sel) > 0L, "leaf_mask selects no instances.")
  pred <- mean(sel)
  mse <- mean((sel - pred)^2)
  list(
    prediction = pred, mse = mse, std = sqrt(mse), n_leaf = length(sel),
    estimate = pred, n = length(yv)
  )
}

# -------------------------------------------------------------- grvae

#' VAE evidence lower bound (Geron Ch 17, morie.fn grvae)
#'
#' ELBO = E_q\[log p(x|z)\] - beta KL, both terms divided by the batch
#' size; the KL is the closed form.
#'
#' @param x Input batch.
#' @param mu,logvar Encoder outputs.
#' @param recon Decoder output.
#' @param likelihood "gaussian" or "bernoulli".
#' @param beta Non-negative KL weight.
#' @return List with `elbo`, `loss`, `reconstruction_term`, `kl`,
#'   `kl_per_dim`.
#' @export
morie_geron_vae_elbo <- function(x, mu, logvar, recon,
                                 likelihood = "gaussian", beta = 1) {
  X <- .morie_gr_a2d(x)
  M <- .morie_gr_a2d(mu)
  LV <- .morie_gr_a2d(logvar)
  R <- .morie_gr_a2d(recon)
  .morie_gr_need(length(X) > 0L, "x is empty.")
  .morie_gr_need(all(dim(R) == dim(X)), "recon must match x.")
  .morie_gr_need(all(dim(M) == dim(LV)), "mu must match logvar.")
  .morie_gr_need(nrow(M) == nrow(X), "mu rows must equal x rows.")
  .morie_gr_fin(X, "x")
  .morie_gr_fin(M, "mu")
  .morie_gr_fin(LV, "logvar")
  .morie_gr_fin(R, "recon")
  beta <- as.numeric(beta)
  .morie_gr_need(is.finite(beta) && beta >= 0, "beta must be finite and non-negative.")
  m <- nrow(X)
  recon_term <- if (likelihood == "gaussian") {
    -0.5 * sum((X - R)^2) / m
  } else if (likelihood == "bernoulli") {
    .morie_gr_need(all(R >= 0 & R <= 1), "bernoulli recon must lie in [0, 1].")
    .morie_gr_need(all(X >= 0 & X <= 1), "bernoulli x must lie in [0, 1].")
    Rc <- pmin(pmax(R, 1e-12), 1 - 1e-12)
    sum(X * log(Rc) + (1 - X) * log(1 - Rc)) / m
  } else {
    stop("likelihood must be 'gaussian' or 'bernoulli'.", call. = FALSE)
  }
  kl_dim <- -0.5 * (1 + LV - M^2 - exp(LV))
  kl <- sum(kl_dim) / m
  elbo <- recon_term - beta * kl
  list(
    elbo = elbo, loss = -elbo, reconstruction_term = recon_term, kl = kl,
    kl_per_dim = colMeans(kl_dim), beta = beta, estimate = elbo, n = m
  )
}

# -------------------------------------------------------------- grvit

#' ViT patch embedding (Geron Ch 17, morie.fn grvit)
#'
#' Row-major patches (top-to-bottom, left-to-right), each flattened
#' ROW-MAJOR over (row, col, channel), projected by E, with the CLS token
#' prepended and the positional table added.
#'
#' @param image (H, W) or (H, W, C) array.
#' @param patch_size Divisor of both H and W.
#' @param E (p*p*C, d_model) projection.
#' @param E_pos Optional (n_patches + 1, d_model) positional table.
#' @param cls_token Optional length-d_model CLS vector; default zeros.
#' @return List with `embeddings`, `patches`, `n_patches`, `d_model`.
#' @export
morie_geron_vit_patch_embedding <- function(image, patch_size, E, E_pos = NULL,
                                            cls_token = NULL) {
  A <- image
  if (length(dim(A)) == 2L) A <- array(A, dim = c(dim(A), 1L))
  .morie_gr_need(
    length(dim(A)) == 3L && length(A) > 0L,
    "image must be (H, W) or (H, W, C)."
  )
  .morie_gr_fin(A, "image")
  p <- as.integer(patch_size)
  .morie_gr_need(p >= 1L, "patch_size must be at least 1.")
  H <- dim(A)[1L]
  W <- dim(A)[2L]
  C <- dim(A)[3L]
  .morie_gr_need(H %% p == 0L && W %% p == 0L, "patch_size must divide the image.")
  Em <- .morie_gr_a2d(E)
  dimn <- p * p * C
  .morie_gr_need(nrow(Em) == dimn, "E must have patch_size^2 * channels rows.")
  d_model <- ncol(Em)
  npatch <- (H %/% p) * (W %/% p)
  P <- matrix(0, npatch, dimn)
  idx <- 0L
  for (i in seq.int(1L, H, by = p)) {
    for (j in seq.int(1L, W, by = p)) {
      idx <- idx + 1L
      blk <- A[i:(i + p - 1L), j:(j + p - 1L), , drop = FALSE]
      # row-major flatten of (p, p, C): channel fastest, then column, then row.
      P[idx, ] <- as.numeric(aperm(blk, c(3L, 2L, 1L)))
    }
  }
  Z <- P %*% Em
  cls <- if (is.null(cls_token)) rep(0, d_model) else as.numeric(cls_token)
  .morie_gr_need(length(cls) == d_model, "cls_token must have d_model entries.")
  Z <- rbind(cls, Z, deparse.level = 0L)
  if (!is.null(E_pos)) {
    Ep <- .morie_gr_a2d(E_pos)
    .morie_gr_need(all(dim(Ep) == dim(Z)), "E_pos must match the embedding shape.")
    Z <- Z + Ep
  }
  list(
    embeddings = Z, patches = P, n_patches = npatch, d_model = d_model,
    estimate = Z, n = nrow(Z)
  )
}

# ----------------------------------------------------- grvoth / grvots

#' Hard voting ensemble (Geron Ch 7, morie.fn grvoth)
#'
#' y_hat = mode of the L predicted labels; ties go to the lowest class
#' index and are counted.
#'
#' @param predictions (L, m) integer label matrix.
#' @return List with `y_hat` (0-based), `vote_counts`, `agreement`,
#'   `ties`.
#' @export
morie_geron_hard_voting <- function(predictions) {
  P <- .morie_gr_a2d(predictions)
  .morie_gr_need(length(P) > 0L, "predictions must be a non-empty (L, m) array.")
  .morie_gr_need(all(P == round(P)), "hard voting needs integer class labels.")
  storage.mode(P) <- "integer"
  .morie_gr_need(min(P) >= 0L, "class labels must be non-negative.")
  L <- nrow(P)
  m <- ncol(P)
  K <- max(P) + 1L
  counts <- matrix(0L, m, K)
  for (j in seq_len(m)) counts[j, ] <- tabulate(P[, j] + 1L, nbins = K)
  yhat <- max.col(counts, ties.method = "first") - 1L
  top <- apply(counts, 1L, max)
  ties <- sum(rowSums(counts == top) > 1L)
  list(
    y_hat = yhat, vote_counts = counts, agreement = top / L, ties = ties,
    estimate = yhat, n = m
  )
}

#' Soft voting ensemble (Geron Ch 7, morie.fn grvots)
#'
#' y_hat = argmax of the weight-averaged class probabilities.
#'
#' @param probabilities (L, m, K) array, or a (L, K) matrix for a single
#'   instance.
#' @param weights Optional non-negative voter weights.
#' @return List with `y_hat` (0-based), `mean_probabilities`,
#'   `confidence`, `margin`.
#' @export
morie_geron_soft_voting <- function(probabilities, weights = NULL) {
  A <- probabilities
  if (is.matrix(A)) A <- array(A, dim = c(nrow(A), 1L, ncol(A)))
  .morie_gr_need(
    length(dim(A)) == 3L && length(A) > 0L,
    "probabilities must be (L, m, K) or (L, K)."
  )
  .morie_gr_fin(A, "probabilities")
  .morie_gr_need(all(A >= 0), "probabilities must be non-negative.")
  .morie_gr_need(
    all(abs(apply(A, c(1L, 2L), sum) - 1) <= 1e-06),
    "each classifier's rows must sum to 1."
  )
  L <- dim(A)[1L]
  w <- if (is.null(weights)) {
    rep(1 / L, L)
  } else {
    ww <- as.numeric(weights)
    .morie_gr_need(length(ww) == L, "weights must have one entry per classifier.")
    .morie_gr_need(
      all(ww >= 0) && sum(ww) > 0,
      "weights must be non-negative with a positive sum."
    )
    ww / sum(ww)
  }
  M <- matrix(0, dim(A)[2L], dim(A)[3L])
  for (l in seq_len(L)) M <- M + w[l] * matrix(A[l, , ], dim(A)[2L], dim(A)[3L])
  yhat <- max.col(M, ties.method = "first") - 1L
  srt <- t(apply(M, 1L, sort))
  if (ncol(M) == 1L) srt <- matrix(srt, ncol = 1L)
  margin <- if (ncol(M) > 1L) {
    srt[, ncol(M)] - srt[, ncol(M) - 1L]
  } else {
    srt[, 1L]
  }
  list(
    y_hat = yhat, mean_probabilities = M, confidence = apply(M, 1L, max),
    margin = margin, estimate = yhat, n = nrow(M)
  )
}

# -------------------------------------------------------------- grwdc

#' AdamW with decoupled weight decay (Geron Ch 11, morie.fn grwdc)
#'
#' theta -= eta (m_hat/(sqrt(s_hat)+eps) + lam theta); the decay sits
#' OUTSIDE the adaptive normalisation. The Adam part comes from
#' [morie_geron_adam_update()].
#'
#' @param theta,grad,m,s Parameters, gradient and moment accumulators.
#' @param t 1-based timestep.
#' @param eta Learning rate.
#' @param b1,b2,eps Adam hyperparameters.
#' @param lam Non-negative decoupled decay.
#' @return List with `theta_new`, `m_new`, `s_new`, `adam_step`,
#'   `decay_step`, `step`.
#' @export
morie_geron_adamw_decoupled_weight_decay <- function(theta, grad, m, s, t, eta,
                                                     b1 = 0.9, b2 = 0.999,
                                                     eps = 1e-08, lam = 0.01) {
  inner <- morie_geron_adam_update(theta, grad, m, s, t, eta,
    b1 = b1,
    b2 = b2, eps = eps
  )
  th <- theta
  lam <- as.numeric(lam)
  .morie_gr_need(is.finite(lam) && lam >= 0, "lam must be finite and non-negative.")
  adam_step <- inner$step
  decay_step <- as.numeric(eta) * lam * th
  step <- adam_step + decay_step
  list(
    theta_new = th - step, m_new = inner$m_new, s_new = inner$s_new,
    adam_step = adam_step, decay_step = decay_step, step = step,
    t = as.integer(t), estimate = sqrt(sum(step^2)), n = length(th)
  )
}

# -------------------------------------------------------------- grwpc

#' WordPiece merge score (Geron Ch 16, morie.fn grwpc)
#'
#' score(A, B) = count(AB) / (count(A) count(B)); BPE takes frequency,
#' WordPiece takes surprise. Ties in the ranking break on the string
#' form of the pair.
#'
#' @param counts Named numeric vector or list of symbol counts.
#' @param pairs List of `list(a, b, count)` triples (or a named list
#'   keyed "a|b").
#' @return List with `scores`, `best_pair`, `best_score`, `ranking`.
#' @export
morie_geron_wordpiece_tokenizer_score <- function(counts, pairs) {
  cnt <- as.list(counts)
  .morie_gr_need(length(cnt) > 0L, "counts is empty.")
  for (k in names(cnt)) {
    .morie_gr_need(
      is.finite(as.numeric(cnt[[k]])) &&
        as.numeric(cnt[[k]]) > 0,
      "every count must be positive."
    )
  }
  items <- lapply(pairs, function(row) {
    .morie_gr_need(length(row) == 3L, "pairs must hold (A, B, count) triples.")
    list(
      a = as.character(row[[1L]]), b = as.character(row[[2L]]),
      c = as.numeric(row[[3L]])
    )
  })
  .morie_gr_need(length(items) > 0L, "pairs is empty.")
  keys <- character(length(items))
  vals <- numeric(length(items))
  for (i in seq_along(items)) {
    it <- items[[i]]
    for (sym in c(it$a, it$b)) {
      .morie_gr_need(!is.null(cnt[[sym]]), "a pair symbol is absent from counts.")
    }
    .morie_gr_need(is.finite(it$c) && it$c > 0, "pair counts must be positive.")
    ca <- as.numeric(cnt[[it$a]])
    cb <- as.numeric(cnt[[it$b]])
    .morie_gr_need(it$c <= min(ca, cb), "a pair cannot outnumber its parts.")
    keys[i] <- paste0("('", it$a, "', '", it$b, "')")
    vals[i] <- it$c / (ca * cb)
  }
  ord <- order(-vals, keys, method = "radix")
  list(
    scores = stats::setNames(as.list(vals), keys),
    best_pair = c(items[[ord[1L]]]$a, items[[ord[1L]]]$b),
    best_score = vals[ord[1L]],
    ranking = stats::setNames(vals[ord], keys[ord]),
    estimate = vals[ord[1L]], n = length(vals)
  )
}

# ------------------------------------------------------------- grxgbg

#' XGBoost regularized split gain (Geron Ch 7, morie.fn grxgbg)
#'
#' Gain = 0.5 \[GL^2/(HL+l) + GR^2/(HR+l) - G^2/(H+l)\] - gamma; a
#' negative gain means prune.
#'
#' @param GL,HL,GR,HR Gradient and Hessian sums per child.
#' @param lam Non-negative L2 leaf penalty.
#' @param gamma Non-negative split cost.
#' @return List with `gain`, `left_score`, `right_score`,
#'   `parent_score`, `left_weight`, `right_weight`, `should_split`.
#' @export
morie_geron_xgboost_gain <- function(GL, HL, GR, HR, lam = 1, gamma = 0) {
  GL <- as.numeric(GL)
  HL <- as.numeric(HL)
  GR <- as.numeric(GR)
  HR <- as.numeric(HR)
  .morie_gr_need(all(is.finite(c(GL, HL, GR, HR))), "GL, HL, GR and HR must be finite.")
  .morie_gr_need(HL >= 0 && HR >= 0, "Hessian sums must be non-negative.")
  lam <- as.numeric(lam)
  gamma <- as.numeric(gamma)
  .morie_gr_need(lam >= 0 && gamma >= 0, "lam and gamma must be non-negative.")
  .morie_gr_need(
    HL + lam > 0 && HR + lam > 0 && HL + HR + lam > 0,
    "a denominator H + lambda is non-positive; raise lam."
  )
  left <- GL^2 / (HL + lam)
  right <- GR^2 / (HR + lam)
  parent <- (GL + GR)^2 / (HL + HR + lam)
  gain <- 0.5 * (left + right - parent) - gamma
  list(
    gain = gain, left_score = left, right_score = right,
    parent_score = parent, left_weight = -GL / (HL + lam),
    right_weight = -GR / (HR + lam), should_split = gain > 0,
    estimate = gain, n = 2L
  )
}

# -------------------------------------------------------------- grxvi

#' Glorot (Xavier) initialization (Geron Ch 11, morie.fn grxvi)
#'
#' Var(W) = 2/(fan_in + fan_out); the uniform limit is
#' sqrt(6/(in+out)). The normal branch consumes an even LCG budget,
#' builds the cos half and the sin half SEPARATELY, and concatenates
#' cos-block then sin-block before truncating -- NOT the interleaved
#' pair order used by grhei / grgrp. `achieved_variance` is the
#' ddof = 0 variance. The weight matrix is reshaped ROW-MAJOR into
#' (fan_in, fan_out).
#'
#' @param fan_in,fan_out Positive widths.
#' @param distribution "normal" or "uniform".
#' @param seed LCG seed.
#' @return List with `weights`, `target_variance`, `achieved_variance`,
#'   `scale`, `distribution`.
#' @export
morie_geron_glorot_xavier_init <- function(fan_in, fan_out,
                                           distribution = "normal", seed = 42) {
  fan_in <- as.integer(fan_in)
  fan_out <- as.integer(fan_out)
  .morie_gr_need(fan_in >= 1L && fan_out >= 1L, "fan_in and fan_out must be positive.")
  target <- 2 / (fan_in + fan_out)
  n <- fan_in * fan_out
  count <- if (distribution == "uniform") n else 2L * ((n + 1L) %/% 2L)
  u <- .morie_gr_lcg_u(count, seed)
  if (distribution == "uniform") {
    limit <- sqrt(6 / (fan_in + fan_out))
    Wv <- (2 * u - 1) * limit
    scale <- limit
  } else if (distribution == "normal") {
    sigma <- sqrt(target)
    u1 <- pmin(pmax(u[seq(1L, count, by = 2L)], 1e-12), 1)
    u2 <- u[seq(2L, count, by = 2L)]
    z <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
    z2 <- sqrt(-2 * log(u1)) * sin(2 * pi * u2)
    Wv <- c(z, z2)[seq_len(n)] * sigma
    scale <- sigma
  } else {
    stop("distribution must be 'normal' or 'uniform'.", call. = FALSE)
  }
  W <- matrix(Wv, nrow = fan_in, ncol = fan_out, byrow = TRUE)
  list(
    weights = W, target_variance = target,
    achieved_variance = .morie_gr_pvar(as.numeric(W)), scale = scale,
    distribution = distribution, estimate = W, n = n
  )
}

# -------------------------------------------------------------- gryol

#' YOLO grid loss (Geron Ch 14, morie.fn gryol)
#'
#' lam_coord (xy + sqrt-wh) + obj + lam_noobj noobj + class, every term
#' but objectness masked to the cells that contain an object.
#'
#' @param predictions,targets (S, S, 5 + C) arrays: x, y, w, h, conf,
#'   then class scores.
#' @param lam_coord,lam_noobj Non-negative loss weights.
#' @return List with `loss`, `loss_coord`, `loss_obj`, `loss_noobj`,
#'   `loss_class`, `n_objects`, `n_cells`.
#' @export
morie_geron_yolo_grid_loss <- function(predictions, targets, lam_coord = 5,
                                       lam_noobj = 0.5) {
  P <- predictions
  Tg <- targets
  .morie_gr_need(
    length(dim(P)) == 3L && dim(P)[3L] >= 5L,
    "predictions must be (S, S, 5 + C)."
  )
  .morie_gr_need(identical(dim(P), dim(Tg)), "targets must match predictions.")
  .morie_gr_fin(P, "predictions")
  .morie_gr_fin(Tg, "targets")
  lam_coord <- as.numeric(lam_coord)
  lam_noobj <- as.numeric(lam_noobj)
  .morie_gr_need(
    lam_coord >= 0 && lam_noobj >= 0,
    "lam_coord and lam_noobj must be non-negative."
  )
  .morie_gr_need(
    all(P[, , 3:4] >= 0) && all(Tg[, , 3:4] >= 0),
    "box width and height must be non-negative."
  )
  conf_t <- matrix(Tg[, , 5L], dim(P)[1L], dim(P)[2L])
  .morie_gr_need(
    all(conf_t == 0 | conf_t == 1),
    "target confidence must be 0 or 1."
  )
  obj <- conf_t == 1
  dxy <- (P[, , 1L] - Tg[, , 1L])^2 + (P[, , 2L] - Tg[, , 2L])^2
  dwh <- (sqrt(P[, , 3L]) - sqrt(Tg[, , 3L]))^2 +
    (sqrt(P[, , 4L]) - sqrt(Tg[, , 4L]))^2
  dxy <- matrix(dxy, dim(P)[1L], dim(P)[2L])
  dwh <- matrix(dwh, dim(P)[1L], dim(P)[2L])
  loss_coord <- lam_coord * (sum(dxy[obj]) + sum(dwh[obj]))
  dconf <- matrix((P[, , 5L] - Tg[, , 5L])^2, dim(P)[1L], dim(P)[2L])
  loss_obj <- sum(dconf[obj])
  loss_noobj <- lam_noobj * sum(dconf[!obj])
  loss_class <- 0
  if (dim(P)[3L] > 5L) {
    dcls <- matrix(0, dim(P)[1L], dim(P)[2L])
    for (k in 6:dim(P)[3L]) {
      dcls <- dcls + matrix((P[, , k] - Tg[, , k])^2, dim(P)[1L], dim(P)[2L])
    }
    loss_class <- sum(dcls[obj])
  }
  loss <- loss_coord + loss_obj + loss_noobj + loss_class
  list(
    loss = loss, loss_coord = loss_coord, loss_obj = loss_obj,
    loss_noobj = loss_noobj, loss_class = loss_class,
    n_objects = sum(obj), n_cells = dim(P)[1L] * dim(P)[2L],
    estimate = loss, n = dim(P)[1L] * dim(P)[2L]
  )
}
