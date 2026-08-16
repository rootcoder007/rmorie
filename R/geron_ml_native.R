# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Geron shelf. Mirrors 80 morie.fn modules (hm* wave 4, gr* wave 5)
# from Geron A, Hands-On Machine Learning with Scikit-Learn, Keras and
# TensorFlow (3rd ed., O'Reilly 2022).
#
# Porting conventions, uniform across this file:
#   * Indices that Python returns 0-based (argmax positions, policies,
#     token ids, matched pairs, masked positions) stay 0-based here so
#     the two languages agree token-for-token. Any index used to SUBSET
#     an R object is converted with +1 internally.
#   * numpy's default variance/std is the POPULATION form (ddof = 0);
#     stats::var / stats::sd are the n-1 form. Wherever Python used the
#     default we use .morie_gr_pvar / .morie_gr_psd here.
#   * numpy ravel/reshape are row-major; R is column-major. Matrix
#     flattening therefore goes through t() first, and matrices built
#     from a flat stream use byrow = TRUE.
#   * %/% binds tighter than + - in R, so every floor-division of a sum
#     is parenthesised explicitly.
#   * Python's argmax and R's which.max both take the FIRST maximum, so
#     tie behaviour already agrees; np.argsort(kind="mergesort") is
#     matched by order(method = "radix"), which is also stable.
#
# Shared helpers already in this package are called rather than
# duplicated: .morie_al_softmax_rows (alammar_llm_native.R) is the
# row-wise softmax used by every attention/contrastive routine here.

#' .morie_gr_pvar
#'
#' A step of the geron_ml_native implementation. Called by \code{.morie_gr_psd}, \code{.morie_w4c_pca_svd}, \code{morie_geron_glorot_xavier_init} and 3 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A vector; its length is taken.
#' @return A numeric value.
#' @export
.morie_gr_pvar <- function(x) {
  n <- length(x)
  if (n == 0) {
    return(NA_real_)
  }
  mean((x - mean(x))^2)
}

#' .morie_gr_psd
#'
#' A step of the geron_ml_native implementation. Called by \code{morie_geron_credit_assignment}, \code{morie_geron_dcgan_generator}, \code{morie_geron_randomized_search_cv}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Passed to \code{.morie_gr_pvar}.
#' @return A numeric value.
#' @export
.morie_gr_psd <- function(x) sqrt(.morie_gr_pvar(x))

#' .morie_gr_softmax
#'
#' A step of the geron_ml_native implementation. Called by \code{morie_geron_a2c}, \code{morie_geron_a3c}, \code{morie_geron_bahdanau_attention} and 10 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z Numeric; passed to \code{max}.
#' @return A numeric value.
#' @export
.morie_gr_softmax <- function(z) {
  e <- exp(z - max(z))
  e / sum(e)
}

# log-softmax of a vector, overflow safe.
#' Log-softmax of a vector, overflow safe
#'
#' A step of the geron_ml_native implementation. Called by \code{morie_geron_dalle_autoregressive_token}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param z Numeric; passed to \code{max}.
#' @return A numeric value.
#' @export
.morie_gr_log_softmax <- function(z) {
  m <- max(z)
  zz <- z - m
  zz - log(sum(exp(zz)))
}

#' .morie_gr_log_softmax_rows
#'
#' A step of the geron_ml_native implementation. Called by \code{morie_geron_bert_nsp_loss}, \code{morie_geron_blip_itm_itc}, \code{morie_geron_classification_localization} and 9 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Z A matrix; passed to \code{as.matrix}.
#' @return A numeric value.
#' @export
.morie_gr_log_softmax_rows <- function(Z) {
  Z <- as.matrix(Z)
  m <- apply(Z, 1, max)
  Z <- Z - m
  Z - log(rowSums(exp(Z)))
}

# Column-wise softmax (numpy softmax(axis = 0)).
#' Column-wise softmax (numpy softmax(axis = 0))
#'
#' A step of the geron_ml_native implementation. Called by \code{morie_geron_blip}, \code{morie_geron_softmax_function}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Z A matrix; passed to \code{as.matrix}.
#' @return A matrix, from \code{t}.
#' @export
.morie_gr_softmax_cols <- function(Z) {
  t(.morie_al_softmax_rows(t(as.matrix(Z))))
}

#' .morie_gr_layernorm
#'
#' A step of the geron_ml_native implementation. Called by \code{.morie_gr_encoder_block}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param eps Numeric; combined arithmetically in the body. Defaults to \code{1e-05}.
#' @return A numeric value.
#' @export
.morie_gr_layernorm <- function(X, eps = 1e-5) {
  X <- as.matrix(X)
  mu <- rowMeans(X)
  v <- rowMeans((X - mu)^2)
  (X - mu) / sqrt(v + eps)
}

# Integer LCG shared by every reproducible morie.fn module:
#   s <- (1664525 s + 1013904223) mod 2^32
# `.morie_gr_lcg_u` returns the (s + 0.5)/2^32 uniform stream,
# `.morie_gr_lcg_w` the symmetric weight stream 2u - 1 scaled.
#' Integer LCG shared by every reproducible morie.fn module:
#'
#' s <- (1664525 s + 1013904223) mod 2^32 `.morie_gr_lcg_u` returns the
#' (s + 0.5)/2^32 uniform stream, `.morie_gr_lcg_w` the symmetric weight
#' stream 2u - 1 scaled.
#'
#' @param count A count; the body uses it as \code{seq_len(...)}.
#' @param seed See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_gr_lcg_u <- function(count, seed) {
  count <- as.integer(count)
  s <- as.numeric(seed) %% 2^32
  out <- numeric(count)
  if (count > 0L) {
    for (i in seq_len(count)) {
      s <- .morie_al_lcg(s)
      out[i] <- (s + 0.5) / 2^32
    }
  }
  out
}

#' .morie_gr_lcg_w
#'
#' A step of the geron_ml_native implementation. Called by \code{.morie_gr_init}, \code{morie_geron_bert}, \code{morie_geron_roberta}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param count Passed to \code{.morie_gr_lcg_u}.
#' @param seed Passed to \code{.morie_gr_lcg_u}.
#' @param scale Numeric; combined arithmetically in the body. Defaults to \code{0.1}.
#' @return A numeric value.
#' @export
.morie_gr_lcg_w <- function(count, seed, scale = 0.1) {
  (2 * .morie_gr_lcg_u(count, seed) - 1) * scale
}

# numpy `_init(shape, seed, scale)`: a row-major reshape of the weight
# stream, so byrow = TRUE.
#' Numpy `_init(shape, seed, scale)`: a row-major reshape of the weight
#'
#' stream, so byrow = TRUE.
#'
#' @param nrow A count; the body uses it as \code{matrix(...)}.
#' @param ncol A count; the body uses it as \code{matrix(...)}.
#' @param seed Passed to \code{.morie_gr_lcg_w}.
#' @param scale Passed to \code{.morie_gr_lcg_w}. Defaults to \code{0.1}.
#' @return A matrix, from \code{matrix}.
#' @export
.morie_gr_init <- function(nrow, ncol, seed, scale = 0.1) {
  matrix(.morie_gr_lcg_w(nrow * ncol, seed, scale),
    nrow = nrow,
    ncol = ncol, byrow = TRUE
  )
}

# Minimum-norm least squares, matching numpy.linalg.lstsq (SVD based),
# which differs from qr.solve on rank-deficient designs.
#' Minimum-norm least squares, matching numpy.linalg.lstsq (SVD based),
#'
#' which differs from qr.solve on rank-deficient designs.
#'
#' @param A A matrix; passed to \code{ncol}.
#' @param b See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.morie_gr_lstsq <- function(A, b) {
  A <- as.matrix(A)
  if (ncol(A) == 0L) {
    return(numeric(0))
  }
  s <- svd(A)
  tol <- max(dim(A)) * .Machine$double.eps * max(s$d)
  dinv <- ifelse(s$d > tol, 1 / s$d, 0)
  as.numeric(s$v %*% (dinv * (t(s$u) %*% as.numeric(b))))
}

#' .morie_gr_need
#'
#' A step of the geron_ml_native implementation. Called by \code{.morie_gr_attend}, \code{.morie_gr_check_mdp}, \code{.morie_gr_conv2d_valid} and 396 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param cond A flag; the body branches on it.
#' @param msg See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.morie_gr_need <- function(cond, msg) if (!cond) stop(msg, call. = FALSE)

#' .morie_gr_mat
#'
#' A step of the geron_ml_native implementation. Called by \code{.morie_gr_lsa}, \code{.morie_gr_w4b_grid_search}, \code{.morie_w4c_centroid_pair} and 82 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A matrix; passed to \code{as.matrix}.
#' @param name See Usage.
#' @return The value of \code{m}, as built in the body.
#' @export
.morie_gr_mat <- function(x, name) {
  m <- if (is.matrix(x)) x else as.matrix(x)
  storage.mode(m) <- "double"
  .morie_gr_need(all(is.finite(m)), paste0(name, " must be finite."))
  m
}

# numpy array_split(seq_len(m), K): first m %% K parts get one extra.
#' Numpy array_split(seq_len(m), K): first m %% K parts get one extra
#'
#' A step of the geron_ml_native implementation. Called by \code{morie_geron_cross_validation_score}, \code{morie_geron_kfold}, \code{morie_geron_kfold_cv} and 1 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param idx A vector; its length is taken and its elements indexed.
#' @param K A count; the body uses it as \code{seq_len(...)}.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_gr_array_split <- function(idx, K) {
  m <- length(idx)
  base <- m %/% K
  extra <- m %% K
  out <- vector("list", K)
  pos <- 1L
  for (k in seq_len(K)) {
    len <- base + if (k <= extra) 1L else 0L
    out[[k]] <- if (len > 0L) idx[pos:(pos + len - 1L)] else idx[0]
    pos <- pos + len
  }
  out
}

# ---------------------------------------------------------------- optimisers

#' Adam step (Geron Ch 11, morie.fn hmadam)
#'
#' m <- b1 m + (1-b1) g; v <- b2 v + (1-b2) g^2; bias-correct with the
#' 1-based timestep t; step = -eta m_hat / (sqrt(v_hat) + eps).
#'
#' @param grads Numeric gradient vector.
#' @param m,v Moment accumulators; default zeros.
#' @param b1,b2 Decay rates in \[0, 1).
#' @param eta Positive learning rate.
#' @param eps Non-negative floor.
#' @param t 1-based timestep.
#' @param theta Current parameters; default zeros.
#' @return List with `theta`, `step`, `m`, `v`, `m_hat`, `v_hat`.
#' @export
morie_geron_adam <- function(grads, m = NULL, v = NULL, b1 = 0.9, b2 = 0.999,
                             eta = 0.001, eps = 1e-8, t = 1, theta = NULL) {
  g <- as.numeric(grads)
  .morie_gr_need(length(g) > 0L, "geron_adam: grads is empty")
  .morie_gr_need(all(is.finite(g)), "geron_adam: grads contains non-finite values")
  mm <- if (is.null(m)) numeric(length(g)) else as.numeric(m)
  vv <- if (is.null(v)) numeric(length(g)) else as.numeric(v)
  th <- if (is.null(theta)) numeric(length(g)) else as.numeric(theta)
  for (nm in c("m", "v", "theta")) {
    arr <- switch(nm,
      m = mm,
      v = vv,
      theta = th
    )
    .morie_gr_need(
      length(arr) == length(g),
      sprintf(
        "geron_adam: %s length %d but grads length %d",
        nm, length(arr), length(g)
      )
    )
  }
  .morie_gr_need(
    b1 >= 0 && b1 < 1 && b2 >= 0 && b2 < 1,
    "geron_adam: b1 and b2 must lie in [0, 1)"
  )
  .morie_gr_need(
    is.finite(eta) && eta > 0,
    "geron_adam: eta must be a positive finite learning rate"
  )
  .morie_gr_need(eps >= 0, "geron_adam: eps must be non-negative")
  step_t <- as.integer(t)
  .morie_gr_need(step_t >= 1L, "geron_adam: t must be a 1-based timestep >= 1")
  .morie_gr_need(all(vv >= 0), "geron_adam: v must be non-negative")

  m_new <- b1 * mm + (1 - b1) * g
  v_new <- b2 * vv + (1 - b2) * g * g
  m_hat <- m_new / (1 - b1^step_t)
  v_hat <- v_new / (1 - b2^step_t)
  step <- -eta * m_hat / (sqrt(v_hat) + eps)
  theta_next <- th + step
  list(
    theta = theta_next, theta_next = theta_next, step = step,
    m = m_new, v = v_new, m_hat = m_hat, v_hat = v_hat, t = step_t,
    estimate = sqrt(sum(step^2)), n = length(g),
    method = "Adam (bias-corrected momentum + RMSProp)"
  )
}

#' Adam update, theta-first signature (Geron Ch 11, morie.fn gradmo)
#'
#' Same kernel as [morie_geron_adam()]; this mirrors gradmo's argument
#' order and its POSITIVE `step` convention (theta_new = theta - step).
#'
#' @param theta,grad,m,s Equal-length numeric vectors.
#' @param t 1-based step counter.
#' @param eta Positive learning rate.
#' @param b1,b2 Decay rates in \[0, 1).
#' @param eps Positive floor.
#' @return List with `theta_new`, `m_new`, `s_new`, `m_hat`, `s_hat`, `step`.
#' @export
morie_geron_adam_update <- function(theta, grad, m, s, t, eta, b1 = 0.9,
                                    b2 = 0.999, eps = 1e-8) {
  theta <- as.numeric(theta)
  grad <- as.numeric(grad)
  m <- as.numeric(m)
  s <- as.numeric(s)
  for (nm in c("grad", "m", "s")) {
    arr <- switch(nm,
      grad = grad,
      m = m,
      s = s
    )
    .morie_gr_need(
      length(arr) == length(theta),
      sprintf(
        "%s length %d != theta length %d", nm, length(arr),
        length(theta)
      )
    )
  }
  .morie_gr_need(length(theta) > 0L, "theta is empty.")
  .morie_gr_need(
    all(is.finite(theta)) && all(is.finite(grad)),
    "theta and grad must be finite."
  )
  .morie_gr_need(all(s >= 0), "s (second moment) must be non-negative.")
  t <- as.integer(t)
  .morie_gr_need(t >= 1L, "t is the 1-based step counter and must be >= 1.")
  .morie_gr_need(is.finite(eta) && eta > 0, "eta must be a positive finite float.")
  .morie_gr_need(b1 >= 0 && b1 < 1, "b1 must lie in [0, 1).")
  .morie_gr_need(b2 >= 0 && b2 < 1, "b2 must lie in [0, 1).")
  .morie_gr_need(eps > 0, "eps must be positive.")

  m_new <- b1 * m + (1 - b1) * grad
  s_new <- b2 * s + (1 - b2) * grad * grad
  m_hat <- m_new / (1 - b1^t)
  s_hat <- s_new / (1 - b2^t)
  step <- eta * m_hat / (sqrt(s_hat) + eps)
  list(
    theta_new = theta - step, m_new = m_new, s_new = s_new,
    m_hat = m_hat, s_hat = s_hat, step = step, t = t,
    estimate = sqrt(sum(step^2)), n = length(theta),
    method = "Adam optimizer step with bias correction"
  )
}

#' AdamW step with decoupled weight decay (Geron Ch 11, morie.fn hmadmw)
#'
#' The decay is applied to the parameters, not folded into the gradient,
#' so it is NOT rescaled by the adaptive denominator.
#'
#' @param grads Gradient vector.
#' @param m,v Moment accumulators.
#' @param b1,b2 Decay rates.
#' @param eta Learning rate.
#' @param wd Weight decay.
#' @param eps Floor.
#' @param t 1-based timestep.
#' @param theta Parameters.
#' @return List with `theta`, `step`, `adam_step`, `decay_step`, `m`, `v`.
#' @export
morie_geron_adamw <- function(grads, m = NULL, v = NULL, b1 = 0.9, b2 = 0.999,
                              eta = 0.001, wd = 0.01, eps = 1e-8, t = 1,
                              theta = NULL) {
  g <- as.numeric(grads)
  .morie_gr_need(length(g) > 0L, "geron_adamw: grads is empty")
  .morie_gr_need(all(is.finite(g)), "geron_adamw: grads contains non-finite values")
  mm <- if (is.null(m)) numeric(length(g)) else as.numeric(m)
  vv <- if (is.null(v)) numeric(length(g)) else as.numeric(v)
  th <- if (is.null(theta)) numeric(length(g)) else as.numeric(theta)
  .morie_gr_need(
    length(mm) == length(g) && length(vv) == length(g) &&
      length(th) == length(g),
    "geron_adamw: m, v and theta must match grads in shape"
  )
  .morie_gr_need(
    b1 >= 0 && b1 < 1 && b2 >= 0 && b2 < 1,
    "geron_adamw: b1 and b2 must lie in [0, 1)"
  )
  .morie_gr_need(is.finite(eta) && eta > 0, "geron_adamw: eta must be positive")
  .morie_gr_need(wd >= 0, "geron_adamw: wd must be non-negative")
  .morie_gr_need(eps >= 0, "geron_adamw: eps must be non-negative")
  step_t <- as.integer(t)
  .morie_gr_need(step_t >= 1L, "geron_adamw: t must be >= 1")
  .morie_gr_need(all(vv >= 0), "geron_adamw: v must be non-negative")

  m_new <- b1 * mm + (1 - b1) * g
  v_new <- b2 * vv + (1 - b2) * g * g
  m_hat <- m_new / (1 - b1^step_t)
  v_hat <- v_new / (1 - b2^step_t)
  adam_step <- -eta * m_hat / (sqrt(v_hat) + eps)
  decay_step <- -eta * wd * th
  step <- adam_step + decay_step
  list(
    theta = th + step, theta_next = th + step, step = step,
    adam_step = adam_step, decay_step = decay_step, m = m_new, v = v_new,
    m_hat = m_hat, v_hat = v_hat, t = step_t,
    estimate = sqrt(sum(step^2)), n = length(g),
    method = "AdamW (Adam with decoupled weight decay)"
  )
}

#' AdaMax step (Geron Ch 11, morie.fn hmadmx)
#'
#' u <- max(b2 u, |g|); only m is bias-corrected, because an
#' exponentially weighted max needs no correction.
#'
#' @param grads Gradient vector.
#' @param m First moment.
#' @param u Infinity-norm.
#' @param b1,b2 Decay rates.
#' @param eta Learning rate.
#' @param t Timestep.
#' @param theta Parameters.
#' @return List with `theta`, `step`, `m`, `u`, `m_hat`.
#' @export
morie_geron_adamax <- function(grads, m = NULL, u = NULL, b1 = 0.9, b2 = 0.999,
                               eta = 0.002, t = 1, theta = NULL) {
  g <- as.numeric(grads)
  .morie_gr_need(length(g) > 0L, "geron_adamax: grads is empty")
  .morie_gr_need(all(is.finite(g)), "geron_adamax: grads contains non-finite values")
  mm <- if (is.null(m)) numeric(length(g)) else as.numeric(m)
  uu <- if (is.null(u)) numeric(length(g)) else as.numeric(u)
  th <- if (is.null(theta)) numeric(length(g)) else as.numeric(theta)
  .morie_gr_need(
    length(mm) == length(g) && length(uu) == length(g) &&
      length(th) == length(g),
    "geron_adamax: m, u and theta must match grads in shape"
  )
  .morie_gr_need(all(uu >= 0), "geron_adamax: u must be non-negative")
  .morie_gr_need(
    b1 >= 0 && b1 < 1 && b2 >= 0 && b2 < 1,
    "geron_adamax: b1 and b2 must lie in [0, 1)"
  )
  .morie_gr_need(is.finite(eta) && eta > 0, "geron_adamax: eta must be positive")
  step_t <- as.integer(t)
  .morie_gr_need(step_t >= 1L, "geron_adamax: t must be >= 1")

  m_new <- b1 * mm + (1 - b1) * g
  u_new <- pmax(b2 * uu, abs(g))
  m_hat <- m_new / (1 - b1^step_t)
  step <- ifelse(u_new == 0, 0, -eta * m_hat / ifelse(u_new == 0, 1, u_new))
  list(
    theta = th + step, theta_next = th + step, step = step, m = m_new,
    u = u_new, m_hat = m_hat, t = step_t, estimate = sqrt(sum(step^2)),
    n = length(g), method = "AdaMax (Adam with an L-infinity second moment)"
  )
}

#' AdaGrad step, negative-step convention (Geron Ch 11, morie.fn hmadgr)
#'
#' s <- s + g^2; step = -eta g / (sqrt(s) + eps).
#'
#' @param grads Gradient vector.
#' @param s Squared-gradient accumulator.
#' @param eta Learning rate.
#' @param eps Floor.
#' @param theta Parameters.
#' @return List with `theta`, `step`, `s`, `effective_lr`.
#' @export
morie_geron_adagrad <- function(grads, s = NULL, eta = 0.01, eps = 1e-10,
                                theta = NULL) {
  g <- as.numeric(grads)
  .morie_gr_need(length(g) > 0L, "geron_adagrad: grads is empty")
  .morie_gr_need(all(is.finite(g)), "geron_adagrad: grads contains non-finite values")
  ss <- if (is.null(s)) numeric(length(g)) else as.numeric(s)
  th <- if (is.null(theta)) numeric(length(g)) else as.numeric(theta)
  .morie_gr_need(length(ss) == length(g), "geron_adagrad: s shape mismatch")
  .morie_gr_need(length(th) == length(g), "geron_adagrad: theta shape mismatch")
  .morie_gr_need(all(ss >= 0), "geron_adagrad: s must be non-negative")
  .morie_gr_need(is.finite(eta) && eta > 0, "geron_adagrad: eta must be positive")
  .morie_gr_need(eps >= 0, "geron_adagrad: eps must be non-negative")

  s_new <- ss + g * g
  denom <- sqrt(s_new) + eps
  .morie_gr_need(
    !any(denom == 0),
    "geron_adagrad: zero accumulator with eps=0 leaves the per-parameter scale undefined"
  )
  step <- -eta * g / denom
  list(
    theta = th + step, theta_next = th + step, step = step, s = s_new,
    effective_lr = eta / denom, estimate = sqrt(sum(step^2)),
    n = length(g),
    method = "AdaGrad (per-parameter rates from accumulated squared gradients)"
  )
}

#' AdaGrad update, theta-first signature (Geron Ch 11, morie.fn grada2)
#'
#' Same kernel as [morie_geron_adagrad()], with grada2's positive-step
#' convention: theta_new = theta - step, step = effective_lr * g.
#'
#' @param theta,grad,s Equal-length numeric vectors.
#' @param eta Positive learning rate.
#' @param eps Non-negative floor.
#' @return List with `theta_new`, `s_new`, `effective_lr`, `step`, `step_norm`.
#' @export
morie_geron_adagrad_update <- function(theta, grad, s, eta, eps = 1e-10) {
  theta <- as.numeric(theta)
  grad <- as.numeric(grad)
  s <- as.numeric(s)
  .morie_gr_need(length(theta) == length(grad), "theta shape != grad shape.")
  .morie_gr_need(length(theta) == length(s), "theta shape != s shape.")
  .morie_gr_need(length(theta) > 0L, "theta is empty.")
  .morie_gr_need(
    all(is.finite(theta)) && all(is.finite(grad)),
    "theta and grad must be finite."
  )
  .morie_gr_need(all(s >= 0), "s (accumulated squared gradients) must be non-negative.")
  .morie_gr_need(is.finite(eta) && eta > 0, "eta must be a positive finite float.")
  .morie_gr_need(eps >= 0, "eps must be non-negative.")
  s_new <- s + grad * grad
  denom <- sqrt(s_new) + eps
  .morie_gr_need(
    !any(denom == 0),
    "zero denominator: a coordinate has zero accumulated gradient and eps=0; pass eps > 0."
  )
  eff <- eta / denom
  step <- eff * grad
  list(
    theta_new = theta - step, s_new = s_new, effective_lr = eff,
    step = step, step_norm = sqrt(sum(step^2)),
    estimate = sqrt(sum(step^2)), n = length(theta),
    method = "AdaGrad parameter update"
  )
}

#' 1cycle learning-rate and momentum schedule (Geron Ch 11, morie.fn gr1cy)
#'
#' Peak step is `(T - 1) %/% 2`; the LR is a linear interpolation over
#' knots (0, peak, T-1) with values (eta_min, eta_max, eta_min) and the
#' momentum mirrors it. T = 2 collapses the peak onto step 0, which is
#' the two-knot branch.
#'
#' @param eta_min,eta_max Learning-rate bounds, 0 < eta_min < eta_max.
#' @param t Reporting step, 0-based, 0 <= t < T.
#' @param T Total steps, at least 2.
#' @param mom_max,mom_min Momentum bounds, 0 <= mom_min < mom_max < 1.
#' @return List with `lr_schedule`, `momentum_schedule`, `peak_step`,
#'   `lr_at_t`, `momentum_at_t`.
#' @export
morie_geron_1cycle_schedule <- function(eta_min, eta_max, t, T, mom_max = 0.95,
                                        mom_min = 0.85) {
  eta_min <- as.numeric(eta_min)
  eta_max <- as.numeric(eta_max)
  .morie_gr_need(
    is.finite(eta_min) && is.finite(eta_max),
    "eta_min and eta_max must be finite."
  )
  .morie_gr_need(eta_min > 0, "eta_min must be positive.")
  .morie_gr_need(eta_max > eta_min, "eta_max must exceed eta_min.")
  T <- as.integer(T)
  .morie_gr_need(T >= 2L, "T must be at least 2 steps.")
  t <- as.integer(t)
  .morie_gr_need(t >= 0L && t < T, "t must satisfy 0 <= t < T.")
  .morie_gr_need(
    mom_min >= 0 && mom_min < mom_max && mom_max < 1,
    "momentum bounds must satisfy 0 <= mom_min < mom_max < 1."
  )
  steps <- seq.int(0L, T - 1L)
  peak <- (T - 1L) %/% 2L
  if (peak == 0L) {
    knots <- c(0, T - 1)
    lr <- stats::approx(knots, c(eta_min, eta_max), xout = steps, rule = 2)$y
    mom <- stats::approx(knots, c(mom_max, mom_min), xout = steps, rule = 2)$y
  } else {
    knots <- c(0, peak, T - 1)
    lr <- stats::approx(knots, c(eta_min, eta_max, eta_min),
      xout = steps,
      rule = 2
    )$y
    mom <- stats::approx(knots, c(mom_max, mom_min, mom_max),
      xout = steps,
      rule = 2
    )$y
  }
  list(
    lr_schedule = lr, momentum_schedule = mom, peak_step = as.integer(peak),
    lr_at_t = lr[t + 1L], momentum_at_t = mom[t + 1L],
    eta_min = eta_min, eta_max = eta_max, estimate = lr[t + 1L], n = T,
    method = "1cycle learning-rate schedule (Smith 2018)"
  )
}

#' Batch-size heuristic (Geron Ch 9, morie.fn hmbsz)
#'
#' Largest power of two in `{32, 64, 128, 256, 512}` not exceeding
#' n_train / steps_per_epoch_target (and memory_limit if supplied),
#' flooring at min(32, n_train) when nothing is feasible.
#'
#' @param n_train Training-set size, >= 1.
#' @param steps_per_epoch_target Minimum updates per epoch, >= 1.
#' @param memory_limit Optional hard cap.
#' @return List with `batch_size`, `steps_per_epoch`, `candidates`, `cap`.
#' @export
morie_geron_batch_size_heuristic <- function(n_train,
                                             steps_per_epoch_target = 10,
                                             memory_limit = NULL) {
  n <- as.integer(n_train)
  .morie_gr_need(n >= 1L, "geron_batch_size_heuristic: n_train must be >= 1")
  target <- as.integer(steps_per_epoch_target)
  .morie_gr_need(
    target >= 1L,
    "geron_batch_size_heuristic: steps_per_epoch_target must be >= 1"
  )
  cand <- c(32, 64, 128, 256, 512)
  cap <- n / target
  if (!is.null(memory_limit)) {
    ml <- as.integer(memory_limit)
    .morie_gr_need(ml >= 1L, "geron_batch_size_heuristic: memory_limit must be >= 1")
    cap <- min(cap, as.numeric(ml))
  }
  feasible <- cand[cand <= cap]
  batch <- if (length(feasible)) max(feasible) else min(32, n)
  steps <- as.integer(ceiling(n / batch))
  list(
    batch_size = batch, steps_per_epoch = steps, candidates = cand,
    cap = cap, clamped = length(feasible) == 0L, estimate = batch, n = n,
    method = "Batch size heuristic (largest power of two in [32, 512] under the step-count cap)"
  )
}

# --------------------------------------------------- criteria and metrics

#' Akaike information criterion, vectorised (Geron Ch 8, morie.fn hmaic)
#'
#' AIC = -2 log L + 2k, with AICc when `n` is given and n - k - 1 > 0
#' (entries where it is not stay NA, as Python leaves them NaN).
#' `best_index` is 0-based, as in Python.
#'
#' @param log_lik Scalar or vector of maximised log-likelihoods.
#' @param k Scalar or vector of free-parameter counts.
#' @param n Optional sample size for AICc.
#' @return List with `aic`, `aicc`, `delta`, `weights`, `best_index`.
#' @export
morie_geron_aic <- function(log_lik, k, n = NULL) {
  ll <- as.numeric(log_lik)
  kk <- as.numeric(k)
  .morie_gr_need(length(ll) > 0L, "geron_aic: log_lik is empty")
  if (length(kk) == 1L && length(ll) > 1L) kk <- rep(kk, length(ll))
  if (length(ll) == 1L && length(kk) > 1L) ll <- rep(ll, length(kk))
  .morie_gr_need(
    length(ll) == length(kk),
    "geron_aic: log_lik shape does not match k shape"
  )
  .morie_gr_need(all(is.finite(ll)), "geron_aic: log_lik contains non-finite values")
  .morie_gr_need(
    all(kk >= 0) && all(kk == floor(kk)),
    "geron_aic: k must be a non-negative integer count of free parameters"
  )
  aic <- -2 * ll + 2 * kk
  aicc <- rep(NA_real_, length(aic))
  if (!is.null(n)) {
    nn <- as.numeric(n)
    .morie_gr_need(nn > 0, "geron_aic: n must be a positive sample size")
    denom <- nn - kk - 1
    ok <- denom > 0
    aicc[ok] <- aic[ok] + (2 * kk[ok] * (kk[ok] + 1)) / denom[ok]
  }
  delta <- aic - min(aic)
  w <- exp(-0.5 * delta)
  weights <- w / sum(w)
  best <- which.min(aic) - 1L
  scalar <- length(ll) == 1L
  list(
    aic = if (scalar) aic[1] else aic,
    aicc = if (scalar) aicc[1] else aicc,
    delta = if (scalar) delta[1] else delta,
    weights = if (scalar) weights[1] else weights,
    best_index = best, k = if (scalar) kk[1] else kk,
    estimate = aic[best + 1L], n = length(ll),
    method = "Akaike information criterion (AIC = -2 log L + 2k)"
  )
}

#' Bayesian information criterion, vectorised (Geron Ch 8, morie.fn hmbic)
#'
#' BIC = -2 log L + k log n. `best_index` is 0-based.
#'
#' @param log_lik Scalar or vector of log-likelihoods.
#' @param k Scalar or vector of parameter counts.
#' @param n Sample size, >= 1.
#' @return List with `bic`, `delta`, `weights`, `best_index`, `penalty`.
#' @export
morie_geron_bic <- function(log_lik, k, n) {
  ll <- as.numeric(log_lik)
  kk <- as.numeric(k)
  .morie_gr_need(length(ll) > 0L, "geron_bic: log_lik is empty")
  if (length(kk) == 1L && length(ll) > 1L) kk <- rep(kk, length(ll))
  if (length(ll) == 1L && length(kk) > 1L) ll <- rep(ll, length(kk))
  .morie_gr_need(
    length(ll) == length(kk),
    "geron_bic: log_lik shape does not match k shape"
  )
  .morie_gr_need(all(is.finite(ll)), "geron_bic: log_lik contains non-finite values")
  .morie_gr_need(
    all(kk >= 0) && all(kk == floor(kk)),
    "geron_bic: k must be a non-negative integer count"
  )
  nn <- as.numeric(n)
  .morie_gr_need(nn >= 1, "geron_bic: n must be >= 1")
  bic <- -2 * ll + kk * log(nn)
  delta <- bic - min(bic)
  w <- exp(-0.5 * delta)
  weights <- w / sum(w)
  best <- which.min(bic) - 1L
  scalar <- length(ll) == 1L
  list(
    bic = if (scalar) bic[1] else bic,
    delta = if (scalar) delta[1] else delta,
    weights = if (scalar) weights[1] else weights,
    best_index = best, penalty = log(nn), estimate = bic[best + 1L],
    n = as.integer(nn),
    method = "Bayesian information criterion (BIC = -2 log L + k log n)"
  )
}

#' Free-parameter count of a GMM (Geron Ch 8, morie.fn graic)
#'
#' (k - 1) weights + k*d means + k * per-covariance, with per-covariance
#' d(d+1)/2 (full), d (diag) or 1 (spherical).
#'
#' @param k Components.
#' @param d Dimension.
#' @param covariance_type One of "full", "diag", "spherical".
#' @return Integer parameter count.
#' @export
morie_geron_gmm_n_params <- function(k, d, covariance_type = "full") {
  k <- as.integer(k)
  d <- as.integer(d)
  .morie_gr_need(k >= 1L && d >= 1L, "k and d must be >= 1.")
  per <- switch(covariance_type,
    full = (d * (d + 1L)) %/% 2L,
    diag = d,
    spherical = 1L,
    stop("covariance_type must be one of diag, full, spherical.",
      call. = FALSE
    )
  )
  as.integer((k - 1L) + k * d + k * per)
}

#' AIC for GMM model selection (Geron Ch 8, morie.fn graic)
#'
#' AIC = 2p - 2 log L.
#'
#' @param log_likelihood Finite scalar.
#' @param n_params Non-negative count.
#' @return List with `aic`, `log_likelihood`, `n_params`, `penalty`.
#' @export
morie_geron_aic_gmm <- function(log_likelihood, n_params) {
  ll <- as.numeric(log_likelihood)
  .morie_gr_need(is.finite(ll), "log_likelihood must be finite.")
  p <- as.integer(n_params)
  .morie_gr_need(p >= 0L, "n_params must be non-negative.")
  penalty <- 2 * p
  list(
    aic = penalty - 2 * ll, log_likelihood = ll, n_params = p,
    penalty = penalty, estimate = penalty - 2 * ll, n = p,
    method = "Akaike information criterion"
  )
}

#' BIC for GMM model selection (Geron Ch 8, morie.fn grbic)
#'
#' BIC = p log n - 2 log L; `stricter_than_aic` is log n > 2.
#'
#' @param log_likelihood Finite scalar.
#' @param n Observations, >= 1.
#' @param n_params Non-negative count.
#' @return List with `bic`, `penalty`, `penalty_per_param`, `stricter_than_aic`.
#' @export
morie_geron_bic_gmm <- function(log_likelihood, n, n_params) {
  ll <- as.numeric(log_likelihood)
  .morie_gr_need(is.finite(ll), "log_likelihood must be finite.")
  n <- as.integer(n)
  .morie_gr_need(n >= 1L, "n must be at least 1 observation.")
  p <- as.integer(n_params)
  .morie_gr_need(p >= 0L, "n_params must be non-negative.")
  per <- log(n)
  penalty <- per * p
  list(
    bic = penalty - 2 * ll, log_likelihood = ll, n_params = p,
    penalty = penalty, penalty_per_param = per,
    stricter_than_aic = per > 2, estimate = penalty - 2 * ll, n = n,
    method = "Bayesian information criterion"
  )
}

#' ROC AUC via the Mann-Whitney identity (Geron Ch 3, morie.fn hmauc)
#'
#' Mid-ranks credit ties 0.5. ROC vertices are returned in descending
#' threshold order, starting from (0, 0) with threshold Inf.
#'
#' @param y_true Binary labels.
#' @param scores Decision scores.
#' @param pos_label Positive-class label.
#' @return List with `auc`, `fpr`, `tpr`, `thresholds`, `n_pos`, `n_neg`.
#' @export
morie_geron_auc_roc <- function(y_true, scores, pos_label = 1) {
  y <- as.vector(y_true)
  s <- as.numeric(scores)
  .morie_gr_need(length(y) > 0L, "geron_auc_roc: y_true is empty")
  .morie_gr_need(
    length(y) == length(s),
    "geron_auc_roc: y_true and scores lengths differ"
  )
  .morie_gr_need(all(is.finite(s)), "geron_auc_roc: scores contain non-finite values")
  pos <- y == pos_label
  n_pos <- sum(pos)
  n_neg <- length(y) - n_pos
  .morie_gr_need(
    n_pos > 0L && n_neg > 0L,
    "geron_auc_roc: ROC AUC needs both classes present"
  )
  ranks <- rank(s, ties.method = "average")
  auc <- (sum(ranks[pos]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)

  desc <- order(-s, method = "radix")
  sd_ <- s[desc]
  pd <- pos[desc]
  tp <- 0
  fp <- 0
  fpr <- 0
  tpr <- 0
  thr <- Inf
  k <- 1L
  N <- length(s)
  while (k <= N) {
    j <- k
    while (j + 1L <= N && sd_[j + 1L] == sd_[k]) j <- j + 1L
    tp <- tp + sum(pd[k:j])
    fp <- fp + sum(!pd[k:j])
    fpr <- c(fpr, fp / n_neg)
    tpr <- c(tpr, tp / n_pos)
    thr <- c(thr, sd_[k])
    k <- j + 1L
  }
  list(
    auc = auc, fpr = fpr, tpr = tpr, thresholds = thr,
    n_pos = as.integer(n_pos), n_neg = as.integer(n_neg),
    estimate = auc, n = length(y),
    method = "ROC AUC via the Mann-Whitney identity (ties credited 0.5)"
  )
}

#' ROC curve and AUC by the trapezoid rule (Geron Ch 3, morie.fn grauc)
#'
#' Only the last index of each run of tied scores becomes a vertex, so
#' the trapezoid sum equals the Mann-Whitney statistic. Same numbers as
#' [morie_geron_auc_roc()], different construction -- the two are each
#' other's independent check.
#'
#' @param y_true Binary labels.
#' @param y_scores Decision scores.
#' @param pos_label Positive-class label.
#' @return List with `auc`, `fpr`, `tpr`, `thresholds`, `n_pos`, `n_neg`.
#' @export
morie_geron_auc_roc_trapezoid <- function(y_true, y_scores, pos_label = 1) {
  y <- as.vector(y_true)
  s <- as.numeric(y_scores)
  .morie_gr_need(
    length(y) == length(s),
    "y_true and y_scores must have equal length."
  )
  .morie_gr_need(length(y) > 0L, "no observations supplied.")
  .morie_gr_need(all(is.finite(s)), "y_scores contains non-finite values.")
  pos <- y == pos_label
  n_pos <- sum(pos)
  n_neg <- length(y) - n_pos
  .morie_gr_need(n_pos > 0L && n_neg > 0L, "ROC needs both classes present.")
  ord <- order(-s, method = "radix")
  ss <- s[ord]
  p <- as.numeric(pos[ord])
  tp <- cumsum(p)
  fp <- cumsum(1 - p)
  keep <- c(diff(ss) != 0, TRUE)
  tp <- c(0, tp[keep])
  fp <- c(0, fp[keep])
  thresholds <- c(Inf, ss[keep])
  tpr <- tp / n_pos
  fpr <- fp / n_neg
  auc <- sum(diff(fpr) * (utils::head(tpr, -1) + utils::tail(tpr, -1)) / 2)
  list(
    auc = auc, fpr = fpr, tpr = tpr, thresholds = thresholds,
    n_pos = as.integer(n_pos), n_neg = as.integer(n_neg),
    estimate = auc, n = length(y),
    method = "Area under the ROC curve (trapezoid rule)"
  )
}

#' Confusion matrix with per-class precision/recall/F1 (Geron Ch 3, grcfm)
#'
#' Rows are truth, columns prediction. Labels are 0-based class indices,
#' as in Python; the returned matrix is K x K in that same order.
#'
#' @param y_true,y_pred Integer class indices.
#' @param n_classes Optional class count.
#' @return List with `matrix`, `accuracy`, `precision`, `recall`, `f1`,
#'   `support`, `macro_f1`.
#' @export
morie_geron_confusion_matrix <- function(y_true, y_pred, n_classes = NULL) {
  yt <- as.vector(y_true)
  yp <- as.vector(y_pred)
  .morie_gr_need(
    length(yt) == length(yp),
    "y_true and y_pred must have equal length."
  )
  .morie_gr_need(length(yt) > 0L, "no observations supplied.")
  .morie_gr_need(
    all(yt == floor(yt)) && all(yp == floor(yp)),
    "labels must be whole numbers used as class indices."
  )
  yt <- as.integer(yt)
  yp <- as.integer(yp)
  .morie_gr_need(
    min(yt) >= 0L && min(yp) >= 0L,
    "class indices must be non-negative."
  )
  K <- if (is.null(n_classes)) max(max(yt), max(yp)) + 1L else as.integer(n_classes)
  .morie_gr_need(K >= 1L, "n_classes must be at least 1.")
  .morie_gr_need(
    max(yt) < K && max(yp) < K,
    "labels reach beyond n_classes."
  )
  cm <- matrix(0L, K, K)
  for (i in seq_along(yt)) {
    cm[yt[i] + 1L, yp[i] + 1L] <- cm[yt[i] + 1L, yp[i] + 1L] + 1L
  }
  tp <- as.numeric(diag(cm))
  pred_tot <- as.numeric(colSums(cm))
  true_tot <- as.numeric(rowSums(cm))
  prec <- ifelse(pred_tot > 0, tp / pmax(pred_tot, 1), NA_real_)
  rec <- ifelse(true_tot > 0, tp / pmax(true_tot, 1), NA_real_)
  denom <- prec + rec
  f1 <- ifelse(!is.na(denom) & denom > 0, 2 * prec * rec / pmax(denom, 1e-300), 0)
  acc <- sum(tp) / sum(cm)
  macro <- if (any(is.finite(f1))) mean(f1[is.finite(f1)]) else NA_real_
  list(
    matrix = cm, accuracy = acc, precision = prec, recall = rec, f1 = f1,
    support = as.integer(true_tot), macro_f1 = macro, n_classes = K,
    estimate = acc, n = length(yt), method = "Confusion matrix"
  )
}

#' CART split cost at a node (Geron Ch 5 Eq 5-2, morie.fn grcart)
#'
#' J = (m_L/m) G_L + (m_R/m) G_R, with gini, entropy (log base 2) or mse
#' impurity, plus the impurity decrease against the parent.
#'
#' @param X Feature matrix.
#' @param y Labels or targets.
#' @param feature 0-based column index, matching Python.
#' @param threshold Split point; `X\[, feature\] <= threshold` goes left.
#' @param criterion One of "gini", "entropy", "mse".
#' @return List with `cost`, `impurity_left`, `impurity_right`,
#'   `impurity_parent`, `impurity_decrease`, `n_left`, `n_right`.
#' @export
morie_geron_cart_split_cost <- function(X, y, feature, threshold,
                                        criterion = "gini") {
  X <- if (is.matrix(X)) X else as.matrix(X)
  .morie_gr_need(length(X) > 0L, "X must be a non-empty 2-D (m, n) array.")
  y <- as.vector(y)
  .morie_gr_need(length(y) == nrow(X), "y length does not match X rows.")
  feature <- as.integer(feature)
  .morie_gr_need(
    feature >= 0L && feature < ncol(X),
    "feature index out of range."
  )
  threshold <- as.numeric(threshold)
  .morie_gr_need(is.finite(threshold), "threshold must be finite.")
  .morie_gr_need(
    criterion %in% c("gini", "entropy", "mse"),
    "criterion must be one of entropy, gini, mse."
  )
  col <- as.numeric(X[, feature + 1L])
  .morie_gr_need(all(is.finite(col)), "column of X contains non-finite values.")
  imp <- switch(criterion,
    gini = function(v) {
      if (!length(v)) {
        0
      } else {
        p <- as.numeric(table(v)) / length(v)
        1 - sum(p * p)
      }
    },
    entropy = function(v) {
      if (!length(v)) {
        0
      } else {
        p <- as.numeric(table(v)) / length(v)
        -sum(p * log2(p))
      }
    },
    mse = function(v) {
      if (!length(v)) {
        0
      } else {
        v <- as.numeric(v)
        mean((v - mean(v))^2)
      }
    }
  )
  if (criterion == "mse") {
    y <- as.numeric(y)
    .morie_gr_need(all(is.finite(y)), "y contains non-finite values.")
  }
  m <- length(y)
  left <- col <= threshold
  yl <- y[left]
  yr <- y[!left]
  ml <- length(yl)
  mr <- length(yr)
  gl <- imp(yl)
  gr <- imp(yr)
  cost <- (ml / m) * gl + (mr / m) * gr
  parent <- imp(y)
  list(
    cost = cost, impurity_left = gl, impurity_right = gr,
    impurity_parent = parent, impurity_decrease = parent - cost,
    n_left = ml, n_right = mr, criterion = criterion,
    threshold = threshold, feature = feature, estimate = cost, n = m,
    method = "CART split cost"
  )
}

#' Binary classification by thresholding a logistic score (Geron Ch 3, hmbin)
#'
#' p_hat = sigmoid(X theta) computed branch-free (exp only on <= 0), then
#' y_pred = 1\[p_hat >= threshold\].
#'
#' @param X Design matrix (bring your own intercept column).
#' @param theta Coefficients.
#' @param threshold Cut in \[0, 1\].
#' @param y_true Optional 0/1 labels for the confusion summary.
#' @return List with `y_pred`, `p_hat`, `logit` and, when `y_true` is
#'   given, `tp`, `fp`, `fn`, `tn`, `accuracy`, `precision`, `recall`, `f1`.
#' @export
morie_geron_binary_classification <- function(X, theta, threshold = 0.5,
                                              y_true = NULL) {
  Xm <- if (is.matrix(X)) X else matrix(as.numeric(X), nrow = 1)
  storage.mode(Xm) <- "double"
  th <- as.numeric(theta)
  .morie_gr_need(nrow(Xm) > 0L, "geron_binary_classification: X has no rows")
  .morie_gr_need(
    ncol(Xm) == length(th),
    "geron_binary_classification: X columns != theta entries"
  )
  t_ <- as.numeric(threshold)
  .morie_gr_need(
    t_ >= 0 && t_ <= 1,
    "geron_binary_classification: threshold must lie in [0, 1]"
  )
  z <- as.numeric(Xm %*% th)
  ez <- exp(-abs(z))
  p <- ifelse(z >= 0, 1 / (1 + ez), ez / (1 + ez))
  y_pred <- as.integer(p >= t_)
  out <- list(
    y_pred = y_pred, p_hat = p, logit = z, threshold = t_,
    tp = NULL, fp = NULL, fn = NULL, tn = NULL, accuracy = NULL,
    precision = NULL, recall = NULL, f1 = NULL,
    estimate = mean(y_pred), n = nrow(Xm),
    method = "Binary classification by thresholding the logistic score"
  )
  if (!is.null(y_true)) {
    yt <- as.integer(as.vector(y_true))
    .morie_gr_need(
      length(yt) == nrow(Xm),
      "geron_binary_classification: y_true length != X rows"
    )
    .morie_gr_need(
      all(yt %in% c(0L, 1L)),
      "geron_binary_classification: y_true must contain only 0 and 1"
    )
    tp <- sum(y_pred == 1L & yt == 1L)
    fp <- sum(y_pred == 1L & yt == 0L)
    fn <- sum(y_pred == 0L & yt == 1L)
    tn <- sum(y_pred == 0L & yt == 0L)
    prec <- if ((tp + fp) > 0) tp / (tp + fp) else 0
    rec <- if ((tp + fn) > 0) tp / (tp + fn) else 0
    out$tp <- tp
    out$fp <- fp
    out$fn <- fn
    out$tn <- tn
    out$accuracy <- (tp + tn) / length(yt)
    out$precision <- prec
    out$recall <- rec
    out$f1 <- if ((prec + rec) > 0) 2 * prec * rec / (prec + rec) else 0
  }
  out
}

#' Bias-variance decomposition (Geron Ch 1, morie.fn hmbv)
#'
#' Variance across models is the POPULATION variance (ddof = 0); the
#' identity only closes with the biased estimator.
#'
#' @param preds Matrix (n_models, n_points).
#' @param y Targets.
#' @param f_true Optional noise-free values.
#' @return List with `bias2`, `variance`, `noise`, `mse`, `mean_pred`.
#' @export
morie_geron_bias_variance_tradeoff <- function(preds, y, f_true = NULL) {
  P <- if (is.matrix(preds)) preds else matrix(as.numeric(preds), nrow = 1)
  storage.mode(P) <- "double"
  yv <- as.numeric(y)
  .morie_gr_need(length(P) > 0L, "geron_bias_variance_tradeoff: preds is empty")
  .morie_gr_need(
    ncol(P) == length(yv),
    "geron_bias_variance_tradeoff: preds points != y entries"
  )
  .morie_gr_need(
    all(is.finite(P)) && all(is.finite(yv)),
    "geron_bias_variance_tradeoff: preds and y must be finite"
  )
  mean_pred <- colMeans(P)
  var_point <- apply(P, 2, .morie_gr_pvar)
  variance <- mean(var_point)
  mse <- mean((P - matrix(yv, nrow(P), ncol(P), byrow = TRUE))^2)
  target <- if (is.null(f_true)) yv else as.numeric(f_true)
  .morie_gr_need(
    length(target) == length(yv),
    "geron_bias_variance_tradeoff: f_true length != y"
  )
  bias2 <- mean((mean_pred - target)^2)
  noise <- if (is.null(f_true)) mse - bias2 - variance else mean((yv - target)^2)
  list(
    bias2 = bias2, variance = variance, noise = noise, mse = mse,
    mean_pred = mean_pred, var_point = var_point, n_models = nrow(P),
    estimate = mse, n = length(yv),
    method = "Bias-variance decomposition E[err] = bias^2 + variance + noise"
  )
}

#' Batch (offline) learning by closed-form least squares (Geron Ch 1, hmbat)
#'
#' Ridge leaves the intercept unpenalised. The returned `predict` closure
#' is frozen: new data needs a full retrain, which is what "offline" means.
#'
#' @param X Design matrix.
#' @param y Targets.
#' @param fit_intercept Prepend a ones column.
#' @param ridge Non-negative L2 penalty.
#' @return List with `theta`, `predict`, `fitted`, `residuals`, `train_mse`, `r2`.
#' @export
morie_geron_batch_learning <- function(X, y, fit_intercept = FALSE, ridge = 0) {
  Xm <- if (is.matrix(X)) X else as.matrix(X)
  storage.mode(Xm) <- "double"
  yv <- as.numeric(y)
  .morie_gr_need(nrow(Xm) > 0L, "geron_batch_learning: training set is empty")
  .morie_gr_need(
    length(yv) == nrow(Xm),
    "geron_batch_learning: X rows != y entries"
  )
  .morie_gr_need(
    all(is.finite(Xm)) && all(is.finite(yv)),
    "geron_batch_learning: X and y must be finite"
  )
  lam <- as.numeric(ridge)
  .morie_gr_need(lam >= 0, "geron_batch_learning: ridge must be non-negative")
  D <- if (fit_intercept) cbind(1, Xm) else Xm
  k <- ncol(D)
  theta <- if (lam == 0) {
    .morie_gr_lstsq(D, yv)
  } else {
    P <- diag(lam, k)
    if (fit_intercept) P[1, 1] <- 0
    as.numeric(solve(crossprod(D) + P, crossprod(D, yv)))
  }
  fitted <- as.numeric(D %*% theta)
  resid <- yv - fitted
  train_mse <- mean(resid^2)
  tss <- sum((yv - mean(yv))^2)
  r2 <- if (tss > 0) 1 - sum(resid^2) / tss else NA_real_
  nfeat <- ncol(Xm)
  predict_fn <- function(Xnew) {
    A <- if (is.matrix(Xnew)) Xnew else matrix(as.numeric(Xnew), nrow = 1)
    storage.mode(A) <- "double"
    .morie_gr_need(ncol(A) == nfeat, "predict: feature count mismatch")
    if (fit_intercept) A <- cbind(1, A)
    as.numeric(A %*% theta)
  }
  list(
    theta = theta, predict = predict_fn, fitted = fitted, residuals = resid,
    train_mse = train_mse, r2 = r2, estimate = train_mse, n = nrow(Xm),
    method = "Batch learning: closed-form minimisation of the full-dataset squared-error loss"
  )
}

#' MSE gradient of linear regression (Geron Ch 4, morie.fn hmbgdg)
#'
#' grad J = (2/m) X^T (X theta - y); with `eta` one descent step too.
#'
#' @param X Design matrix.
#' @param y Targets.
#' @param theta Parameters.
#' @param eta Optional learning rate.
#' @return List with `gradient`, `cost`, `residuals`, `theta_next`, `grad_norm`.
#' @export
morie_geron_batch_gd_grad <- function(X, y, theta, eta = NULL) {
  Xm <- if (is.matrix(X)) X else as.matrix(X)
  storage.mode(Xm) <- "double"
  yv <- as.numeric(y)
  th <- as.numeric(theta)
  m <- nrow(Xm)
  k <- ncol(Xm)
  .morie_gr_need(m > 0L, "geron_batch_gd_grad: X has no rows")
  .morie_gr_need(length(yv) == m, "geron_batch_gd_grad: X rows != y entries")
  .morie_gr_need(length(th) == k, "geron_batch_gd_grad: X columns != theta entries")
  .morie_gr_need(
    all(is.finite(Xm)) && all(is.finite(yv)) && all(is.finite(th)),
    "geron_batch_gd_grad: X, y and theta must all be finite"
  )
  resid <- as.numeric(Xm %*% th) - yv
  grad <- (2 / m) * as.numeric(crossprod(Xm, resid))
  cost <- sum(resid * resid) / m
  theta_next <- NULL
  if (!is.null(eta)) {
    e <- as.numeric(eta)
    .morie_gr_need(
      is.finite(e) && e > 0,
      "geron_batch_gd_grad: eta must be a positive finite learning rate"
    )
    theta_next <- th - e * grad
  }
  list(
    gradient = grad, cost = cost, residuals = resid, theta = th,
    theta_next = theta_next, grad_norm = sqrt(sum(grad^2)),
    estimate = cost, n = m,
    method = "Gradient of linear-regression MSE cost, (2/m) X^T (X theta - y)"
  )
}

#' Batch gradient descent with full loss curve (Geron Ch 4 Eq 4-6, grbgd)
#'
#' `loss_history` has n_iter + 1 entries: the initial MSE then one per
#' step. `eta_max_stable` is 2 / lambda_max of (2/m) X^T X.
#'
#' @param X,y,theta Design, targets, starting parameters.
#' @param eta Positive learning rate.
#' @param n_iter Steps, >= 1.
#' @return List with `theta`, `theta_path`, `loss_history`, `gradient`,
#'   `eta_max_stable`, `converged`.
#' @export
morie_geron_batch_gradient_descent <- function(X, y, theta, eta, n_iter) {
  Xm <- if (is.matrix(X)) X else as.matrix(X)
  storage.mode(Xm) <- "double"
  yv <- as.numeric(y)
  th <- as.numeric(theta)
  .morie_gr_need(nrow(Xm) == length(yv), "X rows != y entries.")
  .morie_gr_need(ncol(Xm) == length(th), "X columns != theta entries.")
  .morie_gr_need(length(Xm) > 0L, "X is empty.")
  .morie_gr_need(
    all(is.finite(Xm)) && all(is.finite(yv)) && all(is.finite(th)),
    "X, y and theta must all be finite."
  )
  eta <- as.numeric(eta)
  .morie_gr_need(is.finite(eta) && eta > 0, "eta must be a positive finite float.")
  n_iter <- as.integer(n_iter)
  .morie_gr_need(n_iter >= 1L, "n_iter must be at least 1.")
  m <- nrow(Xm)
  H <- (2 / m) * crossprod(Xm)
  lam_max <- if (ncol(Xm)) max(eigen(H, symmetric = TRUE, only.values = TRUE)$values) else 0
  eta_max <- if (lam_max <= 0) Inf else 2 / lam_max
  mse <- function(t) {
    r <- as.numeric(Xm %*% t) - yv
    sum(r * r) / m
  }
  path <- list(th)
  losses <- mse(th)
  grad <- numeric(length(th))
  for (i in seq_len(n_iter)) {
    grad <- (2 / m) * as.numeric(crossprod(Xm, as.numeric(Xm %*% th) - yv))
    th <- th - eta * grad
    .morie_gr_need(
      all(is.finite(th)),
      sprintf(
        "gradient descent diverged; eta=%g exceeds the stability bound %g.",
        eta, eta_max
      )
    )
    path[[length(path) + 1L]] <- th
    losses <- c(losses, mse(th))
  }
  list(
    theta = th, theta_path = path, loss_history = losses, gradient = grad,
    eta = eta, eta_max_stable = eta_max,
    converged = losses[length(losses)] <= losses[1],
    estimate = losses[length(losses)], n = m,
    method = "Batch gradient descent (linear regression MSE)"
  )
}

#' Convolution output size per axis (Geron Ch 12, morie.fn grcos)
#'
#' out = floor((in + 2p - d(k-1) - 1)/s) + 1. `dropped_cells` counts the
#' input columns no window ever covers.
#'
#' @param in_size,kernel,padding,stride,dilation Scalars or per-axis vectors.
#' @return List with `out_size`, `receptive_field`, `same_padding`,
#'   `dropped_cells`, `is_same`.
#' @export
morie_geron_conv_output_size <- function(in_size, kernel, padding = 0,
                                         stride = 1, dilation = 1) {
  vec <- function(v, name, nd = NULL) {
    a <- as.numeric(v)
    .morie_gr_need(all(a == floor(a)), paste0(name, " must contain whole numbers."))
    a <- as.integer(a)
    if (!is.null(nd) && length(a) == 1L) a <- rep(a, nd)
    if (!is.null(nd)) {
      .morie_gr_need(
        length(a) == nd,
        paste0(name, " must have 1 or ", nd, " entries.")
      )
    }
    a
  }
  ins <- vec(in_size, "in_size")
  nd <- length(ins)
  ks <- vec(kernel, "kernel", nd)
  ps <- vec(padding, "padding", nd)
  ss <- vec(stride, "stride", nd)
  ds <- vec(dilation, "dilation", nd)
  .morie_gr_need(all(ins >= 1L), "in_size must be positive.")
  .morie_gr_need(all(ks >= 1L), "kernel must be positive.")
  .morie_gr_need(all(ps >= 0L), "padding must be non-negative.")
  .morie_gr_need(all(ss >= 1L), "stride must be positive.")
  .morie_gr_need(all(ds >= 1L), "dilation must be positive.")
  rf <- ds * (ks - 1L) + 1L
  span <- ins + 2L * ps - rf
  .morie_gr_need(
    all(span >= 0L),
    "the receptive field exceeds the padded input; output would be empty."
  )
  out <- (span %/% ss) + 1L
  dropped <- span - (out - 1L) * ss
  same_pad <- (rf - 1L) %/% 2L
  list(
    out_size = out, receptive_field = rf, same_padding = same_pad,
    dropped_cells = dropped, is_same = all(out == ins),
    estimate = as.numeric(out[1]), n = nd,
    method = "Convolution output-size arithmetic"
  )
}

#' 2-D convolution forward pass, single filter (Geron Ch 12, morie.fn grcvf)
#'
#' Cross-correlation, not flipped convolution -- as in every DL library.
#' Multi-channel input is a list of matrices or a 3-D array (C, H, W).
#'
#' @param X Matrix (H, W) or 3-D array (C, H, W).
#' @param W Matrix (kh, kw) or 3-D array (C, kh, kw).
#' @param b Bias.
#' @param stride Scalar or (sh, sw).
#' @param padding Scalar or (ph, pw).
#' @return List with `Y`, `out_shape`, `padded_shape`, `n_multiply_adds`.
#' @export
morie_geron_conv2d_forward <- function(X, W, b = 0, stride = 1, padding = 0) {
  to3 <- function(A) {
    if (is.matrix(A)) {
      array(as.numeric(A), dim = c(1L, nrow(A), ncol(A)))
    } else {
      .morie_gr_need(length(dim(A)) == 3L, "X and W must be 2-D or 3-D.")
      A
    }
  }
  X <- to3(X)
  W <- to3(W)
  storage.mode(X) <- "double"
  storage.mode(W) <- "double"
  .morie_gr_need(dim(X)[1] == dim(W)[1], "channel mismatch between X and W.")
  .morie_gr_need(length(X) > 0L && length(W) > 0L, "X and W must be non-empty.")
  .morie_gr_need(all(is.finite(X)) && all(is.finite(W)), "X and W must be finite.")
  b <- as.numeric(b)
  .morie_gr_need(is.finite(b), "b must be finite.")
  pr <- function(v, name) {
    a <- as.numeric(v)
    .morie_gr_need(all(a == floor(a)), paste0(name, " must be whole numbers."))
    a <- as.integer(a)
    if (length(a) == 1L) a <- rep(a, 2L)
    .morie_gr_need(length(a) == 2L, paste0(name, " must have 1 or 2 entries."))
    a
  }
  s <- pr(stride, "stride")
  p <- pr(padding, "padding")
  .morie_gr_need(all(s >= 1L), "stride must be positive.")
  .morie_gr_need(all(p >= 0L), "padding must be non-negative.")
  C <- dim(X)[1]
  H <- dim(X)[2]
  Wd <- dim(X)[3]
  kh <- dim(W)[2]
  kw <- dim(W)[3]
  Hp <- H + 2L * p[1]
  Wp <- Wd + 2L * p[2]
  Xp <- array(0, dim = c(C, Hp, Wp))
  Xp[, (p[1] + 1L):(p[1] + H), (p[2] + 1L):(p[2] + Wd)] <- X
  .morie_gr_need(Hp >= kh && Wp >= kw, "filter does not fit the padded input.")
  oh <- ((Hp - kh) %/% s[1]) + 1L
  ow <- ((Wp - kw) %/% s[2]) + 1L
  Y <- matrix(0, oh, ow)
  for (i in seq_len(oh)) {
    r0 <- (i - 1L) * s[1]
    for (j in seq_len(ow)) {
      c0 <- (j - 1L) * s[2]
      Y[i, j] <- sum(Xp[, (r0 + 1L):(r0 + kh), (c0 + 1L):(c0 + kw), drop = FALSE] * W) + b
    }
  }
  list(
    Y = Y, out_shape = c(oh, ow), padded_shape = c(Hp, Wp),
    n_multiply_adds = as.integer(oh * ow * C * kh * kw),
    stride = s, padding = p, estimate = mean(Y), n = length(Y),
    method = "2-D convolution forward pass"
  )
}

# ------------------------------------------- attention, RNN, architectures

#' Bahdanau (additive) attention (Geron Ch 14, morie.fn hmbdn and grbah)
#'
#' e_i = v^T tanh(W h_i + U s + b); alpha = softmax(e); context = alpha h.
#' ONE R function serves both Python modules, whose only difference is
#' argument order and payload naming: grbah's
#' `geron_bahdanau_attention(decoder_state, encoder_states, Wh, Ws, v)`
#' maps onto this call as `h = encoder_states`, `s_prev = decoder_state`,
#' `W = Ws`, `U = Wh`. Both payload spellings are returned (`alpha` is
#' hmbdn's, `weights` grbah's), and `argmax` is 0-based in both.
#'
#' @param h Encoder states (T, d_h).
#' @param s_prev Decoder state (d_s).
#' @param W (d_a, d_h).
#' @param U (d_a, d_s).
#' @param v (d_a).
#' @param b Optional bias (d_a).
#' @return List with `alpha`, `weights`, `scores`, `context`, `entropy`, `argmax`.
#' @export
morie_geron_bahdanau_attention <- function(h, s_prev, W, U, v, b = NULL) {
  H <- if (is.matrix(h)) h else matrix(as.numeric(h), nrow = 1)
  storage.mode(H) <- "double"
  Tn <- nrow(H)
  d_h <- ncol(H)
  .morie_gr_need(Tn > 0L, "geron_bahdanau_attention: h has no time steps")
  s <- as.numeric(s_prev)
  Wm <- as.matrix(W)
  Um <- as.matrix(U)
  storage.mode(Wm) <- "double"
  storage.mode(Um) <- "double"
  vv <- as.numeric(v)
  .morie_gr_need(
    ncol(Wm) == d_h,
    "geron_bahdanau_attention: W columns != h features"
  )
  .morie_gr_need(
    ncol(Um) == length(s),
    "geron_bahdanau_attention: U columns != s_prev entries"
  )
  .morie_gr_need(
    nrow(Wm) == nrow(Um),
    "geron_bahdanau_attention: W and U must share the alignment dimension"
  )
  d_a <- nrow(Wm)
  .morie_gr_need(
    length(vv) == d_a,
    "geron_bahdanau_attention: v does not match the alignment dim"
  )
  bias <- if (is.null(b)) numeric(d_a) else as.numeric(b)
  .morie_gr_need(
    length(bias) == d_a,
    "geron_bahdanau_attention: b does not match the alignment dim"
  )
  pre <- H %*% t(Wm)
  pre <- pre + matrix(as.numeric(Um %*% s) + bias, Tn, d_a, byrow = TRUE)
  scores <- as.numeric(tanh(pre) %*% vv)
  alpha <- .morie_gr_softmax(scores)
  context <- as.numeric(alpha %*% H)
  nz <- alpha > 0
  entropy <- -sum(alpha[nz] * log(alpha[nz]))
  list(
    alpha = alpha, weights = alpha, scores = scores, context = context,
    entropy = entropy, argmax = which.max(alpha) - 1L,
    estimate = max(alpha), n = Tn,
    method = "Bahdanau additive attention e = v^T tanh(W h + U s)"
  )
}

#' Scaled dot-product cross-attention (Geron Ch 15, morie.fn grca)
#'
#' softmax(X_dec WQ (X_enc WK)^T / sqrt(d_k)) X_enc WV. `mask` marks
#' DISALLOWED pairs with TRUE, matching Python.
#'
#' @param X_dec,X_enc Decoder and encoder token matrices.
#' @param WQ,WK,WV Projection matrices.
#' @param mask Optional logical matrix.
#' @return List with `output`, `attention_weights`, `logits`, `scale`, `d_k`.
#' @export
morie_geron_cross_attention <- function(X_dec, X_enc, WQ, WK, WV, mask = NULL) {
  Xd <- .morie_gr_mat(X_dec, "X_dec")
  Xe <- .morie_gr_mat(X_enc, "X_enc")
  WQ <- .morie_gr_mat(WQ, "WQ")
  WK <- .morie_gr_mat(WK, "WK")
  WV <- .morie_gr_mat(WV, "WV")
  .morie_gr_need(
    length(Xd) > 0L && length(Xe) > 0L,
    "X_dec and X_enc must be non-empty."
  )
  .morie_gr_need(ncol(Xd) == nrow(WQ), "X_dec width != WQ rows.")
  .morie_gr_need(ncol(Xe) == nrow(WK), "X_enc width != WK rows.")
  .morie_gr_need(ncol(Xe) == nrow(WV), "X_enc width != WV rows.")
  .morie_gr_need(ncol(WQ) == ncol(WK), "WQ and WK must map into the same d_k.")
  d_k <- ncol(WQ)
  scale <- 1 / sqrt(d_k)
  Q <- Xd %*% WQ
  K <- Xe %*% WK
  V <- Xe %*% WV
  logits <- (Q %*% t(K)) * scale
  if (!is.null(mask)) {
    M <- matrix(as.logical(mask), nrow(logits), ncol(logits))
    .morie_gr_need(all(dim(M) == dim(logits)), "mask shape mismatch.")
    .morie_gr_need(
      !any(rowSums(M) == ncol(M)),
      "mask blocks every encoder position for at least one query."
    )
    logits[M] <- -Inf
  }
  A <- .morie_al_softmax_rows(logits)
  out <- A %*% V
  list(
    output = out, attention_weights = A, logits = logits,
    scale = scale, d_k = d_k, estimate = mean(out), n = nrow(Xd),
    method = "Scaled dot-product cross-attention"
  )
}

#' Bidirectional RNN forward pass (Geron Ch 14, morie.fn hmbrnn)
#'
#' Backward states are re-aligned to the original time index before the
#' concatenation, so `output\[t, \]` mixes prefix and suffix at t.
#'
#' @param X Sequence (T, d).
#' @param Wx_f,Wh_f,Wx_b,Wh_b Weight matrices.
#' @param b_f,b_b,h0_f,h0_b Optional biases and initial states.
#' @param activation One of "tanh", "relu", "sigmoid", "identity".
#' @return List with `output`, `h_fwd`, `h_bwd`, `final`, `hidden_size`.
#' @export
morie_geron_bidirectional_rnn <- function(X, Wx_f, Wh_f, Wx_b, Wh_b,
                                          b_f = NULL, b_b = NULL, h0_f = NULL,
                                          h0_b = NULL, activation = "tanh") {
  phi <- switch(activation,
    tanh = tanh,
    relu = function(z) pmax(z, 0),
    sigmoid = function(z) 1 / (1 + exp(-z)),
    identity = function(z) z,
    stop("geron_bidirectional_rnn: unknown activation.", call. = FALSE)
  )
  A <- if (is.matrix(X)) X else as.matrix(X)
  storage.mode(A) <- "double"
  Tn <- nrow(A)
  d <- ncol(A)
  .morie_gr_need(Tn > 0L, "geron_bidirectional_rnn: X has no time steps")
  Wxf <- as.matrix(Wx_f)
  Whf <- as.matrix(Wh_f)
  Wxb <- as.matrix(Wx_b)
  Whb <- as.matrix(Wh_b)
  storage.mode(Wxf) <- "double"
  storage.mode(Whf) <- "double"
  storage.mode(Wxb) <- "double"
  storage.mode(Whb) <- "double"
  h <- ncol(Wxf)
  .morie_gr_need(
    nrow(Wxf) == d && nrow(Wxb) == d,
    "geron_bidirectional_rnn: X features do not match Wx_f/Wx_b"
  )
  .morie_gr_need(
    ncol(Wxb) == h,
    "geron_bidirectional_rnn: forward/backward hidden size mismatch"
  )
  .morie_gr_need(
    all(dim(Whf) == c(h, h)) && all(dim(Whb) == c(h, h)),
    "geron_bidirectional_rnn: Wh_f and Wh_b must both be (h, h)"
  )
  bf <- if (is.null(b_f)) numeric(h) else as.numeric(b_f)
  bb <- if (is.null(b_b)) numeric(h) else as.numeric(b_b)
  hf <- if (is.null(h0_f)) numeric(h) else as.numeric(h0_f)
  hb <- if (is.null(h0_b)) numeric(h) else as.numeric(h0_b)
  for (nm in c("b_f", "b_b", "h0_f", "h0_b")) {
    vv <- switch(nm,
      b_f = bf,
      b_b = bb,
      h0_f = hf,
      h0_b = hb
    )
    .morie_gr_need(
      length(vv) == h,
      paste0("geron_bidirectional_rnn: ", nm, " length != hidden size")
    )
  }
  H_f <- matrix(0, Tn, h)
  for (t in seq_len(Tn)) {
    hf <- phi(as.numeric(A[t, ] %*% Wxf) + as.numeric(hf %*% Whf) + bf)
    H_f[t, ] <- hf
  }
  H_b <- matrix(0, Tn, h)
  for (t in seq.int(Tn, 1L)) {
    hb <- phi(as.numeric(A[t, ] %*% Wxb) + as.numeric(hb %*% Whb) + bb)
    H_b[t, ] <- hb
  }
  out <- cbind(H_f, H_b)
  list(
    output = out, h_fwd = H_f, h_bwd = H_b,
    final = c(H_f[Tn, ], H_b[1, ]), hidden_size = h,
    estimate = mean(out), n = Tn,
    method = "Bidirectional RNN with concatenated forward/backward states"
  )
}

#' Combine the two directions of a bidirectional layer (Geron Ch 14, grbrnn)
#'
#' Pairs already-computed forward and backward states. Set
#' `backward_in_reverse_order = TRUE` when the backward stack is still in
#' emission order (last time step first) -- the classic silent bug.
#'
#' @param h_forward,h_backward State matrices (T, H).
#' @param backward_in_reverse_order Flip `h_backward` before pairing.
#' @param combine One of "concat", "sum", "mean".
#' @return List with `h`, `output_dim`, `n_steps`, `forward_dim`, `backward_dim`.
#' @export
morie_geron_bidirectional_combine <- function(h_forward, h_backward,
                                              backward_in_reverse_order = FALSE,
                                              combine = "concat") {
  F <- .morie_gr_mat(h_forward, "h_forward")
  B <- .morie_gr_mat(h_backward, "h_backward")
  .morie_gr_need(
    length(F) > 0L && length(B) > 0L,
    "h_forward and h_backward must be non-empty."
  )
  .morie_gr_need(nrow(F) == nrow(B), "time-step count mismatch.")
  if (backward_in_reverse_order) B <- B[seq.int(nrow(B), 1L), , drop = FALSE]
  h <- switch(combine,
    concat = cbind(F, B),
    sum = {
      .morie_gr_need(ncol(F) == ncol(B), "sum needs equal widths.")
      F + B
    },
    mean = {
      .morie_gr_need(ncol(F) == ncol(B), "mean needs equal widths.")
      (F + B) / 2
    },
    stop("combine must be one of 'concat', 'sum', 'mean'.", call. = FALSE)
  )
  list(
    h = h, output_dim = ncol(h), n_steps = nrow(F), forward_dim = ncol(F),
    backward_dim = ncol(B), combine = combine, estimate = mean(h),
    n = nrow(F), method = "Bidirectional RNN state concatenation"
  )
}

#' McCulloch-Pitts neuron (Geron Ch 9, morie.fn hmbnm)
#'
#' a = phi(w . x + b); phi in step, sign, sigmoid, tanh, relu, identity.
#'
#' @param x Input vector or (n, d) matrix.
#' @param w Weights.
#' @param b Bias.
#' @param activation Activation name.
#' @return List with `a`, `z`, `fires`, `threshold`.
#' @export
morie_geron_biological_neuron <- function(x, w, b, activation = "step") {
  ws <- as.numeric(w)
  .morie_gr_need(length(ws) > 0L, "geron_biological_neuron: w is empty")
  xs <- if (is.matrix(x)) x else matrix(as.numeric(x), nrow = 1)
  storage.mode(xs) <- "double"
  .morie_gr_need(
    ncol(xs) == length(ws),
    "geron_biological_neuron: x features != w weights"
  )
  bb <- if (length(b)) as.numeric(b)[1] else 0
  .morie_gr_need(
    all(is.finite(xs)) && all(is.finite(ws)),
    "geron_biological_neuron: x and w must be finite"
  )
  z <- as.numeric(xs %*% ws) + bb
  a <- switch(activation,
    step = ifelse(z >= 0, 1, 0),
    sign = ifelse(z >= 0, 1, -1),
    sigmoid = 1 / (1 + exp(-z)),
    tanh = tanh(z),
    relu = pmax(z, 0),
    identity = z,
    stop("geron_biological_neuron: unknown activation.", call. = FALSE)
  )
  fires <- z >= 0
  scalar <- length(z) == 1L
  list(
    a = if (scalar) a[1] else a, z = if (scalar) z[1] else z,
    fires = if (scalar) fires[1] else fires, threshold = -bb,
    activation = activation, estimate = a[1], n = length(z),
    method = "McCulloch-Pitts neuron a = phi(w.x + b)"
  )
}

#' Batch normalization with running statistics (Geron Ch 11, morie.fn hmbntr)
#'
#' Batch statistics use the POPULATION variance; the INFERENCE running
#' variance uses the unbiased (n-1) batch estimate, per the original
#' paper. When `running_mean`/`running_var` are absent the batch values
#' become the running values outright.
#'
#' @param x Mini-batch (n, d) or vector.
#' @param gamma,beta Scale and shift.
#' @param eps Variance floor.
#' @param momentum EMA factor in \[0, 1\].
#' @param running_mean,running_var Optional previous statistics.
#' @return List with `y`, `x_hat`, `mu`, `var`, `running_mean`, `running_var`.
#' @export
morie_geron_batch_normalization <- function(x, gamma = 1, beta = 0, eps = 1e-5,
                                            momentum = 0.9, running_mean = NULL,
                                            running_var = NULL) {
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = 1)
  storage.mode(X) <- "double"
  n <- nrow(X)
  d <- ncol(X)
  .morie_gr_need(n > 0L, "geron_batch_normalization: mini-batch is empty")
  .morie_gr_need(all(is.finite(X)), "geron_batch_normalization: x must be finite")
  e <- as.numeric(eps)
  .morie_gr_need(e >= 0, "geron_batch_normalization: eps must be non-negative")
  g <- as.numeric(gamma)
  b <- as.numeric(beta)
  .morie_gr_need(
    length(g) %in% c(1L, d) && length(b) %in% c(1L, d),
    "geron_batch_normalization: gamma and beta must be scalars or length-d"
  )
  g <- rep(g, length.out = d)
  b <- rep(b, length.out = d)
  mu <- colMeans(X)
  v <- apply(X, 2, .morie_gr_pvar)
  denom <- sqrt(v + e)
  .morie_gr_need(
    !any(denom == 0),
    "geron_batch_normalization: a feature has zero variance and eps=0; pass eps > 0"
  )
  x_hat <- sweep(sweep(X, 2, mu, "-"), 2, denom, "/")
  y <- sweep(sweep(x_hat, 2, g, "*"), 2, b, "+")
  mom <- as.numeric(momentum)
  .morie_gr_need(
    mom >= 0 && mom <= 1,
    "geron_batch_normalization: momentum must lie in [0, 1]"
  )
  rm_ <- if (is.null(running_mean)) mu else mom * as.numeric(running_mean) + (1 - mom) * mu
  var_unb <- if (n > 1L) apply(X, 2, stats::var) else v
  rv <- if (is.null(running_var)) var_unb else mom * as.numeric(running_var) + (1 - mom) * var_unb
  .morie_gr_need(
    length(rm_) == d && length(rv) == d,
    "geron_batch_normalization: running statistics must have length d"
  )
  list(
    y = y, x_hat = x_hat, mu = mu, var = v, running_mean = rm_,
    running_var = rv, gamma = g, beta = b, estimate = mean(y), n = n,
    method = "Batch normalization (per-feature standardisation then affine rescale)"
  )
}

#' Batch normalization, Ioffe-Szegedy running form (Geron Ch 11, morie.fn grbn)
#'
#' Same normalisation as [morie_geron_batch_normalization()] but grbn's
#' running-statistic convention: defaults are zeros/ones, the update is
#' optional (only when `momentum` is given), and it uses the BIASED batch
#' variance rather than the unbiased one.
#'
#' @param X Mini-batch (m, d).
#' @param gamma,beta Scale and shift.
#' @param eps Variance floor.
#' @param momentum Optional EMA factor in \[0, 1).
#' @param running_mean,running_var Optional statistics to update.
#' @return List with `Y`, `x_hat`, `batch_mean`, `batch_var`,
#'   `running_mean`, `running_var`.
#' @export
morie_geron_batch_normalization_paper <- function(X, gamma, beta, eps = 1e-5,
                                                  momentum = NULL,
                                                  running_mean = NULL,
                                                  running_var = NULL) {
  X <- .morie_gr_mat(X, "X")
  .morie_gr_need(length(X) > 0L, "X must be a non-empty 2-D (m, d) array.")
  m <- nrow(X)
  d <- ncol(X)
  g <- as.numeric(gamma)
  .morie_gr_need(length(g) %in% c(1L, d), "gamma must have 1 or d entries.")
  bt <- as.numeric(beta)
  .morie_gr_need(length(bt) %in% c(1L, d), "beta must have 1 or d entries.")
  g <- rep(g, length.out = d)
  bt <- rep(bt, length.out = d)
  eps <- as.numeric(eps)
  .morie_gr_need(eps >= 0, "eps must be non-negative.")
  mu <- colMeans(X)
  v <- apply(X, 2, .morie_gr_pvar)
  denom <- sqrt(v + eps)
  .morie_gr_need(
    !any(denom == 0),
    "features are constant across the batch and eps=0; pass eps > 0."
  )
  x_hat <- sweep(sweep(X, 2, mu, "-"), 2, denom, "/")
  Y <- sweep(sweep(x_hat, 2, g, "*"), 2, bt, "+")
  rm_ <- if (is.null(running_mean)) numeric(d) else as.numeric(running_mean)
  rv <- if (is.null(running_var)) rep(1, d) else as.numeric(running_var)
  .morie_gr_need(
    length(rm_) == d && length(rv) == d,
    "running_mean/running_var must have d entries."
  )
  if (!is.null(momentum)) {
    mo <- as.numeric(momentum)
    .morie_gr_need(mo >= 0 && mo < 1, "momentum must lie in [0, 1).")
    rm_ <- mo * rm_ + (1 - mo) * mu
    rv <- mo * rv + (1 - mo) * v
  }
  list(
    Y = Y, x_hat = x_hat, batch_mean = mu, batch_var = v,
    running_mean = rm_, running_var = rv, eps = eps, estimate = mean(Y),
    n = m, method = "Batch normalization (Ioffe & Szegedy 2015)"
  )
}

#' Backpropagation through a feed-forward net (Geron Ch 9, morie.fn hmbp)
#'
#' delta_L = grad_a L * phi'(z_L); delta_l = `(W_{l+1} delta_{l+1})` phi'(z_l).
#' `weights` entries are either a matrix W or a list(W, b). Class labels
#' for loss = "ce" are 0-based, as in Python.
#'
#' @param X Inputs (n, d0).
#' @param y Targets or class labels.
#' @param weights List of matrices or list(W, b) pairs.
#' @param activations Character vector, one per layer.
#' @param loss "mse" or "ce".
#' @return List with `grads_W`, `grads_b`, `deltas`, `loss`, `output`.
#' @export
morie_geron_backpropagation <- function(X, y, weights, activations,
                                        loss = "mse") {
  acts_ok <- c("identity", "relu", "sigmoid", "tanh", "softmax")
  A0 <- if (is.matrix(X)) X else matrix(as.numeric(X), nrow = 1)
  storage.mode(A0) <- "double"
  n <- nrow(A0)
  .morie_gr_need(n > 0L, "geron_backpropagation: X has no rows")
  .morie_gr_need(
    loss %in% c("mse", "ce"),
    "geron_backpropagation: loss must be 'mse' or 'ce'"
  )
  Ws <- list()
  bs <- list()
  for (i in seq_along(weights)) {
    layer <- weights[[i]]
    if (is.list(layer) && length(layer) == 2L && !is.matrix(layer)) {
      W <- as.matrix(layer[[1]])
      bvec <- as.numeric(layer[[2]])
    } else {
      W <- as.matrix(layer)
      bvec <- numeric(ncol(as.matrix(layer)))
    }
    storage.mode(W) <- "double"
    .morie_gr_need(
      length(bvec) == ncol(W),
      "geron_backpropagation: bias length != layer units"
    )
    Ws[[i]] <- W
    bs[[i]] <- bvec
  }
  L <- length(Ws)
  .morie_gr_need(L > 0L, "geron_backpropagation: weights is empty")
  acts <- as.character(activations)
  .morie_gr_need(
    length(acts) == L,
    "geron_backpropagation: layer/activation count mismatch"
  )
  .morie_gr_need(
    all(acts %in% acts_ok),
    "geron_backpropagation: unknown activation"
  )
  .morie_gr_need(
    !("softmax" %in% utils::head(acts, -1)),
    "geron_backpropagation: softmax is only allowed on the output layer"
  )
  .morie_gr_need(
    !(acts[L] == "softmax" && loss != "ce"),
    "geron_backpropagation: softmax output requires loss='ce'"
  )
  .morie_gr_need(
    ncol(A0) == nrow(Ws[[1]]),
    "geron_backpropagation: X features != layer 0 inputs"
  )
  if (L > 1L) {
    for (i in 2:L) {
      .morie_gr_need(
        ncol(Ws[[i - 1L]]) == nrow(Ws[[i]]),
        "geron_backpropagation: layer width mismatch"
      )
    }
  }
  d_out <- ncol(Ws[[L]])
  if (loss == "mse") {
    Y <- if (is.matrix(y)) y else matrix(as.numeric(y), ncol = 1)
    storage.mode(Y) <- "double"
    .morie_gr_need(
      nrow(Y) == n && ncol(Y) == d_out,
      "geron_backpropagation: y shape mismatch"
    )
  } else {
    if (is.matrix(y) && nrow(y) == n && ncol(y) == d_out) {
      Y <- matrix(as.numeric(y), n, d_out)
    } else {
      idx <- as.integer(as.vector(y))
      .morie_gr_need(
        length(idx) == n,
        "geron_backpropagation: y must have n class labels"
      )
      .morie_gr_need(
        min(idx) >= 0L && max(idx) < d_out,
        "geron_backpropagation: class labels out of range"
      )
      Y <- matrix(0, n, d_out)
      Y[cbind(seq_len(n), idx + 1L)] <- 1
    }
  }
  fwd <- function(z, kind) {
    switch(kind,
      identity = z,
      relu = pmax(z, 0),
      sigmoid = 1 / (1 + exp(-z)),
      tanh = tanh(z),
      softmax = .morie_al_softmax_rows(z)
    )
  }
  dact <- function(a, z, kind) {
    switch(kind,
      identity = matrix(1, nrow(z), ncol(z)),
      relu = (z > 0) * 1,
      sigmoid = a * (1 - a),
      tanh = 1 - a * a,
      stop("geron_backpropagation: activation has no elementwise derivative",
        call. = FALSE
      )
    )
  }
  a <- A0
  As <- list(A0)
  Zs <- list()
  for (i in seq_len(L)) {
    z <- a %*% Ws[[i]] + matrix(bs[[i]], nrow(a), ncol(Ws[[i]]), byrow = TRUE)
    a <- fwd(z, acts[i])
    Zs[[i]] <- z
    As[[i + 1L]] <- a
  }
  out <- As[[L + 1L]]
  if (loss == "mse") {
    resid <- out - Y
    total <- sum(resid^2) / n
    delta <- (2 / n) * resid * dact(out, Zs[[L]], acts[L])
  } else if (acts[L] == "softmax") {
    p <- pmin(pmax(out, 1e-15), 1)
    total <- -sum(Y * log(p)) / n
    delta <- (out - Y) / n
  } else if (acts[L] == "sigmoid") {
    p <- pmin(pmax(out, 1e-15), 1 - 1e-15)
    total <- -sum(Y * log(p) + (1 - Y) * log(1 - p)) / n
    delta <- (out - Y) / n
  } else {
    stop("geron_backpropagation: loss='ce' requires a softmax or sigmoid output activation",
      call. = FALSE
    )
  }
  grads_W <- vector("list", L)
  grads_b <- vector("list", L)
  deltas <- vector("list", L)
  for (l in seq.int(L, 1L)) {
    deltas[[l]] <- delta
    grads_W[[l]] <- crossprod(As[[l]], delta)
    grads_b[[l]] <- colSums(delta)
    if (l > 1L) {
      delta <- (delta %*% t(Ws[[l]])) * dact(As[[l]], Zs[[l - 1L]], acts[l - 1L])
    }
  }
  list(
    grads_W = grads_W, grads_b = grads_b, deltas = deltas, loss = total,
    output = out, activations_out = As, pre_activations = Zs,
    estimate = total, n = n,
    method = sprintf("Backpropagation of the %s loss through %d layers", loss, L)
  )
}

#' Backpropagation from stored activations (Geron Ch 9/10, morie.fn grbp)
#'
#' Squared-error loss L = 0.5 sum ||a_L - y||^2, derivatives taken from
#' the POST-activation values, which is why only activations are needed.
#'
#' @param activations List of matrices, one longer than `weights`.
#' @param weights List of weight matrices.
#' @param y_true Targets matching the last activation.
#' @param activation Hidden nonlinearity.
#' @param output_activation Output nonlinearity; defaults to `activation`.
#' @return List with `grad_weights`, `grad_biases`, `deltas`, `loss`, `grad_norm`.
#' @export
morie_geron_backpropagation_gradient <- function(activations, weights, y_true,
                                                 activation = "sigmoid",
                                                 output_activation = NULL) {
  acts_ok <- c("sigmoid", "tanh", "relu", "identity")
  acts <- lapply(activations, function(a) .morie_gr_mat(a, "activations"))
  Ws <- lapply(weights, function(W) .morie_gr_mat(W, "weights"))
  y <- .morie_gr_mat(y_true, "y_true")
  .morie_gr_need(
    length(acts) == length(Ws) + 1L,
    "activations must be one longer than weights."
  )
  .morie_gr_need(length(Ws) > 0L, "weights is empty; nothing to differentiate.")
  .morie_gr_need(activation %in% acts_ok, "activation must be a known name.")
  out_act <- if (is.null(output_activation)) activation else output_activation
  .morie_gr_need(out_act %in% acts_ok, "output_activation must be a known name.")
  m <- nrow(acts[[1]])
  for (i in seq_along(acts)) {
    .morie_gr_need(nrow(acts[[i]]) == m, "activation row count mismatch.")
  }
  for (l in seq_along(Ws)) {
    .morie_gr_need(
      nrow(Ws[[l]]) == ncol(acts[[l]]) &&
        ncol(Ws[[l]]) == ncol(acts[[l + 1L]]),
      "weight shape mismatch."
    )
  }
  .morie_gr_need(
    all(dim(y) == dim(acts[[length(acts)]])),
    "y_true shape must match the output activation shape."
  )
  dact <- function(name, a) {
    switch(name,
      sigmoid = a * (1 - a),
      tanh = 1 - a * a,
      relu = (a > 0) * 1,
      matrix(1, nrow(a), ncol(a))
    )
  }
  L <- length(Ws)
  deltas <- vector("list", L)
  err <- acts[[L + 1L]] - y
  deltas[[L]] <- err * dact(out_act, acts[[L + 1L]])
  if (L >= 2L) {
    for (l in seq.int(L - 1L, 1L)) {
      deltas[[l]] <- (deltas[[l + 1L]] %*% t(Ws[[l + 1L]])) *
        dact(activation, acts[[l + 1L]])
    }
  }
  grads <- lapply(seq_len(L), function(l) crossprod(acts[[l]], deltas[[l]]))
  gbias <- lapply(seq_len(L), function(l) colSums(deltas[[l]]))
  lo <- 0.5 * sum(err^2)
  gnorm <- sqrt(sum(vapply(grads, function(g) sum(g^2), numeric(1))))
  list(
    grad_weights = grads, grad_biases = gbias, deltas = deltas, loss = lo,
    grad_norm = gnorm, activation = activation, output_activation = out_act,
    estimate = lo, n = m,
    method = "Backpropagation through a feed-forward net"
  )
}

#' Backpropagation through time (Geron Ch 13, morie.fn grbptt)
#'
#' delta_t = (dL/dh_t + W_h `delta_{t+1}`) * (1 - h_t^2); the shared weight
#' picks up a term at every step, which is where the vanishing gradient
#' shows up in `per_step_delta_norm`.
#'
#' @param loss_grads,hiddens Matrices (T, H).
#' @param inputs Matrix (T, D).
#' @param W_h Optional recurrent weights (H, H).
#' @param h_init Optional state before step 1.
#' @return List with `grad_Wx`, `grad_Wh`, `grad_b`, `deltas`,
#'   `per_step_delta_norm`, `vanishing_ratio`.
#' @export
morie_geron_backprop_through_time <- function(loss_grads, hiddens, inputs,
                                              W_h = NULL, h_init = NULL) {
  G <- .morie_gr_mat(loss_grads, "loss_grads")
  H <- .morie_gr_mat(hiddens, "hiddens")
  X <- .morie_gr_mat(inputs, "inputs")
  .morie_gr_need(all(dim(G) == dim(H)), "loss_grads shape must match hiddens.")
  .morie_gr_need(nrow(X) == nrow(H), "inputs/hiddens time-step mismatch.")
  .morie_gr_need(length(H) > 0L, "no time steps supplied.")
  Tn <- nrow(H)
  hdim <- ncol(H)
  .morie_gr_need(
    !any(abs(H) > 1 + 1e-9),
    "hiddens fall outside [-1, 1]; this routine assumes a tanh RNN."
  )
  Wh <- if (is.null(W_h)) NULL else .morie_gr_mat(W_h, "W_h")
  if (!is.null(Wh)) {
    .morie_gr_need(
      all(dim(Wh) == c(hdim, hdim)),
      "W_h must be (H, H)."
    )
  }
  h0 <- if (is.null(h_init)) numeric(hdim) else as.numeric(h_init)
  .morie_gr_need(length(h0) == hdim, "h_init length mismatch.")
  deltas <- matrix(0, Tn, hdim)
  carry <- numeric(hdim)
  for (t in seq.int(Tn, 1L)) {
    upstream <- G[t, ] + carry
    deltas[t, ] <- upstream * (1 - H[t, ]^2)
    carry <- if (!is.null(Wh)) as.numeric(deltas[t, ] %*% t(Wh)) else numeric(hdim)
  }
  H_prev <- rbind(h0, H[-Tn, , drop = FALSE])
  grad_Wx <- crossprod(X, deltas)
  grad_Wh <- crossprod(H_prev, deltas)
  grad_b <- colSums(deltas)
  norms <- sqrt(rowSums(deltas^2))
  ratio <- if (norms[Tn] > 0) norms[1] / norms[Tn] else NA_real_
  list(
    grad_Wx = grad_Wx, grad_Wh = grad_Wh, grad_b = grad_b, deltas = deltas,
    per_step_delta_norm = norms, vanishing_ratio = ratio,
    estimate = sqrt(sum(grad_Wh^2)), n = Tn,
    method = "Backpropagation through time (tanh RNN)"
  )
}

#' Reverse-mode autodiff over a chain of Jacobians (Geron Ch 10, graut)
#'
#' Walks the chain from the output end, one vector-Jacobian product per
#' node. Nodes are matrices (out_dim, in_dim) or functions g -> g J.
#' `intermediate_grads` is output-end first, as in Python.
#'
#' @param graph List of nodes in FORWARD order.
#' @param grad_output Seed gradient.
#' @return List with `grad_input`, `intermediate_grads`, `depth`, `grad_norm`.
#' @export
morie_geron_autograd_chain_rule <- function(graph, grad_output) {
  nodes <- as.list(graph)
  .morie_gr_need(length(nodes) > 0L, "graph is empty; nothing to differentiate.")
  g <- as.numeric(grad_output)
  .morie_gr_need(length(g) > 0L, "grad_output is empty.")
  .morie_gr_need(all(is.finite(g)), "grad_output contains non-finite values.")
  intermediates <- list(g)
  for (k in seq.int(length(nodes), 1L)) {
    node <- nodes[[k]]
    if (is.function(node)) {
      out <- as.numeric(node(g))
      .morie_gr_need(length(out) > 0L, "a node returned an empty gradient.")
    } else {
      J <- as.matrix(node)
      storage.mode(J) <- "double"
      .morie_gr_need(all(is.finite(J)), "a node Jacobian contains non-finite values.")
      .morie_gr_need(
        nrow(J) == length(g),
        "node Jacobian out_dim != upstream gradient length."
      )
      out <- as.numeric(g %*% J)
    }
    .morie_gr_need(all(is.finite(out)), "a node produced a non-finite gradient.")
    g <- out
    intermediates[[length(intermediates) + 1L]] <- g
  }
  list(
    grad_input = g, intermediate_grads = intermediates,
    depth = length(nodes), grad_norm = sqrt(sum(g^2)),
    estimate = sqrt(sum(g^2)), n = length(g),
    method = "Reverse-mode automatic differentiation (chain rule)"
  )
}

# --------------------------------------------- contrastive and multimodal

#' CLIP symmetric contrastive loss (Geron Ch 16, morie.fn grclp)
#'
#' Mean of the image->text and text->image InfoNCE terms with diagonal
#' targets. `accuracy_*` compare 0-based argmax to the diagonal.
#'
#' @param image_embeddings,text_embeddings Paired matrices (B, d).
#' @param tau Positive temperature.
#' @param normalize L2-normalise first.
#' @return List with `loss`, `loss_i2t`, `loss_t2i`, `similarity`,
#'   `accuracy_i2t`, `accuracy_t2i`, `chance_loss`.
#' @export
morie_geron_clip_contrastive_loss <- function(image_embeddings,
                                              text_embeddings, tau = 0.07,
                                              normalize = TRUE) {
  I <- .morie_gr_mat(image_embeddings, "image embeddings")
  T_ <- .morie_gr_mat(text_embeddings, "text embeddings")
  .morie_gr_need(
    all(dim(I) == dim(T_)),
    "image and text embeddings must have the same shape."
  )
  .morie_gr_need(length(I) > 0L, "embeddings are empty.")
  tau <- as.numeric(tau)
  .morie_gr_need(is.finite(tau) && tau > 0, "tau must be a positive finite float.")
  B <- nrow(I)
  if (normalize) {
    ni <- sqrt(rowSums(I^2))
    nt <- sqrt(rowSums(T_^2))
    .morie_gr_need(
      !any(ni == 0) && !any(nt == 0),
      "cannot L2-normalise a zero embedding."
    )
    I <- I / ni
    T_ <- T_ / nt
  }
  sim <- I %*% t(T_)
  logits <- sim / tau
  idx <- cbind(seq_len(B), seq_len(B))
  loss_i2t <- -mean(.morie_gr_log_softmax_rows(logits)[idx])
  loss_t2i <- -mean(.morie_gr_log_softmax_rows(t(logits))[idx])
  loss <- 0.5 * (loss_i2t + loss_t2i)
  acc_i2t <- mean((apply(logits, 1, which.max) - 1L) == (seq_len(B) - 1L))
  acc_t2i <- mean((apply(logits, 2, which.max) - 1L) == (seq_len(B) - 1L))
  list(
    loss = loss, loss_i2t = loss_i2t, loss_t2i = loss_t2i,
    similarity = sim, accuracy_i2t = acc_i2t, accuracy_t2i = acc_t2i,
    chance_loss = log(B), tau = tau, estimate = loss, n = B,
    method = "CLIP symmetric contrastive loss"
  )
}

#' InfoNCE contrastive loss (Geron Ch 16, morie.fn grctr)
#'
#' Cross-entropy over 1 positive and N negatives; chance loss log(1+N).
#'
#' @param anchors,positives Matrices (B, d).
#' @param negatives Matrix (N, d) shared, or 3-D array (B, N, d).
#' @param tau Positive temperature.
#' @param normalize Cosine similarity.
#' @return List with `loss`, `per_anchor_loss`, `pos_sim`, `neg_sim`,
#'   `hardest_negative`, `accuracy`, `chance_loss`.
#' @export
morie_geron_contrastive_infonce <- function(anchors, positives, negatives,
                                            tau = 0.1, normalize = TRUE) {
  A <- .morie_gr_mat(anchors, "anchors")
  P <- .morie_gr_mat(positives, "positives")
  .morie_gr_need(
    all(dim(A) == dim(P)),
    "anchors and positives must have the same shape."
  )
  .morie_gr_need(length(A) > 0L, "anchors is empty.")
  B <- nrow(A)
  d <- ncol(A)
  if (is.matrix(negatives) || is.data.frame(negatives)) {
    Nm <- .morie_gr_mat(negatives, "negatives")
    .morie_gr_need(ncol(Nm) == d, "negatives width != anchor width.")
    Nn <- nrow(Nm)
    N <- array(0, dim = c(B, Nn, d))
    for (b in seq_len(B)) N[b, , ] <- Nm
  } else {
    N <- negatives
    .morie_gr_need(length(dim(N)) == 3L, "negatives must be 2-D or 3-D.")
    .morie_gr_need(
      dim(N)[1] == B && dim(N)[3] == d,
      "per-anchor negatives must have shape (B, N, d)."
    )
    storage.mode(N) <- "double"
    Nn <- dim(N)[2]
  }
  .morie_gr_need(Nn > 0L, "at least one negative per anchor is required.")
  .morie_gr_need(all(is.finite(N)), "negatives contains non-finite values.")
  tau <- as.numeric(tau)
  .morie_gr_need(is.finite(tau) && tau > 0, "tau must be a positive finite float.")
  l2 <- function(M) {
    nn <- sqrt(rowSums(M^2))
    .morie_gr_need(!any(nn == 0), "cannot cosine-normalise a zero vector.")
    M / nn
  }
  if (normalize) {
    A <- l2(A)
    P <- l2(P)
    for (b in seq_len(B)) N[b, , ] <- l2(matrix(N[b, , ], Nn, d))
  }
  pos <- rowSums(A * P)
  neg <- matrix(0, B, Nn)
  for (b in seq_len(B)) neg[b, ] <- as.numeric(matrix(N[b, , ], Nn, d) %*% A[b, ])
  logits <- cbind(pos, neg) / tau
  mm <- apply(logits, 1, max)
  lse <- mm + log(rowSums(exp(logits - mm)))
  per_anchor <- lse - logits[, 1]
  loss <- mean(per_anchor)
  acc <- mean(apply(logits, 1, which.max) == 1L)
  list(
    loss = loss, per_anchor_loss = per_anchor, pos_sim = pos,
    neg_sim = neg, hardest_negative = apply(neg, 1, max), accuracy = acc,
    chance_loss = log(1 + Nn), tau = tau, estimate = loss, n = B,
    method = "InfoNCE contrastive loss"
  )
}

#' BLIP pretraining objectives on paired embeddings (Geron Ch 16, hmblip)
#'
#' ITC is symmetric InfoNCE over the cosine matrix / tau; ITM is binary
#' cross-entropy of the matched pairs against the batch rotated by one;
#' LM is the mean negative caption log-probability when supplied.
#'
#' @param images,texts Row-aligned embedding matrices (n, d), n >= 2.
#' @param temperature Positive temperature.
#' @param caption_logprobs Optional per-caption mean token log-probs.
#' @return List with `itc_loss`, `itm_loss`, `lm_loss`, `total_loss`,
#'   `similarity`, `retrieval_acc`.
#' @export
morie_geron_blip <- function(images, texts, temperature = 1,
                             caption_logprobs = NULL) {
  I <- .morie_gr_mat(images, "geron_blip: images")
  T_ <- .morie_gr_mat(texts, "geron_blip: texts")
  .morie_gr_need(all(dim(I) == dim(T_)), "geron_blip: images/texts shape mismatch")
  n <- nrow(I)
  .morie_gr_need(
    n >= 2L,
    "geron_blip: contrastive learning needs at least 2 pairs in the batch"
  )
  tau <- as.numeric(temperature)
  .morie_gr_need(is.finite(tau) && tau > 0, "geron_blip: temperature must be positive")
  nrm <- function(M) {
    v <- sqrt(rowSums(M^2))
    .morie_gr_need(
      !any(v == 0),
      "geron_blip: an embedding has zero norm and cannot be projected onto the sphere"
    )
    M / v
  }
  In <- nrm(I)
  Tn <- nrm(T_)
  sim <- In %*% t(Tn)
  logits <- sim / tau
  dg <- cbind(seq_len(n), seq_len(n))
  p_i2t <- .morie_al_softmax_rows(logits)
  p_t2i <- .morie_gr_softmax_cols(logits)
  loss_i2t <- -mean(log(pmax(p_i2t[dg], 1e-15)))
  loss_t2i <- -mean(log(pmax(p_t2i[dg], 1e-15)))
  itc <- 0.5 * (loss_i2t + loss_t2i)
  pos <- sim[dg]
  nxt <- (seq_len(n) %% n) + 1L
  neg <- sim[cbind(seq_len(n), nxt)]
  sig <- function(z) 1 / (1 + exp(-z))
  itm <- (-mean(log(pmax(sig(pos / tau), 1e-15))) -
    mean(log(pmax(1 - sig(neg / tau), 1e-15)))) / 2
  lm <- NULL
  if (!is.null(caption_logprobs)) {
    cl <- as.numeric(caption_logprobs)
    .morie_gr_need(length(cl) == n, "geron_blip: caption_logprobs length != batch")
    .morie_gr_need(
      !any(cl > 0),
      "geron_blip: caption_logprobs must be log-probabilities (<= 0)"
    )
    lm <- -mean(cl)
  }
  total <- itc + itm + (if (is.null(lm)) 0 else lm)
  retrieval <- mean((apply(sim, 1, which.max) - 1L) == (seq_len(n) - 1L))
  list(
    itc_loss = itc, itc_i2t = loss_i2t, itc_t2i = loss_t2i,
    itm_loss = itm, lm_loss = lm, total_loss = total, similarity = sim,
    retrieval_acc = retrieval, temperature = tau, estimate = total, n = n,
    method = "BLIP: contrastive + matching (+ captioning) objectives on paired embeddings"
  )
}

#' BLIP multi-task loss with captioning head (Geron Ch 16, morie.fn grblip)
#'
#' ITC (symmetric InfoNCE) + ITM (pairwise BCE over all B^2 pairs,
#' diagonal = match) + LM (token cross-entropy, negative targets are
#' padding). Caption targets are 0-based token ids.
#'
#' @param image_emb,text_emb Matrices (B, d).
#' @param caption_logits 3-D array (B, L, V).
#' @param caption_targets Matrix (B, L) of 0-based ids; negatives are padding.
#' @param tau Temperature.
#' @param lam_itc,lam_itm,lam_lm Task weights.
#' @param normalize Cosine similarity.
#' @return List with `loss`, `itc`, `itm`, `lm`, `similarity`,
#'   `itm_accuracy`, `lm_perplexity`.
#' @export
morie_geron_blip_itm_itc <- function(image_emb, text_emb, caption_logits,
                                     caption_targets, tau = 0.07,
                                     lam_itc = 1, lam_itm = 1, lam_lm = 1,
                                     normalize = TRUE) {
  I <- .morie_gr_mat(image_emb, "image_emb")
  T_ <- .morie_gr_mat(text_emb, "text_emb")
  .morie_gr_need(all(dim(I) == dim(T_)), "image_emb shape must match text_emb.")
  .morie_gr_need(length(I) > 0L, "embeddings are empty.")
  B <- nrow(I)
  CL <- caption_logits
  .morie_gr_need(length(dim(CL)) == 3L, "caption_logits must be 3-D (B, L, V).")
  storage.mode(CL) <- "double"
  .morie_gr_need(dim(CL)[1] == B, "caption_logits batch mismatch.")
  .morie_gr_need(all(is.finite(CL)), "caption_logits contains non-finite values.")
  Lseq <- dim(CL)[2]
  V <- dim(CL)[3]
  tgt <- if (is.matrix(caption_targets)) {
    caption_targets
  } else {
    matrix(as.numeric(caption_targets), nrow = 1)
  }
  tgt <- matrix(as.integer(tgt), nrow(tgt), ncol(tgt))
  .morie_gr_need(
    nrow(tgt) == B && ncol(tgt) == Lseq,
    "caption_targets must have shape (B, L)."
  )
  .morie_gr_need(
    !any(tgt >= V),
    "caption target indices must be below the vocabulary size."
  )
  tau <- as.numeric(tau)
  .morie_gr_need(is.finite(tau) && tau > 0, "tau must be a positive finite float.")
  for (lam in c(lam_itc, lam_itm, lam_lm)) {
    .morie_gr_need(
      is.finite(lam) && lam >= 0,
      "task weights must be non-negative finite floats."
    )
  }
  if (normalize) {
    ni <- sqrt(rowSums(I^2))
    nt <- sqrt(rowSums(T_^2))
    .morie_gr_need(
      !any(ni == 0) && !any(nt == 0),
      "cannot cosine-normalise a zero embedding."
    )
    I <- I / ni
    T_ <- T_ / nt
  }
  sim <- I %*% t(T_)
  logits <- sim / tau
  dg <- cbind(seq_len(B), seq_len(B))
  itc <- 0.5 * (-mean(.morie_gr_log_softmax_rows(logits)[dg]) +
    -mean(.morie_gr_log_softmax_rows(t(logits))[dg]))
  labels <- diag(1, B)
  softplus <- function(z) pmax(z, 0) + log1p(exp(-abs(z)))
  itm <- mean(softplus(logits) - labels * logits)
  itm_acc <- mean((logits > 0) == (labels > 0))
  mask <- tgt >= 0L
  .morie_gr_need(any(mask), "every caption target is padding; the LM loss is undefined.")
  tok_lp <- matrix(0, B, Lseq)
  for (b in seq_len(B)) {
    ls <- .morie_gr_log_softmax_rows(matrix(CL[b, , ], Lseq, V))
    safe <- ifelse(mask[b, ], tgt[b, ], 0L)
    tok_lp[b, ] <- ls[cbind(seq_len(Lseq), safe + 1L)]
  }
  lm <- -sum(tok_lp * mask) / sum(mask)
  loss <- lam_itc * itc + lam_itm * itm + lam_lm * lm
  list(
    loss = loss, itc = itc, itm = itm, lm = lm, similarity = sim,
    itm_accuracy = itm_acc, lm_perplexity = exp(lm), tau = tau,
    weights = c(lam_itc, lam_itm, lam_lm), estimate = loss, n = B,
    method = "BLIP multi-task objective (ITC + ITM + LM)"
  )
}

#' BLIP-2 Q-Former bridge (Geron Ch 16, morie.fn hmblp2)
#'
#' Learned queries cross-attend frozen image patch features and are
#' projected to the LLM width. Only the Q-Former is trainable, which is
#' what `trainable_params` makes explicit.
#'
#' @param image Patch features (n_patches, d_v).
#' @param text Embedding or token matrix.
#' @param n_query,d_query Q-Former geometry.
#' @param d_llm LLM width.
#' @param temperature Positive temperature.
#' @param seed LCG seed.
#' @return List with `query_output`, `llm_input`, `attention`, `similarity`,
#'   `query_similarities`, `trainable_params`.
#' @export
morie_geron_blip2 <- function(image, text, n_query = 4, d_query = 8,
                              d_llm = NULL, temperature = 1, seed = 0) {
  V <- if (is.matrix(image)) image else matrix(as.numeric(image), nrow = 1)
  storage.mode(V) <- "double"
  .morie_gr_need(nrow(V) > 0L, "geron_blip2: image has no patches")
  .morie_gr_need(all(is.finite(V)), "geron_blip2: image features must be finite")
  Tt <- if (is.matrix(text)) text else matrix(as.numeric(text), nrow = 1)
  storage.mode(Tt) <- "double"
  .morie_gr_need(nrow(Tt) > 0L, "geron_blip2: text must be non-empty")
  .morie_gr_need(all(is.finite(Tt)), "geron_blip2: text features must be finite")
  Q <- as.integer(n_query)
  dq <- as.integer(d_query)
  .morie_gr_need(
    Q >= 1L && dq >= 1L,
    "geron_blip2: n_query and d_query must both be >= 1"
  )
  dl <- if (is.null(d_llm)) dq else as.integer(d_llm)
  .morie_gr_need(dl >= 1L, "geron_blip2: d_llm must be >= 1")
  tau <- as.numeric(temperature)
  .morie_gr_need(is.finite(tau) && tau > 0, "geron_blip2: temperature must be positive")
  n_patches <- nrow(V)
  dv <- ncol(V)
  dt <- ncol(Tt)
  queries <- .morie_gr_init(Q, dq, seed + 11)
  Wk <- .morie_gr_init(dv, dq, seed + 22)
  Wv <- .morie_gr_init(dv, dq, seed + 33)
  Wo <- .morie_gr_init(dq, dq, seed + 44)
  Wllm <- .morie_gr_init(dq, dl, seed + 55)
  Wtxt <- .morie_gr_init(dt, dq, seed + 66)
  K <- V %*% Wk
  Vv <- V %*% Wv
  attn <- .morie_al_softmax_rows(queries %*% t(K) / sqrt(dq))
  qout <- (attn %*% Vv) %*% Wo
  llm_input <- qout %*% Wllm
  text_vec <- as.numeric(colMeans(Tt) %*% Wtxt)
  qn <- qout / pmax(sqrt(rowSums(qout^2)), 1e-12)
  tn <- text_vec / max(sqrt(sum(text_vec^2)), 1e-12)
  sims <- as.numeric(qn %*% tn)
  similarity <- max(sims) / tau
  trainable <- as.integer(Q * dq + dv * dq * 2 + dq * dq + dq * dl + dt * dq)
  list(
    query_output = qout, llm_input = llm_input, attention = attn,
    similarity = similarity, query_similarities = sims,
    trainable_params = trainable, frozen_encoders = c("image", "text"),
    d_llm = dl, estimate = similarity, n = n_patches,
    method = "BLIP-2: learned queries cross-attending frozen image features, projected to the LLM width"
  )
}

#' DeiT hard-label distillation loss (Geron Ch 16, morie.fn grdeit)
#'
#' L = (1 - alpha) CE(cls, y) + alpha CE(dist, argmax teacher). Labels
#' are 0-based; `teacher_labels` comes back 0-based too.
#'
#' @param logits_cls,logits_dist Matrices (B, C).
#' @param y 0-based labels.
#' @param teacher_preds Matrix (B, C) or vector of labels.
#' @param alpha Distillation weight in \[0, 1\].
#' @return List with `loss`, `loss_cls`, `loss_dist`, `teacher_labels`,
#'   `teacher_agreement`, `accuracy_cls`, `accuracy_dist`.
#' @export
morie_geron_deit_distillation_loss <- function(logits_cls, logits_dist, y,
                                               teacher_preds, alpha = 0.5) {
  Lc <- .morie_gr_mat(logits_cls, "logits_cls")
  Ld <- .morie_gr_mat(logits_dist, "logits_dist")
  .morie_gr_need(
    all(dim(Lc) == dim(Ld)),
    "logits_cls shape must match logits_dist shape."
  )
  .morie_gr_need(length(Lc) > 0L, "logits are empty.")
  B <- nrow(Lc)
  C <- ncol(Lc)
  y <- as.integer(as.vector(y))
  .morie_gr_need(length(y) == B, "y must have one label per instance.")
  .morie_gr_need(min(y) >= 0L && max(y) < C, "y labels out of range.")
  if (is.matrix(teacher_preds)) {
    tp <- .morie_gr_mat(teacher_preds, "teacher_preds")
    .morie_gr_need(nrow(tp) == B && ncol(tp) == C, "teacher_preds shape mismatch.")
    t_lab <- apply(tp, 1, which.max) - 1L
  } else {
    t_lab <- as.integer(as.vector(teacher_preds))
    .morie_gr_need(length(t_lab) == B, "teacher_preds must have B labels.")
  }
  .morie_gr_need(min(t_lab) >= 0L && max(t_lab) < C, "teacher labels out of range.")
  alpha <- as.numeric(alpha)
  .morie_gr_need(alpha >= 0 && alpha <= 1, "alpha must lie in [0, 1].")
  loss_cls <- -mean(.morie_gr_log_softmax_rows(Lc)[cbind(seq_len(B), y + 1L)])
  loss_dist <- -mean(.morie_gr_log_softmax_rows(Ld)[cbind(seq_len(B), t_lab + 1L)])
  loss <- (1 - alpha) * loss_cls + alpha * loss_dist
  list(
    loss = loss, loss_cls = loss_cls, loss_dist = loss_dist,
    teacher_labels = t_lab, teacher_agreement = mean(t_lab == y),
    accuracy_cls = mean((apply(Lc, 1, which.max) - 1L) == y),
    accuracy_dist = mean((apply(Ld, 1, which.max) - 1L) == t_lab),
    alpha = alpha, estimate = loss, n = B,
    method = "DeiT hard-label distillation loss"
  )
}

#' DALL-E autoregressive image-token scoring (Geron Ch 16, morie.fn grdal)
#'
#' Teacher-forced log-likelihood of the prefix plus the next-token
#' distribution after temperature and optional top-k truncation.
#' `logits_fn` receives the 0-based context as a numeric vector.
#'
#' @param text_tokens,image_tokens_prefix 0-based token vectors.
#' @param logits_fn Function(context) -> numeric logits of length V.
#' @param temperature Positive temperature for the next-token step only.
#' @param top_k Optional truncation.
#' @return List with `log_likelihood`, `token_logprobs`, `perplexity`,
#'   `next_token_probs`, `next_token`, `vocab_size`.
#' @export
morie_geron_dalle_autoregressive_token <- function(text_tokens,
                                                   image_tokens_prefix,
                                                   logits_fn, temperature = 1,
                                                   top_k = NULL) {
  text <- as.integer(as.vector(text_tokens))
  prefix <- as.integer(as.vector(image_tokens_prefix))
  .morie_gr_need(
    is.function(logits_fn),
    "logits_fn must be callable(context) -> logits."
  )
  temperature <- as.numeric(temperature)
  .morie_gr_need(
    is.finite(temperature) && temperature > 0,
    "temperature must be a positive finite float."
  )
  V <- NULL
  logprobs <- numeric(0)
  context <- text
  next_logits <- NULL
  for (step in seq_len(length(prefix) + 1L)) {
    z <- as.numeric(logits_fn(context))
    .morie_gr_need(length(z) > 0L, "logits_fn returned an empty array.")
    .morie_gr_need(all(is.finite(z)), "logits_fn returned non-finite logits.")
    if (is.null(V)) {
      V <- length(z)
    } else {
      .morie_gr_need(length(z) == V, "logits_fn changed vocabulary size.")
    }
    if (step <= length(prefix)) {
      tok <- prefix[step]
      .morie_gr_need(
        tok >= 0L && tok < V,
        "an image token is outside the vocabulary."
      )
      logprobs <- c(logprobs, .morie_gr_log_softmax(z)[tok + 1L])
      context <- c(context, tok)
    } else {
      next_logits <- z
    }
  }
  if (!is.null(top_k)) {
    k <- as.integer(top_k)
    .morie_gr_need(k >= 1L && k <= V, "top_k must lie in [1, V].")
    cut <- sort(next_logits)[V - k + 1L]
    next_logits <- ifelse(next_logits >= cut, next_logits, -Inf)
  }
  scaled <- next_logits / temperature
  finite <- is.finite(scaled)
  probs <- numeric(V)
  probs[finite] <- exp(.morie_gr_log_softmax(scaled[finite]))
  ll <- sum(logprobs)
  ppl <- if (length(prefix)) exp(-ll / length(prefix)) else NA_real_
  list(
    log_likelihood = ll, token_logprobs = logprobs, perplexity = ppl,
    next_token_probs = probs, next_token = which.max(probs) - 1L,
    vocab_size = V, context_length = length(text) + length(prefix),
    estimate = ll, n = length(prefix),
    method = "Autoregressive image-token likelihood"
  )
}

#' DETR Hungarian matching and set-prediction loss (Geron Ch 16, grdetr)
#'
#' Cost C(i,j) = -p_i(c_j) + lam_bbox ||b_i - b_j||_1 - lam_giou GIoU,
#' minimised exactly by the Jonker-Volgenant shortest-augmenting-path
#' assignment. `matching` pairs are 0-based (pred, gt), as in Python.
#'
#' @param pred_boxes,gt_boxes Box matrices (N, 4) / (M, 4).
#' @param pred_classes Logits or probabilities (N, C).
#' @param gt_classes 0-based ground-truth classes.
#' @param lam_bbox,lam_giou Non-negative cost weights.
#' @param box_format "xyxy" or "cxcywh".
#' @param no_object_class Optional 0-based no-object index.
#' @param eos_coef No-object down-weight.
#' @param class_is_logits Softmax needed.
#' @return List with `matching`, `cost_matrix`, `total_cost`, `loss`,
#'   `loss_class`, `loss_bbox`, `loss_giou`, `loss_no_object`,
#'   `matched_giou`, `matched_l1`, `unmatched_predictions`.
#' @export
morie_geron_detr_hungarian_matching <- function(pred_boxes, pred_classes,
                                                gt_boxes, gt_classes,
                                                lam_bbox = 5, lam_giou = 2,
                                                box_format = "xyxy",
                                                no_object_class = NULL,
                                                eos_coef = 0.1,
                                                class_is_logits = TRUE) {
  to_xyxy <- function(B, fmt) {
    B <- .morie_gr_mat(B, "boxes")
    .morie_gr_need(ncol(B) == 4L, "boxes must have shape (N, 4).")
    out <- if (fmt == "xyxy") {
      B
    } else if (fmt == "cxcywh") {
      cx <- B[, 1]
      cy <- B[, 2]
      w <- B[, 3]
      h <- B[, 4]
      .morie_gr_need(
        all(w >= 0) && all(h >= 0),
        "cxcywh boxes must have non-negative width and height."
      )
      cbind(cx - w / 2, cy - h / 2, cx + w / 2, cy + h / 2)
    } else {
      stop("box_format must be 'xyxy' or 'cxcywh'.", call. = FALSE)
    }
    .morie_gr_need(
      !any(out[, 3] < out[, 1]) && !any(out[, 4] < out[, 2]),
      "boxes have x2 < x1 or y2 < y1 after conversion."
    )
    out
  }
  P <- to_xyxy(pred_boxes, box_format)
  G <- to_xyxy(gt_boxes, box_format)
  N <- nrow(P)
  M <- nrow(G)
  .morie_gr_need(
    N > 0L && M > 0L,
    "need at least one prediction and one ground-truth box."
  )
  Z <- .morie_gr_mat(pred_classes, "pred_classes")
  .morie_gr_need(nrow(Z) == N, "pred_classes rows != predicted boxes.")
  C <- ncol(Z)
  gt <- as.integer(as.vector(gt_classes))
  .morie_gr_need(length(gt) == M, "gt_classes entries != ground-truth boxes.")
  .morie_gr_need(min(gt) >= 0L && max(gt) < C, "gt_classes out of range.")
  lam_bbox <- as.numeric(lam_bbox)
  lam_giou <- as.numeric(lam_giou)
  .morie_gr_need(
    lam_bbox >= 0 && lam_giou >= 0,
    "lam_bbox and lam_giou must be non-negative."
  )
  .morie_gr_need(N >= M, "DETR needs at least as many queries as objects.")
  if (class_is_logits) {
    logp <- .morie_gr_log_softmax_rows(Z)
  } else {
    .morie_gr_need(!any(Z < 0), "probabilities must be non-negative.")
    .morie_gr_need(all(abs(rowSums(Z) - 1) < 1e-6), "probability rows must sum to 1.")
    logp <- log(pmax(Z, 1e-300))
  }
  prob <- exp(logp)
  l1 <- matrix(0, N, M)
  giou <- matrix(0, N, M)
  for (i in seq_len(N)) {
    for (j in seq_len(M)) {
      l1[i, j] <- sum(abs(P[i, ] - G[j, ]))
      iw <- max(min(P[i, 3], G[j, 3]) - max(P[i, 1], G[j, 1]), 0)
      ih <- max(min(P[i, 4], G[j, 4]) - max(P[i, 2], G[j, 2]), 0)
      inter <- iw * ih
      area_a <- (P[i, 3] - P[i, 1]) * (P[i, 4] - P[i, 2])
      area_b <- (G[j, 3] - G[j, 1]) * (G[j, 4] - G[j, 2])
      union <- area_a + area_b - inter
      iou <- if (union > 0) inter / union else 0
      cw <- max(P[i, 3], G[j, 3]) - min(P[i, 1], G[j, 1])
      ch <- max(P[i, 4], G[j, 4]) - min(P[i, 2], G[j, 2])
      carea <- cw * ch
      giou[i, j] <- if (carea > 0) iou - (carea - union) / carea else iou
    }
  }
  cost <- -prob[, gt + 1L, drop = FALSE] + lam_bbox * l1 - lam_giou * giou
  asg <- .morie_gr_lsa(cost)
  rows <- asg$rows
  cols <- asg$cols
  matching <- lapply(seq_along(rows), function(i) c(rows[i], cols[i]))
  ridx <- cbind(rows + 1L, cols + 1L)
  total_cost <- sum(cost[ridx])
  loss_cls <- -sum(logp[cbind(rows + 1L, gt[cols + 1L] + 1L)])
  loss_bbox <- lam_bbox * sum(l1[ridx])
  loss_giou <- lam_giou * sum(1 - giou[ridx])
  unmatched <- sort(setdiff(seq_len(N) - 1L, rows))
  loss_noobj <- 0
  if (!is.null(no_object_class)) {
    noc <- as.integer(no_object_class)
    .morie_gr_need(noc >= 0L && noc < C, "no_object_class out of range.")
    eos_coef <- as.numeric(eos_coef)
    .morie_gr_need(eos_coef >= 0, "eos_coef must be non-negative.")
    if (length(unmatched)) loss_noobj <- -eos_coef * sum(logp[unmatched + 1L, noc + 1L])
  }
  loss <- loss_cls + loss_bbox + loss_giou + loss_noobj
  list(
    matching = matching, cost_matrix = cost, total_cost = total_cost,
    loss = loss, loss_class = loss_cls, loss_bbox = loss_bbox,
    loss_giou = loss_giou, loss_no_object = loss_noobj,
    matched_giou = giou[ridx], matched_l1 = l1[ridx],
    unmatched_predictions = unmatched, estimate = loss, n = M,
    method = "DETR Hungarian matching and set-prediction loss"
  )
}

# Jonker-Volgenant shortest augmenting path, ported line-for-line from
# grdetr._linear_sum_assignment. Returns 0-based rows/cols.
#' Jonker-Volgenant shortest augmenting path, ported line-for-line from
#'
#' grdetr._linear_sum_assignment. Returns 0-based rows/cols.
#'
#' @param cost Passed to \code{.morie_gr_mat}.
#' @return A list with \code{rows}, \code{cols}.
#' @export
.morie_gr_lsa <- function(cost) {
  C <- .morie_gr_mat(cost, "cost matrix")
  .morie_gr_need(length(C) > 0L, "cost must be a non-empty 2-D matrix.")
  transposed <- nrow(C) > ncol(C)
  if (transposed) C <- t(C)
  n <- nrow(C)
  m <- ncol(C)
  u <- numeric(n + 1L)
  v <- numeric(m + 1L)
  p <- integer(m + 1L)
  way <- integer(m + 1L)
  for (i in seq_len(n)) {
    p[1] <- i
    j0 <- 0L
    minv <- rep(Inf, m + 1L)
    used <- rep(FALSE, m + 1L)
    repeat {
      used[j0 + 1L] <- TRUE
      i0 <- p[j0 + 1L]
      delta <- Inf
      j1 <- -1L
      for (j in seq_len(m)) {
        if (used[j + 1L]) next
        cur <- C[i0, j] - u[i0 + 1L] - v[j + 1L]
        if (cur < minv[j + 1L]) {
          minv[j + 1L] <- cur
          way[j + 1L] <- j0
        }
        if (minv[j + 1L] < delta) {
          delta <- minv[j + 1L]
          j1 <- j
        }
      }
      for (j in 0:m) {
        if (used[j + 1L]) {
          u[p[j + 1L] + 1L] <- u[p[j + 1L] + 1L] + delta
          v[j + 1L] <- v[j + 1L] - delta
        } else {
          minv[j + 1L] <- minv[j + 1L] - delta
        }
      }
      j0 <- j1
      if (p[j0 + 1L] == 0L) break
    }
    while (j0 != 0L) {
      j1 <- way[j0 + 1L]
      p[j0 + 1L] <- p[j1 + 1L]
      j0 <- j1
    }
  }
  rows <- integer(0)
  cols <- integer(0)
  for (j in seq_len(m)) {
    if (p[j + 1L] != 0L) {
      rows <- c(rows, p[j + 1L] - 1L)
      cols <- c(cols, j - 1L)
    }
  }
  ord <- order(rows, method = "radix")
  rows <- rows[ord]
  cols <- cols[ord]
  if (transposed) {
    tmp <- rows
    rows <- cols
    cols <- tmp
    ord <- order(rows, method = "radix")
    rows <- rows[ord]
    cols <- cols[ord]
  }
  list(rows = rows, cols = cols)
}

# -------------------------------------------- reinforcement learning, diffusion

#' One-step actor-critic advantage (Geron Ch 19, morie.fn grac)
#'
#' A = r + gamma (1 - done) V(s') - V(s). State indices are 0-based.
#'
#' @param V Value table.
#' @param s,s_next 0-based state indices.
#' @param r Rewards.
#' @param gamma Discount in \[0, 1\].
#' @param done Optional terminal flags.
#' @return List with `advantage`, `td_target`, `value_s`, `value_s_next`,
#'   `critic_loss`.
#' @export
morie_geron_actor_critic_advantage <- function(V, s, s_next, r, gamma,
                                               done = NULL) {
  V <- as.numeric(V)
  .morie_gr_need(length(V) > 0L, "V must contain at least one state value.")
  .morie_gr_need(all(is.finite(V)), "V contains non-finite values.")
  s <- as.integer(as.vector(s))
  s_next <- as.integer(as.vector(s_next))
  r <- as.numeric(r)
  .morie_gr_need(
    length(s) == length(s_next) && length(s) == length(r),
    "s, s_next and r must have equal length."
  )
  .morie_gr_need(length(s) > 0L, "no transitions supplied.")
  .morie_gr_need(
    min(s) >= 0L && max(s) < length(V) &&
      min(s_next) >= 0L && max(s_next) < length(V),
    "state indices out of range."
  )
  gamma <- as.numeric(gamma)
  .morie_gr_need(gamma >= 0 && gamma <= 1, "gamma must lie in [0, 1].")
  mask <- if (is.null(done)) numeric(length(s)) else as.numeric(as.logical(done))
  .morie_gr_need(
    length(mask) == length(s),
    "done must have one flag per transition."
  )
  v_s <- V[s + 1L]
  v_next <- V[s_next + 1L]
  target <- r + gamma * (1 - mask) * v_next
  adv <- target - v_s
  list(
    advantage = adv, td_target = target, value_s = v_s,
    value_s_next = v_next, critic_loss = mean(adv^2), gamma = gamma,
    estimate = mean(adv), n = length(adv),
    method = "Actor-critic one-step advantage (A2C)"
  )
}

#' Double DQN targets (Geron Ch 19, morie.fn grddqn)
#'
#' y = r + gamma Q_target(s', argmax_a Q_online(s', a)); the vanilla max
#' target and the removed overestimation are returned alongside.
#' `selected_action` is 0-based.
#'
#' @param Q_online,Q_target Matrices (S, A).
#' @param s_next 0-based indices.
#' @param r Rewards.
#' @param gamma Discount.
#' @param done Optional flags.
#' @return List with `target`, `selected_action`, `q_eval`,
#'   `vanilla_target`, `overestimation_gap`.
#' @export
morie_geron_double_dqn_target <- function(Q_online, Q_target, s_next, r, gamma,
                                          done = NULL) {
  Qo <- .morie_gr_mat(Q_online, "Q_online")
  Qt <- .morie_gr_mat(Q_target, "Q_target")
  .morie_gr_need(all(dim(Qo) == dim(Qt)), "Q_online shape must match Q_target.")
  .morie_gr_need(length(Qo) > 0L, "Q tables are empty.")
  S <- nrow(Qo)
  sn <- as.integer(as.vector(s_next))
  rew <- as.numeric(r)
  .morie_gr_need(length(sn) == length(rew), "s_next and r must have equal length.")
  .morie_gr_need(length(sn) > 0L, "no transitions supplied.")
  .morie_gr_need(min(sn) >= 0L && max(sn) < S, "s_next indices out of range.")
  .morie_gr_need(all(is.finite(rew)), "r contains non-finite values.")
  gamma <- as.numeric(gamma)
  .morie_gr_need(gamma >= 0 && gamma <= 1, "gamma must lie in [0, 1].")
  cont <- if (is.null(done)) rep(1, length(sn)) else 1 - as.numeric(as.logical(done))
  .morie_gr_need(
    length(cont) == length(sn),
    "done must have one flag per transition."
  )
  a_star <- apply(Qo[sn + 1L, , drop = FALSE], 1, which.max) - 1L
  q_eval <- Qt[cbind(sn + 1L, a_star + 1L)]
  target <- rew + gamma * cont * q_eval
  vanilla <- rew + gamma * cont * apply(Qt[sn + 1L, , drop = FALSE], 1, max)
  list(
    target = target, selected_action = a_star, q_eval = q_eval,
    vanilla_target = vanilla, overestimation_gap = vanilla - target,
    gamma = gamma, estimate = mean(target), n = length(sn),
    method = "Double DQN target"
  )
}

#' Value iteration on the Bellman optimality operator (Geron Ch 19, hmbel)
#'
#' V*(s) = max_a \[R(s,a) + gamma sum_s' P(s'|s,a) V*(s')\]. `policy` is
#' 0-based.
#'
#' @param V Initial values (S).
#' @param P Array (S, A, S).
#' @param R Matrix (S, A).
#' @param gamma Discount in \[0, 1).
#' @param tol,max_iter Stopping rule.
#' @return List with `V`, `policy`, `Q`, `iterations`, `residual`, `converged`.
#' @export
morie_geron_value_iteration <- function(V, P, R, gamma, tol = 1e-10,
                                        max_iter = 10000) {
  Vv <- as.numeric(V)
  Pm <- P
  .morie_gr_need(
    length(dim(Pm)) == 3L,
    "geron_bellman_optimality: P must be 3-D (S, A, S)"
  )
  storage.mode(Pm) <- "double"
  S <- dim(Pm)[1]
  A <- dim(Pm)[2]
  S2 <- dim(Pm)[3]
  .morie_gr_need(
    S > 0L && A > 0L,
    "geron_bellman_optimality: P must have at least one state and one action"
  )
  .morie_gr_need(
    S == S2,
    "geron_bellman_optimality: P must be square in the state axes"
  )
  Rm <- matrix(as.numeric(R), S, A)
  .morie_gr_need(
    length(Vv) == S,
    "geron_bellman_optimality: V entries != P states"
  )
  .morie_gr_need(
    !any(Pm < 0),
    "geron_bellman_optimality: P contains negative probabilities"
  )
  rowsum <- apply(Pm, c(1, 2), sum)
  .morie_gr_need(
    all(abs(rowsum - 1) <= 1e-8),
    "geron_bellman_optimality: a P[s, a, ] row does not sum to 1"
  )
  g <- as.numeric(gamma)
  .morie_gr_need(
    g >= 0 && g < 1,
    "geron_bellman_optimality: gamma must lie in [0, 1)"
  )
  residual <- Inf
  it <- 0L
  for (i in seq_len(as.integer(max_iter))) {
    it <- i
    Q <- Rm + g * apply(Pm, c(1, 2), function(row) sum(row * Vv))
    Vn <- apply(Q, 1, max)
    residual <- max(abs(Vn - Vv))
    Vv <- Vn
    if (residual <= tol) break
  }
  Q <- Rm + g * apply(Pm, c(1, 2), function(row) sum(row * Vv))
  policy <- apply(Q, 1, which.max) - 1L
  list(
    V = Vv, policy = policy, Q = Q, iterations = it, residual = residual,
    converged = residual <= tol, gamma = g, estimate = max(Vv), n = S,
    method = "Value iteration on the Bellman optimality operator"
  )
}

#' Q-value iteration (Geron Ch 19, morie.fn grbo)
#'
#' Q*(s,a) = sum_s' T(s,a,s')\[R(s,a,s') + gamma max_a' Q*(s',a')\]. Rows
#' of `transitions` may sum to 1 (available) or 0 (unavailable).
#' `policy` is 0-based.
#'
#' @param Q Initial table (S, A).
#' @param transitions Array (S, A, S).
#' @param rewards Array (S, A, S) or matrix (S, A).
#' @param gamma Discount.
#' @param max_iter,tol Stopping rule.
#' @return List with `Q`, `V`, `policy`, `residual`, `iterations`, `converged`.
#' @export
morie_geron_q_value_iteration <- function(Q, transitions, rewards, gamma,
                                          max_iter = 1000, tol = 1e-10) {
  Q <- .morie_gr_mat(Q, "Q")
  .morie_gr_need(length(Q) > 0L, "Q must be a non-empty 2-D (S, A) array.")
  S <- nrow(Q)
  A <- ncol(Q)
  T_ <- transitions
  .morie_gr_need(
    length(dim(T_)) == 3L && all(dim(T_) == c(S, A, S)),
    "transitions must have shape (S, A, S)."
  )
  storage.mode(T_) <- "double"
  R <- rewards
  if (is.matrix(R) || length(dim(R)) != 3L) {
    Rm <- matrix(as.numeric(R), S, A)
    R <- array(0, dim = c(S, A, S))
    for (k in seq_len(S)) R[, , k] <- Rm
  }
  .morie_gr_need(
    all(dim(R) == c(S, A, S)),
    "rewards must have shape (S, A, S) or (S, A)."
  )
  storage.mode(R) <- "double"
  .morie_gr_need(!any(T_ < 0), "transition probabilities must be non-negative.")
  rowsum <- apply(T_, c(1, 2), sum)
  .morie_gr_need(
    all(abs(rowsum - 1) < 1e-8 | abs(rowsum) < 1e-8),
    "each (s, a) row must sum to 1 (available) or 0 (unavailable)."
  )
  .morie_gr_need(
    all(is.finite(R)) && all(is.finite(Q)),
    "Q and rewards must be finite."
  )
  gamma <- as.numeric(gamma)
  .morie_gr_need(gamma >= 0 && gamma <= 1, "gamma must lie in [0, 1].")
  max_iter <- as.integer(max_iter)
  .morie_gr_need(max_iter >= 1L, "max_iter must be at least 1.")
  tol <- as.numeric(tol)
  .morie_gr_need(tol > 0, "tol must be positive.")
  ER <- apply(T_ * R, c(1, 2), sum)
  residual <- Inf
  it <- 0L
  for (i in seq_len(max_iter)) {
    it <- i
    Vv <- apply(Q, 1, max)
    Q_new <- ER + gamma * apply(T_, c(1, 2), function(row) sum(row * Vv))
    residual <- max(abs(Q_new - Q))
    Q <- Q_new
    if (residual <= tol) break
  }
  Vv <- apply(Q, 1, max)
  policy <- apply(Q, 1, which.max) - 1L
  list(
    Q = Q, V = Vv, policy = policy, residual = residual, iterations = it,
    converged = residual <= tol, gamma = gamma, estimate = mean(Vv), n = S,
    method = "Q-value iteration (Bellman optimality)"
  )
}

#' Deterministic DDIM sampling step (Geron Ch 18, morie.fn grddim)
#'
#' x0 = (x_t - sqrt(1 - ab_t) eps) / sqrt(ab_t), then re-noise to t_prev
#' with eta = 0. Timestep indices are 0-based into `alpha_bar`.
#'
#' @param x_t Current sample.
#' @param t,t_prev 0-based timesteps, t_prev < t.
#' @param eps_pred Noise prediction.
#' @param alpha_bar Non-increasing schedule in (0, 1\].
#' @param clip_x0 Optional c(lo, hi) clamp on x0.
#' @return List with `x_prev`, `x0_pred`, `alpha_bar_t`, `alpha_bar_prev`,
#'   `signal_scale`, `noise_scale`.
#' @export
morie_geron_ddim_sampling_step <- function(x_t, t, t_prev, eps_pred, alpha_bar,
                                           clip_x0 = NULL) {
  x_t <- as.numeric(x_t)
  eps <- as.numeric(eps_pred)
  ab <- as.numeric(alpha_bar)
  .morie_gr_need(length(eps) == length(x_t), "eps_pred shape must match x_t.")
  .morie_gr_need(length(x_t) > 0L, "x_t is empty.")
  .morie_gr_need(
    all(is.finite(x_t)) && all(is.finite(eps)),
    "x_t and eps_pred must be finite."
  )
  .morie_gr_need(length(ab) >= 2L, "alpha_bar needs at least 2 entries.")
  .morie_gr_need(
    !any(ab <= 0) && !any(ab > 1),
    "alpha_bar entries must lie in (0, 1]."
  )
  .morie_gr_need(!any(diff(ab) > 1e-12), "alpha_bar must be non-increasing in t.")
  t <- as.integer(t)
  t_prev <- as.integer(t_prev)
  .morie_gr_need(t >= 0L && t < length(ab), "t out of range for alpha_bar.")
  .morie_gr_need(
    t_prev >= 0L && t_prev < length(ab),
    "t_prev out of range for alpha_bar."
  )
  .morie_gr_need(t_prev < t, "t_prev must be strictly below t.")
  ab_t <- ab[t + 1L]
  ab_p <- ab[t_prev + 1L]
  x0 <- (x_t - sqrt(1 - ab_t) * eps) / sqrt(ab_t)
  if (!is.null(clip_x0)) {
    lo <- as.numeric(clip_x0)[1]
    hi <- as.numeric(clip_x0)[2]
    .morie_gr_need(lo < hi, "clip_x0 must be (lo, hi) with lo < hi.")
    x0 <- pmin(pmax(x0, lo), hi)
  }
  sig <- sqrt(ab_p)
  noi <- sqrt(1 - ab_p)
  x_prev <- sig * x0 + noi * eps
  list(
    x_prev = x_prev, x0_pred = x0, alpha_bar_t = ab_t, alpha_bar_prev = ab_p,
    signal_scale = sig, noise_scale = noi, t = t, t_prev = t_prev,
    estimate = mean(x_prev), n = length(x_t),
    method = "DDIM deterministic sampling step"
  )
}

#' Advantage actor-critic, A2C (Geron Ch 19, morie.fn hma2c)
#'
#' Linear softmax policy plus linear value baseline, trained by
#' Monte-Carlo advantage. `env` is a list with `reset()` and
#' `step(action)` (action arrives 0-based, as in Python) returning
#' list(state, reward, done). Actions are sampled from the same integer
#' LCG stream as Python, so runs match token for token.
#'
#' @param env List with reset/step.
#' @param actor Matrix (n_actions, n_features).
#' @param critic Value weights.
#' @param epochs Episodes.
#' @param lr Actor step.
#' @param gamma Discount.
#' @param critic_lr Critic step.
#' @param max_steps Episode cap.
#' @param seed LCG seed.
#' @return List with `actor`, `critic`, `returns`, `policy`, `value`,
#'   `advantages`.
#' @export
morie_geron_a2c <- function(env, actor, critic, epochs = 100, lr = 0.1,
                            gamma = 0.99, critic_lr = NULL, max_steps = 200,
                            seed = 0) {
  reset <- env$reset
  step <- env$step
  .morie_gr_need(
    is.function(reset) && is.function(step),
    "geron_a2c: env must provide callable reset() and step(action)"
  )
  A <- .morie_gr_mat(actor, "geron_a2c: actor")
  n_actions <- nrow(A)
  n_feat <- ncol(A)
  .morie_gr_need(n_actions >= 2L, "geron_a2c: actor must define at least 2 actions")
  V <- as.numeric(critic)
  .morie_gr_need(length(V) == n_feat, "geron_a2c: critic weights != actor features")
  E <- as.integer(epochs)
  .morie_gr_need(E >= 1L, "geron_a2c: epochs must be >= 1")
  alpha <- as.numeric(lr)
  .morie_gr_need(
    is.finite(alpha) && alpha > 0,
    "geron_a2c: lr must be a positive finite step size"
  )
  g <- as.numeric(gamma)
  .morie_gr_need(g >= 0 && g <= 1, "geron_a2c: gamma must lie in [0, 1]")
  beta <- if (is.null(critic_lr)) alpha else as.numeric(critic_lr)
  .morie_gr_need(beta > 0, "geron_a2c: critic_lr must be positive")
  M <- as.integer(max_steps)
  .morie_gr_need(M >= 1L, "geron_a2c: max_steps must be >= 1")
  s_state <- as.numeric(seed) %% 2^32
  draw <- function() {
    s_state <<- .morie_al_lcg(s_state)
    (s_state + 0.5) / 2^32
  }
  ep_returns <- numeric(E)
  last_adv <- numeric(0)
  for (ep in seq_len(E)) {
    states <- list()
    acts <- integer(0)
    rewards <- numeric(0)
    s <- as.numeric(reset())
    .morie_gr_need(length(s) == n_feat, "geron_a2c: env.reset() feature mismatch")
    for (i in seq_len(M)) {
      probs <- .morie_gr_softmax(as.numeric(A %*% s))
      u <- draw()
      a <- sum(cumsum(probs) <= u)
      a <- min(a, n_actions - 1L)
      out <- step(a)
      .morie_gr_need(
        length(out) == 3L,
        "geron_a2c: env.step(action) must return a (state, reward, done) triple"
      )
      s2 <- as.numeric(out[[1]])
      r <- as.numeric(out[[2]])
      done <- as.logical(out[[3]])
      .morie_gr_need(length(s2) == n_feat, "geron_a2c: env.step() feature mismatch")
      states[[length(states) + 1L]] <- s
      acts <- c(acts, a)
      rewards <- c(rewards, r)
      s <- s2
      if (done) break
    }
    .morie_gr_need(length(states) > 0L, "geron_a2c: env produced an empty episode")
    S <- do.call(rbind, states)
    Tn <- length(rewards)
    G <- numeric(Tn)
    accum <- 0
    for (t in seq.int(Tn, 1L)) {
      accum <- rewards[t] + g * accum
      G[t] <- accum
    }
    ep_returns[ep] <- sum(rewards)
    baseline <- as.numeric(S %*% V)
    adv <- G - baseline
    last_adv <- adv
    gradA <- matrix(0, n_actions, n_feat)
    for (t in seq_len(length(acts))) {
      probs <- .morie_gr_softmax(as.numeric(A %*% S[t, ]))
      onehot <- numeric(n_actions)
      onehot[acts[t] + 1L] <- 1
      gradA <- gradA + adv[t] * outer(onehot - probs, S[t, ])
    }
    A <- A + alpha * gradA / length(acts)
    V <- V + beta * as.numeric(crossprod(S, adv)) / length(acts)
  }
  policy <- function(state) {
    st <- as.numeric(state)
    .morie_gr_need(length(st) == n_feat, "policy: feature count mismatch")
    .morie_gr_softmax(as.numeric(A %*% st))
  }
  value <- function(state) {
    st <- as.numeric(state)
    .morie_gr_need(length(st) == n_feat, "value: feature count mismatch")
    sum(V * st)
  }
  list(
    actor = A, critic = V, returns = ep_returns, policy = policy,
    value = value, advantages = last_adv, estimate = ep_returns[E], n = E,
    method = "A2C with a linear softmax policy and a linear value baseline"
  )
}

#' Asynchronous advantage actor-critic, A3C (Geron Ch 19, morie.fn hma3c)
#'
#' Workers are stepped in deterministic round-robin: each pulls the
#' shared parameters, rolls out one episode, and pushes its gradient into
#' parameters other workers may have already moved -- the staleness that
#' makes A3C asynchronous. Worker w uses LCG seed + 7919 w.
#'
#' @param env List with reset/step, or a function(worker_id) returning one.
#' @param actor,critic Initial parameters.
#' @param n_workers Workers.
#' @param lr,critic_lr Step sizes.
#' @param epochs Episodes per worker.
#' @param gamma Discount.
#' @param max_steps Episode cap.
#' @param seed Base seed.
#' @return List with `actor`, `critic`, `returns`, `worker_returns`,
#'   `policy`, `updates`.
#' @export
morie_geron_a3c <- function(env, actor, critic, n_workers = 4, lr = 0.1,
                            epochs = 50, gamma = 0.99, critic_lr = NULL,
                            max_steps = 200, seed = 0) {
  A <- .morie_gr_mat(actor, "geron_a3c: actor")
  n_actions <- nrow(A)
  n_feat <- ncol(A)
  .morie_gr_need(n_actions >= 2L, "geron_a3c: actor must define at least 2 actions")
  V <- as.numeric(critic)
  .morie_gr_need(length(V) == n_feat, "geron_a3c: critic weights != actor features")
  W <- as.integer(n_workers)
  .morie_gr_need(W >= 1L, "geron_a3c: n_workers must be >= 1")
  E <- as.integer(epochs)
  .morie_gr_need(E >= 1L, "geron_a3c: epochs must be >= 1")
  alpha <- as.numeric(lr)
  .morie_gr_need(is.finite(alpha) && alpha > 0, "geron_a3c: lr must be positive")
  g <- as.numeric(gamma)
  .morie_gr_need(g >= 0 && g <= 1, "geron_a3c: gamma must lie in [0, 1]")
  beta <- if (is.null(critic_lr)) alpha else as.numeric(critic_lr)
  .morie_gr_need(beta > 0, "geron_a3c: critic_lr must be positive")
  M <- as.integer(max_steps)
  .morie_gr_need(M >= 1L, "geron_a3c: max_steps must be >= 1")
  envs <- lapply(seq_len(W) - 1L, function(w) {
    e <- if (is.function(env) && is.null(env$reset)) env(w) else env
    .morie_gr_need(
      is.function(e$reset) && is.function(e$step),
      "geron_a3c: env must provide callable reset() and step(action)"
    )
    e
  })
  states_seed <- (as.numeric(seed) + 7919 * (seq_len(W) - 1L)) %% 2^32
  shared_A <- A
  shared_V <- V
  worker_returns <- matrix(0, W, E)
  updates <- 0L
  for (ep in seq_len(E)) {
    for (w in seq_len(W)) {
      reset <- envs[[w]]$reset
      step <- envs[[w]]$step
      localA <- shared_A
      localV <- shared_V
      states <- list()
      acts <- integer(0)
      rewards <- numeric(0)
      s <- as.numeric(reset())
      .morie_gr_need(length(s) == n_feat, "geron_a3c: env.reset() feature mismatch")
      for (i in seq_len(M)) {
        probs <- .morie_gr_softmax(as.numeric(localA %*% s))
        states_seed[w] <- .morie_al_lcg(states_seed[w])
        u <- (states_seed[w] + 0.5) / 2^32
        a <- min(sum(cumsum(probs) <= u), n_actions - 1L)
        out <- step(a)
        .morie_gr_need(
          length(out) == 3L,
          "geron_a3c: env.step(action) must return a (state, reward, done) triple"
        )
        s2 <- as.numeric(out[[1]])
        r <- as.numeric(out[[2]])
        done <- as.logical(out[[3]])
        .morie_gr_need(length(s2) == n_feat, "geron_a3c: env.step() feature mismatch")
        states[[length(states) + 1L]] <- s
        acts <- c(acts, a)
        rewards <- c(rewards, r)
        s <- s2
        if (done) break
      }
      .morie_gr_need(length(states) > 0L, "geron_a3c: env produced an empty episode")
      S <- do.call(rbind, states)
      Tn <- length(rewards)
      G <- numeric(Tn)
      accum <- 0
      for (t in seq.int(Tn, 1L)) {
        accum <- rewards[t] + g * accum
        G[t] <- accum
      }
      adv <- G - as.numeric(S %*% localV)
      gradA <- matrix(0, n_actions, n_feat)
      for (t in seq_len(length(acts))) {
        probs <- .morie_gr_softmax(as.numeric(localA %*% S[t, ]))
        onehot <- numeric(n_actions)
        onehot[acts[t] + 1L] <- 1
        gradA <- gradA + adv[t] * outer(onehot - probs, S[t, ])
      }
      shared_A <- shared_A + alpha * gradA / length(acts)
      shared_V <- shared_V + beta * as.numeric(crossprod(S, adv)) / length(acts)
      worker_returns[w, ep] <- sum(rewards)
      updates <- updates + 1L
    }
  }
  policy <- function(state) {
    st <- as.numeric(state)
    .morie_gr_need(length(st) == n_feat, "policy: feature count mismatch")
    .morie_gr_softmax(as.numeric(shared_A %*% st))
  }
  returns <- colMeans(worker_returns)
  list(
    actor = shared_A, critic = shared_V, returns = returns,
    worker_returns = worker_returns, policy = policy, updates = updates,
    estimate = returns[E], n = W * E,
    method = "A3C: round-robin workers pushing advantage gradients into shared parameters"
  )
}

# ------------------------------------------------ clustering, trees, ensembles

#' DBSCAN core / border / noise points (Geron Ch 8, morie.fn grdbs)
#'
#' The eps-neighbourhood INCLUDES the point itself, so min_samples = 1
#' makes everything core. `neighbors` entries are 0-based index vectors.
#'
#' @param X Points (m, d).
#' @param eps Positive radius.
#' @param min_samples Density threshold, >= 1.
#' @param metric "euclidean", "manhattan" or "chebyshev".
#' @return List with `is_core`, `is_border`, `is_noise`, `neighbor_counts`,
#'   `neighbors`, `n_core`, `n_noise`.
#' @export
morie_geron_dbscan_core_point <- function(X, eps, min_samples,
                                          metric = "euclidean") {
  X <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(X) <- "double"
  .morie_gr_need(length(X) > 0L, "X must be a non-empty 2-D (m, d) array.")
  .morie_gr_need(all(is.finite(X)), "X contains non-finite values.")
  eps <- as.numeric(eps)
  .morie_gr_need(is.finite(eps) && eps > 0, "eps must be a positive finite float.")
  min_samples <- as.integer(min_samples)
  .morie_gr_need(min_samples >= 1L, "min_samples must be at least 1.")
  m <- nrow(X)
  D <- matrix(0, m, m)
  for (i in seq_len(m)) {
    for (j in seq_len(m)) {
      d <- X[i, ] - X[j, ]
      D[i, j] <- switch(metric,
        euclidean = sqrt(sum(d^2)),
        manhattan = sum(abs(d)),
        chebyshev = max(abs(d)),
        stop("metric must be 'euclidean', 'manhattan' or 'chebyshev'.",
          call. = FALSE
        )
      )
    }
  }
  within <- D <= eps
  counts <- rowSums(within)
  core <- counts >= min_samples
  border <- (!core) & apply(within & matrix(core, m, m, byrow = TRUE), 1, any)
  noise <- (!core) & (!border)
  neighbors <- lapply(seq_len(m), function(i) which(within[i, ]) - 1L)
  list(
    is_core = core, is_border = border, is_noise = noise,
    neighbor_counts = as.integer(counts), neighbors = neighbors,
    n_core = sum(core), n_noise = sum(noise), eps = eps,
    min_samples = min_samples, estimate = mean(core), n = m,
    method = "DBSCAN core-point identification"
  )
}

#' Agglomerative hierarchical clustering (Geron Ch 8, morie.fn hmagc)
#'
#' Clusters are numbered by the index of their lowest-indexed member, so
#' `labels` is deterministic regardless of merge order. `merges` entries
#' are 0-based (min member of a, min member of b).
#'
#' @param X Observations (n, d).
#' @param n_clusters Stop count.
#' @param linkage "single", "complete", "average" or "centroid".
#' @return List with `labels`, `merges`, `heights`, `n_clusters`, `distances`.
#' @export
morie_geron_agglomerative <- function(X, n_clusters = 2, linkage = "single") {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(A) <- "double"
  n <- nrow(A)
  .morie_gr_need(n > 0L, "geron_agglomerative: X has no rows")
  .morie_gr_need(all(is.finite(A)), "geron_agglomerative: X must be finite")
  k <- as.integer(n_clusters)
  .morie_gr_need(
    k >= 1L && k <= n,
    "geron_agglomerative: n_clusters must lie in [1, n]"
  )
  .morie_gr_need(
    linkage %in% c("single", "complete", "average", "centroid"),
    "geron_agglomerative: unknown linkage"
  )
  D <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) D[i, j] <- sqrt(sum((A[i, ] - A[j, ])^2))
  members <- lapply(seq_len(n), function(i) i)
  active <- seq_len(n)
  merges <- list()
  heights <- numeric(0)
  cluster_dist <- function(a, b) {
    sub <- D[members[[a]], members[[b]], drop = FALSE]
    switch(linkage,
      single = min(sub),
      complete = max(sub),
      average = mean(sub),
      centroid = {
        ca <- colMeans(A[members[[a]], , drop = FALSE])
        cb <- colMeans(A[members[[b]], , drop = FALSE])
        sqrt(sum((ca - cb)^2))
      }
    )
  }
  while (length(active) > k) {
    best <- NULL
    best_d <- Inf
    for (ii in seq_len(length(active) - 1L)) {
      for (jj in seq.int(ii + 1L, length(active))) {
        a <- active[ii]
        b <- active[jj]
        dab <- cluster_dist(a, b)
        if (dab < best_d) {
          best_d <- dab
          best <- c(a, b)
        }
      }
    }
    a <- best[1]
    b <- best[2]
    merges[[length(merges) + 1L]] <- c(
      min(members[[a]]) - 1L,
      min(members[[b]]) - 1L
    )
    heights <- c(heights, best_d)
    members[[a]] <- sort(c(members[[a]], members[[b]]))
    members[[b]] <- integer(0)
    active <- active[active != b]
  }
  labels <- integer(n)
  ordered <- active[order(vapply(active, function(c) min(members[[c]]), numeric(1)))]
  for (new_id in seq_along(ordered)) labels[members[[ordered[new_id]]]] <- new_id - 1L
  list(
    labels = labels, merges = merges, heights = heights,
    n_clusters = length(active), linkage = linkage, distances = D,
    estimate = if (length(heights)) heights[length(heights)] else 0, n = n,
    method = sprintf("Agglomerative clustering with %s linkage", linkage)
  )
}

#' BIRCH clustering (Geron Ch 8, morie.fn hmbrch)
#'
#' Phase 1 streams points into leaf clustering features (N, LS, SS),
#' absorbing into the nearest CF whose radius would stay under
#' `threshold`; phase 3 does centroid-linkage agglomerative on the CF
#' centroids. Labels are 0-based.
#'
#' @param X Points (n, d).
#' @param n_clusters Final count or NULL.
#' @param threshold Maximum sub-cluster radius.
#' @param branching_factor Leaf cap.
#' @return List with `labels`, `subcluster_centers`, `subcluster_labels`,
#'   `subcluster_sizes`, `n_subclusters`, `radii`, `n_leaves`.
#' @export
morie_geron_birch <- function(X, n_clusters = 3, threshold = 0.5,
                              branching_factor = 50) {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(A) <- "double"
  n <- nrow(A)
  .morie_gr_need(n > 0L, "geron_birch: X has no rows")
  .morie_gr_need(all(is.finite(A)), "geron_birch: X must be finite")
  t_ <- as.numeric(threshold)
  .morie_gr_need(is.finite(t_) && t_ > 0, "geron_birch: threshold must be positive")
  B <- as.integer(branching_factor)
  .morie_gr_need(B >= 2L, "geron_birch: branching_factor must be >= 2")
  k <- if (is.null(n_clusters)) NULL else as.integer(n_clusters)
  .morie_gr_need(is.null(k) || k >= 1L, "geron_birch: n_clusters must be >= 1 or NULL")
  # leaves[[l]] is a list of CFs; a CF is list(n, ls, ss, members).
  leaves <- list(list())
  radius_if_added <- function(cf, x) {
    nn <- cf$n + 1L
    ls <- cf$ls + x
    ss <- cf$ss + sum(x * x)
    sqrt(max(ss / nn - sum(ls * ls) / (nn * nn), 0))
  }
  for (i in seq_len(n)) {
    x <- A[i, ]
    best_leaf <- NA_integer_
    best_entry <- NA_integer_
    best_d <- Inf
    for (li in seq_along(leaves)) {
      leaf <- leaves[[li]]
      if (!length(leaf)) next
      for (ei in seq_along(leaf)) {
        cf <- leaf[[ei]]
        d <- sqrt(sum((cf$ls / cf$n - x)^2))
        if (d < best_d && radius_if_added(cf, x) <= t_) {
          best_d <- d
          best_leaf <- li
          best_entry <- ei
        }
      }
    }
    if (!is.na(best_leaf)) {
      cf <- leaves[[best_leaf]][[best_entry]]
      cf$n <- cf$n + 1L
      cf$ls <- cf$ls + x
      cf$ss <- cf$ss + sum(x * x)
      cf$members <- c(cf$members, i)
      leaves[[best_leaf]][[best_entry]] <- cf
    } else {
      target <- NA_integer_
      for (li in seq_along(leaves)) {
        if (length(leaves[[li]]) < B) {
          target <- li
          break
        }
      }
      if (is.na(target)) {
        leaves[[length(leaves) + 1L]] <- list()
        target <- length(leaves)
      }
      leaves[[target]][[length(leaves[[target]]) + 1L]] <-
        list(n = 1L, ls = x, ss = sum(x * x), members = i)
    }
  }
  cfs <- unlist(leaves, recursive = FALSE)
  centers <- do.call(rbind, lapply(cfs, function(cf) cf$ls / cf$n))
  radii <- vapply(cfs, function(cf) {
    sqrt(max(cf$ss / cf$n - sum(cf$ls * cf$ls) / (cf$n * cf$n), 0))
  }, numeric(1))
  m <- length(cfs)
  if (is.null(k) || k >= m) {
    sub_labels <- seq_len(m) - 1L
  } else {
    groups <- lapply(seq_len(m), function(i) i)
    active <- seq_len(m)
    while (length(active) > k) {
      best <- NULL
      best_d <- Inf
      for (ii in seq_len(length(active) - 1L)) {
        for (jj in seq.int(ii + 1L, length(active))) {
          a <- active[ii]
          b <- active[jj]
          ca <- colMeans(centers[groups[[a]], , drop = FALSE])
          cb <- colMeans(centers[groups[[b]], , drop = FALSE])
          d <- sqrt(sum((ca - cb)^2))
          if (d < best_d) {
            best_d <- d
            best <- c(a, b)
          }
        }
      }
      a <- best[1]
      b <- best[2]
      groups[[a]] <- c(groups[[a]], groups[[b]])
      groups[[b]] <- integer(0)
      active <- active[active != b]
    }
    sub_labels <- integer(m)
    ordered <- active[order(vapply(active, function(gg) min(groups[[gg]]), numeric(1)))]
    for (new_id in seq_along(ordered)) sub_labels[groups[[ordered[new_id]]]] <- new_id - 1L
  }
  labels <- integer(n)
  for (ci in seq_len(m)) labels[cfs[[ci]]$members] <- sub_labels[ci]
  list(
    labels = labels, subcluster_centers = centers,
    subcluster_labels = sub_labels,
    subcluster_sizes = vapply(cfs, function(cf) cf$n, numeric(1)),
    n_subclusters = m, radii = radii, n_leaves = length(leaves),
    estimate = m, n = n,
    method = "BIRCH: CF-tree leaf summarisation then centroid-linkage global clustering"
  )
}

#' Discrete AdaBoost (Geron Ch 6, morie.fn hmadab)
#'
#' alpha_t = 0.5 log((1 - err)/err); w <- w exp(-alpha y f(x)), renormalised.
#' Default weak learner is a weighted 1-split stump.
#'
#' @param X Design matrix.
#' @param y Two-class labels.
#' @param base_estimator Optional function(X, y, w) -> predict(X) `in {-1, 1}`.
#' @param n_estimators Rounds.
#' @param eps Error floor/ceiling in (0, 0.5).
#' @return List with `alphas`, `errors`, `train_errors`, `predict`,
#'   `decision`, `margin`, `weights`, `classes`.
#' @export
morie_geron_adaboost <- function(X, y, base_estimator = NULL,
                                 n_estimators = 10, eps = 1e-10) {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(A) <- "double"
  n <- nrow(A)
  .morie_gr_need(n > 0L, "geron_adaboost: X has no rows")
  yy <- as.vector(y)
  .morie_gr_need(length(yy) == n, "geron_adaboost: X rows != y entries")
  classes <- sort(unique(yy))
  .morie_gr_need(
    length(classes) == 2L,
    "geron_adaboost: discrete AdaBoost needs exactly 2 classes"
  )
  ys <- ifelse(yy == classes[2], 1, -1)
  M <- as.integer(n_estimators)
  .morie_gr_need(M >= 1L, "geron_adaboost: n_estimators must be >= 1")
  e <- as.numeric(eps)
  .morie_gr_need(e > 0 && e < 0.5, "geron_adaboost: eps must lie in (0, 0.5)")
  fit_stump <- function(w) {
    best <- list(err = Inf, j = 1L, thr = -Inf, pol = 1)
    for (j in seq_len(ncol(A))) {
      vals <- sort(unique(A[, j]))
      thrs <- if (length(vals) > 1L) {
        (utils::head(vals, -1) + utils::tail(vals, -1)) / 2
      } else {
        vals
      }
      for (thr in thrs) {
        left <- A[, j] <= thr
        for (pol in c(1, -1)) {
          pred <- ifelse(left, pol, -pol)
          err <- sum(w[pred != ys])
          if (err < best$err) best <- list(err = err, j = j, thr = thr, pol = pol)
        }
      }
    }
    best
  }
  w <- rep(1 / n, n)
  predictors <- list()
  alphas <- numeric(0)
  errs <- numeric(0)
  train_errs <- numeric(0)
  F_ <- numeric(n)
  for (m in seq_len(M)) {
    if (is.null(base_estimator)) {
      st <- fit_stump(w)
      pred <- ifelse(A[, st$j] <= st$thr, st$pol, -st$pol)
      err <- st$err
      fitted <- (function(j, thr, pol) {
        function(Anew) {
          Bm <- if (is.matrix(Anew)) Anew else matrix(as.numeric(Anew), ncol = ncol(A))
          ifelse(Bm[, j] <= thr, pol, -pol)
        }
      })(st$j, st$thr, st$pol)
    } else {
      fitted <- base_estimator(A, ys, w)
      .morie_gr_need(
        is.function(fitted),
        "geron_adaboost: base_estimator must return a callable predictor"
      )
      pred <- as.numeric(fitted(A))
      .morie_gr_need(
        length(pred) == n,
        "geron_adaboost: weak learner prediction count mismatch"
      )
      .morie_gr_need(
        all(pred %in% c(-1, 1)),
        "geron_adaboost: weak learner must return labels in {-1, +1}"
      )
      err <- sum(w[pred != ys])
    }
    err_c <- min(max(err, e), 1 - e)
    alpha <- 0.5 * log((1 - err_c) / err_c)
    w <- w * exp(-alpha * ys * pred)
    w <- w / sum(w)
    predictors[[length(predictors) + 1L]] <- fitted
    alphas <- c(alphas, alpha)
    errs <- c(errs, err)
    F_ <- F_ + alpha * pred
    train_errs <- c(train_errs, mean(sign(F_) != ys))
  }
  predict_fn <- function(Xnew) {
    Bm <- if (is.matrix(Xnew)) Xnew else matrix(as.numeric(Xnew), ncol = ncol(A))
    s <- numeric(nrow(Bm))
    for (i in seq_along(predictors)) s <- s + alphas[i] * as.numeric(predictors[[i]](Bm))
    ifelse(s >= 0, classes[2], classes[1])
  }
  denom <- sum(abs(alphas))
  list(
    alphas = alphas, errors = errs, train_errors = train_errs,
    predict = predict_fn, decision = F_,
    margin = if (denom > 0) ys * F_ / denom else numeric(n),
    weights = w, classes = classes,
    estimate = train_errs[length(train_errs)], n = n,
    method = "Discrete AdaBoost with reweighted weak learners"
  )
}

#' AdaBoost sample-weight update (Geron Ch 6, morie.fn gradaw)
#'
#' w <- w exp(alpha `1{misclassified}`), then renormalise. The weighted
#' error BEFORE the update is reported so alpha can be checked against
#' log((1-r)/r).
#'
#' @param y_true,y_pred Label vectors of equal length.
#' @param weights Non-negative sample weights.
#' @param alpha_t Predictor weight of the round just fitted.
#' @return List with `weights_new`, `misclassified`, `weighted_error`,
#'   `boost_factor`.
#' @export
morie_geron_adaboost_weight_update <- function(y_true, y_pred, weights,
                                               alpha_t) {
  y_true <- as.vector(y_true)
  y_pred <- as.vector(y_pred)
  w <- as.numeric(weights)
  .morie_gr_need(
    length(y_true) == length(y_pred),
    "y_true and y_pred must have equal length."
  )
  .morie_gr_need(
    length(y_true) == length(w),
    "weights must have one entry per sample."
  )
  .morie_gr_need(length(y_true) > 0L, "no samples supplied.")
  .morie_gr_need(all(w >= 0), "weights must be non-negative.")
  total <- sum(w)
  .morie_gr_need(
    is.finite(total) && total > 0,
    "weights must sum to a positive finite value."
  )
  alpha_t <- as.numeric(alpha_t)
  .morie_gr_need(is.finite(alpha_t), "alpha_t must be finite.")
  wrong <- as.numeric(y_true != y_pred)
  weighted_error <- sum(w * wrong) / total
  w_new <- w * exp(alpha_t * wrong)
  new_total <- sum(w_new)
  .morie_gr_need(
    is.finite(new_total) && new_total > 0,
    "weight update overflowed; alpha_t is too large."
  )
  w_new <- w_new / new_total
  list(
    weights_new = w_new, misclassified = wrong > 0,
    weighted_error = weighted_error, boost_factor = exp(alpha_t),
    alpha_t = alpha_t, estimate = weighted_error, n = length(y_true),
    method = "AdaBoost sample-weight update"
  )
}

#' Bagging over LCG-seeded bootstrap replicates (Geron Ch 6, morie.fn hmbag)
#'
#' Bootstrap indices come from the exact-integer LCG, index m using seed
#' + 7919 m, so replicates are byte-identical across machines. Default
#' weak learner is a least-squares 1-split stump.
#'
#' @param X,y Training data.
#' @param base_estimator Optional function(Xb, yb).
#' @param n_estimators Replicates.
#' @param seed LCG seed.
#' @param task "auto", "regression" or "classification".
#' @return List with `predict`, `train_pred`, `train_mse`, `oob_pred`,
#'   `oob_mse`, `oob_coverage`, `member_preds`, `task`.
#' @export
morie_geron_bagging <- function(X, y, base_estimator = NULL, n_estimators = 10,
                                seed = 0, task = "auto") {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(A) <- "double"
  n <- nrow(A)
  .morie_gr_need(n > 0L, "geron_bagging: X has no rows")
  yv <- as.numeric(y)
  .morie_gr_need(length(yv) == n, "geron_bagging: X rows != y entries")
  M <- as.integer(n_estimators)
  .morie_gr_need(M >= 1L, "geron_bagging: n_estimators must be >= 1")
  .morie_gr_need(
    task %in% c("auto", "regression", "classification"),
    "geron_bagging: unknown task"
  )
  classify <- task == "classification" ||
    (task == "auto" && all(unique(yv) %in% c(0, 1)))
  lcg_indices <- function(count, seed) {
    s <- as.numeric(seed) %% 2^32
    out <- integer(count)
    for (i in seq_len(count)) {
      s <- .morie_al_lcg(s)
      out[i] <- floor(s * n / 2^32)
    }
    out
  }
  stump <- function(Xb, yb) {
    best <- list(sse = Inf, j = 1L, thr = Inf, lp = mean(yb), rp = mean(yb))
    for (j in seq_len(ncol(Xb))) {
      vals <- sort(unique(Xb[, j]))
      if (length(vals) < 2L) next
      for (thr in (utils::head(vals, -1) + utils::tail(vals, -1)) / 2) {
        left <- Xb[, j] <= thr
        if (!any(left) || all(left)) next
        lv <- yb[left]
        rv <- yb[!left]
        lp <- mean(lv)
        rp <- mean(rv)
        sse <- sum((lv - lp)^2) + sum((rv - rp)^2)
        if (sse < best$sse) best <- list(sse = sse, j = j, thr = thr, lp = lp, rp = rp)
      }
    }
    lp <- best$lp
    rp <- best$rp
    if (classify) {
      lp <- if (lp >= 0.5) 1 else 0
      rp <- if (rp >= 0.5) 1 else 0
    }
    j <- best$j
    thr <- best$thr
    function(Anew) {
      Bm <- if (is.matrix(Anew)) Anew else matrix(as.numeric(Anew), ncol = ncol(A))
      ifelse(Bm[, j] <= thr, lp, rp)
    }
  }
  models <- list()
  oob_sum <- numeric(n)
  oob_cnt <- numeric(n)
  train_stack <- matrix(0, M, n)
  for (m in seq_len(M)) {
    idx <- lcg_indices(n, seed + 7919 * (m - 1L)) + 1L
    Xb <- A[idx, , drop = FALSE]
    yb <- yv[idx]
    f <- if (is.null(base_estimator)) stump(Xb, yb) else base_estimator(Xb, yb)
    .morie_gr_need(
      is.function(f),
      "geron_bagging: base_estimator must return a callable predictor"
    )
    pm <- as.numeric(f(A))
    .morie_gr_need(length(pm) == n, "geron_bagging: estimator prediction count mismatch")
    models[[m]] <- f
    train_stack[m, ] <- pm
    oob <- setdiff(seq_len(n), unique(idx))
    if (length(oob)) {
      oob_sum[oob] <- oob_sum[oob] + pm[oob]
      oob_cnt[oob] <- oob_cnt[oob] + 1
    }
  }
  aggregate <- function(P) if (classify) as.numeric(colMeans(P) >= 0.5) else colMeans(P)
  predict_fn <- function(Xnew) {
    Bm <- if (is.matrix(Xnew)) Xnew else matrix(as.numeric(Xnew), ncol = ncol(A))
    .morie_gr_need(ncol(Bm) == ncol(A), "predict: feature count mismatch")
    P <- do.call(rbind, lapply(models, function(f) as.numeric(f(Bm))))
    aggregate(P)
  }
  train_pred <- aggregate(train_stack)
  train_mse <- mean((train_pred - yv)^2)
  has_oob <- oob_cnt > 0
  oob_pred <- rep(NA_real_, n)
  oob_pred[has_oob] <- oob_sum[has_oob] / oob_cnt[has_oob]
  oob_mse <- if (any(has_oob)) mean((oob_pred[has_oob] - yv[has_oob])^2) else NA_real_
  list(
    predict = predict_fn, train_pred = train_pred, train_mse = train_mse,
    oob_pred = oob_pred, oob_mse = oob_mse, oob_coverage = mean(has_oob),
    estimators = models, member_preds = train_stack,
    task = if (classify) "classification" else "regression",
    estimate = train_mse, n = n,
    method = "Bagging over LCG-seeded bootstrap replicates"
  )
}

#' Bagging aggregator over member predictions (Geron Ch 6, morie.fn grbag)
#'
#' Mean / median / hard-vote aggregation, with the across-predictor
#' variance (ddof = 1, matching grbag) as the disagreement diagnostic.
#'
#' @param predictions Matrix (B, m).
#' @param aggregate "mean", "median", "vote".
#' @return List with `prediction`, `per_instance_variance`,
#'   `mean_disagreement`, `n_predictors`, `se`.
#' @export
morie_geron_bagging_predictor <- function(predictions, aggregate = "mean") {
  P <- .morie_gr_mat(predictions, "predictions")
  .morie_gr_need(length(P) > 0L, "predictions is empty.")
  B <- nrow(P)
  m <- ncol(P)
  agg <- switch(aggregate,
    mean = colMeans(P),
    median = apply(P, 2, stats::median),
    vote = vapply(seq_len(m), function(j) {
      vals <- sort(unique(P[, j]))
      counts <- vapply(vals, function(v) sum(P[, j] == v), numeric(1))
      vals[which.max(counts)]
    }, numeric(1)),
    stop("aggregate must be one of 'mean', 'median', 'vote'.", call. = FALSE)
  )
  vr <- if (B > 1L) apply(P, 2, stats::var) else numeric(m)
  se <- if (B > 1L) sqrt(mean(vr) / B) else NA_real_
  list(
    prediction = agg, per_instance_variance = vr,
    mean_disagreement = mean(vr), n_predictors = B, aggregate = aggregate,
    se = se, estimate = mean(agg), n = m,
    method = "Bagging ensemble aggregation"
  )
}

#' K-fold cross-validation score (Geron Ch 1/2, morie.fn grcvs)
#'
#' Folds follow numpy's array_split: the first m %% K folds get one extra
#' row. Default model is OLS via minimum-norm least squares, scored by
#' R^2. `shuffle` is not supported, because numpy's default_rng stream
#' cannot be reproduced in R -- passing TRUE is an error rather than a
#' silently different split.
#'
#' @param X,y Data.
#' @param K Folds, 2 <= K <= m.
#' @param fit,predict,score Optional callables.
#' @param shuffle Must be FALSE.
#' @param random_state Ignored.
#' @return List with `cv_score`, `fold_scores`, `fold_sizes`, `se`,
#'   `worst_fold`, `spread`.
#' @export
morie_geron_cross_validation_score <- function(X, y, K, fit = NULL,
                                               predict = NULL, score = NULL,
                                               shuffle = FALSE,
                                               random_state = NULL) {
  X <- .morie_gr_mat(X, "X")
  y <- as.numeric(y)
  .morie_gr_need(nrow(X) == length(y), "X rows != y entries.")
  .morie_gr_need(length(X) > 0L, "X is empty.")
  m <- nrow(X)
  K <- as.integer(K)
  .morie_gr_need(K >= 2L, "K must be at least 2 folds.")
  .morie_gr_need(K <= m, "K exceeds the available observations.")
  .morie_gr_need(
    !isTRUE(shuffle),
    "shuffle=TRUE is not portable: numpy's default_rng stream has no R equivalent."
  )
  .morie_gr_need(
    !(is.null(fit) && !is.null(predict)),
    "predict was supplied without fit."
  )
  .morie_gr_need(
    !(!is.null(fit) && is.null(predict)),
    "fit was supplied without predict."
  )
  fit_fn <- if (is.null(fit)) function(Xtr, ytr) .morie_gr_lstsq(Xtr, ytr) else fit
  pred_fn <- if (is.null(predict)) function(model, Xte) as.numeric(Xte %*% model) else predict
  score_fn <- if (is.null(score)) {
    function(yt, yp) {
      ss_res <- sum((yt - yp)^2)
      ss_tot <- sum((yt - mean(yt))^2)
      .morie_gr_need(
        ss_tot != 0,
        "a validation fold has zero target variance, so R^2 is undefined."
      )
      1 - ss_res / ss_tot
    }
  } else {
    score
  }
  idx <- seq_len(m)
  folds <- .morie_gr_array_split(idx, K)
  scores <- numeric(0)
  sizes <- integer(0)
  for (k in seq_len(K)) {
    test_idx <- folds[[k]]
    train_idx <- setdiff(idx, test_idx)
    .morie_gr_need(length(train_idx) > 0L, "a fold leaves no training data.")
    model <- fit_fn(X[train_idx, , drop = FALSE], y[train_idx])
    yp <- as.numeric(pred_fn(model, X[test_idx, , drop = FALSE]))
    .morie_gr_need(
      length(yp) == length(test_idx),
      "predict returned the wrong number of predictions."
    )
    s <- as.numeric(score_fn(y[test_idx], yp))
    .morie_gr_need(is.finite(s), "a fold score is not finite.")
    scores <- c(scores, s)
    sizes <- c(sizes, length(test_idx))
  }
  se <- if (K > 1L) stats::sd(scores) / sqrt(K) else NA_real_
  list(
    cv_score = mean(scores), fold_scores = scores, fold_sizes = sizes,
    se = se, worst_fold = which.min(scores) - 1L,
    spread = max(scores) - min(scores), K = K, estimate = mean(scores),
    n = m, method = "K-fold cross-validation"
  )
}

# -------------------------------------------------------------- autoencoders

#' Linear undercomplete autoencoder solved by SVD (Geron Ch 18, hmaen)
#'
#' For a linear autoencoder the optimum is spanned by the top principal
#' components, so the encoder is the leading right singular vectors and
#' the decoder their transpose. SVD sign is arbitrary and differs between
#' LAPACK builds, so `codes` and `encoder` carry a per-column sign
#' convention; `reconstruction`, `recon_error` and
#' `explained_variance_ratio` are sign-invariant and are what parity
#' tests should assert.
#'
#' @param X Training data (n, d).
#' @param bottleneck Code width in \[1, d\].
#' @param center Subtract feature means (this is what makes it PCA).
#' @return List with `encoder`, `decoder`, `codes`, `reconstruction`,
#'   `recon_error`, `explained_variance_ratio`, `mean`, `encode`, `decode`.
#' @export
morie_geron_autoencoder <- function(X, bottleneck, center = TRUE) {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(A) <- "double"
  n <- nrow(A)
  d <- ncol(A)
  .morie_gr_need(n > 0L, "geron_autoencoder: X has no rows")
  .morie_gr_need(all(is.finite(A)), "geron_autoencoder: X must be finite")
  k <- as.integer(bottleneck)
  .morie_gr_need(k >= 1L && k <= d, "geron_autoencoder: bottleneck out of range")
  mean_ <- if (center) colMeans(A) else numeric(d)
  Xc <- sweep(A, 2, mean_, "-")
  sv <- svd(Xc)
  W1 <- t(sv$v)[seq_len(k), , drop = FALSE]
  W2 <- t(W1)
  codes <- Xc %*% W2
  recon <- sweep(codes %*% W1, 2, mean_, "+")
  err <- mean(rowSums((A - recon)^2))
  total <- sum(sv$d^2)
  evr <- if (total > 0) sv$d^2 / total else numeric(length(sv$d))
  encode <- function(Xnew) {
    B <- if (is.matrix(Xnew)) Xnew else matrix(as.numeric(Xnew), nrow = 1)
    .morie_gr_need(ncol(B) == d, "encode: feature count mismatch")
    sweep(B, 2, mean_, "-") %*% W2
  }
  decode <- function(C) {
    Cm <- if (is.matrix(C)) C else matrix(as.numeric(C), nrow = 1)
    .morie_gr_need(ncol(Cm) == k, "decode: code width mismatch")
    sweep(Cm %*% W1, 2, mean_, "+")
  }
  list(
    encoder = W1, decoder = W2, codes = codes, reconstruction = recon,
    recon_error = err, explained_variance_ratio = evr, mean = mean_,
    encode = encode, decode = decode, estimate = err, n = n,
    method = "Linear undercomplete autoencoder solved in closed form by SVD (equals PCA)"
  )
}

#' Autoencoder reconstruction loss and compression (Geron Ch 18, grael)
#'
#' loss is the mean squared L2 norm PER SAMPLE, `mse_per_element` the
#' plain elementwise mean; compression_ratio = input_dim / code_dim.
#'
#' @param X Inputs (m, d).
#' @param encoded Codes (m, k).
#' @param decoded Reconstructions.
#' @return List with `loss`, `mse_per_element`, `per_sample_loss`,
#'   `code_dim`, `input_dim`, `compression_ratio`, `explained_variance`.
#' @export
morie_geron_autoencoder_reconstruction_loss <- function(X, encoded, decoded) {
  X <- .morie_gr_mat(X, "X")
  decoded <- .morie_gr_mat(decoded, "decoded")
  encoded <- if (is.matrix(encoded)) encoded else matrix(as.numeric(encoded), nrow = 1)
  .morie_gr_need(all(dim(X) == dim(decoded)), "decoded shape must match X shape.")
  .morie_gr_need(length(X) > 0L, "X is empty.")
  .morie_gr_need(nrow(encoded) == nrow(X), "encoded rows != X rows.")
  resid <- X - decoded
  per_sample <- rowSums(resid^2)
  vr <- sum(sweep(X, 2, colMeans(X), "-")^2)
  ev <- if (vr == 0) NA_real_ else 1 - sum(resid^2) / vr
  list(
    loss = mean(per_sample), mse_per_element = mean(resid^2),
    per_sample_loss = per_sample, code_dim = ncol(encoded),
    input_dim = ncol(X), compression_ratio = ncol(X) / ncol(encoded),
    explained_variance = ev, estimate = mean(per_sample), n = nrow(X),
    method = "Autoencoder reconstruction loss"
  )
}

#' Denoising autoencoder loss against the clean input (Geron Ch 18, grdae)
#'
#' Scoring against x rather than x_tilde is the point: copying the input
#' is now a losing strategy. `denoising_gain` = noise_energy / loss.
#'
#' @param x Clean inputs (m, d).
#' @param noise Corruption, broadcastable, or
#'   a 0/1 keep-mask when `corruption = "dropout"`.
#' @param decoded Reconstructions.
#' @param corruption "additive" or "dropout".
#' @return List with `loss`, `mse_per_element`, `per_sample_loss`,
#'   `x_tilde`, `noise_energy`, `denoising_gain`, `snr_db`.
#' @export
morie_geron_denoising_autoencoder <- function(x, noise, decoded,
                                              corruption = "additive") {
  x <- .morie_gr_mat(x, "x")
  decoded <- .morie_gr_mat(decoded, "decoded")
  .morie_gr_need(length(x) > 0L, "x is empty.")
  .morie_gr_need(all(dim(x) == dim(decoded)), "decoded shape must match x shape.")
  nb <- as.numeric(noise)
  .morie_gr_need(
    length(nb) == length(x) || length(nb) == 1L ||
      length(nb) == ncol(x),
    "noise is not broadcastable to x."
  )
  noise_b <- if (length(nb) == length(x)) {
    matrix(as.numeric(noise), nrow(x), ncol(x))
  } else {
    matrix(nb, nrow(x), ncol(x), byrow = TRUE)
  }
  .morie_gr_need(all(is.finite(noise_b)), "noise must be finite.")
  x_tilde <- if (corruption == "additive") {
    x + noise_b
  } else if (corruption == "dropout") {
    .morie_gr_need(
      all(noise_b == 0 | noise_b == 1),
      "with corruption='dropout', noise must be a 0/1 keep-mask."
    )
    x * noise_b
  } else {
    stop("corruption must be 'additive' or 'dropout'.", call. = FALSE)
  }
  resid <- x - decoded
  per_sample <- rowSums(resid^2)
  loss <- mean(per_sample)
  corrupt_err <- mean(rowSums((x - x_tilde)^2))
  sig <- mean(rowSums(x^2))
  snr <- if (corrupt_err == 0) Inf else 10 * log10(sig / corrupt_err)
  list(
    loss = loss, mse_per_element = mean(resid^2),
    per_sample_loss = per_sample, x_tilde = x_tilde,
    noise_energy = corrupt_err,
    denoising_gain = if (loss == 0) Inf else corrupt_err / loss,
    snr_db = snr, corruption = corruption, estimate = loss, n = nrow(x),
    method = "Denoising autoencoder reconstruction loss"
  )
}

#' .morie_gr_conv2d_valid
#'
#' A step of the geron_ml_native implementation. Called by \code{morie_geron_convolutional_autoencoder}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Z A matrix; indexed by row and column.
#' @param K A matrix; passed to \code{nrow}.
#' @param s Numeric; combined arithmetically in the body.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_gr_conv2d_valid <- function(Z, K, s) {
  kh <- nrow(K)
  kw <- ncol(K)
  h <- nrow(Z)
  w <- ncol(Z)
  .morie_gr_need(
    h >= kh && w >= kw,
    "encoder kernel does not fit the feature map."
  )
  oh <- ((h - kh) %/% s) + 1L
  ow <- ((w - kw) %/% s) + 1L
  out <- matrix(0, oh, ow)
  for (i in seq_len(oh)) {
    for (j in seq_len(ow)) {
      r0 <- (i - 1L) * s
      c0 <- (j - 1L) * s
      out[i, j] <- sum(Z[(r0 + 1L):(r0 + kh), (c0 + 1L):(c0 + kw), drop = FALSE] * K)
    }
  }
  out
}

#' .morie_gr_conv_transpose2d
#'
#' A step of the geron_ml_native implementation. Called by \code{morie_geron_convolutional_autoencoder}, \code{morie_geron_dcgan_generator}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Z A matrix; indexed by row and column.
#' @param K A matrix; passed to \code{nrow}.
#' @param s Numeric; combined arithmetically in the body.
#' @return The value of \code{out}, as built in the body.
#' @export
.morie_gr_conv_transpose2d <- function(Z, K, s) {
  kh <- nrow(K)
  kw <- ncol(K)
  h <- nrow(Z)
  w <- ncol(Z)
  out <- matrix(0, (h - 1L) * s + kh, (w - 1L) * s + kw)
  for (i in seq_len(h)) {
    for (j in seq_len(w)) {
      r0 <- (i - 1L) * s
      c0 <- (j - 1L) * s
      out[(r0 + 1L):(r0 + kh), (c0 + 1L):(c0 + kw)] <-
        out[(r0 + 1L):(r0 + kh), (c0 + 1L):(c0 + kw)] + Z[i, j] * K
    }
  }
  out
}

#' Convolutional autoencoder forward pass (Geron Ch 18, morie.fn grcae)
#'
#' Strided-conv encoder with ReLU after every layer; transposed-conv
#' decoder with ReLU after every layer but the last. Transposed conv
#' gives (n-1)s + k per axis, so a decoder that does not land back on
#' the input size is an error rather than a silent crop.
#'
#' @param x Image (H, W).
#' @param encoder_weights,decoder_weights Lists of kernels.
#' @param stride Shared stride.
#' @param output_activation "identity", "tanh", "sigmoid".
#' @return List with `x_hat`, `code`, `code_shape`, `loss`, `mse_per_pixel`,
#'   `compression_ratio`.
#' @export
morie_geron_convolutional_autoencoder <- function(x, encoder_weights,
                                                  decoder_weights, stride = 2,
                                                  output_activation = "identity") {
  x <- .morie_gr_mat(x, "x")
  .morie_gr_need(length(x) > 0L, "x must be a non-empty 2-D image.")
  enc <- lapply(encoder_weights, function(K) .morie_gr_mat(K, "encoder_weights"))
  dec <- lapply(decoder_weights, function(K) .morie_gr_mat(K, "decoder_weights"))
  .morie_gr_need(length(enc) > 0L, "encoder_weights is empty.")
  .morie_gr_need(length(dec) > 0L, "decoder_weights is empty.")
  stride <- as.integer(stride)
  .morie_gr_need(stride >= 1L, "stride must be positive.")
  acts <- list(
    identity = function(a) a, tanh = tanh,
    sigmoid = function(a) 1 / (1 + exp(-a))
  )
  .morie_gr_need(
    output_activation %in% names(acts),
    "output_activation must be one of identity, sigmoid, tanh."
  )
  z <- x
  for (K in enc) z <- pmax(.morie_gr_conv2d_valid(z, K, stride), 0)
  code <- z
  a <- code
  for (i in seq_along(dec)) {
    a <- .morie_gr_conv_transpose2d(a, dec[[i]], stride)
    if (i < length(dec)) a <- pmax(a, 0)
  }
  x_hat <- acts[[output_activation]](a)
  .morie_gr_need(
    all(dim(x_hat) == dim(x)),
    "the decoder reconstruction shape does not match the input; adjust kernels or stride."
  )
  resid <- x - x_hat
  loss <- sum(resid^2)
  list(
    x_hat = x_hat, code = code, code_shape = dim(code), loss = loss,
    mse_per_pixel = mean(resid^2),
    compression_ratio = length(x) / length(code), estimate = loss,
    n = length(x), method = "Convolutional autoencoder forward pass"
  )
}

#' Autoencoder anomaly detection (Geron Ch 8, morie.fn hmanae)
#'
#' Flags rows whose squared reconstruction error exceeds `threshold`;
#' with no threshold the empirical `quantile` of the errors is used,
#' which calibrates the detector on the data being scored.
#'
#' @param model Function(X) -> reconstruction, or a list with
#'   `reconstruct`, `predict`, or `encode` + `decode`.
#' @param X Data to score.
#' @param threshold Optional squared-error cut.
#' @param quantile Quantile in (0, 1\] used when `threshold` is NULL.
#' @return List with `errors`, `is_anomaly`, `threshold`,
#'   `threshold_calibrated`, `n_anomalies`, `reconstruction`.
#' @export
morie_geron_anomaly_autoencoder <- function(model, X, threshold = NULL,
                                            quantile = 0.99) {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), ncol = 1)
  storage.mode(A) <- "double"
  .morie_gr_need(nrow(A) > 0L, "geron_anomaly_autoencoder: X has no rows")
  .morie_gr_need(all(is.finite(A)), "geron_anomaly_autoencoder: X must be finite")
  recon <- if (is.function(model)) {
    model(A)
  } else if (is.function(model$reconstruct)) {
    model$reconstruct(A)
  } else if (is.function(model$predict)) {
    model$predict(A)
  } else if (is.function(model$encode) && is.function(model$decode)) {
    model$decode(model$encode(A))
  } else {
    stop("geron_anomaly_autoencoder: model must be callable or expose reconstruct/predict, or encode+decode",
      call. = FALSE
    )
  }
  recon <- if (is.matrix(recon)) recon else matrix(as.numeric(recon), ncol = 1)
  storage.mode(recon) <- "double"
  .morie_gr_need(
    all(dim(recon) == dim(A)),
    "geron_anomaly_autoencoder: model reconstruction shape mismatch"
  )
  .morie_gr_need(
    all(is.finite(recon)),
    "geron_anomaly_autoencoder: model returned non-finite reconstructions"
  )
  errors <- rowSums((A - recon)^2)
  if (is.null(threshold)) {
    q <- as.numeric(quantile)
    .morie_gr_need(
      q > 0 && q <= 1,
      "geron_anomaly_autoencoder: quantile must lie in (0, 1]"
    )
    thr <- as.numeric(stats::quantile(errors, q, type = 7, names = FALSE))
    calibrated <- TRUE
  } else {
    thr <- as.numeric(threshold)
    .morie_gr_need(
      is.finite(thr) && thr >= 0,
      "geron_anomaly_autoencoder: threshold must be a finite non-negative squared error"
    )
    calibrated <- FALSE
  }
  flags <- errors > thr
  list(
    errors = errors, is_anomaly = flags, threshold = thr,
    threshold_calibrated = calibrated, n_anomalies = sum(flags),
    reconstruction = recon, estimate = mean(errors), n = nrow(A),
    method = "Autoencoder anomaly detection by squared reconstruction error"
  )
}

#' DCGAN generator forward pass (Geron Ch 18, morie.fn grdcgan)
#'
#' Latent -> projection -> spatial seed -> transposed-conv stack, batch
#' norm + ReLU on every layer but the last, tanh on the output (which is
#' why DCGAN data is scaled to \[-1, 1\]). The batch norm here is the
#' whole-tensor population form, as in Python.
#'
#' @param z Latent vector.
#' @param weights List: projection matrix then kernels.
#' @param seed_shape Optional c(h0, w0).
#' @param stride Upsampling stride.
#' @param batch_norm Apply batch norm on hidden layers.
#' @return List with `image`, `image_shape`, `seed`, `layer_shapes`,
#'   `upsample_factor`, `latent_dim`.
#' @export
morie_geron_dcgan_generator <- function(z, weights, seed_shape = NULL,
                                        stride = 2, batch_norm = TRUE) {
  z <- as.numeric(z)
  .morie_gr_need(length(z) > 0L, "z is empty.")
  .morie_gr_need(all(is.finite(z)), "z contains non-finite values.")
  ws <- as.list(weights)
  .morie_gr_need(
    length(ws) >= 2L,
    "weights must hold a projection matrix followed by at least one transposed-conv kernel."
  )
  W0 <- .morie_gr_mat(ws[[1]], "the projection matrix")
  .morie_gr_need(nrow(W0) == length(z), "projection rows != z entries.")
  flat <- as.numeric(z %*% W0)
  if (is.null(seed_shape)) {
    side <- round(sqrt(length(flat)))
    .morie_gr_need(
      side * side == length(flat),
      "projection size is not a perfect square; pass seed_shape."
    )
    seed_shape <- c(side, side)
  }
  h0 <- as.integer(seed_shape[1])
  w0 <- as.integer(seed_shape[2])
  .morie_gr_need(
    h0 >= 1L && w0 >= 1L && h0 * w0 == length(flat),
    "seed_shape does not match the projected units."
  )
  stride <- as.integer(stride)
  .morie_gr_need(stride >= 1L, "stride must be positive.")
  # numpy reshape(h0, w0) is row-major.
  A <- matrix(flat, h0, w0, byrow = TRUE)
  seed_mat <- A
  shapes <- list(c(h0, w0))
  kernels <- ws[-1]
  bn <- function(M) {
    mu <- mean(M)
    sd_ <- .morie_gr_psd(as.numeric(M))
    if (sd_ < sqrt(1e-5)) M - mu else (M - mu) / sqrt(sd_^2 + 1e-5)
  }
  for (i in seq_along(kernels)) {
    K <- .morie_gr_mat(kernels[[i]], "a decoder kernel")
    A <- .morie_gr_conv_transpose2d(A, K, stride)
    if (i < length(kernels)) {
      if (batch_norm) A <- bn(A)
      A <- pmax(A, 0)
    }
    shapes[[length(shapes) + 1L]] <- dim(A)
  }
  image <- tanh(A)
  list(
    image = image, image_shape = dim(image), seed = seed_mat,
    layer_shapes = shapes, upsample_factor = nrow(image) / h0,
    latent_dim = length(z), estimate = mean(image), n = length(image),
    method = "DCGAN generator forward pass"
  )
}

#' Auxiliary-task pretraining then fine-tuning (Geron Ch 11, hmauxpt)
#'
#' Gradient descent on the auxiliary task, then the SAME parameter vector
#' is fine-tuned on the target; the from-scratch control gets an
#' identical budget, so `transfer_gain` is measured, not asserted.
#'
#' @param model Initial parameters or NULL for zeros.
#' @param aux_data,target_data Lists of (X, y).
#' @param aux_epochs,epochs Gradient steps.
#' @param lr Step size.
#' @return List with `theta`, `theta_pretrained`, `theta_scratch`,
#'   `target_loss`, `scratch_loss`, `transfer_gain`, `aux_losses`,
#'   `finetune_losses`, `scratch_losses`.
#' @export
morie_geron_auxiliary_task_pretraining <- function(model, aux_data,
                                                   target_data,
                                                   aux_epochs = 200,
                                                   epochs = 20, lr = 0.05) {
  pair <- function(name, data) {
    .morie_gr_need(
      is.list(data) && length(data) == 2L,
      paste0("geron_auxiliary_task_pretraining: ", name, " must be a (X, y) pair")
    )
    Xd <- if (is.matrix(data[[1]])) data[[1]] else matrix(as.numeric(data[[1]]), ncol = 1)
    storage.mode(Xd) <- "double"
    yd <- as.numeric(data[[2]])
    .morie_gr_need(
      nrow(Xd) > 0L,
      paste0("geron_auxiliary_task_pretraining: ", name, " X must be non-empty")
    )
    .morie_gr_need(
      length(yd) == nrow(Xd),
      paste0("geron_auxiliary_task_pretraining: ", name, " row/target mismatch")
    )
    .morie_gr_need(
      all(is.finite(Xd)) && all(is.finite(yd)),
      paste0("geron_auxiliary_task_pretraining: ", name, " must be finite")
    )
    list(X = Xd, y = yd)
  }
  aux <- pair("aux_data", aux_data)
  tgt <- pair("target_data", target_data)
  .morie_gr_need(
    ncol(aux$X) == ncol(tgt$X),
    "geron_auxiliary_task_pretraining: aux and target feature counts differ"
  )
  d <- ncol(aux$X)
  theta0 <- if (is.null(model)) numeric(d) else as.numeric(model)
  .morie_gr_need(
    length(theta0) == d,
    "geron_auxiliary_task_pretraining: model parameter count mismatch"
  )
  AE <- as.integer(aux_epochs)
  FE <- as.integer(epochs)
  .morie_gr_need(
    AE >= 1L && FE >= 1L,
    "geron_auxiliary_task_pretraining: aux_epochs and epochs must both be >= 1"
  )
  step <- as.numeric(lr)
  .morie_gr_need(
    is.finite(step) && step > 0,
    "geron_auxiliary_task_pretraining: lr must be a positive finite step size"
  )
  gd <- function(theta, X, y, lr, ep) {
    losses <- numeric(ep)
    n <- nrow(X)
    for (e in seq_len(ep)) {
      resid <- as.numeric(X %*% theta) - y
      losses[e] <- sum(resid * resid) / n
      theta <- theta - lr * (2 / n) * as.numeric(crossprod(X, resid))
    }
    list(theta = theta, losses = losses)
  }
  pre <- gd(theta0, aux$X, aux$y, step, AE)
  ft <- gd(pre$theta, tgt$X, tgt$y, step, FE)
  sc <- gd(theta0, tgt$X, tgt$y, step, FE)
  mse <- function(th) {
    r <- as.numeric(tgt$X %*% th) - tgt$y
    sum(r * r) / length(tgt$y)
  }
  target_loss <- mse(ft$theta)
  scratch_loss <- mse(sc$theta)
  list(
    theta = ft$theta, theta_pretrained = pre$theta, theta_scratch = sc$theta,
    target_loss = target_loss, scratch_loss = scratch_loss,
    transfer_gain = scratch_loss - target_loss, aux_losses = pre$losses,
    finetune_losses = ft$losses, scratch_losses = sc$losses,
    estimate = target_loss, n = length(tgt$y),
    method = "Auxiliary-task pretraining then fine-tuning, against an equal-budget scratch control"
  )
}

#' Fine-tune a frozen encoder with a softmax head (Geron Ch 15, hmbftn)
#'
#' `bert` is any function(X) -> (n, d) pooled embeddings. The head is
#' trained by full-batch gradient descent with the exact gradient
#' X^T (p - onehot) / n. Labels and predictions are 0-based.
#'
#' @param bert Embedding function.
#' @param X Inputs passed to `bert`.
#' @param y 0-based class labels.
#' @param epochs Steps.
#' @param lr Step size.
#' @param l2 Non-negative penalty on W (bias unpenalised).
#' @return List with `W`, `b`, `losses`, `accuracy`, `predict`,
#'   `probabilities`, `embeddings`.
#' @export
morie_geron_bert_finetune <- function(bert, X, y, epochs = 100, lr = 0.1,
                                      l2 = 0) {
  .morie_gr_need(
    is.function(bert),
    "geron_bert_finetune: bert must be callable, returning pooled [CLS] embeddings"
  )
  labels <- as.vector(y)
  .morie_gr_need(length(labels) > 0L, "geron_bert_finetune: y is empty")
  .morie_gr_need(
    all(labels == floor(as.numeric(labels))),
    "geron_bert_finetune: y must contain integer class labels"
  )
  labels <- as.integer(labels)
  .morie_gr_need(
    min(labels) >= 0L,
    "geron_bert_finetune: class labels must be non-negative"
  )
  Z <- bert(X)
  Z <- if (is.matrix(Z)) Z else matrix(as.numeric(Z), ncol = 1)
  storage.mode(Z) <- "double"
  n <- nrow(Z)
  dim_ <- ncol(Z)
  .morie_gr_need(
    n == length(labels),
    "geron_bert_finetune: embedding count != label count"
  )
  .morie_gr_need(
    all(is.finite(Z)),
    "geron_bert_finetune: bert returned non-finite embeddings"
  )
  K <- max(labels) + 1L
  .morie_gr_need(
    K >= 2L,
    "geron_bert_finetune: need at least 2 classes to fine-tune a classifier head"
  )
  EP <- as.integer(epochs)
  .morie_gr_need(EP >= 1L, "geron_bert_finetune: epochs must be >= 1")
  step <- as.numeric(lr)
  .morie_gr_need(
    is.finite(step) && step > 0,
    "geron_bert_finetune: lr must be a positive finite step size"
  )
  lam <- as.numeric(l2)
  .morie_gr_need(lam >= 0, "geron_bert_finetune: l2 must be non-negative")
  W <- matrix(0, dim_, K)
  b <- numeric(K)
  Y <- matrix(0, n, K)
  Y[cbind(seq_len(n), labels + 1L)] <- 1
  losses <- numeric(EP)
  for (e in seq_len(EP)) {
    p <- .morie_al_softmax_rows(Z %*% W + matrix(b, n, K, byrow = TRUE))
    losses[e] <- -mean(log(pmax(p[cbind(seq_len(n), labels + 1L)], 1e-15))) +
      0.5 * lam * sum(W * W)
    G <- (p - Y) / n
    W <- W - step * (crossprod(Z, G) + lam * W)
    b <- b - step * colSums(G)
  }
  p_final <- .morie_al_softmax_rows(Z %*% W + matrix(b, n, K, byrow = TRUE))
  pred <- apply(p_final, 1, which.max) - 1L
  acc <- mean(pred == labels)
  predict_fn <- function(Xnew) {
    Zn <- bert(Xnew)
    Zn <- if (is.matrix(Zn)) Zn else matrix(as.numeric(Zn), nrow = 1)
    .morie_gr_need(ncol(Zn) == dim_, "predict: encoder width mismatch")
    apply(Zn %*% W + matrix(b, nrow(Zn), K, byrow = TRUE), 1, which.max) - 1L
  }
  list(
    W = W, b = b, losses = losses, accuracy = acc, predict = predict_fn,
    probabilities = p_final, embeddings = Z, estimate = losses[EP], n = n,
    method = "Softmax classification head fine-tuned on pooled [CLS] embeddings"
  )
}

# ------------------------------------------------------------------- BF16
#
# R has no float32 type, but writeBin/readBin with size = 4 round-trip a
# double through IEEE-754 binary32, and reading the same bytes back as a
# 32-bit integer exposes the bit pattern. Endianness is pinned to little
# so the port is machine-independent.

#' .morie_gr_f32_bits
#'
#' A step of the geron_ml_native implementation. Called by \code{.morie_gr_f32}, \code{morie_geron_bf16}, \code{morie_geron_bf16_range}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A vector; its length is taken.
#' @return The value of \code{ifelse}.
#' @export
.morie_gr_f32_bits <- function(x) {
  raw4 <- writeBin(as.double(x), raw(), size = 4, endian = "little")
  i <- readBin(raw4, "integer", n = length(x), size = 4, endian = "little")
  u <- as.numeric(i)
  ifelse(u < 0, u + 2^32, u)
}

#' .morie_gr_bits_f32
#'
#' A step of the geron_ml_native implementation. Called by \code{.morie_gr_f32}, \code{morie_geron_bf16}, \code{morie_geron_bf16_range}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param u A vector; its length is taken.
#' @return The value of \code{readBin}.
#' @export
.morie_gr_bits_f32 <- function(u) {
  signed <- ifelse(u >= 2^31, u - 2^32, u)
  raw4 <- writeBin(as.integer(signed), raw(), size = 4, endian = "little")
  readBin(raw4, "double", n = length(u), size = 4, endian = "little")
}

#' .morie_gr_f32
#'
#' A step of the geron_ml_native implementation. Called by \code{morie_geron_bf16}, \code{morie_geron_bf16_range}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Passed to \code{.morie_gr_f32_bits}.
#' @return The value of \code{.morie_gr_bits_f32}.
#' @export
.morie_gr_f32 <- function(x) .morie_gr_bits_f32(.morie_gr_f32_bits(x))

# Round the float32 bit pattern to the BF16 grid. mode is
# "nearest_even" or "truncate"; arithmetic is done in doubles and folded
# back mod 2^32, exactly reproducing numpy's uint32/uint64 masking.
#' Round the float32 bit pattern to the BF16 grid. mode is
#'
#' "nearest_even" or "truncate"; arithmetic is done in doubles and
#' folded back mod 2^32, exactly reproducing numpy\'s uint32/uint64
#' masking.
#'
#' @param u Numeric; combined arithmetically in the body.
#' @param mode Compared against \code{"nearest_even"}. Defaults to \code{"nearest_even"}.
#' @return A numeric value.
#' @export
.morie_gr_bf16_bits <- function(u, mode = "nearest_even") {
  if (mode == "nearest_even") {
    lsb <- floor(u / 2^16) %% 2
    u <- (u + 32767 + lsb) %% 2^32
  }
  floor(u / 65536) * 65536
}

#' BF16 quantisation of float32 values (Geron Appendix B, morie.fn hmbf16)
#'
#' Keeps the top 16 bits of the binary32 encoding: 1 sign, 8 exponent,
#' 7 mantissa bits. Default rounding is round-to-nearest ties-to-even, as
#' hardware does; "truncate" drops the low bits. NaN lanes are restored
#' so a rounding carry cannot turn a NaN into an infinity.
#'
#' @param x Values to quantise.
#' @param rounding "nearest_even" or "truncate".
#' @return List with `values`, `bits` (16-character strings), `abs_error`,
#'   `rel_error`, `max_rel_error`, `mantissa_bits`, `exponent_bits`.
#' @export
morie_geron_bf16 <- function(x, rounding = "nearest_even") {
  .morie_gr_need(
    rounding %in% c("nearest_even", "truncate"),
    "geron_bf16: rounding must be 'nearest_even' or 'truncate'"
  )
  arr <- .morie_gr_f32(as.numeric(x))
  .morie_gr_need(length(arr) > 0L, "geron_bf16: input is empty")
  u <- .morie_gr_f32_bits(arr)
  out_bits <- .morie_gr_bf16_bits(u, rounding)
  nan_mask <- is.nan(arr)
  if (any(nan_mask)) out_bits[nan_mask] <- floor(u[nan_mask] / 65536) * 65536
  quant <- .morie_gr_bits_f32(out_bits)
  finite <- is.finite(arr) & is.finite(quant)
  abs_err <- numeric(length(arr))
  abs_err[finite] <- abs(quant[finite] - arr[finite])
  nz <- finite & arr != 0
  rel <- numeric(length(arr))
  rel[nz] <- abs_err[nz] / abs(arr[nz])
  max_rel <- if (any(nz)) max(rel) else 0
  bits <- vapply(out_bits, function(b) {
    v <- floor(b / 65536)
    paste(rev(as.integer(intToBits(as.integer(ifelse(v >= 2^31, v - 2^32, v)))[1:16])),
      collapse = ""
    )
  }, character(1))
  list(
    values = quant, bits = bits, abs_error = abs_err, rel_error = rel,
    max_rel_error = max_rel, mantissa_bits = 7L, exponent_bits = 8L,
    rounding = rounding, estimate = quant[1], n = length(arr),
    method = "BF16 quantisation (1 sign / 8 exponent / 7 mantissa bits)"
  )
}

#' bfloat16 round-trip and range report (Geron Appendix B, morie.fn grbf16)
#'
#' Same ties-to-even rounding as [morie_geron_bf16()], with grbf16's
#' range diagnostics: bf16 shares fp32's exponent range, so overflow is
#' rare and the cost is precision (machine epsilon 2^-7). NaN input is
#' rejected here, unlike hmbf16.
#'
#' @param x Values to round.
#' @return List with `bf16`, `abs_error`, `rel_error`, `max_rel_error`,
#'   `machine_eps`, `max_normal`, `min_normal`, `n_overflow`,
#'   `n_underflow`, `exact`.
#' @export
morie_geron_bf16_range <- function(x) {
  x32 <- .morie_gr_f32(as.numeric(x))
  .morie_gr_need(length(x32) > 0L, "x is empty.")
  .morie_gr_need(
    !any(is.nan(x32)),
    "x contains NaN; bf16 rounding of NaN is not meaningful here."
  )
  b <- .morie_gr_bits_f32(.morie_gr_bf16_bits(.morie_gr_f32_bits(x32)))
  abs_err <- abs(x32 - b)
  rel <- ifelse(x32 != 0, abs_err / abs(x32), 0)
  rel <- ifelse(is.finite(rel), rel, 0)
  bf16_max <- .morie_gr_f32(3.3895314e38)
  bf16_min_normal <- 1.1754943508222875e-38
  finite <- is.finite(x32)
  n_over <- sum(abs(x32[finite]) > bf16_max)
  n_under <- sum(abs(x32[finite]) > 0 & abs(x32[finite]) < bf16_min_normal)
  list(
    bf16 = b, abs_error = abs_err, rel_error = rel,
    max_rel_error = max(rel), machine_eps = 2^-7, max_normal = bf16_max,
    min_normal = bf16_min_normal, n_overflow = n_over,
    n_underflow = n_under, exact = abs_err == 0, estimate = max(rel),
    n = length(x32), method = "bfloat16 round-trip and range analysis"
  )
}

# ------------------------------------------------------- reverse-mode tape

.morie_gr_tape_counter <- new.env(parent = emptyenv())
.morie_gr_tape_counter$id <- 0L

#' .morie_gvar
#'
#' A step of the geron_ml_native implementation. Called by \code{.morie_gvar_wrap}, \code{Math.morie_gvar}, \code{morie_gvar_relu} and 2 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param value See Usage.
#' @param parents Defaults to \code{list()}.
#' @param op Defaults to \code{"leaf"}.
#' @return The value of \code{e}, as built in the body.
#' @export
.morie_gvar <- function(value, parents = list(), op = "leaf") {
  e <- new.env(parent = emptyenv())
  .morie_gr_tape_counter$id <- .morie_gr_tape_counter$id + 1L
  e$id <- .morie_gr_tape_counter$id
  e$value <- as.numeric(value)
  e$grad <- 0
  e$parents <- parents
  e$op <- op
  class(e) <- "morie_gvar"
  e
}

#' .morie_gvar_wrap
#'
#' A step of the geron_ml_native implementation. Called by \code{Ops.morie_gvar}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Passed to \code{.morie_gvar}.
#' @return One of two values, depending on the branch taken.
#' @export
.morie_gvar_wrap <- function(x) if (inherits(x, "morie_gvar")) x else .morie_gvar(x)

#' Arithmetic on reverse-mode tape nodes
#'
#' Group generic supporting `+ - * / ^` and unary minus for
#' [morie_geron_autograd()] leaves.
#'
#' @param e1,e2 Tape nodes or numbers.
#' @return A tape node.
#' @export
Ops.morie_gvar <- function(e1, e2) {
  if (missing(e2)) {
    if (.Generic == "-") {
      return(.morie_gvar(-e1$value, list(list(e1, -1)), "neg"))
    }
    if (.Generic == "+") {
      return(e1)
    }
    stop("Ops.morie_gvar: unsupported unary operator ", .Generic, call. = FALSE)
  }
  a <- .morie_gvar_wrap(e1)
  b <- .morie_gvar_wrap(e2)
  switch(.Generic,
    "+" = .morie_gvar(a$value + b$value, list(list(a, 1), list(b, 1)), "add"),
    "-" = .morie_gvar(a$value - b$value, list(list(a, 1), list(b, -1)), "sub"),
    "*" = .morie_gvar(
      a$value * b$value,
      list(list(a, b$value), list(b, a$value)), "mul"
    ),
    "/" = {
      .morie_gr_need(b$value != 0, "division by zero on the tape")
      .morie_gvar(
        a$value / b$value,
        list(
          list(a, 1 / b$value),
          list(b, -a$value / (b$value * b$value))
        ), "div"
      )
    },
    "^" = {
      cc <- b$value
      .morie_gr_need(
        !(a$value == 0 && cc < 1),
        sprintf("derivative of x^%g is undefined at x=0", cc)
      )
      .morie_gr_need(
        !(a$value < 0 && cc != floor(cc)),
        "negative base with fractional exponent is not real"
      )
      .morie_gvar(a$value^cc, list(list(a, cc * a$value^(cc - 1))), "pow")
    },
    stop("Ops.morie_gvar: unsupported operator ", .Generic, call. = FALSE)
  )
}

#' Elementary functions on reverse-mode tape nodes
#'
#' Group generic for `exp`, `log`, `sqrt`, `tanh`, `sin`, `cos`.
#'
#' @param x A tape node.
#' @param ... Ignored.
#' @return A tape node.
#' @export
Math.morie_gvar <- function(x, ...) {
  switch(.Generic,
    exp = {
      e <- exp(x$value)
      .morie_gvar(e, list(list(x, e)), "exp")
    },
    log = {
      .morie_gr_need(x$value > 0, sprintf("log is undefined at %g", x$value))
      .morie_gvar(log(x$value), list(list(x, 1 / x$value)), "log")
    },
    sqrt = {
      .morie_gr_need(
        x$value > 0,
        sprintf("derivative of sqrt is undefined at %g", x$value)
      )
      s <- sqrt(x$value)
      .morie_gvar(s, list(list(x, 0.5 / s)), "sqrt")
    },
    tanh = {
      t_ <- tanh(x$value)
      .morie_gvar(t_, list(list(x, 1 - t_ * t_)), "tanh")
    },
    sin = .morie_gvar(sin(x$value), list(list(x, cos(x$value))), "sin"),
    cos = .morie_gvar(cos(x$value), list(list(x, -sin(x$value))), "cos"),
    stop("Math.morie_gvar: unsupported function ", .Generic, call. = FALSE)
  )
}

#' ReLU on a reverse-mode tape node
#' @param x A tape node. @return A tape node.
#' @export
morie_gvar_relu <- function(x) {
  .morie_gvar(max(x$value, 0), list(list(x, if (x$value > 0) 1 else 0)), "relu")
}

#' Logistic sigmoid on a reverse-mode tape node
#' @param x A tape node. @return A tape node.
#' @export
morie_gvar_sigmoid <- function(x) {
  s <- 1 / (1 + exp(-x$value))
  .morie_gvar(s, list(list(x, s * (1 - s))), "sigmoid")
}

#' Reverse-mode autograd over a scalar tape (Geron Ch 10, morie.fn hmagrd)
#'
#' `loss` receives a list of tape leaves and must return a tape node --
#' returning a plain number means the tape was bypassed and no gradient
#' exists, which is an error rather than a silent zero. Leaves support
#' `+ - * / ^` plus `exp log sqrt tanh sin cos` and
#' [morie_gvar_relu()] / [morie_gvar_sigmoid()]. One backward sweep fills
#' every leaf's gradient at the cost of one forward pass.
#'
#' @param loss Function(list of leaves) -> tape node.
#' @param params Leaf values (numeric vector).
#' @return List with `grad`, `value`, `tape_size`, `params`.
#' @export
morie_geron_autograd <- function(loss, params) {
  .morie_gr_need(is.function(loss), "geron_autograd: loss must be callable")
  p <- as.numeric(params)
  .morie_gr_need(length(p) > 0L, "geron_autograd: params is empty")
  .morie_gr_need(all(is.finite(p)), "geron_autograd: params must be finite")
  leaves <- lapply(p, .morie_gvar)
  out <- loss(leaves)
  .morie_gr_need(
    inherits(out, "morie_gvar"),
    "geron_autograd: loss must return a tape node built from the supplied leaves; a raw number means the tape was bypassed"
  )
  # Iterative post-order DFS, then reverse -- the same traversal Python uses.
  order_nodes <- list()
  seen <- new.env(parent = emptyenv())
  stack <- list(list(out, FALSE))
  while (length(stack)) {
    top <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    node <- top[[1]]
    expanded <- top[[2]]
    key <- as.character(node$id)
    if (expanded) {
      order_nodes[[length(order_nodes) + 1L]] <- node
      next
    }
    if (!is.null(seen[[key]])) next
    assign(key, TRUE, envir = seen)
    stack[[length(stack) + 1L]] <- list(node, TRUE)
    for (pr in node$parents) {
      pk <- as.character(pr[[1]]$id)
      if (is.null(seen[[pk]])) stack[[length(stack) + 1L]] <- list(pr[[1]], FALSE)
    }
  }
  for (nd in order_nodes) nd$grad <- 0
  out$grad <- 1
  for (i in seq.int(length(order_nodes), 1L)) {
    nd <- order_nodes[[i]]
    g <- nd$grad
    if (g == 0) next
    for (pr in nd$parents) pr[[1]]$grad <- pr[[1]]$grad + g * pr[[2]]
  }
  grad <- vapply(leaves, function(l) l$grad, numeric(1))
  list(
    grad = grad, value = out$value, tape_size = length(order_nodes),
    params = p, estimate = out$value, n = length(p),
    method = "Reverse-mode automatic differentiation over a scalar tape"
  )
}

# ------------------------------------------------------- time series, decoding

#' ARIMA(p, d, q) by Hannan-Rissanen (Geron Ch 13, morie.fn hmarim)
#'
#' Pure AR models (q = 0) are plain OLS on lags; otherwise a long
#' autoregression of order k supplies the innovations and the ARMA
#' coefficients come from OLS on lags plus fitted innovations. `forecast`
#' is a closure taking a horizon h.
#'
#' @param y Univariate series.
#' @param p,q AR and MA orders.
#' @param d Differencing order.
#' @param include_mean Fit an intercept.
#' @return List with `ar`, `ma`, `intercept`, `residuals`, `sigma2`, `aic`,
#'   `fitted`, `differenced`, `forecast`, `order`.
#' @export
morie_geron_arima <- function(y, p = 1, d = 0, q = 0, include_mean = TRUE) {
  ys <- as.numeric(y)
  .morie_gr_need(length(ys) > 0L, "geron_arima: y is empty")
  .morie_gr_need(all(is.finite(ys)), "geron_arima: y contains non-finite values")
  P <- as.integer(p)
  D <- as.integer(d)
  Q <- as.integer(q)
  .morie_gr_need(
    P >= 0L && D >= 0L && Q >= 0L,
    "geron_arima: p, d and q must all be non-negative"
  )
  .morie_gr_need(D < length(ys), "geron_arima: cannot difference that many times")
  anchors <- numeric(0)
  z <- ys
  if (D > 0L) {
    for (i in seq_len(D)) {
      anchors <- c(anchors, z[length(z)])
      z <- diff(z)
    }
  }
  m <- length(z)
  .morie_gr_need(
    m > P + Q,
    "geron_arima: differenced series is too short for the requested ARMA order"
  )
  if (Q == 0L) {
    e_hat <- NULL
    start <- P
  } else {
    k <- as.integer(min(
      max(P + Q + 1L, as.integer(ceiling(log(max(m, 2))^2))),
      max(1L, (m - 1L) %/% 2L)
    ))
    rows <- m - k
    .morie_gr_need(
      rows > k,
      "geron_arima: series too short for the Hannan-Rissanen long autoregression"
    )
    Xa <- do.call(cbind, lapply(
      seq_len(k) - 1L,
      function(i) z[(k - i):(m - i - 1L)]
    ))
    if (include_mean) Xa <- cbind(rep(1, rows), Xa)
    beta <- .morie_gr_lstsq(Xa, z[(k + 1L):m])
    e_hat <- numeric(m)
    e_hat[(k + 1L):m] <- z[(k + 1L):m] - as.numeric(Xa %*% beta)
    start <- max(P, Q, k)
  }
  cols <- list()
  if (P > 0L) {
    for (i in seq_len(P)) {
      cols[[length(cols) + 1L]] <- z[(start - i + 1L):(m - i)]
    }
  }
  if (Q > 0L) {
    for (j in seq_len(Q)) {
      cols[[length(cols) + 1L]] <- e_hat[(start - j + 1L):(m - j)]
    }
  }
  Xd <- if (length(cols)) do.call(cbind, cols) else matrix(0, m - start, 0)
  if (include_mean) Xd <- cbind(rep(1, m - start), Xd)
  target <- z[(start + 1L):m]
  .morie_gr_need(
    nrow(Xd) > ncol(Xd),
    "geron_arima: too few usable observations for the parameter count"
  )
  coef <- .morie_gr_lstsq(Xd, target)
  off <- if (include_mean) 1L else 0L
  intercept <- if (include_mean) coef[1] else 0
  ar <- if (P > 0L) coef[(off + 1L):(off + P)] else numeric(0)
  ma <- if (Q > 0L) coef[(off + P + 1L):(off + P + Q)] else numeric(0)
  fitted <- as.numeric(Xd %*% coef)
  resid <- target - fitted
  dof <- max(length(target) - ncol(Xd), 1L)
  sigma2 <- sum(resid * resid) / dof
  nobs <- length(target)
  kpar <- ncol(Xd) + 1L
  aic <- if (sigma2 > 0) nobs * log(sigma2) + 2 * kpar else -Inf
  forecast <- function(h = 1) {
    H <- as.integer(h)
    .morie_gr_need(H >= 1L, "forecast: h must be >= 1")
    hist <- z
    errs <- resid
    out <- numeric(0)
    for (i in seq_len(H)) {
      val <- intercept
      if (length(ar)) for (a in seq_along(ar)) val <- val + ar[a] * hist[length(hist) - a + 1L]
      if (length(ma)) {
        for (b in seq_along(ma)) {
          val <- val + ma[b] * (if (length(errs) > b - 1L) errs[length(errs) - b + 1L] else 0)
        }
      }
      hist <- c(hist, val)
      errs <- c(errs, 0)
      out <- c(out, val)
    }
    f <- out
    if (length(anchors)) for (a in rev(anchors)) f <- a + cumsum(f)
    f
  }
  list(
    ar = ar, ma = ma, intercept = intercept, residuals = resid,
    sigma2 = sigma2, aic = aic, fitted = fitted, differenced = z,
    forecast = forecast, order = c(P, D, Q), estimate = sigma2, n = nobs,
    method = if (Q) {
      sprintf("ARIMA(%d,%d,%d) by Hannan-Rissanen", P, D, Q)
    } else {
      sprintf("ARIMA(%d,%d,0) by OLS", P, D)
    }
  )
}

#' One-step ARIMA forecast with known coefficients (Geron Ch 13, grarma)
#'
#' Innovations are recovered with zero pre-sample values, the ARMA
#' forecast is formed on the differenced scale, and the differencing is
#' undone by adding back the last value of each lower-order difference.
#' `theta` uses the PLUS sign convention.
#'
#' @param y Observed series.
#' @param phi AR coefficients.
#' @param theta MA coefficients.
#' @param d Differencing order.
#' @return List with `forecast`, `forecast_differenced`, `residuals`,
#'   `differenced`, `sigma2`, `order`.
#' @export
morie_geron_arima_forecast <- function(y, phi, theta, d = 0) {
  y <- as.numeric(y)
  phi <- as.numeric(phi)
  theta <- as.numeric(theta)
  .morie_gr_need(all(is.finite(y)), "y contains non-finite values.")
  .morie_gr_need(
    all(is.finite(phi)) && all(is.finite(theta)),
    "phi and theta must be finite."
  )
  d <- as.integer(d)
  .morie_gr_need(d >= 0L, "d must be non-negative.")
  p <- length(phi)
  q <- length(theta)
  need <- d + max(p, 1L)
  .morie_gr_need(
    length(y) >= need,
    "y is too short to form one forecast at this order."
  )
  last_levels <- numeric(0)
  w <- y
  if (d > 0L) {
    for (i in seq_len(d)) {
      last_levels <- c(last_levels, w[length(w)])
      w <- diff(w)
    }
  }
  Tn <- length(w)
  eps <- numeric(Tn)
  for (t in seq_len(Tn)) {
    ar <- 0
    if (p > 0L) for (i in seq_len(p)) if (t - i >= 1L) ar <- ar + phi[i] * w[t - i]
    ma <- 0
    if (q > 0L) for (j in seq_len(q)) if (t - j >= 1L) ma <- ma + theta[j] * eps[t - j]
    eps[t] <- w[t] - ar - ma
  }
  fc <- 0
  if (p > 0L) for (i in seq_len(p)) if (Tn - i >= 0L) fc <- fc + phi[i] * w[Tn - i + 1L]
  if (q > 0L) for (j in seq_len(q)) if (Tn - j >= 0L) fc <- fc + theta[j] * eps[Tn - j + 1L]
  level <- fc
  if (length(last_levels)) for (lv in rev(last_levels)) level <- level + lv
  .morie_gr_need(
    is.finite(level),
    "forecast is not finite; check phi/theta for explosive roots."
  )
  list(
    forecast = level, forecast_differenced = fc, residuals = eps,
    differenced = w, sigma2 = mean(eps^2), order = c(p, d, q),
    estimate = level, n = length(y),
    method = "ARIMA(p, d, q) one-step-ahead forecast"
  )
}

#' Beam search over a next-token scorer (Geron Ch 14, morie.fn hmbms)
#'
#' `model(src, prefix)` returns log-probabilities over the vocabulary for
#' the token after `prefix` (a 0-based integer vector). The scorer is
#' checked for normalisation, because an unnormalised one turns beam
#' search into an arbitrary greedy walk. Emitted tokens are 0-based.
#'
#' @param model Scorer function.
#' @param src Source, passed through.
#' @param beam_width Hypotheses kept per step.
#' @param max_len Token cap.
#' @param eos Optional 0-based end-of-sequence token.
#' @param length_penalty Exponent alpha in score / len^alpha for the final rank.
#' @return List with `sequence`, `score`, `beams`, `scores`, `finished`.
#' @export
morie_geron_beam_search <- function(model, src, beam_width = 3, max_len = 10,
                                    eos = NULL, length_penalty = 0) {
  .morie_gr_need(is.function(model), "geron_beam_search: model must be callable")
  K <- as.integer(beam_width)
  .morie_gr_need(K >= 1L, "geron_beam_search: beam_width must be >= 1")
  L <- as.integer(max_len)
  .morie_gr_need(L >= 1L, "geron_beam_search: max_len must be >= 1")
  score_next <- function(prefix) {
    lp <- as.numeric(model(src, prefix))
    .morie_gr_need(
      length(lp) > 0L,
      "geron_beam_search: model returned an empty log-probability vector"
    )
    .morie_gr_need(
      all(is.finite(lp[lp > -Inf])),
      "geron_beam_search: model returned non-finite log-probabilities"
    )
    total <- sum(exp(lp))
    .morie_gr_need(
      abs(total - 1) <= 1e-6 + 1e-5 * abs(1),
      "geron_beam_search: model log-probabilities do not exponentiate to 1"
    )
    lp
  }
  live <- list(list(seq = integer(0), sc = 0))
  finished <- list()
  for (i in seq_len(L)) {
    cand <- list()
    for (h in live) {
      lp <- score_next(h$seq)
      V <- length(lp)
      if (!is.null(eos)) {
        .morie_gr_need(
          as.integer(eos) >= 0L && as.integer(eos) < V,
          "geron_beam_search: eos is outside the vocabulary"
        )
      }
      top <- order(-lp, method = "radix")[seq_len(min(K, V))]
      for (tok in top) {
        cand[[length(cand) + 1L]] <- list(
          seq = c(h$seq, tok - 1L),
          sc = h$sc + lp[tok]
        )
      }
    }
    if (!length(cand)) break
    ord <- order(-vapply(cand, function(c) c$sc, numeric(1)), method = "radix")
    cand <- cand[ord]
    live <- list()
    for (cc in cand) {
      if (!is.null(eos) && cc$seq[length(cc$seq)] == as.integer(eos)) {
        finished[[length(finished) + 1L]] <- cc
      } else {
        live[[length(live) + 1L]] <- cc
      }
      if (length(live) >= K) break
    }
    if (!length(live)) break
  }
  finished <- c(finished, live)
  normed <- vapply(
    finished, function(it) {
      if (length_penalty) it$sc / (length(it$seq)^length_penalty) else it$sc
    },
    numeric(1)
  )
  finished <- finished[order(-normed, method = "radix")]
  best <- finished[[1]]
  keep <- finished[seq_len(min(K, length(finished)))]
  list(
    sequence = best$seq, score = best$sc,
    beams = lapply(keep, function(it) it$seq),
    scores = vapply(keep, function(it) it$sc, numeric(1)),
    finished = length(finished), estimate = best$sc,
    n = length(best$seq),
    method = sprintf("Beam search with width %d", K)
  )
}

#' Beam search over a fixed per-step score table (Geron Ch 14, grbeam)
#'
#' Every step expands the whole vocabulary and keeps the top-k by
#' cumulative log-probability. Prefix-independent scores make the beam
#' exhaustive over the retained hypotheses. Tokens are 0-based.
#'
#' @param scores Matrix (T, V) of log-scale scores.
#' @param beam_width Hypotheses kept.
#' @param max_len Steps, defaults to T.
#' @param length_penalty GNMT exponent for the final ranking.
#' @return List with `best_sequence`, `best_score`, `beams` (list of
#'   list(sequence, score)), `normalised_scores`, `greedy_sequence`.
#' @export
morie_geron_beam_search_decoder <- function(scores, beam_width, max_len = NULL,
                                            length_penalty = 0) {
  S <- .morie_gr_mat(scores, "scores")
  .morie_gr_need(length(S) > 0L, "scores must be a non-empty 2-D (T, V) array.")
  Tn <- nrow(S)
  V <- ncol(S)
  beam_width <- as.integer(beam_width)
  .morie_gr_need(beam_width >= 1L, "beam_width must be at least 1.")
  steps <- if (is.null(max_len)) Tn else as.integer(max_len)
  .morie_gr_need(steps >= 1L, "max_len must be at least 1.")
  .morie_gr_need(steps <= Tn, "max_len exceeds the steps of scores supplied.")
  length_penalty <- as.numeric(length_penalty)
  .morie_gr_need(length_penalty >= 0, "length_penalty must be non-negative.")
  beams <- list(list(seq = integer(0), sc = 0))
  for (t in seq_len(steps)) {
    cand <- list()
    for (h in beams) {
      for (yv in seq_len(V)) {
        cand[[length(cand) + 1L]] <- list(
          seq = c(h$seq, yv - 1L),
          sc = h$sc + S[t, yv]
        )
      }
    }
    ord <- order(-vapply(cand, function(c) c$sc, numeric(1)), method = "radix")
    beams <- cand[ord][seq_len(min(beam_width, length(cand)))]
  }
  denom <- if (length_penalty) steps^length_penalty else 1
  ranked <- beams[order(-vapply(beams, function(c) c$sc / denom, numeric(1)),
    method = "radix"
  )]
  best <- ranked[[1]]
  greedy <- apply(S[seq_len(steps), , drop = FALSE], 1, which.max) - 1L
  list(
    best_sequence = best$seq, best_score = best$sc,
    beams = lapply(ranked, function(it) list(sequence = it$seq, score = it$sc)),
    normalised_scores = vapply(ranked, function(it) it$sc / denom, numeric(1)),
    greedy_sequence = greedy, beam_width = beam_width,
    estimate = best$sc, n = steps, method = "Beam search decoding"
  )
}

#' Byte-pair encoding to a target vocabulary size (Geron Ch 14, hmbpet)
#'
#' Ties on pair frequency are broken by FIRST APPEARANCE in corpus order,
#' so the merge sequence is deterministic. Merging stops at the target
#' vocabulary size or when no pair occurs more than once; when the target
#' is below the base-symbol count no merge is performed at all.
#'
#' @param corpus A single string, a character vector, or a named integer
#'   vector / list of word -> count.
#' @param vocab_size Target size, counting base symbols.
#' @return List with `merges` (list of c(a, b)), `vocab`, `tokenize`,
#'   `word_tokens`, `n_merges`.
#' @export
morie_geron_bpe_tokenizer <- function(corpus, vocab_size = 100) {
  EOW <- "</w>"
  if (!is.null(names(corpus))) {
    words_ <- names(corpus)
    cnts <- as.integer(unlist(corpus))
    .morie_gr_need(
      all(cnts >= 0L),
      "geron_bpe_tokenizer: corpus counts must be non-negative"
    )
  } else {
    toks <- unlist(strsplit(as.character(corpus), "[[:space:]]+"))
    toks <- toks[nzchar(toks)]
    words_ <- unique(toks)
    cnts <- vapply(words_, function(w) sum(toks == w), numeric(1))
  }
  .morie_gr_need(length(words_) > 0L, "geron_bpe_tokenizer: corpus is empty")
  target <- as.integer(vocab_size)
  .morie_gr_need(target >= 1L, "geron_bpe_tokenizer: vocab_size must be >= 1")
  syms <- lapply(words_, function(w) c(strsplit(w, "")[[1]], EOW))
  names(syms) <- words_
  vocab_keys <- character(0)
  vocab_vals <- numeric(0)
  for (wi in seq_along(words_)) {
    for (s in syms[[wi]]) {
      pos <- match(s, vocab_keys)
      if (is.na(pos)) {
        vocab_keys <- c(vocab_keys, s)
        vocab_vals <- c(vocab_vals, cnts[wi])
      } else {
        vocab_vals[pos] <- vocab_vals[pos] + cnts[wi]
      }
    }
  }
  merges <- list()
  if (target >= length(vocab_keys)) {
    while (length(vocab_keys) < target) {
      pk <- character(0)
      pv <- numeric(0)
      for (wi in seq_along(words_)) {
        ss <- syms[[wi]]
        cc <- cnts[wi]
        if (length(ss) < 2L) next
        for (i in seq_len(length(ss) - 1L)) {
          key <- paste0(ss[i], "\u001f", ss[i + 1L])
          pos <- match(key, pk)
          if (is.na(pos)) {
            pk <- c(pk, key)
            pv <- c(pv, cc)
          } else {
            pv[pos] <- pv[pos] + cc
          }
        }
      }
      if (!length(pk)) break
      best <- max(pv)
      if (best < 2) break
      key <- pk[which(pv == best)[1]]
      parts <- strsplit(key, "\u001f", fixed = TRUE)[[1]]
      a <- parts[1]
      b <- parts[2]
      new_sym <- paste0(a, b)
      for (wi in seq_along(words_)) {
        ss <- syms[[wi]]
        out <- character(0)
        i <- 1L
        while (i <= length(ss)) {
          if (i + 1L <= length(ss) && ss[i] == a && ss[i + 1L] == b) {
            out <- c(out, new_sym)
            i <- i + 2L
          } else {
            out <- c(out, ss[i])
            i <- i + 1L
          }
        }
        syms[[wi]] <- out
      }
      merges[[length(merges) + 1L]] <- c(a, b)
      vocab_keys <- c(vocab_keys, new_sym)
      vocab_vals <- c(vocab_vals, best)
    }
  }
  ordered_merges <- merges
  tokenize <- function(word) {
    ss <- c(strsplit(as.character(word), "")[[1]], EOW)
    for (mg in ordered_merges) {
      a <- mg[1]
      b <- mg[2]
      out <- character(0)
      i <- 1L
      while (i <= length(ss)) {
        if (i + 1L <= length(ss) && ss[i] == a && ss[i + 1L] == b) {
          out <- c(out, paste0(a, b))
          i <- i + 2L
        } else {
          out <- c(out, ss[i])
          i <- i + 1L
        }
      }
      ss <- out
    }
    ss
  }
  vocab <- stats::setNames(vocab_vals, vocab_keys)
  wt <- syms
  names(wt) <- words_
  list(
    merges = ordered_merges, vocab = vocab, tokenize = tokenize,
    word_tokens = wt, n_merges = length(ordered_merges),
    estimate = length(vocab_keys), n = length(words_),
    method = "Byte-pair encoding with greedy most-frequent-pair merges"
  )
}

#' Learn a fixed number of BPE merges (Geron Ch 14, morie.fn grbpe)
#'
#' Ties are broken by the LEXICOGRAPHICALLY SMALLEST pair -- note this is
#' a different rule from [morie_geron_bpe_tokenizer()], which uses first
#' appearance; both are deterministic, and they can disagree. Merging
#' stops early when no adjacent pair occurs more than once.
#'
#' @param corpus Character vector of words, or a named count vector.
#' @param n_merges Maximum merges, non-negative.
#' @return List with `merges`, `vocab`, `splits`, `merge_counts`,
#'   `n_tokens_before`, `n_tokens_after`, `compression`.
#' @export
morie_geron_bpe_merge <- function(corpus, n_merges) {
  EOW <- "</w>"
  if (!is.null(names(corpus))) {
    words_ <- names(corpus)
    freqs <- as.integer(unlist(corpus))
  } else {
    toks <- as.character(corpus)
    words_ <- unique(toks)
    freqs <- vapply(words_, function(w) sum(toks == w), numeric(1))
  }
  .morie_gr_need(length(words_) > 0L, "corpus is empty.")
  .morie_gr_need(all(freqs > 0), "word frequencies must be positive.")
  .morie_gr_need(all(nchar(words_) > 0L), "corpus contains an empty word.")
  n_merges <- as.integer(n_merges)
  .morie_gr_need(n_merges >= 0L, "n_merges must be non-negative.")
  splits <- lapply(words_, function(w) c(strsplit(w, "")[[1]], EOW))
  n_before <- sum(vapply(
    seq_along(words_),
    function(i) length(splits[[i]]) * freqs[i], numeric(1)
  ))
  merges <- list()
  counts <- numeric(0)
  for (it in seq_len(n_merges)) {
    pk <- character(0)
    pv <- numeric(0)
    for (i in seq_along(words_)) {
      ss <- splits[[i]]
      f <- freqs[i]
      if (length(ss) < 2L) next
      for (j in seq_len(length(ss) - 1L)) {
        key <- paste0(ss[j], "\u001f", ss[j + 1L])
        pos <- match(key, pk)
        if (is.na(pos)) {
          pk <- c(pk, key)
          pv <- c(pv, f)
        } else {
          pv[pos] <- pv[pos] + f
        }
      }
    }
    if (!length(pk)) break
    best_count <- max(pv)
    if (best_count < 2) break
    # Lexicographic minimum over (first, second), matching Python's tuple order.
    cands <- pk[pv == best_count]
    parts <- do.call(rbind, strsplit(cands, "\u001f", fixed = TRUE))
    ord <- order(parts[, 1], parts[, 2], method = "radix")
    a <- parts[ord[1], 1]
    b <- parts[ord[1], 2]
    merged <- paste0(a, b)
    for (i in seq_along(words_)) {
      ss <- splits[[i]]
      out <- character(0)
      j <- 1L
      while (j <= length(ss)) {
        if (j < length(ss) && ss[j] == a && ss[j + 1L] == b) {
          out <- c(out, merged)
          j <- j + 2L
        } else {
          out <- c(out, ss[j])
          j <- j + 1L
        }
      }
      splits[[i]] <- out
    }
    merges[[length(merges) + 1L]] <- c(a, b)
    counts <- c(counts, best_count)
  }
  n_after <- sum(vapply(
    seq_along(words_),
    function(i) length(splits[[i]]) * freqs[i], numeric(1)
  ))
  vocab <- sort(unique(unlist(splits)))
  names(splits) <- words_
  list(
    merges = merges, vocab = vocab, splits = splits, merge_counts = counts,
    n_tokens_before = n_before, n_tokens_after = n_after,
    compression = if (n_after) n_before / n_after else NA_real_,
    estimate = length(merges), n = length(words_),
    method = "Byte-pair encoding merges"
  )
}

# ------------------------------------------------------------- transformers

# Shared post-layernorm encoder block. `W` is a list of Wq/Wk/Wv/Wo/W1/b1/W2/b2.
#' Shared post-layernorm encoder block. `W` is a list of
#' Wq/Wk/Wv/Wo/W1/b1/W2/b2
#'
#' A step of the geron_ml_native implementation. Called by \code{morie_geron_albert}, \code{morie_geron_bert}, \code{morie_geron_roberta}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param W A list; the body reads \code{$b1}, \code{$b2}, \code{$W1}, \code{$W2}, \code{$Wk}, \code{$Wo}, \code{$Wq}, \code{$Wv} from it.
#' @param n_heads A count; the body uses it as \code{seq_len(...)}.
#' @param keep_attn A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return One of two values, depending on the branch taken.
#' @export
.morie_gr_encoder_block <- function(X, W, n_heads, keep_attn = FALSE) {
  Tn <- nrow(X)
  d <- ncol(X)
  dh <- d %/% n_heads
  Q <- X %*% W$Wq
  K <- X %*% W$Wk
  V <- X %*% W$Wv
  out <- matrix(0, Tn, d)
  attn <- if (keep_attn) array(0, dim = c(n_heads, Tn, Tn)) else NULL
  for (h in seq_len(n_heads)) {
    sl <- ((h - 1L) * dh + 1L):(h * dh)
    a <- .morie_al_softmax_rows(Q[, sl, drop = FALSE] %*%
      t(K[, sl, drop = FALSE]) / sqrt(dh))
    if (keep_attn) attn[h, , ] <- a
    out[, sl] <- a %*% V[, sl, drop = FALSE]
  }
  X <- .morie_gr_layernorm(X + out %*% W$Wo)
  f <- pmax(X %*% W$W1 + matrix(W$b1, Tn, length(W$b1), byrow = TRUE), 0) %*% W$W2
  f <- f + matrix(W$b2, Tn, length(W$b2), byrow = TRUE)
  Xo <- .morie_gr_layernorm(X + f)
  if (keep_attn) list(X = Xo, attn = attn) else Xo
}

#' .morie_gr_block_weights
#'
#' A step of the geron_ml_native implementation. Called by \code{morie_geron_albert}, \code{morie_geron_bert}, \code{morie_geron_roberta}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param d_model A count; the body uses it as \code{numeric(...)}.
#' @param d_ff A count; the body uses it as \code{numeric(...)}.
#' @param seed Numeric; combined arithmetically in the body.
#' @return A list with \code{Wq}, \code{Wk}, \code{Wv}, \code{Wo}, \code{W1}, \code{b1}, \code{W2}, \code{b2}.
#' @export
.morie_gr_block_weights <- function(d_model, d_ff, seed) {
  list(
    Wq = .morie_gr_init(d_model, d_model, seed + 1),
    Wk = .morie_gr_init(d_model, d_model, seed + 2),
    Wv = .morie_gr_init(d_model, d_model, seed + 3),
    Wo = .morie_gr_init(d_model, d_model, seed + 4),
    W1 = .morie_gr_init(d_model, d_ff, seed + 5),
    b1 = numeric(d_ff),
    W2 = .morie_gr_init(d_ff, d_model, seed + 6),
    b2 = numeric(d_model)
  )
}

#' ALBERT encoder: factorised embedding + cross-layer sharing (Geron Ch 15, hmalbt)
#'
#' The embedding goes V -> E -> H instead of V -> H, and ONE block object
#' is applied `n_layers` times, so depth costs no extra parameters --
#' which is why `n_params` is independent of `n_layers` while
#' `n_params_unshared` is not. Token ids are 0-based.
#'
#' @param X 0-based token ids, vector or (batch, T) matrix.
#' @param n_layers,n_heads,d_model,d_ff Encoder geometry.
#' @param d_embed Factorised width E in \[1, d_model\].
#' @param vocab_size Optional; defaults to max(X) + 1.
#' @param seed LCG seed.
#' @return List with `hidden` (array batch x T x d), `n_params`,
#'   `n_params_unshared`, `block_params`, `embedding_params`,
#'   `embedding_params_direct`, `shared`, `d_embed`.
#' @export
morie_geron_albert <- function(X, n_layers = 4, n_heads = 2, d_model = 8,
                               d_embed = 4, vocab_size = NULL, d_ff = NULL,
                               seed = 0) {
  ids <- if (is.matrix(X)) X else matrix(as.numeric(X), nrow = 1)
  .morie_gr_need(length(ids) > 0L, "geron_albert: X is empty")
  .morie_gr_need(
    all(ids == floor(ids)),
    "geron_albert: X must contain integer token ids"
  )
  ids <- matrix(as.integer(ids), nrow(ids), ncol(ids))
  .morie_gr_need(min(ids) >= 0L, "geron_albert: token ids must be non-negative")
  B <- nrow(ids)
  Tn <- ncol(ids)
  L <- as.integer(n_layers)
  H <- as.integer(n_heads)
  d <- as.integer(d_model)
  .morie_gr_need(
    L >= 1L && H >= 1L && d >= 1L,
    "geron_albert: n_layers, n_heads and d_model must all be >= 1"
  )
  .morie_gr_need(
    d %% H == 0L,
    "geron_albert: d_model is not divisible by n_heads"
  )
  E <- as.integer(d_embed)
  .morie_gr_need(E >= 1L && E <= d, "geron_albert: d_embed must lie in [1, d_model]")
  Vsz <- if (is.null(vocab_size)) max(ids) + 1L else as.integer(vocab_size)
  .morie_gr_need(Vsz > max(ids), "geron_albert: vocab_size is too small")
  ff <- if (is.null(d_ff)) 2L * d else as.integer(d_ff)
  .morie_gr_need(ff >= 1L, "geron_albert: d_ff must be >= 1")
  Wemb <- .morie_gr_init(Vsz, E, seed + 101)
  Wproj <- .morie_gr_init(E, d, seed + 202)
  Wpos <- .morie_gr_init(Tn, d, seed + 303)
  shared <- .morie_gr_block_weights(d, ff, seed)
  hidden <- array(0, dim = c(B, Tn, d))
  for (b in seq_len(B)) {
    Xh <- (Wemb[ids[b, ] + 1L, , drop = FALSE] %*% Wproj) + Wpos
    for (l in seq_len(L)) Xh <- .morie_gr_encoder_block(Xh, shared, H)
    hidden[b, , ] <- Xh
  }
  block_params <- as.integer(4 * d * d + d * ff + ff + ff * d + d)
  emb_params <- as.integer(Vsz * E + E * d)
  emb_direct <- as.integer(Vsz * d)
  total <- as.integer(emb_params + Tn * d + block_params)
  list(
    hidden = hidden, n_params = total,
    n_params_unshared = as.integer(emb_params + Tn * d + L * block_params),
    block_params = block_params, embedding_params = emb_params,
    embedding_params_direct = emb_direct, shared = TRUE, d_embed = E,
    estimate = as.numeric(total), n = B * Tn,
    method = "ALBERT: factorised embedding plus one cross-layer-shared encoder block"
  )
}

#' BERT encoder forward pass with MLM + NSP heads (Geron Ch 15, hmbert)
#'
#' Token + learned positional embeddings, `n_layers` distinct post-layernorm
#' blocks, an MLM head tied to the embedding matrix, and an NSP head on the
#' pooled first position. Masked positions come from the same LCG draw as
#' Python and are returned 0-based.
#'
#' @param X 0-based token ids.
#' @param n_layers,n_heads,d_model,d_ff Geometry.
#' @param vocab_size Optional; one extra row is appended for \[MASK\].
#' @param mask_prob Masking fraction in (0, 1).
#' @param seed LCG seed.
#' @return List with `hidden`, `mlm_loss`, `mlm_losses`, `masked_positions`,
#'   `nsp_logits`, `attentions`, `embeddings`, `n_params`, `vocab_size`.
#' @export
morie_geron_bert <- function(X, n_layers = 2, n_heads = 2, d_model = 8,
                             vocab_size = NULL, d_ff = NULL, mask_prob = 0.15,
                             seed = 0) {
  ids <- if (is.matrix(X)) X else matrix(as.numeric(X), nrow = 1)
  .morie_gr_need(length(ids) > 0L, "geron_bert: X is empty")
  .morie_gr_need(all(ids == floor(ids)), "geron_bert: X must contain integer token ids")
  ids <- matrix(as.integer(ids), nrow(ids), ncol(ids))
  .morie_gr_need(min(ids) >= 0L, "geron_bert: token ids must be non-negative")
  B <- nrow(ids)
  Tn <- ncol(ids)
  L <- as.integer(n_layers)
  H <- as.integer(n_heads)
  d <- as.integer(d_model)
  .morie_gr_need(
    L >= 1L && H >= 1L && d >= 1L,
    "geron_bert: n_layers, n_heads and d_model must all be >= 1"
  )
  .morie_gr_need(d %% H == 0L, "geron_bert: d_model is not divisible by n_heads")
  Vsz <- if (is.null(vocab_size)) max(ids) + 1L else as.integer(vocab_size)
  .morie_gr_need(Vsz > max(ids), "geron_bert: vocab_size is too small")
  ff <- if (is.null(d_ff)) 2L * d else as.integer(d_ff)
  .morie_gr_need(ff >= 1L, "geron_bert: d_ff must be >= 1")
  p <- as.numeric(mask_prob)
  .morie_gr_need(p > 0 && p < 1, "geron_bert: mask_prob must lie in (0, 1)")
  mask_id <- Vsz
  Emb <- .morie_gr_init(Vsz + 1L, d, seed + 101)
  Pos <- .morie_gr_init(Tn, d, seed + 202)
  blocks <- lapply(seq_len(L), function(i) {
    .morie_gr_block_weights(d, ff, seed + 1000 * i)
  })
  W_nsp <- .morie_gr_init(d, 2L, seed + 303)
  n_mask <- max(1L, as.integer(round(p * Tn)))
  u <- .morie_gr_lcg_w(B * Tn, seed + 404, scale = 0.5) + 0.5
  hidden <- array(0, dim = c(B, Tn, d))
  attentions <- vector("list", B)
  masked_positions <- vector("list", B)
  losses <- numeric(B)
  nsp <- matrix(0, B, 2)
  for (b in seq_len(B)) {
    ub <- u[((b - 1L) * Tn + 1L):(b * Tn)]
    ord <- order(ub, method = "radix")
    pos <- sort(ord[seq_len(n_mask)])
    masked_positions[[b]] <- pos - 1L
    toks <- ids[b, ]
    toks[pos] <- mask_id
    Xh <- Emb[toks + 1L, , drop = FALSE] + Pos
    atts <- vector("list", L)
    for (l in seq_len(L)) {
      res <- .morie_gr_encoder_block(Xh, blocks[[l]], H, keep_attn = TRUE)
      Xh <- res$X
      atts[[l]] <- res$attn
    }
    hidden[b, , ] <- Xh
    attentions[[b]] <- atts
    logits <- Xh[pos, , drop = FALSE] %*% t(Emb)
    probs <- .morie_al_softmax_rows(logits)
    losses[b] <- -mean(log(pmax(probs[cbind(seq_along(pos), ids[b, pos] + 1L)], 1e-15)))
    nsp[b, ] <- as.numeric(Xh[1, ] %*% W_nsp)
  }
  n_params <- as.integer((Vsz + 1) * d + Tn * d +
    L * (4 * d * d + d * ff + ff + ff * d + d) + d * 2)
  list(
    hidden = hidden, mlm_loss = mean(losses), mlm_losses = losses,
    masked_positions = masked_positions, nsp_logits = nsp,
    attentions = attentions, embeddings = Emb, n_params = n_params,
    vocab_size = Vsz, estimate = mean(losses), n = B * Tn,
    method = sprintf("BERT-style bidirectional encoder, %d layers x %d heads, MLM + NSP heads", L, H)
  )
}

#' RoBERTa pretraining with dynamic masking (Geron Ch 15, morie.fn hmbrob)
#'
#' Same encoder stack as BERT with two deliberate differences: no NSP head
#' (so `n_params` is d_model*2 smaller than `n_params_with_nsp`), and a
#' fresh mask drawn every epoch instead of frozen in preprocessing.
#' `masks` records sequence 0's positions per epoch, 0-based.
#'
#' @param X 0-based token ids.
#' @param n_layers,n_heads,d_model,d_ff Geometry.
#' @param vocab_size Optional.
#' @param mask_prob Fraction in (0, 1).
#' @param epochs Dynamic-masking passes.
#' @param seed LCG seed.
#' @return List with `hidden`, `mlm_loss`, `epoch_losses`, `masks`,
#'   `dynamic_masking`, `has_nsp_head`, `n_params`, `n_params_with_nsp`,
#'   `n_masked_per_sequence`.
#' @export
morie_geron_roberta <- function(X, n_layers = 2, n_heads = 2, d_model = 8,
                                vocab_size = NULL, d_ff = NULL,
                                mask_prob = 0.15, epochs = 4, seed = 0) {
  ids <- if (is.matrix(X)) X else matrix(as.numeric(X), nrow = 1)
  .morie_gr_need(length(ids) > 0L, "geron_roberta: X is empty")
  .morie_gr_need(
    all(ids == floor(ids)),
    "geron_roberta: X must contain integer token ids"
  )
  ids <- matrix(as.integer(ids), nrow(ids), ncol(ids))
  .morie_gr_need(min(ids) >= 0L, "geron_roberta: token ids must be non-negative")
  B <- nrow(ids)
  Tn <- ncol(ids)
  L <- as.integer(n_layers)
  H <- as.integer(n_heads)
  d <- as.integer(d_model)
  .morie_gr_need(
    L >= 1L && H >= 1L && d >= 1L,
    "geron_roberta: n_layers, n_heads and d_model must all be >= 1"
  )
  .morie_gr_need(d %% H == 0L, "geron_roberta: d_model is not divisible by n_heads")
  Vsz <- if (is.null(vocab_size)) max(ids) + 1L else as.integer(vocab_size)
  .morie_gr_need(Vsz > max(ids), "geron_roberta: vocab_size is too small")
  ff <- if (is.null(d_ff)) 2L * d else as.integer(d_ff)
  .morie_gr_need(ff >= 1L, "geron_roberta: d_ff must be >= 1")
  p <- as.numeric(mask_prob)
  .morie_gr_need(p > 0 && p < 1, "geron_roberta: mask_prob must lie in (0, 1)")
  EP <- as.integer(epochs)
  .morie_gr_need(EP >= 1L, "geron_roberta: epochs must be >= 1")
  n_mask <- max(1L, as.integer(round(p * Tn)))
  .morie_gr_need(
    !(n_mask >= Tn && Tn > 1L),
    "geron_roberta: mask_prob would mask the entire sequence"
  )
  mask_id <- Vsz
  Emb <- .morie_gr_init(Vsz + 1L, d, seed + 101)
  Pos <- .morie_gr_init(Tn, d, seed + 202)
  blocks <- lapply(seq_len(L), function(i) {
    .morie_gr_block_weights(d, ff, seed + 1000 * i)
  })
  hidden <- array(0, dim = c(B, Tn, d))
  masks <- list()
  epoch_losses <- numeric(EP)
  for (ep in seq_len(EP)) {
    losses <- numeric(B)
    for (b in seq_len(B)) {
      u <- .morie_gr_lcg_w(Tn, seed + 977 * ep + 31 * (b - 1L), scale = 0.5) + 0.5
      pos <- sort(order(u, method = "radix")[seq_len(n_mask)])
      if (b == 1L) masks[[length(masks) + 1L]] <- pos - 1L
      toks <- ids[b, ]
      toks[pos] <- mask_id
      Xh <- Emb[toks + 1L, , drop = FALSE] + Pos
      for (l in seq_len(L)) Xh <- .morie_gr_encoder_block(Xh, blocks[[l]], H)
      if (ep == EP) hidden[b, , ] <- Xh
      probs <- .morie_al_softmax_rows(Xh[pos, , drop = FALSE] %*% t(Emb))
      losses[b] <- -mean(log(pmax(probs[cbind(seq_along(pos), ids[b, pos] + 1L)], 1e-15)))
    }
    epoch_losses[ep] <- mean(losses)
  }
  base <- as.integer((Vsz + 1) * d + Tn * d +
    L * (4 * d * d + d * ff + ff + ff * d + d))
  list(
    hidden = hidden, mlm_loss = epoch_losses[EP], epoch_losses = epoch_losses,
    masks = masks, dynamic_masking = TRUE, has_nsp_head = FALSE,
    n_params = base, n_params_with_nsp = as.integer(base + d * 2),
    n_masked_per_sequence = n_mask, estimate = epoch_losses[EP], n = B * Tn,
    method = "RoBERTa: NSP-free encoder pretraining with per-epoch dynamic masking"
  )
}

#' BART text-infilling corruption and reconstruction score (Geron Ch 15, hmbart)
#'
#' Contiguous spans collapse to a SINGLE `<mask>` token, so the decoder
#' must recover both content and length. With no `model` the scorer is an
#' add-one-smoothed bigram LM estimated on the target, giving a genuine
#' cross-entropy in nats per token. `spans` entries are 0-based
#' c(start, length).
#'
#' @param src,tgt Token vectors.
#' @param mask_ratio Fraction in (0, 1).
#' @param mean_span Mean geometric span length, >= 1.
#' @param permute Also rotate the corrupted token list.
#' @param model Optional function(corrupted, tgt) -> per-token log-probs.
#' @param seed LCG seed.
#' @return List with `corrupted`, `spans`, `n_masked`, `n_spans`, `loss`,
#'   `perplexity`, `token_logprobs`.
#' @export
morie_geron_bart <- function(src, tgt, mask_ratio = 0.3, mean_span = 3,
                             permute = FALSE, model = NULL, seed = 0) {
  MASK <- "<mask>"
  s_toks <- as.character(src)
  t_toks <- as.character(tgt)
  .morie_gr_need(length(s_toks) > 0L, "geron_bart: src is empty")
  .morie_gr_need(length(t_toks) > 0L, "geron_bart: tgt is empty")
  r <- as.numeric(mask_ratio)
  .morie_gr_need(r > 0 && r < 1, "geron_bart: mask_ratio must lie in (0, 1)")
  ms <- as.numeric(mean_span)
  .morie_gr_need(ms >= 1, "geron_bart: mean_span must be >= 1")
  ns <- length(s_toks)
  budget <- max(1L, as.integer(round(r * ns)))
  u <- .morie_gr_lcg_u(4L * ns + 8L, seed + 17)
  covered <- rep(FALSE, ns)
  spans <- list()
  ui <- 0L
  guard <- 0L
  while (sum(covered) < budget && guard < 10L * ns) {
    guard <- guard + 1L
    start <- as.integer(floor(u[(ui %% length(u)) + 1L] * ns))
    ui <- ui + 1L
    pp <- 1 / ms
    len <- if (pp >= 1) {
      1L
    } else {
      as.integer(floor(log(max(u[(ui %% length(u)) + 1L], 1e-12)) / log(1 - pp))) + 1L
    }
    ui <- ui + 1L
    len <- max(1L, min(len, budget - sum(covered), ns - start))
    idx <- (start + 1L):(start + len)
    if (any(covered[idx])) next
    covered[idx] <- TRUE
    spans[[length(spans) + 1L]] <- c(start, len)
  }
  if (length(spans)) {
    spans <- spans[order(vapply(spans, function(sp) sp[1], numeric(1)),
      vapply(spans, function(sp) sp[2], numeric(1)),
      method = "radix"
    )]
  }
  span_start <- vapply(spans, function(sp) sp[1], numeric(1))
  span_len <- vapply(spans, function(sp) sp[2], numeric(1))
  corrupted <- character(0)
  i <- 0L
  while (i < ns) {
    hit <- match(i, span_start)
    if (!is.na(hit)) {
      corrupted <- c(corrupted, MASK)
      i <- i + as.integer(span_len[hit])
    } else {
      corrupted <- c(corrupted, s_toks[i + 1L])
      i <- i + 1L
    }
  }
  if (permute) {
    k <- as.integer(floor(u[length(u)] * length(corrupted)))
    if (k > 0L) corrupted <- c(corrupted[(k + 1L):length(corrupted)], corrupted[1:k])
  }
  if (is.null(model)) {
    vocab <- sort(unique(t_toks))
    V <- length(vocab)
    counts <- matrix(1, V + 1L, V)
    for (i in seq_along(t_toks)) {
      prev <- if (i == 1L) V + 1L else match(t_toks[i - 1L], vocab)
      counts[prev, match(t_toks[i], vocab)] <- counts[prev, match(t_toks[i], vocab)] + 1
    }
    probs <- counts / rowSums(counts)
    logps <- vapply(seq_along(t_toks), function(i) {
      prev <- if (i == 1L) V + 1L else match(t_toks[i - 1L], vocab)
      log(probs[prev, match(t_toks[i], vocab)])
    }, numeric(1))
  } else {
    .morie_gr_need(is.function(model), "geron_bart: model must be callable")
    logps <- as.numeric(model(corrupted, t_toks))
    .morie_gr_need(
      length(logps) == length(t_toks),
      "geron_bart: model returned the wrong number of token log-probs"
    )
    .morie_gr_need(
      all(is.finite(logps)),
      "geron_bart: model returned non-finite log-probabilities"
    )
    .morie_gr_need(
      !any(logps > 0),
      "geron_bart: model returned positive log-probabilities"
    )
  }
  loss <- -mean(logps)
  list(
    corrupted = corrupted, spans = spans, n_masked = sum(covered),
    n_spans = length(spans), loss = loss, perplexity = exp(loss),
    token_logprobs = logps, estimate = loss, n = length(t_toks),
    method = "BART text-infilling corruption scored by seq2seq reconstruction cross-entropy"
  )
}

#' AlexNet architecture resolved to shapes and parameter counts (Geron Ch 12, hmalex)
#'
#' out = floor((in - k + 2p)/s) + 1 and params = filters*(k*k*C) + filters,
#' so an input size that collapses a feature map to zero is an error
#' rather than a silent negative dimension.
#'
#' @param n_classes Output units, >= 1.
#' @param input_size Square side length.
#' @param in_channels Input channels.
#' @param dropout Rate in \[0, 1).
#' @return List with `layers`, `total_params`, `trainable_params`,
#'   `output_shape`, `flatten_dim`, `conv_params`, `fc_params`.
#' @export
morie_geron_alexnet <- function(n_classes = 1000, input_size = 227,
                                in_channels = 3, dropout = 0.5) {
  C <- as.integer(n_classes)
  .morie_gr_need(C >= 1L, "geron_alexnet: n_classes must be >= 1")
  S <- as.integer(input_size)
  .morie_gr_need(S >= 1L, "geron_alexnet: input_size must be >= 1")
  ch <- as.integer(in_channels)
  .morie_gr_need(ch >= 1L, "geron_alexnet: in_channels must be >= 1")
  p_drop <- as.numeric(dropout)
  .morie_gr_need(
    p_drop >= 0 && p_drop < 1,
    "geron_alexnet: dropout must lie in [0, 1)"
  )
  spec <- list(
    list("conv", 96L, 11L, 4L, 0L), list("pool", NA_integer_, 3L, 2L, 0L),
    list("conv", 256L, 5L, 1L, 2L), list("pool", NA_integer_, 3L, 2L, 0L),
    list("conv", 384L, 3L, 1L, 1L), list("conv", 384L, 3L, 1L, 1L),
    list("conv", 256L, 3L, 1L, 1L), list("pool", NA_integer_, 3L, 2L, 0L),
    list("fc", 4096L, NA_integer_, NA_integer_, NA_integer_),
    list("fc", 4096L, NA_integer_, NA_integer_, NA_integer_)
  )
  layers <- list()
  size <- S
  channels <- ch
  flatten_dim <- NA_integer_
  for (sp in spec) {
    kind <- sp[[1]]
    units <- sp[[2]]
    k <- sp[[3]]
    s <- sp[[4]]
    pad <- sp[[5]]
    if (kind %in% c("conv", "pool")) {
      out <- ((size - k + 2L * pad) %/% s) + 1L
      .morie_gr_need(
        out >= 1L,
        "geron_alexnet: input_size is too small for this architecture"
      )
      params <- if (kind == "conv") {
        pr <- units * (k * k * channels) + units
        channels <- units
        pr
      } else {
        0L
      }
      layers[[length(layers) + 1L]] <- list(
        kind = kind, filters = units,
        kernel = k, stride = s, pad = pad,
        out = out, params = params
      )
      size <- out
    } else {
      if (is.na(flatten_dim)) {
        flatten_dim <- size * size * channels
        in_units <- flatten_dim
      } else {
        in_units <- layers[[length(layers)]]$filters
      }
      layers[[length(layers) + 1L]] <- list(
        kind = "fc", filters = units,
        "in" = in_units, out = units,
        params = in_units * units + units,
        dropout = p_drop
      )
    }
  }
  last_hidden <- layers[[length(layers)]]$filters
  layers[[length(layers) + 1L]] <- list(
    kind = "fc", filters = C,
    "in" = last_hidden, out = C,
    params = last_hidden * C + C,
    dropout = 0
  )
  total <- sum(vapply(layers, function(l) as.numeric(l$params), numeric(1)))
  conv_p <- sum(vapply(layers, function(l) {
    if (l$kind == "conv") as.numeric(l$params) else 0
  }, numeric(1)))
  fc_p <- sum(vapply(layers, function(l) {
    if (l$kind == "fc") as.numeric(l$params) else 0
  }, numeric(1)))
  list(
    layers = layers, total_params = total, trainable_params = total,
    output_shape = C, flatten_dim = flatten_dim, conv_params = conv_p,
    fc_params = fc_p, dropout = p_drop, estimate = total,
    n = length(layers),
    method = "AlexNet architecture resolved to concrete shapes and parameter counts"
  )
}
