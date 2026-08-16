# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Wave-4a Geron ports. Python canonical sources:
#   hmcst hmdae hmdale hmdbd hmdbrt hmdbs hmdcg hmdctr hmddim hmdfw hmdqn
#   hmgmm hmdrp hmfa hmddpg hmddpm hmddqn hmdeit hmdetr hmdino hmdld
#   hmdldqn hmencox
# (src/morie/fn/<name>.py). Modules that DELEGATE their numeric core to a
# gr* function reuse the ALREADY-EXPORTED morie_geron_* core from
# geron_ml_native.R (grctr->morie_geron_contrastive_infonce, grdae->
# morie_geron_denoising_autoencoder, grdal->morie_geron_dalle_autoregressive_token,
# grdbs->morie_geron_dbscan_core_point, grdcgan->morie_geron_dcgan_generator,
# grddim->morie_geron_ddim_sampling_step, grddqn->morie_geron_double_dqn_target,
# grdeit->morie_geron_deit_distillation_loss, grdetr->morie_geron_detr_hungarian_matching).
# Only the code NOT already ported is written here.
#
# Remaining 34 modules in rw4_a.txt were not reached in this pass (see
# report); they still need the same read+wrap treatment.

#' .w4a_need
#'
#' Part of the geron_w4a_native implementation; see the file header for
#' the source it follows.
#'
#' @param cond See Usage.
#' @param msg See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.w4a_need <- function(cond, msg) if (!isTRUE(cond)) stop(msg, call. = FALSE)

#' .w4a_lcg_u
#'
#' Part of the geron_w4a_native implementation; see the file header for
#' the source it follows.
#'
#' @param n See Usage.
#' @param seed See Usage.
#' @return The value of \code{u}, as built in the body.
#' @export
.w4a_lcg_u <- function(n, seed) {
  s <- as.integer(seed) %% 2^32
  u <- numeric(n)
  for (i in seq_len(n)) {
    s <- (1664525 * s + 1013904223) %% 2^32
    u[i] <- (s + 0.5) / 2^32
  }
  u
}

#' Deterministic standard normals from the shared LCG + Box-Muller (hmdfw)
#'
#' Draw-for-draw match to Python `morie.fn.hmdfw.lcg_normal`: odd-length
#' shapes draw one extra pair and discard the last value.
#' @param shape Integer vector of output dimensions.
#' @param seed LCG seed.
#' @return Array of `shape` with row-major (Python) fill order.
#' @export
morie_lcg_normal <- function(shape, seed) {
  n <- prod(shape)
  m <- n + (n %% 2)
  u <- .w4a_lcg_u(m, seed)
  u1 <- u[seq(1, m, by = 2)]
  u2 <- u[seq(2, m, by = 2)]
  r <- sqrt(-2 * log(u1))
  z <- numeric(m)
  z[seq(1, m, by = 2)] <- r * cos(2 * pi * u2)
  z[seq(2, m, by = 2)] <- r * sin(2 * pi * u2)
  z <- z[seq_len(n)]
  if (length(shape) <= 1L) {
    return(z)
  }
  array(aperm(array(z, dim = rev(shape)), rev(seq_along(shape))), dim = shape)
}

#' Beta schedule values for T diffusion steps (hmdfw)
#' @param T Number of steps.
#' @param beta_schedule "linear", "cosine", or an explicit length-T vector.
#' @param beta_start,beta_end Linear-schedule endpoints.
#' @return Numeric vector of length T.
#' @export
morie_beta_schedule_values <- function(T, beta_schedule = "linear", beta_start = 1e-4, beta_end = 0.02) {
  if (is.character(beta_schedule)) {
    if (beta_schedule == "linear") {
      return(seq(beta_start, beta_end, length.out = T))
    }
    if (beta_schedule == "cosine") {
      s <- 0.008
      t <- (0:T) / T
      f <- cos((t + s) / (1 + s) * pi / 2)^2
      ab <- f / f[1]
      b <- 1 - ab[-1] / ab[-length(ab)]
      return(pmin(pmax(b, 1e-8), 0.999))
    }
    stop("beta_schedule must be 'linear', 'cosine' or a numeric vector.", call. = FALSE)
  }
  b <- as.numeric(beta_schedule)
  .w4a_need(length(b) == T, "beta_schedule has wrong length for T.")
  b
}

#' Diffusion forward process: chain and closed form, cross-checked (hmdfw)
#'
#' @param x0 Clean sample.
#' @param T Diffusion steps (>=1).
#' @param beta_schedule "linear"/"cosine"/vector.
#' @param t Step to report (default T).
#' @param seed LCG seed.
#' @return List with `x_t`, `x_chain`, `betas`, `alphas`, `alpha_bar`,
#'   `signal_scale`, `noise_scale`, `noise`, `snr`, `variance_check`.
#' @export
morie_geron_diffusion_forward <- function(x0, T, beta_schedule = "linear", t = NULL, seed = 0) {
  x <- as.numeric(x0)
  .w4a_need(length(x) > 0 && all(is.finite(x)), "x0 must be non-empty and finite.")
  Ti <- as.integer(T)
  .w4a_need(Ti >= 1L, "T must be >= 1.")
  betas <- morie_beta_schedule_values(Ti, beta_schedule)
  .w4a_need(!any(betas <= 0 | betas >= 1), "every beta must lie strictly in (0, 1).")
  ti <- if (is.null(t)) Ti else as.integer(t)
  .w4a_need(ti >= 0L && ti <= Ti, "t must lie in 0..T.")

  alphas <- 1 - betas
  abar <- cumprod(alphas)

  chain <- vector("list", Ti + 1L)
  chain[[1]] <- x
  cur <- x
  for (k in seq_len(Ti)) {
    z <- morie_lcg_normal(length(x), seed + k)
    cur <- sqrt(alphas[k]) * cur + sqrt(betas[k]) * z
    chain[[k + 1L]] <- cur
  }

  if (ti == 0L) {
    xt <- x
    sig <- 1
    noi <- 0
    eps <- rep(0, length(x))
    ab_t <- 1
  } else {
    ab_t <- abar[ti]
    sig <- sqrt(ab_t)
    noi <- sqrt(1 - ab_t)
    eps <- morie_lcg_normal(length(x), seed + 10000)
    xt <- sig * x + noi * eps
  }
  chain_var <- mean((chain[[ti + 1L]] - sig * x)^2)

  list(
    x_t = xt, x_chain = chain, betas = betas, alphas = alphas,
    alpha_bar = abar, alpha_bar_t = ab_t, signal_scale = sig, noise_scale = noi,
    noise = eps, snr = if (ab_t < 1) ab_t / (1 - ab_t) else Inf,
    variance_check = list(chain_mse_from_signal = chain_var, closed_form_variance = 1 - ab_t),
    t = ti, T = Ti, estimate = sig, n = length(x),
    method = "forward diffusion, chain and closed form, deterministic LCG/Box-Muller noise"
  )
}

#' Contrastive learning with in-batch negatives (hmcst)
#'
#' Delegates the InfoNCE loss to `morie_geron_contrastive_infonce`; this
#' wrapper builds the negative set from `positives` (every other row of
#' the batch except the anchor and its positive).
#' @param embeddings Matrix (B, d), B >= 3.
#' @param positives 0-based
#'   index of each anchor's positive partner (length B).
#' @param tau Temperature.
#' @param normalize Cosine-normalise rows.
#' @return List with `loss`, `per_anchor_loss`, `pos_sim`, `neg_sim`,
#'   `hardest_negative`, `accuracy`, `chance_loss`, `n_negatives`.
#' @export
morie_geron_contrastive_learning <- function(embeddings, positives, tau = 0.1, normalize = TRUE) {
  E <- .morie_gr_mat(embeddings, "embeddings")
  B <- nrow(E)
  d <- ncol(E)
  .w4a_need(B >= 3L, "in-batch negatives need at least 3 embeddings.")
  pi_ <- as.integer(round(as.numeric(positives)))
  .w4a_need(length(pi_) == B, "positives length must equal the batch size.")
  .w4a_need(all(pi_ >= 0L & pi_ <= B - 1L), "positive indices must lie in 0..B-1.")
  .w4a_need(!any(pi_ == (seq_len(B) - 1L)), "an anchor may not be its own positive.")

  P <- E[pi_ + 1L, , drop = FALSE]
  neg_idx <- lapply(seq_len(B), function(i) setdiff(seq_len(B) - 1L, c(i - 1L, pi_[i])))
  n_neg <- length(neg_idx[[1]])
  .w4a_need(all(vapply(neg_idx, length, 0L) == n_neg), "every anchor must have the same negative count.")
  N <- array(0, dim = c(B, n_neg, d))
  for (i in seq_len(B)) N[i, , ] <- E[neg_idx[[i]] + 1L, , drop = FALSE]

  base <- morie_geron_contrastive_infonce(E, P, N, tau = tau, normalize = normalize)
  list(
    loss = base$loss, per_anchor_loss = base$per_anchor_loss, pos_sim = base$pos_sim,
    neg_sim = base$neg_sim, hardest_negative = base$hardest_negative, accuracy = base$accuracy,
    chance_loss = base$chance_loss, n_negatives = n_neg, negative_indices = neg_idx,
    tau = as.numeric(tau), estimate = base$loss, n = B,
    method = "InfoNCE with in-batch negatives; loss delegated to grctr"
  )
}

#' Denoising autoencoder, trained (hmdae)
#'
#' A linear encoder/decoder trained by gradient descent to reconstruct
#' `X` from `X + noise`. Final evaluation is delegated to
#' `morie_geron_denoising_autoencoder` (the grdae core); this function is
#' the training loop, distinct from that core by the `_train` suffix.
#' @param X Clean data (m, d).
#' @param noise_std Corruption sd (>0).
#' @param epochs,lr Training config.
#' @param hidden Code width (default d).
#' @param seed LCG seed.
#' @return List with `loss_history`, `final_loss`, `encoder`, `decoder`,
#'   `reconstruction`, `denoising_gain`, `snr_db`, `passthrough_loss`.
#' @export
morie_geron_denoising_autoencoder_train <- function(X, noise_std = 0.3, epochs = 300, lr = 0.05,
                                                    hidden = NULL, seed = 0) {
  A <- .morie_gr_mat(X, "X")
  m <- nrow(A)
  d <- ncol(A)
  sd_ <- as.numeric(noise_std)
  .w4a_need(is.finite(sd_) && sd_ > 0, "noise_std must be positive.")
  E <- as.integer(epochs)
  .w4a_need(E >= 1L, "epochs must be >= 1.")
  eta <- as.numeric(lr)
  .w4a_need(is.finite(eta) && eta > 0, "lr must be positive.")
  h <- if (is.null(hidden)) d else as.integer(hidden)
  .w4a_need(h >= 1L, "hidden must be >= 1.")

  noise <- sd_ * matrix(morie_lcg_normal(m * d, seed + 1), m, d, byrow = TRUE)
  Xt <- A + noise

  s <- as.integer(seed) %% 2^32
  draw <- function(n, scale) {
    u <- numeric(n)
    for (i in seq_len(n)) {
      s <<- (1664525 * s + 1013904223) %% 2^32
      u[i] <- (s + 0.5) / 2^32
    }
    (2 * u - 1) * sqrt(3) * scale
  }
  Wenc <- matrix(draw(d * h, 1 / sqrt(d)), d, h, byrow = TRUE)
  Vdec <- matrix(draw(h * d, 1 / sqrt(h)), h, d, byrow = TRUE)

  hist <- numeric(E)
  for (ep in seq_len(E)) {
    code <- Xt %*% Wenc
    rec <- code %*% Vdec
    diff <- rec - A
    hist[ep] <- mean(diff^2)
    gV <- (2 / (m * d)) * (t(code) %*% diff)
    gW <- (2 / (m * d)) * (t(Xt) %*% (diff %*% t(Vdec)))
    Wenc <- Wenc - eta * gW
    Vdec <- Vdec - eta * gV
    .w4a_need(all(is.finite(Wenc)) && all(is.finite(Vdec)), "training diverged; lower lr.")
  }
  rec <- (Xt %*% Wenc) %*% Vdec
  final <- mean((rec - A)^2)
  ev <- morie_geron_denoising_autoencoder(A, noise, rec, corruption = "additive")

  list(
    loss_history = hist, final_loss = final, encoder = Wenc, decoder = Vdec,
    reconstruction = rec, corrupted = Xt, noise = noise, clean_loss = ev$loss,
    denoising_gain = ev$denoising_gain, snr_db = ev$snr_db, noise_energy = ev$noise_energy,
    passthrough_loss = mean(noise^2), hidden = h, noise_std = sd_, estimate = final, n = m,
    method = "linear denoising autoencoder trained on corrupted inputs; evaluation delegated to grdae"
  )
}

#' Decision boundary for logistic regression theta^T x = 0 (hmdbd)
#' @param theta Parameters (bias first if `fit_intercept`).
#' @param X_grid Points to classify (m, n), no bias column.
#' @param fit_intercept Logical.
#' @return List with `scores`, `signed_distance`, `labels`,
#'   `probabilities`, `on_boundary`, `normal`, `margin`, `line`.
#' @export
morie_geron_decision_boundary <- function(theta, X_grid, fit_intercept = TRUE) {
  th <- as.numeric(theta)
  G <- .morie_gr_mat(X_grid, "X_grid")
  .w4a_need(length(th) > 0 && nrow(G) > 0, "theta and X_grid must be non-empty.")
  if (fit_intercept) {
    .w4a_need(length(th) == ncol(G) + 1L, "theta length must be ncol(X_grid) + 1.")
    b <- th[1]
    w <- th[-1]
  } else {
    .w4a_need(length(th) == ncol(G), "theta length must equal ncol(X_grid).")
    b <- 0
    w <- th
  }
  nw <- sqrt(sum(w^2))
  .w4a_need(nw > 0, "weight vector is zero; no hyperplane defined.")
  scores <- as.numeric(G %*% w + b)
  dist <- scores / nw
  on <- abs(dist) <= 1e-12
  labels <- as.integer(scores > 0)
  probs <- 1 / (1 + exp(-scores))
  line <- NULL
  if (length(w) == 2L) {
    line <- if (w[2] != 0) c(-w[1] / w[2], -b / w[2]) else c(Inf, -b / w[1])
  }
  list(
    scores = scores, signed_distance = dist, labels = labels, probabilities = probs,
    on_boundary = on, normal = w / nw, bias = b, margin = min(abs(dist)), line = line,
    estimate = min(abs(dist)), n = nrow(G), method = "hyperplane theta^T x = 0 with signed distances"
  )
}

#' DistilBERT triple-loss distillation (hmdbrt)
#'
#' Distillation KL (T^2-scaled) + MLM cross-entropy + optional cosine
#' hidden-state term; architectures resolved via `morie_geron_encoder_only`
#' (hmencox) for the real 12- vs 6-layer parameter reduction.
#' @param teacher,student Logits matrix (B, C), or `list(logits, hidden)`.
#' @param X Inputs (only its length is used for the token count).
#' @param temperature Distillation temperature (>=1).
#' @param alpha_ce,alpha_mlm,alpha_cos Non-negative loss weights.
#' @param mlm_labels 0-based labels, required if `alpha_mlm > 0`.
#' @return List with `loss`, `loss_ce`, `loss_mlm`, `loss_cos`,
#'   `teacher_params`, `student_params`, `param_reduction`, `agreement`.
#' @export
morie_geron_distilbert <- function(teacher, student, X, temperature = 2.0,
                                   alpha_ce = 0.5, alpha_mlm = 0.5, alpha_cos = 0.0,
                                   mlm_labels = NULL) {
  T_ <- as.numeric(temperature)
  .w4a_need(is.finite(T_) && T_ >= 1, "temperature must be >= 1.")
  a_ce <- as.numeric(alpha_ce)
  a_mlm <- as.numeric(alpha_mlm)
  a_cos <- as.numeric(alpha_cos)
  .w4a_need(min(a_ce, a_mlm, a_cos) >= 0 && (a_ce + a_mlm + a_cos) > 0, "loss weights must be non-negative and not all zero.")

  unpack <- function(v) {
    if (is.list(v) && !is.data.frame(v) && length(v) == 2 && is.null(dim(v[[1]]))) {
      list(logits = .morie_gr_mat(v[[1]], "logits"), hidden = .morie_gr_mat(v[[2]], "hidden"))
    } else {
      list(logits = .morie_gr_mat(v, "logits"), hidden = NULL)
    }
  }
  Zt <- unpack(teacher)
  Zs <- unpack(student)
  .w4a_need(all(dim(Zt$logits) == dim(Zs$logits)), "teacher/student logits shape mismatch.")
  B <- nrow(Zt$logits)
  C <- ncol(Zt$logits)

  logsoftmax <- function(Z, temp) {
    Zn <- Z / temp
    Zn <- Zn - apply(Zn, 1, max)
    Zn - log(rowSums(exp(Zn)))
  }
  lpt <- logsoftmax(Zt$logits, T_)
  lps <- logsoftmax(Zs$logits, T_)
  pt <- exp(lpt)
  loss_ce <- T_ * T_ * mean(rowSums(pt * (lpt - lps)))

  loss_mlm <- 0
  if (a_mlm > 0) {
    .w4a_need(!is.null(mlm_labels), "alpha_mlm > 0 requires mlm_labels.")
    y <- as.integer(round(as.numeric(mlm_labels)))
    .w4a_need(length(y) == B && all(y >= 0 & y <= C - 1L), "mlm_labels invalid.")
    lp1 <- logsoftmax(Zs$logits, 1.0)
    loss_mlm <- -mean(lp1[cbind(seq_len(B), y + 1L)])
  }

  loss_cos <- 0
  if (a_cos > 0) {
    .w4a_need(!is.null(Zs$hidden) && !is.null(Zt$hidden), "alpha_cos > 0 requires hidden states.")
    ns <- sqrt(rowSums(Zs$hidden^2))
    nt <- sqrt(rowSums(Zt$hidden^2))
    .w4a_need(!any(ns == 0) && !any(nt == 0), "a hidden state is zero; cosine undefined.")
    loss_cos <- mean(1 - rowSums(Zs$hidden * Zt$hidden) / (ns * nt))
  }
  total <- a_ce * loss_ce + a_mlm * loss_mlm + a_cos * loss_cos

  tokens <- max(length(as.numeric(X)), 1L)
  teach <- morie_geron_encoder_only(seq_len(tokens) - 1L, n_layers = 12)
  stud <- morie_geron_encoder_only(seq_len(tokens) - 1L, n_layers = 6)
  tp <- teach$total_params
  sp <- stud$total_params

  list(
    loss = total, loss_ce = loss_ce, loss_mlm = loss_mlm, loss_cos = loss_cos,
    teacher_params = tp, student_params = sp, param_reduction = 1 - sp / tp,
    agreement = mean(max.col(Zt$logits) == max.col(Zs$logits)), temperature = T_,
    weights = list(ce = a_ce, mlm = a_mlm, cos = a_cos), estimate = total, n = B,
    method = "DistilBERT triple loss with T^2-scaled KL; architectures resolved through hmencox"
  )
}

#' DBSCAN clustering, BFS over density-connected cores (hmdbs)
#'
#' Core/border/noise classification is delegated to
#' `morie_geron_dbscan_core_point`; this wrapper merges cores into
#' clusters and assigns border points.
#' @param X Points (m, d).
#' @param eps Neighbourhood radius (>0).
#' @param min_samples Core threshold (>=1).
#' @param metric "euclidean"/
#'   "manhattan"/"chebyshev".
#' @return List with `labels`, `n_clusters`, `n_noise`, `n_core`,
#'   `n_border`, `core_indices`, `cluster_sizes`.
#' @export
morie_geron_dbscan <- function(X, eps, min_samples, metric = "euclidean") {
  base <- morie_geron_dbscan_core_point(X, eps = eps, min_samples = min_samples, metric = metric)
  is_core <- base$is_core
  neighbors <- base$neighbors
  m <- length(is_core)
  labels <- rep(-1L, m)
  cid <- 0L
  for (i in seq_len(m)) {
    if (!is_core[i] || labels[i] != -1L) next
    labels[i] <- cid
    queue <- i
    while (length(queue) > 0) {
      p <- queue[1]
      queue <- queue[-1]
      for (q in neighbors[[p]] + 1L) {
        if (labels[q] == -1L) {
          labels[q] <- cid
          if (is_core[q]) queue <- c(queue, q)
        }
      }
    }
    cid <- cid + 1L
  }
  sizes <- vapply(seq_len(cid) - 1L, function(c) sum(labels == c), 0L)
  n_border <- sum(labels >= 0 & !is_core)
  list(
    labels = labels, n_clusters = cid, n_noise = sum(labels == -1L), n_core = base$n_core,
    n_border = n_border, core_indices = which(is_core) - 1L, cluster_sizes = sizes,
    is_core = is_core, neighbor_counts = base$neighbor_counts, eps = as.numeric(eps),
    min_samples = as.integer(min_samples), metric = metric, estimate = cid, n = m,
    method = "DBSCAN by BFS over density-connected cores; core detection delegated to grdbs"
  )
}

#' DCGAN generator/discriminator resolved to concrete shapes (hmdcg)
#'
#' Architecture spec (like hmalex): layer-by-layer channel/param counts
#' for a square image reached from `seed_shape` by stride-2 transposed
#' convs. One generator forward pass on unit kernels is delegated to
#' `morie_geron_dcgan_generator` to demonstrate the resolved shapes.
#' @param X Real images (H,W) or (m,H,W).
#' @param z_dim,filters int.
#' @param epochs,lr Training config (recorded only; see hmgan for the loop).
#' @param seed_shape c(h0, w0).
#' @param stride Upsample stride (>=2).
#' @return List with `generator_layers`, `discriminator_layers`,
#'   `generator_params`, `discriminator_params`, `total_params`, `n_layers`.
#' @export
morie_geron_dcgan <- function(X, z_dim = 100, filters = 64, epochs = 50, lr = 0.0002,
                              seed_shape = c(4, 4), stride = 2) {
  A <- if (length(dim(X)) == 2) array(as.numeric(X), dim = c(1, dim(X))) else array(as.numeric(X), dim = dim(X))
  m <- dim(A)[1]
  H <- dim(A)[2]
  W <- dim(A)[3]
  .w4a_need(H == W, "DCGAN assumes square images.")
  k <- as.integer(z_dim)
  f <- as.integer(filters)
  st <- as.integer(stride)
  .w4a_need(st >= 2L, "stride must be >= 2.")
  h0 <- as.integer(seed_shape[1])
  w0 <- as.integer(seed_shape[2])
  ratio <- H / h0
  L <- if (ratio >= 1) as.integer(round(log(ratio) / log(st))) else -1L
  .w4a_need(
    L >= 1L && h0 * st^L == H,
    sprintf("image side %d is not %d times a power of %d.", H, h0, st)
  )

  kern <- 4L
  ch0 <- f * st^(L - 1L)
  gen <- list(list(
    kind = "project", `in` = k, out = h0, channels = ch0,
    params = as.integer(k * (h0 * w0 * ch0) + h0 * w0 * ch0)
  ))
  size <- h0
  ch <- ch0
  for (i in seq_len(L)) {
    out_ch <- if (i == L) 1L else ch %/% st
    size <- size * st
    gen[[length(gen) + 1L]] <- list(
      kind = "deconv", in_channels = ch, channels = out_ch,
      kernel = kern, stride = st, out = as.integer(size),
      params = as.integer(out_ch * (kern * kern * ch) + out_ch),
      batch_norm = i < L, activation = if (i == L) "tanh" else "relu"
    )
    ch <- out_ch
  }
  dis <- list()
  size <- H
  ch <- 1L
  for (i in seq_len(L)) {
    out_ch <- if (i == 1L) f else ch * st
    size <- size %/% st
    dis[[length(dis) + 1L]] <- list(
      kind = "conv", in_channels = ch, channels = out_ch,
      kernel = kern, stride = st, out = as.integer(size),
      params = as.integer(out_ch * (kern * kern * ch) + out_ch),
      batch_norm = i > 1L, activation = "leaky_relu"
    )
    ch <- out_ch
  }
  flat <- as.integer(size * size * ch)
  dis[[length(dis) + 1L]] <- list(kind = "fc", `in` = flat, out = 1L, params = flat + 1L, activation = "sigmoid")

  g_params <- sum(vapply(gen, function(l) l$params, 0L))
  d_params <- sum(vapply(dis, function(l) l$params, 0L))

  Wproj <- matrix(1 / k, k, h0 * w0)
  kernels <- rep(list(matrix(1, st, st)), L)
  demo <- morie_geron_dcgan_generator(rep(1, k), c(list(Wproj), kernels), seed_shape = c(h0, w0), stride = st)

  list(
    generator_layers = gen, discriminator_layers = dis, generator_params = g_params,
    discriminator_params = d_params, total_params = g_params + d_params,
    image_shape = c(H, W), n_layers = L, z_dim = k, sample_shape = demo$image_shape,
    training_config = list(epochs = as.integer(epochs), lr = as.numeric(lr), adam_beta1 = 0.5),
    estimate = g_params + d_params, n = m,
    method = "DCGAN generator/discriminator resolved to concrete shapes; forward pass delegated to grdcgan"
  )
}

#' Exact transformer block parameter count, itemised (hmdctr)
#' @param d_model Model width.
#' @param d_ff FFN width (default 4*d_model).
#' @param cross_attention Add a cross-attention sub-layer + norm.
#' @return List with `self_attention`, `ffn`, `layer_norms`,
#'   (`cross_attention`), `total`.
#' @export
morie_geron_block_params <- function(d_model, d_ff = NULL, cross_attention = FALSE) {
  d <- as.integer(d_model)
  .w4a_need(d >= 1L, "d_model must be >= 1.")
  ff <- if (is.null(d_ff)) 4L * d else as.integer(d_ff)
  .w4a_need(ff >= 1L, "d_ff must be >= 1.")
  attn <- 4L * d * d + 4L * d
  ffn <- 2L * d * ff + ff + d
  norms <- 2L * (2L * d)
  out <- list(self_attention = attn, ffn = ffn, layer_norms = norms)
  if (cross_attention) {
    out$cross_attention <- attn
    out$layer_norms <- norms + 2L * d
  }
  out$total <- sum(unlist(out))
  out
}

#' Causal mask: TRUE where attention is forbidden (strictly future) (hmdctr)
#' @param n Sequence length.
#' @return n x n logical matrix, upper-triangular (excl. diagonal) TRUE.
#' @export
morie_geron_causal_mask <- function(n) {
  n <- as.integer(n)
  .w4a_need(n >= 1L, "n must be >= 1.")
  m <- matrix(FALSE, n, n)
  if (n > 1L) for (i in seq_len(n - 1L)) m[i, (i + 1L):n] <- TRUE
  m
}

#' Decoder-only transformer (GPT family), resolved to exact param counts (hmdctr)
#' @param X Token ids, used for length only.
#' @param n_layers,n_heads int.
#' @param d_model,vocab_size,max_len int.
#' @param d_ff FFN width (default 4*d_model).
#' @param tie_embeddings Reuse token embedding as output head.
#' @return List with `total_params`, `block_params`, `per_block`,
#'   `embedding_params`, `d_head`, `mask`, `seq_len`.
#' @export
morie_geron_decoder_only <- function(X, n_layers = 12, n_heads = 12, d_model = 768,
                                     vocab_size = 50257, max_len = 1024, d_ff = NULL,
                                     tie_embeddings = TRUE) {
  T_ <- length(X)
  .w4a_need(T_ > 0L, "X must contain at least one token.")
  L <- as.integer(n_layers)
  Hh <- as.integer(n_heads)
  d <- as.integer(d_model)
  V <- as.integer(vocab_size)
  M <- as.integer(max_len)
  .w4a_need(d %% Hh == 0L, "d_model must be divisible by n_heads.")
  .w4a_need(T_ <= M, "sequence length exceeds max_len.")
  per <- morie_geron_block_params(d, d_ff = d_ff, cross_attention = FALSE)
  emb <- V * d + M * d
  head <- if (tie_embeddings) 0L else V * d + V
  final_norm <- 2L * d
  total <- emb + L * per$total + final_norm + head
  list(
    total_params = total, block_params = per$total, per_block = per, embedding_params = emb,
    output_head_params = head, d_head = d %/% Hh, mask = morie_geron_causal_mask(T_), seq_len = T_,
    n_layers = L, n_heads = Hh, d_model = d, d_ff = if (is.null(d_ff)) 4L * d else as.integer(d_ff),
    vocab_size = V, max_len = M, tie_embeddings = as.logical(tie_embeddings),
    flops_per_token = 2 * total, estimate = total, n = L,
    method = "decoder-only transformer resolved to exact parameter counts and a causal mask"
  )
}

#' Encoder-only transformer (BERT-family), resolved to exact param counts (hmencox)
#' @param X Token ids (raw, before CLS/SEP).
#' @param n_layers,n_heads int.
#' @param d_model,vocab_size,max_len int.
#' @param d_ff FFN width.
#' @param n_segments Segment embedding rows.
#' @param n_classes Optional CLS head.
#' @return List with `total_params`, `block_params`, `embedding_params`,
#'   `head_params`, `attention_mask`, `seq_len`, `cls_index`.
#' @export
morie_geron_encoder_only <- function(X, n_layers = 12, n_heads = 12, d_model = 768,
                                     vocab_size = 30522, max_len = 512, d_ff = NULL,
                                     n_segments = 2, n_classes = NULL) {
  raw <- length(X)
  .w4a_need(raw > 0L, "X must contain at least one token.")
  T_ <- raw + 2L
  L <- as.integer(n_layers)
  Hh <- as.integer(n_heads)
  d <- as.integer(d_model)
  V <- as.integer(vocab_size)
  M <- as.integer(max_len)
  S <- as.integer(n_segments)
  .w4a_need(d %% Hh == 0L, "d_model must be divisible by n_heads.")
  .w4a_need(T_ <= M, "sequence exceeds max_len with CLS/SEP added.")
  per <- morie_geron_block_params(d, d_ff = d_ff, cross_attention = FALSE)
  emb <- V * d + M * d + S * d
  head <- if (is.null(n_classes)) {
    0L
  } else {
    .w4a_need(as.integer(n_classes) >= 2L, "n_classes must be >= 2.")
    as.integer(n_classes) * d + as.integer(n_classes)
  }
  final_norm <- 2L * d
  total <- emb + L * per$total + final_norm + head
  list(
    total_params = total, block_params = per$total, per_block = per, embedding_params = emb,
    head_params = head, attention_mask = matrix(FALSE, T_, T_), seq_len = T_, raw_len = raw,
    cls_index = 0L, sep_index = T_ - 1L, is_bidirectional = TRUE, d_head = d %/% Hh,
    n_layers = L, n_heads = Hh, d_model = d, vocab_size = V, max_len = M,
    estimate = total, n = L,
    method = "encoder-only transformer resolved to exact parameter counts; block cost delegated to hmdctr"
  )
}

#' DALL-E autoregressive image-token generation (hmdale)
#'
#' Per-step scoring is delegated to
#' `morie_geron_dalle_autoregressive_token`; this wrapper runs the
#' generation loop and reshapes into the token grid.
#' @param text Prompt token ids.
#' @param model function(context) -> logits.
#' @param n_image_tokens int (>=1).
#' @param temperature,top_k Sampling knobs.
#' @param image_vocab Codebook size (default model width).
#' @param grid c(h,w).
#' @return List with `image_tokens`, `token_grid`, `log_likelihood`,
#'   `perplexity`, `n_steps`.
#' @export
morie_geron_dalle <- function(text, model, n_image_tokens = 4, temperature = 1.0,
                              top_k = NULL, image_vocab = NULL, grid = NULL) {
  .w4a_need(is.function(model), "model must be callable(context) -> logits.")
  prompt <- as.integer(round(as.numeric(text)))
  .w4a_need(length(prompt) > 0L, "text prompt is empty.")
  N <- as.integer(n_image_tokens)
  .w4a_need(N >= 1L, "n_image_tokens must be >= 1.")

  tokens <- integer(0)
  logprobs <- numeric(N)
  V0 <- NULL
  for (step in seq_len(N)) {
    ctx <- c(prompt, tokens)
    logits <- as.numeric(model(ctx))
    if (is.null(V0)) {
      V0 <- length(logits)
    } else {
      .w4a_need(length(logits) == V0, "model vocabulary must be constant across steps.")
    }
    step_res <- morie_geron_dalle_autoregressive_token(prompt, tokens, model, temperature = temperature, top_k = top_k)
    probs <- as.numeric(step_res$next_token_probs)
    nxt <- which.max(probs) - 1L
    shift <- logits - max(logits)
    lp <- shift[nxt + 1L] - log(sum(exp(shift)))
    logprobs[step] <- lp
    tokens <- c(tokens, nxt)
  }
  V <- V0
  vocab <- if (is.null(image_vocab)) V else as.integer(image_vocab)
  .w4a_need(max(tokens) < vocab, "generated token outside the codebook.")

  if (is.null(grid)) {
    side <- as.integer(round(sqrt(N)))
    gr <- if (side * side == N) c(side, side) else c(1L, N)
  } else {
    gr <- as.integer(grid)
    .w4a_need(gr[1] * gr[2] == N, "grid does not hold n_image_tokens.")
  }
  ll <- sum(logprobs)
  list(
    image_tokens = tokens, token_grid = matrix(tokens, gr[1], gr[2], byrow = TRUE),
    log_likelihood = ll, token_logprobs = logprobs, perplexity = exp(-ll / N),
    context = c(prompt, tokens), prompt = prompt, n_steps = N, vocab_size = vocab, grid = gr,
    temperature = as.numeric(temperature), estimate = ll, n = N,
    method = "autoregressive image-token generation; per-step scoring delegated to grdal"
  )
}

#' DDIM deterministic sub-sequence sampling (hmddim)
#'
#' Each reverse step is delegated to `morie_geron_ddim_sampling_step`;
#' this wrapper picks the `n_steps` evenly-spaced timesteps out of `T`.
#' @param x_T Starting noise.
#' @param model function(x_t, t) -> eps.
#' @param T Training schedule length.
#' @param n_steps Reverse steps taken.
#' @param beta_schedule "linear"/"cosine"/vector.
#' @param clip_x0 c(lo, hi).
#' @return List with `x_0`, `trajectory`, `timesteps`, `model_calls`, `speedup`.
#' @export
morie_geron_ddim <- function(x_T, model, T, n_steps, beta_schedule = "linear", clip_x0 = NULL) {
  x <- as.numeric(x_T)
  .w4a_need(length(x) > 0L, "x_T is empty.")
  .w4a_need(is.function(model), "model must be callable(x_t, t) -> eps.")
  Ti <- as.integer(T)
  K <- as.integer(n_steps)
  .w4a_need(K >= 1L && K <= Ti, "n_steps must lie in 1..T.")
  betas <- morie_beta_schedule_values(Ti, beta_schedule)
  .w4a_need(!any(betas <= 0 | betas >= 1), "every beta must lie strictly in (0, 1).")
  abar <- c(1.0, cumprod(1 - betas)) # abar[1] = t=0 (clean), abar[k+1] = t=k

  steps <- sort(unique(as.integer(round(seq(Ti, 1, length.out = K)))), decreasing = TRUE)
  seq_t <- c(steps, 0L)

  cur <- x
  traj <- list(cur)
  x0s <- list()
  calls <- 0L
  for (i in seq_len(length(seq_t) - 1L)) {
    t <- seq_t[i]
    t_prev <- seq_t[i + 1L]
    eps <- as.numeric(model(cur, t))
    calls <- calls + 1L
    step <- morie_geron_ddim_sampling_step(cur, t = t, t_prev = t_prev, eps_pred = eps, alpha_bar = abar, clip_x0 = clip_x0)
    x0s[[length(x0s) + 1L]] <- step$x0_pred
    cur <- as.numeric(step$x_prev)
    traj[[length(traj) + 1L]] <- cur
  }
  list(
    x_0 = cur, trajectory = traj, timesteps = steps, x0_preds = x0s, model_calls = calls,
    speedup = Ti / length(steps), alpha_bar = abar, T = Ti, n_steps = length(steps),
    estimate = mean(cur), n = length(x),
    method = "DDIM sub-sequence sampling; each step delegated to grddim"
  )
}

#' Validate a DQN replay buffer of (s, a, r, s2\[, done\]) rows (hmdqn)
#' @param buffer List of length-4/5 transitions.
#' @param n_states,n_actions Bounds.
#' @param name Caller name for error messages.
#' @return List with `s`,`a`,`r`,`s2`,`done` (0-based indices as given).
#' @export
morie_check_buffer <- function(buffer, n_states, n_actions, name) {
  .w4a_need(length(buffer) > 0L, paste0(name, ": buffer is empty."))
  s <- integer(0)
  a <- integer(0)
  r <- numeric(0)
  s2 <- integer(0)
  d <- logical(0)
  for (tr in buffer) {
    .w4a_need(length(tr) %in% c(4L, 5L), paste0(name, ": transition must have 4 or 5 fields."))
    s <- c(s, as.integer(tr[[1]]))
    a <- c(a, as.integer(tr[[2]]))
    r <- c(r, as.numeric(tr[[3]]))
    s2 <- c(s2, as.integer(tr[[4]]))
    d <- c(d, if (length(tr) == 5L) as.logical(tr[[5]]) else FALSE)
  }
  .w4a_need(min(s) >= 0L && max(s) < n_states && min(s2) >= 0L && max(s2) < n_states, paste0(name, ": state index out of range."))
  .w4a_need(min(a) >= 0L && max(a) < n_actions, paste0(name, ": action index out of range."))
  list(s = s, a = a, r = r, s2 = s2, done = d)
}

#' Tabular DQN: replay mini-batches + periodically synced target net (hmdqn)
#' @param env Kept for provenance only (unused).
#' @param Q,Q_target (S,A) tables.
#' @param buffer Transitions.
#' @param epochs,lr,gamma,target_sync,batch_size Config.
#' @return List with `Q`, `Q_target`, `loss_history`, `greedy_policy`, `sync_epochs`.
#' @export
morie_geron_dqn <- function(env, Q, Q_target, buffer, epochs = 10, lr = 0.1, gamma = 0.95,
                            target_sync = 5, batch_size = NULL) {
  Qa <- .morie_gr_mat(Q, "Q")
  Qt <- .morie_gr_mat(Q_target, "Q_target")
  S <- nrow(Qa)
  A <- ncol(Qa)
  buf <- morie_check_buffer(buffer, S, A, "geron_dqn")
  s <- buf$s
  a <- buf$a
  r <- buf$r
  s2 <- buf$s2
  done <- buf$done
  E <- as.integer(epochs)
  eta <- as.numeric(lr)
  g <- as.numeric(gamma)
  sync <- as.integer(target_sync)
  N <- length(s)
  bs <- if (is.null(batch_size)) N else as.integer(batch_size)
  .w4a_need(bs >= 1L && bs <= N, "batch_size out of range.")

  hist <- numeric(0)
  syncs <- integer(0)
  td_last <- NULL
  updates <- 0L
  pos <- 0L
  for (ep in seq_len(E)) {
    idx <- (pos + seq_len(bs) - 1L) %% N
    pos <- (pos + bs) %% N
    boot <- ifelse(done[idx + 1L], 0.0, g * apply(Qt[s2[idx + 1L] + 1L, , drop = FALSE], 1, max))
    target <- r[idx + 1L] + boot
    td <- target - Qa[cbind(s[idx + 1L] + 1L, a[idx + 1L] + 1L)]
    for (k in seq_along(idx)) {
      j <- idx[k] + 1L
      Qa[s[j] + 1L, a[j] + 1L] <- Qa[s[j] + 1L, a[j] + 1L] + eta * td[k]
      updates <- updates + 1L
    }
    hist <- c(hist, mean(td^2))
    td_last <- td
    if (ep %% sync == 0L) {
      Qt <- Qa
      syncs <- c(syncs, ep)
    }
  }
  list(
    Q = Qa, Q_target = Qt, loss_history = hist, td_errors = td_last,
    greedy_policy = apply(Qa, 1, which.max) - 1L, state_values = apply(Qa, 1, max),
    sync_epochs = syncs, n_updates = updates, gamma = g, lr = eta, estimate = hist[length(hist)], n = N,
    method = "tabular DQN with replay mini-batches and a periodically synced target network"
  )
}

#' Log density of a multivariate normal at every row of X (hmgmm)
#' @param X Points (m, d).
#' @param mu Mean vector.
#' @param Sigma Covariance (d, d).
#' @return Numeric vector length m.
#' @export
morie_gmm_log_pdf <- function(X, mu, Sigma) {
  d <- ncol(X)
  ch <- chol(Sigma) # errors if not PD, matching Python's slogdet sign check
  logdet <- 2 * sum(log(diag(ch)))
  diff <- sweep(X, 2, mu, "-")
  sol <- t(solve(Sigma, t(diff)))
  quad <- rowSums(diff * sol)
  -0.5 * (d * log(2 * pi) + logdet + quad)
}

#' Gaussian mixture model fit via EM, log-space responsibilities (hmgmm)
#' @param X Data (m, d).
#' @param n_components int (>=1, <=m).
#' @param seed LCG seed.
#' @param max_iter,tol,reg EM controls.
#' @return List with `weights`, `means`, `covariances`, `responsibilities`,
#'   `labels`, `log_likelihood`, `ll_history`, `n_iter`, `converged`, `monotone`.
#' @export
morie_geron_gaussian_mixture <- function(X, n_components = 2, seed = 0, max_iter = 100,
                                         tol = 1e-6, reg = 1e-6) {
  A <- .morie_gr_mat(X, "X")
  m <- nrow(A)
  d <- ncol(A)
  K <- as.integer(n_components)
  .w4a_need(K >= 1L && K <= m, "n_components out of range.")
  Tm <- as.integer(max_iter)
  rg <- as.numeric(reg)

  s <- as.integer(seed) %% 2^32
  chosen <- integer(0)
  while (length(chosen) < K) {
    s <- (1664525 * s + 1013904223) %% 2^32
    i <- as.integer((((s + 0.5) / 2^32) * m)) %% m
    if (!(i %in% chosen)) chosen <- c(chosen, i)
  }
  mu <- A[chosen + 1L, , drop = FALSE]
  d2 <- sapply(seq_len(K), function(k) rowSums(sweep(A, 2, mu[k, ], "-")^2))
  hard <- max.col(-d2)
  cov0 <- (cov(A) * (m - 1) / m) + rg * diag(d)
  Sig <- array(0, dim = c(K, d, d))
  pi_ <- numeric(K)
  for (k in seq_len(K)) {
    sel <- A[hard == k, , drop = FALSE]
    pi_[k] <- max(nrow(sel), 1) / m
    if (nrow(sel) > 1L) {
      mu[k, ] <- colMeans(sel)
      Sig[k, , ] <- (cov(sel) * (nrow(sel) - 1) / nrow(sel)) + rg * diag(d)
    } else {
      Sig[k, , ] <- cov0 / max(K, 1)
    }
    ok <- all(is.finite(Sig[k, , ])) && (det(matrix(Sig[k, , ], d, d)) > 0)
    if (!ok) Sig[k, , ] <- diag(d) * (sum(diag(cov0)) / d / max(K, 1) + rg)
  }
  pi_ <- pi_ / sum(pi_)

  ll_hist <- numeric(0)
  converged <- FALSE
  it <- 0L
  for (it in seq_len(Tm)) {
    logp <- sapply(seq_len(K), function(k) log(pi_[k] + 1e-300) + morie_gmm_log_pdf(A, mu[k, ], matrix(Sig[k, , ], d, d)))
    mx <- apply(logp, 1, max)
    lse <- mx + log(rowSums(exp(logp - mx)))
    R <- exp(logp - lse)
    ll <- sum(lse)
    ll_hist <- c(ll_hist, ll)
    Nk <- colSums(R)
    .w4a_need(all(Nk > 0), "a component lost all responsibility.")
    pi_ <- Nk / m
    mu <- (t(R) %*% A) / Nk
    for (k in seq_len(K)) {
      diff <- sweep(A, 2, mu[k, ], "-")
      Sig[k, , ] <- (t(diff * R[, k]) %*% diff) / Nk[k] + rg * diag(d)
    }
    if (length(ll_hist) > 2 && abs(ll_hist[length(ll_hist)] - ll_hist[length(ll_hist) - 1]) < tol) {
      converged <- TRUE
      break
    }
  }
  mono <- all(diff(ll_hist) >= -1e-8)
  list(
    weights = pi_, means = mu, covariances = Sig, responsibilities = R,
    labels = max.col(R) - 1L, log_likelihood = ll_hist[length(ll_hist)], ll_history = ll_hist,
    n_iter = it, converged = converged, monotone = mono, n_components = K,
    estimate = ll_hist[length(ll_hist)], n = m, method = "Gaussian mixture fitted by EM with log-space responsibilities"
  )
}

#' Inverted dropout: y = mask * x / (1 - p) (hmdrp)
#' @param x Activations.
#' @param p Drop probability in \[0, 1).
#' @param training If FALSE, pass through unchanged.
#' @param seed LCG seed.
#' @return List with `y`, `mask`, `scale`, `n_dropped`, `drop_fraction`.
#' @export
morie_geron_dropout_alt <- function(x, p, training = TRUE, seed = 0) {
  a <- as.numeric(x)
  .w4a_need(length(a) > 0L, "x is empty.")
  pr <- as.numeric(p)
  .w4a_need(pr >= 0 && pr < 1, "p must lie in [0, 1).")
  if (!training) {
    return(list(
      y = a, mask = rep(1, length(a)), scale = 1.0, n_dropped = 0L, drop_fraction = 0.0,
      p = pr, training = FALSE, expectation_ok = TRUE, estimate = mean(a), n = length(a),
      method = "inverted dropout, inference pass-through"
    ))
  }
  u <- .w4a_lcg_u(length(a), seed)
  mask <- as.numeric(u >= pr)
  scale <- 1 / (1 - pr)
  y <- mask * a * scale
  dropped <- as.integer(length(a) - sum(mask))
  list(
    y = y, mask = mask, scale = scale, n_dropped = dropped, drop_fraction = dropped / length(a),
    p = pr, training = TRUE, expectation_ok = TRUE, estimate = mean(y), n = length(a),
    method = "inverted dropout y = mask * x / (1 - p)"
  )
}

#' FlashAttention: tiled online-softmax, exact (hmfa)
#' @param Q,K,V Matrices.
#' @param block_size Tile side (>=1).
#' @param causal Mask future keys.
#' @return List with `output`, `direct_output`, `max_abs_error`, `n_blocks`, `peak_score_memory`.
#' @export
morie_geron_flash_attention <- function(Q, K, V, block_size = 2, causal = FALSE) {
  Qa <- .morie_gr_mat(Q, "Q")
  Ka <- .morie_gr_mat(K, "K")
  Va <- .morie_gr_mat(V, "V")
  B <- as.integer(block_size)
  .w4a_need(B >= 1L, "block_size must be >= 1.")
  N <- nrow(Qa)
  d <- ncol(Qa)
  M <- nrow(Ka)
  dv <- ncol(Va)
  if (causal) .w4a_need(N == M, "causal masking needs N == M.")
  scale <- 1 / sqrt(d)
  out <- matrix(0, N, dv)
  row_m <- rep(-Inf, N)
  row_l <- rep(0, N)
  n_blocks <- 0L

  i0s <- seq(0L, N - 1L, by = B)
  for (i0 in i0s) {
    i1 <- min(i0 + B, N)
    qi <- Qa[(i0 + 1L):i1, , drop = FALSE]
    ni <- i1 - i0
    m_i <- rep(-Inf, ni)
    l_i <- rep(0, ni)
    o_i <- matrix(0, ni, dv)
    j0s <- seq(0L, M - 1L, by = B)
    for (j0 in j0s) {
      j1 <- min(j0 + B, M)
      n_blocks <- n_blocks + 1L
      s <- (qi %*% t(Ka[(j0 + 1L):j1, , drop = FALSE])) * scale
      if (causal) {
        qi_idx <- (i0 + 1L):i1
        kj_idx <- (j0 + 1L):j1
        mask <- outer(qi_idx, kj_idx, function(a, b) b > a)
        s[mask] <- -Inf
      }
      blk_max <- apply(s, 1, max)
      m_new <- pmax(m_i, blk_max)
      m_new <- ifelse(is.finite(m_new), m_new, 0.0)
      corr <- exp(ifelse(is.finite(m_i), m_i, -Inf) - m_new)
      corr <- ifelse(is.finite(corr), corr, 0.0)
      p <- exp(s - m_new)
      p[!is.finite(p)] <- 0.0
      l_i <- corr * l_i + rowSums(p)
      o_i <- corr * o_i + p %*% Va[(j0 + 1L):j1, , drop = FALSE]
      m_i <- m_new
    }
    .w4a_need(!any(l_i == 0), "a query row has no unmasked keys.")
    out[(i0 + 1L):i1, ] <- o_i / l_i
    row_m[(i0 + 1L):i1] <- m_i
    row_l[(i0 + 1L):i1] <- l_i
  }
  S <- (Qa %*% t(Ka)) * scale
  if (causal) {
    mask <- outer(seq_len(N), seq_len(M), function(a, b) b > a)
    S[mask] <- -Inf
  }
  Sm <- S - apply(S, 1, max)
  Ex <- exp(Sm)
  direct <- (Ex / rowSums(Ex)) %*% Va
  err <- max(abs(out - direct))
  peak <- min(B, N) * min(B, M)

  list(
    output = out, direct_output = direct, max_abs_error = err, row_max = row_m, row_sum = row_l,
    n_blocks = n_blocks, block_size = B, peak_score_memory = peak, naive_score_memory = N * M,
    memory_ratio = (N * M) / peak, causal = causal, estimate = err, n = N,
    method = "tiled online-softmax attention (FlashAttention), exact"
  )
}

#' Linear DDPG: TD critic, deterministic-policy-gradient actor, Polyak targets (hmddpg)
#' @param env function(s, a) -> list(s2, r, done).
#' @param actor,critic Initial weights.
#' @param epochs,lr,gamma,tau,ou_theta,ou_sigma,seed,s0,actor_target,critic_target Config.
#' @return List with `actor`, `critic`, `actor_target`, `critic_target`,
#'   `critic_losses`, `rewards`, `actions`, `ou_noise`, `q_values`.
#' @export
morie_geron_ddpg <- function(env, actor, critic, epochs = 20, lr = 0.01, gamma = 0.95,
                             tau = 0.01, ou_theta = 0.15, ou_sigma = 0.2, seed = 0, s0 = NULL,
                             actor_target = NULL, critic_target = NULL) {
  .w4a_need(is.function(env), "env must be callable(s, a) -> list(s2, r, done).")
  th <- as.numeric(actor)
  w <- as.numeric(critic)
  .w4a_need(length(w) == length(th) + 1L, "critic must have length(actor)+1 weights.")
  E <- as.integer(epochs)
  eta <- as.numeric(lr)
  g <- as.numeric(gamma)
  tt <- as.numeric(tau)
  s <- if (is.null(s0)) rep(1, length(th)) else as.numeric(s0)
  th_t <- if (is.null(actor_target)) th else as.numeric(actor_target)
  w_t <- if (is.null(critic_target)) w else as.numeric(critic_target)
  rng <- as.integer(seed) %% 2^32
  normal1 <- function() {
    rng <<- (1664525 * rng + 1013904223) %% 2^32
    u1 <- (rng + 0.5) / 2^32
    rng <<- (1664525 * rng + 1013904223) %% 2^32
    u2 <- (rng + 0.5) / 2^32
    sqrt(-2 * log(u1)) * cos(2 * pi * u2)
  }
  noise <- 0.0
  losses <- numeric(E)
  rewards <- numeric(E)
  actions <- numeric(E)
  noises <- numeric(E)
  qs <- numeric(E)
  for (ep in seq_len(E)) {
    mu <- sum(th * s)
    noise <- noise + ou_theta * (0 - noise) + ou_sigma * normal1()
    a <- mu + noise
    out <- env(s, a)
    s2 <- as.numeric(out[[1]])
    rew <- as.numeric(out[[2]])
    done <- as.logical(out[[3]])
    feat <- c(s, a)
    q <- sum(w * feat)
    a2 <- sum(th_t * s2)
    q2 <- if (done) 0 else sum(w_t * c(s2, a2))
    td <- rew + g * q2 - q
    w <- w + eta * td * feat
    dq_da <- w[length(w)]
    th <- th + eta * dq_da * s
    th_t <- tt * th + (1 - tt) * th_t
    w_t <- tt * w + (1 - tt) * w_t
    losses[ep] <- td^2
    rewards[ep] <- rew
    actions[ep] <- a
    noises[ep] <- noise
    qs[ep] <- q
    if (!done) s <- s2
  }
  list(
    actor = th, critic = w, actor_target = th_t, critic_target = w_t, critic_losses = losses,
    rewards = rewards, actions = actions, ou_noise = noises, q_values = qs, gamma = g, tau = tt,
    estimate = mean(rewards), n = E,
    method = "linear DDPG: TD critic, deterministic policy gradient actor, Polyak targets, OU exploration"
  )
}

#' DDPM: per-timestep affine eps-model trained on the noise-prediction objective (hmddpm)
#' @param X Training data (m, d).
#' @param T Diffusion steps.
#' @param beta_schedule "linear"/"cosine"/vector.
#' @param epochs,lr,seed Training config.
#' @return List with `loss_history`, `final_loss`, `loss_by_t`, `A`, `b`, `alpha_bar`, `sample`.
#' @export
morie_geron_ddpm <- function(X, T = 10, beta_schedule = "linear", epochs = 200, lr = 0.05, seed = 0) {
  A0 <- .morie_gr_mat(X, "X")
  m <- nrow(A0)
  d <- ncol(A0)
  Ti <- as.integer(T)
  betas <- morie_beta_schedule_values(Ti, beta_schedule)
  E <- as.integer(epochs)
  eta <- as.numeric(lr)
  abar <- cumprod(1 - betas)

  eps <- array(0, dim = c(Ti, m, d))
  xt <- array(0, dim = c(Ti, m, d))
  for (t in seq_len(Ti)) {
    eps[t, , ] <- matrix(morie_lcg_normal(m * d, seed + t), m, d, byrow = TRUE)
    xt[t, , ] <- sqrt(abar[t]) * A0 + sqrt(1 - abar[t]) * matrix(eps[t, , ], m, d)
  }
  Aw <- numeric(Ti)
  bw <- matrix(0, Ti, d)
  hist <- numeric(E)
  for (ep in seq_len(E)) {
    total <- 0
    for (t in seq_len(Ti)) {
      xtt <- matrix(xt[t, , ], m, d)
      epst <- matrix(eps[t, , ], m, d)
      pred <- Aw[t] * xtt + matrix(bw[t, ], m, d, byrow = TRUE)
      diff <- pred - epst
      total <- total + mean(diff^2)
      Aw[t] <- Aw[t] - eta * mean(2 * diff * xtt)
      bw[t, ] <- bw[t, ] - eta * colMeans(2 * diff)
    }
    hist[ep] <- total / Ti
  }
  loss_by_t <- numeric(Ti)
  for (t in seq_len(Ti)) {
    xtt <- matrix(xt[t, , ], m, d)
    epst <- matrix(eps[t, , ], m, d)
    pred <- Aw[t] * xtt + matrix(bw[t, ], m, d, byrow = TRUE)
    loss_by_t[t] <- mean((pred - epst)^2)
  }
  final <- mean(loss_by_t)

  x <- matrix(morie_lcg_normal(d, seed + 999), 1, d)
  for (t in seq(Ti, 1, by = -1)) {
    e <- Aw[t] * x + matrix(bw[t, ], 1, d)
    b_ <- betas[t]
    a_ <- 1 - betas[t]
    ab_ <- abar[t]
    mu <- (x - (b_ / sqrt(1 - ab_)) * e) / sqrt(a_)
    x <- if (t > 1) mu + sqrt(b_) * matrix(morie_lcg_normal(d, seed + 500 + t), 1, d) else mu
  }
  mono <- all(diff(hist) <= 1e-9)
  list(
    loss_history = hist, final_loss = final, loss_by_t = loss_by_t, A = Aw, b = bw,
    alpha_bar = abar, betas = betas, sample = x, monotone = mono, T = Ti, estimate = final, n = m,
    method = "DDPM with a per-timestep affine eps-model trained on the noise-prediction objective"
  )
}

#' Double DQN training loop; target computed by the online/target argmax split (hmddqn)
#'
#' Target computation delegated to `morie_geron_double_dqn_target`.
#' @param env Unused (provenance only).
#' @param Q,Q_target (S,A) tables.
#' @param buffer Transitions.
#' @param epochs,lr,gamma,target_sync,batch_size Config.
#' @return List with `Q`, `Q_target`, `loss_history`, `overestimation_gap`.
#' @export
morie_geron_double_dqn <- function(env, Q, Q_target, buffer, epochs = 10, lr = 0.1, gamma = 0.95,
                                   target_sync = 5, batch_size = NULL) {
  Qa <- .morie_gr_mat(Q, "Q")
  Qt <- .morie_gr_mat(Q_target, "Q_target")
  S <- nrow(Qa)
  A <- ncol(Qa)
  buf <- morie_check_buffer(buffer, S, A, "geron_double_dqn")
  s <- buf$s
  a <- buf$a
  r <- buf$r
  s2 <- buf$s2
  done <- buf$done
  E <- as.integer(epochs)
  eta <- as.numeric(lr)
  g <- as.numeric(gamma)
  sync <- as.integer(target_sync)
  N <- length(s)
  bs <- if (is.null(batch_size)) N else as.integer(batch_size)

  hist <- numeric(0)
  syncs <- integer(0)
  targets <- vanilla <- gaps <- NULL
  pos <- 0L
  for (ep in seq_len(E)) {
    idx <- (pos + seq_len(bs) - 1L) %% N
    pos <- (pos + bs) %% N
    step <- morie_geron_double_dqn_target(Qa, Qt, s_next = s2[idx + 1L], r = r[idx + 1L], gamma = g, done = done[idx + 1L])
    tgt <- as.numeric(step$target)
    td <- tgt - Qa[cbind(s[idx + 1L] + 1L, a[idx + 1L] + 1L)]
    for (k in seq_along(idx)) {
      j <- idx[k] + 1L
      Qa[s[j] + 1L, a[j] + 1L] <- Qa[s[j] + 1L, a[j] + 1L] + eta * td[k]
    }
    hist <- c(hist, mean(td^2))
    targets <- tgt
    vanilla <- as.numeric(step$vanilla_target)
    gaps <- as.numeric(step$overestimation_gap)
    if (ep %% sync == 0L) {
      Qt <- Qa
      syncs <- c(syncs, ep)
    }
  }
  list(
    Q = Qa, Q_target = Qt, loss_history = hist, targets = targets, vanilla_targets = vanilla,
    overestimation_gap = gaps, greedy_policy = apply(Qa, 1, which.max) - 1L, sync_epochs = syncs,
    gamma = g, lr = eta, estimate = hist[length(hist)], n = N,
    method = "double DQN training loop; target computation delegated to grddqn"
  )
}

#' DeiT: ViT architecture + distillation token, resolved concretely (hmdeit)
#'
#' Block costs delegated to `morie_geron_block_params`; the distillation
#' objective (when logits are supplied) to `morie_geron_deit_distillation_loss`.
#' @param image (H,W) or (C,H,W).
#' @param patch_size,n_layers,d_model,n_heads,n_classes,in_channels int.
#' @param teacher Logits or function(image).
#' @param logits_cls,logits_dist,y,alpha Loss inputs.
#' @return List with `n_patches`, `n_tokens`, `total_params`,
#'   `distillation_overhead`, `loss`, `teacher_agreement`.
#' @export
morie_geron_deit <- function(image, patch_size = 16, n_layers = 12, teacher = NULL,
                             d_model = 384, n_heads = 6, n_classes = 1000, in_channels = 3,
                             logits_cls = NULL, logits_dist = NULL, y = NULL, alpha = 0.5) {
  dims <- if (length(dim(image)) == 2) c(1, dim(image)) else dim(image)
  C_in <- dims[1]
  H <- dims[2]
  W <- dims[3]
  P <- as.integer(patch_size)
  .w4a_need(H %% P == 0L && W %% P == 0L, "image side not divisible by patch_size.")
  L <- as.integer(n_layers)
  d <- as.integer(d_model)
  Hh <- as.integer(n_heads)
  K <- as.integer(n_classes)
  .w4a_need(d %% Hh == 0L, "d_model must be divisible by n_heads.")
  grid_h <- H %/% P
  grid_w <- W %/% P
  n_patches <- grid_h * grid_w
  n_tokens <- n_patches + 2L
  patch_embed <- P * P * C_in * d + d
  pos <- n_tokens * d
  tok <- 2L * d
  per <- morie_geron_block_params(d, cross_attention = FALSE)
  heads <- 2L * (d * K + K)
  total <- patch_embed + pos + tok + L * per$total + 2L * d + heads
  overhead <- d + (d * K + K)

  loss <- lcls <- ldist <- agree <- NULL
  if (!is.null(logits_cls) || !is.null(logits_dist)) {
    .w4a_need(
      !is.null(logits_cls) && !is.null(logits_dist) && !is.null(y) && !is.null(teacher),
      "computing the loss needs logits_cls, logits_dist, y and teacher together."
    )
    t_ <- if (is.function(teacher)) teacher(image) else teacher
    base <- morie_geron_deit_distillation_loss(logits_cls, logits_dist, y, t_, alpha = alpha)
    loss <- base$loss
    lcls <- base$loss_cls
    ldist <- base$loss_dist
    agree <- base$teacher_agreement
  }
  list(
    n_patches = n_patches, n_tokens = n_tokens, patch_grid = c(grid_h, grid_w), total_params = total,
    patch_embed_params = patch_embed, position_params = pos, block_params = per$total,
    head_params = heads, distillation_overhead = overhead, d_head = d %/% Hh, loss = loss,
    loss_cls = lcls, loss_dist = ldist, teacher_agreement = agree, alpha = as.numeric(alpha),
    estimate = if (is.null(loss)) total else loss, n = n_tokens,
    method = "DeiT architecture resolved concretely; blocks via hmdctr, distillation loss via grdeit"
  )
}

#' DETR: CNN-transformer pipeline resolved concretely; matching via Hungarian (hmdetr)
#'
#' Block costs delegated to `morie_geron_block_params`; bipartite matching
#' and set-prediction loss to `morie_geron_detr_hungarian_matching`.
#' @param image (H,W) or (C,H,W).
#' @param n_queries,n_layers,d_model,n_heads,n_classes,backbone_stride int.
#' @param pred_boxes,pred_classes,gt_boxes,gt_classes Optional matching inputs.
#' @return List with `feature_shape`, `n_tokens`, `total_params`,
#'   `matching`, `loss`, `max_detections`.
#' @export
morie_geron_detr <- function(image, n_queries = 100, n_layers = 6, d_model = 256, n_heads = 8,
                             n_classes = 91, backbone_stride = 32, pred_boxes = NULL,
                             pred_classes = NULL, gt_boxes = NULL, gt_classes = NULL) {
  dims <- if (length(dim(image)) == 2) c(1, dim(image)) else dim(image)
  H <- dims[2]
  W <- dims[3]
  st <- as.integer(backbone_stride)
  Q <- as.integer(n_queries)
  L <- as.integer(n_layers)
  d <- as.integer(d_model)
  Hh <- as.integer(n_heads)
  K <- as.integer(n_classes)
  .w4a_need(d %% Hh == 0L, "d_model must be divisible by n_heads.")
  fh <- H %/% st
  fw <- W %/% st
  .w4a_need(fh >= 1L && fw >= 1L, "image smaller than backbone stride.")
  tokens <- fh * fw
  enc <- morie_geron_block_params(d, cross_attention = FALSE)
  dec <- morie_geron_block_params(d, cross_attention = TRUE)
  proj <- 2048L * d + d
  queries <- Q * d
  heads <- (d * (K + 1L) + (K + 1L)) + (3L * (d * d) + 3L * d + d * 4L + 4L)
  total <- proj + queries + L * enc$total + L * dec$total + heads

  match <- loss <- lbox <- lcls <- NULL
  any_supplied <- !is.null(pred_boxes) || !is.null(pred_classes) || !is.null(gt_boxes) || !is.null(gt_classes)
  if (any_supplied) {
    .w4a_need(
      !is.null(pred_boxes) && !is.null(pred_classes) && !is.null(gt_boxes) && !is.null(gt_classes),
      "matching needs pred_boxes, pred_classes, gt_boxes and gt_classes together."
    )
    P <- .morie_gr_mat(pred_boxes, "pred_boxes")
    .w4a_need(nrow(P) <= Q, "more predictions than n_queries.")
    G <- .morie_gr_mat(gt_boxes, "gt_boxes")
    .w4a_need(nrow(G) <= Q, "more ground-truth objects than n_queries.")
    base <- morie_geron_detr_hungarian_matching(pred_boxes, pred_classes, gt_boxes, gt_classes)
    match <- base$matching
    loss <- base$loss
    lbox <- base$loss_bbox
    lcls <- base$loss_class
  }
  list(
    feature_shape = c(fh, fw), n_tokens = tokens, n_queries = Q, total_params = total,
    encoder_params = L * enc$total, decoder_params = L * dec$total, projection_params = proj,
    query_params = queries, head_params = heads, encoder_attention_cost = tokens * tokens,
    max_detections = Q, matching = match, loss = loss, loss_bbox = lbox, loss_class = lcls,
    estimate = if (is.null(loss)) total else loss, n = tokens,
    method = "DETR pipeline resolved concretely; blocks via hmdctr, set matching via grdetr"
  )
}

#' DINO cross-view self-distillation with centering, sharpening, momentum teacher (hmdino)
#' @param images Passed to callables, otherwise ignored.
#' @param student,teacher (V,K) logits or function.
#' @param center Running teacher center (default zeros).
#' @param tau_s,tau_t Temperatures (tau_t < tau_s).
#' @param momentum,center_momentum EMA coefficients in \[0, 1).
#' @return List with `loss`, `teacher_probs`, `student_probs`, `teacher_entropy`, `teacher_next`.
#' @export
morie_geron_dino <- function(images, student, teacher, center = NULL, tau_s = 0.1, tau_t = 0.04,
                             momentum = 0.996, center_momentum = 0.9) {
  S <- .morie_gr_mat(if (is.function(student)) student(images) else student, "student")
  Tt <- .morie_gr_mat(if (is.function(teacher)) teacher(images) else teacher, "teacher")
  .w4a_need(all(dim(S) == dim(Tt)), "student/teacher shape mismatch.")
  V <- nrow(S)
  K <- ncol(S)
  .w4a_need(V >= 2L, "need at least 2 views.")
  ts <- as.numeric(tau_s)
  tt <- as.numeric(tau_t)
  .w4a_need(tt < ts, "tau_t must be smaller than tau_s for sharpening.")
  mom <- as.numeric(momentum)
  cmom <- as.numeric(center_momentum)
  c_ <- if (is.null(center)) rep(0, K) else as.numeric(center)

  softmax <- function(Z) {
    Z <- Z - apply(Z, 1, max)
    E <- exp(Z)
    E / rowSums(E)
  }
  Pt <- softmax(sweep(Tt, 2, c_, "-") / tt)
  Ps <- softmax(S / ts)
  logPs <- log(pmax(Ps, 1e-30))

  pairs <- numeric(0)
  for (i in seq_len(V)) for (j in seq_len(V)) if (i != j) pairs <- c(pairs, -sum(Pt[i, ] * logPs[j, ]))
  loss <- mean(pairs)
  ent <- mean(-rowSums(Pt * log(pmax(Pt, 1e-30))))
  kl_unif <- mean(rowSums(Pt * (log(pmax(Pt, 1e-30)) + log(K))))
  c_next <- cmom * c_ + (1 - cmom) * colMeans(Tt)
  t_next <- mom * Tt + (1 - mom) * S

  list(
    loss = loss, per_pair_loss = pairs, teacher_probs = Pt, student_probs = Ps,
    teacher_entropy = ent, max_entropy = log(K), kl_to_uniform = kl_unif, center_next = c_next,
    teacher_next = t_next, n_views = V, n_pairs = length(pairs), tau_s = ts, tau_t = tt,
    momentum = mom, estimate = loss, n = V,
    method = "DINO cross-view self-distillation with centering, sharpening and a momentum teacher"
  )
}

#' Mini-batch index plan with deterministic Fisher-Yates shuffling (hmdld)
#' @param dataset Array (batched on dim 1) or an integer length.
#' @param batch_size int (>=1).
#' @param shuffle,drop_last Logical.
#' @param seed LCG seed.
#' @param num_workers Non-negative int, only affects `worker_assignment`.
#' @return List with `batches` (0-based indices), `order`, `n_batches`, `dropped`.
#' @export
morie_geron_dataloader <- function(dataset, batch_size, shuffle = FALSE, drop_last = FALSE,
                                   seed = 0, num_workers = 0) {
  bs <- as.integer(batch_size)
  .w4a_need(bs >= 1L, "batch_size must be >= 1.")
  nw <- as.integer(num_workers)
  .w4a_need(nw >= 0L, "num_workers must be non-negative.")
  data <- NULL
  if (length(dataset) == 1L && is.numeric(dataset) && is.null(dim(dataset))) {
    m <- as.integer(dataset)
  } else {
    data <- dataset
    m <- if (is.matrix(data) || is.array(data)) dim(data)[1] else length(data)
  }
  .w4a_need(m >= 1L, "dataset must contain at least one item.")

  order <- 0:(m - 1L)
  if (shuffle) {
    u <- .w4a_lcg_u(m - 1L, seed)
    for (i in (m - 1L):1L) {
      j <- min(as.integer(u[m - i] * (i + 1L)), i)
      tmp <- order[i + 1L]
      order[i + 1L] <- order[j + 1L]
      order[j + 1L] <- tmp
    }
  }
  n_full <- m %/% bs
  batches <- lapply(seq_len(n_full) - 1L, function(i) order[(i * bs + 1L):(i * bs + bs)])
  rest <- if (n_full * bs < m) order[(n_full * bs + 1L):m] else integer(0)
  dropped <- 0L
  if (length(rest) > 0L) {
    if (drop_last) dropped <- length(rest) else batches[[length(batches) + 1L]] <- rest
  }
  batch_data <- NULL
  if (!is.null(data)) {
    batch_data <- lapply(batches, function(b) if (is.matrix(data)) data[b + 1L, , drop = FALSE] else data[b + 1L])
  }
  assign <- if (nw > 0L) (seq_along(batches) - 1L) %% nw else rep(0L, length(batches))

  list(
    batches = batches, order = order, n_batches = length(batches),
    last_batch_size = if (length(batches)) length(batches[[length(batches)]]) else 0L,
    dropped = dropped, batch_size = bs, shuffle = shuffle, drop_last = drop_last,
    worker_assignment = assign, num_workers = nw, batch_data = batch_data,
    estimate = length(batches), n = m, method = "mini-batch index plan with deterministic Fisher-Yates shuffling"
  )
}

#' Combine value + advantage streams: Q = V + A - mean_a A (hmdldqn)
#' @param V State values (S,).
#' @param Adv Advantages (S, nA).
#' @return Matrix (S, nA).
#' @export
morie_dueling_q <- function(V, Adv) {
  Vv <- as.numeric(V)
  Av <- .morie_gr_mat(Adv, "Adv")
  .w4a_need(length(Vv) == nrow(Av), "V/A row mismatch.")
  Vv + Av - rowMeans(Av)
}

#' Direct preference optimization (DPO) loss (hmdpo)
#' @param pi,pi_ref Log-probs (B, 2) under policy / reference; col 0 = chosen unless `preferences` given.
#' @param preferences 0/1 winner index per row (default all 0).
#' @param beta Inverse temperature (>0).
#' @return List with `loss`, `per_pair_loss`, `margin`, `reward_chosen`, `reward_rejected`, `accuracy`.
#' @export
morie_geron_dpo <- function(pi, pi_ref, preferences = NULL, beta = 0.1) {
  lp <- .morie_gr_mat(pi, "pi")
  lr <- .morie_gr_mat(pi_ref, "pi_ref")
  .w4a_need(all(dim(lp) == dim(lr)) && ncol(lp) == 2L, "pi/pi_ref must both be (B, 2).")
  .w4a_need(all(lp <= 0) && all(lr <= 0), "pi and pi_ref must be LOG probabilities (<= 0).")
  b <- as.numeric(beta)
  .w4a_need(is.finite(b) && b > 0, "beta must be positive.")
  B <- nrow(lp)
  pref <- if (is.null(preferences)) rep(0L, B) else as.integer(round(as.numeric(preferences)))
  win <- pref + 1L
  lose <- (1L - pref) + 1L
  rw <- b * (lp[cbind(seq_len(B), win)] - lr[cbind(seq_len(B), win)])
  rl <- b * (lp[cbind(seq_len(B), lose)] - lr[cbind(seq_len(B), lose)])
  margin <- rw - rl
  logsigmoid <- function(z) ifelse(z >= 0, -log1p(exp(-abs(z))), z - log1p(exp(-abs(z))))
  per <- -logsigmoid(margin)
  loss <- mean(per)
  prob <- 1 / (1 + exp(-margin))
  list(
    loss = loss, per_pair_loss = per, margin = margin, reward_chosen = rw, reward_rejected = rl,
    prob_preferred = prob, accuracy = mean(margin > 0), beta = b, estimate = loss, n = B,
    method = "DPO loss -log sigmoid(beta*(log pi/pi_ref)_w - beta*(log pi/pi_ref)_l)"
  )
}

#' Dynamic quantization: static per-tensor weights, dynamic per-batch activations (hmdqnt)
#' @param model Named list of weight tensors, or one array.
#' @param dtype "int8"/"uint8"/"int16".
#' @param activations Optional batch to quantize dynamically.
#' @return List with `quantized`, `scales`, `zero_points`, `dequantized`, `max_abs_error`, `compression`.
#' @export
morie_geron_dynamic_quantization_alt <- function(model, dtype = "int8", activations = NULL) {
  ranges <- list(int8 = c(-128, 127, TRUE), uint8 = c(0, 255, FALSE), int16 = c(-32768, 32767, TRUE))
  .w4a_need(dtype %in% names(ranges), "dtype must be int8, uint8 or int16.")
  rg <- ranges[[dtype]]
  qmin <- rg[1]
  qmax <- rg[2]
  symmetric <- as.logical(rg[3])
  bits <- if (dtype %in% c("int8", "uint8")) 8 else 16
  tensors <- if (is.list(model)) model else list(weight = model)
  q <- list()
  scales <- list()
  zps <- list()
  deq <- list()
  errs <- list()
  for (nm in names(tensors)) {
    W <- as.numeric(tensors[[nm]])
    if (symmetric) {
      amax <- max(abs(W))
      .w4a_need(amax > 0, "tensor is all zeros; scale undefined.")
      s <- amax / qmax
      z <- 0
    } else {
      lo <- min(W)
      hi <- max(W)
      .w4a_need(hi > lo, "tensor is constant; scale undefined.")
      s <- (hi - lo) / (qmax - qmin)
      z <- round(qmin - lo / s)
    }
    qi <- pmin(pmax(round(W / s) + z, qmin), qmax)
    back <- (qi - z) * s
    q[[nm]] <- qi
    scales[[nm]] <- s
    zps[[nm]] <- z
    deq[[nm]] <- back
    errs[[nm]] <- max(abs(back - W))
  }
  act <- NULL
  if (!is.null(activations)) {
    A <- as.numeric(activations)
    amax <- max(abs(A))
    .w4a_need(amax > 0, "activations are all zero; scale undefined.")
    sa <- amax / qmax
    qa <- pmin(pmax(round(A / sa), qmin), qmax)
    act <- list(scale = sa, quantized = qa, dequantized = qa * sa, max_abs_error = max(abs(qa * sa - A)))
  }
  n_params <- sum(vapply(tensors, length, 0L))
  list(
    quantized = q, scales = scales, zero_points = zps, dequantized = deq, max_abs_error = errs,
    compression = 32 / bits, bits = bits, dtype = dtype, symmetric = symmetric, activation = act,
    estimate = max(unlist(errs)), n = n_params,
    method = "per-tensor affine weight quantization with per-batch dynamic activation scaling"
  )
}

#' Deep (stacked) RNN forward pass, bottom-up per time step (hmdrnn)
#' @param X Input sequence (T, d).
#' @param hidden_sizes Width(s) per layer.
#' @param n_layers Optional repeat count.
#' @param weights Optional list of (Wx, Wh, b) triples.
#' @param seed LCG seed.
#' @param activation "tanh"/"relu".
#' @return List with `outputs`, `states`, `final_states`, `layer_sizes`, `n_params`, `state_norms`.
#' @export
morie_geron_deep_rnn <- function(X, hidden_sizes = 4, n_layers = NULL, weights = NULL, seed = 0, activation = "tanh") {
  Xa <- .morie_gr_mat(X, "X")
  Tn <- nrow(Xa)
  d <- ncol(Xa)
  phi <- if (activation == "tanh") tanh else function(z) pmax(z, 0)
  if (!is.null(weights)) {
    layers <- list()
    fan <- d
    for (trio in weights) {
      Wx <- .morie_gr_mat(trio[[1]], "Wx")
      Wh <- .morie_gr_mat(trio[[2]], "Wh")
      bb <- as.numeric(trio[[3]])
      layers[[length(layers) + 1L]] <- list(Wx = Wx, Wh = Wh, b = bb)
      fan <- ncol(Wx)
    }
  } else {
    if (length(hidden_sizes) == 1L) {
      L <- if (is.null(n_layers)) 1L else as.integer(n_layers)
      sizes <- rep(as.integer(hidden_sizes), L)
    } else {
      sizes <- as.integer(hidden_sizes)
    }
    s <- as.integer(seed) %% 2^32
    draw <- function(n, sd) {
      u <- numeric(n)
      for (i in seq_len(n)) {
        s <<- (1664525 * s + 1013904223) %% 2^32
        u[i] <- (s + 0.5) / 2^32
      }
      (2 * u - 1) * sqrt(3) * sd
    }
    layers <- list()
    fan <- d
    for (H in sizes) {
      Wx <- matrix(draw(fan * H, 1 / sqrt(fan)), fan, H, byrow = TRUE)
      Wh <- matrix(draw(H * H, 1 / sqrt(H)), H, H, byrow = TRUE)
      layers[[length(layers) + 1L]] <- list(Wx = Wx, Wh = Wh, b = rep(0, H))
      fan <- H
    }
  }
  L <- length(layers)
  states <- vector("list", L)
  for (l in seq_len(L)) states[[l]] <- vector("list", Tn)
  h_prev <- lapply(layers, function(ly) rep(0, ncol(ly$Wx)))
  outputs <- vector("list", Tn)
  for (t in seq_len(Tn)) {
    inp <- Xa[t, ]
    for (l in seq_len(L)) {
      ly <- layers[[l]]
      h <- phi(as.numeric(inp %*% ly$Wx) + as.numeric(h_prev[[l]] %*% ly$Wh) + ly$b)
      h_prev[[l]] <- h
      states[[l]][[t]] <- h
      inp <- h
    }
    outputs[[t]] <- inp
  }
  n_params <- sum(vapply(layers, function(ly) length(ly$Wx) + length(ly$Wh) + length(ly$b), 0L))
  norms <- lapply(states, function(layer) vapply(layer, function(h) sqrt(sum(h^2)), 0))
  list(
    outputs = outputs, states = states, final_states = h_prev,
    layer_sizes = vapply(layers, function(ly) ncol(ly$Wx), 0L), n_layers = L, n_params = n_params,
    state_norms = norms, activation = activation, estimate = mean(unlist(outputs)), n = Tn,
    method = "stacked RNN forward pass, bottom-up per time step"
  )
}

#' Ancestral DDPM reverse sampling from x_T to x_0 (hmdrv)
#'
#' Uses the shared `morie_beta_schedule_values`/`morie_lcg_normal` (hmdfw)
#' so forward and reverse schedules cannot drift apart.
#' @param x_T Starting noise.
#' @param model function(x_t, t) -> eps, t is 1-based.
#' @param T Steps (>=1).
#' @param beta_schedule "linear"/"cosine"/vector.
#' @param seed LCG seed.
#' @param clip_x0 Optional c(lo, hi).
#' @return List with `x_0`, `trajectory`, `means`, `betas`, `alpha_bar`, `model_calls`.
#' @export
morie_geron_diffusion_reverse <- function(x_T, model, T, beta_schedule = "linear", seed = 0, clip_x0 = NULL) {
  x <- as.numeric(x_T)
  Ti <- as.integer(T)
  betas <- morie_beta_schedule_values(Ti, beta_schedule)
  alphas <- 1 - betas
  abar <- cumprod(alphas)
  cur <- x
  traj <- list(cur)
  means <- list()
  calls <- 0L
  for (t in seq(Ti, 1, by = -1)) {
    eps <- as.numeric(model(cur, t))
    calls <- calls + 1L
    b <- betas[t]
    a <- alphas[t]
    ab <- abar[t]
    if (!is.null(clip_x0)) {
      lo <- clip_x0[1]
      hi <- clip_x0[2]
      x0_hat <- pmin(pmax((cur - sqrt(1 - ab) * eps) / sqrt(ab), lo), hi)
      ab_prev <- if (t > 1) abar[t - 1] else 1.0
      coef0 <- sqrt(ab_prev) * b / (1 - ab)
      coef_t <- sqrt(a) * (1 - ab_prev) / (1 - ab)
      mu <- coef0 * x0_hat + coef_t * cur
    } else {
      mu <- (cur - (b / sqrt(1 - ab)) * eps) / sqrt(a)
    }
    means[[length(means) + 1L]] <- mu
    if (t > 1) {
      z <- morie_lcg_normal(length(cur), seed + t)
      cur <- mu + sqrt(b) * z
    } else {
      cur <- mu
    }
    traj[[length(traj) + 1L]] <- cur
  }
  list(
    x_0 = cur, trajectory = traj, means = means, betas = betas, alphas = alphas, alpha_bar = abar,
    n_steps = Ti, model_calls = calls, estimate = mean(cur), n = length(x),
    method = "ancestral DDPM reverse sampling with an enforced model(x_t, t) -> eps contract"
  )
}

#' Decision-tree variance via bootstrap resampling of CART trees (hmdthv)
#'
#' Growth delegated to `morie_geron_cart_algorithm`/`morie_geron_predict_tree`.
#' @param X,y Data.
#' @param n_resamples Bootstrap count (>=2).
#' @param seed LCG seed.
#' @param criterion,max_depth CART controls.
#' @return List with `variance`, `bias2`, `root_splits`, `structural_instability`, `ensemble_prediction`.
#' @export
morie_geron_tree_high_variance <- function(X, y, n_resamples = 20, seed = 0, criterion = "gini", max_depth = NULL) {
  Xa <- .morie_gr_mat(X, "X")
  ya <- as.vector(y)
  B <- as.integer(n_resamples)
  m <- nrow(Xa)
  full <- morie_geron_cart_algorithm(Xa, ya, criterion = criterion, max_depth = max_depth)
  root <- full$tree
  root_key <- if (root$leaf) NULL else c(root$feature, root$threshold)

  s <- as.integer(seed) %% 2^32
  preds <- list()
  roots <- list()
  for (b in seq_len(B)) {
    idx <- integer(m)
    for (i in seq_len(m)) {
      s <- (1664525 * s + 1013904223) %% 2^32
      idx[i] <- as.integer(((s + 0.5) / 2^32) * m)
    }
    idx <- pmin(idx, m - 1L)
    yb <- ya[idx + 1L]
    if (criterion != "mse" && length(unique(yb)) < 2L) {
      tree <- list(leaf = TRUE, value = yb[1], n = m, impurity = 0, depth = 0L)
      roots[[b]] <- NULL
    } else {
      res <- morie_geron_cart_algorithm(Xa[idx + 1L, , drop = FALSE], yb, criterion = criterion, max_depth = max_depth)
      tree <- res$tree
      roots[[b]] <- if (tree$leaf) NULL else c(tree$feature, tree$threshold)
    }
    preds[[b]] <- morie_geron_predict_tree(tree, Xa)
  }
  P <- do.call(rbind, preds)
  if (criterion == "mse") {
    mean_pred <- colMeans(P)
    per_var <- apply(P, 2, function(col) mean((col - mean(col))^2))
    bias2 <- mean((mean_pred - ya)^2)
    ens <- mean_pred
    ens_score <- mean((ens - ya)^2)
    single <- full$train_mse
  } else {
    classes <- sort(unique(ya))
    counts <- sapply(classes, function(c) colSums(P == c))
    if (is.null(dim(counts))) counts <- matrix(counts, nrow = 1)
    maj <- classes[apply(counts, 1, which.max)]
    per_var <- 1 - apply(counts, 1, max) / B
    bias2 <- mean(as.numeric(maj != ya))
    ens <- maj
    ens_score <- mean(maj == ya)
    single <- full$train_accuracy
  }
  same <- function(rk) if (is.null(rk) && is.null(root_key)) TRUE else if (is.null(rk) || is.null(root_key)) FALSE else all(rk == root_key)
  instability <- mean(!vapply(roots, same, TRUE))
  list(
    variance = mean(per_var), bias2 = bias2, per_point_variance = per_var, root_splits = roots,
    reference_root = root_key, structural_instability = instability, ensemble_prediction = ens,
    ensemble_score = ens_score, single_tree_score = single, n_resamples = B, criterion = criterion,
    estimate = mean(per_var), n = m,
    method = "bootstrap resampling of CART trees to measure prediction variance and root instability"
  )
}

#' Tree regularization: constrained vs unconstrained CART (hmdtr)
#' @param X,y Data.
#' @param max_depth,min_samples_leaf,min_samples_split,criterion CART controls.
#' @return List with `tree`, `n_leaves`, `baseline_leaves`, `leaves_saved`, `train_score`, `train_score_cost`.
#' @export
morie_geron_tree_regularization <- function(X, y, max_depth = NULL, min_samples_leaf = 1,
                                            min_samples_split = 2, criterion = "gini") {
  constrained <- morie_geron_cart_algorithm(X, y,
    criterion = criterion, max_depth = max_depth,
    min_samples_split = min_samples_split, min_samples_leaf = min_samples_leaf
  )
  baseline <- morie_geron_cart_algorithm(X, y, criterion = criterion)
  key <- if (criterion == "mse") "train_mse" else "train_accuracy"
  score <- constrained[[key]]
  base_score <- baseline[[key]]
  cost <- if (criterion != "mse") (base_score - score) else (score - base_score)
  list(
    tree = constrained$tree, predictions = constrained$predictions, n_leaves = constrained$n_leaves,
    depth = constrained$depth, baseline_leaves = baseline$n_leaves, baseline_depth = baseline$depth,
    leaves_saved = baseline$n_leaves - constrained$n_leaves, train_score = score,
    baseline_train_score = base_score, train_score_cost = cost,
    constraints = list(
      max_depth = max_depth, min_samples_split = as.integer(min_samples_split),
      min_samples_leaf = as.integer(min_samples_leaf)
    ),
    criterion = criterion, estimate = score, n = constrained$n,
    method = "constrained vs unconstrained CART, both grown via hmcart"
  )
}

#' Decision-tree scale invariance: CART thresholds under x' = a*x + b (hmdtst)
#' @param X,y Data.
#' @param a,b Affine transform (a > 0).
#' @param feature Optional single column.
#' @param criterion,max_depth CART controls.
#' @return List with `predictions_match`, `thresholds`, `scaled_thresholds`, `thresholds_match`, `knn_match`.
#' @export
morie_geron_tree_sensitivity_scale <- function(X, y, a = 100.0, b = -7.0, feature = NULL,
                                               criterion = "gini", max_depth = NULL) {
  Xa <- .morie_gr_mat(X, "X")
  ya <- as.vector(y)
  af <- as.numeric(a)
  bf <- as.numeric(b)
  .w4a_need(af > 0, "a must be positive.")
  cols <- if (is.null(feature)) seq_len(ncol(Xa)) else as.integer(feature) + 1L
  Xs <- Xa
  Xs[, cols] <- af * Xa[, cols] + bf

  base <- morie_geron_cart_algorithm(Xa, ya, criterion = criterion, max_depth = max_depth)
  scaled <- morie_geron_cart_algorithm(Xs, ya, criterion = criterion, max_depth = max_depth)

  thresholds <- function(node, out) {
    if (node$leaf) {
      return(out)
    }
    out[[length(out) + 1L]] <- c(node$feature, node$threshold)
    out <- thresholds(node$left, out)
    out <- thresholds(node$right, out)
    out
  }
  t0 <- thresholds(base$tree, list())
  t1 <- thresholds(scaled$tree, list())
  expected <- lapply(t0, function(kv) {
    k <- kv[1]
    tt <- kv[2]
    if ((k + 1L) %in% cols) c(k, af * tt + bf) else kv
  })
  t_match <- length(t0) == length(t1) &&
    all(mapply(function(e, o) e[1] == o[1] && abs(e[2] - o[2]) < 1e-9 * max(1, abs(e[2])), expected, t1))
  p_match <- identical(base$predictions, scaled$predictions)

  knn <- function(Xtr) {
    n <- nrow(Xtr)
    vapply(seq_len(n), function(i) {
      d <- rowSums(sweep(Xtr, 2, Xtr[i, ], "-")^2)
      d[i] <- Inf
      ya[which.min(d)]
    }, ya[1])
  }
  k0 <- knn(Xa)
  k1 <- knn(Xs)

  list(
    predictions_match = p_match, predictions = base$predictions, scaled_predictions = scaled$predictions,
    thresholds = vapply(t0, `[`, 0, 2), scaled_thresholds = vapply(t1, `[`, 0, 2),
    expected_thresholds = vapply(expected, `[`, 0, 2), thresholds_match = t_match,
    knn_predictions = k0, knn_scaled_predictions = k1, knn_match = identical(k0, k1),
    transform = list(a = af, b = bf, columns = cols - 1L), estimate = if (p_match) 1.0 else 0.0, n = nrow(Xa),
    method = "affine rescaling experiment on CART vs a 1-NN control"
  )
}

#' Error analysis via row-normalised confusion matrix, diagonal removed (hmeaf)
#'
#' Counting delegated to `morie_geron_confusion_matrix`.
#' @param y_true,y_pred Labels.
#' @param top_k Confusions to list.
#' @return List with `normalized`, `error_matrix`, `top_confusions`, `worst_class`, `error_rate`.
#' @export
morie_geron_error_analysis <- function(y_true, y_pred, top_k = 5) {
  k <- as.integer(top_k)
  base <- morie_geron_confusion_matrix(y_true, y_pred)
  cm <- base$matrix
  labels <- seq_len(nrow(cm)) - 1L
  row_tot <- rowSums(cm)
  col_tot <- colSums(cm)
  .w4a_need(!any(row_tot == 0), "some classes never occur in y_true.")
  norm <- cm / row_tot
  colnorm <- ifelse(matrix(col_tot, nrow(cm), ncol(cm), byrow = TRUE) > 0,
    cm / matrix(ifelse(col_tot == 0, 1, col_tot), nrow(cm), ncol(cm), byrow = TRUE), 0
  )
  err <- norm
  diag(err) <- 0

  pairs <- list()
  for (i in seq_len(nrow(err))) {
    for (j in seq_len(ncol(err))) {
      if (err[i, j] > 0) {
        pairs[[length(pairs) + 1L]] <- c(i - 1L, j - 1L, err[i, j])
      }
    }
  }
  ord <- order(-vapply(pairs, `[`, 0, 3), vapply(pairs, `[`, 0, 1), vapply(pairs, `[`, 0, 2))
  pairs <- pairs[ord]
  top <- pairs[seq_len(min(k, length(pairs)))]
  per_class_err <- rowSums(err)
  worst <- which.max(per_class_err) - 1L

  list(
    normalized = norm, error_matrix = err, column_normalized = colnorm, top_confusions = top,
    per_class_error_rate = per_class_err, worst_class = worst, labels = labels,
    error_rate = 1 - base$accuracy, accuracy = base$accuracy, estimate = 1 - base$accuracy, n = base$n,
    method = "row-normalised confusion matrix with the diagonal removed; counting delegated to hmcfm"
  )
}

#' Early stopping: batch GD keeping the best validation snapshot (hmearl)
#' @param X_train,y_train,X_val,y_val Data.
#' @param n_iter,eta,fit_intercept Training config.
#' @param patience Optional online stopping patience.
#' @return List with `theta`, `best_iter`, `best_val_rmse`, `stopped_iter`, `is_u_shaped`.
#' @export
morie_geron_early_stopping_alt <- function(X_train, y_train, X_val, y_val, n_iter = 100, eta = 0.01,
                                           patience = NULL, fit_intercept = TRUE) {
  Xt <- .morie_gr_mat(X_train, "X_train")
  yt <- as.numeric(y_train)
  Xv <- .morie_gr_mat(X_val, "X_val")
  yv <- as.numeric(y_val)
  T_ <- as.integer(n_iter)
  lr <- as.numeric(eta)
  pat <- if (is.null(patience)) NULL else as.integer(patience)
  A <- if (fit_intercept) cbind(1, Xt) else Xt
  Bm <- if (fit_intercept) cbind(1, Xv) else Xv
  theta <- rep(0, ncol(A))
  m <- nrow(A)
  rmse <- function(D, t, target) sqrt(mean((D %*% t - target)^2))
  tr_hist <- rmse(A, theta, yt)
  va_hist <- rmse(Bm, theta, yv)
  best <- list(val = va_hist[1], iter = 0L, theta = theta)
  stopped <- NULL
  since <- 0L
  for (it in seq_len(T_)) {
    grad <- (2 / m) * (t(A) %*% (A %*% theta - yt))
    theta <- theta - lr * as.numeric(grad)
    tr_hist <- c(tr_hist, rmse(A, theta, yt))
    v <- rmse(Bm, theta, yv)
    va_hist <- c(va_hist, v)
    if (v < best$val - 1e-15) {
      best <- list(val = v, iter = it, theta = theta)
      since <- 0L
    } else {
      since <- since + 1L
      if (!is.null(pat) && since >= pat && is.null(stopped)) stopped <- it
    }
  }
  if (!is.null(pat) && is.null(stopped)) stopped <- T_
  u_shaped <- best$iter < T_
  list(
    theta = best$theta, best_iter = best$iter, best_val_rmse = best$val, stopped_iter = stopped,
    val_rmse = va_hist, train_rmse = tr_hist, final_theta = theta, final_val_rmse = va_hist[length(va_hist)],
    is_u_shaped = u_shaped, patience = pat, eta = lr, estimate = best$val, n = m,
    method = "batch gradient descent with best-snapshot early stopping"
  )
}

#' Epsilon-greedy action distribution, deterministic LCG draw (hmeg)
#' @param Q Table (S, A) or (A,).
#' @param s State index (0-based).
#' @param epsilon Rate in \[0, 1\].
#' @param seed LCG seed.
#' @return List with `action`, `probabilities`, `greedy_action`, `greedy_actions`, `is_exploratory`.
#' @export
morie_geron_epsilon_greedy_alt <- function(Q, s, epsilon, seed = 0) {
  Qa <- if (is.matrix(Q)) Q else matrix(as.numeric(Q), nrow = 1)
  si <- as.integer(s) + 1L
  eps <- as.numeric(epsilon)
  q <- Qa[si, ]
  A <- length(q)
  best <- which(q == max(q))
  p <- rep(eps / A, A)
  p[best] <- p[best] + (1 - eps) / length(best)
  st <- (as.integer(seed) * 1664525 + 1013904223) %% 2^32
  u <- (st + 0.5) / 2^32
  a <- min(sum(cumsum(p) < u) + 1L, A)
  list(
    action = a - 1L, probabilities = p, greedy_action = best[1] - 1L, greedy_actions = best - 1L,
    q_values = q, is_exploratory = !((a) %in% best), epsilon = eps, estimate = max(p), n = A,
    method = "epsilon-greedy action distribution with deterministic LCG sampling"
  )
}

#' VAE evidence lower bound with closed-form Gaussian KL (hmelb)
#' @param x Inputs (m, d).
#' @param mu,log_sigma Posterior params (m, k).
#' @param x_recon Optional decoder output (default x).
#' @param likelihood "gaussian"/"bernoulli".
#' @param sigma_x Fixed decoder sd for the Gaussian likelihood.
#' @return List with `elbo`, `loss`, `kl`, `reconstruction_log_lik`, `per_sample_kl`.
#' @export
morie_geron_elbo <- function(x, mu, log_sigma, x_recon = NULL, likelihood = "gaussian", sigma_x = 1.0) {
  X <- .morie_gr_mat(x, "x")
  M <- .morie_gr_mat(mu, "mu")
  LS <- .morie_gr_mat(log_sigma, "log_sigma")
  sx <- as.numeric(sigma_x)
  R <- if (is.null(x_recon)) X else .morie_gr_mat(x_recon, "x_recon")
  var_ <- exp(2 * LS)
  kl_i <- -0.5 * rowSums(1 + 2 * LS - M^2 - var_)
  if (likelihood == "gaussian") {
    rec_i <- -0.5 * rowSums((X - R)^2) / (sx * sx) - ncol(X) * (log(2 * pi) / 2 + log(sx))
  } else {
    eps <- 1e-12
    rec_i <- rowSums(X * log(R + eps) + (1 - X) * log(1 - R + eps))
  }
  elbo_i <- rec_i - kl_i
  elbo <- mean(elbo_i)
  kl <- mean(kl_i)
  rec <- mean(rec_i)
  list(
    elbo = elbo, loss = -elbo, kl = kl, reconstruction_log_lik = rec, per_sample_kl = kl_i,
    per_sample_elbo = elbo_i, latent_dim = ncol(M), likelihood = likelihood, estimate = elbo, n = nrow(X),
    method = "ELBO = E_q[log p(x|z)] - KL(q||p) with closed-form Gaussian KL"
  )
}

#' Original encoder-decoder transformer, resolved to exact params + 3 masks (hmencd)
#'
#' Block costs delegated to `morie_geron_block_params`; only the decoder
#' block carries the extra cross-attention sub-layer.
#' @param src,tgt Token ids.
#' @param n_layers,n_heads,d_model,vocab_size,max_len,d_ff int.
#' @param share_embeddings Share one embedding matrix between the stacks.
#' @return List with `total_params`, `encoder_params`, `decoder_params`, `src_mask`, `tgt_mask`, `cross_mask`.
#' @export
morie_geron_encoder_decoder_transformer <- function(src, tgt, n_layers = 6, n_heads = 8, d_model = 512,
                                                    vocab_size = 37000, max_len = 512, d_ff = 2048,
                                                    share_embeddings = TRUE) {
  Ts <- length(src)
  Tt <- length(tgt)
  L <- as.integer(n_layers)
  Hh <- as.integer(n_heads)
  d <- as.integer(d_model)
  V <- as.integer(vocab_size)
  M <- as.integer(max_len)
  .w4a_need(d %% Hh == 0L, "d_model must be divisible by n_heads.")
  .w4a_need(Ts <= M && Tt <= M, "sequence lengths exceed max_len.")
  enc <- morie_geron_block_params(d, d_ff = d_ff, cross_attention = FALSE)
  dec <- morie_geron_block_params(d, d_ff = d_ff, cross_attention = TRUE)
  emb <- if (share_embeddings) V * d + M * d else 2L * V * d + M * d
  out_head <- V * d + V
  norms <- 2L * (2L * d)
  total <- emb + L * enc$total + L * dec$total + norms + out_head
  list(
    total_params = total, encoder_params = L * enc$total, decoder_params = L * dec$total,
    encoder_block_params = enc$total, decoder_block_params = dec$total,
    extra_per_decoder_block = dec$total - enc$total, embedding_params = emb, output_head_params = out_head,
    src_mask = matrix(FALSE, Ts, Ts), tgt_mask = morie_geron_causal_mask(Tt), cross_mask = matrix(FALSE, Tt, Ts),
    src_len = Ts, tgt_len = Tt, d_head = d %/% Hh, n_layers = L, n_heads = Hh, d_model = d,
    d_ff = as.integer(d_ff), share_embeddings = share_embeddings, estimate = total, n = 2L * L,
    method = "original encoder-decoder transformer resolved to exact parameter counts and its three masks"
  )
}

#' Elastic net cost: MSE + r*alpha*L1 + (1-r)/2*alpha*L2 (hmenet)
#' @param X,y Data.
#' @param theta Parameters (bias first if fit_intercept).
#' @param alpha Overall penalty (>=0).
#' @param r L1 ratio in \[0, 1\].
#' @param fit_intercept Logical.
#' @return List with `cost`, `mse`, `l1_penalty`, `l2_penalty`, `gradient`.
#' @export
morie_geron_elastic_net <- function(X, y, theta, alpha, r, fit_intercept = TRUE) {
  Xm <- .morie_gr_mat(X, "X")
  yv <- as.numeric(y)
  th <- as.numeric(theta)
  a <- as.numeric(alpha)
  ratio <- as.numeric(r)
  Xd <- if (fit_intercept) cbind(1, Xm) else Xm
  m <- nrow(Xd)
  resid <- as.numeric(Xd %*% th) - yv
  mse <- mean(resid^2)
  pen <- th
  if (fit_intercept) pen[1] <- 0
  l1 <- ratio * a * sum(abs(pen))
  l2 <- 0.5 * (1 - ratio) * a * sum(pen^2)
  cost <- mse + l1 + l2
  grad <- (2 / m) * as.numeric(t(Xd) %*% resid) + ratio * a * sign(pen) + (1 - ratio) * a * pen
  list(
    cost = cost, mse = mse, l1_penalty = l1, l2_penalty = l2, penalty = l1 + l2, gradient = grad,
    alpha = a, r = ratio, estimate = cost, n = m,
    method = "elastic net J = MSE + r*alpha*L1 + (1-r)/2*alpha*L2"
  )
}

#' Explained variance ratio from the SVD of the centred data matrix (hmevr)
#' @param X Data (m, n).
#' @param n_components Truncate to leading k (default all).
#' @param center Subtract column means.
#' @return List with `explained_variance_ratio`, `explained_variance`, `singular_values`, `cumulative`, `n_for_95`.
#' @export
morie_geron_explained_variance_ratio_alt <- function(X, n_components = NULL, center = TRUE) {
  A <- .morie_gr_mat(X, "X")
  m <- nrow(A)
  Ac <- if (center) sweep(A, 2, colMeans(A), "-") else A
  sv <- svd(Ac)
  var_ <- sv$d^2 / (m - 1)
  total <- sum(var_)
  .w4a_need(total > 0, "X has zero total variance.")
  evr <- var_ / total
  cum <- cumsum(evr)
  k <- if (is.null(n_components)) length(evr) else as.integer(n_components)
  list(
    explained_variance_ratio = evr[seq_len(k)], explained_variance = var_[seq_len(k)],
    singular_values = sv$d[seq_len(k)], cumulative = cum[seq_len(k)], total_variance = total,
    n_for_95 = sum(cum < 0.95) + 1L, components = t(sv$v[, seq_len(k), drop = FALSE]), centered = center,
    estimate = evr[1], n = m, method = "EVR from the SVD of the centred data matrix"
  )
}

#' Extra-trees: uniformly random per-feature split thresholds (hmext)
#'
#' Split cost delegated to `morie_geron_cart_split_cost`; the deterministic
#' baseline tree via `morie_geron_cart_algorithm`.
#' @param X,y Data.
#' @param n_estimators,max_features,seed,criterion,max_depth,min_samples_leaf Config.
#' @return List with `predictions`, `tree_predictions`, `train_score`, `single_tree_score`, `disagreement`.
#' @export
morie_geron_extra_trees <- function(X, y, n_estimators = 10, max_features = NULL, seed = 0,
                                    criterion = "gini", max_depth = NULL, min_samples_leaf = 1) {
  Xa <- .morie_gr_mat(X, "X")
  ya <- as.vector(y)
  B <- as.integer(n_estimators)
  n_feat <- ncol(Xa)
  mf <- if (is.null(max_features)) (if (criterion == "mse") n_feat else max(1L, as.integer(sqrt(n_feat)))) else as.integer(max_features)
  msl <- as.integer(min_samples_leaf)
  st <- as.integer(seed) %% 2^32
  unif <- function(n) {
    u <- numeric(n)
    for (i in seq_len(n)) {
      st <<- (1664525 * st + 1013904223) %% 2^32
      u[i] <- (st + 0.5) / 2^32
    }
    u
  }

  grow <- function(Xs, ys, depth) {
    n_classes <- length(unique(ys))
    pure <- if (criterion != "mse") n_classes < 2L else all(ys == ys[1])
    if (pure || (!is.null(max_depth) && depth >= max_depth) || length(ys) < 2L * msl) {
      val <- if (criterion == "mse") {
        mean(ys)
      } else {
        tb <- table(ys)
        as.numeric(names(tb)[which.max(tb)])
      }
      return(list(leaf = TRUE, value = val, n = length(ys), depth = depth))
    }
    k <- min(mf, n_feat)
    perm <- order(unif(n_feat))[seq_len(k)]
    best <- NULL
    for (f in perm) {
      lo <- min(Xs[, f])
      hi <- max(Xs[, f])
      if (hi <= lo) next
      tt <- lo + (hi - lo) * unif(1)
      if (tt >= hi) tt <- (lo + hi) / 2
      left <- Xs[, f] <= tt
      if (sum(left) < msl || sum(!left) < msl) next
      cost <- morie_geron_cart_split_cost(Xs, ys, feature = f - 1L, threshold = tt, criterion = criterion)$cost
      if (is.null(best) || cost < best$cost) best <- list(cost = cost, f = f, t = tt)
    }
    if (is.null(best)) {
      val <- if (criterion == "mse") {
        mean(ys)
      } else {
        tb <- table(ys)
        as.numeric(names(tb)[which.max(tb)])
      }
      return(list(leaf = TRUE, value = val, n = length(ys), depth = depth))
    }
    mask <- Xs[, best$f] <= best$t
    list(
      leaf = FALSE, feature = best$f - 1L, threshold = best$t, n = length(ys), depth = depth,
      left = grow(Xs[mask, , drop = FALSE], ys[mask], depth + 1L),
      right = grow(Xs[!mask, , drop = FALSE], ys[!mask], depth + 1L)
    )
  }

  trees <- list()
  tree_preds <- list()
  for (i in seq_len(B)) {
    t_ <- grow(Xa, ya, 0L)
    trees[[i]] <- t_
    tree_preds[[i]] <- morie_geron_predict_tree(t_, Xa)
  }
  P <- do.call(rbind, tree_preds)
  single <- morie_geron_cart_algorithm(Xa, ya, criterion = criterion, max_depth = max_depth)
  out <- list(
    trees = trees, tree_predictions = tree_preds, n_estimators = B, max_features = mf,
    criterion = criterion, n = nrow(Xa),
    method = "extra-trees with uniformly random per-feature thresholds; split cost delegated to grcart"
  )
  if (criterion == "mse") {
    pred <- colMeans(P)
    mse <- mean((pred - ya)^2)
    out$predictions <- pred
    out$train_mse <- mse
    out$train_score <- mse
    out$single_tree_score <- single$train_mse
    out$disagreement <- mean(apply(P, 2, var))
    out$estimate <- mse
  } else {
    classes <- sort(unique(ya))
    counts <- sapply(classes, function(c) colSums(P == c))
    if (is.null(dim(counts))) counts <- matrix(counts, nrow = 1)
    pred <- classes[apply(counts, 1, which.max)]
    acc <- mean(pred == ya)
    out$predictions <- pred
    out$train_score <- acc
    out$train_accuracy <- acc
    out$single_tree_score <- single$train_accuracy
    out$disagreement <- mean(1 - apply(counts, 1, max) / B)
    out$estimate <- acc
  }
  out
}

#' F1 score from a confusion matrix delegated to hmcfm (hmf1)
#' @param y_true,y_pred Labels.
#' @param pos_label Positive class for average="binary".
#' @param average "binary"/"macro"/"micro"/NULL.
#' @return List with `f1`, `precision`, `recall`, `tp`, `fp`, `fn`, `per_class_f1`.
#' @export
morie_geron_f1_score_alt <- function(y_true, y_pred, pos_label = 1, average = "binary") {
  cm_res <- morie_geron_confusion_matrix(y_true, y_pred)
  cm <- cm_res$matrix
  labels <- seq_len(nrow(cm)) - 1L
  per_class <- cm_res$f1
  if (average == "binary") {
    .w4a_need(length(labels) == 2L, "average='binary' needs exactly 2 classes.")
    k <- which(labels == pos_label)
    tp <- cm[k, k]
    fp <- sum(cm[, k]) - tp
    fn <- sum(cm[k, ]) - tp
    prec <- if (tp + fp > 0) tp / (tp + fp) else 0
    rec <- if (tp + fn > 0) tp / (tp + fn) else 0
    f1 <- if (prec + rec > 0) 2 * prec * rec / (prec + rec) else 0
  } else if (average == "macro") {
    prec <- mean(cm_res$precision, na.rm = TRUE)
    rec <- mean(cm_res$recall, na.rm = TRUE)
    f1 <- mean(per_class, na.rm = TRUE)
    tp <- sum(diag(cm))
    fp <- fn <- sum(cm) - tp
  } else if (average == "micro") {
    tp <- sum(diag(cm))
    fp <- fn <- sum(cm) - tp
    prec <- rec <- if (sum(cm)) tp / sum(cm) else 0
    f1 <- prec
  } else {
    prec <- cm_res$precision
    rec <- cm_res$recall
    f1 <- per_class
    tp <- sum(diag(cm))
    fp <- fn <- sum(cm) - tp
  }
  scalar <- if (length(f1) > 1) mean(f1, na.rm = TRUE) else f1
  list(
    f1 = f1, precision = prec, recall = rec, tp = tp, fp = fp, fn = fn, per_class_f1 = per_class,
    labels = labels, average = average, estimate = scalar, n = cm_res$n,
    method = "F1 = 2PR/(P+R) from a confusion matrix delegated to hmcfm"
  )
}

#' Forward-mode automatic differentiation via dual numbers (hmfad)
#'
#' `f` receives a list of Dual-like R lists `list(value=, deriv=)` and
#' must return one; port note: R has no operator overloading for a
#' bespoke S3 class here, so `f` is called with plain numeric duals via
#' `.w4a_dual_*` helpers exposed to it. A finite-difference check runs
#' alongside for an independent verification route.
#' @param f function(x_numeric) -> numeric, evaluated exactly (analytic
#'   caller-supplied derivative not required: this port differentiates
#'   by finite differences AND by dual numbers on simple arithmetic
#'   expressions passed as an R function of one Dual list argument via
#'   `f(list(value=xi, deriv=1))$deriv`. See tests for the exact contract.
#' @param x Point(s) at which to differentiate.
#' @return List with `value`, `grad`, `n_passes`, `fd_check`, `max_fd_gap`.
#' @export
morie_geron_forward_autodiff <- function(f, x) {
  xs <- as.numeric(x)
  n <- length(xs)
  grad <- numeric(n)
  value <- NULL
  for (i in seq_len(n)) {
    duals <- lapply(seq_len(n), function(j) list(value = xs[j], deriv = if (j == i) 1.0 else 0.0))
    out <- f(duals)
    grad[i] <- out$deriv
    value <- out$value
  }
  plain <- function(vals) f(lapply(vals, function(v) list(value = v, deriv = 0.0)))$value
  h <- 1e-5
  fd <- numeric(n)
  for (i in seq_len(n)) {
    up <- xs
    dn <- xs
    up[i] <- up[i] + h
    dn[i] <- dn[i] - h
    fd[i] <- (plain(up) - plain(dn)) / (2 * h)
  }
  gap <- max(abs(fd - grad))
  list(
    value = value, grad = grad, gradient = grad, n_passes = n, fd_check = fd, max_fd_gap = gap,
    estimate = sqrt(sum(grad^2)), n = n, method = "forward-mode autodiff with dual numbers (exact chain rule)"
  )
}

#' Fully convolutional network forward pass, dense per-pixel prediction (hmfcn)
#'
#' Each convolution delegated to `morie_geron_conv2d_forward`.
#' @param image (H,W) or (C,H,W).
#' @param model List of kernel arrays or (kernels, bias, stride) triples.
#' @param upsample Nearest-neighbour upsample factor.
#' @param activation "relu"/"identity".
#' @return List with `class_map`, `scores`, `segmentation`, `out_shape`, `n_classes`.
#' @export
morie_geron_fcn <- function(image, model, upsample = 1, activation = "relu") {
  dims <- if (length(dim(image)) == 2) c(1, dim(image)) else dim(image)
  cur <- array(as.numeric(image), dim = dims)
  up <- as.integer(upsample)
  rf <- 1L
  stride_total <- 1L
  for (li in seq_along(model)) {
    layer <- model[[li]]
    if (is.list(layer) && length(layer) == 3) {
      K <- layer[[1]]
      bias <- layer[[2]]
      stride <- layer[[3]]
    } else {
      K <- layer
      bias <- 0
      stride <- 1
    }
    Kd <- if (length(dim(K)) == 3) dim(K) else dim(K)
    nF <- Kd[1]
    b <- as.numeric(bias)
    if (length(b) == 1) b <- rep(b, nF)
    st <- as.integer(stride)
    maps <- vector("list", nF)
    for (f in seq_len(nF)) {
      Kf <- if (length(dim(K)) == 4) array(K[f, , , ], dim = dim(K)[2:4]) else array(K[f, , ], dim = c(1, dim(K)[2], dim(K)[3]))
      out <- morie_geron_conv2d_forward(cur, Kf, b = b[f], stride = st, padding = 0)
      maps[[f]] <- out$Y
    }
    oh <- dim(maps[[1]])[1]
    ow <- dim(maps[[1]])[2]
    cur <- array(0, dim = c(nF, oh, ow))
    for (f in seq_len(nF)) cur[f, , ] <- maps[[f]]
    rf <- rf + (Kd[length(Kd) - 1L] - 1L) * stride_total
    stride_total <- stride_total * st
    if (li < length(model) && activation == "relu") cur <- pmax(cur, 0)
  }
  scores <- cur
  seg <- apply(scores, c(2, 3), which.max) - 1L
  up_map <- if (up > 1) {
    a <- scores[, rep(seq_len(dim(scores)[2]), each = up), , drop = FALSE]
    a[, , rep(seq_len(dim(scores)[3]), each = up), drop = FALSE]
  } else {
    scores
  }
  up_seg <- apply(up_map, c(2, 3), which.max) - 1L
  list(
    class_map = scores, scores = scores, segmentation = up_seg, coarse_segmentation = seg,
    out_shape = dim(scores), upsampled_shape = dim(up_map), n_classes = dim(scores)[1],
    receptive_field = rf, stride_total = stride_total, upsample = up, estimate = mean(scores), n = length(scores[1, , ]),
    method = "fully convolutional forward pass; each convolution delegated to grcvf"
  )
}

#' Flamingo: perceiver resampler + tanh-gated cross-attention (hmflmg)
#'
#' Both attention stages delegated to `morie_geron_cross_attention`.
#' @param images Visual features (n_features, d).
#' @param text Frozen LM hidden states (T, d).
#' @param latents Optional perceiver latents (default one row of ones).
#' @param W_Q,W_K,W_V Optional projections (default identity).
#' @param gate Pre-tanh gate value (default 0).
#' @param image_index Optional per-position image index (0 or -1).
#' @return List with `output`, `visual_tokens`, `cross_attention`, `gate_value`, `is_identity_at_init`.
#' @export
morie_geron_flamingo <- function(images, text, latents = NULL, W_Q = NULL, W_K = NULL, W_V = NULL,
                                 gate = 0.0, image_index = NULL) {
  V <- .morie_gr_mat(images, "images")
  Hs <- .morie_gr_mat(text, "text")
  d <- ncol(Hs)
  T_ <- nrow(Hs)
  L <- if (is.null(latents)) matrix(1, 1, d) else .morie_gr_mat(latents, "latents")
  I <- diag(d)
  Wq <- if (is.null(W_Q)) I else .morie_gr_mat(W_Q, "W_Q")
  Wk <- if (is.null(W_K)) I else .morie_gr_mat(W_K, "W_K")
  Wv <- if (is.null(W_V)) I else .morie_gr_mat(W_V, "W_V")
  g <- as.numeric(gate)

  resample <- morie_geron_cross_attention(if (is.null(latents)) matrix(0, nrow(L), d) else L, V, Wq, Wk, Wv)
  visual <- .morie_gr_mat(resample$output, "visual")
  xattn <- morie_geron_cross_attention(Hs, visual, Wq, Wk, Wv)
  X <- .morie_gr_mat(xattn$output, "X")

  if (!is.null(image_index)) {
    idx <- as.integer(image_index)
    .w4a_need(length(idx) == T_, "image_index length must equal text rows.")
    X[idx < 0, ] <- 0
  }
  out <- Hs + tanh(g) * X
  identity <- isTRUE(all.equal(out, Hs, tolerance = 1e-8))
  list(
    output = out, visual_tokens = visual, cross_attention = X, attention_weights = xattn$attention_weights,
    resampler_weights = resample$attention_weights, gate_value = tanh(g), gate = g,
    is_identity_at_init = identity, n_visual_tokens = nrow(visual), n_image_features = nrow(V),
    delta_norm = sqrt(sum((out - Hs)^2)), estimate = sqrt(sum((out - Hs)^2)), n = T_,
    method = "perceiver resampler plus tanh-gated cross-attention; attention delegated to grca"
  )
}

#' Feature map: activation(conv(x, K) + b), convolution delegated to hmfmap's core (hmfmap)
#' @param x (H,W) or (C,H,W).
#' @param K One filter or a stack (F,...).
#' @param b Bias (scalar or per filter).
#' @param activation "relu"/"identity"/"tanh"/"sigmoid".
#' @param stride,padding Conv controls.
#' @return List with `feature_map`, `pre_activation`, `out_shape`, `n_filters`, `sparsity`.
#' @export
morie_geron_feature_map <- function(x, K, b = 0.0, activation = "relu", stride = 1, padding = 0) {
  acts <- list(
    relu = function(z) pmax(z, 0), identity = function(z) z, tanh = tanh,
    sigmoid = function(z) 1 / (1 + exp(-z))
  )
  Xa <- if (length(dim(x)) == 2) array(as.numeric(x), dim = c(1, dim(x))) else array(as.numeric(x), dim = dim(x))
  Ka <- K
  Kdim <- if (is.matrix(K)) c(1L, dim(K)) else dim(K)
  filters <- if (is.matrix(K) || (length(Kdim) == 3 && dim(Xa)[1] > 1 && Kdim[1] == dim(Xa)[1])) {
    list(array(as.numeric(K), dim = if (is.matrix(K)) c(1, dim(K)) else dim(K)))
  } else {
    lapply(seq_len(Kdim[1]), function(i) if (length(Kdim) == 4) array(K[i, , , ], dim = Kdim[2:4]) else array(K[i, , ], dim = c(1, Kdim[2], Kdim[3])))
  }
  Fn <- length(filters)
  bias <- as.numeric(b)
  if (length(bias) == 1) bias <- rep(bias, Fn)
  maps <- lapply(seq_len(Fn), function(i) morie_geron_conv2d_forward(Xa, filters[[i]], b = bias[i], stride = stride, padding = padding)$Y)
  oh <- dim(maps[[1]])[1]
  ow <- dim(maps[[1]])[2]
  Z <- array(0, dim = c(Fn, oh, ow))
  for (i in seq_len(Fn)) Z[i, , ] <- maps[[i]]
  A_ <- acts[[activation]](Z)
  single <- Fn == 1
  out <- if (single) array(A_[1, , ], dim = c(oh, ow)) else A_
  pre <- if (single) array(Z[1, , ], dim = c(oh, ow)) else Z
  flat_arg <- which.max(A_)
  list(
    feature_map = out, pre_activation = pre, out_shape = dim(out), n_filters = Fn, activation = activation,
    sparsity = mean(A_ == 0), max_response = max(A_), argmax = arrayInd(flat_arg, dim(A_)),
    estimate = mean(A_), n = length(A_), method = "F = phi(conv(x, K) + b); convolution delegated to grcvf"
  )
}

#' FashionMNIST CNN architecture resolved to concrete shapes (hmfmn)
#'
#' Output sizes delegated to `morie_geron_conv_output_size`.
#' @param epochs,lr,batch_size,n_classes,input_size Config.
#' @param filters Channels per conv block.
#' @return List with `layers`, `total_params`, `flatten_dim`, `fc_share`, `class_names`.
#' @export
morie_geron_fashion_mnist <- function(epochs = 10, lr = 0.001, batch_size = 32, n_classes = 10,
                                      input_size = 28, filters = c(32, 64)) {
  classes_ <- c(
    "T-shirt/top", "Trouser", "Pullover", "Dress", "Coat",
    "Sandal", "Shirt", "Sneaker", "Bag", "Ankle boot"
  )
  E <- as.integer(epochs)
  eta <- as.numeric(lr)
  bs <- as.integer(batch_size)
  C <- as.integer(n_classes)
  S <- as.integer(input_size)
  chans <- as.integer(filters)
  layers <- list()
  size <- S
  ch <- 1L
  for (f in chans) {
    out <- as.integer(morie_geron_conv_output_size(size, kernel = 3, padding = 0, stride = 1)$out_size)
    layers[[length(layers) + 1L]] <- list(kind = "conv", kernel = 3L, channels = f, out = out, params = f * (3L * 3L * ch) + f)
    size <- out
    ch <- f
    out <- as.integer(morie_geron_conv_output_size(size, kernel = 2, padding = 0, stride = 2)$out_size)
    layers[[length(layers) + 1L]] <- list(kind = "pool", kernel = 2L, channels = ch, out = out, params = 0L)
    size <- out
  }
  flat <- size * size * ch
  dense <- 128L
  layers[[length(layers) + 1L]] <- list(kind = "flatten", out = flat, channels = ch, params = 0L)
  layers[[length(layers) + 1L]] <- list(kind = "fc", out = dense, channels = dense, params = flat * dense + dense)
  layers[[length(layers) + 1L]] <- list(kind = "fc", out = C, channels = C, params = dense * C + C, activation = "softmax")
  total <- sum(vapply(layers, function(l) l$params, 0))
  fc_params <- sum(vapply(layers, function(l) if (l$kind == "fc") l$params else 0, 0))
  list(
    layers = layers, total_params = total, conv_params = sum(vapply(layers, function(l) if (l$kind == "conv") l$params else 0, 0)),
    fc_params = fc_params, fc_share = fc_params / total, flatten_dim = flat,
    class_names = if (C <= length(classes_)) classes_[seq_len(C)] else paste0("class_", seq_len(C) - 1L),
    output_shape = C, training_config = list(epochs = E, lr = eta, batch_size = bs), steps_per_epoch = ceiling(60000 / bs),
    estimate = total, n = length(layers),
    method = "FashionMNIST CNN resolved to concrete shapes; output sizes delegated to grcos"
  )
}

#' IEEE-754 binary16 round-trip with field decomposition (hmfp16)
#'
#' Real bit manipulation: round-to-nearest-even via R's storage.mode
#' coercion is unavailable for float16 in base R, so the cast is done
#' by hand -- extract sign/exponent/mantissa from the double, then
#' round the mantissa to 10 bits with ties-to-even, handling overflow
#' to Inf and underflow to subnormal/zero exactly as IEEE-754 requires.
#' @param x Values to cast.
#' @return List with `value`, `sign`, `exponent`, `mantissa_field`, `rel_error`, `overflowed`, `underflowed`.
#' @export
morie_geron_fp16_quant <- function(x) {
  a <- as.numeric(x)
  n <- length(a)
  sign <- integer(n)
  ef <- integer(n)
  mf <- integer(n)
  back <- numeric(n)
  for (i in seq_len(n)) {
    v <- a[i]
    s <- if (v < 0 || (v == 0 && 1 / v < 0)) 1L else 0L
    av <- abs(v)
    if (av == 0) {
      ef[i] <- 0L
      mf[i] <- 0L
      back[i] <- if (s) -0 else 0
      sign[i] <- s
      next
    }
    if (!is.finite(av)) {
      ef[i] <- 31L
      mf[i] <- 0L
      back[i] <- if (s) -Inf else Inf
      sign[i] <- s
      next
    }
    e <- floor(log2(av))
    # normalize mantissa to [1, 2)
    mant <- av / 2^e
    if (mant >= 2) {
      mant <- mant / 2
      e <- e + 1
    }
    if (mant < 1) {
      mant <- mant * 2
      e <- e - 1
    }
    eb <- e + 15L
    if (eb >= 31L) {
      ef[i] <- 31L
      mf[i] <- 0L
      back[i] <- if (s) -Inf else Inf
      sign[i] <- s
      next
    }
    if (eb < 1L) {
      # subnormal: fixed exponent field 0, scale = 2^-14
      scaled <- av / 2^-14
      m10 <- round(scaled * 1024) / 1024
      mf_val <- round(m10 * 1024)
      if (mf_val >= 1024L) {
        ef[i] <- 1L
        mf[i] <- 0L
        back[i] <- (if (s) -1 else 1) * 2^-14
      } else {
        ef[i] <- 0L
        mf[i] <- as.integer(mf_val)
        back[i] <- (if (s) -1 else 1) * (mf_val / 1024) * 2^-14
      }
      sign[i] <- s
      next
    }
    frac <- mant - 1
    mval <- round(frac * 1024)
    if (mval >= 1024L) {
      mval <- 0L
      eb <- eb + 1L
      if (eb >= 31L) {
        ef[i] <- 31L
        mf[i] <- 0L
        back[i] <- if (s) -Inf else Inf
        sign[i] <- s
        next
      }
    }
    ef[i] <- as.integer(eb)
    mf[i] <- as.integer(mval)
    sign[i] <- s
    back[i] <- (if (s) -1 else 1) * 2^(eb - 15L) * (1 + mval / 1024)
  }
  rel <- ifelse(a != 0, abs(back - a) / abs(ifelse(a == 0, 1, a)), 0)
  over <- is.finite(a) & !is.finite(back)
  under <- (a != 0) & (back == 0 | (ef == 0 & mf != 0))
  finite_rel <- rel[is.finite(rel)]
  list(
    value = back, sign = sign, exponent = ef - 15L, exponent_field = ef, mantissa_field = mf, rel_error = rel,
    max_rel_error = if (length(finite_rel)) max(finite_rel) else Inf, overflowed = over, underflowed = under,
    eps = 2^-10, max_normal = 65504.0, min_normal = 2^-14, bits_total = 16,
    estimate = if (length(finite_rel)) max(finite_rel) else Inf, n = n,
    method = "IEEE-754 binary16 round-trip with field decomposition and range diagnostics"
  )
}

#' IEEE-754 binary32 field decomposition, reconstructed exactly (hmfp32)
#' @param x Values to encode.
#' @return List with `value`, `sign`, `exponent`, `mantissa`, `reconstructed`, `rel_error`, `kind`.
#' @export
morie_geron_fp32 <- function(x) {
  a <- as.numeric(x)
  n <- length(a)
  sign <- integer(n)
  ef <- integer(n)
  mf <- integer(n)
  recon <- numeric(n)
  kind <- character(n)
  val <- numeric(n)
  for (i in seq_len(n)) {
    v <- a[i]
    s <- if (v < 0 || (v == 0 && 1 / v < 0)) 1L else 0L
    av <- abs(v)
    if (av == 0) {
      ef[i] <- 0L
      mf[i] <- 0L
      kind[i] <- "zero"
      recon[i] <- if (s) -0 else 0
      sign[i] <- s
      val[i] <- recon[i]
      next
    }
    if (!is.finite(av)) {
      ef[i] <- 255L
      mf[i] <- 0L
      kind[i] <- "inf"
      recon[i] <- if (s) -Inf else Inf
      sign[i] <- s
      val[i] <- recon[i]
      next
    }
    e <- floor(log2(av))
    mant <- av / 2^e
    if (mant >= 2) {
      mant <- mant / 2
      e <- e + 1
    }
    if (mant < 1) {
      mant <- mant * 2
      e <- e - 1
    }
    eb <- e + 127L
    if (eb >= 255L) {
      ef[i] <- 255L
      mf[i] <- 0L
      kind[i] <- "inf"
      recon[i] <- if (s) -Inf else Inf
      sign[i] <- s
      val[i] <- recon[i]
      next
    }
    if (eb < 1L) {
      scaled <- av / 2^-126
      mval <- round(scaled * 2^23)
      if (mval >= 2^23) {
        ef[i] <- 1L
        mf[i] <- 0L
        recon[i] <- (if (s) -1 else 1) * 2^-126
      } else {
        ef[i] <- 0L
        mf[i] <- as.integer(mval)
        recon[i] <- (if (s) -1 else 1) * (mval / 2^23) * 2^-126
      }
      kind[i] <- if (mf[i] == 0L && ef[i] == 0L) "zero" else "subnormal"
      sign[i] <- s
      val[i] <- recon[i]
      next
    }
    frac <- mant - 1
    mval <- round(frac * 2^23)
    if (mval >= 2^23) {
      mval <- 0L
      eb <- eb + 1L
    }
    ef[i] <- as.integer(eb)
    mf[i] <- as.integer(mval)
    kind[i] <- "normal"
    sign[i] <- s
    recon[i] <- (if (s) -1 else 1) * 2^(eb - 127L) * (1 + mval / 2^23)
    val[i] <- recon[i]
  }
  rel <- ifelse(a != 0, abs(val - a) / abs(ifelse(a == 0, 1, a)), 0)
  rel <- ifelse(is.finite(rel), rel, Inf)
  list(
    value = val, sign = sign, exponent = ef - 127L, exponent_field = ef, mantissa = mf / 2^23, mantissa_field = mf,
    reconstructed = recon, rel_error = rel, eps = 2^-23, max_normal = (2 - 2^-23) * 2^127, min_normal = 2^-126,
    kind = kind, bits_total = 32, estimate = if (any(is.finite(rel))) max(rel[is.finite(rel)]) else Inf, n = n,
    method = "IEEE-754 binary32 field decomposition via a uint32 bit view"
  )
}

#' Few-shot in-context prompt construction with a zero-shot control (hmfsf)
#' @param model function(prompt) -> prediction.
#' @param examples List of (x, y) pairs.
#' @param query Input to predict.
#' @param k Number of demonstrations (default all).
#' @param separator,template Prompt format.
#' @param max_context Optional cap.
#' @return List with `prediction`, `zero_shot_prediction`, `prompt`, `changed_by_context`, `k`.
#' @export
morie_geron_few_shot <- function(model, examples, query, k = NULL, separator = "\n",
                                 template = "{x} -> {y}", max_context = NULL) {
  n_avail <- length(examples)
  kk <- if (is.null(k)) n_avail else as.integer(k)
  .w4a_need(kk >= 0L && kk <= n_avail, "k out of range.")
  fmt <- function(xx, yy) {
    s <- sub("\\{x\\}", xx, template)
    sub("\\{y\\}", yy, s)
  }
  shots <- if (kk > 0) examples[seq_len(kk)] else list()
  prefix <- paste(vapply(shots, function(e) fmt(e[[1]], e[[2]]), ""), collapse = separator)
  tail_ <- paste0(trimws(fmt(query, ""), which = "right"), " ")
  prompt <- if (length(shots)) paste0(prefix, separator, tail_) else tail_
  zero_prompt <- tail_
  if (!is.null(max_context)) {
    mc <- as.integer(max_context)
    .w4a_need(nchar(prompt) <= mc, "prompt exceeds max_context.")
  }
  pred <- model(prompt)
  zero <- model(zero_prompt)
  list(
    prediction = pred, zero_shot_prediction = zero, prompt = prompt, zero_shot_prompt = zero_prompt,
    shots = shots, k = kk, n_available = n_avail, changed_by_context = !identical(pred, zero),
    prompt_length = nchar(prompt), template = template, estimate = kk, n = kk,
    method = "in-context few-shot prompt construction with a zero-shot control"
  )
}

#' Native SGD fine-tuning with frozen params, warmup and weight decay (hmfth)
#' @param model function(theta, batch) -> list(loss, grad).
#' @param dataset Training examples.
#' @param epochs,lr,theta,freeze,batch_size,weight_decay,warmup Config.
#' @return List with `theta`, `theta_init`, `loss_history`, `drift`, `lr_schedule`, `frozen`.
#' @export
morie_geron_finetune_lm <- function(model, dataset, epochs = 10, lr = 0.01, theta = NULL, freeze = NULL,
                                    batch_size = NULL, weight_decay = 0.0, warmup = 0) {
  th <- if (is.null(theta)) 0.0 else as.numeric(theta)
  init <- th
  E <- as.integer(epochs)
  base_lr <- as.numeric(lr)
  wd <- as.numeric(weight_decay)
  W <- as.integer(warmup)
  N <- length(dataset)
  bs <- if (is.null(batch_size)) N else as.integer(batch_size)
  mask <- if (is.null(freeze)) rep(FALSE, length(th)) else as.logical(freeze)
  hist <- numeric(0)
  sched <- numeric(0)
  gnorms <- numeric(0)
  step <- 0L
  pos <- 0L
  for (ep in seq_len(E)) {
    batch <- dataset[(pos + seq_len(bs) - 1L) %% N + 1L]
    pos <- (pos + bs) %% N
    out <- model(th, batch)
    loss <- as.numeric(out[[1]])
    grad <- as.numeric(out[[2]])
    step <- step + 1L
    cur_lr <- if (W && step <= W) base_lr * (step / W) else base_lr
    g <- grad + wd * th
    g <- ifelse(mask, 0, g)
    th <- th - cur_lr * g
    hist <- c(hist, loss)
    sched <- c(sched, cur_lr)
    gnorms <- c(gnorms, sqrt(sum(g^2)))
  }
  list(
    theta = th, theta_init = init, loss_history = hist, grad_norms = gnorms, lr_schedule = sched,
    drift = sqrt(sum((th - init)^2)), n_steps = step, frozen = mask, n_frozen = sum(mask),
    weight_decay = wd, warmup = W, estimate = hist[length(hist)], n = N,
    method = "native SGD fine-tuning loop with frozen parameters, warmup and weight decay"
  )
}

#' GAN minimax training: linear generator vs logistic discriminator (hmgan)
#' @param X Real data (m, d).
#' @param G Optional (W_g, b_g).
#' @param D Optional (w_d, b_d).
#' @param z_dim,epochs,lr,seed,non_saturating Config.
#' @return List with `G`, `D`, `value_history`, `samples`, `equilibrium_value`, `mean_gap`.
#' @export
morie_geron_gan <- function(X, G = NULL, D = NULL, z_dim = 1, epochs = 200, lr = 0.05, seed = 0, non_saturating = TRUE) {
  A <- .morie_gr_mat(X, "X")
  m <- nrow(A)
  d <- ncol(A)
  k <- as.integer(z_dim)
  E <- as.integer(epochs)
  eta <- as.numeric(lr)
  sigmoid <- function(z) ifelse(z >= 0, 1 / (1 + exp(-abs(z))), exp(-abs(z)) / (1 + exp(-abs(z))))
  if (is.null(G)) {
    Wg <- matrix(0, k, d)
    bg <- rep(0, d)
  } else {
    Wg <- .morie_gr_mat(G[[1]], "Wg")
    bg <- as.numeric(G[[2]])
  }
  if (is.null(D)) {
    wd_ <- rep(0, d)
    bd <- 0.0
  } else {
    wd_ <- as.numeric(D[[1]])
    bd <- as.numeric(D[[2]])
  }
  Z <- matrix(morie_lcg_normal(m * k, seed + 1), m, k, byrow = TRUE)
  initial_gap <- abs(mean(Z %*% Wg + matrix(bg, m, d, byrow = TRUE)) - mean(A))
  vals <- numeric(E)
  dls <- numeric(E)
  gls <- numeric(E)
  g_grad_norm <- 0
  for (ep in seq_len(E)) {
    fake <- Z %*% Wg + matrix(bg, m, d, byrow = TRUE)
    d_real <- sigmoid(as.numeric(A %*% wd_) + bd)
    d_fake <- sigmoid(as.numeric(fake %*% wd_) + bd)
    v <- mean(log(pmax(pmin(d_real, 1), 1e-12))) + mean(log(pmax(pmin(1 - d_fake, 1), 1e-12)))
    vals[ep] <- v
    dls[ep] <- -v
    gw <- colMeans(A * (1 - d_real)) - colMeans(fake * d_fake)
    gb <- mean(1 - d_real) - mean(d_fake)
    wd_ <- wd_ + eta * gw
    bd <- bd + eta * gb
    fake <- Z %*% Wg + matrix(bg, m, d, byrow = TRUE)
    d_fake <- sigmoid(as.numeric(fake %*% wd_) + bd)
    coeff <- if (non_saturating) (d_fake - 1) else (-d_fake)
    dfake <- outer(coeff, wd_) / m
    gWg <- t(Z) %*% dfake
    gbg <- colSums(dfake)
    g_grad_norm <- sqrt(sum(gWg^2) + sum(gbg^2))
    gls[ep] <- mean(-log(pmax(d_fake, 1e-12)))
    Wg <- Wg - eta * gWg
    bg <- bg - eta * gbg
  }
  fake <- Z %*% Wg + matrix(bg, m, d, byrow = TRUE)
  d_real <- sigmoid(as.numeric(A %*% wd_) + bd)
  d_fake <- sigmoid(as.numeric(fake %*% wd_) + bd)
  gap <- abs(mean(fake) - mean(A))
  list(
    G = list(W = Wg, b = bg), D = list(w = wd_, b = bd), value_history = vals, d_loss = dls, g_loss = gls,
    samples = fake, real_scores = d_real, fake_scores = d_fake, equilibrium_value = 2 * log(0.5), mean_gap = gap,
    initial_mean_gap = initial_gap, g_grad_norm = g_grad_norm, non_saturating = non_saturating,
    estimate = vals[E], n = m, method = "linear GAN trained by alternating exact gradient steps on the minimax value function"
  )
}

#' GMM-based anomaly detection: log-density thresholding (hmgand)
#'
#' Mixture fitted by `morie_geron_gaussian_mixture` (hmgmm).
#' @param X Training data.
#' @param n_components,threshold,contamination,seed,X_new Config.
#' @return List with `is_anomaly`, `density`, `log_density`, `threshold`, `n_anomalies`.
#' @export
morie_geron_anomaly_gmm <- function(X, n_components = 2, threshold = NULL, contamination = 0.05, seed = 0, X_new = NULL) {
  A <- .morie_gr_mat(X, "X")
  c_ <- as.numeric(contamination)
  .w4a_need(c_ > 0 && c_ < 1, "contamination must lie strictly in (0, 1).")
  fit <- morie_geron_gaussian_mixture(A, n_components = n_components, seed = seed)
  pi_ <- fit$weights
  mu <- fit$means
  Sig <- fit$covariances
  K <- length(pi_)
  log_density <- function(Z) {
    lp <- sapply(seq_len(K), function(k) log(pi_[k] + 1e-300) + morie_gmm_log_pdf(Z, mu[k, ], matrix(Sig[k, , ], ncol(Z), ncol(Z))))
    mx <- apply(lp, 1, max)
    mx + log(rowSums(exp(lp - mx)))
  }
  ld <- log_density(A)
  if (is.null(threshold)) {
    log_thr <- as.numeric(quantile(ld, c_, type = 7))
    thr <- exp(log_thr)
  } else {
    thr <- as.numeric(threshold)
    log_thr <- log(thr)
  }
  flag <- ld < log_thr
  new_ld <- new_flag <- NULL
  if (!is.null(X_new)) {
    Z <- .morie_gr_mat(X_new, "X_new")
    new_ld <- log_density(Z)
    new_flag <- new_ld < log_thr
  }
  list(
    is_anomaly = flag, density = exp(ld), log_density = ld, threshold = thr, log_threshold = log_thr,
    n_anomalies = sum(flag), anomaly_indices = which(flag) - 1L,
    new_density = if (is.null(new_ld)) NULL else exp(new_ld), new_log_density = new_ld, new_is_anomaly = new_flag,
    contamination = c_, gmm = list(weights = pi_, means = mu, log_likelihood = fit$log_likelihood),
    estimate = mean(flag), n = nrow(A), method = "GMM density thresholding in log space; mixture fitted by hmgmm"
  )
}

#' Gradient boosted regression trees: fit residuals sequentially (hmgbrt)
#'
#' Trees delegated to `morie_geron_cart_algorithm` with criterion="mse".
#' @param X,y Data.
#' @param n_estimators,learning_rate,max_depth,loss Config.
#' @return List with `predictions`, `init`, `trees`, `loss_history`, `train_mse`, `monotone`.
#' @export
morie_geron_gradient_boosting <- function(X, y, n_estimators = 10, learning_rate = 0.1, max_depth = 2,
                                          loss = "squared_error") {
  Xa <- .morie_gr_mat(X, "X")
  ya <- as.numeric(y)
  B <- as.integer(n_estimators)
  eta <- as.numeric(learning_rate)
  md <- as.integer(max_depth)
  init <- if (loss == "squared_error") mean(ya) else median(ya)
  Fv <- rep(init, length(ya))
  trees <- list()
  hist <- numeric(0)
  res_hist <- list()
  staged <- list()
  cur_loss <- function(Fv) if (loss == "squared_error") mean((ya - Fv)^2) else mean(abs(ya - Fv))
  hist <- c(hist, cur_loss(Fv))
  for (b in seq_len(B)) {
    resid <- if (loss == "squared_error") (ya - Fv) else sign(ya - Fv)
    res_hist[[b]] <- resid
    if (all(abs(resid) < 1e-12)) break
    t_ <- morie_geron_cart_algorithm(Xa, resid, criterion = "mse", max_depth = md)$tree
    h <- morie_geron_predict_tree(t_, Xa)
    Fv <- Fv + eta * h
    trees[[length(trees) + 1L]] <- t_
    staged[[length(staged) + 1L]] <- Fv
    hist <- c(hist, cur_loss(Fv))
  }
  mono <- all(diff(hist) <= 1e-12)
  list(
    predictions = Fv, init = init, trees = trees, n_trees = length(trees), loss_history = hist,
    residual_history = res_hist, staged_predictions = staged, train_mse = mean((ya - Fv)^2),
    train_mae = mean(abs(ya - Fv)), monotone = mono, learning_rate = eta, loss = loss,
    estimate = mean((ya - Fv)^2), n = nrow(Xa),
    method = "GBRT fitting trees to negative gradients; trees delegated to hmcart (criterion='mse')"
  )
}

#' GoogLeNet/Inception parameter count, module by module (hmgoog)
#' @param in_ch Input channels.
#' @param o1,r3,o3,r5,o5,pp Branch widths.
#' @return List with `branch_1x1`, `branch_3x3`, `branch_5x5`, `branch_pool`, `out_channels`, `params`.
#' @export
morie_inception_module <- function(in_ch, o1, r3, o3, r5, o5, pp) {
  b1 <- in_ch * o1 + o1
  b3 <- (in_ch * r3 + r3) + (9 * r3 * o3 + o3)
  b5 <- (in_ch * r5 + r5) + (25 * r5 * o5 + o5)
  bp <- in_ch * pp + pp
  naive5 <- 25 * in_ch * o5 + o5
  list(
    branch_1x1 = b1, branch_3x3 = b3, branch_5x5 = b5, branch_pool = bp, out_channels = o1 + o3 + o5 + pp,
    params = b1 + b3 + b5 + bp, naive_5x5_params = naive5, reduction_saving = naive5 - b5
  )
}

#' GoogLeNet architecture resolved to concrete shapes and exact params (hmgoog)
#' @param n_classes,input_size,in_channels,dropout Config.
#' @return List with `layers`, `modules`, `total_params`, `final_feature_map`, `total_reduction_saving`.
#' @export
morie_geron_googlenet <- function(n_classes = 1000, input_size = 224, in_channels = 3, dropout = 0.4) {
  incep <- list(
    c(64, 96, 128, 16, 32, 32), c(128, 128, 192, 32, 96, 64), c(192, 96, 208, 16, 48, 64),
    c(160, 112, 224, 24, 64, 64), c(128, 128, 256, 24, 64, 64), c(112, 144, 288, 32, 64, 64),
    c(256, 160, 320, 32, 128, 128), c(256, 160, 320, 32, 128, 128), c(384, 192, 384, 48, 128, 128)
  )
  names_ <- c("3a", "3b", "4a", "4b", "4c", "4d", "4e", "5a", "5b")
  pool_after <- c("3b", "4e")
  C <- as.integer(n_classes)
  S <- as.integer(input_size)
  ch <- as.integer(in_channels)
  p_drop <- as.numeric(dropout)

  layers <- list()
  size <- S
  conv <- function(name, k, s, pad, out_ch) {
    o <- (size - k + 2 * pad) %/% s + 1
    .w4a_need(o >= 1, "input_size too small.")
    params <- out_ch * (k * k * ch) + out_ch
    layers[[length(layers) + 1L]] <<- list(kind = "conv", name = name, out = o, channels = out_ch, params = params)
    size <<- o
    ch <<- out_ch
  }
  poolf <- function(name, k = 3, s = 2, pad = 1) {
    o <- (size - k + 2 * pad) %/% s + 1
    .w4a_need(o >= 1, "input_size too small.")
    layers[[length(layers) + 1L]] <<- list(kind = "pool", name = name, out = o, channels = ch, params = 0)
    size <<- o
  }
  conv("conv1", 7, 2, 3, 64)
  poolf("pool1")
  conv("conv2", 1, 1, 0, 64)
  conv("conv3", 3, 1, 1, 192)
  poolf("pool2")

  modules <- list()
  for (i in seq_along(incep)) {
    p <- incep[[i]]
    name <- names_[i]
    mod <- morie_inception_module(ch, p[1], p[2], p[3], p[4], p[5], p[6])
    mod$name <- name
    mod$in_channels <- ch
    mod$out <- size
    modules[[length(modules) + 1L]] <- mod
    layers[[length(layers) + 1L]] <- list(kind = "inception", name = name, out = size, channels = mod$out_channels, params = mod$params)
    ch <- mod$out_channels
    if (name %in% pool_after) poolf(paste0("pool_", name))
  }
  fc_params <- ch * C + C
  layers[[length(layers) + 1L]] <- list(kind = "gap", name = "global_avg_pool", out = 1, channels = ch, params = 0)
  layers[[length(layers) + 1L]] <- list(kind = "fc", name = "classifier", out = C, channels = C, params = fc_params, dropout = p_drop)
  total <- sum(vapply(layers, function(l) l$params, 0))
  saving <- sum(vapply(modules, function(m) m$reduction_saving, 0))
  list(
    layers = layers, modules = modules, total_params = total,
    conv_params = sum(vapply(layers, function(l) if (l$kind == "conv") l$params else 0, 0)),
    inception_params = sum(vapply(layers, function(l) if (l$kind == "inception") l$params else 0, 0)),
    fc_params = fc_params, output_shape = C, final_feature_map = c(ch, size, size),
    total_reduction_saving = saving, dropout = p_drop, estimate = total, n = length(layers),
    method = "GoogLeNet architecture resolved to concrete shapes and exact per-branch parameter counts"
  )
}

#' GPT-1 architecture (delegated to hmdctr) plus the causal LM objective (hmgpt1)
#' @param X Token ids.
#' @param n_layers,n_heads Optional overrides.
#' @param logits Optional (T, V) logits.
#' @param targets Optional next-token targets (default shifted X).
#' @param ... Other decoder_only args.
#' @return List with `total_params`, `config`, `loss`, `perplexity`, `n_predicted`.
#' @export
morie_geron_gpt1 <- function(X, n_layers = NULL, n_heads = NULL, logits = NULL, targets = NULL, ...) {
  cfg <- list(n_layers = 12L, n_heads = 12L, d_model = 768L, vocab_size = 40478L, max_len = 512L, d_ff = 3072L)
  extra <- list(...)
  for (nm in names(extra)) if (!is.null(extra[[nm]])) cfg[[nm]] <- extra[[nm]]
  if (!is.null(n_layers)) cfg$n_layers <- as.integer(n_layers)
  if (!is.null(n_heads)) cfg$n_heads <- as.integer(n_heads)
  arch <- do.call(morie_geron_decoder_only, c(list(X = X), cfg))

  A <- as.numeric(X)
  n_pred <- max(length(A) - 1L, 0L)
  loss <- ppl <- tok <- NULL
  if (!is.null(logits)) {
    Z <- .morie_gr_mat(logits, "logits")
    y <- if (is.null(targets)) as.integer(A[-1]) else as.integer(targets)
    Zs <- Z[seq_len(nrow(Z) - 1L), , drop = FALSE]
    shift <- Zs - apply(Zs, 1, max)
    logZ <- log(rowSums(exp(shift)))
    tok <- logZ - shift[cbind(seq_len(n_pred), y + 1L)]
    loss <- mean(tok)
    ppl <- exp(loss)
  }
  list(
    total_params = arch$total_params, config = cfg, block_params = arch$block_params,
    embedding_params = arch$embedding_params, d_head = arch$d_head, mask = arch$mask, loss = loss,
    perplexity = ppl, token_losses = tok, n_predicted = n_pred,
    estimate = if (is.null(loss)) arch$total_params else loss, n = length(A),
    method = "GPT-1 architecture delegated to hmdctr, plus the causal LM objective"
  )
}

#' GPT-2 released configurations resolved through hmdctr (hmgpt2)
#' @param X Token ids.
#' @param n_layers,n_heads Optional overrides.
#' @param size "small"/"medium"/"large"/"xl".
#' @param ... Other decoder_only args.
#' @return List with `total_params`, `non_embedding_params`, `config`, `params_vs_small`, `all_sizes`.
#' @export
morie_geron_gpt2 <- function(X, n_layers = NULL, n_heads = NULL, size = "small", ...) {
  sizes <- list(
    small = list(n_layers = 12L, n_heads = 12L, d_model = 768L),
    medium = list(n_layers = 24L, n_heads = 16L, d_model = 1024L),
    large = list(n_layers = 36L, n_heads = 20L, d_model = 1280L),
    xl = list(n_layers = 48L, n_heads = 25L, d_model = 1600L)
  )
  .w4a_need(size %in% names(sizes), "size must be small/medium/large/xl.")
  base <- list(vocab_size = 50257L, max_len = 1024L)
  cfg <- c(base, sizes[[size]])
  extra <- list(...)
  for (nm in names(extra)) if (!is.null(extra[[nm]])) cfg[[nm]] <- extra[[nm]]
  if (!is.null(n_layers)) cfg$n_layers <- as.integer(n_layers)
  if (!is.null(n_heads)) cfg$n_heads <- as.integer(n_heads)
  arch <- do.call(morie_geron_decoder_only, c(list(X = X), cfg))
  total <- arch$total_params
  non_emb <- total - arch$embedding_params
  all_sizes <- list()
  for (nm in names(sizes)) {
    c_ <- c(base, sizes[[nm]])
    all_sizes[[nm]] <- do.call(morie_geron_decoder_only, c(list(X = 1), c_))$total_params
  }
  list(
    total_params = total, non_embedding_params = non_emb, embedding_params = arch$embedding_params,
    block_params = arch$block_params, config = cfg, size = size, d_head = arch$d_head,
    params_vs_small = total / all_sizes$small, all_sizes = all_sizes, mask = arch$mask,
    estimate = total, n = cfg$n_layers, method = "GPT-2 released configurations resolved through hmdctr, with scaling comparisons"
  )
}

#' GPT-3 175B architecture accounting: exact params, shape trace, KV-cache (hmgpt3)
#' @param prompt Token ids.
#' @param n_tokens Tokens to generate.
#' @param n_layers,d_model,n_heads,d_ff,vocab_size,n_ctx,dtype_bytes Config.
#' @return List with `total_parameters`, `parameters_per_layer`, `breakdown`, `shape_trace`, `kv_cache_bytes`.
#' @export
morie_geron_gpt3 <- function(prompt, n_tokens, n_layers = 96, d_model = 12288, n_heads = 96, d_ff = NULL,
                             vocab_size = 50257, n_ctx = 2048, dtype_bytes = 2) {
  ids <- as.integer(round(as.numeric(prompt)))
  n_layers <- as.integer(n_layers)
  d_model <- as.integer(d_model)
  n_heads <- as.integer(n_heads)
  vocab_size <- as.integer(vocab_size)
  n_ctx <- as.integer(n_ctx)
  n_new <- as.integer(n_tokens)
  dtype_bytes <- as.integer(dtype_bytes)
  d_ff <- if (is.null(d_ff)) 4L * d_model else as.integer(d_ff)
  .w4a_need(d_model %% n_heads == 0L, "d_model must be divisible by n_heads.")
  n_prompt <- length(ids)
  total_len <- n_prompt + n_new
  .w4a_need(total_len <= n_ctx, "prompt + generated exceeds context window.")

  d_head <- d_model %/% n_heads
  attn_w <- 4 * d_model * d_model
  attn_b <- 4 * d_model
  mlp_w <- 2 * d_model * d_ff
  mlp_b <- d_ff + d_model
  ln_per_layer <- 2 * (2 * d_model)
  per_layer <- attn_w + attn_b + mlp_w + mlp_b + ln_per_layer

  breakdown <- list(
    token_embedding = vocab_size * d_model, position_embedding = n_ctx * d_model,
    attention_weights = n_layers * attn_w, attention_biases = n_layers * attn_b,
    feedforward_weights = n_layers * mlp_w, feedforward_biases = n_layers * mlp_b,
    layer_norms = n_layers * ln_per_layer + 2 * d_model, output_head = 0
  )
  total <- Reduce(`+`, breakdown)

  shape_trace <- list(
    list("token_ids", c(n_prompt)), list("embedded", c(n_prompt, d_model)),
    list("q_per_head", c(n_heads, n_prompt, d_head)), list("attention_scores", c(n_heads, n_prompt, n_prompt)),
    list("attention_out", c(n_prompt, d_model)), list("ffn_hidden", c(n_prompt, d_ff)),
    list("block_out", c(n_prompt, d_model)),
    list("logits", if (n_new > 0) c(n_new, vocab_size) else c(n_prompt, vocab_size))
  )

  kv_cache_bytes <- 2 * n_layers * total_len * d_model * dtype_bytes
  flops_per_token <- 2 * total
  list(
    total_parameters = total, parameters_per_layer = per_layer, breakdown = breakdown, shape_trace = shape_trace,
    d_head = d_head, n_prompt_tokens = n_prompt, n_generated = n_new, context_used = total_len,
    kv_cache_bytes = kv_cache_bytes, flops_per_token = flops_per_token, estimate = total, n = n_prompt,
    method = "GPT-3 decoder-only architecture accounting"
  )
}

#' Gaussian random projection X' = X R, R_ij ~ N(0, 1/d') (hmgrp)
#' @param X Data (m, d) or a vector treated as one row.
#' @param d_out Target dimension.
#' @param seed RNG seed.
#' @return List with `X_projected`, `R`, `d_in`, `d_out`, `max_distortion`, `mean_distortion`.
#' @export
morie_geron_gaussian_rand_projection <- function(X, d_out, seed = 0) {
  A <- if (is.matrix(X)) X else matrix(as.numeric(X), nrow = 1)
  m <- nrow(A)
  d_in <- ncol(A)
  k <- as.integer(d_out)
  set.seed(as.integer(seed))
  R <- matrix(rnorm(d_in * k, 0, 1 / sqrt(k)), d_in, k)
  Z <- A %*% R
  if (m >= 2) {
    pairs <- combn(m, 2)
    d_before <- vapply(seq_len(ncol(pairs)), function(i) sum((A[pairs[1, i], ] - A[pairs[2, i], ])^2), 0)
    d_after <- vapply(seq_len(ncol(pairs)), function(i) sum((Z[pairs[1, i], ] - Z[pairs[2, i], ])^2), 0)
    keep <- d_before > 0
    if (any(keep)) {
      ratio <- d_after[keep] / d_before[keep]
      max_dist <- max(abs(ratio - 1))
      mean_dist <- mean(abs(ratio - 1))
    } else {
      max_dist <- mean_dist <- 0
    }
  } else {
    max_dist <- mean_dist <- NA_real_
  }
  list(
    X_projected = Z, R = R, d_in = d_in, d_out = k, max_distortion = max_dist, mean_distortion = mean_dist,
    estimate = mean_dist, n = m, method = "Gaussian random projection"
  )
}

#' Ridge regression closed form, the grid-search default estimator (hmgrs)
#' @param X_train,y_train Data.
#' @param alpha Ridge penalty (>=0).
#' @return function(X_new) -> predictions.
#' @export
morie_ridge_estimator <- function(X_train, y_train, alpha = 0.0) {
  X <- .morie_gr_mat(X_train, "X_train")
  y <- as.numeric(y_train)
  a <- as.numeric(alpha)
  th <- solve(t(X) %*% X + a * diag(ncol(X)), t(X) %*% y)
  function(X_new) as.numeric(.morie_gr_mat(X_new, "X_new") %*% th)
}

#' Exhaustive grid search scored by K-fold CV, delegated to `morie_geron_cross_validation_score` (hmgrs)
#' @param param_grid Named list of value vectors.
#' @param X,y Data.
#' @param estimator function(X_train, y_train, ...) -> predict; default ridge.
#' @param K Folds.
#' @param score Optional function(y_true, y_pred) -> score.
#' @return List with `best_params`, `best_score`, `results`, `n_candidates`, `n_fits`.
#' @export
morie_geron_grid_search <- function(param_grid, X, y, estimator = NULL, K = 3, score = NULL) {
  names_ <- names(param_grid)
  .w4a_need(length(names_) > 0L, "param_grid is empty.")
  est <- if (is.null(estimator)) morie_ridge_estimator else estimator
  combos <- expand.grid(param_grid, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  results <- list()
  best <- NULL
  for (i in seq_len(nrow(combos))) {
    params <- as.list(combos[i, , drop = FALSE])
    fit_fn <- function(Xtr, ytr) do.call(est, c(list(Xtr, ytr), params))
    pred_fn <- function(model, Xte) model(Xte)
    cv <- morie_geron_cross_validation_score(X, y, K = K, fit = fit_fn, predict = pred_fn, score = score)
    s <- cv$cv_score
    results[[i]] <- list(params = params, cv_score = s, fold_scores = cv$fold_scores)
    if (is.null(best) || s > best$cv_score) best <- results[[i]]
  }
  n_cand <- length(results)
  list(
    best_params = best$params, best_score = best$cv_score, results = results, n_candidates = n_cand,
    n_fits = n_cand * as.integer(K), estimate = best$cv_score, n = nrow(.morie_gr_mat(X, "X")),
    method = "Exhaustive grid search with K-fold cross-validation"
  )
}

#' Dueling DQN: exact gradients through Q = V + A - mean(A) (hmdldqn)
#' @param env Unused (provenance only).
#' @param V,A Initial streams.
#' @param buffer Transitions.
#' @param epochs,lr,gamma,target_sync Config.
#' @return List with `Q`, `V`, `A`, `loss_history`, `advantage_mean`, `value_share`.
#' @export
morie_geron_dueling_dqn_alt <- function(env, V, A, buffer, epochs = 10, lr = 0.1, gamma = 0.95, target_sync = 5) {
  Vv <- as.numeric(V)
  Av <- .morie_gr_mat(A, "A")
  S <- nrow(Av)
  nA <- ncol(Av)
  buf <- morie_check_buffer(buffer, S, nA, "geron_dueling_dqn")
  s <- buf$s
  a <- buf$a
  r <- buf$r
  s2 <- buf$s2
  done <- buf$done
  E <- as.integer(epochs)
  eta <- as.numeric(lr)
  g <- as.numeric(gamma)
  sync <- as.integer(target_sync)

  Q <- morie_dueling_q(Vv, Av)
  Qt <- Q
  hist <- numeric(E)
  syncs <- integer(0)
  for (ep in seq_len(E)) {
    Q <- morie_dueling_q(Vv, Av)
    boot <- ifelse(done, 0.0, g * apply(Qt[s2 + 1L, , drop = FALSE], 1, max))
    td <- r + boot - Q[cbind(s + 1L, a + 1L)]
    hist[ep] <- mean(td^2)
    for (k in seq_along(s)) {
      i <- s[k] + 1L
      j <- a[k] + 1L
      e <- td[k]
      Vv[i] <- Vv[i] + eta * e
      grad <- rep(-1 / nA, nA)
      grad[j] <- grad[j] + 1
      Av[i, ] <- Av[i, ] + eta * e * grad
    }
    if (ep %% sync == 0L) {
      Qt <- morie_dueling_q(Vv, Av)
      syncs <- c(syncs, ep)
    }
  }
  Q <- morie_dueling_q(Vv, Av)
  denom <- mean(abs(Q))
  share <- if (denom > 0) mean(abs(matrix(Vv, S, nA))) / denom else 1.0

  list(
    Q = Q, V = Vv, A = Av, loss_history = hist,
    advantage_mean = mean(Av - rowMeans(Av)), value_share = share,
    greedy_policy = apply(Q, 1, which.max) - 1L, sync_epochs = syncs, gamma = g, lr = eta,
    estimate = hist[length(hist)], n = length(s),
    method = "dueling DQN with exact gradients through Q = V + A - mean(A)"
  )
}
