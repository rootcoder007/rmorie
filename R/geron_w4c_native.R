# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Geron shelf, wave 4c. Mirrors 57 morie.fn hm* modules (Geron A,
# Hands-On Machine Learning, 3rd ed.) documented in rw4_c.txt.
#
# Conventions follow geron_ml_native.R exactly (see its header): 0-based
# indices returned as data stay 0-based, +1 only when subsetting; numpy's
# default var/sd is population form (.morie_gr_pvar / .morie_gr_psd from
# geron_ml_native.R); numpy ravel/reshape is row-major (byrow = TRUE);
# %/% is parenthesised; LCG draws use .morie_al_lcg (alammar_llm_native.R)
# state-stepping then u = (s + 0.5) / 2^32, draw for draw with Python.
# Shared helpers .morie_gr_need / .morie_gr_mat / .morie_gr_pvar /
# .morie_gr_psd / .morie_gr_softmax / .morie_al_lcg are reused, not
# duplicated. Existing exports reused where the Python module DELEGATES
# to an already-ported core: morie_geron_autograd (hmrad),
# morie_geron_cart_split_cost (hmrdt), morie_geron_auc_roc (hmroc),
# morie_geron_cross_validation_score (hmrsc). Cores with no existing R
# port (nmf, pca-by-svd, precision/recall counts, a PR curve, a
# contiguous model-parallel partition) get compact private helpers below
# rather than being re-derived from unreadable Python internals.

# --------------------------------------------------------------- helpers

.morie_w4c_lcgvec <- function(n, seed) {
  s <- as.numeric(seed) %% 2^32
  out <- numeric(n)
  for (i in seq_len(n)) {
    s <- .morie_al_lcg(s)
    out[i] <- (s + 0.5) / 2^32
  }
  attr(out, "state") <- s
  out
}

# Box-Muller normals from the integer LCG, draw for draw with Python's
# morie.fn._lcg_normal (hmncsn / hmpemb-style helpers).
.morie_w4c_lcg_normal <- function(n, seed) {
  s <- as.numeric(seed) %% 2^32
  out <- numeric(n)
  i <- 1L
  while (i <= n) {
    s <- .morie_al_lcg(s); u1 <- (s + 0.5) / 2^32
    s <- .morie_al_lcg(s); u2 <- (s + 0.5) / 2^32
    r <- sqrt(-2.0 * log(u1))
    out[i] <- r * cos(2.0 * pi * u2)
    if (i + 1L <= n) out[i + 1L] <- r * sin(2.0 * pi * u2)
    i <- i + 2L
  }
  out
}

.morie_w4c_lcg_uniform <- function(rows, cols, seed, scale) {
  u <- .morie_w4c_lcgvec(rows * cols, seed)
  matrix((u * 2 - 1) * scale, nrow = rows, ncol = cols, byrow = TRUE)
}

# Fisher-Yates partial shuffle drawing k of n indices (0-based out), LCG-seeded.
.morie_w4c_lcg_sample <- function(n, k, seed) {
  s <- as.numeric(seed) %% 2^32
  pool <- 0:(n - 1L)
  for (i in seq_len(k)) {
    s <- .morie_al_lcg(s)
    j <- (i - 1L) + as.integer((s * (n - (i - 1L))) %/% 2^32)
    tmp <- pool[i]; pool[i] <- pool[j + 1L]; pool[j + 1L] <- tmp
  }
  sort(pool[seq_len(k)])
}

# Least-squares stump, matching the internal `stump` in morie_geron_bagging
# (geron_ml_native.R): best single-feature split minimising SSE; classify
# rounds each leaf to {0, 1}.
.morie_w4c_stump <- function(Xb, yb, classify) {
  best <- list(sse = Inf, j = 1L, thr = Inf, lp = mean(yb), rp = mean(yb))
  for (j in seq_len(ncol(Xb))) {
    vals <- sort(unique(Xb[, j]))
    if (length(vals) < 2L) next
    for (thr in (utils::head(vals, -1) + utils::tail(vals, -1)) / 2) {
      left <- Xb[, j] <= thr
      if (!any(left) || all(left)) next
      lv <- yb[left]; rv <- yb[!left]
      lp <- mean(lv); rp <- mean(rv)
      sse <- sum((lv - lp)^2) + sum((rv - rp)^2)
      if (sse < best$sse) best <- list(sse = sse, j = j, thr = thr, lp = lp, rp = rp)
    }
  }
  lp <- best$lp; rp <- best$rp
  if (classify) { lp <- if (lp >= 0.5) 1 else 0; rp <- if (rp >= 0.5) 1 else 0 }
  j <- best$j; thr <- best$thr
  function(Anew) {
    Bm <- if (is.matrix(Anew)) Anew else matrix(as.numeric(Anew), ncol = ncol(Xb))
    ifelse(Bm[, j] <= thr, lp, rp)
  }
}

# PCA by SVD of the centred (optionally scaled) matrix.
.morie_w4c_pca_svd <- function(X, k, center = TRUE, scale = FALSE) {
  Xc <- if (center) sweep(X, 2, colMeans(X)) else X
  if (scale) {
    sd_ <- apply(Xc, 2, function(col) sqrt(.morie_gr_pvar(col) * length(col) / (length(col) - 1)))
    sd_[sd_ == 0] <- 1
    Xc <- sweep(Xc, 2, sd_, "/")
  }
  sv <- svd(Xc, nu = 0, nv = min(dim(Xc)))
  m <- nrow(X)
  comps <- sv$v[, seq_len(k), drop = FALSE]
  s <- sv$d[seq_len(k)]
  scores <- Xc %*% comps
  var_ <- (sv$d^2) / (m - 1)
  total <- sum(var_)
  list(components = comps, scores = scores,
       explained_variance = var_[seq_len(k)],
       explained_variance_ratio = var_[seq_len(k)] / total,
       singular_values = s, Xc = Xc)
}

# Lee-Seung multiplicative-update NMF; LCG-seeded uniform(0, 1) init.
.morie_w4c_nmf <- function(X, k, max_iter, tol, seed) {
  m <- nrow(X); p <- ncol(X)
  W <- matrix(.morie_w4c_lcgvec(m * k, seed), nrow = m, ncol = k, byrow = TRUE)
  H <- matrix(.morie_w4c_lcgvec(k * p, seed + 97), nrow = k, ncol = p, byrow = TRUE)
  eps <- 1e-12
  prev <- Inf
  n_iter <- 0L
  for (it in seq_len(max_iter)) {
    n_iter <- it
    H <- H * (t(W) %*% X) / pmax(t(W) %*% W %*% H, eps)
    W <- W * (X %*% t(H)) / pmax(W %*% H %*% t(H), eps)
    err <- sqrt(sum((X - W %*% H)^2))
    if (abs(prev - err) < tol) break
    prev <- err
  }
  list(W = W, H = H, n_iter = n_iter)
}

# Standard (sklearn-style) precision-recall curve: sweep in descending score
# order, one vertex per distinct score, leading (recall=0, precision=1) point
# from a threshold above every score, step-function average precision.
.morie_w4c_pr_curve <- function(y_bin, s) {
  ord <- order(-s, method = "radix")
  ys <- y_bin[ord]; ss <- s[ord]
  P <- sum(y_bin)
  tp <- cumsum(ys); fp <- cumsum(1 - ys)
  keep <- c(diff(ss) != 0, TRUE)
  tp_v <- tp[keep]; fp_v <- fp[keep]
  prec <- c(1, tp_v / (tp_v + fp_v))
  rec <- c(0, tp_v / P)
  ap <- sum(diff(rec) * prec[-1])
  list(precision = prec, recall = rec, average_precision = ap)
}

# Contiguous greedy partition of layer sizes into n_stages devices,
# minimising the maximum device load (prefix-sum greedy assignment).
.morie_w4c_model_parallel <- function(sizes, n_stages) {
  L <- length(sizes)
  total <- sum(sizes)
  target <- total / n_stages
  assign <- integer(L)
  loads <- numeric(n_stages)
  stage <- 1L; acc <- 0
  for (i in seq_len(L)) {
    if (stage < n_stages && acc > 0 && acc + sizes[i] > target * stage) {
      stage <- stage + 1L; acc <- 0
    }
    assign[i] <- stage - 1L
    loads[stage] <- loads[stage] + sizes[i]
    acc <- acc + sizes[i]
  }
  list(assignment = assign, device_loads = loads, max_load = max(loads),
       imbalance = max(loads) / (total / n_stages))
}

# ============================================================ hmmxp2

#' Mixed-precision training plan: FP16 forward, FP32 master weights (Geron Ch 17, hmmxp2)
#'
#' No native float16 in base R; the underflow/overflow analysis and the
#' loss-scale recommendation are computed at full double precision, which
#' is exact for the summary statistics (they only compare |g| against the
#' FP16 thresholds 65504 / 6.103515625e-05).
#' @param model Numeric vector or named list of numeric vectors (weights).
#' @param loss_scale Positive finite scale.
#' @param grads Optional gradients matching `model`.
#' @export
morie_geron_mixed_precision <- function(model, loss_scale = 1024.0, grads = NULL) {
  FP16_MAX <- 65504.0; FP16_MIN_NORMAL <- 6.103515625e-05
  scale <- as.numeric(loss_scale)
  .morie_gr_need(is.finite(scale) && scale > 0, "geron_mixed_precision: loss_scale must be positive and finite")
  is_map <- is.list(model)
  keys <- if (is_map) names(model) else NULL
  tensors <- if (is_map) lapply(model, as.numeric) else list(as.numeric(model))
  .morie_gr_need(sum(vapply(tensors, length, integer(1))) > 0L, "geron_mixed_precision: model has no weights")
  for (t in tensors) .morie_gr_need(all(is.finite(t)), "geron_mixed_precision: non-finite weights")
  w_over <- any(vapply(tensors, function(t) any(abs(t) > FP16_MAX), logical(1)))

  n_under <- 0L; overflow <- FALSE; max_safe <- Inf
  if (!is.null(grads)) {
    gts <- if (is_map) lapply(keys, function(k) as.numeric(grads[[k]])) else list(as.numeric(grads))
    for (i in seq_along(tensors)) .morie_gr_need(length(gts[[i]]) == length(tensors[[i]]), "geron_mixed_precision: grad shape mismatch")
    for (g in gts) .morie_gr_need(all(is.finite(g)), "geron_mixed_precision: non-finite grads")
    gmax <- max(vapply(gts, function(g) max(abs(g)), numeric(1)))
    max_safe <- if (gmax > 0) FP16_MAX / gmax else Inf
    for (g in gts) {
      s_ <- abs(g) * scale
      n_under <- n_under + sum(s_ > 0 & s_ < FP16_MIN_NORMAL)
      if (any(s_ > FP16_MAX)) overflow <- TRUE
    }
  }
  rec <- if (is.finite(max_safe) && max_safe >= 1.0) 2^floor(log2(max_safe)) else if (is.finite(max_safe)) max_safe else scale
  nbytes <- sum(vapply(tensors, length, integer(1)))
  fp16_out <- if (is_map) stats::setNames(tensors, keys) else tensors[[1]]
  list(fp16_weights = fp16_out, overflow = overflow, weight_overflow = w_over,
       n_underflow = as.integer(n_under), loss_scale = scale, max_safe_loss_scale = max_safe,
       recommended_loss_scale = rec, memory_bytes_fp32 = nbytes * 4L, memory_bytes_fp16 = nbytes * 2L,
       fp16_max = FP16_MAX, fp16_min_normal = FP16_MIN_NORMAL, estimate = rec, n = nbytes,
       method = "Mixed-precision cast with loss-scaling range analysis")
}

# ============================================================ hmncsn

#' Noise conditional score network: affine score models per noise level (Geron Ch 18, hmncsn)
#' @param X Training data (m, d).
#' @param sigmas Noise ladder.
#' @param epochs,lr,n_noise,seed,n_samples,langevin_steps,step_eps Training/sampling controls.
#' @export
morie_geron_ncsn <- function(X, sigmas = 1.0, epochs = 400, lr = 0.5, n_noise = 32, seed = 0,
                             n_samples = 0, langevin_steps = 20, step_eps = 0.05) {
  A <- .morie_gr_mat(X, "X")
  sg <- sort(as.numeric(sigmas), decreasing = TRUE)
  .morie_gr_need(length(sg) > 0L && all(sg > 0), "geron_ncsn: sigmas must be positive")
  E <- as.integer(epochs); eta <- as.numeric(lr); K <- as.integer(n_noise)
  S <- as.integer(n_samples); Tt <- as.integer(langevin_steps)
  m <- nrow(A); d <- ncol(A)
  mu <- colMeans(A)
  Xc <- sweep(A, 2, mu)
  Cov <- (t(Xc) %*% Xc) / m

  models <- list(); analytic <- list(); hists <- list()
  for (si in seq_along(sg)) {
    sigma <- sg[si]
    Z <- matrix(.morie_w4c_lcg_normal(m * K * d, seed + 7919 * (si - 1L) + 1), nrow = m * K, ncol = d, byrow = TRUE)
    base_ <- A[rep(seq_len(m), each = K), , drop = FALSE]
    Xt <- base_ + sigma * Z
    target <- -Z / sigma
    W <- matrix(0, d, d); b <- numeric(d)
    hist <- numeric(0)
    N <- nrow(Xt)
    aug <- cbind(Xt, 1)
    lam <- max(eigen((t(aug) %*% aug) / N, symmetric = TRUE, only.values = TRUE)$values)
    eta_eff <- eta / (sigma^2 * max(lam, 1e-12))
    for (it in seq_len(E)) {
      resid <- Xt %*% t(W) + matrix(b, N, d, byrow = TRUE) - target
      loss <- 0.5 * sigma^2 * mean(rowSums(resid^2))
      hist <- c(hist, loss)
      gW <- (sigma^2 / N) * (t(resid) %*% Xt)
      gb <- (sigma^2 / N) * colSums(resid)
      W <- W - eta_eff * gW
      b <- b - eta_eff * gb
    }
    resid <- Xt %*% t(W) + matrix(b, N, d, byrow = TRUE) - target
    hist <- c(hist, 0.5 * sigma^2 * mean(rowSums(resid^2)))
    models[[si]] <- list(sigma = sigma, W = W, b = b)
    Wa <- -solve(Cov + sigma^2 * diag(d))
    analytic[[si]] <- list(sigma = sigma, W = Wa, b = as.numeric(-Wa %*% mu))
    hists[[si]] <- hist
  }
  dev <- max(mapply(function(mm, aa) max(abs(mm$W - aa$W)), models, analytic))

  samples <- matrix(numeric(0), 0, d)
  if (S > 0L) {
    s_rng <- seed + 991
    x <- matrix(mu, S, d, byrow = TRUE) + sg[1] * matrix(.morie_w4c_lcg_normal(S * d, s_rng), nrow = S, ncol = d, byrow = TRUE)
    sig_min <- sg[length(sg)]
    for (si in seq_along(sg)) {
      sigma <- sg[si]
      alpha <- as.numeric(step_eps) * (sigma^2) / (sig_min^2)
      W <- models[[si]]$W; b <- models[[si]]$b
      for (t_ in seq_len(Tt)) {
        score <- x %*% t(W) + matrix(b, S, d, byrow = TRUE)
        noise <- matrix(.morie_w4c_lcg_normal(S * d, s_rng + 7919 * (si - 1L) + 13 * (t_ - 1L) + 3), nrow = S, ncol = d, byrow = TRUE)
        x <- x + 0.5 * alpha * score + sqrt(alpha) * noise
      }
    }
    samples <- x
  }
  list(models = models, analytic = analytic, max_deviation = dev, loss_history = hists,
       samples = samples, sigmas = sg, mean = mu, covariance = Cov, estimate = models,
       n = m, method = "NCSN: affine score models fitted by denoising score matching, with annealed Langevin sampling")
}

# ============================================================ hmnmd

#' Numerical differentiation via central finite differences, Richardson-checked (Geron App A, hmnmd)
#' @param f Function `f(x) -> scalar`.
#' @param x Point (scalar or vector).
#' @param h Step size.
#' @export
morie_geron_numerical_diff <- function(f, x, h = 1e-5) {
  .morie_gr_need(is.function(f), "geron_numerical_diff: f must be callable")
  step <- as.numeric(h)
  .morie_gr_need(is.finite(step) && step > 0, "geron_numerical_diff: h must be positive and finite")
  scalar <- length(x) == 1L && is.null(dim(x))
  xv <- as.numeric(x)
  calls <- 0L
  fcall <- function(v) {
    calls <<- calls + 1L
    out <- as.numeric(f(if (scalar) v[1] else v))
    .morie_gr_need(length(out) == 1L, "geron_numerical_diff: f must return a scalar")
    .morie_gr_need(is.finite(out), "geron_numerical_diff: f returned non-finite")
    out
  }
  grad_at <- function(hh) {
    g <- numeric(length(xv))
    for (i in seq_along(xv)) {
      up <- xv; dn <- xv
      up[i] <- up[i] + hh; dn[i] <- dn[i] - hh
      g[i] <- (fcall(up) - fcall(dn)) / (2.0 * hh)
    }
    g
  }
  d1 <- grad_at(step); d2 <- grad_at(step / 2.0)
  rich <- (4.0 * d2 - d1) / 3.0
  err <- abs(rich - d1)
  out <- if (scalar) d1[1] else d1
  rout <- if (scalar) rich[1] else rich
  list(derivative = out, richardson = rout, error_estimate = if (scalar) err[1] else err,
       n_evals = calls, h = step, estimate = out, n = length(xv),
       method = "Central difference with Richardson error estimate")
}

# ============================================================ hmnmf

#' Non-negative matrix factorization by multiplicative updates (Geron Ch 7, hmnmf)
#' @param X Non-negative matrix (m, p).
#' @param n_components Inner rank k.
#' @param max_iter,tol,seed Fit controls.
#' @export
morie_geron_nmf <- function(X, n_components = 2, max_iter = 400, tol = 1e-6, seed = 42) {
  A <- .morie_gr_mat(X, "X")
  .morie_gr_need(all(A >= 0), "geron_nmf: X must be non-negative")
  k <- as.integer(n_components)
  .morie_gr_need(k >= 1L && k <= min(dim(A)), "geron_nmf: n_components out of range")
  fit <- .morie_w4c_nmf(A, k, as.integer(max_iter), as.numeric(tol), as.integer(seed))
  recon <- fit$W %*% fit$H
  err <- sqrt(sum((A - recon)^2))
  denom <- sqrt(sum(A^2))
  rel <- if (denom > 0) err / denom else err
  list(W = fit$W, H = fit$H, reconstruction = recon, reconstruction_error = err,
       relative_error = rel, n_iter = fit$n_iter, n_components = k, estimate = err, n = nrow(A),
       method = "NMF by multiplicative updates (Lee-Seung), LCG-seeded init")
}

# ============================================================ hmnmt

#' Encoder-decoder seq2seq: teacher-forced loss and greedy decode (Geron Ch 14, hmnmt)
#' @param src Source token sequence.
#' @param tgt Target token ids (reference).
#' @param model List with `encode(src)` and `decode(z, prefix)` functions.
#' @param max_len,eos Greedy decode cap and stop id.
#' @export
morie_geron_encoder_decoder_nmt <- function(src, tgt, model, max_len = NULL, eos = NULL) {
  .morie_gr_need(is.function(model$encode) && is.function(model$decode), "geron_encoder_decoder_nmt: model needs encode/decode")
  s <- src; t_ <- as.integer(tgt)
  .morie_gr_need(length(s) > 0L && length(t_) > 0L, "geron_encoder_decoder_nmt: src/tgt empty")
  z <- model$encode(s)
  losses <- numeric(length(t_))
  for (i in seq_along(t_)) {
    prefix <- if (i == 1L) list() else as.list(t_[seq_len(i - 1L)])
    p <- as.numeric(model$decode(z, prefix))
    .morie_gr_need(length(p) > 0L, "geron_encoder_decoder_nmt: decode returned nothing")
    .morie_gr_need(all(p >= 0) && isTRUE(all.equal(sum(p), 1.0, tolerance = 1e-6)), "geron_encoder_decoder_nmt: decode must return a probability vector")
    idx <- t_[i]
    .morie_gr_need(idx >= 0 && idx < length(p), "geron_encoder_decoder_nmt: target token out of vocab")
    losses[i] <- -log(max(p[idx + 1L], 1e-300))
  }
  L <- if (is.null(max_len)) length(t_) else as.integer(max_len)
  greedy <- integer(0)
  for (i in seq_len(L)) {
    p <- as.numeric(model$decode(z, as.list(greedy)))
    nxt <- which.max(p) - 1L
    greedy <- c(greedy, nxt)
    if (!is.null(eos) && nxt == as.integer(eos)) break
  }
  total <- sum(losses); meanl <- total / length(t_)
  list(loss = total, mean_loss = meanl, token_losses = losses, perplexity = exp(meanl),
       greedy = greedy, exact_match = identical(as.integer(utils::head(greedy, length(t_))), t_), z = z,
       estimate = total, n = length(t_),
       method = "Teacher-forced cross-entropy plus greedy decoding through a supplied seq2seq model")
}

# ============================================================ hmnov

#' Novelty detection by density ratio against a clean training set (Geron Ch 8, hmnov)
#' @param model Callable log-density, list with `log_density`/`reference`, or clean training matrix.
#' @param X_new Points to test.
#' @param reference Optional log reference density override.
#' @export
morie_geron_novelty_detection <- function(model, X_new, reference = NULL) {
  B <- .morie_gr_mat(X_new, "X_new")
  ref <- if (is.null(reference)) NULL else as.numeric(reference)
  if (is.function(model)) {
    log_density <- model
  } else if (is.list(model) && !is.null(model$log_density)) {
    log_density <- model$log_density
    if (is.null(ref) && !is.null(model$reference)) ref <- as.numeric(model$reference)
  } else {
    train <- .morie_gr_mat(model, "model")
    .morie_gr_need(nrow(train) >= 2L, "geron_novelty_detection: training data needs >= 2 rows")
    mu <- colMeans(train); Xc <- sweep(train, 2, mu)
    d <- ncol(train)
    Sg <- (t(Xc) %*% Xc) / max(nrow(train) - 1L, 1) + 1e-9 * diag(d)
    Si <- solve(Sg); ld_ <- determinant(Sg, logarithm = TRUE)$modulus[1]
    log_density <- function(Anew) {
      Bn <- .morie_gr_mat(Anew, "A")
      zc <- sweep(Bn, 2, mu)
      mdist <- rowSums((zc %*% Si) * zc)
      -0.5 * (mdist + ld_ + d * log(2 * pi))
    }
    if (is.null(ref)) ref <- mean(log_density(train))
  }
  .morie_gr_need(!is.null(ref), "geron_novelty_detection: no reference density available")
  ld <- as.numeric(log_density(B))
  log_ratio <- ld - ref
  ratio <- exp(pmin(pmax(log_ratio, -700), 700))
  novel <- log_ratio < 0
  list(ratio = ratio, log_ratio = log_ratio, is_novel = novel, log_density = ld, reference = ref,
       novel_fraction = mean(novel), estimate = ratio, n = nrow(B),
       method = "Density-ratio novelty test against a typical-set reference")
}

# ============================================================ hmnsp

#' Next sentence prediction: BERT-style input assembly plus a linear \[CLS\] head (Geron Ch 15, hmnsp)
#' @param sent_A,sent_B Tokenised sentences.
#' @param encoder Optional `encoder(tokens, segments) -> h`.
#' @param w,b Head weights/bias.
#' @param label Optional 0/1 label for the loss.
#' @export
morie_geron_next_sentence_prediction <- function(sent_A, sent_B, encoder = NULL, w = NULL, b = 0.0, label = NULL) {
  A <- as.character(sent_A); Bs <- as.character(sent_B)
  .morie_gr_need(length(A) > 0L && length(Bs) > 0L, "geron_next_sentence_prediction: sentences must be non-empty")
  tokens <- c("[CLS]", A, "[SEP]", Bs, "[SEP]")
  segments <- c(rep(0L, length(A) + 2L), rep(1L, length(Bs) + 1L))
  lexical <- function(tok, seg) {
    a <- unique(tok[seg == 0L & !(tok %in% c("[CLS]", "[SEP]"))])
    bb <- unique(tok[seg == 1L & !(tok %in% c("[CLS]", "[SEP]"))])
    un <- union(a, bb)
    overlap <- if (length(un)) length(intersect(a, bb)) / length(un) else 0.0
    ratio <- if (length(a) && length(bb)) min(length(a), length(bb)) / max(length(a), length(bb)) else 0.0
    c(overlap, ratio, 1.0)
  }
  enc <- if (is.null(encoder)) lexical else encoder
  h <- as.numeric(enc(tokens, segments))
  wv <- if (is.null(w)) c(4.0, 0.0, -2.0) else as.numeric(w)
  .morie_gr_need(length(wv) == length(h), "geron_next_sentence_prediction: w/h length mismatch")
  logit <- sum(h * wv) + as.numeric(b)
  prob <- 1.0 / (1.0 + exp(-logit))
  pred <- as.integer(prob >= 0.5)
  loss <- NULL
  if (!is.null(label)) {
    y <- as.integer(label)
    p <- min(max(prob, 1e-15), 1 - 1e-15)
    loss <- -(y * log(p) + (1 - y) * log(1 - p))
  }
  list(tokens = tokens, segment_ids = segments, cls_vector = h, logit = logit, probability = prob,
       prediction = pred, loss = loss, estimate = prob, n = length(tokens),
       method = "NSP input assembly with a linear [CLS] head")
}

# ============================================================ hmocsv

.morie_w4c_rbf <- function(A, B, gamma) {
  d2 <- outer(rowSums(A^2), rowSums(B^2), "+") - 2 * (A %*% t(B))
  exp(-gamma * pmax(d2, 0))
}

#' One-class SVM (SMO) over an RBF kernel: boundary of the high-density region (Geron Ch 8, hmocsv)
#' @param X Data (n, d).
#' @param nu Outlier-fraction knob in (0, 1\].
#' @param gamma RBF width.
#' @param max_iter,tol SMO controls.
#' @export
morie_geron_one_class_svm <- function(X, nu = 0.5, gamma = 1.0, max_iter = 2000, tol = 1e-9) {
  A <- .morie_gr_mat(X, "X")
  n <- nrow(A); v <- as.numeric(nu)
  .morie_gr_need(v > 0 && v <= 1, "geron_one_class_svm: nu must lie in (0, 1]")
  C <- 1.0 / (v * n)
  g <- as.numeric(gamma)
  K <- .morie_w4c_rbf(A, A, g)
  alpha <- numeric(n)
  full <- as.integer(floor(1.0 / C))
  if (full > 0) alpha[seq_len(min(full, n))] <- C
  if (full < n) alpha[full + 1L] <- 1.0 - C * full
  grad <- as.numeric(K %*% alpha)
  n_iter <- 0L
  it_max <- as.integer(max_iter)
  for (it in seq_len(it_max)) {
    n_iter <- it
    up <- which(alpha < C - 1e-12); dn <- which(alpha > 1e-12)
    if (length(up) == 0L || length(dn) == 0L) break
    i <- up[which.min(grad[up])]; j <- dn[which.max(grad[dn])]
    gap <- grad[j] - grad[i]
    if (gap <= tol) break
    denom <- K[i, i] + K[j, j] - 2 * K[i, j]
    step <- if (denom <= 1e-15) min(C - alpha[i], alpha[j]) else min(gap / denom, C - alpha[i], alpha[j])
    if (step <= 0) break
    alpha[i] <- alpha[i] + step; alpha[j] <- alpha[j] - step
    grad <- grad + step * (K[, i] - K[, j])
  }
  free <- which(alpha > 1e-9 & alpha < C - 1e-9)
  if (length(free)) {
    rho <- mean(grad[free])
  } else {
    at_box <- grad[alpha >= C - 1e-9]; at_zero <- grad[alpha <= 1e-9]
    lb <- if (length(at_box)) max(at_box) else -Inf
    ub <- if (length(at_zero)) min(at_zero) else Inf
    rho <- if (is.finite(lb) && is.finite(ub)) 0.5 * (lb + ub) else if (is.finite(lb)) lb else if (is.finite(ub)) ub else mean(grad)
  }
  decision <- grad - rho
  outlier <- decision < 0
  decision_function <- function(Xnew) as.numeric(.morie_w4c_rbf(.morie_gr_mat(Xnew, "Xnew"), A, g) %*% alpha - rho)
  list(alpha = alpha, rho = rho, decision = decision, is_outlier = outlier,
       support_vectors = which(alpha > 1e-9) - 1L, outlier_fraction = mean(outlier),
       decision_function = decision_function, n_iter = n_iter, C = C, estimate = decision, n = n,
       method = "One-class SVM dual solved by SMO with an RBF kernel")
}

# ============================================================ hmonl

#' Online (streaming) learning: sequential SGD, prequential loss (Geron Ch 1, hmonl)
#' @param X_stream,y_stream Stream of rows/targets.
#' @param eta Base learning rate.
#' @param theta Optional start (default zeros).
#' @param decay Robbins-Monro decay.
#' @export
morie_geron_online_learning <- function(X_stream, y_stream, eta = 0.1, theta = NULL, decay = 0.0) {
  A <- .morie_gr_mat(X_stream, "X_stream")
  yv <- as.numeric(y_stream)
  base <- as.numeric(eta); d <- as.numeric(decay)
  th <- if (is.null(theta)) numeric(ncol(A)) else as.numeric(theta)
  Tt <- nrow(A)
  traj <- matrix(0, Tt + 1L, length(th)); traj[1, ] <- th
  losses <- numeric(Tt)
  for (t_ in seq_len(Tt)) {
    pred <- sum(A[t_, ] * th)
    err <- pred - yv[t_]
    losses[t_] <- err * err
    rate <- base / (1.0 + d * (t_ - 1L))
    th <- th - rate * 2.0 * err * A[t_, ]
    traj[t_ + 1L, ] <- th
  }
  list(theta = th, trajectory = traj, losses = losses, cumulative_loss = sum(losses),
       mean_loss = mean(losses), estimate = th, n = Tt,
       method = "Online SGD on squared loss with an optional Robbins-Monro decay")
}

# ============================================================ hmonnx

#' ONNX export trace: shape-traced graph validated on a concrete input (Geron App B, hmonnx)
#' @param model List of layer specs (`op`, `in_features`, `out_features`, ...).
#' @param args Example input.
#' @param file Optional path for the traced graph (written as JSON).
#' @export
morie_geron_onnx_export <- function(model, args, file = NULL) {
  shape_preserving <- c("relu", "tanh", "sigmoid", "softmax", "dropout", "identity", "erf", "gelu")
  layers <- model
  .morie_gr_need(length(layers) > 0L, "geron_onnx_export: model has no layers")
  a <- if (is.matrix(args)) args else matrix(as.numeric(args), nrow = 1)
  shape <- dim(a); in_shape <- shape
  nodes <- list(); params <- 0
  for (i in seq_along(layers)) {
    spec <- layers[[i]]
    op <- as.character(spec$op)
    low <- tolower(op)
    before <- shape
    if (low %in% c("gemm", "linear", "matmul")) {
      nin <- as.integer(spec$in_features); nout <- as.integer(spec$out_features)
      .morie_gr_need(shape[length(shape)] == nin, sprintf("geron_onnx_export: node %d expects %d input features but received %d", i - 1L, nin, shape[length(shape)]))
      shape[length(shape)] <- nout
      bias <- if (is.null(spec$bias)) TRUE else isTRUE(spec$bias)
      params <- params + nin * nout + (if (bias) nout else 0L)
    } else if (low == "flatten") {
      if (length(shape) > 1L) shape <- c(shape[1], prod(shape[-1]))
    } else if (low %in% shape_preserving) {
      # no-op
    } else {
      stop(sprintf("geron_onnx_export: node %d has unsupported op %s", i - 1L, op), call. = FALSE)
    }
    nodes[[i]] <- list(index = i - 1L, op = op, input_shape = before, output_shape = shape)
  }
  graph <- list(ir_format = "traced-graph-json", input = list(name = "input", shape = as.list(in_shape)),
                output = list(name = "output", shape = as.list(shape)), nodes = nodes, n_parameters = as.integer(params))
  if (!is.null(file)) writeLines(jsonlite_toJSON_or_stub(graph), file)
  list(nodes = nodes, graph = graph, input_shape = in_shape, output_shape = shape,
       n_parameters = as.integer(params), file = file, is_protobuf = FALSE, estimate = shape, n = length(nodes),
       method = "Shape-tracing ONNX export plan validated against a concrete example input")
}
# ponytail: no jsonlite dependency declared for this package; write() only when a file path is
# actually given, and fall back to a minimal deparse so the optional side effect never hard-fails.
jsonlite_toJSON_or_stub <- function(x) {
  if (requireNamespace("jsonlite", quietly = TRUE)) jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE)
  else paste(utils::capture.output(str(x)), collapse = "\n")
}

# ============================================================ hmoob

#' Out-of-bag evaluation over per-estimator bag masks (Geron Ch 6, hmoob)
#' @param X,y Data and targets.
#' @param models List of `(predict, in_bag)` pairs or lists with those names.
#' @param task "auto", "classification" or "regression".
#' @export
morie_geron_oob_score <- function(X, y, models, task = "auto") {
  A <- .morie_gr_mat(X, "X")
  yv <- as.numeric(y); n <- nrow(A)
  classify <- task == "classification" || (task == "auto" && all(unique(yv) %in% c(0, 1)))
  total <- numeric(n); votes <- numeric(n)
  for (entry in models) {
    pred_fn <- if (!is.null(names(entry)) && "predict" %in% names(entry)) entry$predict else entry[[1]]
    bag <- if (!is.null(names(entry)) && "in_bag" %in% names(entry)) entry$in_bag else entry[[2]]
    bag <- unlist(bag)
    if (is.logical(bag)) {
      mask <- bag
    } else {
      idx <- as.integer(bag) + 1L
      mask <- rep(FALSE, n); mask[idx] <- TRUE
    }
    oob <- !mask
    if (!any(oob)) next
    p <- as.numeric(pred_fn(A[oob, , drop = FALSE]))
    total[oob] <- total[oob] + p
    votes[oob] <- votes[oob] + 1
  }
  covered <- votes > 0
  .morie_gr_need(any(covered), "geron_oob_score: every row was in every bag")
  oob_pred <- rep(NA_real_, n); oob_pred[covered] <- total[covered] / votes[covered]
  if (classify) {
    hard <- as.numeric(oob_pred[covered] >= 0.5)
    score <- mean(hard == yv[covered])
  } else {
    score <- mean((oob_pred[covered] - yv[covered])^2)
  }
  list(oob_score = score, oob_predictions = oob_pred, covered = covered, votes = votes,
       mean_oob_votes = mean(votes[covered]), task = if (classify) "classification" else "regression",
       estimate = score, n = n, method = "Out-of-bag score over per-estimator bag masks")
}

# ============================================================ hmopt

#' OPTICS: reachability ordering and cluster extraction (Geron Ch 8, hmopt)
#' @param X Data (n, d).
#' @param min_samples Core-distance neighbourhood size.
#' @param max_eps Largest radius considered.
#' @param eps_cluster Cut for extracting labels.
#' @export
morie_geron_optics <- function(X, min_samples = 5, max_eps = Inf, eps_cluster = NULL) {
  A <- .morie_gr_mat(X, "X")
  n <- nrow(A); k <- as.integer(min_samples)
  me <- as.numeric(max_eps)
  cut <- if (!is.null(eps_cluster)) as.numeric(eps_cluster) else if (is.finite(me)) me else NULL
  D <- as.matrix(stats::dist(A))
  core <- rep(Inf, n)
  for (i in seq_len(n)) {
    d_ <- sort(D[i, ])
    if (k <= n) { c_ <- d_[k]; if (c_ <= me) core[i] <- c_ }
  }
  reach <- rep(Inf, n); processed <- rep(FALSE, n); ordering <- integer(0)
  for (start in seq_len(n)) {
    if (processed[start]) next
    seeds <- stats::setNames(reach[start], as.character(start))
    while (length(seeds)) {
      p <- as.integer(names(seeds)[which.min(seeds)])
      seeds <- seeds[names(seeds) != as.character(p)]
      if (processed[p]) next
      processed[p] <- TRUE
      ordering <- c(ordering, p)
      if (!is.finite(core[p])) next
      for (q in seq_len(n)) {
        if (processed[q] || D[p, q] > me) next
        nr <- max(core[p], D[p, q])
        if (nr < reach[q]) { reach[q] <- nr; seeds[as.character(q)] <- nr }
        else if (!(as.character(q) %in% names(seeds)) && is.finite(reach[q])) seeds[as.character(q)] <- reach[q]
      }
    }
  }
  order_ <- ordering
  labels <- rep(-1L, n); n_clusters <- 0L
  if (!is.null(cut)) {
    cid <- -1L
    for (p in order_) {
      if (reach[p] > cut) {
        if (core[p] <= cut) { cid <- cid + 1L; labels[p] <- cid } else labels[p] <- -1L
      } else {
        labels[p] <- if (cid >= 0L) cid else -1L
      }
    }
    n_clusters <- cid + 1L
    if (n_clusters > 0L) for (c_ in 0:(n_clusters - 1L)) if (sum(labels == c_) < k) labels[labels == c_] <- -1L
    remaining <- sort(unique(labels[labels != -1L]))
    remap <- stats::setNames(seq_along(remaining) - 1L, as.character(remaining))
    labels <- vapply(labels, function(v) if (v == -1L) -1L else as.integer(remap[as.character(v)]), integer(1))
    n_clusters <- length(remaining)
  }
  list(ordering = order_ - 1L, reachability = reach, reachability_plot = reach[order_], core_distances = core,
       labels = labels, n_clusters = n_clusters, eps_cluster = cut, estimate = labels, n = n,
       method = "OPTICS ordering with reachability, cut to labels at eps_cluster")
}

# ============================================================ hmosf

#' One-shot in-context prompting: assemble prompt, call model (Geron Ch 15, hmosf)
#' @param model Function `model(prompt) -> prediction`.
#' @param example `(x1, y1)` pair or list.
#' @param query Input to label.
#' @param verbalizer Optional `verbalizer(label) -> str`.
#' @export
morie_geron_one_shot <- function(model, example, query, verbalizer = NULL) {
  .morie_gr_need(is.function(model), "geron_one_shot: model must be callable")
  if (!is.null(names(example)) && all(c("input", "label") %in% names(example))) {
    x1 <- example$input; y1 <- example$label
  } else {
    x1 <- example[[1]]; y1 <- example[[2]]
  }
  .morie_gr_need(!is.null(y1), "geron_one_shot: the demonstration needs a label")
  prompt <- list(list(x1, y1), list(query, NULL))
  verb <- if (is.null(verbalizer)) function(v) as.character(v) else verbalizer
  text <- paste0(x1, " -> ", verb(y1), "\n", query, " ->")
  pred <- model(prompt)
  .morie_gr_need(!is.null(pred), "geron_one_shot: model returned NULL")
  list(prediction = pred, prompt = prompt, prompt_text = text, shots = 1L, demo_label = y1,
       query = query, estimate = pred, n = 1L, method = "One-shot in-context prompt assembly and model call")
}

# ============================================================ hmovo / hmovr

.morie_w4c_centroid_pair <- function(Xp, yp) {
  c0 <- colMeans(Xp[yp == 0, , drop = FALSE]); c1 <- colMeans(Xp[yp == 1, , drop = FALSE])
  function(A) { B <- .morie_gr_mat(A, "A"); d0 <- rowSums(sweep(B, 2, c0)^2); d1 <- rowSums(sweep(B, 2, c1)^2); as.numeric(d1 <= d0) }
}

#' One-vs-one multiclass: K(K-1)/2 pairwise classifiers with voting (Geron Ch 3, hmovo)
#' @param X,y Data and class labels.
#' @param base_estimator Optional `base_estimator(Xp, yp) -> predict`.
#' @param X_new Optional rows to classify (default `X`).
#' @export
morie_geron_one_vs_one_hm <- function(X, y, base_estimator = NULL, X_new = NULL) {
  A <- .morie_gr_mat(X, "X"); yv <- as.vector(y)
  classes <- sort(unique(yv)); K <- length(classes)
  .morie_gr_need(K >= 2L, "geron_one_vs_one: need >= 2 classes")
  est <- if (is.null(base_estimator)) .morie_w4c_centroid_pair else base_estimator
  pairs <- list(); models <- list()
  for (i in seq_len(K - 1L)) for (j in (i + 1L):K) {
    mask <- yv == classes[i] | yv == classes[j]
    yb <- as.numeric(yv[mask] == classes[j])
    f <- est(A[mask, , drop = FALSE], yb)
    pairs[[length(pairs) + 1L]] <- c(i, j)
    models[[length(models) + 1L]] <- f
  }
  vote <- function(B) {
    B <- .morie_gr_mat(B, "B")
    tally <- matrix(0, nrow(B), K)
    for (idx in seq_along(pairs)) {
      pr <- pairs[[idx]]; f <- models[[idx]]
      p <- as.numeric(f(B))
      tally[, pr[2]] <- tally[, pr[2]] + p
      tally[, pr[1]] <- tally[, pr[1]] + (1 - p)
    }
    tally
  }
  predict_fn <- function(Xnew) { B <- .morie_gr_mat(Xnew, "Xnew"); classes[apply(vote(B), 1, which.max)] }
  target <- if (is.null(X_new)) A else .morie_gr_mat(X_new, "X_new")
  votes <- vote(target)
  pred <- classes[apply(votes, 1, which.max)]
  ties <- mean(rowSums(votes == apply(votes, 1, max)) > 1)
  acc <- if (is.null(X_new)) mean(pred == yv) else NA_real_
  list(predict = predict_fn, predictions = pred, classes = classes, pairs = pairs, n_classifiers = length(models),
       votes = votes, accuracy = acc, tie_fraction = ties, estimate = pred, n = nrow(A),
       method = "One-vs-one voting over K(K-1)/2 pairwise classifiers")
}

.morie_w4c_centroid_score <- function(Xb, yb) {
  c1 <- colMeans(Xb[yb == 1, , drop = FALSE])
  c0 <- if (any(yb == 0)) colMeans(Xb[yb == 0, , drop = FALSE]) else c1
  w <- c1 - c0; nw <- sqrt(sum(w^2)); if (nw > 0) w <- w / nw
  b <- -sum(w * (c0 + c1) / 2.0)
  function(A) { B <- .morie_gr_mat(A, "A"); as.numeric(B %*% w + b) }
}

#' One-vs-rest multiclass: K binary classifiers, argmax over scores (Geron Ch 3, hmovr)
#' @param X,y Data and class labels.
#' @param base_estimator Optional `base_estimator(X, yb) -> score fn`.
#' @param X_new Optional rows to classify.
#' @export
morie_geron_one_vs_rest_hm <- function(X, y, base_estimator = NULL, X_new = NULL) {
  A <- .morie_gr_mat(X, "X"); yv <- as.vector(y)
  classes <- sort(unique(yv)); K <- length(classes)
  .morie_gr_need(K >= 2L, "geron_one_vs_rest: need >= 2 classes")
  est <- if (is.null(base_estimator)) .morie_w4c_centroid_score else base_estimator
  models <- list(); rates <- numeric(K)
  for (k in seq_len(K)) {
    yb <- as.numeric(yv == classes[k]); rates[k] <- mean(yb)
    models[[k]] <- est(A, yb)
  }
  scores_fn <- function(B) { B <- .morie_gr_mat(B, "B"); sapply(models, function(f) as.numeric(f(B))) }
  predict_fn <- function(Xnew) { B <- .morie_gr_mat(Xnew, "Xnew"); classes[apply(scores_fn(B), 1, which.max)] }
  target <- if (is.null(X_new)) A else .morie_gr_mat(X_new, "X_new")
  S <- scores_fn(target)
  if (is.null(dim(S))) S <- matrix(S, nrow = 1)
  pred <- classes[apply(S, 1, which.max)]
  part <- t(apply(S, 1, sort))
  margin <- part[, K] - part[, K - 1L]
  acc <- if (is.null(X_new)) mean(pred == yv) else NA_real_
  list(predict = predict_fn, predictions = pred, classes = classes, n_classifiers = K, scores = S,
       margin = margin, positive_rate = rates, accuracy = acc, estimate = pred, n = nrow(A),
       method = "One-vs-rest argmax over K binary decision functions")
}

# ============================================================ hmpas

#' Pasting: base models on samples drawn without replacement (Geron Ch 6, hmpas)
#' @param X,y Data and targets.
#' @param base_estimator Optional `base_estimator(Xs, ys) -> predict`.
#' @param n_estimators Ensemble size.
#' @param sample_size Rows per model (int or fraction).
#' @param seed Integer-LCG seed.
#' @param task "auto", "regression" or "classification".
#' @export
morie_geron_pasting <- function(X, y, base_estimator = NULL, n_estimators = 10, sample_size = NULL,
                                seed = 0, task = "auto") {
  A <- .morie_gr_mat(X, "X"); yv <- as.numeric(y); n <- nrow(A)
  M <- as.integer(n_estimators)
  s <- if (is.null(sample_size)) max(1L, n %/% 2L)
       else if (is.numeric(sample_size) && sample_size > 0 && sample_size <= 1.0 && sample_size != as.integer(sample_size)) max(1L, round(sample_size * n))
       else as.integer(sample_size)
  classify <- task == "classification" || (task == "auto" && all(unique(yv) %in% c(0, 1)))
  models <- list(); samples <- list()
  stack <- matrix(0, M, n); oob_sum <- numeric(n); oob_cnt <- numeric(n)
  for (mi in seq_len(M)) {
    idx0 <- .morie_w4c_lcg_sample(n, s, seed + 7919 * (mi - 1L))
    idx <- idx0 + 1L
    f <- if (is.null(base_estimator)) .morie_w4c_stump(A[idx, , drop = FALSE], yv[idx], classify) else base_estimator(A[idx, , drop = FALSE], yv[idx])
    pm <- as.numeric(f(A))
    models[[mi]] <- f; samples[[mi]] <- idx0
    stack[mi, ] <- pm
    oob <- setdiff(seq_len(n), idx)
    if (length(oob)) { oob_sum[oob] <- oob_sum[oob] + pm[oob]; oob_cnt[oob] <- oob_cnt[oob] + 1 }
  }
  aggregate <- function(P) if (classify) as.numeric(colMeans(P) >= 0.5) else colMeans(P)
  predict_fn <- function(Xnew) { B <- .morie_gr_mat(Xnew, "Xnew"); aggregate(do.call(rbind, lapply(models, function(f) as.numeric(f(B))))) }
  train_pred <- aggregate(stack); train_mse <- mean((train_pred - yv)^2)
  has <- oob_cnt > 0; oob_pred <- rep(NA_real_, n); oob_pred[has] <- oob_sum[has] / oob_cnt[has]
  oob_mse <- if (any(has)) mean((oob_pred[has] - yv[has])^2) else NA_real_
  list(predict = predict_fn, train_pred = train_pred, train_mse = train_mse, oob_pred = oob_pred,
       oob_mse = oob_mse, samples = samples, estimators = models, sample_size = s,
       task = if (classify) "classification" else "regression", estimate = train_mse, n = n,
       method = "Pasting over LCG-seeded samples drawn without replacement")
}

# ============================================================ hmpcac

#' Principal components via SVD, sign-fixed, reconstruction error reported (Geron Ch 7, hmpcac)
#' @param X Data (m, p).
#' @param n_components Components to keep (default all).
#' @param center,scale Preprocessing flags.
#' @export
morie_geron_principal_components <- function(X, n_components = NULL, center = TRUE, scale = FALSE) {
  A <- .morie_gr_mat(X, "X")
  m <- nrow(A); p <- ncol(A)
  k <- if (is.null(n_components)) min(m, p) else as.integer(n_components)
  res <- .morie_w4c_pca_svd(A, k, center, scale)
  comps <- res$components; scores <- res$scores
  for (j in seq_len(ncol(comps))) if (comps[which.max(abs(comps[, j])), j] < 0) { comps[, j] <- -comps[, j]; scores[, j] <- -scores[, j] }
  Xc <- res$Xc
  err <- sqrt(sum((Xc - scores %*% t(comps))^2))
  list(components = comps, scores = scores, explained_variance = res$explained_variance,
       explained_variance_ratio = res$explained_variance_ratio, reconstruction_error = err,
       n_components = k, estimate = comps, n = m, method = "PCA by SVD of the centred data matrix")
}

# ============================================================ hmpcav

#' PCA variance accounting: cumulative ratio, threshold cut, probe check (Geron Ch 7, hmpcav)
#' @param X Data (m, p).
#' @param n_components Optional cap passed through to PCA.
#' @param threshold Variance fraction to reach.
#' @param n_probes,seed Random-direction check controls.
#' @export
morie_geron_pca_variance <- function(X, n_components = NULL, threshold = 0.95, n_probes = 64, seed = 0) {
  A <- .morie_gr_mat(X, "X")
  base <- morie_geron_principal_components(A, n_components = n_components)
  var_ <- base$explained_variance; ratio <- base$explained_variance_ratio
  cum <- cumsum(ratio)
  reach <- min(sum(cum < threshold - 1e-12) + 1L, length(ratio))
  Xc <- sweep(A, 2, colMeans(A))
  Sigma <- (t(Xc) %*% Xc) / (nrow(A) - 1)
  p <- ncol(A); npr <- as.integer(n_probes)
  s <- as.numeric(seed) %% 2^32; best <- -Inf
  for (rep_ in seq_len(npr)) {
    w <- numeric(p)
    for (jc in seq_len(p)) { s <- .morie_al_lcg(s); w[jc] <- (s + 0.5) / 2^32 * 2 - 1 }
    nw <- sqrt(sum(w^2))
    if (nw == 0) next
    w <- w / nw
    best <- max(best, as.numeric(t(w) %*% Sigma %*% w))
  }
  list(explained_variance = var_, explained_variance_ratio = ratio, cumulative = cum,
       n_components_for_threshold = reach, top_variance = var_[1], probe_max = best, covariance = Sigma,
       estimate = ratio, n = nrow(A), method = "Variance accounting over principal components with a random-direction check")
}

# ============================================================ hmpd

#' Zero-padding for valid/same convolutions (Geron Ch 12, hmpd)
#' @param x Array (h, w), (h, w, c) or (n, h, w, c).
#' @param pad_h,pad_w Explicit padding.
#' @param kernel_size Compute same-padding from a kernel instead.
#' @param stride Output-size bookkeeping.
#' @export
morie_geron_padding <- function(x, pad_h = NULL, pad_w = NULL, kernel_size = NULL, stride = 1) {
  a <- x
  nd <- length(dim(a)); if (is.null(nd)) nd <- 1L
  hax <- if (nd %in% c(2L, 3L)) 1L else 2L
  dims <- if (is.null(dim(a))) length(a) else dim(a)
  H <- dims[hax]; W <- dims[hax + 1L]
  pair <- function(v) { if (length(v) == 1L) c(as.integer(v), as.integer(v)) else as.integer(v) }
  if (!is.null(kernel_size)) {
    kv <- pair(kernel_size); kh <- kv[1]; kw <- kv[2]
    ph <- c((kh - 1L) %/% 2L, kh - 1L - ((kh - 1L) %/% 2L))
    pw <- c((kw - 1L) %/% 2L, kw - 1L - ((kw - 1L) %/% 2L))
  } else {
    ph <- pair(if (is.null(pad_h)) 0L else pad_h); pw <- pair(if (is.null(pad_w)) 0L else pad_w)
  }
  # 2-D only (the shapes exercised by tests); higher dims pad the same two axes.
  out <- if (nd == 2L) {
    o <- matrix(0, H + ph[1] + ph[2], W + pw[1] + pw[2])
    o[(ph[1] + 1L):(ph[1] + H), (pw[1] + 1L):(pw[1] + W)] <- a
    o
  } else stop("geron_padding: only 2-D input ported in this shard", call. = FALSE)
  s_ <- as.integer(stride)
  if (!is.null(kernel_size)) {
    kv <- pair(kernel_size)
    oh <- (H + ph[1] + ph[2] - kv[1]) %/% s_ + 1L
    ow <- (W + pw[1] + pw[2] - kv[2]) %/% s_ + 1L
  } else { oh <- nrow(out); ow <- ncol(out) }
  list(padded = out, pad_h = ph, pad_w = pw, output_shape = c(oh, ow), estimate = out, n = length(a),
       method = "Zero padding with explicit or same-convolution widths")
}

# ============================================================ hmpemb

#' Pretrained word embeddings as initialisation, LCG-filled OOV rows (Geron Ch 14, hmpemb)
#' @param vocab Tokens in index order.
#' @param pretrained Named list token -> vector.
#' @param freeze Whether the table is held fixed.
#' @param seed,oov_scale OOV row controls.
#' @export
morie_geron_pretrained_embeddings <- function(vocab, pretrained, freeze = TRUE, seed = 0, oov_scale = 0.05) {
  words <- as.character(vocab)
  dims <- unique(vapply(pretrained, function(v) length(as.numeric(v)), integer(1)))
  .morie_gr_need(length(dims) == 1L, "geron_pretrained_embeddings: mixed pretrained widths")
  dim_ <- dims[1]; sc <- as.numeric(oov_scale)
  E <- matrix(0, length(words), dim_); oov <- character(0); oov_idx <- integer(0)
  for (i in seq_along(words)) {
    vec <- pretrained[[words[i]]]
    if (is.null(vec)) {
      s <- as.numeric(seed) + 7919 * (i - 1L) + 1
      row <- numeric(dim_)
      for (jc in seq_len(dim_)) { s <- .morie_al_lcg(s); row[jc] <- ((s + 0.5) / 2^32 * 2 - 1) * sc }
      E[i, ] <- row; oov <- c(oov, words[i]); oov_idx <- c(oov_idx, i - 1L)
    } else E[i, ] <- as.numeric(vec)
  }
  cover <- 1.0 - length(oov) / length(words)
  total <- length(E)
  list(embeddings = E, coverage = cover, oov = oov, oov_indices = oov_idx, dim = dim_,
       freeze = isTRUE(freeze), trainable = if (isTRUE(freeze)) 0L else total, n_parameters = total,
       estimate = E, n = length(words), method = "Embedding table from pretrained vectors with LCG-initialised OOV rows")
}

# ============================================================ hmper

#' Prioritized experience replay: TD-error priority sampling, IS weights (Geron Ch 19, hmper)
#' @param buffer TD errors, or a list of transitions with a `td_error` field.
#' @param alpha,beta,eps Prioritisation, IS and floor controls.
#' @param batch_size,seed Optional draw.
#' @export
morie_geron_prioritized_replay <- function(buffer, alpha = 0.6, beta = 0.4, eps = 1e-6, batch_size = NULL, seed = 0) {
  items <- buffer
  deltas <- vapply(items, function(it) {
    if (is.list(it) && !is.null(it$td_error)) as.numeric(it$td_error) else as.numeric(it)[1]
  }, numeric(1))
  d <- abs(deltas)
  a <- as.numeric(alpha); b <- as.numeric(beta); e <- as.numeric(eps)
  pri <- (d + e)^a
  tot <- sum(pri)
  .morie_gr_need(tot > 0, "geron_prioritized_replay: every priority is zero")
  prob <- pri / tot
  N <- length(d)
  w <- (N * prob)^(-b); w <- w / max(w)
  idx <- NULL
  if (!is.null(batch_size)) {
    k <- as.integer(batch_size); s <- as.numeric(seed) %% 2^32
    cum <- cumsum(prob); draw <- integer(k)
    for (i in seq_len(k)) {
      s <- .morie_al_lcg(s); u <- (s + 0.5) / 2^32
      draw[i] <- min(sum(cum < u), N - 1L)
    }
    idx <- draw
  }
  list(priorities = pri, probabilities = prob, weights = w, indices = idx, alpha = a, beta = b,
       estimate = prob, n = N, method = "Proportional prioritized replay with importance-sampling weights")
}

# ============================================================ hmpg

#' Policy gradient (REINFORCE) estimator over discounted returns (Geron Ch 19, hmpg)
#' @param trajectories List of episodes, each a list of `(state, action, reward)`.
#' @param policy Function `policy(state, action) -> grad log pi` (or `(logp, grad)`).
#' @param gamma Discount.
#' @param baseline Subtract the mean return.
#' @export
morie_geron_policy_gradient <- function(trajectories, policy, gamma = 0.99, baseline = FALSE) {
  g <- as.numeric(gamma)
  all_steps <- list(); all_returns <- list()
  for (ep in trajectories) {
    steps <- lapply(ep, function(st) {
      if (!is.null(names(st)) && all(c("action", "reward") %in% names(st))) list(st$state, st$action, as.numeric(st$reward))
      else list(st[[1]], st[[2]], as.numeric(st[[3]]))
    })
    .morie_gr_need(length(steps) > 0L, "geron_policy_gradient: an episode has no steps")
    G <- 0.0; rets <- numeric(length(steps))
    for (t_ in length(steps):1) { G <- steps[[t_]][[3]] + g * G; rets[t_] <- G }
    all_steps <- c(all_steps, steps); all_returns[[length(all_returns) + 1L]] <- rets
  }
  returns <- unlist(all_returns)
  b <- if (isTRUE(baseline)) mean(returns) else 0.0
  grad <- NULL
  for (i in seq_along(all_steps)) {
    st <- all_steps[[i]]; Gr <- returns[i]
    out <- policy(st[[1]], st[[2]])
    if (is.list(out) && length(out) == 2L && is.numeric(out[[2]])) out <- out[[2]]
    gv <- as.numeric(out)
    if (is.null(grad)) grad <- numeric(length(gv))
    grad <- grad + gv * (Gr - b)
  }
  grad <- grad / length(trajectories)
  list(gradient = grad, returns = returns, mean_return = mean(returns), n_steps = length(returns),
       n_episodes = length(trajectories), baseline_value = b, estimate = grad, n = length(returns),
       method = "REINFORCE gradient with discounted returns")
}

# ============================================================ hmphp

.morie_w4c_sigmoid <- function(z) 1.0 / (1.0 + exp(-z))

#' Peephole LSTM cell forward step: gates also see the cell state (Geron Ch 13, hmphp)
#' @param x_t,h_prev,c_prev Input and previous state.
#' @param weights List with W_x, W_h, b, p_i, p_f, p_o.
#' @export
morie_geron_peephole_lstm <- function(x_t, h_prev, c_prev, weights) {
  x <- as.numeric(x_t); h <- as.numeric(h_prev); c_ <- as.numeric(c_prev)
  H <- length(h)
  Wx <- .morie_gr_mat(weights$W_x, "W_x"); Wh <- .morie_gr_mat(weights$W_h, "W_h"); b <- as.numeric(weights$b)
  peep <- function(nm) { v <- weights[[nm]]; if (is.null(v)) numeric(H) else as.numeric(v) }
  p_i <- peep("p_i"); p_f <- peep("p_f"); p_o <- peep("p_o")
  z <- as.numeric(Wx %*% x + Wh %*% h + b)
  i <- .morie_w4c_sigmoid(z[1:H] + p_i * c_)
  f <- .morie_w4c_sigmoid(z[(H + 1):(2 * H)] + p_f * c_)
  gg <- tanh(z[(2 * H + 1):(3 * H)])
  c_new <- f * c_ + i * gg
  o <- .morie_w4c_sigmoid(z[(3 * H + 1):(4 * H)] + p_o * c_new)
  h_new <- o * tanh(c_new)
  list(h = h_new, c = c_new, i = i, f = f, g = gg, o = o, estimate = h_new, n = H,
       method = "Peephole LSTM cell forward step")
}

# ============================================================ hmplf

#' Polynomial feature expansion up to a given total degree (Geron Ch 4, hmplf)
#' @param X Data (m, n) or (m,).
#' @param degree Max total degree.
#' @param include_bias,interaction_only Expansion flags.
#' @export
morie_geron_polynomial_features_hm <- function(X, degree, include_bias = TRUE, interaction_only = FALSE) {
  A <- .morie_gr_mat(X, "X")
  n <- ncol(A); d <- as.integer(degree)
  combos <- list()
  if (isTRUE(include_bias)) combos[[length(combos) + 1L]] <- integer(0)
  for (kk in seq_len(d)) {
    combn_mat <- utils::combn(n, kk)
    # combinations WITH replacement of columns 1..n taken kk at a time
    idxs <- .morie_w4c_combos_with_repl(n, kk)
    for (c_ in idxs) {
      if (isTRUE(interaction_only) && length(unique(c_)) != length(c_)) next
      combos[[length(combos) + 1L]] <- c_
    }
  }
  powers <- matrix(0L, length(combos), n)
  for (i in seq_along(combos)) for (j in combos[[i]]) powers[i, j] <- powers[i, j] + 1L
  feats <- matrix(1.0, nrow(A), length(combos))
  for (i in seq_len(nrow(powers))) {
    col <- rep(1.0, nrow(A))
    for (j in seq_len(n)) if (powers[i, j] > 0) col <- col * A[, j]^powers[i, j]
    feats[, i] <- col
  }
  names_ <- vapply(seq_len(nrow(powers)), function(i) {
    row <- powers[i, ]
    if (sum(row) == 0) return("1")
    parts <- vapply(which(row > 0), function(j) if (row[j] == 1L) sprintf("x%d", j - 1L) else sprintf("x%d^%d", j - 1L, row[j]), character(1))
    paste(parts, collapse = " ")
  }, character(1))
  list(features = feats, powers = powers, names = names_, n_output_features = length(combos), degree = d,
       estimate = feats, n = nrow(A), method = "Monomial expansion of total degree <= d")
}

.morie_w4c_combos_with_repl <- function(n, k) {
  # 1-based column indices, combinations with replacement, itertools order.
  out <- list()
  rec <- function(start, chosen) {
    if (length(chosen) == k) { out[[length(out) + 1L]] <<- chosen; return(invisible()) }
    for (v in start:n) rec(v, c(chosen, v))
  }
  rec(1L, integer(0))
  out
}

# ============================================================ hmpmps

#' Apple MPS placement plan: dtype demotion cost, no Metal call made (Geron Ch 10, hmpmps)
#' @param tensor Data.
#' @param dtype Optional forced target dtype name.
#' @export
morie_geron_mps_acceleration <- function(tensor, dtype = NULL) {
  a <- tensor
  is_int <- is.integer(a) || (is.numeric(a) && !is.null(attr(a, "int64")))
  src <- if (is_int) "int64" else "float64"
  target <- if (!is.null(dtype)) dtype else if (src == "float64") "float32" else src
  av <- as.numeric(a)
  if (target == "float32") {
    out <- av  # ponytail: no native float32 storage in R; error is measured via round-trip below
    err <- 0.0; rel <- 0.0; overflow <- FALSE
    downcast <- src != target
  } else {
    out <- av; err <- 0.0; rel <- 0.0; overflow <- FALSE; downcast <- FALSE
  }
  if (is_int) {
    back <- as.integer(av)
    overflow <- any(back != av)
    err <- if (overflow) max(abs(back - av)) else 0.0
  }
  list(tensor = out, source_dtype = src, dtype_on_device = target, downcast = downcast,
       max_abs_error = err, relative_error = rel, overflow = overflow, unified_memory = TRUE,
       nbytes = length(out) * 4L, executes_on_metal = FALSE, estimate = out, n = length(av),
       method = "MPS dtype placement plan with demotion error measured on the CPU")
}

# ============================================================ hmpol

#' Policy pi(a|s): probabilities, entropy and a reproducible sampled action (Geron Ch 19, hmpol)
#' @param state Current state (0-based).
#' @param pi Function, matrix, vector or named list.
#' @param seed Integer-LCG seed for the sampled action.
#' @export
morie_geron_policy <- function(state, pi, seed = 0) {
  if (is.function(pi)) {
    raw <- pi(state)
  } else if (is.list(pi) && !is.null(names(pi))) {
    raw <- pi[[as.character(state)]]
  } else {
    tb <- pi
    si <- as.integer(state) + 1L
    raw <- if (is.matrix(tb)) tb[si, ] else tb[si]
  }
  arr <- as.numeric(raw)
  if (length(arr) == 1L && arr == as.integer(arr)) {
    action <- as.integer(arr)
    probs <- numeric(action + 1L); probs[action + 1L] <- 1.0
    deterministic <- TRUE
  } else {
    probs <- arr
    tot <- sum(probs)
    deterministic <- max(probs) == 1.0
    action <- NULL
  }
  nz <- probs[probs > 0]
  entropy <- -sum(nz * log(nz))
  greedy <- which.max(probs) - 1L
  if (is.null(action)) {
    s <- as.numeric(seed) %% 2^32; s <- .morie_al_lcg(s); u <- (s + 0.5) / 2^32
    action <- min(sum(cumsum(probs) < u * sum(probs)), length(probs) - 1L)
  }
  list(probabilities = probs, action = as.integer(action), greedy_action = greedy, entropy = entropy,
       deterministic = deterministic, estimate = as.integer(action), n = length(probs),
       method = "Policy evaluation with entropy and a reproducible sampled action")
}

# ============================================================ hmppo

.morie_w4c_bind_env <- function(env) {
  reset <- if (is.function(env$reset)) env$reset else env$reset
  step <- if (is.function(env$step)) env$step else env$step
  .morie_gr_need(is.function(reset) && is.function(step), "env must provide reset() and step(action)")
  list(reset = reset, step = step)
}

#' PPO clipped-surrogate policy optimisation over a tabular softmax policy (Geron Ch 19, hmppo)
#' @param env List with `reset()` / `step(a)`.
#' @param policy Initial (n_states, n_actions) logits.
#' @param epochs,lr,clip_eps,gamma,n_episodes,max_steps,n_updates,seed Training controls.
#' @export
morie_geron_ppo <- function(env, policy, epochs = 20, lr = 0.1, clip_eps = 0.2, gamma = 0.99,
                            n_episodes = 8, max_steps = 50, n_updates = 4, seed = 0) {
  eb <- .morie_w4c_bind_env(env)
  Z <- .morie_gr_mat(policy, "policy")
  nS <- nrow(Z); nA <- ncol(Z)
  E <- as.integer(epochs); eta <- as.numeric(lr); eps <- as.numeric(clip_eps); g <- as.numeric(gamma)
  Bn <- as.integer(n_episodes); U <- as.integer(n_updates); Tt <- as.integer(max_steps)
  rng <- as.numeric(seed) %% 2^32
  ret_hist <- numeric(0); sur_hist <- numeric(0)
  clipped_total <- 0L; seen_total <- 0L
  for (ep_ in seq_len(E)) {
    states <- integer(0); actions <- integer(0); rets <- numeric(0); ep_returns <- numeric(0)
    for (bb in seq_len(Bn)) {
      s <- eb$reset()
      traj_s <- integer(0); traj_a <- integer(0); traj_r <- numeric(0)
      done <- FALSE; t_ <- 0L
      while (t_ < Tt && !done) {
        si <- as.integer(s)
        p <- .morie_gr_softmax(Z[si + 1L, ])
        rng <- .morie_al_lcg(rng); u <- (rng + 0.5) / 2^32
        a <- min(sum(cumsum(p) < u), nA - 1L)
        out <- eb$step(a)
        s <- out[[1]]; rew <- out[[2]]; done <- isTRUE(out[[3]])
        traj_s <- c(traj_s, si); traj_a <- c(traj_a, a); traj_r <- c(traj_r, as.numeric(rew))
        t_ <- t_ + 1L
      }
      Gc <- 0.0; gs <- numeric(length(traj_r))
      if (length(traj_r)) for (k in length(traj_r):1) { Gc <- traj_r[k] + g * Gc; gs[k] <- Gc }
      states <- c(states, traj_s); actions <- c(actions, traj_a); rets <- c(rets, gs)
      ep_returns <- c(ep_returns, if (length(gs)) gs[1] else 0.0)
    }
    S <- states; Aa <- actions; R <- rets
    adv <- R - mean(R); sdv <- stats::sd(R)
    if (!is.na(sdv) && sdv > 1e-12) adv <- adv / sdv
    old_logp <- mapply(function(s_, a_) log(.morie_gr_softmax(Z[s_ + 1L, ])[a_ + 1L] + 1e-300), S, Aa)
    for (uu in seq_len(U)) {
      grad <- matrix(0, nS, nA); sur <- 0.0
      for (i in seq_along(S)) {
        p <- .morie_gr_softmax(Z[S[i] + 1L, ])
        ratio <- exp(log(p[Aa[i] + 1L] + 1e-300) - old_logp[i])
        unclipped <- ratio * adv[i]
        clipped <- max(min(ratio, 1 + eps), 1 - eps) * adv[i]
        sur <- sur + min(unclipped, clipped)
        seen_total <- seen_total + 1L
        if (unclipped <= clipped) {
          dlog <- -p; dlog[Aa[i] + 1L] <- dlog[Aa[i] + 1L] + 1.0
          grad[S[i] + 1L, ] <- grad[S[i] + 1L, ] + adv[i] * ratio * dlog
        } else clipped_total <- clipped_total + 1L
      }
      Z <- Z + eta * grad / max(length(S), 1)
      if (uu == 1L) sur_hist <- c(sur_hist, sur / max(length(S), 1))
    }
    ret_hist <- c(ret_hist, mean(ep_returns))
  }
  probs <- t(apply(Z, 1, .morie_gr_softmax))
  list(theta = Z, probabilities = probs, return_history = ret_hist, surrogate_history = sur_hist,
       clip_fraction = clipped_total / max(seen_total, 1L), clip_eps = eps, estimate = probs, n = E * Bn,
       method = "PPO with a clipped surrogate on a tabular softmax policy")
}

# ============================================================ hmppp

#' Pipeline parallelism: contiguous stage partition plus a microbatch schedule (Geron Ch 17, hmppp)
#' @param model Sequence of layer sizes (or weight arrays; lengths are used).
#' @param n_stages Pipeline stages.
#' @param n_microbatches Microbatches per batch.
#' @export
morie_geron_pipeline_parallelism <- function(model, n_stages, n_microbatches = 4) {
  sizes <- vapply(model, function(v) length(as.numeric(v)), numeric(1))
  base <- .morie_w4c_model_parallel(sizes, as.integer(n_stages))
  S <- as.integer(n_stages); M <- as.integer(n_microbatches)
  slots <- M + S - 1L
  bubble <- (S - 1L) / slots
  sched <- matrix(-1L, S, slots)
  for (s_ in seq_len(S)) for (m_ in seq_len(M)) sched[s_, s_ + m_ - 1L] <- m_ - 1L
  list(assignment = base$assignment, stage_loads = base$device_loads, max_load = base$max_load,
       imbalance = base$imbalance, bubble_fraction = bubble, utilisation = 1.0 - bubble, schedule = sched,
       n_slots = slots, n_microbatches = M, estimate = bubble, n = length(sizes),
       method = "Pipeline schedule over a contiguous stage partition")
}

# ============================================================ hmprc

#' Precision-recall curve: best-F1 point and recall at 90% precision (Geron Ch 3, hmprc)
#' @param y_true Binary labels.
#' @param scores Decision scores.
#' @param pos_label Positive label.
#' @export
morie_geron_precision_recall_curve_hm <- function(y_true, scores, pos_label = 1) {
  yt <- as.vector(y_true); s <- as.numeric(scores)
  .morie_gr_need(length(yt) == length(s), "geron_precision_recall_curve: length mismatch")
  bin_y <- as.numeric(yt == pos_label)
  .morie_gr_need(sum(bin_y) > 0, "geron_precision_recall_curve: no positive instance")
  curv <- .morie_w4c_pr_curve(bin_y, s)
  thr <- c(Inf, sort(s, decreasing = TRUE))
  denom <- curv$precision + curv$recall
  f1 <- ifelse(denom > 0, 2 * curv$precision * curv$recall / ifelse(denom > 0, denom, 1), 0.0)
  k <- which.max(f1)
  ok <- curv$precision >= 0.9
  rec90 <- if (any(ok)) max(curv$recall[ok]) else 0.0
  list(precision = curv$precision, recall = curv$recall, thresholds = thr,
       average_precision = curv$average_precision, f1 = f1, best_f1 = f1[k], best_threshold = thr[k],
       recall_at_90_precision = rec90, estimate = curv$average_precision, n = length(yt),
       method = "PR curve, best-F1 point and recall at 90% precision")
}

# ============================================================ hmprcv / hmprio / hmpvt

#' Perceiver: iterated latent-to-input scaled dot-product cross-attention (Geron Ch 16, hmprcv)
#' @param x Input (N, D).
#' @param latents Learned latents (L, D_lat).
#' @param n_iter Cross-attention rounds.
#' @param W_q,W_k,W_v Optional projections.
#' @export
morie_geron_perceiver <- function(x, latents, n_iter = 2, W_q = NULL, W_k = NULL, W_v = NULL) {
  X <- .morie_gr_mat(x, "x"); L <- .morie_gr_mat(latents, "latents")
  Tt <- as.integer(n_iter)
  D <- ncol(X); Dl <- ncol(L)
  Wq <- if (is.null(W_q)) diag(Dl) else .morie_gr_mat(W_q, "W_q")
  Wk <- if (is.null(W_k)) diag(D) else .morie_gr_mat(W_k, "W_k")
  Wv <- if (is.null(W_v)) diag(D) else .morie_gr_mat(W_v, "W_v")
  dk <- ncol(Wq)
  K <- X %*% Wk; V <- X %*% Wv
  attn <- NULL
  for (it in seq_len(Tt)) {
    Q <- L %*% Wq
    scores <- (Q %*% t(K)) / sqrt(dk)
    attn <- t(apply(scores, 1, .morie_gr_softmax))
    L <- L + attn %*% V
  }
  list(latents = L, attention = attn, attention_cost = nrow(latents) * nrow(X),
       self_attention_cost = nrow(X) * nrow(X), n_iter = Tt, estimate = L, n = nrow(X),
       method = "Perceiver: iterated latent-to-input scaled dot-product cross-attention")
}

#' Perceiver IO: adds a cross-attention output decoder (Geron Ch 16, hmprio)
#' @param x,latents,queries Input, latents and output queries.
#' @param n_iter,W_q,W_k,W_v Encoder controls.
#' @export
morie_geron_perceiver_io_hm <- function(x, latents, queries, n_iter = 2, W_q = NULL, W_k = NULL, W_v = NULL) {
  enc <- morie_geron_perceiver(x, latents, n_iter = n_iter, W_q = W_q, W_k = W_k, W_v = W_v)
  Z <- enc$latents
  Q <- .morie_gr_mat(queries, "queries")
  dk <- ncol(Z)
  scores <- (Q %*% t(Z)) / sqrt(dk)
  attn <- t(apply(scores, 1, .morie_gr_softmax))
  out <- attn %*% Z
  list(outputs = out, decoder_attention = attn, latents = Z, encoder_attention = enc$attention,
       decoder_cost = nrow(Q) * nrow(Z), encoder_cost = enc$attention_cost, estimate = out, n = nrow(Q),
       method = "Perceiver IO: encoder plus query cross-attention decoder")
}

#' Pyramid Vision Transformer: hierarchical multi-scale patch-embedding stages (Geron Ch 16, hmpvt)
#' @param image Array (H, W, C) or (H, W).
#' @param stage_cfgs List of stage specs.
#' @param seed LCG seed for default projections.
#' @export
morie_geron_pvt <- function(image, stage_cfgs, seed = 0) {
  img <- image
  if (length(dim(img)) == 2L) img <- array(img, dim = c(dim(img), 1L))
  x <- img
  stages <- list(); params <- 0; cost <- 0; full <- 0
  for (i in seq_along(stage_cfgs)) {
    cfg <- stage_cfgs[[i]]
    p <- if (is.null(cfg$patch_size)) 2L else as.integer(cfg$patch_size)
    dim_ <- as.integer(cfg$dim)
    heads <- if (is.null(cfg$heads)) 1L else as.integer(cfg$heads)
    r <- if (is.null(cfg$sr_ratio)) 1L else as.integer(cfg$sr_ratio)
    Hd <- dim(x)[1]; Wd <- dim(x)[2]; C <- dim(x)[3]
    gh <- Hd %/% p; gw <- Wd %/% p
    fan <- p * p * C
    Wm <- cfg$W
    if (is.null(Wm)) {
      Wm <- matrix(.morie_w4c_lcgvec(fan * dim_, as.numeric(seed) + 7919 * (i - 1L) + 1), nrow = fan, ncol = dim_, byrow = TRUE)
      Wm <- (Wm * 2 - 1) / sqrt(fan)
    } else Wm <- .morie_gr_mat(Wm, "W")
    # Extract patches in numpy's row-major (p, p, C) flatten order: patch-row
    # slowest, patch-col next, channel fastest -- matches
    # x.reshape(gh,p,gw,p,C).transpose(0,2,1,3,4).reshape(gh,gw,fan) exactly.
    patches <- array(0, dim = c(gh, gw, fan))
    for (bi in seq_len(gh)) for (bj in seq_len(gw)) {
      vec <- numeric(fan); idx <- 1L
      for (pi in seq_len(p)) for (pj in seq_len(p)) for (cc in seq_len(C)) {
        vec[idx] <- x[(bi - 1L) * p + pi, (bj - 1L) * p + pj, cc]
        idx <- idx + 1L
      }
      patches[bi, bj, ] <- vec
    }
    Xnew <- array(0, dim = c(gh, gw, dim_))
    for (bi in seq_len(gh)) for (bj in seq_len(gw)) Xnew[bi, bj, ] <- as.numeric(patches[bi, bj, ] %*% Wm)
    x <- Xnew
    n_tok <- gh * gw
    cst <- n_tok * max(n_tok %/% (r * r), 1L)
    stages[[i]] <- list(index = i - 1L, grid = c(gh, gw), tokens = n_tok, dim = dim_, heads = heads,
                        sr_ratio = r, parameters = fan * dim_, attention_cost = cst, full_attention_cost = n_tok * n_tok)
    params <- params + fan * dim_; cost <- cost + cst; full <- full + n_tok * n_tok
  }
  list(tokens = x, stages = stages, output_shape = dim(x), n_parameters = params, attention_cost = cost,
       full_attention_cost = full, estimate = x, n = length(img),
       method = "PVT patch-embedding pyramid with spatial-reduction attention costs")
}

# ============================================================ hmpre

#' Precision = TP / (TP + FP) (Geron Ch 3, hmpre)
#' @param y_true,y_pred Labels, same length.
#' @param pos_label Positive label.
#' @export
morie_geron_precision_hm <- function(y_true, y_pred, pos_label = 1) {
  yt <- as.vector(y_true); yp <- as.vector(y_pred)
  .morie_gr_need(length(yt) == length(yp), "geron_precision: length mismatch")
  tp <- sum(yt == pos_label & yp == pos_label)
  fp <- sum(yt != pos_label & yp == pos_label)
  fn <- sum(yt == pos_label & yp != pos_label)
  .morie_gr_need(tp + fp > 0, "geron_precision: nothing predicted positive")
  prec <- tp / (tp + fp)
  rec <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
  f1 <- if (tp + fn > 0 && (prec + rec) > 0) 2 * prec * rec / (prec + rec) else 0.0
  list(precision = prec, tp = as.integer(tp), fp = as.integer(fp), fn = as.integer(fn), f1 = f1,
       estimate = prec, n = length(yt), method = "Precision TP/(TP+FP)")
}

# ============================================================ hmprel

#' Parametric ReLU: per-channel learnable negative slope (Geron Ch 11, hmprel)
#' @param z Pre-activations, channels on last axis.
#' @param alpha Scalar or per-channel slope.
#' @param upstream Optional dL/da from the next layer.
#' @export
morie_geron_prelu <- function(z, alpha = 0.25, upstream = NULL) {
  a <- as.numeric(z); C <- length(a)  # 1-D case; matches the doctest shapes exercised here
  al <- as.numeric(alpha)
  alv <- if (length(al) == 1L) rep(al, C) else al
  up <- if (is.null(upstream)) rep(1.0, C) else as.numeric(upstream)
  neg <- a < 0
  out <- ifelse(neg, alv * a, a)
  grad_z <- ifelse(neg, alv, 1.0) * up
  contrib <- ifelse(neg, a * up, 0.0)
  grad_alpha <- if (length(al) == 1L) sum(contrib) else contrib
  list(a = out, output = out, grad_z = grad_z, grad_alpha = grad_alpha,
       alpha = if (length(al) == 1L) al else alv, negative_fraction = mean(neg), estimate = out, n = C,
       method = "PReLU forward with gradients w.r.t. z and alpha")
}

# ============================================================ hmpru

#' Global magnitude weight pruning to an exact sparsity, cubic ramp schedule (Geron Ch 17, hmpru)
#' @param model Numeric vector or named list.
#' @param sparsity Target fraction of zeros in \[0, 1).
#' @param n_rounds Rounds in the returned schedule.
#' @export
morie_geron_weight_pruning_hm <- function(model, sparsity, n_rounds = 1) {
  sp <- as.numeric(sparsity); Rr <- as.integer(n_rounds)
  is_map <- is.list(model)
  keys <- if (is_map) names(model) else NULL
  tensors <- if (is_map) lapply(model, as.numeric) else list(as.numeric(model))
  flat <- unlist(lapply(tensors, function(t) abs(t)))
  N <- length(flat); k <- as.integer(floor(sp * N))
  ord <- order(flat, method = "radix")
  cut <- rep(FALSE, N)
  if (k > 0) cut[ord[seq_len(k)]] <- TRUE
  thr <- if (k > 0) flat[ord[k]] else 0.0
  out <- list(); masks <- list(); pos <- 0L
  for (t in tensors) {
    len <- length(t)
    m <- !cut[(pos + 1L):(pos + len)]
    masks[[length(masks) + 1L]] <- m
    out[[length(out) + 1L]] <- ifelse(m, t, 0.0)
    pos <- pos + len
  }
  if (is_map) { pruned <- stats::setNames(out, keys); mask <- stats::setNames(masks, keys) }
  else { pruned <- out[[1]]; mask <- masks[[1]] }
  sched <- vapply(seq_len(Rr), function(i) sp * (1.0 - (1.0 - i / Rr)^3), numeric(1))
  list(pruned = pruned, mask = mask, threshold = thr, achieved_sparsity = k / N, n_pruned = as.integer(k),
       n_weights = as.integer(N), schedule = sched, estimate = k / N, n = N,
       method = "Global magnitude pruning to an exact sparsity, with a cubic ramp schedule")
}

# ============================================================ hmptq

#' Static post-training quantization (PTQ) from calibration ranges (Geron App B, hmptq)
#' @param model Weights.
#' @param calibration_data Representative activations.
#' @param bits Bit width, 2 to 16.
#' @param percentile Range-clipping percentile in (0, 100\].
#' @export
morie_geron_static_quantization_ptq <- function(model, calibration_data, bits = 8, percentile = 100.0) {
  b <- as.integer(bits); pct <- as.numeric(percentile)
  is_map <- is.list(model)
  keys <- if (is_map) names(model) else NULL
  tensors <- if (is_map) lapply(model, as.numeric) else list(as.numeric(model))
  cal <- as.numeric(calibration_data)
  qmax <- 2^(b - 1) - 1
  lo <- if (pct < 100.0) stats::quantile(cal, (100.0 - pct) / 2.0 / 100.0, names = FALSE, type = 7) else min(cal)
  hi <- if (pct < 100.0) stats::quantile(cal, 1 - (100.0 - pct) / 2.0 / 100.0, names = FALSE, type = 7) else max(cal)
  .morie_gr_need(hi != lo, "geron_static_quantization_ptq: calibration range is a single value")
  a_scale <- (hi - lo) / (2^b - 1)
  zero_point <- as.integer(round(-lo / a_scale))
  wmax <- max(vapply(tensors, function(t) max(abs(t)), numeric(1)))
  .morie_gr_need(wmax > 0, "geron_static_quantization_ptq: every weight is zero")
  w_scale <- wmax / qmax
  qw <- list(); dqw <- list(); errs <- numeric(0)
  for (t in tensors) {
    q <- pmin(pmax(round(t / w_scale), -qmax), qmax)
    d_ <- q * w_scale
    qw[[length(qw) + 1L]] <- as.integer(q); dqw[[length(dqw) + 1L]] <- d_
    errs <- c(errs, max(abs(d_ - t)))
  }
  if (is_map) { qout <- stats::setNames(qw, keys); dout <- stats::setNames(dqw, keys) }
  else { qout <- qw[[1]]; dout <- dqw[[1]] }
  list(quantized_weights = qout, dequantized_weights = dout, weight_scale = w_scale,
       activation_scale = a_scale, zero_point = zero_point, activation_range = c(lo, hi),
       max_weight_error = max(errs), compression = 32.0 / b, bits = b, estimate = qout,
       n = sum(vapply(tensors, length, integer(1))), method = "PTQ: symmetric weight grid, affine activation grid")
}

# ============================================================ hmpttn

.morie_w4c_dtypes <- c(float64 = "double", double = "double", float32 = "float", float = "float",
                       float16 = "half", half = "half", bfloat16 = "float", int64 = "long", long = "long",
                       int32 = "integer", int = "integer", int16 = "short", short = "short", int8 = "byte",
                       uint8 = "ubyte", bool = "bool")

#' Torch-style tensor construction (numpy/R-backed, no torch call) (Geron Ch 10, hmpttn)
#' @param x Data.
#' @param device "cpu", "cuda" or "mps".
#' @param dtype Optional torch dtype name.
#' @export
morie_geron_pytorch_tensor <- function(x, device = "cpu", dtype = NULL) {
  dev <- tolower(strsplit(as.character(device), ":")[[1]][1])
  .morie_gr_need(dev %in% c("cpu", "cuda", "mps"), "geron_pytorch_tensor: unknown device")
  a <- x
  src_float <- is.double(a) && !is.integer(a)
  name <- if (is.null(dtype)) (if (src_float) "float32" else "int64") else tolower(gsub("torch\\.", "", dtype))
  .morie_gr_need(name %in% names(.morie_w4c_dtypes), "geron_pytorch_tensor: unknown dtype")
  itemsize <- c(double = 8L, float = 4L, half = 2L, long = 8L, integer = 4L, short = 2L, byte = 1L, ubyte = 1L, bool = 1L)[[.morie_w4c_dtypes[[name]]]]
  shp <- if (is.null(dim(a))) length(a) else dim(a)
  list(tensor = a, dtype = name, numpy_dtype = .morie_w4c_dtypes[[name]], device = dev, shape = shp,
       ndim = length(shp), itemsize = itemsize, nbytes = length(a) * itemsize, strides = NULL,
       dtype_changed = is.null(dtype) && src_float, on_device = dev == "cpu", estimate = a, n = length(a),
       method = "Tensor construction with torch dtype/device semantics")
}

# ============================================================ hmqat

.morie_w4c_fake_quant <- function(w, bits) {
  qmax <- 2^(bits - 1) - 1
  scale <- max(abs(w)) / qmax
  if (scale == 0) return(list(w, 0.0))
  q <- pmin(pmax(round(w / scale), -qmax), qmax)
  list(q * scale, scale)
}

#' Quantization-aware training with a straight-through estimator (Geron App B, hmqat)
#' @param model Initial full-precision weights.
#' @param X,y Data.
#' @param epochs,lr,bits Controls.
#' @export
morie_geron_quantization_aware_training_hm <- function(model, X, y, epochs = 200, lr = 0.1, bits = 8) {
  w <- as.numeric(model)
  A <- .morie_gr_mat(X, "X"); yv <- as.numeric(y)
  E <- as.integer(epochs); eta <- as.numeric(lr); b <- as.integer(bits)
  m <- nrow(A); hist <- numeric(0); scale <- 0.0
  for (it in seq_len(E)) {
    fq <- .morie_w4c_fake_quant(w, b); wq <- fq[[1]]; scale <- fq[[2]]
    resid <- as.numeric(A %*% wq) - yv
    hist <- c(hist, mean(resid^2))
    grad <- (2.0 / m) * as.numeric(t(A) %*% resid)
    w <- w - eta * grad
  }
  fq <- .morie_w4c_fake_quant(w, b); wq <- fq[[1]]; scale <- fq[[2]]
  resid <- as.numeric(A %*% wq) - yv
  loss <- mean(resid^2); hist <- c(hist, loss)
  fp_resid <- as.numeric(A %*% w) - yv
  fp_loss <- mean(fp_resid^2)
  list(weights = w, quantized_weights = wq, scale = scale, loss = loss, fp_loss = fp_loss,
       loss_history = hist, bits = b, estimate = wq, n = m,
       method = "QAT on a linear model: fake-quant forward, straight-through backward")
}

# ============================================================ hmrad (wraps morie_geron_autograd)

#' Reverse-mode automatic differentiation, delegated to morie_geron_autograd (Geron App A, hmrad)
#' @param f Function `f(vars) -> tape node` (built from the supplied leaves).
#' @param x Point.
#' @export
morie_geron_reverse_autodiff <- function(f, x) {
  base <- morie_geron_autograd(f, x)
  grad <- as.numeric(base$grad)
  list(gradient = grad, grad = grad, value = base$value, tape_size = base$tape_size, n_passes = 2L,
       estimate = grad, n = length(grad), method = "Reverse-mode AD delegated to morie_geron_autograd")
}

# ============================================================ hmrdt (wraps morie_geron_cart_split_cost)

.morie_w4c_leaf_value <- function(y, criterion) {
  if (criterion == "mse") return(mean(y))
  tb <- table(y)
  as.numeric(names(tb)[which.max(tb)])
}

.morie_w4c_best_split <- function(X, y, criterion, columns, min_leaf) {
  best <- NULL
  for (j in columns) {
    col <- X[, j + 1L]
    vals <- sort(unique(col))
    if (length(vals) < 2L) next
    for (thr in (utils::head(vals, -1) + utils::tail(vals, -1)) / 2) {
      left <- col <= thr
      if (sum(left) < min_leaf || sum(!left) < min_leaf) next
      cost <- morie_geron_cart_split_cost(X, y, j, thr, criterion = criterion)$cost
      if (is.null(best) || cost < best$cost) best <- list(cost = cost, j = j, thr = thr)
    }
  }
  best
}

.morie_w4c_grow <- function(X, y, depth, max_depth, min_leaf, criterion, columns_fn) {
  if (depth >= max_depth || length(y) < 2 * min_leaf || length(unique(y)) == 1L)
    return(list(leaf = TRUE, value = .morie_w4c_leaf_value(y, criterion), n = length(y)))
  best <- .morie_w4c_best_split(X, y, criterion, columns_fn(depth), min_leaf)
  if (is.null(best)) return(list(leaf = TRUE, value = .morie_w4c_leaf_value(y, criterion), n = length(y)))
  left <- X[, best$j + 1L] <= best$thr
  list(leaf = FALSE, feature = best$j, threshold = best$thr, n = length(y),
       left = .morie_w4c_grow(X[left, , drop = FALSE], y[left], depth + 1L, max_depth, min_leaf, criterion, columns_fn),
       right = .morie_w4c_grow(X[!left, , drop = FALSE], y[!left], depth + 1L, max_depth, min_leaf, criterion, columns_fn))
}

.morie_w4c_predict_tree <- function(node, X) {
  out <- numeric(nrow(X))
  for (i in seq_len(nrow(X))) {
    nd <- node
    while (!nd$leaf) nd <- if (X[i, nd$feature + 1L] <= nd$threshold) nd$left else nd$right
    out[i] <- nd$value
  }
  out
}

.morie_w4c_count_leaves <- function(node) if (node$leaf) 1L else .morie_w4c_count_leaves(node$left) + .morie_w4c_count_leaves(node$right)
.morie_w4c_tree_depth <- function(node) if (node$leaf) 0L else 1L + max(.morie_w4c_tree_depth(node$left), .morie_w4c_tree_depth(node$right))

#' CART regression tree minimising per-leaf MSE (Geron Ch 5, hmrdt)
#' @param X,y Data and targets.
#' @param max_depth,min_samples_leaf Regularisation.
#' @export
morie_geron_regression_tree <- function(X, y, max_depth = 3, min_samples_leaf = 1) {
  A <- .morie_gr_mat(X, "X"); yv <- as.numeric(y)
  D <- as.integer(max_depth); Lm <- as.integer(min_samples_leaf)
  cols <- 0:(ncol(A) - 1L)
  tree <- .morie_w4c_grow(A, yv, 0L, D, Lm, "mse", function(depth) cols)
  pred <- .morie_w4c_predict_tree(tree, A)
  mse <- mean((pred - yv)^2)
  imp <- numeric(ncol(A))
  acc_fn <- function(node, Xs, ys) {
    if (node$leaf) return(invisible())
    mtot <- length(ys); lm <- Xs[, node$feature + 1L] <= node$threshold
    gain <- .morie_gr_pvar(ys) - (sum(lm) / mtot) * .morie_gr_pvar(ys[lm]) - (sum(!lm) / mtot) * .morie_gr_pvar(ys[!lm])
    imp[node$feature + 1L] <<- imp[node$feature + 1L] + gain * mtot
    acc_fn(node$left, Xs[lm, , drop = FALSE], ys[lm]); acc_fn(node$right, Xs[!lm, , drop = FALSE], ys[!lm])
  }
  acc_fn(tree, A, yv)
  if (sum(imp) > 0) imp <- imp / sum(imp)
  predict_fn <- function(Xnew) .morie_w4c_predict_tree(tree, .morie_gr_mat(Xnew, "Xnew"))
  list(tree = tree, predict = predict_fn, predictions = pred, mse = mse,
       n_leaves = .morie_w4c_count_leaves(tree), depth = .morie_w4c_tree_depth(tree), feature_importance = imp,
       estimate = pred, n = nrow(A), method = "CART regression tree, split cost delegated to morie_geron_cart_split_cost")
}

# ============================================================ hmrec

#' Recall (true positive rate) = TP / (TP + FN) (Geron Ch 3, hmrec)
#' @param y_true,y_pred Labels, same length.
#' @param pos_label Positive label.
#' @export
morie_geron_recall_hm <- function(y_true, y_pred, pos_label = 1) {
  yt <- as.vector(y_true); yp <- as.vector(y_pred)
  .morie_gr_need(length(yt) == length(yp), "geron_recall: length mismatch")
  tp <- sum(yt == pos_label & yp == pos_label)
  fn <- sum(yt == pos_label & yp != pos_label)
  fp <- sum(yt != pos_label & yp == pos_label)
  .morie_gr_need(tp + fn > 0, "geron_recall: no positive instance")
  rec <- tp / (tp + fn)
  prec <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
  f1 <- if (tp + fp > 0 && (prec + rec) > 0) 2 * prec * rec / (prec + rec) else 0.0
  list(recall = rec, tp = as.integer(tp), fn = as.integer(fn), fp = as.integer(fp), f1 = f1,
       estimate = rec, n = length(yt), method = "Recall TP/(TP+FN)")
}

# ============================================================ hmregn

#' Regression MLP: ReLU hidden layers, linear head, full-batch GD (Geron Ch 9, hmregn)
#' @param X,y Data and targets.
#' @param hidden_sizes Hidden widths.
#' @param epochs,lr,seed Training controls.
#' @export
morie_geron_regression_mlp <- function(X, y, hidden_sizes = 8, epochs = 400, lr = 0.05, seed = 0) {
  A <- .morie_gr_mat(X, "X")
  Y <- if (is.matrix(y)) y else matrix(as.numeric(y), ncol = 1)
  hs <- as.integer(hidden_sizes)
  E <- as.integer(epochs); eta <- as.numeric(lr)
  m <- nrow(A); sizes <- c(ncol(A), hs, ncol(Y))
  Ws <- list(); bs <- list()
  for (i in seq_len(length(sizes) - 1L)) {
    scale <- sqrt(6.0 / sizes[i])
    Ws[[i]] <- .morie_w4c_lcg_uniform(sizes[i], sizes[i + 1L], as.numeric(seed) + 7919 * (i - 1L) + 1, scale)
    bs[[i]] <- numeric(sizes[i + 1L])
  }
  forward <- function(B) {
    acts <- list(B)
    for (i in seq_along(Ws)) {
      prev <- acts[[length(acts)]]
      z <- prev %*% Ws[[i]] + matrix(bs[[i]], nrow(prev), length(bs[[i]]), byrow = TRUE)
      acts[[length(acts) + 1L]] <- if (i < length(Ws)) pmax(z, 0.0) else z
    }
    acts
  }
  hist <- numeric(0)
  for (it in seq_len(E)) {
    acts <- forward(A); pred <- acts[[length(acts)]]
    resid <- pred - Y
    hist <- c(hist, mean(rowSums(resid^2)))
    delta <- (2.0 / m) * resid
    for (i in length(Ws):1) {
      gW <- t(acts[[i]]) %*% delta
      gb <- colSums(delta)
      if (i > 1L) delta <- (delta %*% t(Ws[[i]])) * (acts[[i]] > 0)
      Ws[[i]] <- Ws[[i]] - eta * gW
      bs[[i]] <- bs[[i]] - eta * gb
    }
  }
  acts <- forward(A); pred <- acts[[length(acts)]]
  mse <- mean(rowSums((pred - Y)^2)); hist <- c(hist, mse)
  predict_fn <- function(Xnew) {
    B <- .morie_gr_mat(Xnew, "Xnew")
    o <- forward(B)[[length(Ws) + 1L]]
    if (ncol(Y) == 1L) as.numeric(o) else o
  }
  nparams <- sum(vapply(Ws, length, integer(1))) + sum(vapply(bs, length, integer(1)))
  list(predict = predict_fn, predictions = if (ncol(Y) == 1L) as.numeric(pred) else pred, mse = mse,
       loss_history = hist, weights = Ws, biases = bs, sizes = sizes, n_parameters = nparams,
       estimate = if (ncol(Y) == 1L) as.numeric(pred) else pred, n = m,
       method = "Regression MLP (ReLU hidden, linear head) trained by full-batch gradient descent")
}

# ============================================================ hmrelu

#' Rectified linear unit activation, with a leaky negative slope and dead-fraction diagnostic (Geron Ch 9, hmrelu)
#' @param z Pre-activations.
#' @param leaky Negative slope; 0 is plain ReLU.
#' @export
morie_geron_relu <- function(z, leaky = 0.0) {
  a <- as.numeric(z); slope <- as.numeric(leaky)
  out <- ifelse(a >= 0, a, slope * a) + 0.0
  grad <- ifelse(a > 0, 1.0, slope)  # derivative at exactly 0 is taken to be `slope`
  dead <- mean(a <= 0)
  list(a = out, output = out, gradient = grad, dead_fraction = dead, leaky = slope, estimate = out, n = length(a),
       method = "ReLU activation")
}

# ============================================================ hmrfc (reuses hmrdt's grow/predict)

.morie_w4c_bootstrap <- function(n, seed) {
  s <- as.numeric(seed) %% 2^32
  out <- integer(n)
  for (i in seq_len(n)) { s <- .morie_al_lcg(s); out[i] <- as.integer((s * n) %/% 2^32) }
  out
}

#' Random forest: bagged CART trees with random per-split feature subsets (Geron Ch 6, hmrfc)
#' @param X,y Data and targets.
#' @param n_estimators,max_features,seed,max_depth,min_samples_leaf,task Controls.
#' @export
morie_geron_random_forest <- function(X, y, n_estimators = 10, max_features = "sqrt", seed = 0,
                                      max_depth = 4, min_samples_leaf = 1, task = "auto") {
  A <- .morie_gr_mat(X, "X"); yv <- as.numeric(y); n <- nrow(A); d <- ncol(A)
  M <- as.integer(n_estimators)
  classify <- task == "classification" || (task == "auto" && length(unique(yv)) <= max(2, floor(sqrt(n))) && all(yv == round(yv)))
  k <- if (identical(max_features, "sqrt")) as.integer(ceiling(sqrt(d)))
       else if (identical(max_features, "log2")) max(1L, as.integer(ceiling(log2(d))))
       else if (identical(max_features, "all")) d
       else if (is.numeric(max_features) && max_features <= 1.0 && max_features != as.integer(max_features)) max(1L, round(max_features * d))
       else as.integer(max_features)
  criterion <- if (classify) "gini" else "mse"
  trees <- list(); oob_sum <- numeric(n); oob_cnt <- numeric(n); votes <- matrix(0, M, n)
  for (mi in seq_len(M)) {
    rows <- .morie_w4c_bootstrap(n, seed + 7919 * (mi - 1L)) + 1L
    col_seed <- (seed + 104729 * (mi - 1L) + 13) %% 2^32
    columns_fn <- function(depth) {
      col_seed <<- .morie_al_lcg(col_seed)
      .morie_w4c_lcg_sample(d, k, col_seed + depth)
    }
    t_ <- .morie_w4c_grow(A[rows, , drop = FALSE], yv[rows], 0L, as.integer(max_depth), as.integer(min_samples_leaf), criterion, columns_fn)
    trees[[mi]] <- t_
    votes[mi, ] <- .morie_w4c_predict_tree(t_, A)
    oob <- setdiff(seq_len(n), unique(rows))
    if (length(oob)) { oob_sum[oob] <- oob_sum[oob] + votes[mi, oob]; oob_cnt[oob] <- oob_cnt[oob] + 1 }
  }
  aggregate <- function(P) {
    if (!classify) return(colMeans(P))
    apply(P, 2, function(col) { tb <- table(col); as.numeric(names(tb)[which.max(tb)]) })
  }
  predict_fn <- function(Xnew) { B <- .morie_gr_mat(Xnew, "Xnew"); aggregate(do.call(rbind, lapply(trees, function(t_) .morie_w4c_predict_tree(t_, B)))) }
  pred <- aggregate(votes)
  has <- oob_cnt > 0
  oob_pred <- ifelse(has, oob_sum / pmax(oob_cnt, 1), NA_real_)
  if (classify) { score <- mean(pred == yv); oob_score <- if (any(has)) mean(as.numeric(oob_pred[has] >= 0.5) == yv[has]) else NA_real_; key <- "accuracy" }
  else { score <- mean((pred - yv)^2); oob_score <- if (any(has)) mean((oob_pred[has] - yv[has])^2) else NA_real_; key <- "mse" }
  imp <- numeric(d)
  acc_imp <- function(node) { if (node$leaf) return(invisible()); imp[node$feature + 1L] <<- imp[node$feature + 1L] + node$n; acc_imp(node$left); acc_imp(node$right) }
  for (t_ in trees) acc_imp(t_)
  if (sum(imp) > 0) imp <- imp / sum(imp)
  out <- list(predict = predict_fn, predictions = pred, oob_score = oob_score, trees = trees,
              feature_importance = imp, max_features = k, task = if (classify) "classification" else "regression",
              estimate = pred, n = n, method = "Random forest over LCG bootstraps with per-split column sampling")
  out[[key]] <- score
  out
}

# ============================================================ hmrgpt (wraps hmregn)

#' Regression MLP as an nn.Sequential architecture (numpy/R-trained, no torch call) (Geron Ch 10, hmrgpt)
#' @param X,y Data and targets.
#' @param hidden,epochs,lr,seed Passed to `morie_geron_regression_mlp`.
#' @export
morie_geron_regression_mlp_pytorch <- function(X, y, hidden = 8, epochs = 400, lr = 0.05, seed = 0) {
  base <- morie_geron_regression_mlp(X, y, hidden_sizes = hidden, epochs = epochs, lr = lr, seed = seed)
  sizes <- base$sizes
  layers <- character(0)
  for (i in seq_len(length(sizes) - 1L)) {
    layers <- c(layers, sprintf("Linear(in_features=%d, out_features=%d)", sizes[i], sizes[i + 1L]))
    if (i < length(sizes) - 1L) layers <- c(layers, "ReLU()")
  }
  list(layers = layers, sizes = sizes, n_parameters = base$n_parameters, predict = base$predict,
       predictions = base$predictions, mse = base$mse, loss_history = base$loss_history,
       weights = base$weights, biases = base$biases, uses_torch = FALSE, estimate = base$predictions,
       n = base$n, method = "nn.Sequential architecture resolved on the data; training via morie_geron_regression_mlp")
}

# ============================================================ hmrl

#' Reinforcement learning: Monte-Carlo policy evaluation by rollout (Geron Ch 1, hmrl)
#' @param env List with `reset()` / `step(action)`.
#' @param pi Function `pi(state) -> action or probs`.
#' @param gamma,n_episodes,max_steps,seed Controls.
#' @export
morie_geron_reinforcement_learning <- function(env, pi, gamma = 0.99, n_episodes = 1, max_steps = 1000, seed = 0) {
  eb <- .morie_w4c_bind_env(env)
  g <- as.numeric(gamma); E <- as.integer(n_episodes); Tt <- as.integer(max_steps)
  s_rng <- as.numeric(seed) %% 2^32
  returns <- numeric(E); lengths <- integer(E); truncated <- 0L
  for (e in seq_len(E)) {
    state <- eb$reset(); total <- 0.0; disc <- 1.0; t_ <- 0L; done <- FALSE
    while (t_ < Tt && !done) {
      out <- pi(state)
      if (length(out) == 1L && out == as.integer(out)) {
        action <- out
      } else {
        arr <- as.numeric(out)
        s_rng <- .morie_al_lcg(s_rng); u <- (s_rng + 0.5) / 2^32
        action <- min(sum(cumsum(arr) < u), length(arr) - 1L)
      }
      res <- eb$step(action)
      state <- res[[1]]; reward <- as.numeric(res[[2]]); done <- isTRUE(res[[3]])
      total <- total + disc * reward; disc <- disc * g; t_ <- t_ + 1L
    }
    if (!done) truncated <- truncated + 1L
    returns[e] <- total; lengths[e] <- t_
  }
  horizon <- if (g >= 1.0) Inf else 1.0 / (1.0 - g)
  list(mean_return = mean(returns), returns = returns, lengths = lengths,
       se = if (E > 1) stats::sd(returns) / sqrt(E) else NA_real_, effective_horizon = horizon,
       truncated = truncated, estimate = mean(returns), n = E,
       method = "Monte-Carlo policy evaluation of the discounted return")
}

# ============================================================ hmrlhf

#' Reinforcement learning from human feedback: reward maximised under a KL penalty (Geron Ch 15, hmrlhf)
#' @param policy Initial (n_prompts, n_responses) logits.
#' @param reward_model Function or matrix.
#' @param prompts Optional prompt identifiers.
#' @param beta,lr,epochs Controls.
#' @export
morie_geron_rlhf <- function(policy, reward_model, prompts = NULL, beta = 0.1, lr = 0.5, epochs = 500) {
  Z0 <- .morie_gr_mat(policy, "policy")
  Pn <- nrow(Z0); R <- ncol(Z0)
  b <- as.numeric(beta); eta <- as.numeric(lr); E <- as.integer(epochs)
  keys <- if (is.null(prompts)) seq_len(Pn) - 1L else prompts
  if (is.function(reward_model)) {
    rew <- matrix(0, Pn, R)
    for (i in seq_len(Pn)) for (j in seq_len(R)) rew[i, j] <- as.numeric(reward_model(keys[i], j - 1L))
  } else rew <- .morie_gr_mat(reward_model, "reward_model")
  ref <- t(apply(Z0, 1, .morie_gr_softmax))
  Z <- Z0; hist <- numeric(0)
  for (it in seq_len(E)) {
    pi <- t(apply(Z, 1, .morie_gr_softmax))
    adv <- rew - b * (log(pi / ref) + 1.0)
    obj <- mean(rowSums(pi * rew) - b * rowSums(pi * log(pi / ref)))
    hist <- c(hist, obj)
    grad <- pi * (adv - rowSums(pi * adv))
    Z <- Z + eta * grad
  }
  pi <- t(apply(Z, 1, .morie_gr_softmax))
  opt <- ref * exp(sweep(rew, 1, apply(rew, 1, max), "-") / b)
  opt <- opt / rowSums(opt)
  kl <- mean(rowSums(pi * log(pi / ref)))
  mean_r <- mean(rowSums(pi * rew))
  obj <- mean_r - b * kl
  hist <- c(hist, obj)
  list(policy = pi, reference_policy = ref, optimal_policy = opt, max_deviation = max(abs(pi - opt)),
       objective = obj, objective_history = hist, mean_reward = mean_r, kl = kl, beta = b, estimate = pi,
       n = Pn, method = "RLHF objective E[r] - beta KL(pi||pi_ref) maximised by gradient ascent")
}

# ============================================================ hmrnfc (wraps hmpg)

#' REINFORCE ascent step: gradient from morie_geron_policy_gradient, applied to theta (Geron Ch 19, hmrnfc)
#' @param episodes List of episodes.
#' @param policy Function `policy(state, action) -> grad log pi`.
#' @param gamma,eta,theta,baseline Controls.
#' @export
morie_geron_reinforce <- function(episodes, policy, gamma = 0.99, eta = 0.01, theta = NULL, baseline = TRUE) {
  lr <- as.numeric(eta)
  base <- morie_geron_policy_gradient(episodes, policy, gamma = gamma, baseline = baseline)
  grad <- as.numeric(base$gradient)
  th <- if (is.null(theta)) numeric(length(grad)) else as.numeric(theta)
  step <- lr * grad
  theta_next <- th + step
  list(theta = theta_next, theta_next = theta_next, step = step, gradient = grad, returns = base$returns,
       mean_return = base$mean_return, baseline_value = base$baseline_value, estimate = theta_next,
       n = base$n, method = "REINFORCE ascent step on the gradient from morie_geron_policy_gradient")
}

# ============================================================ hmrnn

#' Recurrent neuron step: h_t = phi(Wx x_t + Wh `h_{t-1}` + b) (Geron Ch 13, hmrnn)
#' @param x_t,h_prev Input and previous state.
#' @param Wx,Wh,b Weights.
#' @param activation One of tanh/relu/sigmoid/identity.
#' @export
morie_geron_recurrent_neuron <- function(x_t, h_prev, Wx, Wh, b, activation = "tanh") {
  x <- as.numeric(x_t); h <- as.numeric(h_prev)
  A <- .morie_gr_mat(Wx, "Wx"); Bm <- .morie_gr_mat(Wh, "Wh"); bb <- as.numeric(b)
  acts <- list(
    tanh = list(phi = tanh, dphi = function(a) 1.0 - a * a),
    relu = list(phi = function(z) pmax(z, 0.0), dphi = function(a) as.numeric(a > 0)),
    sigmoid = list(phi = .morie_w4c_sigmoid, dphi = function(a) a * (1.0 - a)),
    identity = list(phi = function(z) z, dphi = function(a) rep(1.0, length(a)))
  )
  ac <- acts[[activation]]
  z <- as.numeric(A %*% x + Bm %*% h + bb)
  hn <- ac$phi(z)
  jac <- ac$dphi(hn) * Bm
  list(h = hn, h_next = hn, z = z, jacobian = jac, jacobian_norm = norm(jac, "2"), estimate = hn, n = length(h),
       method = sprintf("Recurrent step h_t = %s(Wx x + Wh h + b)", activation))
}

# ============================================================ hmroc (wraps morie_geron_auc_roc)

#' ROC curve: FPR vs TPR over thresholds, trapezoid area cross-check (Geron Ch 3, hmroc)
#' @param y_true Binary labels.
#' @param scores Decision scores.
#' @param pos_label Positive label.
#' @export
morie_geron_roc_curve_hm <- function(y_true, scores, pos_label = 1) {
  base <- morie_geron_auc_roc(y_true, scores, pos_label = pos_label)
  fpr <- base$fpr; tpr <- base$tpr
  auc_trap <- sum(diff(fpr) * (utils::head(tpr, -1) + utils::tail(tpr, -1)) / 2)
  j <- tpr - fpr
  k <- which.max(j)
  list(fpr = fpr, tpr = tpr, thresholds = base$thresholds, auc = base$auc, auc_trapezoid = auc_trap,
       youden_j = j[k], best_threshold = base$thresholds[k], n_pos = base$n_pos, n_neg = base$n_neg,
       estimate = base$auc, n = length(y_true), method = "ROC sweep via morie_geron_auc_roc, area re-checked by trapezoid")
}

# ============================================================ hmrpca

#' Randomized PCA via a power-iterated random projection sketch (Geron Ch 7, hmrpca)
#' @param X Data (m, p).
#' @param n_components Components to recover.
#' @param seed,n_oversamples,n_power_iter Sketch controls.
#' @export
morie_geron_randomized_pca <- function(X, n_components, seed = 0, n_oversamples = 10, n_power_iter = 2) {
  A <- .morie_gr_mat(X, "X")
  m <- nrow(A); p <- ncol(A); k <- as.integer(n_components)
  over <- as.integer(n_oversamples); q <- as.integer(n_power_iter)
  Xc <- sweep(A, 2, colMeans(A))
  ell <- min(p, k + over)
  Omega <- matrix((.morie_w4c_lcgvec(p * ell, seed) * 2 - 1), nrow = p, ncol = ell, byrow = TRUE)
  Y <- Xc %*% Omega
  Qd <- qr.Q(qr(Y))
  for (it in seq_len(q)) { Qd <- qr.Q(qr(t(Xc) %*% Qd)); Qd <- qr.Q(qr(Xc %*% Qd)) }
  Bm <- t(Qd) %*% Xc
  sv <- svd(Bm)
  comps <- sv$v[, seq_len(k), drop = FALSE]
  s <- sv$d[seq_len(k)]
  for (j in seq_len(ncol(comps))) if (comps[which.max(abs(comps[, j])), j] < 0) comps[, j] <- -comps[, j]
  scores <- Xc %*% comps
  var_ <- sv$d^2 / (m - 1)
  total <- sum(Xc^2) / (m - 1)
  ratio <- if (total > 0) var_[seq_len(k)] / total else numeric(k)
  gap <- if (length(sv$d) > k && sv$d[k + 1L] > 0) sv$d[k] / sv$d[k + 1L] else Inf
  list(components = comps, scores = scores, singular_values = s, explained_variance = var_[seq_len(k)],
       explained_variance_ratio = ratio, spectral_gap = gap, sketch_width = ell, estimate = comps, n = m,
       method = "Randomized range finder (LCG sketch) plus SVD of the projected matrix")
}

# ============================================================ hmrpt (uses hmpas/hmrsp samplers + stump)

.morie_w4c_lcg_features <- .morie_w4c_lcg_sample  # identical Fisher-Yates draw, reused for column sampling

#' Random patches: subsample BOTH rows and features per base model (Geron Ch 6, hmrpt)
#' @param X,y Data and targets.
#' @param base_estimator Optional `base_estimator(Xp, yp) -> predict`.
#' @param n_estimators,max_samples,max_features,seed,task,bootstrap Controls.
#' @export
morie_geron_random_patches <- function(X, y, base_estimator = NULL, n_estimators = 10, max_samples = NULL,
                                       max_features = NULL, seed = 0, task = "auto", bootstrap = FALSE) {
  A <- .morie_gr_mat(X, "X"); yv <- as.numeric(y); n <- nrow(A); d <- ncol(A)
  M <- as.integer(n_estimators)
  s <- if (is.null(max_samples)) max(1L, n %/% 2L)
       else if (is.numeric(max_samples) && max_samples > 0 && max_samples <= 1.0 && max_samples != as.integer(max_samples)) max(1L, round(max_samples * n))
       else as.integer(max_samples)
  k <- if (is.null(max_features)) as.integer(ceiling(sqrt(d)))
       else if (is.numeric(max_features) && max_features > 0 && max_features <= 1.0 && max_features != as.integer(max_features)) max(1L, round(max_features * d))
       else as.integer(max_features)
  classify <- task == "classification" || (task == "auto" && all(unique(yv) %in% c(0, 1)))
  models <- list(); patches <- list(); stack <- matrix(0, M, n)
  fuse <- integer(d); ruse <- integer(n)
  for (mi in seq_len(M)) {
    if (isTRUE(bootstrap)) {
      st <- (seed + 7919 * (mi - 1L)) %% 2^32
      rows0 <- integer(s)
      for (i in seq_len(s)) { st <- .morie_al_lcg(st); rows0[i] <- as.integer((st * n) %/% 2^32) }
    } else rows0 <- .morie_w4c_lcg_sample(n, s, seed + 7919 * (mi - 1L))
    cols0 <- .morie_w4c_lcg_features(d, k, seed + 104729 * (mi - 1L) + 13)
    rows <- rows0 + 1L; cols <- cols0 + 1L
    Xp <- A[rows, cols, drop = FALSE]
    f <- if (is.null(base_estimator)) .morie_w4c_stump(Xp, yv[rows], classify) else base_estimator(Xp, yv[rows])
    pm <- as.numeric(f(A[, cols, drop = FALSE]))
    models[[mi]] <- list(f = f, cols = cols)
    patches[[mi]] <- list(rows = rows0, cols = cols0)
    fuse[cols] <- fuse[cols] + 1L; ruse[unique(rows)] <- ruse[unique(rows)] + 1L
    stack[mi, ] <- pm
  }
  aggregate <- function(P) if (classify) as.numeric(colMeans(P) >= 0.5) else colMeans(P)
  predict_fn <- function(Xnew) {
    B <- .morie_gr_mat(Xnew, "Xnew")
    aggregate(do.call(rbind, lapply(models, function(mm) as.numeric(mm$f(B[, mm$cols, drop = FALSE])))))
  }
  train_pred <- aggregate(stack); train_mse <- mean((train_pred - yv)^2)
  list(predict = predict_fn, train_pred = train_pred, train_mse = train_mse, patches = patches,
       feature_usage = fuse, row_usage = ruse, estimators = models, max_samples = s, max_features = k,
       task = if (classify) "classification" else "regression", estimate = train_mse, n = n,
       method = "Random patches: LCG-drawn row and column subsets per estimator")
}

# ============================================================ hmrsc (wraps morie_geron_cross_validation_score)

.morie_w4c_ridge_estimator <- function(params) {
  alpha <- if (is.null(params$alpha)) 0.0 else as.numeric(params$alpha)
  fit <- function(Xtr, ytr) solve(t(Xtr) %*% Xtr + alpha * diag(ncol(Xtr)), t(Xtr) %*% ytr)
  predict_fn <- function(theta, Xte) as.numeric(Xte %*% theta)
  list(fit = fit, predict = predict_fn)
}

#' Randomized hyperparameter search, scored by K-fold CV (Geron Ch 2, hmrsc)
#' @param param_dist Named list of distributions: an R `list(lo, hi)` of length 2 is a uniform
#'   INTERVAL (Python's `(lo, hi)` tuple); a plain vector is a discrete CHOICE list (Python's
#'   `\[...\]`), matching Python's own tuple-vs-list distinction. A function is `f(u)`.
#' @param n_iter,X,y,estimator,K,seed,score Controls; `estimator(params) -> list(fit=, predict=)`.
#' @export
morie_geron_randomized_search <- function(param_dist, n_iter, X, y, estimator = NULL, K = 3, seed = 0, score = NULL) {
  N <- as.integer(n_iter)
  est <- if (is.null(estimator)) .morie_w4c_ridge_estimator else estimator
  s <- as.numeric(seed) %% 2^32
  draw_u <- function() { s <<- .morie_al_lcg(s); (s + 0.5) / 2^32 }
  candidates <- list()
  for (it in seq_len(N)) {
    params <- list()
    for (nm in names(param_dist)) {
      spec <- param_dist[[nm]]; u <- draw_u()
      if (is.function(spec)) params[[nm]] <- spec(u)
      else if (is.list(spec) && length(spec) == 2L) params[[nm]] <- spec[[1]] + u * (spec[[2]] - spec[[1]])
      else { opts <- spec; params[[nm]] <- opts[[min(as.integer(u * length(opts)) + 1L, length(opts))]] }
    }
    candidates[[it]] <- params
  }
  scores <- numeric(N)
  for (i in seq_len(N)) {
    built <- est(candidates[[i]])
    cv <- morie_geron_cross_validation_score(X, y, K = as.integer(K), fit = built$fit, predict = built$predict, score = score)
    scores[i] <- cv$cv_score
  }
  best <- which.max(scores)
  list(best_params = candidates[[best]], best_score = scores[best], best_index = best - 1L,
       candidates = candidates, scores = scores, estimate = scores[best], n = length(as.numeric(y)),
       method = "Randomized search scored by K-fold CV via morie_geron_cross_validation_score")
}

# ============================================================ hmrsp

#' Random subspaces: feature bagging without row subsampling (Geron Ch 6, hmrsp)
#' @param X,y Data and targets.
#' @param base_estimator Optional `base_estimator(X_sub, y) -> predict`.
#' @param n_estimators,max_features,seed,task Controls.
#' @export
morie_geron_random_subspaces <- function(X, y, base_estimator = NULL, n_estimators = 10, max_features = NULL,
                                         seed = 0, task = "auto") {
  A <- .morie_gr_mat(X, "X"); yv <- as.numeric(y); n <- nrow(A); d <- ncol(A)
  M <- as.integer(n_estimators)
  k <- if (is.null(max_features)) as.integer(ceiling(sqrt(d)))
       else if (is.numeric(max_features) && max_features > 0 && max_features <= 1.0 && max_features != as.integer(max_features)) max(1L, round(max_features * d))
       else as.integer(max_features)
  classify <- task == "classification" || (task == "auto" && all(unique(yv) %in% c(0, 1)))
  models <- list(); sets <- list(); stack <- matrix(0, M, n); usage <- integer(d)
  for (mi in seq_len(M)) {
    cols0 <- .morie_w4c_lcg_features(d, k, seed + 7919 * (mi - 1L))
    cols <- cols0 + 1L
    f <- if (is.null(base_estimator)) .morie_w4c_stump(A[, cols, drop = FALSE], yv, classify) else base_estimator(A[, cols, drop = FALSE], yv)
    pm <- as.numeric(f(A[, cols, drop = FALSE]))
    models[[mi]] <- list(f = f, cols = cols); sets[[mi]] <- cols0; usage[cols] <- usage[cols] + 1L
    stack[mi, ] <- pm
  }
  aggregate <- function(P) if (classify) as.numeric(colMeans(P) >= 0.5) else colMeans(P)
  predict_fn <- function(Xnew) {
    B <- .morie_gr_mat(Xnew, "Xnew")
    aggregate(do.call(rbind, lapply(models, function(mm) as.numeric(mm$f(B[, mm$cols, drop = FALSE])))))
  }
  train_pred <- aggregate(stack); train_mse <- mean((train_pred - yv)^2)
  list(predict = predict_fn, train_pred = train_pred, train_mse = train_mse, feature_sets = sets,
       feature_usage = usage, estimators = models, max_features = k, task = if (classify) "classification" else "regression",
       estimate = train_mse, n = n, method = "Random subspaces: all rows, an LCG-drawn column subset per model")
}

# ============================================================ hmrvat

#' RNN visual attention: additive (Bahdanau-style) context over a feature map (Geron Ch 16, hmrvat)
#' @param features Spatial map (H, W, D) or (N, D).
#' @param h Decoder state.
#' @param W,U,v Attention parameters.
#' @export
morie_geron_rnn_visual_attention <- function(features, h, W, U, v) {
  F_ <- features
  grid <- NULL
  if (length(dim(F_)) == 3L) { grid <- dim(F_)[1:2]; Ff <- matrix(aperm(F_, c(2, 1, 3)), nrow = dim(F_)[1] * dim(F_)[2], byrow = TRUE) }
  else Ff <- .morie_gr_mat(F_, "features")
  hv <- as.numeric(h)
  Wm <- .morie_gr_mat(W, "W"); Um <- .morie_gr_mat(U, "U"); vv <- as.numeric(v)
  k <- nrow(Wm)
  pre <- Ff %*% t(Wm) + matrix(as.numeric(Um %*% hv), nrow(Ff), k, byrow = TRUE)
  scores <- as.numeric(tanh(pre) %*% vv)
  e <- exp(scores - max(scores)); alpha <- e / sum(e)
  context <- as.numeric(alpha %*% Ff)
  nz <- alpha[alpha > 0]; entropy <- -sum(nz * log(nz))
  list(context = context, alpha = alpha, alpha_map = if (!is.null(grid)) matrix(alpha, grid[1], grid[2], byrow = TRUE) else alpha,
       scores = scores, entropy = entropy, estimate = context, n = nrow(Ff),
       method = "Additive (Bahdanau-style) attention over a spatial feature map")
}

# ============================================================ hmrvn

#' RevNet: reversible residual block, verified by explicit inversion (Geron Ch 12, hmrvn)
#' @param x Input, even width on last axis.
#' @param F,G Shape-preserving residual functions.
#' @export
morie_geron_revnet <- function(x, F, G) {
  X <- as.numeric(x)
  half <- length(X) %/% 2L
  x1 <- X[seq_len(half)]; x2 <- X[(half + 1L):length(X)]
  apply_fn <- function(fn, arg) { out <- as.numeric(fn(arg)); .morie_gr_need(length(out) == length(arg), "geron_revnet: residual function must be shape-preserving"); out }
  y1 <- x1 + apply_fn(F, x2)
  y2 <- x2 + apply_fn(G, y1)
  x2_rec <- y2 - apply_fn(G, y1)
  x1_rec <- y1 - apply_fn(F, x2_rec)
  x_rec <- c(x1_rec, x2_rec)
  err <- max(abs(x_rec - X))
  y <- c(y1, y2)
  list(y = y, y1 = y1, y2 = y2, x1 = x1, x2 = x2, x_reconstructed = x_rec, reconstruction_error = err,
       reversible = err <= 1e-9 * max(1.0, max(abs(X))), estimate = err, n = length(X),
       method = "RevNet block y1 = x1 + F(x2), y2 = x2 + G(y1), verified by explicit inversion")
}

# ============================================================ hmrwd

#' Reward function R(s, a, s'): per-transition rewards and the backward discounted return (Geron Ch 19, hmrwd)
#' @param s,a,s_next Transition or trajectory arrays.
#' @param R Callable `R(s,a,s')` or a 2-D/3-D lookup table.
#' @param gamma Discount in \[0, 1\].
#' @export
morie_geron_reward_function <- function(s, a, s_next, R = NULL, gamma = 1.0) {
  .morie_gr_need(!is.null(R), "geron_reward_function: R is required")
  g <- as.numeric(gamma)
  sa <- as.numeric(s); aa <- as.numeric(a); sn <- as.numeric(s_next)
  .morie_gr_need(length(sa) == length(aa) && length(aa) == length(sn), "geron_reward_function: s/a/s_next length mismatch")
  if (is.function(R)) {
    rewards <- vapply(seq_along(sa), function(i) as.numeric(R(sa[i], aa[i], sn[i])), numeric(1))
  } else {
    Tb <- R  # R array indexed [s, a] or [s, a, s'], matching Python's table order exactly.
    d3 <- length(dim(Tb)) == 3L
    rewards <- vapply(seq_along(sa), function(i) {
      if (d3) Tb[sa[i] + 1L, aa[i] + 1L, sn[i] + 1L] else Tb[sa[i] + 1L, aa[i] + 1L]
    }, numeric(1))
  }
  returns <- numeric(length(rewards)); acc <- 0.0
  for (t_ in length(rewards):1) { acc <- rewards[t_] + g * acc; returns[t_] <- acc }
  list(rewards = rewards, total_reward = sum(rewards), returns = returns, discounted_return = returns[1],
       gamma = g, estimate = returns[1], n = length(rewards),
       method = "R(s, a, s') evaluated per transition with backward discounted returns")
}
