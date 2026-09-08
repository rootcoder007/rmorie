# SPDX-License-Identifier: AGPL-3.0-or-later
# Parity tests for geron_w4a_native.R against A4a (Python-generated anchors).

test_that("hmcst: contrastive learning matches Python anchor", {
  r <- morie_geron_contrastive_learning(matrix(c(1, 0, 1, 0, 0, 1), 3, 2, byrow = TRUE),
                                        c(1, 0, 0), tau = 1.0)
  expect_equal(r$loss, A4a$hmcst$loss, tolerance = 1e-9)
  expect_equal(r$per_anchor_loss[1], A4a$hmcst$per_anchor_loss_0, tolerance = 1e-9)
  expect_equal(r$pos_sim[1], A4a$hmcst$pos_sim_0, tolerance = 1e-9)
  expect_equal(r$n_negatives, A4a$hmcst$n_negatives)
  # independent route: anchor 2's positive and negative are both orthogonal to it,
  # so it sits exactly at chance, log(2).
  expect_equal(r$per_anchor_loss[3], log(2), tolerance = 1e-9)
})

test_that("hmdae: denoising autoencoder trains below the passthrough baseline", {
  X <- matrix(c(1, 1, 2, 2, 3, 3, 4, 4), 4, 2, byrow = TRUE)
  r <- morie_geron_denoising_autoencoder_train(X, noise_std = 0.2, epochs = 800, lr = 0.02, hidden = 1, seed = 1)
  expect_equal(r$final_loss, A4a$hmdae$final_loss, tolerance = 1e-6)
  expect_equal(r$passthrough_loss, A4a$hmdae$passthrough_loss, tolerance = 1e-9)
  expect_equal(length(r$loss_history), A4a$hmdae$loss_history_len)
  expect_true(r$final_loss < r$passthrough_loss)
})

test_that("hmdale: DALL-E autoregressive generation matches Python", {
  skewed <- function(ctx) c(0.0, 5.0)
  r <- morie_geron_dalle(0, skewed, n_image_tokens = 4)
  expect_equal(r$image_tokens, A4a$hmdale$image_tokens)
  expect_equal(r$token_grid, A4a$hmdale$token_grid)
  expect_equal(r$log_likelihood, A4a$hmdale$log_likelihood, tolerance = 1e-9)
})

test_that("hmdbd: decision boundary matches Python, independent geometry check", {
  r <- morie_geron_decision_boundary(c(-1, 1, 1), matrix(c(0, 0, 1, 1, 0.5, 0.5), 3, 2, byrow = TRUE))
  expect_equal(r$labels, A4a$hmdbd$labels)
  expect_equal(round(r$signed_distance, 9), A4a$hmdbd$signed_distance)
  expect_equal(r$on_boundary, A4a$hmdbd$on_boundary)
  expect_equal(round(r$line, 6), A4a$hmdbd$line)
  expect_equal(round(r$probabilities[3], 12), A4a$hmdbd$prob_2)
  # independent route: signed distance is |x1+x2-1| / sqrt(2) by hand.
  expect_equal(r$signed_distance[2], (1 + 1 - 1) / sqrt(2), tolerance = 1e-9)
})

test_that("hmdbrt: DistilBERT triple loss matches Python", {
  r <- morie_geron_distilbert(matrix(c(2, 0), 1, 2), matrix(c(2, 0), 1, 2), c(1, 2, 3),
                              alpha_mlm = 0, alpha_ce = 1)
  expect_equal(round(r$loss_ce, 12), A4a$hmdbrt$loss_ce_match)
  expect_equal(r$agreement, A4a$hmdbrt$agreement)
  expect_equal(round(r$param_reduction, 3), A4a$hmdbrt$param_reduction)
  a <- morie_geron_distilbert(matrix(c(10, 0), 1, 2), matrix(c(0, 0), 1, 2), c(1), alpha_mlm = 0, alpha_ce = 1, temperature = 1)
  b <- morie_geron_distilbert(matrix(c(10, 0), 1, 2), matrix(c(0, 0), 1, 2), c(1), alpha_mlm = 0, alpha_ce = 1, temperature = 2)
  expect_equal(round(a$loss_ce, 6), A4a$hmdbrt$loss_ce_t1)
  expect_equal(round(b$loss_ce, 6), A4a$hmdbrt$loss_ce_t2)
})

test_that("hmdbs: DBSCAN clustering matches Python", {
  r <- morie_geron_dbscan(matrix(c(0, 0.5, 10, 10.5, 50), 5, 1), eps = 1.0, min_samples = 2)
  expect_equal(r$labels, A4a$hmdbs$labels)
  expect_equal(r$n_clusters, A4a$hmdbs$n_clusters)
  expect_equal(r$n_noise, A4a$hmdbs$n_noise)
  expect_equal(unname(r$cluster_sizes), A4a$hmdbs$cluster_sizes)
  # raising min_samples above local density leaves nothing but noise
  r2 <- morie_geron_dbscan(matrix(c(0, 0.5, 10, 10.5, 50), 5, 1), eps = 1.0, min_samples = 3)
  expect_true(all(r2$labels == -1))
})

test_that("hmdcg: DCGAN architecture resolves to Python's exact shapes", {
  r <- morie_geron_dcgan(array(0, dim = c(3, 16, 16)), z_dim = 8, filters = 4)
  expect_equal(r$n_layers, A4a$hmdcg$n_layers)
  expect_equal(r$generator_layers[[1]]$params, A4a$hmdcg$gen0_params)
  expect_equal(as.numeric(r$sample_shape), A4a$hmdcg$sample_shape)
  deconv_outs <- vapply(r$generator_layers, function(l) if (l$kind == "deconv") l$out else NA_integer_, 0L)
  expect_equal(deconv_outs[!is.na(deconv_outs)], A4a$hmdcg$deconv_outs)
  conv_outs <- vapply(r$discriminator_layers, function(l) if (l$kind == "conv") l$out else NA_integer_, 0L)
  expect_equal(conv_outs[!is.na(conv_outs)], A4a$hmdcg$conv_outs)
  # independent route: projection param count by hand, 8*(4*4*8)+128.
  expect_equal(A4a$hmdcg$gen0_params, 8 * (4 * 4 * 8) + 128)
})

test_that("hmdctr: decoder-only transformer param/mask counts match Python", {
  r <- morie_geron_decoder_only(c(1, 2, 3), n_layers = 1, n_heads = 2, d_model = 4, vocab_size = 10, max_len = 8)
  expect_equal(r$per_block$self_attention, A4a$hmdctr$self_attention)
  expect_equal(r$per_block$ffn, A4a$hmdctr$ffn)
  expect_equal(r$per_block$layer_norms, A4a$hmdctr$layer_norms)
  expect_equal(r$block_params, A4a$hmdctr$block_params)
  expect_equal(r$d_head, A4a$hmdctr$d_head)
  expect_equal(r$embedding_params, A4a$hmdctr$embedding_params)
  expect_equal(r$mask, A4a$hmdctr$mask)
  # independent route: attention = 4d^2+4d = 4*16+16 = 80; ffn = 2*4*16+16+4 = 148.
  expect_equal(A4a$hmdctr$self_attention, 4 * 4^2 + 4 * 4)
  expect_equal(A4a$hmdctr$ffn, 2 * 4 * 16 + 16 + 4)
})

test_that("hmdetr / hmdeit share hmdctr's block_params (used by three modules)", {
  bp <- morie_geron_block_params(4, cross_attention = FALSE)
  expect_equal(bp$total, A4a$hmdctr$block_params)
})

test_that("hmddim: DDIM sub-sequence sampling matches Python", {
  zero <- function(x, t) rep(0, length(x))
  r <- morie_geron_ddim(1.0, zero, T = 4, n_steps = 2, beta_schedule = rep(0.5, 4))
  expect_equal(r$timesteps, A4a$hmddim$timesteps)
  expect_equal(r$model_calls, A4a$hmddim$model_calls)
  expect_equal(round(r$x_0[1], 9), A4a$hmddim$x0)
  # 2 steps vs 4 steps end at the same point (deterministic map), speedup differs
  r2 <- morie_geron_ddim(1.0, zero, T = 4, n_steps = 4, beta_schedule = rep(0.5, 4))
  expect_equal(round(r2$x_0[1], 9), A4a$hmddim$x0)
  expect_equal(r$speedup, 2.0)
  expect_equal(r2$speedup, 1.0)
})

test_that("hmdfw: diffusion forward alpha_bar and SNR match Python", {
  r <- morie_geron_diffusion_forward(1.0, T = 3, beta_schedule = c(0.5, 0.5, 0.5))
  expect_equal(round(r$alpha_bar, 12), A4a$hmdfw$alpha_bar)
  expect_equal(round(r$snr, 12), A4a$hmdfw$snr)
  # independent route: alpha_bar_k = 0.5^k by hand.
  expect_equal(r$alpha_bar, 0.5^(1:3))
  r0 <- morie_geron_diffusion_forward(c(2.0, -1.0), T = 2, beta_schedule = c(0.1, 0.2), t = 0)
  expect_equal(r0$x_t, A4a$hmdfw$t0_x_t)
})

test_that("hmdqn: tabular DQN one-step update matches Python", {
  r <- morie_geron_dqn(NULL, matrix(c(0, 0), 1, 2), matrix(c(0, 0), 1, 2),
                       list(list(0, 0, 1.0, 0, TRUE)), epochs = 1, lr = 0.5)
  expect_equal(round(r$Q[1, 1], 6), A4a$hmdqn$Q00)
  expect_equal(round(r$loss_history[1], 6), A4a$hmdqn$loss0)
})

test_that("hmgmm: EM Gaussian mixture recovers the two clusters, matches Python", {
  X <- matrix(c(0, 0.2, 0.1, 10, 10.2, 9.9), 6, 1)
  r <- morie_geron_gaussian_mixture(X, n_components = 2, seed = 1)
  expect_equal(sort(round(r$means[, 1], 1)), A4a$hmgmm$means_sorted)
  expect_equal(round(sort(r$weights), 6), A4a$hmgmm$weights_sorted)
  expect_equal(r$monotone, A4a$hmgmm$monotone)
  expect_equal(r$converged, A4a$hmgmm$converged)
  # independent route: EM log-likelihood must be non-decreasing (checked structurally).
  expect_true(all(diff(r$ll_history) >= -1e-8))
})

test_that("hmdrp: inverted dropout matches Python at p=0 and p=0.5", {
  r <- morie_geron_dropout_alt(c(1, 2, 3), p = 0.0)
  expect_equal(r$y, A4a$hmdrp$y_p0)
  expect_equal(r$n_dropped, A4a$hmdrp$n_dropped_p0)
  r2 <- morie_geron_dropout_alt(c(4, 4, 4, 4), p = 0.5, seed = 7)
  expect_equal(r2$scale, A4a$hmdrp$scale_p5)
  expect_equal(r2$y, A4a$hmdrp$y_p5)
  # independent route: E[y] = x under inverted dropout (checked on the survivors).
  expect_true(all(r2$y %in% c(0, 8)))
})

test_that("hmfa: FlashAttention tiling is exact, matches Python", {
  r <- morie_geron_flash_attention(matrix(0, 1, 1), matrix(c(1, 3), 2, 1), matrix(c(1, 3), 2, 1))
  expect_equal(round(r$output[1, 1], 6), A4a$hmfa$output00)
  expect_equal(r$max_abs_error, A4a$hmfa$max_abs_error, tolerance = 1e-12)
  # independent route: tiling must not change the answer at block_size 1 vs 4.
  Q <- matrix(c(1, 0, 0, 1, 1, 1, 0.5, 0.5), 4, 2, byrow = TRUE)
  K <- matrix(c(1, 0, 0, 1, 1, 1, 2, 0), 4, 2, byrow = TRUE)
  V <- matrix(c(1, 2, 3, 4), 4, 1)
  a <- morie_geron_flash_attention(Q, K, V, block_size = 1)
  b <- morie_geron_flash_attention(Q, K, V, block_size = 4)
  expect_equal(a$output, b$output, tolerance = 1e-9)
})

test_that("hmddpg: DDPG zero-noise action and Polyak averaging match Python", {
  env <- function(s, a) list(s, -((a - 1.0)^2), FALSE)
  r <- morie_geron_ddpg(env, 0.5, c(0, 0), epochs = 1, lr = 0.0, ou_sigma = 0.0, s0 = 2.0)
  expect_equal(round(r$actions[1], 12), A4a$hmddpg$action0)
  expect_equal(r$ou_noise[1], A4a$hmddpg$ou_noise0)
  r3 <- morie_geron_ddpg(env, 0.0, c(1, 1), epochs = 1, lr = 0.0, tau = 0.25, ou_sigma = 0.0, critic_target = c(0, 0))
  expect_equal(round(r3$critic_target, 6), A4a$hmddpg$critic_target_polyak)
})

test_that("hmddpm: DDPM training loss decreases, matches Python", {
  X <- matrix(c(1, 2, 3, 4), 4, 1)
  r <- morie_geron_ddpm(X, T = 4, epochs = 400, lr = 0.1, seed = 1)
  expect_equal(r$final_loss < r$loss_history[1], A4a$hmddpm$final_below_first)
  expect_equal(length(r$loss_by_t), A4a$hmddpm$loss_by_t_len)
  expect_equal(r$monotone, A4a$hmddpm$monotone)
  expect_equal(dim(r$sample), A4a$hmddpm$sample_shape)
})

test_that("hmddqn: Double DQN removes the overestimation gap, matches Python", {
  r <- morie_geron_double_dqn(NULL, matrix(c(0, 1), 1, 2), matrix(c(10, -10), 1, 2),
                              list(list(0, 0, 0.0, 0, FALSE)), epochs = 1, lr = 1.0, gamma = 1.0)
  expect_equal(r$targets[1], A4a$hmddqn$target0)
  expect_equal(r$vanilla_targets[1], A4a$hmddqn$vanilla0)
  expect_equal(r$overestimation_gap[1], A4a$hmddqn$gap0)
  expect_equal(round(r$Q[1, 1], 6), A4a$hmddqn$Q00)
  # independent route: vanilla max - double-DQN value = the overestimation gap, by definition.
  expect_equal(r$vanilla_targets[1] - r$targets[1], r$overestimation_gap[1])
})

test_that("hmdeit: DeiT architecture matches Python's patch/token counts", {
  img <- array(0, dim = c(3, 32, 32))
  r <- morie_geron_deit(img, patch_size = 16, n_layers = 1, d_model = 8, n_heads = 2, n_classes = 4)
  expect_equal(r$n_patches, A4a$hmdeit$n_patches)
  expect_equal(r$n_tokens, A4a$hmdeit$n_tokens)
  expect_equal(r$patch_embed_params, A4a$hmdeit$patch_embed_params)
  expect_equal(r$distillation_overhead, A4a$hmdeit$distillation_overhead)
  # independent route: distillation overhead = d + d*K + K = 8 + 8*4 + 4.
  expect_equal(A4a$hmdeit$distillation_overhead, 8 + 8 * 4 + 4)
})

test_that("hmdetr: DETR feature grid and query ceiling match Python", {
  r <- morie_geron_detr(array(0, dim = c(3, 224, 224)), n_queries = 10, n_layers = 1, d_model = 8, n_heads = 2, n_classes = 3)
  expect_equal(as.numeric(r$feature_shape), A4a$hmdetr$feature_shape)
  expect_equal(r$n_tokens, A4a$hmdetr$n_tokens)
  expect_equal(r$max_detections, A4a$hmdetr$max_detections)
  expect_equal(r$encoder_attention_cost, A4a$hmdetr$encoder_attention_cost)
  # independent route: 224 / 32 = 7, tokens = 7*7 = 49, attention cost = 49^2.
  expect_equal(A4a$hmdetr$encoder_attention_cost, 49^2)
})

test_that("hmdino: DINO uniform-logit loss and entropy match Python's log(2)", {
  r <- morie_geron_dino(NULL, matrix(0, 2, 2), matrix(0, 2, 2))
  expect_equal(round(r$loss, 9), A4a$hmdino$loss)
  expect_equal(round(r$teacher_entropy, 9), A4a$hmdino$teacher_entropy)
  expect_equal(round(r$kl_to_uniform, 12), A4a$hmdino$kl_to_uniform)
  expect_equal(round(r$loss, 9), round(log(2), 9))
})

test_that("hmdld: DataLoader batch plan and Fisher-Yates shuffle match Python", {
  r <- morie_geron_dataloader(7, batch_size = 3)
  expect_equal(r$batches, A4a$hmdld$batches)
  expect_equal(r$n_batches, A4a$hmdld$n_batches)
  expect_equal(r$last_batch_size, A4a$hmdld$last_batch_size)
  expect_equal(r$dropped, A4a$hmdld$dropped)
  r2 <- morie_geron_dataloader(7, 3, shuffle = TRUE, seed = 42)
  expect_equal(r2$order, A4a$hmdld$shuffled_order)
  # independent route: a shuffle is a permutation of 0..6.
  expect_equal(sort(r2$order), 0:6)
})

test_that("hmdldqn: Dueling DQN gradient split matches Python", {
  r <- morie_geron_dueling_dqn_alt(NULL, 0.0, matrix(c(0, 0), 1, 2), list(list(0, 0, 1.0, 0, TRUE)), epochs = 1, lr = 1.0)
  expect_equal(round(r$V[1], 6), A4a$hmdldqn$V0)
  expect_equal(round(r$A[1, ], 6), A4a$hmdldqn$A0)
  expect_equal(round(r$Q[1, ], 6), A4a$hmdldqn$Q0)
  # independent route: advantages are always mean-centred by construction.
  expect_equal(mean(r$A[1, ]), 0, tolerance = 1e-9)
})

test_that("hmdpo: DPO loss matches Python", {
  r <- morie_geron_dpo(matrix(c(-1, -2), 1, 2), matrix(c(-1, -2), 1, 2))
  expect_equal(round(r$loss, 9), A4a$hmdpo$loss_match_log2)
  expect_equal(round(r$prob_preferred[1], 9), A4a$hmdpo$prob0)
  r2 <- morie_geron_dpo(matrix(c(0, -2), 1, 2), matrix(c(-1, -2), 1, 2), beta = 1.0)
  expect_equal(round(r2$margin[1], 9), A4a$hmdpo$margin0)
  expect_equal(round(r2$loss, 6), A4a$hmdpo$loss2)
  expect_equal(r2$accuracy, A4a$hmdpo$acc2)
  # independent route: at margin=0 the loss is exactly log(2).
  expect_equal(round(r$loss, 9), round(log(2), 9))
})

test_that("hmdqnt: dynamic quantization matches Python", {
  r <- morie_geron_dynamic_quantization_alt(list(W = c(-1, 0, 1)))
  expect_equal(round(r$scales$W, 12), A4a$hmdqnt$scale_w)
  expect_equal(r$quantized$W, A4a$hmdqnt$quant_w)
  expect_equal(r$compression, A4a$hmdqnt$compression)
  r2 <- morie_geron_dynamic_quantization_alt(list(W = c(-0.3, 0, 0.9)))
  expect_equal(r2$zero_points$W, A4a$hmdqnt$zero_point)
  r3 <- morie_geron_dynamic_quantization_alt(list(W = 1.0), activations = c(0, 2))
  expect_equal(round(r3$activation$scale, 12), A4a$hmdqnt$act_scale)
  # independent route: symmetric scale is amax/127.
  expect_equal(round(r$scales$W, 12), round(1 / 127, 12))
})

test_that("hmdrnn: stacked RNN forward pass matches Python", {
  W <- list(list(matrix(1, 1, 1), matrix(1, 1, 1), 0))
  r <- morie_geron_deep_rnn(matrix(c(1, 1, 1), 3, 1), weights = W, activation = "relu")
  expect_equal(vapply(r$outputs, `[`, 0, 1), A4a$hmdrnn$outputs1)
  expect_equal(r$final_states[[1]], A4a$hmdrnn$final1[[1]])
  W2 <- list(list(matrix(1, 1, 1), matrix(1, 1, 1), 0), list(matrix(1, 1, 1), matrix(1, 1, 1), 0))
  r2 <- morie_geron_deep_rnn(matrix(c(1, 1, 1), 3, 1), weights = W2, activation = "relu")
  expect_equal(vapply(r2$outputs, `[`, 0, 1), A4a$hmdrnn$outputs2)
  r4 <- morie_geron_deep_rnn(matrix(c(0, 0), 1, 2), hidden_sizes = 3)
  expect_equal(r4$n_params, A4a$hmdrnn$n_params4)
  # independent route: 1,3,6 are triangular numbers (identity ReLU integration twice).
  expect_equal(A4a$hmdrnn$outputs2, c(1, 3, 6))
})

test_that("hmdrv: DDPM ancestral reverse sampling matches Python", {
  zero <- function(x, t) rep(0, length(x))
  r <- morie_geron_diffusion_reverse(1.0, zero, T = 1, beta_schedule = 0.75)
  expect_equal(round(r$x_0[1], 12), A4a$hmdrv$x0_1)
  expect_equal(r$model_calls, A4a$hmdrv$calls1)
  eps <- 1.0
  x1 <- 0.5 * 3.0 + sqrt(0.75) * eps
  r2 <- morie_geron_diffusion_reverse(x1, function(x, t) rep(eps, length(x)), T = 1, beta_schedule = 0.75)
  expect_equal(round(r2$x_0[1], 9), A4a$hmdrv$x0_2)
})

test_that("hmdthv: tree variance via bootstrap matches Python", {
  r <- morie_geron_tree_high_variance(matrix(c(1, 2, 8, 9), 4, 1), c(0, 0, 1, 1), n_resamples = 10, seed = 1)
  expect_equal(r$structural_instability, A4a$hmdthv$instability)
  expect_equal(round(r$variance, 6), A4a$hmdthv$variance)
  expect_equal(r$bias2, A4a$hmdthv$bias2)
})

test_that("hmdtr: tree regularization matches Python", {
  X <- matrix(c(1, 2, 3, 4), 4, 1)
  y <- c(0, 0, 1, 1)
  r <- morie_geron_tree_regularization(X, y, max_depth = 0)
  expect_equal(r$n_leaves, A4a$hmdtr$n_leaves)
  expect_equal(r$baseline_leaves, A4a$hmdtr$baseline_leaves)
  expect_equal(r$train_score, A4a$hmdtr$train_score)
  expect_equal(r$baseline_train_score, A4a$hmdtr$baseline_train_score)
  expect_equal(round(r$train_score_cost, 6), A4a$hmdtr$cost)
  r2 <- morie_geron_tree_regularization(X, y, min_samples_leaf = 3)
  expect_equal(r2$n_leaves, A4a$hmdtr$n_leaves2)
  expect_equal(r2$leaves_saved, A4a$hmdtr$saved2)
})

test_that("hmdtst: tree scale invariance matches Python", {
  X <- matrix(c(1, 5, 2, 4, 3, 9, 4, 1), 4, 2, byrow = TRUE)
  y <- c(0, 0, 1, 1)
  r <- morie_geron_tree_sensitivity_scale(X, y)
  expect_equal(r$predictions_match, A4a$hmdtst$pred_match)
  expect_equal(r$thresholds_match, A4a$hmdtst$thr_match)
  expect_equal(r$thresholds, A4a$hmdtst$thresholds)
  expect_equal(r$scaled_thresholds, A4a$hmdtst$scaled_thresholds)
  # independent route: threshold scales as a*t+b = 100*2.5-7 = 243.
  expect_equal(r$scaled_thresholds, 100 * r$thresholds - 7)
})

test_that("hmeaf: error analysis matches Python", {
  r <- morie_geron_error_analysis(c(0, 0, 1, 1), c(0, 1, 1, 1))
  expect_equal(round(r$normalized, 6), A4a$hmeaf$normalized)
  expect_equal(as.numeric(r$top_confusions[[1]]), A4a$hmeaf$top0)
  expect_equal(round(r$error_rate, 6), A4a$hmeaf$error_rate)
  expect_equal(r$worst_class, A4a$hmeaf$worst_class)
})

test_that("hmearl: early stopping keeps the best snapshot, matches Python", {
  Xt <- matrix(c(0, 1, 2, 3), 4, 1)
  yt <- c(0, 2, 4, 6)
  r <- morie_geron_early_stopping_alt(Xt, yt, matrix(c(4, 5), 2, 1), c(8, 10), n_iter = 200, eta = 0.05)
  expect_equal(r$best_iter == 200, A4a$hmearl$best_iter_eq_200)
  expect_equal(round(r$theta[2], 2), A4a$hmearl$theta1_round2)
  r2 <- morie_geron_early_stopping_alt(Xt, yt, matrix(c(0, 1), 2, 1), c(3, 3), n_iter = 200, eta = 0.05)
  expect_equal(r2$is_u_shaped, A4a$hmearl$u_shaped)
  expect_equal(r2$best_iter < 200, A4a$hmearl$best_lt_200)
  # independent route: the best snapshot's val RMSE can never exceed the final one.
  expect_true(r2$best_val_rmse <= r2$final_val_rmse)
})

test_that("hmeg: epsilon-greedy distribution matches Python", {
  r <- morie_geron_epsilon_greedy_alt(matrix(c(1, 5, 2), 1, 3), s = 0, epsilon = 0.3)
  expect_equal(round(r$probabilities, 12), A4a$hmeg$probs)
  expect_equal(r$greedy_action, A4a$hmeg$greedy)
  # independent route: probabilities always sum to 1.
  expect_equal(sum(r$probabilities), 1, tolerance = 1e-12)
})

test_that("hmelb: VAE ELBO matches Python", {
  r <- morie_geron_elbo(matrix(0, 1, 1), mu = matrix(0, 1, 1), log_sigma = matrix(0, 1, 1))
  expect_equal(round(r$kl, 12), A4a$hmelb$kl0)
  expect_equal(round(r$reconstruction_log_lik, 9), A4a$hmelb$rec0)
  r2 <- morie_geron_elbo(matrix(0, 1, 1), matrix(1, 1, 1), matrix(0, 1, 1))
  expect_equal(round(r2$kl, 12), A4a$hmelb$kl1)
  # independent route: KL formula by hand at mu=1, log_sigma=0.
  expect_equal(round(r2$kl, 12), round(-0.5 * (1 - 1), 12) + 0.5)
})

test_that("hmencd: encoder-decoder transformer matches Python", {
  r <- morie_geron_encoder_decoder_transformer(c(1, 2), c(3, 4, 5), n_layers = 1, n_heads = 2, d_model = 4,
                                               vocab_size = 10, max_len = 8, d_ff = 16)
  expect_equal(r$encoder_block_params, A4a$hmencd$enc_block)
  expect_equal(r$decoder_block_params, A4a$hmencd$dec_block)
  expect_equal(r$extra_per_decoder_block, A4a$hmencd$extra)
  expect_equal(r$tgt_mask, A4a$hmencd$tgt_mask)
  # decoder block cost = encoder + extra cross-attention (shared hmdctr core).
  expect_equal(r$decoder_block_params, r$encoder_block_params + r$extra_per_decoder_block)
})

test_that("hmenet: elastic net cost matches Python", {
  r <- morie_geron_elastic_net(matrix(c(1, 2), 2, 1), c(2, 4), c(0, 2), alpha = 1.0, r = 0.5)
  expect_equal(round(r$mse, 12), A4a$hmenet$mse)
  expect_equal(round(r$l1_penalty, 12), A4a$hmenet$l1)
  expect_equal(round(r$l2_penalty, 12), A4a$hmenet$l2)
  expect_equal(round(r$cost, 12), A4a$hmenet$cost)
})

test_that("hmevr: explained variance ratio matches Python", {
  r <- morie_geron_explained_variance_ratio_alt(matrix(c(1, 1, 2, 2, 3, 3), 3, 2, byrow = TRUE))
  expect_equal(round(r$explained_variance_ratio, 12), A4a$hmevr$evr1)
  Y <- matrix(c(-2, -1, 2, 1, -2, 1, 2, -1), 4, 2, byrow = TRUE)
  r2 <- morie_geron_explained_variance_ratio_alt(Y)
  expect_equal(round(r2$explained_variance_ratio, 12), A4a$hmevr$evr2)
  expect_equal(round(r2$explained_variance, 12), A4a$hmevr$var2)
  # independent route: ratios always sum to 1.
  expect_equal(sum(r2$explained_variance_ratio), 1, tolerance = 1e-9)
})

test_that("hmext: extra-trees ensemble matches Python", {
  r <- morie_geron_extra_trees(matrix(c(1, 2, 8, 9), 4, 1), c(0, 0, 1, 1), n_estimators = 9, seed = 5)
  expect_equal(r$train_score, A4a$hmext$train_score)
  expect_equal(r$n_estimators, A4a$hmext$n_trees)
})

test_that("hmf1: F1 score matches Python", {
  r <- morie_geron_f1_score_alt(c(0, 0, 1, 1), c(0, 1, 1, 1))
  expect_equal(round(r$precision, 6), A4a$hmf1$precision)
  expect_equal(round(r$recall, 6), A4a$hmf1$recall)
  expect_equal(round(r$f1, 6), A4a$hmf1$f1)
  expect_equal(c(r$tp, r$fp, r$fn), c(A4a$hmf1$tp, A4a$hmf1$fp, A4a$hmf1$fn))
  r2 <- morie_geron_f1_score_alt(c(0, 0, 1, 1), c(0, 1, 1, 1), average = "macro")
  expect_equal(round(r2$f1, 6), A4a$hmf1$macro_f1)
  # independent route: F1 is the harmonic mean of precision and recall.
  expect_equal(r$f1, 2 * r$precision * r$recall / (r$precision + r$recall), tolerance = 1e-9)
})

test_that("hmfcn: fully convolutional network forward pass matches Python", {
  img <- matrix(c(1, -1, 2, 0), 2, 2, byrow = TRUE)
  model <- list(list(array(c(1, -1), dim = c(2, 1, 1, 1)), 0, 1))
  r <- morie_geron_fcn(img, model)
  expect_equal(as.numeric(r$out_shape), A4a$hmfcn$out_shape)
  expect_equal(r$segmentation, A4a$hmfcn$segmentation)
})

test_that("hmflmg: Flamingo gated cross-attention matches Python", {
  r <- morie_geron_flamingo(matrix(c(1, 3), 2, 1), matrix(c(2, 4), 2, 1))
  expect_equal(r$output, A4a$hmflmg$output)
  expect_equal(r$is_identity_at_init, A4a$hmflmg$identity)
  r2 <- morie_geron_flamingo(matrix(c(1, 3), 2, 1), matrix(c(0, 0), 2, 1), gate = 1.0)
  expect_equal(round(r2$output[1, ], 6), A4a$hmflmg$output2_0)
  # independent route: at gate=0, tanh(0)=0, so output is exactly the text input.
  expect_true(r$is_identity_at_init)
})

test_that("hmfmap: feature map matches Python", {
  X <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3, byrow = TRUE)
  r <- morie_geron_feature_map(X, matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE))
  expect_equal(r$feature_map, A4a$hmfmap$fmap)
  expect_equal(r$sparsity, A4a$hmfmap$sparsity0)
  r2 <- morie_geron_feature_map(X, matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE), b = -9.0)
  expect_equal(r2$feature_map, A4a$hmfmap$fmap2)
  expect_equal(round(r2$sparsity, 6), A4a$hmfmap$sparsity2)
})

test_that("hmfmn: FashionMNIST CNN architecture matches Python", {
  r <- morie_geron_fashion_mnist()
  outs <- vapply(r$layers, function(l) if (l$kind %in% c("conv", "pool")) l$out else NA_integer_, 0L)
  expect_equal(outs[!is.na(outs)], A4a$hmfmn$conv_pool_outs)
  expect_equal(r$flatten_dim, A4a$hmfmn$flatten_dim)
  expect_equal(r$total_params, A4a$hmfmn$total_params)
  expect_equal(round(r$fc_share, 3), A4a$hmfmn$fc_share)
  expect_equal(r$steps_per_epoch, A4a$hmfmn$steps_per_epoch)
})

test_that("hmfp16: IEEE-754 binary16 round trip matches Python", {
  r <- morie_geron_fp16_quant(1.0)
  expect_equal(r$value, A4a$hmfp16$value1)
  expect_equal(r$rel_error, A4a$hmfp16$rel_error1)
  expect_equal(round(r$eps, 12), A4a$hmfp16$eps)
  expect_equal(r$exponent, A4a$hmfp16$exponent1)
  expect_equal(r$mantissa_field, A4a$hmfp16$mantissa1)
  r2 <- morie_geron_fp16_quant(c(70000.0, 1e-9))
  expect_equal(r2$overflowed, A4a$hmfp16$overflow2)
  expect_equal(r2$underflowed, A4a$hmfp16$underflow2)
  r3 <- morie_geron_fp16_quant(0.1)
  expect_equal(round(r3$rel_error[1], 9), A4a$hmfp16$rel_error3)
})

test_that("hmfp32: IEEE-754 binary32 field decomposition matches Python", {
  r <- morie_geron_fp32(1.0)
  expect_equal(r$sign, A4a$hmfp32$sign1)
  expect_equal(r$exponent_field, A4a$hmfp32$exp_field1)
  expect_equal(r$mantissa_field, A4a$hmfp32$mant_field1)
  expect_equal(r$exponent, A4a$hmfp32$exponent1)
  expect_equal(round(r$eps, 12), A4a$hmfp32$eps)
  r2 <- morie_geron_fp32(c(1e40, 0.0, 1e-42))
  expect_equal(r2$kind, A4a$hmfp32$kind2)
})

test_that("hmfsf: few-shot prompting matches Python", {
  copycat <- function(prompt) {
    lines <- Filter(function(l) grepl("->", l) && !grepl("-> $", l), strsplit(prompt, "\n")[[1]])
    if (length(lines)) trimws(strsplit(lines[length(lines)], "-> ")[[1]][2]) else "?"
  }
  r <- morie_geron_few_shot(copycat, list(list("a", "1"), list("b", "2")), "c")
  expect_equal(r$prediction, A4a$hmfsf$prediction)
  expect_equal(r$zero_shot_prediction, A4a$hmfsf$zero_shot)
  expect_equal(r$changed_by_context, A4a$hmfsf$changed)
  r2 <- morie_geron_few_shot(copycat, list(list("a", "1"), list("b", "2")), "c", k = 1)
  expect_equal(r2$prompt, A4a$hmfsf$prompt2)
  expect_equal(r2$prediction, A4a$hmfsf$pred2)
})

test_that("hmfth: LM fine-tuning matches Python", {
  task <- function(th, batch) list((th[1] - 3.0)^2, 2.0 * (th[1] - 3.0))
  r <- morie_geron_finetune_lm(task, list(1), epochs = 1, lr = 0.1, theta = 0.0)
  expect_equal(round(r$theta[1], 12), A4a$hmfth$theta1)
  expect_equal(round(r$loss_history[1], 12), A4a$hmfth$loss1)
  r2 <- morie_geron_finetune_lm(task, list(1), epochs = 100, lr = 0.1, theta = 0.0)
  expect_equal(round(r2$theta[1], 6), A4a$hmfth$theta2)
  expect_equal(round(r2$drift, 6), A4a$hmfth$drift2)
  r4 <- morie_geron_finetune_lm(task, list(1), epochs = 4, lr = 0.1, theta = 0.0, warmup = 2)
  expect_equal(round(r4$lr_schedule, 6), A4a$hmfth$lr_sched4)
})

test_that("hmgan: GAN minimax equilibrium matches Python", {
  X <- matrix(c(0, 1, 2, 3), 4, 1)
  r0 <- morie_geron_gan(X, G = list(matrix(0, 1, 1), 1.5), D = list(0.0, 0.0), epochs = 1, lr = 0.0)
  expect_equal(round(r0$value_history[1], 6), A4a$hmgan$value0)
  expect_equal(round(r0$equilibrium_value, 6), A4a$hmgan$equilibrium)
  # independent route: at D(x)=0.5 everywhere, V = 2*log(0.5) by hand.
  expect_equal(round(r0$equilibrium_value, 6), round(2 * log(0.5), 6))
})

test_that("hmgand: GMM anomaly detection matches Python", {
  X <- matrix(c(0, 0.1, 0.2, 10, 10.1, 10.2, 100, 0.05, 10.05, 0.15), 10, 1)
  r <- morie_geron_anomaly_gmm(X, n_components = 2, contamination = 0.1, seed = 1)
  expect_equal(r$anomaly_indices, A4a$hmgand$anomaly_indices)
  expect_equal(r$n_anomalies, A4a$hmgand$n_anomalies)
})

test_that("hmgbrt: gradient boosting matches Python", {
  X <- matrix(c(1, 2, 3, 4), 4, 1)
  y <- c(0, 0, 10, 10)
  r <- morie_geron_gradient_boosting(X, y, n_estimators = 1, learning_rate = 0.1, max_depth = 1)
  expect_equal(r$init, A4a$hmgbrt$init)
  expect_equal(round(r$predictions, 6), A4a$hmgbrt$predictions)
  r2 <- morie_geron_gradient_boosting(X, y, n_estimators = 1, learning_rate = 1.0, max_depth = 1)
  expect_equal(round(r2$predictions, 6), A4a$hmgbrt$predictions2)
  expect_equal(r2$train_mse, A4a$hmgbrt$train_mse2)
  # independent route: init is the target mean, 5.0.
  expect_equal(r$init, mean(y))
})

test_that("hmgoog: GoogLeNet architecture matches Python", {
  r <- morie_geron_googlenet(1000)
  expect_equal(r$total_params, A4a$hmgoog$total_params)
  expect_equal(as.numeric(r$final_feature_map), A4a$hmgoog$final_feature_map)
  expect_equal(r$modules[[1]]$name, A4a$hmgoog$mod0_name)
  expect_equal(r$modules[[1]]$out_channels, A4a$hmgoog$mod0_out_ch)
  expect_equal(r$modules[[1]]$params, A4a$hmgoog$mod0_params)
})

test_that("hmgpt1: GPT-1 architecture + LM loss matches Python", {
  r <- morie_geron_gpt1(0:7)
  expect_equal(r$total_params, A4a$hmgpt1$total_params)
  r2 <- morie_geron_gpt1(c(0, 1, 0), logits = matrix(0, 3, 2), n_layers = 1, n_heads = 1, d_model = 2,
                         vocab_size = 2, max_len = 4)
  expect_equal(round(r2$loss, 9), A4a$hmgpt1$loss2_match_log2)
  expect_equal(r2$n_predicted, A4a$hmgpt1$n_predicted2)
  expect_equal(round(r2$perplexity, 6), A4a$hmgpt1$ppl2)
})

test_that("hmgpt2: GPT-2 released sizes match Python", {
  r <- morie_geron_gpt2(c(1, 2, 3))
  expect_equal(r$total_params, A4a$hmgpt2$total_small)
  sizes <- vapply(c("medium", "large", "xl"), function(s) morie_geron_gpt2(1, size = s)$total_params, 0)
  expect_equal(unname(sizes), A4a$hmgpt2$totals_msl_xl)
  expect_equal(r$non_embedding_params, A4a$hmgpt2$non_embedding)
})

test_that("hmgpt3: GPT-3 architecture accounting matches Python", {
  r <- morie_geron_gpt3(c(1, 2, 3), n_tokens = 5)
  expect_equal(r$total_parameters, A4a$hmgpt3$total_parameters)
  expect_equal(r$d_head, A4a$hmgpt3$d_head)
  expect_equal(r$parameters_per_layer, A4a$hmgpt3$parameters_per_layer)
  expect_equal(r$breakdown$token_embedding, A4a$hmgpt3$token_embedding)
  t <- morie_geron_gpt3(c(0, 1), n_tokens = 1, n_layers = 2, d_model = 4, n_heads = 2, vocab_size = 10, n_ctx = 8)
  expect_equal(t$parameters_per_layer, A4a$hmgpt3$toy_per_layer)
  expect_equal(t$total_parameters, A4a$hmgpt3$toy_total)
  # independent route: toy model by hand, 12*4^2 + 13*4 = 244.
  expect_equal(A4a$hmgpt3$toy_per_layer, 12 * 4^2 + 13 * 4)
})

test_that("hmgrp: Gaussian random projection matches Python's isometry-in-expectation identity", {
  r <- morie_geron_gaussian_rand_projection(matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE), d_out = 3, seed = 0)
  expect_equal(r$d_in, A4a$hmgrp$d_in)
  expect_equal(r$d_out, A4a$hmgrp$d_out)
  expect_equal(dim(r$X_projected), c(2, 3))
})

test_that("hmgrs: grid search selects on cross-validated score, matches Python", {
  X <- matrix(c(1, 1, 1, 2, 1, 3, 1, 4), 4, 2, byrow = TRUE)
  y <- c(3, 5, 7, 9)
  r <- morie_geron_grid_search(list(alpha = c(0.0, 1.0, 100.0)), X, y, K = 2)
  expect_equal(r$best_params, A4a$hmgrs$best_params)
  expect_equal(round(r$best_score, 8), A4a$hmgrs$best_score)
  expect_equal(r$n_candidates, A4a$hmgrs$n_candidates)
  expect_equal(r$n_fits, A4a$hmgrs$n_fits)
})

test_that("hmfad: forward-mode autodiff recovers x^2 -> 2x by finite-difference cross-check", {
  # Contract differs from Python's Dual class (documented in roxygen); verified
  # independently by finite differences rather than anchored against Python.
  f <- function(duals) {
    v <- duals[[1]]$value
    list(value = v^2, deriv = 2 * duals[[1]]$deriv * v)
  }
  r <- morie_geron_forward_autodiff(f, 3.0)
  expect_equal(r$value, 9.0)
  expect_equal(r$grad, 6.0, tolerance = 1e-9)
  expect_true(r$max_fd_gap < 1e-6)
})

test_that("hmencox: encoder-only transformer matches Python, block cost shared with hmdctr", {
  r <- morie_geron_encoder_only(c(1, 2, 3), n_layers = 1, n_heads = 2, d_model = 4, vocab_size = 10, max_len = 8, n_segments = 2)
  expect_equal(r$block_params, A4a$hmencox$block_params)
  expect_equal(r$seq_len, A4a$hmencox$seq_len)
  expect_equal(r$cls_index, A4a$hmencox$cls_index)
  expect_equal(r$embedding_params, A4a$hmencox$embedding_params)
  # same block_params as hmdctr's decoder block: only the mask differs.
  expect_equal(r$block_params, A4a$hmdctr$block_params)
})
