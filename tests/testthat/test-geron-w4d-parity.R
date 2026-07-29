# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Parity tests for the wave-4d hm* R port (54 modules) against A4d
# anchors generated from the Python originals (helper-geron-w4d-anchors.R).

tol <- 1e-6

test_that("hmsdp: scaled dot-product attention matches anchor", {
  r <- morie_geron_scaled_dot_product(matrix(c(0, 0), 1), matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE),
                                       matrix(c(1, 0, 0, 10), 2, 2, byrow = TRUE))
  expect_equal(as.numeric(r$attention[1, ]), A4d$hmsdp$attention_row0, tolerance = tol)
  expect_equal(as.numeric(r$Y[1, ]), A4d$hmsdp$Y_row0, tolerance = tol)
  expect_equal(r$estimate, A4d$hmsdp$estimate, tolerance = tol)
})

test_that("hmsatt: self-attention row sums to 1", {
  X <- matrix(c(1, 0, 0, 1), 2, 2, byrow = TRUE)
  I2 <- diag(2)
  r <- morie_geron_self_attention(X, I2, I2, I2)
  expect_equal(rowSums(r$attention), A4d$hmsatt$attn_row_sums, tolerance = tol)
  expect_equal(r$estimate, A4d$hmsatt$estimate, tolerance = tol)
})

test_that("hmsac: soft actor-critic converges to the paying arm", {
  bandit <- list(n_states = 1L, n_actions = 2L, reset = function() 0L,
                  step = function(a) list(0L, as.numeric(a), FALSE))
  r <- morie_geron_sac(bandit, epochs = 30, lr = 0.5, alpha = 0.05)
  expect_equal(r$policy[1, 2], A4d$hmsac$policy_1, tolerance = tol)
  expect_lt(r$entropy[30], r$entropy[1])
  expect_equal(r$estimate, A4d$hmsac$estimate, tolerance = tol)
})

test_that("hmsae: stacked autoencoder reconstructs a line exactly", {
  X <- matrix(c(0, 0, 0.5, 0.5, 1, 1, 1.5, 1.5, 2, 2), ncol = 2, byrow = TRUE)
  r <- morie_geron_stacked_autoencoder(X, hidden_sizes = c(1), epochs = 400, lr = 0.3)
  expect_equal(r$recon_error, A4d$hmsae$recon_error, tolerance = 1e-4)
})

test_that("hmself: self-supervised mask pretext solves the linear feature exactly", {
  X <- matrix(c(1, 2, 3, 2, 1, 3, 3, 5, 8, 0, 4, 4), ncol = 3, byrow = TRUE)
  r <- morie_geron_self_supervised(X, "mask")
  expect_lt(r$task_losses[3], 1e-20)
  expect_equal(r$loss, A4d$hmself$loss, tolerance = 1e-15)
})

test_that("hmsem: semisupervised with alpha=0 reduces to OLS", {
  r <- morie_geron_semisupervised(matrix(c(0, 1, 2), 3, 1), c(1, 2, 3), matrix(c(0.5, 1.5, 3), 3, 1), alpha = 0)
  expect_equal(r$theta, A4d$hmsem$theta, tolerance = 1e-6)
  expect_lt(r$sup_loss, 1e-15)
})

test_that("hmsenet: SE gate matches sigmoid(z) under identity weights", {
  I2 <- diag(2)
  r <- morie_geron_senet(c(1, 0), r = 1, W1 = I2, W2 = I2)
  expect_equal(r$s, A4d$hmsenet$s, tolerance = tol)
  expect_equal(r$n_params, A4d$hmsenet$n_params)
})

test_that("hmsent: sentiment analysis predicts correctly and softmaxes", {
  pos <- c("good", "great"); neg <- c("bad", "awful")
  model <- function(toks) {
    s <- sum(toks %in% pos) - sum(toks %in% neg)
    c(-s, s)
  }
  r <- morie_geron_sentiment_analysis(c("good great", "awful"), model, y_true = c(1L, 0L))
  expect_equal(r$predicted, A4d$hmsent$predicted)
  expect_equal(r$accuracy, A4d$hmsent$accuracy, tolerance = tol)
  expect_equal(as.numeric(r$probabilities[2, ]), A4d$hmsent$prob_1, tolerance = tol)
})

test_that("hmseq2: seq2seq with uniform decoder gives loss log(V)", {
  z_of <- function(s) sum(unlist(s))
  dec <- function(z, prefix) c(0, 0, 0)
  r <- morie_geron_seq2seq(c(1, 2), c(1L, 2L), z_of, dec)
  expect_equal(r$loss, A4d$hmseq2$loss, tolerance = tol)
  expect_equal(r$greedy, A4d$hmseq2$greedy)
})

test_that("hmsft: SFT starts at log(2) and drives loss to accuracy 1", {
  data <- list(list("translate hello", "bonjour"), list("summarise text", "resume"))
  r <- morie_geron_sft(NULL, data, epochs = 300, lr = 0.5)
  expect_equal(r$loss_curve[1], A4d$hmsft$loss_curve0, tolerance = 1e-8)
  expect_equal(r$accuracy, A4d$hmsft$accuracy, tolerance = tol)
})

test_that("hmsgdc: one-step hinge update matches lr*y*x", {
  r <- morie_geron_sgd_classifier(matrix(c(1, 0), 1, 2), 1, lr = 0.1, n_iter = 1, alpha = 0, shuffle = FALSE)
  expect_equal(r$w, A4d$hmsgdc$w, tolerance = tol)
  expect_equal(r$b, A4d$hmsgdc$b, tolerance = tol)
})

test_that("hmsil: silhouette matches the two-tight-pairs anchor", {
  r <- morie_geron_silhouette(matrix(c(0, 0.1, 10, 10.1), 4, 1), c(0, 0, 1, 1))
  expect_equal(r$samples[1], A4d$hmsil$sample0, tolerance = tol)
  expect_equal(r$silhouette, A4d$hmsil$silhouette, tolerance = tol)
})

test_that("hmsslc: semisupervised cluster label propagation matches anchor", {
  X <- matrix(c(0, 0.1, 0.2, 9.8, 9.9, 10.0), 6, 1)
  r <- morie_geron_semisupervised_cluster(X, matrix(c(0.05, 9.95), 2, 1), c(0L, 1L), n_clusters = 2)
  expect_equal(as.integer(r$labels), A4d$hmsslc$labels)
})

test_that("hmstk: stacking blender beats the mean model out-of-fold", {
  X <- matrix(1:6, 6, 1); y <- c(2, 4, 6, 8, 10, 12)
  mean_model <- function(Xtr, ytr, Xte) rep(mean(ytr), nrow(as.matrix(Xte)))
  ols_model <- function(Xtr, ytr, Xte) {
    P <- cbind(1, as.matrix(Xtr)); Q <- cbind(1, as.matrix(Xte))
    theta <- .morie_gr_lstsq(P, as.numeric(ytr))
    as.numeric(Q %*% theta)
  }
  r <- morie_geron_stacking(X, y, list(mean_model, ols_model), k_folds = 3)
  expect_lt(r$stacked_mse, 1e-10)
  expect_equal(r$oof_mse, A4d$hmstk$oof_mse, tolerance = 1e-6)
})

test_that("hmstr: stratified sampling largest-remainder allocation matches anchor", {
  X <- matrix(0:5, 6, 1)
  r <- morie_geron_stratified_sampling(X, c(0, 0, 0, 0, 1, 1), n_total = 3)
  expect_equal(sort(as.integer(r$indices)), sort(A4d$hmstr$indices))
  expect_equal(r$max_share_error, A4d$hmstr$max_share_error, tolerance = tol)
})

test_that("hmstr2: stride arithmetic (AlexNet layer 1)", {
  r <- morie_geron_stride(227, 11, 0, 4)
  expect_equal(r$output_dim, A4d$hmstr2$output_dim)
})

test_that("hmsup: supervised learning recovers exact linear fit with zero LOO risk", {
  X <- matrix(1:5, 5, 1)
  r <- morie_geron_supervised_learning(X, c(3, 5, 7, 9, 11))
  expect_equal(r$theta, A4d$hmsup$theta, tolerance = 1e-6)
  expect_lt(r$loo_risk, 1e-15)
})

test_that("hmsvdp: SVD pseudoinverse OLS matches anchor", {
  r <- morie_geron_svd_pseudoinverse(matrix(c(1, 0, 0, 1, 1, 1), 3, 2, byrow = TRUE), c(1, 1, 2))
  expect_equal(r$theta, A4d$hmsvdp$theta, tolerance = tol)
  expect_equal(r$rank, A4d$hmsvdp$rank)
})

test_that("hmsvm2: state-dict round trip is exact", {
  d <- tempfile("state_"); dir.create(d)
  sd <- list(w1 = matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE), b1 = c(0.5, -0.5))
  r <- morie_geron_save_load_pytorch(sd, file.path(d, "state"))
  expect_true(r$exact)
  expect_equal(r$n_params, A4d$hmsvm2$n_params)
  expect_equal(r$max_diff, 0.0)
})

test_that("hmswin: Swin transformer window count and shift schedule match anchor", {
  img <- matrix(0:15, 4, 4, byrow = TRUE)
  r <- morie_geron_swin(img, window_size = 2, n_layers = 1, d_model = 4)
  expect_equal(r$n_windows, A4d$hmswin$n_windows)
  expect_equal(r$window_tokens, A4d$hmswin$window_tokens)
  expect_equal(r$shifted_layers, A4d$hmswin$shifted_layers)
})

test_that("hmsymd: symbolic diff of x^2 is 2*x, chain rule checks vs finite diff", {
  r <- morie_geron_symbolic_diff("x^2", "x")
  expect_equal(r$derivative, A4d$hmsymd$derivative)
  r2 <- morie_geron_symbolic_diff("exp(2*x)", "x", at = list(x = 0.5))
  expect_equal(r2$value, A4d$hmsymd$value, tolerance = 1e-6)
  expect_lt(r2$error, 1e-6)
})

test_that("hmt5: span corruption is a lossless round trip", {
  r <- morie_geron_t5(strsplit("the quick brown fox jumps over the lazy dog", " ")[[1]],
                       noise_density = 0.3, mean_span = 2, seed = 0)
  expect_true(r$lossless)
  expect_equal(r$sentinels, A4d$hmt5$sentinels)
  expect_equal(r$n_masked, A4d$hmt5$n_masked)
})

test_that("hmtd3: TD3 learns to prefer the paying action", {
  bandit <- list(n_states = 1L, n_actions = 2L, reset = function() 0L,
                  step = function(a) list(0L, as.numeric(a), FALSE))
  r <- morie_geron_td3(bandit, epochs = 40)
  expect_equal(as.integer(r$policy[1]), A4d$hmtd3$policy_0)
  expect_equal(r$policy_updates, A4d$hmtd3$policy_updates)
  expect_true(r$overestimation_gap >= 0.0)
})

test_that("hmtfl: transfer learning keeps the frozen layer bit-identical", {
  W0 <- matrix(c(0.5, -0.5, 0.5, 0.5), 2, 2, byrow = TRUE)
  W1 <- matrix(c(1, 1), 2, 1)
  X <- matrix(c(1, 0, 0, 1, 1, 1, 2, 1), 4, 2, byrow = TRUE)
  y <- c(1, 2, 3, 4)
  r <- morie_geron_transfer_learning(list(W0, W1), X, y, n_frozen = 1, epochs = 300, lr = 0.1)
  expect_equal(r$weights[[1]], W0)
  expect_false(isTRUE(all.equal(r$weights[[2]], W1)))
  expect_lt(r$final_loss, r$initial_loss)
  expect_equal(r$trainable_params, A4d$hmtfl$trainable_params)
  expect_equal(r$total_params, A4d$hmtfl$total_params)
})

test_that("hmtsc: TorchScript trace/replay matches anchor and rejects wrong shapes", {
  W <- diag(2)
  r <- morie_geron_torchscript(list(list(kind = "linear", param = W), list(kind = "relu")),
                                matrix(c(1, -1), 1, 2))
  expect_equal(r$n_nodes, A4d$hmtsc$n_nodes)
  expect_equal(as.numeric(r$output[1, ]), A4d$hmtsc$output, tolerance = tol)
  expect_error(morie_geron_run_graph(r$graph, matrix(c(1, 2, 3), 1, 3)))
})

test_that("hmtfm: transformer encoder parameter count and attention rows sum to 1", {
  X <- matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0), 3, 4, byrow = TRUE)
  r <- morie_geron_transformer(X, n_heads = 2, n_layers = 1)
  expect_equal(r$total_params, A4d$hmtfm$total_params)
  expect_equal(rowSums(r$attention[[1]][1, , ]), A4d$hmtfm$attn_row0_sums, tolerance = tol)
})

test_that("hmtpp: tensor parallelism column scheme matches unsharded reference", {
  W <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), 2, 4, byrow = TRUE)
  r <- morie_geron_tensor_parallelism(W, 2, x = matrix(c(1, 1), 1, 2))
  expect_equal(as.numeric(r$output[1, ]), A4d$hmtpp$output, tolerance = tol)
  expect_lt(r$max_diff, 1e-10)
  expect_equal(r$comm_elements, A4d$hmtpp$comm_elements)
})

test_that("hmtrlf: DPO loss starts at log(2) and margin grows", {
  pairs <- list(list(c(1, 0), c(0, 1)), list(c(1, 1), c(0, 1)))
  r <- morie_geron_trl_finetune(NULL, pairs, method = "dpo", epochs = 300, lr = 0.5, beta = 1.0)
  expect_equal(r$loss_curve[1], A4d$hmtrlf$loss_curve0, tolerance = 1e-8)
  expect_lt(r$loss_curve[length(r$loss_curve)], r$loss_curve[1])
  expect_gt(r$margin, 0)
})

test_that("hmtcmp: torch.compile fuses linear chains and preserves output", {
  A2 <- diag(2) * 2; B2 <- diag(2) * 3; C2 <- diag(2) * 5
  r <- morie_geron_torch_compile(
    list(list(kind = "linear", param = A2), list(kind = "linear", param = B2), list(kind = "linear", param = C2)),
    example_inputs = matrix(c(1, 1), 1, 2)
  )
  expect_equal(r$n_ops, A4d$hmtcmp$n_ops)
  expect_equal(r$n_compiled, A4d$hmtcmp$n_compiled)
  expect_equal(as.numeric(r$output[1, ]), A4d$hmtcmp$output, tolerance = tol)
})

test_that("hmuns: unsupervised learning finds the two groups exactly", {
  X <- matrix(c(0, 0, 0.2, 0.2, 5, 5, 5.2, 5.2), 4, 2, byrow = TRUE)
  r <- morie_geron_unsupervised_learning(X, n_clusters = 2, bottleneck = 1)
  expect_equal(as.integer(r$labels), A4d$hmuns$labels)
  expect_lt(r$recon_error, 1e-10)
})

test_that("hmunsp: pretrained encoder reconstructs the line exactly", {
  Xu <- matrix(c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5), 6, 2, byrow = TRUE)
  Xl <- matrix(c(0, 0, 2, 2, 4, 4, 5, 5), 4, 2, byrow = TRUE)
  r <- morie_geron_unsupervised_pretraining(Xu, Xl, c(0, 2, 4, 5), bottleneck = 1)
  expect_lt(r$recon_error, 1e-10)
  expect_lt(r$pretrained_loo, 1e-15)
})

test_that("hmvgr: vanishing gradients geometric ratio matches anchor", {
  r <- morie_geron_vanishing_gradients(list(1e-6, 1e-4, 1e-2, 1.0))
  expect_equal(r$ratios, A4d$hmvgr$ratios, tolerance = 1e-6)
  expect_equal(r$geometric_ratio, A4d$hmvgr$geometric_ratio, tolerance = 1e-6)
  expect_true(r$vanishing)
})

test_that("hmvae: VAE loss falls and KL is non-negative", {
  X <- matrix(c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4), 5, 2, byrow = TRUE)
  r <- morie_geron_vae(X, latent_dim = 1, epochs = 300, lr = 0.05)
  expect_equal(r$loss_curve[1], A4d$hmvae$loss_curve0, tolerance = 1e-6)
  expect_lt(r$loss_curve[300], r$loss_curve[1])
  expect_true(r$kl >= 0)
})

test_that("hmvbgm: VBGMM prunes the surplus component", {
  X <- matrix(c(0, 0.1, 0.2, 9.8, 9.9, 10.0), 6, 1)
  r <- morie_geron_variational_bayes_gmm(X, n_components = 3, alpha0 = 1e-3, max_iter = 200)
  expect_equal(r$n_effective, A4d$hmvbgm$n_effective)
  expect_equal(sum(r$weights), 1.0, tolerance = tol)
})

test_that("hmvbrt: VideoBERT attention rows sum to 1 and shapes match", {
  r <- morie_geron_videobert(c(0L, 1L, 2L, 1L), c(0L, 1L), d_model = 4)
  expect_equal(r$n_video, A4d$hmvbrt$n_video)
  expect_equal(r$n_text, A4d$hmvbrt$n_text)
  expect_equal(rowSums(r$attention), A4d$hmvbrt$attn_row_sums, tolerance = tol)
})

test_that("hmvilb: ViLBERT co-attention shapes and row sums match", {
  img <- matrix(c(1, 0, 0, 1, 1, 1), 3, 2, byrow = TRUE)
  r <- morie_geron_vilbert(img, c(0L, 1L), d_model = 4)
  expect_equal(r$n_regions, A4d$hmvilb$n_regions)
  expect_equal(r$n_tokens, A4d$hmvilb$n_tokens)
  expect_equal(rowSums(r$attention_v2t), A4d$hmvilb$attn_v2t_row_sums, tolerance = tol)
})

test_that("hmvit: ViT patch/sequence bookkeeping matches anchor", {
  img <- matrix(0:15, 4, 4, byrow = TRUE)
  r <- morie_geron_vision_transformer(img, patch_size = 2, n_layers = 1, d_model = 4, n_heads = 2, n_classes = 3)
  expect_equal(r$n_patches, A4d$hmvit$n_patches)
  expect_equal(r$seq_len, A4d$hmvit$seq_len)
  expect_equal(r$patch_dim, A4d$hmvit$patch_dim)
  expect_equal(r$total_params, A4d$hmvit$total_params)
})

test_that("hmvqv: VQ-VAE learns two distinct codes and reduces loss", {
  X <- matrix(c(0, 0, 0.1, 0.1, 5, 5, 5.1, 5.1), 4, 2, byrow = TRUE)
  r <- morie_geron_vq_vae(X, codebook_size = 2, latent_dim = 1, epochs = 400, lr = 0.05)
  expect_equal(length(unique(r$indices)), A4d$hmvqv$n_codes_used)
  expect_lt(r$loss_curve[400], r$loss_curve[1])
  expect_true(r$perplexity >= 1.0 && r$perplexity <= 2.0)
})

test_that("hmwemb: word embedding table is a unit lookup with cosine diag 1", {
  r <- morie_geron_word_embeddings(c("cat", "dog", "the"), d = 4)
  expect_equal(r$n_params, A4d$hmwemb$n_params)
  expect_equal(diag(r$similarity), A4d$hmwemb$sim_diag, tolerance = tol)
})

test_that("hmwpt: WordPiece learns '##u' early on the hug/hugs corpus", {
  r <- morie_geron_wordpiece_tokenizer("hug hug hugs pug pun", vocab_size = 14)
  expect_equal("##u" %in% r$alphabet, A4d$hmwpt$has_double_u)
  expect_true(length(r$vocab) <= 14)
})

test_that("hmwrst: warm restarts cosine schedule matches anchor", {
  r <- morie_geron_warm_restarts(c(0, 5, 10), T0 = 10, factor = 2.0, eta_max = 0.1)
  expect_equal(r$eta, A4d$hmwrst$eta, tolerance = tol)
  expect_equal(r$cycle, A4d$hmwrst$cycle)
})

test_that("hmxcpt: Xception parameter count matches the ImageNet reference", {
  r <- morie_geron_xception(1000)
  expect_equal(r$trainable_params, A4d$hmxcpt$trainable_params)
  expect_equal(r$total_params, A4d$hmxcpt$total_params)
  expect_equal(r$n_separable, A4d$hmxcpt$n_separable)
})

test_that("hmxgb: XGBoost single stump recovers group means exactly", {
  r <- morie_geron_xgboost(matrix(0:3, 4, 1), c(1, 1, 10, 10), n_estimators = 1, learning_rate = 1.0,
                            max_depth = 1, reg_lambda = 0.0)
  expect_equal(r$predicted, A4d$hmxgb$predicted, tolerance = tol)
  expect_equal(r$base_score, A4d$hmxgb$base_score, tolerance = tol)
  expect_equal(r$trees[[1]]$gain, A4d$hmxgb$tree0_gain, tolerance = tol)
  expect_equal(r$trees[[1]]$left$weight, A4d$hmxgb$tree0_left_weight, tolerance = tol)
})

test_that("hmxgr: exploding gradients geometric ratio matches anchor", {
  r <- morie_geron_exploding_gradients(list(1000, 100, 10, 1))
  expect_equal(r$ratios, A4d$hmxgr$ratios, tolerance = tol)
  expect_equal(r$geometric_ratio, A4d$hmxgr$geometric_ratio, tolerance = tol)
  expect_true(r$exploding)
})

test_that("hmxln: XLNet permutation and masks satisfy the invariants", {
  r <- morie_geron_xlnet(c(0L, 1L, 2L, 1L), n_layers = 1, vocab_size = 3)
  expect_equal(sort(as.integer(r$permutation)), A4d$hmxln$permutation_sorted)
  expect_equal(sum(diag(r$query_mask)), A4d$hmxln$query_diag_sum)
  expect_equal(sum(diag(r$content_mask)), A4d$hmxln$content_diag_sum)
  expect_lt(r$total_logprob, 0)
})

test_that("hmyolo: YOLO decode + NMS keeps two non-overlapping detections", {
  model <- function(x) {
    p <- array(0.0, dim = c(2, 2, 7))
    p[1, 1, 1:5] <- c(0.5, 0.5, 0.5, 0.5, 1.0)
    p[1, 1, 6] <- 1.0
    p[2, 2, 1:5] <- c(0.5, 0.5, 0.5, 0.5, 0.8)
    p[2, 2, 7] <- 1.0
    p
  }
  r <- morie_geron_yolo(NULL, model)
  expect_equal(r$n_detections, A4d$hmyolo$n_detections)
  expect_equal(as.numeric(r$boxes[1, ]), A4d$hmyolo$box0, tolerance = tol)
  expect_equal(as.integer(r$classes), A4d$hmyolo$classes)
})

test_that("hmzsl: zero-shot scoring matches the softmax anchor", {
  scores <- c(negative = 0.0, positive = 1.0)
  r <- morie_geron_zero_shot(function(p) scores, "Review: it was great. Sentiment:")
  expect_equal(r$predicted_label, A4d$hmzsl$predicted_label)
  expect_equal(r$probabilities, A4d$hmzsl$probabilities, tolerance = tol)
})

test_that("hmspcl: spectral clustering finds 2 components and groups", {
  X <- matrix(c(0, 0.2, 10, 10.2), 4, 1)
  r <- morie_geron_spectral_clustering(X, 2)
  expect_equal(r$n_components, A4d$hmspcl$n_components)
  expect_equal(r$labels[1] == r$labels[2], A4d$hmspcl$same_group)
})

test_that("hmsrnn: simple RNN forward pass matches anchor", {
  r <- morie_geron_simple_rnn(matrix(c(1, 0), 2, 1), matrix(1, 1, 1), matrix(1, 1, 1))
  expect_equal(as.numeric(r$H), A4d$hmsrnn$H, tolerance = tol)
  expect_equal(r$h_T, A4d$hmsrnn$h_T, tolerance = tol)
})

test_that("hmsrp: dense sparse-random-projection entries are +-1/sqrt(d_out)", {
  X <- diag(3)
  r <- morie_geron_sparse_rand_projection(X, 2, density = 1.0, seed = 7)
  expect_equal(sort(unique(round(abs(as.numeric(r$R)), 12))), unique(A4d$hmsrp$R_flat), tolerance = tol)
  expect_equal(r$nnz, A4d$hmsrp$nnz)
})

test_that("hmssg: semantic segmentation IoU/accuracy match the half-and-half anchor", {
  model <- function(x) {
    s <- array(0.0, dim = c(2, 2, 2))
    s[, 1, 1] <- 1.0
    s[, 2, 2] <- 1.0
    s
  }
  r <- morie_geron_semantic_segmentation(matrix(0, 2, 2), model, y_true = matrix(0L, 2, 2))
  expect_equal(as.integer(t(r$labels)), A4d$hmssg$labels)
  expect_equal(r$pixel_accuracy, A4d$hmssg$pixel_accuracy, tolerance = tol)
  expect_equal(r$iou[1], A4d$hmssg$iou0, tolerance = tol)
})

test_that("hmtsf: lag-window forecast extrapolates the linear ramp exactly", {
  r <- morie_geron_time_series_forecast(1:8, horizon = 3, window = 2)
  expect_equal(r$forecast, A4d$hmtsf$forecast, tolerance = 1e-4)
  expect_lt(r$train_mse, 1e-10)
})

test_that("hmtsne: t-SNE KL falls and P sums to 1", {
  X <- matrix(c(0, 0.1, 0.2, 10, 10.1, 10.2), 6, 1)
  r <- morie_geron_tsne(X, n_components = 1, perplexity = 2.0, n_iter = 250)
  expect_equal(r$kl_curve[1], A4d$hmtsne$kl_curve0, tolerance = 1e-4)
  expect_lt(r$kl_curve[length(r$kl_curve)], r$kl_curve[1])
  expect_equal(sum(r$P), A4d$hmtsne$P_sum, tolerance = tol)
})

test_that("hmumap: UMAP cross-entropy falls and rho matches nearest-neighbour distance", {
  X <- matrix(c(0, 0.1, 0.2, 10, 10.1, 10.2), 6, 1)
  r <- morie_geron_umap(X, n_components = 1, n_neighbors = 2, n_iter = 200)
  expect_equal(r$rho[1], A4d$hmumap$rho0, tolerance = tol)
  expect_lt(r$ce_curve[length(r$ce_curve)], r$ce_curve[1])
})
