# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Parity tests for wave-4c (57 hm* modules, rw4_c.txt), against A4c
# anchors captured from the canonical Python (helper-geron-w4c-anchors.R).
# One exact-anchor test per module plus a few cheap independent routes
# (shape/property checks that don't depend on the anchor numbers).

test_that("hmmxp2: mixed precision matches Python", {
  r <- morie_geron_mixed_precision(c(1.0, 2.0), loss_scale = 1024.0, grads = c(1e-8, 1.0))
  expect_equal(r$n_underflow, A4c$hmmxp2$n_underflow)
  expect_equal(r$overflow, A4c$hmmxp2$overflow)
  expect_equal(r$max_safe_loss_scale, A4c$hmmxp2$max_safe_loss_scale, tolerance = 1e-6)
  expect_equal(r$recommended_loss_scale, A4c$hmmxp2$recommended_loss_scale, tolerance = 1e-6)
  expect_equal(r$memory_bytes_fp32, A4c$hmmxp2$memory_bytes_fp32)
})

test_that("hmncsn: NCSN analytic score matches Python", {
  X <- matrix(c(-2, -1, 0, 1, 2), ncol = 1)
  r <- morie_geron_ncsn(X, sigmas = 1.0, epochs = 600)
  expect_equal(round(r$analytic[[1]]$W[1, 1], 6), A4c$hmncsn$analytic_W)
  expect_true(r$loss_history[[1]][length(r$loss_history[[1]])] < r$loss_history[[1]][1])
})

test_that("hmnmd: numerical derivative matches Python", {
  r <- morie_geron_numerical_diff(function(x) x^3, 2.0)
  expect_equal(round(r$derivative, 6), A4c$hmnmd$derivative)
  expect_equal(r$n_evals, A4c$hmnmd$n_evals)
})

test_that("hmnmf: NMF converges with non-negative factors", {
  r <- morie_geron_nmf(matrix(c(1, 2, 3, 2, 4, 6), ncol = 2), 1)
  expect_true(r$relative_error < 1e-3)
  expect_true(all(r$W >= 0) && all(r$H >= 0))
})

test_that("hmnmt: seq2seq teacher-forced loss matches Python", {
  model <- list(encode = function(s) length(s), decode = function(z, prefix) c(0.2, 0.5, 0.3))
  r <- morie_geron_encoder_decoder_nmt(c(9, 9), c(1, 1), model)
  expect_equal(round(r$token_losses, 6), A4c$hmnmt$token_losses)
  expect_equal(round(r$loss, 6), A4c$hmnmt$loss)
  expect_equal(r$greedy, A4c$hmnmt$greedy)
})

test_that("hmnov: novelty detection matches Python", {
  train <- matrix(c(0.0, 0.1, -0.1, 0.05, -0.05), ncol = 1)
  r <- morie_geron_novelty_detection(train, matrix(c(0.0, 10.0), ncol = 1))
  expect_equal(r$is_novel, A4c$hmnov$is_novel)
  expect_equal(r$novel_fraction, A4c$hmnov$novel_fraction)
})

test_that("hmnsp: NSP assembly and logit match Python", {
  r <- morie_geron_next_sentence_prediction(c("the", "cat", "sat"), c("the", "cat", "sat"))
  expect_equal(r$tokens, A4c$hmnsp$tokens)
  expect_equal(r$segment_ids, A4c$hmnsp$segment_ids)
  expect_equal(round(r$logit, 6), A4c$hmnsp$logit)
  expect_equal(round(r$probability, 6), A4c$hmnsp$probability)
})

test_that("hmocsv: one-class SVM flags the far point", {
  X <- matrix(c(0.0, 0.05, -0.05, 0.1, -0.1, 5.0), ncol = 1)
  r <- morie_geron_one_class_svm(X, nu = 0.5, gamma = 1.0)
  expect_equal(which.min(r$decision) - 1L, A4c$hmocsv$argmin_decision)
  expect_true(r$outlier_fraction <= 0.5 + 1e-9)
})

test_that("hmonl: online learning matches Python", {
  r <- morie_geron_online_learning(matrix(c(1.0, 1.0), ncol = 1), c(1.0, 1.0), eta = 0.5)
  expect_equal(r$losses, A4c$hmonl$losses)
  expect_equal(r$theta[1], A4c$hmonl$theta0)
})

test_that("hmonnx: export trace shapes match Python", {
  m <- list(list(op = "Gemm", in_features = 3, out_features = 2), list(op = "Relu"))
  r <- morie_geron_onnx_export(m, matrix(0.0, 1, 3))
  expect_equal(r$input_shape, A4c$hmonnx$input_shape)
  expect_equal(r$output_shape, A4c$hmonnx$output_shape)
  expect_equal(r$n_parameters, A4c$hmonnx$n_parameters)
})

test_that("hmoob: OOB score matches Python", {
  f <- function(A) as.numeric(A[, 1])
  r <- morie_geron_oob_score(matrix(c(0.0, 1.0), ncol = 1), c(0, 1),
                             list(list(f, c(TRUE, FALSE)), list(f, c(FALSE, TRUE))))
  expect_equal(r$oob_score, A4c$hmoob$oob_score)
})

test_that("hmopt: OPTICS clusters match Python", {
  X <- matrix(c(0.0, 0.1, 0.2, 10.0, 10.1, 10.2), ncol = 1)
  r <- morie_geron_optics(X, min_samples = 2, eps_cluster = 1.0)
  expect_equal(r$n_clusters, A4c$hmopt$n_clusters)
  expect_equal(sort(unique(r$labels)), A4c$hmopt$labels_set)
  expect_equal(round(r$core_distances[1], 6), A4c$hmopt$core0)
})

test_that("hmosf: one-shot prompt matches Python", {
  copy <- function(prompt) prompt[[1]][[2]]
  r <- morie_geron_one_shot(copy, list("hello", "greeting"), "goodbye")
  expect_equal(r$prediction, A4c$hmosf$prediction)
  expect_equal(r$prompt_text, A4c$hmosf$prompt_text)
})

test_that("hmovo: one-vs-one voting matches Python", {
  X <- matrix(c(0, 1, 5, 6, 10, 11), ncol = 1)
  r <- morie_geron_one_vs_one_hm(X, c(0, 0, 1, 1, 2, 2))
  expect_equal(r$n_classifiers, A4c$hmovo$n_classifiers)
  expect_equal(r$accuracy, A4c$hmovo$accuracy)
  expect_equal(as.integer(r$predict(matrix(c(0.5, 10.5), ncol = 1))), A4c$hmovo$predict_new)
})

test_that("hmovr: one-vs-rest matches Python", {
  X <- matrix(c(0, 1, 5, 6, 10, 11), ncol = 1)
  r <- morie_geron_one_vs_rest_hm(X, c(0, 0, 1, 1, 2, 2))
  expect_equal(r$accuracy, A4c$hmovr$accuracy)
  expect_equal(round(r$positive_rate, 6), A4c$hmovr$positive_rate)
})

test_that("hmpas: pasting matches Python", {
  const <- function(Xb, yb) function(A) rep(2.0, nrow(as.matrix(A)))
  r <- morie_geron_pasting(matrix(c(1.0, 2.0), ncol = 1), c(1.0, 3.0), const, 4, sample_size = 1, seed = 3)
  expect_equal(r$predict(matrix(c(1.0, 2.0), ncol = 1)), A4c$hmpas$predict)
  expect_equal(r$train_mse, A4c$hmpas$train_mse)
})

test_that("hmpcac: PCA matches Python", {
  X <- matrix(c(-2, 0, 2, 0, 0, 0), ncol = 2)
  r <- morie_geron_principal_components(X, 2)
  expect_equal(round(r$explained_variance_ratio, 12), A4c$hmpcac$ratio)
  expect_equal(round(r$components[, 1], 12), A4c$hmpcac$comp0)
})

test_that("hmpcav: PCA variance matches Python", {
  X <- matrix(c(-2, 0, 2, 0, 0, 0), ncol = 2)
  r <- morie_geron_pca_variance(X)
  expect_equal(round(r$top_variance, 12), A4c$hmpcav$top_variance)
  expect_equal(r$n_components_for_threshold, A4c$hmpcav$n_components_for_threshold)
})

test_that("hmpd: padding matches Python", {
  r <- morie_geron_padding(matrix(1.0, 1, 1), 1, 1)
  expect_equal(dim(r$padded), A4c$hmpd$shape)
  expect_equal(sum(r$padded), A4c$hmpd$sum)
  expect_equal(r$padded[2, 2], A4c$hmpd$center)
})

test_that("hmpemb: pretrained embeddings match Python", {
  r <- morie_geron_pretrained_embeddings(c("cat", "zzz"), list(cat = c(1.0, 0.0)))
  expect_equal(r$embeddings[1, ], A4c$hmpemb$emb0)
  expect_equal(r$coverage, A4c$hmpemb$coverage)
  expect_equal(r$oov, A4c$hmpemb$oov)
})

test_that("hmper: prioritized replay matches Python", {
  r <- morie_geron_prioritized_replay(c(1.0, 1.0, 2.0), alpha = 1.0, beta = 1.0, eps = 0.0)
  expect_equal(r$probabilities, A4c$hmper$probabilities)
  expect_equal(r$weights, A4c$hmper$weights)
})

test_that("hmpg: policy gradient matches Python", {
  g <- function(s, a) c(1.0, 0.0)
  r <- morie_geron_policy_gradient(list(list(list(0, 0, 2.0))), g, gamma = 1.0)
  expect_equal(r$gradient, A4c$hmpg$gradient)
})

test_that("hmphp: peephole LSTM matches Python", {
  W <- list(W_x = matrix(c(0, 0, 1, 0), ncol = 1), W_h = matrix(0, 4, 1), b = rep(0, 4))
  r <- morie_geron_peephole_lstm(1.0, 0.0, 0.0, W)
  expect_equal(round(r$g[1], 6), A4c$hmphp$g0)
  expect_equal(round(r$c[1], 6), A4c$hmphp$c0)
  expect_equal(round(r$h[1], 6), A4c$hmphp$h0)
})

test_that("hmplf: polynomial features match Python", {
  r <- morie_geron_polynomial_features_hm(matrix(c(2.0, 3.0), ncol = 2), 2)
  expect_equal(r$features[1, ], A4c$hmplf$features)
  expect_equal(r$names, A4c$hmplf$names)
})

test_that("hmpmps: MPS placement plan matches Python", {
  r <- morie_geron_mps_acceleration(c(1.0 / 3.0, 2.0 / 3.0))
  expect_equal(r$source_dtype, A4c$hmpmps$source_dtype)
  expect_equal(r$dtype_on_device, A4c$hmpmps$dtype_on_device)
})

test_that("hmpol: policy entropy matches Python", {
  r <- morie_geron_policy(0, matrix(c(0.25, 0.75), nrow = 1))
  expect_equal(r$probabilities, A4c$hmpol$probabilities)
  expect_equal(r$greedy_action, A4c$hmpol$greedy_action)
  expect_equal(round(r$entropy, 6), A4c$hmpol$entropy)
})

test_that("hmppo: PPO learns the better action", {
  reset <- function() 0
  step <- function(a) list(0, as.numeric(a), TRUE)
  r <- morie_geron_ppo(list(reset = reset, step = step), matrix(c(0.0, 0.0), nrow = 1), epochs = 30, lr = 0.5, seed = 1)
  expect_true(r$probabilities[1, 2] > 0.9)
  expect_true(r$return_history[length(r$return_history)] >= r$return_history[1])
})

test_that("hmppp: pipeline schedule matches Python", {
  r <- morie_geron_pipeline_parallelism(list(1, 1, 1, 1), 4, n_microbatches = 4)
  expect_equal(round(r$bubble_fraction, 6), A4c$hmppp$bubble_fraction)
  expect_equal(r$n_slots, A4c$hmppp$n_slots)
  expect_equal(r$schedule, A4c$hmppp$schedule)
})

test_that("hmprc: PR curve average precision matches Python", {
  r <- morie_geron_precision_recall_curve_hm(c(0, 1), c(0.1, 0.9))
  expect_equal(r$average_precision, A4c$hmprc$average_precision)
  r2 <- morie_geron_precision_recall_curve_hm(c(1, 0), c(0.1, 0.9))
  expect_equal(r2$average_precision, A4c$hmprc$ap_reversed)
  expect_equal(round(r2$best_f1, 6), A4c$hmprc$best_f1_reversed)
})

test_that("hmprcv: perceiver cross-attention matches Python", {
  r <- morie_geron_perceiver(matrix(c(1, 0, 0, 1), ncol = 2, byrow = TRUE), matrix(c(1, 0), nrow = 1), n_iter = 1)
  expect_equal(dim(r$attention), A4c$hmprcv$attn_shape)
  expect_equal(round(sum(r$attention), 12), A4c$hmprcv$attn_sum)
  expect_equal(r$attention_cost, A4c$hmprcv$cost)
})

test_that("hmpre: precision matches Python", {
  r <- morie_geron_precision_hm(c(1, 0, 1, 1, 0), c(1, 1, 1, 0, 0))
  expect_equal(round(r$precision, 6), A4c$hmpre$precision)
  expect_equal(r$tp, A4c$hmpre$tp)
  expect_equal(r$fp, A4c$hmpre$fp)
})

test_that("hmprel: PReLU matches Python", {
  r <- morie_geron_prelu(c(-2.0, 3.0), 0.25)
  expect_equal(r$a, A4c$hmprel$a)
  expect_equal(r$grad_z, A4c$hmprel$grad_z)
  expect_equal(r$grad_alpha, A4c$hmprel$grad_alpha)
})

test_that("hmprio: Perceiver IO matches Python", {
  x <- matrix(c(1, 0, 0, 1, 1, 1), ncol = 2, byrow = TRUE)
  r <- morie_geron_perceiver_io_hm(x, matrix(c(1, 0, 0, 1), ncol = 2, byrow = TRUE), matrix(c(1, 0, 0, 1, 1, 1), ncol = 2, byrow = TRUE))
  expect_equal(dim(r$outputs), A4c$hmprio$outputs_shape)
  expect_equal(round(rowSums(r$decoder_attention), 12), A4c$hmprio$attn_row_sums)
})

test_that("hmpru: weight pruning matches Python", {
  r <- morie_geron_weight_pruning_hm(c(1.0, -2.0, 3.0, -4.0), 0.5)
  expect_equal(r$pruned, A4c$hmpru$pruned)
  expect_equal(r$threshold, A4c$hmpru$threshold)
  expect_equal(r$n_pruned, A4c$hmpru$n_pruned)
})

test_that("hmptq: static PTQ matches Python", {
  r <- morie_geron_static_quantization_ptq(c(-1.0, 0.5, 1.0), c(-1.0, 0.0, 1.0))
  expect_equal(round(r$activation_scale, 9), A4c$hmptq$activation_scale)
  expect_equal(r$zero_point, A4c$hmptq$zero_point)
  expect_equal(r$quantized_weights, A4c$hmptq$quantized_weights)
})

test_that("hmpttn: tensor construction matches Python", {
  r <- morie_geron_pytorch_tensor(matrix(c(1.0, 3.0, 2.0, 4.0), 2, 2))
  expect_equal(r$dtype, A4c$hmpttn$dtype)
  expect_equal(r$shape, A4c$hmpttn$shape)
  expect_equal(r$nbytes, A4c$hmpttn$nbytes)
})

test_that("hmpvt: PVT patch embedding matches Python", {
  img <- array(0:47, dim = c(3, 4, 4))
  img <- aperm(img, c(3, 2, 1))  # numpy arange(48).reshape(4,4,3), row-major
  Wm <- matrix(1 / 12, 12, 1)
  r <- morie_geron_pvt(img, list(list(patch_size = 2, dim = 1, W = Wm)))
  expect_equal(r$output_shape, A4c$hmpvt$output_shape)
  expect_equal(r$tokens[1, 1, 1], A4c$hmpvt$token00)
})

test_that("hmqat: QAT matches Python", {
  r <- morie_geron_quantization_aware_training_hm(0.0, matrix(c(1.0, 2.0, 3.0), ncol = 1), c(2.0, 4.0, 6.0), epochs = 300, lr = 0.05)
  expect_true(abs(r$quantized_weights[1] - 2.0) < 0.05)
  expect_true(r$loss < 1e-3)
})

test_that("hmrad: reverse autodiff matches Python", {
  r <- morie_geron_reverse_autodiff(function(v) v[[1]] * v[[2]], c(3.0, 4.0))
  expect_equal(r$value, A4c$hmrad$value)
  expect_equal(r$gradient, A4c$hmrad$gradient)
})

test_that("hmrdt: regression tree matches Python", {
  r <- morie_geron_regression_tree(matrix(c(1, 2, 3, 4), ncol = 1), c(1, 1, 5, 5), max_depth = 1)
  expect_equal(r$tree$threshold, A4c$hmrdt$threshold)
  expect_equal(r$n_leaves, A4c$hmrdt$n_leaves)
  expect_equal(r$predictions, A4c$hmrdt$predictions)
})

test_that("hmrec: recall matches Python", {
  r <- morie_geron_recall_hm(c(1, 1, 0, 0), c(1, 0, 0, 0))
  expect_equal(r$tp, A4c$hmrec$tp)
  expect_equal(r$fn, A4c$hmrec$fn)
  expect_equal(r$recall, A4c$hmrec$recall)
})

test_that("hmregn: regression MLP converges", {
  r <- morie_geron_regression_mlp(matrix(c(1, 2, 3, 4), ncol = 1), c(2, 4, 6, 8), hidden_sizes = 8, epochs = 800, lr = 0.02)
  expect_true(r$mse < 0.05)
  expect_true(r$loss_history[length(r$loss_history)] < r$loss_history[1])
  expect_equal(r$n_parameters, A4c$hmregn$n_parameters)
})

test_that("hmrelu: ReLU matches Python", {
  r <- morie_geron_relu(c(-2.0, 0.0, 3.0))
  expect_equal(r$a, A4c$hmrelu$a)
  expect_equal(r$gradient, A4c$hmrelu$gradient)
  expect_equal(round(r$dead_fraction, 6), A4c$hmrelu$dead_fraction)
})

test_that("hmrfc: random forest separates classes", {
  X <- matrix(c(1, 9, 2, 8, 8, 2, 9, 1), ncol = 2, byrow = TRUE)
  r <- morie_geron_random_forest(X, c(0, 0, 1, 1), n_estimators = 9, seed = 4)
  expect_equal(r$accuracy, A4c$hmrfc$accuracy)
  expect_equal(as.integer(r$predict(matrix(c(1.5, 8.5, 8.5, 1.5), ncol = 2, byrow = TRUE))), A4c$hmrfc$predict)
})

test_that("hmrgpt: nn.Sequential architecture matches Python", {
  r <- morie_geron_regression_mlp_pytorch(matrix(c(1, 2, 3, 4), ncol = 1), c(2, 4, 6, 8), hidden = 8, epochs = 800, lr = 0.02)
  expect_equal(r$layers, A4c$hmrgpt$layers)
  expect_equal(r$n_parameters, A4c$hmrgpt$n_parameters)
})

test_that("hmrl: RL rollout return matches Python", {
  clock <- new.env()
  clock$t <- 0
  reset <- function() { clock$t <- 0
  0 }
  step <- function(a) { clock$t <- clock$t + 1
  list(clock$t, 1.0, clock$t >= 3) }
  r <- morie_geron_reinforcement_learning(list(reset = reset, step = step), function(s) 0, gamma = 0.5)
  expect_equal(r$mean_return, A4c$hmrl$mean_return)
  expect_equal(r$lengths, A4c$hmrl$lengths)
})

test_that("hmrlhf: RLHF optimum matches Python", {
  r <- morie_geron_rlhf(matrix(c(0.0, 0.0), nrow = 1), matrix(c(0.0, 1.0), nrow = 1), beta = 1.0, epochs = 800, lr = 0.5)
  expect_equal(round(r$optimal_policy[1, ], 6), A4c$hmrlhf$optimal_policy)
  expect_true(r$max_deviation < 1e-4)
})

test_that("hmrnfc: REINFORCE step matches Python", {
  gs <- function(s, a) if (a == 0) c(1.0, 0.0) else c(0.0, 1.0)
  r <- morie_geron_reinforce(list(list(list(0, 0, 1.0), list(1, 1, 1.0))), gs, gamma = 0.5, eta = 0.1)
  expect_equal(r$step, A4c$hmrnfc$step)
  expect_equal(r$theta, A4c$hmrnfc$theta)
})

test_that("hmrnn: recurrent neuron matches Python", {
  r <- morie_geron_recurrent_neuron(1.0, 0.0, matrix(1.0, 1, 1), matrix(1.0, 1, 1), 0.0)
  expect_equal(round(r$h[1], 6), A4c$hmrnn$h0)
})

test_that("hmroc: ROC curve matches Python", {
  r <- morie_geron_roc_curve_hm(c(0, 0, 1, 1), c(0.1, 0.4, 0.35, 0.8))
  expect_equal(r$auc, A4c$hmroc$auc)
  expect_equal(round(r$auc_trapezoid, 12), A4c$hmroc$auc_trapezoid)
  expect_equal(r$tpr, A4c$hmroc$tpr)
})

test_that("hmrpca: randomized PCA matches Python", {
  r <- morie_geron_randomized_pca(matrix(c(1, 2, 3, 1, 2, 3), ncol = 2), 1)
  expect_equal(round(r$singular_values[1], 9), A4c$hmrpca$sv0)
  expect_equal(round(r$explained_variance[1], 9), A4c$hmrpca$var0)
})

test_that("hmrpt: random patches matches Python", {
  const <- function(Xb, yb) function(A) rep(2.0, nrow(as.matrix(A)))
  X <- matrix(c(1, 0, 5, 2, 1, 4, 3, 2, 3, 4, 3, 2), ncol = 3, byrow = TRUE)
  r <- morie_geron_random_patches(X, c(1.0, 1.0, 3.0, 3.0), const, 8, max_samples = 2, max_features = 2, seed = 11)
  expect_equal(r$predict(X), A4c$hmrpt$predict)
  expect_equal(r$train_mse, A4c$hmrpt$train_mse)
})

test_that("hmrsc: randomized search matches Python", {
  X <- matrix(c(1, 2, 3, 4), ncol = 1)
  r <- morie_geron_randomized_search(list(alpha = c(0.0, 100.0)), 6, X, c(2.0, 4.0, 6.0, 8.0), K = 2)
  expect_equal(r$best_params$alpha, A4c$hmrsc$best_alpha)
  expect_equal(round(r$best_score, 8), A4c$hmrsc$best_score)
})

test_that("hmrsp: random subspaces matches Python", {
  const <- function(Xb, yb) function(A) rep(2.0, nrow(as.matrix(A)))
  X <- matrix(c(1, 0, 5, 9, 2, 1, 4, 8), ncol = 4, byrow = TRUE)
  r <- morie_geron_random_subspaces(X, c(1.0, 3.0), const, 6, max_features = 2, seed = 5)
  expect_equal(r$predict(X), A4c$hmrsp$predict)
})

test_that("hmrvat: visual attention matches Python", {
  f <- matrix(c(1, 0, 0, 1), ncol = 2, byrow = TRUE)
  r <- morie_geron_rnn_visual_attention(f, 0.0, diag(2), matrix(0, 2, 1), c(1.0, 0.0))
  expect_equal(round(sum(r$alpha), 12), A4c$hmrvat$alpha_sum)
  expect_true(r$alpha[1] > r$alpha[2])
})

test_that("hmrvn: RevNet inverts exactly, matches Python", {
  r <- morie_geron_revnet(c(1.0, 2.0, 3.0, 4.0), function(a) 2 * a, function(a) a + 1)
  expect_equal(r$y, A4c$hmrvn$y)
  expect_equal(r$reconstruction_error, A4c$hmrvn$reconstruction_error)
})

test_that("hmrwd: reward function matches Python", {
  tbl <- array(c(0, 0, 2, 0, 1, 0, 0, 2), dim = c(2, 2, 2))
  # R array [s, a, s'] filled to match table[[s]][[a]][[s']] from Python's nested list.
  tbl <- array(0, dim = c(2, 2, 2))
  py_table <- list(list(c(0.0, 1.0), c(2.0, 0.0)), list(c(0.0, 0.0), c(0.0, 2.0)))
  for (s in 0:1) for (a in 0:1) for (sp in 0:1) tbl[s + 1, a + 1, sp + 1] <- py_table[[s + 1]][[a + 1]][sp + 1]
  r <- morie_geron_reward_function(c(0, 1, 1), c(0, 0, 1), c(1, 0, 1), R = tbl, gamma = 0.5)
  expect_equal(r$rewards, A4c$hmrwd$rewards)
  expect_equal(r$discounted_return, A4c$hmrwd$discounted_return)
  expect_equal(r$returns, A4c$hmrwd$returns)
})
