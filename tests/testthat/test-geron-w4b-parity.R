# SPDX-License-Identifier: AGPL-3.0-or-later
# Parity tests for the rw4_b R-port shard against A4b anchors (Python-generated).

test_that("hmgru matches Python anchor", {
  W <- list(W_z = matrix(0, 2, 2), U_z = matrix(0, 2, 2), b_z = c(0, 0),
            W_r = matrix(0, 2, 2), U_r = matrix(0, 2, 2), b_r = c(0, 0),
            W_h = matrix(0, 2, 2), U_h = matrix(0, 2, 2), b_h = c(0, 0))
  r <- morie_geron_gru(c(0, 0), c(4, -2), W)
  expect_equal(r$h_t, A4b$hmgru$h_t)
  expect_equal(r$z_t, A4b$hmgru$z_t)
  W2 <- W; W2$b_z <- c(-40, -40)
  r2 <- morie_geron_gru(c(0, 0), c(4, -2), W2)
  expect_equal(round(r2$h_t, 9), c(4, -2))
})

test_that("hmhei matches deterministic targets; draw is statistically sane", {
  r <- morie_geron_he_init(8, seed = 0, fan_out = 4)
  expect_equal(r$var_target, A4b$hmhei$var_target)
  expect_equal(r$std_target, A4b$hmhei$std_target)
  expect_equal(dim(r$W), A4b$hmhei$shape)
  u <- morie_geron_he_init(6, seed = 1, fan_out = 3, distribution = "uniform")
  expect_equal(round(u$limit, 10), round(sqrt(6 / 6), 10))
  expect_true(all(abs(u$W) <= u$limit + 1e-12))
})

test_that("hmhfpi matches Python anchor", {
  logits <- function(xs) matrix(c(2, 0, 0, 3), nrow = 2, byrow = TRUE)
  r <- morie_geron_hf_pipelines("sentiment-analysis", list("good", "bad"), logits,
                                 labels = c("POSITIVE", "NEGATIVE"))
  expect_equal(vapply(r$predictions, function(p) p$label, ""), A4b$hmhfpi$labels)
  expect_equal(r$predictions[[1]]$score, A4b$hmhfpi$score0, tolerance = 1e-9)
})

test_that("hmhftn full-batch matches Python anchor (shuffle-independent)", {
  X <- matrix(c(1, 2, 3, 4), ncol = 1); y <- c(3, 6, 9, 12)
  lg <- function(p, Xb, yb) { r <- as.numeric(Xb %*% p) - yb; list(mean(r^2), (2 / length(yb)) * as.numeric(t(Xb) %*% r)) }
  m0 <- list(params = 0, loss_and_grad = lg)
  r <- morie_geron_hf_trainer(m0, list(epochs = 50, batch_size = 4, learning_rate = 0.05), list(X, y), list(X, y))
  expect_equal(round(r$params[1], 6), A4b$hmhftn$params0)
  expect_equal(length(r$history), A4b$hmhftn$n_history)
})

test_that("hmhgb matches Python anchor", {
  X <- matrix(c(0, 1, 2, 3, 4, 5), ncol = 1); y <- c(0, 0, 5, 5, 10, 10)
  r <- morie_geron_histogram_gradient_boosting(X, y, max_iter = 50, learning_rate = 0.3)
  expect_lt(r$train_mse, A4b$hmhgb$train_mse_tol)
  expect_equal(r$baseline, A4b$hmhgb$baseline)
  expect_equal(r$bins_used, A4b$hmhgb$bins_used)
})

test_that("hmhplm matches Python anchor", {
  X <- matrix(as.numeric(0:19), ncol = 1); y <- as.numeric(0:19)
  vshape <- function(L, Xt, yt, Xv, yv) abs(L - 3) + 0.5
  r <- morie_geron_hidden_layers_heuristic(vshape, X, y, max_layers = 10, patience = 2)
  expect_equal(r$best_n_layers, A4b$hmhplm$best_n_layers)
  expect_equal(r$depths_tried, A4b$hmhplm$depths_tried)
  expect_equal(r$stopped_early, A4b$hmhplm$stopped_early)
})

test_that("hmhpt grid branch matches Python anchor exactly", {
  X <- matrix(c(1, 1, 1, 2, 1, 3, 1, 4), ncol = 2, byrow = TRUE); y <- c(3, 5, 7, 9)
  r <- morie_geron_hyperparameter_tuning(list(alpha = c(0, 1, 100)), X, y, K = 2)
  expect_equal(r$best_params$alpha, A4b$hmhpt$best_alpha)
  expect_equal(round(r$best_score, 8), A4b$hmhpt$best_score)
})

test_that("hmicl matches Python anchor", {
  scorer <- function(prompt, cand) as.numeric(lengths(regmatches(prompt, gregexpr(as.character(cand), prompt, fixed = TRUE))))
  ex <- list(list("a", "pos"), list("b", "pos"), list("c", "neg"))
  r <- morie_geron_in_context_learning(scorer, ex, "d")
  expect_equal(r$prediction, A4b$hmicl$prediction)
  expect_equal(r$n_shot, A4b$hmicl$n_shot)
})

test_that("hmigr matches Python anchor", {
  r <- morie_geron_information_gain(c(0, 0, 1, 1), c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(r$parent_entropy, A4b$hmigr$parent_entropy)
  expect_equal(r$information_gain, A4b$hmigr$information_gain)
})

test_that("hmins matches Python anchor", {
  X <- matrix(c(0, 1, 10), ncol = 1)
  r <- morie_geron_instance_based(X, c(0, 0, 1), c(0.9), k = 1)
  expect_equal(as.integer(r$prediction[1]), A4b$hmins$prediction0)
})

test_that("hmint8 matches Python anchor", {
  r <- morie_geron_int8_quant(c(-1, 0, 0.5, 1))
  expect_equal(r$scale, A4b$hmint8$scale)
  expect_equal(r$q, A4b$hmint8$q)
})

test_that("hmipca matches Python anchor", {
  X <- matrix(c(1, 2, 3, 4, 5, 6, 0, 0, 0, 0, 0, 0), ncol = 2)
  r <- morie_geron_incremental_pca(X, n_components = 1, batch_size = 2)
  expect_equal(r$explained_variance[1], A4b$hmipca$explained_variance0)
  expect_equal(r$n_batches, A4b$hmipca$n_batches)
})

test_that("hmiseg matches Python anchor", {
  img <- array(c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1), dim = c(2, 2, 3))
  r <- morie_geron_image_segmentation(img, n_clusters = 2, seed = 0)
  expect_equal(round(r$inertia, 9), A4b$hmiseg$inertia)
  expect_equal(sort(round(as.vector(r$palette), 6)), A4b$hmiseg$palette_sorted)
})

test_that("hmiso matches Python anchor", {
  X <- matrix(c(0, 1, 2, 3), ncol = 1)
  r <- morie_geron_isomap(X, n_components = 1, n_neighbors = 2)
  emb <- r$embedding[, 1]
  expect_equal(round(abs(emb - emb[1]), 9), A4b$hmiso$gaps)
})

test_that("hmkd matches Python anchor", {
  logits <- matrix(c(2, 0, 0), nrow = 1)
  r <- morie_geron_knowledge_distillation(logits, logits, y = 0L, T = 2.0, alpha = 0.5)
  expect_equal(round(r$kl_loss, 9), A4b$hmkd$kl_loss)
  expect_equal(round(r$ce_loss, 9), A4b$hmkd$ce_loss)
  expect_equal(round(r$loss, 9), A4b$hmkd$loss)
})

test_that("hmkfd matches Python anchor", {
  X <- matrix(c(1, 2, 3, 4), ncol = 1)
  r <- morie_geron_kfold(X, c(2, 4, 6, 8), k = 2)
  expect_equal(round(r$fold_scores, 8), A4b$hmkfd$fold_scores)
  expect_equal(round(r$cv_score, 8), A4b$hmkfd$cv_score)
})

test_that("hmkmlim matches Python anchor", {
  X <- matrix(c(0, 0.1, 0.2, 0.3, 10.0), ncol = 1)
  r <- morie_geron_kmeans_limits(X, n_clusters = 2, seed = 0)
  expect_equal(r$size_ratio, A4b$hmkmlim$size_ratio)
})

test_that("hmkmn matches Python anchor", {
  X <- matrix(c(0, 1, 10, 11), ncol = 1)
  r <- morie_geron_kmeans(X, n_clusters = 2, seed = 0)
  expect_equal(sort(as.vector(r$centers)), A4b$hmkmn$centers_sorted)
  expect_equal(round(r$inertia, 9), A4b$hmkmn$inertia)
})

test_that("hmkmpp matches Python anchor", {
  r <- morie_geron_kmeans_plus_plus(matrix(c(0, 1, 2), ncol = 1), n_clusters = 3, seed = 5)
  expect_equal(sort(as.integer(r$indices)), A4b$hmkmpp$indices_sorted)
})

test_that("hmkppl matches Python anchor", {
  r <- morie_geron_kernel_pca_poly(matrix(c(1, 2), ncol = 1), n_components = 1, degree = 2, gamma = 1.0, coef0 = 1.0)
  expect_equal(r$K[1, 2], A4b$hmkppl$K01)
  expect_equal(r$feature_space_dim, A4b$hmkppl$feature_space_dim)
})

test_that("hmkprbf matches Python anchor", {
  X <- matrix(c(0, 1, 2, 5), ncol = 1)
  r <- morie_geron_kernel_pca_rbf(X, n_components = 2, gamma = 0.5)
  expect_equal(sum(diag(r$K)), A4b$hmkprbf$trace_K)
})

test_that("hmkpsg matches Python anchor", {
  r <- morie_geron_kernel_pca_sigmoid(matrix(c(1, 2, 3), ncol = 1), n_components = 1, gamma = 1.0, coef0 = 0.0)
  expect_equal(round(r$K[1, 2], 9), A4b$hmkpsg$K01)
})

test_that("hmkrn matches Python anchor", {
  r <- morie_geron_filter_kernel(3, 3, 64, 64, seed = 0)
  expect_equal(as.integer(r$shape), A4b$hmkrn$shape)
  expect_equal(r$n_parameters, A4b$hmkrn$n_parameters)
  expect_equal(r$fan_in, A4b$hmkrn$fan_in)
  expect_equal(r$std, A4b$hmkrn$std)
})

test_that("hmkvc matches Python anchor", {
  loud <- array(1, dim = c(2, 8, 4)); loud[1, , ] <- 1000
  a <- morie_geron_kv_cache_compress(loud, loud, n_bits = 8, per_head = TRUE)
  b <- morie_geron_kv_cache_compress(loud, loud, n_bits = 8, per_head = FALSE)
  expect_lt(a$max_error, b$max_error)
})

test_that("hml1c matches Python anchor", {
  r <- morie_geron_one_cycle(t = 0, T = 5, lr_max = 0.5, lr_min = 0.1)
  expect_equal(round(r$lr_schedule, 6), A4b$hml1c$lr_schedule)
  expect_equal(r$peak_step, A4b$hml1c$peak_step)
  expect_equal(r$phase, A4b$hml1c$phase)
})

test_that("hml2r matches Python anchor", {
  r <- morie_geron_l2_regularization(c(3, -1, 0), alpha = 0.5)
  expect_equal(r$penalty, A4b$hml2r$penalty)
  expect_equal(r$gradient, A4b$hml2r$gradient)
})

test_that("hmlaso matches Python anchor", {
  r <- morie_geron_lasso_cost(matrix(c(1, 2), ncol = 1), c(2, 4), c(2), alpha = 0.5)
  expect_equal(r$mse, A4b$hmlaso$mse)
  expect_equal(r$penalty, A4b$hmlaso$penalty)
  expect_equal(r$cost, A4b$hmlaso$cost)
})

test_that("hmlcv matches Python anchor", {
  X <- cbind(1, as.numeric(0:19))
  y <- 3 + 2 * (0:19)
  r <- morie_geron_learning_curves(X, y, n_splits = 4, seed = 0)
  expect_equal(round(r$rmse_val[length(r$rmse_val)], 6), A4b$hmlcv$rmse_val_last)
  expect_equal(r$verdict, A4b$hmlcv$verdict)
  expect_equal(length(r$train_sizes), A4b$hmlcv$n_sizes)
})

test_that("hmlle matches Python anchor", {
  X <- matrix(c(0, 1, 2, 3, 4, 5, rep(0, 6)), ncol = 2)
  r <- morie_geron_locally_linear_embedding(X, n_components = 1, n_neighbors = 2)
  expect_equal(round(rowSums(r$weights), 9), A4b$hmlle$weight_rowsums)
  expect_equal(round(r$reconstruction_error[3], 9), A4b$hmlle$recon_err2)
})

test_that("hmlnet matches Python anchor", {
  r <- morie_geron_lenet5(n_classes = 10)
  expect_equal(r$total_parameters, A4b$hmlnet$total_parameters)
  withparam <- Filter(function(L) L$parameters > 0, r$layers)
  got <- lapply(withparam, function(L) c(L$name, as.character(L$parameters)))
  expect_equal(got, A4b$hmlnet$layer_params)
})

test_that("hmlnr matches Python anchor", {
  r <- morie_geron_layer_norm_rnn(matrix(c(1, 3), nrow = 1), eps = 0.0)
  expect_equal(round(as.vector(r$h), 9), A4b$hmlnr$h)
})

test_that("hmlntr matches Python anchor", {
  r <- morie_geron_layer_normalization(matrix(c(1, 3), nrow = 1), eps = 0.0)
  expect_equal(as.vector(r$x_hat), A4b$hmlntr$x_hat)
  expect_equal(r$mu[1], A4b$hmlntr$mu0)
  expect_equal(r$var[1], A4b$hmlntr$var0)
})

test_that("hmlof matches Python anchor", {
  X <- matrix(c(0, 0.1, 0.2, 0.3, 0.4, 10.0), ncol = 1)
  r <- morie_geron_local_outlier_factor(X, n_neighbors = 2)
  expect_equal(r$lof[length(r$lof)] > 3, A4b$hmlof$lof_last_gt3)
  expect_equal(all(r$lof[1:5] < 2), A4b$hmlof$lof_head_lt2)
})

test_that("hmlrh matches Python anchor", {
  curve <- matrix(c(1e-4, 2.0, 1e-3, 1.0, 1e-2, 0.5, 1e-1, 4.0, 1.0, 50.0), ncol = 2, byrow = TRUE)
  r <- morie_geron_learning_rate_heuristic(curve)
  expect_equal(r$lr_diverge, A4b$hmlrh$lr_diverge)
  expect_equal(round(r$lr, 12), A4b$hmlrh$lr)
})

test_that("hmlrl matches Python anchor", {
  r <- morie_geron_linear_regression_life(c(40000.0), theta0 = 4.85, theta1 = 4.91e-5)
  expect_equal(round(r$prediction[1], 6), A4b$hmlrl$prediction0)
})

test_that("hmlrpt full-batch matches Python anchor (shuffle-independent)", {
  X <- matrix(c(0, 1, 2, 3), ncol = 1); y <- c(1, 3, 5, 7)
  r <- morie_geron_linreg_pytorch(X, y, epochs = 2000, lr = 0.05)
  expect_equal(round(r$w[1], 4), A4b$hmlrpt$w0)
  expect_equal(round(r$b, 4), A4b$hmlrpt$b)
  expect_equal(r$gap < 1e-4, A4b$hmlrpt$gap_small)
})

test_that("hmlstm matches Python anchor", {
  Z <- list(W_i = matrix(0, 2, 2), U_i = matrix(0, 2, 2), b_i = c(0, 0),
            W_f = matrix(0, 2, 2), U_f = matrix(0, 2, 2), b_f = c(0, 0),
            W_o = matrix(0, 2, 2), U_o = matrix(0, 2, 2), b_o = c(0, 0),
            W_g = matrix(0, 2, 2), U_g = matrix(0, 2, 2), b_g = c(0, 0))
  r <- morie_geron_lstm(c(0, 0), c(0, 0), c(2, -2), Z)
  expect_equal(r$c_t, A4b$hmlstm$c_t)
  expect_equal(round(r$h_t, 9), A4b$hmlstm$h_t)
})

test_that("hmmbkm converges near the two clusters (structural, RNG non-portable)", {
  X <- matrix(c(0, 0.5, 10.0, 10.5), ncol = 1)
  r <- morie_geron_minibatch_kmeans(X, n_clusters = 2, batch_size = 2, seed = 0, n_iter = 200)
  cs <- sort(as.vector(r$centers))
  expect_true(abs(cs[1] - 0.25) < A4b$hmmbkm$tol)
  expect_true(abs(cs[2] - 10.25) < A4b$hmmbkm$tol)
})

test_that("hmmcd p=0 matches Python anchor exactly", {
  f <- function(z) sum(z)
  r <- morie_geron_mc_dropout(f, c(1, 2, 3), K = 10, p = 0.0)
  expect_equal(r$mean[1], A4b$hmmcd$mean0)
  expect_equal(r$var[1], A4b$hmmcd$var0)
})

test_that("hmmcel matches Python anchor", {
  leaky <- function(c, x) 0.5 * c + x
  r <- morie_geron_memory_cell(c(2, 4), c(1, 1), leaky)
  expect_equal(r$c_t, A4b$hmmcel$c_t)
})

test_that("hmmcp matches Python anchor", {
  TOOLS <- list(list(name = "add", description = "add two numbers"))
  server <- function(req) {
    if (req$method == "tools/list") return(list(jsonrpc = "2.0", id = req$id, result = list(tools = TOOLS)))
    if (req$method == "tools/call") {
      a <- req$params$arguments$a; b <- req$params$arguments$b
      return(list(jsonrpc = "2.0", id = req$id, result = list(content = a + b)))
    }
    list(jsonrpc = "2.0", id = req$id, error = list(code = -32601L, message = "Method not found"))
  }
  reqs <- list(list(jsonrpc = "2.0", id = 1, method = "tools/list"),
               list(jsonrpc = "2.0", id = 2, method = "tools/call",
                    params = list(name = "add", arguments = list(a = 2, b = 3))))
  r <- morie_geron_model_context_protocol(server, reqs)
  expect_equal(r$n_ok, A4b$hmmcp$n_ok)
  expect_equal(r$n_errors, A4b$hmmcp$n_errors)
  expect_equal(r$exchanges[[2]]$response$result$content, A4b$hmmcp$content1)
})

test_that("hmmdc matches Python anchor", {
  real <- matrix(c(0, 10, 20, 30), ncol = 1)
  r <- morie_geron_mode_collapse(matrix(c(0.1, 10.1, 0.2, 9.9), ncol = 1), reference = real)
  expect_equal(r$coverage, A4b$hmmdc$coverage)
  expect_equal(r$n_modes, A4b$hmmdc$n_modes)
})

test_that("hmmdp matches Python anchor", {
  P <- array(1.0, dim = c(1, 2, 1)); R <- matrix(c(1, 0), nrow = 1)
  r <- morie_geron_mdp(c("s"), c("good", "bad"), P, R, gamma = 0.9)
  expect_equal(round(r$V[1], 6), A4b$hmmdp$V0)
  expect_equal(r$policy_labels, A4b$hmmdp$policy_labels)
})

test_that("hmmds matches Python anchor", {
  X <- matrix(c(0, 1, 3, 6), ncol = 1)
  r <- morie_geron_mds(X, n_components = 1)
  expect_equal(round(r$stress, 9), A4b$hmmds$stress)
})

test_that("hmmha matches Python anchor", {
  Q <- matrix(c(1, 0), nrow = 1); K <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE); V <- K
  r <- morie_geron_multihead_attention(Q, K, V, n_heads = 2)
  expect_equal(r$d_head, A4b$hmmha$d_head)
  expect_equal(dim(r$output), A4b$hmmha$output_shape)
})

test_that("hmmis7 matches Python anchor", {
  r <- morie_geron_mistral7b(c(1, 2, 3, 4), n_tokens = 2)
  expect_equal(r$total_parameters, A4b$hmmis7$total_parameters)
  expect_equal(r$d_head, A4b$hmmis7$d_head)
  expect_equal(round(r$kv_cache_saving, 6), A4b$hmmis7$kv_cache_saving)
})

test_that("hmmish matches Python anchor", {
  r <- morie_geron_mish(c(20, 800))
  expect_equal(round(r$activation, 6), A4b$hmmish$activation)
})

test_that("hmmlb matches Python anchor", {
  Y <- matrix(c(1, 0, 1, 0, 1, 1), nrow = 2, byrow = TRUE)
  P <- matrix(c(1, 0, 0, 0, 1, 1), nrow = 2, byrow = TRUE)
  r <- morie_geron_multilabel(matrix(c(0, 1), ncol = 1), Y, Y_pred = P)
  expect_equal(round(r$hamming_loss, 6), A4b$hmmlb$hamming_loss)
  expect_equal(r$subset_accuracy, A4b$hmmlb$subset_accuracy)
  expect_equal(r$jaccard, A4b$hmmlb$jaccard)
})

test_that("hmmlm matches Python anchor", {
  X <- rep(c(0, 1, 2, 3), 5)
  r <- morie_geron_masked_lm(X, mask_frac = 0.15, seed = 0)
  expect_equal(r$n_masked, A4b$hmmlm$n_masked)
  expect_equal(round(r$loss, 9) == round(r$baseline_loss, 9), A4b$hmmlm$loss_eq_baseline)
})

test_that("hmmlpf matches Python anchor", {
  W1 <- matrix(c(1, 1, 1, 1), nrow = 2, byrow = TRUE); b1 <- c(0, -1)
  W2 <- matrix(c(1, -2), ncol = 1); b2 <- c(0)
  X <- matrix(c(0, 0, 0, 1, 1, 0, 1, 1), ncol = 2, byrow = TRUE)
  r <- morie_geron_mlp(X, list(W1, W2), list(b1, b2), list("relu", "identity"))
  expect_equal(as.vector(r$output), A4b$hmmlpf$output)
  expect_equal(r$n_parameters, A4b$hmmlpf$n_parameters)
})

test_that("hmmms matches Python anchor", {
  r <- morie_geron_min_max_scaling(c(1, 2, 3, 4, 5))
  expect_equal(as.vector(r$X_scaled), A4b$hmmms$X_scaled)
})

test_that("hmmnl matches Python anchor", {
  X <- matrix(c(0, 0.1, 5, 5.1, 10, 10.1), ncol = 1)
  y <- c(0, 0, 1, 1, 2, 2)
  r <- morie_geron_multinomial_logistic(X, y, lr = 0.5, n_iter = 3000)
  expect_equal(r$accuracy, A4b$hmmnl$accuracy)
  expect_equal(r$prediction, A4b$hmmnl$prediction)
})

test_that("hmmnsh matches Python anchor", {
  X <- matrix(c(0, 0.2, 10.0, 10.2), ncol = 1)
  r <- morie_geron_mean_shift(X, bandwidth = 1.0)
  expect_equal(r$n_clusters, A4b$hmmnsh$n_clusters)
  expect_equal(sort(round(as.vector(r$modes), 4)), A4b$hmmnsh$modes_sorted)
})

test_that("hmmod matches Python anchor", {
  X <- matrix(c(0, 1, 2, 3), ncol = 1); y <- c(3, 5, 7, 9)
  r <- morie_geron_model_based(X, y)
  expect_equal(round(r$theta, 9), A4b$hmmod$theta)
  expect_equal(round(r$mse, 9), A4b$hmmod$mse)
})

test_that("hmmpp matches Python anchor", {
  r <- morie_geron_model_parallelism(list(10, 20, 30, 40), 2)
  expect_equal(as.integer(r$assignment), A4b$hmmpp$assignment)
  expect_equal(as.vector(r$device_loads), A4b$hmmpp$device_loads)
})

test_that("hmmto matches Python anchor", {
  X <- matrix(c(0, 1, 10, 11), ncol = 1)
  Y <- matrix(c(0, 1, 0, 1, 1, 0, 1, 0), ncol = 2, byrow = TRUE)
  r <- morie_geron_multioutput(X, Y)
  pred <- matrix(as.numeric(r$predictions), nrow = 4)
  expect_equal(pred, A4b$hmmto$predictions)
  expect_equal(r$accuracy, A4b$hmmto$accuracy)
})

test_that("hmmxp matches Python anchor", {
  x <- matrix(0:15, nrow = 4, byrow = TRUE)
  r <- morie_geron_max_pool(x, 2)
  expect_equal(r$pooled, A4b$hmmxp$pooled)
})
